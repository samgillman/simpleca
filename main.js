// SimpleCa²⁺ Desktop — Electron Main Process
// Launches a bundled R/Shiny server and displays it in a native window.

const { app, BrowserWindow, dialog, Menu } = require('electron');
const { spawn, spawnSync, execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const http = require('http');

// ─── Configuration ───────────────────────────────────────────────
const APP_NAME = 'SimpleCa\u00b2\u207a';
const SHINY_HOST = '127.0.0.1';
const POLL_INTERVAL_MS = 500;
const STARTUP_TIMEOUT_MS = 90000; // 90 seconds max to wait for Shiny
const MIN_WIDTH = 1100;
const MIN_HEIGHT = 700;

let mainWindow = null;
let loadingWindow = null;
let rProcess = null;
let shinyPort = null;
let isQuitting = false;

// ─── Paths ───────────────────────────────────────────────────────

function isPackaged() {
  return app.isPackaged;
}

function getRuntimeResourcePath() {
  if (isPackaged()) {
    return process.resourcesPath;
  }
  return __dirname;
}

function getRendererAssetPath(fileName) {
  return path.join(app.getAppPath(), fileName);
}

function getShinyAppPath() {
  return path.join(getRuntimeResourcePath(), 'shiny');
}

function getAppLibraryPath() {
  return path.join(getRuntimeResourcePath(), 'r-portable', 'site-library');
}

/**
 * Find a working R executable.
 * Priority:
 *   1. Bundled r-portable/bin/Rscript or r-portable/bin/R
 *   2. System Rscript/R on macOS
 *   3. System Rscript/R via PATH
 */
function findRscript() {
  const isWindows = process.platform === 'win32';
  const bundledCandidates = [
    path.join(getRuntimeResourcePath(), 'r-portable', 'bin', isWindows ? 'R.exe' : 'R'),
    path.join(getRuntimeResourcePath(), 'r-portable', 'bin', isWindows ? 'Rscript.exe' : 'Rscript')
  ];

  for (const candidate of bundledCandidates) {
    if (isUsableRCommand(candidate)) {
      console.log(`[SimpleCa] Found bundled R command at: ${candidate}`);
      return candidate;
    }
  }

  if (isPackaged()) {
    console.error(`[SimpleCa] Bundled R runtime is missing or not executable.`);
    return null;
  }

  const candidates = [];

  // 2. macOS framework R
  if (process.platform === 'darwin') {
    candidates.push('/Library/Frameworks/R.framework/Resources/bin/R');
    candidates.push('/Library/Frameworks/R.framework/Resources/bin/Rscript');
    candidates.push('/opt/homebrew/bin/R');
    candidates.push('/opt/homebrew/bin/Rscript');
    candidates.push('/usr/local/bin/R');
    candidates.push('/usr/local/bin/Rscript');
  }

  // 3. Linux common paths
  if (process.platform === 'linux') {
    candidates.push('/usr/bin/R');
    candidates.push('/usr/bin/Rscript');
    candidates.push('/usr/local/bin/R');
    candidates.push('/usr/local/bin/Rscript');
  }

  for (const candidate of candidates) {
    if (isUsableRCommand(candidate)) {
      console.log(`[SimpleCa] Found R command at: ${candidate}`);
      return candidate;
    }
  }

  // 4. Try PATH as last resort
  const pathCandidates = process.platform === 'win32' ? ['R', 'Rscript'] : ['R', 'Rscript'];
  for (const command of pathCandidates) {
    try {
      const which = process.platform === 'win32' ? `where ${command}` : `which ${command}`;
      const result = execSync(which, { encoding: 'utf-8' }).trim().split('\n')[0];
      if (result && isUsableRCommand(result)) {
        console.log(`[SimpleCa] Found ${command} on PATH: ${result}`);
        return result;
      }
    } catch (e) {
      // not found on PATH
    }
  }

  return null;
}

function isUsableRCommand(commandPath) {
  if (!commandPath || !fs.existsSync(commandPath)) {
    return false;
  }

  const lowerPath = commandPath.toLowerCase();
  const isRscript = lowerPath.endsWith('rscript') || lowerPath.endsWith('rscript.exe');
  const probeArgs = isRscript
    ? ['--vanilla', '-e', 'cat("ok\\n")']
    : ['--vanilla', '--slave', '-e', 'cat("ok\\n")'];

  try {
    const probe = spawnSync(commandPath, probeArgs, { stdio: 'ignore', timeout: 15000 });
    return probe.status === 0;
  } catch (e) {
    return false;
  }
}

function getRExecutionArgs(rCommand, rCode) {
  const lowerPath = rCommand.toLowerCase();
  const isRscript = lowerPath.endsWith('rscript') || lowerPath.endsWith('rscript.exe');
  if (isRscript) {
    return ['--vanilla', '-e', rCode];
  }
  return ['--vanilla', '--slave', '-e', rCode];
}

// ─── Port Selection ──────────────────────────────────────────────

async function getAvailablePort() {
  try {
    const getPort = require('get-port');
    return await getPort({ port: getPort.portNumbers(3838, 3900) });
  } catch (e) {
    // Fallback: try ports sequentially
    for (let port = 3838; port <= 3900; port++) {
      const available = await checkPortAvailable(port);
      if (available) return port;
    }
    return 3838;
  }
}

function checkPortAvailable(port) {
  return new Promise((resolve) => {
    const server = require('net').createServer();
    server.listen(port, '127.0.0.1', () => {
      server.close(() => resolve(true));
    });
    server.on('error', () => resolve(false));
  });
}

// ─── Shiny Process Management ────────────────────────────────────

function startShinyProcess(port) {
  const rscript = findRscript();
  const appDir = getShinyAppPath();
  const appLibrary = getAppLibraryPath();

  if (!rscript) {
    const isMissingBundledR = isPackaged();
    const title = isMissingBundledR ? 'Runtime Missing' : 'R Not Found';
    const message = isMissingBundledR
      ? 'SimpleCa\u00b2\u207a is missing its bundled R runtime.\n\n' +
        'Please reinstall the application from a complete installer package.'
      : 'SimpleCa\u00b2\u207a requires R to be installed.\n\n' +
        'Please install R from:\nhttps://cloud.r-project.org\n\n' +
        'After installing R, restart SimpleCa\u00b2\u207a.';
    dialog.showErrorBox(title, message);
    app.quit();
    return null;
  }

  // Verify app.R exists
  if (!fs.existsSync(path.join(appDir, 'app.R'))) {
    dialog.showErrorBox(
      'App Files Missing',
      `Could not find app.R at:\n${appDir}\n\nThe application may be corrupted. Please reinstall.`
    );
    app.quit();
    return null;
  }

  console.log(`[SimpleCa] R command: ${rscript}`);
  console.log(`[SimpleCa] App dir: ${appDir}`);
  console.log(`[SimpleCa] App library: ${appLibrary}`);
  console.log(`[SimpleCa] Starting Shiny on port ${port}...`);

  // Build the R startup command
  // Prepend our app library to .libPaths so our packages take priority,
  // but also keep system libraries as fallback
  const rCode = [
    `.libPaths(c("${appLibrary.replace(/\\/g, '/')}", .libPaths()))`,
    `options(shiny.port=${port}L, shiny.host="${SHINY_HOST}", shiny.launch.browser=FALSE, shiny.maxRequestSize=100*1024^2)`,
    `shiny::runApp("${appDir.replace(/\\/g, '/')}", launch.browser=FALSE)`
  ].join('; ');

  const env = {
    ...process.env,
    SHINY_ENV: 'production'
  };

  // Prepend our library to R_LIBS so it's found even with --vanilla
  if (fs.existsSync(appLibrary)) {
    const existingLibs = env.R_LIBS || '';
    env.R_LIBS = existingLibs ? `${appLibrary}${path.delimiter}${existingLibs}` : appLibrary;
  }

  const child = spawn(rscript, getRExecutionArgs(rscript, rCode), {
    env,
    stdio: ['ignore', 'pipe', 'pipe']
  });

  child.stdout.on('data', (data) => {
    console.log(`[R stdout] ${data.toString().trim()}`);
  });

  child.stderr.on('data', (data) => {
    console.log(`[R stderr] ${data.toString().trim()}`);
  });

  child.on('error', (err) => {
    console.error(`[SimpleCa] Failed to start R process: ${err.message}`);
    if (!isQuitting) {
      dialog.showErrorBox('R Process Error', `Failed to start R:\n${err.message}`);
      app.quit();
    }
  });

  child.on('exit', (code, signal) => {
    console.log(`[SimpleCa] R process exited (code=${code}, signal=${signal})`);
    if (!isQuitting && code !== 0 && code !== null) {
      dialog.showErrorBox(
        'R Process Crashed',
        `The R process exited unexpectedly (code ${code}).\nThe app will now close.`
      );
      app.quit();
    }
  });

  return child;
}

// ─── Polling for Shiny Readiness ─────────────────────────────────

function pollShiny(port) {
  return new Promise((resolve, reject) => {
    const startTime = Date.now();

    const check = () => {
      if (Date.now() - startTime > STARTUP_TIMEOUT_MS) {
        reject(new Error(`Shiny did not start within ${STARTUP_TIMEOUT_MS / 1000}s`));
        return;
      }

      const req = http.get(`http://${SHINY_HOST}:${port}`, (res) => {
        if (res.statusCode === 200 || res.statusCode === 302) {
          resolve();
        } else {
          setTimeout(check, POLL_INTERVAL_MS);
        }
      });

      req.on('error', () => {
        setTimeout(check, POLL_INTERVAL_MS);
      });

      req.setTimeout(2000, () => {
        req.destroy();
        setTimeout(check, POLL_INTERVAL_MS);
      });
    };

    check();
  });
}

// ─── Window Creation ─────────────────────────────────────────────

function createLoadingWindow() {
  const win = new BrowserWindow({
    width: 420,
    height: 340,
    frame: false,
    transparent: false,
    resizable: false,
    alwaysOnTop: true,
    backgroundColor: '#1a1a2e',
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true
    }
  });

  win.loadFile(getRendererAssetPath('loading.html'));
  return win;
}

function createMainWindow(port) {
  const win = new BrowserWindow({
    width: 1400,
    height: 900,
    minWidth: MIN_WIDTH,
    minHeight: MIN_HEIGHT,
    show: false,
    title: APP_NAME,
    backgroundColor: '#f5f6fa',
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      preload: getRendererAssetPath('preload.js')
    }
  });

  win.loadURL(`http://${SHINY_HOST}:${port}`);

  win.webContents.on('did-finish-load', () => {
    win.setTitle(APP_NAME);
    win.show();
    if (loadingWindow && !loadingWindow.isDestroyed()) {
      loadingWindow.close();
      loadingWindow = null;
    }
  });

  // Keep the native window title clean even if the web app sets an HTML title.
  win.on('page-title-updated', (event) => {
    event.preventDefault();
    win.setTitle(APP_NAME);
  });

  win.on('closed', () => {
    mainWindow = null;
  });

  return win;
}

// ─── Application Menu ────────────────────────────────────────────

function buildMenu() {
  const isMac = process.platform === 'darwin';

  const template = [
    ...(isMac ? [{
      label: APP_NAME,
      submenu: [
        { role: 'about' },
        { type: 'separator' },
        { role: 'hide' },
        { role: 'hideOthers' },
        { role: 'unhide' },
        { type: 'separator' },
        { role: 'quit' }
      ]
    }] : []),
    {
      label: 'Edit',
      submenu: [
        { role: 'undo' },
        { role: 'redo' },
        { type: 'separator' },
        { role: 'cut' },
        { role: 'copy' },
        { role: 'paste' },
        { role: 'selectAll' }
      ]
    },
    {
      label: 'View',
      submenu: [
        { role: 'reload' },
        { role: 'forceReload' },
        { role: 'toggleDevTools' },
        { type: 'separator' },
        { role: 'resetZoom' },
        { role: 'zoomIn' },
        { role: 'zoomOut' },
        { type: 'separator' },
        { role: 'togglefullscreen' }
      ]
    },
    {
      label: 'Window',
      submenu: [
        { role: 'minimize' },
        { role: 'zoom' },
        ...(isMac ? [
          { type: 'separator' },
          { role: 'front' }
        ] : [
          { role: 'close' }
        ])
      ]
    },
    {
      label: 'Help',
      submenu: [
        {
          label: 'About SimpleCa\u00b2\u207a',
          click: () => {
            dialog.showMessageBox(mainWindow, {
              type: 'info',
              title: 'About SimpleCa\u00b2\u207a',
              message: 'SimpleCa\u00b2\u207a v1.1.0',
              detail: 'A modern desktop application for calcium imaging data analysis.\n\nBuilt with R Shiny + Electron.'
            });
          }
        }
      ]
    }
  ];

  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

// ─── Graceful Shutdown ───────────────────────────────────────────

function killRProcess() {
  if (rProcess && !rProcess.killed) {
    console.log('[SimpleCa] Terminating R process...');
    try {
      if (process.platform === 'win32') {
        execSync(`taskkill /pid ${rProcess.pid} /f /t`, { stdio: 'ignore' });
      } else {
        rProcess.kill('SIGTERM');
        setTimeout(() => {
          if (rProcess && !rProcess.killed) {
            rProcess.kill('SIGKILL');
          }
        }, 3000);
      }
    } catch (e) {
      console.error('[SimpleCa] Error killing R process:', e.message);
    }
  }
}

// ─── App Lifecycle ───────────────────────────────────────────────

app.on('ready', async () => {
  console.log(`[SimpleCa] ${APP_NAME} starting...`);
  console.log(`[SimpleCa] Platform: ${process.platform}, Arch: ${process.arch}`);
  console.log(`[SimpleCa] Packaged: ${isPackaged()}`);
  console.log(`[SimpleCa] Runtime resources: ${getRuntimeResourcePath()}`);

  buildMenu();

  // Show loading screen
  loadingWindow = createLoadingWindow();

  try {
    // Find available port
    shinyPort = await getAvailablePort();
    console.log(`[SimpleCa] Selected port: ${shinyPort}`);

    // Start R/Shiny
    rProcess = startShinyProcess(shinyPort);
    if (!rProcess) return;

    // Wait for Shiny to be ready
    await pollShiny(shinyPort);
    console.log('[SimpleCa] Shiny is ready!');

    // Create main window
    mainWindow = createMainWindow(shinyPort);

  } catch (err) {
    console.error(`[SimpleCa] Startup error: ${err.message}`);
    if (loadingWindow && !loadingWindow.isDestroyed()) {
      loadingWindow.close();
    }
    dialog.showErrorBox(
      'Startup Failed',
      `${APP_NAME} could not start:\n${err.message}\n\nPlease check the console for details.`
    );
    killRProcess();
    app.quit();
  }
});

app.on('before-quit', () => {
  isQuitting = true;
  killRProcess();
});

app.on('window-all-closed', () => {
  killRProcess();
  app.quit();
});

app.on('activate', () => {
  if (mainWindow === null && shinyPort) {
    mainWindow = createMainWindow(shinyPort);
  }
});

process.on('uncaughtException', (err) => {
  console.error('[SimpleCa] Uncaught exception:', err);
  killRProcess();
});

process.on('SIGTERM', () => {
  killRProcess();
  app.quit();
});

process.on('SIGINT', () => {
  killRProcess();
  app.quit();
});
