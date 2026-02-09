console.log('Entry point loading...');
console.log('process.type:', process.type);
console.log('electron version:', process.versions.electron);

// When running as Electron main process, require('electron') should
// return the electron API object, not the npm package path
const electron = require('electron');
console.log('electron:', typeof electron, electron === Object(electron) ? 'is object' : 'is NOT object');

if (typeof electron === 'object' && electron.app) {
  console.log('SUCCESS: Running as Electron main process');
  electron.app.whenReady().then(() => {
    console.log('App ready!');
    electron.app.quit();
  });
} else {
  console.log('FAIL: Not in main process context');
  console.log('This could mean the electron npm package is shadowing the built-in module');
  process.exit(1);
}
