// SimpleCa²⁺ Desktop — Preload Script
// Runs in the renderer process before the page loads.
// Provides a safe bridge between Electron and the Shiny web content.

const { contextBridge } = require('electron');

contextBridge.exposeInMainWorld('simpleca', {
  platform: process.platform,
  isDesktop: true,
  version: '1.1.0'
});
