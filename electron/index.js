"use strict";

const { app, dialog, BrowserWindow } = require("electron");
const server = require("./server");


// CONSTANTS


const URL = "http://127.0.0.1:8080";



// 🍯


let win;


function createWindow() {
  win = new BrowserWindow({
    titleBarStyle: "hiddenInset"
  });

  win.maximize();
  win.loadURL(URL);
  win.on("closed", () => win = null);
}


app.on("ready", createWindow);
app.on("activate", () => win === null && createWindow());
app.on("window-all-closed", () => process.platform !== "darwin" && app.quit());
