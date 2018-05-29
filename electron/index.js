const electron = require("electron")
const server = require("./server")
const { app, globalShortcut, ipcMain, shell, BrowserWindow, Menu } = electron


// Constants


const URL = "http://127.0.0.1:44999"



// Windows


let win


function createWindow() {
  if (win) return

  win = new BrowserWindow({
    backgroundColor: "#02070E",
    center: true,
    titleBarStyle: "hiddenInset",
    webPreferences: {}
  })

  const workArea = electron.screen.getPrimaryDisplay().workArea

  win.setBounds({
    x: 10,
    y: workArea.y + 10,
    height: workArea.height - 20,
    width: workArea.width - 20,
  })

  win.loadURL(URL)
  win.on("closed", () => win = null)

  if (process.env.ENV === "DEV") {
    win.webContents.openDevTools()
    win.webContents.session.clearCache(_ => null)
  }

  Menu.setApplicationMenu(menu)
}


app.on("ready", createWindow)
app.on("activate", createWindow)
app.on("window-all-closed", app.quit)



// Shortcuts


ipcMain.on("register-shortcuts", event => {
  globalShortcut.register("MediaNextTrack", _ => {
    event.sender.send("media-next-track")
  })

  globalShortcut.register("MediaPlayPause", _ => {
    event.sender.send("media-play-pause")
  })

  globalShortcut.register("MediaPreviousTrack", _ => {
    event.sender.send("media-previous-track")
  })
})


app.on("will-quit", () => {
  globalShortcut.unregisterAll()
})



// Menu


const menuTemplate = [
  {
    label: "Edit",
    submenu: [
      { role: "undo" },
      { role: "redo" },
      { type: "separator" },
      { role: "cut" },
      { role: "copy" },
      { role: "paste" },
      { role: "delete" },
      { role: "selectall" }
    ]
  },
  {
    label: "View",
    submenu: [
      { role: "reload" },
      { role: "forcereload" },
      { role: "toggledevtools" },
      { type: "separator" },
      { role: "togglefullscreen" }
    ]
  },
  {
    role: "window",
    submenu: [
      { role: "minimize" },
      { role: "close" }
    ]
  },
  {
    role: "help",
    submenu: [
      {
        label: "Report an issue",
        click() { shell.openExternal("https://github.com/icidasset/diffuse/issues") }
      }
    ]
  }
]


if (process.platform === "darwin") {
   menuTemplate.unshift({
     label: app.getName(),
     submenu: [
       { role: "hide" },
       { role: "unhide" },
       { role: "quit" }
     ]
   })
}


const menu = Menu.buildFromTemplate(menuTemplate)
