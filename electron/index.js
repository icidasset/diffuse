const { app, BrowserWindow } = require("electron")
const server = require("./server")


// Constants


const URL = "http://127.0.0.1:44999"



// Windows


let win


function createWindow() {
  if (win) return

  win = new BrowserWindow({
    titleBarStyle: "hiddenInset",
    webPreferences: {}
  })

  win.maximize()
  win.loadURL(URL)
  win.on("closed", () => win = null)

  if (process.env.ENV === "DEV") {
    win.webContents.openDevTools()
    win.webContents.session.clearCache(_ => null)
  }
}


app.on("ready", createWindow)
app.on("activate", createWindow)
app.on("window-all-closed", () => process.platform !== "darwin" && app.quit())
