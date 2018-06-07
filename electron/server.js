const express = require("express")
const expandHomeDir = require("expand-home-dir")
const fetch = require("node-fetch")
const globby = require("globby")
const path = require("path")
const sendSeekable = require("send-seekable")

const app = express()
const buildDirectory = path.resolve(__dirname, "../")


// 🍯


app.use(sendSeekable)

app.get("/local/file", provideLocalFile)
app.get("/local/tree", makeLocalTree)

app.get("/webdav/file", provideWebDavFile)
app.get("/webdav/tree", makeWebDavTree)

app.use(express.static(buildDirectory))
app.use(function(req, res, next) {
  if (req.is("html")) {
    res.sendFile(`${buildDirectory}/index.html`)
  }
})



// Local Service


function provideLocalFile(req, res) {
  res.sendFile(expandHomeDir(req.query.path))
}


function makeLocalTree(req, res) {
  if (!req.query.path) {
    return res.status(422).json({ errors: ["Invalid request"] })
  }

  globby(["**/*.*"], { cwd: expandHomeDir(req.query.path) }).then(results => {
    res.json(results)
  })
}



// WebDAV Service


function provideWebDavFile(req, res) {
  fetch(
    req.query.url,
    {
      method: req.method,
      headers: Object.assign(
        { "Range": req.headers.Range },
        webDavHeaders(req.query)
      )
    }
  ).then(r => req.method === "GET" ? r.buffer() : r.text()
  ).then(r => req.method === "GET" ? res.sendSeekable(r) : res.send(r)
  ).catch(_ => res.status(500).send("Request failed")
  )
}


function makeWebDavTree(req, res) {
  fetch(
    req.query.url,
    { method: "PROPFIND", headers: webDavHeaders(req.query) }
  ).then(r => r.text()
  ).then(r => res.send(r)
  ).catch(_ => res.status(500).send("Request failed")
  )
}


function webDavHeaders(query) {
  return {
    "Authorization": "Basic " + Buffer.from(
      query.username + ":" + query.password
    ).toString("base64")
  }
}



// 👨‍🚀


app.listen(44999, () => console.log("HTTP Server running on port 44999"))
