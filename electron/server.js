const express = require("express")
const expandHomeDir = require("expand-home-dir")
const globby = require("globby")
const https = require("https")
const path = require("path")
const request = require("request")

const app = express()
const buildDirectory = path.resolve(__dirname, "../")


// 🍯


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
  const headers = Object.assign({}, req.headers, webDavHeaders(req.query))

  delete headers["accept-language"]
  delete headers["host"]
  delete headers["referrer"]
  delete headers["user-agent"]

  return request({
    agentOptions: { rejectUnauthorized: false },
    url: req.query.url,
    method: req.query.method,
    headers: headers,
  })
  .on("error", err => {
    res.status(500)
    res.send(err.message)
  })
  .pipe(res)
}


function makeWebDavTree(req, res) {
  return request({
    agentOptions: { rejectUnauthorized: false },
    url: req.query.url,
    method: "PROPFIND",
    headers: Object.assign(
      { depth: "1" },
      webDavHeaders(req.query)
    ),
  })
  .on("error", err => {
    res.status(500)
    res.send(err.message)
  })
  .pipe(res)
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
