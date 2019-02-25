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

app.all("/webdav/file", webdavProxy)
app.all("/webdav/tree", webdavProxy)

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


function webdavProxy(req, res) {
  return request({
    agentOptions: { rejectUnauthorized: false },
    url: req.query.url,
    method: req.method,
    headers: Object.assign({}, req.headers, { authorization: req.query.auth, host: undefined })
  })
  .on("error", err => res.status(500).send(err.message))
  .pipe(res)
}



// 👨‍🚀


app.listen(44999, () => console.log("HTTP Server running on port 44999"))
