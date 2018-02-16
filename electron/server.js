const express = require("express")
const expandHomeDir = require("expand-home-dir")
const globby = require("globby")
const path = require("path")

const app = express()
const buildDirectory = path.resolve(__dirname, "../")


// 🍯


app.get("/local/file", provideLocalFile)
app.get("/local/tree", makeLocalTree)

app.use(express.static(buildDirectory))
app.use(function(req, res, next) {
  if (req.is("html")) {
    res.sendFile(`${buildDirectory}/index.html`)
  }
})


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



// 👨‍🚀


app.listen(44999, () => console.log("HTTP Server running on port 44999"))
