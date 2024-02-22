import { fileExtension } from "../common"
import { transformUrl } from "../urls"


async function download(group) {
  const { saveAs } = await import("file-saver").then(a => a.default)
  const JSZip = await import("jszip").then(a => a.default)

  const zip = new JSZip()
  const folder = zip.folder("Diffuse - " + group.name)
  if (!folder) throw new Error("Failed to create ZIP file")

  return group.tracks.reduce(
    (acc, track) => {
      return acc
        .then(_ => transformUrl(track.url, app))
        .then(fetch)
        .then(r => {
          const mimeType = r.headers.get("content-type")
          const fileExt = fileExtension(mimeType) || "unknown"

          return r.blob().then(
            b => folder.file(track.filename + "." + fileExt, b)
          )
        })
    },
    Promise.resolve()

  ).then(_ => zip.generateAsync({ type: "blob" })
  ).then(zipFile => {
    saveAs(zipFile, "Diffuse - " + group.name + ".zip")
    app.ports.downloadTracksFinished.send(null)

  })
}
