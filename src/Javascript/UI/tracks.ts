import type { App } from "./elm/types"
import { fileExtension } from "../common"
import { transformUrl } from "../urls"


// 🏔️


let app: App



// 🚀


export function init(a: App) {
  app = a
}



// 🛠️


export async function download(group) {
  const { saveAs } = await import("file-saver").then(a => a.default)
  const JSZip = await import("jszip").then(a => a.default)

  const zip = new JSZip()
  const folder = zip.folder("Diffuse - " + group.name)
  if (!folder) throw new Error("Failed to create ZIP file")

  return group.tracks
    .reduce((acc, track) => {
      return acc
        .then(() => transformUrl(track.url, app))
        .then(fetch)
        .then((r: Response) => {
          const mimeType = r.headers.get("content-type")
          const fileExt = (mimeType ? fileExtension(mimeType) : null) || track.path.match(/\.(\w+)$/)[1] || "unknown-ext"

          return r.blob().then((b: Blob) => folder.file(track.filename + "." + fileExt, b))
        })
    }, Promise.resolve())
    .then(() => zip.generateAsync({ type: "blob" }))
    .then((zipFile: Blob) => {
      saveAs(zipFile, "Diffuse - " + group.name + ".zip")
      app.ports.downloadTracksFinished.send(null)
    })
}
