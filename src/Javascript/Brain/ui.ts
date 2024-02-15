import type { App } from "./elm/types"
import * as Artwork from "./artwork"


export function link(worker: Worker, app: App) {
  app.ports.toUI.subscribe(event => {
    worker.postMessage(event)
  })

  worker.onmessage = event => {
    if (event.data.action) return handleAction(event.data.action, event.data.data)
    if (event.data.tag) return app.ports.fromAlien.send(event.data)
  }


  function handleAction(action: string, data: unknown) {
    switch (action) {
      case "DOWNLOAD_ARTWORK": return Artwork.download(data)
    }
  }
}
