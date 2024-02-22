import type { App } from "./elm/types"


export async function load(): Promise<Worker> {
  const brain = new Worker(
    "./js/brain/index.js#appHref=" + encodeURIComponent(window.location.href),
    { type: "module" }
  )

  await new Promise((resolve, reject) => {
    brain.onmessage = event => {
      if (event.data.action === "READY") resolve(null)
    }

    brain.addEventListener("error", () => {
      reject("<strong>Failed to load web worker.</strong><br />If you're using Firefox, you might need to upgrade your browser (version 113 and up) and set `dom.workers.modules.enabled` to `true` in `about:config`")
    })
  })

  // Fin
  return brain
}


export function link({ app, brain }: { app: App, brain: Worker }) {
  function handleAction(action, data, _ports) {
    switch (action) {
      // TODO:
      // case "DOWNLOAD_TRACKS": return downloadTracks(data)
      // case "FINISHED_DOWNLOADING_ARTWORK": return finishedDownloadingArtwork()
    }
  }

  brain.onmessage = event => {
    if (event.data.action) return handleAction(event.data.action, event.data.data, event.ports)
    if (event.data.tag) app.ports.fromAlien.send(event.data)

    switch (event.data.tag) {
      // TODO:
      // case "GOT_CACHED_COVER": return gotCachedCover(event.data.data)
    }
  }

  app.ports.toBrain.subscribe(a => brain.postMessage(a))
}
