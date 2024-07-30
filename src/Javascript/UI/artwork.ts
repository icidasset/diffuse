import { debounce } from "throttle-debounce"

import type { App } from "./elm/types"
import { type CoverPrep, db } from "../common"



// 🏔️


let app: App
let brain: Worker



// 🚀


export function init(a: App, b: Worker) {
  app = a
  brain = b

  app.ports.loadAlbumCovers.subscribe(
    debounce(500, loadAlbumCoversFromDom)
  )

  db().keys().then(cachedCovers)
}



// 🛠️


export function albumCover(coverKey: string): Promise<Blob | "TRIED" | null> {
  return db().getItem(`coverCache.${coverKey}`)
}


async function loadAlbumCoversFromDom({ coverView, list }: { coverView: boolean, list: boolean }): Promise<void> {
  let nodes: HTMLElement[] = []

  if (list) nodes = nodes.concat(Array.from(
    document.querySelectorAll("#diffuse__track-covers [data-key]")
  ))

  if (coverView) nodes = nodes.concat(Array.from(
    document.querySelectorAll("#diffuse__track-covers + div [data-key]")
  ))

  if (!nodes.length) return;

  const coverPrepList = nodes.reduce((acc: CoverPrep[], node: HTMLElement) => {
    const a = {
      cacheKey: node.getAttribute("data-key"),
      trackFilename: node.getAttribute("data-filename"),
      trackPath: node.getAttribute("data-path"),
      trackSourceId: node.getAttribute("data-source-id"),
      variousArtists: node.getAttribute("data-various-artists")
    }

    if (a.cacheKey && a.trackFilename && a.trackPath && a.trackSourceId && a.variousArtists) {
      return [...acc, a as CoverPrep]
    } else {
      return acc
    }
  }, [] as CoverPrep[])

  return loadAlbumCovers(coverPrepList)
}


export async function loadAlbumCovers(coverPrepList: CoverPrep[]): Promise<void> {
  const withoutEarlierAttempts = await coverPrepList.reduce(async (
    acc: Promise<CoverPrep[]>,
    prep: CoverPrep
  ): Promise<CoverPrep[]> => {
    const arr = await acc
    const a = await albumCover(prep.cacheKey)
    if (!a) return [...arr, prep]
    return arr
  }, Promise.resolve([]))

  brain.postMessage({
    action: "DOWNLOAD_ARTWORK",
    data: withoutEarlierAttempts
  })
}


// Send a dictionary of the cached covers to the app.
async function cachedCovers(keys: string[]) {
  const cacheKeys = keys.filter(
    k => k.startsWith("coverCache.")
  )

  const cache = await cacheKeys.reduce(async (acc, key) => {
    const c = await acc
    const blob = await db().getItem(key)
    const cacheKey = key.slice(11)

    if (blob && typeof blob !== "string" && blob instanceof Blob) {
      c[cacheKey] = URL.createObjectURL(blob)
    }

    return c
  }, Promise.resolve({}))

  app.ports.insertCoverCache.send(cache)
  setTimeout(() => loadAlbumCoversFromDom({ list: true, coverView: true }), 500)
}
