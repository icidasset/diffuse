//
// Audio engine
// ♪(´ε｀ )

import type { App } from "./elm/types"

import Timer from "timer.js"
import { debounce } from "throttle-debounce"
import { CoverPrep, db, mimeType } from "../common"
import { albumCover, loadAlbumCovers } from "./artwork"
import { transformUrl } from "../urls"


// 🏔️


const silentMp3File =
  "data:audio/mp3;base64,SUQzBAAAAAAAI1RTU0UAAAAPAAADTGF2ZjU2LjM2LjEwMAAAAAAAAAAAAAAA//OEAAAAAAAAAAAAAAAAAAAAAAAASW5mbwAAAA8AAAAEAAABIADAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDV1dXV1dXV1dXV1dXV1dXV1dXV1dXV1dXV6urq6urq6urq6urq6urq6urq6urq6urq6v////////////////////////////////8AAAAATGF2YzU2LjQxAAAAAAAAAAAAAAAAJAAAAAAAAAAAASDs90hvAAAAAAAAAAAAAAAAAAAA//MUZAAAAAGkAAAAAAAAA0gAAAAATEFN//MUZAMAAAGkAAAAAAAAA0gAAAAARTMu//MUZAYAAAGkAAAAAAAAA0gAAAAAOTku//MUZAkAAAGkAAAAAAAAA0gAAAAANVVV"


let app: App
let container: Element | null = null
let scrobbleTimer: Timer | null = null



// 🚀


export function init(a: App) {
  app = a

  app.ports.adjustEqualizerSetting.subscribe(adjustEqualizerSetting)
  app.ports.pause.subscribe(pause)
  app.ports.pauseScrobbleTimer.subscribe(pauseScrobbleTimer)
  app.ports.play.subscribe(play)
  app.ports.reloadAudioNodeIfNeeded.subscribe(reloadAudioNodeIfNeeded)
  app.ports.renderAudioElements.subscribe(renderAudioElements)
  app.ports.resetScrobbleTimer.subscribe(resetScrobbleTimer)
  app.ports.seek.subscribe(seek)
  app.ports.setMediaSessionArtwork.subscribe(setMediaSessionArtwork)
  app.ports.setMediaSessionMetadata.subscribe(setMediaSessionMetadata)
  app.ports.setMediaSessionPlaybackState.subscribe(setMediaSessionPlaybackState)
  app.ports.setMediaSessionPositionState.subscribe(setMediaSessionPositionState)
  app.ports.startScrobbleTimer.subscribe(startScrobbleTimer)
}



// 🌳


/**
 * Javascript representation of `Queue.EngineItem` in Elm.
 */
type EngineItem = {
  isCached: boolean
  isPreload: boolean
  progress: number | null
  sourceId: string
  trackId: string
  trackTags: TrackTags
  trackPath: string
  url: string
}


/**/
type TrackTags = {
  disc: number
  nr: number

  // Main
  album: string | null
  artist: string | null
  title: string

  // Extra
  genre: string | null
  picture: string | null
  year: number | null
}



// Ports
// -----


function adjustEqualizerSetting({ knob, value }: { knob: string, value: number }): void {
  switch (knob) {
    case "VOLUME":
      Array.from(
        document.body.querySelectorAll('#audio-elements audio[data-is-preload="false"]'),
      ).forEach((audio) => ((audio as HTMLAudioElement).volume = value))
      break
  }
}


function pause({ trackId }: { trackId: string }) {
  withAudioNode(trackId, (audio) => audio.pause())
}


function pauseScrobbleTimer() {
  if (scrobbleTimer) scrobbleTimer.pause()
}


function play({ trackId, volume }: { trackId: string, volume: number }) {
  withAudioNode(trackId, (audio) => {
    audio.volume = volume
    audio.muted = false

    if (audio.readyState === 0) audio.load()
    if (!audio.isConnected) return

    const promise = audio.play() || Promise.resolve()
    const didPreload = audio.getAttribute("data-did-preload") === "true"
    const isPreload = audio.getAttribute("data-is-preload") === "true"

    if (didPreload && !isPreload) {
      audio.removeAttribute("data-did-preload")
      app.ports.audioDurationChange.send({
        trackId: audio.id,
        duration: audio.duration,
      })
    }

    promise.catch((e) => {
      if (!audio.isConnected) return /* The node was removed from the DOM, we can ignore this error */
      const err = "Couldn't play audio automatically. Please resume playback manually."
      console.error(err, e)
      if (app) app.ports.fromAlien.send({ tag: "", data: null, error: err })
    })
  })
}


async function reloadAudioNodeIfNeeded(args: { play: boolean, progress: number | null, trackId: string }) {
  withAudioNode(args.trackId, (audio) => {
    if (audio.readyState === 0 || audio.error?.code === 2) {
      audio.load()

      if (args.progress) {
        audio.setAttribute("data-initial-progress", JSON.stringify(args.progress))
      }

      if (args.play) {
        play({ trackId: args.trackId, volume: audio.volume })
      }
    }
  })
}


async function renderAudioElements(args: {
  items: Array<EngineItem>
  play: string | null
  volume: number
}) {
  await render(args.items)
  if (args.play) play({ trackId: args.play, volume: args.volume })
}


function resetScrobbleTimer({ duration, trackId }: { duration: number, trackId: string }) {
  const timestamp = Math.round(Date.now() / 1000)
  const scrobbleTimeoutDuration = Math.min(240 + 0.5, duration / 1.95)

  if (scrobbleTimer) scrobbleTimer.stop()

  scrobbleTimer = new Timer({
    onend: () => {
      scrobbleTimer = undefined
      app.ports.scrobble.send({
        duration: Math.round(duration),
        timestamp,
        trackId,
      })
    }
  })

  scrobbleTimer.start(scrobbleTimeoutDuration)
}


function seek({ percentage, trackId }: { percentage: number, trackId: string }) {
  withAudioNode(trackId, (audio) => {
    if (!isNaN(audio.duration)) {
      audio.currentTime = audio.duration * percentage
    }
  })
}


async function setMediaSessionArtwork({ blobUrl, imageType }: { blobUrl: string, imageType: string }) {
  const artwork: MediaImage[] = [{
    src: blobUrl,
    type: imageType
  }]

  navigator.mediaSession.metadata = new MediaMetadata({
    title: navigator.mediaSession.metadata?.title,
    artist: navigator.mediaSession.metadata?.artist,
    album: navigator.mediaSession.metadata?.album,
    artwork: artwork,
  })
}


async function setMediaSessionMetadata({
  album,
  artist,
  title,

  coverPrep,
}: {
  album: string | null
  artist: string | null
  title: string

  coverPrep: CoverPrep | null
}) {
  let artwork: MediaImage[] = []

  if (coverPrep) {
    const blob = await albumCover(coverPrep.cacheKey)

    artwork = blob && typeof blob !== "string"
      ? [{
        src: URL.createObjectURL(blob),
        type: blob.type
      }]
      : []

    if (!blob) {
      // Download artwork and set it later
      loadAlbumCovers([coverPrep])
    }
  }

  navigator.mediaSession.metadata = new MediaMetadata({
    title,
    artist: artist || undefined,
    album: album || undefined,
    artwork: artwork,
  })
}


function setMediaSessionPlaybackState(state: MediaSessionPlaybackState) {
  if (navigator.mediaSession) navigator.mediaSession.playbackState = state
}


function setMediaSessionPositionState({
  currentTime,
  duration,
}: {
  currentTime: number
  duration: number
}) {
  try {
    navigator?.mediaSession?.setPositionState({
      duration: duration,
      position: currentTime,
    })
  } catch (_err) {
    //
  }
}


function startScrobbleTimer() {
  if (scrobbleTimer) scrobbleTimer.start()
}



// Media Keys
// ----------


if ("mediaSession" in navigator) {
  navigator.mediaSession.setActionHandler("play", () => {
    app.ports.requestPlay.send(null)
  })

  navigator.mediaSession.setActionHandler("pause", () => {
    app.ports.requestPause.send(null)
  })

  navigator.mediaSession.setActionHandler("previoustrack", () => {
    app.ports.requestPrevious.send(null)
  })

  navigator.mediaSession.setActionHandler("nexttrack", () => {
    app.ports.requestNext.send(null)
  })

  navigator.mediaSession.setActionHandler("seekbackward", (event) => {
    const seekOffset = event.seekOffset || 10
    withActiveAudioNode(
      (audio) => (audio.currentTime = Math.max(audio.currentTime - seekOffset, 0)),
    )
  })

  navigator.mediaSession.setActionHandler("seekforward", (event) => {
    const seekOffset = event.seekOffset || 10
    withActiveAudioNode(
      (audio) => (audio.currentTime = Math.min(audio.currentTime + seekOffset, audio.duration)),
    )
  })

  navigator.mediaSession.setActionHandler("seekto", (event) => {
    withActiveAudioNode((audio) => (audio.currentTime = event.seekTime || audio.currentTime))
  })
}



// 🖼️


async function render(items: Array<EngineItem>) {
  if (!container) {
    container = document.createElement("div")
    container.id = "audio-elements"
    container.className = "absolute h-0 invisible left-0 pointer-events-none top-0 w-0"

    document.body.appendChild(container)
  }

  const trackIds = items.map((e) => e.trackId)
  const existingNodes = {}

  // Manage existing nodes
  Array.from(container.querySelectorAll("audio")).map((node: HTMLAudioElement) => {
    if (trackIds.includes(node.id)) {
      existingNodes[node.id] = node
    } else {
      node.src = silentMp3File
      container?.removeChild(node)
    }
  })

  // Adjust existing and add new
  await items.reduce(async (acc: Promise<void>, item: EngineItem) => {
    await acc

    const existingNode = existingNodes[item.trackId]

    if (existingNode) {
      const isPreload = existingNode.getAttribute("data-is-preload")
      if (isPreload === "true") existingNode.setAttribute("data-did-preload", "true")

      existingNode.setAttribute(
        "data-is-preload",
        item.isPreload ? "true" : "false",
      )
    } else {
      await createElement(item)
    }
  }, Promise.resolve())
}


export async function createElement(item: EngineItem) {
  const url = item.isCached
    ? await db("tracks")
        .getItem(item.trackId)
        .then((blob) => (blob ? URL.createObjectURL(blob as Blob) : item.url))
    : await transformUrl(item.url, app)

  // Mime + SRC
  const fileName = item.trackPath.split("/").reverse()[0]
  const fileExtMatch = fileName.match(/\.(\w+)$/)
  const fileExt = fileExtMatch && fileExtMatch[1]
  const mime = fileExt ? mimeType(fileExt) : null

  const source = document.createElement("source")
  if (mime) source.setAttribute("type", mime)
  source.setAttribute("src", url)

  // Audio node
  const audio = new Audio()
  audio.setAttribute("id", item.trackId)
  audio.setAttribute("crossorigin", "anonymous")
  audio.setAttribute("data-initial-progress", JSON.stringify(item.progress))
  audio.setAttribute("data-is-preload", item.isPreload ? "true" : "false")
  audio.setAttribute("muted", "true")
  audio.setAttribute("preload", "auto")
  audio.appendChild(source)

  audio.addEventListener("canplay", canplayEvent)
  audio.addEventListener("durationchange", durationchangeEvent)
  audio.addEventListener("ended", endedEvent)
  audio.addEventListener("error", errorEvent)
  audio.addEventListener("pause", pauseEvent)
  audio.addEventListener("play", playEvent)
  audio.addEventListener("suspend", suspendEvent)
  audio.addEventListener("timeupdate", timeupdateEvent)
  audio.addEventListener("waiting", debounce(1500, waitingEvent))

  container?.appendChild(audio)
}



// 🖼  ░░  EVENTS


function canplayEvent(event: Event) {
  const target = event.target as HTMLAudioElement

  if (target.hasAttribute("data-initial-progress") && target.duration && !isNaN(target.duration)) {
    const progress = JSON.parse(target.getAttribute("data-initial-progress") as string)
    target.currentTime = target.duration * progress
    target.removeAttribute("data-initial-progress")
  }

  finishedLoading(event)
}


function durationchangeEvent(event: Event) {
  const target = event.target as HTMLAudioElement

  if (!isNaN(target.duration)) {
    app.ports.audioDurationChange.send({
      trackId: target.id,
      duration: target.duration,
    })
  }
}

function endedEvent(event: Event) {
  app.ports.audioEnded.send({
    trackId: (event.target as HTMLAudioElement).id,
  })
}

function errorEvent(event: Event) {
  const audio = event.target as HTMLAudioElement

  app.ports.audioError.send({
    trackId: audio.id,
    code: audio.error?.code || 0
  })
}


function pauseEvent(event: Event) {
  app.ports.audioPlaybackStateChanged.send({
    trackId: (event.target as HTMLAudioElement).id,
    isPlaying: false,
  })
}


function playEvent(event: Event) {
  const audio = event.target as HTMLAudioElement

  app.ports.audioPlaybackStateChanged.send({
    trackId: audio.id,
    isPlaying: true,
  })

  // In case audio was preloaded:
  if (audio.readyState === 4) finishedLoading(event)
}


function suspendEvent(event: Event) {
  finishedLoading(event)
}


function timeupdateEvent(event: Event) {
  const target = event.target as HTMLAudioElement

  app.ports.audioTimeUpdated.send({
    trackId: target.id,
    currentTime: target.currentTime,
    duration: isNaN(target.duration) ? null : target.duration,
  })
}


function waitingEvent(event: Event) {
  initiateLoading(event)
}



// 🛠️


function finishedLoading(event: Event) {
  app.ports.audioHasLoaded.send({
    trackId: (event.target as HTMLAudioElement).id,
  })
}


function initiateLoading(event: Event) {
  const audio = event.target as HTMLAudioElement

  if (audio.readyState < 4)
    app.ports.audioIsLoading.send({
      trackId: audio.id,
    })
}


function withActiveAudioNode(fn: (node: HTMLAudioElement) => void): void {
  const nonPreloadNodes: HTMLAudioElement[] = Array.from(
    document.body.querySelectorAll(`#audio-elements audio[data-is-preload="false"]`),
  )
  const playingNodes = nonPreloadNodes.filter((n) => n.paused === false)
  const node = playingNodes.length ? playingNodes[0] : nonPreloadNodes[0]
  if (node) fn(node)
}


function withAudioNode(trackId: string, fn: (node: HTMLAudioElement) => void): void {
  const node = document.body.querySelector(
    `#audio-elements audio[id="${trackId}"][data-is-preload="false"]`,
  )
  if (node) fn(node as HTMLAudioElement)
}
