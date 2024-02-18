//
// Audio engine
// ♪(´ε｀ )

import { debounce } from "throttle-debounce"
import { db, mimeType } from "../common"


// 🏔️


let app: any // TODO
let container: Element | null = null


export function init(a: any) { // TODO
  app = a
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

export function adjustEqualizerSetting({ knob, value }: { knob: string; value: number }): void {
  switch (knob) {
    case "VOLUME":
      Array.from(
        document.body.querySelectorAll("#audio-elements audio[data-is-preload=\"false\"]")
      ).forEach(
        audio => (audio as HTMLAudioElement).volume = value
      )
      break;
  }
}

export function pause({ trackId }: { trackId: string }) {
  withAudioNode(trackId, audio => audio.pause())
}

export function play({ trackId, volume }: { trackId: string; volume: number }) {
  console.log("play", trackId)
  withAudioNode(trackId, audio => {
    audio.volume = volume
    audio.muted = false

    const promise = audio.play() || Promise.resolve()

    promise.catch(e => {
      const err = "Couldn't play audio automatically. Please resume playback manually."
      console.error(err, e)
      if (app) app.ports.fromAlien.send({ tag: "", data: null, error: err })
    })
  })
}

export async function renderAudioElements(args: {
  items: Array<EngineItem>
  play: string | null
  volume: number
}) {
  await render(args.items)
  if (args.play) play({ trackId: args.play, volume: args.volume })
}

export function seek({ percentage, trackId }: { percentage: number; trackId: string }) {
  withAudioNode(trackId, audio => {
    if (!isNaN(audio.duration)) {
      audio.currentTime = audio.duration * percentage
    }
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

  const trackIds = items.map(e => e.trackId)
  const existingNodes = {}

  // Manage existing nodes
  Array.from(container.querySelectorAll("audio")).map((node: Element) => {
    if (trackIds.includes(node.id)) {
      existingNodes[node.id] = node
    } else {
      container?.removeChild(node)
    }
  })

  // Adjust existing and add new
  await items.reduce(async (acc: Promise<void>, item: EngineItem) => {
    await acc

    if (existingNodes[item.trackId]) {
      existingNodes[item.trackId].setAttribute(
        "data-is-preload",
        item.isPreload ? "true" : "false"
      )

    } else {
      await createElement(item)

    }
  }, Promise.resolve())
}


export async function createElement(item: EngineItem) {
  const url = item.isCached
    ? await db("tracks").getItem(item.trackId).then(blob => blob ? URL.createObjectURL(blob as Blob) : item.url)
    : item.url

  // Mime + SRC
  const fileName = item.trackPath.split("/").reverse()[ 0 ]
  const fileExtMatch = fileName.match(/\.(\w+)$/)
  const fileExt = fileExtMatch && fileExtMatch[ 1 ]
  const mime = fileExt ? mimeType(fileExt) : null

  const source = document.createElement("source")
  if (mime) source.setAttribute("type", mime)
  source.setAttribute("src", url)

  // Audio node
  const audio = new Audio()
  audio.setAttribute("id", item.trackId)
  audio.setAttribute("crossorigin", "anonymous")
  audio.setAttribute("data-is-preload", item.isPreload ? "true" : "false")
  audio.setAttribute("muted", "true")
  audio.setAttribute("preload", "auto")
  audio.appendChild(source)

  audio.addEventListener("canplay", canplayEvent)
  audio.addEventListener("ended", endedEvent)
  audio.addEventListener("error", errorEvent)
  audio.addEventListener("loadstart", loadstartEvent)
  audio.addEventListener("loadeddata", loadeddataEvent)
  audio.addEventListener("pause", pauseEvent)
  audio.addEventListener("play", playEvent)
  audio.addEventListener("seeking", seekingEvent)
  audio.addEventListener("seeked", seekedEvent)
  audio.addEventListener("timeupdate", timeupdateEvent)

  container?.appendChild(audio)
}



// 🖼  ░░  EVENTS


function canplayEvent(event: Event) {
  const target = event.target as HTMLAudioElement
  if (!isNaN(target.duration)) app.ports.audioCanPlay.send({
    trackId: target.id,
    duration: target.duration
  })
}

function endedEvent(event: Event) {
  app.ports.audioEnded.send({
    trackId: (event.target as HTMLAudioElement).id
  })
}

function errorEvent() {}

function loadstartEvent(event: Event) {
  initiateLoading(event)
}

function loadeddataEvent(event: Event) {
  finishedLoading(event)
}

function pauseEvent(event: Event) {
  app.ports.audioPlaybackStateChanged.send({
    trackId: (event.target as HTMLAudioElement).id,
    isPlaying: false
  })
}

function playEvent(event: Event) {
  app.ports.audioPlaybackStateChanged.send({
    trackId: (event.target as HTMLAudioElement).id,
    isPlaying: true
  })
}

function seekingEvent(event: Event) {
  initiateLoading(event)
}

function seekedEvent(event: Event) {
  finishedLoading(event)
}

function timeupdateEvent(event: Event) {
  const target = event.target as HTMLAudioElement

  app.ports.audioTimeUpdated.send({
    trackId: target.id,
    currentTime : target.currentTime,
    duration : target.duration
  })
}



// 🛠️


function finishedLoading(event: Event) {
  app.ports.audioHasLoaded.send({
    trackId: (event.target as HTMLAudioElement).id
  })
}


const initiateLoading = debounce(1500, (event: Event) => {
  app.ports.audioIsLoading.send({
    trackId: (event.target as HTMLAudioElement).id
  })
})


function withAudioNode(trackId: string, fn: (node: HTMLAudioElement) => void): void {
  const node = document.body.querySelector(`#audio-elements audio[id="${trackId}"][data-is-preload="false"]`);
  if (node) fn(node as HTMLAudioElement)
}
