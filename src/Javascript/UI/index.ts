//
// | (• ◡•)| (❍ᴥ❍ʋ)
//
// The bit where we launch the Elm apps & workers,
// and connect the other bits to it.

import "./pointer-events"

import * as Application from "./application"
import * as Artwork from "./artwork"
import * as Audio from "./audio"
import * as Backdrop from "./backdrop"
import * as Brain from "./brain"
import * as Broadcast from "./broadcast"
import * as Errors from "./errors"
import * as Misc from "./misc"
import * as ServiceWorker from "./service-worker"
import * as Tracks from "./tracks"



// 🌸


const isNativeWrapper = !!globalThis.__TAURI__



// 🚀


ServiceWorker
  .load({ isNativeWrapper })
  .then(async (reg: ServiceWorkerRegistration) => {
    const brain = await Brain.load()
    const app = Application.load({ isNativeWrapper, reg })
    const channel = Broadcast.channel()

    // 🧑‍🏭
    ServiceWorker.link({
      app, isNativeWrapper, reg
    })

    // 🧠
    Brain.link({
      app, brain
    })

    // ⚡
    Application.init(app, channel)
    Artwork.init(app, brain)
    Audio.init(app)
    Backdrop.init(app)
    Misc.init(app)
    Tracks.init(app)
  })
  .catch(
    Errors.failure
  )
