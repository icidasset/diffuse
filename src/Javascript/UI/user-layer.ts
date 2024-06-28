import type { Program as OddProgram } from "@oddjs/odd"
import type { App } from "./elm/types.js"

import { ODD_CONFIG } from "../common"


// 🏔️


let app: App
let odd



// 🚀


export function init(a: App) {
  app = a

  app.ports.authenticateWithFission.subscribe(async () => {
    const program = await oddProgram()
    await program.capabilities.request({
      returnUrl: location.origin + "?action=authenticate/fission"
    })
  })

  app.ports.collectFissionCapabilities.subscribe(() => {
    // The ODD SDK should collect the capabilities for us,
    // if everything is valid, we'll receive a session.
    oddProgram().then(
      () => {
        history.replaceState({}, "", location.origin)
        app.ports.collectedFissionCapabilities.send(null)
      }
    ).catch(
      err => console.error(err)
    )
  })
}



// Fission ~ ODD
// -------------


async function oddProgram(): Promise<OddProgram> {
  try {
    await loadOdd()
  } catch (err) {
    console.trace(err)
    throw new Error("Failed to load the ODD SDK")
  }

  const capComponent = await import("../Odd/components/capabilities.js")

  const crypto = await odd.defaultCryptoComponent(ODD_CONFIG)
  const storage = await odd.defaultStorageComponent(ODD_CONFIG)
  const depot = await odd.defaultDepotComponent({ storage }, ODD_CONFIG)

  return odd.program({
    ...ODD_CONFIG,
    capabilities: capComponent.implementation({
      crypto,
      depot
    }),
    fileSystem: { loadImmediately: false }
  })
}


async function loadOdd() {
  if (odd) return
  odd = await import("@oddjs/odd")
}
