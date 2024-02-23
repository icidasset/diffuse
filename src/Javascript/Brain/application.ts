import "./index.d"
import type { App } from "./elm/types"

// @ts-ignore
import { Elm } from "brain.elm.js"


// 🏔️


let app: App



// 🚀


const flags: Record<string, string> = location
  .hash
  .substring(1)
  .split("&")
  .reduce((acc, flag) => {
    const [k, v] = flag.split("=")
    return { ...acc, [k]: v }
  }, {})


export const load = () => Elm.Brain.init({
  flags: {
    initialUrl: decodeURIComponent(flags.appHref) || ""
  }
})
