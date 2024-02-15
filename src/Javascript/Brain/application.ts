import "./index.d"

// @ts-ignore
import { Elm } from "brain.elm.js"


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
