// @ts-ignore
import * as TaskPort from "elm-taskport"

import { fromCache, removeCache, toCache } from "./common"


export function register() {
  TaskPort.install()

  TaskPort.register("fromCache", fromCache)
  TaskPort.register("removeCache", removeCache)
  TaskPort.register("toCache", ({ key, value }) => toCache(key, value))
}
