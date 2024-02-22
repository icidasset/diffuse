export type App = any // TODO: ElmApp<ElmPorts>


export type ElmPorts = {
  // ← Elm
  openUrlOnNewPage: PortFromElm<string>

  // → Elm
  fromAlien: PortToElm<unknown>
  indicateTouchDevice: PortToElm<void>
}
