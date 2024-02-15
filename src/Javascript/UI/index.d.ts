export { }

declare global {
  const BUILD_TIMESTAMP: string

  const Elm: { UI: ElmMain<ElmPorts> }
  const tocca: any
}

// ELM
// ---

export type ElmPorts = {
  // ← Elm
  openUrlOnNewPage: PortFromElm<string>

  // → Elm
  fromAlien: PortToElm<unknown>
  indicateTouchDevice: PortToElm<void>
}
