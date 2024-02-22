export function channel() {
  const bc = new BroadcastChannel(`diffuse-${location.hostname}`)

  bc.addEventListener("message", event => {
    switch (event.data) {
      case "PING": return bc.postMessage("PONG")
    }
  })

  return bc
}
