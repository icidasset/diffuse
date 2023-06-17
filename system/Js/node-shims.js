import BufferPolyfill from "buffer/"

export let Buffer = BufferPolyfill.Buffer
export let global = globalThis
export let process = { env: { NODE_DEBUG: false } }
export let localStorage = globalThis.localStorage || {
  getItem: () => null,
  setItem: () => null,
  removeItem: () => null
}
