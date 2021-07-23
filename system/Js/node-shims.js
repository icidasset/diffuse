import BufferPolyfill from "buffer/"

export let Buffer = BufferPolyfill.Buffer
export let global = globalThis
export let process = { env: {} }
