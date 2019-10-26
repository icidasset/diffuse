//
// Textile
// \ (•◡•) /
//
// Communicating with the Textile HTTP API.


const TEXTILE_THREAD = "Diffuse"
const TEXTILE_SCHEMA = {
  name: TEXTILE_THREAD,
  pin: true,
  mill: "/blob",
  plaintext: false
}


const Textile = {
  activeThreadId: undefined
}



// Blocks
// ------

Textile.deleteBlock = (apiOrigin, file) => {
  return fetch(apiOrigin + "/api/v0/blocks/" + file.block, {
    method: "DELETE"
  })
    .then(r => r.json())
}



// Threads
// -------

Textile.ensureThread = (apiOrigin) => {
  if (!Textile.activeThreadId) {
    return Textile.getThread(apiOrigin)
      .then(thread => {
        if (thread) return thread

        return Textile
          .createSchema(apiOrigin)
          .then(schema => Textile.createThread(apiOrigin, schema))
      })
      .then(t => {
        Textile.activeThreadId = t.id
        return t.id
      })
  }

  return Promise.resolve(Textile.activeThreadId)
}


Textile.getThread = (apiOrigin) => {
  return fetch(apiOrigin + "/api/v0/threads")
    .then(r => r.json())
    .then(r => r.items.find(i => i.name === TEXTILE_THREAD))
}


Textile.createThread = (apiOrigin, schema) => {
  const headers = new Headers()
  headers.append("X-Textile-Args", TEXTILE_THREAD)
  headers.append("X-Textile-Opts", "schema=" + schema.hash + ",type=private,sharing=invite_only")

  return fetch(apiOrigin + "/api/v0/threads", {
    headers: headers,
    method: "POST"
  })
    .then(r => r.json())
}



// Files
// -----

Textile.addFileToThread = (apiOrigin, millResult) => {
  const headers = new Headers()
  headers.append("Content-Type", "application/json")

  const files = {}
  files[":single"] = millResult

  return fetch(apiOrigin + "/api/v0/threads/" + Textile.activeThreadId + "/files", {
    body: JSON.stringify({
      items: [{ files: files }]
    }),
    headers: headers,
    method: "POST"
  })
    .then(r => r.json())
}


Textile.getFile = (apiOrigin, file_name) => {
  const headers = new Headers()
  headers.append("X-Textile-Opts", "thread=" + Textile.activeThreadId)

  return fetch(apiOrigin + "/api/v0/files", {
    headers: headers,
    method: "GET"
  })
    .then(r => r.json())
    .then(r => r.items.find(i => i.files[0].file.name === file_name))
}


Textile.readFile = (apiOrigin, file) => {
  const headers = new Headers()
  headers.append("X-Textile-Opts", "key=" + file.files[0].file.key)

  return fetch(apiOrigin + "/api/v0/ipfs/cat/" + file.files[0].file.hash, {
    headers: headers,
    method: "GET"
  })
    .then(r => r.text())
}



// Mills
// -----

Textile.createSchema = (apiOrigin) => {
  const headers = new Headers()
  headers.append("Content-Type", "application/json")

  return fetch(apiOrigin + "/api/v0/mills/schema", {
    body: JSON.stringify(TEXTILE_SCHEMA),
    headers: headers,
    method: "POST"
  })
    .then(r => r.json())
}


Textile.useMill = (apiOrigin, file_name, data) => {
  const blob = new Blob([data], { type : "application/json" })
  const file = new File([blob], file_name)

  const formData = new FormData()
  formData.append("file", file)

  const headers = new Headers()
  headers.append("X-Textile-Opts", "plaintext=false")

  return fetch(apiOrigin + "/api/v0/mills/blob", {
    body: formData,
    method: "POST"
  })
    .then(r => r.json())
}



// Export
// ------

export default Textile
