//
// URLs
// \ (•◡•) /
//
// Some URLs are special you know.

function transformUrl(url) {
  const parts = url.split("://")

  switch (parts[0]) {

    case "dropbox":
      const dropboxBits = parts[1].split("@")
      const accessToken = dropboxBits[0]
      const filePath = dropboxBits[1]

      return fetch(
        "https://api.dropboxapi.com/2/files/get_temporary_link",
        { method: "POST"
        , body: JSON.stringify({ path: filePath })
        , headers: new Headers({
            "Authorization": "Bearer " + accessToken,
            "Content-Type": "application/json"
          })
        }
      ).then(
        response => response.json()
      ).then(
        response => response.link
      )

    default:
      return Promise.resolve(url)

  }
}
