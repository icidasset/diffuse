//
// URLs
// \ (•◡•) /
//
// Some URLs are special you know.


const EXPIRED_ACCESS_TOKENS = {
  GOOGLE: {}
}


export async function transformUrl(url, app) {
  const parts = url.split("://")

  switch (parts[ 0 ]) {

    case "dropbox": {
      const dropboxBits = parts[ 1 ].split("@")
      const accessToken = dropboxBits[ 0 ]
      const filePath = dropboxBits[ 1 ]

      return fetch(
        "https://api.dropboxapi.com/2/files/get_temporary_link",
        {
          method: "POST"
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
    }

    case "google": {
      let finalAccessToken

      const googleBits = parts[ 1 ].split("@")
      const [ accessToken, expiresAtString, refreshToken, clientId, clientSecret, srcId ] = googleBits[ 0 ].split(":")
      const fileId = googleBits[ 1 ]

      // Unix timestamp in milliseconds
      const inXminutes = Date.now() + 5 * 60 * 1000 // 5 minutes
      const expiresAt = parseInt(expiresAtString, 10)
      const isAlmostExpired = expiresAt <= inXminutes

      if (EXPIRED_ACCESS_TOKENS.GOOGLE[ accessToken ]) {
        const replacement = EXPIRED_ACCESS_TOKENS.GOOGLE[ accessToken ]

        if (replacement.newExpiresAt <= inXminutes) {
          finalAccessToken = await refreshGoogleAccessToken({
            app, clientId, clientSecret, refreshToken, srcId, oldToken: accessToken
          })
        } else {
          finalAccessToken = replacement.newToken
        }

      } else if (isAlmostExpired) {
        finalAccessToken = await refreshGoogleAccessToken({
          app, clientId, clientSecret, refreshToken, srcId, oldToken: accessToken
        })

      } else {
        finalAccessToken = accessToken

      }

      return Promise.resolve(
        `https://www.googleapis.com/drive/v3/files/${encodeURIComponent(fileId)}?alt=media&bearer_token=${encodeURIComponent(finalAccessToken)}`
      )
    }

    default:
      return Promise.resolve(url)

  }
}



// GOOGLE


async function refreshGoogleAccessToken({ app, clientId, clientSecret, oldToken, refreshToken, srcId }) {
  console.log("🔐 Refreshing Google Drive access token")

  const url = new URL("https://www.googleapis.com/oauth2/v4/token")

  url.searchParams.set("client_id", clientId)
  url.searchParams.set("client_secret", clientSecret)
  url.searchParams.set("refresh_token", refreshToken)
  url.searchParams.set("grant_type", "refresh_token")

  const serverResponse = await fetch(url, { method: "POST" }).then(r => r.json())
  const newToken = serverResponse.access_token
  const newExpiresAt = Date.now() + (serverResponse.expires_in * 1000)

  EXPIRED_ACCESS_TOKENS.GOOGLE[ oldToken ] = {
    oldToken,
    newToken,
    newExpiresAt,
    refreshToken
  }

  app.ports.refreshedAccessToken.send({
    service: "Google",
    sourceId: srcId,
    accessToken: newToken,
    expiresAt: newExpiresAt
  })

  return serverResponse.access_token
}
