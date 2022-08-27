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
      const [ accessToken, expiresAtString, refreshToken, clientId, clientSecret ] = googleBits[ 0 ].split(":")
      const fileId = googleBits[ 1 ]

      // Unix timestamp in milliseconds
      const in15minutes = Date.now() + 15 * 60 * 1000
      const expiresAt = parseInt(expiresAtString, 10)
      const isAlmostExpired = expiresAt <= in15minutes

      if (EXPIRED_ACCESS_TOKENS.GOOGLE[ accessToken ]) {
        const replacement = EXPIRED_ACCESS_TOKENS.GOOGLE[ accessToken ]

        if (replacement.newExpiresAt <= in15minutes) {
          finalAccessToken = await refreshGoogleAccessToken({
            clientId, clientSecret, refreshToken, oldToken: accessToken
          })
        } else {
          finalAccessToken = replacement.newToken
        }

      } else if (isAlmostExpired) {
        finalAccessToken = await refreshGoogleAccessToken({
          clientId, clientSecret, refreshToken, oldToken: accessToken
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


async function refreshGoogleAccessToken({ clientId, clientSecret, oldToken, refreshToken }) {
  console.log("🔐 Refreshing Google Drive access token")

  const url = new URL("https://www.googleapis.com/oauth2/v4/token")

  url.searchParams.set("client_id", clientId)
  url.searchParams.set("client_secret", clientSecret)
  url.searchParams.set("refresh_token", refreshToken)
  url.searchParams.set("grant_type", "refresh_token")

  const serverResponse = await fetch(url, { method: "POST" }).then(r => r.json())

  EXPIRED_ACCESS_TOKENS.GOOGLE[ oldToken ] = {
    oldToken,
    newToken: serverResponse.access_token,
    newExpiresAt: Date.now() + (serverResponse.expires_in * 1000),
    refreshToken
  }

  return serverResponse.access_token
}
