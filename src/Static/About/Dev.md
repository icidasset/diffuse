> A music player that connects to your cloud &amp; distributed storage

[Return to the application](../../)  
[About](../)  



## Adding Sources Using Query Parameter

```js
JSON = encodeURIComponent(JSON.stringify({
  // Object contents depends on type of source, see below.
  kind: "ipfs",
  data: {
    name: "Music from IPFS",

    // Source type specific
    directoryHash: "Qm..."
  }
}))

"https://diffuse.sh?source=JSON"
```

You can add multiple "source" query parameters, if you want to add multiple sources.

### Amazon S3

```shell
# Required
accessKey
bucketName
region
secretKey

# Optional
directoryPath
host
```

### Azure

```shell
# Required
accountName
accountKey
container

# Optional
directoryPath
```

### BTFS

```shell
# Required
directoryHash

# Optional
gateway
```

### Dropbox

```shell
# Required
accessToken
appKey

# Optional
directoryPath
```

### Google

```shell
# Required
authCode
clientId
clientSecret

# Optional
folderId
```

### IPFS

```shell
# Required
directoryHash

# Optional
gateway
ipns              ← boolean, `t` of `f`
local             ← boolean, `t` of `f`
```

### WebDAV

```shell
# Required
url

# Optional
directoryPath
password
username
```
