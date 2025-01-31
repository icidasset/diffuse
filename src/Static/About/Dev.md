> A music player that connects to your cloud & distributed storage

[Return to the application](../../)<br />
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

```yaml
kind:
  amazon_s3

data:
  # Required
  accessKey
  bucketName
  name
  region
  secretKey

  # Optional
  directoryPath
  host
```

### Azure

```yaml
kind:
  "azure_file" # or "azure_blob"

data:
  # Required
  accountName
  accountKey
  container
  name

  # Optional
  directoryPath
```

### Dropbox

```yaml
kind:
  dropbox

data:
  # Required
  accessToken
  appKey
  name

  # Optional
  directoryPath
```

### IPFS

```yaml
kind:
  ipfs

data:
  # Required
  directoryHash
  name

  # Optional
  gateway
  ipns              ← boolean, `t` of `f`
  local             ← boolean, `t` of `f`
```

### WebDAV

```yaml
kind:
  webdav

data:
  # Required
  name
  url

  # Optional
  directoryPath
  password
  username
```
