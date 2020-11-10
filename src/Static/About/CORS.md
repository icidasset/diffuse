> A music player that connects to your cloud &amp; distributed storage

[Return to the application](../../)  
[About](../)  



<div id="CORS" />

### CORS

There's only one thing you need to do yourself so that the service you chose will work with the application, and that's setting up [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) (Cross-Origin Resource Sharing). Here are the instructions you'll need for each service:

<div id="CORS__S3" />

#### Amazon S3

You can find the CORS configuration editor under the "Permissions" tab, on the S3 AWS Console.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CORSConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
<CORSRule>
    <AllowedOrigin>*</AllowedOrigin>
    <AllowedMethod>GET</AllowedMethod>
    <AllowedMethod>HEAD</AllowedMethod>
    <MaxAgeSeconds>31536000</MaxAgeSeconds>
    <ExposeHeader>Content-Length</ExposeHeader>
    <ExposeHeader>Content-Type</ExposeHeader>
    <AllowedHeader>Range</AllowedHeader>
</CORSRule>
</CORSConfiguration>
```

<div id="CORS__BTFS" />

#### BTFS

Add the domain of the app, with the protocol, to the __list of allowed origins__.  

```shell
btfs config --json API.HTTPHeaders.Access-Control-Allow-Origin '["https://diffuse.sh", "http://diffuse.sh.ipns.localhost:8080", "http://127.0.0.1:44999"]'
```

You can also make this change in the Web UI, you'll find it under "Settings → BTFS Config".

```javascript
{
  "API": {
    "HTTPHeaders": {
      "Access-Control-Allow-Origin": [
        "https://diffuse.sh",                       // 🎵 Default
        "http://diffuse.sh.ipns.localhost:8080",    // IPNS
        "http://127.0.0.1:44999"                    // Electron app
      ]
    }
  }
}
```

<div id="CORS__Dropbox" />

#### Dropbox

_Not necessary._

<div id="CORS__Google-Drive" />

#### Google Drive

_Not necessary._

<div id="CORS__IPFS" />

#### IPFS

Add the domain of the app, with the protocol, to the __list of allowed origins__.  

```shell
ipfs config --json API.HTTPHeaders.Access-Control-Allow-Origin '["https://diffuse.sh", "http://diffuse.sh.ipns.localhost:8080", "http://127.0.0.1:44999"]'
```

You can also make this change in the Web UI, you'll find it under "Settings → IPFS Config".

```javascript
{
  "API": {
    "HTTPHeaders": {
      "Access-Control-Allow-Origin": [
        "https://diffuse.sh",                       // 🎵 Default
        "http://diffuse.sh.ipns.localhost:8080",    // IPNS through IPFS Companion
        "http://127.0.0.1:44999"                    // Electron app
      ]
    }
  }
}
```

<div id="CORS__Azure" />

#### Microsoft Azure Storage

You can find the CORS configuration under the "Settings -> CORS".  
Then fill in the following in the input boxes (left to right):

```
ALLOWED ORIGINS       *
ALLOWED METHODS       GET, HEAD
ALLOWED HEADERS       Range
EXPOSED HEADERS       Content-Length, Content-Range
MAX AGE               0
```

<div id="CORS__WebDAV" />

#### WebDAV

__Depends on your WebDAV server.__  
Example setup for Henrique Dias's [WebDAV server](https://github.com/hacdias/webdav):

```yaml
cors:
  enabled: true
  credentials: true

  allowed_headers:
    - Authorization
    - Content-Type
    - Depth
    - Range
  allowed_methods:
    - GET
    - HEAD
    - PROPFIND
  allowed_hosts:
    - https://diffuse.sh
    - http://127.0.0.1:44999
  exposed_headers:
    - Content-Length
    - Content-Type
```
