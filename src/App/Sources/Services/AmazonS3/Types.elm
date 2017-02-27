module Sources.Services.AmazonS3.Types exposing (..)

import Http


type alias AmazonS3Source =
    { accessKey : String
    , bucketName : String
    , directoryPath : String
    , name : String
    , region : String
    , secretKey : String
    }


type alias ParsedResponse =
    { filePaths : List String
    , marker : String
    }
