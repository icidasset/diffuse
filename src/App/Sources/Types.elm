module Sources.Types exposing (..)


type Source
    = Aws AwsSource


type alias AwsSource =
    { accessKey : String
    , bucketName : String
    , directoryPath : String
    , region : String
    , secretKey : String
    }


type Page
    = Index
    | New
