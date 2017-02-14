module Sources.Types exposing (..)


type Source
    = Aws AwsSource


type alias AwsSource =
    { accessKey : String
    , bucketName : String
    , region : String
    , secretKey : String
    }
