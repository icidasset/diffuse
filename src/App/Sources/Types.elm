module Sources.Types
    exposing
        ( Source(..)
        , AmazonS3Source
        , Model
        , Msg(..)
        , Page(..)
        )

-- Sources


type Source
    = AmazonS3 AmazonS3Source


type alias GenericSourceAttributes =
    { name : String }



-- Services


{-| Amazon S3
-}
type alias AmazonS3Source =
    AmazonS3Properties GenericSourceAttributes


type alias AmazonS3Properties a =
    { a
        | accessKey : String
        , bucketName : String
        , directoryPath : String
        , region : String
        , secretKey : String
    }



-- Other types


type alias Model =
    { newSource : Source
    }


type Msg
    = SetNewSource Source


type Page
    = Index
    | New
