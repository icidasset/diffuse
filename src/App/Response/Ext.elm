module Response.Ext exposing (..)


andAlso : (model -> Cmd msg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andAlso fnForCmd ( model, cmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , fnForCmd model
        ]
    )


withAlso : (model -> Cmd msg) -> model -> ( model, Cmd msg )
withAlso fnForCmd model =
    ( model
    , fnForCmd model
    )
