module UI.Routing.State exposing (linkClicked, openUrlOnNewPage, resetUrl, transition, urlChanged)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import List.Extra as List
import Monocle.Lens as Lens
import Return exposing (return)
import Sources
import Sources.Services.Dropbox
import Sources.Services.Google
import UI.Common.State as Common
import UI.Page as Page exposing (Page)
import UI.Ports as Ports
import UI.Sources.Form
import UI.Sources.Page
import UI.Sources.State as Sources
import UI.Sources.Types as Sources
import UI.Types as UI exposing (Manager)
import Url exposing (Url)



-- 🔱


linkClicked : UrlRequest -> Manager
linkClicked urlRequest model =
    case urlRequest of
        Browser.Internal urlWithFragment ->
            let
                url =
                    if urlWithFragment.fragment == Just "/" then
                        { urlWithFragment | fragment = Nothing }

                    else
                        urlWithFragment
            in
            if url.path /= model.url.path then
                return model (Nav.load url.path)

            else
                return model (Nav.pushUrl model.navKey <| Url.toString url)

        Browser.External href ->
            return model (Nav.load href)


openUrlOnNewPage : String -> Manager
openUrlOnNewPage url model =
    url
        |> Ports.openUrlOnNewPage
        |> return model


urlChanged : Url -> Manager
urlChanged url model =
    let
        rewrittenUrl =
            Page.rewriteUrl { url | query = Nothing }
    in
    case ( url.query, Page.fromUrl rewrittenUrl ) of
        ( Nothing, Just page ) ->
            transition page { model | page = page, url = url }

        ( Just _, Just page ) ->
            return model (resetUrl model.navKey url page)

        _ ->
            return model (resetUrl model.navKey url Page.Index)



-- TRANSITIONING


transition : Page -> Manager
transition page model =
    case page of
        -----------------------------------------
        -- Sources.NewThroughRedirect
        -----------------------------------------
        Page.Sources (UI.Sources.Page.NewThroughRedirect service args) ->
            let
                ( form, defaultContext ) =
                    ( model.sourceForm
                    , UI.Sources.Form.defaultContext
                    )
            in
            { defaultContext
                | data =
                    case service of
                        Sources.Dropbox ->
                            Sources.Services.Dropbox.authorizationSourceData args

                        Sources.Google ->
                            Sources.Services.Google.authorizationSourceData args

                        _ ->
                            defaultContext.data
                , service =
                    service
            }
                |> (\c -> { form | context = c, step = Sources.By })
                |> (\f -> { model | sourceForm = f })
                |> Return.singleton

        -----------------------------------------
        -- Sources.Edit
        -----------------------------------------
        Page.Sources (UI.Sources.Page.Edit sourceId) ->
            loadSourceForForm sourceId model

        -----------------------------------------
        -- Sources.Rename
        -----------------------------------------
        Page.Sources (UI.Sources.Page.Rename sourceId) ->
            loadSourceForForm sourceId model

        -----------------------------------------
        -- 📭
        -----------------------------------------
        _ ->
            Return.singleton model



-- 🚀


resetUrl : Nav.Key -> Url -> Page.Page -> Cmd UI.Msg
resetUrl key url page =
    Nav.replaceUrl key (url.path ++ Page.toString page)



-- ㊙️


loadSourceForForm : String -> Manager
loadSourceForForm sourceId model =
    let
        isLoading =
            model.isLoading

        maybeSource =
            List.find (.id >> (==) sourceId) model.sources
    in
    case ( isLoading, maybeSource ) of
        ( False, Just source ) ->
            model
                |> Lens.modify Sources.formLens (\m -> { m | context = source })
                |> Return.singleton

        ( False, Nothing ) ->
            Return.singleton model

        ( True, _ ) ->
            -- Redirect away from edit-source page
            Common.changeUrlUsingPage
                (Page.Sources UI.Sources.Page.Index)
                model
