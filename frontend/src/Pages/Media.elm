module Pages.Media exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.MediaSelector as MediaSelector
import Css
import Dict exposing (Dict)
import Effect exposing (Effect)
import Hex
import Html.Styled as Html exposing (Html, a, aside, button, code, div, h2, h3, h4, img, input, label, node, p, span, text, textarea)
import Html.Styled.Attributes as Attr exposing (class, css, style)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode
import Layouts
import MilkdownEditor as Milkdown
import Murmur3
import Nostr
import Nostr.Event as Event exposing (Event, Kind(..), Tag(..))
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Time
import Translations.Write
import Ui.Styles exposing (Theme)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared route
        , update = update shared user
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout shared.theme)

toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


-- INIT


type alias Model =
    { mediaSelector : MediaSelector.Model
    }

init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared route () =
    let
        (mediaSelector, mediaSelectorEffect) =
            MediaSelector.init
                { selected = Nothing
                , toMsg = MediaSelectorSent
                , blossomServers = Nostr.getBlossomServers shared.nostr user.pubKey
                , nip96Servers = Nostr.getNip96Servers shared.nostr user.pubKey
                , displayType = MediaSelector.DisplayEmbedded
                }

    in
    ( { mediaSelector = mediaSelector }, mediaSelectorEffect )


-- UPDATE


type Msg
    = MediaSelectorSent (MediaSelector.Msg Msg)


update : Shared.Model -> Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update shared user msg model =
    case msg of
        MediaSelectorSent innerMsg ->
            MediaSelector.update
                { user = user
                , msg = innerMsg
                , model = model.mediaSelector
                , toModel = \mediaSelector -> { model | mediaSelector = mediaSelector }
                , toMsg = MediaSelectorSent
                , nostr = shared.nostr
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map MediaSelectorSent (MediaSelector.subscribe model.mediaSelector)


-- VIEW


view : Auth.User -> Shared.Model -> Model -> View Msg
view user shared model =
    { title = "Media"
    , body =
        [ MediaSelector.new
            { model = model.mediaSelector
            , toMsg = MediaSelectorSent
            , pubKey = user.pubKey
            , browserEnv = shared.browserEnv
            , theme = shared.theme
            }
            |> MediaSelector.view
        ]
    }
