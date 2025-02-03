module Pages.Media exposing (Model, Msg, page)

import Auth
import Components.MediaSelector as MediaSelector
import Effect exposing (Effect)
import Html.Styled as Html exposing (div)
import Html.Styled.Attributes exposing (css)
import Layouts
import Nostr
import Nostr.Event exposing (Kind(..), Tag(..))
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Tailwind.Utilities as Tw
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
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { mediaSelector : MediaSelector.Model
    }


init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared _ () =
    let
        ( mediaSelector, mediaSelectorEffect ) =
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
                , browserEnv = shared.browserEnv
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
        [ div
            [ css
                [ Tw.m_10
                ]
            ]
            [ MediaSelector.new
                { model = model.mediaSelector
                , toMsg = MediaSelectorSent
                , onSelected = Nothing
                , pubKey = user.pubKey
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> MediaSelector.view
            ]
        ]
    }
