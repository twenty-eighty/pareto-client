module Pages.Media exposing (Model, Msg, page)

import Auth
import Components.MediaSelector as MediaSelector
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Event exposing (Kind(..), Tag(..))
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (LoginMethod(..), LoginStatus(..))
import Tailwind.Utilities as Tw
import Translations.MediaPage as Translations
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
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { mediaSelector : MediaSelector.Model
    }


init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared _ () =
    let
        (blossomServers, nip96Servers )  =
            case shared.loginStatus of
                LoggedIn _ LoginMethodReadOnly ->
                    -- In order to list files via Blossom or NIP-96
                    -- a Nostr event has to be signed. This is impossible
                    -- without a private key.
                    ([], [])

                LoggedIn _ _ ->
                    ( Nostr.getBlossomServers shared.nostr user.pubKey
                    , Nostr.getNip96Servers shared.nostr user.pubKey
                    )

                _ ->
                    ([], [])

        ( mediaSelector, mediaSelectorEffect ) =
            MediaSelector.init
                { selected = Nothing
                , toMsg = MediaSelectorSent
                , blossomServers = blossomServers
                , nip96Servers = nip96Servers
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
                { pubKey = user.pubKey
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
view _ shared model =
    { title = "Media"
    , body =
        [ div
            [ css
                [ Tw.m_10
                ]
            ]
            [ viewMediaSelector shared model
            ]
        ]
    }

viewMediaSelector : Shared.Model -> Model -> Html Msg
viewMediaSelector shared model =
    case shared.loginStatus of
        LoggedIn _ LoginMethodReadOnly ->
            viewAlternativeMessage <| Translations.loggedInReadOnlyMessage [ shared.browserEnv.translations ]

        LoggedIn pubKey _ ->
            MediaSelector.new
                { model = model.mediaSelector
                , toMsg = MediaSelectorSent
                , onSelected = Nothing
                , pubKey = pubKey
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> MediaSelector.view

        _ ->
            viewAlternativeMessage <| Translations.notLoggedInMessage [ shared.browserEnv.translations ]

viewAlternativeMessage : String -> Html Msg
viewAlternativeMessage message =
            div []
                [ Html.text message
                ]