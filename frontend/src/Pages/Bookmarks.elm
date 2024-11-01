module Pages.Bookmarks exposing (Model, Msg, page)

import Auth
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h1, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Effect exposing (Effect)
import Layouts
import Nostr exposing (getBookmarks)
import Nostr.Event exposing (Kind(..))
import Nostr.Request exposing (RequestData(..))
import Route exposing (Route)
import Page exposing (Page)
import Ports
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.Bookmarks as Translations
import Ui.Shared exposing (fontFamilyUnbounded, fontFamilyInter)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared user
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Sidebar
        {}




-- INIT


type alias Model =
    {
    }


init : Shared.Model -> Auth.User -> () -> ( Model, Effect Msg )
init shared user () =
    let
        filter =
            { authors = Just [ user.pubKey ]
            , ids = Nothing
            , kinds = Just [ KindBookmarkList, KindBookmarkSets ]
            , tagReferences = Nothing
            , limit = Nothing
            , since = Nothing
            , until = Nothing
            }
    in
    ( { }
    , RequestBookmarks filter
    |> Nostr.createRequest shared.nostr "Bookmarks" []
    |> Shared.Msg.RequestNostrEvents
    |> Effect.sendSharedMsg
    )



-- UPDATE


type Msg
    =  ReceivedMessage Nostr.IncomingMessage


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ReceivedMessage message ->
            updateWithMessage shared model message


updateWithMessage : Shared.Model.Model -> Model -> Nostr.IncomingMessage -> (Model, Effect Msg)
updateWithMessage shared model message =
    ( model, Effect.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.receiveMessage ReceivedMessage


-- VIEW

view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.bookmarksTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.min_h_screen
                , Tw.bg_color Theme.gray_100
                ]
            ]
            [ div
                [ css
                    [ Tw.bg_color Theme.white
                    , Tw.p_6
                    , Tw.rounded_lg
                    , Tw.shadow_lg
                    , Tw.max_w_3xl
                    , Tw.space_y_2
                    ]
                ]
                [ h1
                    [ css
                        [ Tw.text_4xl
                        , Tw.font_bold
                        , Tw.text_color Theme.gray_900
                        , Tw.mb_2
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text <| Translations.bookmarksTitle [ shared.browserEnv.translations ]
                    ]
                , h3
                    [ css
                        [ Tw.text_2xl
                        , Tw.font_bold
                        , Tw.text_color Theme.gray_900
                        , Tw.mb_2
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text <| Translations.myBookmarksTitle [ shared.browserEnv.translations ]
                    ]
                , viewBookmarks shared model
                ]
            ]
        ]
    }

viewBookmarks : Shared.Model -> Model -> Html Msg
viewBookmarks shared model =
    div [][]
{-
    case shared.loginStatus of
        Shared.Model.LoggedIn pubKey ->
            case getBookmarks shared.nostr pubKey of
                Just bookmarks ->
                    bookmarks.articles
                    |> List.map (viewArticlePreview model)
                    |> div 
                        [ css
                            [ Tw.space_y_2
                            ]
                        ]

        Nothing ->
            div
                []
                []
-}