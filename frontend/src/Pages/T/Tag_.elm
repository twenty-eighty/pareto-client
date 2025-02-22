module Pages.T.Tag_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Styled as Html exposing (div, h3, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Request exposing (RequestData(..))
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Time exposing (Month(..))
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import Url
import View exposing (View)


page : Shared.Model -> Route { tag : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { tag : String
    , articles : List Article
    , filter : EventFilter
    }


init : Shared.Model -> Route { tag : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        decodedParam =
            Url.percentDecode route.params.tag
                |> Maybe.withDefault route.params.tag

        filter =
            { emptyEventFilter
                | kinds = Just [ KindLongFormContent ]
                , tagReferences = Just <| tagReferencesForParam decodedParam
                , limit = Just 20
            }
    in
    ( { tag = decodedParam
      , articles = []
      , filter = filter
      }
    , RequestArticlesFeed filter
        |> Nostr.createRequest shared.nostr ("Articles for hashtag " ++ route.params.tag) [ KindUserMetadata ]
        |> Shared.Msg.RequestNostrEvents
        |> Effect.sendSharedMsg
    )


tagReferencesForParam : String -> List TagReference
tagReferencesForParam hashtag =
    let
        decoded =
            decodedTagParam hashtag

        lowercase =
            decoded
                |> String.toLower

        firstcharUpper =
            decoded
                |> String.left 1
                |> String.toUpper

        remainingLower =
            decoded
                |> String.dropLeft 1
                |> String.toLower

        mixedCase =
            firstcharUpper ++ remainingLower
    in
    [ TagReferenceTag hashtag
    , TagReferenceTag lowercase
    , TagReferenceTag mixedCase
    ]


decodedTagParam : String -> String
decodedTagParam hashtag =
    case Url.percentDecode hashtag of
        Just decoded ->
            decoded

        Nothing ->
            hashtag



-- UPDATE


type Msg
    = OpenGetStarted


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        OpenGetStarted ->
            ( model
            , Effect.sendCmd <| Ports.requestUser
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme
    in
    { title = model.tag
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.items_center
                , Tw.justify_center
                , Tw.my_4
                ]
            ]
            [ h3
                (styles.textStyleHashtagLarge
                    ++ styles.colorStyleGrayscaleTitle
                    ++ [ css
                            [ Tw.mb_4
                            ]
                       ]
                )
                [ text <| "#" ++ model.tag
                ]
            , Nostr.getArticlesByDate shared.nostr
                |> Ui.View.viewArticlePreviews
                    ArticlePreviewList
                    { theme = shared.theme
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    , userPubKey = Shared.loggedInPubKey shared.loginStatus
                    , onBookmark = Nothing
                    , onReaction = Nothing
                    , onZap = Nothing
                    }
            ]
        ]
    }
