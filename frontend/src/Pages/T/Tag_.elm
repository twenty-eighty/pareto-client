module Pages.T.Tag_ exposing (Model, Msg, page)

import Components.ArticleComments as ArticleComments
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (div, h3, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Request exposing (RequestData(..))
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Time exposing (Month(..))
import Ui.Styles exposing (Theme, fontFamilyRobotoMono)
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
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar



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
    , RequestArticlesFeed False [ filter ]
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
    = NoOp


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


textStyleHashtagLarge : List (Html.Attribute msg)
textStyleHashtagLarge =
    [ css
        [ Tw.text_4xl
        , Tw.font_bold
        ]
    , fontFamilyRobotoMono
    ]


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
                (textStyleHashtagLarge
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
                    { articleComments = ArticleComments.init
                    , articleToInteractionsMsg = \_ _ -> NoOp
                    , bookmarkButtonMsg = \_ _ -> NoOp
                    , bookmarkButtons = Dict.empty
                    , browserEnv = shared.browserEnv
                    , commentsToMsg = \_ -> NoOp
                    , deleteButtonMsg = Nothing
                    , nostr = shared.nostr
                    , loginStatus = shared.loginStatus
                    , onLoadMore = Nothing
                    , sharing = Nothing
                    , theme = shared.theme
                    }
            ]
        ]
    }
