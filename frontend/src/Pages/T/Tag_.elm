module Pages.T.Tag_ exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Css
import Json.Decode as Decode
import Effect exposing (Effect)
import Graphics
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Layouts.Sidebar
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
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import Url
import View exposing (View)
import Time exposing (Month(..))


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
toLayout theme model =
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
            { emptyEventFilter | kinds = Just [KindLongFormContent], tagReferences = tagReferencesForParam decodedParam, limit = Just 20 }
    in
    ( { tag = decodedParam
      , articles = []
      , filter = filter
      }
    , RequestArticlesFeed filter
      |> Nostr.createRequest shared.nostr ("Articles for hashtag " ++ route.params.tag) [KindUserMetadata] 
      |> Shared.Msg.RequestNostrEvents
      |> Effect.sendSharedMsg
    )


tagReferencesForParam : String -> Maybe (List TagReference)
tagReferencesForParam tag =
    decodedTagParam tag
    |> Maybe.map TagReferenceTag
    |> Maybe.map (List.singleton)

decodedTagParam : String -> Maybe String
decodedTagParam tag =
    Url.percentDecode tag

-- UPDATE


type Msg
    = OpenGetStarted


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model
            , Effect.sendCmd <| Ports.requestUser
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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
                , Tw.items_center
                , Tw.justify_center
                , Tw.mb_4
                ]
            ]
            [ div
                [ css
                    [ Tw.p_6
                    , Tw.rounded_lg
                    , Tw.shadow_lg
                    , Tw.max_w_3xl
                    ]
                ]
                [ h3
                    (styles.textStyleHashtagLarge ++ styles.colorStyleGrayscaleTitle ++
                    [ css
                        [ Tw.mb_4
                        ]
                    ])
                    [ text <| "#" ++ model.tag
                    ]
                , Nostr.getArticlesByDate shared.nostr
                |> Ui.View.viewArticlePreviews ArticlePreviewList shared.theme shared.browserEnv shared.nostr Nothing
                ]
            ]
        ]
    }
