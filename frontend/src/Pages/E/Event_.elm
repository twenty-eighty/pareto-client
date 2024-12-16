module Pages.E.Event_ exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.RelayStatus exposing (Purpose(..))
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), kindFromNumber)
import Nostr.Nip19 as Nip19
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Types exposing (IncomingMessage)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Translations.Sidebar as Translations
import Ui.Styles exposing (Theme)
import Ui.View exposing (viewRelayStatus)
import Url
import View exposing (View)
import Nostr.ShortNote exposing (ShortNote)


page : Shared.Model -> Route { event : String } -> Page Model Msg
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

type ContentToView
    = ShortNote String
    | Article Nip19.NAddrData
    | NonSupportedNip19 String
    | DecodingError String

type alias Model =
    { contentToView : ContentToView
    , requestId : Maybe RequestId
    }


init : Shared.Model -> Route { event : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        decoded =
            Nip19.decode route.params.event

        contentToView =
            case decoded of
                Ok (Nip19.Note noteId) ->
                    ShortNote noteId

                Ok (Nip19.NAddr naddrData) ->
                    Article naddrData

                Ok _ ->
                    NonSupportedNip19 route.params.event

                Err error ->
                    DecodingError ("Error decoding " ++ route.params.event ++ "(" ++ error ++ ")")

        (effect, requestId) =
            case contentToView of
                ShortNote noteId ->
                    ( { authors = Nothing
                        , ids = Just [noteId]
                        , kinds = Nothing
                        , tagReferences = Nothing
                        , limit = Just 1
                        , since = Nothing
                        , until = Nothing
                     }
                        |> RequestShortNote
                        |> Nostr.createRequest shared.nostr ("NIP-19 note " ++ route.params.event) [ KindUserMetadata]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                Article naddrData ->
                    ( { authors = Just [naddrData.pubKey]
                      , ids = Nothing
                      , kinds = Just [ kindFromNumber naddrData.kind ]
                      , tagReferences = Just [ TagReferenceIdentifier naddrData.identifier]
                      , limit = Just 1
                      , since = Nothing
                      , until = Nothing
                      }
                        |> RequestArticle (Just naddrData.relays)
                        |> Nostr.createRequest shared.nostr ("NIP-19 article " ++ route.params.event) [ KindUserMetadata]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                NonSupportedNip19 _ ->
                    ( Effect.none, Nothing )

                DecodingError _ ->
                    ( Effect.none, Nothing )
    in
    ( { contentToView = contentToView
      , requestId = requestId
      }
    , effect
    )

tagReferencesForParam : String -> Maybe (List TagReference)
tagReferencesForParam tag =
    decodedTagParam tag
    |> Maybe.map TagReferenceEventId
    |> Maybe.map (List.singleton)

decodedTagParam : String -> Maybe String
decodedTagParam tag =
    Url.percentDecode tag

-- UPDATE


type Msg
    = OpenGetStarted
    | ReceivedMessage IncomingMessage
    | NostrMsg Nostr.Msg


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model
            , Effect.sendCmd <| Ports.requestUser
            )

        ReceivedMessage message ->
            ( model, Effect.none )

        NostrMsg _ ->
            ( model, Effect.none )

addArticle : List Article -> Article -> List Article
addArticle articleList newArticle =
    if List.any (isArticleWithIdAndAuthor newArticle.author newArticle.id) articleList then
        newArticle :: articleList
    else
        newArticle :: articleList

isArticleWithIdAndAuthor : String -> String -> Article -> Bool
isArticleWithIdAndAuthor author articleId article =
    article.author == author && article.id == articleId

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    { title = Translations.readMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ viewContent shared model
        ]
    }

viewContent : Shared.Model -> Model -> Html Msg
viewContent shared model =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme
    in
    case model.contentToView of
        ShortNote noteId ->
            Nostr.getShortNoteById shared.nostr noteId
            |> Maybe.map (Ui.View.viewShortNote (styles) shared.browserEnv shared.nostr)
            |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingNote model.requestId)

        Article naddrData ->
            Nostr.getArticleForNip19 shared.nostr (Nip19.NAddr naddrData)
            |> Maybe.map
                (Ui.View.viewArticle 
                    { theme = shared.theme
                    , browserEnv = shared.browserEnv
                    , nostr = shared.nostr
                    , userPubKey = Shared.loggedInPubKey shared.loginStatus
                    , onBookmark = Nothing
                    }
                )
            |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle model.requestId)

        NonSupportedNip19 parameter ->
            div
                [
                ]
                [ text <| "Non-supported NIP-19 parameter: " ++ parameter
                ]

        DecodingError error ->
            div
                [
                ]
                [ text <| "Error decoding NIP-19 parameter " ++ error
                ]

