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
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..), eventFilterForNip19, informationForKind, kindFromNumber)
import Nostr.Nip19 as Nip19
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Types exposing (IncomingMessage, RelayUrl)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Translations.Sidebar as Translations
import Ui.ShortNote
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
    | Article AddressComponents (List RelayUrl)
    | NonSupportedNip19 String
    | NonSupportedKind Kind
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

                Ok (Nip19.NAddr { identifier, pubKey, kind, relays }) ->
                    case kindFromNumber kind of
                        KindLongFormContent ->
                            Article (kindFromNumber kind, pubKey, identifier) relays 

                        _ ->
                            NonSupportedKind (kindFromNumber kind)

                Ok _ ->
                    NonSupportedNip19 route.params.event

                Err error ->
                    DecodingError ("Error decoding " ++ route.params.event ++ "(" ++ error ++ ")")

        (effect, requestId) =
            case (contentToView, Result.toMaybe decoded |> Maybe.andThen eventFilterForNip19 ) of
                ( ShortNote noteId, Just eventFilter ) ->
                    ( eventFilter
                        |> RequestShortNote
                        |> Nostr.createRequest shared.nostr ("NIP-19 note " ++ route.params.event) [ KindUserMetadata]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                ( ShortNote _, Nothing ) ->
                    ( Effect.none, Nothing
                    )

                ( Article _ relays, Just eventFilter ) ->
                    ( eventFilter
                        |> RequestArticle (Just relays)
                        |> Nostr.createRequest shared.nostr ("NIP-19 article " ++ route.params.event) [ KindUserMetadata]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                ( Article _ _, Nothing ) ->
                    ( Effect.none, Nothing
                    )

                ( NonSupportedNip19 _, Just eventFilter ) ->
                    -- request event so we can implement test with it
                    ( eventFilter
                        |> RequestArticle Nothing
                        |> Nostr.createRequest shared.nostr ("NIP-19 event " ++ route.params.event) [ KindUserMetadata]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                ( NonSupportedNip19 _, _ ) ->
                    ( Effect.none, Nothing
                    )

                ( NonSupportedKind _, _ ) ->
                    ( Effect.none, Nothing
                    )

                ( DecodingError _, _) ->
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
            |> Maybe.map (\shortNote ->
                    Ui.ShortNote.viewShortNote 
                        { theme = shared.theme
                        , browserEnv = shared.browserEnv
                        , nostr = shared.nostr
                        , userPubKey = Nothing
                        , onBookmark = Nothing
                        }
                        { author = Nostr.getAuthor shared.nostr shortNote.pubKey
                        , interactions = 
                            { zaps = Nothing
                            , highlights = Nothing
                            , reactions = Nothing
                            , reposts = Nothing
                            , notes = Nothing
                            , bookmarks = Nothing
                            , isBookmarked = False
                            }
                        }
                        shortNote
                )
            |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingNote model.requestId)

        Article addressComponents relays ->
            Nostr.getArticleForAddressComponents shared.nostr addressComponents
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

        NonSupportedKind kind ->
            let
                info =
                    informationForKind kind
            in
            div
                [
                ]
                [ text <| "Non-supported kind: " ++ info.description
                ]

        DecodingError error ->
            div
                [
                ]
                [ text <| "Error decoding NIP-19 parameter " ++ error
                ]

