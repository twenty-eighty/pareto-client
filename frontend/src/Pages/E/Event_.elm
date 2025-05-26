module Pages.E.Event_ exposing (..)

import Components.Interactions
import Components.RelayStatus exposing (Purpose(..))
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, article, div, text)
import Layouts
import Layouts.Sidebar
import LinkPreview exposing (LoadedContent)
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..), eventFilterForNip19, informationForKind, kindFromNumber)
import Nostr.Nip19 as Nip19
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (IncomingMessage, RelayUrl)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Set
import Shared
import Shared.Model
import Shared.Msg
import Translations.Sidebar as Translations
import Ui.ShortNote
import Ui.Styles exposing (Theme)
import Ui.View exposing (viewRelayStatus)
import Url
import View exposing (View)


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
toLayout theme _ =
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar



-- INIT


type ContentToView
    = ShortNote String (Maybe (List RelayUrl))
    | Article AddressComponents (List RelayUrl)
    | NonSupportedNip19 String
    | NonSupportedKind Kind
    | DecodingError String


type alias Model =
    { contentToView : ContentToView
    , loadedContent : LoadedContent Msg
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
                    ShortNote noteId Nothing

                Ok (Nip19.NEvent { id, relays }) ->
                    ShortNote id (Just relays)

                Ok (Nip19.NAddr { identifier, pubKey, kind, relays }) ->
                    case kindFromNumber kind of
                        KindLongFormContent ->
                            Article ( kindFromNumber kind, pubKey, identifier ) relays

                        _ ->
                            NonSupportedKind (kindFromNumber kind)

                Ok _ ->
                    NonSupportedNip19 route.params.event

                Err error ->
                    DecodingError ("Error decoding " ++ route.params.event ++ "(" ++ error ++ ")")

        ( effect, requestId ) =
            case ( contentToView, Result.toMaybe decoded |> Maybe.andThen eventFilterForNip19 ) of
                ( ShortNote _ relays, Just eventFilter ) ->
                    ( eventFilter
                        |> RequestShortNote relays
                        |> Nostr.createRequest shared.nostr ("NIP-19 note " ++ route.params.event) [ KindUserMetadata ]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                ( ShortNote _ _, Nothing ) ->
                    ( Effect.none
                    , Nothing
                    )

                ( Article _ relays, Just eventFilter ) ->
                    ( eventFilter
                        |> RequestArticle (Just relays)
                        |> Nostr.createRequest shared.nostr ("NIP-19 article " ++ route.params.event) [ KindUserMetadata ]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                ( Article _ _, Nothing ) ->
                    ( Effect.none
                    , Nothing
                    )

                ( NonSupportedNip19 _, Just eventFilter ) ->
                    -- request event so we can implement test with it
                    ( eventFilter
                        |> RequestArticle Nothing
                        |> Nostr.createRequest shared.nostr ("NIP-19 event " ++ route.params.event) [ KindUserMetadata ]
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    , Just <| Nostr.getLastRequestId shared.nostr
                    )

                ( NonSupportedNip19 _, _ ) ->
                    ( Effect.none
                    , Nothing
                    )

                ( NonSupportedKind _, _ ) ->
                    ( Effect.none
                    , Nothing
                    )

                ( DecodingError _, _ ) ->
                    ( Effect.none, Nothing )
    in
    ( { contentToView = contentToView
      , loadedContent = { loadedUrls = Set.empty, addLoadedContentFunction = AddLoadedContent }
      , requestId = requestId
      }
    , effect
    )


tagReferencesForParam : String -> Maybe (List TagReference)
tagReferencesForParam tag =
    decodedTagParam tag
        |> Maybe.map TagReferenceEventId
        |> Maybe.map List.singleton


decodedTagParam : String -> Maybe String
decodedTagParam tag =
    Url.percentDecode tag



-- UPDATE


type Msg
    = AddLoadedContent String
    | ReceivedMessage IncomingMessage
    | NostrMsg Nostr.Msg
    | NoOp


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        AddLoadedContent url ->
            ( { model | loadedContent = LinkPreview.addLoadedContent model.loadedContent url }, Effect.none )

        ReceivedMessage _ ->
            ( model, Effect.none )

        NostrMsg _ ->
            ( model, Effect.none )

        NoOp ->
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
subscriptions _ =
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
    case model.contentToView of
        ShortNote noteId _ ->
            Nostr.getShortNoteById shared.nostr noteId
                |> Maybe.map
                    (\shortNote ->
                        Ui.ShortNote.viewShortNote
                            { theme = shared.theme
                            , browserEnv = shared.browserEnv
                            , nostr = shared.nostr
                            , userPubKey = Nothing
                            , onBookmark = Nothing
                            }
                            { author = Nostr.getAuthor shared.nostr shortNote.pubKey
                            , actions =
                                { addBookmark = Nothing
                                , removeBookmark = Nothing
                                , addReaction = Nothing
                                , removeReaction = Nothing
                                , addRepost = Nothing
                                , startComment = Nothing
                                }
                            , interactions =
                                { zaps = Nothing
                                , articleComments = []
                                , articleCommentComments = Dict.empty
                                , highlights = Nothing
                                , reactions = Nothing
                                , reposts = Nothing
                                , notes = Nothing
                                , bookmarks = Nothing
                                , isBookmarked = False
                                , reaction = Nothing
                                , repost = Nothing
                                }
                            }
                            shortNote
                    )
                |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingNote model.requestId)

        Article addressComponents _ ->
            Nostr.getArticleForAddressComponents shared.nostr addressComponents
                |> Maybe.map
                    (Ui.View.viewArticle
                        { theme = shared.theme
                        , bookmarkButtonMsg = \_ _ -> NoOp
                        , bookmarkButtons = Dict.empty
                        , browserEnv = shared.browserEnv
                        , nostr = shared.nostr
                        , loginStatus = shared.loginStatus
                        , onBookmark = Nothing
                        , commenting = Nothing
                        , onReaction = Nothing
                        , onRepost = Nothing
                        , onZap = Nothing
                        , articleToInteractionsMsg = \_ _ -> NoOp
                        , openCommentMsg = Nothing
                        , sharing = Nothing
                        }
                        (Just model.loadedContent)
                        Components.Interactions.init
                    )
                |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle model.requestId)

        NonSupportedNip19 parameter ->
            div
                []
                [ text <| "Non-supported NIP-19 parameter: " ++ parameter
                ]

        NonSupportedKind kind ->
            let
                info =
                    informationForKind kind
            in
            div
                []
                [ text <| "Non-supported kind: " ++ info.description
                ]

        DecodingError error ->
            div
                []
                [ text <| "Error decoding NIP-19 parameter " ++ error
                ]
