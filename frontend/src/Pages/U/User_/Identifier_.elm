module Pages.U.User_.Identifier_ exposing (Model, Msg, page)

import Browser.Dom
import Components.ArticleInfo as ArticleInfo
import Components.Comment as Comment
import Components.RelayStatus exposing (Purpose(..))
import Components.SharingButtonDialog as SharingButtonDialog
import Components.ZapDialog as ZapDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Layouts
import Layouts.Sidebar
import LinkPreview exposing (LoadedContent)
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip05 as Nip05
import Nostr.Nip18 exposing (articleRepostEvent)
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (CommentType)
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Set
import Shared
import Shared.Msg
import Task
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)
import Ui.View exposing (viewRelayStatus)
import View exposing (View)


page : Shared.Model -> Route { user : String, identifier : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    let
        styles = Ui.Styles.stylesForTheme shared.theme

        maybeArticle = articleFromModel shared model

        userPubKey =
            Shared.loggedInPubKey shared.loginStatus

        articleInfo =
            maybeArticle
            |> Maybe.map (\article ->
                ArticleInfo.view
                    styles
                    (Nostr.getAuthor shared.nostr article.author)
                    article
                    shared.browserEnv
                    (Nostr.getInteractionsForArticle shared.nostr userPubKey article)
                    shared.nostr
            )
            |> Maybe.withDefault emptyHtml
    in
    Layouts.Sidebar.new
        { styles = styles
        }
        |> Layouts.Sidebar.withLeftPart articleInfo
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { loadedContent : LoadedContent Msg
    , comment : Comment.Model
    , identifier : String
    , nip05 : Maybe Nip05.Nip05
    , requestId : Maybe RequestId
    , zapDialog : ZapDialog.Model
    , sharingButtonDialog : SharingButtonDialog.Model
    }


init : Shared.Model -> Route { user : String, identifier : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        model =
            { identifier = route.params.identifier
            , comment = Comment.init {}
            , nip05 = Nip05.parseNip05 route.params.user
            , loadedContent = { loadedUrls = Set.empty, addLoadedContentFunction = AddLoadedContent }
            , requestId = Nothing
            , zapDialog = ZapDialog.init {}
            , sharingButtonDialog = SharingButtonDialog.init
            }

        ( requestEffect, requestId ) =
            model.nip05
                |> Maybe.map
                    (\nip05 ->
                        case Nostr.getPubKeyByNip05 shared.nostr nip05 of
                            Just pubKey ->
                                case Nostr.getArticleWithIdentifier shared.nostr pubKey model.identifier of
                                    Just _ ->
                                        -- article already loaded, accessible in view function
                                        ( Effect.none, Nothing )

                                    Nothing ->
                                        ( -- pubkey already loaded, request article
                                          { emptyEventFilter
                                            | authors = Just [ pubKey ]
                                            , kinds = Just [ KindLongFormContent ]
                                            , tagReferences = Just [ TagReferenceIdentifier model.identifier ]
                                          }
                                            |> RequestArticle (Just <| Nostr.getReadRelayUrlsForPubKey shared.nostr pubKey)
                                            |> Nostr.createRequest shared.nostr ("Article of NIP-05 user " ++ Nip05.nip05ToString nip05) []
                                            |> Shared.Msg.RequestNostrEvents
                                            |> Effect.sendSharedMsg
                                        , Just <| Nostr.getLastRequestId shared.nostr
                                        )

                            Nothing ->
                                ( RequestNip05AndArticle nip05 model.identifier
                                    |> Nostr.createRequest shared.nostr ("Article of NIP-05 user " ++ Nip05.nip05ToString nip05) [ KindLongFormContent, KindHighlights, KindBookmarkList, KindBookmarkSets ]
                                    |> Shared.Msg.RequestNostrEvents
                                    |> Effect.sendSharedMsg
                                , Just <| Nostr.getLastRequestId shared.nostr
                                )
                    )
                |> Maybe.withDefault ( Effect.none, Nothing )
    in
    ( { model | requestId = requestId }
    , Effect.batch
        [ requestEffect

        -- jump to top of article
        , Effect.sendCmd <| Task.perform (\_ -> NoOp) (Browser.Dom.setViewport 0 0)
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | AddArticleBookmark PubKey AddressComponents
    | RemoveArticleBookmark PubKey AddressComponents
    | AddArticleReaction PubKey EventId PubKey AddressComponents -- 2nd pubkey author of article to be liked
    | AddRepost PubKey Article
    | AddLoadedContent String
    | OpenComment CommentType
    | CommentSent Comment.Msg
    | ZapReaction PubKey (List ZapDialog.Recipient)
    | ZapDialogSent (ZapDialog.Msg Msg)
    | SharingButtonDialogMsg SharingButtonDialog.Msg

update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        AddArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        RemoveArticleBookmark pubKey addressComponents ->
            ( model
            , SendBookmarkListWithoutArticle pubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AddArticleReaction userPubKey eventId articlePubKey addressComponents ->
            ( model
            , SendReaction userPubKey eventId articlePubKey addressComponents
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AddRepost userPubKey article ->
            let
                relays =
                    article.relays
                        |> Set.toList
                        |> List.append (Nostr.getWriteRelayUrlsForPubKey shared.nostr userPubKey)
            in
            ( model
            , SendRepost relays (articleRepostEvent userPubKey article)
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AddLoadedContent url ->
            ( { model | loadedContent = LinkPreview.addLoadedContent model.loadedContent url }, Effect.none )

        OpenComment comment ->
            ( { model | comment = Comment.show model.comment comment }, Effect.none )

        CommentSent innerMsg ->
            Comment.update
                { nostr = shared.nostr
                , msg = innerMsg
                , model = model.comment
                , toModel = \comment -> { model | comment = comment }
                , toMsg = CommentSent
                }

        ZapReaction _ recipients ->
            showZapDialog model recipients

        ZapDialogSent innerMsg ->
            ZapDialog.update
                { nostr = shared.nostr
                , msg = innerMsg
                , model = model.zapDialog
                , toModel = \zapDialog -> { model | zapDialog = zapDialog }
                , toMsg = ZapDialogSent
                }

        SharingButtonDialogMsg innerMsg ->
            SharingButtonDialog.update
                { browserEnv = shared.browserEnv
                , model = model.sharingButtonDialog
                , msg = innerMsg
                , toModel = \sharingButtonDialog -> { model | sharingButtonDialog = sharingButtonDialog }
                , toMsg = SharingButtonDialogMsg
                }


showZapDialog : Model -> List ZapDialog.Recipient -> ( Model, Effect Msg )
showZapDialog model recipients =
    let
        ( zapDialogModel, effect ) =
            ZapDialog.show model.zapDialog ZapDialogSent recipients
    in
    ( { model | zapDialog = zapDialogModel }
    , effect
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map CommentSent (Comment.subscriptions model.comment)



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        maybeArticle =
            articleFromModel shared model
    in
    { title = maybeArticle |> Maybe.andThen .title |> Maybe.withDefault "Article"
    , body = [ viewArticle shared model maybeArticle ]
    }

articleFromModel : Shared.Model -> Model -> Maybe Article
articleFromModel shared model =
    model.nip05
        |> Maybe.andThen (Nostr.getPubKeyByNip05 shared.nostr)
        |> Maybe.andThen (\pubKey -> Nostr.getArticleWithIdentifier shared.nostr pubKey model.identifier)

viewArticle : Shared.Model -> Model -> Maybe Article -> Html Msg
viewArticle shared model maybeArticle =
    let
        signingUserPubKey =
            Shared.loggedInSigningPubKey shared.loginStatus

        commenting =
            signingUserPubKey
                |> Maybe.andThen (Nostr.getProfile shared.nostr)
                |> Maybe.andThen
                    (\profile ->
                        ( Comment.new
                            { model = model.comment
                            , toMsg = CommentSent
                            , nostr = shared.nostr
                            , profile = profile
                            , loginStatus = shared.loginStatus
                            , browserEnv = shared.browserEnv
                            , theme = shared.theme
                            }
                        , OpenComment
                        )
                            |> Just
                    )

        userPubKey =
            Shared.loggedInPubKey shared.loginStatus
    in
    case maybeArticle of
        Just article ->
            Ui.View.viewArticle
                { theme = shared.theme
                , browserEnv = shared.browserEnv
                , nostr = shared.nostr
                , userPubKey = Shared.loggedInPubKey shared.loginStatus
                , onBookmark = Maybe.map (\pubKey -> ( AddArticleBookmark pubKey, RemoveArticleBookmark pubKey )) signingUserPubKey
                , commenting = commenting
                , onRepost = Maybe.map (\pubKey -> AddRepost pubKey article) signingUserPubKey
                , onReaction = Maybe.map (\pubKey -> AddArticleReaction pubKey) signingUserPubKey

                -- signing is possible also with read-only login
                , onZap = Maybe.map (\pubKey -> ZapReaction pubKey) userPubKey
                , sharing = Just ( model.sharingButtonDialog, SharingButtonDialogMsg )
                }
                (Just model.loadedContent)
                article

        Nothing ->
            viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle model.requestId
