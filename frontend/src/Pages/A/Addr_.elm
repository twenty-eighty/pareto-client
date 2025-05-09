module Pages.A.Addr_ exposing (..)

import Components.ArticleInfo as ArticleInfo
import Components.Comment as Comment
import Components.RelayStatus exposing (Purpose(..))
import Components.SharingButtonDialog as SharingButtonDialog
import Components.ZapDialog as ZapDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (div)
import Html.Styled.Attributes exposing (css)
import Layouts
import Layouts.Sidebar
import LinkPreview exposing (LoadedContent)
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event as Event exposing (AddressComponents, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip18 exposing (articleRepostEvent)
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (CommentType)
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Set
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.ArticlePage as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (stylesForTheme)
import Ui.View exposing (viewRelayStatus)
import Url
import View exposing (View)


page : Shared.Model -> Route { addr : String } -> Page Model Msg
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
        styles =
            Ui.Styles.stylesForTheme shared.theme

        maybeArticle =
            case model of
                Nip19Model { nip19 } ->
                    Nostr.getArticleForNip19 shared.nostr nip19

                ErrorModel _ ->
                    Nothing

        userPubKey =
            Shared.loggedInPubKey shared.loginStatus

        articleInfo =
            maybeArticle
                |> Maybe.map
                    (\article ->
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
        { theme = shared.theme
        }
        |> Layouts.Sidebar.withLeftPart articleInfo
        |> Layouts.Sidebar



-- INIT


type Model
    = Nip19Model Nip19ModelData
    | ErrorModel String


type alias Nip19ModelData =
    { loadedContent : LoadedContent Msg
    , comment : Comment.Model
    , nip19 : NIP19Type
    , requestId : RequestId
    , zapDialog : ZapDialog.Model
    , sharingButtonDialog : SharingButtonDialog.Model
    }


init : Shared.Model -> Route { addr : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        decoded =
            Nip19.decode route.params.addr

        ( model, maybeArticle ) =
            case decoded of
                Ok nip19 ->
                    ( Nip19Model
                        { loadedContent =
                            { loadedUrls = Set.empty
                            , addLoadedContentFunction = AddLoadedContent
                            }
                        , comment = Comment.init {}
                        , nip19 = nip19
                        , requestId = Nostr.getLastRequestId shared.nostr
                        , zapDialog = ZapDialog.init {}
                        , sharingButtonDialog = SharingButtonDialog.init
                        }
                    , Nostr.getArticleForNip19 shared.nostr nip19
                    )

                Err error ->
                    ( ErrorModel error, Nothing )

        effect =
            case ( maybeArticle, model ) of
                ( Nothing, Nip19Model { nip19 } ) ->
                    let
                        maybeAuthorsPubKey =
                            Nostr.getArticleForNip19 shared.nostr nip19 |> Maybe.map .author

                        followersEffect =
                            Shared.createFollowersEffect shared.nostr maybeAuthorsPubKey
                    in
                    -- article not loaded yet, request it now
                    case nip19 of
                        NAddr naddrData ->
                            Effect.batch
                                [ followersEffect
                                , Event.eventFilterForNaddr naddrData
                                    |> RequestArticle
                                        (if naddrData.relays /= [] then
                                            Just naddrData.relays

                                         else
                                            Nothing
                                        )
                                    |> Nostr.createRequest shared.nostr "Article described as NIP-19 NAddr" [ KindUserMetadata ]
                                    |> Shared.Msg.RequestNostrEvents
                                    |> Effect.sendSharedMsg
                                ]

                        NEvent neventData ->
                            Effect.batch
                                [ followersEffect
                                , Event.eventFilterForNevent neventData
                                    |> RequestArticle
                                        (if neventData.relays /= [] then
                                            Just neventData.relays

                                         else
                                            Nothing
                                        )
                                    |> Nostr.createRequest shared.nostr "Article described as NIP-19 NEvent" [ KindUserMetadata ]
                                    |> Shared.Msg.RequestNostrEvents
                                    |> Effect.sendSharedMsg
                                ]

                        _ ->
                            Effect.none

                ( _, _ ) ->
                    Effect.none
    in
    ( model
    , Effect.batch
        [ effect

        -- jump to top of article
        , Effect.scrollContentToTop
        ]
    )


decodedTagParam : String -> Maybe (List String)
decodedTagParam tag =
    Url.percentDecode tag
        |> Maybe.map List.singleton



-- UPDATE


type Msg
    = AddArticleBookmark PubKey AddressComponents
    | RemoveArticleBookmark PubKey AddressComponents
    | AddArticleReaction PubKey EventId PubKey AddressComponents -- 2nd pubkey author of article to be liked
    | AddRepost PubKey Article
    | AddLoadedContent String
    | OpenComment CommentType
    | CommentSent Comment.Msg
    | ZapReaction PubKey (List ZapDialog.Recipient)
    | ZapDialogSent (ZapDialog.Msg Msg)
    | SharingButtonDialogMsg SharingButtonDialog.Msg
    | NoOp


update : Shared.Model.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
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
            case model of
                Nip19Model nip19ModelData ->
                    ( Nip19Model { nip19ModelData | loadedContent = LinkPreview.addLoadedContent nip19ModelData.loadedContent url }, Effect.none )

                _ ->
                    ( model, Effect.none )

        OpenComment comment ->
            case model of
                Nip19Model nip19ModelData ->
                    ( Nip19Model { nip19ModelData | comment = Comment.show nip19ModelData.comment comment }, Effect.none )

                _ ->
                    ( model, Effect.none )

        CommentSent innerMsg ->
            case model of
                Nip19Model nip19ModelData ->
                    Comment.update
                        { nostr = shared.nostr
                        , msg = innerMsg
                        , model = nip19ModelData.comment
                        , toModel = \comment -> Nip19Model { nip19ModelData | comment = comment }
                        , toMsg = CommentSent
                        }

                _ ->
                    ( model, Effect.none )

        ZapReaction _ recipients ->
            case model of
                Nip19Model nip19ModelData ->
                    showZapDialog nip19ModelData recipients

                _ ->
                    ( model, Effect.none )

        ZapDialogSent innerMsg ->
            case model of
                Nip19Model nip19ModelData ->
                    ZapDialog.update
                        { nostr = shared.nostr
                        , msg = innerMsg
                        , model = nip19ModelData.zapDialog
                        , toModel = \zapDialog -> Nip19Model { nip19ModelData | zapDialog = zapDialog }
                        , toMsg = ZapDialogSent
                        }

                _ ->
                    ( model, Effect.none )

        SharingButtonDialogMsg innerMsg ->
            case model of
                Nip19Model nip19ModelData ->
                    SharingButtonDialog.update
                        { browserEnv = shared.browserEnv
                        , model = nip19ModelData.sharingButtonDialog
                        , msg = innerMsg
                        , toModel = \sharingButtonDialog -> Nip19Model { nip19ModelData | sharingButtonDialog = sharingButtonDialog }
                        , toMsg = SharingButtonDialogMsg
                        }

                _ ->
                    ( model, Effect.none )

        NoOp ->
            let
                maybeAuthorPubKey =
                    case model of
                        Nip19Model nip19ModelData ->
                            Nostr.getArticleForNip19 shared.nostr nip19ModelData.nip19 |> Maybe.map .author

                        _ ->
                            Nothing

                followersEffect =
                    Shared.createFollowersEffect shared.nostr maybeAuthorPubKey
            in
            ( model, followersEffect )


showZapDialog : Nip19ModelData -> List ZapDialog.Recipient -> ( Model, Effect Msg )
showZapDialog nip19ModelData recipients =
    let
        ( zapDialogModel, effect ) =
            ZapDialog.show nip19ModelData.zapDialog ZapDialogSent recipients
    in
    ( Nip19Model { nip19ModelData | zapDialog = zapDialogModel }
    , effect
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Nip19Model nip19ModelData ->
            Sub.map CommentSent (Comment.subscriptions nip19ModelData.comment)

        _ ->
            Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    case model of
        Nip19Model { loadedContent, comment, nip19, requestId, sharingButtonDialog } ->
            viewContent shared nip19 comment loadedContent requestId sharingButtonDialog

        ErrorModel error ->
            viewError shared error


viewContent : Shared.Model -> NIP19Type -> Comment.Model -> LoadedContent Msg -> RequestId -> SharingButtonDialog.Model -> View Msg
viewContent shared nip19 comment loadedContent requestId sharingButtonDialog =
    let
        maybeArticle =
            Nostr.getArticleForNip19 shared.nostr nip19

        userPubKey =
            Shared.loggedInPubKey shared.loginStatus

        signingUserPubKey =
            Shared.loggedInSigningPubKey shared.loginStatus

        commenting =
            signingUserPubKey
                |> Maybe.andThen (Nostr.getProfile shared.nostr)
                |> Maybe.andThen
                    (\profile ->
                        ( Comment.new
                            { model = comment
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
    in
    { title =
        maybeArticle
            |> Maybe.andThen .title
            |> Maybe.withDefault (Translations.defaultPageTitle [ shared.browserEnv.translations ])
    , body =
        [ maybeArticle
            |> Maybe.map
                (\article ->
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
                        , sharing = Just ( sharingButtonDialog, SharingButtonDialogMsg )
                        }
                        (Just loadedContent)
                        article
                )
            |> Maybe.withDefault (viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle (Just requestId))
        ]
    }


viewError : Shared.Model -> String -> View Msg
viewError shared error =
    let
        styles =
            stylesForTheme shared.theme
    in
    { title = Translations.defaultPageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            (styles.colorStyleGrayscaleTitle
                ++ styles.textStyleH3
                ++ [ css
                        [ Tw.m_4
                        ]
                   ]
            )
            [ Html.text <| "Error loading content: " ++ error
            ]
        ]
    }
