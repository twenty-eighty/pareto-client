module Components.ArticlePage exposing
    ( Model
    , MsgConfig
    , initModel
    , layout
    , viewBody
    , subscriptions
    , mapAuthorBarMsg
    , updateAddLoadedContent
    , updateArticleInteractions
    , updateComments
    , updateHighlights
    , updateSharingDialog
    , followAuthorEffect
    , unfollowAuthorEffect
    , navigateBackEffect
    , toggleArticleInfoEffect
    , followersEffectForAuthor
    , effectsForCachedArticle
    , scrollToTopEffect
    , pageTitle
    )

{-| Shared chrome and update helpers for full article pages (`/u/...`, `/a/...`).
Pages keep route-specific resolution and supply `MsgConfig` wrappers.
-}

import Components.ArticleComments as ArticleComments
import Components.ArticleHighlights as ArticleHighlights
import Components.ArticleInfo as ArticleInfo
import Components.AuthorInteractionsBar as AuthorInteractionsBar
import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import Components.SharingButtonDialog as SharingButtonDialog
import Dict
import Effect exposing (Effect)
import Html.Styled exposing (Html)
import Layouts
import Layouts.Sidebar
import LinkPreview exposing (LoadedContent)
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Query exposing (ContentQueryStatus(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, loggedInPubKey)
import Ports
import Set
import Shared
import Shared.Msg
import Ui.Article exposing (sharingInfoForArticle)
import Ui.ContentQuery
import Ui.Shared exposing (emptyHtml)
import Ui.Styles
import Ui.View


type alias Model msg =
    { loadedContent : LoadedContent msg
    , articleComments : ArticleComments.Model
    , articleHighlights : ArticleHighlights.Model
    , articleInteractions : Interactions.Model
    , sharingButtonDialog : SharingButtonDialog.Model
    }


type alias MsgConfig msg =
    { addLoadedContent : String -> msg
    , articleInteractionsSent : InteractionButton.InteractionObject -> Interactions.Msg msg -> msg
    , commentsSent : ArticleComments.Msg msg -> msg
    , highlightsSent : ArticleHighlights.Msg -> msg
    , sharingButtonDialogMsg : SharingButtonDialog.Msg -> msg
    , navigateBack : msg
    , followAuthor : PubKey -> PubKey -> msg
    , unfollowAuthor : PubKey -> PubKey -> msg
    , toggleArticleInfo : msg
    , noOp : msg
    }


initModel : (String -> msg) -> Model msg
initModel addLoadedContent =
    { loadedContent =
        { loadedUrls = Set.empty
        , addLoadedContentFunction = addLoadedContent
        }
    , articleComments = ArticleComments.init
    , articleHighlights = ArticleHighlights.init
    , articleInteractions = Interactions.init
    , sharingButtonDialog = SharingButtonDialog.init
    }


layout : Shared.Model -> Model msg -> Maybe Article -> MsgConfig msg -> Layouts.Layout msg
layout shared model maybeArticle msgConfig =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme

        articleInfo =
            maybeArticle
                |> Maybe.map
                    (\article ->
                        addressComponentsForArticle article
                            |> Maybe.map
                                (\addressComponents ->
                                    let
                                        interactionObject =
                                            InteractionButton.Article article.id addressComponents
                                    in
                                    ArticleInfo.view
                                        styles
                                        (Nostr.getAuthor shared.nostr article.author)
                                        article
                                        { browserEnv = shared.browserEnv
                                        , model = Just model.articleInteractions
                                        , toMsg = msgConfig.articleInteractionsSent interactionObject
                                        , theme = shared.theme
                                        , interactionObject = interactionObject
                                        , nostr = shared.nostr
                                        , loginStatus = shared.loginStatus
                                        , shareInfo = sharingInfoForArticle article (Nostr.getAuthor shared.nostr article.author)
                                        , zapRelays = article.relays
                                        }
                                )
                            |> Maybe.withDefault emptyHtml
                    )
                |> Maybe.withDefault emptyHtml

        articlePreviewsData =
            { articleComments = model.articleComments
            , articleHighlights = model.articleHighlights
            , articleToInteractionsMsg = msgConfig.articleInteractionsSent
            , bookmarkButtonMsg = \_ _ -> msgConfig.noOp
            , bookmarkButtons = Dict.empty
            , browserEnv = shared.browserEnv
            , commentsToMsg = msgConfig.commentsSent
            , highlightsToMsg = msgConfig.highlightsSent
            , deleteButtonMsg = Nothing
            , onLoadMore = Nothing
            , nostr = shared.nostr
            , loginStatus = shared.loginStatus
            , sharing = Just ( model.sharingButtonDialog, msgConfig.sharingButtonDialogMsg )
            , theme = shared.theme
            }

        authorInteractionsBar =
            maybeArticle
                |> Maybe.map
                    (\article ->
                        (AuthorInteractionsBar.new
                            { articlePreviewsData = articlePreviewsData
                            , model = AuthorInteractionsBar.init
                            , interactionsModel = model.articleInteractions
                            , article = article
                            , toMsg = mapAuthorBarMsg msgConfig
                            }
                            |> AuthorInteractionsBar.view
                        )
                            { articleInfoToggle = False }
                    )
                |> Maybe.withDefault emptyHtml
    in
    Layouts.Sidebar.new
        { theme = shared.theme }
        |> Layouts.Sidebar.withTopPart authorInteractionsBar "64px"
        |> Layouts.Sidebar.withRightPart articleInfo
        |> Layouts.Sidebar


viewBody : Shared.Model -> Model msg -> ContentQueryStatus Article -> MsgConfig msg -> Html msg
viewBody shared model queryStatus msgConfig =
    case queryStatus of
        ContentQueryReady article ->
            Ui.View.viewArticle
                { articleComments = model.articleComments
                , articleHighlights = model.articleHighlights
                , articleToInteractionsMsg = msgConfig.articleInteractionsSent
                , bookmarkButtonMsg = \_ _ -> msgConfig.noOp
                , bookmarkButtons = Dict.empty
                , browserEnv = shared.browserEnv
                , commentsToMsg = msgConfig.commentsSent
                , highlightsToMsg = msgConfig.highlightsSent
                , deleteButtonMsg = Nothing
                , nostr = shared.nostr
                , loginStatus = shared.loginStatus
                , onLoadMore = Nothing
                , sharing = Just ( model.sharingButtonDialog, msgConfig.sharingButtonDialogMsg )
                , theme = shared.theme
                }
                (Just model.loadedContent)
                model.articleInteractions
                article

        _ ->
            Ui.ContentQuery.viewStatus shared.theme shared.browserEnv.translations shared.nostr queryStatus


subscriptions : Shared.Model -> Model msg -> Maybe Article -> MsgConfig msg -> Sub msg
subscriptions shared model maybeArticle msgConfig =
    Sub.batch
        [ maybeArticle
            |> Maybe.andThen
                (\article ->
                    addressComponentsForArticle article
                        |> Maybe.map
                            (\addressComponents ->
                                Sub.map (msgConfig.articleInteractionsSent (InteractionButton.Article article.id addressComponents))
                                    (Interactions.subscriptions model.articleInteractions)
                            )
                )
            |> Maybe.withDefault Sub.none
        , articleCommentsSubscriptions shared model maybeArticle msgConfig
        , ArticleHighlights.subscriptions model.articleHighlights
            |> Sub.map msgConfig.highlightsSent
        ]


articleCommentsSubscriptions : Shared.Model -> Model msg -> Maybe Article -> MsgConfig msg -> Sub msg
articleCommentsSubscriptions shared model maybeArticle msgConfig =
    let
        articleComments =
            maybeArticle
                |> Maybe.andThen addressComponentsForArticle
                |> Maybe.map (Nostr.getArticleComments shared.nostr (loggedInPubKey shared.loginStatus))
                |> Maybe.withDefault []
    in
    ArticleComments.subscriptions model.articleComments articleComments
        |> Sub.map msgConfig.commentsSent


mapAuthorBarMsg : MsgConfig msg -> AuthorInteractionsBar.Msg -> msg
mapAuthorBarMsg msgConfig authorBarMsg =
    case authorBarMsg of
        AuthorInteractionsBar.NavBack ->
            msgConfig.navigateBack

        AuthorInteractionsBar.Follow pubKeyUser pubKeyToFollow ->
            msgConfig.followAuthor pubKeyUser pubKeyToFollow

        AuthorInteractionsBar.Unfollow pubKeyUser pubKeyToUnfollow ->
            msgConfig.unfollowAuthor pubKeyUser pubKeyToUnfollow

        AuthorInteractionsBar.ToggleArticleInfo ->
            msgConfig.toggleArticleInfo

        _ ->
            msgConfig.noOp


updateAddLoadedContent : String -> Model msg -> Model msg
updateAddLoadedContent url model =
    { model | loadedContent = LinkPreview.addLoadedContent model.loadedContent url }


updateArticleInteractions :
    Shared.Model
    -> InteractionButton.InteractionObject
    -> Interactions.Msg msg
    -> Model msg
    -> MsgConfig msg
    -> ( Model msg, Effect msg )
updateArticleInteractions shared interactionObject innerMsg model msgConfig =
    Interactions.update
        { browserEnv = shared.browserEnv
        , msg = innerMsg
        , model = Just model.articleInteractions
        , nostr = shared.nostr
        , interactionObject = interactionObject
        , loginStatus = shared.loginStatus
        , openCommentMsg = Nothing
        , toModel = \interactionsModel -> { model | articleInteractions = interactionsModel }
        , toMsg = msgConfig.articleInteractionsSent interactionObject
        }


updateComments :
    Shared.Model
    -> ArticleComments.Msg msg
    -> Model msg
    -> MsgConfig msg
    -> ( Model msg, Effect msg )
updateComments shared innerMsg model msgConfig =
    ArticleComments.update
        { browserEnv = shared.browserEnv
        , msg = innerMsg
        , model = model.articleComments
        , nostr = shared.nostr
        , loginStatus = shared.loginStatus
        , toModel = \articleComments -> { model | articleComments = articleComments }
        , toMsg = msgConfig.commentsSent
        , translations = shared.browserEnv.translations
        }


updateHighlights :
    Shared.Model
    -> ArticleHighlights.Msg
    -> Article
    -> Model msg
    -> MsgConfig msg
    -> ( Model msg, Effect msg )
updateHighlights shared innerMsg article model msgConfig =
    ArticleHighlights.update
        { browserEnv = shared.browserEnv
        , msg = innerMsg
        , model = model.articleHighlights
        , article = article
        , loginStatus = shared.loginStatus
        , toModel = \articleHighlights -> { model | articleHighlights = articleHighlights }
        , toMsg = msgConfig.highlightsSent
        }


updateSharingDialog :
    Shared.Model
    -> SharingButtonDialog.Msg
    -> Model msg
    -> MsgConfig msg
    -> ( Model msg, Effect msg )
updateSharingDialog shared innerMsg model msgConfig =
    SharingButtonDialog.update
        { browserEnv = shared.browserEnv
        , model = model.sharingButtonDialog
        , msg = innerMsg
        , toModel = \sharingButtonDialog -> { model | sharingButtonDialog = sharingButtonDialog }
        , toMsg = msgConfig.sharingButtonDialogMsg
        }


followAuthorEffect : PubKey -> PubKey -> Effect msg
followAuthorEffect pubKeyUser pubKeyToBeFollowed =
    SendFollowListWithPubKey pubKeyUser pubKeyToBeFollowed
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


unfollowAuthorEffect : PubKey -> PubKey -> Effect msg
unfollowAuthorEffect pubKeyUser pubKeyToBeUnfollowed =
    SendFollowListWithoutPubKey pubKeyUser pubKeyToBeUnfollowed
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


navigateBackEffect : Effect msg
navigateBackEffect =
    Effect.back


toggleArticleInfoEffect : Effect msg
toggleArticleInfoEffect =
    Effect.sendCmd Ports.toggleArticleInfo


followersEffectForAuthor : Shared.Model -> Maybe PubKey -> Effect msg
followersEffectForAuthor shared maybePubKey =
    Shared.createFollowersEffect shared.nostr maybePubKey


effectsForCachedArticle : Shared.Model -> Article -> Effect msg
effectsForCachedArticle shared article =
    Effect.batch
        [ Shared.createFollowersEffect shared.nostr (Just article.author)
        , Shared.createArticleDetailsEffect shared.nostr (Just article)
        ]


scrollToTopEffect : Effect msg
scrollToTopEffect =
    Effect.scrollContentToTop


pageTitle : ContentQueryStatus Article -> String -> String
pageTitle queryStatus fallback =
    case queryStatus of
        ContentQueryReady article ->
            Maybe.withDefault fallback article.title

        _ ->
            fallback
