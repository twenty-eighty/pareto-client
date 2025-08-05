module Components.AuthorInteractionsBar exposing (..)

import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Nostr
import Nostr.Article exposing (Article, nip19ForArticle)
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Relay exposing (websocketUrl)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (Following(..), PubKey, loggedInPubKey)
import Set
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Ui.Article exposing (ArticlePreviewsData, sharingInfoForArticle, viewInteractions, viewProfilePubKey)
import Ui.Interactions
import Ui.Profile exposing (FollowType(..), followButton, viewProfileSmall)
import Ui.Styles exposing (Theme(..), darkMode, print)


type alias Model =
    { articleInfoToggle : Bool }


type Msg
    = Follow PubKey PubKey
    | Unfollow PubKey PubKey
    | ToggleArticleInfo Bool


type AuthorInteractionsBar msg
    = Settings
        { articlePreviewsData : ArticlePreviewsData msg
        , interactionsModel : Interactions.Model
        , article : Article
        , toMsg : Msg -> msg
        }


new :
    { articlePreviewsData : ArticlePreviewsData msg
    , interactionsModel : Interactions.Model
    , article : Article
    , toMsg : Msg -> msg
    }
    -> AuthorInteractionsBar msg
new props =
    Settings props


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Follow pubKeyUser pubKeyToBeFollowed ->
            ( model
            , SendFollowListWithPubKey pubKeyUser pubKeyToBeFollowed
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        Unfollow pubKeyUser pubKeyToBeUnfollowed ->
            ( model
            , SendFollowListWithoutPubKey pubKeyUser pubKeyToBeUnfollowed
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        ToggleArticleInfo flag ->
            ( { model | articleInfoToggle = flag }, Effect.none )


view : AuthorInteractionsBar msg -> Model -> Html msg
view (Settings { articlePreviewsData, interactionsModel, article, toMsg }) model =
    let
        styles =
            Ui.Styles.stylesForTheme articlePreviewsData.theme

        getProfile =
            Nostr.getProfile articlePreviewsData.nostr

        maybeProfile =
            getProfile article.author

        validationStatus =
            Nostr.getProfileValidationStatus articlePreviewsData.nostr article.author
                |> Maybe.withDefault ValidationUnknown

        interactionObject =
            InteractionButton.Article article.id ( article.kind, article.author, article.identifier |> Maybe.withDefault "" )

        articleRelays =
            article.relays
                |> Set.map websocketUrl

        author =
            Nostr.getAuthor articlePreviewsData.nostr article.author

        previewData : Ui.Interactions.PreviewData msg
        previewData =
            { browserEnv = articlePreviewsData.browserEnv
            , loginStatus = articlePreviewsData.loginStatus
            , maybeNip19Target = nip19ForArticle article
            , zapRelays = articleRelays
            , interactionsModel = interactionsModel
            , interactionObject = interactionObject
            , toInteractionsMsg = articlePreviewsData.articleToInteractionsMsg interactionObject
            , nostr = articlePreviewsData.nostr
            , sharing = articlePreviewsData.sharing
            , sharingInfo = sharingInfoForArticle article author
            , translations = articlePreviewsData.browserEnv.translations
            , theme = articlePreviewsData.theme
            }

        authorFollowButton =
            createAuthorFollowButton articlePreviewsData article toMsg
    in
    div
        [ css
            [ Tw.relative
            , Tw.flex
            , Bp.lg [ Tw.flex_row ]
            , Tw.flex_col
            , Tw.items_start
            , Tw.gap_5
            , Tw.p_3
            , Tw.bg_color styles.colorG5
            , darkMode [ Tw.bg_color styles.colorG2 ]
            , print [ Tw.hidden ]
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                ]
            ]
            [ div [] [ text "<--" ]
            , maybeProfile
                |> Maybe.map (\profile -> viewProfileSmall articlePreviewsData.browserEnv.environment styles True profile validationStatus)
                |> Maybe.withDefault (viewProfilePubKey articlePreviewsData.browserEnv.environment articlePreviewsData.browserEnv.translations article.author)
            ]
        , div
            [ css
                [ Bp.lg [ Tw.absolute, Tw.right_48, Tw.mr_4 ] ]
            ]
            [ viewInteractions previewData "1" ]
        , div [ css [ Tw.absolute, Tw.right_0, Tw.mr_4 ] ]
            [ authorFollowButton ]
        ]


viewInteractions : Ui.Interactions.PreviewData msg -> String -> Html msg
viewInteractions previewData instanceId =
    Interactions.new
        { browserEnv = previewData.browserEnv
        , model = Just previewData.interactionsModel
        , toMsg = previewData.toInteractionsMsg
        , theme = previewData.theme
        , interactionObject = previewData.interactionObject
        , nostr = previewData.nostr
        , loginStatus = previewData.loginStatus
        , showLabel = False
        }
        |> Interactions.withInteractionElements
            [ Interactions.LikeButtonElement
            , Interactions.ZapButtonElement instanceId previewData.zapRelays
            , Interactions.RepostButtonElement
            , Interactions.ShareButtonElement previewData.sharingInfo
            , Interactions.BookmarkButtonElement
            ]
        |> Interactions.view


followingProfile : Nostr.Model -> PubKey -> Maybe PubKey -> FollowType Msg
followingProfile nostr profilePubKey maybePubKey =
    case maybePubKey of
        Just userPubKey ->
            Nostr.getFollowsList nostr userPubKey
                |> Maybe.andThen
                    (\followList ->
                        followList
                            |> List.filterMap
                                (\following ->
                                    case following of
                                        FollowingPubKey { pubKey } ->
                                            if profilePubKey == pubKey then
                                                Just (Following (Unfollow userPubKey))

                                            else
                                                Nothing

                                        FollowingHashtag _ ->
                                            Nothing
                                )
                            |> List.head
                    )
                |> Maybe.withDefault (NotFollowing (Follow userPubKey))

        Nothing ->
            UnknownFollowing


createAuthorFollowButton : ArticlePreviewsData msg -> Article -> (Msg -> msg) -> Html msg
createAuthorFollowButton articleData article toMsg =
    let
        followType =
            mapFollowType toMsg (followingProfile articleData.nostr article.author (loggedInPubKey articleData.loginStatus))
    in
    followButton articleData.theme articleData.browserEnv article.author followType


mapFollowType : (Msg -> msg) -> FollowType Msg -> FollowType msg
mapFollowType toMsg followType =
    case followType of
        Following msgConstructor ->
            Following (\pubKey -> toMsg (msgConstructor pubKey))

        NotFollowing msgConstructor ->
            NotFollowing (\pubKey -> toMsg (msgConstructor pubKey))

        UnknownFollowing ->
            UnknownFollowing
