module Components.AuthorInteractionsBar exposing (..)

import Components.Button as Button
import Components.Icon as Icon
import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import FeatherIcons
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Nostr
import Nostr.Article exposing (Article, nip19ForArticle)
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Relay exposing (websocketUrl)
import Set
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Translations.ArticleView as Translations
import Ui.Article exposing (ArticlePreviewsData, sharingInfoForArticle, viewInteractions, viewProfilePubKey)
import Ui.Interactions
import Ui.Profile exposing (viewProfileSmall)
import Ui.Styles exposing (Theme(..), darkMode, print)


type alias Model =
    { articleInfoToggle : Bool }


type Msg
    = ToggleArticleInfo Bool


type AuthorInteractionsBar msg
    = Settings
        { articlePreviewsData : ArticlePreviewsData msg
        , interactionsModel : Interactions.Model
        , article : Article
        }


new :
    { articlePreviewsData : ArticlePreviewsData msg
    , interactionsModel : Interactions.Model
    , article : Article
    }
    -> AuthorInteractionsBar msg
new props =
    Settings props


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleArticleInfo flag ->
            { model | articleInfoToggle = flag }


view : AuthorInteractionsBar msg -> Model -> Html msg
view (Settings { articlePreviewsData, interactionsModel, article }) model =
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

        articleInfoToggle =
            div
                [ css
                    [ darkMode [ Tw.text_color styles.colorB4DarkMode ]
                    , Tw.text_color styles.colorB4
                    , Bp.lg [ Tw.hidden ]
                    , Tw.block
                    ]

                --, Events.onClick (ToggleArticleInfo (not model.articleInfoToggle))
                ]
                [ if model.articleInfoToggle == True then
                    Icon.FeatherIcon FeatherIcons.bookOpen |> Icon.view

                  else
                    Icon.FeatherIcon FeatherIcons.info |> Icon.view
                ]

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
            [ viewInteractions previewData "1"
            , articleInfoToggle
            ]
        , div [ css [ Tw.absolute, Tw.right_0, Tw.mr_4 ] ]
            [ Button.new
                { label = Translations.followAuthor [ articlePreviewsData.browserEnv.translations ]
                , onClick = Nothing
                , theme = articlePreviewsData.theme
                }
                |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.plus)
                |> Button.view
            ]
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
            [ Interactions.CommentButtonElement Nothing
            , Interactions.LikeButtonElement
            , Interactions.RepostButtonElement
            , Interactions.ZapButtonElement instanceId previewData.zapRelays
            , Interactions.BookmarkButtonElement
            , Interactions.ShareButtonElement previewData.sharingInfo
            ]
        |> Interactions.view
