module Components.ArticleComments exposing (ArticleComments, new, view)

import BrowserEnv exposing (BrowserEnv)
import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Locale exposing (Language(..))
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.Nip22 exposing (ArticleComment, ArticleCommentComment, CommentType(..))
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, LoginStatus)
import Tailwind.Utilities as Tw
import Time
import Ui.Profile
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


type ArticleComments msg
    = Settings
        { nostr : Nostr.Model
        , browserEnv : BrowserEnv
        , articleComments : List ArticleComment
        , articleCommentComments : Dict EventId (List ArticleCommentComment) -- event ID is the one of the parent comment
        , interactions : Dict EventId Interactions.Model
        , toInteractionsMsg : InteractionButton.InteractionObject -> Interactions.Msg msg -> msg
        , loginStatus : LoginStatus
        , theme : Theme
        }


new :
    { nostr : Nostr.Model
    , browserEnv : BrowserEnv
    , articleComments : List ArticleComment
    , articleCommentComments : Dict EventId (List ArticleCommentComment) -- event ID is the one of the parent comment
    , interactions : Dict EventId Interactions.Model
    , toInteractionsMsg : InteractionButton.InteractionObject -> Interactions.Msg msg -> msg
    , loginStatus : LoginStatus
    , theme : Theme
    }
    -> ArticleComments msg
new props =
    Settings
        { nostr = props.nostr
        , browserEnv = props.browserEnv
        , articleComments = props.articleComments
        , articleCommentComments = props.articleCommentComments
        , interactions = props.interactions
        , toInteractionsMsg = props.toInteractionsMsg
        , loginStatus = props.loginStatus
        , theme = props.theme
        }



-- VIEW


view : ArticleComments msg -> Html msg
view articleComments =
    let
        (Settings settings) =
            articleComments
    in
    div
        [ css
            [ Tw.self_stretch
            , Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.flex
            ]
        ]
        [ {- div
                 [ css
                     [ Tw.justify_start
                     , Tw.items_center
                     , Tw.gap_3
                     , Tw.inline_flex
                     ]
                 ]
                 [ div
                     (styles.textStyleH2 ++ styles.colorStyleGrayscaleTitle)
                     [ Html.text "Comments" ]
                 ]
             ,
          -}
          div
            [ css
                [ Tw.flex_col
                , Tw.justify_end
                , Tw.items_end
                , Tw.gap_4
                , Tw.flex
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_6
                    ]
                ]
                (settings.articleComments
                    |> sortComments
                    |> List.map (viewArticleComment articleComments 0 settings.articleCommentComments)
                )
            ]
        ]


sortComments =
    List.sortBy (\comment -> -1 * Time.posixToMillis comment.createdAt)


viewArticleComment : ArticleComments msg -> Int -> Dict EventId (List ArticleCommentComment) -> ArticleComment -> Html msg
viewArticleComment articleComments level articleCommentComments articleComment =
    let
        (Settings settings) =
            articleComments

        styles =
            stylesForTheme settings.theme

        commentsOfComment =
            articleCommentComments
                |> Dict.get articleComment.eventId
                |> Maybe.withDefault []

        followLinks =
            Nostr.isAuthor settings.nostr articleComment.pubKey

        profileDisplay =
            Nostr.getProfile settings.nostr articleComment.pubKey
                |> Maybe.map
                    (\profile ->
                        Nostr.getProfileValidationStatus settings.nostr profile.pubKey
                            |> Maybe.withDefault ValidationUnknown
                            |> Ui.Profile.viewProfileSmall styles followLinks profile
                    )
                |> Maybe.withDefault emptyHtml

        viewInteractions =
            Interactions.new
                { browserEnv = settings.browserEnv
                , model = Dict.get articleComment.eventId settings.interactions
                , toMsg = settings.toInteractionsMsg (InteractionButton.Comment articleComment.pubKey articleComment.eventId)
                , theme = settings.theme
                , interactionObject = InteractionButton.Comment articleComment.pubKey articleComment.eventId
                , nostr = settings.nostr
                , loginStatus = settings.loginStatus
                }
                |> Interactions.view
    in
    div
        [ css
            [ Tw.w_96
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.items_center
                , Tw.gap_2
                , Tw.mb_2
                ]
            ]
            [ profileDisplay
            , div
                [ css
                    []
                ]
                [ Html.text <| BrowserEnv.formatDate settings.browserEnv articleComment.createdAt
                ]
            ]
        , div
            (styles.textStyleBody
                ++ styles.colorStyleGrayscaleMuted
                ++ [ css
                        [ Tw.h_auto
                        , Tw.p_4
                        , Tw.left_0
                        , Tw.top_0
                        , Tw.text_color styles.color4
                        , Tw.bg_color styles.color1
                        , Tw.rounded_xl
                        ]
                   ]
            )
            (articleComment.content
                |> String.split "\n"
                |> List.map Html.text
                |> List.intersperse (Html.br [] [])
            )
        , viewInteractions
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.justify_start
                , Tw.items_start
                , Tw.gap_4
                ]
            ]
            (commentsOfComment
                |> sortComments
                |> List.map (viewArticleCommentComment styles settings.browserEnv 0 articleCommentComments)
            )
        ]


viewArticleCommentComment : Styles msg -> BrowserEnv -> Int -> Dict EventId (List ArticleCommentComment) -> ArticleCommentComment -> Html msg
viewArticleCommentComment styles browserEnv level articleCommentComments articleCommentComment =
    let
        commentsOfComment =
            articleCommentComments
                |> Dict.get articleCommentComment.eventId
                |> Maybe.withDefault []
    in
    div
        [ css
            [ Tw.w_96
            , Tw.h_28
            ]
        ]
        [ div
            []
            [ Html.text <| BrowserEnv.formatDate browserEnv articleCommentComment.createdAt
            ]
        , div
            (styles.textStyleBody
                ++ styles.colorStyleGrayscaleMuted
                ++ [ css
                        [ Tw.w_96
                        , Tw.h_28
                        , Tw.p_2
                        , Tw.left_0
                        , Tw.top_0
                        , Tw.bg_color styles.color1
                        , Tw.rounded_xl
                        ]
                   ]
            )
            [ Html.text articleCommentComment.content
            , div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    ]
                ]
                (commentsOfComment
                    |> List.map (viewArticleCommentComment styles browserEnv (level + 1) articleCommentComments)
                )
            ]
        ]
