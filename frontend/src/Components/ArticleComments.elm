module Components.ArticleComments exposing (ArticleComments, new, view)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Locale exposing (Language(..))
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.Nip22 exposing (ArticleComment, ArticleCommentComment, CommentType(..))
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId)
import Shared.Model exposing (LoginStatus(..))
import Tailwind.Utilities as Tw
import Time
import Ui.Profile
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


type ArticleComments
    = Settings
        { nostr : Nostr.Model
        , browserEnv : BrowserEnv
        , articleComments : List ArticleComment
        , articleCommentComments : Dict EventId (List ArticleCommentComment) -- event ID is the one of the parent comment
        , theme : Theme
        }


new :
    { nostr : Nostr.Model
    , browserEnv : BrowserEnv
    , articleComments : List ArticleComment
    , articleCommentComments : Dict EventId (List ArticleCommentComment) -- event ID is the one of the parent comment
    , theme : Theme
    }
    -> ArticleComments
new props =
    Settings
        { nostr = props.nostr
        , browserEnv = props.browserEnv
        , articleComments = props.articleComments
        , articleCommentComments = props.articleCommentComments
        , theme = props.theme
        }



-- VIEW


view : ArticleComments -> Html msg
view articleComments =
    let
        (Settings settings) =
            articleComments
    in
    viewArticleComments settings.theme settings.browserEnv settings.nostr settings.articleComments settings.articleCommentComments


viewArticleComments : Theme -> BrowserEnv -> Nostr.Model -> List ArticleComment -> Dict EventId (List ArticleCommentComment) -> Html msg
viewArticleComments theme browserEnv nostr articleComments articleCommentComments =
    let
        styles =
            stylesForTheme theme
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
                (articleComments
                    |> sortComments
                    |> List.map (viewArticleComment styles browserEnv nostr 0 articleCommentComments)
                )
            ]
        ]


sortComments =
    List.sortBy (\comment -> -1 * Time.posixToMillis comment.createdAt)


viewArticleComment : Styles msg -> BrowserEnv -> Nostr.Model -> Int -> Dict EventId (List ArticleCommentComment) -> ArticleComment -> Html msg
viewArticleComment styles browserEnv nostr _ articleCommentComments articleComment =
    let
        commentsOfComment =
            articleCommentComments
                |> Dict.get articleComment.eventId
                |> Maybe.withDefault []

        followLinks =
            Nostr.isAuthor nostr articleComment.pubKey

        profileDisplay =
            Nostr.getProfile nostr articleComment.pubKey
                |> Maybe.map
                    (\profile ->
                        Nostr.getProfileValidationStatus nostr profile.pubKey
                            |> Maybe.withDefault ValidationUnknown
                            |> Ui.Profile.viewProfileSmall styles followLinks profile
                    )
                |> Maybe.withDefault emptyHtml
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
                [ Html.text <| BrowserEnv.formatDate browserEnv articleComment.createdAt
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
                |> List.map (viewArticleCommentComment styles browserEnv 0 articleCommentComments)
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
