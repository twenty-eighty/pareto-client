module Ui.ContentQuery exposing (viewStatus, viewNoteStatus)

{-| Shared loading / not-found UI for single-content queries (articles, notes).
-}

import Components.ContentStatus exposing (Status(..))
import Css
import Css.Animations as Animations
import Html.Styled as Html exposing (Html, div, h3, span, text)
import Html.Styled.Attributes exposing (css)
import I18Next
import Nostr
import Nostr.Query exposing (ContentLoadPhase(..), ContentQueryStatus(..))
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.RelayStatusComponent as StatusTranslations
import Ui.Styles
import Ui.View exposing (viewContentStatus)


viewStatus : Ui.Styles.Theme -> I18Next.Translations -> Nostr.Model -> ContentQueryStatus a -> Html msg
viewStatus theme translations nostr status =
    case status of
        ContentQueryReady _ ->
            text ""

        ContentQueryLoading phase ->
            viewLoading theme translations phase True

        ContentQueryNotFound ->
            viewContentStatus theme translations nostr ArticleNotFound Nothing

        ContentQueryFailed _ ->
            viewContentStatus theme translations nostr ArticleLoadFailed Nothing


{-| Note queries never need the NIP-05 author step.
-}
viewNoteStatus : Ui.Styles.Theme -> I18Next.Translations -> Nostr.Model -> ContentQueryStatus a -> Html msg
viewNoteStatus theme translations nostr status =
    case status of
        ContentQueryReady _ ->
            text ""

        ContentQueryLoading _ ->
            viewLoading theme translations FetchingContent False

        ContentQueryNotFound ->
            viewContentStatus theme translations nostr NoteNotFound Nothing

        ContentQueryFailed _ ->
            viewContentStatus theme translations nostr NoteLoadFailed Nothing


viewLoading : Ui.Styles.Theme -> I18Next.Translations -> ContentLoadPhase -> Bool -> Html msg
viewLoading theme translations phase showAuthorStep =
    let
        styles =
            Ui.Styles.stylesForTheme theme

        headline =
            case phase of
                ResolvingAuthor ->
                    StatusTranslations.resolvingAuthor [ translations ]

                FetchingContent ->
                    if showAuthorStep then
                        StatusTranslations.loadingArticle [ translations ]

                    else
                        StatusTranslations.loadingNote [ translations ]
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_6
            , Tw.m_4
            , Tw.max_w_md
            ]
        ]
        [ h3
            (styles.textStyleH3 ++ styles.colorStyleGrayscaleTitle)
            [ text headline ]
        , if showAuthorStep then
            viewLoadSteps styles translations phase

          else
            viewLoadStep styles
                { label = StatusTranslations.loadingNote [ translations ]
                , done = False
                , active = True
                }
        ]


viewLoadSteps : Ui.Styles.Styles msg -> I18Next.Translations -> ContentLoadPhase -> Html msg
viewLoadSteps styles translations phase =
    let
        authorDone =
            phase == FetchingContent

        authorActive =
            phase == ResolvingAuthor

        contentActive =
            phase == FetchingContent
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_3
            ]
        ]
        [ viewLoadStep styles
            { label = StatusTranslations.stepResolveAuthor [ translations ]
            , done = authorDone
            , active = authorActive
            }
        , viewLoadStep styles
            { label = StatusTranslations.stepLoadArticle [ translations ]
            , done = False
            , active = contentActive
            }
        ]


viewLoadStep : Ui.Styles.Styles msg -> { label : String, done : Bool, active : Bool } -> Html msg
viewLoadStep styles { label, done, active } =
    let
        ( marker, markerColor, textColorAttrs ) =
            if done then
                ( "✓"
                , Theme.green_500
                , styles.colorStyleGrayscaleText
                )

            else if active then
                ( "●"
                , Theme.blue_500
                , styles.colorStyleGrayscaleTitle
                )

            else
                ( "○"
                , Theme.gray_400
                , styles.colorStyleGrayscaleMuted
                )

        markerAnimation =
            if active then
                [ Css.animationName
                    (Animations.keyframes
                        [ ( 0, [ Animations.opacity (Css.num 0.35) ] )
                        , ( 50, [ Animations.opacity (Css.num 1) ] )
                        , ( 100, [ Animations.opacity (Css.num 0.35) ] )
                        ]
                    )
                , Css.animationDuration (Css.ms 1200)
                , Css.property "animation-iteration-count" "infinite"
                ]

            else
                []
    in
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.gap_3
            ]
        ]
        [ span
            [ css
                ([ Tw.inline_flex
                 , Tw.items_center
                 , Tw.justify_center
                 , Tw.w_6
                 , Tw.h_6
                 , Tw.text_sm
                 , Tw.font_semibold
                 , Tw.text_color markerColor
                 ]
                    ++ markerAnimation
                )
            ]
            [ text marker ]
        , span
            (css [ Tw.text_sm ] :: textColorAttrs)
            [ text label ]
        ]
