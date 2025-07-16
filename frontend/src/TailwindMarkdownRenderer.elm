module TailwindMarkdownRenderer exposing (renderer)

import BrowserEnv exposing (Environment)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css, src)
import LinkPreview
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer
import Nostr.Nip27 exposing (GetProfileFunction, subsituteNostrLinks)
import Nostr.Shared exposing (ensureHttps)
import Parser
import SyntaxHighlight
import Tailwind.Utilities as Tw
import Ui.Links
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme(..), darkMode, fontFamilyRobotoMono, print, stylesForTheme)


textStyleArticleCode : List (Html.Attribute msg)
textStyleArticleCode =
    [ css
        [ Tw.text_xs
        , Tw.font_normal
        , Tw.capitalize
        , Tw.leading_tight
        ]
    , fontFamilyRobotoMono
    ]


renderer : Environment -> Styles msg -> GetProfileFunction -> Markdown.Renderer.Renderer (Html msg)
renderer environment styles fnGetProfile =
    { heading = heading styles
    , paragraph =
        Html.p
            (styles.textStyleBody
                ++ styles.colorStyleGrayscaleText
                ++ [ css
                        [ Tw.mb_6
                        , Tw.text_justify
                        , Tw.hyphens_auto
                        , Css.property "overflow-wrap" "break-word"

                        -- , Css.property "word-break" "break-word"
                        , print
                            [ Tw.break_inside_avoid_page
                            ]
                        ]
                   ]
            )
    , thematicBreak =
        Html.hr
            [ css
                [ Tw.my_14
                ]
            ]
            []
    , text = subsituteNostrLinks styles fnGetProfile
    , strong = \content -> Html.strong [ css [ Tw.font_bold ] ] content
    , emphasis = \content -> Html.em [ css [ Tw.italic ] ] content
    , blockQuote =
        Html.blockquote
            [ css
                [ Tw.border_s_2
                , Tw.ps_4
                , Tw.font_semibold
                , Tw.italic
                ]
            ]
    , codeSpan =
        \content ->
            Html.code
                (textStyleArticleCode ++ styles.colorStyleCode)
                [ Html.text content ]

    --, codeSpan = code
    , link =
        formatLink styles
    , hardLineBreak = Html.br [] []
    , image =
        \image ->
            let
                imagesrc =
                    image.src
                        |> ensureHttps
                        |> Ui.Links.scaledImageLink environment 650
            in
            case ( image.title, image.src ) of
                ( _, "" ) ->
                    -- ignore images without src attribute
                    emptyHtml

                ( Just "1.00", _ ) ->
                    -- dirty fix - route96 server delivers caption as "1.00" even if it wasn't set explicitly
                    Html.node "center"
                        []
                        [ Html.img
                            [ Attr.src imagesrc
                            , Attr.alt image.alt
                            , css
                                [ Tw.max_h_96
                                ]
                            ]
                            []
                        ]

                ( Just title, _ ) ->
                    Html.node "center"
                        []
                        [ Html.figure
                            [ css
                                []
                            ]
                            [ Html.img
                                [ Attr.src imagesrc
                                , Attr.alt image.alt
                                ]
                                []
                            , Html.figcaption
                                []
                                [ Html.text title ]
                            ]
                        ]

                ( Nothing, _ ) ->
                    Html.node "center"
                        []
                        [ Html.img
                            [ Attr.src imagesrc
                            , Attr.alt image.alt
                            , css
                                [ Tw.max_h_96
                                ]
                            ]
                            []
                        ]
    , unorderedList =
        \items ->
            Html.ul (styles.textStyleBody ++ styles.colorStyleGrayscaleText)
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li
                                        [ css
                                            [ Tw.list_disc
                                            , Tw.m_6
                                            , Tw.text_justify
                                            , Tw.hyphens_auto
                                            ]
                                        ]
                                        (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        styles.textStyleBody
                            ++ styles.colorStyleGrayscaleText
                            ++ [ Attr.start startingIndex
                               , css
                                    [ Tw.list_decimal
                                    , Tw.ps_6
                                    ]
                               ]

                    _ ->
                        styles.textStyleBody
                            ++ styles.colorStyleGrayscaleText
                            ++ [ css
                                    [ Tw.list_decimal
                                    , Tw.ps_6
                                    ]
                               ]
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li
                                [ css
                                    [ Tw.ps_2
                                    , Tw.my_3
                                    ]
                                ]
                                itemBlocks
                        )
                )
    , html = htmlBlock
    , codeBlock = codeBlock

    --\{ body, language } ->
    --    let
    --        classes =
    --            -- Only the first word is used in the class
    --            case Maybe.map String.words language of
    --                Just (actualLanguage :: _) ->
    --                    [ Attr.class <| "language-" ++ actualLanguage ]
    --
    --                _ ->
    --                    []
    --    in
    --    Html.pre []
    --        [ Html.code classes
    --            [ Html.text body
    --            ]
    --        ]
    , table =
        Html.table
            [ {-
                 table-layout: auto;
                     text-align: left;
                     width: 100%;
                     margin-top: 2em;
                     margin-bottom: 2em;
              -}
              css
                [--Tw.table_auto
                 --, Tw.w_full
                 --, Tw.mt_4
                 --, Tw.mb_4
                ]
            ]
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , strikethrough =
        \children -> Html.del [] children
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.td attrs
    }


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


heading : Styles msg -> { level : Block.HeadingLevel, rawText : String, children : List (Html msg) } -> Html msg
heading styles { level, rawText, children } =
    case level of
        Block.H1 ->
            Html.h1
                (styles.textStyleH1Article
                    ++ styles.colorStyleGrayscaleText
                    ++ [ css
                            [ Tw.mt_2
                            , Tw.mb_4
                            ]
                       ]
                )
                children

        Block.H2 ->
            Html.h2
                (styles.textStyleH2
                    ++ styles.colorStyleGrayscaleText
                    ++ [ Attr.id (rawTextToId rawText)
                       , Attr.attribute "name" (rawTextToId rawText)
                       , css
                            [ Tw.mt_10
                            , Tw.pb_1
                            , Tw.border_b
                            ]
                       ]
                )
                [ Html.a
                    [ Attr.href <| "#" ++ rawTextToId rawText
                    , css
                        [ Tw.no_underline |> Css.important
                        ]
                    ]
                    children
                ]

        Block.H3 ->
            Html.h3
                (styles.textStyleH3
                    ++ styles.colorStyleGrayscaleText
                    ++ [ css
                            [ Tw.mt_10
                            , Tw.mb_4
                            ]
                       ]
                )
                children

        _ ->
            (case level of
                Block.H1 ->
                    Html.h1

                Block.H2 ->
                    Html.h2

                Block.H3 ->
                    Html.h3

                Block.H4 ->
                    Html.h4

                Block.H5 ->
                    Html.h5

                Block.H6 ->
                    Html.h6
            )
                [ css
                    [ Tw.font_bold
                    , Tw.text_lg
                    , Tw.mt_8
                    , Tw.mb_4
                    ]
                ]
                children


htmlBlock : Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlBlock =
    Markdown.Html.oneOf
        [ htmlAElement
        , htmlCiteElement
        , htmlIframeElement
        , htmlImgElement
        , htmlPElement
        , htmlStrongElement
        , htmlGenericElement "col"
        , htmlGenericElement "colgroup"
        , htmlGenericElement "table"
        , htmlGenericElement "tbody"
        , htmlGenericElement "td"
        , htmlGenericElement "tr"
        ]


htmlIframeElement : Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlIframeElement =
    Markdown.Html.tag "iframe"
        (\src maybeTitle _ _ _ _ _ _ ->
            renderHtmlIframeElement src maybeTitle
        )
        |> Markdown.Html.withAttribute "src"
        |> Markdown.Html.withOptionalAttribute "title"
        |> Markdown.Html.withOptionalAttribute "width"
        |> Markdown.Html.withOptionalAttribute "height"
        |> Markdown.Html.withOptionalAttribute "frameborder"
        |> Markdown.Html.withOptionalAttribute "allow"
        |> Markdown.Html.withOptionalAttribute "allowfullscreen"
        |> Markdown.Html.withOptionalAttribute "referrerpolicy"


htmlImgElement : Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlImgElement =
    Markdown.Html.tag "img"
        (\src maybeAlt ->
            renderHtmlImgElement src maybeAlt
        )
        |> Markdown.Html.withAttribute "src"
        |> Markdown.Html.withOptionalAttribute "alt"


htmlAElement : Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlAElement =
    Markdown.Html.tag "a"
        (\maybeHref ->
            renderHtmlAElement maybeHref
        )
        |> Markdown.Html.withOptionalAttribute "href"


htmlCiteElement : Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlCiteElement =
    Markdown.Html.tag "cite"
        (\maybeSrc ->
            renderHtmlCiteElement maybeSrc
        )
        |> Markdown.Html.withOptionalAttribute "src"


htmlPElement : Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlPElement =
    Markdown.Html.tag "p" (\children -> Html.p [] children)


htmlStrongElement : Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlStrongElement =
    Markdown.Html.tag "strong" (\children -> Html.strong [] children)


htmlGenericElement : String -> Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlGenericElement tagName =
    Markdown.Html.tag tagName (\children -> Html.div [] children)


renderHtmlAElement : Maybe String -> (List (Html msg) -> Html msg)
renderHtmlAElement maybeHref children =
    let
        srcAttr =
            maybeHref
                |> Maybe.map (\href -> [ Attr.href href ])
                |> Maybe.withDefault []
    in
    Html.a
        (css
            []
            :: srcAttr
        )
        children


renderHtmlCiteElement : Maybe String -> (List (Html msg) -> Html msg)
renderHtmlCiteElement maybeSrc children =
    let
        srcAttr =
            maybeSrc
                |> Maybe.map (\src -> [ Attr.src src ])
                |> Maybe.withDefault []
    in
    Html.cite
        (css
            []
            :: srcAttr
        )
        children


renderHtmlIframeElement : String -> Maybe String -> (List (Html msg) -> Html msg)
renderHtmlIframeElement src _ children =
    LinkPreview.generatePreviewHtml Nothing src [] children


renderHtmlImgElement : String -> Maybe String -> (List (Html msg) -> Html msg)
renderHtmlImgElement src maybeAlt children =
    let
        altAttr =
            maybeAlt
                |> Maybe.map (\alt -> [ Attr.alt alt ])
                |> Maybe.withDefault [ Attr.alt "Image in article" ]
    in
    Html.img
        -- don't reference unsafe (http) resources
        -- as this might display our site as unsafe
        ([ Attr.src (ensureHttps src)
         , css
            [ Tw.max_h_96
            ]
         ]
            ++ altAttr
        )
        children


codeBlock : { body : String, language : Maybe String } -> Html msg
codeBlock details =
    case details.language of
        Just language ->
            codeParsingFunction language details.body
                |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                |> Result.map Html.fromUnstyled
                |> Result.withDefault (defaultFormatCodeBlock details.body)

        Nothing ->
            defaultFormatCodeBlock details.body


codeParsingFunction : String -> (String -> Result (List Parser.DeadEnd) SyntaxHighlight.HCode)
codeParsingFunction language =
    case language of
        "css" ->
            SyntaxHighlight.css

        "elm" ->
            SyntaxHighlight.elm

        "javascript" ->
            SyntaxHighlight.javascript

        "python" ->
            SyntaxHighlight.python

        "sql" ->
            SyntaxHighlight.sql

        "xml" ->
            SyntaxHighlight.xml

        "json" ->
            SyntaxHighlight.json

        "nix" ->
            SyntaxHighlight.nix

        _ ->
            SyntaxHighlight.noLang


defaultFormatCodeBlock : String -> Html msg
defaultFormatCodeBlock body =
    let
        styles =
            stylesForTheme ParetoTheme
    in
    Html.pre
        [ css
            [ Tw.bg_scroll
            , Tw.overflow_x_auto
            , Tw.max_w_full
            , Tw.p_3
            , Tw.rounded_2xl
            , Tw.text_sm
            , Tw.text_color styles.colorB1
            , Tw.bg_color styles.colorB4
            , darkMode
                [ Tw.text_color styles.colorB4DarkMode
                , Tw.bg_color styles.colorB2DarkMode
                ]
            , Tw.mb_3
            ]
        ]
        [ Html.code [] [ Html.text body ] ]


formatLink : Styles msg -> { title : Maybe String, destination : String } -> List (Html msg) -> Html msg
formatLink styles { destination } body =
    Html.a
        (styles.colorStyleLinks
            ++ styles.textStyleLinks
            ++ [ Attr.href destination
               , Attr.rel "nofollow"
               ]
        )
        body
