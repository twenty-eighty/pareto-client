module TailwindMarkdownRenderer exposing (renderer)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr exposing (css)
import LinkPreview
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer
import Nostr.Nip27 exposing (subsituteNostrLinks)
import SyntaxHighlight
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Styles exposing (Styles)


renderer : Styles msg -> Markdown.Renderer.Renderer (Html.Html msg)
renderer styles =
    { heading = heading styles
    , paragraph =
        Html.p
            (styles.textStyleBody ++ styles.colorStyleGrayscaleText ++
            [ css
                [ Tw.mb_6
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
    , text = (Html.div []) << subsituteNostrLinks
    , strong = \content -> Html.strong [ css [ Tw.font_bold ] ] content
    , emphasis = \content -> Html.em [ css [ Tw.italic ] ] content
    , blockQuote = Html.blockquote []
    , codeSpan =
        \content ->
            Html.code
                (styles.textStyleArticleCode ++ styles.colorStyleArticleHashtags)
                [ Html.text content ]

    --, codeSpan = code
    , link =
        formatLink styles
    , hardLineBreak = Html.br [] []
    , image =
        \image ->
            case image.title of
                Just _ ->
                    Html.img [ Attr.src image.src, Attr.alt image.alt ] []

                Nothing ->
                    Html.img [ Attr.src image.src, Attr.alt image.alt ] []
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
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        (styles.textStyleBody ++ styles.colorStyleGrayscaleText ++
                        [ Attr.start startingIndex
                        , css
                            [ Tw.list_decimal
                            , Tw.ps_6
                            ]
                        ]
                        )

                    _ ->
                        (styles.textStyleBody ++ styles.colorStyleGrayscaleText ++
                        [ css
                            [ Tw.list_decimal
                            , Tw.ps_6
                            ]
                        ])
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


heading : Styles msg -> { level : Block.HeadingLevel, rawText : String, children : List (Html.Html msg) } -> Html.Html msg
heading styles { level, rawText, children } =
    case level of
        Block.H1 ->
            Html.h1
                (styles.textStyleH1Article ++ styles.colorStyleGrayscaleText ++
                [ css
                    [ Tw.mt_2
                    , Tw.mb_4
                    ]
                ])
                children

        Block.H2 ->
            Html.h2
                (styles.textStyleH2 ++ styles.colorStyleGrayscaleText ++
                [ Attr.id (rawTextToId rawText)
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
                (styles.textStyleH3 ++ styles.colorStyleGrayscaleText ++
                [ css
                    [ Tw.mt_10
                    , Tw.mb_4
                    ]
                ])
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



--code : String -> Element msg
--code snippet =
--    Element.el
--        [ Element.Background.color
--            (Element.rgba255 50 50 50 0.07)
--        , Element.Border.rounded 2
--        , Element.paddingXY 5 3
--        , Font.family [ Font.typeface "Roboto Mono", Font.monospace ]
--        ]
--        (Element.text snippet)
--
--

htmlBlock : Markdown.Html.Renderer view
htmlBlock =
    Markdown.Html.oneOf []

codeBlock : { body : String, language : Maybe String } -> Html.Html msg
codeBlock details =
    SyntaxHighlight.elm details.body
        |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
        |> Result.map Html.fromUnstyled
        |> Result.withDefault (Html.pre [] [ Html.code [] [ Html.text details.body ] ])


formatLink : Styles msg -> { title: Maybe String, destination : String } -> List (Html.Html msg) -> Html.Html msg
formatLink styles { destination } body =
    LinkPreview.generatePreviewHtml
        destination
        (styles.textStyleLinks ++ styles.colorStyleArticleHashtags ++ [ css [ Tw.underline ] ])
        body