module Markdown exposing (collectImageUrls, collectText, markdownViewHtml, summaryFromContent)

-- import Html exposing (Attribute, Html)

import BrowserEnv exposing (Environment)
import Html.Styled as Html exposing (..)
import LinkPreview exposing (LoadedContent)
import Markdown.Block exposing (Block(..), ListItem(..), Task(..))
import Markdown.Parser
import Markdown.Renderer as Renderer
import Nostr.Nip27 exposing (GetProfileFunction)
import Parser
import Parser.Advanced as Advanced
import Regex exposing (Regex)
import TailwindMarkdownRenderer
import Ui.Styles exposing (Styles)


summaryFromContent : String -> Maybe String
summaryFromContent markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen
            (\blocks ->
                firstMarkdownText blocks
            )
        |> Result.toMaybe


firstMarkdownText : List Markdown.Block.Block -> Result String String
firstMarkdownText blocks =
    blocks
        |> List.filterMap filterParagraphs
        |> List.head
        |> Maybe.andThen
            (\firstInlines ->
                firstInlines
                    |> List.filterMap filterFirstText
                    |> List.head
            )
        |> Result.fromMaybe "No text found"


filterParagraphs : Markdown.Block.Block -> Maybe (List Markdown.Block.Inline)
filterParagraphs block =
    case block of
        Markdown.Block.Paragraph inlineList ->
            Just inlineList

        _ ->
            Nothing


filterFirstText : Markdown.Block.Inline -> Maybe String
filterFirstText inline =
    case inline of
        Markdown.Block.Text str ->
            Just str

        Markdown.Block.Strong inlines ->
            inlines
                |> List.filterMap filterFirstText
                |> List.head

        Markdown.Block.Emphasis inlines ->
            inlines
                |> List.filterMap filterFirstText
                |> List.head

        _ ->
            Nothing



-- Replace HTML img tags with Markdown-style image links, including alt text if present
-- the <img> elements found in Markdown may contain no end tag which breaks parsing otherwise


replaceImgTags : String -> String
replaceImgTags input =
    let
        -- Regex to match <img src="..." alt="..."> tags
        imgRegex : Maybe Regex
        imgRegex =
            Regex.fromString "<img src=\\\"([^\"]+)\\\"(?: alt=\\\"([^\"]*)\\\")?(?: width=\\\"([^\"]*)\\\")?>"

        -- Function to replace matched strings with Markdown-style image links
        replacer : Regex.Match -> String
        replacer match =
            case match.submatches of
                [ Just url, Just altText, _ ] ->
                    "![" ++ altText ++ "](" ++ url ++ ")"

                [ Just url, Nothing, _ ] ->
                    "![](" ++ url ++ ")"

                _ ->
                    ""
    in
    case imgRegex of
        Just regex ->
            Regex.replace regex replacer input

        Nothing ->
            input



-- Replace broken HTML col tags from Primal


replaceBrokenColTag : String -> String
replaceBrokenColTag input =
    String.replace "<colgroup><col></colgroup>" "<colgroup><col></col></colgroup>" input



-- A regex to match http:// or https:// followed by one or more non-whitespace chars
--   urlRegex : Regex
--   urlRegex =
--       Regex.fromString "(https?://[^\\s]+)"
--           |> Maybe.withDefault Regex.never
-- Replace all HTTP(S) URLs with Markdown links.
-- If no match exists, the string is unchanged.
--   substituteHttpLinks : String -> String
--   substituteHttpLinks text =
--       Regex.replace urlRegex
--           (\match -> "[" ++ match.match ++ "](" ++ match.match ++ ")")
--           text


deadEndsToString : List (Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"


markdownViewHtml : Environment -> Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> String -> Result String (Html msg)
markdownViewHtml environment styles loadedContent fnGetProfile markdown =
    render environment styles loadedContent fnGetProfile markdown
        |> Result.map elementFromHtmlList


render : Environment -> Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> String -> Result String (List (Html msg))
render environment styles loadedContent fnGetProfile markdown =
    markdown
        |> replaceImgTags
        |> replaceBrokenColTag
        |> Markdown.Parser.parse
        |> Result.map determineBlockTypes
        |> Result.mapError deadEndsToString
        |> Result.andThen
            (\ast ->
                ast
                    |> Renderer.renderWithMeta
                        (\blockType ->
                            rendererForBlockType environment styles loadedContent fnGetProfile blockType
                        )
            )

filterImageUrls : List Block -> List String
filterImageUrls blocks =
    blocks
        |> List.map
            (\block ->
                case block of
                    HtmlBlock _ ->
                        []

                    UnorderedList _ listItems ->
                        listItems 
                        |> List.map (\(ListItem _ listItemBlocks) -> filterImageUrls listItemBlocks)
                        |> List.concat

                    OrderedList _ _ listItems ->
                        listItems 
                        |> List.map filterImageUrls
                        |> List.concat

                    BlockQuote quoteBlocks ->
                        filterImageUrls quoteBlocks

                    Heading _ inlines ->
                        extractInlineImageUrls inlines

                    Paragraph inlines ->
                        extractInlineImageUrls inlines

                    Table columnHeaders tableBody ->
                        let
                            headerUrls =
                                columnHeaders
                                |> List.map (\{ label } -> extractInlineImageUrls label)
                                |> List.concat

                            bodyUrls =
                                tableBody
                                |> List.map (\row ->
                                        row
                                        |> List.map extractInlineImageUrls
                                        |> List.concat
                                    )
                                    |> List.concat
                        in
                        headerUrls ++ bodyUrls

                    CodeBlock _ ->
                        []

                    ThematicBreak ->
                        []
            )
        |> List.concat

extractInlineImageUrls : List Markdown.Block.Inline -> List String
extractInlineImageUrls inlines =
    inlines
        |> List.filterMap
            (\inline ->
                case inline of
                    Markdown.Block.Image url _ _ ->
                        Just url

                    _ ->
                        Nothing
            )

collectText : String -> Result String String
collectText markdown =
    markdown
        |> replaceImgTags
        |> replaceBrokenColTag
        |> Markdown.Parser.parse
        |> Result.map filterText
        |> Result.mapError deadEndsToString

collectImageUrls : String -> Result String (List String)
collectImageUrls markdown =
    markdown
        |> replaceImgTags
        |> replaceBrokenColTag
        |> Markdown.Parser.parse
        |> Result.map filterImageUrls
        |> Result.mapError deadEndsToString

filterText : List Block -> String
filterText blocks =
    blocks
        |> List.map
            (\block ->
                case block of
                    HtmlBlock _ ->
                        ""

                    UnorderedList _ listItems ->
                        listItems 
                        |> List.map (\(ListItem _ listItemBlocks) -> filterText listItemBlocks)
                        |> String.join " "

                    OrderedList _ _ listItems ->
                        listItems 
                        |> List.map filterText
                        |> String.join " "

                    BlockQuote quoteBlocks ->
                        filterText quoteBlocks

                    Heading _ inlines ->
                        Markdown.Block.extractInlineText inlines

                    Paragraph inlines ->
                        Markdown.Block.extractInlineText inlines

                    Table columnHeaders tableBody ->
                        let
                            headerText =
                                columnHeaders
                                |> List.map (\{ label } -> Markdown.Block.extractInlineText label)
                                |> String.join " "

                            bodyText =
                                tableBody
                                |> List.map (\row ->
                                        row
                                        |> List.map Markdown.Block.extractInlineText
                                        |> String.join " "
                                    )
                                    |> String.join " "
                        in
                        headerText ++ " " ++ bodyText

                    CodeBlock { body } ->
                        body

                    ThematicBreak ->
                        ""
            )
        |> String.join " "



type BlockType
    = DefaultBlock
    | EmbedBlock


determineBlockTypes : List Block -> List ( Block, BlockType )
determineBlockTypes blocks =
    blocks
        |> List.map
            (\block ->
                case block of
                    Paragraph inlines ->
                        if generateEmbedForParagraph inlines then
                            ( block, EmbedBlock )

                        else
                            ( block, DefaultBlock )

                    _ ->
                        ( block, DefaultBlock )
            )



-- in case there's only one link in a paragraph
-- it should be transformed to an embedded object/preview


generateEmbedForParagraph : List Markdown.Block.Inline -> Bool
generateEmbedForParagraph inlines =
    case inlines of
        [ Markdown.Block.Link _ _ _ ] ->
            True

        [ Markdown.Block.Link _ _ _, Markdown.Block.Text txt ] ->
            -- tolerate whitespace after link
            txt
                |> String.trim
                |> String.isEmpty

        _ ->
            False


rendererForBlockType : Environment -> Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> BlockType -> Renderer.Renderer (Html msg)
rendererForBlockType environment styles loadedContent fnGetProfile blockType =
    let
        defaultRenderer =
            TailwindMarkdownRenderer.renderer environment styles fnGetProfile
    in
    case blockType of
        DefaultBlock ->
            defaultRenderer

        EmbedBlock ->
            { defaultRenderer | link = embedPreview styles loadedContent }


embedPreview : Styles msg -> Maybe (LoadedContent msg) -> { title : Maybe String, destination : String } -> List (Html msg) -> Html msg
embedPreview styles loadedContent { destination } body =
    LinkPreview.generatePreviewHtml
        loadedContent
        destination
        (styles.textStyleLinks ++ styles.colorStyleLinks)
        body


elementFromHtmlList : List (Html msg) -> Html msg
elementFromHtmlList htmlList =
    Html.div [] htmlList
