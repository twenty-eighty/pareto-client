module Markdown exposing (markdownViewHtml, summaryFromContent)

-- import Html exposing (Attribute, Html)

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


markdownViewHtml : Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> String -> Result String (Html msg)
markdownViewHtml styles loadedContent fnGetProfile markdown =
    render styles loadedContent fnGetProfile markdown
        |> Result.map elementFromHtmlList


render : Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> String -> Result String (List (Html msg))
render styles loadedContent fnGetProfile markdown =
    markdown
        |> replaceImgTags
        |> replaceBrokenColTag
        |> Markdown.Parser.parse
        |> Result.map determmineBlockTypes
        |> Result.mapError deadEndsToString
        |> Result.andThen
            (\ast ->
                ast
                    |> Renderer.renderWithMeta
                        (\blockType ->
                            rendererForBlockType styles loadedContent fnGetProfile blockType
                        )
            )


type BlockType
    = DefaultBlock
    | EmbedBlock


determmineBlockTypes : List Block -> List ( Block, BlockType )
determmineBlockTypes blocks =
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


rendererForBlockType : Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> BlockType -> Renderer.Renderer (Html msg)
rendererForBlockType styles loadedContent fnGetProfile blockType =
    let
        defaultRenderer =
            TailwindMarkdownRenderer.renderer styles fnGetProfile
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
