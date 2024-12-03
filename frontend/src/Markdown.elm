module Markdown exposing (Msg, markdownViewHtml, summaryFromContent)

-- import Html exposing (Attribute, Html)

import Html.Styled as Html exposing (..)
import Markdown.Block as Block exposing (Block, Inline, ListItem(..), Task(..))
import Markdown.Block exposing (Block(..))
import Markdown.Parser
import Markdown.Renderer as Renderer
import Parser
import Parser.Advanced as Advanced
import Regex exposing (Regex)
import TailwindMarkdownRenderer
import Ui.Styles exposing (Styles)

type Msg
    = OnMarkdownInput String

summaryFromContent : String -> Maybe String
summaryFromContent markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\blocks ->
                    firstMarkdownText blocks
            )
        |> Result.toMaybe

firstMarkdownText : List Markdown.Block.Block -> Result String String
firstMarkdownText blocks =
    blocks
    |> List.filterMap filterParagraphs
    |> List.head
    |> Maybe.andThen (\firstInlines ->
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
replaceImgTags : String -> String
replaceImgTags input =
    let
        -- Regex to match <img src="..." alt="..."> tags
        imgRegex : Maybe Regex
        imgRegex =
            Regex.fromString "<img src=\\\"([^\"]+)\\\"(?: alt=\\\"([^\"]*)\\\")?>"

        -- Function to replace matched strings with Markdown-style image links
        replacer : Regex.Match -> String
        replacer match =
            case match.submatches of
                [Just url, Just altText] ->
                    "![" ++ altText ++ "](" ++ url ++ ")"
                [Just url, Nothing] ->
                    "![](" ++ url ++ ")"
                _ ->
                    ""
    in
    case imgRegex of
        Just regex ->
            Regex.replace regex replacer input

        Nothing ->
            input


deadEndsToString : List (Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"


markdownViewHtml : Styles msg -> String -> Result String (Html msg)
markdownViewHtml styles markdown =
    render styles markdown
        |> Result.map elementFromHtmlList

render : Styles msg -> String -> Result String (List (Html msg))
render styles markdown =
    markdown
        |> replaceImgTags
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Renderer.render (TailwindMarkdownRenderer.renderer styles) ast)



elementFromHtmlList : List (Html msg) -> Html msg
elementFromHtmlList htmlList =
    Html.div [] htmlList
