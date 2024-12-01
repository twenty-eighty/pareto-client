module Markdown exposing (Msg, markdownViewHtml, summaryFromContent)

-- import Html exposing (Attribute, Html)

import Browser
import Html as PlainHtml
import Html.Styled as Html exposing (..)
import Markdown.Block as Block exposing (Block, Inline, ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer as Renderer
import Parser
import Parser.Advanced as Advanced
import Ui.Styles exposing (Styles)
import TailwindMarkdownRenderer
import Markdown.Block exposing (Block(..))

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
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Renderer.render (TailwindMarkdownRenderer.renderer styles) ast)



elementFromHtmlList : List (Html msg) -> Html msg
elementFromHtmlList htmlList =
    Html.div [] htmlList
