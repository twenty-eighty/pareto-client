module Markdown exposing (Msg, markdownViewHtml, maybeHtmlFromMarkdown)

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
import TailwindMarkdownRenderer


type Msg
    = OnMarkdownInput String


render : String -> Result String (List (Html msg))
render markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Renderer.render TailwindMarkdownRenderer.renderer ast)


deadEndsToString : List (Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"


markdownViewHtml : String -> Result String (Html msg)
markdownViewHtml markdown =
    render markdown
        |> Result.map elementFromHtmlList


elementFromHtmlList : List (Html msg) -> Html msg
elementFromHtmlList htmlList =
    Html.div [] htmlList


maybeHtmlFromMarkdown : String -> Maybe (Html msg)
maybeHtmlFromMarkdown markdownFile =
    let
        markdownResult =
            markdownFile
                |> markdownViewHtml
    in
    case markdownResult of
        Ok contentData ->
            Just contentData

        Err error ->
            Nothing


