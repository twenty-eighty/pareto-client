module Markdown exposing (Msg, markdownViewHtml)

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

type Msg
    = OnMarkdownInput String


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
