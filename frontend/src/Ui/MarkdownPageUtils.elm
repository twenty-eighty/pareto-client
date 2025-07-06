module Ui.MarkdownPageUtils exposing (..)

import Effect exposing (Effect)
import Html.Styled exposing (div, text)
import Html.Styled.Attributes exposing (css)
import Http
import Layouts
import Layouts.Sidebar
import Markdown
import Nostr.Event exposing (KindInformationLink(..))
import Shared
import Tailwind.Utilities as Tw
import Time exposing (Month(..))
import Translations.TechDetails as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)
import View exposing (View)


type alias Model =
    { markdown : Maybe String }


init : String -> Shared.Model -> () -> ( Model, Effect Msg )
init mdPath _ () =
    let
        requestMarkdown url =
            Http.get
                { url = url
                , expect = Http.expectString ReceivedMarkdown
                }
    in
    ( { markdown = Nothing }
    , mdPath
        |> requestMarkdown
        |> Effect.sendCmd
    )


type Msg
    = ReceivedMarkdown (Result Http.Error String)
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReceivedMarkdown (Ok markdown) ->
            ( { model | markdown = Just markdown }, Effect.none )

        ReceivedMarkdown (Err _) ->
            ( model, Effect.none )

        NoOp ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme
    in
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            (styles.colorStyleBackground
                ++ styles.colorStyleGrayscaleTitle
                ++ [ css
                        [ Tw.flex
                        , Tw.flex_col
                        , Tw.gap_3
                        , Tw.m_10
                        ]
                   ]
            )
            [ model.markdown
                |> foldMaybe (emptyHtml)
                    (\markdown ->
                        case Markdown.markdownViewHtml shared.browserEnv.environment styles Nothing (\_ -> Nothing) markdown of
                            Ok html ->
                                html

                            Err error ->
                                div [] [ text <| "Error rendering Markdown: " ++ error ]
                    )
            ]
        ]
    }


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar


foldMaybe : b -> (a -> b) -> Maybe a -> b
foldMaybe default fn maybeA =
    maybeA |> Maybe.map fn |> Maybe.withDefault default
