module Pages.TechDetails exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Styled exposing (div, text)
import Html.Styled.Attributes exposing (css)
import Http
import Layouts
import Markdown
import Nostr.Event exposing (KindInformationLink(..))
import Page exposing (Page)
import Pages.A.Addr_ exposing (toLayout)
import Route exposing (Route)
import Shared
import Tailwind.Utilities as Tw
import Time exposing (Month(..))
import Translations.TechDetails as Translations
import Ui.Styles exposing (Theme)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { techDetailsMarkdown : Maybe String }


init : () -> ( Model, Effect Msg )
init () =
    ( { techDetailsMarkdown = Nothing }
    , "/md/tech-details.md"
        |> requestMarkdown
        |> Effect.sendCmd
    )



-- UPDATE


type Msg
    = ReceivedMarkdown (Result Http.Error String)
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReceivedMarkdown (Ok markdown) ->
            ( { model | techDetailsMarkdown = Just markdown }, Effect.none )

        ReceivedMarkdown (Err _) ->
            ( model, Effect.none )

        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


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
            [ model.techDetailsMarkdown
                |> foldMaybe (div [] [])
                    (\markdown ->
                        case Markdown.markdownViewHtml styles Nothing (\_ -> Nothing) markdown of
                            Ok html ->
                                html

                            Err error ->
                                div [] [ text <| "Error rendering Markdown: " ++ error ]
                    )
            ]
        ]
    }


requestMarkdown : String -> Cmd Msg
requestMarkdown url =
    Http.get
        { url = url
        , expect = Http.expectString ReceivedMarkdown
        }


foldMaybe : b -> (a -> b) -> Maybe a -> b
foldMaybe default fn maybeA =
    maybeA |> Maybe.map fn |> Maybe.withDefault default
