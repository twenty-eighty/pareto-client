module Pages.Newsletters exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import I18Next
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Layouts
import Nostr
import Nostr.Event as Event exposing (AddressComponents, Event, EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.External
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, PubKey)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Subscribers
import Svg.Loaders as Loaders
import Table.Paginated as Table exposing (defaultCustomizations)
import Tailwind.Utilities as Tw
import Time
import Translations.Newsletters as Translations
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


type alias Newsletter =
    { articleAddress : AddressComponents
    , status : String
    , processed : Int
    , skipped : Int
    , total : Int
    , error : Maybe String
    , timestamp : Time.Posix
    , article : Maybe Subscribers.ArticleData
    }


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared _ =
    Page.new
        { init = init user shared
        , update = update shared
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
    { errors : List String
    , requestId : RequestId
    , state : ModelState
    , newsletters : Dict Int Newsletter
    , newslettersTable : Table.State
    }


type ModelState
    = Loading
    | Loaded
    | ErrorLoadingSubscribers String


type Field
    = FieldDate
    | FieldStatus
    | FieldProcessed
    | FieldSkipped
    | FieldTotal
    | FieldError
    | FieldTitle


fieldName : Field -> String
fieldName field =
    case field of
        FieldDate ->
            "Date"

        FieldStatus ->
            "Status"

        FieldProcessed ->
            "Processed"

        FieldSkipped ->
            "Skipped"

        FieldTotal ->
            "Total"

        FieldError ->
            "Error"

        FieldTitle ->
            "Title"


translatedFieldName : I18Next.Translations -> Field -> String
translatedFieldName translations field =
    case field of
        FieldDate ->
            Translations.dateColumnName [ translations ]

        FieldStatus ->
            Translations.statusColumnName [ translations ]

        FieldProcessed ->
            Translations.processedColumnName [ translations ]

        FieldSkipped ->
            Translations.skippedColumnName [ translations ]

        FieldTotal ->
            Translations.totalColumnName [ translations ]

        FieldError ->
            Translations.errorColumnName [ translations ]

        FieldTitle ->
            Translations.subjectColumnName [ translations ]


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { errors = []
      , requestId = Nostr.getLastRequestId shared.nostr
      , state = Loading
      , newsletters = Dict.empty
      , newslettersTable = Table.initialState (fieldName FieldDate) 25
      }
    , [ loadNewsletters shared.nostr user.pubKey
      ]
        |> List.map Effect.sendSharedMsg
        |> Effect.batch
    )


loadNewsletters : Nostr.Model -> PubKey -> Shared.Msg.Msg
loadNewsletters nostr userPubKey =
    newslettersEventFilter userPubKey
        |> RequestSubscribers
        |> Nostr.createRequest nostr "Load newsletters" []
        |> Shared.Msg.RequestNostrEvents


newslettersEventFilter : PubKey -> EventFilter
newslettersEventFilter pubKey =
    { emptyEventFilter
        | authors = Just [ Pareto.emailGatewayKey ]
        , kinds = Just [ KindApplicationSpecificData ]
        , tagReferences = Just [ TagReferencePubKey pubKey ]
    }



-- UPDATE


type Msg
    = NewTableState Table.State
    | ReceivedMessage IncomingMessage


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        NewTableState tableState ->
            ( { model | newslettersTable = tableState }, Effect.none )

        ReceivedMessage message ->
            updateWithMessage model message


updateWithMessage : Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage model message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeRequestId message.value of
                Ok incomingRequestId ->
                    if model.requestId == incomingRequestId then
                        case Nostr.External.decodeEvents message.value of
                            Ok [] ->
                                ( { model | state = Loaded }, Effect.none )

                            Ok events ->
                                case Nostr.External.decodeEventsKind message.value of
                                    Ok KindApplicationSpecificData ->
                                        let
                                            ( newsletters, errors ) =
                                                processEvents model.newsletters events
                                        in
                                        ( { model
                                            | state = Loaded
                                            , newsletters = newsletters
                                            , newslettersTable = Table.setTotal (Dict.size newsletters) model.newslettersTable
                                            , errors = model.errors ++ errors
                                          }
                                        , Effect.none
                                        )

                                    _ ->
                                        ( model, Effect.none )

                            Err error ->
                                ( { model | state = ErrorLoadingSubscribers (Decode.errorToString error) }, Effect.none )

                    else
                        ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


processEvents : Dict Int Newsletter -> List Event -> ( Dict Int Newsletter, List String )
processEvents existingNewsletters events =
    events
        |> List.filter (\event -> event.pubKey == Pareto.emailGatewayKey)
        |> List.map newsletterFromEvent
        |> List.foldl
            (\result ( newsletterDict, errorList ) ->
                case result of
                    Ok decodedNewsletter ->
                        ( Dict.insert (Time.posixToMillis decodedNewsletter.timestamp) decodedNewsletter newsletterDict, errorList )

                    Err error ->
                        ( newsletterDict, errorList ++ [ Decode.errorToString error ] )
            )
            ( existingNewsletters, [] )


newsletterFromEvent : Event -> Result Decode.Error Newsletter
newsletterFromEvent event =
    Decode.decodeString (Decode.field "newsletter" newsletterDecoder) event.content


newsletterDecoder : Decode.Decoder Newsletter
newsletterDecoder =
    Decode.succeed Newsletter
        |> required "articleAddress" Event.decodeAddress
        |> required "status" Decode.string
        |> required "processed" Decode.int
        |> required "skipped" Decode.int
        |> required "total" Decode.int
        |> optional "error" (Decode.maybe Decode.string) Nothing
        |> required "timestamp" Iso8601.decoder
        |> optional "article" (Decode.maybe Subscribers.decodeArticleData) Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage



-- VIEW


newslettersTableConfig : BrowserEnv -> Table.Config Newsletter Msg
newslettersTableConfig browserEnv =
    Table.customConfig
        { toId = \newsletter -> newsletter.timestamp |> Time.posixToMillis |> String.fromInt
        , toMsg = NewTableState
        , columns =
            [ Table.stringColumn (fieldName FieldDate) (translatedFieldName browserEnv.translations FieldDate) (\newsletter -> newsletter.timestamp |> BrowserEnv.formatDate browserEnv)
            , Table.stringColumn (fieldName FieldStatus) (translatedFieldName browserEnv.translations FieldStatus) (\newsletter -> newsletter.status)
            , intColumn browserEnv.translations FieldProcessed (\newsletter -> newsletter.processed)
            , intColumn browserEnv.translations FieldSkipped (\newsletter -> newsletter.skipped)
            , intColumn browserEnv.translations FieldTotal (\newsletter -> newsletter.total)
            , Table.stringColumn (fieldName FieldError) (translatedFieldName browserEnv.translations FieldError) (\newsletter -> newsletter.error |> Maybe.withDefault "")
            , Table.stringColumn (fieldName FieldTitle) (translatedFieldName browserEnv.translations FieldTitle) (\newsletter -> newsletter.article |> Maybe.map .title |> Maybe.withDefault "")
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = Table.defaultCustomizations.tableAttrs
            }
        }


intColumn : I18Next.Translations -> Field -> (Newsletter -> Int) -> Table.Column Newsletter Msg
intColumn translations field viewData =
    Table.veryCustomColumn
        { id = fieldName field
        , name = translatedFieldName translations field
        , viewData =
            \newsletter ->
                [ Html.node "center"
                    []
                    [ viewData newsletter
                        |> String.fromInt
                        |> Html.text
                    ]
                    |> Html.toUnstyled
                ]
                    |> Table.HtmlDetails []
        , sorter = Table.unsortable
        }


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    { title = Translations.newslettersTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                , Tw.m_2
                ]
            ]
            [ viewNewsletters shared.browserEnv model
            ]
        ]
    }


viewNewsletters : BrowserEnv -> Model -> Html Msg
viewNewsletters browserEnv model =
    case ( model.state, Dict.size model.newsletters ) of
        ( Loading, _ ) ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.m_2
                    ]
                ]
                [ text <| Translations.loadingNewslettersText [ browserEnv.translations ]
                , Loaders.rings [] |> Html.fromUnstyled
                ]

        ( ErrorLoadingSubscribers error, _ ) ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.m_2
                    ]
                ]
                [ text <| Translations.errorLoadingNewslettersText [ browserEnv.translations ] ++ ": " ++ error
                ]

        ( _, 0 ) ->
            div
                []
                [ text <| Translations.noNewslettersText [ browserEnv.translations ]
                ]

        ( _, _ ) ->
            div
                [ css
                    [ Tw.m_2
                    ]
                ]
                [ Table.view
                    (newslettersTableConfig browserEnv)
                    model.newslettersTable
                    (Dict.values model.newsletters)
                    |> Html.fromUnstyled
                ]
