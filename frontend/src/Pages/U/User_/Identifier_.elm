module Pages.U.User_.Identifier_ exposing (Model, Msg, page)

import Browser.Dom
import Components.RelayStatus exposing (Purpose(..))
import Effect exposing (Effect)
import FeatherIcons exposing (user)
import Html.Styled as Html exposing (Html)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Nip05 as Nip05
import Nostr.Request exposing (RequestData(..), RequestId)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg
import Task
import View exposing (View)
import Ui.Styles exposing (Theme)
import Ui.View exposing (viewRelayStatus)


page : Shared.Model -> Route { user : String, identifier : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)

toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { nip05 : Maybe Nip05.Nip05
    , identifier : String
    , requestId : Maybe RequestId
    }


init : Shared.Model -> Route { user : String, identifier : String } -> () -> ( Model, Effect Msg )
init shared route () =
    let
        model =
            { nip05 = Nip05.parseNip05 route.params.user
            , identifier = route.params.identifier
            , requestId = Nothing
            }

        (requestEffect, requestId) =
            model.nip05
            |> Maybe.map (\nip05 ->
                case Nostr.getPubKeyByNip05 shared.nostr nip05 of
                    Just pubKey ->
                        case Nostr.getArticleWithIdentifier shared.nostr pubKey model.identifier of
                            Just _ ->
                                -- article already loaded, accessible in view function
                                ( Effect.none, Nothing )

                            Nothing ->
                                ( -- pubkey already loaded, request article
                                    { emptyEventFilter
                                        | authors = Just [ pubKey ]
                                        , kinds = Just [ KindLongFormContent ]
                                        , tagReferences = Just [ TagReferenceIdentifier model.identifier ]
                                    }
                                    |> RequestArticle (Just <| Nostr.getReadRelayUrlsForPubKey shared.nostr pubKey)
                                    |> Nostr.createRequest shared.nostr ("Article of NIP-05 user " ++ Nip05.nip05ToString nip05) [ ]
                                    |> Shared.Msg.RequestNostrEvents
                                    |> Effect.sendSharedMsg
                                , Just <| Nostr.getLastRequestId shared.nostr
                                )

                    Nothing ->
                        ( RequestNip05AndArticle nip05 model.identifier
                            |> Nostr.createRequest shared.nostr ("Article of NIP-05 user " ++ Nip05.nip05ToString nip05) [KindLongFormContent, KindHighlights, KindBookmarkList, KindBookmarkSets]
                            |> Shared.Msg.RequestNostrEvents
                            |> Effect.sendSharedMsg
                        , Just <| Nostr.getLastRequestId shared.nostr
                        )
            )
            |> Maybe.withDefault (Effect.none, Nothing)
    in
    ( { model | requestId = requestId }
    , Effect.batch
        [ requestEffect
        -- jump to top of article
        , Effect.sendCmd <| Task.perform (\_ -> NoOp) (Browser.Dom.setViewport 0 0)
        ]
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        maybeArticle =
            model.nip05
            |> Maybe.andThen (Nostr.getPubKeyByNip05 shared.nostr)
            |> Maybe.andThen (\pubKey -> Nostr.getArticleWithIdentifier shared.nostr pubKey model.identifier)
    in
    { title = maybeArticle |> Maybe.andThen .title |> Maybe.withDefault "Article"
    , body = [ viewArticle shared model maybeArticle ]
    }


viewArticle : Shared.Model -> Model -> Maybe Article -> Html Msg
viewArticle shared model maybeArticle =
    case maybeArticle of
        Just article ->
            Ui.View.viewArticle 
                { theme = shared.theme
                , browserEnv = shared.browserEnv
                , nostr = shared.nostr
                , userPubKey = Shared.loggedInPubKey shared.loginStatus
                , onBookmark = Nothing
                }
                article

        Nothing ->
            viewRelayStatus shared.theme shared.browserEnv.translations shared.nostr LoadingArticle model.requestId