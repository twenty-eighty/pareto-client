module ContactDatabase exposing (..)

import Effect exposing (Effect)
import Http
import Json.Decode as Decode
import Jwt
import Nostr
import Nostr.External exposing (decodeAuthHeaderReceived)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Request exposing (RequestData(..), HttpRequestMethod(..))
import Nostr.Types exposing (IncomingMessage)
import Ports
import Shared.Msg

type alias Model =  
    { jwtToken : Maybe JwtToken
    , errors : List String
    }

type Msg
    = GetContactDatabaseChallengeResponse (Result Http.Error String)
    | GetContactDatabaseLoginResponse (Result Http.Error String)
    | ReceivedMessage IncomingMessage


type alias JwtToken =
    { iat : Int
    , exp : Int
    , userId : String
    }


jwtTokenDecoder : Decode.Decoder JwtToken
jwtTokenDecoder =
    Decode.map3 JwtToken
        (Decode.field "iat" Decode.int)
        (Decode.field "exp" Decode.int)
        (Decode.field "userId" Decode.string)


contactDatabaseServerUrl : String
contactDatabaseServerUrl =
    "http://localhost:4000"

init : (Model, Effect Msg)
init =
    ( { jwtToken = Nothing
      , errors = []
      }
    , getContactDatabaseChallenge
    )

getContactDatabaseChallenge : Effect Msg
getContactDatabaseChallenge =
    Http.get
        { url = contactDatabaseServerUrl ++ "/api/auth/challenge"
        , expect = Http.expectJson GetContactDatabaseChallengeResponse (Decode.field "challenge" Decode.string)
        }
        |> Effect.sendCmd


update : Nostr.Model -> Msg -> Model -> (Model, Effect Msg)
update nostr msg model =
    case msg of
        GetContactDatabaseChallengeResponse (Ok challenge) ->
            ( model
            , PostRequest 1 ""
                |> RequestNip98Auth contactDatabaseServerUrl "/api/auth/login" challenge
                |> Nostr.createRequest nostr "NIP-96 auth request for log-in to contact database" []
                |> Shared.Msg.RequestNostrEvents
                |> Effect.sendSharedMsg
             )

        GetContactDatabaseChallengeResponse (Err error) ->
            ( { model | errors = [ httpErrorToString error ] }, Effect.none )

        GetContactDatabaseLoginResponse (Ok jwt) ->
            ( model
            , Effect.none
             )

        GetContactDatabaseLoginResponse (Err error) ->
            ( { model | errors = [ httpErrorToString error ] }, Effect.none )

        ReceivedMessage message ->
            updateWithMessage model message


updateWithMessage : Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage model message =
    case message.messageType of
        "nip98AuthHeader" ->
            case ( Decode.decodeValue decodeAuthHeaderReceived message.value ) of
                ( Ok decoded ) ->
                    ( model
                    , Http.request
                        { url = contactDatabaseServerUrl ++ "/api/auth/login"
                        , method = "POST"
                        , headers = [ Http.header "Authorization" ("Bearer " ++ decoded.authHeader) ]
                        , body = Http.emptyBody
                        , expect = Http.expectJson GetContactDatabaseLoginResponse (Decode.field "challenge" Decode.string)
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                        |> Effect.sendCmd
                    )

                ( Err error ) ->
                    ( { model | errors = ("Error receiving NIP-98 auth header: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        _ ->
            ( model, Effect.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage
