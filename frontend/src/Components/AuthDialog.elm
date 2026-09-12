module Components.AuthDialog exposing
    ( Model
    , Msg(..)
    , Screen(..)
    , identityPubKeys
    , init
    , isOpen
    , open
    , openEmailLogin
    , update
    , view
    )

{-| Login / identity dialog: email, extension, npub, bunker, ncryptsec, multi-identity.
-}

import Browser.Dom as Dom
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.ModalDialog as ModalDialog
import Dict
import EmailValidation
import Html.Styled as Html exposing (Html, div, input, p, span, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Http
import I18Next
import Json.Decode as Decode
import Json.Encode as Encode
import Nostr
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (profileDisplayName, shortenedPubKey)
import Nostr.Types exposing (IncomingMessage, LoginStatus(..), PubKey)
import Pareto
import Ports
import Process
import SHA256
import Task
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Translations.AuthDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)


firstFieldId : String
firstFieldId =
    "auth-dialog-first-field"


focusFirstField : Cmd Msg
focusFirstField =
    -- Wait a tick so the target input is in the DOM after a screen change.
    Process.sleep 50
        |> Task.andThen (\_ -> Dom.focus firstFieldId)
        |> Task.attempt (\_ -> FocusDone)


maybeFocusFirstField : Screen -> Cmd Msg
maybeFocusFirstField screen =
    if screenHasEntryField screen then
        focusFirstField

    else
        Cmd.none


screenHasEntryField : Screen -> Bool
screenHasEntryField screen =
    case screen of
        EmailLoginForm ->
            True

        CreateAccountForm ->
            True

        NpubForm ->
            True

        BunkerForm ->
            True

        NcryptsecForm ->
            True

        UnlockForm ->
            True

        Home ->
            False

        AddIdentity ->
            False

        NostrMethods ->
            False

        CheckEmail ->
            False

        CreatePasskey ->
            False


type Model
    = Model Internal


type alias Internal =
    { open : Bool
    , screen : Screen
    , identities : List Identity
    , activeId : Maybe String
    , emailInput : String
    , displayNameInput : String
    , npubInput : String
    , bunkerInput : String
    , ncryptsecInput : String
    , passwordInput : String
    , passwordConfirmInput : String
    , unlockId : Maybe String
    , pendingEmail : Maybe PendingEmail
    , pendingSignupKey : Maybe PendingSignupKey
    , loginHash : Maybe String
    , awaitingConfirmation : Bool
    , error : Maybe String
    , busy : Bool
    , extensionAvailable : Bool
    , passkeySupported : Maybe Bool
    , hasPasskeyCredential : Bool
    , pendingPasskeyPubKey : Maybe String
    , addPasskeyAfterUnlock : Bool
    }


type alias PendingEmail =
    { email : String
    , ncryptsec : String
    , publicKey : String
    , displayName : Maybe String
    }


type alias PendingSignupKey =
    { publicKey : String
    , ncryptsec : String
    , displayName : String
    }


type Screen
    = Home
    | AddIdentity
    | NostrMethods
    | EmailLoginForm
    | CreateAccountForm
    | CheckEmail
    | NpubForm
    | BunkerForm
    | NcryptsecForm
    | UnlockForm
    | CreatePasskey


type alias Identity =
    { id : String
    , method : String
    , pubkey : String
    , label : Maybe String
    , locked : Bool
    , hasPasskey : Bool
    }


{-| Server login key: SHA-256 hex of `lowercase(trim(email)) ++ ":" ++ password`.
Password is never sent; only this hash.
-}
computeLoginHash : String -> String -> String
computeLoginHash email password =
    (String.toLower (String.trim email) ++ ":" ++ password)
        |> SHA256.fromString
        |> SHA256.toHex


{-| NIP-05 / portal username: lowercase a-z, 0-9, -, \_, . from display name.
-}
usernameFromDisplayName : String -> String
usernameFromDisplayName displayName =
    displayName
        |> String.trim
        |> String.toLower
        |> String.toList
        |> List.map
            (\c ->
                if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' then
                    c

                else if c == ' ' then
                    '-'

                else
                    '-'
            )
        |> String.fromList
        |> collapseUsernameSeparators
        |> String.left 30


collapseUsernameSeparators : String -> String
collapseUsernameSeparators value =
    value
        |> String.replace "--" "-"
        |> (\s ->
                if String.contains "--" s then
                    collapseUsernameSeparators s

                else
                    s
           )
        |> trimUsernameEdges


trimUsernameEdges : String -> String
trimUsernameEdges value =
    if String.startsWith "-" value then
        trimUsernameEdges (String.dropLeft 1 value)

    else if String.endsWith "-" value then
        trimUsernameEdges (String.dropRight 1 value)

    else
        value


createAccountFormReady : Internal -> Bool
createAccountFormReady m =
    let
        displayName =
            String.trim m.displayNameInput

        password =
            String.trim m.passwordInput

        confirm =
            String.trim m.passwordConfirmInput

        username =
            usernameFromDisplayName displayName
    in
    displayName
        /= ""
        && username
        /= ""
        && password
        /= ""
        && String.length password
        >= 8
        && password
        == confirm
        && not m.busy


init : Model
init =
    Model
        { open = False
        , screen = Home
        , identities = []
        , activeId = Nothing
        , emailInput = ""
        , displayNameInput = ""
        , npubInput = ""
        , bunkerInput = ""
        , ncryptsecInput = ""
        , passwordInput = ""
        , passwordConfirmInput = ""
        , unlockId = Nothing
        , pendingEmail = Nothing
        , pendingSignupKey = Nothing
        , loginHash = Nothing
        , awaitingConfirmation = False
        , error = Nothing
        , busy = False
        , extensionAvailable = False
        , passkeySupported = Nothing
        , hasPasskeyCredential = False
        , pendingPasskeyPubKey = Nothing
        , addPasskeyAfterUnlock = False
        }


isOpen : Model -> Bool
isOpen (Model m) =
    m.open


open : Model -> ( Model, Cmd Msg )
open (Model m) =
    ( Model
        { m
            | open = True
            , screen = Home
            , error = Nothing
            , busy = False
            , passwordInput = ""
            , passwordConfirmInput = ""
            , pendingEmail = Nothing
            , pendingSignupKey = Nothing
            , loginHash = Nothing
            , awaitingConfirmation = False
        }
    , Cmd.none
    )


openEmailLogin : Model -> ( Model, Cmd Msg )
openEmailLogin (Model m) =
    ( Model
        { m
            | open = True
            , screen = EmailLoginForm
            , error = Nothing
            , busy = False
            , passwordInput = ""
            , passwordConfirmInput = ""
            , pendingEmail = Nothing
            , pendingSignupKey = Nothing
            , loginHash = Nothing
            , awaitingConfirmation = False
        }
    , focusFirstField
    )


identityPubKeys : Model -> List PubKey
identityPubKeys (Model m) =
    List.map .pubkey m.identities


type Msg
    = Close
    | ShowScreen Screen
    | PortMsg IncomingMessage
    | ClickExtension
    | InputEmail String
    | InputDisplayName String
    | InputNpub String
    | InputBunker String
    | InputNcryptsec String
    | InputPassword String
    | InputPasswordConfirm String
    | SubmitCreateAccount
    | SubmitEmailLogin
    | ResendConfirmation
    | GotEmailLookup (Result String EmailLookupResult)
    | GotSignup (Result String ())
    | GotResendConfirmation (Result String ())
    | SubmitNpub
    | SubmitBunker
    | SubmitNcryptsec
    | UseIdentity String Bool
    | ConfirmUnlock
    | ClickUnlockWithPasskey
    | DeleteIdentity String
    | ClickLogout
    | ClickPasskeyLogin
    | ClickCreatePasskey
    | ClickDismissPasskey
    | ClickAddPasskey String
    | FocusDone


type EmailLookupResult
    = AccountFound PendingEmail
    | AccountUnknown String
    | AccountPendingConfirmation String


update : BrowserEnv -> Msg -> Model -> ( Model, Cmd Msg )
update browserEnv msg (Model m) =
    case msg of
        Close ->
            ( Model
                { m
                    | open = False
                    , error = Nothing
                    , busy = False
                    , pendingEmail = Nothing
                    , pendingSignupKey = Nothing
                    , loginHash = Nothing
                    , awaitingConfirmation = False
                }
            , Cmd.none
            )

        ShowScreen screen ->
            ( Model
                { m
                    | screen = screen
                    , error = Nothing
                    , pendingEmail = Nothing
                    , awaitingConfirmation =
                        if screen == CheckEmail then
                            m.awaitingConfirmation

                        else
                            False
                }
            , maybeFocusFirstField screen
            )

        FocusDone ->
            ( Model m, Cmd.none )

        PortMsg incoming ->
            handlePort browserEnv (Model m) incoming

        ClickExtension ->
            ( Model { m | busy = True, error = Nothing }
            , Ports.loginWithExtension
            )

        InputEmail v ->
            ( Model { m | emailInput = v }, Cmd.none )

        InputDisplayName v ->
            ( Model { m | displayNameInput = v }, Cmd.none )

        InputNpub v ->
            ( Model { m | npubInput = v }, Cmd.none )

        InputBunker v ->
            ( Model { m | bunkerInput = v }, Cmd.none )

        InputNcryptsec v ->
            ( Model { m | ncryptsecInput = v }, Cmd.none )

        InputPassword v ->
            ( Model { m | passwordInput = v }, Cmd.none )

        InputPasswordConfirm v ->
            ( Model { m | passwordConfirmInput = v }, Cmd.none )

        SubmitCreateAccount ->
            if not (createAccountFormReady m) then
                ( Model { m | error = Just "Enter a display name and matching passwords (8+ characters)" }, Cmd.none )

            else
                ( Model
                    { m
                        | busy = True
                        , error = Nothing
                        , loginHash = Just (computeLoginHash m.emailInput m.passwordInput)
                        , awaitingConfirmation = False
                    }
                , Ports.generateEncryptedKey m.passwordInput
                )

        SubmitEmailLogin ->
            let
                email =
                    String.trim m.emailInput |> String.toLower

                password =
                    m.passwordInput

                hash =
                    computeLoginHash email password
            in
            if not (EmailValidation.emailValid email) then
                ( Model { m | error = Just "Enter a valid email" }, Cmd.none )

            else if password == "" then
                ( Model { m | error = Just "Enter your password" }, Cmd.none )

            else
                ( Model
                    { m
                        | busy = True
                        , error = Nothing
                        , pendingEmail = Nothing
                        , emailInput = email
                        , loginHash = Just hash
                        , awaitingConfirmation = False
                    }
                , fetchEmailAccount browserEnv.authApiBaseUrl email hash
                )

        ResendConfirmation ->
            let
                email =
                    String.trim m.emailInput |> String.toLower

                hash =
                    case m.loginHash of
                        Just existing ->
                            existing

                        Nothing ->
                            computeLoginHash email m.passwordInput
            in
            if not m.awaitingConfirmation && m.screen /= CheckEmail then
                ( Model m, Cmd.none )

            else if not (EmailValidation.emailValid email) then
                ( Model { m | error = Just "Enter a valid email" }, Cmd.none )

            else if hash == "" || (m.loginHash == Nothing && m.passwordInput == "") then
                ( Model { m | error = Just "Enter your password to resend confirmation" }, Cmd.none )

            else
                ( Model { m | busy = True, error = Nothing, loginHash = Just hash }
                , postResendConfirmation browserEnv.authApiBaseUrl email hash
                )

        GotEmailLookup result ->
            case result of
                Ok (AccountFound pending) ->
                    ( Model
                        { m
                            | busy = True
                            , error = Nothing
                            , emailInput = pending.email
                            , pendingEmail = Just pending
                            , awaitingConfirmation = False
                        }
                    , Ports.unlockEmailAccount
                        { email = pending.email
                        , password = m.passwordInput
                        , ncryptsec = pending.ncryptsec
                        , publicKeyHint = pending.publicKey
                        , displayName = pending.displayName
                        }
                    )

                Ok (AccountUnknown email) ->
                    ( Model
                        { m
                            | busy = False
                            , error = Nothing
                            , emailInput = email
                            , passwordConfirmInput = ""
                            , displayNameInput = ""
                            , pendingEmail = Nothing
                            , awaitingConfirmation = False
                            , screen = CreateAccountForm
                        }
                    , focusFirstField
                    )

                Ok (AccountPendingConfirmation email) ->
                    ( Model
                        { m
                            | busy = False
                            , error = Just "Please confirm your email before signing in"
                            , emailInput = email
                            , pendingEmail = Nothing
                            , awaitingConfirmation = True
                            , screen = EmailLoginForm
                        }
                    , Cmd.none
                    )

                Err reason ->
                    ( Model { m | busy = False, error = Just reason, awaitingConfirmation = False }, Cmd.none )

        GotSignup result ->
            case result of
                Ok () ->
                    let
                        email =
                            String.trim m.emailInput |> String.toLower

                        saveCmd =
                            case m.pendingSignupKey of
                                Just key ->
                                    Ports.saveLockedEmailIdentity
                                        { email = email
                                        , ncryptsec = key.ncryptsec
                                        , publicKey = key.publicKey
                                        , displayName =
                                            if key.displayName == "" then
                                                Nothing

                                            else
                                                Just key.displayName
                                        }

                                Nothing ->
                                    Cmd.none
                    in
                    ( Model
                        { m
                            | busy = False
                            , error = Nothing
                            , passwordInput = ""
                            , passwordConfirmInput = ""
                            , pendingEmail = Nothing
                            , pendingSignupKey = Nothing
                            , awaitingConfirmation = True
                            , screen = CheckEmail
                        }
                    , saveCmd
                    )

                Err reason ->
                    ( Model
                        { m
                            | busy = False
                            , error = Just reason
                            , pendingSignupKey = Nothing
                        }
                    , Cmd.none
                    )

        GotResendConfirmation result ->
            case result of
                Ok () ->
                    ( Model
                        { m
                            | busy = False
                            , error = Nothing
                            , awaitingConfirmation = True
                            , screen = CheckEmail
                        }
                    , Cmd.none
                    )

                Err reason ->
                    ( Model { m | busy = False, error = Just reason }, Cmd.none )

        SubmitNpub ->
            ( Model { m | busy = True, error = Nothing }
            , Ports.loginWithNpub m.npubInput
            )

        SubmitBunker ->
            ( Model { m | busy = True, error = Nothing }
            , Ports.loginWithBunker m.bunkerInput
            )

        SubmitNcryptsec ->
            ( Model { m | busy = True, error = Nothing }
            , Ports.loginWithNcryptsec m.ncryptsecInput m.passwordInput
            )

        UseIdentity id locked ->
            if locked then
                ( Model
                    { m
                        | screen = UnlockForm
                        , unlockId = Just id
                        , passwordInput = ""
                        , error = Nothing
                    }
                , focusFirstField
                )

            else
                ( Model { m | busy = True, error = Nothing }
                , Ports.activateIdentity id Nothing
                )

        ConfirmUnlock ->
            case m.unlockId of
                Just id ->
                    ( Model { m | busy = True, error = Nothing }
                    , Ports.activateIdentity id (Just m.passwordInput)
                    )

                Nothing ->
                    ( Model m, Cmd.none )

        ClickUnlockWithPasskey ->
            case m.unlockId of
                Just id ->
                    ( Model { m | busy = True, error = Nothing }
                    , Ports.unlockIdentityWithPasskey id
                    )

                Nothing ->
                    ( Model { m | error = Just "No identity selected" }, Cmd.none )

        DeleteIdentity id ->
            ( Model { m | busy = True }
            , Ports.removeIdentity id
            )

        ClickLogout ->
            ( Model { m | busy = True, pendingEmail = Nothing }
            , Ports.logout
            )

        ClickPasskeyLogin ->
            ( Model { m | busy = True, error = Nothing }
            , Ports.loginWithPasskey
            )

        ClickCreatePasskey ->
            ( Model { m | busy = True, error = Nothing }
            , Ports.createPasskey Nothing
            )

        ClickAddPasskey identityId ->
            case List.filter (\identity -> identity.id == identityId) m.identities |> List.head of
                Nothing ->
                    ( Model { m | error = Just "Identity not found" }, Cmd.none )

                Just identity ->
                    if m.passkeySupported /= Just True then
                        ( Model { m | error = Just "Passkeys are not available in this browser" }, Cmd.none )

                    else if identity.hasPasskey then
                        ( Model m, Cmd.none )

                    else if identity.method /= "ncryptsec" then
                        ( Model { m | error = Just "Unlock an encrypted key account to add a passkey" }, Cmd.none )

                    else if identity.locked then
                        ( Model
                            { m
                                | screen = UnlockForm
                                , unlockId = Just identity.id
                                , passwordInput = ""
                                , error = Nothing
                                , pendingPasskeyPubKey = Just identity.pubkey
                                , addPasskeyAfterUnlock = True
                            }
                        , focusFirstField
                        )

                    else
                        ( Model
                            { m
                                | screen = CreatePasskey
                                , error = Nothing
                                , pendingPasskeyPubKey = Just identity.pubkey
                                , addPasskeyAfterUnlock = False
                            }
                        , Cmd.none
                        )

        ClickDismissPasskey ->
            let
                keepOpen =
                    m.addPasskeyAfterUnlock || not (List.isEmpty m.identities)
            in
            ( Model
                { m
                    | open = keepOpen
                    , screen = Home
                    , error = Nothing
                    , busy = False
                    , pendingPasskeyPubKey = Nothing
                    , addPasskeyAfterUnlock = False
                }
            , m.pendingPasskeyPubKey
                |> Maybe.map Ports.dismissPasskeyPrompt
                |> Maybe.withDefault Cmd.none
            )


handlePort : BrowserEnv -> Model -> IncomingMessage -> ( Model, Cmd Msg )
handlePort browserEnv (Model m) incoming =
    case incoming.messageType of
        "identities" ->
            case Decode.decodeValue identitiesPayloadDecoder incoming.value of
                Ok data ->
                    ( Model
                        { m
                            | identities = data.identities
                            , activeId = data.activeId
                            , busy = False
                        }
                    , Cmd.none
                    )

                Err _ ->
                    ( Model m, Cmd.none )

        "user" ->
            case Decode.decodeValue userOfferDecoder incoming.value of
                Ok { pubKey, offerPasskey } ->
                    if m.addPasskeyAfterUnlock || offerPasskey then
                        ( Model
                            { m
                                | open = True
                                , busy = False
                                , error = Nothing
                                , passwordInput = ""
                                , passwordConfirmInput = ""
                                , pendingEmail = Nothing
                                , pendingPasskeyPubKey = Just pubKey
                                , passkeySupported = Just True
                                , addPasskeyAfterUnlock = False
                                , screen = CreatePasskey
                            }
                        , Cmd.none
                        )

                    else
                        ( Model
                            { m
                                | open = False
                                , busy = False
                                , error = Nothing
                                , passwordInput = ""
                                , passwordConfirmInput = ""
                                , pendingEmail = Nothing
                                , pendingPasskeyPubKey = Nothing
                                , addPasskeyAfterUnlock = False
                                , screen = Home
                            }
                        , Cmd.none
                        )

                Err _ ->
                    ( Model
                        { m
                            | open = False
                            , busy = False
                            , error = Nothing
                            , passwordInput = ""
                            , passwordConfirmInput = ""
                            , pendingEmail = Nothing
                            , pendingPasskeyPubKey = Nothing
                            , addPasskeyAfterUnlock = False
                            , screen = Home
                        }
                    , Cmd.none
                    )

        "encryptedKeyGenerated" ->
            case Decode.decodeValue encryptedKeyDecoder incoming.value of
                Ok key ->
                    let
                        email =
                            String.trim m.emailInput |> String.toLower

                        displayName =
                            String.trim m.displayNameInput

                        hash =
                            case m.loginHash of
                                Just existing ->
                                    existing

                                Nothing ->
                                    computeLoginHash email m.passwordInput
                    in
                    ( Model
                        { m
                            | loginHash = Just hash
                            , pendingSignupKey =
                                Just
                                    { publicKey = key.publicKey
                                    , ncryptsec = key.ncryptsec
                                    , displayName = displayName
                                    }
                        }
                    , postSignup browserEnv.authApiBaseUrl
                        { email = email
                        , loginHash = hash
                        , username = usernameFromDisplayName displayName
                        , publicKey = key.publicKey
                        , ncryptsec = key.ncryptsec
                        , displayName = displayName
                        , locale = BrowserEnv.translationsLocale browserEnv.language
                        }
                    )

                Err _ ->
                    ( Model { m | busy = False, error = Just "Could not create key", pendingSignupKey = Nothing }, Cmd.none )

        "loggedOut" ->
            ( Model { m | busy = False, activeId = Nothing, pendingEmail = Nothing, screen = Home }
            , Cmd.none
            )

        "authError" ->
            case Decode.decodeValue (Decode.field "reason" Decode.string) incoming.value of
                Ok reason ->
                    ( Model { m | busy = False, error = Just reason }, Cmd.none )

                Err _ ->
                    ( Model { m | busy = False, error = Just "Authentication failed" }, Cmd.none )

        "authNeedsUnlock" ->
            case Decode.decodeValue (Decode.field "id" Decode.string) incoming.value of
                Ok id ->
                    ( Model
                        { m
                            | open = True
                            , screen = UnlockForm
                            , unlockId = Just id
                            , passwordInput = ""
                        }
                    , focusFirstField
                    )

                Err _ ->
                    ( Model m, Cmd.none )

        "nostrExtension" ->
            case Decode.decodeValue (Decode.field "available" Decode.bool) incoming.value of
                Ok available ->
                    ( Model { m | extensionAvailable = available }, Cmd.none )

                Err _ ->
                    ( Model m, Cmd.none )

        "passkeySupport" ->
            case Decode.decodeValue passkeySupportDecoder incoming.value of
                Ok data ->
                    ( Model
                        { m
                            | passkeySupported = Just data.supported
                            , hasPasskeyCredential = data.hasCredential
                        }
                    , Cmd.none
                    )

                Err _ ->
                    ( Model m, Cmd.none )

        "passkeyCreated" ->
            ( Model
                { m
                    | open = True
                    , busy = False
                    , error = Nothing
                    , screen = Home
                    , pendingPasskeyPubKey = Nothing
                    , addPasskeyAfterUnlock = False
                    , hasPasskeyCredential = True
                }
            , Cmd.none
            )

        _ ->
            ( Model m, Cmd.none )


type alias EncryptedKey =
    { publicKey : String
    , ncryptsec : String
    }


encryptedKeyDecoder : Decode.Decoder EncryptedKey
encryptedKeyDecoder =
    Decode.map2 EncryptedKey
        (Decode.field "publicKey" Decode.string)
        (Decode.field "ncryptsec" Decode.string)


type alias UserOffer =
    { pubKey : String
    , offerPasskey : Bool
    }


userOfferDecoder : Decode.Decoder UserOffer
userOfferDecoder =
    Decode.map2 UserOffer
        (Decode.oneOf
            [ Decode.field "pubKey" Decode.string
            , Decode.field "pubkey" Decode.string
            ]
        )
        (Decode.map (Maybe.withDefault False) (Decode.maybe (Decode.field "offerPasskey" Decode.bool)))


type alias PasskeySupport =
    { supported : Bool
    , hasCredential : Bool
    }


passkeySupportDecoder : Decode.Decoder PasskeySupport
passkeySupportDecoder =
    Decode.map2 PasskeySupport
        (Decode.field "supported" Decode.bool)
        (Decode.map (Maybe.withDefault False) (Decode.maybe (Decode.field "hasCredential" Decode.bool)))


fetchEmailAccount : String -> String -> String -> Cmd Msg
fetchEmailAccount baseUrl email loginHash =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = baseUrl ++ "/api/auth/login"
        , body = Http.jsonBody (Encode.object [ ( "login_hash", Encode.string loginHash ) ])
        , expect = expectEmailLookup email GotEmailLookup
        , timeout = Nothing
        , tracker = Nothing
        }


postSignup :
    String
    ->
        { email : String
        , loginHash : String
        , username : String
        , publicKey : String
        , ncryptsec : String
        , displayName : String
        , locale : String
        }
    -> Cmd Msg
postSignup baseUrl params =
    let
        bodyFields =
            [ ( "email", Encode.string params.email )
            , ( "login_hash", Encode.string params.loginHash )
            , ( "username", Encode.string params.username )
            , ( "public_key", Encode.string params.publicKey )
            , ( "ncryptsec", Encode.string params.ncryptsec )
            , ( "locale", Encode.string params.locale )
            , ( "display_name", Encode.string params.displayName )
            ]
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = baseUrl ++ "/api/auth/signup"
        , body = Http.jsonBody (Encode.object bodyFields)
        , expect = expectSignup GotSignup
        , timeout = Nothing
        , tracker = Nothing
        }


postResendConfirmation : String -> String -> String -> Cmd Msg
postResendConfirmation baseUrl email loginHash =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = baseUrl ++ "/api/auth/resend-confirmation"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string email )
                    , ( "login_hash", Encode.string loginHash )
                    ]
                )
        , expect = expectResendConfirmation GotResendConfirmation
        , timeout = Nothing
        , tracker = Nothing
        }


expectEmailLookup : String -> (Result String EmailLookupResult -> msg) -> Http.Expect msg
expectEmailLookup email toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err ("Bad URL: " ++ url)

                Http.Timeout_ ->
                    Err "Network timeout"

                Http.NetworkError_ ->
                    Err "Network error"

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString loginErrorDecoder body of
                        Ok "not_found" ->
                            Ok (AccountUnknown email)

                        Ok "email_not_confirmed" ->
                            Ok (AccountPendingConfirmation email)

                        Ok other ->
                            Err other

                        Err _ ->
                            if metadata.statusCode == 404 then
                                Ok (AccountUnknown email)

                            else
                                Err ("Login failed (" ++ String.fromInt metadata.statusCode ++ ")")

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString (pendingEmailDecoder email) body of
                        Ok pending ->
                            Ok (AccountFound pending)

                        Err _ ->
                            case Decode.decodeString loginErrorDecoder body of
                                Ok "not_found" ->
                                    Ok (AccountUnknown email)

                                Ok "email_not_confirmed" ->
                                    Ok (AccountPendingConfirmation email)

                                Ok other ->
                                    Err other

                                Err _ ->
                                    Err "Login failed"


expectSignup : (Result String () -> msg) -> Http.Expect msg
expectSignup toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err ("Bad URL: " ++ url)

                Http.Timeout_ ->
                    Err "Network timeout"

                Http.NetworkError_ ->
                    Err "Network error"

                Http.BadStatus_ metadata body ->
                    Err (signupErrorMessage body metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString signupOkDecoder body of
                        Ok False ->
                            Err (signupErrorMessage body 200)

                        _ ->
                            Ok ()


expectResendConfirmation : (Result String () -> msg) -> Http.Expect msg
expectResendConfirmation toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err ("Bad URL: " ++ url)

                Http.Timeout_ ->
                    Err "Network timeout"

                Http.NetworkError_ ->
                    Err "Network error"

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString loginErrorDecoder body of
                        Ok "already_confirmed" ->
                            Err "This email is already confirmed. Sign in instead."

                        Ok "email_required" ->
                            Err "Email is required to resend confirmation"

                        Ok "login_hash_required" ->
                            Err "Password is required to resend confirmation"

                        Ok other ->
                            Err other

                        Err _ ->
                            Err (signupErrorMessage body metadata.statusCode)

                Http.GoodStatus_ _ _ ->
                    Ok ()


loginErrorDecoder : Decode.Decoder String
loginErrorDecoder =
    Decode.field "error" Decode.string


pendingEmailDecoder : String -> Decode.Decoder PendingEmail
pendingEmailDecoder email =
    Decode.map3
        (\ncryptsec publicKey displayName ->
            { email = email
            , ncryptsec = ncryptsec
            , publicKey = publicKey
            , displayName = displayName
            }
        )
        (Decode.field "ncryptsec" Decode.string)
        (Decode.map (Maybe.withDefault "") (Decode.maybe (Decode.field "public_key" Decode.string)))
        (Decode.map normalizeOptionalName (Decode.maybe (Decode.field "display_name" Decode.string)))


normalizeOptionalName : Maybe String -> Maybe String
normalizeOptionalName maybeName =
    maybeName
        |> Maybe.map String.trim
        |> Maybe.andThen
            (\name ->
                if name == "" then
                    Nothing

                else
                    Just name
            )


signupOkDecoder : Decode.Decoder Bool
signupOkDecoder =
    Decode.oneOf
        [ Decode.field "ok" Decode.bool
        , Decode.succeed True
        ]


signupErrorMessage : String -> Int -> String
signupErrorMessage body status =
    case Decode.decodeString signupErrorsDecoder body of
        Ok (Just message) ->
            message

        _ ->
            case Decode.decodeString loginErrorDecoder body of
                Ok message ->
                    message

                Err _ ->
                    "Signup failed (" ++ String.fromInt status ++ ")"


signupErrorsDecoder : Decode.Decoder (Maybe String)
signupErrorsDecoder =
    Decode.field "errors" (Decode.dict (Decode.list Decode.string))
        |> Decode.map
            (\dict ->
                dict
                    |> Dict.values
                    |> List.concat
                    |> List.head
            )


type alias IdentitiesPayload =
    { identities : List Identity
    , activeId : Maybe String
    }


identitiesPayloadDecoder : Decode.Decoder IdentitiesPayload
identitiesPayloadDecoder =
    Decode.map2 IdentitiesPayload
        (Decode.field "identities" (Decode.list identityDecoder))
        (Decode.maybe (Decode.field "activeId" Decode.string))


identityDecoder : Decode.Decoder Identity
identityDecoder =
    Decode.map6 Identity
        (Decode.field "id" Decode.string)
        (Decode.field "method" Decode.string)
        (Decode.field "pubkey" Decode.string)
        (Decode.maybe (Decode.field "label" Decode.string))
        (Decode.map (Maybe.withDefault False) (Decode.maybe (Decode.field "locked" Decode.bool)))
        (Decode.map (Maybe.withDefault False) (Decode.maybe (Decode.field "hasPasskey" Decode.bool)))


view : Theme -> BrowserEnv -> LoginStatus -> Nostr.Model -> Model -> Html Msg
view theme browserEnv _ nostr (Model m) =
    if not m.open then
        emptyHtml

    else
        let
            t =
                [ browserEnv.translations ]

            title =
                case m.screen of
                    Home ->
                        Translations.signInDialogTitle t

                    AddIdentity ->
                        Translations.addIdentityDialogTitle t

                    NostrMethods ->
                        Translations.nostrMethodsDialogTitle t

                    EmailLoginForm ->
                        Translations.emailLoginDialogTitle t

                    CreateAccountForm ->
                        Translations.createAccountDialogTitle t

                    CheckEmail ->
                        Translations.checkEmailDialogTitle t

                    NpubForm ->
                        Translations.npubDialogTitle t

                    BunkerForm ->
                        Translations.bunkerDialogTitle t

                    NcryptsecForm ->
                        Translations.ncryptsecDialogTitle t

                    UnlockForm ->
                        Translations.unlockDialogTitle t

                    CreatePasskey ->
                        Translations.createPasskeyDialogTitle t
        in
        ModalDialog.new
            { title = title
            , buttons = []
            , content =
                [ viewError m.error
                , case m.screen of
                    Home ->
                        viewHome theme t nostr m

                    AddIdentity ->
                        viewAddIdentity theme t m

                    NostrMethods ->
                        viewNostrMethods theme t m

                    EmailLoginForm ->
                        viewEmailLogin theme t m

                    CreateAccountForm ->
                        viewCreateAccount theme t m

                    CheckEmail ->
                        viewCheckEmail theme t m

                    NpubForm ->
                        viewNpub theme t m

                    BunkerForm ->
                        viewBunker theme t m

                    NcryptsecForm ->
                        viewNcryptsec theme t m

                    UnlockForm ->
                        viewUnlock theme t m

                    CreatePasskey ->
                        viewCreatePasskey theme t m
                ]
            , onClose = Close
            , theme = theme
            }
            |> ModalDialog.view


viewError : Maybe String -> Html Msg
viewError maybeError =
    case maybeError of
        Just err ->
            p [ css [ Tw.text_sm, Tw.text_color TwTheme.red_600 ] ] [ text err ]

        Nothing ->
            emptyHtml


methodsBackScreen : Internal -> Screen
methodsBackScreen m =
    if List.isEmpty m.identities then
        Home

    else
        AddIdentity


viewHome : Theme -> List I18Next.Translations -> Nostr.Model -> Internal -> Html Msg
viewHome theme t nostr m =
    if List.isEmpty m.identities then
        viewWelcomeChoices theme t m

    else
        div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_72 ] ]
            [ viewIdentityList theme t nostr m
            , fullButton theme (Translations.addIdentityButtonTitle t) (ShowScreen AddIdentity) m.busy
            , secondaryButton theme (Translations.logOutButtonTitle t) ClickLogout
            ]


viewAddIdentity : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewAddIdentity theme t m =
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_72 ] ]
        [ viewWelcomeChoices theme t m
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen Home)
        ]


viewWelcomeChoices : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewWelcomeChoices theme t m =
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_72 ] ]
        [ p [ css [ Tw.text_sm, Tw.opacity_70 ] ]
            [ text (Translations.signInHelpText t) ]
        , fullButton theme (Translations.continueWithEmailChoiceTitle t) (ShowScreen EmailLoginForm) m.busy
        , p [ css [ Tw.text_xs, Tw.opacity_60 ] ]
            [ text (Translations.emailChoiceHelpText t) ]
        , fullButton theme (Translations.useNostrAccountButtonTitle t) (ShowScreen NostrMethods) m.busy
        , p [ css [ Tw.text_xs, Tw.opacity_60 ] ]
            [ text (Translations.useNostrAccountHelpText t) ]
        ]


viewNostrMethods : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewNostrMethods theme t m =
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_72 ] ]
        (passkeyLoginBlock theme t m
            ++ [ extensionMethodButton theme t m
               , fullButton theme (Translations.bunkerButtonTitle t) (ShowScreen BunkerForm) m.busy
               , fullButton theme (Translations.importNcryptsecButtonTitle t) (ShowScreen NcryptsecForm) m.busy
               , quietLink (Translations.browseOnlyLinkTitle t) (ShowScreen NpubForm)
               , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen (methodsBackScreen m))
               ]
        )


passkeyLoginBlock : Theme -> List I18Next.Translations -> Internal -> List (Html Msg)
passkeyLoginBlock theme t m =
    case m.passkeySupported of
        Just True ->
            if showPasskeyLogin m then
                [ fullButton theme (Translations.loginWithPasskeyButtonTitle t) ClickPasskeyLogin m.busy
                , p [ css [ Tw.text_xs, Tw.opacity_60 ] ]
                    [ text (Translations.loginWithPasskeyHelpText t) ]
                ]

            else
                []

        Just False ->
            [ p [ css [ Tw.text_xs, Tw.opacity_60 ] ]
                [ text (Translations.passkeyUnsupportedHelpText t) ]
            ]

        Nothing ->
            []


{-| Show passkey login when relays/local index know of a credential, or when there
are no saved identities yet (discoverable/synced passkeys via WebAuthn).
-}
showPasskeyLogin : Internal -> Bool
showPasskeyLogin m =
    m.hasPasskeyCredential
        || List.any .hasPasskey m.identities
        || List.isEmpty m.identities


viewCreatePasskey : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewCreatePasskey theme t m =
    formStack
        [ p [ css [ Tw.text_sm, Tw.opacity_70 ] ]
            [ text (Translations.createPasskeyHelpText t) ]
        , fullButton theme (Translations.createPasskeyButtonTitle t) ClickCreatePasskey m.busy
        , secondaryButton theme (Translations.notNowButtonTitle t) ClickDismissPasskey
        ]


extensionMethodButton : Theme -> List I18Next.Translations -> Internal -> Html Msg
extensionMethodButton theme t m =
    if m.extensionAvailable then
        fullButton theme (Translations.browserExtensionButtonTitle t) ClickExtension m.busy

    else
        Button.new
            { label = Translations.installBrowserExtensionButtonTitle t
            , onClick = Nothing
            , theme = theme
            }
            |> Button.withTypePrimary
            |> Button.withNewTabLink Pareto.browserExtensionInstallUrl
            |> Button.withWidthFull
            |> Button.view


viewIdentityList : Theme -> List I18Next.Translations -> Nostr.Model -> Internal -> Html Msg
viewIdentityList theme t nostr m =
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_2 ] ]
        (p [ css [ Tw.text_sm, Tw.font_semibold ] ] [ text (Translations.savedIdentitiesTitle t) ]
            :: List.map (viewIdentityRow theme t nostr m) m.identities
        )


viewIdentityRow : Theme -> List I18Next.Translations -> Nostr.Model -> Internal -> Identity -> Html Msg
viewIdentityRow theme t nostr m identity =
    let
        isActive =
            m.activeId == Just identity.id

        npub =
            npubForPubKey identity.pubkey

        name =
            identityDisplayName nostr identity

        canAddPasskey =
            m.passkeySupported
                == Just True
                && identity.method
                == "ncryptsec"
                && not identity.hasPasskey
    in
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_between
            , Tw.gap_2
            , Tw.border
            , Tw.border_solid
            , Tw.rounded_md
            , Tw.px_3
            , Tw.py_2
            ]
        ]
        [ div [ css [ Tw.flex, Tw.flex_col, Tw.min_w_0 ] ]
            [ span [ css [ Tw.text_sm, Tw.font_medium, Tw.truncate ] ] [ text name ]
            , span [ css [ Tw.text_xs, Tw.opacity_60, Tw.truncate ] ]
                [ text
                    (identity.method
                        ++ (if identity.hasPasskey then
                                " · passkey"

                            else
                                ""
                           )
                        ++ " · "
                        ++ shortenedPubKey 11 npub
                    )
                ]
            ]
        , div [ css [ Tw.flex, Tw.flex_wrap, Tw.gap_2, Tw.justify_end ] ]
            [ if isActive then
                span [ css [ Tw.text_xs, Tw.font_semibold ] ] [ text (Translations.activeIdentityLabel t) ]

              else
                Button.new
                    { label =
                        if identity.locked then
                            Translations.unlockIdentityButtonTitle t

                        else
                            Translations.useIdentityButtonTitle t
                    , onClick = Just (UseIdentity identity.id identity.locked)
                    , theme = theme
                    }
                    |> Button.withSizeSmall
                    |> Button.view
            , if canAddPasskey then
                Button.new
                    { label = Translations.addPasskeyButtonTitle t
                    , onClick = Just (ClickAddPasskey identity.id)
                    , theme = theme
                    }
                    |> Button.withSizeSmall
                    |> Button.view

              else
                emptyHtml
            , Button.new
                { label = Translations.deleteIdentityButtonTitle t
                , onClick = Just (DeleteIdentity identity.id)
                , theme = theme
                }
                |> Button.withSizeSmall
                |> Button.withStyleDanger
                |> Button.view
            ]
        ]


identityDisplayName : Nostr.Model -> Identity -> String
identityDisplayName nostr identity =
    case Nostr.getProfile nostr identity.pubkey of
        Just profile ->
            profileDisplayName identity.pubkey profile

        Nothing ->
            Maybe.withDefault (shortenedPubKey 11 (npubForPubKey identity.pubkey)) identity.label


npubForPubKey : PubKey -> String
npubForPubKey pubKey =
    case Nip19.encode (Nip19.Npub pubKey) of
        Ok npub ->
            npub

        Err _ ->
            pubKey


viewEmailLogin : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewEmailLogin theme t m =
    formStack
        ([ p [ css [ Tw.text_sm, Tw.opacity_70 ] ]
            [ text (Translations.emailLookupHelpText t) ]
         , field "Email" "email" m.emailInput InputEmail True
         , field "Password" "password" m.passwordInput InputPassword False
         , fullButton theme
            (Translations.signInButtonTitle t)
            SubmitEmailLogin
            (m.busy
                || not (EmailValidation.emailValid (String.trim m.emailInput))
                || String.trim m.passwordInput == ""
            )
         ]
            ++ (if m.awaitingConfirmation then
                    [ secondaryButton theme "Resend confirmation email" ResendConfirmation ]

                else
                    []
               )
            ++ [ secondaryButton theme (Translations.backButtonTitle t) (ShowScreen (methodsBackScreen m)) ]
        )


viewCreateAccount : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewCreateAccount theme t m =
    let
        usernamePreview =
            usernameFromDisplayName m.displayNameInput
    in
    formStack
        [ p [ css [ Tw.text_sm, Tw.opacity_70 ] ]
            [ text (Translations.createAccountHelpText t) ]
        , p [ css [ Tw.text_sm, Tw.font_semibold ] ] [ text m.emailInput ]
        , field "Display name" "text" m.displayNameInput InputDisplayName True
        , if usernamePreview == "" then
            emptyHtml

          else
            p [ css [ Tw.text_sm, Tw.opacity_70 ] ]
                [ text ("Username: " ++ usernamePreview) ]
        , field "Password" "password" m.passwordInput InputPassword False
        , field "Confirm password" "password" m.passwordConfirmInput InputPasswordConfirm False
        , fullButton theme
            (Translations.createAccountButtonTitle t)
            SubmitCreateAccount
            (not (createAccountFormReady m))
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen EmailLoginForm)
        ]


viewCheckEmail : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewCheckEmail theme t m =
    formStack
        [ p [ css [ Tw.text_sm ] ]
            [ text "We sent a confirmation link to "
            , span [ css [ Tw.font_semibold ] ] [ text m.emailInput ]
            , text ". Confirm your email, then sign in here."
            ]
        , fullButton theme (Translations.emailLoginButtonTitle t) (ShowScreen EmailLoginForm) False
        , secondaryButton theme "Resend confirmation email" ResendConfirmation
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen (methodsBackScreen m))
        ]


viewNpub : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewNpub theme t m =
    formStack
        [ field "npub or hex pubkey" "text" m.npubInput InputNpub True
        , fullButton theme
            (Translations.continueButtonTitle t)
            SubmitNpub
            (m.busy || not (npubOrPubkeyValid m.npubInput))
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen NostrMethods)
        ]


npubOrPubkeyValid : String -> Bool
npubOrPubkeyValid input =
    let
        trimmed =
            String.trim input
    in
    isHexPubkey trimmed
        || (case Nip19.decode trimmed of
                Ok (Nip19.Npub _) ->
                    True

                _ ->
                    False
           )


isHexPubkey : String -> Bool
isHexPubkey value =
    String.length value == 64 && String.all Char.isHexDigit value


viewBunker : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewBunker theme t m =
    formStack
        [ field "bunker:// or nostrconnect:// URI" "text" m.bunkerInput InputBunker True
        , fullButton theme
            (Translations.connectButtonTitle t)
            SubmitBunker
            (m.busy || not (bunkerUriValid m.bunkerInput))
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen NostrMethods)
        ]


bunkerUriValid : String -> Bool
bunkerUriValid input =
    let
        trimmed =
            String.trim input

        hasScheme scheme =
            String.startsWith scheme trimmed
                && String.dropLeft (String.length scheme) trimmed /= ""
    in
    hasScheme "bunker://" || hasScheme "nostrconnect://"


viewNcryptsec : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewNcryptsec theme t m =
    formStack
        [ field "ncryptsec1…" "text" m.ncryptsecInput InputNcryptsec True
        , field "Password" "password" m.passwordInput InputPassword False
        , fullButton theme
            (Translations.importAndUnlockButtonTitle t)
            SubmitNcryptsec
            (m.busy || not (ncryptsecImportValid m.ncryptsecInput m.passwordInput))
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen NostrMethods)
        ]


ncryptsecImportValid : String -> String -> Bool
ncryptsecImportValid ncryptsec password =
    let
        trimmed =
            String.trim ncryptsec
    in
    String.startsWith "ncryptsec" trimmed
        && String.dropLeft (String.length "ncryptsec") trimmed /= ""
        && String.trim password /= ""


viewUnlock : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewUnlock theme t m =
    let
        unlockIdentity =
            case m.unlockId of
                Just id ->
                    List.filter (\identity -> identity.id == id) m.identities
                        |> List.head

                Nothing ->
                    Nothing

        canUnlockWithPasskey =
            m.passkeySupported
                == Just True
                && (case unlockIdentity of
                        Just identity ->
                            identity.hasPasskey

                        Nothing ->
                            False
                   )
    in
    formStack
        ([ p [ css [ Tw.text_sm ] ]
            [ text
                (if canUnlockWithPasskey then
                    "Unlock with your passkey, or enter the password for this encrypted key."

                 else
                    "Enter the password for this encrypted key."
                )
            ]
         ]
            ++ (if canUnlockWithPasskey then
                    [ fullButton theme
                        (Translations.unlockWithPasskeyButtonTitle t)
                        ClickUnlockWithPasskey
                        m.busy
                    , p [ css [ Tw.text_xs, Tw.opacity_60, Tw.text_center ] ]
                        [ text "or use your password" ]
                    ]

                else
                    []
               )
            ++ [ field "Password" "password" m.passwordInput InputPassword True
               , fullButton theme (Translations.unlockButtonTitle t) ConfirmUnlock m.busy
               , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen Home)
               ]
        )


formStack : List (Html Msg) -> Html Msg
formStack children =
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_72 ] ] children


fullButton : Theme -> String -> Msg -> Bool -> Html Msg
fullButton theme label msg busy =
    Button.new { label = label, onClick = Just msg, theme = theme }
        |> Button.withTypePrimary
        |> Button.withDisabled busy
        |> Button.withWidthFull
        |> Button.view


secondaryButton : Theme -> String -> Msg -> Html Msg
secondaryButton theme label msg =
    Button.new { label = label, onClick = Just msg, theme = theme }
        |> Button.withTypeSecondary
        |> Button.withWidthFull
        |> Button.view


quietLink : String -> Msg -> Html Msg
quietLink label msg =
    p
        [ css
            [ Tw.text_sm
            , Tw.opacity_60
            , Tw.text_center
            , Tw.underline
            , Tw.cursor_pointer
            , Tw.mt_1
            ]
        , Events.onClick msg
        ]
        [ text label ]


field : String -> String -> String -> (String -> Msg) -> Bool -> Html Msg
field placeholder inputType value toMsg isFirst =
    input
        ([ Attr.type_ inputType
         , Attr.placeholder placeholder
         , Attr.value value
         , Events.onInput toMsg
         , css
            [ Tw.w_full
            , Tw.border
            , Tw.border_solid
            , Tw.rounded_md
            , Tw.px_3
            , Tw.py_2
            , Tw.text_sm
            ]
         ]
            ++ (if isFirst then
                    [ Attr.id firstFieldId ]

                else
                    []
               )
        )
        []
