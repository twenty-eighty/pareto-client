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
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Translations.AuthDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)


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
    , error : Maybe String
    , busy : Bool
    , extensionAvailable : Bool
    , passkeySupported : Maybe Bool
    , hasPasskeyCredential : Bool
    , pendingPasskeyPubKey : Maybe String
    }


type alias PendingEmail =
    { email : String
    , ncryptsec : String
    , publicKey : String
    , displayName : Maybe String
    }


type Screen
    = Home
    | AddIdentity
    | NostrMethods
    | EmailLoginForm
    | EmailPasswordForm
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
    }


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
        , error = Nothing
        , busy = False
        , extensionAvailable = False
        , passkeySupported = Nothing
        , hasPasskeyCredential = False
        , pendingPasskeyPubKey = Nothing
        }


isOpen : Model -> Bool
isOpen (Model m) =
    m.open


open : Model -> Model
open (Model m) =
    Model
        { m
            | open = True
            , screen = Home
            , error = Nothing
            , busy = False
            , passwordInput = ""
            , passwordConfirmInput = ""
            , pendingEmail = Nothing
        }


openEmailLogin : Model -> Model
openEmailLogin (Model m) =
    Model
        { m
            | open = True
            , screen = EmailLoginForm
            , error = Nothing
            , busy = False
            , passwordInput = ""
            , passwordConfirmInput = ""
            , pendingEmail = Nothing
        }


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
    | SubmitEmailLookup
    | SubmitEmailLogin
    | GotEmailLookup (Result String EmailLookupResult)
    | GotSignup (Result String ())
    | SubmitNpub
    | SubmitBunker
    | SubmitNcryptsec
    | UseIdentity String Bool
    | ConfirmUnlock
    | DeleteIdentity String
    | ClickLogout
    | ClickPasskeyLogin
    | ClickCreatePasskey
    | ClickDismissPasskey


type EmailLookupResult
    = AccountFound PendingEmail
    | AccountUnknown String


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
                }
            , Cmd.none
            )

        ShowScreen screen ->
            ( Model
                { m
                    | screen = screen
                    , error = Nothing
                    , pendingEmail =
                        if screen == EmailPasswordForm then
                            m.pendingEmail

                        else
                            Nothing
                }
            , Cmd.none
            )

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
            if String.trim m.passwordInput /= String.trim m.passwordConfirmInput then
                ( Model { m | error = Just "Passwords do not match" }, Cmd.none )

            else if String.length (String.trim m.passwordInput) < 8 then
                ( Model { m | error = Just "Password must be at least 8 characters" }, Cmd.none )

            else
                ( Model { m | busy = True, error = Nothing }
                , Ports.generateEncryptedKey m.passwordInput
                )

        SubmitEmailLookup ->
            let
                email =
                    String.trim m.emailInput |> String.toLower
            in
            ( Model { m | busy = True, error = Nothing, pendingEmail = Nothing, emailInput = email }
            , fetchEmailAccount browserEnv.authApiBaseUrl email
            )

        SubmitEmailLogin ->
            case m.pendingEmail of
                Just pending ->
                    ( Model { m | busy = True, error = Nothing }
                    , Ports.unlockEmailAccount
                        { email = pending.email
                        , password = m.passwordInput
                        , ncryptsec = pending.ncryptsec
                        , publicKeyHint = pending.publicKey
                        , displayName = pending.displayName
                        }
                    )

                Nothing ->
                    ( Model { m | error = Just "Start again with your email" }
                    , Cmd.none
                    )

        GotEmailLookup result ->
            case result of
                Ok (AccountFound pending) ->
                    ( Model
                        { m
                            | busy = False
                            , error = Nothing
                            , emailInput = pending.email
                            , passwordInput = ""
                            , pendingEmail = Just pending
                            , screen = EmailPasswordForm
                        }
                    , Cmd.none
                    )

                Ok (AccountUnknown email) ->
                    ( Model
                        { m
                            | busy = False
                            , error = Nothing
                            , emailInput = email
                            , passwordInput = ""
                            , passwordConfirmInput = ""
                            , displayNameInput = ""
                            , pendingEmail = Nothing
                            , screen = CreateAccountForm
                        }
                    , Cmd.none
                    )

                Err reason ->
                    ( Model { m | busy = False, error = Just reason }, Cmd.none )

        GotSignup result ->
            case result of
                Ok () ->
                    ( Model
                        { m
                            | busy = False
                            , error = Nothing
                            , passwordInput = ""
                            , passwordConfirmInput = ""
                            , pendingEmail = Nothing
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
                , Cmd.none
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

        ClickDismissPasskey ->
            ( Model
                { m
                    | open = False
                    , screen = Home
                    , error = Nothing
                    , busy = False
                    , pendingPasskeyPubKey = Nothing
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
                    if offerPasskey then
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
                            , screen = Home
                        }
                    , Cmd.none
                    )

        "encryptedKeyGenerated" ->
            case Decode.decodeValue encryptedKeyDecoder incoming.value of
                Ok key ->
                    ( Model m
                    , postSignup browserEnv.authApiBaseUrl
                        { email = String.trim m.emailInput |> String.toLower
                        , publicKey = key.publicKey
                        , ncryptsec = key.ncryptsec
                        , displayName = String.trim m.displayNameInput
                        , locale = BrowserEnv.translationsLocale browserEnv.language
                        }
                    )

                Err _ ->
                    ( Model { m | busy = False, error = Just "Could not create key" }, Cmd.none )

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
                    , Cmd.none
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
                    | open = False
                    , busy = False
                    , error = Nothing
                    , screen = Home
                    , pendingPasskeyPubKey = Nothing
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


fetchEmailAccount : String -> String -> Cmd Msg
fetchEmailAccount baseUrl email =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = baseUrl ++ "/api/auth/login"
        , body = Http.jsonBody (Encode.object [ ( "email", Encode.string email ) ])
        , expect = expectEmailLookup email GotEmailLookup
        , timeout = Nothing
        , tracker = Nothing
        }


postSignup :
    String
    ->
        { email : String
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
            , ( "public_key", Encode.string params.publicKey )
            , ( "ncryptsec", Encode.string params.ncryptsec )
            , ( "locale", Encode.string params.locale )
            ]
                ++ (if params.displayName == "" then
                        []

                    else
                        [ ( "display_name", Encode.string params.displayName ) ]
                   )
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
                            Err "Please confirm your email before signing in"

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
                                    Err "Please confirm your email before signing in"

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
    Decode.map5 Identity
        (Decode.field "id" Decode.string)
        (Decode.field "method" Decode.string)
        (Decode.field "pubkey" Decode.string)
        (Decode.maybe (Decode.field "label" Decode.string))
        (Decode.map (Maybe.withDefault False) (Decode.maybe (Decode.field "locked" Decode.bool)))


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

                    EmailPasswordForm ->
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

                    EmailPasswordForm ->
                        viewEmailPassword theme t m

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
        , secondaryButton theme (Translations.useNostrAccountButtonTitle t) (ShowScreen NostrMethods)
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
            [ fullButton theme (Translations.loginWithPasskeyButtonTitle t) ClickPasskeyLogin m.busy
            , p [ css [ Tw.text_xs, Tw.opacity_60 ] ]
                [ text (Translations.loginWithPasskeyHelpText t) ]
            ]

        Just False ->
            [ p [ css [ Tw.text_xs, Tw.opacity_60 ] ]
                [ text (Translations.passkeyUnsupportedHelpText t) ]
            ]

        Nothing ->
            []


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
                [ text (identity.method ++ " · " ++ shortenedPubKey 11 npub) ]
            ]
        , div [ css [ Tw.flex, Tw.gap_2 ] ]
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
        [ p [ css [ Tw.text_sm, Tw.opacity_70 ] ]
            [ text (Translations.emailLookupHelpText t) ]
        , field "Email" "email" m.emailInput InputEmail
        , fullButton theme
            (Translations.continueWithEmailButtonTitle t)
            SubmitEmailLookup
            (m.busy || not (EmailValidation.emailValid (String.trim m.emailInput)))
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen (methodsBackScreen m))
        ]


viewEmailPassword : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewEmailPassword theme t m =
    formStack
        [ p [ css [ Tw.text_sm, Tw.opacity_70 ] ]
            [ text (Translations.emailPasswordHelpText t) ]
        , p [ css [ Tw.text_sm, Tw.font_semibold ] ] [ text m.emailInput ]
        , field "Password" "password" m.passwordInput InputPassword
        , fullButton theme
            (Translations.signInButtonTitle t)
            SubmitEmailLogin
            (m.busy || String.trim m.passwordInput == "")
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen EmailLoginForm)
        ]


viewCreateAccount : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewCreateAccount theme t m =
    formStack
        [ p [ css [ Tw.text_sm, Tw.opacity_70 ] ]
            [ text (Translations.createAccountHelpText t) ]
        , p [ css [ Tw.text_sm, Tw.font_semibold ] ] [ text m.emailInput ]
        , field "Display name" "text" m.displayNameInput InputDisplayName
        , field "Password" "password" m.passwordInput InputPassword
        , field "Confirm password" "password" m.passwordConfirmInput InputPasswordConfirm
        , fullButton theme (Translations.createAccountButtonTitle t) SubmitCreateAccount m.busy
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
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen (methodsBackScreen m))
        ]


viewNpub : Theme -> List I18Next.Translations -> Internal -> Html Msg
viewNpub theme t m =
    formStack
        [ field "npub or hex pubkey" "text" m.npubInput InputNpub
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
        [ field "bunker:// or nostrconnect:// URI" "text" m.bunkerInput InputBunker
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
        [ field "ncryptsec1…" "text" m.ncryptsecInput InputNcryptsec
        , field "Password" "password" m.passwordInput InputPassword
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
    formStack
        [ p [ css [ Tw.text_sm ] ] [ text "Enter the password for this encrypted key." ]
        , field "Password" "password" m.passwordInput InputPassword
        , fullButton theme (Translations.unlockButtonTitle t) ConfirmUnlock m.busy
        , secondaryButton theme (Translations.backButtonTitle t) (ShowScreen Home)
        ]


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


field : String -> String -> String -> (String -> Msg) -> Html Msg
field placeholder inputType value toMsg =
    input
        [ Attr.type_ inputType
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
        []
