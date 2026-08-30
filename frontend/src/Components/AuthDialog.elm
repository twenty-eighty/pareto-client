module Components.AuthDialog exposing
    ( Model
    , Msg(..)
    , Screen(..)
    , identityPubKeys
    , init
    , isOpen
    , open
    , update
    , view
    )

{-| Login / identity dialog: extension, npub, bunker, ncryptsec, multi-identity.
-}

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.ModalDialog as ModalDialog
import Html.Styled as Html exposing (Html, div, input, p, span, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Nostr
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (profileDisplayName, shortenedPubKey)
import Nostr.Types exposing (IncomingMessage, LoginStatus(..), PubKey)
import Ports
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)


type Model
    = Model Internal


type alias Internal =
    { open : Bool
    , screen : Screen
    , identities : List Identity
    , activeId : Maybe String
    , npubInput : String
    , bunkerInput : String
    , ncryptsecInput : String
    , passwordInput : String
    , unlockId : Maybe String
    , error : Maybe String
    , busy : Bool
    }


type Screen
    = Home
    | NpubForm
    | BunkerForm
    | NcryptsecForm
    | UnlockForm


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
        , npubInput = ""
        , bunkerInput = ""
        , ncryptsecInput = ""
        , passwordInput = ""
        , unlockId = Nothing
        , error = Nothing
        , busy = False
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
        }


identityPubKeys : Model -> List PubKey
identityPubKeys (Model m) =
    List.map .pubkey m.identities


type Msg
    = Close
    | ShowScreen Screen
    | PortMsg IncomingMessage
    | ClickExtension
    | InputNpub String
    | InputBunker String
    | InputNcryptsec String
    | InputPassword String
    | SubmitNpub
    | SubmitBunker
    | SubmitNcryptsec
    | UseIdentity String Bool
    | ConfirmUnlock
    | DeleteIdentity String
    | ClickLogout


update : Msg -> Model -> ( Model, Cmd msg )
update msg (Model m) =
    case msg of
        Close ->
            ( Model { m | open = False, error = Nothing, busy = False }
            , Cmd.none
            )

        ShowScreen screen ->
            ( Model { m | screen = screen, error = Nothing }
            , Cmd.none
            )

        PortMsg incoming ->
            handlePort (Model m) incoming

        ClickExtension ->
            ( Model { m | busy = True, error = Nothing }
            , Ports.loginWithExtension
            )

        InputNpub v ->
            ( Model { m | npubInput = v }, Cmd.none )

        InputBunker v ->
            ( Model { m | bunkerInput = v }, Cmd.none )

        InputNcryptsec v ->
            ( Model { m | ncryptsecInput = v }, Cmd.none )

        InputPassword v ->
            ( Model { m | passwordInput = v }, Cmd.none )

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
            ( Model { m | busy = True }
            , Ports.logout
            )


handlePort : Model -> IncomingMessage -> ( Model, Cmd msg )
handlePort (Model m) incoming =
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
            ( Model
                { m
                    | open = False
                    , busy = False
                    , error = Nothing
                    , passwordInput = ""
                    , screen = Home
                }
            , Cmd.none
            )

        "loggedOut" ->
            ( Model { m | busy = False, activeId = Nothing, screen = Home }
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

        _ ->
            ( Model m, Cmd.none )


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
view theme _ loginStatus nostr (Model m) =
    if not m.open then
        emptyHtml

    else
        let
            title =
                case m.screen of
                    Home ->
                        "Sign in"

                    NpubForm ->
                        "Read-only (npub)"

                    BunkerForm ->
                        "Bunker / Amber"

                    NcryptsecForm ->
                        "Import ncryptsec"

                    UnlockForm ->
                        "Unlock identity"
        in
        ModalDialog.new
            { title = title
            , buttons = []
            , content =
                [ viewError m.error
                , case m.screen of
                    Home ->
                        viewHome theme loginStatus nostr m

                    NpubForm ->
                        viewNpub theme m

                    BunkerForm ->
                        viewBunker theme m

                    NcryptsecForm ->
                        viewNcryptsec theme m

                    UnlockForm ->
                        viewUnlock theme m
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


viewHome : Theme -> LoginStatus -> Nostr.Model -> Internal -> Html Msg
viewHome theme loginStatus nostr m =
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_72 ] ]
        [ viewIdentityList theme nostr m
        , fullButton theme "Browser extension" ClickExtension m.busy
        , fullButton theme "Read-only npub" (ShowScreen NpubForm) m.busy
        , fullButton theme "Bunker / Amber" (ShowScreen BunkerForm) m.busy
        , fullButton theme "Import ncryptsec" (ShowScreen NcryptsecForm) m.busy
        , case loginStatus of
            LoggedIn _ _ ->
                fullButton theme "Log out" ClickLogout m.busy

            _ ->
                emptyHtml
        ]


viewIdentityList : Theme -> Nostr.Model -> Internal -> Html Msg
viewIdentityList theme nostr m =
    if List.isEmpty m.identities then
        p [ css [ Tw.text_sm, Tw.opacity_70 ] ] [ text "No saved identities yet." ]

    else
        div [ css [ Tw.flex, Tw.flex_col, Tw.gap_2 ] ]
            (p [ css [ Tw.text_sm, Tw.font_semibold ] ] [ text "Saved identities" ]
                :: List.map (viewIdentityRow theme nostr m) m.identities
            )


viewIdentityRow : Theme -> Nostr.Model -> Internal -> Identity -> Html Msg
viewIdentityRow theme nostr m identity =
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
                span [ css [ Tw.text_xs, Tw.font_semibold ] ] [ text "Active" ]

              else
                Button.new
                    { label =
                        if identity.locked then
                            "Unlock"

                        else
                            "Use"
                    , onClick = Just (UseIdentity identity.id identity.locked)
                    , theme = theme
                    }
                    |> Button.withSizeSmall
                    |> Button.view
            , Button.new
                { label = "✕"
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


viewNpub : Theme -> Internal -> Html Msg
viewNpub theme m =
    formStack
        [ field "npub or hex pubkey" "text" m.npubInput InputNpub
        , fullButton theme "Continue" SubmitNpub m.busy
        , fullButton theme "Back" (ShowScreen Home) False
        ]


viewBunker : Theme -> Internal -> Html Msg
viewBunker theme m =
    formStack
        [ field "bunker:// or nostrconnect:// URI" "text" m.bunkerInput InputBunker
        , fullButton theme "Connect" SubmitBunker m.busy
        , fullButton theme "Back" (ShowScreen Home) False
        ]


viewNcryptsec : Theme -> Internal -> Html Msg
viewNcryptsec theme m =
    formStack
        [ field "ncryptsec1…" "text" m.ncryptsecInput InputNcryptsec
        , field "Password" "password" m.passwordInput InputPassword
        , fullButton theme "Import & unlock" SubmitNcryptsec m.busy
        , fullButton theme "Back" (ShowScreen Home) False
        ]


viewUnlock : Theme -> Internal -> Html Msg
viewUnlock theme m =
    formStack
        [ p [ css [ Tw.text_sm ] ] [ text "Enter the password for this encrypted key." ]
        , field "Password" "password" m.passwordInput InputPassword
        , fullButton theme "Unlock" ConfirmUnlock m.busy
        , fullButton theme "Back" (ShowScreen Home) False
        ]


formStack : List (Html Msg) -> Html Msg
formStack children =
    div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_72 ] ] children


fullButton : Theme -> String -> Msg -> Bool -> Html Msg
fullButton theme label msg busy =
    Button.new { label = label, onClick = Just msg, theme = theme }
        |> Button.withDisabled busy
        |> Button.withWidthFull
        |> Button.view


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
