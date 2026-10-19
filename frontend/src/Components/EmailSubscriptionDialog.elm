module Components.EmailSubscriptionDialog exposing (EmailSubscriptionDialog, Model, Msg, Recipient, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.EntryField as EntryField
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, p, text)
import Html.Styled.Attributes exposing (css)
import I18Next
import Json.Decode as Decode
import Locale exposing (Language(..))
import Mailcheck
import Newsletters.Subscribers as Subscribers
import Nostr
import Nostr.Model exposing (TestMode(..))
import Nostr.Profile exposing (Profile, profileDisplayName)
import Nostr.Types exposing (IncomingMessage, LoginStatus, loggedInPubKey, loggedInSigningPubKey)
import Ports
import Tailwind.Utilities as Tw
import Translations.EmailSubscriptionDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)


type Msg msg
    = CloseDialog
    | UpdateEmail String
    | SubscribeClicked String Profile LoginStatus String
    | ReceivedMessage IncomingMessage


type Model
    = Model
        { state : DialogState
        , nextRequestId : Int
        }


type DialogState
    = DialogHidden
    | DialogVisible EmailSubscriptionData
    | DialogSending String EmailSubscriptionData
    | DialogResult OptInStatus
    | DialogSendError String


type OptInStatus
    = OptInSent
    | OptInQueued
    | OptInRejected


type alias Recipient =
    { email : String
    , name : Maybe String
    }


type alias EmailSubscriptionData =
    { email : Maybe String
    }


type EmailSubscriptionDialog msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , nostr : Nostr.Model
        , profile : Profile
        , loginStatus : LoginStatus
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , profile : Profile
    , loginStatus : LoginStatus
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> EmailSubscriptionDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , nostr = props.nostr
        , profile = props.profile
        , loginStatus = props.loginStatus
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : {} -> Model
init _ =
    Model
        { state = DialogHidden
        , nextRequestId = 1
        }


show : Model -> ( Model, Effect msg )
show (Model model) =
    ( Model { model | state = DialogVisible { email = Nothing } }
    , Ports.markOptInOpened |> Effect.sendCmd
    )


hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            CloseDialog ->
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            UpdateEmail email ->
                case model.state of
                    DialogVisible emailSubscriptionData ->
                        if email /= "" then
                            ( Model { model | state = DialogVisible { emailSubscriptionData | email = Just <| String.toLower email } }, Effect.none )

                        else
                            ( Model { model | state = DialogVisible { emailSubscriptionData | email = Nothing } }, Effect.none )

                    _ ->
                        ( Model model, Effect.none )

            SubscribeClicked email authorProfile loginStatus locale ->
                case model.state of
                    DialogVisible emailSubscriptionData ->
                        let
                            userPubKey =
                                loggedInPubKey loginStatus

                            signingPubKey =
                                loggedInSigningPubKey loginStatus

                            ( firstName, lastName ) =
                                userPubKey
                                    |> Maybe.andThen (Nostr.getProfile props.nostr)
                                    |> Maybe.andThen .displayName
                                    |> Maybe.map Subscribers.splitName
                                    |> Maybe.withDefault ( Nothing, Nothing )

                            requestId =
                                String.fromInt model.nextRequestId
                        in
                        ( Model
                            { model
                                | state = DialogSending requestId emailSubscriptionData
                                , nextRequestId = model.nextRequestId + 1
                            }
                        , Ports.sendOptIn
                            { requestId = requestId
                            , author = authorProfile.pubKey
                            , email = email
                            , firstName = firstName
                            , lastName = lastName
                            , pubKey = userPubKey
                            , locale = Just locale
                            , signingPubKey = signingPubKey
                            , test = props.nostr.testMode == TestModeEnabled
                            }
                            |> Effect.sendCmd
                        )

                    _ ->
                        ( Model model, Effect.none )

            ReceivedMessage message ->
                ( updateWithMessage (Model model) message, Effect.none )


updateWithMessage : Model -> IncomingMessage -> Model
updateWithMessage (Model model) message =
    case ( model.state, message.messageType ) of
        ( DialogSending requestId _, "optInResult" ) ->
            case Decode.decodeValue (Decode.field "requestId" Decode.string) message.value of
                Ok incomingId ->
                    if incomingId == requestId then
                        applyOptInResult (Model model) message.value

                    else
                        Model model

                Err _ ->
                    Model model

        _ ->
            Model model


applyOptInResult : Model -> Decode.Value -> Model
applyOptInResult (Model model) value =
    case Decode.decodeValue optInResultDecoder value of
        Ok (OptInAccepted status) ->
            Model { model | state = DialogResult status }

        Ok (OptInFailed error) ->
            Model { model | state = DialogSendError error }

        Err decodingError ->
            Model { model | state = DialogSendError <| Decode.errorToString decodingError }


type OptInReply
    = OptInAccepted OptInStatus
    | OptInFailed String


optInResultDecoder : Decode.Decoder OptInReply
optInResultDecoder =
    Decode.oneOf
        [ Decode.field "error" Decode.string
            |> Decode.map OptInFailed
        , Decode.field "status" optInStatusDecoder
            |> Decode.map OptInAccepted
        ]


optInStatusDecoder : Decode.Decoder OptInStatus
optInStatusDecoder =
    Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "SENT" ->
                        Decode.succeed OptInSent

                    "QUEUED" ->
                        Decode.succeed OptInQueued

                    "REJECTED" ->
                        Decode.succeed OptInRejected

                    _ ->
                        Decode.fail ("Unknown opt-in status: " ++ status)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (Msg msg)
subscriptions _ =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : EmailSubscriptionDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
    case model.state of
        DialogHidden ->
            emptyHtml

        DialogVisible emailSubscriptionData ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewSubscribeDialog dialog emailSubscriptionData ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogSending _ _ ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewSendingDialog dialog ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogResult status ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewResultDialog dialog status ]
                CloseDialog
                |> Html.map settings.toMsg

        DialogSendError error ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewErrorDialog dialog error ]
                CloseDialog
                |> Html.map settings.toMsg


viewSubscribeDialog : EmailSubscriptionDialog msg -> EmailSubscriptionData -> Html (Msg msg)
viewSubscribeDialog (Settings settings) data =
    let
        emailIsValid =
            Subscribers.emailValid <| Maybe.withDefault "" data.email

        submitMsg =
            data.email
                |> Maybe.map (\email -> SubscribeClicked email settings.profile settings.loginStatus settings.browserEnv.locale)

        suggestions =
            emailSuggestion settings.browserEnv.language data.email
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            , Tw.w_full
            , Tw.max_w_sm
            , Tw.mt_2
            ]
        ]
        [ p
            [ css
                [ Tw.mb_4
                ]
            ]
            [ text <|
                Translations.infoText
                    [ settings.browserEnv.translations ]
                    { profileName = profileDisplayName settings.profile.pubKey settings.profile }
            ]
        , div []
            [ div
                [ css
                    [ Tw.mb_2
                    ]
                ]
                [ EntryField.new
                    { value = Maybe.withDefault "" data.email
                    , onInput = UpdateEmail
                    , theme = settings.theme
                    }
                    |> EntryField.withLabel (Translations.emailLabel [ settings.browserEnv.translations ])
                    |> EntryField.withPlaceholder (Translations.emailPlaceholder [ settings.browserEnv.translations ])
                    |> EntryField.withRequired
                    |> EntryField.withSubmitMsg submitMsg
                    |> EntryField.withSuggestions "email" suggestions
                    |> EntryField.withType EntryField.FieldTypeEmail
                    |> EntryField.view
                ]
            , Button.new
                { label = Translations.subscribeButtonTitle [ settings.browserEnv.translations ]
                , onClick = submitMsg
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled (not emailIsValid)
                |> Button.view
            , viewPrivacyText settings.theme settings.browserEnv.translations
            ]
        ]


emailSuggestion : Language -> Maybe String -> Maybe String
emailSuggestion language maybeEmail =
    maybeEmail
        |> Maybe.andThen
            (\email ->
                Mailcheck.suggestWith (suggestedDomains language) (suggestedSlds language) (suggestedTlds language) email
                    |> Maybe.andThen
                        (\( _, _, suggestion ) ->
                            if Subscribers.emailValid suggestion then
                                Just suggestion

                            else
                                Nothing
                        )
            )


suggestedDomains : Language -> List String
suggestedDomains language =
    case language of
        German "AT" ->
            [ "a1.at"
            , "chello.at"
            , "gmx.at"
            , "iinet.at"
            , "inode.at"
            ]
                ++ Mailcheck.defaultDomains

        German "CH" ->
            [ "bluewin.ch"
            , "gmx.ch"
            , "green.ch"
            , "hispeed.ch"
            , "sunrise.ch"
            ]
                ++ Mailcheck.defaultDomains

        German "DE" ->
            [ "freenet.de"
            , "gmx.de"
            , "gmx.net"
            , "mail.de"
            , "posteo.de"
            , "t-online.de"
            , "web.de"
            ]
                ++ Mailcheck.defaultDomains

        English "AU" ->
            [ "bigpond.com"
            , "bigpond.net.au"
            , "iinet.net.au"
            , "internode.on.net"
            , "opusnet.com.au"
            , "tpg.com.au"
            ]
                ++ Mailcheck.defaultDomains

        English "UK" ->
            [ "btinternet.com"
            , "sky.com"
            , "talktalk.com"
            , "virginmedia.com"
            , "yahoo.co.uk"
            ]
                ++ Mailcheck.defaultDomains

        English "US" ->
            [ "att.net"
            , "comcast.net"
            , "hotmail.com"
            , "live.com"
            , "outlook.com"
            , "yahoo.com"
            ]
                ++ Mailcheck.defaultDomains

        _ ->
            Mailcheck.defaultDomains


suggestedTlds : Language -> List String
suggestedTlds language =
    case language of
        German "AT" ->
            [ "at"
            , "com"
            , "net"
            ]

        German "DE" ->
            [ "com"
            , "de"
            , "net"
            ]

        German "CH" ->
            [ "ch"
            , "com"
            , "net"
            ]

        English "AU" ->
            [ "com"
            , "com.au"
            , "net.au"
            ]

        English "UK" ->
            [ "com"
            , "co.uk"
            ]

        English "US" ->
            [ "com"
            , "edu"
            , "gov"
            , "net"
            , "org"
            ]

        _ ->
            Mailcheck.defaultTopLevelDomains


suggestedSlds : Language -> List String
suggestedSlds language =
    case language of
        German "AT" ->
            [ "aon"
            , "web"
            ]
                ++ Mailcheck.defaultSecondLevelDomains

        German "DE" ->
            [ "web"
            , "posteo"
            , "pm"
            , "protonmail"
            ]
                ++ Mailcheck.defaultSecondLevelDomains

        German "CH" ->
            [ "web"
            , "pm"
            , "protonmail"
            ]
                ++ Mailcheck.defaultSecondLevelDomains

        _ ->
            Mailcheck.defaultSecondLevelDomains


viewPrivacyText : Theme -> I18Next.Translations -> Html msg
viewPrivacyText theme translations =
    let
        styles =
            stylesForTheme theme
    in
    div
        (styles.colorStyleGrayscaleMuted
            ++ styles.textStyle14
            ++ [ css
                    [ Tw.mt_2
                    , Tw.flex
                    , Tw.justify_center
                    ]
               ]
        )
        [ text <| Translations.privacyInfo [ translations ] ]


viewSendingDialog : EmailSubscriptionDialog msg -> Html (Msg msg)
viewSendingDialog (Settings settings) =
    div
        [ css
            [ Tw.w_full
            , Tw.max_w_sm
            , Tw.mt_2
            ]
        ]
        [ div
            [ css
                [ Tw.mb_4
                ]
            ]
            [ text <| Translations.sendingMessageText [ settings.browserEnv.translations ]
            ]
        ]


viewResultDialog : EmailSubscriptionDialog msg -> OptInStatus -> Html (Msg msg)
viewResultDialog (Settings settings) status =
    let
        message =
            case status of
                OptInSent ->
                    Translations.sentMessageText [ settings.browserEnv.translations ]

                OptInQueued ->
                    Translations.queuedMessageText [ settings.browserEnv.translations ]

                OptInRejected ->
                    Translations.rejectedMessageText [ settings.browserEnv.translations ]
    in
    div
        [ css
            [ Tw.w_full
            , Tw.max_w_sm
            , Tw.mt_2
            ]
        ]
        [ div
            [ css
                [ Tw.mb_4
                ]
            ]
            [ text message
            ]
        , Button.new
            { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
            , onClick = Just CloseDialog
            , theme = settings.theme
            }
            |> Button.withTypePrimary
            |> Button.view
        ]


viewErrorDialog : EmailSubscriptionDialog msg -> String -> Html (Msg msg)
viewErrorDialog (Settings settings) error =
    div
        [ css
            [ Tw.w_full
            , Tw.max_w_sm
            , Tw.mt_2
            ]
        ]
        [ div
            [ css
                [ Tw.mb_4
                ]
            ]
            [ text <| Translations.errorMessageText [ settings.browserEnv.translations ] { error = error }
            ]
        , Button.new
            { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
            , onClick = Just CloseDialog
            , theme = settings.theme
            }
            |> Button.withTypePrimary
            |> Button.view
        ]
