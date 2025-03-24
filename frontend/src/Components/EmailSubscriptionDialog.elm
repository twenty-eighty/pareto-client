module Components.EmailSubscriptionDialog exposing (EmailSubscriptionDialog, Model, Msg, Recipient, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, input, label, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import I18Next
import Json.Decode as Decode
import Locale exposing (Language(..))
import Mailcheck
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Profile exposing (Profile, profileDisplayName)
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Types exposing (IncomingMessage, PubKey)
import Ports
import Shared
import Shared.Model exposing (Model, LoginStatus(..))
import Shared.Msg exposing (Msg)
import Subscribers exposing (SubscribeInfo)
import Tailwind.Theme as Theme
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
        }


type DialogState
    = DialogHidden
    | DialogVisible EmailSubscriptionData
    | DialogSending SendRequestId EmailSubscriptionData
    | DialogSent
    | DialogSendError String


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
        }


show : Model -> Model
show (Model model) =
    Model { model | state = DialogVisible { email = Nothing } }


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
                                Shared.loggedInPubKey loginStatus

                            signingPubKey =
                                Shared.loggedInSigningPubKey loginStatus

                            ( firstName, lastName ) =
                                userPubKey
                                    |> Maybe.andThen (Nostr.getProfile props.nostr)
                                    |> Maybe.andThen .displayName
                                    |> Maybe.map Subscribers.splitName
                                    |> Maybe.withDefault ( Nothing, Nothing )
                        in
                        ( Model { model | state = DialogSending (Nostr.getLastSendRequestId props.nostr) emailSubscriptionData }
                        , sendOptInEmail props.nostr authorProfile signingPubKey { pubKey = userPubKey, email = email, firstName = firstName, lastName = lastName, locale = Just locale }
                        )

                    _ ->
                        ( Model model, Effect.none )

            ReceivedMessage message ->
                ( updateWithMessage (Model model) message, Effect.none )


updateWithMessage : Model -> IncomingMessage -> Model
updateWithMessage (Model model) message =
    case ( model.state, Nostr.External.decodeSendId message.value ) of
        ( DialogSending sendRequestId _, Ok sendId ) ->
            if sendRequestId == sendId then
                case message.messageType of
                    "published" ->
                        Model { model | state = DialogSent }

                    "error" ->
                        case Nostr.External.decodeReason message.value of
                            Ok errorReason ->
                                Model { model | state = DialogSendError errorReason }

                            Err decodingError ->
                                Model { model | state = DialogSendError <| Decode.errorToString decodingError }

                    _ ->
                        Model model

            else
                Model model

        ( _, _ ) ->
            Model model


sendOptInEmail : Nostr.Model -> Profile -> Maybe PubKey -> SubscribeInfo -> Effect msg
sendOptInEmail nostr authorProfile maybeSigningPubKey subscribeInfo =
    Subscribers.subscribeEvent nostr authorProfile maybeSigningPubKey subscribeInfo
        |> SendApplicationData
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg



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

        DialogSent ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewSentDialog dialog ]
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

        emailFieldId =
            "email"
    in
    div
        [ css
            [ Tw.w_full
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
                    [ Tw.mb_1
                    ]
                ]
                [ label
                    [ Attr.for emailFieldId
                    , css
                        [ Tw.block
                        , Tw.mb_2
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_color Theme.gray_700
                        ]
                    ]
                    [ text <| Translations.emailLabel [ settings.browserEnv.translations ] ]
                , input
                    [ Attr.type_ "email"
                    , Attr.id emailFieldId
                    , Attr.name "email"
                    , css
                        [ Tw.w_full
                        , Tw.px_4
                        , Tw.py_2
                        , Tw.border
                        , Tw.border_color Theme.gray_300
                        , Tw.rounded
                        , Css.focus
                            [ Tw.outline_none
                            , Tw.ring_2
                            , Tw.ring_color Theme.blue_500
                            ]
                        ]
                    , Attr.placeholder <| Translations.emailPlaceholder [ settings.browserEnv.translations ]
                    , Attr.required True
                    , Attr.value (Maybe.withDefault "" data.email)
                    , Events.onInput UpdateEmail
                    ]
                    []
                ]
            , viewSuggestion settings.theme settings.browserEnv data.email
            , Button.new
                { label = Translations.subscribeButtonTitle [ settings.browserEnv.translations ]
                , onClick = data.email |> Maybe.map (\email -> SubscribeClicked email settings.profile settings.loginStatus settings.browserEnv.locale)
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled (not emailIsValid)
                |> Button.view
            , viewPrivacyText settings.theme settings.browserEnv.translations
            ]
        ]


viewSuggestion : Theme -> BrowserEnv -> Maybe String -> Html (Msg msg)
viewSuggestion theme browserEnv maybeEmail =
    let
        defaultHtml =
            div [ css [ Tw.mt_4 ] ] []
    in
    maybeEmail
        |> Maybe.map
            (\email ->
                emailSuggestion browserEnv.language (Just email)
                    |> Maybe.map
                        (\suggestion ->
                            let
                                styles =
                                    stylesForTheme theme
                            in
                            div
                                (styles.colorStyleGrayscaleMuted
                                    ++ styles.textStyle14
                                    ++ [ css
                                            [ Tw.ml_4
                                            , Tw.mb_4
                                            , Tw.cursor_pointer
                                            ]
                                       , Events.onClick (UpdateEmail suggestion)
                                       ]
                                )
                                [ text suggestion ]
                        )
                    |> Maybe.withDefault defaultHtml
            )
        |> Maybe.withDefault defaultHtml


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


viewSentDialog : EmailSubscriptionDialog msg -> Html (Msg msg)
viewSentDialog (Settings settings) =
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
            [ text <| Translations.sentMessageText [ settings.browserEnv.translations ]
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
