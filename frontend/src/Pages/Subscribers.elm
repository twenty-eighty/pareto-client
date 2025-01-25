module Pages.Subscribers exposing (Model, Msg, page)

import Auth
import Components.Button as Button exposing (Button)
import Components.EmailImportDialog as EmailImportDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Layouts
import Nostr.Event exposing (Kind(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Tailwind.Utilities as Tw
import Translations.Sidebar
import Translations.Subscribers as Translations
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared
        , update = update user shared
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { emailImportDialog : EmailImportDialog.Model
    }


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { emailImportDialog = EmailImportDialog.init {}
      }
    , loadSubscribers
    )


loadSubscribers : Effect Msg
loadSubscribers =
    Effect.none



-- UPDATE


type Msg
    = ImportClicked
    | EmailImportDialogSent (EmailImportDialog.Msg Msg)


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ImportClicked ->
            ( { model | emailImportDialog = EmailImportDialog.show model.emailImportDialog }, Effect.none )

        EmailImportDialogSent innerMsg ->
            EmailImportDialog.update
                { msg = innerMsg
                , model = model.emailImportDialog
                , nostr = shared.nostr
                , toModel = \emailImportDialog -> { model | emailImportDialog = emailImportDialog }
                , toMsg = EmailImportDialogSent
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Auth.User -> Shared.Model.Model -> Model -> View Msg
view user shared model =
    { title = Translations.Sidebar.subscribersMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                , Tw.m_2
                ]
            ]
            [ Button.new
                { label = Translations.importButtonTitle [ shared.browserEnv.translations ]
                , onClick = Just <| ImportClicked
                , theme = shared.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            , viewSubscribers user.pubKey shared
            , EmailImportDialog.new
                { model = model.emailImportDialog
                , toMsg = EmailImportDialogSent
                , nostr = shared.nostr
                , pubKey = user.pubKey
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> EmailImportDialog.view
            ]
        ]
    }


viewSubscribers : PubKey -> Shared.Model -> Html Msg
viewSubscribers userPubKey shared =
    div
        []
        [ text <| Translations.noSubscribersText [ shared.browserEnv.translations ]
        ]
