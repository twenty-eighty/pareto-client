module ModalDialog exposing (ModalDialog(..), view)

import Components.OnboardingDialog
import Html.Styled as Html exposing (Html)
import Ui.Shared exposing (emptyHtml)


type ModalDialog
    = None
    | OnboardingDialog (Components.OnboardingDialog.OnboardingDialog Msg)


type Msg
    = NoOp


view : ModalDialog -> Html Msg
view dialog =
    case dialog of
        OnboardingDialog onboardingData ->
            Components.OnboardingDialog.view onboardingData

        None ->
            emptyHtml
