module ModalDialog exposing (ModalDialog(..), view)

import Components.OnboardingDialog
import Html.Styled as Html exposing (Html)


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
            Html.div [] []
