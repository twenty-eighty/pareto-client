module Ui.Links exposing (..)

import Html.Styled as Html exposing (Attribute, Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Nostr.Nip05 as Nip05
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Author(..), Profile, ProfileValidation(..))
import Nostr.Types exposing (PubKey)


linkElementForAuthor : Author -> ProfileValidation -> (List (Html msg) -> Html msg)
linkElementForAuthor author validation =
    case linkToAuthor author validation of
        Just url ->
            a [ href url ]

        Nothing ->
            div []


linkElementForProfile : Profile -> ProfileValidation -> (List (Html msg) -> Html msg)
linkElementForProfile profile validation =
    case linkToProfile profile validation of
        Just url ->
            a [ href url ]

        Nothing ->
            div []


linkElementForProfilePubKey : PubKey -> (List (Html msg) -> Html msg)
linkElementForProfilePubKey pubKey =
    case linkToProfilePubKey pubKey of
        Just url ->
            a [ href url ]

        Nothing ->
            div []


linkToAuthor : Author -> ProfileValidation -> Maybe String
linkToAuthor author validation =
    case author of
        AuthorPubkey pubKey ->
            linkToProfilePubKey pubKey

        AuthorProfile profile _ ->
            linkToProfile profile validation


linkToProfile : Profile -> ProfileValidation -> Maybe String
linkToProfile profile validation =
    case ( validation, profile.nip05 ) of
        ( ValidationSucceeded, Just nip05 ) ->
            -- only link to NIP-05 if profile was validated
            -- otherwise the page may not be loadable
            Just <| "/u/" ++ Nip05.nip05ToString nip05

        ( _, _ ) ->
            linkToProfilePubKey profile.pubKey


linkToProfilePubKey : PubKey -> Maybe String
linkToProfilePubKey pubKey =
    case Nip19.encode <| Nip19.NProfile { pubKey = pubKey, relays = [] } of
        Ok nprofile ->
            Just <| "/p/" ++ nprofile

        Err _ ->
            Nothing
