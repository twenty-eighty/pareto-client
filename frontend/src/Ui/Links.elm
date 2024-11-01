module Ui.Links exposing (..)

import Html.Styled as Html exposing (Html, Attribute, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Author(..), Profile)
import Nostr.Types exposing (PubKey)
import Nostr.Nip05 as Nip05

linkElementForAuthor : Author -> (List (Html msg) -> Html msg)
linkElementForAuthor author =
    case linkToAuthor author of
        Just url ->
            a [ href url ]

        Nothing ->
            div []

linkElementForProfile : Profile -> (List (Html msg) -> Html msg)
linkElementForProfile profile =
    case linkToProfile profile of
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

linkToAuthor : Author -> Maybe String
linkToAuthor author =
    case author of
        AuthorPubkey pubKey ->
            linkToProfilePubKey pubKey

        AuthorProfile profile ->
            linkToProfile profile


linkToProfile : Profile -> Maybe String
linkToProfile profile =
    case profile.nip05 of
        Just nip05 ->
            Just <| "/u/" ++ Nip05.nip05ToString nip05

        Nothing ->
            linkToProfilePubKey profile.pubKey

linkToProfilePubKey : PubKey -> Maybe String
linkToProfilePubKey pubKey =
    case Nip19.encode <| Nip19.NProfile { pubKey = pubKey, relays = [] } of
        Ok nprofile ->
            Just <| "/p/" ++ nprofile

        Err _ ->
            Nothing