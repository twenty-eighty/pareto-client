module Ui.Links exposing (..)

import Erl
import Html.Styled as Html exposing (Html, a, div)
import Html.Styled.Attributes as Attr
import Nostr.Nip05 as Nip05
import Nostr.Nip19  as Nip19 exposing (NIP19Type(..))
import Nostr.Profile exposing (Author(..), Profile, ProfileValidation(..))
import Nostr.Types exposing (PubKey)
import Pareto
import Url.Builder


scaledImageLink : Int -> String -> String
scaledImageLink width url =
    let
        imageCacheServerApi =
            "https://image-caching-server.onrender.com"
    in
    if url /= "" then
        Url.Builder.crossOrigin imageCacheServerApi
            [ "api", "scale" ]
            [ Url.Builder.string "url" url
            , Url.Builder.int "width" width
            ]

    else
        url



-- unused as our NIP-96 server doesn't support scaling


extendUrlForScaling : Int -> String -> String
extendUrlForScaling width urlString =
    let
        parsed =
            urlString
                |> Erl.parse
    in
    if isNip96Server parsed then
        parsed
            -- add NIP-96 scaling parameter
            |> Erl.addQuery "w" (String.fromInt width)
            |> Erl.toString

    else
        urlString


isNip96Server : Erl.Url -> Bool
isNip96Server url =
    List.member (String.join "." url.host)
        [ Pareto.paretoNip96Server
        ]


linkElementForAuthor : Bool -> Author -> (List (Html msg) -> Html msg)
linkElementForAuthor followLinks author =
    let
        linkAttributes =
            if not followLinks then
                [ Attr.rel "nofollow" ]

            else
                []
    in
    case linkToAuthor author of
        Just url ->
            a (linkAttributes ++ [ Attr.href url ])

        Nothing ->
            div []


linkElementForProfile : Bool -> Profile -> ProfileValidation -> (List (Html msg) -> Html msg)
linkElementForProfile followLinks profile validation =
    let
        linkAttributes =
            if not followLinks then
                [ Attr.rel "nofollow" ]

            else
                []
    in
    case linkToProfile profile validation of
        Just url ->
            a (linkAttributes ++ [ Attr.href url ])

        Nothing ->
            div []


linkElementForProfilePubKey : Bool -> PubKey -> (List (Html msg) -> Html msg)
linkElementForProfilePubKey followLinks pubKey =
    let
        linkAttributes =
            if not followLinks then
                [ Attr.rel "nofollow" ]

            else
                []
    in
    case linkToProfilePubKey pubKey of
        Just url ->
            a (linkAttributes ++ [ Attr.href url ])

        Nothing ->
            div []


linkToAuthor : Author -> Maybe String
linkToAuthor author =
    case author of
        AuthorPubkey pubKey ->
            linkToProfilePubKey pubKey

        AuthorProfile profile validation ->
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


linkToNJump : NIP19Type -> Maybe String
linkToNJump nip19 =
    case Nip19.encode nip19 of
        Ok encoded ->
            Just <| "https://njump.me/" ++ encoded

        Err _ ->
            Nothing


linkToProfilePubKey : PubKey -> Maybe String
linkToProfilePubKey pubKey =
    case Nip19.encode <| Nip19.NProfile { pubKey = pubKey, relays = [] } of
        Ok nprofile ->
            Just <| "/p/" ++ nprofile

        Err _ ->
            Nothing
