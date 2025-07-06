module Ui.Links exposing (..)

import BrowserEnv exposing (Environment(..))
import Erl
import Html.Styled as Html exposing (Html, a, div)
import Html.Styled.Attributes as Attr
import Nostr.Nip05 as Nip05
import Nostr.Nip19  as Nip19 exposing (NIP19Type(..))
import Nostr.Profile exposing (Author(..), Profile, ProfileValidation(..))
import Nostr.Types exposing (PubKey)
import Pareto
import Route.Path exposing (Path(..))
import Url.Builder


scaledImageLink : Environment -> Int -> String -> String
scaledImageLink environment width url =
    let
        imageCacheServerApi =
            "https://image-caching-server.onrender.com"
    in
    if url /= "" && environment /= StandAlone then
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
    case linkToAuthor True author of
        Just url ->
            a (linkAttributes ++ [ Attr.href url ])

        Nothing ->
            div []


linkElementForProfile : Bool -> Bool -> Profile -> ProfileValidation -> (List (Html msg) -> Html msg)
linkElementForProfile relative followLinks profile validation =
    let
        linkAttributes =
            if not followLinks then
                [ Attr.rel "nofollow" ]

            else
                []
    in
    case linkToProfile relative profile validation of
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
    case linkToProfilePubKey True pubKey of
        Just url ->
            a (linkAttributes ++ [ Attr.href url ])

        Nothing ->
            div []


linkToAuthor : Bool -> Author -> Maybe String
linkToAuthor relative author =
    case author of
        AuthorPubkey pubKey ->
            linkToProfilePubKey relative pubKey

        AuthorProfile profile validation ->
            linkToProfile relative profile validation


linkToProfile : Bool -> Profile -> ProfileValidation -> Maybe String
linkToProfile relative profile validation =
    let
        path =
            case ( validation, profile.nip05 ) of
                ( ValidationSucceeded, Just nip05 ) ->
                    -- only link to NIP-05 if profile was validated
                    -- otherwise the page may not be loadable
                    U_User_ { user = Nip05.nip05ToString nip05 }
                    |> Route.Path.toString
                    |> Just

                ( _, _ ) ->
                    linkToProfilePubKey relative profile.pubKey
    in
    if not relative then
        Maybe.map (String.append Pareto.applicationUrl) path

    else
        path


linkToNJump : NIP19Type -> Maybe String
linkToNJump nip19 =
    case Nip19.encode nip19 of
        Ok encoded ->
            Just <| "https://njump.me/" ++ encoded

        Err _ ->
            Nothing



linkToPicturePost : Bool -> NIP19Type -> Maybe String
linkToPicturePost relative nip19 =
    let
        path =
            case Nip19.encode nip19 of
                Ok encoded ->
                    E_Event_ { event = encoded }
                    |> Route.Path.toString
                    |> Just

                Err _ ->
                    Nothing

    in
    if not relative then
        Maybe.map (String.append Pareto.applicationUrl) path

    else
        path


linkToProfilePubKey : Bool -> PubKey -> Maybe String
linkToProfilePubKey relative pubKey =
    let
        path =
            case Nip19.encode <| Nip19.NProfile { pubKey = pubKey, relays = [] } of
                Ok nprofile ->
                    P_Profile_ { profile = nprofile }
                    |> Route.Path.toString
                    |> Just

                Err _ ->
                    Nothing
    in
    if not relative then
        Maybe.map (String.append Pareto.applicationUrl) path

    else
        path

