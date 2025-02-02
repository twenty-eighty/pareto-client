module Nostr.Nip27 exposing (GetProfileFunction, collectNostrLinks, subsituteNostrLinks)

import Html.Styled as Html exposing (Html, a, text)
import Html.Styled.Attributes exposing (href)
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileDisplayName)
import Nostr.Types exposing (PubKey)
import Regex exposing (Regex)
import String
import Ui.Styles exposing (Styles)


type alias GetProfileFunction =
    PubKey -> Maybe Profile



-- Main function to parse text and substitute nostr links


subsituteNostrLinks : Styles msg -> GetProfileFunction -> String -> Html msg
subsituteNostrLinks styles fnGetProfile text =
    case parseHelper styles fnGetProfile text [] of
        [] ->
            Html.text text

        [ oneElement ] ->
            oneElement

        moreElements ->
            Html.span [] moreElements



-- Define the regex for Nostr NIP-27 links


nostrRegex : Regex
nostrRegex =
    Regex.fromString "nostr:[a-zA-Z0-9]+(?:[?#].*)?" |> Maybe.withDefault Regex.never



-- Function to collect all Nostr links from a given string


collectNostrLinks : String -> List Nip19.NIP19Type
collectNostrLinks input =
    Regex.find nostrRegex input
        |> List.map (\match -> match.match)
        |> List.map (String.dropLeft 6)
        |> List.map Nip19.decode
        |> List.filterMap Result.toMaybe



-- Helper function to recursively parse the text


parseHelper : Styles msg -> GetProfileFunction -> String -> List (Html msg) -> List (Html msg)
parseHelper styles fnGetProfile remainingText acc =
    case String.indexes "nostr:" remainingText |> List.head of
        Nothing ->
            -- No more nostr links
            List.reverse (text remainingText :: acc)

        Just index ->
            let
                -- Split the text at the index of "nostr:"
                before =
                    String.left index remainingText

                afterNostr =
                    String.dropLeft index remainingText

                -- Drop the "nostr:" prefix before extracting the link
                afterPrefix =
                    String.dropLeft 6 afterNostr

                ( nostrLink, rest ) =
                    extractNostrLink afterPrefix

                -- Build the Html elements
                newAcc =
                    if String.isEmpty before then
                        acc

                    else
                        text before :: acc

                -- Reconstruct the full matched link string: "nostr:" ++ nostrLink
                fullNostrLink =
                    "nostr:" ++ nostrLink

                linkHtml =
                    generateNostrLink styles fnGetProfile fullNostrLink
            in
            parseHelper styles fnGetProfile rest (linkHtml :: newAcc)



-- Function to extract the nostr link and the rest of the text


extractNostrLink : String -> ( String, String )
extractNostrLink str =
    let
        -- Bech32 data characters
        bech32Chars =
            "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

        -- HRP can be lowercase alphanumeric
        isHrpChar c =
            Char.isAlpha c && Char.isLower c || Char.isDigit c

        isDataChar c =
            String.contains (String.fromChar c) bech32Chars

        extractHelper acc seenOne remaining =
            case String.uncons remaining of
                Nothing ->
                    ( String.fromList (List.reverse acc), "" )

                Just ( c, rest ) ->
                    if isWhitespace c then
                        -- Stop at whitespace
                        ( String.fromList (List.reverse acc), remaining )

                    else if seenOne then
                        -- We're past the '1'; only bech32 chars allowed
                        if isDataChar c then
                            extractHelper (c :: acc) True rest

                        else
                            ( String.fromList (List.reverse acc), remaining )

                    else
                    -- Before we hit '1', we allow HRP chars + '1'
                    if
                        c == '1'
                    then
                        -- Now we start strict bech32 data mode
                        extractHelper (c :: acc) True rest

                    else if isHrpChar c then
                        extractHelper (c :: acc) False rest

                    else
                        -- Invalid HRP char
                        ( String.fromList (List.reverse acc), remaining )
    in
    extractHelper [] False str



-- Helper function to check for whitespace characters


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}'



-- Function to generate the Html link based on the nostr link type


generateNostrLink : Styles msg -> GetProfileFunction -> String -> Html msg
generateNostrLink styles fnGetProfile nostrLink =
    let
        -- Remove the "nostr:" prefix
        linkContent =
            String.dropLeft 6 nostrLink

        linkData =
            case Nip19.decode linkContent of
                Ok (Nip19.NAddr _) ->
                    Just ( shortenedLinkText linkContent, "/a/" ++ linkContent )

                Ok (Nip19.NEvent _) ->
                    Just ( shortenedLinkText linkContent, "/a/" ++ linkContent )

                Ok (Nip19.Note _) ->
                    Just ( shortenedLinkText linkContent, "/a/" ++ linkContent )

                Ok (Nip19.Npub pubKey) ->
                    case fnGetProfile pubKey of
                        Just profile ->
                            Just ( profileDisplayName pubKey profile, "/p/" ++ linkContent )

                        Nothing ->
                            Just ( shortenedLinkText linkContent, "/p/" ++ linkContent )

                Ok (Nip19.NProfile { pubKey }) ->
                    case fnGetProfile pubKey of
                        Just profile ->
                            Just ( profileDisplayName pubKey profile, "/p/" ++ linkContent )

                        Nothing ->
                            Just ( shortenedLinkText linkContent, "/p/" ++ linkContent )

                Ok (Nip19.Nsec _) ->
                    Nothing

                Ok (Nip19.NRelay _) ->
                    Nothing

                Ok (Nip19.Unknown _) ->
                    Nothing

                Err _ ->
                    Nothing
    in
    case linkData of
        Just ( linkText, linkUrl ) ->
            a
                (styles.textStyleLinks
                    ++ styles.colorStyleLinks
                    ++ [ href linkUrl ]
                )
                [ text <| linkText ]

        Nothing ->
            Html.text "<invalid reference>"


shortenedLinkText : String -> String
shortenedLinkText linkText =
    String.left 9 linkText ++ "..." ++ String.right 6 linkText
