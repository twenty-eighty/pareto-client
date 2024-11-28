module Nostr.Nip27 exposing (subsituteNostrLinks)

import Html.Styled as Html exposing (Html, text, a)
import Html.Styled.Attributes exposing (href)
import String


-- Main function to parse text and substitute nostr links
subsituteNostrLinks : String -> List (Html msg)
subsituteNostrLinks text =
    parseHelper text []



-- Helper function to recursively parse the text
parseHelper : String -> List (Html msg) -> List (Html msg)
parseHelper remainingText acc =
    case String.indexes "nostr:" remainingText |> List.head of
        Nothing ->
            -- No more nostr links
            List.reverse (text remainingText :: acc)

        Just index ->
            let
                -- Split the text at the index of "nostr:"
                before = String.left index remainingText
                afterNostr = String.dropLeft index remainingText

                -- Extract the nostr link and the rest of the text
                (nostrLink, rest) = extractNostrLink afterNostr

                -- Build the Html elements
                newAcc =
                    if String.isEmpty before then
                        acc
                    else
                        text before :: acc

                linkHtml = generateNostrLink nostrLink
            in
            parseHelper rest (linkHtml :: newAcc)


-- Function to extract the nostr link and the rest of the text
extractNostrLink : String -> (String, String)
extractNostrLink str =
    let
        extractHelper acc remaining =
            case String.uncons remaining of
                Nothing ->
                    (String.fromList (List.reverse acc), "")
                Just (c, rest) ->
                    if isWhitespace c then
                        (String.fromList (List.reverse acc), remaining)
                    else
                        extractHelper (c :: acc) rest
    in
    extractHelper [] str


-- Helper function to check for whitespace characters
isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t' || c == '\n' || c == '\r'


-- Function to generate the Html link based on the nostr link type
generateNostrLink : String -> Html msg
generateNostrLink nostrLink =
    let
        -- Remove the "nostr:" prefix
        linkContent = String.dropLeft 6 nostrLink

        -- Determine the type and corresponding path
        (linkType, path) =
            if String.startsWith "nprofile" linkContent then
                ( "nprofile", "/p/" ++ linkContent )
            else if String.startsWith "naddr" linkContent then
                ( "naddr", "/a/" ++ linkContent )
            else if String.startsWith "nevent" linkContent then
                ( "nevent", "/e/" ++ linkContent )
            else
                ( "", "" )
    in
    if linkType == "" then
        -- Not a recognized link type; output as text
        text nostrLink
    else
        a [ href path ] [ text nostrLink ]