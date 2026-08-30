module Markdown.Footnotes exposing (rewrite)

{-| Rewrite GFM-style footnotes into HTML tags that elm-markdown can parse.

Syntax supported:

  - References: `[^label]`
  - Definitions: `[^label]: content`
  - Continuations: indented lines (2+ spaces or a tab), as in GFM

Unindented content after a blank line ends the footnote and stays in the
article body (same as other GFM renderers), so lone URL lines there use the
normal oembed path.

A footnote whose entire body is a single URL is rewritten to an `embedlink`
so it can use the same LinkPreview/oembed rendering as article embeds.
-}

import Dict exposing (Dict)
import Regex exposing (Regex)


rewrite : String -> String
rewrite markdown =
    let
        ( bodyWithoutDefs, defs ) =
            extractDefinitions markdown
    in
    if Dict.isEmpty defs then
        markdown

    else
        let
            ( rewrittenBody, orderedLabels ) =
                rewriteReferencesOutsideCode bodyWithoutDefs defs
        in
        if List.isEmpty orderedLabels then
            rewrittenBody

        else
            rewrittenBody
                ++ "\n\n"
                ++ footnotesSectionHtml orderedLabels defs



-- Definitions


type alias ExtractState =
    { bodyLines : List String
    , defs : Dict String String
    , current : Maybe CurrentDef
    }


type alias CurrentDef =
    { label : String
    , bodyLines : List String
    }


extractDefinitions : String -> ( String, Dict String String )
extractDefinitions markdown =
    let
        result =
            markdown
                |> String.split "\n"
                |> List.foldl consumeDefinitionLine emptyExtractState
                |> flushCurrentDef
    in
    ( result.bodyLines
        |> List.reverse
        |> String.join "\n"
    , result.defs
    )


emptyExtractState : ExtractState
emptyExtractState =
    { bodyLines = []
    , defs = Dict.empty
    , current = Nothing
    }


consumeDefinitionLine : String -> ExtractState -> ExtractState
consumeDefinitionLine line state =
    case parseDefinitionStart line of
        Just ( label, firstContent ) ->
            state
                |> flushCurrentDef
                |> startDef label firstContent

        Nothing ->
            case state.current of
                Just current ->
                    if isDefinitionContinuation line then
                        { state
                            | current =
                                Just
                                    { current
                                        | bodyLines = unindentContinuation line :: current.bodyLines
                                    }
                        }

                    else if String.trim line == "" then
                        -- Blank lines may sit between indented footnote paragraphs.
                        -- An unindented line after this will end the footnote (GFM).
                        { state
                            | current =
                                Just { current | bodyLines = "" :: current.bodyLines }
                        }

                    else
                        -- Unindented non-blank line: footnote ends. Remaining content
                        -- is normal markdown (so lone URL lines use the usual oembed path).
                        state
                            |> flushCurrentDef
                            |> appendBodyLine line

                Nothing ->
                    appendBodyLine line state


parseDefinitionStart : String -> Maybe ( String, String )
parseDefinitionStart line =
    case Regex.find definitionStartRegex line of
        match :: _ ->
            case match.submatches of
                [ Just label, Just content ] ->
                    Just ( label, content )

                [ Just label, Nothing ] ->
                    Just ( label, "" )

                _ ->
                    Nothing

        [] ->
            Nothing


definitionStartRegex : Regex
definitionStartRegex =
    Regex.fromString "^ {0,3}\\[\\^([^\\]\\s]+)\\]:\\s?(.*)$"
        |> Maybe.withDefault Regex.never


isDefinitionContinuation : String -> Bool
isDefinitionContinuation line =
    String.startsWith "\t" line
        || String.startsWith "  " line


unindentContinuation : String -> String
unindentContinuation line =
    if String.startsWith "\t" line then
        String.dropLeft 1 line

    else if String.startsWith "    " line then
        String.dropLeft 4 line

    else if String.startsWith "  " line then
        String.dropLeft 2 line

    else
        line


startDef : String -> String -> ExtractState -> ExtractState
startDef label firstContent state =
    { state
        | current =
            Just
                { label = label
                , bodyLines = [ firstContent ]
                }
    }


appendBodyLine : String -> ExtractState -> ExtractState
appendBodyLine line state =
    { state | bodyLines = line :: state.bodyLines }


flushCurrentDef : ExtractState -> ExtractState
flushCurrentDef state =
    case state.current of
        Nothing ->
            state

        Just current ->
            let
                body =
                    current.bodyLines
                        |> List.reverse
                        |> dropWhileBlank
                        |> dropTrailingBlankLines
                        |> String.join "\n"
                        |> String.trimRight
            in
            { state
                | current = Nothing
                , defs =
                    -- Drop empty defs (e.g. `[^n]:` whose body was only blank lines
                    -- before unindented article content). Matching refs stay literal.
                    if String.trim body == "" then
                        state.defs

                    else
                        Dict.insert current.label body state.defs
            }


dropTrailingBlankLines : List String -> List String
dropTrailingBlankLines lines =
    lines
        |> List.reverse
        |> dropWhileBlank
        |> List.reverse


dropWhileBlank : List String -> List String
dropWhileBlank lines =
    case lines of
        line :: rest ->
            if String.trim line == "" then
                dropWhileBlank rest

            else
                lines

        [] ->
            []



-- References


type alias Segment =
    { isCode : Bool
    , text : String
    }


type alias RefRewriteState =
    { pieces : List String
    , orderedLabels : List String
    , occurrenceCounts : Dict String Int
    , cursor : Int
    }


rewriteReferencesOutsideCode : String -> Dict String String -> ( String, List String )
rewriteReferencesOutsideCode body defs =
    let
        ( rewrittenSegments, orderedLabels, _ ) =
            body
                |> splitFencedCode
                |> List.foldl
                    (\segment ( accSegments, accLabels, accCounts ) ->
                        if segment.isCode then
                            ( segment.text :: accSegments, accLabels, accCounts )

                        else
                            let
                                ( rewritten, labels, counts ) =
                                    rewriteReferencesInText segment.text defs accLabels accCounts
                            in
                            ( rewritten :: accSegments, labels, counts )
                    )
                    ( [], [], Dict.empty )
    in
    ( rewrittenSegments
        |> List.reverse
        |> String.concat
    , orderedLabels
    )


{-| Split on fenced code blocks delimited by ``` so footnote refs inside code are preserved.
-}
splitFencedCode : String -> List Segment
splitFencedCode text =
    let
        parts =
            String.split "```" text

        partCount =
            List.length parts
    in
    parts
        |> List.indexedMap
            (\index part ->
                if modBy 2 index == 0 then
                    { isCode = False, text = part }

                else if index == partCount - 1 then
                    -- Unclosed fence: keep the opening delimiter and remainder as-is.
                    { isCode = True, text = "```" ++ part }

                else
                    { isCode = True, text = "```" ++ part ++ "```" }
            )


rewriteReferencesInText : String -> Dict String String -> List String -> Dict String Int -> ( String, List String, Dict String Int )
rewriteReferencesInText text defs existingLabels existingCounts =
    let
        matches =
            Regex.find footnoteRefRegex text

        initial : RefRewriteState
        initial =
            { pieces = []
            , orderedLabels = existingLabels
            , occurrenceCounts = existingCounts
            , cursor = 0
            }

        final =
            List.foldl
                (\match state ->
                    case match.submatches of
                        (Just label) :: _ ->
                            let
                                before =
                                    String.slice state.cursor match.index text

                                ( html, nextLabels, nextCounts ) =
                                    buildRefHtml label defs state.orderedLabels state.occurrenceCounts
                            in
                            { pieces = html :: before :: state.pieces
                            , orderedLabels = nextLabels
                            , occurrenceCounts = nextCounts
                            , cursor = match.index + String.length match.match
                            }

                        _ ->
                            state
                )
                initial
                matches

        trailing =
            String.dropLeft final.cursor text
    in
    ( (trailing :: final.pieces)
        |> List.reverse
        |> String.concat
    , final.orderedLabels
    , final.occurrenceCounts
    )


footnoteRefRegex : Regex
footnoteRefRegex =
    Regex.fromString "\\[\\^([^\\]\\s]+)\\]"
        |> Maybe.withDefault Regex.never


buildRefHtml : String -> Dict String String -> List String -> Dict String Int -> ( String, List String, Dict String Int )
buildRefHtml label defs orderedLabels occurrenceCounts =
    case Dict.get label defs of
        Nothing ->
            ( "[^" ++ label ++ "]", orderedLabels, occurrenceCounts )

        Just _ ->
            let
                ( number, nextLabels ) =
                    case labelNumber label orderedLabels of
                        Just n ->
                            ( n, orderedLabels )

                        Nothing ->
                            let
                                labels =
                                    orderedLabels ++ [ label ]
                            in
                            ( List.length labels, labels )

                nextCount =
                    (Dict.get label occurrenceCounts |> Maybe.withDefault 0) + 1

                nextCounts =
                    Dict.insert label nextCount occurrenceCounts

                safe =
                    sanitizeId label

                refId =
                    if nextCount == 1 then
                        "fnref-" ++ safe

                    else
                        "fnref-" ++ safe ++ "-" ++ String.fromInt nextCount
            in
            ( "<footnoteref id=\""
                ++ refId
                ++ "\" href=\"#fn-"
                ++ safe
                ++ "\" number=\""
                ++ String.fromInt number
                ++ "\"></footnoteref>"
            , nextLabels
            , nextCounts
            )


labelNumber : String -> List String -> Maybe Int
labelNumber label orderedLabels =
    orderedLabels
        |> List.indexedMap (\i l -> ( i + 1, l ))
        |> List.filterMap
            (\( n, l ) ->
                if l == label then
                    Just n

                else
                    Nothing
            )
        |> List.head



-- Footnotes section HTML


footnotesSectionHtml : List String -> Dict String String -> String
footnotesSectionHtml orderedLabels defs =
    let
        items =
            orderedLabels
                |> List.indexedMap
                    (\index label ->
                        let
                            number =
                                index + 1

                            body =
                                Dict.get label defs
                                    |> Maybe.withDefault ""
                                    |> escapeFootnoteCloseTags
                                    |> formatFootnoteBody

                            safe =
                                sanitizeId label
                        in
                        "<footnote id=\"fn-"
                            ++ safe
                            ++ "\" number=\""
                            ++ String.fromInt number
                            ++ "\" backhref=\"#fnref-"
                            ++ safe
                            ++ "\">\n"
                            ++ body
                            ++ "\n</footnote>"
                    )
                |> String.join "\n"
    in
    "<footnotes>\n" ++ items ++ "\n</footnotes>"


{-| Lone URL footnote bodies become an embedlink tag so the renderer can show
an oembed (or a normal link fallback). elm-markdown does not autolink bare URLs.
-}
formatFootnoteBody : String -> String
formatFootnoteBody body =
    case loneFootnoteUrl body of
        Just url ->
            "<embedlink href=\""
                ++ escapeHtmlAttr url
                ++ "\">"
                ++ escapeHtmlText url
                ++ "</embedlink>"

        Nothing ->
            body


loneFootnoteUrl : String -> Maybe String
loneFootnoteUrl body =
    let
        trimmed =
            String.trim body
    in
    if isBareHttpUrl trimmed then
        Just trimmed

    else
        case parseSoleMarkdownLink trimmed of
            Just url ->
                Just url

            Nothing ->
                parseSoleAngleAutolink trimmed


isBareHttpUrl : String -> Bool
isBareHttpUrl text =
    (String.startsWith "http://" text || String.startsWith "https://" text)
        && not (String.contains " " text)
        && not (String.contains "\t" text)
        && not (String.contains "\n" text)


parseSoleMarkdownLink : String -> Maybe String
parseSoleMarkdownLink text =
    case Regex.find soleMarkdownLinkRegex text of
        match :: [] ->
            case match.submatches of
                [ _, Just url ] ->
                    if match.match == text then
                        Just url

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


soleMarkdownLinkRegex : Regex
soleMarkdownLinkRegex =
    Regex.fromString "^\\[([^\\]]*)\\]\\((https?://[^\\s\\)]+)\\)$"
        |> Maybe.withDefault Regex.never


parseSoleAngleAutolink : String -> Maybe String
parseSoleAngleAutolink text =
    case Regex.find soleAngleAutolinkRegex text of
        match :: [] ->
            case match.submatches of
                [ Just url ] ->
                    if match.match == text then
                        Just url

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


soleAngleAutolinkRegex : Regex
soleAngleAutolinkRegex =
    Regex.fromString "^<(https?://[^\\s>]+)>$"
        |> Maybe.withDefault Regex.never


escapeHtmlAttr : String -> String
escapeHtmlAttr value =
    value
        |> String.replace "&" "&amp;"
        |> String.replace "\"" "&quot;"
        |> String.replace "<" "&lt;"


escapeHtmlText : String -> String
escapeHtmlText value =
    value
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"


escapeFootnoteCloseTags : String -> String
escapeFootnoteCloseTags body =
    body
        |> String.replace "</footnote>" "&lt;/footnote>"
        |> String.replace "</footnotes>" "&lt;/footnotes>"
        |> String.replace "</embedlink>" "&lt;/embedlink>"


sanitizeId : String -> String
sanitizeId label =
    label
        |> String.toList
        |> List.map
            (\char ->
                if Char.isAlphaNum char || char == '-' || char == '_' then
                    String.fromChar char

                else
                    "-"
            )
        |> String.concat
