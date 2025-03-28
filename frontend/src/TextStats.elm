module TextStats exposing (TextStats, compute, computeTask, emptyTextStats, view)

import BrowserEnv exposing (BrowserEnv)
import Char
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Locale exposing (Language(..))
import Markdown
import String
import Tailwind.Utilities as Tw
import Task exposing (Task)
import Translations.TextStats as Translations
import Ui.Styles exposing (Theme, stylesForTheme)


type alias TextStats =
    { bytes : Int
    , characters : Int
    , words : Int
    , sentences : Int
    , readingTime : Float -- in minutes
    , speakingTime : Float -- in minutes
    }


emptyTextStats : TextStats
emptyTextStats =
    { bytes = 0
    , characters = 0
    , words = 0
    , sentences = 0
    , readingTime = 0.0
    , speakingTime = 0.0
    }


computeTask : Maybe Language -> String -> Task Never TextStats
computeTask maybeLanguage content =
    Task.succeed (compute maybeLanguage content)


{-| Compute statistics about the given text.
The statistics include:

  - Byte count (assuming the text is stored as UTF-8)
  - Character count
  - Word count
  - Sentence count (with language-specific abbreviation handling)
  - Estimated reading time (200 words per minute)
  - Estimated speaking time (130 words per minute)

-}
compute : Maybe Language -> String -> TextStats
compute maybeLanguage markdown =
    let
        plainText =
            Markdown.collectText markdown
                |> Result.toMaybe
                |> Maybe.withDefault markdown

        bytes =
            utf8ByteLength markdown

        characters =
            String.length plainText

        words =
            plainText
                |> String.split " "
                |> List.filter (\w -> String.trim w /= "")
                |> List.length

        -- Clean abbreviations so that dots in things like "e.g." aren’t counted.
        cleanedText =
            case maybeLanguage of
                Just language ->
                    cleanAbbreviations language plainText

                Nothing ->
                    plainText

        sentences =
            countSentences cleanedText

        readingTime =
            toFloat words / 200.0

        speakingTime =
            toFloat words / 130.0
    in
    { bytes = bytes
    , characters = characters
    , words = words
    , sentences = sentences
    , readingTime = readingTime
    , speakingTime = speakingTime
    }


{-| Compute the number of bytes a string takes when encoded in UTF-8.
For each character, the number of bytes is determined as:

  - 1 byte if the code point is ≤ 0x7F
  - 2 bytes if the code point is ≤ 0x7FF
  - 3 bytes if the code point is ≤ 0xFFFF
  - 4 bytes otherwise

-}
utf8ByteLength : String -> Int
utf8ByteLength text =
    text
        |> String.toList
        |> List.map Char.toCode
        |> List.map
            (\code ->
                if code <= 0x7F then
                    1

                else if code <= 0x07FF then
                    2

                else if code <= 0xFFFF then
                    3

                else
                    4
            )
        |> List.sum


{-| Clean known abbreviations from the text by removing their trailing dot.
This helps prevent miscounting dots within abbreviations as sentence ends.
-}
cleanAbbreviations : Language -> String -> String
cleanAbbreviations language text =
    let
        abbreviations =
            case language of
                English _ ->
                    [ "e.g.", "i.e.", "Mr.", "Mrs.", "Dr.", "Inc.", "Ltd.", "Co.", "Corp.", "Jr.", "Sr.", "etc.", "vs.", "U.S.", "U.K." ]

                French ->
                    [ "M.", "Mme.", "Mlle.", "Dr.", "Sté.", "etc." ]

                German _ ->
                    [ "z.B.", "Dr.", "Prof.", "u.a.", "d.h.", "Hr.", "etc.", "usw.", "u.U." ]

                Italian ->
                    [ "ad es.", "Dr.", "Sig.", "Sig.ra", "Prof.", "ecc.", "avv." ]

                Portuguese ->
                    [ "Sr.", "Sra.", "Dr.", "Prof.", "etc." ]

                Spanish ->
                    [ "Sr.", "Sra.", "Dr.", "Prof.", "etc.", "p.ej.", "Srta." ]

                Swedish ->
                    [ "t.ex.", "Dr.", "bl.a.", "d.v.s.", "etc." ]

                Dutch ->
                    [ "dhr.", "mevr.", "bv.", "enz.", "etc.", "prof." ]

                Finnish ->
                    [ "esim.", "jne.", "m.m." ]

                Greek ->
                    [ "π.χ.", "κ.λπ.", "δηλ." ]

                Norwegian ->
                    [ "f.eks.", "bl.a.", "osv.", "m.fl.", "dvs." ]

                Russian ->
                    [ "т.е.", "и т.д.", "т.к.", "рис.", "стр." ]
    in
    List.foldl (\abbr acc -> String.replace abbr (removeTrailingDot abbr) acc) text abbreviations


{-| Remove the trailing dot from a string if it exists.
-}
removeTrailingDot : String -> String
removeTrailingDot abbr =
    if String.endsWith "." abbr then
        String.dropRight 1 abbr

    else
        abbr


{-| Determines if a character is a sentence-ending delimiter.
-}
isSentenceDelimiter : Char -> Bool
isSentenceDelimiter c =
    c == '.' || c == '!' || c == '?'


{-| Checks whether a punctuation mark qualifies as a sentence delimiter.
It qualifies if it is the last character, or if the next character is whitespace.
-}
qualifiesAsSentenceDelimiter : List Char -> Bool
qualifiesAsSentenceDelimiter rest =
    case rest of
        [] ->
            True

        c :: _ ->
            isWhitespace c


{-| Checks if a character is considered whitespace.
This custom function treats space, newline, tab, and carriage return as whitespace.
-}
isWhitespace : Char -> Bool
isWhitespace c =
    case c of
        ' ' ->
            True

        '\n' ->
            True

        '\t' ->
            True

        '\u{000D}' ->
            True

        _ ->
            False


{-| Count the number of sentences in the text.
This function traverses the list of characters and only counts a punctuation
as a sentence end if it is followed by whitespace (or is the last character).
Contiguous punctuation marks that qualify are treated as a single sentence boundary.
-}
countSentences : String -> Int
countSentences text =
    text
        |> String.toList
        |> countSentencesHelper 0


countSentencesHelper : Int -> List Char -> Int
countSentencesHelper count chars =
    case chars of
        [] ->
            count

        c :: rest ->
            if isSentenceDelimiter c then
                if qualifiesAsSentenceDelimiter rest then
                    let
                        remaining =
                            dropWhile isSentenceDelimiter rest
                    in
                    countSentencesHelper (count + 1) remaining

                else
                    countSentencesHelper count rest

            else
                countSentencesHelper count rest


{-| Custom implementation of dropWhile.
It drops elements from the list as long as they satisfy the given predicate.
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list


view : BrowserEnv -> Theme -> TextStats -> Html msg
view browserEnv theme textStats =
    let
        styles =
            stylesForTheme theme
    in
    div
        (styles.colorStyleGrayscaleMuted
            ++ [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_3
                    , Tw.mb_5
                    ]
               ]
        )
        [ Html.span [] [ text <| Translations.bytesLabel [ browserEnv.translations ] ++ " " ++ String.fromInt textStats.bytes ]
        , Html.span [] [ text <| Translations.charactersLabel [ browserEnv.translations ] ++ " " ++ String.fromInt textStats.characters ]
        , Html.span [] [ text <| Translations.wordsLabel [ browserEnv.translations ] ++ " " ++ String.fromInt textStats.words ]
        , Html.span [] [ text <| Translations.sentencesLabel [ browserEnv.translations ] ++ " " ++ String.fromInt textStats.sentences ]

        -- , Html.span [][text <| Translations.readingTimeLabel [browserEnv.translations] ++ " " ++ String.fromFloat textStats.readingTime]
        -- , Html.span [][text <| Translations.speakingTimeLabel [browserEnv.translations] ++ " " ++ String.fromFloat textStats.speakingTime]
        ]
