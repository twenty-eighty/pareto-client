module EmailValidation exposing (emailValid)

{-| Shared email format check used by auth and newsletter flows.
-}

import Mailcheck


emailValid : String -> Bool
emailValid email =
    Mailcheck.mailParts email
        |> Maybe.map
            (\mailParts ->
                (mailParts.address /= "")
                    && (String.length mailParts.topLevelDomain > 1)
                    && (mailParts.secondLevelDomain /= "")
                    && (numberOfAtChars email == 1)
                    && (not <| String.contains " " email)
            )
        |> Maybe.withDefault False


numberOfAtChars : String -> Int
numberOfAtChars email =
    email
        |> String.indexes "@"
        |> List.length
