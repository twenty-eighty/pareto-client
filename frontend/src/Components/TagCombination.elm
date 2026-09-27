module Components.TagCombination exposing
    ( Model
    , Msg
    , TagCombination
    , TagFilter
    , encode
    , init
    , new
    , removeTag
    , toFilter
    , update
    , view
    )

import Components.Checkbox as Checkbox
import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, button, div, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import I18Next exposing (Translations)
import Json.Encode as Encode
import Tailwind.Utilities as Tw
import Translations.TagCombination as Translations
import Ui.Styles exposing (Theme, darkMode, stylesForTheme)


type TagCombination msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , tags : List String
        , theme : Theme
        , translations : Translations
        }


type Model
    = Model
        { combine : Combine
        , clauses : List Clause
        , nextId : Int
        }


type alias Clause =
    { id : Int
    , polarity : Polarity
    , match : TagMatch
    , tags : List String
    }


type Combine
    = CombineAll
    | CombineAny


type Polarity
    = Include
    | Exclude


type TagMatch
    = AnyOf
    | AllOf


{-| Filter tree accepted by the contacts API.
`any` and `all` name tags. `not`, `and`, and `or` combine those groups.
-}
type TagFilter
    = FilterAny (List String)
    | FilterAll (List String)
    | FilterNot TagFilter
    | FilterAnd (List TagFilter)
    | FilterOr (List TagFilter)


type Msg
    = SetCombine Combine
    | AddClause
    | RemoveClause Int
    | SetPolarity Int Polarity
    | SetMatch Int TagMatch
    | ToggleTag Int String Bool


new :
    { model : Model
    , toMsg : Msg -> msg
    , tags : List String
    , theme : Theme
    , translations : Translations
    }
    -> TagCombination msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , tags = props.tags
        , theme = props.theme
        , translations = props.translations
        }


init : Model
init =
    Model
        { combine = CombineAll
        , clauses = []
        , nextId = 1
        }


removeTag : String -> Model -> Model
removeTag tag (Model model) =
    Model
        { model
            | clauses =
                List.map
                    (\clause ->
                        { clause | tags = List.filter (\selected -> selected /= tag) clause.tags }
                    )
                    model.clauses
        }


toFilter : Model -> Maybe TagFilter
toFilter (Model model) =
    case List.filterMap clauseFilter model.clauses of
        [] ->
            Nothing

        [ single ] ->
            Just single

        many ->
            Just <|
                case model.combine of
                    CombineAll ->
                        FilterAnd many

                    CombineAny ->
                        FilterOr many


encode : TagFilter -> Encode.Value
encode filter =
    case filter of
        FilterAny tags ->
            Encode.object [ ( "any", Encode.list Encode.string tags ) ]

        FilterAll tags ->
            Encode.object [ ( "all", Encode.list Encode.string tags ) ]

        FilterNot inner ->
            Encode.object [ ( "not", encode inner ) ]

        FilterAnd parts ->
            Encode.object [ ( "and", Encode.list encode parts ) ]

        FilterOr parts ->
            Encode.object [ ( "or", Encode.list encode parts ) ]


update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model
    in
    ( props.toModel (Model (apply props.msg model))
    , Effect.none
    )


apply :
    Msg
    ->
        { combine : Combine
        , clauses : List Clause
        , nextId : Int
        }
    ->
        { combine : Combine
        , clauses : List Clause
        , nextId : Int
        }
apply msg model =
    case msg of
        SetCombine combine ->
            { model | combine = combine }

        AddClause ->
            { model
                | clauses =
                    model.clauses
                        ++ [ { id = model.nextId, polarity = Include, match = AnyOf, tags = [] } ]
                , nextId = model.nextId + 1
            }

        RemoveClause id ->
            { model | clauses = List.filter (\clause -> clause.id /= id) model.clauses }

        SetPolarity id polarity ->
            { model | clauses = List.map (mapClause id (\clause -> { clause | polarity = polarity })) model.clauses }

        SetMatch id match ->
            { model | clauses = List.map (mapClause id (\clause -> { clause | match = match })) model.clauses }

        ToggleTag id tag checked ->
            { model | clauses = List.map (mapClause id (toggleTag tag checked)) model.clauses }


view : TagCombination msg -> Html msg
view (Settings settings) =
    viewCombination settings
        |> Html.map settings.toMsg


viewCombination :
    { model : Model
    , toMsg : Msg -> msg
    , tags : List String
    , theme : Theme
    , translations : Translations
    }
    -> Html Msg
viewCombination settings =
    let
        (Model model) =
            settings.model

        styles =
            stylesForTheme settings.theme
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_3
            ]
        ]
        [ if List.length model.clauses > 1 then
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.flex_wrap
                    , Tw.gap_2
                    ]
                ]
                [ choice styles (model.combine == CombineAll) (Translations.matchAllGroupsText [ settings.translations ]) (SetCombine CombineAll)
                , choice styles (model.combine == CombineAny) (Translations.matchAnyGroupsText [ settings.translations ]) (SetCombine CombineAny)
                ]

          else if List.isEmpty model.clauses then
            p (styles.colorStyleGrayscaleMuted ++ [ css [ Tw.text_sm, Tw.m_0 ] ])
                [ text <| Translations.noFilterText [ settings.translations ] ]

          else
            text ""
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_3
                ]
            ]
            (List.map (viewClause styles settings.translations settings.theme settings.tags) model.clauses)
        , div []
            [ textButton styles (Translations.addGroupText [ settings.translations ]) AddClause ]
        ]


viewClause : Ui.Styles.Styles Msg -> Translations -> Theme -> List String -> Clause -> Html Msg
viewClause styles translations theme availableTags clause =
    div
        (styles.colorStyleBorders
            ++ styles.colorStyleBackground
            ++ [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_2
                    , Tw.p_3
                    , Tw.rounded_md
                    , Tw.border
                    ]
               ]
        )
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.flex_wrap
                , Tw.items_center
                , Tw.gap_2
                ]
            ]
            [ choice styles (clause.polarity == Include) (Translations.includeText [ translations ]) (SetPolarity clause.id Include)
            , choice styles (clause.polarity == Exclude) (Translations.excludeText [ translations ]) (SetPolarity clause.id Exclude)
            , spanText styles (Translations.contactsWithText [ translations ])
            , choice styles (clause.match == AnyOf) (Translations.anyOfText [ translations ]) (SetMatch clause.id AnyOf)
            , choice styles (clause.match == AllOf) (Translations.allOfText [ translations ]) (SetMatch clause.id AllOf)
            , div [ css [ Tw.ml_auto ] ]
                [ textButton styles (Translations.removeGroupText [ translations ]) (RemoveClause clause.id) ]
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.flex_wrap
                , Tw.gap_x_4
                , Tw.gap_y_1
                ]
            ]
            (List.map (viewTag theme clause) availableTags)
        ]


viewTag : Theme -> Clause -> String -> Html Msg
viewTag theme clause tag =
    Checkbox.new
        { label = tag
        , onClick = ToggleTag clause.id tag
        , checked = List.member tag clause.tags
        , theme = theme
        }
        |> Checkbox.view


choice : Ui.Styles.Styles Msg -> Bool -> String -> Msg -> Html Msg
choice styles selected label msg =
    button
        (styles.colorStyleBorders
            ++ styles.colorStyleGrayscaleText
            ++ [ Attr.type_ "button"
               , Events.onClick msg
               , css (choiceStyle styles selected)
               ]
        )
        [ text label ]


textButton : Ui.Styles.Styles Msg -> String -> Msg -> Html Msg
textButton styles label msg =
    button
        (styles.colorStyleBorders
            ++ styles.colorStyleGrayscaleText
            ++ [ Attr.type_ "button"
               , Events.onClick msg
               , css (choiceStyle styles False)
               ]
        )
        [ text label ]


choiceStyle styles selected =
    [ Tw.px_3
    , Tw.py_1
    , Tw.text_sm
    , Tw.rounded_md
    , Tw.border
    , Tw.cursor_pointer
    ]
        ++ (if selected then
                [ Tw.font_semibold
                , Tw.bg_color styles.colorB4
                , Tw.text_color styles.colorB1
                , darkMode
                    [ Tw.bg_color styles.colorB4DarkMode
                    , Tw.text_color styles.colorB1DarkMode
                    ]
                ]

            else
                [ Css.backgroundColor Css.transparent ]
           )


spanText : Ui.Styles.Styles msg -> String -> Html msg
spanText styles label =
    Html.span (styles.colorStyleGrayscaleMuted ++ [ css [ Tw.text_sm ] ])
        [ text label ]


clauseFilter : Clause -> Maybe TagFilter
clauseFilter clause =
    case clause.tags of
        [] ->
            Nothing

        tags ->
            let
                matched =
                    case clause.match of
                        AnyOf ->
                            FilterAny tags

                        AllOf ->
                            FilterAll tags
            in
            case clause.polarity of
                Include ->
                    Just matched

                Exclude ->
                    Just (FilterNot matched)


mapClause : Int -> (Clause -> Clause) -> Clause -> Clause
mapClause id change clause =
    if clause.id == id then
        change clause

    else
        clause


toggleTag : String -> Bool -> Clause -> Clause
toggleTag tag checked clause =
    if checked then
        if List.member tag clause.tags then
            clause

        else
            { clause | tags = clause.tags ++ [ tag ] }

    else
        { clause | tags = List.filter (\selected -> selected /= tag) clause.tags }
