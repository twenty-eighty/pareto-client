module Components.ArticleComments exposing (ArticleComments, Model, Msg, init, new, subscriptions, update, view, withNewComment, withZapRelayUrls)

import BrowserEnv exposing (BrowserEnv)
import Components.Comment as Comment
import Components.InteractionButton as InteractionButton exposing (eventIdOfInteractionObject)
import Components.Interactions as Interactions
import Css
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import I18Next
import Locale exposing (Language(..))
import Nostr
import Nostr.Event exposing (Kind(..), numberForKind)
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (ArticleComment, ArticleCommentComment, CommentType(..))
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, LoginStatus, PubKey, RelayUrl)
import Set exposing (Set)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Time
import Ui.Links
import Ui.Profile
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, darkMode, stylesForTheme)


type ArticleComments msg
    = Settings
        { articleComments : List ArticleComment
        , articleCommentComments : Dict EventId (List ArticleCommentComment) -- event ID is the one of the parent comment
        , browserEnv : BrowserEnv
        , loginStatus : LoginStatus
        , model : Model
        , newComment : Maybe CommentType
        , nostr : Nostr.Model
        , toMsg : Msg msg -> msg
        , theme : Theme
        , zapRelayUrls : Set RelayUrl
        }


new :
    { browserEnv : BrowserEnv
    , articleComments : List ArticleComment
    , articleCommentComments : Dict EventId (List ArticleCommentComment) -- event ID is the one of the parent comment
    , loginStatus : LoginStatus
    , model : Model
    , nostr : Nostr.Model
    , theme : Theme
    , toMsg : Msg msg -> msg
    }
    -> ArticleComments msg
new props =
    Settings
        { browserEnv = props.browserEnv
        , articleComments = props.articleComments
        , articleCommentComments = props.articleCommentComments
        , loginStatus = props.loginStatus
        , model = props.model
        , newComment = Nothing
        , nostr = props.nostr
        , theme = props.theme
        , toMsg = props.toMsg
        , zapRelayUrls = Set.empty
        }

withNewComment : Maybe CommentType -> ArticleComments msg -> ArticleComments msg
withNewComment newComment (Settings settings) =
    Settings { settings | newComment = newComment } 

withZapRelayUrls : Set RelayUrl -> ArticleComments msg -> ArticleComments msg
withZapRelayUrls zapRelayUrls (Settings settings) =
    Settings { settings | zapRelayUrls = zapRelayUrls }


type Model =
    Model
        { comment : Comment.Model
        , interactions : Dict EventId Interactions.Model
        }


init : Model
init =
    Model
        { comment = Comment.init {}
        , interactions = Dict.empty
        }

type Msg msg
    = CommentSent Comment.Msg
    | InteractionsMsg InteractionButton.InteractionObject (Interactions.Msg (Msg msg))

update :
    { browserEnv : BrowserEnv
    , msg : Msg msg
    , model : Model
    , nostr : Nostr.Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , translations : I18Next.Translations
    } -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            CommentSent innerMsg ->
                Comment.update
                    { nostr = props.nostr
                    , msg = innerMsg
                    , model = model.comment
                    , toModel = \comment -> Model { model | comment = comment }
                    , toMsg = CommentSent >> props.toMsg
                    }


            InteractionsMsg interactionObject innerMsg ->
                let
                    eventId =
                        eventIdOfInteractionObject interactionObject
                    
                    ( updatedInteractions, effect ) =
                        Interactions.update
                            { browserEnv = props.browserEnv
                            , msg = innerMsg
                            , model = Dict.get eventId model.interactions
                            , nostr = props.nostr
                            , interactionObject = interactionObject
                            , openCommentMsg = Nothing
                            , toModel = \interactionsModel -> Model { model | interactions = Dict.insert eventId interactionsModel model.interactions }
                            , toMsg = InteractionsMsg interactionObject
                            }
                in
                ( updatedInteractions, effect |> Effect.map props.toMsg )

-- VIEW


view : ArticleComments msg -> Html msg
view articleComments =
    let
        (Settings settings) =
            articleComments
    in
    div
        [ css
            [ Tw.self_stretch
            , Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.flex
            , Tw.mb_4
            ]
        ]
        [ div
            [ css
                [ Tw.flex_col
                , Tw.justify_end
                , Tw.items_end
                , Tw.gap_4
                , Tw.flex
                ]
            ]
            [ viewCommenting (Settings settings)
            , div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_6
                    ]
                ]
                (settings.articleComments
                    |> sortComments
                    |> List.map (viewArticleComment articleComments settings.articleCommentComments)
                )
            ]
        ]

viewCommenting : ArticleComments msg -> Html msg
viewCommenting articleComments =
    let
        (Settings settings) =
            articleComments

        (Model model) =
            settings.model
    in
    div
        [ css
            [ Tw.w_80
            , Tw.mb_4
            , Bp.sm
                [ Tw.w_96
                ]
            ]
        ]
        [ Comment.new
            { browserEnv = settings.browserEnv
            , loginStatus = settings.loginStatus
            , model = model.comment
            , newComment = settings.newComment
            , nostr = settings.nostr
            , theme = settings.theme
            , toMsg = CommentSent
            }
            |> Comment.view
        ]
        |> Html.map settings.toMsg

sortComments : List { a | createdAt : Time.Posix } -> List { a | createdAt : Time.Posix }
sortComments comments =
    List.sortBy (\comment -> -1 * Time.posixToMillis comment.createdAt) comments


viewArticleComment : ArticleComments msg -> Dict EventId (List ArticleCommentComment) -> ArticleComment -> Html msg
viewArticleComment articleComments articleCommentComments articleComment =
    let
        (Settings settings) =
            articleComments

        styles =
            stylesForTheme settings.theme

        commentsOfComment =
            articleCommentComments
                |> Dict.get articleComment.eventId
                |> Maybe.withDefault []
    in
    div
        [ css
            [ Tw.relative
            , Tw.mt_6
            , Tw.pl_4
            , Tw.w_auto
            , Tw.max_w_sm
            , Bp.sm
                [ Tw.max_w_lg
                ]
            , Bp.md
                [ Tw.max_w_2xl
                ]
            ]
        ]
        [ viewCommentHeader settings.browserEnv styles settings.nostr articleComment.pubKey articleComment.createdAt
            |> Html.map settings.toMsg
        , viewCommentContent styles articleComment.content
            |> Html.map settings.toMsg
        , viewInteractions articleComments articleComment.eventId articleComment.pubKey articleComment.kind Nothing (Just articleComment.content)
            |> Html.map settings.toMsg
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.justify_start
                , Tw.items_start
                , Tw.overflow_visible
                , Tw.relative
                ]
            ]
            (commentsOfComment
                |> sortComments
                |> List.map (viewArticleCommentComment articleComments 1 articleCommentComments)
            )
        ]

viewInteractions : ArticleComments msg -> EventId -> PubKey -> Kind -> Maybe String -> Maybe String -> Html (Msg msg)
viewInteractions articleComments eventId pubKey kind title description =
    let
        (Settings settings) =
            articleComments

        (Model model) =
            settings.model

        interactionObject =
            InteractionButton.Comment eventId pubKey

        shareButtonElement =
            Nip19.NEvent { id = eventId, author = Just pubKey, kind = Just (kind |> numberForKind), relays = settings.zapRelayUrls |> Set.toList }
            |> Ui.Links.linkToNJump
            |> Maybe.map (\url ->
                [ Interactions.ShareButtonElement
                    { url = url
                    , title = title |> Maybe.withDefault ""
                    , text = description |> Maybe.withDefault ""
                    , hashtags = []
                    }
                ]
            )
            |> Maybe.withDefault []
    in
    div [ css [ Tw.flex, Tw.justify_center, Tw.w_full ] ]
        [ Interactions.new
            { browserEnv = settings.browserEnv
            , model = Dict.get eventId model.interactions
            , toMsg = InteractionsMsg interactionObject
            , theme = settings.theme
            , interactionObject = interactionObject
            , nostr = settings.nostr
            , loginStatus = settings.loginStatus
            }
            |> Interactions.withInteractionElements
                ([ Interactions.LikeButtonElement
                , Interactions.RepostButtonElement
                , Interactions.ZapButtonElement "0" settings.zapRelayUrls
                ] ++ shareButtonElement)
            |> Interactions.view
        ]


viewInteractionsAndReplies :
    ArticleComments msg
    -> Int
    -> ArticleCommentComment
    -> Dict EventId (List ArticleCommentComment)
    -> Html msg          -- NOTE: return Html (Msg msg)
viewInteractionsAndReplies articleComments level parent dict =
    let
        (Settings settings) =
            articleComments

        styles =
            stylesForTheme settings.theme

        interactions : Html msg
        interactions =
            viewInteractions articleComments parent.eventId parent.pubKey parent.kind Nothing (Just parent.content)
            |> Html.map settings.toMsg

        replies : List (Html msg)
        replies =
            dict
                |> Dict.get parent.eventId
                |> Maybe.withDefault []
                |> List.map (viewArticleCommentComment articleComments (level + 1) dict)
    in
    replyGroupWrapper
        styles
        (level * indentPerLevel)
        elbowHeight
        interactions
        replies



viewCommentHeader : BrowserEnv -> Styles msg -> Nostr.Model -> PubKey -> Time.Posix -> Html msg
viewCommentHeader browserEnv styles nostr pubKey createdAt =
    let
        followLinks =
            Nostr.isAuthor nostr pubKey

        profileDisplay =
            Nostr.getProfile nostr pubKey
                |> Maybe.map
                    (\profile ->
                        Nostr.getProfileValidationStatus nostr profile.pubKey
                            |> Maybe.withDefault ValidationUnknown
                            |> Ui.Profile.viewProfileSmall browserEnv.environment styles followLinks profile
                    )
                |> Maybe.withDefault emptyHtml
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.gap_2
            , Tw.mb_2
            ]
        ]
        [ profileDisplay
        , div
            [ css
                []
            ]
            [ Html.text <| BrowserEnv.formatDate browserEnv createdAt
            ]
        ]

viewCommentContent : Styles msg -> String -> Html msg
viewCommentContent styles content =
    div
        (styles.textStyleBody
            ++ styles.colorStyleGrayscaleMuted
            ++ [ css
                    [ Tw.h_auto
                    , Tw.p_4
                    , Tw.left_0
                    , Tw.top_0
                    , Tw.text_color styles.color4
                    , Tw.bg_color styles.color1
                    , Tw.rounded_xl
                    , Tw.break_words
                    ]
                ]
        )
        (content
            |> String.split "\n"
            |> List.map Html.text
            |> List.intersperse (Html.br [] [])
        )


replyGroupWrapper :
    Styles msg
    -> Int               -- indentPx
    -> Float             -- elbowHeight   ( = bendY – 10 )
    -> Html msg    -- interaction-row
    -> List (Html msg)
    -> Html msg
replyGroupWrapper styles indentPx elbowH interactionRow replyNodes =
    let
        hasReplies = not (List.isEmpty replyNodes)
    in
    div
        [ css
            ([ Tw.relative
            , indentTailwind indentPx
            , Tw.overflow_visible
            , Css.marginTop  (Css.px (-elbowH))
            , Css.paddingTop (Css.px   elbowH)
            ] ++
            (if hasReplies then
                [ Css.before
                    [ Tw.absolute
                    , Css.left (Css.px (toFloat threadLineX))
                    , Css.top (Css.px (toFloat (-threadLineGapOffset)))
                    , Css.bottom (Css.px 0)
                    , Tw.w_px
                    , Tw.rounded_full
                    , Tw.bg_color styles.color2
                    ]
                ]
            else
                []
            ))
        ]
        (interactionRow :: replyNodes)


viewArticleCommentComment : ArticleComments msg -> Int -> Dict EventId (List ArticleCommentComment) -> ArticleCommentComment -> Html msg
viewArticleCommentComment articleComments level articleCommentComments articleCommentComment =
    let
        (Settings settings) =
            articleComments

        styles =
            stylesForTheme settings.theme

        indentPx =
            level * indentPerLevel
    in
    div
        [ css
            [ Tw.relative
            , Tw.w_auto
            , Tw.max_w_sm
            , Bp.sm
                [ Tw.max_w_lg
                ]
            , Bp.md
                [ Tw.max_w_2xl
                ]
            ]
        ]
        [ svgElbowConnector styles indentPerLevel bendY
        , div
            [ css [ indentTailwind indentPx, Tw.pl_4 ] ]
            [ viewCommentHeader settings.browserEnv styles settings.nostr articleCommentComment.pubKey articleCommentComment.createdAt
            , viewCommentContent styles articleCommentComment.content
            , viewInteractionsAndReplies
                articleComments
                level
                articleCommentComment
                articleCommentComments
            ]
        ]



indentPerLevel : Int
indentPerLevel =
    32

interactionH : Int
interactionH =
    30


baseBend : Int
baseBend =
    54     -- header+bubble top before the icons row


bendY : Int
bendY =
    baseBend + interactionH

elbowHeight : Float
elbowHeight =
    toFloat (bendY - 10)

-- Threading line positioning constants
threadLineX : Int
threadLineX =
    14

threadLineGapOffset : Int
threadLineGapOffset =
    100

threadLineCurveRadius : Int
threadLineCurveRadius =
    10

svgElbowConnector : Styles msg -> Int -> Int -> Html msg
svgElbowConnector styles indentPx bndY =
    let
        startX = threadLineX
        horizontalStart = startX + threadLineCurveRadius
        verticalStart = 0
        svgHeight = bndY + threadLineGapOffset + 10
        -- Instead of extending upward, connect the horizontal line higher up in the comment
        horizontalConnectionY = 70  -- How far down from top of comment to connect horizontally
    in
    div
        [ css [ Tw.absolute, Tw.left_0, Css.top (Css.px (toFloat (-threadLineGapOffset))), Tw.text_color styles.color1, darkMode [ Tw.text_color styles.color1DarkMode ] ] ]
        [ Svg.svg
            [ SvgAttr.width (String.fromInt indentPx)
            , SvgAttr.height (String.fromInt svgHeight)
            , SvgAttr.viewBox ("0 0 " ++ String.fromInt indentPx ++ " " ++ String.fromInt svgHeight)
            ]
            [ Svg.path
                [ SvgAttr.d
                    ("M" ++ String.fromInt startX ++ "," ++ String.fromInt verticalStart ++
                    "V" ++ String.fromInt (threadLineGapOffset + horizontalConnectionY - 10) ++
                    " Q" ++ String.fromInt startX ++ "," ++ String.fromInt (threadLineGapOffset + horizontalConnectionY) ++
                    " " ++ String.fromInt horizontalStart ++ "," ++ String.fromInt (threadLineGapOffset + horizontalConnectionY) ++
                    " H" ++ String.fromInt indentPx)
                , SvgAttr.stroke "currentColor"
                , SvgAttr.fill "none"
                , SvgAttr.strokeWidth "1"
                ]
                []
            ]
        ]


indentTailwind : Int -> Css.Style
indentTailwind indentPx =
    case indentPx // indentPerLevel of
        0 ->
            Tw.ml_0

        1 ->
            Tw.ml_4

        2 ->
            Tw.ml_8

        3 ->
            Tw.ml_12

        4 ->
            Tw.ml_4

        5 ->
            Tw.ml_20

        6 ->
            Tw.ml_6

        _ ->
            Tw.ml_auto


subscriptions : Model -> List ArticleComment -> Sub (Msg msg)
subscriptions (Model model) articleComments =
    Sub.batch
        [ Sub.map CommentSent (Comment.subscriptions model.comment)
        , interactionSubscriptions (Model model) articleComments
        ]


-- only some comments had an interaction/model, we only subscribe to the ones that have one
interactionSubscriptions : Model -> List ArticleComment -> Sub (Msg msg)
interactionSubscriptions (Model model) articleComments =
    articleComments
    |> List.map (\articleComment ->
        Dict.get articleComment.eventId model.interactions
        |> Maybe.map (\interactions ->
            Interactions.subscriptions interactions
            |> Sub.map (InteractionsMsg (InteractionButton.Comment articleComment.eventId articleComment.pubKey))
        )
        |> Maybe.withDefault Sub.none
    )
    |> Sub.batch

