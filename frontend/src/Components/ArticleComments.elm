module Components.ArticleComments exposing (ArticleComments, Model, Msg, init, new, subscriptions, update, view, withNewComment)

import BrowserEnv exposing (BrowserEnv)
import Components.Comment as Comment
import Components.InteractionButton as InteractionButton exposing (eventIdOfInteractionObject)
import Components.Interactions as Interactions
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import I18Next
import Locale exposing (Language(..))
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.Nip22 exposing (ArticleComment, ArticleCommentComment, CommentType(..))
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, LoginStatus)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Time
import Ui.Profile
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


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
        }

withNewComment : Maybe CommentType -> ArticleComments msg -> ArticleComments msg
withNewComment newComment (Settings settings) =
    Settings { settings | newComment = newComment } 

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

        (Model model) =
            settings.model
    in
    div
        [ css
            [ Tw.self_stretch
            , Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.flex
            ]
        ]
        [ {- div
                 [ css
                     [ Tw.justify_start
                     , Tw.items_center
                     , Tw.gap_3
                     , Tw.inline_flex
                     ]
                 ]
                 [ div
                     (styles.textStyleH2 ++ styles.colorStyleGrayscaleTitle)
                     [ Html.text "Comments" ]
                 ]
             ,
          -}
          div
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
                    |> List.map (viewArticleComment articleComments 0 settings.articleCommentComments)
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

sortComments =
    List.sortBy (\comment -> -1 * Time.posixToMillis comment.createdAt)


viewArticleComment : ArticleComments msg -> Int -> Dict EventId (List ArticleCommentComment) -> ArticleComment -> Html msg
viewArticleComment articleComments level articleCommentComments articleComment =
    let
        (Settings settings) =
            articleComments

        (Model model) =
            settings.model

        styles =
            stylesForTheme settings.theme

        commentsOfComment =
            articleCommentComments
                |> Dict.get articleComment.eventId
                |> Maybe.withDefault []

        followLinks =
            Nostr.isAuthor settings.nostr articleComment.pubKey

        profileDisplay =
            Nostr.getProfile settings.nostr articleComment.pubKey
                |> Maybe.map
                    (\profile ->
                        Nostr.getProfileValidationStatus settings.nostr profile.pubKey
                            |> Maybe.withDefault ValidationUnknown
                            |> Ui.Profile.viewProfileSmall styles followLinks profile
                    )
                |> Maybe.withDefault emptyHtml

        interactionObject =
            InteractionButton.Comment articleComment.eventId articleComment.pubKey

        viewInteractions =
            Interactions.new
                { browserEnv = settings.browserEnv
                , model = Dict.get articleComment.eventId model.interactions
                , toMsg = InteractionsMsg interactionObject
                , theme = settings.theme
                , interactionObject = interactionObject
                , nostr = settings.nostr
                , loginStatus = settings.loginStatus
                }
                |> Interactions.view
    in
    div
        [ css
            [ Tw.w_96
            ]
        ]
        [ div
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
                [ Html.text <| BrowserEnv.formatDate settings.browserEnv articleComment.createdAt
                ]
            ]
        , div
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
                        ]
                   ]
            )
            (articleComment.content
                |> String.split "\n"
                |> List.map Html.text
                |> List.intersperse (Html.br [] [])
            )
        , viewInteractions
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.justify_start
                , Tw.items_start
                , Tw.gap_4
                ]
            ]
            (commentsOfComment
                |> sortComments
                |> List.map (viewArticleCommentComment styles settings.browserEnv 0 articleCommentComments)
            )
        ]
        |> Html.map settings.toMsg


viewArticleCommentComment : Styles msg -> BrowserEnv -> Int -> Dict EventId (List ArticleCommentComment) -> ArticleCommentComment -> Html msg
viewArticleCommentComment styles browserEnv level articleCommentComments articleCommentComment =
    let
        commentsOfComment =
            articleCommentComments
                |> Dict.get articleCommentComment.eventId
                |> Maybe.withDefault []
    in
    div
        [ css
            [ Tw.w_96
            , Tw.h_28
            ]
        ]
        [ div
            []
            [ Html.text <| BrowserEnv.formatDate browserEnv articleCommentComment.createdAt
            ]
        , div
            (styles.textStyleBody
                ++ styles.colorStyleGrayscaleMuted
                ++ [ css
                        [ Tw.w_96
                        , Tw.h_28
                        , Tw.p_2
                        , Tw.left_0
                        , Tw.top_0
                        , Tw.bg_color styles.color1
                        , Tw.rounded_xl
                        ]
                   ]
            )
            [ Html.text articleCommentComment.content
            , div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    ]
                ]
                (commentsOfComment
                    |> List.map (viewArticleCommentComment styles browserEnv (level + 1) articleCommentComments)
                )
            ]
        ]


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

