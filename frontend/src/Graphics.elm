module Graphics exposing (..)

import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Svg.Styled as Svg exposing (svg, path)
import Svg.Styled.Attributes as SvgAttr

readIcon : Html msg
readIcon =
    svg
        [ SvgAttr.width "22"
        , SvgAttr.height "20"
        , SvgAttr.viewBox "0 0 22 20"
        , SvgAttr.fill "none"
        , Attr.attribute "aria-hidden" "true"
        , Attr.attribute "focusable" "false"
        ]
        [ path
            [ SvgAttr.d "M11 19L10.8999 18.8499C10.2053 17.808 9.85798 17.287 9.3991 16.9098C8.99286 16.5759 8.52476 16.3254 8.02161 16.1726C7.45325 16 6.82711 16 5.57482 16H4.2C3.07989 16 2.51984 16 2.09202 15.782C1.71569 15.5903 1.40973 15.2843 1.21799 14.908C1 14.4802 1 13.9201 1 12.8V4.2C1 3.07989 1 2.51984 1.21799 2.09202C1.40973 1.71569 1.71569 1.40973 2.09202 1.21799C2.51984 1 3.07989 1 4.2 1H4.6C6.84021 1 7.96031 1 8.81596 1.43597C9.56861 1.81947 10.1805 2.43139 10.564 3.18404C11 4.03968 11 5.15979 11 7.4M11 19V7.4M11 19L11.1001 18.8499C11.7947 17.808 12.142 17.287 12.6009 16.9098C13.0071 16.5759 13.4752 16.3254 13.9784 16.1726C14.5467 16 15.1729 16 16.4252 16H17.8C18.9201 16 19.4802 16 19.908 15.782C20.2843 15.5903 20.5903 15.2843 20.782 14.908C21 14.4802 21 13.9201 21 12.8V4.2C21 3.07989 21 2.51984 20.782 2.09202C20.5903 1.71569 20.2843 1.40973 19.908 1.21799C19.4802 1 18.9201 1 17.8 1H17.4C15.1598 1 14.0397 1 13.184 1.43597C12.4314 1.81947 11.8195 2.43139 11.436 3.18404C11 4.03968 11 5.15979 11 7.4"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
    
searchIcon : Html msg
searchIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , Attr.attribute "aria-hidden" "true"
        , Attr.attribute "focusable" "false"
        ]
        [ path
            [ SvgAttr.d "M21 21L15.0001 15M17 10C17 13.866 13.866 17 10 17C6.13401 17 3 13.866 3 10C3 6.13401 6.13401 3 10 3C13.866 3 17 6.13401 17 10Z"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
    
worldIcon : Html msg
worldIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , Attr.attribute "aria-hidden" "true"
        , Attr.attribute "focusable" "false"
        ]
        [ path
            [ SvgAttr.d "M12 2C14.5013 4.73835 15.9228 8.29203 16 12C15.9228 15.708 14.5013 19.2616 12 22M12 2C9.49872 4.73835 8.07725 8.29203 8 12C8.07725 15.708 9.49872 19.2616 12 22M12 2C6.47715 2 2 6.47715 2 12C2 17.5228 6.47715 22 12 22M12 2C17.5228 2 22 6.47715 22 12C22 17.5228 17.5228 22 12 22M2.50002 9H21.5M2.5 15H21.5"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , Attr.attribute "strokeinejoin" "round"
            ]
            []
        ]
    
bookmarkIcon : Html msg
bookmarkIcon =
    svg
        [ SvgAttr.width "16"
        , SvgAttr.height "20"
        , SvgAttr.viewBox "0 0 16 20"
        , SvgAttr.fill "none"
        , Attr.attribute "aria-hidden" "true"
        , Attr.attribute "focusable" "false"
        ]
        [ path
            [ SvgAttr.d "M1 5.8C1 4.11984 1 3.27976 1.32698 2.63803C1.6146 2.07354 2.07354 1.6146 2.63803 1.32698C3.27976 1 4.11984 1 5.8 1H10.2C11.8802 1 12.7202 1 13.362 1.32698C13.9265 1.6146 14.3854 2.07354 14.673 2.63803C15 3.27976 15 4.11984 15 5.8V19L8 15L1 19V5.8Z"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
 
writeIcon : Html msg
writeIcon =
    svg
        [ SvgAttr.width "22"
        , SvgAttr.height "22"
        , SvgAttr.viewBox "0 0 22 22"
        , SvgAttr.fill "none"
        , Attr.attribute "aria-hidden" "true"
        , Attr.attribute "focusable" "false"
        ]
        [ path
            [ SvgAttr.d "M15 7.00001L1 21M17 14H8M5.6 18H12.3373C12.5818 18 12.7041 18 12.8192 17.9724C12.9213 17.9479 13.0188 17.9075 13.1083 17.8526C13.2092 17.7908 13.2957 17.7043 13.4686 17.5314L18.5 12.5C18.739 12.261 18.8584 12.1416 18.9546 12.0358C21.0348 9.74733 21.0348 6.25269 18.9546 3.96424C18.8584 3.85845 18.739 3.73897 18.5 3.50001C18.261 3.26105 18.1416 3.14157 18.0358 3.04541C15.7473 0.965251 12.2527 0.965251 9.96423 3.04541C9.85844 3.14157 9.73896 3.26105 9.5 3.50001L4.46863 8.53138C4.29568 8.70433 4.2092 8.79081 4.14736 8.89172C4.09253 8.9812 4.05213 9.07874 4.02763 9.18078C4 9.29586 4 9.41816 4 9.66275V16.4C4 16.9601 4 17.2401 4.10899 17.454C4.20487 17.6422 4.35785 17.7951 4.54601 17.891C4.75992 18 5.03995 18 5.6 18Z"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]

chakraIcon : Html msg
chakraIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.fill "currentColor"
            , SvgAttr.d "M16.59 8.59L12 13.17 7.41 8.59 6 10l6 6 6-6z"
            ]
            []
        ]

atIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.fill "currentColor"
            , SvgAttr.d "M12,.5A11.634,11.634,0,0,0,.262,12,11.634,11.634,0,0,0,12,23.5a11.836,11.836,0,0,0,6.624-2,1.25,1.25,0,1,0-1.393-2.076A9.34,9.34,0,0,1,12,21a9.132,9.132,0,0,1-9.238-9A9.132,9.132,0,0,1,12,3a9.132,9.132,0,0,1,9.238,9v.891a1.943,1.943,0,0,1-3.884,0V12A5.355,5.355,0,1,0,12,17.261a5.376,5.376,0,0,0,3.861-1.634,4.438,4.438,0,0,0,7.877-2.736V12A11.634,11.634,0,0,0,12,.5Zm0,14.261A2.763,2.763,0,1,1,14.854,12,2.812,2.812,0,0,1,12,14.761Z"
            ]
            []
        ]
 
aboutIcon =
     svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        , Attr.attribute "aria-hidden" "true"
        , Attr.attribute "focusable" "false"
        ]
        [ Svg.circle
            [ SvgAttr.cx "12"
            , SvgAttr.cy "12"
            , SvgAttr.r "10"
            ]
            []
        , path
            [ SvgAttr.d "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "12"
            , SvgAttr.y1 "17"
            , SvgAttr.x2 "12.01"
            , SvgAttr.y2 "17"
            ]
            []
        ]

smallBookmarkIcon =
    svg
        [ SvgAttr.width "16"
        , SvgAttr.height "20"
        , SvgAttr.viewBox "0 0 16 20"
        , SvgAttr.fill "none"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "variant" "unstyled"
        ]
        [ path
            [ SvgAttr.d "M1 5.8C1 4.11984 1 3.27976 1.32698 2.63803C1.6146 2.07354 2.07354 1.6146 2.63803 1.32698C3.27976 1 4.11984 1 5.8 1H10.2C11.8802 1 12.7202 1 13.362 1.32698C13.9265 1.6146 14.3854 2.07354 14.673 2.63803C15 3.27976 15 4.11984 15 5.8V19L8 15L1 19V5.8Z"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
    

commentsIcon =
    svg
        [ SvgAttr.width "18"
        , SvgAttr.height "18"
        , SvgAttr.viewBox "0 0 18 18"
        , SvgAttr.fill "none"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "variant" "unstyled"
        ]
        [ path
            [ SvgAttr.d "M1.5 5.125C1.5 3.72487 1.5 3.0248 1.77248 2.49002C2.01217 2.01962 2.39462 1.63717 2.86502 1.39748C3.3998 1.125 4.09987 1.125 5.5 1.125H12.5C13.9001 1.125 14.6002 1.125 15.135 1.39748C15.6054 1.63717 15.9878 2.01962 16.2275 2.49002C16.5 3.0248 16.5 3.72487 16.5 5.125V9.625C16.5 11.0251 16.5 11.7252 16.2275 12.26C15.9878 12.7304 15.6054 13.1128 15.135 13.3525C14.6002 13.625 13.9001 13.625 12.5 13.625H10.4031C9.88308 13.625 9.62306 13.625 9.37435 13.676C9.15369 13.7213 8.94017 13.7962 8.73957 13.8987C8.51347 14.0142 8.31043 14.1767 7.90434 14.5015L5.91646 16.0918C5.56973 16.3692 5.39636 16.5079 5.25045 16.5081C5.12356 16.5082 5.00352 16.4505 4.92436 16.3513C4.83333 16.2373 4.83333 16.0153 4.83333 15.5713V13.625C4.05836 13.625 3.67087 13.625 3.35295 13.5398C2.49022 13.3086 1.81635 12.6348 1.58519 11.772C1.5 11.4541 1.5 11.0666 1.5 10.2917V5.125Z"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "1.66667"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
    

likeIcon =
    svg
        [ SvgAttr.width "20"
        , SvgAttr.height "20"
        , SvgAttr.viewBox "0 0 24 24"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "variant" "unstyled"
        ]
        [ Svg.g
            [ SvgAttr.fill "none"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeWidth "2"
            ]
            [ path
                [ SvgAttr.d "M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"
                ]
                []
            , path
                [ SvgAttr.d "M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z"
                ]
                []
            ]
        ]
    
    

repostIcon =
    svg
        [ SvgAttr.width "20"
        , SvgAttr.height "19"
        , SvgAttr.viewBox "0 0 20 19"
        , SvgAttr.fill "none"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "variant" "unstyled"
        ]
        [ path
            [ SvgAttr.d "M10.8327 17.9584L8.33268 15.4584M8.33268 15.4584L10.8327 12.9584M8.33268 15.4584H12.4993C15.721 15.4584 18.3327 12.8467 18.3327 9.62502C18.3327 7.29802 16.9701 5.28927 14.9993 4.35305M4.99935 14.897C3.02856 13.9608 1.66602 11.952 1.66602 9.62502C1.66602 6.40336 4.27769 3.79169 7.49935 3.79169H11.666M11.666 3.79169L9.16602 1.29169M11.666 3.79169L9.16602 6.29169"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "1.66667"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
    

zapIcon =
    svg
        [ SvgAttr.width "16"
        , SvgAttr.height "19"
        , SvgAttr.viewBox "0 0 16 19"
        , SvgAttr.fill "none"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "variant" "unstyled"
        ]
        [ path
            [ SvgAttr.d "M8.83354 1.29169L1.41142 10.1982C1.12075 10.547 0.975413 10.7214 0.973192 10.8687C0.971261 10.9968 1.02832 11.1186 1.12792 11.1991C1.2425 11.2917 1.46952 11.2917 1.92357 11.2917H8.00021L7.16688 17.9584L14.589 9.05181C14.8797 8.703 15.025 8.5286 15.0272 8.3813C15.0292 8.25326 14.9721 8.13143 14.8725 8.05094C14.7579 7.95835 14.5309 7.95835 14.0768 7.95835H8.00021L8.83354 1.29169Z"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "1.66667"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
    

settingsIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.d "M9.39516 19.3711L9.97961 20.6856C10.1533 21.0768 10.4369 21.4093 10.7958 21.6426C11.1548 21.8759 11.5737 22.0001 12.0018 22C12.4299 22.0001 12.8489 21.8759 13.2078 21.6426C13.5668 21.4093 13.8503 21.0768 14.0241 20.6856L14.6085 19.3711C14.8165 18.9047 15.1665 18.5159 15.6085 18.26C16.0533 18.0034 16.5678 17.8941 17.0785 17.9478L18.5085 18.1C18.9342 18.145 19.3638 18.0656 19.7452 17.8713C20.1266 17.6771 20.4435 17.3763 20.6574 17.0056C20.8716 16.635 20.9736 16.2103 20.9511 15.7829C20.9286 15.3555 20.7826 14.9438 20.5307 14.5978L19.6841 13.4344C19.3826 13.0171 19.2215 12.5148 19.2241 12C19.224 11.4866 19.3866 10.9864 19.6885 10.5711L20.5352 9.40778C20.787 9.06175 20.9331 8.65007 20.9556 8.22267C20.978 7.79528 20.876 7.37054 20.6618 7C20.4479 6.62923 20.1311 6.32849 19.7496 6.13423C19.3682 5.93997 18.9386 5.86053 18.5129 5.90556L17.0829 6.05778C16.5723 6.11141 16.0577 6.00212 15.6129 5.74556C15.1701 5.48825 14.82 5.09736 14.6129 4.62889L14.0241 3.31444C13.8503 2.92317 13.5668 2.59072 13.2078 2.3574C12.8489 2.12408 12.4299 1.99993 12.0018 2C11.5737 1.99993 11.1548 2.12408 10.7958 2.3574C10.4369 2.59072 10.1533 2.92317 9.97961 3.31444L9.39516 4.62889C9.18809 5.09736 8.83804 5.48825 8.39516 5.74556C7.95038 6.00212 7.43583 6.11141 6.92516 6.05778L5.49072 5.90556C5.06505 5.86053 4.63546 5.93997 4.25403 6.13423C3.87261 6.32849 3.55574 6.62923 3.34183 7C3.12765 7.37054 3.02561 7.79528 3.0481 8.22267C3.07058 8.65007 3.21662 9.06175 3.4685 9.40778L4.31516 10.5711C4.61711 10.9864 4.7797 11.4866 4.77961 12C4.7797 12.5134 4.61711 13.0137 4.31516 13.4289L3.4685 14.5922C3.21662 14.9382 3.07058 15.3499 3.0481 15.7773C3.02561 16.2047 3.12765 16.6295 3.34183 17C3.55595 17.3706 3.87286 17.6712 4.25423 17.8654C4.6356 18.0596 5.06508 18.1392 5.49072 18.0944L6.92072 17.9422C7.43139 17.8886 7.94594 17.9979 8.39072 18.2544C8.83525 18.511 9.18693 18.902 9.39516 19.3711Z"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        , path
            [ SvgAttr.d "M12 15C13.6569 15 15 13.6569 15 12C15 10.3431 13.6569 9 12 9C10.3432 9 9.00004 10.3431 9.00004 12C9.00004 13.6569 10.3432 15 12 15Z"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
    
relaysIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.d "M22 10.5L21.5256 6.70463C21.3395 5.21602 21.2465 4.47169 20.8961 3.9108C20.5875 3.41662 20.1416 3.02301 19.613 2.77804C19.013 2.5 18.2629 2.5 16.7626 2.5H7.23735C5.73714 2.5 4.98704 2.5 4.38702 2.77804C3.85838 3.02301 3.4125 3.41662 3.10386 3.9108C2.75354 4.47169 2.6605 5.21601 2.47442 6.70463L2 10.5M5.5 14.5H18.5M5.5 14.5C3.567 14.5 2 12.933 2 11C2 9.067 3.567 7.5 5.5 7.5H18.5C20.433 7.5 22 9.067 22 11C22 12.933 20.433 14.5 18.5 14.5M5.5 14.5C3.567 14.5 2 16.067 2 18C2 19.933 3.567 21.5 5.5 21.5H18.5C20.433 21.5 22 19.933 22 18C22 16.067 20.433 14.5 18.5 14.5M6 11H6.01M6 18H6.01M12 11H18M12 18H18"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            ]
            []
        ]
    
moderationIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ Svg.g
            [ SvgAttr.fill "currentColor"
            ]
            [ path
                [ SvgAttr.d "M23.2,10.549a20.954,20.954,0,0,0-4.3-3.6l4-3.995a1,1,0,1,0-1.414-1.414l-.018.018a.737.737,0,0,1-.173.291l-19.5,19.5c-.008.007-.018.009-.026.017a1,1,0,0,0,1.631,1.088l4.146-4.146a11.26,11.26,0,0,0,4.31.939h.3c4.256,0,8.489-2.984,11.051-5.8A2.171,2.171,0,0,0,23.2,10.549ZM16.313,13.27a4.581,4.581,0,0,1-3,3.028,4.3,4.3,0,0,1-3.1-.19.253.253,0,0,1-.068-.407l5.56-5.559a.252.252,0,0,1,.407.067A4.3,4.3,0,0,1,16.313,13.27Z"
                ]
                []
            , path
                [ SvgAttr.d "M7.615,13.4a.244.244,0,0,0,.061-.24A4.315,4.315,0,0,1,7.5,12,4.5,4.5,0,0,1,12,7.5a4.276,4.276,0,0,1,1.16.173.244.244,0,0,0,.24-.062l1.941-1.942a.254.254,0,0,0-.1-.421A10.413,10.413,0,0,0,12,4.75C7.7,4.692,3.4,7.7.813,10.549a2.15,2.15,0,0,0-.007,2.9,21.209,21.209,0,0,0,3.438,3.03.256.256,0,0,0,.326-.029Z"
                ]
                []
            ]
        ]
    
logoutIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.fill "currentColor"
            , SvgAttr.d "M11.983,0a12.206,12.206,0,0,0-8.51,3.653A11.8,11.8,0,0,0,0,12.207,11.779,11.779,0,0,0,11.8,24h.214A12.111,12.111,0,0,0,24,11.791h0A11.766,11.766,0,0,0,11.983,0ZM10.5,16.542a1.476,1.476,0,0,1,1.449-1.53h.027a1.527,1.527,0,0,1,1.523,1.47,1.475,1.475,0,0,1-1.449,1.53h-.027A1.529,1.529,0,0,1,10.5,16.542ZM11,12.5v-6a1,1,0,0,1,2,0v6a1,1,0,1,1-2,0Z"
            ]
            []
        ]
    
checkIcon =
    svg
        [ SvgAttr.viewBox "0 0 14 14"
        , Attr.attribute "focusable" "false"
        ]
        [ Svg.g
            [ SvgAttr.fill "currentColor"
            ]
            [ Svg.polygon
                [ SvgAttr.points "5.5 11.9993304 14 3.49933039 12.5 2 5.5 8.99933039 1.5 4.9968652 0 6.49933039"
                ]
                []
            ]
        ]

featherSmileIcon width =
    svg
        [ SvgAttr.width <| String.fromInt width
        , SvgAttr.height <| String.fromInt width
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ Svg.circle
            [ SvgAttr.cx "12"
            , SvgAttr.cy "12"
            , SvgAttr.r "10"
            ]
            []
        , path
            [ SvgAttr.d "M8 14s1.5 2 4 2 4-2 4-2"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "9"
            , SvgAttr.y1 "9"
            , SvgAttr.x2 "9.01"
            , SvgAttr.y2 "9"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "15"
            , SvgAttr.y1 "9"
            , SvgAttr.x2 "15.01"
            , SvgAttr.y2 "9"
            ]
            []
        ]
    
    
featherFrownIcon width =
    svg
        [ SvgAttr.width <| String.fromInt width
        , SvgAttr.height <| String.fromInt width
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ Svg.circle
            [ SvgAttr.cx "12"
            , SvgAttr.cy "12"
            , SvgAttr.r "10"
            ]
            []
        , path
            [ SvgAttr.d "M16 16s-1.5-2-4-2-4 2-4 2"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "9"
            , SvgAttr.y1 "9"
            , SvgAttr.x2 "9.01"
            , SvgAttr.y2 "9"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "15"
            , SvgAttr.y1 "9"
            , SvgAttr.x2 "15.01"
            , SvgAttr.y2 "9"
            ]
            []
        ]
    
    
featherMehIcon width =
    svg
        [ SvgAttr.width <| String.fromInt width
        , SvgAttr.height  <| String.fromInt width
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ Svg.circle
            [ SvgAttr.cx "12"
            , SvgAttr.cy "12"
            , SvgAttr.r "10"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "8"
            , SvgAttr.y1 "15"
            , SvgAttr.x2 "16"
            , SvgAttr.y2 "15"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "9"
            , SvgAttr.y1 "9"
            , SvgAttr.x2 "9.01"
            , SvgAttr.y2 "9"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "15"
            , SvgAttr.y1 "9"
            , SvgAttr.x2 "15.01"
            , SvgAttr.y2 "9"
            ]
            []
        ]
    
-- landing page

keyboardIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ path
            [ SvgAttr.stroke "none"
            , SvgAttr.d "M0 0h24v24H0z"
            , SvgAttr.fill "none"
            ]
            []
        , path
            [ SvgAttr.d "M2 6m0 2a2 2 0 0 1 2 -2h16a2 2 0 0 1 2 2v8a2 2 0 0 1 -2 2h-16a2 2 0 0 1 -2 -2z"
            ]
            []
        , path
            [ SvgAttr.d "M6 10l0 .01"
            ]
            []
        , path
            [ SvgAttr.d "M10 10l0 .01"
            ]
            []
        , path
            [ SvgAttr.d "M14 10l0 .01"
            ]
            []
        , path
            [ SvgAttr.d "M18 10l0 .01"
            ]
            []
        , path
            [ SvgAttr.d "M6 14l0 .01"
            ]
            []
        , path
            [ SvgAttr.d "M18 14l0 .01"
            ]
            []
        , path
            [ SvgAttr.d "M10 14l4 .01"
            ]
            []
        ]
    
listSearchIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ path
            [ SvgAttr.stroke "none"
            , SvgAttr.d "M0 0h24v24H0z"
            , SvgAttr.fill "none"
            ]
            []
        , path
            [ SvgAttr.d "M15 15m-4 0a4 4 0 1 0 8 0a4 4 0 1 0 -8 0"
            ]
            []
        , path
            [ SvgAttr.d "M18.5 18.5l2.5 2.5"
            ]
            []
        , path
            [ SvgAttr.d "M4 6h16"
            ]
            []
        , path
            [ SvgAttr.d "M4 12h4"
            ]
            []
        , path
            [ SvgAttr.d "M4 18h4"
            ]
            []
        ]
    
mailIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ path
            [ SvgAttr.stroke "none"
            , SvgAttr.d "M0 0h24v24H0z"
            , SvgAttr.fill "none"
            ]
            []
        , path
            [ SvgAttr.d "M3 7a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v10a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-10z"
            ]
            []
        , path
            [ SvgAttr.d "M3 7l9 6l9 -6"
            ]
            []
        ]
    
speakerPhoneIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ path
            [ SvgAttr.stroke "none"
            , SvgAttr.d "M0 0h24v24H0z"
            , SvgAttr.fill "none"
            ]
            []
        , path
            [ SvgAttr.d "M18 8a3 3 0 0 1 0 6"
            ]
            []
        , path
            [ SvgAttr.d "M10 8v11a1 1 0 0 1 -1 1h-1a1 1 0 0 1 -1 -1v-5"
            ]
            []
        , path
            [ SvgAttr.d "M12 8h0l4.524 -3.77a.9 .9 0 0 1 1.476 .692v12.156a.9 .9 0 0 1 -1.476 .692l-4.524 -3.77h-8a1 1 0 0 1 -1 -1v-4a1 1 0 0 1 1 -1h8"
            ]
            []
        ]
    
sunIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ path
            [ SvgAttr.stroke "none"
            , SvgAttr.d "M0 0h24v24H0z"
            , SvgAttr.fill "none"
            ]
            []
        , path
            [ SvgAttr.d "M12 12m-4 0a4 4 0 1 0 8 0a4 4 0 1 0 -8 0"
            ]
            []
        , path
            [ SvgAttr.d "M3 12h1m8 -9v1m8 8h1m-9 8v1m-6.4 -15.4l.7 .7m12.1 -.7l-.7 .7m0 11.4l.7 .7m-12.1 -.7l-.7 .7"
            ]
            []
        ]
    
topologyStar3Icon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ path
            [ SvgAttr.stroke "none"
            , SvgAttr.d "M0 0h24v24H0z"
            , SvgAttr.fill "none"
            ]
            []
        , path
            [ SvgAttr.d "M10 19a2 2 0 1 0 -4 0a2 2 0 0 0 4 0z"
            ]
            []
        , path
            [ SvgAttr.d "M18 5a2 2 0 1 0 -4 0a2 2 0 0 0 4 0z"
            ]
            []
        , path
            [ SvgAttr.d "M10 5a2 2 0 1 0 -4 0a2 2 0 0 0 4 0z"
            ]
            []
        , path
            [ SvgAttr.d "M6 12a2 2 0 1 0 -4 0a2 2 0 0 0 4 0z"
            ]
            []
        , path
            [ SvgAttr.d "M18 19a2 2 0 1 0 -4 0a2 2 0 0 0 4 0z"
            ]
            []
        , path
            [ SvgAttr.d "M14 12a2 2 0 1 0 -4 0a2 2 0 0 0 4 0z"
            ]
            []
        , path
            [ SvgAttr.d "M22 12a2 2 0 1 0 -4 0a2 2 0 0 0 4 0z"
            ]
            []
        , path
            [ SvgAttr.d "M6 12h4"
            ]
            []
        , path
            [ SvgAttr.d "M14 12h4"
            ]
            []
        , path
            [ SvgAttr.d "M15 7l-2 3"
            ]
            []
        , path
            [ SvgAttr.d "M9 7l2 3"
            ]
            []
        , path
            [ SvgAttr.d "M11 14l-2 3"
            ]
            []
        , path
            [ SvgAttr.d "M13 14l2 3"
            ]
            []
        ]
    
plant2Icon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ path
            [ SvgAttr.stroke "none"
            , SvgAttr.d "M0 0h24v24H0z"
            , SvgAttr.fill "none"
            ]
            []
        , path
            [ SvgAttr.d "M2 9a10 10 0 1 0 20 0"
            ]
            []
        , path
            [ SvgAttr.d "M12 19a10 10 0 0 1 10 -10"
            ]
            []
        , path
            [ SvgAttr.d "M2 9a10 10 0 0 1 10 10"
            ]
            []
        , path
            [ SvgAttr.d "M12 4a9.7 9.7 0 0 1 2.99 7.5"
            ]
            []
        , path
            [ SvgAttr.d "M9.01 11.5a9.7 9.7 0 0 1 2.99 -7.5"
            ]
            []
        ]
    
infinityIcon =
    svg
        [ SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ path
            [ SvgAttr.stroke "none"
            , SvgAttr.d "M0 0h24v24H0z"
            , SvgAttr.fill "none"
            ]
            []
        , path
            [ SvgAttr.d "M9.828 9.172a4 4 0 1 0 0 5.656a10 10 0 0 0 2.172 -2.828a10 10 0 0 1 2.172 -2.828a4 4 0 1 1 0 5.656a10 10 0 0 1 -2.172 -2.828a10 10 0 0 0 -2.172 -2.828"
            ]
            []
        ]
    
roadmapCheckIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.width "1.2em"
        , SvgAttr.height "1.2em"
        ]
        [ Svg.g
            [ SvgAttr.fill "none"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            ]
            [ path
                [ SvgAttr.d "M3 12a9 9 0 1 0 18 0a9 9 0 1 0-18 0"
                ]
                []
            , path
                [ SvgAttr.d "m9 12l2 2l4-4"
                ]
                []
            ]
        ]
    
plusIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.width "1.2em"
        , SvgAttr.height "1.2em"
        ]
        [ path
            [ SvgAttr.fill "none"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M12 5v14m-7-7h14"
            ]
            []
        ]
    
minusIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.width "1.2em"
        , SvgAttr.height "1.2em"
        ]
        [ path
            [ SvgAttr.fill "none"
            , SvgAttr.stroke "currentColor"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M5 12h14"
            ]
            []
        ]
    
videoPlayIcon =
    svg
        [ SvgAttr.fill "none"
        , Attr.attribute "aria-hidden" "true"
        , Attr.attribute "focusable" "false"
        , SvgAttr.viewBox "0 0 32 32"
        , Attr.attribute "data-icon" "play"
        ]
        [ path
            [ SvgAttr.d "M10.6667 6.6548C10.6667 6.10764 11.2894 5.79346 11.7295 6.11862L24.377 15.4634C24.7377 15.7298 24.7377 16.2692 24.3771 16.5357L11.7295 25.8813C11.2895 26.2065 10.6667 25.8923 10.6667 25.3451L10.6667 6.6548Z"
            , SvgAttr.fill "currentColor"
            ]
            []
        ]
    