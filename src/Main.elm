module Main exposing (main)

import Browser exposing (sandbox)
import Debug
import Element exposing (Element, column, el, fill, height, row, shrink, spacing, text, width)
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Srt exposing (Srt, srtFromString, srtToString, timestampToString)


type alias InputmodeModel =
    { srtText : String
    , parseError : Maybe String
    }


type alias EditmodeModel =
    { parsedSrt : Srt
    }


type Model
    = InputMode InputmodeModel
    | EditMode EditmodeModel


data =
    """1
00:02:42,941 --> 00:02:46,144
<i>Volwassen Toni: Rockport, Maine, is</i>
<i> het enige thuis dat ik ooit heb gekend. </i>

2
00:02:46,144 --> 00:02:49,347
<i>Ik zal nooit die lente</i>
<i>van 1962 vergeten.</i>

3
00:02:49,347 --> 00:02:52,550
<i> het grote avontuur van mijn familie </i>
ging <b><i>net</i></b> beginnen.

"""


init : Model
init =
    case srtFromString data of
        Ok parsedSrt ->
            EditMode
                { parsedSrt = Debug.log "parsedSrt" parsedSrt
                }

        Err e ->
            InputMode
                { srtText = data
                , parseError = Just e
                }


type Msg
    = ChangedInputText String
    | ClickedParseSrt String
    | ClickedRemoveNewlines
    | ClickedTrimNodes


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( ChangedInputText newText, InputMode inputModeModel ) ->
            { inputModeModel
                | srtText = newText
            }
                |> InputMode

        ( ClickedParseSrt srtText, InputMode _ ) ->
            parseSrt srtText

        ( ClickedRemoveNewlines, EditMode editModeModel ) ->
            removeSrtNewlines editModeModel

        ( ClickedTrimNodes, EditMode editModeModel ) ->
            trimSrtNodes editModeModel

        _ ->
            model


changeInputText : String -> InputmodeModel -> Model
changeInputText newText inputModeModel =
    { inputModeModel
        | srtText = newText
    }
        |> InputMode


parseSrt : String -> Model
parseSrt srtText =
    case srtFromString srtText of
        Ok parsedSrt ->
            EditMode
                { parsedSrt = parsedSrt
                }

        Err e ->
            InputMode
                { srtText = srtText
                , parseError = Just e
                }


removeSrtNewlines : EditmodeModel -> Model
removeSrtNewlines editModeModel =
    { editModeModel
        | parsedSrt = Srt.removeNewlines editModeModel.parsedSrt
    }
        |> EditMode


trimSrtNodes : EditmodeModel -> Model
trimSrtNodes editModeModel =
    { editModeModel
        | parsedSrt = Srt.trimNodes editModeModel.parsedSrt
    }
        |> EditMode


highlightBgColor =
    Element.rgba255 50 50 50 0.8


highlightColor =
    Element.rgb255 200 200 200


parsedSrtView : Srt -> Element msg
parsedSrtView parsedSrt =
    Element.table
        [ Element.scrollbarY
        , height <| Element.maximum 400 fill
        , width fill
        , spacing 10
        ]
        { data = Srt.srtRecords parsedSrt
        , columns =
            [ { header = text "#"
              , width = Element.shrink
              , view = \i -> el [] <| text <| String.fromInt i.index
              }
            , { header = text "from"
              , width = Element.shrink
              , view = \i -> el [] <| text <| timestampToString <| i.timespan.from
              }
            , { header = text "to"
              , width = Element.shrink
              , view = \i -> el [] <| text <| timestampToString <| i.timespan.to
              }
            , { header = text "duration"
              , width = Element.shrink
              , view = \i -> el [] <| text <| timestampToString <| Srt.duration i.timespan
              }
            , { header = text "subtitle"
              , width = fill
              , view = \i -> text <| Srt.srtContentToString i.content
              }
            ]
        }


roundedBorderButton : List (Element.Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
roundedBorderButton attrs =
    Element.Input.button
        ([ Element.Border.color <|
            Element.rgb255 50 50 50
         , Element.Border.solid
         , Element.Border.width 1
         , Element.Border.rounded 5
         , Element.paddingXY 10 7
         ]
            ++ attrs
        )


inputModeView : InputmodeModel -> Element Msg
inputModeView inputModeModel =
    column [ width fill, height fill, spacing 20 ]
        [ Element.Input.multiline
            [ width fill
            , height <| Element.maximum 400 fill
            ]
            { onChange = ChangedInputText
            , text = inputModeModel.srtText
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelAbove [] (text "Enter SRT")
            }
        , roundedBorderButton []
            { label = text "Parse SRT"
            , onPress = Just <| ClickedParseSrt inputModeModel.srtText
            }
        , inputModeModel.parseError
            |> Maybe.map (text >> el [ Element.Font.family [ Element.Font.monospace ] ])
            |> Maybe.withDefault Element.none
        ]


editModeView : EditmodeModel -> Element Msg
editModeView editModeModel =
    column [ width fill, height fill, spacing 20 ]
        [ parsedSrtView editModeModel.parsedSrt
        , roundedBorderButton []
            { label = text "Remove new lines"
            , onPress = Just ClickedRemoveNewlines
            }
        , roundedBorderButton []
            { label = text "Trim nodes"
            , onPress = Just ClickedTrimNodes
            }
        , outputView editModeModel
        ]


outputView : EditmodeModel -> Element Msg
outputView editModeModel =
    column [ width fill, height fill, spacing 20 ]
        [ text <| srtToString editModeModel.parsedSrt ]


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill, Element.padding 10 ] <|
        case model of
            InputMode inputModeModel ->
                inputModeView inputModeModel

            EditMode editmodeModel ->
                editModeView editmodeModel


main =
    sandbox
        { init = init
        , update = update
        , view = view
        }
