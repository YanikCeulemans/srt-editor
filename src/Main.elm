module Main exposing (main)

import Browser exposing (sandbox)
import Debug
import Element exposing (Element, column, el, fill, height, row, shrink, spacing, text, width, wrappedRow)
import Element.Background
import Element.Font
import Element.Input
import Html exposing (Html)
import Parser exposing (..)


type alias InputmodeModel =
    { srtText : String
    , parseError : Maybe String
    }


type alias EditmodeModel =
    { parsedSrt : List SubtitleRecord
    , timeshiftForward : Timestamp
    , timeshiftForwardError : Maybe String
    }


type Model
    = InputMode InputmodeModel
    | EditMode EditmodeModel


init : Model
init =
    InputMode
        { srtText = ""
        , parseError = Nothing
        }


type Msg
    = ChangedInputText String
    | ChangedTimeshiftForward String
    | ClickedParseSrt String
    | ClickedShiftTimeForward


type alias Timestamp =
    { hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    }


intToPaddedString : Int -> Int -> String
intToPaddedString amount =
    String.fromInt >> String.padLeft amount '0'


timestampToMilliseconds : Timestamp -> Int
timestampToMilliseconds { hour, minute, second, millisecond } =
    millisecond
        + (second * 1000)
        + (minute * 60 * 1000)
        + (hour * 60 * 60 * 1000)


timestampFromMilliseconds : Int -> Timestamp
timestampFromMilliseconds milliseconds =
    Timestamp
        (toFloat milliseconds / 60 / 60 / 1000 |> floor)
        (toFloat milliseconds / 60 / 1000 |> floor)
        (toFloat (modBy 60000 milliseconds) / 1000 |> floor)
        (modBy 1000 milliseconds)


timestampToString : Timestamp -> String
timestampToString { hour, minute, second, millisecond } =
    intToPaddedString 2 hour
        ++ ":"
        ++ intToPaddedString 2 minute
        ++ ":"
        ++ intToPaddedString 2 second
        ++ ","
        ++ intToPaddedString 3 millisecond


type alias Timespan =
    { from : Timestamp
    , to : Timestamp
    }


duration : Timespan -> Timestamp
duration { from, to } =
    timestampToMilliseconds to
        - timestampToMilliseconds from
        |> timestampFromMilliseconds


timespanToString : Timespan -> String
timespanToString { from, to } =
    timestampToString from ++ " => " ++ timestampToString to


addToTimestamp : Timestamp -> Timestamp -> Timestamp
addToTimestamp timestamp1 timestamp2 =
    timestampToMilliseconds timestamp1
        + timestampToMilliseconds timestamp2
        |> timestampFromMilliseconds


shiftTimespanForward : Timestamp -> Timespan -> Timespan
shiftTimespanForward timeAmount timespan =
    Timespan
        (addToTimestamp timeAmount timespan.from)
        (addToTimestamp timeAmount timespan.to)


type alias SubtitleRecord =
    { index : Int
    , timespan : Timespan
    , content : String
    }


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')


lineNumberParser : Parser Int
lineNumberParser =
    succeed identity
        |= int
        |. whitespace


maybeIntToParser : Maybe Int -> Parser Int
maybeIntToParser maybeInt =
    case maybeInt of
        Just int ->
            succeed int

        Nothing ->
            problem "Expected an integer"


timeStringParser : Parser Int
timeStringParser =
    succeed String.toInt
        |= (getChompedString <| chompWhile Char.isDigit)
        |> andThen maybeIntToParser


hourStringParser : Parser Int
hourStringParser =
    succeed String.toInt
        |= (getChompedString <| chompWhile Char.isDigit)
        |> andThen maybeIntToParser


checkLength : Int -> String -> Parser String
checkLength amount strToCheck =
    if String.length strToCheck == amount then
        succeed strToCheck

    else
        problem <| "Expected a max length of " ++ String.fromInt amount


checkMaximum : Int -> Int -> Parser Int
checkMaximum max intToCheck =
    if intToCheck <= max then
        succeed intToCheck

    else
        problem <| "Expected a maximum number of " ++ String.fromInt max


minuteSecondStringParser : Parser Int
minuteSecondStringParser =
    succeed String.toInt
        |= (getChompedString <| chompWhile Char.isDigit)
        |> andThen maybeIntToParser
        |> andThen (checkMaximum 59)


millisecondParser : Parser Int
millisecondParser =
    succeed String.toInt
        |= (getChompedString <| chompWhile Char.isDigit)
        |> andThen maybeIntToParser
        |> andThen (checkMaximum 999)


timestampParser : Parser Timestamp
timestampParser =
    succeed Timestamp
        |= hourStringParser
        |. symbol ":"
        |= minuteSecondStringParser
        |. symbol ":"
        |= minuteSecondStringParser
        |. symbol ","
        |= millisecondParser


timespanParser : Parser Timespan
timespanParser =
    succeed Timespan
        |= timestampParser
        |. whitespace
        |. symbol "-->"
        |. whitespace
        |= timestampParser


subtitleTextParser : Parser String
subtitleTextParser =
    chompUntilEndOr "\n\n"
        |> getChompedString


subtitleRecordParser : Parser SubtitleRecord
subtitleRecordParser =
    succeed SubtitleRecord
        |= lineNumberParser
        |. symbol "\n"
        |= timespanParser
        |. symbol "\n"
        |= subtitleTextParser


srtParser : Parser (List SubtitleRecord)
srtParser =
    loop [] srtHelp


srtHelp : List SubtitleRecord -> Parser (Step (List SubtitleRecord) (List SubtitleRecord))
srtHelp revRecordsSoFar =
    oneOf
        [ succeed (\r -> Loop (r :: revRecordsSoFar))
            |= subtitleRecordParser
            |. oneOf [ symbol "\n\n", end ]
        , succeed ()
            |. end
            |> map (\_ -> Done (List.reverse revRecordsSoFar))
        ]


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( ChangedInputText newText, InputMode inputModeModel ) ->
            { inputModeModel
                | srtText = newText
            }
                |> InputMode

        ( ChangedTimeshiftForward newTimeshift, EditMode editModeModel ) ->
            parseAndUpdateTimeshiftForward newTimeshift editModeModel

        ( ClickedShiftTimeForward, EditMode editModeModel ) ->
            shiftTimeForward editModeModel

        ( ClickedParseSrt srtText, InputMode _ ) ->
            parseSrt srtText

        _ ->
            model


changeInputText : String -> InputmodeModel -> Model
changeInputText newText inputModeModel =
    { inputModeModel
        | srtText = newText
    }
        |> InputMode


parseAndUpdateTimeshiftForward : String -> EditmodeModel -> Model
parseAndUpdateTimeshiftForward newTimeshift editModeModel =
    case run timestampParser newTimeshift of
        Ok parsedTimeshift ->
            { editModeModel
                | timeshiftForward = parsedTimeshift
                , timeshiftForwardError = Nothing
            }
                |> EditMode

        Err deadEnds ->
            { editModeModel
                | timeshiftForwardError =
                    Just <|
                        Debug.toString deadEnds
            }
                |> EditMode


shiftTimeForward : EditmodeModel -> Model
shiftTimeForward editModeModel =
    { editModeModel
        | parsedSrt =
            editModeModel.parsedSrt
                |> List.map
                    (\r ->
                        { r
                            | timespan =
                                shiftTimespanForward
                                    editModeModel.timeshiftForward
                                    r.timespan
                        }
                    )
    }
        |> EditMode


parseSrt : String -> Model
parseSrt srtText =
    case Debug.log "result" <| Parser.run srtParser srtText of
        Ok parsedSrt ->
            EditMode
                { parsedSrt = parsedSrt
                , timeshiftForward = Timestamp 0 0 0 0
                , timeshiftForwardError = Nothing
                }

        Err deadEnds ->
            InputMode
                { srtText = srtText
                , parseError = deadEnds |> Debug.toString |> Just
                }


viewRecord : SubtitleRecord -> Element msg
viewRecord { index, timespan, content } =
    column []
        [ row [ spacing 10 ]
            [ text <| String.fromInt index ++ ")"
            , text <| timespanToString timespan
            , text <|
                (duration timespan
                    |> timestampToString
                    |> (\s -> "(" ++ s ++ ")")
                )
            ]
        , text content
        ]


highlightBgColor =
    Element.rgba255 50 50 50 0.8


highlightColor =
    Element.rgb255 200 200 200


parsedSrtView : List SubtitleRecord -> Element msg
parsedSrtView parsedSrt =
    Element.table
        [ Element.scrollbarY
        , height <| Element.maximum 400 fill
        , width fill
        , spacing 10
        ]
        { data = parsedSrt
        , columns =
            [ { header = text "#"
              , width = Element.shrink
              , view = \i -> el [ Element.centerY ] <| text <| String.fromInt i.index
              }
            , { header = text "from"
              , width = Element.shrink
              , view = \i -> el [ Element.centerY ] <| text <| timestampToString <| i.timespan.from
              }
            , { header = text "to"
              , width = Element.shrink
              , view = \i -> el [ Element.centerY ] <| text <| timestampToString <| i.timespan.to
              }
            , { header = text "duration"
              , width = Element.shrink
              , view = \i -> el [ Element.centerY ] <| text <| timestampToString <| duration i.timespan
              }
            , { header = text "subtitle"
              , width = fill
              , view = \i -> text <| i.content
              }
            ]
        }


timeshifterView : EditmodeModel -> Element Msg
timeshifterView editModeModel =
    column []
        [ row [ spacing 10 ]
            [ Element.Input.text []
                { onChange = ChangedTimeshiftForward
                , text = editModeModel.timeshiftForward |> timestampToString
                , placeholder = Nothing
                , label = Element.Input.labelAbove [] (text "Time shift forward")
                }
            , Element.Input.button []
                { onPress = Just ClickedShiftTimeForward
                , label = text "Shift forward"
                }
            ]
        , Maybe.map text editModeModel.timeshiftForwardError
            |> Maybe.withDefault Element.none
        ]


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
        , Element.Input.button []
            { label = text "Parse SRT"
            , onPress = Just <| ClickedParseSrt inputModeModel.srtText
            }
        ]


editModeView : EditmodeModel -> Element Msg
editModeView editModeModel =
    column [ width fill, height fill, spacing 20 ]
        [ parsedSrtView editModeModel.parsedSrt
        , timeshifterView editModeModel
        ]


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
