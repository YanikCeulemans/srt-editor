module Main exposing (main)

import Browser exposing (sandbox)
import Debug
import Element exposing (Element, column, el, fill, fillPortion, height, row, spacing, text, width, wrappedRow)
import Element.Input
import Html exposing (Html)
import Parser exposing (..)


type alias Model =
    { srtText : String
    , parsedSrt : Maybe (List SubtitleRecord)
    , timeshiftForward : Timestamp
    , timeshiftForwardError : Maybe String
    }


init : Model
init =
    { srtText = ""
    , parsedSrt = Nothing
    , timeshiftForward = Timestamp 0 0 0 0
    , timeshiftForwardError = Nothing
    }


type Msg
    = ChangedSrtText String
    | ChangedTimeshiftForward String
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
            |> map (\_ -> Done (List.reverse revRecordsSoFar))
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedSrtText newText ->
            { model
                | srtText = newText
                , parsedSrt =
                    run srtParser newText
                        |> Result.toMaybe
            }

        ChangedTimeshiftForward newTimeshift ->
            parseAndUpdateTimeshiftForward newTimeshift model

        ClickedShiftTimeForward ->
            shiftTimeForward model


parseAndUpdateTimeshiftForward : String -> Model -> Model
parseAndUpdateTimeshiftForward newTimeshift model =
    case run timestampParser newTimeshift of
        Ok parsedTimeshift ->
            { model
                | timeshiftForward = parsedTimeshift
                , timeshiftForwardError = Nothing
            }

        Err deadEnds ->
            { model
                | timeshiftForwardError =
                    Just <|
                        Debug.toString deadEnds
            }


shiftTimeForward : Model -> Model
shiftTimeForward model =
    case model.parsedSrt of
        Just parsedSrt ->
            { model
                | parsedSrt =
                    parsedSrt
                        |> List.map
                            (\r ->
                                { r
                                    | timespan =
                                        shiftTimespanForward
                                            model.timeshiftForward
                                            r.timespan
                                }
                            )
                        |> Just
            }

        Nothing ->
            model


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


viewParsedResult : List SubtitleRecord -> Element msg
viewParsedResult parsedSrt =
    Element.table
        [ Element.scrollbarY
        , height <| Element.maximum 400 fill
        , width fill
        ]
        { data = parsedSrt
        , columns =
            [ { header = text "#"
              , width = fill
              , view = \i -> el [ Element.centerY ] <| text <| String.fromInt i.index
              }
            , { header = text "from"
              , width = fill
              , view = \i -> el [ Element.centerY ] <| text <| timestampToString <| i.timespan.from
              }
            , { header = text "to"
              , width = fill
              , view = \i -> el [ Element.centerY ] <| text <| timestampToString <| i.timespan.to
              }
            , { header = text "duration"
              , width = fill
              , view = \i -> el [ Element.centerY ] <| text <| timestampToString <| duration i.timespan
              }
            , { header = text "subtitle"
              , width = fill
              , view = \i -> text <| i.content
              }
            ]
        }


viewTimeshifter : Model -> Element Msg
viewTimeshifter model =
    column []
        [ row [ spacing 10 ]
            [ Element.Input.text []
                { onChange = ChangedTimeshiftForward
                , text = model.timeshiftForward |> timestampToString
                , placeholder = Nothing
                , label = Element.Input.labelAbove [] (text "Time shift forward")
                }
            , Element.Input.button []
                { onPress = Just ClickedShiftTimeForward
                , label = text "Shift forward"
                }
            ]
        , Maybe.map text model.timeshiftForwardError
            |> Maybe.withDefault Element.none
        ]


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill, Element.padding 10 ] <|
        column [ width fill, height fill, spacing 20 ]
            [ Element.Input.multiline
                [ width fill
                , height <| Element.maximum 400 fill
                ]
                { onChange = ChangedSrtText
                , text = model.srtText
                , placeholder = Nothing
                , spellcheck = False
                , label = Element.Input.labelAbove [] (text "Enter SRT")
                }
            , Maybe.map viewParsedResult model.parsedSrt
                |> Maybe.withDefault Element.none
            , viewTimeshifter model
            ]


main =
    sandbox
        { init = init
        , update = update
        , view = view
        }
