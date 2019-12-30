module Srt exposing
    ( Srt
    , SubtitleRecord
    , Timespan
    , Timestamp
    , duration
    , empty
    , removeNewlines
    , srtFromString
    , srtRecords
    , srtToString
    , timestampToString
    )

import Debug
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , chompUntilEndOr
        , chompWhile
        , end
        , getChompedString
        , oneOf
        , problem
        , succeed
        , symbol
        )


type Srt
    = Srt (List SubtitleRecord)


type alias SubtitleRecord =
    { index : Int
    , timespan : Timespan
    , content : String
    }


type alias Timespan =
    { from : Timestamp
    , to : Timestamp
    }


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


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')


lineNumberParser : Parser Int
lineNumberParser =
    succeed identity
        |= Parser.int
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
        |> Parser.andThen maybeIntToParser


hourStringParser : Parser Int
hourStringParser =
    succeed String.toInt
        |= (getChompedString <| chompWhile Char.isDigit)
        |> Parser.andThen maybeIntToParser


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
        |> Parser.andThen maybeIntToParser
        |> Parser.andThen (checkMaximum 59)


millisecondParser : Parser Int
millisecondParser =
    succeed String.toInt
        |= (getChompedString <| chompWhile Char.isDigit)
        |> Parser.andThen maybeIntToParser
        |> Parser.andThen (checkMaximum 999)


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


srtParser : Parser Srt
srtParser =
    Parser.loop [] srtHelp


srtHelp : List SubtitleRecord -> Parser (Parser.Step (List SubtitleRecord) Srt)
srtHelp revRecordsSoFar =
    oneOf
        [ succeed (\r -> Parser.Loop (r :: revRecordsSoFar))
            |= subtitleRecordParser
            |. oneOf [ symbol "\n\n", end ]
        , succeed ()
            |. end
            |> Parser.map (\_ -> Parser.Done (Srt (List.reverse revRecordsSoFar)))
        ]


srtRecords : Srt -> List SubtitleRecord
srtRecords (Srt records) =
    records


empty : Srt
empty =
    Srt []


removeNewlines : Srt -> Srt
removeNewlines (Srt records) =
    List.map removeNewlinesHelp records
        |> Srt


removeNewlinesHelp : SubtitleRecord -> SubtitleRecord
removeNewlinesHelp record =
    { record
        | content = String.replace "\n" "" record.content
    }


srtFromString : String -> Result String Srt
srtFromString input =
    Parser.run srtParser input
        |> Result.mapError (Debug.toString >> (++) "TODO MAP ERROR: ")


srtToString : Srt -> String
srtToString (Srt records) =
    List.map srtToStringHelp records
        |> String.join "\n\n"


srtToStringHelp : SubtitleRecord -> String
srtToStringHelp record =
    [ record.index |> String.fromInt
    , record.timespan |> timespanToSrtString
    , record.content
    ]
        |> String.join "\n"


timespanToSrtString : Timespan -> String
timespanToSrtString { from, to } =
    timestampToString from ++ " --> " ++ timestampToString to
