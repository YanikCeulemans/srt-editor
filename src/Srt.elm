module Srt exposing
    ( Srt
    , SubtitleRecord
    , Timespan
    , Timestamp
    , duration
    , empty
    , removeNewlines
    , srtContentToString
    , srtFromString
    , srtRecords
    , srtToString
    , testDecoration
    , timestampToString
    )

import Array
import Debug
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , chompIf
        , chompUntil
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
    , content : List DecoratedText
    }


type alias DecoratedText =
    { bold : Bool
    , italic : Bool
    , underlined : Bool
    , content : String
    }


type alias Decoration =
    { bold : Bool
    , italic : Bool
    , underlined : Bool
    }


bold : Decoration
bold =
    { bold = True, italic = False, underlined = False }


italic : Decoration
italic =
    { bold = False, italic = True, underlined = False }


underlined : Decoration
underlined =
    { bold = False, italic = False, underlined = True }


type DecoratedNode
    = PlainText String
    | Newline
    | DecoratedNode Decoration (List DecoratedNode)


myDecoration : List DecoratedNode
myDecoration =
    [ PlainText "This is plain text"
    , Newline
    , DecoratedNode bold [ PlainText "This is ", DecoratedNode italic [ PlainText "bold" ], PlainText " text." ]
    , Newline
    , PlainText "Plain text as "
    , DecoratedNode bold [ PlainText "root" ]
    , PlainText " node is also possible"
    ]


wrapWithTag : Bool -> String -> String -> String
wrapWithTag shouldWrap tag toWrap =
    if shouldWrap then
        "<" ++ tag ++ ">" ++ toWrap ++ "</" ++ tag ++ ">"

    else
        toWrap


decoratedNodesToString : List DecoratedNode -> String
decoratedNodesToString nodes =
    List.map decoratedNodeToString nodes
        |> String.concat


decoratedNodeToString : DecoratedNode -> String
decoratedNodeToString decoratedNode =
    case decoratedNode of
        Newline ->
            "\n"

        PlainText str ->
            str

        DecoratedNode decoration nodes ->
            List.map decoratedNodeToString nodes
                |> String.concat
                |> wrapWithTag decoration.bold "b"
                |> wrapWithTag decoration.italic "i"
                |> wrapWithTag decoration.underlined "u"


testDecoration =
    decoratedNodesToString myDecoration


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


subtitleTextParser : Parser (List DecoratedText)
subtitleTextParser =
    Parser.loop [] subtitleTextParserHelp


subtitleTextParserHelp : List DecoratedText -> Parser (Parser.Step (List DecoratedText) (List DecoratedText))
subtitleTextParserHelp decoratedTextSoFar =
    oneOf
        [ succeed (\dt -> Parser.Loop (dt :: decoratedTextSoFar))
            |= decoratedTextParser
            |. symbol "\n"
        , succeed (\_ -> Parser.Done (List.reverse decoratedTextSoFar))
            |= oneOf [ symbol "\n", end ]
        ]


processTag : String -> String -> DecoratedText
processTag tag content =
    { bold = False, italic = True, underlined = False, content = content }


decoratedTextParser : Parser DecoratedText
decoratedTextParser =
    succeed processTag
        |= tagParser
        |= (chompUntil "<" |> getChompedString |> Parser.map String.trim)
        |. tagParser


tagParser : Parser String
tagParser =
    succeed identity
        |. chompIf (\c -> c == '<')
        |. oneOf [ chompIf (\c -> c == '/'), succeed () ]
        |= (Parser.chompUntil ">" |> getChompedString)
        |. symbol ">"



-- oneOf
--     [ boldParser
--     , italicParser
--     , underlinedParser
--     ]


boldParser : Parser DecoratedText
boldParser =
    succeed (\s -> { bold = True, italic = False, underlined = False, content = s })
        |. symbol "<b>"
        |= (chompUntilEndOr "</b>" |> getChompedString)


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

        -- |. oneOf [ symbol "\n\n", end ]
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
    record



-- { record
--     | content = String.replace "\n" "" record.content
-- }


srtFromString : String -> Result String Srt
srtFromString input =
    Parser.run srtParser input
        |> Result.mapError (formatError input)


formatError : String -> List Parser.DeadEnd -> String
formatError src deadEnds =
    "Oops, it looks like I encountered a few errors:\n\n"
        ++ String.concat (List.map (formatErrorHelp src) deadEnds)


listAppendPiped : List a -> List a -> List a
listAppendPiped a b =
    List.append b a


formatErrorHelp : String -> Parser.DeadEnd -> String
formatErrorHelp src deadEnd =
    let
        srcStr =
            String.split "\n" src
                |> Array.fromList
                |> Array.get (deadEnd.row - 1)
                |> Maybe.withDefault "THIS SHOULD NEVER HAPPEN"

        problemStr =
            Debug.toString deadEnd.problem ++ "\n"
    in
    [ problemStr ++ String.repeat (String.length problemStr) "-"
    , srcStr
    , String.repeat (deadEnd.col - 1) " " ++ "^"
    , Debug.toString deadEnd
    , String.toList srcStr
        |> List.indexedMap
            (\i c ->
                if modBy 10 i == 0 then
                    String.fromInt i
                        |> String.toList
                        |> listAppendPiped [ c ]

                else
                    [ c ]
            )
        |> List.concat
        |> String.fromList
    ]
        |> List.intersperse "\n"
        |> listAppendPiped [ "\n\n" ]
        |> String.concat


srtToString : Srt -> String
srtToString (Srt records) =
    List.map srtToStringHelp records
        |> String.join "\n\n"


srtToStringHelp : SubtitleRecord -> String
srtToStringHelp record =
    [ record.index |> String.fromInt
    , record.timespan |> timespanToSrtString
    , record.content |> srtContentToString
    ]
        |> String.join "\n"


timespanToSrtString : Timespan -> String
timespanToSrtString { from, to } =
    timestampToString from ++ " --> " ++ timestampToString to


decoratedTextToString : DecoratedText -> String
decoratedTextToString ({ content } as decoration) =
    wrapWithTag decoration.bold "b" content
        |> wrapWithTag decoration.italic "i"
        |> wrapWithTag decoration.underlined "u"


srtContentToString : List DecoratedText -> String
srtContentToString =
    List.map decoratedTextToString
        >> List.intersperse "\n"
        >> String.concat
