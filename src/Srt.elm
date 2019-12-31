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
    , timestampToString
    , transcript
    , trimNodes
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
    , content : List DecoratedNode
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


hourStringParser : Parser Int
hourStringParser =
    succeed String.toInt
        |= (getChompedString <| chompWhile Char.isDigit)
        |> Parser.andThen maybeIntToParser


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


closingTag : String -> String
closingTag tag =
    "</" ++ tag ++ ">"


tagParser : Parser String
tagParser =
    succeed identity
        |. chompIf (\c -> c == '<')
        |. oneOf [ chompIf (\c -> c == '/'), succeed () ]
        |= (Parser.chompUntil ">" |> getChompedString)
        |. symbol ">"


processNode : String -> DecoratedNode -> DecoratedNode
processNode nodeName decoratedNode =
    case nodeName of
        "i" ->
            DecoratedNode italic [ decoratedNode ]

        "b" ->
            DecoratedNode bold [ decoratedNode ]

        "u" ->
            DecoratedNode underlined [ decoratedNode ]

        _ ->
            decoratedNode


decorationParser : Parser DecoratedNode
decorationParser =
    oneOf
        [ tagParser
            |> Parser.andThen
                (\tag ->
                    succeed (\content -> processNode tag content)
                        |= Parser.lazy (\_ -> decorationParser)
                        |. symbol (closingTag tag)
                )
        , succeed Newline
            |. symbol "\n"
        , succeed PlainText
            |= (oneOf [ chompUntil "<", chompUntil "\n", end ] |> getChompedString)
        ]


contentParserHelp : List DecoratedNode -> Parser (Parser.Step (List DecoratedNode) (List DecoratedNode))
contentParserHelp lst =
    oneOf
        [ succeed (Parser.Done (List.reverse lst))
            |. symbol "\n\n"
        , succeed (\d -> Parser.Loop (d :: lst))
            |= decorationParser
        ]


contentParser : Parser (List DecoratedNode)
contentParser =
    Parser.loop [] contentParserHelp


subtitleRecordParser : Parser SubtitleRecord
subtitleRecordParser =
    succeed SubtitleRecord
        |= lineNumberParser
        |. symbol "\n"
        |= timespanParser
        |. symbol "\n"
        |= contentParser


srtParser : Parser Srt
srtParser =
    Parser.loop [] srtHelp


srtHelp : List SubtitleRecord -> Parser (Parser.Step (List SubtitleRecord) Srt)
srtHelp revRecordsSoFar =
    oneOf
        [ succeed (\r -> Parser.Loop (r :: revRecordsSoFar))
            |= subtitleRecordParser
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
    { record | content = List.filter (\n -> n /= Newline) record.content }


trimNodeText : DecoratedNode -> DecoratedNode
trimNodeText decoratedNode =
    case decoratedNode of
        DecoratedNode d [ PlainText t ] ->
            DecoratedNode d << List.singleton << PlainText <| String.trim t

        _ ->
            decoratedNode


trimNodesHelp : SubtitleRecord -> SubtitleRecord
trimNodesHelp record =
    { record | content = List.map trimNodeText record.content }


trimNodes : Srt -> Srt
trimNodes (Srt records) =
    List.map trimNodesHelp records
        |> Srt


transcriptFromNode : DecoratedNode -> String
transcriptFromNode decoratedNode =
    case decoratedNode of
        PlainText pt ->
            pt

        Newline ->
            " "

        DecoratedNode _ innerNodes ->
            List.map transcriptFromNode innerNodes
                |> String.concat


transcriptHelp : SubtitleRecord -> String
transcriptHelp { content } =
    List.map transcriptFromNode content
        |> String.concat


transcript : Srt -> String
transcript (Srt records) =
    List.map transcriptHelp records
        |> String.concat


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
                        |> List.append [ '|' ]
                        |> listAppendPiped [ '|' ]
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
    , record.content |> decoratedNodesToString
    ]
        |> String.join "\n"


timespanToSrtString : Timespan -> String
timespanToSrtString { from, to } =
    timestampToString from ++ " --> " ++ timestampToString to


srtContentToString : List DecoratedNode -> String
srtContentToString =
    decoratedNodesToString
