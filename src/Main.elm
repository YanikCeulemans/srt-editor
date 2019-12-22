module Main exposing (main)

import Browser exposing (sandbox)
import Debug
import Element exposing (Element, column, el, fill, fillPortion, height, row, spacing, text, width, wrappedRow)
import Element.Input
import Html exposing (Html)
import Parser exposing (..)


type alias Model =
    String


type Msg
    = ChangedText String


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


timestampParser : Parser Timestamp
timestampParser =
    succeed Timestamp
        |= timeStringParser
        |. symbol ":"
        |= timeStringParser
        |. symbol ":"
        |= timeStringParser
        |. symbol ","
        |= timeStringParser


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
        ChangedText newText ->
            newText


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


viewParsedResult : String -> Element msg
viewParsedResult dataToParse =
    case run srtParser dataToParse of
        Ok parsedSrt ->
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

        -- List.map viewRecord parsedSrt
        --     |> column
        --         [ spacing 10
        --         , Element.scrollbarY
        --         , height <| Element.maximum 400 fill
        --         , width fill
        --         ]
        Err deadEnds ->
            Debug.toString deadEnds
                |> text


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill ] <|
        column [ width fill, height fill, spacing 20 ]
            [ Element.Input.multiline
                [ width fill
                , height <| Element.maximum 400 fill
                ]
                { onChange = ChangedText
                , text = model
                , placeholder = Nothing
                , spellcheck = False
                , label = Element.Input.labelAbove [] (text "Enter SRT")
                }
            , viewParsedResult model
            ]


main =
    sandbox
        { init = ""
        , update = update
        , view = view
        }
