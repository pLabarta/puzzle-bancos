module Main exposing (main)

import Axioms
import Browser
import Browser.Dom as Dom
import Browser.Events
import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode
import Layout exposing (DropTarget(..))
import Puzzle
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Types exposing (Axiom, AxiomStatus(..), Board, Row(..), Seat, Student, StudentId)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias DragState =
    { studentId : StudentId
    , x : Float
    , y : Float
    }


type alias Model =
    { board : Board
    , boardOrigin : { x : Float, y : Float }
    , drag : Maybe DragState
    }


initialBoard : Board
initialBoard =
    { seats = Dict.empty
    , buffer = List.map .id Puzzle.students
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = initialBoard
      , boardOrigin = { x = 0, y = 0 }
      , drag = Nothing
      }
    , Task.attempt GotBoardOrigin (Dom.getElement "board")
    )



-- UPDATE


type Msg
    = GotBoardOrigin (Result Dom.Error Dom.Element)
    | StudentMouseDown StudentId Float Float
    | MouseMoved Float Float
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBoardOrigin result ->
            case result of
                Ok element ->
                    ( { model
                        | boardOrigin =
                            { x = element.element.x, y = element.element.y }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        StudentMouseDown studentId clientX clientY ->
            ( { model
                | drag =
                    Just
                        { studentId = studentId
                        , x = clientX - model.boardOrigin.x
                        , y = clientY - model.boardOrigin.y
                        }
              }
            , Cmd.none
            )

        MouseMoved clientX clientY ->
            case model.drag of
                Nothing ->
                    ( model, Cmd.none )

                Just drag ->
                    ( { model
                        | drag =
                            Just
                                { drag
                                    | x = clientX - model.boardOrigin.x
                                    , y = clientY - model.boardOrigin.y
                                }
                      }
                    , Cmd.none
                    )

        MouseUp ->
            case model.drag of
                Nothing ->
                    ( model, Cmd.none )

                Just drag ->
                    ( { model
                        | board = dropStudent drag.studentId ( drag.x, drag.y ) model.board
                        , drag = Nothing
                      }
                    , Cmd.none
                    )


{-| Remove a student from wherever they currently are on the board.
-}
removeStudent : StudentId -> Board -> Board
removeStudent studentId board =
    { seats = Dict.filter (\_ occupant -> occupant /= studentId) board.seats
    , buffer = List.filter (\id -> id /= studentId) board.buffer
    }


seatKey : Seat -> ( Int, Int )
seatKey seat =
    ( Types.rowOf seat, Types.colOf seat )


seatFromKey : ( Int, Int ) -> Seat
seatFromKey ( rowIdx, col ) =
    { row =
        if rowIdx == 0 then
            Front

        else
            Back
    , col = col
    }


type PreviousLocation
    = SeatLocation Seat
    | BufferLocation


previousLocation : StudentId -> Board -> PreviousLocation
previousLocation studentId board =
    board.seats
        |> Dict.toList
        |> List.filter (\( _, occupant ) -> occupant == studentId)
        |> List.head
        |> Maybe.map (\( key, _ ) -> SeatLocation (seatFromKey key))
        |> Maybe.withDefault BufferLocation


dropStudent : StudentId -> ( Float, Float ) -> Board -> Board
dropStudent studentId point board =
    let
        boardWithoutStudent =
            removeStudent studentId board
    in
    case Layout.hitTest point of
        OnSeat seat ->
            case Dict.get (seatKey seat) boardWithoutStudent.seats of
                Just occupant ->
                    let
                        swappedBoard =
                            { boardWithoutStudent
                                | seats = Dict.insert (seatKey seat) studentId boardWithoutStudent.seats
                            }
                    in
                    case previousLocation studentId board of
                        SeatLocation prevSeat ->
                            { swappedBoard
                                | seats = Dict.insert (seatKey prevSeat) occupant swappedBoard.seats
                            }

                        BufferLocation ->
                            { swappedBoard | buffer = occupant :: swappedBoard.buffer }

                Nothing ->
                    { boardWithoutStudent
                        | seats = Dict.insert (seatKey seat) studentId boardWithoutStudent.seats
                    }

        InBuffer ->
            { boardWithoutStudent | buffer = studentId :: boardWithoutStudent.buffer }

        Nowhere ->
            board



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map2 MouseMoved clientXDecoder clientYDecoder)
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]


clientXDecoder : Decode.Decoder Float
clientXDecoder =
    Decode.field "clientX" Decode.float


clientYDecoder : Decode.Decoder Float
clientYDecoder =
    Decode.field "clientY" Decode.float



-- VIEW


findStudent : StudentId -> Maybe Student
findStudent id =
    Puzzle.students
        |> List.filter (\s -> s.id == id)
        |> List.head


studentName : StudentId -> String
studentName id =
    findStudent id
        |> Maybe.map .name
        |> Maybe.withDefault "?"


allSolved : Board -> Bool
allSolved board =
    Dict.size board.seats
        == 8
        && List.all (\axiom -> Axioms.axiomStatus board axiom == Satisfied) Puzzle.axioms


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "font-family" "sans-serif"
        , HA.style "display" "flex"
        , HA.style "gap" "24px"
        , HA.style "padding" "24px"
        ]
        [ Html.div []
            [ boardView model
            , if allSolved model.board then
                Html.p
                    [ HA.style "color" "#118ab2"
                    , HA.style "font-weight" "bold"
                    ]
                    [ Html.text "¡Puzzle resuelto!" ]

              else
                Html.text ""
            ]
        , cluesView model.board
        ]


boardView : Model -> Svg Msg
boardView model =
    Svg.svg
        [ SA.id "board"
        , SA.width (String.fromFloat Layout.boardWidth)
        , SA.height (String.fromFloat Layout.boardHeight)
        , SA.viewBox
            ("0 0 "
                ++ String.fromFloat Layout.boardWidth
                ++ " "
                ++ String.fromFloat Layout.boardHeight
            )
        , SA.style "background:#faf7f2;border:1px solid #ddd;"
        ]
        (List.concat
            [ benchesView
            , [ bufferZoneView ]
            , seatedStudentsView model.board
            , bufferedStudentsView model.board
            , dragGhostView model
            ]
        )


benchesView : List (Svg Msg)
benchesView =
    List.concatMap
        (\row ->
            List.map
                (\benchIndex ->
                    let
                        rect =
                            Layout.benchRect row benchIndex
                    in
                    Svg.rect
                        [ SA.x (String.fromFloat rect.x)
                        , SA.y (String.fromFloat rect.y)
                        , SA.width (String.fromFloat rect.width)
                        , SA.height (String.fromFloat rect.height)
                        , SA.rx "6"
                        , SA.fill "#d9c3a0"
                        , SA.stroke "#b89b6d"
                        ]
                        []
                )
                [ 0, 1 ]
        )
        [ Front, Back ]


bufferZoneView : Svg Msg
bufferZoneView =
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat Layout.bufferRect.x)
            , SA.y (String.fromFloat Layout.bufferRect.y)
            , SA.width (String.fromFloat Layout.bufferRect.width)
            , SA.height (String.fromFloat Layout.bufferRect.height)
            , SA.rx "6"
            , SA.fill "#eef1f5"
            , SA.stroke "#ccc"
            , SA.strokeDasharray "6,4"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (Layout.bufferRect.x + 12))
            , SA.y (String.fromFloat (Layout.bufferRect.y + 20))
            , SA.fill "#888"
            , SA.fontSize "13"
            ]
            [ Svg.text "Zona de espera" ]
        ]


seatedStudentsView : Board -> List (Svg Msg)
seatedStudentsView board =
    board.seats
        |> Dict.toList
        |> List.map
            (\( key, studentId ) ->
                let
                    ( cx, cy ) =
                        Layout.seatCenter (seatFromKey key)
                in
                studentCircle studentId cx cy
            )


bufferedStudentsView : Board -> List (Svg Msg)
bufferedStudentsView board =
    board.buffer
        |> List.indexedMap
            (\index studentId ->
                let
                    ( cx, cy ) =
                        Layout.bufferSlotCenter index
                in
                studentCircle studentId cx cy
            )


dragGhostView : Model -> List (Svg Msg)
dragGhostView model =
    case model.drag of
        Nothing ->
            []

        Just drag ->
            [ studentCircleAt drag.studentId drag.x drag.y True ]


studentCircle : StudentId -> Float -> Float -> Svg Msg
studentCircle studentId cx cy =
    studentCircleAt studentId cx cy False


studentCircleAt : StudentId -> Float -> Float -> Bool -> Svg Msg
studentCircleAt studentId cx cy isGhost =
    let
        student =
            findStudent studentId

        color =
            student |> Maybe.map .color |> Maybe.withDefault "#999"

        name =
            student |> Maybe.map .name |> Maybe.withDefault "?"

        mouseDownDecoder =
            Decode.map2 (StudentMouseDown studentId) clientXDecoder clientYDecoder
    in
    Svg.g
        (if isGhost then
            [ SA.style "pointer-events:none;", SA.opacity "0.85" ]

         else
            [ SE.on "mousedown" mouseDownDecoder
            , SA.style "cursor:grab;"
            ]
        )
        [ Svg.circle
            [ SA.cx (String.fromFloat cx)
            , SA.cy (String.fromFloat cy)
            , SA.r (String.fromFloat Layout.seatRadius)
            , SA.fill color
            , SA.stroke "#333"
            , SA.strokeWidth "1.5"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat (cy + 4))
            , SA.textAnchor "middle"
            , SA.fill "white"
            , SA.fontSize "12"
            , SA.style "pointer-events:none;user-select:none;"
            ]
            [ Svg.text name ]
        ]


cluesView : Board -> Html Msg
cluesView board =
    Html.div
        [ HA.style "min-width" "280px" ]
        [ Html.h3 [] [ Html.text "Pistas" ]
        , Html.ul
            [ HA.style "list-style" "none"
            , HA.style "padding" "0"
            ]
            (List.map (clueItem board) Puzzle.axioms)
        ]


clueItem : Board -> Axiom -> Html Msg
clueItem board axiom =
    let
        status =
            Axioms.axiomStatus board axiom

        ( icon, color ) =
            case status of
                Satisfied ->
                    ( "✓", "#2a9d8f" )

                Violated ->
                    ( "✗", "#e63946" )

                Pending ->
                    ( "•", "#999" )
    in
    Html.li
        [ HA.style "color" color
        , HA.style "margin-bottom" "6px"
        ]
        [ Html.text (icon ++ " " ++ Axioms.clueText studentName axiom) ]
