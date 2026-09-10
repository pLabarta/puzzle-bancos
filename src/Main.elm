module Main exposing (main)

import Axioms
import Browser
import Browser.Dom as Dom
import Browser.Events
import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Layout exposing (DropTarget(..))
import Puzzle
import Random
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Types exposing (Axiom, AxiomStatus(..), Board, Seat, Student, StudentId)


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
    , discoveredAxioms : Set Int
    , cluesOpen : Bool
    }


initialBoard : Board
initialBoard =
    { seats = Dict.empty
    , buffer = List.map .id Puzzle.students
    }


{-| A permutation of `list`, via the classic "pair each item with a random
number and sort by that" trick.
-}
shuffle : List a -> Random.Generator (List a)
shuffle list =
    Random.list (List.length list) (Random.float 0 1)
        |> Random.map
            (\weights ->
                List.map2 Tuple.pair weights list
                    |> List.sortBy Tuple.first
                    |> List.map Tuple.second
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = initialBoard
      , boardOrigin = { x = 0, y = 0 }
      , drag = Nothing
      , discoveredAxioms = Set.empty
      , cluesOpen = False
      }
    , Cmd.batch
        [ Task.attempt GotBoardOrigin (Dom.getElement "board")
        , Random.generate ShuffledBuffer (shuffle initialBoard.buffer)
        ]
    )



-- UPDATE


type Msg
    = GotBoardOrigin (Result Dom.Error Dom.Element)
    | ShuffledBuffer (List StudentId)
    | StudentMouseDown StudentId Float Float
    | MouseMoved Float Float
    | MouseUp
    | ToggleClues


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

        ShuffledBuffer shuffledIds ->
            let
                board =
                    model.board
            in
            ( { model | board = { board | buffer = shuffledIds } }, Cmd.none )

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
                    let
                        newBoard =
                            dropStudent drag.studentId ( drag.x, drag.y ) model.board
                    in
                    ( { model
                        | board = newBoard
                        , drag = Nothing
                        , discoveredAxioms = discoverViolatedAxioms newBoard model.discoveredAxioms
                      }
                    , Cmd.none
                    )

        ToggleClues ->
            ( { model | cluesOpen = not model.cluesOpen }, Cmd.none )


{-| Axioms start hidden - a clue is only revealed once the board violates it,
and stays visible (even once satisfied again) from then on.
-}
discoverViolatedAxioms : Board -> Set Int -> Set Int
discoverViolatedAxioms board discovered =
    Puzzle.axioms
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( index, axiom ) acc ->
                if Axioms.axiomStatus board axiom == Violated then
                    Set.insert index acc

                else
                    acc
            )
            discovered


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
seatFromKey ( row, col ) =
    { row = row, col = col }


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


{-| Touch events don't carry clientX/clientY directly - they're on the first
entry of the `touches` array. Touch targeting "locks" to whichever element
received the `touchstart`, so a touchmove/touchend handler placed on the
board keeps firing for a drag even once the finger has moved over other
elements (unlike mouse events, which don't need this).
-}
touchPointDecoder : Decode.Decoder ( Float, Float )
touchPointDecoder =
    Decode.field "touches"
        (Decode.index 0
            (Decode.map2 Tuple.pair
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
        )



-- VIEW


findStudent : StudentId -> Maybe Student
findStudent id =
    Puzzle.students
        |> List.filter (\s -> s.id == id)
        |> List.head


studentOrPlaceholder : StudentId -> Student
studentOrPlaceholder id =
    findStudent id
        |> Maybe.withDefault { id = id, name = "?", color = "#999", handedness = Types.RightHanded }


allSolved : Board -> Bool
allSolved board =
    Dict.size board.seats
        == List.length Puzzle.students
        && List.all (\axiom -> Axioms.axiomStatus board axiom == Satisfied) Puzzle.axioms


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "font-family" "sans-serif"
        , HA.style "background" "#16181c"
        , HA.style "min-height" "100vh"
        ]
        [ Html.div
            [ HA.style "padding" "24px"
            , HA.style "padding-right" "88px"
            ]
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
        , cluesDrawer model.board model.discoveredAxioms model.cluesOpen
        ]


boardView : Model -> Svg Msg
boardView model =
    Svg.svg
        (List.concat
            [ [ SA.id "board"
              , SA.width (String.fromFloat Layout.boardWidth)
              , SA.height (String.fromFloat Layout.boardHeight)
              , SA.viewBox
                    ("0 0 "
                        ++ String.fromFloat Layout.boardWidth
                        ++ " "
                        ++ String.fromFloat Layout.boardHeight
                    )
              , SA.style "background:#24262b;border:1px solid #3a3d44;touch-action:none;"
              ]
            , if model.drag == Nothing then
                []

              else
                [ HE.preventDefaultOn "touchmove"
                    (touchPointDecoder |> Decode.map (\( x, y ) -> ( MouseMoved x y, True )))
                , SE.on "touchend" (Decode.succeed MouseUp)
                , SE.on "touchcancel" (Decode.succeed MouseUp)
                ]
            ]
        )
        (List.concat
            [ [ chalkboardView ]
            , benchesView
            , [ bufferZoneView ]
            , seatedStudentsView model.board
            , bufferedStudentsView model.board
            , dragGhostView model
            ]
        )


chalkboardView : Svg Msg
chalkboardView =
    let
        rect =
            Layout.chalkboardRect
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat rect.x)
            , SA.y (String.fromFloat rect.y)
            , SA.width (String.fromFloat rect.width)
            , SA.height (String.fromFloat rect.height)
            , SA.rx "4"
            , SA.fill "#2a7a4f"
            , SA.stroke "#1e5c3a"
            , SA.strokeWidth "2"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (rect.x + rect.width / 2))
            , SA.y (String.fromFloat (rect.y + rect.height / 2 + 4))
            , SA.textAnchor "middle"
            , SA.fill "white"
            , SA.fontSize "13"
            , SA.style "user-select:none;"
            ]
            [ Svg.text "PIZARRA" ]
        ]


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
                (List.range 0 (Types.benchesPerRow - 1))
        )
        (List.range 1 Types.rowsPerBoard)


bufferZoneView : Svg Msg
bufferZoneView =
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat Layout.bufferRect.x)
            , SA.y (String.fromFloat Layout.bufferRect.y)
            , SA.width (String.fromFloat Layout.bufferRect.width)
            , SA.height (String.fromFloat Layout.bufferRect.height)
            , SA.rx "6"
            , SA.fill "#2a2d33"
            , SA.stroke "#53565c"
            , SA.strokeDasharray "6,4"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (Layout.bufferRect.x + 12))
            , SA.y (String.fromFloat (Layout.bufferRect.y + 20))
            , SA.fill "#9aa0a6"
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

        touchStartDecoder =
            touchPointDecoder
                |> Decode.map (\( x, y ) -> ( StudentMouseDown studentId x y, True ))
    in
    Svg.g
        (if isGhost then
            [ SA.style "pointer-events:none;", SA.opacity "0.85" ]

         else
            [ SE.on "mousedown" mouseDownDecoder
            , HE.preventDefaultOn "touchstart" touchStartDecoder
            , SA.style "cursor:grab;touch-action:none;"
            ]
        )
        [ Svg.circle
            [ SA.cx (String.fromFloat cx)
            , SA.cy (String.fromFloat cy)
            , SA.r (String.fromFloat Layout.seatRadius)
            , SA.fill color
            , SA.stroke "#e3e1dc"
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


drawerWidthCollapsed : String
drawerWidthCollapsed =
    "64px"


drawerWidthOpen : String
drawerWidthOpen =
    "320px"


{-| Counts of currently-discovered axioms that are satisfied vs. violated
right now (a discovered axiom can also be `Pending` if a referenced student
got pulled back into the buffer - that's neither, so it's not counted).
-}
discoveredCounts : Board -> Set Int -> { ok : Int, failing : Int }
discoveredCounts board discoveredAxioms =
    Puzzle.axioms
        |> List.indexedMap Tuple.pair
        |> List.filter (\( index, _ ) -> Set.member index discoveredAxioms)
        |> List.foldl
            (\( _, axiom ) acc ->
                case Axioms.axiomStatus board axiom of
                    Satisfied ->
                        { acc | ok = acc.ok + 1 }

                    Violated ->
                        { acc | failing = acc.failing + 1 }

                    Pending ->
                        acc
            )
            { ok = 0, failing = 0 }


cluesDrawer : Board -> Set Int -> Bool -> Html Msg
cluesDrawer board discoveredAxioms isOpen =
    let
        counts =
            discoveredCounts board discoveredAxioms
    in
    Html.div
        (List.concat
            [ [ HA.style "position" "fixed"
              , HA.style "top" "0"
              , HA.style "right" "0"
              , HA.style "height" "100vh"
              , HA.style "width"
                    (if isOpen then
                        drawerWidthOpen

                     else
                        drawerWidthCollapsed
                    )
              , HA.style "background" "#1d1f24"
              , HA.style "border-left" "1px solid #3a3d44"
              , HA.style "box-shadow" "-4px 0 12px rgba(0,0,0,0.35)"
              , HA.style "transition" "width 0.2s ease"
              , HA.style "overflow" "hidden"
              , HA.style "box-sizing" "border-box"
              , HA.style "display" "flex"
              , HA.style "flex-direction" "column"
              , HA.style "z-index" "10"
              ]
            , if isOpen then
                []

              else
                -- Collapsed: the whole rail expands the drawer, not just
                -- the chevron, so it's easy to tap on a tablet.
                [ HE.onClick ToggleClues, HA.style "cursor" "pointer" ]
            ]
        )
        [ drawerHeader counts isOpen
        , if isOpen then
            cluesList board discoveredAxioms

          else
            Html.text ""
        ]


drawerHeader : { ok : Int, failing : Int } -> Bool -> Html Msg
drawerHeader counts isOpen =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "padding" "12px 8px"
        , HA.style "gap" "10px"
        , if isOpen then
            HA.style "justify-content" "space-between"

          else
            HA.style "flex-direction" "column"
        ]
        [ if isOpen then
            Html.h3 [ HA.style "margin" "0", HA.style "color" "#ffffff" ] [ Html.text "Pistas" ]

          else
            Html.text ""
        , Html.div
            [ HA.style "display" "flex"
            , if isOpen then
                HA.style "flex-direction" "row"

              else
                HA.style "flex-direction" "column"
            , HA.style "gap" "8px"
            ]
            [ countBadge "✓" counts.ok "#2a9d8f"
            , countBadge "✗" counts.failing "#e63946"
            ]
        , if isOpen then
            -- Expanded: only this button collapses it, so clicking around
            -- the clue list (to read/scroll) doesn't accidentally close it.
            Html.button
                [ HE.onClick ToggleClues
                , HA.style "background" "none"
                , HA.style "border" "none"
                , HA.style "color" "#e3e1dc"
                , HA.style "cursor" "pointer"
                , HA.style "font-size" "18px"
                , HA.style "padding" "4px"
                , HA.attribute "aria-label" "Cerrar pistas"
                ]
                [ Html.text "›" ]

          else
            -- Collapsed: purely decorative - the whole rail (an ancestor)
            -- already expands on click, so this must NOT have its own
            -- click handler or the click would bubble and toggle twice.
            Html.span
                [ HA.style "color" "#e3e1dc"
                , HA.style "font-size" "18px"
                , HA.style "padding" "4px"
                ]
                [ Html.text "‹" ]
        ]


countBadge : String -> Int -> String -> Html Msg
countBadge icon count color =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "gap" "4px"
        , HA.style "color" color
        , HA.style "font-weight" "bold"
        , HA.style "font-size" "14px"
        ]
        [ Html.span [] [ Html.text icon ]
        , Html.span [] [ Html.text (String.fromInt count) ]
        ]


cluesList : Board -> Set Int -> Html Msg
cluesList board discoveredAxioms =
    let
        discovered =
            Puzzle.axioms
                |> List.indexedMap Tuple.pair
                |> List.filter (\( index, _ ) -> Set.member index discoveredAxioms)
    in
    Html.div
        [ HA.style "padding" "0 12px 12px 12px"
        , HA.style "overflow-y" "auto"
        ]
        [ if List.isEmpty discovered then
            Html.p
                [ HA.style "color" "#9aa0a6" ]
                [ Html.text "Las pistas se revelan cuando algo no cuadra. Prueba a sentar a alguien." ]

          else
            Html.ul
                [ HA.style "list-style" "none"
                , HA.style "padding" "0"
                ]
                (List.map (\( _, axiom ) -> clueItem board axiom) discovered)
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
                    ( "•", "#9aa0a6" )
    in
    Html.li
        [ HA.style "color" color
        , HA.style "margin-bottom" "6px"
        ]
        [ Html.text (icon ++ " " ++ Axioms.clueText studentOrPlaceholder axiom) ]
