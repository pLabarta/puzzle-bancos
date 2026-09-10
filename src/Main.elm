module Main exposing (main)

import Axioms
import Browser
import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Layout
import Puzzle
import Random
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
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


{-| Where a student was before they got picked up, so a cancelled selection
(or a swap) knows where to put them back.
-}
type PreviousLocation
    = SeatLocation Seat
    | BufferLocation


type alias Selection =
    { studentId : StudentId
    , origin : PreviousLocation
    }


type alias Model =
    { board : Board
    , selected : Maybe Selection
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
      , selected = Nothing
      , discoveredAxioms = Set.empty
      , cluesOpen = False
      }
    , Random.generate ShuffledBuffer (shuffle initialBoard.buffer)
    )



-- UPDATE


type Msg
    = ShuffledBuffer (List StudentId)
    | StudentClicked StudentId
    | SeatClicked Seat
    | BufferAreaClicked
    | ToggleClues


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffledBuffer shuffledIds ->
            let
                board =
                    model.board
            in
            ( { model | board = { board | buffer = shuffledIds } }, Cmd.none )

        StudentClicked studentId ->
            ( selectOrSwap studentId model, Cmd.none )

        SeatClicked seat ->
            ( placeSelected (SeatLocation seat) model, Cmd.none )

        BufferAreaClicked ->
            ( placeSelected BufferLocation model, Cmd.none )

        ToggleClues ->
            ( { model | cluesOpen = not model.cluesOpen }, Cmd.none )


{-| Click on a student. With nobody selected, pick them up: remove them from
the board and remember where they were. With someone already selected,
clicking that same student again cancels the selection and returns them
home; clicking a different student swaps the two - the clicked student is
picked up in turn, and the previously-selected student takes their old
spot (seat or waiting zone).
-}
selectOrSwap : StudentId -> Model -> Model
selectOrSwap studentId model =
    case model.selected of
        Nothing ->
            { model
                | board = removeStudent studentId model.board
                , selected = Just { studentId = studentId, origin = previousLocation studentId model.board }
            }

        Just sel ->
            if sel.studentId == studentId then
                { model
                    | board = placeAt sel.origin studentId model.board
                    , selected = Nothing
                }

            else
                let
                    otherOrigin =
                        previousLocation studentId model.board

                    newBoard =
                        model.board
                            |> removeStudent studentId
                            |> placeAt otherOrigin sel.studentId
                in
                { model
                    | board = newBoard
                    , selected = Just { studentId = studentId, origin = otherOrigin }
                    , discoveredAxioms = discoverViolatedAxioms newBoard model.discoveredAxioms
                }


{-| Click on a seat or the waiting zone: if a student is selected, place them
there. An occupied seat's click never reaches here - the student sitting in
it captures the click first (see `StudentClicked`) - so this only ever
fires for an empty destination.
-}
placeSelected : PreviousLocation -> Model -> Model
placeSelected destination model =
    case model.selected of
        Nothing ->
            model

        Just sel ->
            let
                newBoard =
                    placeAt destination sel.studentId model.board
            in
            { model
                | board = newBoard
                , selected = Nothing
                , discoveredAxioms = discoverViolatedAxioms newBoard model.discoveredAxioms
            }


placeAt : PreviousLocation -> StudentId -> Board -> Board
placeAt location studentId board =
    case location of
        SeatLocation seat ->
            { board | seats = Dict.insert (seatKey seat) studentId board.seats }

        BufferLocation ->
            { board | buffer = studentId :: board.buffer }


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


previousLocation : StudentId -> Board -> PreviousLocation
previousLocation studentId board =
    board.seats
        |> Dict.toList
        |> List.filter (\( _, occupant ) -> occupant == studentId)
        |> List.head
        |> Maybe.map (\( key, _ ) -> SeatLocation (seatFromKey key))
        |> Maybe.withDefault BufferLocation



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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

        -- The board is sized in vw and can afford to sit a little under
        -- the drawer's edge (it has its own inner margin there); this just
        -- guards against a sub-pixel overflow causing page-wide scroll.
        , HA.style "overflow-x" "hidden"
        ]
        [ Html.div
            [ HA.style "padding" "24px" ]
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
        [ SA.width (String.fromFloat Layout.boardWidth)
        , SA.height (String.fromFloat Layout.boardHeight)
        , SA.viewBox
            ("0 0 "
                ++ String.fromFloat Layout.boardWidth
                ++ " "
                ++ String.fromFloat Layout.boardHeight
            )
        , SA.style
            ("background:#24262b;border:1px solid #3a3d44;"
                ++ "display:block;margin:0 auto;"
                ++ "width:90vw;max-width:1100px;min-width:280px;height:auto;"
            )
        ]
        (List.concat
            [ [ chalkboardView ]
            , benchesView
            , seatTargetsView model
            , [ selectedZoneView model ]
            , [ bufferZoneView model ]
            , seatedStudentsView model.board
            , bufferedStudentsView model.board
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


{-| One invisible click target per seat, drawn under the seated-students
layer so an occupied seat's click is captured by the student sitting there
instead (see `studentCircle`). Only rendered visible (a dashed highlight)
over empty seats while something is selected, to hint where it can go.
-}
seatTargetsView : Model -> List (Svg Msg)
seatTargetsView model =
    List.map (seatTargetView model) Types.allSeats


seatTargetView : Model -> Seat -> Svg Msg
seatTargetView model seat =
    let
        ( cx, cy ) =
            Layout.seatCenter seat

        occupied =
            Dict.member (seatKey seat) model.board.seats

        highlight =
            model.selected /= Nothing && not occupied
    in
    Svg.circle
        [ SA.cx (String.fromFloat cx)
        , SA.cy (String.fromFloat cy)
        , SA.r (String.fromFloat Layout.seatRadius)
        , SA.fill "transparent"
        , SA.stroke
            (if highlight then
                "#2a9d8f"

             else
                "transparent"
            )
        , SA.strokeWidth "2"
        , SA.strokeDasharray "4,3"
        , SA.style "cursor:pointer;"
        , SE.onClick (SeatClicked seat)
        ]
        []


{-| The single-slot zone between the benches and the waiting zone, holding
whichever student is currently selected. Clicking that student again
(handled by `studentCircle`, same as anywhere else) cancels the selection.
-}
selectedZoneView : Model -> Svg Msg
selectedZoneView model =
    let
        rect =
            Layout.selectedRect

        ( cx, cy ) =
            Layout.selectedSlotCenter
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat rect.x)
            , SA.y (String.fromFloat rect.y)
            , SA.width (String.fromFloat rect.width)
            , SA.height (String.fromFloat rect.height)
            , SA.rx "6"
            , SA.fill "#2a2d33"
            , SA.stroke
                (if model.selected == Nothing then
                    "#53565c"

                 else
                    "#2a9d8f"
                )
            , SA.strokeWidth "2"
            , SA.strokeDasharray "6,4"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (rect.x + 12))
            , SA.y (String.fromFloat (rect.y + 20))
            , SA.fill "#9aa0a6"
            , SA.fontSize "13"
            , SA.style "pointer-events:none;"
            ]
            [ Svg.text "Seleccionado" ]
        , case model.selected of
            Just sel ->
                studentCircle sel.studentId cx cy

            Nothing ->
                Svg.text ""
        ]


bufferZoneView : Model -> Svg Msg
bufferZoneView model =
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat Layout.bufferRect.x)
            , SA.y (String.fromFloat Layout.bufferRect.y)
            , SA.width (String.fromFloat Layout.bufferRect.width)
            , SA.height (String.fromFloat Layout.bufferRect.height)
            , SA.rx "6"
            , SA.fill "#2a2d33"
            , SA.stroke
                (if model.selected == Nothing then
                    "#53565c"

                 else
                    "#2a9d8f"
                )
            , SA.strokeWidth "2"
            , SA.strokeDasharray "6,4"
            , SA.style "cursor:pointer;"
            , SE.onClick BufferAreaClicked
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat (Layout.bufferRect.x + 12))
            , SA.y (String.fromFloat (Layout.bufferRect.y + 20))
            , SA.fill "#9aa0a6"
            , SA.fontSize "13"
            , SA.style "pointer-events:none;"
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


studentCircle : StudentId -> Float -> Float -> Svg Msg
studentCircle studentId cx cy =
    let
        student =
            findStudent studentId

        color =
            student |> Maybe.map .color |> Maybe.withDefault "#999"

        name =
            student |> Maybe.map .name |> Maybe.withDefault "?"
    in
    Svg.g
        [ SE.onClick (StudentClicked studentId)
        , SA.style "cursor:pointer;"
        ]
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
