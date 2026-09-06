module Types exposing
    ( Axiom
    , AxiomStatus(..)
    , Board
    , Relation(..)
    , Row(..)
    , Seat
    , Student
    , StudentId
    , allSeats
    , benchOf
    , colOf
    , occupantAt
    , rowOf
    )

import Dict exposing (Dict)


type alias StudentId =
    Int


type alias Student =
    { id : StudentId
    , name : String
    , color : String
    }


type Row
    = Front
    | Back


{-| A physical seat, addressed by row and column (1..4). Column spans across
both benches in a row, so adjacent columns are "next to" each other even when
they belong to different benches.
-}
type alias Seat =
    { row : Row
    , col : Int
    }


allSeats : List Seat
allSeats =
    List.concatMap
        (\r -> List.map (Seat r) (List.range 1 4))
        [ Front, Back ]


{-| Which bench (0 or 1, within its row) a seat belongs to.
-}
benchOf : Seat -> Int
benchOf seat =
    (seat.col - 1) // 2


{-| Encode row+col as a single Int key, for use as a Dict key.
-}
rowOf : Seat -> Int
rowOf seat =
    case seat.row of
        Front ->
            0

        Back ->
            1


colOf : Seat -> Int
colOf seat =
    seat.col


{-| The board: which student occupies each seat, plus the buffer tray for
students not currently seated. Seats are keyed by (rowIndex, col).
-}
type alias Board =
    { seats : Dict ( Int, Int ) StudentId
    , buffer : List StudentId
    }


seatKey : Seat -> ( Int, Int )
seatKey seat =
    ( rowOf seat, colOf seat )


occupantAt : Board -> Seat -> Maybe StudentId
occupantAt board seat =
    Dict.get (seatKey seat) board.seats


type Relation
    = NextTo StudentId StudentId
    | LeftOf StudentId StudentId
    | SameBench StudentId StudentId
    | Behind StudentId StudentId
    | FrontRow StudentId


type alias Axiom =
    { relation : Relation
    , negated : Bool
    }


type AxiomStatus
    = Satisfied
    | Violated
    | Pending
