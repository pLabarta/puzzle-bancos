module Types exposing
    ( Axiom
    , AxiomStatus(..)
    , Board
    , Handedness(..)
    , Relation(..)
    , Seat
    , Student
    , StudentId
    , allSeats
    , benchOf
    , benchesPerRow
    , colOf
    , columnsPerRow
    , occupantAt
    , rowOf
    , rowsPerBoard
    )

import Dict exposing (Dict)


type alias StudentId =
    Int


type Handedness
    = LeftHanded
    | RightHanded


type alias Student =
    { id : StudentId
    , name : String
    , color : String
    , handedness : Handedness
    }


{-| How many rows of benches the classroom has (1 = front row).
-}
rowsPerBoard : Int
rowsPerBoard =
    2


{-| How many benches sit side by side in each row.
-}
benchesPerRow : Int
benchesPerRow =
    3


{-| How many seat columns a row has (two seats per bench).
-}
columnsPerRow : Int
columnsPerRow =
    benchesPerRow * 2


{-| A physical seat, addressed by row (1..rowsPerBoard, 1 = front) and column
(1..columnsPerRow). Column spans across all benches in a row, so adjacent
columns are "next to" each other even when they belong to different benches.
-}
type alias Seat =
    { row : Int
    , col : Int
    }


allSeats : List Seat
allSeats =
    List.concatMap
        (\r -> List.map (Seat r) (List.range 1 columnsPerRow))
        (List.range 1 rowsPerBoard)


{-| Which bench (0-indexed, within its row) a seat belongs to.
-}
benchOf : Seat -> Int
benchOf seat =
    (seat.col - 1) // 2


rowOf : Seat -> Int
rowOf seat =
    seat.row


colOf : Seat -> Int
colOf seat =
    seat.col


{-| The board: which student occupies each seat, plus the buffer tray for
students not currently seated. Seats are keyed by (row, col).
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
    | InRow StudentId Int


type alias Axiom =
    { relation : Relation
    , negated : Bool
    }


type AxiomStatus
    = Satisfied
    | Violated
    | Pending
