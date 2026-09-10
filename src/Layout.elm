module Layout exposing
    ( DropTarget(..)
    , benchRect
    , boardHeight
    , boardWidth
    , bufferRect
    , bufferSlotCenter
    , chalkboardRect
    , hitTest
    , seatCenter
    , seatRadius
    , seatTopLeft
    )

import Types exposing (Seat)


seatSize : Float
seatSize =
    70


seatRadius : Float
seatRadius =
    seatSize / 2 - 4


benchGap : Float
benchGap =
    6


aisleGap : Float
aisleGap =
    40


rowGap : Float
rowGap =
    70


marginX : Float
marginX =
    40


marginTop : Float
marginTop =
    76


chalkboardHeight : Float
chalkboardHeight =
    30


{-| A green chalkboard drawn above the front row, marking which side of the
room is the front.
-}
chalkboardRect : { x : Float, y : Float, width : Float, height : Float }
chalkboardRect =
    { x = colX 1 - 20
    , y = 14
    , width = (colX Types.columnsPerRow + seatSize) - colX 1 + 40
    , height = chalkboardHeight
    }


{-| X position of the given column (1..columnsPerRow). Columns are grouped in
pairs (one bench each), with a small gap within a pair and a wider aisle gap
between benches.
-}
colX : Int -> Float
colX col =
    let
        zeroIndexed =
            col - 1

        benchIndex =
            zeroIndexed // 2

        posInBench =
            modBy 2 zeroIndexed
    in
    marginX
        + toFloat benchIndex
        * (2 * seatSize + benchGap + aisleGap)
        + toFloat posInBench
        * (seatSize + benchGap)


{-| Y position of the given row (1..rowsPerBoard, 1 = front).
-}
rowY : Int -> Float
rowY row =
    marginTop + toFloat (row - 1) * (seatSize + rowGap)


seatTopLeft : Seat -> ( Float, Float )
seatTopLeft seat =
    ( colX seat.col, rowY seat.row )


seatCenter : Seat -> ( Float, Float )
seatCenter seat =
    let
        ( x, y ) =
            seatTopLeft seat
    in
    ( x + seatSize / 2, y + seatSize / 2 )


{-| Bounding rect for the bench holding the two seats in the given row and
bench index (0..benchesPerRow - 1).
-}
benchRect : Int -> Int -> { x : Float, y : Float, width : Float, height : Float }
benchRect row benchIndex =
    let
        firstCol =
            benchIndex * 2 + 1

        pad =
            8

        x =
            colX firstCol - pad

        y =
            rowY row - pad
    in
    { x = x
    , y = y
    , width = seatSize * 2 + benchGap + 2 * pad
    , height = seatSize + 2 * pad
    }


boardWidth : Float
boardWidth =
    colX Types.columnsPerRow + seatSize + marginX


bufferY : Float
bufferY =
    rowY Types.rowsPerBoard + seatSize + 50


bufferHeight : Float
bufferHeight =
    260


bufferRect : { x : Float, y : Float, width : Float, height : Float }
bufferRect =
    { x = marginX / 2
    , y = bufferY
    , width = boardWidth - marginX
    , height = bufferHeight
    }


boardHeight : Float
boardHeight =
    bufferY + bufferHeight + marginTop


{-| Matches columnsPerRow so the buffer holds exactly one row's worth of
students per line, whatever the bench count is.
-}
slotsPerRow : Int
slotsPerRow =
    Types.columnsPerRow


slotSize : Float
slotSize =
    seatSize + 16


bufferSlotCenter : Int -> ( Float, Float )
bufferSlotCenter index =
    let
        row =
            index // slotsPerRow

        col =
            modBy slotsPerRow index

        x =
            bufferRect.x + 24 + toFloat col * slotSize + seatSize / 2

        y =
            bufferRect.y + 24 + toFloat row * slotSize + seatSize / 2
    in
    ( x, y )


type DropTarget
    = OnSeat Seat
    | InBuffer
    | Nowhere


{-| Determine what's under a given point: a seat, the buffer zone, or
nothing.
-}
hitTest : ( Float, Float ) -> DropTarget
hitTest ( px, py ) =
    let
        seatHit =
            Types.allSeats
                |> List.filter
                    (\seat ->
                        let
                            ( x, y ) =
                                seatTopLeft seat
                        in
                        px >= x && px <= x + seatSize && py >= y && py <= y + seatSize
                    )
                |> List.head
    in
    case seatHit of
        Just seat ->
            OnSeat seat

        Nothing ->
            if
                px
                    >= bufferRect.x
                    && px
                    <= bufferRect.x
                    + bufferRect.width
                    && py
                    >= bufferRect.y
                    && py
                    <= bufferRect.y
                    + bufferRect.height
            then
                InBuffer

            else
                Nowhere
