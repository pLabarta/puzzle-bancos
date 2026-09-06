module Layout exposing
    ( DropTarget(..)
    , benchRect
    , boardHeight
    , boardWidth
    , bufferRect
    , bufferSlotCenter
    , hitTest
    , seatCenter
    , seatRadius
    , seatTopLeft
    )

import Types exposing (Row(..), Seat)


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
    40


colX : Int -> Float
colX col =
    case col of
        1 ->
            marginX

        2 ->
            colX 1 + seatSize + benchGap

        3 ->
            colX 2 + seatSize + aisleGap

        4 ->
            colX 3 + seatSize + benchGap

        _ ->
            0


rowY : Row -> Float
rowY row =
    case row of
        Front ->
            marginTop

        Back ->
            marginTop + seatSize + rowGap


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
bench index (0 or 1).
-}
benchRect : Row -> Int -> { x : Float, y : Float, width : Float, height : Float }
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
    colX 4 + seatSize + marginX


bufferY : Float
bufferY =
    rowY Back + seatSize + 50


bufferHeight : Float
bufferHeight =
    140


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


slotsPerRow : Int
slotsPerRow =
    4


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
