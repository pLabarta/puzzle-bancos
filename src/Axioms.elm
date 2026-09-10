module Axioms exposing (axiomStatus, clueText)

import Dict
import Types
    exposing
        ( Axiom
        , AxiomStatus(..)
        , Board
        , Handedness(..)
        , Relation(..)
        , Seat
        , Student
        , StudentId
        )


seatOfStudent : Board -> StudentId -> Maybe Seat
seatOfStudent board studentId =
    board.seats
        |> Dict.toList
        |> List.filter (\( _, occupant ) -> occupant == studentId)
        |> List.head
        |> Maybe.map (\( ( row, col ), _ ) -> { row = row, col = col })


relationValue : Board -> Relation -> Maybe Bool
relationValue board relation =
    case relation of
        NextTo a b ->
            Maybe.map2 nextTo (seatOfStudent board a) (seatOfStudent board b)

        LeftOf a b ->
            Maybe.map2 leftOf (seatOfStudent board a) (seatOfStudent board b)

        SameBench a b ->
            Maybe.map2 sameBench (seatOfStudent board a) (seatOfStudent board b)

        Behind a b ->
            Maybe.map2 behind (seatOfStudent board a) (seatOfStudent board b)

        InRow a row ->
            seatOfStudent board a |> Maybe.map (\seat -> seat.row == row)


sameRow : Seat -> Seat -> Bool
sameRow s1 s2 =
    s1.row == s2.row


nextTo : Seat -> Seat -> Bool
nextTo s1 s2 =
    sameRow s1 s2 && abs (s1.col - s2.col) == 1


sameBench : Seat -> Seat -> Bool
sameBench s1 s2 =
    sameRow s1 s2 && (s1.col - 1) // 2 == (s2.col - 1) // 2


{-| s1 sits immediately to the left of s2, on the same two-seat bench. Unlike
`nextTo`, this does NOT hold across the aisle between benches (e.g. the last
seat of one bench and the first seat of the next) - "immediately to the
left" is meant as desk-mates, not just adjacent columns.
-}
leftOf : Seat -> Seat -> Bool
leftOf s1 s2 =
    sameBench s1 s2 && s2.col - s1.col == 1


{-| a sits directly behind b: one row further back, same column.
-}
behind : Seat -> Seat -> Bool
behind s1 s2 =
    s1.row == s2.row + 1 && s1.col == s2.col


{-| Evaluate an axiom against the current board. Pending means one or both
students involved aren't seated yet, so the axiom can't be checked.
-}
axiomStatus : Board -> Axiom -> AxiomStatus
axiomStatus board axiom =
    case relationValue board axiom.relation of
        Nothing ->
            Pending

        Just value ->
            if value /= axiom.negated then
                Satisfied

            else
                Violated


rowPhrase : Int -> String
rowPhrase row =
    case row of
        1 ->
            "adelante"

        2 ->
            "atrás"

        _ ->
            "en la fila " ++ String.fromInt row


{-| True when seating `a` immediately to the left of `b` would make their
writing arms collide: `a` writes with the hand facing `b` (right-handed,
since `a` is on the left) and `b` also writes with the hand facing `a`
(left-handed, since `b` is on the right).
-}
elbowsWouldClash : Student -> Student -> Bool
elbowsWouldClash a b =
    a.handedness == RightHanded && b.handedness == LeftHanded


clueText : (StudentId -> Student) -> Axiom -> String
clueText lookupStudent axiom =
    let
        student =
            lookupStudent

        name id =
            (student id).name
    in
    case axiom.relation of
        NextTo a b ->
            if axiom.negated then
                name a ++ " no se sienta al lado de " ++ name b ++ "."

            else
                name a ++ " y " ++ name b ++ " se sientan juntos."

        LeftOf a b ->
            if axiom.negated then
                if elbowsWouldClash (student a) (student b) then
                    name a
                        ++ " no se sienta inmediatamente a la izquierda de "
                        ++ name b
                        ++ ": "
                        ++ name a
                        ++ " escribe con la derecha y "
                        ++ name b
                        ++ " con la izquierda, y chocarían los codos."

                else
                    name a ++ " no se sienta inmediatamente a la izquierda de " ++ name b ++ "."

            else
                name a ++ " se sienta inmediatamente a la izquierda de " ++ name b ++ "."

        SameBench a b ->
            if axiom.negated then
                name a ++ " y " ++ name b ++ " no comparten banco."

            else
                name a ++ " y " ++ name b ++ " comparten banco."

        Behind a b ->
            if axiom.negated then
                name a ++ " no se sienta justo detrás de " ++ name b ++ "."

            else
                name a ++ " se sienta justo detrás de " ++ name b ++ "."

        InRow a row ->
            if axiom.negated then
                name a ++ " no se sienta " ++ rowPhrase row ++ "."

            else
                name a ++ " se sienta " ++ rowPhrase row ++ "."
