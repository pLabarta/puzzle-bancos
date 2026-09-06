module Axioms exposing (axiomStatus, clueText)

import Dict
import Types
    exposing
        ( Axiom
        , AxiomStatus(..)
        , Board
        , Relation(..)
        , Row(..)
        , Seat
        , StudentId
        )


seatOfStudent : Board -> StudentId -> Maybe Seat
seatOfStudent board studentId =
    board.seats
        |> Dict.toList
        |> List.filter (\( _, occupant ) -> occupant == studentId)
        |> List.head
        |> Maybe.map
            (\( ( rowIdx, col ), _ ) ->
                { row =
                    if rowIdx == 0 then
                        Front

                    else
                        Back
                , col = col
                }
            )


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

        FrontRow a ->
            seatOfStudent board a |> Maybe.map (\seat -> seat.row == Front)


sameRow : Seat -> Seat -> Bool
sameRow s1 s2 =
    s1.row == s2.row


nextTo : Seat -> Seat -> Bool
nextTo s1 s2 =
    sameRow s1 s2 && abs (s1.col - s2.col) == 1


leftOf : Seat -> Seat -> Bool
leftOf s1 s2 =
    sameRow s1 s2 && s2.col - s1.col == 1


sameBench : Seat -> Seat -> Bool
sameBench s1 s2 =
    sameRow s1 s2 && (s1.col - 1) // 2 == (s2.col - 1) // 2


behind : Seat -> Seat -> Bool
behind s1 s2 =
    s1.row == Back && s2.row == Front && s1.col == s2.col


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


nameOf : (StudentId -> String) -> StudentId -> String
nameOf lookupName id =
    lookupName id


clueText : (StudentId -> String) -> Axiom -> String
clueText lookupName axiom =
    let
        name =
            nameOf lookupName
    in
    case axiom.relation of
        NextTo a b ->
            if axiom.negated then
                name a ++ " no se sienta junto a " ++ name b ++ "."

            else
                name a ++ " y " ++ name b ++ " se sientan juntos."

        LeftOf a b ->
            if axiom.negated then
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

        FrontRow a ->
            if axiom.negated then
                name a ++ " no se sienta en la fila delantera."

            else
                name a ++ " se sienta en la fila delantera."
