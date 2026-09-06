module Puzzle exposing (axioms, students)

import Types exposing (Axiom, Relation(..), Student)


{-| Student ids, fixed for this puzzle instance:
0 Mateo, 1 Lucía, 2 Nicolás, 3 Sofía, 4 Bruno, 5 Martina, 6 Valentina, 7 Emma

The unique solution (verified offline by brute force over all 8! = 40320
seatings) is:

    Front row: Mateo, Lucía, Nicolás, Sofía
    Back row:  Bruno, Martina, Valentina, Emma

-}
students : List Student
students =
    [ { id = 0, name = "Mateo", color = "#e07a5f" }
    , { id = 1, name = "Lucía", color = "#3d405b" }
    , { id = 2, name = "Nicolás", color = "#81b29a" }
    , { id = 3, name = "Sofía", color = "#f2cc8f" }
    , { id = 4, name = "Bruno", color = "#9d4edd" }
    , { id = 5, name = "Martina", color = "#118ab2" }
    , { id = 6, name = "Valentina", color = "#ef476f" }
    , { id = 7, name = "Emma", color = "#06d6a0" }
    ]


mateo : Int
mateo =
    0


lucia : Int
lucia =
    1


nicolas : Int
nicolas =
    2


sofia : Int
sofia =
    3


bruno : Int
bruno =
    4


martina : Int
martina =
    5


valentina : Int
valentina =
    6


emma : Int
emma =
    7


axioms : List Axiom
axioms =
    [ Axiom (LeftOf mateo lucia) False
    , Axiom (LeftOf lucia nicolas) False
    , Axiom (LeftOf nicolas sofia) False
    , Axiom (Behind bruno mateo) False
    , Axiom (Behind valentina nicolas) False
    , Axiom (Behind emma sofia) False
    , Axiom (SameBench lucia nicolas) True
    , Axiom (Behind martina sofia) True
    , Axiom (FrontRow sofia) False
    , Axiom (NextTo mateo sofia) True
    ]
