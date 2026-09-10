module Puzzle exposing (axioms, students)

import Types exposing (Axiom, Handedness(..), Relation(..), Student)


{-| 12 students across 2 rows of 6 (3 benches per row). Front row: Mateo,
Lucía, Nicolás, Sofía, Diego, Camila. Back row: Bruno, Martina, Valentina,
Emma, Joaquín, Valeria.

NOTE: a unique solution is NOT a requirement for this puzzle. Chasing full
uniqueness (verified via tools/'s `solve`/`verify`/`author-grid` commands)
made authoring an earlier (18-student) version of this puzzle take much
longer than it needed to - don't repeat that. A clue set just needs to be
internally consistent (satisfiable) and give the player enough to reason
with; multiple valid seatings are fine.

Generated with tools/ (see tools/README.md): `author-row` found each row's
minimal negative-clue ordering, `InRow` facts anchor the front row (the back
row falls out by elimination, since it's the only row left with exactly
enough seats), and `emit-elm` rendered this file from the roster + clue
JSON.

Mateo, Nicolas, Bruno and Valentina are left-handed; everyone else is
right-handed - chosen so that every `b` side of a negated `LeftOf a b` clue
below is left-handed while its `a` side is right-handed. A negated `LeftOf`
clue gets flavor text about elbows bumping (rather than the plain phrasing)
whenever that specific pair would actually clash pens - see
`Axioms.elbowsWouldClash` - so keep new negated `LeftOf` clues paired with a
right-handed `a` / left-handed `b` to get that framing too. Keep this in
mind if `emit-elm` ever regenerates this file wholesale: it only emits the
plain phrasing (handedness isn't part of the roster/clue JSON), so the
flavor text is derived at render time in `Axioms.elm`, not baked in here -
and the handedness assignments below would need to be redone by hand to
match whatever new `LeftOf` clues come out.

-}
students : List Student
students =
    [ { id = 0, name = "Mateo", color = "#cb4d4d", handedness = LeftHanded }
    , { id = 1, name = "Lucia", color = "#cb8c4d", handedness = RightHanded }
    , { id = 2, name = "Nicolas", color = "#cbcb4d", handedness = LeftHanded }
    , { id = 3, name = "Sofia", color = "#8ccb4d", handedness = RightHanded }
    , { id = 4, name = "Diego", color = "#4dcb4d", handedness = RightHanded }
    , { id = 5, name = "Camila", color = "#4dcb8c", handedness = RightHanded }
    , { id = 6, name = "Bruno", color = "#4dcbcb", handedness = LeftHanded }
    , { id = 7, name = "Martina", color = "#4d8ccb", handedness = RightHanded }
    , { id = 8, name = "Valentina", color = "#4d4dcb", handedness = LeftHanded }
    , { id = 9, name = "Emma", color = "#8c4dcb", handedness = RightHanded }
    , { id = 10, name = "Joaquin", color = "#cb4dcb", handedness = RightHanded }
    , { id = 11, name = "Valeria", color = "#cb4d8c", handedness = RightHanded }
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


diego : Int
diego =
    4


camila : Int
camila =
    5


bruno : Int
bruno =
    6


martina : Int
martina =
    7


valentina : Int
valentina =
    8


emma : Int
emma =
    9


joaquin : Int
joaquin =
    10


valeria : Int
valeria =
    11


axioms : List Axiom
axioms =
    [ Axiom (InRow mateo 1) False
    , Axiom (InRow lucia 1) False
    , Axiom (InRow nicolas 1) False
    , Axiom (InRow sofia 1) False
    , Axiom (InRow diego 1) False
    , Axiom (InRow camila 1) False
    , Axiom (NextTo mateo nicolas) True
    , Axiom (NextTo mateo sofia) True
    , Axiom (NextTo mateo diego) True
    , Axiom (NextTo mateo camila) True
    , Axiom (LeftOf lucia mateo) True
    , Axiom (NextTo nicolas diego) True
    , Axiom (NextTo nicolas camila) True
    , Axiom (LeftOf sofia nicolas) True
    , Axiom (NextTo sofia camila) True
    , Axiom (NextTo bruno valentina) True
    , Axiom (NextTo bruno emma) True
    , Axiom (NextTo bruno joaquin) True
    , Axiom (NextTo bruno valeria) True
    , Axiom (LeftOf martina bruno) True
    , Axiom (NextTo valentina joaquin) True
    , Axiom (NextTo valentina valeria) True
    , Axiom (LeftOf emma valentina) True
    , Axiom (NextTo emma valeria) True
    ]
