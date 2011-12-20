Merge: 0 to 1
(F11 -> "gray" ("forward" F10))
(F12 -> "red" ("forward" F11))
(F14 -> "gray" ("forward" F10))
(F15 -> "red" ("forward" F14))
===>
(F20 -> "gray" ("forward" F10))
(F12 -> "red" ("forward" F20))
(F15 -> "red" ("forward" F20))
d-posterior: 6.0 d-likelihood: 0.0 d-prior: 6.0 d-dl: -6 
Merge: 1 to 2
(F12 -> "red" ("forward" F20))
(F15 -> "red" ("forward" F20))
(F16 -> "gray" ("l-forward" F12) ("r-forward" F15))
===>
(F21 -> "red" ("forward" F20))
(F16 -> "gray" ("l-forward" F21) ("r-forward" F21))
d-posterior: 6.0 d-likelihood: 0.0 d-prior: 6.0 d-dl: -6 
Merge: 2 to 3
(F4 -> "gray" ("forward" F0))
(F5 -> "blue" ("forward" F4))
(F7 -> "gray" ("forward" F0))
(F8 -> "blue" ("forward" F7))
===>
(F22 -> "gray" ("forward" F0))
(F5 -> "blue" ("forward" F22))
(F8 -> "blue" ("forward" F22))
d-posterior: 6.0 d-likelihood: 0.0 d-prior: 6.0 d-dl: -6 
Merge: 3 to 4
(F5 -> "blue" ("forward" F22))
(F8 -> "blue" ("forward" F22))
(F9 -> "gray" ("l-forward" F5) ("r-forward" F8))
===>
(F23 -> "blue" ("forward" F22))
(F9 -> "gray" ("l-forward" F23) ("r-forward" F23))
d-posterior: 6.0 d-likelihood: 0.0 d-prior: 6.0 d-dl: -6 
Merge: 4 to 5
(F16 -> "gray" ("l-forward" F21) ("r-forward" F21))
(F19 -> "gray" ("l-forward" F10) ("r-forward" F10))
(TopLevel -> F19 / F16 / F9 / F2)
(abstraction TopLevel () (choose (F2) (F9) (F16) (F19)))
===>
(F24 -> "gray" ("l-forward" F10) ("r-forward" F10) / "gray"
  ("l-forward" F21) ("r-forward" F21))
(TopLevel -> F24 / F9 / F2)
(abstraction TopLevel () (choose (F2) (F9) (F24)))
d-posterior: 0.9013877113318784 d-likelihood: 0.0 d-prior: 0.9013877113318927 d-dl: -2 
Merge: 5 to 6
(F24 -> "gray" ("l-forward" F10) ("r-forward" F10) / "gray"
  ("l-forward" F21) ("r-forward" F21))
(F21 -> "red" ("forward" F20))
(F20 -> "gray" ("forward" F10))
(F10 -> "red")
===>
(F25 -> "red" ("forward" F20) / "red")
(F24 -> "gray" ("l-forward" F25) ("r-forward" F25))
(F20 -> "gray" ("forward" F25))
d-posterior: 6.567209351351039 d-likelihood: -2.4327906486489868 d-prior: 9.0 d-dl: -9 
Merge: 6 to 7
(F2 -> "gray" ("l-forward" F0) ("r-forward" F0))
(F9 -> "gray" ("l-forward" F23) ("r-forward" F23))
(TopLevel -> F24 / F9 / F2)
(abstraction TopLevel () (choose (F2) (F9) (F24)))
===>
(F26 -> "gray" ("l-forward" F23) ("r-forward" F23) / "gray"
  ("l-forward" F0) ("r-forward" F0))
(TopLevel -> F24 / F26)
(abstraction TopLevel () (choose (F26) (F24)))
d-posterior: 1.3068528194400386 d-likelihood: 0.0 d-prior: 1.3068528194400528 d-dl: -2 
Merge: 7 to 8
(F26 -> "gray" ("l-forward" F23) ("r-forward" F23) / "gray"
  ("l-forward" F0) ("r-forward" F0))
(F23 -> "blue" ("forward" F22))
(F22 -> "gray" ("forward" F0))
(F0 -> "blue")
===>
(F27 -> "blue" ("forward" F22) / "blue")
(F26 -> "gray" ("l-forward" F27) ("r-forward" F27))
(F22 -> "gray" ("forward" F27))
d-posterior: 6.567209351351018 d-likelihood: -2.432790648648986 d-prior: 9.0 d-dl: -9 
Merge: 8 to 9
(F26 -> "gray" ("l-forward" F27) ("r-forward" F27))
(F24 -> "gray" ("l-forward" F25) ("r-forward" F25))
(TopLevel -> F24 / F26)
(abstraction TopLevel () (choose (F26) (F24)))
===>
(F28 -> "gray" ("l-forward" F27) ("r-forward" F27) / "gray"
  ("l-forward" F25) ("r-forward" F25))
(TopLevel -> . F28)
(abstraction TopLevel () (choose (F28)))
d-posterior: 2.0 d-likelihood: 0.0 d-prior: 2.0 d-dl: -2 
