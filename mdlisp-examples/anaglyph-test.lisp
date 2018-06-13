(load "mdlisp-examples/anaglyph.lisp")

(with-anaglyph 0 2
  (box 0 300 0 100 100 100)
  (sphere 250 250 250 100)
  (move 450 250 150)
  (rotate z 90)
  (torus 0 0 0 20 100))
(display)
(save "anaglyph.png")
