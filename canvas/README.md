## DrawSome: A Collaborative Drawing Application ##

An application that lets multiple uses collaboratively edit a drawing
canvas. Represents the canvas as a quaternary tree, where each
sub-tree represents a quadrant. Each leaf node represents a pixel. The
representation optimizes for sparse canvases by not expanding trees
until needed. Thus, an empty canvas is represented by a single tree
node whose pixel value (rgb format) is default (say, white).

An sample collaborative drawing session between Alice and Bob is
presented in `test.ml`.

### To Compile ###

Threads:

    ocamlfind ocamlopt canvas.ml icanvas.ml test.ml -o canvas -package jsonm,irmin-unix,lwt.unix -linkpkg -thread 

Remote actors (Alice and Bob):

    ocamlfind ocamlopt canvas.ml icanvas.ml alice.ml -o alice -package jsonm,irmin-unix,lwt.unix -linkpkg -thread 
    ocamlfind ocamlopt canvas.ml icanvas.ml bob.ml -o bob -package jsonm,irmin-unix,lwt.unix -linkpkg -thread 
  
