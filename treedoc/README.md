## EditAnd: A Collaborative Text Editing Application ##

An application that lets multiple uses collaboratively edit a text
document. Represents the document as a binary tree whose in-order
traversal produces the original text document. The tree data structure
admits operations similar to the TreeDoc CRDT, but without the need
for additional complexity in the form of mini-nodes and dense position
identifiers.

An sample collaborative editing session between Alice and Bob is
presented in `main.ml`.

### To Compile ###

    ocamlfind ocamlopt treedoc.ml itreedoc.ml main.ml -o treedoc -package ezjsonm,irmin.unix,lwt.unix -linkpkg

