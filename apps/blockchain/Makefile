all:
	ocamlfind ocamlopt utils.ml rbset.ml blockchain.ml -o blockchain  -package uuidm,digestif.ocaml,jsonm,irmin-unix,lwt.unix -linkpkg -thread

test: msigs.ml utils.ml rbmap.ml tpcc.ml db.ml test_monkey.ml
	ocamlfind ocamlopt -g msigs.ml utils.ml rbmap.ml tpcc.ml db.ml test_monkey.ml -o test -package jsonm,irmin-unix,lwt.unix -linkpkg -thread

clean:
	rm *.cmi *.cmo *.o *.cmx blockchain
