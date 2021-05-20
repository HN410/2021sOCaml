multiset2: orderedType.cmo orderedString.cmo multiset2.cmo 
	ocamlc -o orderedString.cmo multiset2.cmo 

orderedType.cmo: orderedType.ml
	ocamlc -c orderedType.ml

orderedString.cmo: orderedString.ml
	ocamlc -c orderedString.ml

multiset2.cmo: multiset2.ml 
	ocamlc -c multiset2.ml

clean: 
	rm stack.cmi stack.cmo 
