all:
	ocamllex calc.mll     
	ocamlyacc -v parser.mly  
	ocamlc -o main.exe ast.ml typechecker.ml utils.ml parser.mli parser.ml calc.ml environment.ml operations.ml interpreter.ml main.ml

test_token: all
	ocamlc -o token_test.exe ast.ml utils.ml parser.mli parser.ml calc.ml token_test.ml
	./token_test.exe

test_ast: all
	ocamlc -o ast_test.exe ast.ml typechecker.ml utils.ml parser.mli parser.ml calc.ml ast_test.ml
	./ast_test.exe

test_eval: all
	bash ./test.sh

clean:
	rm -f *.cm* *.o *.mli dsl calc.ml parser.ml main *.exe parser.output
