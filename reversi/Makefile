export OCAMLMAKEFILE = ./OCamlMakefile

export LIBS=unix

define PROJ_client
	RESULT =reversi
	SOURCES=original/game.mli original/game.ml original/translate.mli original/translate.ml original/ai.mli original/ai.ml color.ml command.ml commandParser.mly commandLexer.mll play.ml main.ml
endef
export PROJ_client
define PROJ_server
	RESULT =reversi-serv
	SOURCES=color.ml command.ml commandParser.mly commandLexer.mll play.ml server.ml
endef
export PROJ_server

ifndef SUBPROJS
  export SUBPROJS = client server
endif

all: byte-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
