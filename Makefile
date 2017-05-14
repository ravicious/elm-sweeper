default: dev

# https://github.com/truqu/real-world-elm/blob/2f6f083c631f4461a5b782d51822ae20450d6d2e/elm/Makefile
ELM_FILES = $(shell find . -path ./elm-stuff -prune -o -type f -name '*.elm')

.PHONY: clean clean-deps server dev release test format-validate check

main.js: $(ELM_FILES)
	yarn elm-make src/Main.elm -- --yes --warn $(ELM_MAKE_FLAGS) --output $@

dev : ELM_MAKE_FLAGS = --debug
dev: main.js

clean-deps:
	rm -rf elm-stuff

clean:
	rm -f *.js
	rm -rf elm-stuff/build-artifacts

server:
	yarn elm-live src/Main.elm -- --path-to-elm-make=node_modules/.bin/elm-make --output=main.js --debug --host=$(shell ipconfig getifaddr en0)

main.min.js : ELM_MAKE_FLAGS =
main.min.js: main.js
	-closure-compiler --js $< --js_output_file $@ --compilation_level SIMPLE_OPTIMIZATIONS 

release: clean main.min.js

test:
	yarn elm-test

format-validate:
	yarn elm-format src/ tests/ -- --validate

check: test format-validate
