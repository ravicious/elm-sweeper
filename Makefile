default: dev

# https://github.com/truqu/real-world-elm/blob/2f6f083c631f4461a5b782d51822ae20450d6d2e/elm/Makefile
ELM_FILES = $(shell find . -path ./elm-stuff -prune -o -type f -name '*.elm')

.PHONY: clean clean-deps watch dev release

main.js: $(ELM_FILES)
	elm-make --yes --warn $(ELM_MAKE_FLAGS) src/Main.elm --output $@

dev : ELM_MAKE_FLAGS = --debug
dev: main.js

clean-deps:
	rm -rf elm-stuff

clean:
	rm -f *.js
	rm -rf elm-stuff/build-artifacts

watch:
	-@make
	@echo Waiting for changes…
	@fswatch -o -0 src | xargs -0 -n1 -I{} make

main.min.js : ELM_MAKE_FLAGS =
main.min.js: main.js
	-closure-compiler --js $< --js_output_file $@ --compilation_level ADVANCED_OPTIMIZATIONS

release: main.min.js
