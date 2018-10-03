default: dev

# https://github.com/truqu/real-world-elm/blob/2f6f083c631f4461a5b782d51822ae20450d6d2e/elm/Makefile
ELM_FILES = $(shell find . -path ./elm-stuff -prune -o -type f -name '*.elm')

.PHONY: clean clean-deps server dev release test format-validate check

main.js: $(ELM_FILES)
	yarn elm make src/Main.elm $(ELM_MAKE_FLAGS) --output $@

dev : ELM_MAKE_FLAGS = --debug
dev: main.js

clean-deps:
	rm -rf elm-stuff

clean:
	rm -f main.*js
	rm -rf elm-stuff/build-artifacts

server:
	$(shell yarn bin)/elm-live src/Main.elm --path-to-elm=node_modules/.bin/elm -- --output=main.js --debug

main.min.js : ELM_MAKE_FLAGS = --optimize
main.min.js: main.js
	$(shell yarn bin)/uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | $(shell yarn bin)/uglifyjs --mangle --output=$@

release: clean main.min.js

test:
	yarn elm-test

format-validate:
	yarn elm-format src/ tests/ --validate

check: test format-validate
