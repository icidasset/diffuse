.PHONY: build system vendor


# Variables

NPM_DIR=./node_modules
SRC_DIR=./src
BUILD_DIR=./build


# Default task

all: dev


#
# Build tasks
#

build: clean css elm js system
	@echo "> Build completed ⚡"


build-prod: clean css elm-prod js-prod system
	@echo "> Production build completed 🛳"


clean:
	@echo "> Cleaning build directory"
	@rm -rf $(BUILD_DIR) || true


css:
	@echo "> Copying CSS dependencies"
	@mkdir -p $(BUILD_DIR)/vendor
	@cp $(NPM_DIR)/tachyons/css/tachyons.min.css $(BUILD_DIR)/vendor/tachyons.min.css


elm:
	@echo "> Compiling Elm application"
	@elm make $(SRC_DIR)/Applications/Brain.elm --output $(BUILD_DIR)/brain.elm.js
	@elm make $(SRC_DIR)/Applications/UI.elm --output $(BUILD_DIR)/ui.elm.js


elm-prod:
	@echo "> Compiling Elm application (optimized)"
	@elm make $(SRC_DIR)/Applications/Brain.elm --output $(BUILD_DIR)/brain.elm.js --optimize
	@elm make $(SRC_DIR)/Applications/UI.elm --output $(BUILD_DIR)/ui.elm.js --optimize
	@closure-compiler --js=$(BUILD_DIR)/brain.elm.js --js_output_file=$(BUILD_DIR)/brain.elm.tmp.js
	@closure-compiler --js=$(BUILD_DIR)/ui.elm.js --js_output_file=$(BUILD_DIR)/ui.elm.tmp.js
	@rm $(BUILD_DIR)/brain.elm.js
	@mv $(BUILD_DIR)/brain.elm.tmp.js $(BUILD_DIR)/brain.elm.js
	@rm $(BUILD_DIR)/ui.elm.js
	@mv $(BUILD_DIR)/ui.elm.tmp.js $(BUILD_DIR)/ui.elm.js


js: vendor-js
	@echo "> Compiling Javascript code"

	@# Service worker
	@cp $(SRC_DIR)/Javascript/workers/service.js $(BUILD_DIR)/service-worker.js

	@# Main builds
	@$(NPM_DIR)/.bin/webpack-cli \
		--entry ./src/Javascript/index.js \
		--mode none \
		--output $(BUILD_DIR)/ui.js

	@$(NPM_DIR)/.bin/webpack-cli \
		--entry ./src/Javascript/Brain/index.js \
		--mode none \
		--output $(BUILD_DIR)/brain.js

	@# Additional builds
	@$(NPM_DIR)/.bin/webpack-cli \
		--entry ./src/Javascript/Workers/search.js \
		--mode none \
		--output $(BUILD_DIR)/search.js


js-prod: vendor-js
	@echo "> Compiling Javascript code (optimized)"

	@# Service worker
	@cp $(SRC_DIR)/Javascript/workers/service.js $(BUILD_DIR)/service-worker.js

	@# Main builds
	@$(NPM_DIR)/.bin/webpack-cli \
		--entry ./src/Javascript/index.js \
		--mode production \
		--output $(BUILD_DIR)/ui.js

	@$(NPM_DIR)/.bin/webpack-cli \
		--entry ./src/Javascript/Brain/index.js \
		--mode production \
		--output $(BUILD_DIR)/brain.js

	@# Additional builds
	@$(NPM_DIR)/.bin/webpack-cli \
		--entry ./src/Javascript/Workers/search.js \
		--mode production \
		--output $(BUILD_DIR)/search.js


system:
	@echo "> Compiling system"
	@stack build && stack exec build


vendor-js:
	@mkdir -p $(BUILD_DIR)/vendor
	@cp $(NPM_DIR)/remotestoragejs/release/remotestorage.js $(BUILD_DIR)/vendor/remotestorage.min.js
	@cp $(NPM_DIR)/blockstack/blockstack.min.js $(BUILD_DIR)/vendor/blockstack.min.js
	@cp $(NPM_DIR)/pep/elm-pep.js $(BUILD_DIR)/vendor/pep.js


#
# Dev tasks
#

dev: build
	@make -j watch-wo-build server


doc-tests:
	@echo "> Running documentation tests"
	@( cd src && \
		find . -name "*.elm" -print0 | \
		xargs -0 -n 1 sh -c 'elm-proofread -- $0 || exit 255; echo "\n\n"'
	)


server:
	@echo "> Booting up web server on port 5000"
	@devd --port 5000 --all --crossdomain --quiet --notfound=301.html $(BUILD_DIR)


test:
	@make -j doc-tests


watch: build
	@make watch_wo_build


watch-wo-build:
	@echo "> Watching"
	@make -j watch-elm watch-js watch-system


watch-elm:
	@watchexec -p \
		-w $(SRC_DIR)/Applications \
		-w $(SRC_DIR)/Library \
		-- make elm


watch-js:
	@watchexec -p \
		-w $(SRC_DIR)/Javascript \
		-- make js


watch-system:
	@watchexec -p --ignore *.elm --ignore *.js -- make system
