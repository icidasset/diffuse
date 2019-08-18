.PHONY: build system vendor


# Variables

NPM_DIR=./node_modules
SRC_DIR=./src
BUILD_DIR=./build
VENDOR_DIR=./vendor


# Default task

all: dev


#
# Build tasks
#

build: clean elm system
	@echo "> Build completed ⚡"


build-prod: clean elm-prod system
	@echo "> Production build completed 🛳"


clean:
	@echo "> Cleaning build directory"
	@rm -rf $(BUILD_DIR) || true


elm:
	@echo "> Compiling Elm application"
	@elm make $(SRC_DIR)/Applications/Brain.elm --output $(BUILD_DIR)/brain.js
	@elm make $(SRC_DIR)/Applications/UI.elm --output $(BUILD_DIR)/application.js


elm-prod:
	@echo "> Compiling Elm application (optimized)"
	@elm make $(SRC_DIR)/Applications/Brain.elm --output $(BUILD_DIR)/brain.js --optimize
	@elm make $(SRC_DIR)/Applications/UI.elm --output $(BUILD_DIR)/application.js --optimize


system:
	@echo "> Compiling system"
	@stack build && stack exec build


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


install:
	@echo "> Downloading dependencies"
	@mkdir -p $(VENDOR_DIR)
	@curl https://unpkg.com/lunr@2.3.6/lunr.js -o $(VENDOR_DIR)/lunr.js
	@curl https://unpkg.com/remotestoragejs@1.2.2/release/remotestorage.js -o $(VENDOR_DIR)/remotestorage.min.js
	@curl https://unpkg.com/fast-text-encoding@1.0.0/text.min.js -o $(VENDOR_DIR)/text-encoding-polyfill.min.js
	@curl https://unpkg.com/tachyons@4.11.1/css/tachyons.min.css -o $(VENDOR_DIR)/tachyons.min.css
	@curl https://unpkg.com/tocca@2.0.4/Tocca.min.js -o $(VENDOR_DIR)/tocca.min.js

	@# Non-NPM dependencies
	@curl https://gist.githubusercontent.com/icidasset/a888e02d7441aeb2af99263a3add0f73/raw/e4ca77c02e91a29e0c3c749d2ba80983a137a7aa/blockstack.min.js -o $(VENDOR_DIR)/blockstack.min.js
	@curl https://raw.githubusercontent.com/icidasset/diffuse-musicmetadata/f3710b047e2622adcf01f8a86cc9dd5b6aeadf3d/dist/musicmetadata.min.js -o $(VENDOR_DIR)/musicmetadata.min.js
	@curl https://raw.githubusercontent.com/mpizenberg/elm-pep/071616d75ca61e261fdefc7b55bc46c34e44ea22/elm-pep.js -o $(VENDOR_DIR)/pep.js
	@curl https://raw.githubusercontent.com/dmihal/Subworkers/6c3a57953615b26cd82fd39894b947f2b954fcfd/subworkers.js -o $(VENDOR_DIR)/subworkers-polyfill.js

	@# Minify non-minified dependencies
	@echo "> Minifying dependencies"
	@closure-compiler --js=$(VENDOR_DIR)/subworkers-polyfill.js --js_output_file=$(VENDOR_DIR)/subworkers-polyfill.min.js
	@closure-compiler --js=$(VENDOR_DIR)/lunr.js --js_output_file=$(VENDOR_DIR)/lunr.min.js
	@closure-compiler --js=$(VENDOR_DIR)/pep.js --js_output_file=$(VENDOR_DIR)/pep.min.js
	@rm $(VENDOR_DIR)/subworkers-polyfill.js
	@rm $(VENDOR_DIR)/lunr.js
	@rm $(VENDOR_DIR)/pep.js


server:
	@echo "> Booting up web server on port 5000"
	@devd --port 5000 --all --crossdomain --quiet --notfound=301.html $(BUILD_DIR)


test:
	@make -j doc-tests


watch: build
	@make watch_wo_build


watch-wo-build:
	@echo "> Watching"
	@make -j watch-elm watch-system


watch-elm:
	@watchexec -p \
		-w $(SRC_DIR)/Applications \
		-w $(SRC_DIR)/Library \
		-- make elm


watch-system:
	@watchexec -p --ignore *.elm -- make system
