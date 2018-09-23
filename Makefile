.PHONY: build system


# Variables

SRC_DIR=./src
BUILD_DIR=./build


# Default task

all: dev


#
# Build tasks
#

build: clean elm system vendor
	@echo "> Build completed ⚡"


clean:
	@echo "> Cleaning build directory"
	@rm -rf $(BUILD_DIR) || true


elm:
	@echo "> Compiling Elm application"
	@elm make $(SRC_DIR)/Applications/Brain.elm --output $(BUILD_DIR)/brain.js
	@elm make $(SRC_DIR)/Applications/UI.elm --output $(BUILD_DIR)/application.js


system:
	@echo "> Compiling system"
	@stack build && stack exec build


vendor:
	@echo "> Copying vendor things"
	@stack build && stack exec vendor


#
# Dev tasks
#

dev: build
	@make -j watch-wo-build server


server:
	@echo "> Booting up web server on port 5000"
	@devd --port 5000 --all --crossdomain --quiet $(BUILD_DIR)


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
