.PHONY: build system


# Variables

NODE_BIN=./node_modules/.bin
SRC_DIR=./src
BUILD_DIR=./build


# Default task

all: dev


#
# Build tasks
#

build: clean vendor system elm
	@echo "> Build completed ⚡"


clean:
	@echo "> Cleaning Build Directory"
	@rm -rf $(BUILD_DIR)


elm:
	@echo "> Compiling Elm"
	@elm-make $(SRC_DIR)/App/App.elm --output $(BUILD_DIR)/application.js --yes
	@elm-make $(SRC_DIR)/Slave/Slave.elm --output $(BUILD_DIR)/slave.js --yes


system:
	@echo "> Compiling System"
	@stack build && stack exec build


vendor:
	@echo "> Browserify vendor dependencies"
	@stack build && stack exec vendor


#
# Dev tasks
#

dev: build
	@make -j watch_wo_build server


server:
	@echo "> Booting up web server"
	@stack build && stack exec server


test:
	@echo "> Run tests"
	@$(NODE_BIN)/elm-doctest \
		src/App/Sources/Crypto/Hex.elm \
		src/App/Sources/Crypto/Hmac.elm


watch: build
	@make watch_wo_build


watch_wo_build:
	@echo "> Watching"
	@make -j watch_elm watch_system


watch_elm:
	@watchexec -p \
		-w $(SRC_DIR)/App \
		-w $(SRC_DIR)/Ext \
		-w $(SRC_DIR)/Slave \
		-w $(SRC_DIR)/Styles \
		-- make elm


watch_system:
	@watchexec -p --ignore *.elm -- make system
