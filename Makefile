.PHONY: build system

#
# Variables
#
NODE_BIN=./node_modules/.bin
SRC_DIR=./src
BUILD_DIR=./build


#
# Tasks
#
all: build


build: clean system elm css
	@echo "> Done ⚡"


clean:
	@echo "> Cleaning Build Directory"
	@rm -rf $(BUILD_DIR)


css:
	@echo "> Compiling Css"
	@$(NODE_BIN)/elm-css ./system/Stylesheets.elm --output $(BUILD_DIR)


elm:
	@echo "> Compiling Elm"
	@elm-make $(SRC_DIR)/App/App.elm --output $(BUILD_DIR)/application.js --yes --debug


server:
	@echo "> Booting up web server"
	@stack build && stack exec server


system:
	@echo "> Compiling System"
	@stack build && stack exec build


test:
	@echo "> Run tests"
	@$(NODE_BIN)/elm-doc-test && $(NODE_BIN)/elm-test tests/Doc/Main.elm


#
# Watch tasks
#
watch: build
	@echo "> Watching"
	@make -j watch_elm watch_css watch_system


watch_elm:
	@watchexec -p -w $(SRC_DIR)/App --filter *.elm -- make elm


watch_css:
	@watchexec -p -w $(SRC_DIR)/Css --filter *.elm -- make elm css


watch_system:
	@watchexec -p --ignore *.elm -- make system
