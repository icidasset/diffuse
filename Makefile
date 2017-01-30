.PHONY: build system


# variables
NODE_BIN=./node_modules/.bin

SRC_DIR=./src
BUILD_DIR=./build


# tasks
all: build


build: clean system elm css
	@echo "> Done ⚡ \n"


clean:
	@echo "> Cleaning Build Directory"
	@rm -rf $(BUILD_DIR)



css:
	@echo "> Compiling CSS"
	@$(NODE_BIN)/postcss \
		-u postcss-import \
		-u postcss-mixins \
		-u postcss-custom-units \
		-u postcss-remify --postcss-remify.base=16 \
		-u postcss-simple-vars \
		-u postcss-cssnext --no-postcss-cssnext.features.rem \
		-o $(BUILD_DIR)/application.css \
		$(SRC_DIR)/Css/index.css


elm:
	elm-make $(SRC_DIR)/App/App.elm --output $(BUILD_DIR)/application.js --yes



server:
	@echo "> Booting up web server"
	@stack build && stack exec server


system:
	@echo "> Compiling System"
	@stack build && stack exec build


watch: build
	@echo "> Watching"
	@hobbes "$(SRC_DIR)/**.*" | xargs -n1 sh -c "make build"
