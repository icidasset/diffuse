.PHONY: build electron system


# Variables

NODE_BIN=./node_modules/.bin
SRC_DIR=./src
BUILD_DIR=./build
DIST_DIR=./dist


# Default task

all: dev


#
# Build tasks
#

build: clean elm system vendor electron
	@echo "> Build completed ⚡"


clean:
	@echo "> Cleaning Build Directory"
	@rm -rf $(BUILD_DIR) || true


clean-dist:
	@echo "> Cleaning Dist Directory"
	@rm -rf $(DIST_DIR) || true


electron:
	@echo "> Copying Electron Script"
	@cp -r ./electron $(BUILD_DIR)
	@cp ./package.json $(BUILD_DIR)/package.json
	@echo "> Creating icons"
	@mkdir -p $(BUILD_DIR)/resources
	@convert $(SRC_DIR)/Static/Images/icon.png -background transparent -gravity center -extent 1152x1152 $(BUILD_DIR)/resources/icon.png
	@makeicns -in $(BUILD_DIR)/resources/icon.png -out $(BUILD_DIR)/resources/icon.icns 2>/dev/null
	@convert $(BUILD_DIR)/resources/icon.png -define icon:auto-resize=256 $(BUILD_DIR)/resources/icon.ico


electron-dist: build clean-dist
	@$(NODE_BIN)/electron-builder build --config=electron/builder.yaml --mac --linux --win


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
	@make -j watch-wo-build server


electron-dev: build
	@make -j watch-wo-build electron-dev-server


electron-dev-server:
	@ENV=DEV $(NODE_BIN)/electron $(BUILD_DIR)/electron/index.js


server:
	@echo "> Booting up web server on port 5000"
	@stack build && stack exec server


test:
	@echo "> Run tests"
	@$(NODE_BIN)/elm-doctest \
		src/App/Sources/Crypto/Hex.elm \
		src/App/Sources/Crypto/Hmac.elm


watch: build
	@make watch_wo_build


watch-wo-build:
	@echo "> Watching"
	@make -j watch-elm watch-system


watch-elm:
	@watchexec -p \
		-w $(SRC_DIR)/App \
		-w $(SRC_DIR)/Lib \
		-w $(SRC_DIR)/Slave \
		-w $(SRC_DIR)/Styles \
		-- make elm


watch-system:
	@watchexec -p --ignore *.elm -- make system
