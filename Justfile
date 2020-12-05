export NODE_NO_WARNINGS := "1"


BUILD_DIR 			:= "./build"
NPM_DIR 				:= "./node_modules"
SRC_DIR 				:= "./src"
SYSTEM_DIR 			:= "./system"
TEMPORARY_DIR 	:= "./elm-stuff/elm-tailwind-css"

ETC_CMD					:= "pnpx etc"


default: dev


# Tasks
# =====

@build: clean system css elm js
	echo "> Build completed ⚡"


@build-prod: quality clean system css elm-prod css-prod js-prod
	echo "> Production build completed 🛳"


@clean:
	echo "> Cleaning build directory"
	rm -rf {{BUILD_DIR}} || true
	mkdir -p {{BUILD_DIR}}


@css:
	echo "> Compiling CSS"

	{{ETC_CMD}} {{SRC_DIR}}/Css/About.css \
	  --config {{SYSTEM_DIR}}/Css/Tailwind.js \
	  --output {{BUILD_DIR}}/about.css \
		\
		--post-plugin-before postcss-import \
		--post-plugin-after postcss-custom-properties

	{{ETC_CMD}} {{SRC_DIR}}/Css/Application.css \
	  --config {{SYSTEM_DIR}}/Css/Tailwind.js \
		--elm-module Css.Classes \
	  --elm-path {{SRC_DIR}}/Library/Css/Classes.elm \
	  --output {{BUILD_DIR}}/application.css \
		\
		--post-plugin-before postcss-import \
		--post-plugin-after postcss-custom-properties


@css-prod:
	echo "> Optimising CSS"

	NODE_ENV=production {{ETC_CMD}} {{SRC_DIR}}/Css/About.css \
	  --config {{SYSTEM_DIR}}/Css/Tailwind.js \
	  --output {{BUILD_DIR}}/about.css \
		\
		--post-plugin-before postcss-import \
		--post-plugin-after postcss-custom-properties \
		\
		--purge-content {{BUILD_DIR}}/about/**/*.html \
		--purge-whitelist hljs-string \
		--purge-whitelist hljs-comment \
		--purge-whitelist hljs-meta \
		--purge-whitelist bash

	NODE_ENV=production {{ETC_CMD}} {{SRC_DIR}}/Css/Application.css \
	  --config {{SYSTEM_DIR}}/Css/Tailwind.js \
	  --output {{BUILD_DIR}}/application.css \
		\
		--post-plugin-before postcss-import \
		--post-plugin-after postcss-custom-properties \
		\
	  --purge-content {{BUILD_DIR}}/ui.elm.js \
		--purge-content {{BUILD_DIR}}/index.html \
		--purge-whitelist button \
		--purge-whitelist input \
		--purge-whitelist select \
		--purge-whitelist textarea


@elm:
	echo "> Compiling Elm application"
	elm make {{SRC_DIR}}/Applications/Brain.elm --output {{BUILD_DIR}}/brain.elm.js
	elm make {{SRC_DIR}}/Applications/UI.elm --output {{BUILD_DIR}}/ui.elm.js


@elm-prod:
	echo "> Compiling Elm application (optimised)"
	elm make {{SRC_DIR}}/Applications/Brain.elm --output {{BUILD_DIR}}/brain.elm.js --optimize
	elm make {{SRC_DIR}}/Applications/UI.elm --output {{BUILD_DIR}}/ui.elm.js --optimize

	{{NPM_DIR}}/.bin/terser {{BUILD_DIR}}/brain.elm.js \
		--output {{BUILD_DIR}}/brain.elm.tmp.js \
		--compress --mangle

	{{NPM_DIR}}/.bin/terser {{BUILD_DIR}}/ui.elm.js \
		--output {{BUILD_DIR}}/ui.elm.tmp.js \
		--compress --mangle

	rm {{BUILD_DIR}}/brain.elm.js
	mv {{BUILD_DIR}}/brain.elm.tmp.js {{BUILD_DIR}}/brain.elm.js
	rm {{BUILD_DIR}}/ui.elm.js
	mv {{BUILD_DIR}}/ui.elm.tmp.js {{BUILD_DIR}}/ui.elm.js


@js: vendor-js
	echo "> Compiling Javascript code"

	# Main builds
	{{NPM_DIR}}/.bin/webpack-cli \
		--entry ./src/Javascript/index.js \
		--mode none \
		--output {{BUILD_DIR}}/ui.js

	{{NPM_DIR}}/.bin/webpack-cli \
		--entry ./src/Javascript/Brain/index.js \
		--mode none \
		--target webworker \
		--output {{BUILD_DIR}}/brain.js

	# Workers
	{{NPM_DIR}}/.bin/webpack-cli \
		--entry ./src/Javascript/Workers/search.js \
		--mode none \
		--target webworker \
		--output {{BUILD_DIR}}/search.js

	{{NPM_DIR}}/.bin/webpack-cli \
		--entry ./src/Javascript/Workers/service.js \
		--mode none \
		--target webworker \
		--output {{BUILD_DIR}}/service-worker.js


@js-prod: vendor-js
	echo "> Compiling Javascript code (optimised)"

	# Main builds
	{{NPM_DIR}}/.bin/webpack-cli \
		--entry ./src/Javascript/index.js \
		--mode production \
		--output {{BUILD_DIR}}/ui.js

	{{NPM_DIR}}/.bin/webpack-cli \
		--entry ./src/Javascript/Brain/index.js \
		--mode production \
		--target webworker \
		--output {{BUILD_DIR}}/brain.js

	# Workers
	{{NPM_DIR}}/.bin/webpack-cli \
		--entry ./src/Javascript/Workers/search.js \
		--mode production \
		--target webworker \
		--output {{BUILD_DIR}}/search.js

	{{NPM_DIR}}/.bin/webpack-cli \
		--entry ./src/Javascript/Workers/service.js \
		--mode production \
		--target webworker \
		--output {{BUILD_DIR}}/service-worker.js


@system:
	echo "> Compiling system"
	stack build --fast 2>&1 | sed '/^Warning:/,/Invalid magic: e49ceb0f$/d' | sed '/^Inferring license/d' && stack exec build --silent


@vendor-js:
	mkdir -p {{BUILD_DIR}}/vendor
	cp {{NPM_DIR}}/subworkers/subworkers.js {{BUILD_DIR}}/subworkers.js
	cp {{NPM_DIR}}/remotestoragejs/release/remotestorage.js {{BUILD_DIR}}/vendor/remotestorage.min.js
	cp {{NPM_DIR}}/ipfs-message-port-client/dist/index.min.js {{BUILD_DIR}}/vendor/ipfs-message-port-client.min.js
	cp ./vendor/pep.js {{BUILD_DIR}}/vendor/pep.js

	{{NPM_DIR}}/.bin/terser {{NPM_DIR}}/webnative/index.umd.js \
		--output {{BUILD_DIR}}/vendor/webnative.min.js \
		--compress --mangle


#
# Dev tasks
#

@dev: build
	just watch-wo-build & just server


@doc-tests:
	echo "> Running documentation tests"
	( cd src && \
		find . -name "*.elm" -print0 | \
		xargs -0 -n 1 -I % sh -c 'elm-proofread -- % || exit 255; echo "\n\n"' \
	)


@elm-housekeeping: reset-elm-css
	echo "> Running elm-review"
	{{NPM_DIR}}/.bin/elm-review {{SRC_DIR}} --config system/Review --fix-all
	echo "> Running elm-format"
	elm-format {{SRC_DIR}} --yes


@install-deps:
	pnpm install

	mkdir -p vendor
	curl --silent --show-error --fail -o ./vendor/pep.js https://raw.githubusercontent.com/mpizenberg/elm-pep/071616d75ca61e261fdefc7b55bc46c34e44ea22/elm-pep.js


@quality: reset-elm-css
	echo "> Running es-lint"
	{{NPM_DIR}}/.bin/eslint src/Javascript/**
	echo "> Running elm-review"
	{{NPM_DIR}}/.bin/elm-review {{SRC_DIR}} --config system/Review


@reset-elm-css:
	# This removes the generated Elm module for the CSS selectors,
	# and also the `tmp` dir which is related to that.
	rm -rf {{TEMPORARY_DIR}}
	rm -f {{SRC_DIR}}/Library/Css/Classes.elm


@server:
	echo "> Booting up web server on port 5000"
	devd --port 5000 --all --crossdomain --quiet --notfound=301.html {{BUILD_DIR}}


@test: doc-tests


@watch: build watch-wo-build


@watch-wo-build:
	echo "> Watching"
	just watch-css & just watch-elm & just watch-js & just watch-system


@watch-css:
	watchexec -p -w {{SRC_DIR}}/Css -w {{SYSTEM_DIR}}/Css -- just css


@watch-elm:
	watchexec -p -w {{SRC_DIR}} -e elm -- just elm


@watch-js:
	watchexec -p -w {{SRC_DIR}} -e js -- just js


@watch-system:
	watchexec -p --ignore *.elm --ignore *.js --ignore *.css -- just system
