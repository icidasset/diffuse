export NODE_NO_WARNINGS := "1"


BUILD_DIR 				:= "./build"
NPM_DIR 					:= "./node_modules"
SRC_DIR 					:= "./src"
SYSTEM_DIR				:= "./system"

ESBUILD						:= NPM_DIR + "/.bin/esbuild --target=es2018 --bundle"


default: dev


# Tasks
# =====

@build: clean css elm js system license
	echo "> Build completed ⚡"


@build-prod: quality clean (css "minify") elm-prod js-prod system license
	echo "> Production build completed 🛳"


check-versions:
	#!/usr/bin/env node
	console.log("> Checking version numbers 🧮")
	const pwd = "{{invocation_directory()}}"

	const package = require(`${pwd}/package.json`)
	const manifest = require(`${pwd}/src/Static/Manifests/manifest.json`)

	if (package.version !== manifest.version) {
		console.error(`The version from package.json doesn't match the one from the app manifest. The package version is '${package.version}' and the manifest version is '${manifest.version}'.`)
		process.exit(1)
	}


@clean:
	echo "> Cleaning build directory"
	rm -rf {{BUILD_DIR}} || true
	mkdir -p {{BUILD_DIR}}


@css minify="false":
	echo "{{ if minify == "minify" { "> Compiling CSS (optimised)" } else { "> Compiling CSS" } }}"

	{{NPM_DIR}}/.bin/tailwind \
		--input {{SRC_DIR}}/Css/About.css \
		--output {{BUILD_DIR}}/about.css \
		--content {{SRC_DIR}}/Static/About/**/*.* \
		--config {{SYSTEM_DIR}}/Css/Tailwind.js \
		--postcss {{SYSTEM_DIR}}/Css/PostCSS.js \
		--jit \
		{{ if minify == "minify" { "--minify" } else { "" } }}

	{{NPM_DIR}}/.bin/tailwind \
		--input {{SRC_DIR}}/Css/Application.css \
		--output {{BUILD_DIR}}/application.css \
		--content "{{SRC_DIR}}/Static/Html/**/*.*,{{SRC_DIR}}/Applications/UI/**/*.elm,{{SRC_DIR}}/Applications/UI.elm,{{SRC_DIR}}/Library/**/*.elm,{{SRC_DIR}}/Javascript/**/*.ts" \
		--config {{SYSTEM_DIR}}/Css/Tailwind.js \
		--postcss {{SYSTEM_DIR}}/Css/PostCSS.js \
		--jit \
		{{ if minify == "minify" { "--minify" } else { "" } }}


@elm:
	echo "> Compiling Elm application"
	elm make {{SRC_DIR}}/Applications/Brain.elm --output {{BUILD_DIR}}/js/brain.elm.js
	elm make {{SRC_DIR}}/Applications/UI.elm --output {{BUILD_DIR}}/js/ui.elm.js


@elm-prod:
	echo "> Compiling Elm application (optimised)"
	elm make {{SRC_DIR}}/Applications/Brain.elm --output {{BUILD_DIR}}/js/brain.elm.js --optimize
	elm make {{SRC_DIR}}/Applications/UI.elm --output {{BUILD_DIR}}/js/ui.elm.js --optimize

	{{NPM_DIR}}/.bin/esbuild {{BUILD_DIR}}/js/brain.elm.js \
		--minify --outfile={{BUILD_DIR}}/js/brain.elm.tmp.js

	{{NPM_DIR}}/.bin/esbuild {{BUILD_DIR}}/js/ui.elm.js \
		--minify --outfile={{BUILD_DIR}}/js/ui.elm.tmp.js

	rm {{BUILD_DIR}}/js/brain.elm.js
	mv {{BUILD_DIR}}/js/brain.elm.tmp.js {{BUILD_DIR}}/js/brain.elm.js
	rm {{BUILD_DIR}}/js/ui.elm.js
	mv {{BUILD_DIR}}/js/ui.elm.tmp.js {{BUILD_DIR}}/js/ui.elm.js


js:
	#!/usr/bin/env bash
	build_timestamp="`date '+%s'`"
	echo "> Compiling Javascript code"

	# Workers
	{{ESBUILD}} ./src/Javascript/Workers/search.ts \
		--outfile={{BUILD_DIR}}/search.js \
		--format=esm \
		--target=esnext

	{{ESBUILD}} ./src/Javascript/Workers/service.ts \
		--outfile={{BUILD_DIR}}/service-worker.js \
		--define:BUILD_TIMESTAMP=$build_timestamp

	{{ESBUILD}} ./src/Javascript/Brain/index.ts \
		--inject:./system/Js/node-shims.js \
		--outdir={{BUILD_DIR}}/js/brain/ \
		--format=esm \
		--target=esnext \
		--splitting \
		--alias:brain.elm.js={{BUILD_DIR}}/js/brain.elm.js \
		--alias:node:buffer=buffer/ \
		--alias:node:stream=readable-stream

	# Main
	{{ESBUILD}} ./src/Javascript/index.ts \
		--outdir={{BUILD_DIR}}/js/ui/ \
		--define:BUILD_TIMESTAMP=$build_timestamp \
		--format=esm \
		--target=esnext \
		--splitting \
		--alias:stream=readable-stream


js-prod:
	#!/usr/bin/env bash
	build_timestamp="`date '+%s'`"
	echo "> Compiling Javascript code (optimised)"

	# Workers
	{{ESBUILD}} ./src/Javascript/Workers/search.ts \
		--minify \
		--outfile={{BUILD_DIR}}/search.js \
		--format=esm \
		--target=esnext

	{{ESBUILD}} ./src/Javascript/Workers/service.ts \
		--minify \
		--outfile={{BUILD_DIR}}/service-worker.js \
		--define:BUILD_TIMESTAMP=$build_timestamp

	{{ESBUILD}} ./src/Javascript/Brain/index.ts \
		--inject:./system/Js/node-shims.js \
		--outdir={{BUILD_DIR}}/js/brain/ \
		--format=esm \
		--target=esnext \
		--splitting \
		--minify \
		--alias:brain.elm.js={{BUILD_DIR}}/js/brain.elm.js

	# Main
	{{ESBUILD}} ./src/Javascript/index.ts \
		--outdir={{BUILD_DIR}}/js/ui/ \
		--define:BUILD_TIMESTAMP=$build_timestamp \
		--format=esm \
		--target=esnext \
		--splitting \
		--minify


@license:
	echo "> Copying license file"
	cp LICENSE {{BUILD_DIR}}/LICENSE


@system:
	echo "> Compiling system"
	stack build --fast 2>&1 | sed '/^Warning:/,/Invalid magic: e49ceb0f$/d' | sed '/^Inferring license/d' && stack exec build --silent


@tauri-build:
	echo "> Building Tauri binaries"
	./src-tauri/bin/cargo-tauri tauri build --config ./src-tauri/tauri.conf.json


@tauri-build-universal:
	echo "> Building Tauri binaries (Universal MacOS build)"
	rustup target add aarch64-apple-darwin
	./src-tauri/bin/cargo-tauri tauri build --target universal-apple-darwin --config ./src-tauri/tauri.conf.json


#
# Dev tasks
#

@dev: build
	just watch & just server


@doc-tests:
	echo "> Running documentation tests"
	( cd src && \
		find . -name "*.elm" -print0 | \
		xargs -0 -n 1 -I % sh -c 'elm-proofread -- % || exit 255; echo "\n\n"' \
	)


@elm-housekeeping:
	echo "> Running elm-review"
	{{NPM_DIR}}/.bin/elm-review {{SRC_DIR}} --config system/Review --fix-all
	echo "> Running elm-format"
	elm-format {{SRC_DIR}} --yes


@install-deps:
	npm install


@install-tauri-cli:
	cargo install tauri-cli --version "^1.2.2" --root ./src-tauri


@quality: check-versions
	echo "> Running es-lint"
	{{NPM_DIR}}/.bin/eslint src/Javascript/**/*
	echo "> Running elm-review"
	{{NPM_DIR}}/.bin/elm-review {{SRC_DIR}} --config system/Review


@server:
	echo "> Booting up web server on port 8000"
	miniserve --spa --index index.html --port 8000 {{BUILD_DIR}}


@tauri-dev:
	./src-tauri/bin/cargo-tauri tauri dev


@test: doc-tests


@watch:
	echo "> Watching"
	just watch-css & just watch-elm & just watch-js & just watch-system


@watch-css:
	watchexec -p -w {{SRC_DIR}}/Css -w {{SYSTEM_DIR}}/Css -- just css js


@watch-elm:
	watchexec -p -w {{SRC_DIR}} -e elm -- just elm css


@watch-js:
	watchexec -p -w {{SRC_DIR}} -e js,ts -- just js


@watch-system:
	watchexec -p --ignore *.elm --ignore *.js --ignore *.ts --ignore *.css -- just system js
