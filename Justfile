export NODE_NO_WARNINGS := "1"


BUILD_DIR       := "./dist"
NPM_DIR         := "./node_modules"
SRC_DIR         := "./src"
SYSTEM_DIR      := "./system"

ESBUILD         := "node system/Js/esbuild.mjs"
ELM_REVIEW      := NPM_DIR + "/.bin/elm-review " + SRC_DIR + " --config system/Review --compiler " + NPM_DIR + "/.bin/elm --elm-format-path " + NPM_DIR + "/.bin/elm-format"


default: dev


# Tasks
# =====

@build: clean css elm copy-wasm js system license
	echo "> Build completed ⚡"


@build-prod: quality clean (css "minify") elm-prod copy-wasm js-prod system license
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


@copy-wasm:
  echo "> Copying WASM files"
  mkdir -p {{BUILD_DIR}}/wasm
  cp {{NPM_DIR}}/mediainfo.js/dist/MediaInfoModule.wasm {{BUILD_DIR}}/wasm/media-info.wasm


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
		--content "{{SRC_DIR}}/Static/Html/**/*.*,{{SRC_DIR}}/Core/Themes/**/*.elm,{{SRC_DIR}}/Core/UI/**/*.elm,{{SRC_DIR}}/Core/UI.elm,{{SRC_DIR}}/Library/**/*.elm,{{SRC_DIR}}/Javascript/**/*.ts" \
		--config {{SYSTEM_DIR}}/Css/Tailwind.js \
		--postcss {{SYSTEM_DIR}}/Css/PostCSS.js \
		--jit \
		{{ if minify == "minify" { "--minify" } else { "" } }}


@elm:
	echo "> Compiling Elm application"
	{{NPM_DIR}}/.bin/elm make {{SRC_DIR}}/Core/Brain.elm --output {{BUILD_DIR}}/js/brain.elm.js
	{{NPM_DIR}}/.bin/elm make {{SRC_DIR}}/Core/UI.elm --output {{BUILD_DIR}}/js/ui.elm.js


@elm-prod:
	echo "> Compiling Elm application (optimised)"
	{{NPM_DIR}}/.bin/elm make {{SRC_DIR}}/Core/Brain.elm --output {{BUILD_DIR}}/js/brain.elm.js --optimize
	{{NPM_DIR}}/.bin/elm make {{SRC_DIR}}/Core/UI.elm --output {{BUILD_DIR}}/js/ui.elm.js --optimize

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
		--outfile={{BUILD_DIR}}/search.js

	{{ESBUILD}} ./src/Javascript/Workers/service.ts \
		--outfile={{BUILD_DIR}}/service-worker.js \
		--define:BUILD_TIMESTAMP=$build_timestamp

	{{ESBUILD}} ./src/Javascript/Brain/index.ts \
		--outdir={{BUILD_DIR}}/js/brain/ \
		--splitting \
		--alias:brain.elm.js={{BUILD_DIR}}/js/brain.elm.js \
		--inject:./system/Js/node-shims.js \
		--alias:node:buffer=buffer/ \
		--alias:node:stream=stream

	# Main
	{{ESBUILD}} ./src/Javascript/UI/index.ts \
		--outdir={{BUILD_DIR}}/js/ui/ \
		--define:BUILD_TIMESTAMP=$build_timestamp \
		--splitting \
		--alias:node:buffer=buffer/ \
		--alias:node:stream=stream


js-prod:
	#!/usr/bin/env bash
	build_timestamp="`date '+%s'`"
	echo "> Compiling Javascript code (optimised)"

	# Workers
	{{ESBUILD}} ./src/Javascript/Workers/search.ts \
		--minify \
		--outfile={{BUILD_DIR}}/search.js

	{{ESBUILD}} ./src/Javascript/Workers/service.ts \
		--minify \
		--outfile={{BUILD_DIR}}/service-worker.js \
		--define:BUILD_TIMESTAMP=$build_timestamp

	{{ESBUILD}} ./src/Javascript/Brain/index.ts \
		--outdir={{BUILD_DIR}}/js/brain/ \
		--splitting \
		--minify \
		--alias:brain.elm.js={{BUILD_DIR}}/js/brain.elm.js \
		--inject:./system/Js/node-shims.js \
		--alias:node:buffer=buffer/ \
		--alias:node:stream=stream

	# Main
	{{ESBUILD}} ./src/Javascript/UI/index.ts \
		--outdir={{BUILD_DIR}}/js/ui/ \
		--define:BUILD_TIMESTAMP=$build_timestamp \
		--splitting \
		--minify \
		--alias:node:buffer=buffer/ \
		--alias:node:stream=stream


@license:
	echo "> Copying license file"
	cp LICENSE {{BUILD_DIR}}/LICENSE


@system:
	echo "> Compiling system"
	{{NPM_DIR}}/.bin/gren make system/Build/Build.gren
	node app
	rm app



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


@elm-format:
  echo "> Running elm-format"
  {{NPM_DIR}}/.bin/elm-format {{SRC_DIR}} --yes


@elm-housekeeping: elm-format elm-review


@elm-review:
  echo "> Running elm-review"
  {{ELM_REVIEW}} --fix-all


@quality: check-versions
	echo "> Running es-lint"
	{{NPM_DIR}}/.bin/eslint src/Javascript/**/*
	echo "> Running elm-review"
	{{ELM_REVIEW}}


@server:
	echo "> Booting up web server on port 8000"
	{{NPM_DIR}}/.bin/serve {{BUILD_DIR}} -p 8000 --no-request-logging


@test: doc-tests


@watch:
	echo "> Watching"
	just watch-css & just watch-elm & just watch-js & just watch-system


@watch-css:
	watchexec -p -w {{SRC_DIR}}/Css -w {{SYSTEM_DIR}}/Css -- just css js


@watch-elm:
	watchexec -p -w {{SRC_DIR}} -e elm -- just elm js css


@watch-js:
	watchexec -p -w {{SRC_DIR}} -e js,ts -- just js


@watch-system:
	watchexec -p --ignore *.elm --ignore *.js --ignore *.ts --ignore *.css --ignore src-tauri/** -- just system js
