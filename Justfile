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
		--content "{{SRC_DIR}}/Static/Html/**/*.*,{{SRC_DIR}}/Applications/UI/**/*.elm,{{SRC_DIR}}/Applications/UI.elm,{{SRC_DIR}}/Library/**/*.elm,{{SRC_DIR}}/Javascript/**/*.js" \
		--config {{SYSTEM_DIR}}/Css/Tailwind.js \
		--postcss {{SYSTEM_DIR}}/Css/PostCSS.js \
		--jit \
		{{ if minify == "minify" { "--minify" } else { "" } }}


@elm:
	echo "> Compiling Elm application"
	elm make {{SRC_DIR}}/Applications/Brain.elm --output {{BUILD_DIR}}/brain.elm.js
	elm make {{SRC_DIR}}/Applications/UI.elm --output {{BUILD_DIR}}/ui.elm.js


@elm-prod:
	echo "> Compiling Elm application (optimised)"
	elm make {{SRC_DIR}}/Applications/Brain.elm --output {{BUILD_DIR}}/brain.elm.js --optimize
	elm make {{SRC_DIR}}/Applications/UI.elm --output {{BUILD_DIR}}/ui.elm.js --optimize

	{{NPM_DIR}}/.bin/esbuild {{BUILD_DIR}}/brain.elm.js \
		--minify --outfile={{BUILD_DIR}}/brain.elm.tmp.js

	{{NPM_DIR}}/.bin/esbuild {{BUILD_DIR}}/ui.elm.js \
		--minify --outfile={{BUILD_DIR}}/ui.elm.tmp.js

	rm {{BUILD_DIR}}/brain.elm.js
	mv {{BUILD_DIR}}/brain.elm.tmp.js {{BUILD_DIR}}/brain.elm.js
	rm {{BUILD_DIR}}/ui.elm.js
	mv {{BUILD_DIR}}/ui.elm.tmp.js {{BUILD_DIR}}/ui.elm.js


js: vendor-js
	#!/usr/bin/env bash
	build_timestamp="`date '+%s'`"
	echo "> Compiling Javascript code"

	# Main builds
	{{ESBUILD}} ./src/Javascript/index.js \
		--outfile={{BUILD_DIR}}/ui.js \
		--define:BUILD_TIMESTAMP=$build_timestamp

	{{ESBUILD}} ./src/Javascript/Brain/index.js \
		--inject:./system/Js/node-shims.js \
		--outfile={{BUILD_DIR}}/brain.js

	# Workers
	{{ESBUILD}} ./src/Javascript/Workers/search.js \
		--outfile={{BUILD_DIR}}/search.js

	{{ESBUILD}} ./src/Javascript/Workers/service.js \
		--outfile={{BUILD_DIR}}/service-worker.js \
		--define:BUILD_TIMESTAMP=$build_timestamp


js-prod: vendor-js
	#!/usr/bin/env bash
	build_timestamp="`date '+%s'`"
	echo "> Compiling Javascript code (optimised)"

	# Main builds
	{{ESBUILD}} ./src/Javascript/index.js \
		--minify \
		--outfile={{BUILD_DIR}}/ui.js \
		--define:BUILD_TIMESTAMP=$build_timestamp

	{{ESBUILD}} ./src/Javascript/Brain/index.js \
		--minify \
		--inject:./system/Js/node-shims.js \
		--outfile={{BUILD_DIR}}/brain.js

	# Workers
	{{ESBUILD}} ./src/Javascript/Workers/search.js \
		--minify \
		--outfile={{BUILD_DIR}}/search.js

	{{ESBUILD}} ./src/Javascript/Workers/service.js \
		--minify \
		--outfile={{BUILD_DIR}}/service-worker.js \
		--define:BUILD_TIMESTAMP=$build_timestamp


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


@vendor-js:
	mkdir -p {{BUILD_DIR}}/vendor
	cp {{NPM_DIR}}/subworkers/subworkers.js {{BUILD_DIR}}/subworkers.js
	cp {{NPM_DIR}}/remotestoragejs/release/remotestorage.js {{BUILD_DIR}}/vendor/remotestorage.min.js
	cp {{NPM_DIR}}/webnative/dist/index.umd.min.js {{BUILD_DIR}}/vendor/webnative.min.js
	cp ./vendor/ipfs.min.js {{BUILD_DIR}}/vendor/ipfs.min.js
	cp ./vendor/pep.js {{BUILD_DIR}}/vendor/pep.js

	{{NPM_DIR}}/.bin/esbuild {{NPM_DIR}}/webnative-elm/src/funnel.js --minify --outfile={{BUILD_DIR}}/vendor/webnative-elm.min.js


#
# Dev tasks
#

@dev: build
	# just watch-wo-build & just server
	just server


@doc-tests:
	echo "> Running documentation tests"
	( cd src && \
		find . -name "*.elm" -print0 | \
		xargs -0 -n 1 -I % sh -c 'elm-proofread -- % || exit 255; echo "\n\n"' \
	)


@download-vendor-dep filename url:
	curl --silent --show-error --fail -o ./vendor/{{filename}} {{url}}


@elm-housekeeping:
	echo "> Running elm-review"
	{{NPM_DIR}}/.bin/elm-review {{SRC_DIR}} --config system/Review --fix-all
	echo "> Running elm-format"
	elm-format {{SRC_DIR}} --yes


@install-deps:
	pnpm install

	mkdir -p vendor

	just download-vendor-dep pep.js https://raw.githubusercontent.com/mpizenberg/elm-pep/071616d75ca61e261fdefc7b55bc46c34e44ea22/elm-pep.js
	just download-vendor-dep ipfs.min.js https://unpkg.com/ipfs@0.62.3/index.min.js


@install-tauri-cli:
	cargo install tauri-cli --version "^1.0.5" --root ./src-tauri


@quality: check-versions
	echo "> Running es-lint"
	{{NPM_DIR}}/.bin/eslint src/Javascript/**
	echo "> Running elm-review"
	{{NPM_DIR}}/.bin/elm-review {{SRC_DIR}} --config system/Review


@server:
	echo "> Booting up web server on port 8000"
	nix-shell --run "simple-http-server --port 8000 --try-file {{BUILD_DIR}}/301.html --cors --index --nocache --silent -- {{BUILD_DIR}}"


@tauri-dev:
	./src-tauri/bin/cargo-tauri tauri dev


@test: doc-tests


@watch: build watch-wo-build


@watch-wo-build:
	echo "> Watching"
	just watch-css & just watch-elm & just watch-js & just watch-system


@watch-css:
	watchexec -p -w {{SRC_DIR}}/Css -w {{SYSTEM_DIR}}/Css -- just css


@watch-elm:
	watchexec -p -w {{SRC_DIR}} -e elm -- just elm css


@watch-js:
	watchexec -p -w {{SRC_DIR}} -e js -- just js


@watch-system:
	watchexec -p --ignore *.elm --ignore *.js --ignore *.css -- just system
