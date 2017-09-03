all: build_dev copy

release: build copy

build:
	npm run build
build_dev:
	npm run build-dev

copy:
	mv output/bundle.js ../static/js/bundle.js
