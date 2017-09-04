all: prod build copy

dev: build_dev copy

build:
	npm run build

prod:
	sed -i 's/http:\/\/localhost:3000/http:\/\/dusk.host/' src/Helper.purs

build_dev:
	npm run build-dev

copy:
	mv output/bundle.js ../static/js/bundle.js
