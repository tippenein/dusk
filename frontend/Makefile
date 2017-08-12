all: js copy

js:
	elm make src/Main.elm --output=elm.js

copy:
	mv elm.js ../static/js/elm.js
