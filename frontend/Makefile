all: js copy

js:
	elm make src/Main.elm --output=app.js

copy:
	mv app.js ../static/js/app.js
