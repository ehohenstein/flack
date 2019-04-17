
all: build test

npm:
	npm install

build: npm
	node_modules/.bin/browserify scripts/main.js -o flack-app.js

test:
	npm test

clean:
	rm -rf node_modules
