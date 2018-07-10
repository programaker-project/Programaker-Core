.PHONY: all build dev watch

all: build

build: popup/plaza.js

popup/plaza.js: popup/src/*.ts
	npm run build

watch:
	npm run watch