.PHONY: all build

DOCKER_IMAGE ?= plaza-addon-builder
TS = src/Background.ts src/BrowserApi.ts src/Injected.ts src/PlazaApi.ts \
   src/PlazaApi.types.ts src/Popup.ts src/Storage.ts

TSC_BUNDLE = node_modules/typescript-bundle

all: build

build: dist/plaza.xpi

docker-build:
	docker build -t $(DOCKER_IMAGE) .
	docker run -i --rm -v `pwd`:/app $(DOCKER_IMAGE)

dist/plaza.xpi: background.js popup/injected.js popup/plaza.js manifest.json popup/plaza.html popup/plaza.css icons/icon-48.png
	zip $@ $+

background.js: $(TS) $(TSC_BUNDLE)
	node $(TSC_BUNDLE) --project tsconfig_background.json

popup/injected.js: $(TS) $(TSC_BUNDLE)
	node $(TSC_BUNDLE) --project tsconfig_injected.json

popup/plaza.js: $(TS) $(TSC_BUNDLE)
	node $(TSC_BUNDLE) --project tsconfig_popup.json

$(TSC_BUNDLE):
	npm install .
