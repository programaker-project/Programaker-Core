.PHONY: all build

DOCKER_IMAGE ?= programaker-addon-builder
TS = src/Background.ts src/BrowserApi.ts src/Injected.ts src/ProgramakerApi.ts \
   src/ProgramakerApi.types.ts src/Popup.ts src/Storage.ts

TSC_BUNDLE = node_modules/typescript-bundle

all: build

build: dist/programaker.xpi

docker-build:
	docker build -t $(DOCKER_IMAGE) .
	docker run -i --rm -v `pwd`:/app $(DOCKER_IMAGE)

dist/programaker.xpi: background.js popup/injected.js popup/programaker.js manifest.json popup/programaker.html popup/programaker.css icons/icon-48.png
	zip $@ $+

background.js: $(TS) $(TSC_BUNDLE)
	node $(TSC_BUNDLE) --project tsconfig_background.json

popup/injected.js: $(TS) $(TSC_BUNDLE)
	node $(TSC_BUNDLE) --project tsconfig_injected.json

popup/programaker.js: $(TS) $(TSC_BUNDLE)
	node $(TSC_BUNDLE) --project tsconfig_popup.json

$(TSC_BUNDLE):
	npm install .
