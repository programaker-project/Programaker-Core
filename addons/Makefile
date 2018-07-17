.PHONY: all build

TS=src/Background.ts src/BrowserApi.ts src/Injected.ts src/PlazaApi.ts \
   src/PlazaApi.types.ts src/Popup.ts src/Storage.ts

TSC_BUNDLE=node_modules/typescript-bundle

all: build

build: dist/plaza.xpi

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
