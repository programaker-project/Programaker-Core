.PHONY: all build javascript media

all: build

build: javascript  media

javascript: blockly_horizontal.js blockly_vertical.js blocks_horizontal.js blocks_vertical.js msg_en.js blocks.js

media: media/sprites.png

# Files to be copied
blocks_horizontal.js: scratch-blocks/blocks_compressed_horizontal.js
	cp -v $< $@

blockly_horizontal.js: scratch-blocks/blockly_compressed_horizontal.js
	cp -v $< $@

blocks_vertical.js: scratch-blocks/blocks_compressed_vertical.js
	cp -v $< $@

blockly_vertical.js: scratch-blocks/blockly_compressed_vertical.js
	cp -v $< $@

blocks.js: scratch-blocks/blocks_compressed.js
	cp -v $< $@

msg_en.js: scratch-blocks/msg/js/en.js
	cp -v $< $@

media/sprites.png: scratch-blocks/package.json
	cp -Rv scratch-blocks/media/ media/

# Intermediate files
scratch-blocks/msg/js/en.js: scratch-blocks/blocks_compressed.js

scratch-blocks/blocks_compressed_horizontal.js: scratch-blocks/blocks_compressed.js

scratch-blocks/blockly_compressed_horizontal.js: scratch-blocks/blocks_compressed.js

scratch-blocks/blocks_compressed_vertical.js: scratch-blocks/blocks_compressed.js

scratch-blocks/blockly_compressed_vertical.js: scratch-blocks/blocks_compressed.js

# Root canary file
scratch-blocks/blocks_compressed.js: closure-library/package.json scratch-blocks/package.json
	cd scratch-blocks && npm install

# Pull submodules if needed
closure-library/package.json:
	git submodule update --init --recursive closure-library

scratch-blocks/package.json:
	git submodule update --init --recursive scratch-blocks
