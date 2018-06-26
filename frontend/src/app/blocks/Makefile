COMPRESSION?=uncompressed

.PHONY: all build default-python-2 javascript media clean-and-update

all: build

build: javascript media

javascript: blockly_horizontal.js blockly_vertical.js blocks_horizontal.js blocks_vertical.js msg_en.js messages.js blocks.js

media: media/sprites.png

# Update scratch version removing and external & build files
clean-and-update: default-python-2
	cd scratch-blocks && git clean -fdx .
	cd scratch-blocks && git stash save "Stashed for clean&update."
	cd scratch-blocks && git pull

# Files to be copied
blocks_horizontal.js: scratch-blocks/blocks_$(COMPRESSION)_horizontal.js
	if [ "`wc -l $< | cut -d\  -f1`" -gt 10 ]; then \
	    cp -v $< $@;                            	\
	else                                        	\
	    echo "Error in $<"; exit 1;                	\
	fi

blockly_horizontal.js: scratch-blocks/blockly_$(COMPRESSION)_horizontal.js
	if [ "`wc -l $< | cut -d\  -f1`" -gt 10 ]; then \
	    cp -v $< $@;                            	\
	else                                        	\
	    echo "Error in $<"; exit 1;                	\
	fi

blocks_vertical.js: scratch-blocks/blocks_$(COMPRESSION)_vertical.js
	if [ "`wc -l $< | cut -d\  -f1`" -gt 10 ]; then \
	    cp -v $< $@;                            	\
	else                                        	\
	    echo "Error in $<"; exit 1;                	\
	fi

blockly_vertical.js: scratch-blocks/blockly_$(COMPRESSION)_vertical.js
	if [ "`wc -l $< | cut -d\  -f1`" -gt 10 ]; then \
	    cp -v $< $@;                            	\
	else                                        	\
	    echo "Error in $<"; exit 1;                	\
	fi

blocks.js: scratch-blocks/blocks_$(COMPRESSION).js
	if [ "`wc -l $< | cut -d\  -f1`" -gt 10 ]; then \
	    cp -v $< $@;                            	\
	else                                        	\
	    echo "Error in $<"; exit 1;                	\
	fi

msg_en.js: scratch-blocks/msg/js/en.js
	if [ "`wc -l $< | cut -d\  -f1`" -gt 10 ]; then \
	    cp -v $< $@;                            	\
	else                                        	\
	    echo "Error in $<"; exit 1;                	\
	fi

media/sprites.png: scratch-blocks/package.json
	cp -Rv scratch-blocks/media/ media/

messages.js: scratch-blocks/msg/messages.js
	cp -Rv $< $@

# Intermediate files
scratch-blocks/msg/js/en.js: scratch-blocks/blocks_$(COMPRESSION).js

scratch-blocks/blocks_$(COMPRESSION)_horizontal.js: scratch-blocks/blocks_$(COMPRESSION).js

scratch-blocks/blockly_$(COMPRESSION)_horizontal.js: scratch-blocks/blocks_$(COMPRESSION).js

scratch-blocks/blocks_$(COMPRESSION)_vertical.js: scratch-blocks/blocks_$(COMPRESSION).js

scratch-blocks/blockly_$(COMPRESSION)_vertical.js: scratch-blocks/blocks_$(COMPRESSION).js

# Root canary file
scratch-blocks/blocks_$(COMPRESSION).js: default-python-2 closure-library/package.json scratch-blocks/package.json
	cd scratch-blocks && npm install && python2 build.py

# Pull submodules if needed
closure-library/package.json:
	git submodule update --init --recursive closure-library
	npm install

scratch-blocks/package.json:
	git submodule update --init --recursive scratch-blocks

# Check that default python installation is python2 (required by scratch)
default-python-2:
	(python -V 2>&1|grep 'Python 2') || \
	(echo 'Scratch builder requires `python2` as default `python` executable'; false)
