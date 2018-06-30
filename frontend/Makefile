CSS_LIBS_PATH=src/app/libs/css
FONTS_LIBS_PATH=src/app/libs/fonts
INDEX_LIBS_PATH=src/libs/

.PHONY: all libs

all: libs

libs: $(CSS_LIBS_PATH)/bootstrap.min.css \
	  $(CSS_LIBS_PATH)/material-icons.css \
	  $(INDEX_LIBS_PATH)/js/nprogress.js \
	  $(INDEX_LIBS_PATH)/css/nprogress.css \
	  $(FONTS_LIBS_PATH)/material-icons.ttf

$(INDEX_LIBS_PATH)/js/nprogress.js: node_modules/nprogress/nprogress.js
	cp -v $< $@

$(INDEX_LIBS_PATH)/css/nprogress.css: node_modules/nprogress/nprogress.css
	cp -v $< $@

$(CSS_LIBS_PATH)/bootstrap.min.css: node_modules/bootstrap/dist/css/bootstrap.min.css
	cp -v $< $@

$(CSS_LIBS_PATH)/material-icons.css:
	wget 'https://fonts.googleapis.com/icon?family=Material+Icons' -O- \
	| sed -r 's/( *src: url\()([^)]+)(\).*)/  \/* Original: \2 *\/\n\1..\/fonts\/material-icons.ttf\3/g' \
	> $@

$(FONTS_LIBS_PATH)/material-icons.ttf: $(CSS_LIBS_PATH)/material-icons.css
	wget `grep Original $< | cut -d':' -f2-|cut -d' ' -f2` -O $@
	touch $@
