CSS_LIBS_PATH=src/app/libs/css
FONTS_LIBS_PATH=src/app/libs/fonts

.PHONY: all libs

all: libs

libs: $(CSS_LIBS_PATH)/material-icons.css \
	  $(FONTS_LIBS_PATH)/material-icons.ttf

$(CSS_LIBS_PATH)/material-icons.css:
	wget 'https://fonts.googleapis.com/icon?family=Material+Icons' -O- \
	| sed -r 's/( *src: url\()([^)]+)(\).*)/  \/* Original: \2 *\/\n\1..\/fonts\/material-icons.ttf\3/g' \
	> $@

$(FONTS_LIBS_PATH)/material-icons.ttf: $(CSS_LIBS_PATH)/material-icons.css
	wget `grep Original $< | cut -d':' -f2-|cut -d' ' -f2` -O $@
	touch $@
