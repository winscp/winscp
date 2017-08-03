all: setup expat_static

setup:
 setup

expat_static:
 make -l -fexpat_static.mak

clean:
 del /s/f/q release\obj

distclean:
 del /s/f/q release\*
