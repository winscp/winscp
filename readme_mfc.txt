1) copy libs\mfc\source to $(BDS)\source\mfc

2) copy libs\mfc\include to $(BDS)\include\mfc

3) compile the mfc library using the command:
   make -fborland.mak NO_WARNINGS=1
   (warnings about missing object in library are ok, for the first build)

4) compile the debug mfc library using the command:
   make -fborland.mak NO_WARNINGS=1 -DDEBUG

By default the library will be at $(BDS)\lib\UafxcW.lib
