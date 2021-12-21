prefix=$<TARGET_PROPERTY:pkgconfig_prefix>
exec_prefix=$<TARGET_PROPERTY:pkgconfig_exec_prefix>
libdir=$<TARGET_PROPERTY:pkgconfig_libdir>
includedir=$<TARGET_PROPERTY:pkgconfig_includedir>

Name: $<TARGET_PROPERTY:pkgconfig_$<LOWER_CASE:$<CONFIG>>_name>
Version: $<TARGET_PROPERTY:pkgconfig_version>
Description: expat XML parser
URL: https://libexpat.github.io/
Libs: -L${libdir} -l$<TARGET_PROPERTY:pkgconfig_$<LOWER_CASE:$<CONFIG>>_name> $<TARGET_PROPERTY:pkgconfig_libm>
Cflags: -I${includedir}
