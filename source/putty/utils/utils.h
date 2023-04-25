/*
 * Internal header to the utils subdirectory, for definitions shared
 * between the library implementations but not intended to be exposed
 * further than that.
 */

void dputs(const char *);

char *dupvprintf_inner(char *buf, size_t oldlen, size_t *sizeptr,
                       const char *fmt, va_list ap);

const char *host_strchr_internal(const char *s, const char *set, bool first);
