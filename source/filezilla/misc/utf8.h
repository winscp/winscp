#ifndef __UTF8_H__
#define __UTF8_H__

// Check for valid UTF-8 string. Code taken from the examples in RFC 2640
int utf8_valid(const unsigned char *buf, unsigned int len);

#endif //__UTF8_H__
