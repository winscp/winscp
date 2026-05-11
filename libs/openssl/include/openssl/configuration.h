#ifndef OPENSSL_CONFIGURATION_H

#ifdef _WIN64
#include "configuration_64.h"
#else
#include "configuration_32.h"
#endif

#define ENGINESDIR "C:\\Program Files (x86)\\OpenSSL\\lib\\engines-3"
#define MODULESDIR "C:\\Program Files (x86)\\OpenSSL\\lib\\ossl-modules"
#define OPENSSLDIR "C:\\Program Files (x86)\\Common Files\\SSL"

#endif
