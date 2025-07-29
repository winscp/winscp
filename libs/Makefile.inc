CC = "$(BDS)\bin\bcc32"
MKLIB = "$(BDS)\bin\tlib" /a /C
CFLAG_BASE=-O2 -c -fp -tWM
# These are defaults anyway
CFLAG_BASE=$(CFLAG_BASE) -a8 -vi -ff
CFLAG_COMMON=$(CFLAG_BASE)
CFLAG_CPP=$(CFLAG_BASE) /P