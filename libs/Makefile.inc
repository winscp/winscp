CC = "$(BDS)\bin\bcc32"
MKLIB = "$(BDS)\bin\tlib" /a /C
CFLAG_COMMON=-O2 -c -fp -tWM
# These are defaults anyway
CFLAG_COMMON=$(CFLAG_COMMON) -a8 -vi -ff
