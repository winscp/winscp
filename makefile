#------------------------------------------------------------------------------
!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$&.mak
BPR2MAK = $(ROOT)\bin\bpr2mak
#------------------------------------------------------------------------------
default: WinSCP3.exe

WinSCP3.exe: WinSCP3.bpr
WinSCP3.exe: lib\Moje_B5.lib lib\DragDrop_B5.lib lib\DriveDir_B5.lib
WinSCP3.exe: lib\Putty53b.lib lib\ScpCore.lib lib\RScpComp.lib lib\ScpForms.lib
 $(BPR2MAK) WinSCP3.bpr
 $(MAKE)

{general}.bpk{lib}.lib:
 cd general
 $(BPR2MAK) $.
 $(MAKE)
 cd ..

{}.bpk{lib}.lib:
 $(BPR2MAK) $.
 $(MAKE)

{}.bpr{lib}.lib:
 $(BPR2MAK) -t$(ROOT)\bin\deflib.bmk $.
 $(MAKE)
