# ---------------------------------------------------------------------------
VERSION = BCB.01
# ---------------------------------------------------------------------------
!ifndef BCB
BCB = $(MAKEDIR)\..
!endif
# ---------------------------------------------------------------------------
PROJECT = RichEdit.exe
OBJFILES = ParaFmt.obj REMain.obj RichEdit.obj
RESFILES = Richedit.res
RESDEPEN = $(RESFILES) REMain.dfm ParaFmt.dfm
LIBFILES = 
DEFFILE = 
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -w -k- -r -vi -c -a4 -b- -w-par -w-inl -Vx -Ve -x
CFLAG2 = -I$(BCB)\rx;$(BCB)\projects;$(BCB)\bin;$(BCB)\include;$(BCB)\include\vcl;e:\work\rx_cb \
   -H=$(BCB)\lib\vcl.csm 
PFLAGS = -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE \
   -U$(BCB)\rx;$(BCB)\projects;$(BCB)\bin;$(BCB)\lib\obj;$(BCB)\lib;e:\work\rx_cb \
   -I$(BCB)\rx;$(BCB)\projects;$(BCB)\bin;$(BCB)\include;$(BCB)\include\vcl;e:\work\rx_cb \
   -$V- -$U -$L- -$D- -JPHNV -M     
RFLAGS = -i$(BCB)\rx;$(BCB)\projects;$(BCB)\bin;$(BCB)\include;$(BCB)\include\vcl;e:\work\rx_cb 
LFLAGS = -L$(BCB)\rx;$(BCB)\projects;$(BCB)\bin;$(BCB)\lib\obj;$(BCB)\lib;e:\work\rx_cb \
   -aa -Tpe -x -V4.0 
IFLAGS = 
LINKER = tlink32
# ---------------------------------------------------------------------------
ALLOBJ = c0w32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) vcl.lib import32.lib cp32mt.lib 
# ---------------------------------------------------------------------------
.autodepend

$(PROJECT): $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES) 
!

.pas.hpp:
    $(BCB)\BIN\dcc32 $(PFLAGS) { $** }

.pas.obj:
    $(BCB)\BIN\dcc32 $(PFLAGS) { $** }

.cpp.obj:
    $(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $* 

.c.obj:
    $(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**

.rc.res:
    $(BCB)\BIN\brcc32 $(RFLAGS) $<
#-----------------------------------------------------------------------------
