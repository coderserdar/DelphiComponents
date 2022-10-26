# ---------------------------------------------------------------------------
VERSION = BCB.01
# ---------------------------------------------------------------------------
!ifndef BCB
BCB = $(MAKEDIR)\..
!endif
# ---------------------------------------------------------------------------
PROJECT = Dbexpl32.exe
OBJFILES = Options.obj Viewblob.obj Userhelp.obj Srctab.obj Sqlmon.obj \
   RenDlg.obj Optdlg.obj Opendlg.obj Filtdlg.obj Editstr.obj Editpict.obj \
   Desttab.obj Dbcbrest.obj Childwin.obj BdeProp.obj About.obj Main.obj \
   Dbexpl32.obj
RESFILES = Dbexpl32.res
RESDEPEN = $(RESFILES) Main.dfm About.dfm BdeProp.dfm Childwin.dfm \
   Dbcbrest.dfm Desttab.dfm Editpict.dfm Editstr.dfm Filtdlg.dfm Opendlg.dfm \
   Optdlg.dfm RenDlg.dfm Sqlmon.dfm Srctab.dfm Userhelp.dfm Viewblob.dfm
LIBFILES = 
DEFFILE = 
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -w -k- -r -vi -c -a4 -b- -w-par -w-inl -Vx -Ve -x
CFLAG2 = -I$(BCB)\rx;$(BCB)\include;$(BCB)\include\vcl;e:\work\rxcb \
   -H=$(BCB)\lib\vcl.csm 
PFLAGS = -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE \
   -U$(BCB)\rx;$(BCB)\lib\obj;$(BCB)\lib;e:\work\rxcb \
   -I$(BCB)\rx;$(BCB)\include;$(BCB)\include\vcl;e:\work\rxcb -v -JPHNV -M 
RFLAGS = -i$(BCB)\rx;$(BCB)\include;$(BCB)\include\vcl;e:\work\rxcb 
LFLAGS = -L$(BCB)\rx;$(BCB)\lib\obj;$(BCB)\lib;e:\work\rxcb -aa -Tpe -x -V4.0 
IFLAGS = 
LINKER = ilink32
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
