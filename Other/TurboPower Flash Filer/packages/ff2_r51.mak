# ---------------------------------------------------------------------------
!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
# IDE SECTION
# ---------------------------------------------------------------------------
# The following section of the project makefile is managed by the BCB IDE.
# It is recommended to use the IDE to change any of the values in this
# section.
# ---------------------------------------------------------------------------

VERSION = BCB.05.03
# ---------------------------------------------------------------------------
PROJECT = FF2_R51.bpl
OBJFILES = ..\source\ffsrsort.obj ..\source\ffsrcur.obj ..\source\ffllcoll.obj \
    ..\source\cocobase.obj ..\source\ffclbase.obj ..\source\ffclbde.obj \
    ..\source\ffclcfg.obj ..\source\ffclconv.obj ..\source\ffclintf.obj \
    ..\source\ffclreng.obj ..\source\ffcltbrg.obj ..\source\ffdb.obj \
    ..\source\fffile.obj ..\source\ffhash.obj ..\source\ffllbase.obj \
    ..\source\ffllcomm.obj ..\source\ffllcomp.obj ..\source\fflldict.obj \
    ..\source\fflleng.obj ..\source\ffllexcp.obj ..\source\ffllgrid.obj \
    ..\source\ffconst.obj ..\source\ffconvff.obj ..\source\ffdbbase.obj \
    ..\source\ffdtmsgq.obj ..\source\fflllgcy.obj ..\source\fflllog.obj \
    ..\source\ffllprot.obj ..\source\ffllreq.obj ..\source\ffllthrd.obj \
    ..\source\ffllunc.obj ..\source\ffllwsck.obj ..\source\ffllwsct.obj \
    ..\source\fflogdlg.obj ..\source\ffnetmsg.obj ..\source\ffsql.obj \
    ..\source\ffsqlbas.obj ..\source\ffsqldb.obj ..\source\ffsqldef.obj \
    ..\source\ffsqleng.obj ..\source\ffsrbase.obj ..\source\ffsrbde.obj \
    ..\source\ffsrblob.obj ..\source\ffsrcfg.obj ..\source\ffsrcmd.obj \
    ..\source\ffsrcvex.obj ..\source\ffsreng.obj ..\source\ffsrfltr.obj \
    ..\source\ffsrfmap.obj ..\source\ffsrfold.obj ..\source\ffsrintf.obj \
    ..\source\ffsrintm.obj ..\source\ffsrixhl.obj ..\source\ffsrlock.obj \
    ..\source\ffsrmgr.obj ..\source\ffsrsec.obj ..\source\ffsrstat.obj \
    ..\source\ffsrtran.obj ..\source\ffsrvdlg.obj ..\source\ffstdate.obj \
    ..\source\fftbbase.obj ..\source\fftbblob.obj ..\source\fftbcryp.obj \
    ..\source\fftbdata.obj ..\source\fftbdict.obj ..\source\fftbindx.obj \
    ..\source\fftbstrm.obj ..\source\ffutil.obj ..\source\ffsrjour.obj \
    ff2_r51.obj
RESFILES = ..\source\ff2_r51.res
MAINSOURCE = ff2_r51.cpp
RESDEPEN = $(RESFILES) ..\source\fflogdlg.dfm ..\source\ffsrvdlg.dfm
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = VCLBDE50.lib
PACKAGES = vcl50.bpi vcldb50.bpi
SPARELIBS = vcl50.lib vcldb50.lib VCLBDE50.lib
DEFFILE = 
# ---------------------------------------------------------------------------
PATHCPP = .;
PATHASM = .;
PATHPAS = .;..\source
PATHRC = .;
DEBUGLIBPATH = $(BCB)\lib\debug
RELEASELIBPATH = $(BCB)\lib\release
USERDEFINES = 
SYSDEFINES = NO_STRICT;_VIS_NOLIB;USEPACKAGES
INCLUDEPATH = ..\source;$(BCB)\include;$(BCB)\include\vcl
LIBPATH = ..\source;$(bcb)\lib;$(BCB)\lib\obj
WARNINGS= -w-par -w-8027 -w-8026
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -Vx -Ve -Tkh30000 -X- -a8 -b- -k- -vi -c -tWM
IDLCFLAGS = -I..\source -I$(BCB)\include -I$(BCB)\include\vcl -src_suffix cpp -boa
PFLAGS = -$Y- -$L- -$D- -v -M -JPHNE
RFLAGS = 
AFLAGS = /mx /w2 /zn
LFLAGS = -D"TurboPower FlashFiler Runtime Package - VCL51" -aa -Tpp -Gpr -x -Gn -Gl \
    -Gi
# ---------------------------------------------------------------------------
ALLOBJ = c0pkg32.obj $(PACKAGES) sysinit.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cp32mt.lib
# ---------------------------------------------------------------------------
!ifdef IDEOPTIONS

[Version Info]
IncludeVerInfo=0
AutoIncBuild=0
MajorVer=1
MinorVer=0
Release=0
Build=0
Debug=0
PreRelease=0
Special=0
Private=0
DLL=0

[Version Info Keys]
CompanyName=
FileDescription=
FileVersion=1.0.0.0
InternalName=
LegalCopyright=
LegalTrademarks=
OriginalFilename=
ProductName=
ProductVersion=1.0.0.0
Comments=

[Debugging]
DebugSourceDirs=$(BCB)\source\vcl

!endif





# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if "$(USERDEFINES)" != ""
AUSERDEFINES = -d$(USERDEFINES:;= -d)
!else
AUSERDEFINES =
!endif

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(CPP32)
CPP32 = cpp32
!endif

!if !$d(DCC32)
DCC32 = dcc32
!endif

!if !$d(TASM32)
TASM32 = tasm32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

!if !$d(BRCC32)
BRCC32 = brcc32
!endif


# ---------------------------------------------------------------------------
!if $d(PATHCPP)
.PATH.CPP = $(PATHCPP)
.PATH.C   = $(PATHCPP)
!endif

!if $d(PATHPAS)
.PATH.PAS = $(PATHPAS)
!endif

!if $d(PATHASM)
.PATH.ASM = $(PATHASM)
!endif

!if $d(PATHRC)
.PATH.RC  = $(PATHRC)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) -L$(LIBPATH) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES)
!
# ---------------------------------------------------------------------------
.pas.hpp:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.pas.obj:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.cpp.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.cpp.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.asm.obj:
    $(BCB)\BIN\$(TASM32) $(AFLAGS) -i$(INCLUDEPATH:;= -i) $(AUSERDEFINES) -d$(SYSDEFINES:;= -d) $<, $@

.rc.res:
    $(BCB)\BIN\$(BRCC32) $(RFLAGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -fo$@ $<
# ---------------------------------------------------------------------------




