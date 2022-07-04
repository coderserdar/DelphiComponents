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
PROJECT = v103_d51.bpl
OBJFILES = ..\source\VpAbout.obj ..\source\VpAlarmDlg.obj ..\source\VpBase.obj \
    ..\source\VpBaseDS.obj ..\source\VpBDEDS.obj ..\source\VpCalendar.obj \
    ..\source\VpCanvasUtils.obj ..\source\VpClock.obj ..\source\VpConst.obj \
    ..\source\VpContactEditDlg.obj ..\source\VpContactGrid.obj \
    ..\source\VpData.obj ..\source\VpDateEdit.obj ..\source\VpDayView.obj \
    ..\source\VpDBDS.obj ..\source\VpDlg.obj ..\source\VpEdPop.obj \
    ..\source\VpEvntEditDlg.obj ..\source\VpException.obj \
    ..\source\VpFlxDS.obj ..\source\VpLEDLabel.obj ..\source\VpMisc.obj \
    ..\source\VpMonthView.obj ..\source\VpNabEd.obj ..\source\VpNavBar.obj \
    ..\source\VpPrtFmt.obj ..\source\VpPrtFmtCBox.obj ..\source\VpPrtPrv.obj \
    ..\source\VpPrtPrvDlg.obj ..\source\VpReg.obj ..\source\VpResEditDlg.obj \
    ..\source\VpSR.obj ..\source\VpTaskEditDlg.obj ..\source\VpTaskList.obj \
    ..\source\VpTimerPool.obj ..\source\VpWavDlg.obj ..\source\VpWavPE.obj \
    ..\source\VpWeekView.obj ..\source\VpXBase.obj ..\source\VpXChrFlt.obj \
    ..\source\VpXParsr.obj ..\source\VpDatePropEdit.obj \
    ..\source\VpLocalize.obj ..\source\Vpflxdsed1.obj \
    ..\source\VpContactButtons.obj v103_d51.obj
RESFILES = v103_d51.res
MAINSOURCE = v103_d51.cpp
RESDEPEN = $(RESFILES) ..\source\VpAbout.dfm ..\source\VpContactEditDlg.dfm \
    ..\source\VpNabEd.dfm ..\source\VpPrtPrvDlg.dfm ..\source\VpResEditDlg.dfm \
    ..\source\VpWavDlg.dfm ..\source\VpDatePropEdit.dfm ..\source\Vpflxdsed1.dfm
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = 
PACKAGES = vclx50.bpi vcl50.bpi vcldb50.bpi vclbde50.bpi
SPARELIBS = vcl50.lib vclx50.lib vcldb50.lib vclbde50.lib
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
LIBPATH = ..\source;$(BCB)\lib\release;$(BCB)\Projects\Lib;$(BCB)\lib\obj;$(BCB)\lib;$(BCB)\source\ToolsAPI
WARNINGS= -w-par -w-8027 -w-8026
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -H=d:\borland\bcb5\CBUILD~1\lib\vcl50.csm -Hc -Vx -Ve -Tkh30000 -X- \
    -a8 -b- -k- -vi -c -tWM
IDLCFLAGS = -I..\source -I$(BCB)\include -I$(BCB)\include\vcl -src_suffix cpp -boa
PFLAGS = -$Y- -$L- -$D- -v -M -JPHNE
RFLAGS = 
AFLAGS = /mx /w2 /zn
LFLAGS = -D"TurboPower Visual PlanIt 1.03 Designtime Package - VCL51" -aa -Tpp -Gpd \
    -x -Gn -Gl -Gi
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




