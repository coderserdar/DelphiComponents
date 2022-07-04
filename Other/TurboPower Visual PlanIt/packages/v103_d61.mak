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

VERSION = BCB.06.00
# ---------------------------------------------------------------------------
PROJECT = V103_D61.bpl
OBJFILES = V103_D61.obj ..\source\VpAbout.obj ..\source\VpAlarmDlg.obj \
    ..\source\VpBase.obj ..\source\VpBaseDS.obj ..\source\VpBDEDS.obj \
    ..\source\VpCalendar.obj ..\source\VpCanvasUtils.obj ..\source\VpClock.obj \
    ..\source\VpConst.obj ..\source\VpContactEditDlg.obj \
    ..\source\VpContactGrid.obj ..\source\VpData.obj ..\source\VpDateEdit.obj \
    ..\source\VpDayView.obj ..\source\VpDBDS.obj ..\source\VpDlg.obj \
    ..\source\VpEdPop.obj ..\source\VpEvntEditDlg.obj \
    ..\source\VpException.obj ..\source\VpFlxDS.obj ..\source\VpLEDLabel.obj \
    ..\source\VpMisc.obj ..\source\VpMonthView.obj ..\source\VpNabEd.obj \
    ..\source\VpNavBar.obj ..\source\VpPrtFmt.obj ..\source\VpPrtFmtCBox.obj \
    ..\source\VpPrtPrv.obj ..\source\VpPrtPrvDlg.obj ..\source\VpReg.obj \
    ..\source\VpResEditDlg.obj ..\source\VpSR.obj ..\source\VpTaskEditDlg.obj \
    ..\source\VpTaskList.obj ..\source\VpTimerPool.obj ..\source\VpWavDlg.obj \
    ..\source\VpWavPE.obj ..\source\VpWeekView.obj ..\source\VpXBase.obj \
    ..\source\VpXChrFlt.obj ..\source\VpXParsr.obj \
    ..\source\VpDatePropEdit.obj ..\source\VpLocalize.obj \
    ..\source\VpFlxDsEd1.obj ..\source\VpContactButtons.obj
RESFILES = V103_D61.res
MAINSOURCE = V103_D61.cpp
RESDEPEN = $(RESFILES) ..\source\VpAbout.dfm ..\source\VpContactEditDlg.dfm \
    ..\source\VpNabEd.dfm ..\source\VpPrtPrvDlg.dfm ..\source\VpResEditDlg.dfm \
    ..\source\VpWavDlg.dfm ..\source\VpDatePropEdit.dfm ..\source\VpFlxDsEd1.dfm
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = bcb2kaxserver.lib indy.lib dbxcds.lib dclocx.lib soaprtl.lib nmfast.lib \
    dbexpress.lib inetdbxpress.lib inetdbbde.lib dsnapcon.lib dsnapcrba.lib \
    visualdbclx.lib teeqr.lib dss.lib tee.lib teedb.lib teeui.lib qrpt.lib \
    bdecds.lib cds.lib ibxpress.lib vcldbx.lib adortl.lib
PACKAGES = vclx.bpi rtl.bpi vcl.bpi dbrtl.bpi vcldb.bpi bdertl.bpi DesignIDE.bpi \
    bcbsmp.bpi bcbie.bpi
SPARELIBS = rtl.lib vcldb.lib adortl.lib bdertl.lib vcldbx.lib ibxpress.lib cds.lib \
    bdecds.lib qrpt.lib teeui.lib teedb.lib tee.lib dss.lib teeqr.lib \
    visualdbclx.lib dsnapcrba.lib dsnapcon.lib bcbsmp.lib inetdbbde.lib \
    inetdbxpress.lib dbexpress.lib nmfast.lib bcbie.lib soaprtl.lib dclocx.lib \
    dbxcds.lib indy.lib bcb2kaxserver.lib
DEFFILE = 
OTHERFILES = 
# ---------------------------------------------------------------------------
DEBUGLIBPATH = $(BCB)\lib\debug
RELEASELIBPATH = $(BCB)\lib\release
USERDEFINES = 
SYSDEFINES = NO_STRICT;_VIS_NOLIB;USEPACKAGES
INCLUDEPATH = ..\source;$(BCB)\include;$(BCB)\include\vcl;$(BCB)\source\vcl
LIBPATH = ..\source;$(BCB)\projects\lib;$(BCB)\lib\obj;$(BCB)\lib\release;$(BCB)\lib;$(BCB)\source\toolsapi
WARNINGS= -w-par -w-8027 -w-8026
PATHCPP = .;
PATHASM = .;
PATHPAS = .;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source
PATHRC = .;
PATHOBJ = .;$(LIBPATH)
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -H=d:\borland\bcb5\CBUILD~1\lib\vcl60.csm -Hc -Vx -Ve -X- -a8 -b- -k- \
    -vi -c -tWM -tWM
IDLCFLAGS = -I..\source -I$(BCB)\include -I$(BCB)\include\vcl -I$(BCB)\source\vcl \
    -src_suffix cpp -boa
PFLAGS = -$YD -$L- -$D- -$A8 -v -M -LUDesignIDE -JPHNE
RFLAGS = 
AFLAGS = /mx /w2 /zn
LFLAGS = -D"TurboPower Visual PlanIt 1.03 Designtime Package - VCL61" -aa -Tpp -Gpd \
    -x -Gn -Gl -Gi
# ---------------------------------------------------------------------------
ALLOBJ = c0pkg32.obj $(PACKAGES) sysinit.obj sysinit.obj $(OBJFILES)
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

!if $d(PATHOBJ)
.PATH.OBJ  = $(PATHOBJ)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(OTHERFILES) $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
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




