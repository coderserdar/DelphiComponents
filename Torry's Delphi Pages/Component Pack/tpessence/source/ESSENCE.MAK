#-------------------------------------------------------------------------
#  ESSENCE.MAK
#-------------------------------------------------------------------------
#  Make file used to build Essentials
#
#  This make file is designed to be used only with Borland's MAKE utility.
#
#      MAKE -fESSENCE.MAK [optional parameters]
#
#  at the DOS command line or in response to Window's File|Run dialog.
#

#-------------------------------------------------------------------------
#  Compiler choice
#-------------------------------------------------------------------------

# Define only one of the following by uncommenting the appropriate line
# or undefine all and state the define as part of the make command line
# as -DDelphi1/2/3

Delphi1=1
#Delphi2=1
#Delphi3=1

#set to the current version (for package naming)
ver=104

Setup1=1   # At work
#Setup2=1   # At home

#-------------------------------------------------------------------------
#  Macros
#-------------------------------------------------------------------------

# Path to compiler
!if $d(Setup1)
  dcc1=c:\borland\delphi\bin\dcc.exe
  dcc2="c:\borland\delphi 2.0\bin\dcc32.exe"
  dcc3="c:\borland\delphi 3\bin\dcc32.exe"
!else
  !if $d(Setup2)
    dcc1=d:\progra~1\borland\delphi\bin\dcc.exe
    dcc2=d:\progra~1\borland\delphi~1.0\bin\dcc32.exe
    dcc3=d:\progra~1\borland\delphi3\bin\dcc32.exe
  !endif
!endif

# Path to resource compilers
!if $d(Setup1)
  brc1=c:\borland\delphi\bin\brcc.exe
  brc2="c:\borland\delphi 2.0\bin\brcc32.exe"
  brc3="c:\borland\delphi 3\bin\brcc32.exe"
  srmgr=d:\srmgr\srmc.exe
!else
  !if $d(Setup2)
    brc1=d:\progra~1\borland\delphi\bin\brcc.exe
    brc2=d:\progra~1\borland\delphi~1.0\bin\brcc32.exe
    brc3=d:\progra~1\borland\delphi3\bin\brcc32.exe
    srmgr=d:\srmgr\srmc.exe
  !endif
!endif


opts=/M /$$D- /$$L- /L

!if $d(Delphi1)
  dcc=$(dcc1) $(opts)
  brc=$(brc1) -31 -dwin16 -fo$&.r16
  res=r16
  srmc=$(srmgr) -16 -fo$&.s16
  srm=s16
!endif

!if $d(Delphi2)
  dcc=$(dcc2) $(opts)
  brc=$(brc2) -w32 -fo$&.r32
  res=r32
  srmc=$(srmgr) -32 -fo$&.s32
  srm=s32
!endif

!if $d(Delphi3)
  dcc=$(dcc3) $(opts)
  brc=$(brc3) -w32 -fo$&.r32
  res=r32
  srmc=$(srmgr) -32 -fo$&.s32
  srm=s32
!endif


#-------------------------------------------------------------------------
#  Implicit rules
#-------------------------------------------------------------------------

.pas.exe:
        $(dcc) $<

.dpr.exe:
        $(dcc) $<

.pas.dcu:
        $(dcc) $<

.pas.dll:
        $(dcc) $<

.dpr.dll:
        $(dcc) $<

.dpk.dcp:
        $(dcc) $<

.dpk.dpl:
        $(dcc) $<

.rc.$(res):
        $(brc) $<

.str.$(srm):
        $(srmc) $<

#-------------------------------------------------------------------------
#  Explicit rules
#-------------------------------------------------------------------------

!if $d(Delphi3)
alles       : essentials resources packages
!else
alles       : essentials resources
!endif

essentials  : resources es.inc \
              esbase.dcu esbase0.dcu escal.dcu escalc.dcu \
              esclrcbx.dcu esconst.dcu esdata.dcu esdbed.dcu \
              esedcal.dcu esedcalc.dcu esedpop.dcu esgrad.dcu \
              eslabel.dcu eslabel0.dcu eslabel1.dcu esmarque.dcu \
              esmnubtn.dcu esrollup.dcu esdir.dcu \
              estile.dcu esutil.dcu esreg.dcu esregdb.dcu

resources   : esconst.$(srm) esconst.$(res) esconst.dcu \
              esreg.$(res) esregdb.$(res) \
              esbase.$(res) esdir.$(res)

!if $d(Delphi3)
packages    : essentials resources \
              tes$(ver)_r.dpl tes$(ver)_r.dcp \
              tes$(ver)_d.dpl tes$(ver)_d.dcp \
              tes$(ver)br.dpl tes$(ver)br.dcp \
              tes$(ver)bd.dpl tes$(ver)bd.dcp
!endif