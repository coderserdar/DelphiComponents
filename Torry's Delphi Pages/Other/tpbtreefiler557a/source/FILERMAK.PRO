#*********************************************************************
#* FILER.MAK - MAKE file to rebuild B-Tree Filer                     *
#*********************************************************************

#* ***** BEGIN LICENSE BLOCK *****
#* Version: MPL 1.1
#*
#* The contents of this file are subject to the Mozilla Public License Version
#* 1.1 (the "License"); you may not use this file except in compliance with
#* the License. You may obtain a copy of the License at
#* http://www.mozilla.org/MPL/
#*
#* Software distributed under the License is distributed on an "AS IS" basis,
#* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
#* for the specific language governing rights and limitations under the
#* License.
#*
#* The Original Code is TurboPower B-Tree Filer
#*
#* The Initial Developer of the Original Code is
#* TurboPower Software
#*
#* Portions created by the Initial Developer are Copyright (C) 1996-2002
#* the Initial Developer. All Rights Reserved.
#*
#* Based in part on code written by Ralf Nagel
#*
#* Contributor(s):
#*
#* ***** END LICENSE BLOCK ***** *)

# Notes:
#
# 1. This make file is designed to be used only with Borland's MAKE
# utility, version 3.6 and above (ie, it will work with the MAKE.EXE
# supplied with TP7, BP7, or Delphi 1.0, 2.0 and 3.0).
#
# 2. To use this MAKE file go to the \FILER diectory and type in a
# command of the form
#
#      MAKE -fFILER.MAK compiler=<compiler> options=<options>
#
# at the DOS prompt (MAKE is assumed to be on the path). <compiler> can
# be one of the following (case is sensitive) and must be supplied
# (there is no default):
#
#      BP7R  - real-mode BP7
#      BP7P  - protected-mode BP7
#      BP7W  - Windows-mode BP7
#      D1    - Delphi 1
#      D2    - Delphi 2
#      D3    - Delphi 3
#      D4    - Delphi 4
#      D5    - Delphi 5
#
# <options> can be one of the following, and if it not supplied the
# default value is shown (note that it is only required if the
# compiler value is BP7R or BP7P):
#
#      NONE   (neither Turbo or Object Professional present *DEFAULT*)
#      OPRO   (Object Professional present)
#      TPRO   (Turbo Professional present)
#      BOTH   (both Turbo or Object Professional present)
#
# A couple of examples:
#
#      MAKE -fFILER.MAK Compiler=D1
#
# will compile B-Tree Filer for use with Delphi 1.
#
#      MAKE -fFILER.MAK Compiler=BP7R Options=OPRO
#
# will compile B-Tree Filer for use with BP7 real mode, using Object
# Professional.
#
# 3. The compilers, assembler and Windows resource compilers used by
# this MAKE file are assumed to be on the DOS path. If they are not,
# you will have to supply the full path to the compiler by altering
# the relevant macro below (look for the heading "Compiler Path
# Macros").
#
# 4. If you do not have a copy of Turbo Assembler, be sure to comment
# out the HaveAssembler macro below. (To comment out a macro, insert
# a # character at the front of the line.)
#
# 5. If BP7R or BP7P is selected, the Turbo Vision parts of B-Tree
# Filer are always compiled. If you do not have TV, or don't want this
# behavior comment out the WantTV macro below.
#
# 6. If you want the B-Tree Filer units compiled with debug
# information, uncomment out the DEBUG macro below.
#
# 7. The directory structure assumed by this MAKE file is
#
#      FILER
#      +---DEMOS
#
# If you have a different directory structure, then alter the DirBase
# and DirDemos macros below.


#---------------------------------------------- Define Assembler Stuff
# Comment this line out if you do not have an assembler. If this macro
# is active, ASM files will be assembled into OBJ files if needed.
#HaveAssembler=1


#------------------------------------------- Define Turbo Vision Stuff
# Comment this line out if you do not want to compile the Turbo Vision
# parts of B-Tree Filer.
WantTV=1


#------------------------------------------- Define Turbo Vision Stuff
# Comment this line out if you do not want to compile the B-Tree Filer
# units with debug information.
# DEBUG=1


#------------------------------------------ Define Directory Structure
# This set of macros define the directory structure for B-Tree Filer's
# source code files and demo programs.
DirBase=\FILER                # this *must* be the default directory
DirDemos=$(DirBase)\DEMOS

#------------------------------------------------ Compiler Path Macros
# These macros define the filenames of the command line compilers,
# assembler and resource compilers.
BP7Compiler=c:\bp7\bin\bpc.exe
Delphi1Compiler=c:\Delphi\bin\dcc.exe
Delphi2Compiler=c:\Delphi2\bin\dcc32.exe
Delphi3Compiler=c:\Delphi3\bin\dcc32.exe
Delphi4Compiler=c:\Delphi4\bin\dcc32.exe
Delphi5Compiler=c:\Delphi5\bin\dcc32.exe
TasmAssembler=c:\TASM\bin\tasm.exe
R16Compiler=c:\Delphi\bin\brcc.exe
R32Compiler=c:\Delphi2\bin\brcc32.exe

# This macro shows the directories where your OPro and/or TPro units
# are found. The format of the macro's value follows the DOS PATH
# format, for example if you have the OPro units in C:\OPRO and the
# TPro units in C:\TPRO, specify the following:
#
#      DirOproTPro=C:\OPRO;C:\TPRO
#
# If you have neither OPro or TPro then leave the macro undefined (ie,
# don't put anything after the equals sign).
DirOProTPro=C:\OPRO;C:\TPRO


#------------------------------- Other factors that affect compilation
#
# Note that you must edit BTDEFINE.INC to specify a network option
# and other conditional defines that affect B-Tree Filer

######################################################################
# From this point there are no more defines that need to be reviewed #
# or changed.                                                        #
######################################################################

# if the "compiler" macro is undefined show error
!if !$d(compiler)
!error You must invoke MAKE with compiler=<value> parameter
!endif

# if the "compiler" macro is invalid show error
!undef PrimTest
!if $(compiler) == BP7R
PrimTest=1
!elif $(compiler) == BP7P
PrimTest=1
!elif $(compiler) == BP7W
PrimTest=1
!elif $(compiler) == D1
PrimTest=1
!elif $(compiler) == D2
PrimTest=1
!elif $(compiler) == D3
PrimTest=1
!elif $(compiler) == D4
PrimTest=1
!elif $(compiler) == D5
PrimTest=1
!endif
!if !$d(PrimTest)
!error The compiler value is invalid (use BP7R, BP7P, BP7W, D1, D2, D3, D4, D5)
!endif

# make sure the "options" macro is valid
!undef HaveTPro
!undef HaveOPro
!undef HaveBoth
!if $(compiler) == BP7W
!undef options
!undef WantTV
!elif $(compiler) == D1
!undef options
!undef WantTV
!elif $(compiler) == D2
!undef options
!undef WantTV
!elif $(compiler) == D3
!undef options
!undef WantTV
!elif $(compiler) == D4
!undef options
!undef WantTV
!elif $(compiler) == D5
!undef options
!undef WantTV
!endif
!if !$d(options)
options=NONE
!else
!undef PrimTest
!if $(options) == NONE
PrimTest=1
!elif $(options) == OPRO
PrimTest=1
HaveOPro=1
!elif $(options) == TPRO
PrimTest=1
HaveTPro=1
!elif $(options) == BOTH
PrimTest=1
HaveOPro=1
HaveTPro=1
HaveBoth=1
!endif
!if !$d(PrimTest)
options=NONE
!endif
!endif

# set up the debug directives for the compiles
!if $d(DEBUG)
!if $(__MAKE__)>=0x0370
DBGDirs=-$$D+ -$$L+ -V
!else
DBGDirs=-$D+ -$L+ -V
!endif
!else
!if $(__MAKE__)>=0x0370
DBGDirs=-$$D- -$$L-
!else
DBGDirs=-$D- -$L-
!endif
!endif

# set up the unit extensions; create the compile macro
!if $(compiler) == BP7R
uext=TPU
Compile=$(BP7Compiler) -CD $(DBGDirs) /L
!elif $(compiler) == BP7P
uext=TPP
Compile=$(BP7Compiler) -CP $(DBGDirs) /L
!elif $(compiler) == BP7W
uext=TPW
Compile=$(BP7Compiler) -CW $(DBGDirs) /L
!elif $(compiler) == D1
uext=DCU
Compile=$(Delphi1Compiler) $(DBGDirs) /L
!elif $(compiler) == D2
uext=DCU
Compile=$(Delphi2Compiler) $(DBGDirs) /L
!elif $(compiler) == D3
uext=DCU
Compile=$(Delphi3Compiler) $(DBGDirs) /L
!elif $(compiler) == D4
uext=DCU
Compile=$(Delphi4Compiler) $(DBGDirs) /L
!elif $(compiler) == D5
uext=DCU
Compile=$(Delphi5Compiler) $(DBGDirs) /L -LE$(DirBase) -LN$(DirBase) 
!endif

# create a couple of handy macros
!undef RMode
!undef PMode
!undef DOSMode
!undef WinMode
!undef Win32Mode
!undef DelphiMode
!if $(compiler) == BP7R
RMode=1
DOSMode=1
!elif $(compiler) == BP7P
PMode=1
DOSMode=1
!elif $(compiler) == BP7W
PMode=1
WinMode=1
!elif $(compiler) == D1
PMode=1
WinMode=1
DelphiMode=1
!elif $(compiler) == D2
Win32Mode=1
DelphiMode=1
!elif $(compiler) == D3
Win32Mode=1
DelphiMode=1
!elif $(compiler) == D4
Win32Mode=1
DelphiMode=1
!elif $(compiler) == D5
Win32Mode=1
DelphiMode=1
!endif

# create a few macros to aid with presence/absence of OPro, TPro, TV
NeedOProFiles=
NeedTProFiles=
NeedTVFiles=
!if $d(HaveOPro)
NeedOProFiles=OProFiles
!endif
!if $d(HaveTPro)
NeedTProFiles=TProFiles
!endif
!if $d(WantTV)
NeedTVFiles=TVFiles
!endif


#---------------------------------Force all units/demos to be compiled
!if $(compiler) == BP7R
dummy: NonWin32Files RModeFiles DOSFiles                              \
       $(NeedOProFiles) $(NeedTProFiles) $(NeedTVFiles)               \
       NWFiles CommonFiles
!elif $(compiler) == BP7P
dummy: NonWin32Files PModeFiles DOSFiles                              \
       $(NeedOProFiles) $(NeedTProFiles) $(NeedTVFiles)               \
       NWFiles CommonFiles
!elif $(compiler) == BP7W
dummy: WBrowser.$(uext)                                               \
       $(DirDemos)\BTWDEMO.EXE                                        \
       $(DirDemos)\OWDemo.EXE                                          \
       NonWin32Files PModeFiles NWFiles CommonFiles
!elif $(compiler) == D1
dummy: DOSSupp.DCU  FVCBrows.R16 FVCReg.$(uext)                       \
       NonWin32Files PModeFiles NWFiles DelphiFiles CommonFiles
!elif $(compiler) == D2
dummy: FVCBrows.R32 FVCReg.$(uext)                                    \
       Win32Files DelphiFiles CommonFiles
!elif $(compiler) == D3
dummy: FVCBrows.R32                                                   \
       T555_R30.DPL T555_D30.DPL                                      \
       Win32Files DelphiFiles CommonFiles
!elif $(compiler) == D4
dummy: FVCBrows.R32                                                   \
       T555_R40.DPL T555_D40.DPL                                      \
       Win32Files DelphiFiles CommonFiles
!elif $(compiler) == D5
dummy: FVCBrows.R32                                                   \
       T555_R50.DPL T555_D50.DPL                                      \
       Win32Files DelphiFiles CommonFiles
!endif

RModeFiles : EMSSupp.$(uext)  EMSHeap.$(uext)                         \
             TPAlloc.$(uext)  MSort.$(uext)                           \
             $(DirDemos)\BigSort.EXE

PModeFiles : DPMI.$(uext)     MSortP.$(uext)

DOSFiles : TPCmd.$(uext)    Browser.$(uext)                           \
           $(DirDemos)\DB2Isam.EXE  $(DirDemos)\Isam2DB.EXE           \
           $(DirDemos)\SimpDemo.EXE

OProFiles : OPBrow.$(uext)                                            \
            $(DirDemos)\OPISDEMO.EXE

TProFiles : $(DirDemos)\NETDEMO.EXE

TVFiles : TVBrows.$(uext)                                             \
          $(DirDemos)\TVISDEMO.EXE

NonWin32Files : NumKeys.OBJ  NumKeys.$(uext) NetBIOS.$(uext)          \
                CarrConv.$(uext) DBImpExp.$(uext)                     \
                $(DirDemos)\BindList.EXE $(DirDemos)\MesExamp.EXE     \
                $(DirDemos)\NBChat.EXE   $(DirDemos)\NBSend.EXE       \
                $(DirDemos)\NETINFO.EXE  $(DirDemos)\NISEND.EXE       \
                $(DirDemos)\NSSEND.EXE   $(DirDemos)\SPX2WAY.EXE      \
                $(DirDemos)\TTSFILER.EXE

Win32Files : NumKey32.$(uext)

DelphiFiles : FVCBrows.$(uext)                                        \
              $(DirDemos)\DelDemo.EXE

NWFiles : NWBase.$(uext)   NWBind.$(uext)   NWConn.$(uext)            \
          NWFile.$(uext)   NWIPXSPX.$(uext) NWMsg.$(uext)             \
          NWPrint.$(uext)  NWSema.$(uext)   NWTTS.$(uext)             \
          OOPSema.$(uext)  Share.$(uext)

CommonFiles : BTDEFINE.INC                                            \
              BTBase.$(uext)   BTFileIO.$(uext) BTIsBase.$(uext)      \
              Filer.$(uext)    VRec.$(uext)     Restruct.$(uext)      \
              Reindex.$(uext)  Rebuild.$(uext)  VRebuild.$(uext)      \
              Reorg.$(uext)    VReorg.$(uext)   FixToVar.$(uext)      \
              IsamTool.$(uext)                                        \
              LowBrows.$(uext) MedBrows.$(uext) HiBrows.$(uext)       \
              $(DirDemos)\Traffic.EXE

#-------------------------------------------- Assembly Language Source

!if $d(HaveAssembler)
!if !$d(Win32Mode)
NumKeys.OBJ: NumKeys.ASM
  $(TasmAssembler) $*;
!endif
!endif


#--------------------------------------------------------------- Units

# DOSSupp, Delphi 1 only
!if $(compiler) == D1
DOSSupp.$(uext):  BTDEFINE.INC DOSSupp.PAS
  $(Compile) $&
!endif

# BaseSupp, not Delphi 2 or 3
!if !$d(Win32Mode)
!if $(compiler) == D1
BaseSupp.$(uext): BTDEFINE.INC BaseSupp.PAS DOSSupp.$(uext)
  $(Compile) $&
!else
BaseSupp.$(uext): BTDEFINE.INC BaseSupp.PAS
  $(Compile) $&
!endif
!endif

# EMSSupp, BP7 real mode only
!if $d(RMode)
EMSSupp.$(uext):  BTDEFINE.INC EMSSupp.PAS                            \
                  BaseSupp.$(uext)
  $(Compile) $&
!endif

# EMSHeap, BP7 real mode only
!if $d(RMode)
EMSHeap.$(uext):  BTDEFINE.INC EMSHeap.PAS  EMSHeap.CFG               \
                  EMSSupp.$(uext)
  $(Compile) $&
!endif

# BTBase, all
BTBase.$(uext): BTDEFINE.INC BTBase.PAS
  $(Compile) $&

# BTFileIO, all
BTFileIO.$(uext): BTDEFINE.INC BTFileIO.PAS BTBase.$(uext)
  $(Compile) $&

# BTIsBase, all
!if !$d(Win32Mode)
BTIsBase.$(uext): BTDEFINE.INC BTIsBase.PAS                           \
                  FILER.CFG    ISNETSUP.INC BTLCKMGR.INC              \
                  BaseSupp.$(uext)                                    \
                  BTBase.$(uext)   BTFileIO.$(uext)
  $(Compile) $&
!else
BTIsBase.$(uext): BTDEFINE.INC BTIsBase.PAS                           \
                  FILER.CFG    ISNETSUP.INC BTLCKMGR.INC              \
                  BTBase.$(uext)   BTFileIO.$(uext)
  $(Compile) $&
!endif

# DPMI, not Delphi 2 or 3
!if !$d(Win32Mode)
DPMI.$(uext):     DPMI.PAS
  $(Compile) $&
!endif

# NWBase, not Delphi 2 or 3
!if !$d(Win32Mode)
!if $(compiler) == D1
NWBase.$(uext):   BTDEFINE.INC NWBase.PAS                             \
                  DPMI.$(uext)     DOSSupp.$(uext)
  $(Compile) $&
!else
NWBase.$(uext):   BTDEFINE.INC NWBase.PAS                             \
                  DPMI.$(uext)
  $(Compile) $&
!endif
!endif

# NWBind, not Delphi 2 or 3
!if !$d(Win32Mode)
NWBind.$(uext):   BTDEFINE.INC NWBind.PAS                             \
                  NWBase.$(uext)
  $(Compile) $&
!endif

# NWConn, not Delphi 2 or 3
!if !$d(Win32Mode)
NWConn.$(uext):   BTDEFINE.INC NWConn.PAS                             \
                  NWBase.$(uext)   NWBind.$(uext)
  $(Compile) $&
!endif

# NWFile, not Delphi 2 or 3
!if !$d(Win32Mode)
NWFile.$(uext):   BTDEFINE.INC NWFile.PAS                             \
                  NWBase.$(uext)   NWConn.$(uext)
  $(Compile) $&
!endif

# NWIPXSPX, not Delphi 2 or 3
!if !$d(Win32Mode)
NWIPXSPX.$(uext): BTDEFINE.INC NWIPXSPX.PAS                           \
                  NWBase.$(uext)
  $(Compile) $&
!endif

# NWMsg, not Delphi 2 or 3
!if !$d(Win32Mode)
NWMsg.$(uext):    BTDEFINE.INC NWMsg.PAS                              \
                  NWBase.$(uext)   NWConn.$(uext)
  $(Compile) $&
!endif

# NWPrint, not Delphi 2 or 3
!if !$d(Win32Mode)
NWPrint.$(uext):  BTDEFINE.INC NWPrint.PAS                            \
                  NWBase.$(uext)   NWBind.$(uext)   NWConn.$(uext)    \
                  NWFile.$(uext)

  $(Compile) $&
!endif

# NWSema, not Delphi 2 or 3
!if !$d(Win32Mode)
NWSema.$(uext):   BTDEFINE.INC NWSema.PAS                             \
                  NWBase.$(uext)   NWConn.$(uext)
  $(Compile) $&
!endif

# NWTTS, not Delphi 2 or 3
!if !$d(Win32Mode)
NWTTS.$(uext):    BTDEFINE.INC NWTTS.PAS                              \
                  NWBase.$(uext)
  $(Compile) $&
!endif

# OOPSema, not Delphi 2 or 3
!if !$d(Win32Mode)
OOPSema.$(uext):  BTDEFINE.INC OOPSema.PAS                            \
                  NWBase.$(uext)   NWSema.$(uext)
  $(Compile) $&
!endif

# Share, not Delphi 2 or 3
!if !$d(Win32Mode)
!if $(compiler) == D1
Share.$(uext):    BTDEFINE.INC Share.PAS                              \
                  DPMI.$(uext)     DOSSupp.$(uext)
  $(Compile) $&
!else
Share.$(uext):    BTDEFINE.INC Share.PAS                              \
                  DPMI.$(uext)
  $(Compile) $&
!endif
!endif

# NetBIOS, not Delphi 2 or 3
!if !$d(Win32Mode)
!if $(compiler) == D1
NetBIOS.$(uext):  BTDEFINE.INC NetBIOS.PAS                            \
                  DPMI.$(uext)     DOSSupp.$(uext)
  $(Compile) $&
!else
NetBIOS.$(uext):  BTDEFINE.INC NetBIOS.PAS                            \
                  DPMI.$(uext)
  $(Compile) $&
!endif
!endif

# Filer, all
!if $d(Win32Mode)
Filer.$(uext):    BTDEFINE.INC FILER.PAS                              \
                  FILER.INC    ISAMBASE.INC ISAMLOW.INC  ISAMWORK.INC \
                  ISAMNWRK.INC                                        \
                  BTBase.$(uext)   BTIsBase.$(uext)
  $(Compile) $&
!else
!if $d(RMode)
Filer.$(uext):    BTDEFINE.INC Filer.PAS                              \
                  Filer.INC    IsamBase.INC IsamLow.INC  IsamWork.INC \
                  IsamNWrk.INC                                        \
                  EMSSupp.$(uext)  EMSHeap.$(uext)                    \
                  BaseSupp.$(uext) BTBase.$(uext)   BTIsBase.$(uext)
  $(Compile) $&
!else
Filer.$(uext):    BTDEFINE.INC Filer.PAS                              \
                  Filer.INC    IsamBase.INC IsamLow.INC  IsamWork.INC \
                  IsamNWrk.INC                                        \
                  BaseSupp.$(uext) BTBase.$(uext)   BTIsBase.$(uext)
  $(Compile) $&
!endif
!endif

# VRec, all
VRec.$(uext):     BTDEFINE.INC VRec.PAS                               \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)
  $(Compile) $&

# NumKeys, not Delphi 2 or 3
!if !$d(Win32Mode)
NumKeys.$(uext):  BTDEFINE.INC NumKeys.PAS                            \
                  NumKeys.OBJ
  $(Compile) $&
!endif

# NumKey32, Delphi 2 or 3 only
!if $d(Win32Mode)
NumKey32.$(uext): BTDEFINE.INC NumKey32.PAS
  $(Compile) $&
!endif

# BufRecIO, all
BufRecIO.$(uext): BTDEFINE.INC BufRecIO.PAS                           \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  VRec.$(uext)
  $(Compile) $&

# Restruct, all
Restruct.$(uext): BTDEFINE.INC Restruct.PAS                           \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  VRec.$(uext)     BufRecIO.$(uext)
  $(Compile) $&

# ReIndex, all
Reindex.$(uext):  BTDEFINE.INC Reindex.PAS                            \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  VRec.$(uext)     BufRecIO.$(uext)
  $(Compile) $&

# Rebuild, all
Rebuild.$(uext):  BTDEFINE.INC Rebuild.PAS                            \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  Restruct.$(uext) Reindex.$(uext)
  $(Compile) $&

# VRebuild, all
VRebuild.$(uext):  BTDEFINE.INC VRebuild.PAS                          \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  Vrec.$(uext)     Restruct.$(uext) Reindex.$(uext)
  $(Compile) $&

# Reorg, all
Reorg.$(uext):    BTDEFINE.INC Reorg.PAS                              \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  Restruct.$(uext) Reindex.$(uext)
  $(Compile) $&

# VReorg, all
VReorg.$(uext):   BTDEFINE.INC VReorg.PAS                             \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  Vrec.$(uext)     Restruct.$(uext) Reindex.$(uext)
  $(Compile) $&

# FixToVar, all
FixToVar.$(uext): BTDEFINE.INC FixToVar.PAS                           \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  Restruct.$(uext) Reindex.$(uext)  VReorg.$(uext)
  $(Compile) $&

# IsamTool, all
IsamTool.$(uext): BTDEFINE.INC IsamTool.PAS                           \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)
  $(Compile) $&

# TPAlloc, BP7 real mode
!if $d(RMode)
TPAlloc.$(uext):  BTDEFINE.INC TPAlloc.PAS
  $(Compile) $&
!endif

# MSort, BP7 real mode
!if $d(RMode)
MSort.$(uext):    BTDEFINE.INC MSort.PAS                              \
                  MSortEMS.INC MSortINF.INC                           \
                  EMSSupp.$(uext)
  $(Compile) $&
!endif

# MSortP, protected mode only
!if $d(PMode)
MSortP.$(uext):   BTDEFINE.INC MSortP.PAS
  $(Compile) $&
!endif

# CarrConv, not Delphi 2 or 3
!if !$d(Win32Mode)
CarrConv.$(uext): BTDEFINE.INC CarrConv.PAS
  $(Compile) $&
!endif

# DBImpExp, not Delphi 2 or 3
!if !$d(Win32Mode)
DBImpExp.$(uext): BTDEFINE.INC DBImpExp.PAS                           \
                  DBImpExp.IN1 DBImpExp.IN2                           \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  CarrConv.$(uext)
  $(Compile) $&
!endif

# TPCmd, BP7R and BP7P only
!if $d(DOSMode)
TPCmd.$(uext):    BTDEFINE.INC TPDEFINE.INC TPCmd.PAS
  $(Compile) $&
!endif

# Browser, BP7R and BP7P only
!if $d(DOSMode)
Browser.$(uext):  BTDEFINE.INC TPDEFINE.INC Browser.PAS               \
                  Browser.INC                                         \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  Vrec.$(uext)     TPCmd.$(uext)
  $(Compile) $& /m -o$(DirOProTPro) -u$(DirOProTPro) -i$(DirOProTPro)
!endif

# LowBrows, all
LowBrows.$(uext): BTDEFINE.INC BRDefOpt.INC LowBrows.PAS              \
                  BRLISAM.INC                                         \
                  BTBase.$(uext)   BTIsBase.$(uext) Filer.$(uext)     \
                  Vrec.$(uext)
  $(Compile) $&

# MedBrows, all
MedBrows.$(uext): BTDEFINE.INC BRDefOpt.INC MedBrows.PAS              \
                  LowBrows.$(uext)
  $(Compile) $&

# HiBrows, all
HiBrows.$(uext):  BTDEFINE.INC BRDefOpt.INC HiBrows.PAS               \
                  LowBrows.$(uext) MedBrows.$(uext)
  $(Compile) $&

# OPBrow, DOS mode only
!if $d(DOSMode)
OPBrow.$(uext):   BTDEFINE.INC BRDefOpt.INC OPBrow.PAS                \
                  OPBrow.ICD                                          \
                  DPMI.$(uext)                                        \
                  Filer.$(uext)    LowBrows.$(uext) MedBrows.$(uext)  \
                  HiBrows.$(uext)
  $(Compile) $& /m -i$(DirOProTPro) -o$(DirOProTPro) -u$(DirOProTPro)
!endif

# TVBrows, DOS mode only with WantTV
!if $d(DOSMode)
!if $d(WantTV)
TVBrows.$(uext):  BTDEFINE.INC BRDefOpt.INC TVBrows.PAS               \
                  Filer.$(uext)    LowBrows.$(uext) MedBrows.$(uext)  \
                  HiBrows.$(uext)
  $(Compile) $&
!endif
!endif

# WBrowser, BP7W mode only
!if $(compiler) == BP7W
WBrowser.$(uext): BTDEFINE.INC BRDefOpt.INC WBrowser.PAS              \
                  Filer.$(uext)    LowBrows.$(uext) MedBrows.$(uext)  \
                  HiBrows.$(uext)
  $(Compile) $&
!endif

# FVCBrows.R16, Delphi 1 only
!if $(compiler) == D1
FVCBrows.R16:     FVCBrows.RC
  $(R16Compiler) -fo$&.R16 $&
!endif

# FVCBrows.R32, Delphi 2 and 3 only
!if $d(Win32Mode)
FVCBrows.R32:     FVCBrows.RC
  $(R32Compiler) -fo$&.R32 $&
!endif

# FVCBrows, Delphi mode only
!if $d(DelphiMode)
FVCBrows.$(uext): BTDEFINE.INC BRDefOpt.INC FVCBrows.PAS              \
                  FVCBrows.RC                                         \
                  Filer.$(uext)    LowBrows.$(uext) MedBrows.$(uext)  \
                  HiBrows.$(uext)
  $(Compile) $&
!endif

# FVCReg, Delphi mode only
!if $d(DelphiMode)
FVCReg.$(uext):   BTDEFINE.INC BRDefOpt.INC FVCReg.PAS                \
                  FVCBrows.RC                                         \
                  FVCBrows.$(uext)
  $(Compile) $&
!endif


#------------------------------------------------------------ Packages

T555_R30.DPL : T555_R30.DPK                                           \
               Filer.DCU    LowBrows.DCU MedBrows.DCU HiBrows.DCU     \
               FVCBrows.DCU
  $(Compile) $&.DPK

T555_D30.DPL : T555_D30.DPK                                           \
               Filer.DCU    LowBrows.DCU MedBrows.DCU HiBrows.DCU     \
               FVCBrows.DCU FVCReg.DCU
  $(Compile) $&.DPK

T555_R40.DPL : T555_R40.DPK                                           \
               Filer.DCU    LowBrows.DCU MedBrows.DCU HiBrows.DCU     \
               FVCBrows.DCU
  $(Compile) $&.DPK

T555_D40.DPL : T555_D40.DPK                                           \
               Filer.DCU    LowBrows.DCU MedBrows.DCU HiBrows.DCU     \
               FVCBrows.DCU FVCReg.DCU
  $(Compile) $&.DPK

T555_R50.DPL : T555_R50.DPK                                           \
               Filer.DCU    LowBrows.DCU MedBrows.DCU HiBrows.DCU     \
               FVCBrows.DCU
  $(Compile) $&.DPK

T555_D50.DPL : T555_D50.DPK                                           \
               Filer.DCU    LowBrows.DCU MedBrows.DCU HiBrows.DCU     \
               FVCBrows.DCU FVCReg.DCU
  $(Compile) $&.DPK

#------------------------------------------------------- Demo Programs

$(DirDemos)\BigSort.EXE : $(DirDemos)\BIGSORT.PAS                     \
                          MSort.$(uext)
  cd $(DirDemos)
  $(Compile) $& -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\BINDLIST.EXE : $(DirDemos)\BINDLIST.PAS                   \
                           NWBase.$(uext)   NWBind.$(uext)            \
                           NWConn.$(uext) 
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\BTWDEMO.EXE : $(DirDemos)\BTWDEMO.PAS                     \
                          $(DirDemos)\BTWSTuff.INC                    \
                          Filer.$(uext)    Reorg.$(uext)              \
                          Rebuild.$(uext)  IsamTool.$(uext)           \
                          WBrowser.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\DB2ISAM.EXE : $(DirDemos)\DB2ISAM.PAS                     \
                          BTBase.$(uext)   BTIsBase.$(uext)           \
                          Filer.$(uext)    IsamTool.$(uext)           \
                          DBImpExp.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\DelDemo.EXE : $(DirDemos)\DelDemo.DPR                     \
                          $(DirDemos)\DelDemo1.PAS                    \
                          $(DirDemos)\DelDemo2.PAS                    \
                          $(DirDemos)\DelDemo3.PAS                    \
                          $(DirDemos)\DelDemo4.PAS                    \
                          $(DirDemos)\DelDemo5.PAS                    \
                          BTBase.$(uext)   BTIsBase.$(uext)           \
                          Filer.$(uext)    Rebuild.$(uext)            \
                          Reorg.$(uext)    VRebuild.$(uext)           \
                          VReorg.$(uext)                              \
                          FVCBrows.$(uext)
  cd $(DirDemos)
  $(Compile) $&.DPR /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\Isam2DB.EXE : $(DirDemos)\Isam2DB.PAS                     \
                          BTBase.$(uext)   BTIsBase.$(uext)           \
                          Filer.$(uext)    IsamTool.$(uext)           \
                          DBImpExp.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\MesExamp.EXE : $(DirDemos)\MesExamp.PAS                   \
                           NWBase.$(uext)   NWIPXSPX.$(uext)          \
                           NetBIOS.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\NBChat.EXE : $(DirDemos)\NBChat.PAS                       \
                         NWBase.$(uext)   NetBIOS.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\NBSend.EXE : $(DirDemos)\NBSend.PAS                       \
                         NWBase.$(uext)   NetBIOS.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase) 
  cd $(DirBase)

$(DirDemos)\NETDEMO.EXE : $(DirDemos)\NETDEMO.PAS                     \
                          BTBase.$(uext)   BTIsBase.$(uext)           \
                          Filer.$(uext)    Rebuild.$(uext)            \
                          Reorg.$(uext)    VRebuild.$(uext)           \
                          VReorg.$(uext)                              \
                          BROWSER.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase) -o$(DirOProTPro) -u$(DirOProTPro) -i$(DirOProTPro)
  cd $(DirBase)

$(DirDemos)\NETINFO.EXE : $(DirDemos)\NETINFO.PAS                     \
                          NWBase.$(uext)   NWConn.$(uext)             \
                          NWFile.$(uext)   NWMsg.$(uext)              \
                          NWTTS.$(uext)    NWPrint.$(uext)            \
                          Share.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase) -o$(DirOProTPro) -u$(DirOProTPro) -i$(DirOProTPro)
  cd $(DirBase)

$(DirDemos)\NISend.EXE : $(DirDemos)\NISend.PAS                       \
                         NWBase.$(uext)   NWIPXSPX.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\NSSend.EXE : $(DirDemos)\NSSend.PAS                       \
                         NWBase.$(uext)   NWIPXSPX.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\OPISDEMO.EXE : $(DirDemos)\OPISDEMO.PAS                   \
                           $(DirDemos)\OPISMAIN.PAS                   \
                           $(DirDemos)\OPISSTUF.INC                   \
                           Filer.$(uext)    Rebuild.$(uext)           \
                           OPBROW.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase) -o$(DirOProTPro) -u$(DirOProTPro) -i$(DirOProTPro)
  cd $(DirBase)

$(DirDemos)\OWDEMO.EXE : $(DirDemos)\OWDEMO.PAS                       \
                         Filer.$(uext)    Reorg.$(uext)               \
                         WBrowser.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\SimpDemo.EXE : $(DirDemos)\SimpDemo.PAS                   \
                           NWBase.$(uext)   NWConn.$(uext)            \
                           NWSema.$(uext)                             \
                           Filer.$(uext)    Reorg.$(uext)             \
                           Reindex.$(uext)  Rebuild.$(uext)           \
                           Browser.$(uext)
  cd $(DirDemos)
  $(Compile) $& -u$(DirBase) -i$(DirBase) -o$(DirOProTPro) -u$(DirOProTPro) -i$(DirOProTPro)
  cd $(DirBase)

$(DirDemos)\SPX2Way.EXE : $(DirDemos)\SPX2Way.PAS                     \
                          NWBase.$(uext)   NWIPXSPX.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\TRAFFIC.EXE : $(DirDemos)\TRAFFIC.PAS                     \
                          Filer.$(uext)
  cd $(DirDemos)
  $(Compile) $& -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\TTSFiler.EXE : $(DirDemos)\TTSFiler.PAS                   \
                           NWBase.$(uext)   NWTTS.$(uext)             \
                           Filer.$(uext)
  cd $(DirDemos)
  $(Compile) $& -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

$(DirDemos)\TVISDEMO.EXE : $(DirDemos)\TVISDEMO.PAS                   \
                           $(DirDemos)\TVISSTUF.INC                   \
                           Filer.$(uext)    Rebuild.$(uext)           \
                           TVBROWS.$(uext)
  cd $(DirDemos)
  $(Compile) $& /m -u$(DirBase) -i$(DirBase)
  cd $(DirBase)

