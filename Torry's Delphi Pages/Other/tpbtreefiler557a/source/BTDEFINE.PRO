(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *   Rob Roberts  robr@pcisys.net
 *
 * ***** END LICENSE BLOCK ***** *)

{---Conditional defines that affect B-Tree Filer units---}


{===B-Tree Filer defines=============================================}
{$DEFINE NoNet}
{.$DEFINE Novell}
{.$DEFINE MsNet}
{-Valid network interfaces. One or more must be defined, but NoNet
  may not be selected except by itself. Novell is not valid for
  32-bit Delphi. For a real network, our recommendation is to always
  use MsNet.}

{$IFDEF Novell}
{.$DEFINE SupportVLM}
{-If compiling for Novell NetWare, defining SupportVLM will link in
  the relevant NWXXXX units to support VLMs as well as NETX. This is
  not an option for the DLL.}
{$ENDIF}

{$IFNDEF DPMI}
{$IFNDEF Windows}
{$IFNDEF Win32}
{$DEFINE UseEMSHeap}
{-Adds code to the FILER unit to store page buffers in EMS in real
  mode only. EMS cannot be used under Windows or DPMI.}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF UseEMSHeap}
{.$DEFINE EMSDisturbance}
{-Adds code to save and restore the EMS page mapping.}
{$ENDIF}

{.$DEFINE InitAllUnits}
{-Activate this define to cause all B-Tree Filer units to have an
  initialization block, even if only an empty one. This works around
  a bug in some very early versions of Borland's Turbo Debugger.}

{$IFDEF Win32}
{$DEFINE SuppressWarnings}
{$ENDIF}
{-Activate this define to force Delphi 2.0 and 3.0  to display all
  Hints and Warnings}

{-Note: the defines for DebugEMSHeap, NoErrorCheckEMSHeap,
        ManualInitEMSHeap, UseTPEMS, and UseOPEMS have been moved into
        EMSHEAP.PAS, which is the only unit they affect.}

{====================================================================}


{===Common defines between static/dynamic linked B-Tree Filer========}

{$IFNDEF NoNet}
{.$DEFINE LockBeforeRead}
{-Automatically locks any file section before reading it, then
  unlocks. May be needed to avoid a bug in some versions of the
  NetWare NETX shell.}
{$ENDIF}

{$DEFINE LengthByteKeys}
{.$DEFINE AsciiZeroKeys}
{-One of LengthByteKeys or AsciiZeroKeys must be defined, but not
  both. LengthByteKeys causes B-Tree Filer to store Turbo Pascal style
  strings in the index file. AsciiZeroKeys causes B-Tree Filer to
  store C-style ASCIIZ strings in the index file.}

{.$DEFINE UseTPCRT}
{.$DEFINE UseOPCRT}
{-Either UseTPCRT or UseOPCRT may be defined, but not both. These
  defines affect the BROWSER unit only (and programs using BROWSER,
  for example NETDEMO). Don't activate either one if the program uses
  neither the TPCRT nor OPCRT units from Turbo Professional and
  Object Professional, respectively.}

{====================================================================}

(********************************************************************)
(********************************************************************)
{Don't change anything beyond this point}

{The following define allows extensions to B-Tree Filer to detect the
 new syntax of versions 5.2 and later.}

{$DEFINE BTree52}

{The following define allows extensions to B-Tree Filer to detect the
 new syntax of versions 5.4 and later.}

{$DEFINE BTree54}

{--Define whether a DPMI or Windows compiler is used}
  {$IFDEF Windows}
    {$DEFINE DPMIOrWnd}
  {$ENDIF}
  {$IFDEF DPMI}
    {$DEFINE DPMIOrWnd}
  {$ENDIF}
  {$IFDEF Win32}
    {$DEFINE DPMIOrWnd}
  {$ENDIF}

{--Define the syntax of BTInitIsam to be used}
  {$IFDEF Windows}
    {$DEFINE UseWindowsInit}
  {$ENDIF}
  {$IFDEF Win32}
    {$DEFINE UseWindowsInit}
  {$ENDIF}

{--Check for .NET}                                             {!!.57}
  {$IFDEF CLR}                                                 {!!.57}
    !! B-Tree Filer does not support .NET                      {!!.57}
  {$ENDIF}                                                     {!!.57}

{--Define whether using Delphi}
  {$IFDEF VER80}
    {$DEFINE UsingDelphi}
  {$ENDIF}
  {$IFDEF VER90}
    {$DEFINE UsingDelphi}
  {$ENDIF}
  {$IFDEF VER100}
    {$DEFINE UsingDelphi}
  {$ENDIF}
  {$IFDEF VER120}
    {$DEFINE UsingDelphi}
  {$ENDIF}
  {$IFDEF VER130}
    {$DEFINE UsingDelphi}
  {$ENDIF}
  {$IFDEF VER140}                                              {!!.56}
    {$DEFINE UsingDelphi}                                      {!!.56}
  {$ENDIF}                                                     {!!.56}
  {$IFDEF VER150}                                              {!!.56}
    {$DEFINE UsingDelphi}                                      {!!.56}
  {$ENDIF}                                                     {!!.56}
  {$IFDEF VER170}                                              {!!.57}
   {$DEFINE UsingDelphi}                                       {!!.57}
  {$ENDIF}                                                     {!!.57}
  {$IFDEF VER180}                                              {!!.57a}
   {$DEFINE UsingDelphi}                                       {!!.57a}
  {$ENDIF}                                                     {!!.57a}

{--Define whether overlays can be allowed}
  {$IFDEF MSDOS}
    {$DEFINE CanAllowOverlays}
  {$ELSE}
    {$UNDEF CanAllowOverlays}
  {$ENDIF}

{--Test the definition of the net interfaces}
  {$IFDEF Novell}
    {$DEFINE RealNetDefined}
  {$ENDIF}
  {$IFDEF MsNet}
    {$DEFINE RealNetDefined}
  {$ENDIF}

  {$IFDEF NoNet}
    {$IFDEF RealNetDefined}
      !! ERROR: You may not define NoNet and any other Net simultaneously
    {$ENDIF}
  {$ELSE}
    {$IFNDEF RealNetDefined}
      !! ERROR: You must define either NoNet or at least one real network
    {$ENDIF}
  {$ENDIF}
  {$UNDEF RealNetDefined}

{--Test the XXKeys defines}
  {$IFDEF LengthByteKeys}
    {$IFDEF ASCIIZeroKeys}
      !! ERROR: You may not define both LengthByteKeys and AsciiZeroKeys
    {$ENDIF}
  {$ELSE}
    {$IFNDEF ASCIIZeroKeys}
      !! ERROR: You must define either LengthByteKeys or AsciiZeroKeys
    {$ENDIF}
  {$ENDIF}

{--Test the UseXXCrt defines}
  {$IFDEF UseTPCRT}
    {$IFDEF UseOPCRT}
      !! ERROR: You may not define both UseTPCrt and UseOPCrt
    {$ENDIF}
  {$ENDIF}

{--Test the FILER.MAK directives}
  {$IFDEF TProOnly}
    {$IFNDEF UseTPCrt}
      !! ERROR: You must define UseTPCRT if compiling with TPro
    {$ENDIF}
  {$ENDIF}
  {$IFDEF OProOnly}
    {$IFNDEF UseOPCrt}
      !! ERROR: You must define UseOPCRT if compiling with OPro
    {$ENDIF}
  {$ENDIF}

{--Test for Win32 exclusions}

  {$IFDEF Win32}
    {$IFDEF Novell}
      !! ERROR: Novell network type is not available for 32-bit Delphi
    {$ENDIF}
    {$IFDEF UseTPCRT}
      {$UNDEF UseTPCrt}
    {$ENDIF}
    {$IFDEF UseOPCRT}
      {$UNDEF UseOPCrt}
    {$ENDIF}
  {$ENDIF}

{--Win32 hints/warnings}

  {$IFDEF Win32}
    {$IFDEF SuppressWarnings}
      {$WARNINGS OFF}
      {$HINTS OFF}
    {$ELSE}
      {$WARNINGS ON}
      {$HINTS ON}
    {$ENDIF}
  {$ENDIF}

{===Compiler options (not to be changed)=============================}
{$IFDEF VER70} {Borland Pascal 7.0}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$I-} {suppress I/O checking}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$S-} {stack checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$ENDIF}

{$IFDEF VER80} {Delphi 1.0}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$G+} {generate 80286 code}
{$I-} {suppress I/O checking}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$S-} {stack checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi1}
{$DEFINE Delphi1Plus}
{$ENDIF}

{$IFDEF VER90} {Delphi 2.0}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$H+} {long string support}
{$I-} {suppress I/O checking}
{$J+} {writeable typed constants}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi2}
{$DEFINE Delphi1Plus}
{$DEFINE Delphi2Plus}
{$ENDIF}

{$IFDEF VER100} {Delphi 3.0}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$H+} {long string support}
{$I-} {suppress I/O checking}
{$J+} {writeable typed constants}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi3}
{$DEFINE Delphi1Plus}
{$DEFINE Delphi2Plus}
{$DEFINE Delphi3Plus}
{$ENDIF}

{$IFDEF VER120} {Delphi 4.0}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$H+} {long string support}
{$I-} {suppress I/O checking}
{$J+} {writeable typed constants}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi4}
{$DEFINE Delphi1Plus}
{$DEFINE Delphi2Plus}
{$DEFINE Delphi3Plus}
{$DEFINE Delphi4Plus}
{$ENDIF}

{$IFDEF VER130} {Delphi 5.0}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$H+} {long string support}
{$I-} {suppress I/O checking}
{$J+} {writeable typed constants}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi5}
{$DEFINE Delphi1Plus}
{$DEFINE Delphi2Plus}
{$DEFINE Delphi3Plus}
{$DEFINE Delphi4Plus}
{$DEFINE Delphi5Plus}
{$ENDIF}

{$IFDEF VER140} {Delphi 6.0}                               {new !!.56}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$H+} {long string support}
{$I-} {suppress I/O checking}
{$J+} {writeable typed constants}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi6}
{$DEFINE Delphi1Plus}
{$DEFINE Delphi2Plus}
{$DEFINE Delphi3Plus}
{$DEFINE Delphi4Plus}
{$DEFINE Delphi5Plus}
{$DEFINE Delphi6Plus}
{$ENDIF}

{$IFDEF VER150} {Delphi 7.0}                               {new !!.56}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$H+} {long string support}
{$I-} {suppress I/O checking}
{$J+} {writeable typed constants}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi7}
{$DEFINE Delphi1Plus}
{$DEFINE Delphi2Plus}
{$DEFINE Delphi3Plus}
{$DEFINE Delphi4Plus}
{$DEFINE Delphi5Plus}
{$DEFINE Delphi6Plus}
{$DEFINE Delphi7Plus}
{$ENDIF}

{$IFDEF VER170} {Delphi 2005}                                  {!!.57}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$H+} {long string support}
{$I-} {suppress I/O checking}
{$J+} {writeable typed constants}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi2005}
{$DEFINE Delphi1Plus}
{$DEFINE Delphi2Plus}
{$DEFINE Delphi3Plus}
{$DEFINE Delphi4Plus}
{$DEFINE Delphi5Plus}
{$DEFINE Delphi6Plus}
{$DEFINE Delphi7Plus}
{$DEFINE Delphi2005Plus}
{$ENDIF}

{$IFDEF VER180} {Delphi 2006}                                  {!!.57a}
{$A-} {align data on byte boundaries}
{$B-} {short circuit boolean evaluation}
{$H+} {long string support}
{$I-} {suppress I/O checking}
{$J+} {writeable typed constants}
{$P-} {do not allow open string parameters}
{$Q-} {overflow checking off}
{$R-} {range checking off}
{$T-} {no type checked pointers with @}
{$V-} {no var string checking}
{$X+} {extended syntax on}
{$DEFINE Delphi2006}
{$DEFINE Delphi1Plus}
{$DEFINE Delphi2Plus}
{$DEFINE Delphi3Plus}
{$DEFINE Delphi4Plus}
{$DEFINE Delphi5Plus}
{$DEFINE Delphi6Plus}
{$DEFINE Delphi7Plus}
{$DEFINE Delphi2005Plus}
{$DEFINE Delphi2006Plus}
{$ENDIF}

