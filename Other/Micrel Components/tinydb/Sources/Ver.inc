(******************************************************************************)
(* Check the the version of Delphi or BCB.                                    *)
(******************************************************************************)
(* Usage:   Add the following line to the top of your unit file:              *)
(*   {$I Ver.inc}                                                             *)
(******************************************************************************)
(*                                                                            *)
(* Complete Boolean Evaluation compiler directive is turned off by including  *)
(*   this file.                                                               *)
(* The $ObjExportAll directive is turned on if compiling with C++Builder 3 or *)
(*   higher.  This is required for Delphi components built in Builder with    *)
(*   run-time packages.                                                       *)
(*                                                                            *)
(* Here is a brief explanation of what each of the defines mean:              *)
(* VER_WIN16            : Compilation target is 16-bit Windows                *)
(* VER_WIN32            : Compilation target is 32-bit Windows                *)
(* VER_USEDEFSHLOBJ     : The SHLOBJ.PAS version contains no none errors.     *)
(*                        Delphi 2.0x and C++Builder 1.0x shipped with a      *)
(*                        version of SHLOBJ.PAS that had many nasty errors.   *)
(*                        See my web site in the Hints section for how to fix *)
(* VER_NO_COM_CLEANUP   : The version of the compiler being used does not     *)
(*                        require COM objects to be released; it is done      *)
(*                        automatically when they go "out of scope".          *)
(* VER_NO_DSGNINTF      : Delphi 6 pretty much kills off the DsgnIntf unit    *)
(*                        for good. Split into a couple of new units.         *)
(* VER_DESIGNERSELECTIONS: IDesignerSelections replaced TDesignerSelectionList*)
(* VER_IPROPERTY        : IProperty introduced for design-time stuff.         *)
(* VER_COMPILER_1       : Delphi 1.0 is the compiler.  Note that C++B 1.0     *)
(*                        does NOT cause this to be defined.  It is really    *)
(*                        the 2.0 compiler.                                   *)
(* VER_COMPILER_1_UP    : Delphi 1.0x or higher, or C++B 1.0x or higher is    *)
(*                        the compiler.                                       *)
(* VER_COMPILER_2       : Delphi 2.0x or C++B 1.0x is the compiler.           *)
(* VER_COMPILER_2_UP    : Delphi 2.0x or higher, or C++B 1.0x or higher is    *)
(*                        the compiler.                                       *)
(* VER_COMPILER_3       : Delphi 3.0x or C++B 3.0x is the compiler.           *)
(* VER_COMPILER_3_UP    : Delphi 3.0x or higher, or C++B 3.0x or higher is    *)
(*                        the compiler.                                       *)
(* VER_COMPILER_4       : Delphi 4.0x or C++B 4.0x is the compiler.           *)
(* VER_COMPILER_4_UP    : Delphi 4.0x or higher, or C++B 4.0x or higher is    *)
(*                        the compiler.                                       *)
(* VER_COMPILER_5       : Delphi 5.0x or C++B 5.0x is the compiler.           *)
(* VER_COMPILER_5_UP    : Delphi 5.0x or higher, or C++B 5.0x or higher is    *)
(*                        the compiler.                                       *)
(* VER_COMPILER_6       : Delphi 6.0x or C++B 6.0x is the compiler.           *)
(* VER_COMPILER_6_UP    : Delphi 6.0x or higher, or C++B 6.0x or higher is    *)
(*                        the compiler.                                       *)
(* VER_COMPILER_7       : Delphi 7.0x or C++B 7.0x is the compiler.           *)
(* VER_COMPILER_7_UP    : Delphi 7.0x or higher, or C++B 7.0x or higher is    *)
(*                        the compiler.                                       *)
(* VER_UNKNOWN_COMPILER : No sense could be made of the compiliation          *)
(*                        environment.                                        *)
(* VER_BCB              : Any version of C++B is being used.                  *)
(* VER_BCB_1            : C++B v1.0x is being used.                           *)
(* VER_BCB_3            : C++B v3.0x is being used.                           *)
(* VER_BCB_3_UP         : C++B v3.0x or higher is being used.                 *)
(* VER_BCB_4            : C++B v4.0x is being used.                           *)
(* VER_BCB_4_UP         : C++B v4.0x or higher is being used.                 *)
(* VER_BCB_5            : C++B v5.0x is being used.                           *)
(* VER_BCB_5_UP         : C++B v5.0x or higher is being used.                 *)
(* VER_BCB_6            : C++B v6.0x is being used.                           *)
(* VER_BCB_6_UP         : C++B v6.0x or higher is being used.                 *)
(* VER_DELPHI           : Any version of Delphi is being used.                *)
(* VER_DELPHI_1         : Delphi v1.0x is being used.                         *)
(* VER_DELPHI_2         : Delphi v2.0x is being used.                         *)
(* VER_DELPHI_2_UP      : Delphi v2.0x or higher is being used.               *)
(* VER_DELPHI_3         : Delphi v3.0x is being used.                         *)
(* VER_DELPHI_3_UP      : Delphi v3.0x or higher is being used.               *)
(* VER_DELPHI_4         : Delphi v4.0x is being used.                         *)
(* VER_DELPHI_4_UP      : Delphi v4.0x or higher is being used.               *)
(* VER_DELPHI_5         : Delphi v5.0x is being used.                         *)
(* VER_DELPHI_5_UP      : Delphi v5.0x or higher is being used.               *)
(* VER_DELPHI_6         : Delphi v6.0x is being used.                         *)
(* VER_DELPHI_6_UP      : Delphi v6.0x or higher is being used.               *)
(* VER_DELPHI_7         : Delphi v7.0x is being used.                         *)
(* VER_DELPHI_7_UP      : Delphi v7.0x or higher is being used.               *)
(******************************************************************************)


{ All components rely on complete boolean eval compiler directive set off. }
{$B-}

{$IFDEF WIN32}
  {$DEFINE VER_WIN32}
{$ELSE}
  {$DEFINE VER_WIN16}
{$ENDIF}

{$IFDEF VER240} //Delphi XE3
  {$DEFINE VER_COMPILER_17}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_17}
{$ENDIF}

{$IFDEF VER230} //Delphi XE2
  {$DEFINE VER_COMPILER_16}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_16}
{$ENDIF}

{$IFDEF VER220} //Delphi XE
  {$DEFINE VER_COMPILER_15}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_15}
{$ENDIF}

{$IFDEF VER210} //Delphi 2010
  {$DEFINE VER_COMPILER_14}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_14}
{$ENDIF}

{$IFDEF VER200} //Delphi 2009
  {$DEFINE VER_COMPILER_12}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_12}
{$ENDIF}

{$IFDEF VER185} //Delphi 2007
  {$DEFINE VER_COMPILER_11}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_11}
{$ENDIF}

{$IFDEF VER180} //Delphi 2006/Turbo
  {$DEFINE VER_COMPILER_10}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_10}
{$ENDIF}

{$IFDEF VER170} //Delphi 2005
  {$DEFINE VER_COMPILER_9}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_9}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE VER_COMPILER_7}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_7}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE VER_COMPILER_6}
  {$IFDEF BCB}
    {$DEFINE VER_BCB}
    {$DEFINE VER_BCB_6}
  {$ELSE}
    {$DEFINE VER_DELPHI}
    {$DEFINE VER_DELPHI_6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE VER_COMPILER_5}
  {$IFDEF BCB}
    {$DEFINE VER_BCB}
    {$DEFINE VER_BCB_5}
  {$ELSE}
    {$DEFINE VER_DELPHI}
    {$DEFINE VER_DELPHI_5}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE VER_COMPILER_4}
  {$DEFINE VER_BCB}
  {$DEFINE VER_BCB_4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE VER_COMPILER_4}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_4}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE VER_COMPILER_3}
  {$DEFINE VER_BCB}
  {$DEFINE VER_BCB_3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE VER_COMPILER_3}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_3}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE VER_COMPILER_2}  { C++B v1 compiler is really v2 }
  {$DEFINE VER_BCB}
  {$DEFINE VER_BCB_1}
  {.$DEFINE VER_USEDEFSHLOBJ} { C++B 1 has the correct SHLOBJ.H, but
                                SHLOBJ.PAS has errors so this isn't defined }
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE VER_COMPILER_2}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_2}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE VER_COMPILER_1}
  {$DEFINE VER_DELPHI}
  {$DEFINE VER_DELPHI_1}
{$ENDIF}

{$IFNDEF VER_BCB}
  {$IFNDEF VER_DELPHI}
    { Don't know what the hell it is.  Could be new version, or could be old BP. }
    {$DEFINE VER_UNKNOWN_COMPILER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER_COMPILER_1}
  {$DEFINE VER_COMPILER_1_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_2}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_3}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_4}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_5}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_6}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_7}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_9}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
  {$DEFINE VER_COMPILER_9_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_10}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
  {$DEFINE VER_COMPILER_9_UP}
  {$DEFINE VER_COMPILER_10_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_11}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
  {$DEFINE VER_COMPILER_9_UP}
  {$DEFINE VER_COMPILER_10_UP}
  {$DEFINE VER_COMPILER_11_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_12}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
  {$DEFINE VER_COMPILER_9_UP}
  {$DEFINE VER_COMPILER_10_UP}
  {$DEFINE VER_COMPILER_11_UP}
  {$DEFINE VER_COMPILER_12_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_14}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
  {$DEFINE VER_COMPILER_9_UP}
  {$DEFINE VER_COMPILER_10_UP}
  {$DEFINE VER_COMPILER_11_UP}
  {$DEFINE VER_COMPILER_12_UP}
  {$DEFINE VER_COMPILER_14_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_15}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
  {$DEFINE VER_COMPILER_9_UP}
  {$DEFINE VER_COMPILER_10_UP}
  {$DEFINE VER_COMPILER_11_UP}
  {$DEFINE VER_COMPILER_12_UP}
  {$DEFINE VER_COMPILER_14_UP}
  {$DEFINE VER_COMPILER_15_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_16}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
  {$DEFINE VER_COMPILER_9_UP}
  {$DEFINE VER_COMPILER_10_UP}
  {$DEFINE VER_COMPILER_11_UP}
  {$DEFINE VER_COMPILER_12_UP}
  {$DEFINE VER_COMPILER_14_UP}
  {$DEFINE VER_COMPILER_15_UP}
  {$DEFINE VER_COMPILER_16_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_17}
  {$DEFINE VER_COMPILER_1_UP}
  {$DEFINE VER_COMPILER_2_UP}
  {$DEFINE VER_COMPILER_3_UP}
  {$DEFINE VER_COMPILER_4_UP}
  {$DEFINE VER_COMPILER_5_UP}
  {$DEFINE VER_COMPILER_6_UP}
  {$DEFINE VER_COMPILER_7_UP}
  {$DEFINE VER_COMPILER_9_UP}
  {$DEFINE VER_COMPILER_10_UP}
  {$DEFINE VER_COMPILER_11_UP}
  {$DEFINE VER_COMPILER_12_UP}
  {$DEFINE VER_COMPILER_14_UP}
  {$DEFINE VER_COMPILER_15_UP}
  {$DEFINE VER_COMPILER_16_UP}
  {$DEFINE VER_COMPILER_17_UP}
{$ENDIF}

{$IFDEF VER_DELPHI_2}
  {$DEFINE VER_DELPHI_2_UP}
{$ENDIF}

{$IFDEF VER_DELPHI_3}
  {$DEFINE VER_DELPHI_2_UP}
  {$DEFINE VER_DELPHI_3_UP}
{$ENDIF}

{$IFDEF VER_DELPHI_4}
  {$DEFINE VER_DELPHI_2_UP}
  {$DEFINE VER_DELPHI_3_UP}
  {$DEFINE VER_DELPHI_4_UP}
{$ENDIF}

{$IFDEF VER_DELPHI_5}
  {$DEFINE VER_DELPHI_2_UP}
  {$DEFINE VER_DELPHI_3_UP}
  {$DEFINE VER_DELPHI_4_UP}
  {$DEFINE VER_DELPHI_5_UP}
{$ENDIF}

{$IFDEF VER_DELPHI_6}
  {$DEFINE VER_DELPHI_2_UP}
  {$DEFINE VER_DELPHI_3_UP}
  {$DEFINE VER_DELPHI_4_UP}
  {$DEFINE VER_DELPHI_5_UP}
  {$DEFINE VER_DELPHI_6_UP}
{$ENDIF}

{$IFDEF VER_DELPHI_7}
  {$DEFINE VER_DELPHI_2_UP}
  {$DEFINE VER_DELPHI_3_UP}
  {$DEFINE VER_DELPHI_4_UP}
  {$DEFINE VER_DELPHI_5_UP}
  {$DEFINE VER_DELPHI_6_UP}
  {$DEFINE VER_DELPHI_7_UP}
{$ENDIF}

{$IFDEF VER_BCB_3}
  {$DEFINE VER_BCB_3_UP}
{$ENDIF}

{$IFDEF VER_BCB_4}
  {$DEFINE VER_BCB_3_UP}
  {$DEFINE VER_BCB_4_UP}
{$ENDIF}

{$IFDEF VER_BCB_5}
  {$DEFINE VER_BCB_3_UP}
  {$DEFINE VER_BCB_4_UP}
  {$DEFINE VER_BCB_5_UP}
{$ENDIF}

{$IFDEF VER_BCB_6}
  {$DEFINE VER_BCB_3_UP}
  {$DEFINE VER_BCB_4_UP}
  {$DEFINE VER_BCB_5_UP}
  {$DEFINE VER_BCB_6_UP}
{$ENDIF}

{$IFDEF VER_COMPILER_3_UP}
  {$DEFINE VER_NO_COM_CLEANUP}
  {$DEFINE VER_USEDEFSHLOBJ} { Only D3+ and C++B 3+ have no errors in SHLOBJ }
{$ENDIF}

{$IFDEF VER_BCB_3_UP}
  // C++Builder requires this if you use Delphi components in run-time packages.
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER_COMPILER_6_UP}
  // Delphi 6 pretty much kills off the DsgnIntf unit for good.
  {$DEFINE VER_NO_DSGNINTF}
  {$DEFINE VER_DESIGNERSELECTIONS}
  {$DEFINE VER_IPROPERTY}
{$ENDIF}