{*********************************************************}
{* FSSQL: Compiler options/directives include file       *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{NOTE: FFDEFINE.INC is included in all FlashFiler units; hence you can
       specify global compiler options here. FFDEFINE.INC is included
       *before* each unit's own required compiler options, so options
       specified here could be overridden by hardcoded options in the
       unit source file.}

{====Compiler options that can be changed====}
{$A+ Force alignment on word/dword boundaries}
{$S- No stack checking}

{====Determination of compiler (do NOT change)====}
{$IFDEF VER100}
{$DEFINE Delphi3}
{$DEFINE IsDelphi}
{$DEFINE ExprParserType1}
{$DEFINE CannotOverrideDispatch}
{$ENDIF}
{$IFDEF VER110}
{$DEFINE CBuilder3}
{$DEFINE ExprParserType1}
{$ENDIF}
{$IFDEF VER120}
{$DEFINE Delphi4}
{$DEFINE IsDelphi}
{$DEFINE IsNoVariantInt64}
{$DEFINE DCC4OrLater}
{$DEFINE HasStrictCardinal}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType2}
{$ENDIF}
{$IFDEF VER125}
{$DEFINE CBuilder4}
{$DEFINE DCC4OrLater}
{$DEFINE IsNoVariantInt64}
{$DEFINE HasStrictCardinal}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType2}
{$ENDIF}
{$IFDEF VER130}
{$DEFINE DCC4OrLater}
{$DEFINE DCC5OrLater}
{$DEFINE ProvidesDatasource}
{$DEFINE IsVerDataset1}
{$IFNDEF BCB}
{$DEFINE Delphi5}
{$DEFINE IsDelphi}
{$DEFINE IsNoVariantInt64}
{$DEFINE HasStrictCardinal}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType3}
{$ELSE}
{$DEFINE CBuilder5}
{$DEFINE IsVerDataset1}
{$DEFINE HasStrictCardinal}
{$DEFINE IsNoVariantInt64}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType3}
{$ENDIF}
{$ENDIF}
{$IFDEF VER140}
{$DEFINE DCC4OrLater}
{$DEFINE DCC5OrLater}
{$DEFINE DCC6OrLater}
{$DEFINE ProvidesDatasource}
{$DEFINE IsVerDataset2}
{$IFNDEF BCB}
{$DEFINE Delphi6}
{$DEFINE IsDelphi}
{$DEFINE IsVerDataset2}
{$DEFINE HasStrictCardinal}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType3}
{$ELSE}
{$DEFINE CBuilder6}
{$DEFINE HasStrictCardinal}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType3}
{$DEFINE IsVerDataset2}
{$ENDIF}
{$ENDIF}
{$IFDEF VER150}
{$DEFINE DCC4OrLater}
{$DEFINE DCC5OrLater}
{$DEFINE DCC6OrLater}
{$DEFINE DCC7OrLater}
{$DEFINE ProvidesDatasource}
{$DEFINE IsVerDataset2}
{$IFNDEF BCB}
{$DEFINE Delphi7}
{$DEFINE IsDelphi}
{$DEFINE IsVerDataset2}
{$DEFINE HasStrictCardinal}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType3}
{$ENDIF}
{$ENDIF}
{$IFDEF VER170}
{$DEFINE DCC4OrLater}
{$DEFINE DCC5OrLater}
{$DEFINE DCC6OrLater}
{$DEFINE DCC7OrLater}
{$DEFINE DCC2005Win32OrLater}
{$DEFINE ProvidesDatasource}
{$DEFINE IsVerDataset2}
{$IFNDEF BCB}
{$DEFINE Delphi2005Win32}
{$DEFINE IsDelphi}
{$DEFINE IsVerDataset2}
{$DEFINE HasStrictCardinal}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType3}
{$ENDIF}
{$ENDIF}
{$IFDEF VER180}
{$DEFINE DCC4OrLater}
{$DEFINE DCC5OrLater}
{$DEFINE DCC6OrLater}
{$DEFINE DCC7OrLater}
{$DEFINE DCC2005Win32OrLater}
{$DEFINE DCC2006Win32OrLater}
{$DEFINE ProvidesDatasource}
{$DEFINE IsVerDataset3}
{$IFNDEF BCB}
{$DEFINE Delphi2006Win32}
{$DEFINE IsDelphi}
{$DEFINE IsVerDataset3}
{$DEFINE HasStrictCardinal}
{$DEFINE ResizePersistFields}
{$DEFINE ExprParserType3}
{$ENDIF}
{$ENDIF}


{$IFDEF DCC5OrLater}
{$UNDEF UsesBDE}
{$ELSE}
{$DEFINE UsesBDE}
{$ENDIF}

{====Global fixed compiler options (do NOT change)====}
{---Delphi 3---}
{$IFDEF Delphi3}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$ENDIF}
{---Delphi 4---}
{$IFDEF Delphi4}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$ENDIF}
{---Delphi 5---}
{$IFDEF Delphi5}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$ENDIF}
{---Delphi 6---}
{$IFDEF Delphi6}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$ENDIF}
{---Delphi 7---}
{$IFDEF Delphi7}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNIT_DEPRECATED OFF}
{$ENDIF}
{---Delphi 2005-Win32--}
{$IFDEF Delphi2005Win32}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNIT_DEPRECATED OFF}
{$ENDIF}
{---Delphi 2006-Win32--}
{$IFDEF Delphi2006Win32}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNIT_DEPRECATED OFF}
{$ENDIF}
{---C++Builder 3---}
{$IFDEF CBuilder3}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$OBJEXPORTALL ON}
{$ENDIF}
{---C++Builder 4---}
{$IFDEF CBuilder4}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$OBJEXPORTALL ON}
{$ENDIF}
{---C++Builder 5---}
{$IFDEF CBuilder5}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$OBJEXPORTALL ON}
{$ENDIF}
{---C++Builder 6---}
{$IFDEF CBuilder6}
{$B- Incomplete boolean evaluation}
{$H+ Long string support}
{$J+ Writeable typed constants}
{$P- No open string parameters}
{$T- No type-checked pointers}
{$V- No var string checking}
{$X+ Extended syntax}
{$Z1 Enumerations are word sized}
{$OBJEXPORTALL ON}
{$ENDIF}

{====General defines=================================================}

{Activate the following define to include extra code to get rid of all
 hints and warnings. Parts of FlashFiler are written in such a way
 that the hint/warning algorithms of the Delphi compilers are
 fooled and report things like variables being used before
 initialisation and so on when in reality the problem does not exist.}
{$DEFINE DefeatWarnings}

{Activate the following define to enable safer text comparisons.
AnsiCompareText has problems comparing text for some locals that cause
this define to be necessary. For instance, in the Norwegian locale,
BAALAM <> BaAlam when using AnsiCompareText, instead AnsiCompareText
should report that the values are equal.
Enabling this define will cause AnsiLowerText to be performed on
both input strings before AnsiCompareText is called.}
{.$DEFINE SafeAnsiCompare}

{====CLIENT specific defines=========================================}

{Activate the following define to look to the Registry for Client
 configuration information.}
{$DEFINE UseRegistryConfig}

{Activate the following define to look to the FF2.INI file for Client
 configuration information}
{.$DEFINE UseINIConfig}

{====SERVER specific defines=========================================}

{Activate the following define to include the tracing facility.}
{.$DEFINE Tracing}

{Activate the following define to allow rebuild operations (reindex,
 pack, restructure) to run in a separate thread from the main server
 process.}
{$DEFINE ThreadedRebuilds}


{Activate the following define to enable some debugging code within the
 FSSQL Server. }
{.$DEFINE FF_DEBUG}
{.$DEFINE FF_DEBUG_THREADS}


{.$DEFINE USETeDEBUG}

{-------- !! DO NOT CHANGE DEFINES BELOW THIS LINE !! --------}


