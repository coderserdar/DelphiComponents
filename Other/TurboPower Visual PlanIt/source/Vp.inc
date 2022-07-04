{*********************************************************}
{*                     VP.INC 1.03                       *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (c) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{Conditional defines that affect compilation}

{$Q-} {Overflow Checking}
{$R-} {Range-Checking}
{$S-} {Stack-Overflow Checking}
{$V-} {Var-String Checking}
{$T-} {No type-checked pointers}
{$X+} {Extended syntax}
{$P-} {No open string parameters}
{$B-} {Incomplete Boolean evaluation}
{$J+} {Writable constants}
{$H+} {Huge strings}
{$A+} {Word Align Data}
{$I+} {Input/Output-Checking}

{-Invalid Platform Defines----------------------------------------------}
{ Visual PlanIt only supports D3 - D6 and BCB3 - BCB6 }
{$IFDEF VER80} {Delphi 1}
  {$DEFINE INVALID_PLATFORM}
{$ENDIF}

{$IFDEF VER90} {Delphi 2}
  {$DEFINE INVALID_PLATFORM}
{$ENDIF}

{$IFDEF VER93} {BCB1}
  {$DEFINE INVALID_PLATFORM}
{$ENDIF}

{$IFDEF INVALID_PLATFORM}
  Error!!! Visual PlanIt supports Delphi 3/BCB3 and above ONLY.
{$ENDIF}

{-C++Builder General Defines--------------------------------------------}
{$IFDEF VER110} {BCB3}
  {$DEFINE CBUILDER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} {BCB4}
  {$DEFINE CBUILDER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER135} {BCB5}
  {$DEFINE CBuilder}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER145} {BCB6}
  {$DEFINE CBuilder}
  {$ObjExportAll On}
{$ENDIF}

{-Version Test----------------------------------------------------------}
{$IFNDEF INVALID_PLATFORM} {D1, D2, or BCB1}
  {$DEFINE VERSION3}
{$ENDIF}

{$IFDEF VERSION3}
 {$IFNDEF VER100}     {Delphi 3}
  {$IFNDEF VER110}    {BCB 3}
   {$DEFINE VERSION4} {Delphi 4.0 or higher}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION4}
 {$IFNDEF VER120}     {Delphi 4}
  {$IFNDEF VER125}    {BCB 4}
   {$DEFINE VERSION5} {Delphi 5.0 or higher}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION5}
 {$IFNDEF VER130}     {Delphi 5}
  {$IFNDEF VER135}    {BCB 5}
   {$DEFINE VERSION6} {Delphi 6.0 or higher}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION6}
 {$IFNDEF VER140}     {Delphi 6}
  {$IFNDEF VER145}    {BCB 6}
   {$DEFINE VERSION7} {Delphi 7.0 or higher}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}
