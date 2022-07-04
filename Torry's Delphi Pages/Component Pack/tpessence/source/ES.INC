
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
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{Conditional defines that affect compilation}

{General define indicating use under C++ Builder}
{$IFDEF VER93}
  {$DEFINE CBuilder}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE CBuilder}
  {$ObjExportAll On}                                                   {!!.04}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE CBuilder}
  {$ObjExportAll On}                                                   {!!.06}
{$ENDIF}

{$IFNDEF VER80}
 {$IFNDEF VER90}
  {$IFNDEF VER93}
    {$DEFINE VERSION3} { Delphi 3.0 or BCB 3.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
 {$IFNDEF VER100}     {Delphi 3}
  {$IFNDEF VER110}    {BCB 3}
   {$DEFINE VERSION4} {Delphi 4.0 or higher}                           {!!.06}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER130}               { Delphi 5 & C++ Builder 5 }
  {$DEFINE VERSION5}
 {$IFDEF BCB}                 { C++ Builder 5 }
  {$DEFINE CBuilder}
  {$ObjExportAll On}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER140}               { Delphi 6 }                           {!!.09}
  {$DEFINE VERSION6}
    {$DEFINE AboveD6}                                                    {!!.11}
    {$IFDEF BCB}
     {$DEFINE CBuilder}
     {$ObjExportAll On}
    {$ENDIF}
{$ENDIF}

{$IFDEF VER150}               {Above Delphi 6}                           {!!.11}
  {$DEFINE AboveD6}
{$ENDIF}

{$IFDEF CBuilder)
  {$A+} {Word Align Data}
{$ELSE}
  {$IFDEF Win32}
    {$A+} {Word Align Data}
  {$ELSE}
    {$A-} {Byte Align Data}
  {$ENDIF}
{$ENDIF}

{$Q-} {Overflow Checking}
{$R-} {Range-Checking}
{$S-} {Stack-Overflow Checking}
{$V-} {Var-String Checking}

{$IFNDEF Win32}
  {$F-} {Force Far Calls}
  {$K-} {Smart Callbacks}
{$ENDIF}

{$IFDEF Win32}
  {$H+} {Long strings}
{$ENDIF}

{$IFDEF Win32}                                                 {!!.05}
  {$DEFINE NeedMouseWheel}                                     {!!.05}
  {$IFDEF Ver120}                                              {!!.05}
    {$UNDEF NeedMouseWheel}                                    {!!.05}
  {$ENDIF}                                                     {!!.05}
{$ENDIF}                                                       {!!.05}

{ turn off the following warnings }
{$IFDEF Delphi6}                                                         {!!.11}
  {$WARN SYMBOL_PLATFORM OFF}                                            {!!.11}
  {$WARN UNIT_PLATFORM OFF}                                              {!!.11}
  {$WARN SYMBOL_DEPRECATED OFF}                                          {!!.11}
{$ENDIF}                                                                 {!!.11}
