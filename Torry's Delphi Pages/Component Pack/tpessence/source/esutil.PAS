
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

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsUtil;
  {-general utility routines}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Graphics, SysUtils, EsSRMgr;                                       {!!.06}

function DaysInMonth(Year, Month : Integer) : Integer;
  {-return the number of days in the specified month of a given year}
function DefaultEpoch : Integer;
  {-return the current century}
function GetLeftButton : Byte;
procedure GetRGB(Clr : TColor; var IR, IG, IB : Byte);
function IsLeapYear(Year : Integer) : Boolean;
  {-return True if Year is a leap year}
function Max(I, J : Integer) : Integer;
function Min(I, J : Integer) : Integer;

var
  StrRes : TEsStringResource;                                        {!!.06}

implementation

uses                                                                 {!!.07}
  EsBase, EsConst;                                                   {!!.07}

{!!.07}
function DaysInMonth(Year, Month : Integer) : Integer;
begin
  if (Year < 100) then
    raise EEssentialsError.Create(StrRes[SCEsInvalidYear] +
                                  ' "' + IntToStr(Year) + '"');
  case Month of
    1, 3, 5, 7, 8, 10, 12 : Result := 31;
    4, 6, 9, 11           : Result := 30;
    2                     : Result := 28+Ord(IsLeapYear(Year));
  else
    Result := 0;
  end;
end;

function DefaultEpoch : Integer;
var
  ThisYear   : Word;
  ThisMonth  : Word;
  ThisDay    : Word;
begin
  DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
  Result := (ThisYear div 100) * 100;                                  {!!.06}
end;

function GetLeftButton : Byte;
const
  RLButton : array[Boolean] of Word = (VK_LBUTTON, VK_RBUTTON);
begin
  Result := RLButton[GetSystemMetrics(SM_SWAPBUTTON) <> 0];
end;

procedure GetRGB(Clr : TColor; var IR, IG, IB : Byte);
begin
  IR := GetRValue(Clr);
  IG := GetGValue(Clr);
  IB := GetBValue(Clr);
end;

function IsLeapYear(Year : Integer) : Boolean;
begin
  Result := (Year mod 4 = 0) and (Year mod 4000 <> 0) and
    ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

function Max(I, J : Integer) : Integer;
begin
  if I > J then
    Result := I
  else
    Result := J;
end;

function Min(I, J : Integer) : Integer;
begin
  if I < J then
    Result := I
  else
    Result := J;
end;

procedure FreeStrRes; far;
begin
  StrRes.Free;
  StrRes := nil;
end;

initialization
  StrRes := TEsStringResource.Create(HInstance, 'ESSENTIALS_STRINGS_ENGLISH'); {!!.06}

{$IFDEF Win32}
finalization
  FreeStrRes;
{$ELSE}
  AddExitProc(FreeStrRes);
{$ENDIF}
end.