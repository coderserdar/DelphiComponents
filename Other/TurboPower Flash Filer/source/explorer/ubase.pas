{*********************************************************}
{* Global data; base classes, defines, functions         *}
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

{$I ffdefine.inc}


unit ubase;

interface

uses
  Windows,
  Classes,
  Controls,
  ExtCtrls,
  StdCtrls,
  ffdb,
  ffclreng,
  ffllbase,
  fflldict,
  uentity,
  uconfig;

type
  TMenuAction = (maServerAttach,
                 maServerDetach,
                 maDatabaseOpen,
                 maDatabaseClose);

var
  ClosingApp : Boolean;
  ServerList : TffeServerList;
  FieldTypes : array[TffFieldType] of string[20];

function FFEBlockSizeIndex(const aBlockSize: LongInt): Integer;

function FFEBoolToStr(B: Boolean): TffShStr;

procedure FFEEnableContainer(Container: TWinControl; Switch: Boolean);

function FFEFieldAllowedDefault(aFieldType : TffFieldType) : Boolean;
{ Returns true if the field type is allowed to have a default value.
  AutoInc, ByteArrays, and Boolean fields are not allowed to have a
  default value}

function FFEFieldTypeHasDecPl(aFieldType: TffFieldType): Boolean;
{ Returns true if the given field type has a "decimal places" factor
  associated with it.  For example, currency and float fields. }

function FFEFieldTypeHasUnits(aFieldType: TffFieldType): Boolean;
{ Returns true if the given field type has a "number of units" factor
  associated with it.  For example, string and character fields. }

function FFEFieldTypeRequiresUnits(aFieldType: TffFieldType): Boolean;
{ Returns true if the given field type requires a units factor. }

function FFEFieldTypeToIndex(aFieldType: TffFieldType): Integer;
{ Converts a given FF fieldtype value to an integer index, skipping
  the reserved positions }

function FFEIndexToFieldType(aIndex: Integer): TffFieldType;
{ Converts an integer index to a FF field type, skipping the
  reserved positions }

function FFEVersionStr: TffShStr;

implementation

uses
  ffnetmsg,
  ffllprot,
  DB,
  uconsts,
  SysUtils,
  TypInfo;

var
  FFEFirstReservedFieldType,
  FFELastReservedFieldType: TffFieldType;
{--------}
function FFEBlockSizeIndex(const aBlockSize: LongInt): Integer;
begin
  case aBlockSize of
    4 * 1024: Result := 0;
    8 * 1024: Result := 1;
   16 * 1024: Result := 2;
   32 * 1024: Result := 3;
   64 * 1024: Result := 4;
   else Result := -1;
  end;
end;
{--------}
function FFEBoolToStr(B: Boolean): TffShStr;
begin
  if B then Result := 'Y' else Result := 'N';
end;
{--------}
procedure FFEEnableContainer(Container: TWinControl; Switch: Boolean);
var
  I: Integer;
begin
  with Container do
  begin
    Enabled := Switch;
    for I := 0 to ControlCount - 1 do
    begin
      Controls[I].Enabled := Switch;
      if (Controls[I] is TGroupBox) or (Controls[I] is TPanel) then
        FFEEnableContainer(Controls[I] as TWinControl, Switch);
    end;
  end;
end;
{--------}
function FFEFieldAllowedDefault(aFieldType : TffFieldType) : Boolean;
begin
  Result := aFieldType in [fftBoolean,
                           fftChar,
                           fftWideChar,
                           fftByte,
                           fftInt8,
                           fftInt16,
                           fftInt32,
                           fftWord16,
                           fftWord32,
                           fftComp,
                           fftSingle,
                           fftDouble,
                           fftExtended,
                           fftCurrency,
                           fftStDate,
                           fftStTime,
                           fftDateTime,
                           fftShortString,  
                           fftShortAnsiStr,
                           fftNullString,
                           fftNullAnsiStr,
                           fftWideString];
end;
{--------}
function FFEFieldTypeHasDecPl(aFieldType: TffFieldType): Boolean;
begin
  Result := aFieldType in [fftSingle,
                           fftDouble,
                           fftExtended,
                           {fftComp,}
                           fftCurrency];
end;
{--------}
function FFEFieldTypeHasUnits(aFieldType: TffFieldType): Boolean;
begin
  Result := aFieldType in [fftByte,
                           fftWord16,
                           fftWord32,
                           fftInt8,
                           fftInt16,
                           fftInt32,
                           fftSingle,
                           fftDouble,
                           fftExtended,
                           fftComp,
                           fftCurrency,
                           fftByteArray,
                           fftShortString..High(TffFieldType)];
end;
{--------}
function FFEFieldTypeRequiresUnits(aFieldType: TffFieldType): Boolean;
begin
  Result := aFieldType in [fftByteArray,
                           fftShortString..High(TffFieldType)];
end;
{--------}
function FFEFieldTypeToIndex(aFieldType: TffFieldType): Integer;
begin
  if aFieldType < FFEFirstReservedFieldType then
    Result := Ord(aFieldType)
  else if aFieldType > FFELastReservedFieldType then
    Result := Ord(aFieldType) -
              (Ord(FFELastReservedFieldType) -
               Ord(FFEFirstReservedFieldType) + 1)
  else
    Result := -1;
end;
{--------}
function FFEIndexToFieldType(aIndex: Integer): TffFieldType;
begin
  if aIndex >= Ord(FFEFirstReservedFieldType) then
    Result := TffFieldType(aIndex +
              (Ord(FFELastReservedFieldType) -
               Ord(FFEFirstReservedFieldType) + 1))
  else
    Result := TffFieldType(Ord(aIndex));
end;
{--------}
procedure PopulateFieldTypes;
var
  I: TffFieldType;
begin
  FFEFirstReservedFieldType := fftBoolean;
  FFELastReservedFieldType := fftBoolean;
  for I := Low(I) to High(I) do begin
    FieldTypes[I] := GetEnumName(TypeInfo(TffFieldType), Ord(I));

    { Find the range of "reserved" slots.  This assumes they will be
      in a single contiguous block }
    if Pos('FFTRESERVED', ANSIUppercase(FieldTypes[I])) = 1 then begin
      if FFEFirstReservedFieldType = fftBoolean then
        FFEFirstReservedFieldType := I;
    end
    else
      if (FFEFirstReservedFieldType <> fftBoolean) and
         (FFELastReservedFieldType = fftBoolean) then
        FFELastReservedFieldType := Pred(I);
  end;
end;
{--------}
function FFEVersionStr: TffShStr;
begin
  Result := Format('%5.4f %d-bit', [FFVersionNumber / 10000, 32]);
end;
{--------}


initialization
  ClosingApp := False;
  PopulateFieldTypes;
end.

