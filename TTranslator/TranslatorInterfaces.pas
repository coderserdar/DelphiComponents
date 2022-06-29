{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: TranslatorInterfaces.pas,v 1.5 2003/03/19 14:39:36 mvj Exp $ }

unit TranslatorInterfaces;

interface
{$i common.inc}

type
  IStringTranslator = interface
    ['{B7F2BFFE-D009-45B8-B8C4-24852A66CDD8}']
{$ifdef D4_OR_HIGHER}
    function GetString(const AProperty : String) : String; overload;
    function GetString(const AProperty : String; Variables : array of String) : String; overload;
    function HasTranslation(const AProperty : String; out Translation : String) : Boolean; overload;
    function HasTranslation(const AProperty : String; Variables : array of String; out Translation : String) : Boolean; overload;
{$else}
    function GetString(const AProperty : String) : String;
    function GetStringOL(const AProperty : String; Variables : array of String) : String;
    function HasTranslation(const AProperty : String; out Translation : String) : Boolean;
    function HasTranslationOL(const AProperty : String; Variables : array of String; out Translation : String) : Boolean;
{$endif D4_OR_HIGHER}
  end;

  TDoubleStringArray = array[1..2] of String;
  TDoubleStringArrayArray = array of TDoubleStringArray;

  TDefaultTranslator = class(TObject, IStringTranslator)
  private
    fTranslations : array of TDoubleStringArray;
    // plain puckoness of delphi, need to define this crap for interfaces
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
{$ifdef D4_OR_HIGHER}
    function GetString(const AProperty : String) : String; overload;
    function GetString(const AProperty : String; Variables : array of String) : String; overload;
    function HasTranslation(const AProperty : String; out Translation : String) : Boolean; overload;
    function HasTranslation(const AProperty : String; Variables : array of String; out Translation : String) : Boolean; overload;
{$else}
    function GetString(const AProperty : String) : String;
    function GetStringOL(const AProperty : String; Variables : array of String) : String;
    function HasTranslation(const AProperty : String; out Translation : String) : Boolean;
    function HasTranslationOL(const AProperty : String; Variables : array of String; out Translation : String) : Boolean;
{$endif D4_OR_HIGHER}
  public
    constructor Create(const Translations : array of TDoubleStringArray);
  end;

function SubStrPos(FirstSearchPos : Integer; SubStr, S : String) : Integer;
function ReplaceVariables(const ATranslation : String; Variables : array of String) : String;

implementation

uses
  SysUtils;

function SubStrPos(FirstSearchPos : Integer; SubStr, S : String) : Integer;
begin
  if FirstSearchPos < 1 then
    FirstSearchPos := 1;

  S := Copy(S, FirstSearchPos, Length(S) - FirstSearchPos + 1);
  Result := Pos(SubStr, S);
  if Result > 0 then
    Result := Result + FirstSearchPos - 1;
end;

function ReplaceVariables(const ATranslation : String; Variables : array of String) : String;
var
  iVar : Integer;
  VarNr : Integer;
  VarPos : Integer;
  VarEndPos : Integer;
begin
  Result := ATranslation;
  
  iVar := 1;
  VarPos := 1;
  repeat
    VarPos := SubStrPos(VarPos, '<$', Result);
    if VarPos < 1 then
      Break;
    VarEndPos := SubStrPos(VarPos, '>', Result);
    if VarEndPos < 1 then
      Break;

    VarNr := iVar;

    if VarEndPos - VarPos > 2 then
    begin
      try
        VarNr := StrToInt(Copy(Result, VarPos + 2, VarEndPos - VarPos - 2));
      except
        VarPos := VarEndPos;
        Continue;
      end;
    end;

    if VarNr - 1 <= High(Variables) - Low(Variables) then
      Result := Copy(Result, 1, VarPos - 1) +
                     Variables[VarNr - 1 + Low(Variables)] +
                     Copy(Result, VarEndPos + 1, Length(Result))
    else
      Result := Copy(Result, 1, VarPos - 1) + Copy(Result, VarEndPos + 1, Length(Result));

    Inc(iVar);
  until False;
end;

{ TDefaultTranslator }

constructor TDefaultTranslator.Create(const Translations : array of TDoubleStringArray);
var
  i : integer;
begin
//  fTranslations := Translations;
  SetLength( fTranslations, Length(Translations) );
  for i := Low(Translations) to High(Translations) do
    fTranslations[i] := Translations[i];
end;

// plain puckoness of delphi, need to define this crap for interfaces
function TDefaultTranslator.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TDefaultTranslator._AddRef: Integer;
begin
  result := -1;
end;

function TDefaultTranslator._Release: Integer;
begin
  result := -1;
end;

function TDefaultTranslator.GetString(const AProperty : String) : String;
begin
  HasTranslation(AProperty, Result);
end;

function TDefaultTranslator.HasTranslation(const AProperty : String; out Translation : String) : Boolean;
var
  iProperty : integer;
begin
  result := False;
  Translation := AProperty;
  for iProperty := Low(fTranslations) to High(fTranslations) do
    if fTranslations[iProperty,1] = AProperty then
    begin
      result := True;
      Translation := fTranslations[iProperty,2];
      break;
    end;
end;

{$ifdef D4_OR_HIGHER}
function TDefaultTranslator.GetString(const AProperty : String; Variables : array of String) : String;
{$else}
function TDefaultTranslator.GetStringOL(const AProperty : String; Variables : array of String) : String;
{$endif D4_OR_HIGHER}
begin
  Result := GetString(AProperty, Variables);
end;

{$ifdef D4_OR_HIGHER}
function TDefaultTranslator.HasTranslation(const AProperty : String; Variables : array of String; out Translation : String) : Boolean;
{$else}
function TDefaultTranslator.HasTranslationOL(const AProperty : String; Variables : array of String; out Translation : String) : Boolean;
{$endif D4_OR_HIGHER}
begin
  Result := HasTranslation(AProperty, Translation);
  if Result then
    Translation := ReplaceVariables(Translation, Variables);
end;

end.

