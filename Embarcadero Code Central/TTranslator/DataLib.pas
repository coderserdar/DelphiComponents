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

{ $Id: DataLib.pas,v 1.17 2002/04/23 08:19:55 mvj Exp $}

unit DataLib;

interface
{$i common.inc}

uses
  Classes;
  
{/** Round function for correctly rounding an extended value, i.e. it rounds
  0,5 to 1 and 1,5 to 2 (and not 0,5 to 0 and 1,5 to 2 as Delphi)! */}
{$ifdef D4_OR_HIGHER}
function CRound( Value : Extended ) : Int64;
{$else}
function CRound( Value : Extended ) : LongInt;
{$endif D4_OR_HIGHER}
function CDecimalRound( Value : Extended; DecFactor : LongInt ) : Extended;

{$ifdef LINUX}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: LongWord);
procedure FillMemory(Destination: Pointer; Length: LongWord; Fill: Byte);
function GetUserName(lpBuffer: PChar; var nSize: LongWord): LongBool;
{$endif LINUX}

function ReadStreamToText(AStream : TStream; StopAt : Char; ExitFalseAt : array of Char; var Text : String) : Boolean;
function ReadStreamToFixedText(AStream : TStream; Count : Integer; var Text : String) : Boolean;

implementation

{$ifdef D4_OR_HIGHER}
function CRound( Value : Extended ) : Int64;
{$else}
function CRound( Value : Extended ) : LongInt; // Fixa MVJ denna pangar i KuntoApu på databasvärden av storlek 25 megaFIM...
{$endif D4_OR_HIGHER}
begin
  if Value >= 0 then
    Result := Trunc( Value + 0.5 )
  else
    Result := Trunc( Value - 0.5 );
//  Result := Round( Value );
end;

function CDecimalRound( Value : Extended; DecFactor : LongInt ) : Extended;
begin
  if DecFactor > 0 then
    Result := CRound( Value * DecFactor ) / DecFactor
  else
    Result := Value;
end;

{$ifdef LINUX}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: LongWord);
begin
  Move(Source^, Destination^, Length);
end;

procedure FillMemory(Destination: Pointer; Length: LongWord; Fill: Byte);
begin
  FillChar(Destination^, Length, Fill);
end;


function GetUserName(lpBuffer: PChar; var nSize: LongWord): LongBool;
begin
  Result := False;
end;

{$endif LINUX}

function ReadStreamToText(AStream : TStream; StopAt : Char; ExitFalseAt : array of Char; var Text : String) : Boolean;
var
  iStart, iStop, iExit : Integer;
  aChar : Char;
begin
  iStart := AStream.Position;
  Result := True;

  repeat
    if AStream.Read(aChar, 1) = 0 then
    begin
      Result := False;
      Break;
    end;

    for iExit := Low(ExitFalseAt) to High(ExitFalseAt) do
      if aChar = ExitFalseAt[iExit] then
      begin
        Result := False;
        Break;
      end;
  until aChar = StopAt;

  iStop := AStream.Position;
  AStream.Position := iStart;

  if Result then
  begin
    if iStop - iStart -1 > 0 then
    begin
      SetLength(Text, iStop - iStart -1);
      AStream.Read(Text[1], iStop - iStart -1);
    end
    else
      Text := '';

    AStream.Read(aChar, 1);
  end;
end;

function ReadStreamToFixedText(AStream : TStream; Count : Integer; var Text : String) : Boolean;
begin
  Result := True;
  if Count > 0 then
  begin
    SetLength(Text, Count);
    Result := AStream.Read(Text[1], Count) = Count;
  end
  else
    Text := '';
end;

end.


