{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is CnvStrUtils.pas                                   }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

unit CnvStrUtils;

{$I XQ_FLAG.INC}
interface

uses
  SysUtils, Classes;

type
  TStringArray = array of string;
  TCharSet = set of AnsiChar; { patched by ccy }
  
  TAdvStringList = class (TStringList)
  private
    FTokenSeparator: Char;
    FQuoteChar: Char;
    function GetTokenizedText: string;
    procedure SetTokenizedText(const Value: string);
  public
    constructor Create;
    property TokenizedText: string read GetTokenizedText write SetTokenizedText;
    property TokenSeparator: Char read FTokenSeparator write FTokenSeparator;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
  end;

function RemoveEscapeChars (const s : string; EscChar : char) : string;
function TextToBool (const Value : string) : boolean;
function BoolToText (Value : boolean) : char;
function EliminateWhiteSpaces (const s : string) : string;
function EliminateChars (const s : string; const chars : TCharSet) : string;
function LastPartOfName (const s : String) : string;
function FirstPartOfName(const s : string): string;
procedure MixTStrings(Source, Dest : TStrings; FromIndex : Integer = 0);
function CommaList(const Items : string): string;
function ListOfItems(const Items : array of string): String;
procedure RemoveBlankItems(List : TStringList);
function FirstNonEmptyString(const Strs : array of string): string;
function AddStrings(const Items : array of string): String; overload;
function AddStrings(const Items, Items2 : array of string): String; overload;
function IndexOf(const Items : array of string; const Item : String; 
    CaseSensitive : boolean = false): Integer;

function HexToInt(Value : string) : integer;
function StringToHex(const s : string): string;
function HexToString(const s : string): string;

function StringListToTStringArray(l : TStrings): TStringArray;
function StrToIntEx(const s : string): Integer;
procedure StrCount(const s : string; var Alpha, Numeric : Integer);

{$if CompilerVersion <= 18.5}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
{$ifend}

implementation

uses
  Windows;
  
const
  _TextToBool : array ['0'..'1'] of Boolean = (False, True);
  _BoolToText : array [False..True] of char = ('0', '1');

function RemoveEscapeChars;
var
  j : Integer;
begin
  Result := s;
  repeat
    j := Pos (EscChar, Result);
    if j > 0
      then system.Delete (Result, j, 2);
  until j <= 0;
end;

function TextToBool;
begin
  if Trim (Value) <> EmptyStr then
    Result := _TextToBool [Value [1]]
  else
    Result := False;
end;

function BoolToText;
begin
  Result := _BoolToText [Value];
end;

function EliminateChars (const s : string; const chars : TCharSet) : string;
var
  i : Integer;
begin
  Result := EmptyStr;
  for i := 1 to Length (s) do
    if not CharInSet(s [i], chars)
      then Result := Result + s [i];
end;

function EliminateWhiteSpaces (const s : string) : string;
begin
  Result := EliminateChars (s, [' ', #255, #13, #10]);
end;

function LastPartOfName (const s : String) : string;
var
  i : Integer;
begin
  Result := EmptyStr;
  for i := Length (s) downto 1 do
   if s [i] = '.'
     then
     begin
       Result := system.Copy (s, i + 1, Length (s) - i);
       Exit;
     end;
  Result := s;
end;

procedure MixTStrings(Source, Dest : TStrings; FromIndex : Integer = 0);
var
  i, j : Integer;
begin
  if (Source <> nil) and (Dest <> nil)
    then
    begin
      Dest.BeginUpdate;
      try
        for i := FromIndex to Source.Count - 1 do
          begin
            j := Dest.IndexOfName (Source.Names [i]);
            if j < 0
              then
              begin
                j := Dest.IndexOf (Source [i]);
                if j < 0
                  then Dest.Add (Source [i]);
              end;
          end;
      finally
        Dest.EndUpdate;
      end;
    end;
end;

function CommaList(const Items : string): string;
begin
  if Items <> ''
    then Result := ',' + Items
    else Result := '';
end;

function ListOfItems(const Items : array of string): String;
var
  i : integer;
begin
  Result := '';
  for i := Low (Items) to High (Items) do
    Result := Result + Items [i] + ',';
  system.Delete (Result, Length (Result), 1);
end;

procedure RemoveBlankItems(List : TStringList);
var
  i : Integer;
begin
  List.BeginUpdate;
  try
    i := 0;
    while i < List.Count do
      if Trim (List [i]) = ''
        then List.Delete (i)
        else Inc (i);
  finally
    List.EndUpdate;
  end;
end;

function FirstNonEmptyString(const Strs : array of string): string;
var
  i : Integer;
begin
  Result := '';
  for i := Low (Strs) to High (Strs) do
    if Strs [i] <> ''
      then
      begin
        Result := Strs [i];
        Exit;
      end;
end;

function AddStrings(const Items : array of string): String;
var
  i : integer;
begin
  Result := '';
  for i := Low (Items) to High (Items) do
    Result := Result + Items [i];
end;

function AddStrings(const Items, Items2 : array of string): String;
var
  i : integer;
begin
  Result := '';
  for i := Low (Items) to High (Items) do
    Result := Result + Items [i] + Items2 [i];
end;

function IndexOf(const Items : array of string; const Item : String; 
    CaseSensitive : boolean = false): Integer;
var
  i : Integer;
  UpItem : string;
begin
  if not CaseSensitive
    then UpItem := UpperCase (Item)
    else UpItem := '';
  Result := -1;
  for i := Low (Items) to High (Items) do
    if (CaseSensitive and (Items [i] = Item)) or
       ((not CaseSensitive) and (UpperCase (Items [i]) = UpItem))
      then
      begin
        Result := i;
        Exit;
      end;
end;

function HexDigitToInt(Ch : char) : integer;
var
  sb : byte;
begin
  sb := ord(ch);
  if (sb >= ord('A')) and (sb <= ord('F')) then
    Result := sb - ord('A') + 10
  else if (sb >= ord('a')) and (sb <= ord('f')) then
    Result := sb - ord('a') + 10
  else if (sb >= ord('0')) and (sb <= ord('9')) then
    Result := sb - ord('0')
  else
    raise Exception.Create(ch + ' is not a hex digit');
end;

function HexToInt(Value : string) : integer;
var
  i : integer;
  base : integer;
begin
  Result := 0;
  Value := UpperCase(Value);
  base := 1;
  for i := Length(Value) downto 1 do
  begin
    Result := Result + HexDigitToInt(Value[i])*base;
    base := base*16
  end;
end;

function StringToHex(const s : string): string;
var
  j : Integer;
  Hex : string;
begin
  SetLength (Result, Length (s) * 2);
  for j := 1 to Length (s) do
    begin
      Hex := IntToHex (Ord (s [j]), 2);
      Move (Hex [1], Result [(j - 1) * 2 + 1], 2);
    end;
end;

function HexToString(const s : string): string;
var
  i : Integer;
  c : Char;
  Hex : string;
begin
  SetLength (Hex, 2);
  SetLength (Result, Length (s) div 2);
  i := 1;
  while i <= Length (s)  do
    begin
      Move (s [i], Hex [1], 2);
      c := char (HexToInt (Hex));
      Move (c, Result [(i + 1) div 2], 1);
      Inc (i, 2);
    end;
end;

function FirstPartOfName(const s : string): string;
var
  i : Integer;
begin
  Result := '';
  for i := 1 to Length (s) do
   if s [i] = '.'
     then
     begin
       Result := system.Copy (s, 1, i - 1);
       Exit;
     end;
  Result := s;
end;

function StringListToTStringArray(l : TStrings): TStringArray;
var
  i : Integer;
begin
  SetLength (Result, l.Count);
  for i := 0 to l.Count - 1 do
    Result [i] := l [i];
end;

function StrToIntEx(const s : string): Integer;
begin
  if s <> ''
    then
    try
      Result := StrToInt (s);
    except
      on EConvertError do Result := 0;
    end
    else Result := 0;
end;

procedure StrCount(const s : string; var Alpha, Numeric : Integer);
var
  i : Integer;
begin
  Alpha := 0;
  Numeric := 0;
  for i := 0 to Length (s) do
    if CharInSet(s [i], ['0'..'9'])
      then Inc (Numeric)
      else Inc (Alpha);
end;

{$if CompilerVersion <= 18.5}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ifend}


{ TAdvStringList }


constructor TAdvStringList.Create;
begin
  inherited Create;
  FQuoteChar := '"';
  FTokenSeparator := ',';
end;

function TAdvStringList.GetTokenizedText: string;
var
  S: string;
  P: PChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '')
    then Result := FQuoteChar + FQuoteChar
    else
    begin
      Result := '';
      for I := 0 to Count - 1 do
        begin
          S := Get(I);
          P := PChar(S);
          while not CharInSet(P^, [#0, FQuoteChar, FTokenSeparator]) do
            P := CharNext(P);
          if P^ <> #0
            then S := AnsiQuotedStr(S, FQuoteChar);
          Result := Result + S + FTokenSeparator;
        end;
      System.Delete(Result, Length(Result), 1);
    end;
end;

procedure TAdvStringList.SetTokenizedText(const Value: string);
var
  P, P1: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    while P^ <> #0 do
      begin
        if P^ = FQuoteChar
          then S := AnsiExtractQuotedStr(P, FQuoteChar)
          else
          begin
            P1 := P;
            while (P^ <> #0) and (P^ <> FTokenSeparator) do
              P := CharNext(P);
            SetString(S, P1, P - P1);
          end;
        Add(S);
        while P^ = FTokenSeparator do
          P := CharNext(P);
        if P^ = FTokenSeparator
          then
          repeat
            P := CharNext(P);
          until P^ <> FTokenSeparator;
      end;
  finally
    EndUpdate;
  end;
end;

end.
