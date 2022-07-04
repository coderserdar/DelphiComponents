{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.205 2008-11-10

  Copyright (c) 2001-2009 Vadim V.Lopushansky

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbcFuncs;

{$i DbxOpenOdbc.inc}

interface

uses
  Windows, SysUtils,
  {$IFNDEF _D12UP_}
    StrUtils,
  {$ENDIF}
  {$IFDEF _D12UP_}
    AnsiStrings,
  {$ENDIF}
  Classes;

//
// Match functions:
//
function min(v1, v2: Integer): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}

//
// AnsiString/WideString constants:
//
const
  cNullAnsiChar = AnsiChar(#0);
  cNullWideChar = WideChar(#0);
  cNullAnsiCharBuf: AnsiChar = #0;
  cNullWideCharBuf: WideChar = #0;

const
  DefaultSystemCodePage: Integer = 0;

//
// AnsiString/WideString functions:
//
function WideStringLengthFromStr(const S: PAnsiChar): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
function WideStringLengthFromStrings(const L: TStrings): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
function WCharFromChar(WCharDest: PWideChar; DestChars: Integer; const CharSource: PAnsiChar; SrcBytes: Integer): Integer;  {$IFDEF _INLINE_} inline; {$ENDIF}
function StringToWideChar(const Source: AnsiString; Dest: PWideChar; DestChars: Integer): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
procedure StringToWStr(const Source: AnsiString; var Dest: PWideChar); {$IFDEF _INLINE_} inline; {$ENDIF} overload;
procedure StringToWStr(const Source: WideString; var Dest: PWideChar); {$IFDEF _INLINE_} inline; {$ENDIF} overload;
procedure StringsToWStr(const L: TStrings; var Dest: PWideChar); {$IFDEF _INLINE_} inline; {$ENDIF}

function WStrLen(const Str: PWideChar): Cardinal;
function WStrCopy(Dest: PWideChar; const Source: PWideChar): Integer;

function StrClone(const Source: PAnsiChar; out Dest: PAnsiChar): Integer; overload;
function StrClone(const Source: PAnsiChar; out Dest: PWideChar): Integer; overload;
function StrClone(const Source: AnsiString; out Dest: PWideChar): Integer; overload;
function StrClone(const Source: AnsiString; out Dest: PAnsiChar): Integer; overload;
function StrClone(const Source: PWideChar; out Dest: PAnsiChar): Integer; overload;
function StrClone(const Source: PWideChar; out Dest: PWideChar): Integer; overload;

function StrPtrToString(const Source: Pointer; bUnicode: Boolean): AnsiString; {$IFDEF _INLINE_} inline; {$ENDIF}

function WStrIComp(const Str1, Str2: PWideChar): Integer;

function AnsiToOEM(const S: AnsiString): AnsiString;
function OemToAnsi(const S: AnsiString): AnsiString;

function ChangeFileExtW(const FileName, Extension: WideString): WideString;
function LoadStringFromFileA(const FileName: AnsiString; var S: AnsiString): Boolean;
function LoadStringFromFileW(const FileName: string; var S: WideString): Boolean;

//
// AnsiString functions:
//
{$IFNDEF _D12UP_}
  {$IFDEF _D9UP_}
  var
    AnsiPos: function (const Substr, S: AnsiString): Integer = Pos;
  {$ELSE}
  function AnsiPos(const Substr, S: AnsiString): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
  {$ENDIF}
{$ENDIF}

//todo: C:\Projects\#CVS\dbxoodbc\devel\source\#todo\fast-strings\3.2\FastStrings.pas

implementation

//
// Match functions:
//
function min;//(v1, v2: Integer): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if v1 <= v2 then
    Result := v1
  else
    Result := v2;
end;

//
// AnsiString/WideString functions:
//
function WideStringLengthFromStr;//(const S: PAnsiChar): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  Result := Length(WideString(StrPas(S)));
end;

function WideStringLengthFromStrings;//(const L: TStrings): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if L.Count > 0 then
    Result := Length(WideString(L.Text))
  else
    Result := 0;
end;

function WCharFromChar;//(WCharDest: PWideChar; DestChars: Integer; const CharSource: PAnsiChar; SrcBytes: Integer): Integer;  {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  Result := MultiByteToWideChar(DefaultSystemCodePage, 0, CharSource, SrcBytes, WCharDest, DestChars);
end;

function StringToWideChar;//(const Source: AnsiString; Dest: PWideChar; DestChars: Integer): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  Result := MultiByteToWideChar(DefaultSystemCodePage, {Flags}0,
    {CharSource:}PAnsiChar(Source), {SrcBytes:}Length(Source), Dest, DestChars);
  Dest[Result] := cNullWideChar;
end;

procedure StringToWStr(const Source: AnsiString; var Dest: PWideChar); //{$IFDEF _INLINE_} inline; {$ENDIF} overload;
var
  iLen: Integer;
begin
  iLen := Length(Source);
  if iLen > 0 then
  begin
    iLen := WCharFromChar(Dest, {DestCharCount}iLen, PAnsiChar(Source), {SrcBytes}iLen);
    Dest[iLen] := cNullWideChar;
  end
  else
    Dest^ := cNullWideChar;
end;

procedure StringToWStr(const Source: WideString; var Dest: PWideChar); //{$IFDEF _INLINE_} inline; {$ENDIF} overload;
begin
  StringToWStr(AnsiString(Source), Dest);
end;

procedure StringsToWStr;//(const L: TStrings; var Dest: PWideChar); {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if L.Count > 0 then
    StringToWStr(L.Text, Dest)
  else
    Dest^ := cNullWideChar;
end;

function WStrLen;//(const Str: PWideChar): Cardinal;
asm
  {Check the first byte}
  cmp word ptr [eax], 0
  je @ZeroLength
  {Get the negative of the string start in edx}
  mov edx, eax
  neg edx
@ScanLoop:
  mov cx, [eax]
  add eax, 2
  test cx, cx
  jnz @ScanLoop
  lea eax, [eax + edx - 2]
  shr eax, 1
  ret
@ZeroLength:
  xor eax, eax
end;

function WStrCopy;//(Dest: PWideChar; const Source: PWideChar): Integer;
begin
  Result := WStrLen(Source);
  Move(Source^, Dest^, (Result + 1) * SizeOf(WideChar));
end;

function StrClone(const Source: PAnsiChar; out Dest: PAnsiChar): Integer;
begin
  if (Source = nil) or (Source^ = cNullAnsiChar) then
  begin
    Dest := nil;
    Result := 0;
  end
  else
  begin
    Result := StrLen(Source);
    Dest := AllocMem(Result + 1);
    StrCopy(Dest, Source);
  end;
end;

function StrClone(const Source: PAnsiChar; out Dest: PWideChar): Integer;
begin
  if (Source = nil) or (Source^ = cNullAnsiChar) then
  begin
    Dest := nil;
    Result := 0;
  end
  else
  begin
    Result := StrLen(Source);
    Pointer(Dest) := AllocMem((Result + 1) * 4);
    Result := WCharFromChar(Dest, {DestCharCount}Result, Source, {SrcBytes}Result);
    Dest[Result] := cNullWideChar;
  end;
end;

function StrClone(const Source: AnsiString; out Dest: PWideChar): Integer;
begin
  Result := Length(Source);
  if (Result = 0) or (Source[1] = cNullAnsiChar) then
    Dest := nil
  else
  begin
    Pointer(Dest) := AllocMem((Result + 1) * 4);
    Result := WCharFromChar(Dest, {DestCharCount}Result, PAnsiChar(Source), {SrcBytes}Result);
    Dest[Result] := cNullWideChar;
  end;
end;

function StrClone(const Source: AnsiString; out Dest: PAnsiChar): Integer;
begin
  Result := Length(Source);
  if (Result = 0) or (Source[1] = cNullAnsiChar) then
    Dest := nil
  else
  begin
    Dest := AllocMem(Result + 1);
    StrCopy(Dest, PAnsiChar(Source));
  end;
end;

function StrClone(const Source: PWideChar; out Dest: PAnsiChar): Integer;
var
  S: string;
begin
  if (Source = nil) or (Source^ = cNullWideChar) then
  begin
    Dest := nil;
    Result := 0;
  end
  else
  begin
    S := AnsiString(WideString(Source));
    Result := Length(S);
    Dest := AllocMem(Result + 1);
    StrCopy(Dest, PAnsiChar(S));
  end;
end;

function StrClone(const Source: PWideChar; out Dest: PWideChar): Integer;
begin
  if (Source = nil) or (Source^ = cNullWideChar) then
  begin
    Dest := nil;
    Result := 0;
  end
  else
  begin
    Result := WStrLen(Source);
    Pointer(Dest) := AllocMem((Result + 1) * SizeOf(WideChar));
    Move(Source^, Dest^, (Result + 1) * SizeOf(WideChar));
  end;
end;

function StrPtrToString;//(const Source: Pointer; bUnicode: Boolean): AnsiString;
begin
  if bUnicode then
    Result := AnsiString(WideString(PWideChar(Source)))
  else
    Result := StrPas(PAnsiChar(Source));
end;

function WStrIComp;//(const Str1, Str2: PWideChar): Integer;
var
  P1, P2: PWideChar;
  C1, C2: WideChar;
begin
  P1 := Str1;
  P2 := Str2;
  while True do
  begin
    C1 := P1^;
    C2 := P2^;

    case C1 of
      'a'..'z':
        C1 := WideChar(Word(C1) xor $0020);
    end;

    case C2 of
      'a'..'z':
        C2 := WideChar(Word(C2) xor $0020);
    end;

    if (C1 <> C2) or (C1 = #0) then
    begin
      Result := Ord(C1) - Ord(C2);
      Exit;
    end;

    Inc(P1);
    Inc(P2);
  end;
end;

function AnsiToOEM;//(const S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S));
  if Length(S) > 0 then
    CharToOem(PAnsiChar(S), PAnsiChar(Result));
end;

function OemToAnsi;//(const S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S));
  if Length(S) > 0 then
    OemToChar(PAnsiChar(S), PAnsiChar(Result));
end;

function ChangeFileExtW;//(const FileName, Extension: WideString): WideString;
var
  I: Integer;
begin
  Result := FileName;
  for I := Length(Result) downto 1 do
  begin
    if Result[i] = WideChar('.') then
    begin
      SetLength(Result, i - 1);
      Break;
    end;
  end;
  Result := Result + Extension;
end;

function LoadStringFromFileA;//(const FileName: AnsiString; var S: AnsiString): Boolean;
var
  fs: TFileStream;
begin
  Result := False;
  if (FileName = '') or (not FileExists(FileName)) then
    Exit;
  fs := nil;
  try
    try
      fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      SetLength(S, fs.Size);
      if Length(S) > 0 then
        fs.Read(S[1], Length(S));
      Result := True;
    finally
      fs.Free;
    end;
  except
  end;
end;

function LoadStringFromFileW;//(const FileName: string; var S: WideString): Boolean;
var
  fs: TFileStream;
begin
  Result := False;
  if (FileName = '') or (not FileExists(FileName)) then
    Exit;
  fs := nil;
  try
    try
      fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      SetLength(S, fs.Size div 2);
      if Length(S) > 0 then
      begin
        fs.Read(S[1], SizeOf(WideChar));
        if S[1] = #$FEFF then
        begin
          SetLength(S, Length(S) - 1);
          fs.Read(S[1], Length(S) * SizeOf(WideChar));
        end
        else
        begin
          fs.Position := 0;
        end;
        fs.Read(S[1], Length(S) * SizeOf(WideChar));
      end;
      Result := True;
    finally
      fs.Free;
    end;
  except
  end;
end;

//
// AnsiString functions:
//
{$IFNDEF _D12UP_}
  {$IFNDEF _D9UP_}
  function AnsiPos;//(const Substr, S: AnsiString): Integer;
  begin
    Result := Pos(Substr, S);
  end;
  {$ENDIF}
{$ENDIF}

end.
