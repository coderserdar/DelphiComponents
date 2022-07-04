{*********************************************************}
{*                  VPXCHRFLT.PAS 1.03                   *}
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
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpXChrFlt;

interface

uses
  SysUtils,
  Classes,
  VpSR,
  VpBase,
  VpXBase;

const
  VpEndOfStream = #1;
  VpEndOfReplaceText = #2;
  VpNullChar = #3;

type
  TVpStreamFormat = {character formats of stream...}
     (sfUTF8,       {..UTF8 -- the default}
      sfUTF16LE,    {..UTF16, little endian (eg, Intel)}
      sfUTF16BE,    {..UTF16, big endian}
      sfISO88591);  {..ISO-8859-1, or Latin 1}

  TVpBaseCharFilter = class(TObject)
    protected
      FBufSize : Longint;
      FBuffer  : PAnsiChar;
      FBufPos  : Longint;
      FFormat  : TVpStreamFormat; {The format of the incoming stream}
      FFreeStream : Boolean;
      FStream  : TStream;
      FStreamPos : Longint;
      FStreamSize : Longint;
    protected
      function csGetSize : Longint; virtual;
      procedure csSetFormat(const aValue : TVpStreamFormat); virtual; abstract;
    public
      constructor Create(aStream : TStream; const aBufSize : Longint); virtual;
      destructor Destroy; override;

      property BufSize : Longint
         read FBufSize;

      property FreeStream : Boolean
         read FFreeStream write FFreeStream;

      property Stream : TStream
         read FStream;

    end;

  TVpInCharFilter = class(TVpBaseCharFilter)
    private
      FBufEnd    : Longint;
      FUCS4Char  : TVpUcs4Char;
      FLine      : Longint;
      FLinePos   : Longint;
      FLastChar  : DOMChar;
      FEOF       : Boolean;
      FBufDMZ    : Longint;
      FInTryRead : Boolean;
    protected
      procedure csAdvanceLine;
      procedure csAdvanceLinePos;
      procedure csGetCharPrim(var aCh : TVpUcs4Char;
                              var aIsLiteral : Boolean);
      function csGetNextBuffer : Boolean;
      function csGetTwoAnsiChars(var Buffer) : Boolean;
      function csGetUtf8Char : TVpUcs4Char;
      procedure csIdentifyFormat;
      procedure csPushCharPrim(aCh : TVpUcs4Char);
      procedure csSetFormat(const aValue : TVpStreamFormat); override;

      procedure csGetChar(var aCh : TVpUcs4Char;
                          var aIsLiteral : Boolean);

    public
      constructor Create(aStream : TStream; const aBufSize : Longint); override;

      property Format : TVpStreamFormat
         read FFormat
         write csSetFormat;
      property EOF : Boolean
         read FEOF;
    public
      procedure SkipChar;
      function TryRead(const S : array of Longint) : Boolean;
      function ReadChar : DOMChar;
      function ReadAndSkipChar : DOMChar;
      property Line : LongInt
         read FLine;
      property LinePos : LongInt
         read FLinePos;
  end;

  TVpOutCharFilter = class(TVpBaseCharFilter)
    protected
      FFormat : TVpStreamFormat;
      FSetUTF8Sig : Boolean;
    protected
      function csGetSize : LongInt; override;
      procedure csPutUtf8Char(const aCh : TVpUcs4Char);
      procedure csSetFormat(const aValue : TVpStreamFormat); override;
      procedure csWriteBuffer;
    public
      constructor Create(aStream : TStream; const aBufSize : Longint); override;
      destructor Destroy; override;

      procedure PutUCS4Char(aCh : TVpUcs4Char);
      function  PutChar(aCh1, aCh2 : DOMChar;
                    var aBothUsed  : Boolean) : Boolean;
      function  PutString(const aText : DOMString) : Boolean;
      function Position : integer;

      property Format : TVpStreamFormat
         read FFormat
         write csSetFormat;
      property WriteUTF8Signature : Boolean
         read FSetUTF8Sig
         write FSetUTF8Sig;
      property Size : LongInt
         read csGetSize;

  end;


implementation

const
  CR      = 13; {Carriage return}
  LF      = 10; {Line feed}

{====================================================================}
constructor TVpBaseCharFilter.Create(aStream  : TStream;
                               const aBufSize : Longint);
begin
  inherited Create;
  Assert(Assigned(aStream));
  FBufSize := aBufSize;
  FBufPos := 0;
  FFormat := sfUTF8;
  FFreeStream := False;
  FStream := aStream;
  FStreamPos := aStream.Position;
  FStreamSize := aStream.Size;
  GetMem(FBuffer, FBufSize);
end;
{--------}
destructor TVpBaseCharFilter.Destroy;
begin
  if Assigned(FBuffer) then begin
    FreeMem(FBuffer, FBufSize);
    FBuffer := nil;
  end;

  if FFreeStream then
    FStream.Free;

  inherited Destroy;
end;
{--------}
function TVpBaseCharFilter.csGetSize : LongInt;
begin
  Result := FStreamSize;
end;
{====================================================================}
constructor TVpInCharFilter.Create(aStream  : TStream;
                             const aBufSize : Longint);
begin
  inherited Create(aStream, aBufSize);
  if FStreamSize <= aBufSize then
    FBufDMZ := 0
  else
    FBufDMZ := 64;
  FBufEnd := 0;
  FLine := 1;
  FLinePos := 1;
  csIdentifyFormat;
  if aStream.Size > 0 then
    FEOF := False
  else
    FEOF := True;
  FUCS4Char := TVpUCS4Char(VpNullChar);
  FInTryRead := False;
end;
{--------}
procedure TVpInCharFilter.csAdvanceLine;
begin
  Inc(FLine);
  FLinePos := 1;
end;
{--------}
procedure TVpInCharFilter.csAdvanceLinePos;
begin
  Inc(FLinePos);
end;
{--------}
procedure TVpInCharFilter.csGetCharPrim(var aCh : TVpUcs4Char;
                                        var aIsLiteral : Boolean);
begin
  {Note: as described in the XML spec (2.11) all end-of-lines are
         passed as LF characters no matter what the original document
         had. This routine converts a CR/LF pair to a single LF, a
         single CR to an LF, and passes LFs as they are.}

  {get the first (test) character}
  {first check the UCS4Char buffer to see if we have a character there;
   if so get it}
 if (FUCS4Char <> TVpUCS4Char(VpNullChar)) then begin
    aCh := FUCS4Char;
    FUCS4Char := TVpUCS4Char(VpNullChar);
  end
  {otherwise get a character from the buffer; this depends on the
   format of the stream of course}
  else begin
    case Format of
      sfUTF8     : aCh := csGetUtf8Char;
    else
      {it is next to impossible that this else clause is reached; if
       it is we're in deep doggy doo-doo, so pretending that it's the
       end of the stream is the least of our worries}
      aCh := TVpUCS4Char(VpEndOfStream);
    end;
  end;

  {if we got a CR, then we need to see what the next character is; if
   it is an LF, return LF; otherwise put the second character  back
   and still return an LF}
  if (aCh = CR) then begin
    if (FUCS4Char <> TVpUCS4Char(VpNullChar)) then begin
      aCh := FUCS4Char;
      FUCS4Char := TVpUCS4Char(VpNullChar);
    end
    else begin
      case Format of
        sfUTF8     : aCh := csGetUtf8Char;
      else
        aCh := TVpUCS4Char(VpEndOfStream);
      end;
    end;
    if (aCh <> LF) then
      csPushCharPrim(aCh);
    aCh := LF;
  end;

  {check to see that the character is valid according to XML}
  if (aCh <> TVpUCS4Char(VpEndOfStream)) and (not VpIsChar(aCh)) then
    raise EVpFilterError.CreateError (FStream.Position,
                                       Line,
                                       LinePos,
                                       sInvalidXMLChar);
end;
{--------}
function TVpInCharFilter.csGetNextBuffer : Boolean;
begin
  if FStream.Position > FBufDMZ then
    {Account for necessary buffer overlap}
    FStream.Position := FStream.Position - (FBufEnd - FBufPos);
  FBufEnd := FStream.Read(FBuffer^, FBufSize);
  FStreamPos := FStream.Position;
  FBufPos := 0;
  Result := FBufEnd <> 0;
end;
{--------}
function TVpInCharFilter.csGetTwoAnsiChars(var Buffer) : Boolean;
type
  TTwoChars = array [0..1] of AnsiChar;
var
  i : integer;
begin
  {get two byte characters from the stream}
  for i := 0 to 1 do begin
    {if the buffer is empty, fill it}
    if (FBufPos >= FBufEnd - FBufDMZ) and
       (not FInTryRead) then begin
      {if we exhaust the stream, we couldn't satisfy the request}
      if not csGetNextBuffer then begin
        Result := false;
        Exit;
      end;
    end;
    {get the first byte character from the buffer}
    TTwoChars(Buffer)[i] := FBuffer[FBufPos];
    inc(FBufPos);
  end;
  Result := true;
end;
{--------}
function TVpInCharFilter.csGetUtf8Char : TVpUcs4Char;
var
  Utf8Char : TVpUtf8Char;
  {Ch       : AnsiChar;}
  Len      : Integer;
  i        : Integer;
begin
  {if the buffer is empty, fill it}
  if (not FInTryRead) and
     (FBufPos >= FBufEnd - FBufDMZ) then begin
    {if we exhaust the stream, there are no more characters}
    if not csGetNextBuffer then begin
      Result := TVpUCS4Char(VpEndOfStream);
      Exit;
    end;
  end;
  {get the first byte character from the buffer}
  Utf8Char[1] := FBuffer[FBufPos];
  FBufPos := FBufPos + 1;
  {determine the length of the Utf8 character from this}
  Len := VpGetLengthUtf8(Utf8Char[1]);
  if (Len < 1) then
    raise EVpFilterError.CreateError (FStream.Position,
                                       Line,
                                       LinePos,
                                       sBadUTF8Char);
  Move(Len, Utf8Char[0], 1);
  {get the remaining characters from the stream}
  for i := 2 to Len do begin
    {if the buffer is empty, fill it}
    if (FBufPos >= FBufEnd - FBufDMZ) and
       (not FInTryRead) then begin
      {if we exhaust the stream now, it's a badly formed UTF8
       character--true--but we'll just pretend that the last character
       does not exist}
      if not csGetNextBuffer then begin
        Result := TVpUCS4Char(VpEndOfStream);
        Exit;
      end;
    end;
    {get the next byte character from the buffer}
    Utf8Char[i] := FBuffer[FBufPos];
    FBufPos := FBufPos + 1;
  end;
  {convert the UTF8 character into a UCS4 character}
  if (not VpUtf8ToUcs4(Utf8Char, Len, Result)) then
    raise EVpFilterError.CreateError (FStream.Position,
                                       Line,
                                       LinePos,
                                       sBadUTF8Char);
end;
{--------}
procedure TVpInCharFilter.csIdentifyFormat;
begin
  {Note: a stream in either of the UTF16 formats will start with a
         byte-order-mark (BOM). This is the unicode value $FEFF. Hence
         if the first two bytes of the stream are read as ($FE, $FF),
         we have a UTF16BE stream. If they are read as ($FF, $FE), we
         have a UTF16LE stream. Otherwise we assume a UTF8 stream (at
         least for now, it can be changed later).}
  csGetNextBuffer;
  if FBufSize > 2 then
    if (FBuffer[0] = #$FE) and (FBuffer[1] = #$FF) then begin
      FFormat := sfUTF16BE;
      FBufPos := 2;
    end else if (FBuffer[0] = #$FF) and (FBuffer[1] = #$FE) then begin
      FFormat := sfUTF16LE;
      FBufPos := 2;
    end else if (FBuffer[0] = #$EF) and
                (FBuffer[1] = #$BB) and
                (FBuffer[2] = #$BF) then begin
      FFormat := sfUTF8;
      FBufPos := 3;
    end else
      FFormat := sfUTF8
  else
    FFormat := sfUTF8;
end;
{--------}
procedure TVpInCharFilter.csPushCharPrim(aCh : TVpUcs4Char);
begin
  Assert(FUCS4Char = TVpUCS4Char(VpNullChar));
  {put the char into the buffer}
  FUCS4Char := aCh;
end;
{--------}
procedure TVpInCharFilter.csSetFormat(const aValue : TVpStreamFormat);
begin
  {we do not allow the UTF16 formats to be changed since they were
   well defined by the BOM at the start of the stream but all other
   changes are allowed (caveat user); this means that an input stream
   that defaulted to UTF8 can be changed at a later stage to
   ISO-8859-1 or whatever if required}
  if (Format <> sfUTF16LE) and (Format <> sfUTF16BE) then
    FFormat := aValue;
end;
{--------}
procedure TVpInCharFilter.csGetChar(var aCh        : TVpUcs4Char;
                                    var aIsLiteral : Boolean);
begin
  {get the next character; for an EOF raise an exception}
  csGetCharPrim(aCh, aIsLiteral);
  if (aCh = TVpUCS4Char(VpEndOfStream)) then
    FEOF := True
  else
    {maintain the line/character counts}
    if (aCh = LF) then
      csAdvanceLine
    else
      csAdvanceLinePos;
end;
{--------}
function TVpInCharFilter.TryRead(const S : array of Longint) : Boolean;
var
  Idx         : Longint;
  Ch          : TVpUcs4Char;
  IL          : Boolean;
  OldBufPos   : Longint;
  OldChar     : DOMChar;
  OldUCS4Char : TVpUcs4Char;
  OldLinePos  : Longint;
  OldLine     : Longint;
begin
  OldBufPos := FBufPos;
  OldChar := FLastChar;
  OldUCS4Char := FUCS4Char;
  OldLinePos := LinePos;
  OldLine := Line;
  Result := True;
  FInTryRead := True;
  try
    for Idx := Low(s) to High(S) do begin
      csGetChar(Ch, IL);
      if Ch <> TVpUcs4Char(S[Idx]) then begin
        Result := False;
        Break;
      end;
    end;
  finally
    if not Result then begin
      FBufPos := OldBufPos;
      FLastChar := OldChar;
      FUCS4Char := OldUCS4Char;
      FLinePos := OldLinePos;
      FLine := OldLine;
    end else begin
      FLastChar := #0;
      FUCS4Char := TVpUCS4Char(VpNullChar);
      if (FStreamSize = FStreamPos) and
         (FBufPos = FBufEnd) then
        FEOF := True;
    end;
    FInTryRead := False;
  end;
end;
{--------}
procedure TVpInCharFilter.SkipChar;
begin
  FLastChar := #0;
  FUCS4Char := TVpUCS4Char(VpNullChar);
  Inc(FLinePos);
end;
{--------}
function TVpInCharFilter.ReadandSkipChar : DOMChar;
var
  Ch     : TVpUCS4Char;
  IL     : Boolean;
begin
  if FLastChar = '' then begin
    csGetChar(Ch, IL);
    VpUcs4ToWideChar(Ch, Result);
  end else begin
    Result := FLastChar;
    Inc(FLinePos);
  end;
  FLastChar := #0;
  FUCS4Char := TVpUCS4Char(VpNullChar);
  if (FStreamSize = FStreamPos) and
     (FBufPos = FBufEnd) then
    FEOF := True;
end;
{--------}
function TVpInCharFilter.ReadChar : DOMChar;
var
  Ch     : TVpUCS4Char;
  IL     : Boolean;
begin
  if FLastChar = '' then begin
    csGetChar(Ch, IL);
    VpUcs4ToWideChar(Ch, Result);
    Dec(FLinePos);
    FLastChar := Result;
    if (FUCS4Char <> TVpUCS4Char(VpNullChar)) then
      if (Format = sfUTF16LE) or
         (Format = sfUTF16BE) then
        Dec(FBufPos, 2)
      else if FBufPos > 0 then
        Dec(FBufPos, 1);
    FUCS4Char := Ch;
  end else
    Result := FLastChar;
end;

{===TVpOutCharFilter=================================================}
constructor TVpOutCharFilter.Create(aStream : TStream; const aBufSize : Longint);
begin
  inherited Create(aStream, aBufSize);
  FSetUTF8Sig := True;
end;
{--------}
destructor TVpOutCharFilter.Destroy;
begin
  if Assigned(FBuffer) then
    if (FBufPos > 0) then
      csWriteBuffer;

  inherited Destroy;
end;
{--------}
function TVpOutCharFilter.csGetSize : LongInt;
begin
  Result := FStream.Size + FBufPos;
end;
{--------}
procedure TVpOutCharFilter.csPutUtf8Char(const aCh : TVpUcs4Char);
var
  UTF8 : TVpUtf8Char;
  i    : integer;
begin
  if not VpUcs4ToUtf8(aCh, UTF8) then
    raise EVpStreamError.CreateError (FStream.Position, sUCS_U8ConverErr);
  for i := 1 to length(UTF8) do begin
    if (FBufPos = FBufSize) then
      csWriteBuffer;
    FBuffer[FBufPos] := UTF8[i];
    inc(FBufPos);
  end;
end;
{--------}
procedure TVpOutCharFilter.csSetFormat(const aValue : TVpStreamFormat);
var
  TooLate : Boolean;
begin
    case Format of
      sfUTF8     : TooLate := (FSetUTF8Sig and (Position > 3)) or
                              ((not FSetUTF8Sig) and (Position > 0));
      sfUTF16LE  : TooLate := (Position > 2);
      sfUTF16BE  : TooLate := (Position > 2);
      sfISO88591 : TooLate := (Position > 0);
    else
      TooLate := true;
    end;
    if not TooLate then begin
      FBufPos := 0;
      FFormat := aValue;
      case Format of
        sfUTF8:
          if FSetUTF8Sig then begin
            FBuffer[0] := #$EF;
            FBuffer[1] := #$BB;
            FBuffer[2] := #$BF;
            FBufPos := 3;
          end;
        sfUTF16LE : begin
                      FBuffer[0] := #$FF;
                      FBuffer[1] := #$FE;
                      FBufPos := 2;
                    end;
        sfUTF16BE : begin
                      FBuffer[0] := #$FE;
                      FBuffer[1] := #$FF;
                      FBufPos := 2;
                    end;
      else
        FBufPos := 0;
      end;
    end;
end;
{--------}
procedure TVpOutCharFilter.csWriteBuffer;
begin
  FStream.WriteBuffer(FBuffer^, FBufPos);
  FBufPos := 0;
end;
{--------}
procedure TVpOutCharFilter.PutUCS4Char(aCh : TVpUcs4Char);
begin
  case Format of
    sfUTF8     : csPutUTF8Char(aCh);
  end;
end;
{--------}
function TVpOutCharFilter.PutChar(aCh1, aCh2 : DOMChar;
                              var aBothUsed  : Boolean) : Boolean;
var
  OutCh : TVpUCS4Char;
begin
  Result := VpUTF16toUCS4(aCh1, aCh2, OutCh, aBothUsed);
  if Result then
    PutUCS4Char(OutCh);
end;
{--------}
function TVpOutCharFilter.PutString(const aText : DOMString) : Boolean;
var
  aBothUsed : Boolean;
  aLen, aPos : Integer;
begin
  aLen := Length(aText);
  aPos := 1;
  Result := True;
  while Result and (aPos <= aLen) do begin
    if aPos = aLen then
      Result := PutChar(aText[aPos], aText[aPos], aBothUsed)
    else
      Result := PutChar(aText[aPos], aText[aPos + 1], aBothUsed);
    if Result then
      if aBothUsed then
        inc(aPos, 2)
      else
        inc(aPos, 1);
  end;
end;
{--------}
function TVpOutCharFilter.Position : integer;
begin
  Result := FStreamPos + FBufPos;
end;

end.

