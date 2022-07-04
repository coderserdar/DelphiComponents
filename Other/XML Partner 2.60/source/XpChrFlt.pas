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
 * The Original Code is TurboPower XMLPartner
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
{*********************************************************}
{* XMLPartner: XpChrFlt.PAS                              *}
{*********************************************************}
{* XMLPartner: Character streams, input and output       *}
{*********************************************************}

{$I XpDefine.inc}

unit XpChrFlt;

interface

uses
  SysUtils,
  Classes,
  XpBase,
  XpExcept;

const
  XPEndOfStream = 1;                                                   {!!.52}{!!.55}
  XPEndOfReplaceText = 2;                                              {!!.52}{!!.55}
  XPNullChar = 3;                                                      {!!.52}{!!.55}

type
  TXpStreamFormat = {character formats of stream...}
     (sfUTF8,       {..UTF8 -- the default}
      sfUTF16LE,    {..UTF16, little endian (eg, Intel)}
      sfUTF16BE,    {..UTF16, big endian}
      sfISO88591);  {..ISO-8859-1, or Latin 1}

{Begin !!.52}
  TXpDirContextType = (xpdcLocal, xpdcHTTP, xpdcFTP, xpdcInetRelative,
                       xpdcLocalRelative);
  TXpBaseDirContext = class
  protected
    FContextType : TXpDirContextType;
    FDir : string;
    FFile : string;
    procedure Evaluate(const sSrcName : string;
                             oContext : TXpBaseDirContext); virtual; abstract;
  public
    constructor Make(oCurrentContext : TXpBaseDirContext;
               const sSrcName : string); virtual; abstract;

    function URL : string;

    property ContextType : TXpDirContextType
      read FContextType;

    property Dir : string
      read FDir;

    property Filename : string
      read FFile;

  end;
{End !!.52}

  TXpBaseCharFilter = class(TObject)
    protected
      FBufSize : Longint;
      FBuffer  : PAnsiChar;
      FBufPos  : Longint;
      FFormat  : TXpStreamFormat; {The format of the incoming stream}
      FFreeStream : Boolean;                                           {!!.52}
      FStream  : TStream;
      FStreamPos : Longint;
      FStreamSize : Longint;
    protected
      function csGetSize : Longint; virtual;
      procedure csSetFormat(const aValue : TXpStreamFormat); virtual; abstract;
    public
      constructor Create(aStream : TStream; const aBufSize : Longint); virtual;
      destructor Destroy; override;

      property BufSize : Longint
         read FBufSize;

{Begin !!.52}
      property FreeStream : Boolean
         read FFreeStream write FFreeStream;
         { If set to True then the character filter will free the stream
           passed to the the constructor when the character filter is freed.
           Defaults to False. }
{End !!.52}

      property Stream : TStream
         read FStream;

    end;

  TXpInCharFilter = class(TXpBaseCharFilter)
    private
      FBufEnd    : Longint;
      FDirContext: TXpBaseDirContext;                                  {!!.52}
      FUCS4Char  : TXpUcs4Char;
      FLine      : Longint;
      FLinePos   : Longint;
      FLastChar  : DOMChar;                                            {!!.52}
      FEOF       : Boolean;
      FBufDMZ    : Longint;                                            {!!.51}
      FInTryRead : Boolean;                                            {!!.51}
    protected
      procedure csAdvanceLine;
      procedure csAdvanceLinePos;
      procedure csGetCharPrim(var aCh : TXPUcs4Char);                  {!!.55}
      function csGetNextBuffer : Boolean;
      function csGetIso88591Char : TXpUcs4Char;
      function csGetTwoAnsiChars(var Buffer) : Boolean;
      function csGetURL : string;                                      {!!.52}
      function csGetUtf16BEChar : TXpUcs4Char;
      function csGetUtf16LEChar : TXPUcs4Char;
      function csGetUtf8Char : TXpUcs4Char;
      procedure csIdentifyFormat;
      procedure csPushCharPrim(aCh : TXpUcs4Char);
      procedure csSetFormat(const aValue : TXpStreamFormat); override;

      procedure csGetChar(var aCh : TXpUcs4Char);                      {!!.55}

    public
      constructor Create(aStream : TStream; const aBufSize : Longint); override;
      destructor Destroy; override;                                    {!!.52}

      procedure SkipChar;                                              {!!.52}

      function TryRead(const S : array of Longint) : Boolean;
      function ReadChar : DOMChar;                                     {!!.52}
      function ReadAndSkipChar : DOMChar;                              {!!.52}

{Begin !!.52}
      property DirContext : TXpBaseDirContext
       read FDirContext
       write FDirContext;
{End !!.52}
      property Format : TXpStreamFormat
        read FFormat
        write csSetFormat;
      property isEOF : Boolean                                         {!!.52}
        read FEOF;
      property Line : LongInt
        read FLine;
      property LinePos : LongInt
        read FLinePos;
      property URL : string                                            {!!.52}
         read csGetURL;                                                {!!.52}
  end;

  TXpOutCharFilter = class(TXpBaseCharFilter)
    protected
      FFormat : TXpStreamFormat;
      FSetUTF8Sig : Boolean;                                           {!!.51}
    protected
      function csGetSize : LongInt; override;
      procedure csPutIso88591Char(const aCh : TXpUcs4Char);
      procedure csPutUtf16BEChar(const aCh : TXpUcs4Char);
      procedure csPutUtf16LEChar(const aCh : TXpUcs4Char);
      procedure csPutUtf8Char(const aCh : TXpUcs4Char);
      procedure csSetFormat(const aValue : TXpStreamFormat); override;
      procedure csWriteBuffer;
    public
      constructor Create(aStream : TStream; const aBufSize : Longint); override;
      destructor Destroy; override;

      procedure PutUCS4Char(aCh : TXpUcs4Char);
      function  PutChar(aCh1, aCh2 : DOMChar;                          {!!.51}
                    var aBothUsed  : Boolean) : Boolean;               {!!.51}
      function  PutString(const aText : DOMString) : Boolean;          {!!.51}
      function Position : integer;

      property Format : TXpStreamFormat
         read FFormat
         write csSetFormat;
{Begin !!.51}
      property WriteUTF8Signature : Boolean
         read FSetUTF8Sig
         write FSetUTF8Sig;
{End !!.51}
      property Size : LongInt
         read csGetSize;

  end;

{Begin !!.51}
{ Utility functions }
function MapCharEncToStreamFormat(const aCharEnc : TXpCharEncoding) : TXpStreamFormat;
function MapStreamFormatToCharEnc(const aStreamFormat: TxpStreamFormat) : TXpCharEncoding;
{End !!.51}

implementation

const
  {BufDMZ  : Longint = 64;}                                            {!!.51 - Deleted}
  CR      = 13; {Carriage return}
  LF      = 10; {Line feed}

{===TXpBaseDirContext================================================}
function TXpBaseDirContext.URL : string;
begin
  Result := FDir + FFile;
end;
{====================================================================}

{===TXpBaseCharFilter=================================================}
constructor TXpBaseCharFilter.Create(aStream  : TStream;
                               const aBufSize : Longint);
begin
  inherited Create;
  Assert(Assigned(aStream));
  FBufSize := aBufSize;
  FBufPos := 0;
  FFormat := sfUTF8;
  FFreeStream := True;                                                 {!!.52}{!!.53}
  FStream := aStream;
  FStreamPos := aStream.Position;
  FStreamSize := aStream.Size;
  GetMem(FBuffer, FBufSize);
end;
{--------}
destructor TXpBaseCharFilter.Destroy;
begin
  if Assigned(FBuffer) then begin
    FreeMem(FBuffer, FBufSize);
    FBuffer := nil;
  end;

  if FFreeStream then                                                  {!!.52}
    FStream.Free;                                                      {!!.52}

  inherited Destroy;
end;
{--------}
function TXpBaseCharFilter.csGetSize : LongInt;
begin
  Result := FStreamSize;
end;
{====================================================================}

{===TXpInCharFilter==================================================}
constructor TXpInCharFilter.Create(aStream  : TStream;
                             const aBufSize : Longint);
begin
  inherited Create(aStream, aBufSize);
  if FStreamSize <= aBufSize then
    FBufDMZ := 0                                                       {!!.51}
  else                                                                 {!!.51}
    FBufDMZ := 64;                                                     {!!.51}
  FBufEnd := 0;
  FDirContext := nil;                                                  {!!.52}
  FLine := 1;
  FLinePos := 1;
  csIdentifyFormat;
  if aStream.Size > 0 then
    FEOF := False
  else
    FEOF := True;
  FUCS4Char := XPNullChar;                                             {!!.52}{!!.55}
  FInTryRead := False;                                                 {!!.51}
end;
{Begin !!.52}
{--------}
destructor TXpInCharFilter.Destroy;
begin
  FDirContext.Free;
  inherited Destroy;
end;
{End !!.52}
{--------}
procedure TXpInCharFilter.csAdvanceLine;
begin
  FLine := FLine + 1;                                                  {!!.55}
  FLinePos := 1;
end;
{--------}
procedure TXpInCharFilter.csAdvanceLinePos;
begin
  FLinePos := FLinePos + 1;                                            {!!.55}
end;
{--------}
procedure TXpInCharFilter.csGetCharPrim(var aCh : TXpUcs4Char);        {!!.55}
begin
  {Note: as described in the XML spec (2.11) all end-of-lines are
         passed as LF characters no matter what the original document
         had. This routine converts a CR/LF pair to a single LF, a
         single CR to an LF, and passes LFs as they are.}

  {get the first (test) character}
  {first check the UCS4Char buffer to see if we have a character there;
   if so get it}
  if (FUCS4Char <> XpNullChar) then begin                              {!!.52}{!!.55}
    aCh := FUCS4Char;
    FUCS4Char := XpNullChar;                                           {!!.52}{!!.55}
  end
  {otherwise get a character from the buffer; this depends on the
   format of the stream of course}
  else begin
    case Format of
      sfUTF8     : aCh := csGetUtf8Char;
      sfUTF16LE  : aCh := csGetUtf16LEChar;
      sfUTF16BE  : aCh := csGetUtf16BEChar;
      sfISO88591 : aCh := csGetIso88591Char;
    else
      {it is next to impossible that this else clause is reached; if
       it is we're in deep doggy doo-doo, so pretending that it's the
       end of the stream is the least of our worries}
      aCh := XpEndOfStream;                                            {!!.52}{!!.55}
    end;
  end;

  {if we got a CR, then we need to see what the next character is; if
   it is an LF, return LF; otherwise put the second character  back
   and still return an LF}
  if (aCh = CR) then begin
    if (FUCS4Char <> XpNullChar) then begin                            {!!.52}{!!.55}
      aCh := FUCS4Char;
      FUCS4Char := XPNullChar;                                         {!!.52}{!!.55}
    end
    else begin
      case Format of
        sfUTF8     : aCh := csGetUtf8Char;
        sfUTF16LE  : aCh := csGetUtf16LEChar;
        sfUTF16BE  : aCh := csGetUtf16BEChar;
        sfISO88591 : aCh := csGetIso88591Char;
      else
        aCh := XPEndOfStream;                                          {!!.52}{!!.55}
      end;
    end;
    if (aCh <> LF) and (aCh <> XpEndOfStream) then                     {!!.55}
      csPushCharPrim(aCh);
    aCh := LF;
  end;

  {check to see that the character is valid according to XML}
  if (aCh <> XpEndOfStream) and (not XpIsChar(aCh)) then               {!!.52}{!!.55}
    raise ExpFilterError.CreateError(FStream.Position,
                                     Line,
                                     LinePos,
                                     sInvalidXMLChar);
end;
{--------}
function TXpInCharFilter.csGetIso88591Char : TXpUcs4Char;
begin
  {if the buffer is empty, fill it}
  if (FBufPos >= FBufEnd - FBufDMZ) and                                {!!.51}
     (not FInTryRead) then begin                                       {!!.51}
    {if we exhaust the stream, there are no more characters}
    if not csGetNextBuffer then begin
      Result := XpEndOfStream;                                         {!!.52}{!!.55}
      Exit;
    end;
  end;
  {get the first byte character from the buffer}
  XPIso88591ToUcs4(FBuffer[FBufPos], Result);
  FBufPos := FBufPos + 1;                                              {!!.55}
end;
{--------}
function TXpInCharFilter.csGetNextBuffer : Boolean;
begin
  if FStream.Position > FBufDMZ then                                   {!!.51}
    {Account for necessary buffer overlap}
    FStream.Position := FStream.Position - (FBufEnd - FBufPos);
  FBufEnd := FStream.Read(FBuffer^, FBufSize);
  FStreamPos := FStream.Position;
  FBufPos := 0;
  Result := FBufEnd <> 0;
end;
{--------}
function TXpInCharFilter.csGetTwoAnsiChars(var Buffer) : Boolean;
type
  TTwoChars = array [0..1] of AnsiChar;
var
  i : integer;
begin
  {get two byte characters from the stream}
  for i := 0 to 1 do begin
    {if the buffer is empty, fill it}
    if (FBufPos >= FBufEnd - FBufDMZ) and                              {!!.51}
       (not FInTryRead) then begin                                     {!!.51}
      {if we exhaust the stream, we couldn't satisfy the request}
      if not csGetNextBuffer then begin
        Result := false;
        Exit;
      end;
    end;
    {get the first byte character from the buffer}
    TTwoChars(Buffer)[i] := FBuffer[FBufPos];
    FBufPos := FBufPos + 1;                                            {!!.55}
  end;
  Result := True;
end;
{Begin !!.52}
{--------}
function TXpInCharFilter.csGetURL : string;
begin
  if FDirContext = nil then
    Result := ''
  else
    Result := FDirContext.URL;
end;
{End !!.52}
{--------}
function TXpInCharFilter.csGetUtf16BEChar : TXpUcs4Char;
var
  BothUsed : Boolean;                                                  {!!.51}
  Chars    : array [0..1] of AnsiChar;
  W1, W2   : DOMChar;                                                  {!!.52}
begin
  {get two byte characters}
  if not csGetTwoAnsiChars(Chars) then begin
    Result := XpEndOfStream;                                           {!!.52}{!!.55}
    Exit;
  end;
  {convert the two byte characters into a DOMChar}
  W1 := DOMChar((Integer(Chars[0]) shl 8) + Integer(Chars[1]));        {!!.52}
  {if this isn't a surrogate character, clear the second DOMChar}
  if not XPIsSurrogate(W1) then
    W2 := #0
  {if it was a surrogate character we'll need the next WideChar as
   well since they'll be combined into one}
  else begin
    {get two byte characters}
    if not csGetTwoAnsiChars(Chars) then begin
      Result := XpEndOfStream;                                         {!!.52}{!!.55}
      Exit;
    end;
    {convert the two byte characters into a DOMChar}
    W2 := DOMChar((Integer(Chars[0]) shl 8) + Integer(Chars[1]));      {!!.52}
  end;
  {convert the one or two DOMChars to a single UCS-4 char}
  if not XPUtf16ToUcs4(W1, W2, Result, BothUsed) then                  {!!.51}
    raise ExpFilterError.CreateError(FStream.Position,
                                     Line, LinePos,
                                     sInvalidBEChar);
end;
{--------}
function TXpInCharFilter.csGetUtf16LEChar : TXpUcs4Char;
var
  BothUsed : Boolean;                                                  {!!.51}
  Chars    : array [0..1] of AnsiChar;
  W1, W2   : DOMChar;                                                  {!!.52}
begin
  {get two byte characters}
  if not csGetTwoAnsiChars(Chars) then begin
    Result := XpEndOfStream;                                           {!!.52}{!!.55}
    Exit;
  end;
  {convert the two byte characters into a DOMChar}
  W1 := DOMChar((Integer(Chars[1]) shl 8) + Integer(Chars[0]));        {!!.52}
  {if this isn't a surrogate character, clear the second DOMChar}
  if not XPIsSurrogate(W1) then
    W2 := #0
  {if it was a surrogate character we'll need the next DOMChar as
   well since they'll be combined into one}
  else begin
    {get two byte characters}
    if not csGetTwoAnsiChars(Chars) then begin
      Result := XpEndOfStream;                                         {!!.52}{!!.55}
      Exit;
    end;
    {convert the two byte characters into a DOMChar}
    W2 := DOMChar((Integer(Chars[1]) shl 8) + Integer(Chars[0]));      {!!.52}
  end;
  {convert the one or two DOMChars to a single UCS-4 char}
  if not XPUtf16ToUcs4(W1, W2, Result, BothUsed) then                  {!!.51}
    raise ExpFilterError.CreateError(FStream.Position,
                                     Line, LinePos,
                                     sInvalidLEChar);
end;
{--------}
function TXpInCharFilter.csGetUtf8Char : TXpUcs4Char;
var
  Utf8Char : TXpUtf8Char;
  {Ch       : AnsiChar;}                                               {!!.52}
  Len      : Byte;                                                     {!!.55}
  i        : Byte;                                                     {!!.55}
begin
  {if the buffer is empty, fill it}
  if (not FInTryRead) and                                              {!!.51}{!!.52}
     (FBufPos >= FBufEnd - FBufDMZ) then begin                         {!!.51}{!!.52}
    {if we exhaust the stream, there are no more characters}
    if not csGetNextBuffer then begin
      Result := XPEndOfStream;                                         {!!.52}{!!.55}
      Exit;
    end;
  end;
  {get the first byte character from the buffer}
  Utf8Char[1] := FBuffer[FBufPos];                                     {!!.52 - Start}
  FBufPos := FBufPos + 1;
  {determine the length of the Utf8 character from this}
  Len := XPGetLengthUtf8(Utf8Char[1]);
  if (Len < 1) then
    raise ExpFilterError.CreateError(FStream.Position,
                                     Line,
                                     LinePos,
                                     sBadUTF8Char);
  {Move(Len, Utf8Char[0], 1);}                                         {!!.52 - End}{!!.55}
  Utf8Char[0] := Char(Len);                                            {!!.55}
  {get the remaining characters from the stream}
  for i := 2 to Len do begin
    {if the buffer is empty, fill it}
    if (FBufPos >= FBufEnd - FBufDMZ) and                              {!!.51}
       (not FInTryRead) then begin                                     {!!.51}
      {if we exhaust the stream now, it's a badly formed UTF8
       character--true--but we'll just pretend that the last character
       does not exist}
      if not csGetNextBuffer then begin
        Result := XpEndOfStream;                                       {!!.52}{!!.55}
        Exit;
      end;
    end;
    {get the next byte character from the buffer}
    Utf8Char[i] := FBuffer[FBufPos];
    FBufPos := FBufPos + 1;                                            {!!.52}
  end;
  {convert the UTF8 character into a UCS4 character}
  if (not XPUtf8ToUcs4(Utf8Char, Len, Result)) then                    {!!.52}
    raise ExpFilterError.CreateError(FStream.Position,
                                     Line,
                                     LinePos,
                                     sBadUTF8Char);
end;
{--------}
procedure TXpInCharFilter.csIdentifyFormat;
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
    end else if (FBuffer[0] = #$EF) and                                {!!.51 - Start}
                (FBuffer[1] = #$BB) and
                (FBuffer[2] = #$BF) then begin
      FFormat := sfUTF8;
      FBufPos := 3;
    end else                                                           {!!.51 - End}
      FFormat := sfUTF8
  else
    FFormat := sfUTF8;
end;
{--------}
procedure TXpInCharFilter.csPushCharPrim(aCh : TXpUcs4Char);
begin
  Assert(FUCS4Char = XPNullChar);                                      {!!.55}
  {put the char into the buffer}
  FUCS4Char := aCh;
end;
{--------}
procedure TXpInCharFilter.csSetFormat(const aValue : TXpStreamFormat);
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
procedure TXpInCharFilter.csGetChar(var aCh        : TXpUcs4Char);     {!!.55}
begin
  {get the next character; for an EOF raise an exception}
  csGetCharPrim(aCh);                                                  {!!.55}
  if (aCh = XpEndOfStream) then                                        {!!.52}{!!.55}
    FEOF := True
  else
    {maintain the line/character counts}
    if (aCh = LF) then
      csAdvanceLine
    else
      csAdvanceLinePos;
end;
{--------}
function TXpInCharFilter.TryRead(const S : array of Longint) : Boolean;
var
  Idx         : Longint;
  Ch          : TxpUcs4Char;
  {IL          : Boolean;}                                             {!!.55}
  OldBufPos   : Longint;
  OldChar     : DOMChar;                                               {!!.52}
  OldUCS4Char : TXpUcs4Char;
  OldLinePos  : Longint;
  OldLine     : Longint;
begin
  OldBufPos := FBufPos;
  OldChar := FLastChar;
  OldUCS4Char := FUCS4Char;
  OldLinePos := LinePos;
  OldLine := Line;
  Result := True;
  FInTryRead := True;                                                  {!!.51}
  try
    for Idx := Low(s) to High(S) do begin
      csGetChar(Ch);                                                   {!!.55}
      if (Ch <> S[Idx]) then begin                                     {!!.55}
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
      FLastChar := #0;                                                 {!!.52}
      FUCS4Char := XpNullChar;                                         {!!.52}{!!.55}
      if (FStreamSize = FStreamPos) and
         (FBufPos = FBufEnd) then
//         (FBufPos = FBufEnd) then
        FEOF := True;
    end;
    FInTryRead := False;                                               {!!.51}
  end;
end;
{--------}
procedure TXpInCharFilter.SkipChar;                                    {!!.52 - Start}
begin
  FLastChar := #0;
  FUCS4Char := XpNullChar;                                             {!!.55}
  FLinePos := FLinePos + 1;                                            {!!.55}
end;
{--------}
function TXpInCharFilter.ReadandSkipChar : DOMChar;
var
  Ch     : TxpUCS4Char;
  {IL     : Boolean;}                                                  {!!.55}
begin
  if FLastChar = '' then begin
    csGetChar(Ch);                                                     {!!.55}
    XPUcs4ToWideChar(Ch, Result);
  end else begin
    Result := FLastChar;
    FLinePos := FLinePos + 1;                                          {!!.55}
  end;
  FLastChar := #0;
  FUCS4Char := XpNullChar;                                             {!!.55}
  if (FStreamSize = FStreamPos) and
     (FBufPos = FBufEnd) then
    FEOF := True;
end;                                                                   {!!.52 - End}
{--------}
function TXpInCharFilter.ReadChar : DOMChar;                           {!!.52 - Rewritten - Start}
var
  Ch     : TxpUCS4Char;
  {IL     : Boolean;}                                                  {!!.55}
 begin
  if FLastChar = '' then begin
    csGetChar(Ch);                                                     {!!.55}
    XPUcs4ToWideChar(Ch, Result);
    Dec(FLinePos);
    FLastChar := Result;
    if (FUCS4Char <> XpNullChar) then                                  {!!.55}
      if (Format = sfUTF16LE) or
         (Format = sfUTF16BE) then
        Dec(FBufPos, 2)
      else if FBufPos > 0 then
        Dec(FBufPos, 1);
    FUCS4Char := Ch;
  end else
    Result := FLastChar;
end;                                                                   {!!.52 - Rewritten - End}
{====================================================================}


{===TXpOutCharFilter=================================================}
constructor TXpOutCharFilter.Create(aStream : TStream; const aBufSize : Longint);
begin
  inherited Create(aStream, aBufSize);
  FFreeStream := False;                                                {!!.53}
  FSetUTF8Sig := True;                                                 {!!.51}
end;
{--------}
destructor TXpOutCharFilter.Destroy;
begin
  if Assigned(FBuffer) then
    if (FBufPos > 0) then
      csWriteBuffer;

  inherited Destroy;
end;
{--------}
function TXpOutCharFilter.csGetSize : LongInt;
begin
  Result := FStream.Size + FBufPos;
end;
{--------}
procedure TXpOutCharFilter.csPutIso88591Char(const aCh : TXpUcs4Char);
var
  Ch : AnsiChar;
begin
  if not XPUcs4ToIso88591(aCh, Ch) then
    raise ExpStreamError.CreateError(FStream.Position, sUCS_ISOConvertErr);
  if (FBufPos = FBufSize) then
    csWriteBuffer;
  FBuffer[FBufPos] := Ch;
  FBufPos := FBufPos + 1;                                              {!!.55}
end;
{--------}
procedure TXpOutCharFilter.csPutUtf16BEChar(const aCh : TXpUcs4Char);
var
  W1, W2 : DOMChar;                                                    {!!.52}
begin
  {convert the UCS-4 character into one (or maybe two) DOMChars}
  if not XPUcs4ToUtf16(aCh, W1, W2) then
    raise ExpStreamError.CreateError(FStream.Position,
                                     sUCS_U16ConvertErr);
  {for big-endian streams we output the most significant byte of the
   DOMChar first, followed by the least significant byte}
  if (FBufPos = FBufSize) then
    csWriteBuffer;
  FBuffer[FBufPos] := AnsiChar(integer(W1) shr 8);
  FBufPos := FBufPos + 1;                                              {!!.55}
  if (FBufPos = FBufSize) then
    csWriteBuffer;
  FBuffer[FBufPos] := AnsiChar(integer(W1) and $FF);
  FBufPos := FBufPos + 1;                                              {!!.55}
  if (W2 <> #0) then begin
    if (FBufPos = FBufSize) then
      csWriteBuffer;
    FBuffer[FBufPos] := AnsiChar(integer(W2) shr 8);
    FBufPos := FBufPos + 1;                                            {!!.55}
    if (FBufPos = FBufSize) then
      csWriteBuffer;
    FBuffer[FBufPos] := AnsiChar(integer(W2) and $FF);
    FBufPos := FBufPos + 1;                                            {!!.55}
  end;
end;
{--------}
procedure TXpOutCharFilter.csPutUtf16LEChar(const aCh : TXpUcs4Char);
var
  W1, W2 : DOMChar;                                                    {!!.52}
begin
  {convert the UCS-4 character into one (or maybe two) DOMChars}
  if not XPUcs4ToUtf16(aCh, W1, W2) then
    raise ExpStreamError.CreateError(FStream.Position,
                                     sUCS_U16ConvertErr);
  {for little-endian streams we output the least significant byte of
   the DOMChar first, followed by the most significant byte}
  if (FBufPos = FBufSize) then
    csWriteBuffer;
  FBuffer[FBufPos] := AnsiChar(integer(W1) and $FF);
  FBufPos := FBufPos + 1;                                              {!!.55}
  if (FBufPos = FBufSize) then
    csWriteBuffer;
  FBuffer[FBufPos] := AnsiChar(integer(W1) shr 8);
  FBufPos := FBufPos + 1;                                              {!!.55}
  if (W2 <> #0) then begin
    if (FBufPos = FBufSize) then
      csWriteBuffer;
    FBuffer[FBufPos] := AnsiChar(integer(W2) and $FF);
    FBufPos := FBufPos + 1;                                            {!!.55}
    if (FBufPos = FBufSize) then
      csWriteBuffer;
    FBuffer[FBufPos] := AnsiChar(integer(W2) shr 8);
    FBufPos := FBufPos + 1;                                            {!!.55}
  end;
end;
{--------}
procedure TXpOutCharFilter.csPutUtf8Char(const aCh : TXpUcs4Char);
var
  UTF8 : TXpUtf8Char;
  i    : integer;
begin
  if not XPUcs4ToUtf8(aCh, UTF8) then
    raise ExpStreamError.CreateError(FStream.Position, sUCS_U8ConverErr);
  for i := 1 to length(UTF8) do begin
    if (FBufPos = FBufSize) then
      csWriteBuffer;
    FBuffer[FBufPos] := UTF8[i];
    FBufPos := FBufPos + 1;                                            {!!.55}
  end;
end;
{--------}
procedure TXpOutCharFilter.csSetFormat(const aValue : TXpStreamFormat);
var
  TooLate : Boolean;
begin
//  if (aValue <> FFormat) then begin                                  {Deleted !!.51}
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
{Begin !!.51}
        sfUTF8:
          if FSetUTF8Sig then begin
            FBuffer[0] := #$EF;
            FBuffer[1] := #$BB;
            FBuffer[2] := #$BF;
            FBufPos := 3;
          end;
{End !!.51}
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
//  end;                                                               {Deleted !!.51}
end;
{--------}
procedure TXpOutCharFilter.csWriteBuffer;
begin
  FStream.WriteBuffer(FBuffer^, FBufPos);
  FBufPos := 0;
end;
{--------}
procedure TXpOutCharFilter.PutUCS4Char(aCh : TXpUcs4Char);
begin
  case Format of
    sfUTF8     : csPutUTF8Char(aCh);
    sfUTF16LE  : csPutUTF16LEChar(aCh);
    sfUTF16BE  : csPutUTF16BEChar(aCh);
    sfISO88591 : csPutIso88591Char(aCh);
  end;
end;
{Begin !!.51}
{--------}
function TXpOutCharFilter.PutChar(aCh1, aCh2 : DOMChar;                {!!.52}
                              var aBothUsed  : Boolean) : Boolean;
var
  OutCh : TXpUCS4Char;
begin
  Result := XPUTF16toUCS4(aCh1, aCh2, OutCh, aBothUsed);
  if Result then
    PutUCS4Char(OutCh);
end;
{--------}
function TXpOutCharFilter.PutString(const aText : DOMString) : Boolean;
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
        aPos := aPos + 2                                               {!!.55}
      else
        aPos := aPos + 1;                                              {!!.55}
  end;
end;
{End !!.51}
{--------}
function TXpOutCharFilter.Position : integer;
begin
  Result := FStreamPos + FBufPos;
end;
{====================================================================}

{===Utility functions================================================}
function MapCharEncToStreamFormat(const aCharEnc : TXpCharEncoding) : TXpStreamFormat;
begin
  case aCharEnc of
    ceUTF8 :
      Result := sfUTF8;
    ceUTF16LE :
      Result := sfUTF16LE;
    ceUTF16BE :
      Result := sfUTF16BE;
    ceISO88591 :
      Result := sfISO88591;
    else
      Result := sfISO88591;
  end;  { case }
end;
{--------}
function MapStreamFormatToCharEnc(const aStreamFormat: TxpStreamFormat) : TXpCharEncoding;
begin
  case aStreamFormat of
    sfUTF8 :
      Result := ceUTF8;
    sfUTF16LE :
      Result := ceUTF16LE;
    sfUTF16BE :
      Result := ceUTF16BE;
    sfISO88591 :
      Result := ceISO88591;
    else
      Result := ceUnknown;
  end;  { case }
end;
{====================================================================}
end.

