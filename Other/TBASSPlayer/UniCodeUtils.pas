// unit UniCodeUtils

//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
//
// Ver 1.0                         3 Sep 2008
//   - Initial release


unit UniCodeUtils;

interface

 {$INCLUDE Delphi_Ver.inc}

uses Windows;

{$IFDEF DELPHI_6_BELOW}

type

  UTF8string = type ansistring;     // defined in system.pas of Delphi 7

 // Following functions are defined in system.pas of Delphi 7
  function Utf8Decode(const S: UTF8String): WideString;
  function UTF8Encode(const WS: WideString): UTF8String;

 // function DupeString is defined in StrUtils.pas of Delphi 7
  function DupeString(const AText: ansistring; ACount: Integer): ansistring;
 {$ENDIF}

  function ToPWideChar(s: pAnsiChar): PWideChar;
  function ToPMultiByte(s: pWideChar): PAnsiChar;
  function ToWideString(s: AnsiString): WideString;
  function StrToUTF8(s: pAnsiChar): UTF8string;


implementation

const
  Max_Wide_Len = 8192;
  Max_Ansi_Len = Max_Wide_Len;

type
  pW = array[0..Max_Wide_Len-1] of WideChar;
  pa = array[0..Max_Ansi_Len-1] of AnsiChar;
var
  pw1 : pW;
  pa1 : pa;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
end;

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := AnsiChar(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := AnsiChar($E0 or (c shr 12));
        Dest[count+1] := AnsiChar($80 or ((c shr 6) and $3F));
        Dest[count+2] := AnsiChar($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := AnsiChar($C0 or (c shr 6));
        Dest[count+1] := AnsiChar($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

{$IFDEF DELPHI_6_BELOW}
function Utf8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PAnsiChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Encode(const WS: WideString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function DupeString(const AText: ansistring; ACount: Integer): ansistring;
var
  P: PAnsiChar;
  C: Integer;
begin
  C := Length(AText);
  SetLength(Result, C * ACount);
  P := Pointer(Result);
  if P = nil then Exit;
  while ACount > 0 do
  begin
    Move(Pointer(AText)^, P^, C);
    Inc(P, C);
    Dec(ACount);
  end;
end;
{$ENDIF}

function ToPWideChar(s: pAnsiChar): PWideChar;
var
 //  pw1 : array[0..1024] of WideChar;
   nLen1, nLen2, nLen3 : integer;
begin
   if s = '' then
   begin
      pw1[0] := AnsiChar(0);  // *** We should put chr(0) at the end of string
      Result := @pw1[0];
      exit;
   end;

   nLen1 := Length(s);
 //  nLen2 := length(pw1);
   nLen2 := SizeOf(pw1) - 1;
   nLen3 := MultiByteToWideChar(CP_OEMCP, MB_PRECOMPOSED, s, nLen1, @pw1[0], nLen2);

  { if nLen3 > 0 then
   begin  }
      pw1[nLen3] := ansichar(0);  // *** We should put chr(0) at the end of string
      Result := @pw1[0];
  { end else
      Result := nil; }
end;

function ToWideString(s: AnsiString): WideString;
var
   p : pByte;
begin
   p := @pa1[0];
   Move(s[1], p^, length(s));
   inc(p, length(s));
   p^ := 0;

   Result := ToPWideChar(@pa1[0]);
end;

function ToPMultiByte(s: pWideChar): PAnsiChar;
var
  // pw1 : array[0..1024] of Char;
   nLen1, nLen2, nLen3 : integer;
begin
   if s = '' then
   begin
      pa1[0] := AnsiChar(0);  // *** We should put chr(0) at the end of string
      Result := @pa1[0];
      exit;
   end;

   nLen1 := Length(s);
 //  nLen2 := length(pw1);
   nLen2 := SizeOf(pa1) - 1;
   nLen3 := WideCharToMultiByte(CP_OEMCP, WC_COMPOSITECHECK, s, nLen1, @pa1[0], nLen2, nil, nil);

  { if nLen3 > 0 then
   begin }
      pa1[nLen3] := AnsiChar(0);  // *** We should put chr(0) at the end of string
      Result := @pa1[0];
  { end else
      Result := nil;  }
end;

function StrToUTF8(s: pAnsiChar): UTF8string;
var
   pw_char : PWideChar;
begin
   pw_char := ToPWideChar(s);
   if pw_char <> nil then
      result := UTF8Encode(WideString(pw_char))
   else
      result := '';
end;

end.


