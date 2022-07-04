unit ATxSProc;

interface

uses
  Windows, SysUtils;

procedure SReplace(var S: string; const SFrom, STo: string);
procedure SReplaceW(var S: WideString; const SFrom, STo: WideString);
procedure SReplaceAll(var S: string; const SFrom, STo: string);
procedure SReplaceAllW(var S: WideString; const SFrom, STo: WideString);
procedure SReplaceIAll(var S: string; const SFrom, STo: string);

type
  TStringDecodeRec = record
    SFrom, STo: AnsiString;
  end;

  TStringTabOptions = record
    TabSize: Integer;
    TabPosition: Integer;
    FontMonospaced: Boolean;
    NonPrintableShow: Boolean;
    NonPrintableChar: WideChar;
  end;

function SDecodeW(const S: WideString; const Decode: array of TStringDecodeRec): WideString;
procedure SReplaceZeros(var S: AnsiString);
procedure SReplaceZerosW(var S: WideString);
procedure SDelLastSpace(var S: AnsiString);
procedure SDelLastSpaceW(var S: WideString);
procedure SDelLastSlash(var S: AnsiString);
procedure SDelLastSlashW(var S: WideString);
procedure SDelLastComma(var S: AnsiString);
function STabReplacement(const TabOptions: TStringTabOptions): WideString;
procedure SReplaceTabsW(var S: WideString; var TabOptions: TStringTabOptions);
function SCharCR(ch: WideChar): Boolean;
function SLastCharCR(const S: WideString): Boolean;

function SFormatW(const Msg: WideString; Params: array of WideString): WideString;
function SFormatWD(const Msg: WideString; Params: array of Integer): WideString;
function SCompareIW(const S1, S2: WideString): Integer;
function SetStringW(Buffer: PAnsiChar; BufSize: Integer; SwapBytes: Boolean): WideString;

procedure SDeleteFromStrA(var S: AnsiString; const SubStr: AnsiString);
procedure SDeleteFromStrW(var S: WideString; const SubStr: WideString);

function SExtractFileDir(const FileName: WideString): WideString;
function SExtractFilePath(const FileName: WideString): WideString;
function SExtractFileExt(const FileName: WideString): WideString;
function SExtractFileName(const FileName: WideString): WideString;

function SFindText(const F, S: AnsiString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;
function SFindTextW(const F, S: WideString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;

function IMin(N1, N2: Integer): Integer;
function IMax(N1, N2: Integer): Integer;
function WMin(N1, N2: Word): Word;
function WMax(N1, N2: Word): Word;
function I64Min(const N1, N2: Int64): Int64;
function I64Max(const N1, N2: Int64): Int64;

procedure ILimitMin(var N: Integer; Value: Integer);
procedure ILimitMax(var N: Integer; Value: Integer);
//procedure WLimitMin(var N: Word; Value: Word);
//procedure WLimitMax(var N: Word; Value: Word);
procedure I64LimitMin(var N: Int64; const Value: Int64);
procedure I64LimitMax(var N: Int64; const Value: Int64);

function SFileExtensionMatch(const FileName: WideString; const ExtList: AnsiString): Boolean;
function SFileExtensionMatch2(const FileName: WideString; const ExtList1, ExtList2: AnsiString): Boolean;


implementation

procedure SReplace(var S: string; const SFrom, STo: string);
var
  i: Integer;
begin
  i := Pos(SFrom, S);
  if i > 0 then
  begin
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  end;
end;

function SReplaceFunc(const S: string; const SFrom, STo: string): string;
begin
  Result := S;
  SReplace(Result, SFrom, STo);
end;

procedure SReplaceW(var S: WideString; const SFrom, STo: WideString);
var
  i: Integer;
begin
  i := Pos(SFrom, S);
  if i > 0 then
  begin
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  end;
end;

procedure SReplaceAll(var S: string; const SFrom, STo: string);
var
  i: Integer;
begin
  repeat
    i := Pos(SFrom, S);
    if i = 0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until False;
end;

procedure SReplaceAllW(var S: WideString; const SFrom, STo: WideString);
var
  i: Integer;
begin
  repeat
    i := Pos(SFrom, S); 
    if i = 0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until False;
end;

procedure SReplaceIAll(var S: string; const SFrom, STo: string);
var
  i: Integer;
begin
  repeat
    i := Pos(LowerCase(SFrom), LowerCase(S)); 
    if i = 0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until False;
end;

function SDecodeW(const S: WideString; const Decode: array of TStringDecodeRec): WideString;
var
  i, j: Integer;
  DoDecode: Boolean;
begin
  Result := '';
  i := 1;
  repeat
    if i > Length(S) then Break;
    DoDecode := False;
    for j := Low(Decode) to High(Decode) do
      with Decode[j] do
        if SFrom = Copy(S, i, Length(SFrom)) then
        begin
          DoDecode := True;
          Result := Result + STo;
          Inc(i, Length(SFrom));
          Break
        end;
    if DoDecode then Continue;
    Result := Result + S[i];
    Inc(i);
  until False;
end;


function SFormatW(const Msg: WideString; Params: array of WideString): WideString;
var
  i: Integer;
begin
  Result := Msg;
  for i := Low(Params) to High(Params) do
    SReplaceW(Result, '%s', Params[i]);
end;

function SFormatWD(const Msg: WideString; Params: array of Integer): WideString;
var
  i: Integer;
begin
  Result := Msg;
  for i := Low(Params) to High(Params) do
    SReplaceW(Result, '%d', IntToStr(Params[i]));
end;

function LastDelimiter(const Delimiters, S: WideString): Integer;
var
  i: Integer;
begin
  for i := Length(S) downto 1 do
    if Pos(S[i], Delimiters) > 0 then
      begin Result := i; Exit end;
  Result := 0;
end;

function SExtractFileDir(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  if (I > 1) and (FileName[I] = '\') 
    //and (not (AnsiChar(FileName[I - 1]) in ['\', ':'])) //was in SysUtils!
    then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function SExtractFilePath(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, 1, I);
end;

function SExtractFileExt(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('.\:', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function SExtractFileName(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function SCompareIW(const S1, S2: WideString): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := lstrcmpiW(PWideChar(S1), PWideChar(S2))
  else
    Result := lstrcmpiA(PAnsiChar(AnsiString(S1)), PAnsiChar(AnsiString(S2)));
end;

function SDefaultDelimiters: AnsiString;
const
  Chars: PAnsiChar = ':;<=>?' + '@[\]^' + '`{|}~';
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Ord('/') do
    Result := Result + AnsiChar(Chr(i));
  Result := Result + Chars;
end;

//--------------------------------------------------
function AnsiLowerCaseW(const S: WideString): WideString;
begin
  Result := S;
  if Result <> '' then
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      CharLowerBuffW(@Result[1], Length(Result))
    else
      Result := AnsiLowerCase(S);
  end;
end;

//--------------------------------------------------
function SFindText(const F, S: AnsiString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;
var
  SBuf, FBuf, Delimiters: AnsiString;
  Match: Boolean;
  LastPos, LengthF, i: Integer;
begin
  Result := 0;

  if (S = '') or (F = '') then Exit;

  Delimiters := SDefaultDelimiters;

  SBuf := S;
  FBuf := F;
  if not fCaseSens then
  begin
    SBuf := AnsiLowerCase(SBuf);
    FBuf := AnsiLowerCase(FBuf);
  end;

  LengthF := Length(F);
  LastPos := Length(S) - LengthF + 1;

  if fForward then
    //Search forward
    for i := 1 to LastPos do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LengthF);

      if fWholeWords then
        Match := Match
          and (fLastBlock or (i < LastPos))
          and ((i <= 1) or (Pos(S[i - 1], Delimiters) > 0))
          and ((i >= LastPos) or (Pos(S[i + LengthF], Delimiters) > 0));

      if Match then
      begin
        Result := i;
        Break
      end;
    end
    else
    //Search backward
    for i := LastPos downto 1 do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LengthF);

      if fWholeWords then
        Match := Match
          and (fLastBlock or (i > 1))
          and ((i <= 1) or (Pos(S[i - 1], Delimiters) > 0))
          and ((i >= LastPos) or (Pos(S[i + LengthF], Delimiters) > 0));

      if Match then
      begin
        Result := i;
        Break
      end;
    end;
end;

//--------------------------------------------------
function SFindTextW(const F, S: WideString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;
var
  SBuf, FBuf, Delimiters: WideString;
  Match: Boolean;
  LastPos, LengthF, i: Integer;
begin
  Result := 0;

  if (S = '') or (F = '') then Exit;

  Delimiters := SDefaultDelimiters;

  SBuf := S;
  FBuf := F;
  if not fCaseSens then
  begin
    SBuf := AnsiLowerCaseW(SBuf);
    FBuf := AnsiLowerCaseW(FBuf);
  end;

  LengthF := Length(F);
  LastPos := Length(S) - LengthF + 1;

  if fForward then
    //Search forward
    for i := 1 to LastPos do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LengthF * 2);

      if fWholeWords then
        Match := Match
          and (fLastBlock or (i < LastPos))
          and ((i <= 1) or (Pos(S[i - 1], Delimiters) > 0))
          and ((i >= LastPos) or (Pos(S[i + LengthF], Delimiters) > 0));

      if Match then
      begin
        Result := i;
        Break
      end;
    end
    else
    //Search backward
    for i := LastPos downto 1 do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LengthF * 2);

      if fWholeWords then
        Match := Match
          and (fLastBlock or (i > 1))
          and ((i <= 1) or (Pos(S[i - 1], Delimiters) > 0))
          and ((i >= LastPos) or (Pos(S[i + LengthF], Delimiters) > 0));

      if Match then
      begin
        Result := i;
        Break
      end;
    end;
end;


function IMin(N1, N2: Integer): Integer;
begin
  if N1 < N2 then
    Result := N1
  else
    Result := N2;
end;

function IMax(N1, N2: Integer): Integer;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
end;

function WMin(N1, N2: Word): Word;
begin
  if N1 < N2 then
    Result := N1
  else
    Result := N2;
end;

function WMax(N1, N2: Word): Word;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
end;

function I64Min(const N1, N2: Int64): Int64;
begin
  if N1 < N2 then
    Result := N1
  else
    Result := N2;
end;

function I64Max(const N1, N2: Int64): Int64;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
end;


procedure ILimitMin(var N: Integer; Value: Integer);
begin
  if N < Value then
    N := Value;
end;

procedure ILimitMax(var N: Integer; Value: Integer);
begin
  if N > Value then
    N := Value;
end;

{
procedure WLimitMin(var N: Word; Value: Word);
begin
  if N < Value then
    N := Value;
end;

procedure WLimitMax(var N: Word; Value: Word);
begin
  if N > Value then
    N := Value;
end;
}

procedure I64LimitMin(var N: Int64; const Value: Int64);
begin
  if N < Value then
    N := Value;
end;

procedure I64LimitMax(var N: Int64; const Value: Int64);
begin
  if N > Value then
    N := Value;
end;


procedure SReplaceZeros(var S: AnsiString);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] = #0 then
      S[i] := ' ';
end;

procedure SReplaceZerosW(var S: WideString);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] = #0 then
      S[i] := ' ';
end;

procedure SDelLastSpaceW(var S: WideString);
begin
  if (S <> '') and ((S[Length(S)] = ' ') or (S[Length(S)] = #9)) then
    SetLength(S, Length(S) - 1);
end;

procedure SDelLastSpace(var S: AnsiString);
begin
  if (S <> '') and (S[Length(S)] = ' ') then
    SetLength(S, Length(S) - 1);
end;

procedure SDelLastSlashW(var S: WideString);
begin
  if (S <> '') and (S[Length(S)] = '\') then
    SetLength(S, Length(S) - 1);
end;

procedure SDelLastSlash(var S: AnsiString);
begin
  if (S <> '') and (S[Length(S)] = '\') then
    SetLength(S, Length(S) - 1);
end;

procedure SDelLastComma(var S: AnsiString);
begin
  if (S <> '') and (S[Length(S)] = ',') then
    SetLength(S, Length(S) - 1);
end;


function SFillW(ch: WideChar; Count: Integer): WideString;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 1 to Length(Result) do
    Result[i] := ch;
end;

function STabReplacement(const TabOptions: TStringTabOptions): WideString;
var
  ASize: Integer;
  APos: Integer;
begin
  with TabOptions do
  begin
    Assert(TabSize > 0, 'Tab size too small');
    if FontMonospaced then
      ASize := TabSize - (TabPosition - 1) mod TabSize
    else
      ASize := TabSize;
    Result := SFillW(' ', ASize);
    APos := Length(Result) div 2 + 1;
    if NonPrintableShow then
      Result[APos] := NonPrintableChar;
  end;
end;

procedure SReplaceTabsW(var S: WideString; var TabOptions: TStringTabOptions);
var
  N: Integer;
begin
  repeat
    N := Pos(#9, S);
    if N = 0 then Break;
    TabOptions.TabPosition := N;
    SReplaceW(S, #9, STabReplacement(TabOptions));
  until False;
end;


function SetStringW(Buffer: PAnsiChar; BufSize: Integer; SwapBytes: Boolean): WideString;
var
  P: PAnsiChar;
  i, j: Integer;
  ch: AnsiChar;
begin
  Result := '';
  if BufSize < 2 then Exit;

  SetLength(Result, BufSize div 2);
  Move(Buffer^, Result[1], Length(Result) * 2);

  if SwapBytes then
  begin
    P := @Result[1];
    for i := 1 to Length(Result) do
    begin
      j := (i - 1) * 2;
      ch := P[j];
      P[j] := P[j + 1];
      P[j + 1] := ch;
    end;
  end;
end;


function SFileExtensionMatch(const FileName: WideString; const ExtList: AnsiString): Boolean;
var
  Ext: AnsiString;
begin
  Ext := LowerCase(SExtractFileExt(FileName));
  if (Ext <> '') and (Ext[1] = '.') then
    Delete(Ext, 1, 1);
  Result := Pos(',' + Ext + ',', ',' + ExtList + ',') > 0;
end;

function SFileExtensionMatch2(const FileName: WideString; const ExtList1, ExtList2: AnsiString): Boolean;
begin
  Result := SFileExtensionMatch(FileName, ExtList1 + ',' + ExtList2);
end;


procedure SDeleteFromStrA(var S: AnsiString; const SubStr: AnsiString);
var
  N: Integer;
begin
  N := Pos(SubStr, S);
  if N > 0 then
    SetLength(S, N - 1);
end;

procedure SDeleteFromStrW(var S: WideString; const SubStr: WideString);
var
  N: Integer;
begin
  N := Pos(SubStr, S);
  if N > 0 then
    SetLength(S, N - 1);
end;

function SCharCR(ch: WideChar): Boolean;
begin
  Result := (ch = #13) or (ch = #10);
end;

function SLastCharCR(const S: WideString): Boolean;
begin
  Result := (S <> '') and SCharCR(S[Length(S)]);
end;


end.
