
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit StrTools;

interface

{$I STD.INC}

uses
  Classes, SysUtils, TypInfo, Windows, Math;

const
   Alpha = ['A'..'Z', 'a'..'z'];
   Numeric = ['0'..'9'];
   AlphaNumeric = Alpha + Numeric;

type
  TStringArray = array of string;

{ The Extractpath function }

function ExtractPath(const S: string): string;

{ The NextToken function returns the string value of the next whitespace
  delimited character array pointed to by Buffer. The value pointed to by
  Buffer is advanced to the next non-whitespace character }

function NextToken(var Buffer: PChar): string;

{ The SearchToken function does a case insensitive search of Buffer for
  the string S and returns True if a match is found. Buffer is advanced to
  one character after the match }

function SearchToken(var Buffer: PChar; const S: string): Boolean;

{ The Split function }

function Split(const Source: string; Terminator: Char): TStringArray;

{ The FieldCount function }

function FieldCount(const Source: string; Terminator: Char): Integer;

{ The FieldValue function }

function FieldValue(const Source: string; Terminator: Char; Index: Integer): string;

{ The FieldSearch function }

function FieldSearch(const Key, Source: string; Terminator: Char): Boolean;

{ The GetComponentName function returns the name of the Component parameter. If
  the component has no name, the function creates a unique name }

function GetComponentName(Component: TComponent): string;

{ The GetComponentPath function returns a string that represents the ownership
  of the Component parameter }

function GetComponentPath(Component: TComponent): string;

{ The GetCommandArguments function }

function GetCommandArguments: string;

{ The GetComputerName function }

function GetComputerName: string;

{ The GetUserName function }

function GetUserName: string;

{ The MaskConvert function returns a formated string from Source as defined by
  Mask. Any characters in Source that don't match the Mask string are replaced
  with DefaultChar.  Mask can contain the following special characters:

    'C' Filters source string for the first occurance of an Alpha character
    'D' Filters source string for the first occurance of an Numeric character
    '?' Accepts any character

   For example:
     MaskConvert('1235551212', '(DDD) DDD-DDDDD', '0') returns '(123) 555-1212'
     MaskConvert('(123) 555-1212', 'DDDDDDDDDDD', '0') returns '1235551212' }

function MaskConvert(const Source: string; const Mask: string; DefaultChar: Char): string;

{ The Hash function computes the total ASCII value of the string S }

function Hash(const S: string): Integer;

{ The RealizeLength procedure }

procedure RealizeLength(var S: string);

{ The ReverseString function }

function ReverseString(const S: string): string;

{ The StripText function }

function StripText(const S: string): string;

{ The StripCharacters function }

function StripCharacters(const S: string; const Characters: string): string;

{ The StripFonts procedure removes all <FONT> markup from the HTML source code
  contained in the string parameter }

function StripFonts(const S: string): string;

{ The PadString procedure copies Source string into the Dest buffer, aligning it
  left or right and filling any extra bytes with the Pad character }

type
  TPadAlign = (paLeft, paRight);

procedure PadString(const Source: string; Dest: PChar; Size: Integer;
  PadAlign: TPadAlign; Pad: Char = ' ');

{ The TrimSeparator function returns a copy of S from the character after the
  Separator parameter to the end of the string }

function TrimSeparator(const S: string; Separator: Char): string;

{ The InvertText function inverts the characters of the S parameter }

function InvertText(const S: string): string;

{ The BinEncodeText function }

function BinEncodeText(const S: string): string;

{ The BinDecodeText function }

function BinDecodeText(Bytes: Pointer): string;

{ The EncodeText function }

function EncodeText(const S: string): string;

{ The DencodeText function }

function DecodeText(const S: string): string;

{ The TextToCode function }

function TextToCode(const S: string): string;

{ The InvertConstant function inverts a string into a pascal constant }

function InvertConstant(const S: string): string;

{ The IsCommand function }

function IsCommand(const Text, Command: string): Boolean;

function IsFloat(const Text: string; var Value: Extended): Boolean;

function StrSearch(Token: PChar; Buffer: PChar): PChar;

{ File string functions }

function FileReadString(const FileName: string; const Default: string = ''): string;
procedure FileWriteString(const FileName, Value: string;
  CreateFlag: Cardinal = CREATE_NEW);
procedure FileWrite(const FileName, Value: string);
procedure FileWriteLn(const FileName, Value: string);

type
  TFieldPath = record
    Folder: string;
    Name: string;
  end;

function ExtractFieldPath(const S: string): TFieldPath;

{ Registry string functions }

function RegReadString(const Key: string; const Default: string = ''): string;
procedure RegWriteString(const Key, Value: string);
procedure RegRemoveKey(const Key: string);
procedure RegRemoveValue(const Key: string);

type
  TModalResult = Integer;
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation);

  TMsgDlgButtons = (mbAbortRetryIgnore, mbOk, mbOkCancel, mbRetryCancel,
    mbYesNo, mbYesNoCancel);

function MessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TModalResult; overload;

function MessageDialog(const Msg: string; const Caption: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult; overload;

{ The EncodeOctalString function }

function EncodeOctalString(const S: string): string;

{ The DecodeOctalString function }

function DecodeOctalString(const S: string): string;

function StrToChar(const S: string): Char;

function LicenseNumber(const First, Last: string; Middle: Char; Birthday: TDateTime;
	Female: Boolean): string;

implementation

function ExtractPath(const S: string): string;
begin
  Result := IncludeTrailingBackslash(ExtractFilePath(S));
end;

function NextToken(var Buffer: PChar): string;
var
  P: PChar;
begin
  Result := '';
  if Buffer <> nil then
  begin
    while (Buffer^ <> #0) and (Buffer^ < #33) do
      Inc(Buffer);
    P := Buffer;
    while (Buffer^ <> #0) and (Buffer^ > #32) do
      Inc(Buffer);
    SetString(Result, P, Integer(Buffer - P));
  end;
end;

function SearchToken(var Buffer: PChar; const S: string): Boolean;
var
  Token: PChar;
begin
  Result := False;
  if Buffer <> nil then
  begin
    Token := PChar(S);
    while Buffer^ <> #0 do
    begin
      if UpCase(Buffer^) = Token^ then
        Inc(Token)
      else if Token <> PChar(S) then
      begin
        Token := PChar(S);
        if UpCase(Buffer^) = Token^ then
          Inc(Token);
      end;
      Inc(Buffer);
      if Token^ = #0 then
        Break;
    end;
    Result := Token^ = #0;
  end;
end;

function Split(const Source: string; Terminator: Char): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, FieldCount(Source, Terminator));
  for I := Low(Result) to High(Result) do
    Result[I] := FieldValue(Source, Terminator, I);
end;

function FieldCount(const Source: string; Terminator: Char): Integer;
var
  P: PChar;
begin
  Result := 0;
  if Source <> '' then
  begin
    P := PChar(Source);
    repeat
      if P^ = Terminator then
        Inc(Result);
      Inc(P);
    until P^ = #0;
    if (Result > 0) and (P[-1] <> Terminator) then
      Inc(Result);
  end;
  if Result = 0 then Result := 1;
end;

function FieldValue(const Source: string; Terminator: Char; Index: Integer): string;
var
  Start, P: PChar;
  I: Integer;
begin
  Result := '';
  if Source <> '' then
  begin
    Start := PChar(Source);
    P := Start;
    I := 0;
    while P^ > #0 do
    begin
      if P^ = Terminator then
      begin
        if I = Index then
        begin
          SetString(Result, Start, Integer(P - Start));
          Exit;
        end;
        Start := P;
        Inc(Start);
        Inc(I);
      end;
      Inc(P);
    end;
    if I = Index then
      SetString(Result, Start, Integer(P - Start));
  end;
end;

function FieldSearch(const Key, Source: string; Terminator: Char): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FieldCount(Source, Terminator) -1 do
    if Key = FieldValue(Source, Terminator, I) then
    begin
      Result := True;
      Break;
    end;
end;

function GetComponentName(Component: TComponent): string;
var
  S: string;
  I: Integer;
begin
  Result := Component.Name;
  if Result = '' then
  begin
    S := Component.ClassName;
    I := 1;
    if Component.Owner <> nil then
      while Component.Owner.FindComponent(PChar(@S[2]) + IntToStr(I)) <> nil do
        Inc(I);
    Result := PChar(@S[2]) + IntToStr(I);
  end;
end;

function GetComponentPath(Component: TComponent): string;
var
  S: string;
begin
  if Component <> nil then
  begin
    S := GetComponentPath(Component.Owner);
    if S <> '' then
      Result := S + DotSep + GetComponentName(Component)
    else
      Result := S + GetComponentName(Component);
  end
  else
    Result := '';
end;

function GetCommandArguments: string;
var
  S: string;
  P: PChar;
begin
  S := GetCommandLine;
  P := PChar(S);
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  while P[0] > ' ' do
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
        Inc(P);
      if P[0] <> #0 then
        Inc(P);
    end
    else
      Inc(P);
  while P[0] = ' ' do
    Inc(P);
  Result := P;
end;

function GetComputerName: string;
var
  Size: DWORD;
begin
  Size := MAX_PATH;
  SetLength(Result, Size);
  if Windows.GetComputerName(PChar(Result), Size) then
    RealizeLength(Result)
  else
    RaiseLastWin32Error;
end;

function GetUserName: string;
var
  Size: DWORD;
begin
  Size := MAX_PATH;
  SetLength(Result, Size);
  if Windows.GetUserName(PChar(Result), Size) then
    RealizeLength(Result)
  else
    RaiseLastWin32Error;
end;

function Hash(const S: string): Integer;
asm
        TEST    EAX, EAX
        JNZ     @Continue
        XOR     EAX, EAX
        RET
@Continue:
        MOV     EDX, ESI
        MOV     ESI, EAX
        XOR     EAX, EAX
        XOR     ECX, ECX
        CLD
@Loop:
        LODSB
        CMP     AL, 00H
        JE      @Done
        CMP     AL, 'a'
        JB      @Add
        CMP     AL, 'z'
        JA      @Add
        SUB     AL, 20H
@Add:
        ADD     ECX, EAX
        JMP     @Loop
@Done:
        MOV     EAX, ECX
        MOV     ESI, EDX
end;

function MaskConvert(const Source: string; const Mask: string; DefaultChar: Char): string;
var
  P: PChar;
  I: Integer;

  procedure DefaultAction;
  begin
    if UpCase(Mask[I]) in ['C', 'D', '?'] then
      Result := Result + DefaultChar
    else
      Result := Result + Mask[I];
  end;

begin
  P := PChar(Source);
  Result := '';
  for I := 1 to Length(Mask) do
  if P^ <> #0 then
  begin
    case UpCase(Mask[I]) of
      'C':
      begin
        while (P^ <> #0) and (not (P^ in Alpha)) do
          Inc(P);
        if P^ <> #0 then
          Result := Result + P^
        else
        begin
          DefaultAction;
          Continue;
        end;
      end;
      'D':
      begin
        while (P^ <> #0) and (not (P^ in Numeric)) do
          Inc(P);
        if P^ <> #0 then
          Result := Result + P^
        else
        begin
          DefaultAction;
          Continue;
        end;
      end;
      '?':
        Result := Result + P^
      else
      begin
        Result := Result + Mask[I];
        Continue;
      end;
    end;
    Inc(P);
  end
  else
    DefaultAction;
end;

procedure RealizeLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

function ReverseString(const S: string): string;
var
  Source, Dest: PChar;
  I: Integer;
begin
  Result := '';
  if S <> '' then
  begin
    I := Length(S);
    SetLength(Result, I);
    Source := PChar(S);
    Dest := PChar(Result);
    Inc(Dest, I - 1);
    while Source^ > #0 do
    begin
      Dest^ := Source^;
      Inc(Source);
      Dec(Dest);
    end;
  end;
end;

function StripText(const S: string): string;
var
  Source, Dest: PChar;
  LastChar: Char;
begin
  Result := '';
  if S = '' then Exit;
  Source := PChar(S);
  SetLength(Result, Length(S));
  Dest := PChar(Result);
  FillChar(Dest^, Length(Result), #0);
  LastChar := #0;
  while Source^ > #0 do
  begin
    if (Source^ > #31) and (not ((Source^ = #32) and (LastChar = #32))) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    LastChar := Source^;
    Inc(Source);
  end;
  SetLength(Result, StrLen(PChar(Result)));
end;

function StripCharacters(const S: string; const Characters: string): string;

  function FindChar(C: Char): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(Characters) do
      if C = Characters[I] then
      begin
        Result := True;
        Break;
      end;
  end;

var
  P: PChar;
  I: Integer;
begin
  Result := S;
  P := PChar(Result);
  for I := 1 to Length(Result) do
    if not FindChar(Result[I]) then
    begin
      P^ := Result[I];
      Inc(P);
    end;
  if P <> nil then
    P^ := #0;
  SetLength(Result, StrLen(PChar(Result)));
end;

function StripFonts(const S: string): string;
var
  Source, Dest: PChar;

  procedure AdvanceSource;
  begin
    while not (Source^ in [#0, '>']) do Inc(Source);
    if Source^ = #0 then Dec(Source);
  end;

begin
  Result := '';
  if S <> '' then
  begin
    SetLength(Result, Length(S));
    Source := PChar(S);
    Dest := PChar(Result);
    while Source^ > #0 do
    begin
      if (Source[0] = '<') and (Upcase(Source[1]) = 'F') and
        (Upcase(Source[2]) = 'O') and (Upcase(Source[3]) = 'N') and
        (Upcase(Source[4]) = 'T') then
        AdvanceSource
      else if (Source[0] = '<') and (Source[1] = '/') and
        (Upcase(Source[2]) = 'F') and (Upcase(Source[3]) = 'O') and
        (Upcase(Source[4]) = 'N') and (Upcase(Source[5]) = 'T') then
        AdvanceSource
      else
      begin
        Dest^ := Source^;
        Inc(Dest);
      end;
      Inc(Source);
    end;
    SetLength(Result, Dest - PChar(Result));
  end;
end;

procedure PadString(const Source: string; Dest: PChar; Size: Integer;
  PadAlign: TPadAlign; Pad: Char = ' ');
begin
  FillMemory(Dest, Size, Byte(Pad));
  if PadAlign = paRight then
    Inc(Dest, Size - MinIntValue([Length(Source), Size]));
  CopyMemory(Dest, Pointer(Source), MinIntValue([Length(Source), Size]));
end;

function TrimSeparator(const S: string; Separator: Char): string;
var
  P: PChar;
begin
  Result := '';
  if S <> '' then
  begin
    P := PChar(S);
    while (P^ <> #0) and (P^ <> Separator) do
      Inc(P);
    if P^ <> #0 then
      Inc(P);
    Result := P;
  end;
end;

function InvertText(const S: string): string;
var
  I: Integer;
begin
  SetLength(Result, Length(S));
  for I := 1 to Length(Result) do
    Result[I] := Char(not Byte(S[I]));
end;

function BinEncodeText(const S: string): string;
var
  Buffer: string;
  I: Integer;
begin
  if S <> '' then
  begin
    Result := '{ Encoded: ' + S + ' }'#13#10;
    Result :=  Result + 'SEncoded: array[0..' + IntToStr(Length(S) * 2) + '] of Byte = (';
    Result := Result + '$' + IntToHex(Length(S) * 2, 2) + ',';
    Buffer := EncodeText(S);
    for I := 1 to Length(Buffer) do
      Result := Result + '$' + Buffer[I] + ',';
    SetLength(Result, Length(Result) - 1);
    Result :=  Result + ');';
  end
  else
    Result := '';
end;

function BinDecodeText(Bytes: Pointer): string;
const
  Chars = '0123456789ABCDEF';
var
  B: PByte;
  I: Integer;
begin
  B := Bytes;
  I := Byte(B^);
  Inc(B);
  Result := '';
  while I > 0 do
  begin
    Result := Result + Chars[B^ + 1];
    Inc(B);
    Dec(I);
  end;
  Result := DecodeText(Result);
end;

function EncodeText(const S: string): string;
var
  Buffer: string;
  I: Integer;
begin
  if S <> '' then
  begin
    I := Length(S) * 2;
    Buffer := InvertText(ReverseString(S));
    SetLength(Result, I);
    BinToHex(PChar(Buffer), PChar(Result), I div 2);
  end
  else
    Result := '';
end;

function DecodeText(const S: string): string;
var
  I: Integer;
begin
  if S <> '' then
  begin
    I := Length(S) div 2;
    SetLength(Result, I);
    HexToBin(PChar(S), PChar(Result), I);
    Result := InvertText(ReverseString(Result));
  end
  else
    Result := '';
end;

function TextToCode(const S: string): string;
const
  Characters: array [0..31] of string = (
    '<NUL>', '<SOH>', '<STX>', '<ETX>'#13#10, '<EOT>', '<ENQ>', '<ACK>', '<BEL>',
    '<BS>', '<HT>', '<LF>', '<VT>', '<FF>', '<CR>', '<00101110>', '<SI>', '<DLE>',
    '<DC1>', '<DC2>', '<DC3>', '<DC4>', '<NAK>', '<SYN>', '<ETB>', '<CAN>', '<EM>',
    '<SUB>', '<ESC>', '<FS>', '<GS>', '<RS>', '<US>');
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if (Ord(S[I]) < 32) then
      Result := Result + Characters[Ord(S[I])]
    else
      Result := Result + S[I];
  Result := StringReplace(Result, '<CR><LF>', #13#10, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, #13#10#13#10, #13#10, [rfReplaceAll, rfIgnoreCase]);
end;

function InvertConstant(const S: string): string;
var
  Text: string;
  I: Integer;
begin
  Result := '';
  Text := InvertText(S);
  if Text <> '' then
  begin
    Result := 'const'#13#10'  S = ';
    for I := 1 to Length(Text) do
      Result := Result + '#' + IntToStr(Ord(Text[I]));
    Result := Result + ';';
  end;
end;

function IsCommand(const Text, Command: string): Boolean;
begin
  Result := UpperCase(Trim(Text)) = UpperCase(Command);
end;

function IsFloat(const Text: string; var Value: Extended): Boolean;
var
  S: string;
begin
  Result := False;
  S := Trim(Text);
  if S <> '' then
  begin
    if S[1] = '$' then
      S := StrPas(PChar(@S[2]));
    try
      Value := StrToFloat(S);
      Result := True;
    except
    end;
  end;
end;

function StrSearch(Token: PChar; Buffer: PChar): PChar;
asm
        PUSH    EBX
        TEST    EAX,EAX
        JZ      @6
        TEST    EDX,EDX
        JZ      @6
        XOR     ECX,ECX
        JMP     @3
@1:     INC     ECX
@2:     INC     EDX
@3:     MOV     BL,[EAX+ECX]
        OR      BL,BL
        JZ      @7
@4:     MOV     BH,[EDX]
        OR      BH,BH
        JZ      @6
        CMP     BH,'a'
        JB      @5
        CMP     BH,'z'
        JA      @5
        SUB     BH,'a'-'A'
@5:     CMP     BL,BH
        JE      @1
        OR      ECX,ECX
        JZ      @2
        SUB     EDX,ECX
        INC     EDX
        XOR     ECX,ECX
        JMP     @3
@6:     XOR     EDX,EDX
@7:     MOV     EAX,EDX
        POP     EBX;
end;

function StrScan(Token: PChar; Buffer: PChar): PChar;
asm
        PUSH    EBX
        XOR     ECX,ECX
        TEST    EAX,EAX
        JZ      @6
        TEST    EDX,EDX
        JZ      @6
        JMP     @3
@1:     INC     ECX
@2:     INC     EDX
@3:     MOV     BL,[EAX+ECX]
        OR      BL,BL
        JZ      @7
@4:     MOV     BH,[EDX]
        OR      BH,BH
        JZ      @6
@5:     CMP     BL,BH
        JE      @1
        OR      ECX,ECX
        JZ      @2
        SUB     EDX,ECX
        INC     EDX
        XOR     ECX,ECX
        JMP     @3
@6:     XOR     EDX,EDX
@7:     MOV     EAX,EDX
        POP     EBX;
end;

{ function StrSearch(Str, Buffer: PChar): PChar;
asm
     PUSH       ESI
     PUSH       EDI
     TEST       EAX,EAX
     JZ         @Exit
     TEST       EDX,EDX
     JZ         @Exit
     MOV        ESI,EAX
     MOV        EDI,EDX
     XOR        EAX,EAX
     CLD
     MOV        ECX,0FFFFFFFFH
     REPNE      SCASB
@Exit:
     POP        EDI
     POP        ESI
end;

function StrReverse(Str: PChar): PChar;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        TEST    EAX,EAX
        JZ      @2
        MOV     ECX,0FFFFFFFFH
        XOR     EAX,EAX
        REPNE   SCASB
        SUB     ECX,0FFFFFFFEH
@1:     TEST    ECX,ECX
        JZ      @2
        SUB     EDI,ECX
        MOV     AL,[EDI+ECX]
        MOV     AH,[EDI]
        MOV     [EDI-ECX],AH
        MOV     [EDI],AL
        DEC     ECX
        INC     EDI
        JMP     @1
@2:
        MOV     EAX,EAX
        MOV     EDI,EDX
end; }

function FileReadString(const FileName: string; const Default: string = ''): string;
var
  F: THandle;
  Bytes: Cardinal;
begin
  Result := '';
  F := CreateFile(PChar(FileName), GENERIC_READ, 0, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    Bytes := GetFileSize(F, nil);
    if Bytes > 0 then
    begin
      SetLength(Result, Bytes);
      ReadFile(F, PChar(Result)^, Bytes, Bytes, nil);
    end;
  finally
    CloseHandle(F);
  end;
end;

procedure FileWriteString(const FileName, Value: string;
  CreateFlag: Cardinal = CREATE_NEW);
var
  F: THandle;
  Bytes: Cardinal;
begin
  if Value = '' then Exit;
  F := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, CreateFlag,
    FILE_ATTRIBUTE_NORMAL, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    SetFilePointer(F, 0, nil, FILE_END);
    WriteFile(F, PChar(Value)^, Length(Value), Bytes, nil);
  finally
    CloseHandle(F);
  end;
end;

procedure FileWrite(const FileName, Value: string);
var
  F: THandle;
  Bytes: Cardinal;
begin
  F := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    SetFilePointer(F, 0, nil, FILE_END);
    WriteFile(F, PChar(Value)^, Length(Value), Bytes, nil);
  finally
    CloseHandle(F);
  end;
end;

procedure FileWriteLn(const FileName, Value: string);
begin
	FileWrite(FileName, Value + #13#10);
end;

function ExtractFieldPath(const S: string): TFieldPath;
var
  Count: Integer;
  I: Integer;
begin
  Result.Folder := '';
  Result.Name := '';
  Count := FieldCount(S, '\');
  if Count = 0 then Exit;
  for I := 0 to Count - 2 do
    Result.Folder := Result.Folder + FieldValue(S, '\', I) + '\';
  if Result.Folder <> '' then
    SetLength(Result.Folder, Length(Result.Folder) - 1);
  if Count > 1 then
    Result.Name :=  FieldValue(S, '\', Count - 1);
end;

function RegLocateKey(const Key: string; var RegKey: HKEY; var SubKey: string;
  CanCreate: Boolean = False): Boolean;
var
  Disposition: Cardinal;
  S: string;
  I: Integer;
begin
  RegKey := 0;
  SubKey := '';
  Result := False;
  S := UpperCase(FieldValue(Key, '\', 0));
  if S = 'HKEY_CLASSES_ROOT' then
    RegKey := HKEY_CLASSES_ROOT
  else if S = 'HKEY_CURRENT_USER' then    RegKey := HKEY_CURRENT_USER  else if S = 'HKEY_LOCAL_MACHINE' then    RegKey := HKEY_LOCAL_MACHINE  else if S = 'HKEY_USERS' then
    RegKey := HKEY_USERS
  else
    Exit;
  S := '';
  for I := 1 to FieldCount(Key, '\') - 2 do
    S := S + FieldValue(Key, '\', I) + '\';
  if S = '' then
    Exit;
  SetLength(S, Length(S) - 1);
  if ((CanCreate) and (RegCreateKeyEx(RegKey, PChar(S), 0, nil, REG_OPTION_NON_VOLATILE,
    KEY_WRITE, nil, RegKey, @Disposition) = ERROR_SUCCESS)) or
    (RegOpenKeyEx(RegKey, PChar(S), 0, KEY_READ, RegKey) = ERROR_SUCCESS) then
  begin
    SubKey := FieldValue(Key, '\', FieldCount(Key, '\') - 1);
    if UpperCase(SubKey) = '(DEFAULT)' then SubKey := '';
    Result := True;
  end;
end;

function RegReadString(const Key: string; const Default: string = ''): string;
var
  RegKey: HKEY;
  SubKey: string;
  Size: Integer;
begin
  Result := Default;
  if RegLocateKey(Key, RegKey, SubKey, True) then
  try
    case RegQueryValueEx(RegKey, PChar(SubKey), nil, nil, nil, @Size) of
      ERROR_SUCCESS, ERROR_MORE_DATA:
        begin
          SetLength(Result, Size);
          RegQueryValueEx(RegKey, PChar(SubKey), nil, nil, Pointer(Result),
            @Size);
          SetLength(Result, StrLen(PChar(Result)));
        end;
   end;
  finally
    RegCloseKey(RegKey);
  end;
end;

procedure RegWriteString(const Key, Value: string);
const
  EmptyKey: Char = #0;
var
  RegKey: HKEY;
  SubKey: string;
begin
  if RegLocateKey(Key, RegKey, SubKey) then
  try
    if Value <> '' then
      RegSetValueEx(RegKey, PChar(SubKey), 0, REG_SZ, Pointer(Value),
        Length(Value) + 1)
    else
      RegSetValueEx(RegKey, PChar(SubKey), 0, REG_SZ, @EmptyKey, 1);
  finally
    RegCloseKey(RegKey);
  end;
end;

procedure RegRemoveKey(const Key: string);
var
  RegKey: HKEY;
  SubKey: string;
begin
  if RegLocateKey(Key, RegKey, SubKey) then
  try
    RegDeleteKey(RegKey, PChar(SubKey));
  finally
    RegCloseKey(RegKey);
  end;
end;

procedure RegRemoveValue(const Key: string);
var
  RegKey: HKEY;
  SubKey: string;
begin
  if RegLocateKey(Key, RegKey, SubKey) then
  try
    RegDeleteValue(RegKey, PChar(SubKey));
  finally
    RegCloseKey(RegKey);
  end;
end;

procedure MessageNotify(const Msg: string);
begin
  MessageDialog(Msg, mtInformation, mbOk);
end;

function MessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TModalResult;
const
  Captions: array[TMsgDlgType] of string = (
    'Warning', 'Error', 'Information', 'Confirmation');
begin
  Result := MessageDialog(Msg, Captions[DlgType], DlgType, Buttons);
end;

function MessageDialog(const Msg: string; const Caption: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult;
const
  Flags: array[TMsgDlgButtons] of LongWord = (
    MB_ABORTRETRYIGNORE, MB_OK, MB_OKCANCEL, MB_RETRYCANCEL,  MB_YESNO,
    MB_YESNOCANCEL);
  Icons: array[TMsgDlgType] of LongWord = (
    MB_ICONWARNING, MB_ICONERROR, MB_ICONINFORMATION, MB_ICONQUESTION);
begin
  Result := MessageBox(0, PChar(Msg), PChar(Caption), Flags[Buttons] or
    Icons[DlgType] or MB_TASKMODAL or MB_SETFOREGROUND or MB_TOPMOST);
end;

procedure ShowException(Msg: string);
begin
  MessageDialog(Msg, mtError, mbOk);
end;

function EncodeOctalString(const S: string): string;
var
  Source: PChar;
  Dest: PChar;
  Ch: Char;
  I: Integer;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Result, Length(S));
  Source := PChar(S);
  Dest := PChar(Result);
  repeat
    if Source^ = '\' then
    begin
      Ch := #0;
      for I := 0 to 2 do
      begin
        Inc(Source);
        if Source^ in ['0'..'7'] then
          Ch := Char(Ord(Ch) shl 3 or Ord(Source^) - Ord('0'))
        else
          Break;
        if I = 2 then
          Inc(Source);
      end;
      Dest^ := Ch;
      Dec(Source);
    end
    else
      Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  until Source^ = #0;
  SetLength(Result, LongWord(Dest - PChar(Result)));
end;

function DecodeOctalString(const S: string): string;
const
  OctalChars: PChar = '01234567';
var
  Source: PChar;
  Dest: PChar;
  I: Integer;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Result, Length(S));
  Source := PChar(S);
  Dest := PChar(Result);
  for I := 0 to Length(S) - 1 do
  begin
    if Source^ in [#0..#31, #128..#255] then
    begin
      SetLength(Result, Length(Result) + 3);
      Dest^ := '\';
      Inc(Dest);
      Dest^ := OctalChars[Ord(Source^) shr 6 and $07];
      Inc(Dest);
      Dest^ := OctalChars[Ord(Source^) shr 3 and $07];
      Inc(Dest);
      Dest^ := OctalChars[Ord(Source^) and $07];
    end
    else
      Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

type
  LStrRec = record
    AllocSize : Longint;
    RefCount  : Longint;
    Length    : Longint;
  end;

const
  StrOffset = SizeOf(LStrRec);

function SoundEx(const S: string) : string;
const
  SoundexTable : array[0..255] of Char =
    (#0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0,
     { A   B    C    D    E   F    G    H   I   J    K    L    M  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
     { N    O   P    Q    R    S    T    U   V    W   X    Y   X  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0,
     { a   b    c    d    e   f    g    h   i   j    k    l    m  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
     { n    o   p    q    r    s    t    u   v    w   x    y   x  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0);
begin
  if S = '' then Exit;
  SetLength(Result, 4);
  asm
    push  edi
    mov   edi, [Result]            { EDI => output string. }
    mov   edi, [edi]
    push  ebx
    push  esi

    mov   esi, S                   { ESI => input string. }
    mov   dword ptr [edi], '0000'  { Initialize output string to '0000'. }
    xor   eax, eax
    mov   [edi+4], al              { Set null at end of string. }

    mov   ecx, [esi-StrOffset].LStrRec.Length
    or    ecx, ecx                 { Exit if null string. }
    jz    @@Done

    mov   al, [esi]                { Get first character of input string. }
    inc   esi

    push  ecx                      { Save ECX across call to CharUpper. }
    push  eax                      { Push Char onto stack for CharUpper. }
    call  CharUpper                { Uppercase AL. }
    pop   ecx                      { Restore saved register. }

    mov   [edi], al                { Store first output character. }
    inc   edi

    dec   ecx                      { One input character used. }
    jz    @@Done                   { Was input string one char long?. }

    mov   bh, 03h                  { Output max 3 chars beyond first. }
    mov   edx, offset SoundexTable { EDX => Soundex table. }
    xor   eax, eax                 { Prepare for address calc. }
    xor   bl, bl                   { BL will be used to store 'previous char'. }

  @@Next:
    mov   al, [esi]                { Get next char in AL. }
    inc   esi
    mov   al, [edx+eax]            { Get soundex code into AL. }
    or    al, al                   { Is AL zero? }
    jz    @@NoStore                { If yes, skip this char. }
    cmp   bl, al                   { Is it the same as the previous stored char? }
    je    @@NoStore                { If yes, skip this char. }
    mov   [edi], al                { Store char to Dest. }
    inc   edi
    dec   bh                       { Decrement output counter. }
    jz    @@Done                   { If zero, we're done. }
    mov   bl, al                   { New previous character. }

  @@NoStore:
    dec   ecx                      { Decrement input counter. }
    jnz   @@Next

  @@Done:
    pop   esi
    pop   ebx
    pop   edi
  end;
end;

function LookupFirst(S: string): Integer;

begin
	S := UpperCase(S);
	if S = '' then Result := 0
	else if S = 'ALBERT' then Result := 20
	else if S = 'ALICE' then Result := 20
	else if S = 'ANN' then Result := 40
	else if S = 'ANNA' then Result := 40
	else if S = 'ANNE' then Result := 40
	else if S = 'ANNIE' then Result := 40
	else if S = 'ARTHUR' then Result := 40
	else if S = 'BERNARD' then Result := 80
	else if S = 'BETTE' then Result := 80
	else if S = 'BETTIE' then Result := 80
	else if S = 'BETTY' then Result := 80
	else if S = 'CARL' then Result := 120
	else if S = 'CATHERINE' then Result := 120
	else if S = 'CHARLES' then Result := 140
	else if S = 'DORTHY' then Result := 180
	else if S = 'EDWARD' then Result := 220
	else if S = 'ELIZABETH' then Result := 220
	else if S = 'FLORENCE' then Result := 260
	else if S = 'DONALD' then Result := 180
	else if S = 'CLARA' then Result := 140
	else if S = 'FRANK' then Result := 260
	else if S = 'GEORGE' then Result := 300
	else if S = 'GRACE' then Result := 300
	else if S = 'HAROLD' then Result := 340
	else if S = 'HARRIET' then Result := 340
	else if S = 'HARRY' then Result := 360
	else if S = 'HAZEL' then Result := 360
	else if S = 'HELEN' then Result := 380
	else if S = 'HENRY' then Result := 380
	else if S = 'JAMES' then Result := 440
	else if S = 'JANE' then Result := 440
	else if S = 'JAYNE' then Result := 440
	else if S = 'JEAN' then Result := 460
	else if S = 'JOAN' then Result := 480
	else if S = 'JOHN' then Result := 460
	else if S = 'JOSEPH' then Result := 480
	else if S = 'MARGARET' then Result := 560
	else if S = 'MARTIN' then Result := 560
	else if S = 'MARVIN' then Result := 580
	else if S = 'MARY' then Result := 580
	else if S = 'MELVIN' then Result := 600
	else if S = 'MILDRED' then Result := 600
	else if S = 'PATRICIA' then Result := 680
	else if S = 'PAUL' then Result := 680
	else if S = 'RICHARD' then Result := 740
	else if S = 'ROBERT' then Result := 760
	else if S = 'RUBY' then Result := 740
	else if S = 'RUTH' then Result := 760
	else if S = 'THELMA' then Result := 820
	else if S = 'THOMAS' then Result := 820
	else if S = 'WALTER' then Result := 900
	else if S = 'WANDA' then Result := 900
	else if S = 'WILLIAM' then Result := 920
	else if S = 'WILMA' then Result := 920
	else case S[1] of
    'A': Result := 0;
    'B': Result := 60;
    'C': Result := 100;
    'D': Result := 160;
    'E': Result := 200;
    'F': Result := 240;
    'G': Result := 280;
    'H': Result := 320;
    'I': Result := 400;
    'J': Result := 420;
    'K': Result := 500;
    'L': Result := 520;
    'M': Result := 540;
    'N': Result := 620;
    'O': Result := 640;
    'P': Result := 660;
    'Q': Result := 700;
    'R': Result := 720;
    'S': Result := 780;
    'T': Result := 800;
    'U': Result := 840;
    'V': Result := 860;
    'W': Result := 880;
    'X': Result := 940;
    'Y': Result := 960;
    'Z': Result := 980;
  else
    Result := 0;
  end;
end;

function LookupMiddle(C: Char): Integer;
begin
	case UpCase(C) of
		'A': Result := 1;
		'B': Result := 2;
		'C': Result := 3;
		'D': Result := 4;
		'E': Result := 5;
		'F': Result := 6;
		'G': Result := 7;
		'H': Result := 8;
		'I': Result := 9;
		'J': Result := 10;
		'K': Result := 11;
		'L': Result := 12;
		'M': Result := 13;
		'N': Result := 14;
		'O': Result := 14;
		'P': Result := 15;
		'Q': Result := 15;
		'R': Result := 16;
		'S': Result := 17;
		'T': Result := 18;
		'U': Result := 18;
		'V': Result := 18;
		'W': Result := 19;
		'X': Result := 19;
		'Y': Result := 19;
		'Z': Result := 19;
	else
  	Result := 0;
	end;
end;

function LookupDate(Birthday: TDateTime; Female: Boolean): string;
var
	Year, Day, Month: Word;
  I: Integer;
begin
	DecodeDate(Birthday, Year, Month, Day);
  if Female then
  	I := 500
  else
  	I := 0;
  Result := Format('%.2d-%.3d', [Year mod 100, (Month - 1) * 40 + Day + I]);
end;

function StrToChar(const S: string): Char;
begin
	if S <> '' then
  	Result := S[1]
  else
  	Result := #0;
end;

function LicenseNumber(const First, Last: string; Middle: Char; Birthday: TDateTime;
	Female: Boolean): string;
begin
	Result := Format('%s-%.3d-%s-?', [SoundEx(Last), LookupFirst(First) +
  	LookupMiddle(Middle), LookupDate(Birthday, Female)]);
end;

end.
