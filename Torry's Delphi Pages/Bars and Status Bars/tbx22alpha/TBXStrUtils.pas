unit TBXStrUtils;

// TBX Package
// Copyright 2001-2005 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id:$

interface

uses
  Types, Windows, Classes, SysUtils, Dialogs;

const
  WSF_AUTO    = 0;
  WSF_ANSI    = 1;
  WSF_UTF8    = 2;
  WSF_UTF16LE = 3;
  WSF_UTF16BE = 4;
  WSF_UTF32LE = 5;
  WSF_UTF32BE = 6;

  WDF_ANSI    = 1;
  WDF_UTF8    = 2;
  WDF_UTF16LE = 3;
  WDF_UTF16BE = 4;
  WDF_UTF32LE = 5;
  WDF_UTF32BE = 6;
  WDF_MASK    = $F;

  WDF_SKIPBOM = $100;

procedure SwapUTF16(Src, Dst: PWideChar; Count: Integer);
procedure SwapUTF32(Src, Dst: PUCS4Char; Count: Integer);

function  LoadWideString(Stream: TStream; out Str: WideString; SrcFormat: Integer = WSF_AUTO): Integer; overload;
function  LoadWideString(const FileName: WideString; out Str: WideString; SrcFormat: Integer = WSF_AUTO): Integer; overload;
procedure SaveWideString(Stream: TStream; const Str: WideString; DstFormat: Integer = WSF_UTF8); overload;
procedure SaveWideString(const FileName: WideString; const Str: WideString; DstFormat: Integer = WSF_UTF8); overload;
function  StringToWideStringEx(const S: AnsiString; CodePage: Cardinal): WideString;
function  WideStringToStringEx(const S: WideString; CodePage: Cardinal): AnsiString;
function  StringOfCharW(Ch: WideChar; Count: Integer): WideString;

type
  TStringItemW = record
    FString: WideString;
    FObject: TObject;
  end;

  TStringItemWDynArray = array of TStringItemW;
  TStringListW = class;

  TStringListW = class(TPersistent)
  private
    FCapacity: Integer;
    FCaseSensitive: Boolean;
    FCount: Integer;
    FItems: TStringItemWDynArray;
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    function  GetObject(Index: Integer): TObject;
    function  GetStr(Index: Integer): WideString;
    function  GetTextStr: WideString;
    procedure SetObject(Index: Integer; Value: TObject);
    procedure SetStr(Index: Integer; const Value: WideString);
    procedure SetTextStr(const Value: WideString);
  protected
    procedure Changed;
    function  CompareStrings(const S1, S2: WideString): Integer; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: WideString; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Modified;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
  public
    function  Add(const S: WideString; AObject: TObject = nil): Integer;
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure AddStrings(StringList: TStringListW); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EndUpdate;
    function  Equals(StringList: TStringListW): Boolean;
    function  IndexOf(const S: WideString): Integer;
    procedure Insert(Index: Integer; const S: WideString; AObject: TObject = nil);
    function  LoadFromFile(const FileName: WideString; SrcFormat: Integer = WSF_AUTO): Integer;
    function  LoadFromStream(Stream: TStream; SrcFormat: Integer = WSF_AUTO): Integer;
    procedure SaveToFile(const FileName: WideString; DstFormat: Integer = WSF_AUTO);
    procedure SaveToStream(Stream: TStream; DstFormat: Integer = WSF_AUTO);
    procedure Sort;
    property Capacity: Integer read FCapacity;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property Count: Integer read FCount;
    property Objects[Index: Integer]: TObject read GetObject write SetObject;
    property Strings[Index: Integer]: WideString read GetStr write SetStr; default;
    property Text: WideString read GetTextStr write SetTextStr;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TItemExchangeProc = procedure(AppData: Pointer; Index1, Index2: Integer);
  TItemCompareProc = function(AppData: Pointer; Index1, Index2: Integer): Integer;

procedure QuickSort(AppData: Pointer; First, Count: Integer;
  Exchange: TItemExchangeProc; Compare: TItemCompareProc);

implementation

uses
  RTLConsts, TB2Common;

const
{ from unicode BOM specification:
  Bytes          Encoding Form
  00 00 FE FF    UTF-32, big-endian
  FF FE 00 00    UTF-32, little-endian
  FE FF          UTF-16, big-endian
  FF FE          UTF-16, little-endian
  EF BB BF       UTF-8   }

  BOM_UTF32LE    = UCS4Char($0000FEFF);
  BOM_UTF32BE    = UCS4Char($FFFE0000);
  BOM_UTF16LE    = WideChar($FEFF);
  BOM_UTF16BE    = WideChar($FFFE);
  BOM_UTF8       = UCS4Char($BFBBEF);

type
  PWideCharArray = ^TWideCharArray;
  TWideCharArray = array [Byte] of WideChar;
  PUCS4CharArray = ^TUCS4CharArray;
  TUCS4CharArray = array [Byte] of UCS4Char;

procedure SwapUTF16(Src, Dst: PWideChar; Count: Integer);
asm
    test ecx,ecx
    jle @2
    push esi
    push edi
    mov esi,eax
    mov edi,edx
@1: lodsw
    xchg al,ah
    stosw
    dec ecx
    jnz @1
    pop edi
    pop esi
@2: ret
end;

procedure SwapUTF32(Src, Dst: PUCS4Char; Count: Integer);
asm
    test ecx,ecx
    jle @2
    push esi
    push edi
@1: lodsd
    bswap eax
    stosd
    dec ecx
    jnz @1
    pop edi
    pop esi
@2: ret
end;

function LoadWideString(Stream: TStream; out Str: WideString; SrcFormat: Integer): Integer;
var
  BOM: UCS4Char;
  BytesRead: Integer;
  CurrentSize, BufferSize: Cardinal;
  ANSI: AnsiString;
  UTF32: UCS4String;
  UTF16: WideString;
  UTF8: UTF8String;
begin
  if (SrcFormat < WSF_AUTO) or (SrcFormat > WSF_ANSI) then
    raise Exception.Create('invalid source format');

  SetLength(Str, 0);
  SetLength(ANSI, 0);
  SetLength(UTF32, 0);
  SetLength(UTF16, 0);
  SetLength(UTF8, 0);

  if SrcFormat = WSF_AUTO then
  begin
    { determine the format of the stream from the thirst two or three bytes }
    BOM := UCS4Char(0);
    BytesRead := Stream.Read(BOM, 4);

    if BytesRead = 4 then
    begin
      if BOM = BOM_UTF32LE then Result := WSF_UTF32LE
      else if BOM = BOM_UTF32BE then Result := WSF_UTF32BE
      else if WideChar(BOM and $0000FFFF) = BOM_UTF16LE then
      begin
        Result := WSF_UTF16LE;
        UTF16 := WideChar(BOM shr 16);
      end
      else if WideChar(BOM and $0000FFFF) = BOM_UTF16BE then
      begin
        Result := WSF_UTF16BE;
        UTF16 := WideChar(BOM shr 16);
      end
      else if (BOM and $00FFFFFF) = BOM_UTF8 then
      begin
        Result := WSF_UTF8;
        UTF8 := Char(BOM shr 24);
      end
      else
      begin
        Result := WSF_ANSI;
        SetLength(ANSI, 4);
        ANSI[1] := Char(BOM and $FF);
        ANSI[2] := Char(BOM shr 8 and $FF);
        ANSI[3] := Char(BOM shr 16 and $FF);
        ANSI[4] := Char(BOM shr 24);
      end;
    end
    else if BytesRead = 3 then
    begin
      if (BOM and $FFFFFF) = BOM_UTF8 then Result := WSF_UTF8
      else if WideChar(BOM and $00FFFF) = BOM_UTF16LE then Result := WSF_UTF16LE
      else if WideChar(BOM and $00FFFF) = BOM_UTF16BE then Result := WSF_UTF16BE
      else
      begin
        Result := WSF_ANSI;
        SetLength(ANSI, 3);
        ANSI[1] := Char(BOM and $FF);
        ANSI[2] := Char(BOM shr 8 and $FF);
        ANSI[3] := Char(BOM shr 16 and $FF);
      end;
    end
    else if BytesRead = 2 then
    begin
      if WideChar(BOM) = BOM_UTF16LE then Result := WSF_UTF16LE
      else if WideChar(BOM) = BOM_UTF16BE then Result := WSF_UTF16BE
      else
      begin
        Result := WSF_ANSI;
        SetLength(ANSI, 2);
        ANSI[1] := Char(BOM and $FF);
        ANSI[2] := Char(BOM shr 8 and $FF);
      end;
    end
    else
    begin
      Result := WSF_ANSI;
      ANSI := Char(BOM);
    end;
  end
  else Result := SrcFormat;

  case Result of
    WSF_UTF32LE, WSF_UTF32BE:
      begin
        BufferSize := 8;
        CurrentSize := 0;
        repeat
          Inc(BufferSize, BufferSize shr 1);    // grow buffer 1.5 times
          SetLength(UTF32, CurrentSize + BufferSize);
          BytesRead := Stream.Read(UTF32[CurrentSize], BufferSize shl 2);
          if BytesRead < 0 then
          begin
            SetLength(UTF32, CurrentSize);
            raise EReadError.CreateRes(@SReadError);
          end;
          Inc(CurrentSize, BytesRead shl 2);
        until Cardinal(BytesRead) <> BufferSize shl 2;
        SetLength(UTF32, CurrentSize);
        if Result = WSF_UTF32BE then SwapUTF32(@UTF32[0], @UTF32[0], Length(UTF32));
        Str := UCS4StringToWideString(UTF32);
      end;
    WSF_UTF16LE, WSF_UTF16BE:
      begin
        BufferSize := 8;
        CurrentSize := Length(UTF16);
        repeat
          Inc(BufferSize, BufferSize shr 1);    // grow buffer 1.5 times
          SetLength(UTF16, CurrentSize + BufferSize);
          BytesRead := Stream.Read(UTF16[CurrentSize + 1], BufferSize shl 1);
          if BytesRead < 0 then
          begin
            SetLength(UTF16, CurrentSize);
            raise EReadError.CreateRes(@SReadError);
          end;
          Inc(CurrentSize, BytesRead shl 1);
        until Cardinal(BytesRead) <> BufferSize shl 1;
        SetLength(UTF16, CurrentSize);
        if Result = WSF_UTF16BE then SwapUTF16(@UTF16[1], @UTF16[1], Length(UTF16));
      end;
    WSF_UTF8:
      begin
        BufferSize := 8;
        CurrentSize := Length(UTF8);
        repeat
          Inc(BufferSize, BufferSize shr 1);    // grow buffer 1.5 times
          SetLength(UTF8, CurrentSize + BufferSize);
          BytesRead := Stream.Read(UTF8[CurrentSize + 1], BufferSize);
          if BytesRead < 0 then
          begin
            SetLength(UTF8, CurrentSize);
            raise EReadError.CreateRes(@SReadError);
          end;
          Inc(CurrentSize, BytesRead);
        until Cardinal(BytesRead) <> BufferSize;
        SetLength(UTF8, CurrentSize);
        Str := Utf8Decode(UTF8);
      end;
    WSF_ANSI:
      begin
        BufferSize := 8;
        CurrentSize := Length(ANSI);
        repeat
          Inc(BufferSize, BufferSize shr 1);    // grow buffer 1.5 times
          SetLength(ANSI, CurrentSize + BufferSize);
          BytesRead := Stream.Read(ANSI[CurrentSize + 1], BufferSize);
          if BytesRead < 0 then
          begin
            SetLength(ANSI, CurrentSize);
            raise EReadError.CreateRes(@SReadError);
          end;
          Inc(CurrentSize, BytesRead);
        until Cardinal(BytesRead) <> BufferSize;
        SetLength(ANSI, CurrentSize);
        Str := ANSI;
      end;
  end;
end;

function LoadWideString(const FileName: WideString; out Str: WideString; SrcFormat: Integer = WSF_AUTO): Integer; overload;
var
  FileHandle: THandle;
  HandleStream: THandleStream;
begin
  FileHandle := CreateFileW(PWideChar(FileName), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then EFOpenError.CreateResFmt(@SFOpenError, [FileName]);
  HandleStream := THandleStream.Create(FileHandle);
  try
    Result := LoadWideString(HandleStream, Str, SrcFormat);
  finally
    HandleStream.Free;
    FileClose(FileHandle);
  end;
end;

procedure SaveWideString(Stream: TStream; const Str: WideString; DstFormat: Integer = WSF_UTF8);
var
  ANSI: AnsiString;
  UTF8: UTF8String;
  UTF16: WideString;
  UTF32: UCS4String;
  V, N: Cardinal;
begin
  UTF32 := nil;
  if DstFormat and WDF_SKIPBOM = 0 then
  begin
    V := 0; N := 0;
    case DstFormat and WDF_MASK of
      WDF_UTF8: begin V := BOM_UTF8; N := 3; end;
      WDF_UTF16LE: begin V := Cardinal(BOM_UTF16LE); N := 2; end;
      WDF_UTF16BE: begin V := Cardinal(BOM_UTF16BE); N := 2; end;
      WDF_UTF32LE: begin V := BOM_UTF32LE; N := 4; end;
      WDF_UTF32BE: begin V := BOM_UTF32BE; N := 4; end;
    end;
    Stream.WriteBuffer(V, N);
  end;

  case DstFormat and WDF_MASK of
    WDF_ANSI:
      begin
        ANSI := Str;
        Stream.WriteBuffer(ANSI[1], Length(ANSI));
      end;
    WDF_UTF8:
      begin
        UTF8 := UTF8Encode(Str);
        Stream.WriteBuffer(UTF8[1], Length(UTF8));
      end;
    WDF_UTF16LE: Stream.WriteBuffer(Str[1], Length(Str) * 2);
    WDF_UTF16BE:
      begin
        SetLength(UTF16, Length(Str));
        SwapUTF16(@Str[1], @UTF16[1], Length(Str));
        Stream.WriteBuffer(UTF16[1], Length(Str) * 2);
      end;
    WDF_UTF32LE, WDF_UTF32BE:
      begin
        UTF32 := WideStringToUCS4String(Str);
        if DstFormat = WDF_UTF32BE then SwapUTF32(@UTF32[0], @UTF32[0], Length(UTF32));
        Stream.WriteBuffer(UTF32[0], Length(UTF32) * 4);
      end;
  end;
end;

procedure SaveWideString(const FileName: WideString; const Str: WideString; DstFormat: Integer = WSF_UTF8); overload;
var
  FileHandle: THandle;
  HandleStream: THandleStream;
begin
  FileHandle := CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE, 0,
    nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then EFCreateError.CreateResFmt(@SFCreateError, [FileName]);
  HandleStream := THandleStream.Create(FileHandle);
  try
    SaveWideString(HandleStream, Str, DstFormat);
  finally
    HandleStream.Free;
    FileClose(FileHandle);
  end;
end;

function StringToWideStringEx(const S: AnsiString; CodePage: Cardinal): WideString;
var
  InputLength, OutputLength: Integer;
begin
  if CodePage = CP_UTF8 then
    Result := UTF8Decode(S)
  else
  begin
    InputLength := Length(S);
    OutputLength := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, nil, 0);
    SetLength(Result, OutputLength);
    MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, PWideChar(Result), OutputLength);
  end;
end;

function WideStringToStringEx(const S: WideString; CodePage: Cardinal): AnsiString;
var
  InputLength, OutputLength: Integer;
begin
  if CodePage = CP_UTF8 then
    Result := UTF8Encode(S)
  else
  begin
    InputLength := Length(S);
    OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(S), InputLength, nil, 0, nil, nil);
    SetLength(Result, OutputLength);
    WideCharToMultiByte(CodePage, 0, PWideChar(S), InputLength, PAnsiChar(Result), OutputLength, nil, nil);
  end;
end;

function StringOfCharW(Ch: WideChar; Count: Integer): WideString;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 1 to Count do Result[I] := Ch;
end;


{ sorting functions }

procedure StringListWExchangeProc(AppData: Pointer; Index1, Index2: Integer);
begin
  with TStringListW(AppData) do ExchangeItems(Index1, Index2);
end;

function StringListWCompareProc(AppData: Pointer; Index1, Index2: Integer): Integer;
begin
  with TStringListW(AppData) do
    Result := CompareStrings(FItems[Index1].FString, FItems[Index2].FString);
end;

{ TStringListW }

function TStringListW.Add(const S: WideString; AObject: TObject): Integer;
begin
  Result := Count;
  Insert(Result, S, AObject);
end;

procedure TStringListW.AddStrings(StringList: TStringListW);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to StringList.Count - 1 do
      with StringList.FItems[I] do Add(FString, FObject);
  finally
    EndUpdate;
  end;
end;

procedure TStringListW.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      Add(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TStringListW.Assign(Source: TPersistent);
begin
  if Source is TStringListW then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TStringListW(Source));
    finally
      EndUpdate;
    end;
  end
  else if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else inherited;
end;

procedure TStringListW.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Clear;
      for I := 0 to Self.Count - 1 do
        with FItems[I] do
          TStrings(Dest).AddObject(FString, FObject);
    finally
      TStrings(Dest).EndUpdate;
    end;
  end
  else inherited;
end;

procedure TStringListW.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TStringListW.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TStringListW.Clear;
begin
  SetLength(FItems, 0);
  FCount := 0;
  FCapacity := 0;
  Modified;
end;

function TStringListW.CompareStrings(const S1, S2: WideString): Integer;
begin
  if CaseSensitive then Result := WideCompareStr(S1, S2)
  else Result := WideCompareText(S1, S2);
end;

procedure TStringListW.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TStringListW then
        Result := not Equals(TStringListW(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TStringListW.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FItems[Index + 1], FItems[Index], (Count - Index) * SizeOf(TStringItemW));
  Modified;
end;

procedure TStringListW.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then Changed;
end;

function TStringListW.Equals(StringList: TStringListW): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Count = StringList.Count then
    for I := 0 to Count - 1 do
      if FItems[I].FString <> StringList.FItems[I].FString then Exit;
  Result := True;
end;

procedure TStringListW.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

procedure TStringListW.Error(const Msg: WideString; Data: Integer);

  function ReturnAddr: Pointer;
  asm
    mov eax,[ebp+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TStringListW.ExchangeItems(Index1, Index2: Integer);
var
  P: Pointer;
begin
  P := Pointer(FItems[Index1].FString);
  Pointer(FItems[Index1].FString) := Pointer(FItems[Index2].FString);
  Pointer(FItems[Index2].FString) := P;
  P := Pointer(FItems[Index1].FObject);
  Pointer(FItems[Index1].FObject) := Pointer(FItems[Index2].FObject);
  Pointer(FItems[Index2].FObject) := P;
end;

function TStringListW.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Result := FItems[Index].FObject;
end;

function TStringListW.GetStr(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Result := FItems[Index].FString;
end;

function TStringListW.GetTextStr: WideString;
var
  I, J, L: Integer;
begin
  L := 0;
  for I := 0 to Count - 1 do Inc(L, Length(FItems[I].FString) + 2);
  SetLength(Result, L);
  J := 1;
  for I := 0 to Count - 1 do
  begin
    L := Length(FItems[I].FString);
    if L > 0 then
    begin
      System.Move(FItems[I].FString[1], Result[J], L * 2);
      Inc(J, L);
    end;
    Result[J] := #13;
    Result[J + 1] := #10;
    Inc(J, 2);
  end;
end;

function TStringListW.IndexOf(const S: WideString): Integer;
begin
  for Result := 0 to Count - 1 do
    if CompareStrings(FItems[Result].FString, S) = 0 then Exit;
  Result := -1;
end;

procedure TStringListW.Insert(Index: Integer; const S: WideString; AObject: TObject);
begin
  if (Index < 0) or (Index > Count) then Error(@SListIndexError, Index);
  if Count = Capacity then
  begin
    if Capacity > 64 then Inc(FCapacity, FCapacity div 4)
    else if Capacity > 8 then Inc(FCapacity, 16)
    else Inc(FCapacity, 4);
    SetLength(FItems, FCapacity);
  end;
  if Index < Count then System.Move(FItems[Index], FItems[Index + 1], (Count - Index) * SizeOf(TStringItemW));
  Pointer(FItems[Index].FString) := nil;
  FItems[Index].FString := S;
  FItems[Index].FObject := AObject;
  Inc(FCount);
  Modified;
end;

function TStringListW.LoadFromFile(const FileName: WideString; SrcFormat: Integer): Integer;
var
  S: WideString;
begin
  Result := LoadWideString(FileName, S, SrcFormat);
  if SrcFormat = 0 then Exit;
  SetTextStr(S);
end;

function TStringListW.LoadFromStream(Stream: TStream; SrcFormat: Integer): Integer;
var
  S: WideString;
begin
  Result := LoadWideString(Stream, S, SrcFormat);
  if SrcFormat = 0 then Exit;
  SetTextStr(S);
end;

procedure TStringListW.Modified;
begin
  if FUpdateCount = 0 then Changed;
end;

procedure TStringListW.ReadData(Reader: TReader);
begin
  try
    Reader.ReadListBegin;
    BeginUpdate;
    try
      Clear;
      while not Reader.EndOfList do Add(FilerReadWideString(Reader));
    finally
      EndUpdate;
    end;
    Reader.ReadListEnd;
  except
    showmessage('could not read stringlistw property');
    raise;
  end;
end;

procedure TStringListW.SaveToFile(const FileName: WideString; DstFormat: Integer);
begin
  SaveWideString(FileName, GetTextStr, DstFormat);
end;

procedure TStringListW.SaveToStream(Stream: TStream; DstFormat: Integer);
begin
  SaveWideString(Stream, GetTextStr, DstFormat);
end;

procedure TStringListW.SetObject(Index: Integer; Value: TObject);
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  FItems[Index].FObject := Value;
end;

procedure TStringListW.SetStr(Index: Integer; const Value: WideString);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  FItems[Index].FString := Value;
end;

procedure TStringListW.SetTextStr(const Value: WideString);
var
  P, PStart: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    if Length(Value) <= 0 then Exit;
    P := @Value[1];
    while P^ <> #0 do
    begin
      PStart := P;
      while (P^ <> #0) and (P^ <> #10) and (P^ <> #13) do Inc(P);

      SetString(S, PStart, P - PStart);
      Add(S);
      if P^ = #13 then
      begin
        Inc(P);
        if P^ = #10 then Inc(P);
      end
      else if P^ = #10 then
      begin
        Inc(P);
        if P^ = #13 then Inc(P);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TStringListW.Sort;
begin
  QuickSort(Self, 0, Count, StringListWExchangeProc, StringListWCompareProc);
end;

procedure TStringListW.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do FilerWriteWideString(Writer, Strings[I]);
  Writer.WriteListEnd;
end;


//----------------------------------------------------------------------------//

{ Quick sort implementation }

procedure QuickSort(AppData: Pointer; First, Count: Integer;
  Exchange: TItemExchangeProc;
  Compare: TItemCompareProc);

  procedure QS(PivotP: Integer; N: Integer);
  label
    TailRecursion,
    QuickBreak;
  var
    LeftP, RightP, PivotEnd, PivotTmp, LeftTmp: Integer;
    LN: Integer;
    RetVal: integer;
  begin

  TailRecursion:

    if N <= 2 then
    begin
      if N = 2 then
      begin
        RightP := PivotP + 1;
        if Compare(AppData, PivotP, RightP) > 0 then Exchange(AppData, PivotP, RightP);
      end;
      Exit;
    end;

    RightP := N - 1 + PivotP;
    LeftP := N shr 1 + PivotP;

    if Compare(AppData, LeftP, RightP) > 0 then Exchange(AppData, LeftP, RightP);
    if Compare(AppData, LeftP, PivotP) > 0 then Exchange(AppData, LeftP, PivotP)
    else if Compare(AppData, PivotP, RightP) > 0 then Exchange(AppData, PivotP, RightP);
    if N = 3 then
    begin
      Exchange(AppData, PivotP, LeftP);
      Exit;
    end;

    { Horae algorithm }
    PivotEnd := PivotP + 1;
    LeftP := PivotEnd;
    repeat
      RetVal := Compare(AppData, LeftP, PivotP);
      while RetVal <= 0 do
      begin
        if RetVal = 0 then
        begin
          Exchange(AppData, LeftP, PivotEnd);
          Inc(PivotEnd);
        end;
        if LeftP < RightP then Inc(LeftP) else goto QuickBreak;
        RetVal := Compare(AppData, LeftP, PivotP);
      end;
      while LeftP < RightP do
      begin
        RetVal := Compare(AppData, PivotP, RightP);
        if RetVal < 0 then Dec(RightP)
        else
        begin
          Exchange(AppData, LeftP, RightP);
          if RetVal <> 0 then
          begin
            Inc(LeftP);
            Dec(RightP);
          end;
          Break;
        end;
      end;
    until LeftP >= RightP;

  QuickBreak:

    if Compare(AppData, LeftP, PivotP) <= 0 then Inc(LeftP);
    LeftTmp := LeftP - 1;
    PivotTmp := PivotP;
    while (PivotTmp < PivotEnd) and (LeftTmp >= PivotEnd) do
    begin
      Exchange(AppData, PivotTmp, LeftTmp);
      Inc(PivotTmp);
      Dec(LeftTmp);
    end;
    LN := LeftP - PivotEnd;
    N := N + PivotP - LeftP;
    if N < LN then
    begin
      QS(LeftP, N);
      N := LN;
    end
    else
    begin
      QS(PivotP, LN);
      PivotP := LeftP;
    end;
    goto TailRecursion;
  end;

begin
  if Count < 2 then Exit;
  QS(First, Count);
end;



end.
