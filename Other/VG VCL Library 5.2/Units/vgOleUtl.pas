{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         OLE Automation                                }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgOleUtl;

interface
uses Windows, Classes, OleConst, SysUtils, vgSystem,
  {$IFDEF _D3_} ActiveX, ComObj {$ELSE} Ole2, OleAuto {$ENDIF};

type
{ TDispInvokeCache }
  TDispInvokeCache = class
  private
    FDispatch: IDispatch;
    FEntries: TList;
    function FindDispIDs(ANames: PChar; ANameCount: Integer): Integer;
    function GetDispIDs(Names: PChar; NameCount: Integer; DispIDs: PDispIDList): Boolean;
    procedure UpdateCache(Names: PChar; NameCount: Integer; DispIDs: PDispIDList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearCache;
    procedure Reset(Instance: Variant);
    procedure Copy(Cache: TDispInvokeCache);
    procedure Read(Stream: TStream);
    procedure Write(Stream: TStream);
    property Instance: IDispatch read FDispatch;
  end;

const
  DefLocale   = (LANG_ENGLISH + SUBLANG_DEFAULT * 1024) + (SORT_DEFAULT shl 16);

{ Variant routines }

procedure BadVariantType(VType: Integer);
{ Raises EInvalidOp exception with SBadVariantType message }

type
  TVarFlag = (vfByRef, vfVariant);
  TVarFlags = set of TVarFlag;

procedure WriteVariant(Stream: TStream; const Value: Variant);
{ Writes Variant into stream }

function ReadVariant(Stream: TStream; var Flags: TVarFlags): Variant;
{ Reads Variant from stream }

function VariantToVarArray(const Value: Variant): Variant;
{ Converts Value into Variant array of bytes }

function VarArrayToVariant(const Value: Variant): Variant;
{ Converts Variant array of bytes into Variant }

{ Variant compression and decompression }
function CompressVariant(Sign: TSignature; const Value: Variant; AData: Pointer): Variant;

function UnCompressVariant(const Value: Variant; AData: Pointer): Variant;

{ Ole utility routines } 
function VarToInterface(Instance: Variant): IDispatch;

function GetActiveOleObject(const ClassName: string; var Obj: Variant): HResult;

procedure GetIDsOfNames(const Dispatch: IDispatch; Names: PChar; NameCount: Integer; DispIDs: Pointer);

procedure VarDispInvoke(Result: PVariant; const Instance: Variant;
  CallDesc: PCallDesc; Params: Pointer); cdecl;

procedure DispatchInvoke(const Dispatch: IDispatch; DispIDs: PDispIDList;
  CallDesc: PCallDesc; Params: Pointer; Result: PVariant);

procedure InitHook;
procedure DoneHook;

implementation
uses vgVCLRes, vgUtils;

procedure BadVariantType(VType: Integer);
begin
  raise EInvalidOp.CreateFmt(LoadStr(SBadVariantType), [IntToHex(VType, 4)]);
end;

procedure WriteArray(Stream: TStream; const Value: Variant);
var
  VType, VSize, i, DimCount, ElemSize: Integer;
  VarArrayPtr: PSafeArray;
  LoDim, HiDim, Indices: PIntArray;
  V: Variant;
  P: Pointer;
begin
  VType := VarType(Value);
  Stream.WriteBuffer(VType, SizeOf(Integer));
  DimCount := VarArrayDimCount(Value);
  Stream.WriteBuffer(DimCount, SizeOf(DimCount));
  VarArrayPtr := PSafeArray(TVarData(Value).VArray);
  VSize := SizeOf(Integer) * DimCount;
  GetMem(LoDim, VSize);
  try
    GetMem(HiDim, VSize);
    try
      for i := 1 to DimCount do
      begin
        LoDim[i - 1] := VarArrayLowBound(Value, i);
        HiDim[i - 1] := VarArrayHighBound(Value, i);
      end;
      Stream.WriteBuffer(LoDim^,VSize);
      Stream.WriteBuffer(HiDim^,VSize);
      if VType and varTypeMask in EasyArrayTypes then
      begin
        ElemSize := SafeArrayGetElemSize(VarArrayPtr);
        VSize := 1;
        for i := 0 to DimCount - 1 do
          VSize := (HiDim[i] - LoDim[i] + 1) * VSize;
        VSize := VSize * ElemSize;
        P := VarArrayLock(Value);
        try
          Stream.WriteBuffer(VSize, SizeOf(VSize));
          Stream.WriteBuffer(P^,VSize);
        finally
          VarArrayUnlock(Value);
        end;
      end else
      begin { For varOleStr and varVariant }
        GetMem(Indices, VSize);
        try
          for I := 0 to DimCount - 1 do
            Indices[I] := LoDim[I];
          while True do
          begin
            if VType and varTypeMask <> varVariant then
            begin
              OleCheck(SafeArrayGetElement(VarArrayPtr, Indices^, TVarData(V).VPointer));
              TVarData(V).VType := VType and varTypeMask;
            end else
              OleCheck(SafeArrayGetElement(VarArrayPtr, Indices^, V));
            WriteVariant(Stream, V);
            Inc(Indices[DimCount - 1]);
            if Indices[DimCount - 1] > HiDim[DimCount - 1] then
              for i := DimCount - 1 downto 0 do
                if Indices[i] > HiDim[i] then
                begin
                  if i = 0 then Exit;
                  Inc(Indices[i - 1]);
                  Indices[i] := LoDim[i];
                end;
          end;
        finally
          FreeMem(Indices);
        end;
      end;
    finally
      FreeMem(HiDim);
    end;
  finally
    FreeMem(LoDim);
  end;
end;

procedure WriteVariant(Stream: TStream; const Value: Variant);
var
  I, VType: Integer;
  S: string;
begin
  VType := VarType(Value);
  if VarIsArray(Value) then
    WriteArray(Stream, Value) else
  case (VType and varTypeMask) of
    varEmpty, varNull: Stream.WriteBuffer(VType, SizeOf(Integer));
    varString:
    begin
      S := Value;
      I := Length(S);
      Stream.WriteBuffer(VType, SizeOf(Integer));
      Stream.WriteBuffer(I, SizeOf(Integer));
      Stream.WriteBuffer(TVarData(Value).VString^, I);
    end;
    varOleStr:
    begin
      S := Value;
      I := Length(S);
      Stream.WriteBuffer(VType, SizeOf(Integer));
      Stream.WriteBuffer(I, SizeOf(Integer));
      Stream.WriteBuffer(TVarData(Value).VOleStr^, I * 2);
    end;
    varDispatch:
      BadVariantType(VType);
    varVariant:
    begin
      if VType and varByRef <> varByRef then BadVariantType(VType);
      i := varByRef;
      Stream.WriteBuffer(i, SizeOf(Integer));
      WriteVariant(Stream, Variant(TVarData(Value).VPointer^));
    end;
    varUnknown:
      BadVariantType(VType);
  else
    Stream.WriteBuffer(VType, SizeOf(Integer));
    if VType and varByRef = varByRef then
      Stream.WriteBuffer(TVarData(Value).VPointer^, VariantSize[VType and varTypeMask]) else
      Stream.WriteBuffer(TVarData(Value).VPointer, VariantSize[VType and varTypeMask]);
  end;
end;

function ReadArray(Stream: TStream; VType: Integer): Variant;
var
  Flags: TVarFlags;
  LoDim, HiDim, Indices, Bounds: PIntArray;
  DimCount, VSize, i: Integer;
  P: Pointer;
  V: Variant;
  VarArrayPtr: PSafeArray;
begin
  Stream.ReadBuffer(DimCount, SizeOf(DimCount));
  VSize := DimCount * SizeOf(Integer);
  GetMem(LoDim, VSize);
  try
    GetMem(HiDim, VSize);
    try
      Stream.ReadBuffer(LoDim^, VSize);
      Stream.ReadBuffer(HiDim^, VSize);
      GetMem(Bounds, VSize * 2);
      try
        for i := 0 to DimCount - 1 do
        begin
          Bounds[i * 2] := LoDim[i];
          Bounds[i * 2 + 1] := HiDim[i];
        end;
        Result := VarArrayCreate(Slice(Bounds^,DimCount * 2), VType and varTypeMask);
      finally
        FreeMem(Bounds);
      end;
      VarArrayPtr := PSafeArray(TVarData(Result).VArray);
      if VType and varTypeMask in EasyArrayTypes then
      begin
        Stream.ReadBuffer(VSize, SizeOf(VSize));
        P := VarArrayLock(Result);
        try
          Stream.ReadBuffer(P^, VSize);
        finally
          VarArrayUnlock(Result);
        end;
      end else
      begin { For varVariant and varOleStr }
        GetMem(Indices, VSize);
        try
          ZeroMem(Indices, VSize);
          for I := 0 to DimCount - 1 do
            Indices[I] := LoDim[I];
          while True do
          begin
            V := ReadVariant(Stream, Flags);
            if VType and varTypeMask = varVariant then
              OleCheck(SafeArrayPutElement(VarArrayPtr, Indices^, V)) else
              OleCheck(SafeArrayPutElement(VarArrayPtr, Indices^, TVarData(V).VPointer^));
            Inc(Indices[DimCount - 1]);
            if Indices[DimCount - 1] > HiDim[DimCount - 1] then
              for i := DimCount - 1 downto 0 do
                if Indices[i] > HiDim[i] then
                begin
                  if i = 0 then Exit;
                  Inc(Indices[i - 1]);
                  Indices[i] := LoDim[i];
                end;
          end;
        finally
          FreeMem(Indices);
        end;
      end;
    finally
      FreeMem(HiDim);
    end;
  finally
    FreeMem(LoDim);
  end;
end;

function ReadVariant(Stream: TStream; var Flags: TVarFlags): Variant;
var
  I, VType: Integer;
  S: string;
  TmpFlags: TVarFlags;
{$IFNDEF _D3_}
  W: PWideChar;
{$ELSE}
  W: WideString;
{$ENDIF}
begin
  Flags := [];
  Stream.ReadBuffer(VType, SizeOf(VType));
  if VType and varByRef = varByRef then Include(Flags, vfByRef);
  { special case for varVariant byRef }
  if VType = varByRef then
  begin
    Include(Flags, vfVariant);
    Result := ReadVariant(Stream, TmpFlags);
    Exit;
  end;
  if vfByRef in Flags then VType := VType xor varByRef;
  if (VType and varArray) = varArray then
    Result := ReadArray(Stream, VType) else
  case VType and varTypeMask of
    varEmpty: VarClear(Result);
    varNull: Result := NULL;
    varString:
    begin
      Stream.ReadBuffer(I, SizeOf(Integer));
      SetLength(S, I);
      Stream.ReadBuffer(S[1], I);
      Result := S;
    end;
    varOleStr:
    begin
      Stream.ReadBuffer(I, SizeOf(Integer));
{$IFNDEF _D3_}
      W := SysAllocStringLen(nil, I);
      try
        Stream.ReadBuffer(W^, I * 2);
        TVarData(Result).VType := varOleStr;
        TVarData(Result).VOleStr := W;
      except
        SysFreeString(W);
        raise;
      end;
{$ELSE}
      SetLength(W, I);
      Stream.ReadBuffer(W[1], I * 2);
      Result := W;
{$ENDIF}
    end;
    varDispatch, varUnknown:
      BadVariantType(VType);
  else
    TVarData(Result).VType := VType;
    Stream.ReadBuffer(TVarData(Result).VPointer, VariantSize[VType and varTypeMask]);
  end;
end;

function VariantToVarArray(const Value: Variant): Variant;
var
  Data: PChar;
  F: TMemoryStream;
begin
  F := TMemoryStream.Create;
  try
    WriteVariant(F, Value);
    Result := VarArrayCreate([0, F.Size - 1], varByte);
    Data := VarArrayLock(Result);
    try
      Move(F.Memory^, Data^, F.Size);
    finally
      VarArrayUnlock(Result);
    end;
  finally
    F.Free;
  end;
end;

function VarArrayToVariant(const Value: Variant): Variant;
var
  F: TMemoryStream;
  TmpFlags: TVarFlags;
  Data: Pointer;
begin
  F := TMemoryStream.Create;
  try
    Data := VarArrayLock(Value);
    try
      F.WriteBuffer(Data^, VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1);
    finally
      VarArrayUnlock(Value);
    end;
    F.Position := 0;
    Result := ReadVariant(F, TmpFlags);
  finally
    F.Free;
  end;
end;

function CompressVariant(Sign: TSignature; const Value: Variant; AData: Pointer): Variant;
var
  Data: Pointer;
  F, C: TMemoryStream;
begin
  F := TMemoryStream.Create;
  try
    WriteVariant(F, Value);
    C := TMemoryStream.Create;
    try
      Compress(Sign, C, F.Memory^, F.Size, AData);
      Result := VarArrayCreate([0, C.Size + SizeOf(TSignature) - 1], varByte);
      Data := VarArrayLock(Result);
      try
        Move(PChar(@Sign)^, Data^, SizeOf(TSignature));
        Move(C.Memory^, (PChar(Data) + SizeOf(TSignature))^, C.Size);
      finally
        VarArrayUnlock(Result);
      end;
    finally
      C.Free;
    end;
  finally
    F.Free;
  end;
end;

function UnCompressVariant(const Value: Variant; AData: Pointer): Variant;
var
  Sign: TSignature;
  Data: Pointer;
  F, U: TMemoryStream;
  TmpFlags: TVarFlags;
begin
  F := TMemoryStream.Create;
  try
    Data := VarArrayLock(Value);
    try
      Move(Data^, Sign, SizeOf(TSignature));
      F.WriteBuffer((PChar(Data) + SizeOf(TSignature))^,
        VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) - SizeOf(TSignature) + 1);
    finally
      VarArrayUnlock(Value);
    end;
    U := TMemoryStream.Create;
    try
      UnCompress(Sign, U, F.Memory^, F.Size, AData);
      U.Position := 0;
      Result := ReadVariant(U, TmpFlags)
    finally
      U.Free;
    end;
  finally
    F.Free;
  end;
end;

type
  PDispInvokeEntry = ^TDispInvokeEntry;
  TDispInvokeEntry = record
    Size: Word;
    Names: TNames;
    Count: Byte;
    DispIDs: TDispIDs;
  end;

function VarToInterface(Instance: Variant): IDispatch;
begin
  Result := VarToDispatch(Instance);
  if not Assigned(Result) then
    raise EOleError.Create(ResStr(SVarNotObject));
end;

function GetActiveOleObject(const ClassName: string; var Obj: Variant): HResult;
var
  ClassID: TCLSID;
  Unk: IUnknown;
{$IFDEF _D3_}
  Disp: IDispatch;
begin
  ClassID := ProgIDToClassID(ClassName);
  Result := GetActiveObject(ClassID, nil, Unk);
  if Result = S_OK then
  begin
    Result := Unk.QueryInterface(IDispatch, Disp);
    if Result = S_OK then Obj := Disp;
  end;
{$ELSE}
  WideCharBuf: array[0..127] of WideChar;
begin
  StringToWideChar(ClassName, WideCharBuf, SizeOf(WideCharBuf) div 2);
  OleCheck(CLSIDFromProgID(WideCharBuf, ClassID));
  Result := GetActiveObject(ClassID, nil, Unk);
  if Result = S_OK then
  try
    Obj := VarFromInterface(Unk);
  finally;
    Unk.Release;
  end;
{$ENDIF}
end;

var
  FCacheList: TList = nil;
  Hooked: Boolean = False;
  OldVarDispProc: Pointer;

procedure InitHook;
begin
  if not Hooked then
  begin
    OldVarDispProc := VarDispProc;
    VarDispProc := @VarDispInvoke;
    Hooked := True;
  end;
end;

procedure DoneHook;
begin
  if Hooked then
  begin
    VarDispProc := OldVarDispProc;
    Hooked := False;
  end;
end;

procedure GetIDsOfNamesInternal(const Dispatch: IDispatch; Names: PChar;
  NameCount: Integer; DispIDs: Pointer);

  procedure Error;
  begin
    raise EOleError.CreateFmt(ResStr(SNoMethod), [Names]);
  end;

{$IFDEF _D3_ }
var
  N, SrcLen, DestLen: Integer;
  Src: PChar;
  Dest: PWideChar;
  NameRefs: PNamesArray;
  StackTop: Pointer;
  Temp: {$IFDEF _D4_} HResult {$ELSE} Integer {$ENDIF};
begin
  Src := Names;
  N := 0;
  asm
    MOV  StackTop, ESP
    MOV  EAX, NameCount
    INC  EAX
    SHL  EAX, 2  // sizeof pointer = 4
    SUB  ESP, EAX
    LEA  EAX, NameRefs
    MOV  [EAX], ESP
  end;
  repeat
    SrcLen := StrLen(Src);
    DestLen := MultiByteToWideChar(0, 0, Src, SrcLen, nil, 0) + 1;
    asm
      MOV  EAX, DestLen
      ADD  EAX, EAX
      ADD  EAX, 3      // round up to 4 byte boundary
      AND  EAX, not 3
      SUB  ESP, EAX
      LEA  EAX, Dest
      MOV  [EAX], ESP
    end;
    if N = 0 then NameRefs[0] := Dest else NameRefs[NameCount - N] := Dest;
    MultiByteToWideChar(0, 0, Src, SrcLen, Dest, DestLen);
    Dest[DestLen-1] := #0;
    Inc(Src, SrcLen+1);
    Inc(N);
  until N = NameCount;

  Temp := Dispatch.GetIDsOfNames(GUID_NULL, NameRefs, NameCount, DefLocale, DispIDs);

  if Temp = DISP_E_UNKNOWNNAME then Error else OleCheck(Temp);
  asm
    MOV  ESP, StackTop
  end;
end;

{$ELSE}

var
  I, N: Integer;
  Ch: WideChar;
  P: PWideChar;
  NameRefs: array[0..MaxDispArgs - 1] of PWideChar;
  WideNames: array[0..1023] of WideChar;
begin
  I := 0;
  N := 0;
  repeat
    P := @WideNames[I];
    if N = 0 then NameRefs[0] := P else NameRefs[NameCount - N] := P;
    repeat
      Ch := WideChar(Names[I]);
      WideNames[I] := Ch;
      Inc(I);
    until Char(Ch) = #0;
    Inc(N);
  until N = NameCount;

  if Dispatch.GetIDsOfNames(GUID_NULL, @NameRefs, NameCount,
    DefLocale, DispIDs) <> 0 then Error;
end;
{$ENDIF}

function FindCache(Dispatch: IDispatch): TDispInvokeCache;
var
  I: Integer;
begin
  if Assigned(FCacheList) then
    for I := 0 to FCacheList.Count - 1 do
    begin
      Result := FCacheList[I];
      if Result.FDispatch = Dispatch then Exit;
    end;
  Result := nil;
end;

procedure GetIDsOfNames(const Dispatch: IDispatch; Names: PChar; NameCount: Integer; DispIDs: Pointer);
var
  Cache: TDispInvokeCache;
begin
  Cache := FindCache(Dispatch);
  if not (Assigned(Cache) and Cache.GetDispIDs(Names, NameCount, DispIDs)) then
  begin
    GetIDsOfNamesInternal(Dispatch, Names, NameCount, DispIDs);

    if Assigned(Cache) then
      Cache.UpdateCache(Names, NameCount, DispIDs);
  end;
end;

procedure DispatchInvoke(const Dispatch: IDispatch; DispIDs: PDispIDList;
  CallDesc: PCallDesc; Params: Pointer; Result: PVariant);
begin
{$IFDEF _D3_}
  ComObj.DispatchInvoke(Dispatch, CallDesc, DispIDs, Params, Result);
{$ELSE}
  OleAuto.DispInvoke(Dispatch, CallDesc, DispIDs, Params, Result);
{$ENDIF}
end;

procedure VarDispInvoke(Result: PVariant; const Instance: Variant;
  CallDesc: PCallDesc; Params: Pointer); cdecl;
var
  Dispatch: IDispatch;
  DispIDs: array[0..MaxDispArgs - 1] of Integer;
begin
  Dispatch := VarToInterface(Instance);

  GetIDsOfNames(Dispatch, @CallDesc^.ArgTypes[CallDesc^.ArgCount],
    CallDesc^.NamedArgCount + 1, @DispIDs);

  if Result <> nil then VarClear(Result^);

  DispatchInvoke(IDispatch(Dispatch), @DispIDs, CallDesc, @Params, Result);
end;

{ TDispInvokeCache }
constructor TDispInvokeCache.Create;
begin
  ListAdd(FCacheList, Self);
  InitHook;
end;

destructor TDispInvokeCache.Destroy;
begin
  Reset(Null);
  ListRemove(FCacheList, Self);
  inherited;
end;

procedure TDispInvokeCache.ClearCache;
begin
  ListFreeMemAll(FEntries);
end;

function TDispInvokeCache.FindDispIDs(ANames: PChar; ANameCount: Integer): Integer;
var
  P: PDispInvokeEntry;
  I: Integer;
  Size: Word;
  Names: TNames;
begin
  Result := -1;
  if Assigned(FEntries) then
  begin
    WideCharToNames(ANames, ANameCount, Names, Size);
    for I := 0 to FEntries.Count - 1 do
    begin
      P := FEntries[I];
      if (Size = P^.Size) and (CompareMem(@P^.Names, @Names, Size)) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TDispInvokeCache.GetDispIDs(Names: PChar; NameCount: Integer; DispIDs: PDispIDList): Boolean;
var
  P: PDispInvokeEntry;
  I: Integer;
begin
  I := FindDispIDs(Names, NameCount);
  if (I >= 0) then
  begin
    P := FEntries[I];
    Move(P^.DispIDs, DispIDs^, SizeOf(TDispIDs));
    Result := True;
  end else
    Result := False;
end;

procedure TDispInvokeCache.UpdateCache(Names: PChar; NameCount: Integer; DispIDs: PDispIDList);
var
  P: PDispInvokeEntry;
begin
  GetMem(P, SizeOf(TDispInvokeEntry));
  try
    WideCharToNames(Names, NameCount, P^.Names, P^.Size);
    P^.Count := NameCount;
    Move(DispIDs^, P^.DispIDs, SizeOf(TDispIDs));
    ListAdd(FEntries, P);
  except
    FreeMem(P);
    raise;
  end;
end;

procedure TDispInvokeCache.Reset(Instance: Variant);
begin
  ClearCache;
  FDispatch := VarToDispatch(Instance);
end;

procedure TDispInvokeCache.Copy(Cache: TDispInvokeCache);
var
  F: TStream;
begin
  F := TMemoryStream.Create;
  try
    Cache.Write(F);
    F.Position := 0;
    Read(F);
  finally
    F.Free;
  end;
end;

procedure TDispInvokeCache.Read(Stream: TStream);
var
  P: PDispInvokeEntry;
  I, Count: Integer;
begin
  ClearCache;
  Stream.ReadBuffer(Count ,SizeOf(I));
  for I := 0 to Count - 1 do
  begin
    GetMem(P, SizeOf(P^));
    if not Assigned(FEntries) then FEntries := TList.Create;
    FEntries.Add(P);
    with P^ do
    begin
      Stream.ReadBuffer(Size, SizeOf(Size));
      Stream.ReadBuffer(Names, Size);
      Stream.ReadBuffer(Count, SizeOf(Count));
      Stream.ReadBuffer(DispIDs, Count * SizeOf(Integer));
    end;
  end;
end;

procedure TDispInvokeCache.Write(Stream: TStream);
var
  P: PDispInvokeEntry;
  I: Integer;
begin
  if Assigned(FEntries) then
  begin
    I := FEntries.Count;
    Stream.WriteBuffer(I ,SizeOf(I));
    for I := 0 to FEntries.Count - 1 do
    begin
      P := FEntries[I];
      with P^ do
      begin
        Stream.WriteBuffer(Size, SizeOf(Size));
        Stream.WriteBuffer(Names, Size);
        Stream.WriteBuffer(Count, SizeOf(Count));
        Stream.WriteBuffer(DispIDs, Count * SizeOf(Integer));
      end;
    end;
  end else begin
    I := 0;
    Stream.WriteBuffer(I ,SizeOf(I));
  end;
end;

end.
