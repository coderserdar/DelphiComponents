
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ClassTools;

interface

{$I STD.INC}

uses
  Classes, TypInfo, SysUtils;

{ TClassPropertyStrings }

type
  TClassPropertyStrings = class(TStrings)
  private
    FCount: Integer;
    FInstance: TObject;
    FPropList: PPropList;
    procedure SetInstance(Value: TObject);
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function GetPropInfo(Index: Integer): PPropInfo; overload;
    function GetPropInfo(const S: string): PPropInfo; overload;
    procedure Insert(Index: Integer; const S: string); override;
    property Instance: TObject read FInstance write SetInstance;
  end;

{ EStreamFormatError }

  EStreamFormatError = class(Exception);

{ TReadMemoryStream }

  TReadMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(Reference: Pointer; Size: Integer);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ TStreamable }

  TStreamable = class(TComponent)
  protected
    procedure ReadComponents(Reader: TReader);
    procedure WriteComponents(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  end;

{ Automation component storage routines }

function ComponentToString(Instance: TComponent): string;
procedure ComponentToVariant(Instance: TComponent; var Data: OleVariant);
procedure VariantToComponent(const Data: OleVariant; var Instance: TComponent);

implementation

uses
  StrConst, Consts;

{ TClassPropertyStrings }

destructor TClassPropertyStrings.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TClassPropertyStrings.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source <> nil then
    SourceName := Source.ClassName
  else
    SourceName := 'nil';
  raise EConvertError.CreateResFmt(@SAssignError, [SourceName, ClassName]);
end;

procedure TClassPropertyStrings.Clear;
begin
  Instance := nil;
end;

procedure TClassPropertyStrings.Delete(Index: Integer);
begin
end;

function TClassPropertyStrings.GetPropInfo(Index: Integer): PPropInfo;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FPropList^[Index];
end;

function TClassPropertyStrings.GetPropInfo(const S: string): PPropInfo;
var
  I: Integer;
begin
  I := IndexOf(S);
  if I > -1 then
    Result := FPropList^[I]
  else
    Result := nil;
end;

procedure TClassPropertyStrings.Insert(Index: Integer; const S: string);
begin
end;

function TClassPropertyStrings.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FPropList^[Index].Name;
end;

function TClassPropertyStrings.GetCount: Integer;
begin
  Result := FCount;
end;

function TClassPropertyStrings.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := TObject(GetOrdProp(Instance, FPropList^[Index]));
end;

procedure TClassPropertyStrings.SetInstance(Value: TObject);
begin
  if Value <> FInstance then
  begin
    if FPropList <> nil then
      FreeMem(FPropList);
    FInstance := Value;
    FPropList := nil;
    FCount := 0;
    if FInstance <> nil then
    begin
      FCount := GetPropList(Instance.ClassInfo, [tkClass], nil);
      GetMem(FPropList, FCount * SizeOf(PPropInfo));
      GetPropList(Instance.ClassInfo, [tkClass], FPropList);
    end;
  end;
end;

{ TReadMemoryStream }

constructor TReadMemoryStream.Create(Reference: Pointer; Size: Integer);
begin
  inherited Create;
  SetPointer(Reference, Size);
end;

function TReadMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamFormatError.Create(SCantWriteStream);
end;

{ TStreamable }

procedure TStreamable.ReadComponents(Reader: TReader);
var
  Strings: TClassPropertyStrings;
  Instance: TObject;
  I: integer;
begin
  Strings := TClassPropertyStrings.Create;
  try
    Strings.Instance := Self;
    for I := 0 to Strings.Count - 1 do
    begin
      Instance := GetObjectProp(Self, Strings.GetPropInfo(I), TComponent);
      if Reader.ReadBoolean then
      begin
        TComponent(Instance) := Reader.ReadComponent(Instance as TComponent);
        SetObjectProp(Self, Strings.GetPropInfo(I), Instance);
      end
      else if Instance <> nil then
      begin
        Instance.Free;
        SetObjectProp(Self, Strings.GetPropInfo(I), nil);
      end;
    end;
  finally
    Strings.Free;
  end;
end;

procedure TStreamable.WriteComponents(Writer: TWriter);
var
  Strings: TClassPropertyStrings;
  Instance: TObject;
  CanWrite: Boolean;
  I: integer;
begin
  Strings := TClassPropertyStrings.Create;
  try
    Strings.Instance := Self;
    for I := 0 to Strings.Count - 1 do
    begin
      Instance := Strings.Objects[I];
      CanWrite := (Instance <> nil) and (Instance is TComponent);
      Writer.WriteBoolean(CanWrite);
      if CanWrite then
        Writer.WriteComponent(Instance as TComponent);
    end;
  finally
    Strings.Free;
  end;
end;

procedure TStreamable.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Components', ReadComponents, WriteComponents, True);
  inherited DefineProperties(Filer);
end;

{ Automation component storage routines }

function ComponentToString(Instance: TComponent): string;
var
  Stream: TMemoryStream;
  Text: TStringStream;
begin
  Stream := TMemoryStream.Create;
  Stream.WriteComponent(Instance);
  Stream.Seek(soFromBeginning, 0);
  Text := TStringStream.Create('');
  ObjectBinaryToText(Stream, Text);
  Result := Text.DataString;
end;

procedure ComponentToVariant(Instance: TComponent; var Data: OleVariant);
var
  MemoryStream: TCustomMemoryStream;
  P: Pointer;
  I: Integer;
begin
  VarClear(Data);
  if Instance = nil then Exit;
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.WriteComponent(Instance);
    I := MemoryStream.Size;
    MemoryStream.Seek(0, 0);
    Data := VarArrayCreate([0, I - 1], varByte);
    P := VarArrayLock(Data);
    try
      Move(MemoryStream.Memory^, P^, I);
    finally
      VarArrayUnlock(Data);
    end;
  finally
    MemoryStream.Free;
  end;
end;

procedure VariantToComponent(const Data: OleVariant; var Instance: TComponent);
const
  varByteArray = varByte or varArray;
var
  MemoryStream: TCustomMemoryStream;
  I: Integer;
begin
  if (TVarData(Data).VType and varByteArray = varByteArray) then
  begin
    I := VarArrayHighBound(Data, 1) - VarArrayLowBound(Data, 1) + 1;
    MemoryStream := TReadMemoryStream.Create(VarArrayLock(Data), I);
    try
      Instance := MemoryStream.ReadComponent(Instance);
    finally
      MemoryStream.Free;
      VarArrayUnlock(Data);
    end;
  end
  else
    raise EStreamError.Create(SInvalidStreamFormat);
end;

end.
