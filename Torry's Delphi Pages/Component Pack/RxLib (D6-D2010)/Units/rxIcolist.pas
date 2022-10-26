{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit rxIcoList;

interface

{$I RX.INC}

uses
  Messages, Windows,
  SysUtils, Classes, Graphics;

type

{ TIconList class }

  TIconList = class(TPersistent)
  private
    FList: TList;
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure SetUpdateState(Updating: Boolean);
    procedure IconChanged(Sender: TObject);
    function AddIcon(Icon: TIcon): Integer;
  protected
    procedure Changed; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function Get(Index: Integer): TIcon; virtual;
    function GetCount: Integer; virtual;
    procedure Put(Index: Integer; Icon: TIcon); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Icon: TIcon): Integer; virtual;
    function AddResource(Instance: THandle; ResId: PChar): Integer; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function IndexOf(Icon: TIcon): Integer; virtual;
    procedure Insert(Index: Integer; Icon: TIcon); virtual;
    procedure InsertResource(Index: Integer; Instance: THandle;
      ResId: PChar); virtual;
    procedure LoadResource(Instance: THandle; const ResIds: array of PChar);
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property Count: Integer read GetCount;
    property Icons[Index: Integer]: TIcon read Get write Put; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TIconList }

constructor TIconList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TIconList.Destroy;
begin
  FOnChange := nil;
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TIconList.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TIconList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TIconList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

procedure TIconList.ReadData(Stream: TStream);
var
  Len, Cnt: Longint;
  I: Integer;
  Icon: TIcon;
  Mem: TMemoryStream;
begin
  BeginUpdate;
  try
    Clear;
    Mem := TMemoryStream.Create;
    try
      Stream.Read(Cnt, SizeOf(Longint));
      for I := 0 to Cnt - 1 do
      begin
        Stream.Read(Len, SizeOf(Longint));
        if Len > 0 then
        begin
          Icon := TIcon.Create;
          try
            Mem.SetSize(Len);
            Stream.Read(Mem.Memory^, Len);
            Mem.Position := 0;
            Icon.LoadFromStream(Mem);
            AddIcon(Icon);
          except
            Icon.Free;
            raise;
          end;
        end
        else AddIcon(nil);
      end;
    finally
      Mem.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TIconList.WriteData(Stream: TStream);
var
  I: Integer;
  Len: Longint;
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Len := FList.Count;
    Stream.Write(Len, SizeOf(Longint));
    for I := 0 to FList.Count - 1 do
    begin
      Mem.Clear;
      if (Icons[I] <> nil) and not Icons[I].Empty then
      begin
        Icons[I].SaveToStream(Mem);
        Len := Mem.Size;
      end
      else
        Len := 0;
      Stream.Write(Len, SizeOf(Longint));
      if Len > 0 then
        Stream.Write(Mem.Memory^, Mem.Size);
    end;
  finally
    Mem.Free;
  end;
end;

procedure TIconList.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  var
    I: Integer;
    Ancestor: TIconList;
  begin
    Ancestor := TIconList(Filer.Ancestor);
    if (Ancestor <> nil) and (Ancestor.Count = Count) and (Count > 0) then
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := Icons[I] <> Ancestor.Icons[I];
        if Result then
          Break;
      end
    end
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineBinaryProperty('Icons', ReadData, WriteData, DoWrite);
end;

function TIconList.Get(Index: Integer): TIcon;
begin
  Result := TObject(FList[Index]) as TIcon;
end;

function TIconList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TIconList.IconChanged(Sender: TObject);
begin
  Changed;
end;

procedure TIconList.Put(Index: Integer; Icon: TIcon);
begin
  BeginUpdate;
  try
    if Index = Count then
      Add(nil);
    if Icons[Index] = nil then
      FList[Index] := TIcon.Create;
    Icons[Index].OnChange := IconChanged;
    Icons[Index].Assign(Icon);
  finally
    EndUpdate;
  end;
end;

function TIconList.AddIcon(Icon: TIcon): Integer;
begin
  Result := FList.Add(Icon);
  if Icon <> nil then
    Icon.OnChange := IconChanged;
  Changed;
end;

function TIconList.Add(Icon: TIcon): Integer;
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  try
    Ico.Assign(Icon);
    Result := AddIcon(Ico);
  except
    Ico.Free;
    raise;
  end;
end;

function TIconList.AddResource(Instance: THandle; ResId: PChar): Integer;
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  try
    Ico.Handle := LoadIcon(Instance, ResId);
    Result := AddIcon(Ico);
  except
    Ico.Free;
    raise;
  end;
end;

procedure TIconList.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source = nil then Clear
  else
  if Source is TIconList then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TIconList(Source).Count - 1 do
        Add(TIconList(Source)[I]);
    finally
      EndUpdate;
    end;
  end
  else
  if Source is TIcon then
  begin
    BeginUpdate;
    try
      Clear;
      Add(TIcon(Source));
    finally
      EndUpdate;
    end;
  end
  else inherited Assign(Source);
end;

procedure TIconList.Clear;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := FList.Count - 1 downto 0 do
      Delete(I);
  finally
    EndUpdate;
  end;
end;

procedure TIconList.Delete(Index: Integer);
var
  Icon: TIcon;
begin
  Icon := Icons[Index];
  if Icon <> nil then
  begin
    Icon.OnChange := nil;
    Icon.Free;
  end;
  FList.Delete(Index);
  Changed;
end;

procedure TIconList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
  Changed;
end;

function TIconList.IndexOf(Icon: TIcon): Integer;
begin
  Result := FList.IndexOf(Icon);
end;

procedure TIconList.InsertResource(Index: Integer; Instance: THandle;
  ResId: PChar);
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  try
    Ico.Handle := LoadIcon(Instance, ResId);
    FList.Insert(Index, Ico);
    Ico.OnChange := IconChanged;
  except
    Ico.Free;
    raise;
  end;
  Changed;
end;

procedure TIconList.Insert(Index: Integer; Icon: TIcon);
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  try
    Ico.Assign(Icon);
    FList.Insert(Index, Ico);
    Ico.OnChange := IconChanged;
  except
    Ico.Free;
    raise;
  end;
  Changed;
end;

procedure TIconList.LoadResource(Instance: THandle; const ResIds: array of PChar);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Low(ResIds) to High(ResIds) do
      AddResource(Instance, ResIds[I]);
  finally
    EndUpdate;
  end;
end;

procedure TIconList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
  Changed;
end;

procedure TIconList.SetUpdateState(Updating: Boolean);
begin
  if not Updating then Changed;
end;

procedure TIconList.LoadFromStream(Stream: TStream);
begin
  ReadData(Stream);
end;

procedure TIconList.SaveToStream(Stream: TStream);
begin
  WriteData(Stream);
end;

end.