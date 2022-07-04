{
    Firesoft - ExportSuite
    Copyright (C) 1997-2006 Federico Firenze

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published
    by the Free Software Foundation; either version 2 of the License,
    or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Federico Firenze,
    Buenos Aires, Argentina
    webmaster@delphi.com.ar

}

unit DataExport;

{$I DELPHI.VER}

interface

uses
  Classes, Db, {DbCtrls,} SysUtils;

type
  TDataExport = class;

{ TExportField class }

  TExportField = class(TCollectionItem)
  private
    FDataField: string;
    FSave: Boolean;
    function GetField: TField;
    function GetDataSet: TDataSet;
  protected
    {$IFNDEF LESS100}
    function GetDisplayName: string; override;
    {$ENDIF}
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property DataSet: TDataSet read GetDataSet;
    property Field: TField read GetField;
  published
    property DataField: string read FDataField write FDataField;
    property Save: Boolean read FSave write FSave default True;
  end;

  TExportFieldClass = class of TExportField;

{ TExportFields class }

  TExportFields = class(TCollection)
  private
    function GetItem(Index: Integer): TExportField;
    procedure SetItem(Index: Integer; const Value: TExportField);
  protected
    FOwner: TDataExport;
  public
    procedure BuildFields;
    function GetOwner: TPersistent; {$IFNDEF LESS100}override;{$ENDIF}
    constructor Create(AOwner : TDataExport; ItemClass: TExportFieldClass);
    function Add: TExportField;
    property Items[Index: Integer]: TExportField read GetItem write SetItem; default;
  end;

  TExportFieldsClass = class of TExportFields;

  TCancelEvent = procedure(Sender: TObject; var Cancel : Boolean) of object;

{ TDataExport class }

  TDataExport = class(TComponent)
  private
    FDataSet: TDataSet;
    FActive: Boolean;
    FSaveIfEmpty: Boolean;
    FFetchFirst: boolean;
    FMaxRecords: Cardinal;
    FRecNo: Cardinal;
    FPreserveBookmark: Boolean;
    FOnBeginExport: TNotifyEvent;
    FOnEndExport: TNotifyEvent;
    FBeforeWriteRecord: TCancelEvent;
    FAfterWriteRecord: TNotifyEvent;
    procedure SetDataSet(const Value: TDataSet);
  protected
    FStream: TStream;
    FDynamicFields: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure OpenFile; virtual;
    procedure CloseFile; virtual;

    procedure SaveRecords; virtual;
    procedure WriteRecord; virtual; abstract;


    function Write(const Buffer; Count: Longint): Longint;
    function WriteLine(AString: string): Longint;
    function WriteString(AString: string; Count:Longint{$IFNDEF LESS110} = 0{$ENDIF}): Longint;
    function WriteChar(AChar: Char): Longint;

  { Propertys to publish  }
    property SaveIfEmpty: Boolean read FSaveIfEmpty write FSaveIfEmpty default False;
    property FetchFirst: boolean read FFetchFirst write FFetchFirst default True;
    property MaxRecords: Cardinal read FMaxRecords write FMaxRecords default 0;
    property RecNo: Cardinal read FRecNo; { Da el número de registro guardado por si no FetchFirst }
    property PreserveBookmark : Boolean read FPreserveBookmark write FPreserveBookmark default True;
    property OnBeginExport: TNotifyEvent read FOnBeginExport write FOnBeginExport;
    property OnEndExport: TNotifyEvent read FOnEndExport write FOnEndExport;
    property BeforeWriteRecord: TCancelEvent read FBeforeWriteRecord write FBeforeWriteRecord;
    property AfterWriteRecord: TNotifyEvent read FAfterWriteRecord write FAfterWriteRecord;
  public
    function GetFields: TExportFields; virtual; abstract;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);

    procedure Cancel;
    property Active: Boolean read FActive;
 {published}
    property DataSet: TDataSet read FDataSet write SetDataSet;
  end;

implementation

uses
  Consts, DBConsts, DBGrids
  {$IFNDEF LESS140}, RTLConsts{$ENDIF};

{$IFDEF LESS100}
const
{$ELSE}
resourcestring
{$ENDIF}
 SDataNotAssigned = 'Cannot perform this operation without a dataset';
 SStreamNotAssigned = 'Cannot perform this operation without a Stream';

{ TExportField }

procedure TExportField.Assign(Source: TPersistent);
begin
  if Source is TExportField then
    with Source as TExportField do
    begin
      FDataField := DataField;
      FSave := Save;
    end
  else if Source is TField then
    with Source as TField do
    begin
      FDataField := FieldName;
      FSave := Visible;
    end
  else if Source is TColumn then
    with Source as TColumn do
    begin
      FDataField := FieldName;
      {$IFNDEF LESS110}
      FSave := Visible;
      {$ENDIF}
    end
  else
    inherited;
end;

constructor TExportField.Create(Collection: TCollection);
begin
  inherited;
  FSave := True;
end;

function TExportField.GetDataSet: TDataSet;
begin
  if (Collection is TExportFields) then
    Result := TExportFields(Collection).FOwner.DataSet
  else
    Result := nil;
end;

{$IFNDEF LESS100}
function TExportField.GetDisplayName: string;
begin
  if FDataField <> '' then
    Result := FDataField
  else
    Result := inherited GetDisplayName;
end;
{$ENDIF}

function TExportField.GetField: TField;
var
  FDataSet : TDataSet;
begin
  FDataSet := GetDataSet;
  if Assigned(FDataSet) and FDataSet.Active then
    Result := FDataSet.FieldByName(FDataField)
  else
    Result := nil;
end;

{ TExportFields }

function TExportFields.Add: TExportField;
begin
  Result := inherited Add as TExportField;
end;

constructor TExportFields.Create(AOwner: TDataExport; ItemClass: TExportFieldClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

procedure TExportFields.BuildFields;
var
  ADataSet: TDataSet;
  iField: Integer;
begin
  Clear;
  if (GetOwner is TDataExport) and Assigned(TDataExport(GetOwner).DataSet) then
  begin
    ADataSet := TDataExport(GetOwner).DataSet;
    for iField := 0 to ADataSet.FieldCount-1 do {Por Delphi 3 ADataSet.Fields.Count -1}
      Add.Assign(ADataSet.Fields[iField]);
  end;
end;

function TExportFields.GetItem(Index: Integer): TExportField;
begin
  Result := inherited GetItem(Index) as TExportField;
end;

function TExportFields.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TExportFields.SetItem(Index: Integer; const Value: TExportField);
begin
  SetItem(Index, Value);
end;

{ TDataExport }

procedure TDataExport.Cancel;
begin
  FActive := False;
  repeat
  until FStream = nil;
end;

procedure TDataExport.CloseFile;
var
  FFields: TExportFields;
begin
  FFields := GetFields;

  if FDynamicFields and Assigned(FFields) then
     FFields.Clear;

  if Assigned(FOnEndExport) then
    FOnEndExport(Self);

  FActive := False;
end;

constructor TDataExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FFetchFirst := True;
  FSaveIfEmpty := False;
  FMaxRecords := 0;
  FStream := nil;
  FPreserveBookmark := True;
end;

destructor TDataExport.Destroy;
begin
  if FActive then
    Cancel;

  inherited;
end;

procedure TDataExport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FDataSet) then
    SetDataSet(nil);
end;

procedure TDataExport.OpenFile;
var
  FFields: TExportFields;
begin
  FActive := True;

  if Assigned(FOnBeginExport) then
    FOnBeginExport(Self);

  FFields := GetFields;
  if Assigned(FFields) and (FFields.Count = 0) then begin
     FDynamicFields := True;
     FFields.BuildFields;
  end else
     FDynamicFields := False;
end;

procedure TDataExport.SaveRecords;
var
  Bookmark: TBookmarkStr;
  ACancel: Boolean;
begin
  FDataSet.DisableControls;
  try
    if FPreserveBookmark then
      Bookmark := DataSet.Bookmark;
    try
      FRecNo := 0;
      if FFetchFirst then
        FDataSet.First;
      while (not FDataSet.EOF) and FActive
        and ((FRecNo <= FMaxRecords) or (FMaxRecords = 0)) do
      begin
        ACancel := False;
        if Assigned(FBeforeWriteRecord) then
          FBeforeWriteRecord(Self, ACancel);

        if not ACancel then
        begin
          WriteRecord;
          Inc(FRecNo);

          if Assigned(FAfterWriteRecord) then
            FAfterWriteRecord(Self);
        end;
        FDataSet.Next;
      end;
    finally
      if FPreserveBookmark then
        FDataSet.Bookmark := Bookmark;
    end;
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TDataExport.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;

  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TDataExport.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(FStream) then
    Result := FStream.Write(Buffer, Count)
  else
    raise Exception.Create(SStreamNotAssigned);
end;

function TDataExport.WriteChar(AChar: Char): Longint;
begin
  Result := Write(AChar, 1);
end;

function TDataExport.WriteLine(AString: string): Longint;
begin
  Result := WriteString(AString + #13#10{$IFDEF LESS110}, 0{$ENDIF});
end;

function TDataExport.WriteString(AString: string; Count: LongInt{$IFNDEF LESS110} = 0{$ENDIF}): Longint;
begin
  if Count = 0 then
    Count := Length(AString);

  Result := Write(AString[1], Count);
end;

procedure TDataExport.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDataExport.SaveToStream(Stream: TStream);
begin
  if FDataset = nil then
    raise Exception.Create(SDataNotAssigned);

  if FDataset.Active = False then
    {$IFDEF LESS100}
    raise Exception.CreateRes(SDataSetClosed);
    {$ELSE}
    raise Exception.Create(SDataSetClosed);
    {$ENDIF}

  {$IFDEF LESS100}
  if (not FSaveIfEmpty) and (FDataset.RecordCount = 0) then
    raise Exception.CreateRes(SDataSetEmpty);
  {$ELSE}
  if (not FSaveIfEmpty) and FDataset.IsEmpty then
    raise Exception.Create(SDataSetEmpty);
  {$ENDIF}

  FStream := Stream;
  try
    OpenFile;
    SaveRecords;
    CloseFile;
  finally
    FStream := Nil;
  end;
end;

end.

