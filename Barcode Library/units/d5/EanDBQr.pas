unit EanDBQr;

interface
uses EanKod,EanQr,DB,DBTables,DBCtrls,Messages,Controls,Classes;
type

  TQrDBEan = Class(TQrEan)
     private
            FDataLink: TFieldDataLink;
            function GetDataField: string;
            function GetDataSource: TDataSource;
            function GetField: TField;
            function GetFieldText: string;
            procedure SetDataField(const Value: string);
            procedure SetDataSource(Value: TDataSource);
            procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
     protected
            procedure Loaded; override;
            procedure Notification(AComponent: TComponent; Operation: TOperation); override;
            procedure DataChange(Sender: TObject);
     public
            constructor Create(AOwner: TComponent); override;
            destructor Destroy; override;
            property Field: TField read GetField;
     published
            property DataField: string read GetDataField write SetDataField;
            property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


implementation


constructor TQrDBEan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := TWinControl(Self);
  FDataLink.OnDataChange := DataChange;
end;

destructor TQrDBEan.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TQrDBEan.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TQrDBEan.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TQrDBEan.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TQrDBEan.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TQrDBEan.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TQrDBEan.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TQrDBEan.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TQrDBEan.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then Result := Name else Result := '';
end;

procedure TQrDBEan.DataChange(Sender: TObject);
begin
  FEan.BarCode:= GetFieldText;
end;

procedure TQrDBEan.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;






end.
