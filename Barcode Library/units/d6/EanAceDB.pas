unit EanAceDB;

interface
uses EanKod,DB,DbTables,DbCtrls,Classes,Controls,Messages, EanAce;

type
  TAceDBEan = class(TAceEan)
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

constructor TAceDBEan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TAceDBEan.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TAceDBEan.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TAceDBEan.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TAceDBEan.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TAceDBEan.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TAceDBEan.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TAceDBEan.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TAceDBEan.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TAceDBEan.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then Result := Name else Result := '';
end;

procedure TAceDBEan.DataChange(Sender: TObject);
begin
  BarCode:= GetFieldText;
end;

procedure TAceDBEan.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;





end.
