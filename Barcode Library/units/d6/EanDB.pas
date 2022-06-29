unit EanDB;

interface

{$I ean.inc}

uses EanKod,
     {$ifdef MSWINDOWS}Messages, DB,DbTables,DbCtrls, Controls, {$endif}
     {$ifdef LINUX} DB, QDBCtrls, QControls, {$endif}
     Classes ;

type
  TDBEan = class(TEan)
     private
            FDataLink: TFieldDataLink;
            function GetDataField: string;
            function GetDataSource: TDataSource;
            function GetField: TField;
            procedure SetDataField(const Value: string);
            procedure SetDataSource(Value: TDataSource);
     protected
            procedure DataChange(Sender: TObject);
     public
            constructor Create(AOwner: TComponent); override;
            destructor  Destroy; override;
            property    Field: TField read GetField;
     published
            property DataField: string read GetDataField write SetDataField;
            property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;




implementation

constructor TDBEan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TDBEan.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  inherited Destroy;
end;

function TDBEan.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBEan.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TDBEan.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBEan.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDBEan.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBEan.DataChange(Sender: TObject);
begin
     if FDataLink.Field<>nil then
           BarCode:= FDataLink.Field.AsString;
end;


end.
