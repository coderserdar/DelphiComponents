unit EanRBDB;

interface
uses EanKod,DB,DbTables,DbCtrls,Classes,Controls,Messages, EanRB,
     ppCtrls, ppDsgnCt, ppDevice, ppDrwCmd, ppTypes, ppClass;

type
  TRBDBEan = class(TRBEan)
     private
     protected
            procedure Loaded; override;
            procedure DataChange(Sender: TObject);
            function  IsDataAware:Boolean; override;
     public
     published
            property DataField;
            property DataPipeLine;
  end;



implementation

function  TRbDBEan.IsDataAware:Boolean;
begin
     Result := True;
end;


procedure TRbDBEan.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;


procedure TRbDBEan.DataChange(Sender: TObject);
begin
  BarCode:= DataPipeline.GetFieldAsString(DataField);
end;



initialization
  ppRegisterComponent(TRBDBEan, 'Ean components', 10, 0, 'RbDBEan', HInstance);

finalization

  ppUnRegisterComponent(TRbDbEan);

end.
