unit DTDBTreeReg;

interface


{$I DTDBTree.Inc}

uses
{$IFNDEF TR_DELPHI5}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf, LibIntf,
{$ENDIF}  
  Classes;

type
  TDTDataFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses DTTableTree, DTClientTree, DTADOTree, DTDBTreeView, DB, TypInfo;

procedure Register;
begin
  RegisterComponents('Virtual Controls', [TDTTableTree, TDTClientTree, TDTADOTree]);
  RegisterPropertyEditor(TypeInfo(String), TDTDBTreeFields, 'KeyFieldName', TDTDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String), TDTDBTreeFields, 'ListFieldName', TDTDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String), TDTDBTreeFields, 'ParentFieldName', TDTDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String), TDTDBTreeFields, 'HasChildrenFieldName', TDTDataFieldProperty);  
end;

{ TVTDataFieldProperty }

function TDTDataFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TDTDataFieldProperty.GetValueList(List: TStrings);
var
  DBTreeFields: TDTDBTreeFields;
  DataSource: TDataSource;
begin
  DBTreeFields := GetComponent(0) as TDTDBTreeFields;
  DataSource := DBTreeFields.DBTreeView.DataSource;
  if DataSource <> nil then
    if DataSource.DataSet <> nil then
      DataSource.DataSet.GetFieldNames(List);
end;

procedure TDTDataFieldProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for i := 0 to Values.Count - 1 do Proc(Values[i]);
  finally
    Values.Free;
  end;
end;

end.
