//---------------------------------------------------------------------------
//  Registering unit for Volga components
//---------------------------------------------------------------------------
unit VolDBReg;

interface
uses Classes, TypInfo,
  {$IFDEF VER140} DesignIntf, DesignEditors,
{$ELSE}{$IFDEF VER150} DesignIntf, DesignEditors,
  {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF}
 DB;

{ TVolgaFieldProperty}
type
  TVolgaFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TVolgaSourceFieldProperty }

  TVolgaSourceFieldProperty = class(TVolgaFieldProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TVolgaLookupFieldProperty }

  TVolgaLookupFieldProperty = class(TVolgaFieldProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure Register;

implementation

uses VolDBGrid, VolColEditor, VolDBEdit, VolCalend, VolFndEd, VolPeriod, VolMeter;

procedure Register;
begin
  RegisterComponents('Volga', [TVolgaDBGrid, TVolgaDBEdit, TVolgaCalendar,
    TVolgaFindEdit, TVolgaPeriod, TVolgaMeter]);

  RegisterPropertyEditor(TypeInfo(string), TVolgaLookupProperties, 'SourceKeyField', TVolgaSourceFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaLookupProperties, 'LookupKeyField', TVolgaLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaLookupProperties, 'ListFieldNames', TVolgaLookupFieldProperty);

  RegisterComponentEditor(TVolgaDBGrid, TVolgaDBGridEditor);
  RegisterPropertyEditor(TypeInfo(TCollection), TVolgaDBGrid, 'Columns',
    TVolgaDBGridColumnsProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaColumn, 'FieldName', TVolgaColumnDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaColumn, 'LookupKeyField', TVolgaColumnDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaColumn, 'LookupLinkField', TVolgaColumnLookupKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaColumn, 'LookupDropDownFields', TVolgaColumnLookupKeyProperty);
end;

{ TVolgaFieldProperty }

function TVolgaFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TVolgaFieldProperty.GetValueList(List: TStrings);
begin
end;

procedure TVolgaFieldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TVolgaSourceFieldProperty }

procedure TVolgaSourceFieldProperty.GetValueList(List: TStrings);
var
  AEdit: TVolgaDBEdit;
  DataSet: TDataSet;
begin
  AEdit := (GetComponent(0) as VolDBEdit.TVolgaLookupProperties).Owner;
  if (AEdit = nil) then Exit;
  if AEdit.DataSource = nil then Exit;
  DataSet := AEdit.DataSource.DataSet;
  if (DataSet <> nil) then
    DataSet.GetFieldNames(List);
end;

{ TVolgaLookupFieldProperty }

procedure TVolgaLookupFieldProperty.GetValueList(List: TStrings);
var
  AEdit: TVolgaDBEdit;
  DataSet: TDataSet;
begin
  AEdit := (GetComponent(0) as VolDBEdit.TVolgaLookupProperties).Owner;
  if (AEdit = nil) then Exit;
  DataSet := AEdit.LookupDataSet;
  if (DataSet <> nil) then
    DataSet.GetFieldNames(List);
end;

end.
