{ Copyright (C) 1998-2006, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  In this unit I defined design-time editors for
  TSMDBComboBox and TSMDBFilterComboBox components
}
unit SMDBCombReg;

interface

uses SMDBComb;

procedure Register;

{$I SMVersion.inc}

implementation
uses Classes, DB, TypInfo,
     {$IFDEF SMForDelphi6} DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF};

{ TSMFieldProperty }
{ For TSMDBFilterComboBox component (FieldDisplay/FieldValue properties) }

type
  TSMFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure GetValueList(List: TStrings); virtual;
    function GetDataSourcePropName: string; virtual;
  end;

function TSMFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TSMFieldProperty.GetValues(Proc: TGetStrProc);
var i: Integer;
    Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for i := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

function TSMFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TSMFieldProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := TComponent(GetComponent(0));
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBComboBox, TSMDBFilterComboBox]);
  RegisterPropertyEditor(TypeInfo(string), TSMDBFilterComboBox, 'FieldDisplay', TSMFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TSMDBFilterComboBox, 'FieldValue', TSMFieldProperty);
end;

end.
