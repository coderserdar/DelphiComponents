//---------------------------------------------------------------------------
//  Registering unit for ESDBGrid components
//---------------------------------------------------------------------------
unit ESDBGridReg;

interface
uses Classes, TypInfo,
  {$IFDEF VER140} DesignIntf, DesignEditors,
{$ELSE}{$IFDEF VER150} DesignIntf, DesignEditors,
  {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF}
 DB;

{ TFieldProperty}
type
  TFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses ESDBGrids, DBCtrls, ColMSEditor, DBMultiNav;

procedure Register;
begin
  RegisterComponents('Data Controls', [TESDBGrid]);
  RegisterComponents('Data Controls', [TDBMultiNav]);
  RegisterComponentEditor(TESDBGrid, TESDBGridEditor);
  RegisterPropertyEditor(TypeInfo(TCollection), TESDBGrid, 'Columns',
    TESDBGridColumnsProperty);
  RegisterPropertyEditor(TypeInfo(string), TColumn, 'FieldName', TColumnMSDataFieldProperty);
end;

{ TFieldProperty }

function TFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TFieldProperty.GetValueList(List: TStrings);
begin
end;

procedure TFieldProperty.GetValues(Proc: TGetStrProc);
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

end.
