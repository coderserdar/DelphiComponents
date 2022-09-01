unit AdvTreeViews_Reg;

interface
{$I Version.Inc}
uses
  Classes, SysUtils, DB,
  {$IFDEF Delphi6} DesignIntf, DesignEditors, // DesignWindows,
     {$IFDEF Delphi8} DesignMenus, {$ENDIF}
  {$ELSE} DsgnIntf, {$ENDIF}
  AdvTreeViews_RegDeclar, gmTreeView;

type

  TTreeViewFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('Win32 Additional',[TAdvTreeView]);

  RegisterComponents('Data Controls',[TAdvDBTreeView]);                                    


  RegisterPropertyEditor(TypeInfo(string), TDBTreeViewDataLinkParams,
                         'KeyField', TTreeViewFieldProperty);

  RegisterPropertyEditor(TypeInfo(string), TDBTreeViewDataLinkParams,
                         'DisplayField', TTreeViewFieldProperty);

  RegisterPropertyEditor(TypeInfo(string), TDBTreeViewDataLinkParams,
                         'ParentField', TTreeViewFieldProperty);
end;

procedure GetDataSetFieldNames(aDataSource: TDataSource; Proc: TGetStrProc);
var
  i: Integer;
  List: TStrings;
begin
  if (aDataSource <> nil) and (aDataSource.DataSet <> nil) then
  begin
    List := TStringList.Create;
    try
      aDataSource.DataSet.GetFieldNames(List);
      for i:=0 to List.Count-1 do
        Proc(List.Strings[i]);
    finally
      List.Free;
    end;
  end;
end;


{ TTreeViewFieldProperty }
function TTreeViewFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TTreeViewFieldProperty.GetValues(Proc: TGetStrProc);
begin
  GetDataSetFieldNames((GetComponent(0) as TDBTreeViewDataLinkParams).DataSource, Proc);
end;

end.

