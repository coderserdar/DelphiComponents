unit AccessHelperForAdoReg;
{$I 'AHADOCD.PAS'}
interface
Uses Classes {$IFNDEF D6UP}, DsgnIntf{$ENDIF}
             {$IFDEF D6UP},  DesignIntf, DesignWindows,  DesignEditors{$ENDIF};

Type
  TDBDatabaseNameEditor = class(TStringProperty)
    Public
      Procedure Edit;override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;
 TDBSystemDatabaseEditor = class(TStringProperty)
    Public
      Procedure Edit;override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;
 TDBDatabaseTypeEditor = class(TStringProperty)
    Public
      Procedure GetValues( Proc: TGetStrProc); override;
      Procedure SetValue(const Value: string); override;
      Function  GetAttributes: TPropertyAttributes; override;
    End;
 TDBConnectionEditor= class(TComponentProperty)
    Public
      TargetProc : TGetStrProc;
      procedure StrProc(const S: string);
      procedure GetValues(Proc: TGetStrProc);override;
    End;

procedure Register;

implementation
Uses
  Sysutils, TypInfo, Dialogs, AccessHelperForAdo;

Function TDBDatabaseNameEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TDBDatabaseNameEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TAdoHelper then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

procedure TDBDatabaseNameEditor.Edit;
var
   DBase    : TAdoHelper;
   OD       : TOpenDialog;
   I        : Integer;
Begin
   if GetComponent(0) is TAdoHelper then
      Begin
        DBase    := TAdoHelper(GetComponent(0));
        OD       := TOpenDialog.Create(Nil);
        Try
          I           := DBase.DBTypesList.IndexOf(DBase.DatabaseType);
          OD.Filename := DBase.Database;
          OD.Options  := OD.Options+[ofFileMustExist];
          if I <> -1 Then
             Begin
               OD.Filter      := DBase.DBExtList.Strings[I];
               OD.FilterIndex := 0;
               If DBase.DBExtList.Objects[I]=TObject(True) Then OD.Filename := OD.Filename+'*.*';
             End;
          if OD.Execute Then
             Begin
              if I <> -1 Then
                 Begin
                   If DBase.DBExtList.Objects[I]=TObject(True) Then
                     SetStrValue(ExtractFilePath(OD.Filename))
                   Else
                     SetStrValue(OD.Filename);
                 End
              Else
                 Begin
                   SetStrValue(OD.Filename);
                 End;
              Modified;
             End;
        Finally
          OD.Free;
        End;
      End;
End;


Function TDBSystemDatabaseEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= [paDialog];
End;

Procedure TDBSystemDatabaseEditor.SetValue(const Value: string);
Begin
if GetComponent(0) is TAdoHelper then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

procedure TDBSystemDatabaseEditor.Edit;
var
   DBase    : TAdoHelper;
   OD       : TOpenDialog;
Begin
   if GetComponent(0) is TAdoHelper then
      Begin
        DBase    := TAdoHelper(GetComponent(0));
        OD       := TOpenDialog.Create(Nil);
        Try
          OD.Filename    := DBase.SystemDatabase;
          OD.Options     := OD.Options+[ofFileMustExist];
          OD.FilterIndex := 0;
          OD.Filter      := 'MS Access workgroup file (*.mda;*.mdw)|*.mda;*.mdw|MS Access Database (*.mdb)|*.mdb|All Files (*.*)|*.*';
          if OD.Execute Then
             Begin
              SetStrValue(OD.Filename);
              Modified;
             End;
        Finally
          OD.Free;
        End;
      End;
End;


Procedure TDBDatabaseTypeEditor.GetValues( Proc: TGetStrProc);
Var
  DBase : TAdoHelper;
  X     : Integer;
Begin
  if GetComponent(0) is TAdoHelper then
  Begin
    DBase := TAdoHelper(GetComponent(0));
    Try
      For X := 0 to DBase.DBTypesList.Count-1 do Proc(DBase.DBTypesList[X]);
    Finally
    End;
  End;
End;

Procedure TDBDatabaseTypeEditor.SetValue(const Value: string);
Begin
 if GetComponent(0) is TAdoHelper then
  Begin
    inherited SetValue(Value);
    Modified;
  End;
End;

Function TDBDatabaseTypeEditor.GetAttributes: TPropertyAttributes;
Begin
  Result:= Inherited GetAttributes + [paValueList, paSortList];
End;

procedure TDBConnectionEditor.StrProc(const S: string);
var
  Comp : TComponent;
Begin
  Comp := Designer.GetComponent(S);
  if (AnsiCompareText(Comp.ClassName,'TKAAdoDatabase')=0)
  OR (AnsiCompareText(Comp.ClassName,'TAdoConnection')=0)
  OR (AnsiCompareText(Comp.ClassName,'TAoAdoConnection')=0) Then TargetProc(S);
End;


procedure TDBConnectionEditor.GetValues(Proc: TGetStrProc);
Begin
 TargetProc := Proc;
 Designer.GetComponentNames(GetTypeData(TComponent.ClassInfo), StrProc);
End;


procedure Register;
begin
 RegisterPropertyEditor(TypeInfo(String),TAdoHelper,'DatabaseType',TDBDatabaseTypeEditor);
 RegisterPropertyEditor(TypeInfo(String),TAdoHelper,'Database',TDBDatabaseNameEditor);
 RegisterPropertyEditor(TypeInfo(String),TAdoHelper,'SystemDatabase',TDBSystemDatabaseEditor);
 RegisterPropertyEditor(TypeInfo(TComponent),TAdoHelper,'AdoConnection',TDBConnectionEditor);
end;



end.
