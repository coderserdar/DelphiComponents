unit KADaoIndexesCombo;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, DB, DBCtrls, StdCtrls, KDaoDatabase, KDaoTable, DaoApi, Dialogs;

type
  TKADaoIndexesCombo = class(TComboBox)
  private
    { Private declarations }
    F_DataLink       : TFieldDataLink;
    F_AutoOpen       : Boolean;
    F_InGetting      : Boolean;
    Procedure          ActiveChange(Sender: TObject);
  protected
    { Protected declarations }
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);
    Procedure   F_Set_AutoOpen(Value: Boolean);
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure   Select;override;
    Procedure   Loaded; override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
  published
    { Published declarations }
    Property    AutoOpen          : Boolean     Read F_AutoOpen        Write F_Set_AutoOpen;
    Property    DataSource        : TDataSource Read F_Get_DataSource  Write F_Set_DataSource;
  end;

procedure Register;

implementation

Constructor TKADaoIndexesCombo.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  F_AutoOpen                 := True;
  F_InGetting                := False;
  F_DataLink                 := TFieldDataLink.Create;
  F_DataLink.Control         := Self;
  F_DataLink.OnActiveChange  := ActiveChange;
  Style                      := csDropDownList;
End;

Destructor TKADaoIndexesCombo.Destroy;
Begin
 F_DataLink.Free;
 F_DataLink := nil;
 inherited Destroy;
End;

procedure TKADaoIndexesCombo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then
      Begin
        DataSource := nil;
      End;
end;

Procedure TKADaoIndexesCombo.Loaded;
Begin
 Inherited Loaded;
 Items.Clear;
 Text := '';
End;

Procedure TKADaoIndexesCombo.Select;
Var
  II : Integer;
Begin
  If  (Assigned(F_DataLink.DataSource))
  And (DataSource.Dataset is TKaDaoTable)
  And (TKaDaoTable(DataSource.Dataset).TableType=DaoApi.dbOpenTable)
  And (F_AutoOpen)
  And (F_DataLink.Active)
  And (NOT F_InGetting) Then
      Begin
        II := Items.IndexOf(Text);
        if II > -1 Then
           Begin
             F_InGetting := True;
             TKaDaoTable(DataSource.Dataset).IndexName := Text;
             ItemIndex := II;
             F_InGetting := False;
           End;
      End;
    Inherited Select;    
End;

Procedure TKADaoIndexesCombo.ActiveChange(Sender: TObject);
Var
  II  : Integer;
  Txt : String;
  T   : TStringList;
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.Dataset.Active)
  And (DataSource.Dataset is TKaDaoTable)
  And (TKaDaoTable(DataSource.Dataset).TableType=DaoApi.dbOpenTable)
  And (NOT F_InGetting) Then
      Begin
        Text := '';
        T    := TStringList.Create;
        Try
         TKaDaoTable(DataSource.Dataset).GetIndexNames(T);
         Txt := T.Text;
        Finally
         T.Free;
        End;
        if Txt <> Items.Text Then Items.SetText(PChar(Txt));
        II := Items.IndexOf(TKaDaoTable(DataSource.Dataset).IndexName);
        if II <> -1 Then ItemIndex := II;
      End;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.Dataset is TKaDaoTable)
  And (NOT DataSource.DataSet.Active)
  And (NOT F_InGetting) Then
      Begin
        Items.Clear;
      End;
End;

Function TKADaoIndexesCombo.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoIndexesCombo.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

Procedure TKADaoIndexesCombo.F_Set_AutoOpen(Value: Boolean);
Begin
 F_AutoOpen := Value;
 Items.Clear;
 ActiveChange(Self);
End;



procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoIndexesCombo]);
end;

end.
