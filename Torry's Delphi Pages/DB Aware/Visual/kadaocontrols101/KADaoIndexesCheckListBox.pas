unit KADaoIndexesCheckListBox;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, DB, DBCtrls, StdCtrls, CheckLst, KDaoDatabase, KDaoTable, DaoApi, Dialogs;

type
  TKADaoIndexesCheckListBox = class(TCheckListBox)
  private
    { Private declarations }
    F_DataLink       : TFieldDataLink;
    F_InGetting      : Boolean;
    F_AutoOpen       : Boolean;
    Procedure          ActiveChange(Sender: TObject);
  protected
    { Protected declarations }
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);
    Procedure   F_Set_AutoOpen(Value: Boolean);
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure   Loaded; override;
    Procedure   Click; override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
  published
    { Published declarations }
    Property    AutoOpen          : Boolean     Read F_AutoOpen        Write F_Set_AutoOpen;
    Property    DataSource        : TDataSource  Read F_Get_DataSource Write F_Set_DataSource;
  end;

procedure Register;

implementation

Constructor TKADaoIndexesCheckListBox.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  F_AutoOpen                 := True;
  F_InGetting                := False;
  F_DataLink                 := TFieldDataLink.Create;
  F_DataLink.Control         := Self;
  F_DataLink.OnActiveChange  := ActiveChange;
End;

Destructor TKADaoIndexesCheckListBox.Destroy;
Begin
 F_DataLink.Free;
 F_DataLink := nil;
 inherited Destroy;
End;

procedure TKADaoIndexesCheckListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then
      Begin
        DataSource := nil;
      End;
end;


Procedure TKADaoIndexesCheckListBox.Loaded;
Begin
 Inherited Loaded;
 Items.Clear;
End;

Procedure TKADaoIndexesCheckListBox.Click;
Begin
  Inherited Click;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.Dataset.Active)
  And (DataSource.Dataset is TKaDaoTable)
  And (TKaDaoTable(DataSource.Dataset).TableType=DaoApi.dbOpenTable)
  And (F_AutoOpen)
  And (NOT F_InGetting) Then
      Begin
        if ItemIndex > -1 Then
           Begin
             F_InGetting := True;
             TKaDaoTable(DataSource.Dataset).IndexName := Items.Strings[ItemIndex];
             F_InGetting := False;
           End;
      End;
End;


Procedure TKADaoIndexesCheckListBox.ActiveChange(Sender: TObject);                              
Var
  II  : Integer;
  Txt : String;
  T   : TStringList;
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.Dataset is TKaDaoTable)
  And (TKaDaoTable(DataSource.Dataset).TableType=DaoApi.dbOpenTable)
  And (DataSource.DataSet.Active) Then
      Begin
        T  := TStringList.Create;
        Try
         TKaDaoTable(DataSource.Dataset).GetIndexNames(T);
         Txt := T.Text;
        Finally
         T.Free;
        End;
        if Txt <> Items.Text Then Items.SetText(PChar(Txt));
        II := Items.IndexOf(TKaDaoTable(DataSource.Dataset).IndexName);
        if II <> -1 Then
           Begin
             F_InGetting := True;
             ItemIndex := II;
             F_InGetting := False;
           End;
      End;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.Dataset is TKaDaoTable)
  And (NOT DataSource.DataSet.Active) Then
      Begin
        Items.Clear;
      End;
End;

Function TKADaoIndexesCheckListBox.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoIndexesCheckListBox.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

Procedure TKADaoIndexesCheckListBox.F_Set_AutoOpen(Value: Boolean);
Begin
 F_AutoOpen := Value;
 Items.Clear;
 ActiveChange(Self);
End;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoIndexesCheckListBox]);
end;

end.
