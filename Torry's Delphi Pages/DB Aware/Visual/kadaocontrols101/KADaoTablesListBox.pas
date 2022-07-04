unit KADaoTablesListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, DB, DBCtrls, StdCtrls, KDaoDatabase, KDaoTable, DaoApi, Dialogs;

type
  TViewObjects = (Tables,QueryDefs,All);

type
  TKADaoTablesListBox = class(TListBox)
  private
    { Private declarations }
    F_AutoOpen       : Boolean;
    F_DataLink       : TFieldDataLink;
    F_InGetting      : Boolean;
    F_ViewType       : TViewObjects;
    Procedure          ActiveChange(Sender: TObject);
  protected
    { Protected declarations }
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);
    Procedure   F_Set_ViewType(Value : TViewObjects);
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
    Property    ViewType          : TViewObjects Read F_ViewType       Write F_Set_ViewType;
  end;

procedure Register;

implementation

Constructor TKADaoTablesListBox.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  F_AutoOpen                 := True;
  F_InGetting                := False;
  F_ViewType                 := Tables;
  F_DataLink                 := TFieldDataLink.Create;
  F_DataLink.Control         := Self;
  F_DataLink.OnActiveChange  := ActiveChange;
End;

Destructor TKADaoTablesListBox.Destroy;
Begin
 F_DataLink.Free;
 F_DataLink := nil;
 inherited Destroy;
End;

procedure TKADaoTablesListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then
      Begin
        DataSource := nil;
      End;
end;


Procedure TKADaoTablesListBox.Loaded;
Begin
 Inherited Loaded;
 Items.Clear;
End;

Procedure TKADaoTablesListBox.Click;
Var
  TT : Integer;
Begin
  Inherited Click;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.Dataset is TKaDaoTable)
  And Assigned(TKaDaoTable(DataSource.Dataset).Database)
  And TKaDaoTable(DataSource.Dataset).Database.Connected
  And (F_AutoOpen)
  And (NOT F_InGetting) Then
      Begin
        if ItemIndex > -1 Then
           Begin
             TT := TKaDaoTable(DataSource.Dataset).TableType;
             F_DataLink.DataSet.Close;
             Case F_ViewType of
                  Tables    :  Begin
                                 TKaDaoTable(DataSource.Dataset).TableType := TT;
                                 TKaDaoTable(DataSource.Dataset).TableName := Items.Strings[ItemIndex];
                               End;
                  QueryDefs :  Begin
                                 TKaDaoTable(DataSource.Dataset).TableType := DaoApi.dbOpenDynaset;
                                 TKaDaoTable(DataSource.Dataset).QueryDefName := Items.Strings[ItemIndex];
                               End;
                  All       :  Begin
                                 if TKaDaoTable(DataSource.Dataset).Database.TableNames.IndexOf(Items.Strings[ItemIndex]) <> -1 Then
                                    Begin
                                      TKaDaoTable(DataSource.Dataset).TableType := TT;
                                      TKaDaoTable(DataSource.Dataset).TableName := Items.Strings[ItemIndex];
                                    End;
                                 if TKaDaoTable(DataSource.Dataset).Database.QueryDefNames.IndexOf(Items.Strings[ItemIndex]) <> -1 Then
                                    Begin
                                      TKaDaoTable(DataSource.Dataset).TableType := DaoApi.dbOpenDynaset;
                                      TKaDaoTable(DataSource.Dataset).QueryDefName := Items.Strings[ItemIndex];
                                    End;
                               End;
             End;
             Try
               TKaDaoTable(DataSource.Dataset).ReadOnly:=False;
               if TKaDaoTable(DataSource.Dataset).QueryDefName <> '' Then
                   Begin
                     If  (TKaDaoTable(DataSource.Dataset).Database.CoreDatabase.QueryDefs.Item[TKaDaoTable(DataSource.Dataset).QueryDefName].Parameters.Count > 0)
                     And (TKaDaoTable(DataSource.Dataset).PromptQueryDefParameters) Then F_DataLink.DataSet.Open
                     Else
                     If  (TKaDaoTable(DataSource.Dataset).Database.CoreDatabase.QueryDefs.Item[TKaDaoTable(DataSource.Dataset).QueryDefName].Parameters.Count = 0) Then F_DataLink.DataSet.Open
                   End
               Else
                   Begin
                     F_DataLink.DataSet.Open;
                   End;
             Except
             End;
           End;
      End;
End;


Procedure TKADaoTablesListBox.ActiveChange(Sender: TObject);
Var
  II  : Integer;
  Txt : String;
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.Dataset is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        II := -1;
        Case F_ViewType of
             Tables    : Txt := TKaDaoTable(DataSource.Dataset).Database.TableNames.Text;
             QueryDefs : Txt := TKaDaoTable(DataSource.Dataset).Database.QueryDefNames.Text;
             All       : Txt := TKaDaoTable(DataSource.Dataset).Database.TableNames.Text + TKaDaoTable(DataSource.Dataset).Database.QueryDefNames.Text;
        End;
        if Txt <> Items.Text Then Items.SetText(PChar(Txt));
        Case F_ViewType of
             Tables    : II := Items.IndexOf(TKaDaoTable(DataSource.Dataset).TableName);
             QueryDefs : II := Items.IndexOf(TKaDaoTable(DataSource.Dataset).QueryDefName);
             All       : Begin
                          II := Items.IndexOf(TKaDaoTable(DataSource.Dataset).TableName);
                          if II = -1 Then II := Items.IndexOf(TKaDaoTable(DataSource.Dataset).QueryDefName);
                         End;
        End;
        if II <> -1 Then
           Begin
             F_InGetting := True;
             ItemIndex := II;
             F_InGetting := False;
           End;
      End;
End;


Procedure TKADaoTablesListBox.F_Set_ViewType(Value : TViewObjects);
Begin
  F_ViewType   := Value;
  ActiveChange(Self);
End;

Function TKADaoTablesListBox.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoTablesListBox.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

Procedure TKADaoTablesListBox.F_Set_AutoOpen(Value: Boolean);
Begin
 F_AutoOpen := Value;
 Items.Clear;
 ActiveChange(Self);
End;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoTablesListBox]);
end;

end.
