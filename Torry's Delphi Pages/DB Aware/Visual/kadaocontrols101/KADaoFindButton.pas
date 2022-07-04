unit KADaoFindButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls;

type
  TKADaoFindButton = class(TButton)
  private
    { Private declarations }
    F_DataLink     : TFieldDataLink;
    F_Before       : Boolean;
    F_LastFilter   : TStringList;
  protected
    { Protected declarations }
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure   HandleClickEvent;
    Procedure   DataChange(Sender: TObject);
    Procedure   ActiveChange(Sender: TObject);
    procedure   Loaded; override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
    Procedure   Click; override;
  published
    { Published declarations }
    Property    DataSource  : TDataSource Read F_Get_DataSource Write F_Set_DataSource;
    Property    CallBefore  : Boolean     Read F_Before         Write F_Before;
  end;

procedure Register;

implementation
Uses
  KDaoTable, KADaoFindSeekUnit, DaoApi;

Constructor TKADaoFindButton.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  Caption     := 'Find';
  F_LastFilter:= TStringList.Create;
  F_DataLink  := TFieldDataLink.Create;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnActiveChange  := ActiveChange;
  F_Before    := True;
End;

Destructor TKADaoFindButton.Destroy;
Begin
  F_LastFilter.Free;
  F_DataLink.Free;
  F_DataLink := nil;
  Inherited Destroy;
End;

Function TKADaoFindButton.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoFindButton.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (TKaDaoTable(DataSource.Dataset).TableType<>dbOpenTable) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
end;

Procedure TKADaoFindButton.ActiveChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (TKaDaoTable(DataSource.Dataset).TableType<>dbOpenTable) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoFindButton.DataChange(Sender: TObject);
Begin
if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (TKaDaoTable(DataSource.Dataset).TableType<>dbOpenTable) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoFindButton.Loaded;
Begin
  Inherited Loaded;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (TKaDaoTable(DataSource.Dataset).TableType<>dbOpenTable) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

procedure TKADaoFindButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
end;

Procedure TKADaoFindButton.HandleClickEvent;
Var
 BK : TBookmarkStr;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
       if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
          BK := TKaDaoTable(DataSource.Dataset).Bookmark;
       Application.CreateForm(TKADaoFindSeekDialog, KADaoFindSeekDialog);
       if KADaoFindSeekDialog.Execute(TKaDaoTable(DataSource.DataSet), False) Then
          Begin
          End
       Else
          Begin
           if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
              TKaDaoTable(DataSource.Dataset).Bookmark:=BK;
          End;
       KADaoFindSeekDialog.Free;
     End;
End;

Procedure TKADaoFindButton.Click;
Begin
 if F_Before Then inherited Click;
 HandleClickEvent;
 if NOT F_Before Then inherited Click;
End;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoFindButton]);
end;

end.
