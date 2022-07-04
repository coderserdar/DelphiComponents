unit KADaoSelectIndexButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls;

type
  TKADaoSelectIndexButton = class(TButton)
  private
    { Private declarations }
    F_DataLink     : TFieldDataLink;
    F_Before       : Boolean;
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
  KDaoTable, KADaoSelectIndexUnit, DaoApi;

Constructor TKADaoSelectIndexButton.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  Caption     := 'Select Index';
  F_DataLink  := TFieldDataLink.Create;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnActiveChange  := ActiveChange;
  F_Before    := True;
End;

Destructor TKADaoSelectIndexButton.Destroy;
Begin
  F_DataLink.Free;
  F_DataLink := nil;
  Inherited Destroy;
End;

Function TKADaoSelectIndexButton.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoSelectIndexButton.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (TKaDaoTable(DataSource.Dataset).TableType=dbOpenTable) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
end;

Procedure TKADaoSelectIndexButton.ActiveChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (TKaDaoTable(DataSource.Dataset).TableType=dbOpenTable) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoSelectIndexButton.DataChange(Sender: TObject);
Begin
if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (TKaDaoTable(DataSource.Dataset).TableType=dbOpenTable) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoSelectIndexButton.Loaded;
Begin
  Inherited Loaded;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (TKaDaoTable(DataSource.Dataset).TableType=dbOpenTable) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

procedure TKADaoSelectIndexButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
end;

Procedure TKADaoSelectIndexButton.HandleClickEvent;
Var
  NewIndex : String;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
    Begin
      if TKaDaoTable(DataSource.Dataset).TableType=dbOpenTable Then
         Begin
           NewIndex:=TKaDaoTable(DataSource.Dataset).IndexName;
           Application.CreateForm(TKADaoSelectIndex, KADaoSelectIndex);
           if KADaoSelectIndex.Execute(TKaDaoTable(DataSource.Dataset), NewIndex) Then
              Begin
              End;
           KADaoSelectIndex.Free;
           TKaDaoTable(DataSource.Dataset).IndexName := NewIndex;
         End;
    End;
End;

Procedure TKADaoSelectIndexButton.Click;
Begin
 if F_Before Then inherited Click;
 HandleClickEvent;
 if NOT F_Before Then inherited Click;
End;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoSelectIndexButton]);
end;

end.
