unit KADaoSortByButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls;

type
  TKADaoSortByButton = class(TButton)
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
  KDaoTable, KADaoSortByDialogUnit, DaoApi;

Constructor TKADaoSortByButton.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  Caption     := 'Sort by';
  F_DataLink  := TFieldDataLink.Create;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnActiveChange  := ActiveChange;
  F_Before    := True;
End;

Destructor TKADaoSortByButton.Destroy;
Begin
  F_DataLink.Free;
  F_DataLink := nil;
  Inherited Destroy;
End;

Function TKADaoSortByButton.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoSortByButton.F_Set_DataSource(Value: TDataSource);
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

Procedure TKADaoSortByButton.ActiveChange(Sender: TObject);
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

Procedure TKADaoSortByButton.DataChange(Sender: TObject);
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

Procedure TKADaoSortByButton.Loaded;
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

procedure TKADaoSortByButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
end;

Procedure TKADaoSortByButton.HandleClickEvent;
Var
 DT : TKADaoTable;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
      DT := TKaDaoTable(DataSource.Dataset);
      Application.CreateForm(TKADaoSortByDialog,KADaoSortByDialog);
      if KADaoSortByDialog.Execute(DT.SortFieldNames,DT.SortedBy,DT.UseBrackets) Then
       Begin
         DT.Sort;
       End;
      KADaoSortByDialog.Free;
     End;
End;

Procedure TKADaoSortByButton.Click;
Begin
 if F_Before Then inherited Click;
 HandleClickEvent;
 if NOT F_Before Then inherited Click;
End;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoSortByButton]);
end;

end.
