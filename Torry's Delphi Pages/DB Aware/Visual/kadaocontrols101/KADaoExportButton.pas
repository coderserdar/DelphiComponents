unit KADaoExportButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls;

type
  TKADaoExportButton = class(TButton)
  private
    { Private declarations }
    F_DataLink     : TFieldDataLink;
    F_Before       : Boolean;
    F_ExportDir    : String;
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
    Property    DataSource       : TDataSource Read F_Get_DataSource Write F_Set_DataSource;
    Property    CallBefore       : Boolean     Read F_Before         Write F_Before;
    Property    DefaultExportDir : String      Read F_ExportDir      Write F_ExportDir;
  end;

procedure Register;

implementation
Uses
  KDaoTable, KADaoSASUnit;

Constructor TKADaoExportButton.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  Caption     := 'Export';
  F_ExportDir := '';
  F_DataLink  := TFieldDataLink.Create;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnActiveChange  := ActiveChange;
  F_Before    := True;
End;

Destructor TKADaoExportButton.Destroy;
Begin
  F_DataLink.Free;
  F_DataLink := nil;
  Inherited Destroy;
End;

Function TKADaoExportButton.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoExportButton.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (NOT DataSource.DataSet.ISEmpty)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
end;

Procedure TKADaoExportButton.ActiveChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (NOT DataSource.DataSet.ISEmpty)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoExportButton.DataChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (NOT DataSource.DataSet.ISEmpty)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoExportButton.Loaded;
Begin
  Inherited Loaded;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (NOT DataSource.DataSet.ISEmpty)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

procedure TKADaoExportButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
end;

Procedure TKADaoExportButton.HandleClickEvent;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (NOT DataSource.DataSet.ISEmpty)
 And (DataSource.DataSet.Active) Then
     Begin
      Application.CreateForm(TSAS, SAS);
      SAS.ExportDir:=F_ExportDir;
      if SAS.Execute(DataSource.DataSet) Then
          Begin
          End;
       SAS.Free;
     End;
End;

Procedure TKADaoExportButton.Click;
Begin
 if F_Before Then inherited Click;
 HandleClickEvent;
 if NOT F_Before Then inherited Click;
End;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoExportButton]);
end;

end.
