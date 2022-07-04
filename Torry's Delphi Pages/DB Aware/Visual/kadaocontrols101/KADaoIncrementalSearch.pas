unit KADaoIncrementalSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, DB, DBCtrls, KDaoTable, DaoApi, Dialogs;

type
  TKADaoIncrementalSearch = class(TEdit)
  private
    { Private declarations }
    F_DataLink          : TFieldDataLink;
    F_AutoComplete      : Boolean;
    F_EnteredText       : String;
    Seek                : Boolean;
    Skip                : Boolean;
  protected
    { Protected declarations }
    Function    F_Get_DataField: string;
    Procedure   F_Set_DataField(const Value: string);
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure   Check;
    Procedure   DataChange(Sender: TObject);
    Procedure   ActiveChange(Sender: TObject);
    procedure   Loaded; override;
    Procedure   Change; override;
    Procedure   KeyPress(var Key: Char);override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
  published
    { Published declarations }
    Property    DataField          : String      Read F_Get_DataField      Write F_Set_DataField;
    Property    DataSource         : TDataSource Read F_Get_DataSource     Write F_Set_DataSource;
    Property    AutoComplete       : Boolean     Read F_AutoComplete       Write F_AutoComplete;
  end;

procedure Register;

implementation
Const
  LeftStart = 10;
  TopStart  = 10;
  Spacing   = 5;

Constructor TKADaoIncrementalSearch.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  F_DataLink                 := TFieldDataLink.Create;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnActiveChange  := ActiveChange;
  F_AutoComplete             := False;
  F_EnteredText              := '';
  Skip                       := False;
End;

Destructor TKADaoIncrementalSearch.Destroy;
Begin
  F_DataLink.Free;
  Inherited Destroy;
End;

Procedure TKADaoIncrementalSearch.Check;
Var
  DaoTable : TKADaoTable;
Begin
  DaoTable := TKADaoTable(F_DataLink.Dataset);
  Seek := False;
  if (DaoTable.TableType=dbOpenTable) Then
     Begin
       if DaoTable.IndexDefs.Count=0 Then
          Begin
            Enabled := False;
            Exit;
          End;
       Seek := True;
     End;
End;

procedure TKADaoIncrementalSearch.Change;
Var
 KeyFields : String;
 KeyValues : Variant;
 Options   : TLocateOptions;
 DaoTable  : TKADaoTable;
 OK        : Boolean;
 OAFI      : Boolean;
 CP        : Integer;
begin
 if Skip Then
    Begin
     Skip := False;
     CP := GetSelStart+GetSelLength;
     F_EnteredText := Copy(Text,1,CP);
     Exit;
    End;
 KeyFields     := F_DataLink.FieldName;
 if F_AutoComplete Then
    Begin
      CP := GetSelStart+GetSelLength;
      F_EnteredText := Copy(Text,1,CP);
    End
 Else
    Begin
      F_EnteredText := Text;
    End;
 KeyValues     := F_EnteredText;
 Options       := [loCaseInsensitive];
 DaoTable      := TKADaoTable(F_DataLink.Dataset);
 OK            := False;
 if NOT Seek Then
    Begin
      Try
        OK := DaoTable.Find_First(KeyFields, KeyValues, Options);
        if Not OK Then
           Begin
             Options := Options+[loPartialKey];
             OK := DaoTable.Find_First(KeyFields, KeyValues, Options);
           End;
      Except
      End;
      VarClear(KeyValues);
    End
 Else
    Begin
      OAFI := DaoTable.AutoFindIndex;
      DaoTable.AutoFindIndex := True;
      Try
        OK := DaoTable.Locate(KeyFields, KeyValues, Options);
        if Not OK Then
           Begin
             Options := Options+[loPartialKey];
             OK := DaoTable.Locate(KeyFields, KeyValues, Options);
           End;
      Except
      End;
      DaoTable.AutoFindIndex := OAFI;
      VarClear(KeyValues);
    End;
 if (OK) And (F_AutoComplete) Then
    Begin
      Skip := True;
      CP := GetSelStart+GetSelLength;
      Self.Text := F_DataLink.DataSet.FieldByName(KeyFields).AsString;
      Self.SetSelStart(CP);
      Skip := False;
    End;
End;

Procedure TKADaoIncrementalSearch.KeyPress(var Key: Char);
Var
 KeyFields : string;
 KeyValues : Variant;
 Options   : TLocateOptions;
 DaoTable  : TKADaoTable;
 OK        : Boolean;
 CP        : Integer;
Begin
 Inherited KeyPress(Key);
 if Key <> #13 Then Exit;
 KeyValues     := F_EnteredText;
 KeyFields     := F_DataLink.FieldName;
 if F_AutoComplete Then
    Begin
      CP := GetSelStart+GetSelLength;
      F_EnteredText := Copy(Text,1,CP);
    End
 Else
    Begin
      F_EnteredText := Text;
    End;
 Options       := [loCaseInsensitive];
 DaoTable      := TKADaoTable(F_DataLink.Dataset);
 if NOT Seek Then
    Begin
      Try
        OK := DaoTable.Find_Next(KeyFields, KeyValues, Options);
        if Not OK Then
           Begin
             Options := Options+[loPartialKey];
             DaoTable.Find_Next(KeyFields, KeyValues, Options);
           End;
      Except
      End;
      VarClear(KeyValues);
    End;
End;

Function TKADaoIncrementalSearch.F_Get_DataField: string;
begin
  Result := F_DataLink.FieldName;
end;

Procedure TKADaoIncrementalSearch.F_Set_DataField(const Value: string);
begin
  F_DataLink.FieldName := Value;
end;

Function TKADaoIncrementalSearch.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoIncrementalSearch.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
        Check;
      End
  Else
      Begin
        Enabled := False;
      End;
end;

Procedure TKADaoIncrementalSearch.ActiveChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
        Check;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoIncrementalSearch.DataChange(Sender: TObject);
Begin
if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
        Check;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoIncrementalSearch.Loaded;
Begin
  Inherited Loaded;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
        Check;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

procedure TKADaoIncrementalSearch.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
end;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoIncrementalSearch]);
end;

end.
