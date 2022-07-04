unit KADaoSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, DB, DBCtrls, KDaoTable, DaoApi;

type
  TKADaoSearch = class(TPanel)
  private
    { Private declarations }
    F_SLabel            : TLabel;
    F_SCombo            : TComboBox;
    F_SEdit             : TEdit;
    F_SButton           : TButton;
    F_DataLink          : TFieldDataLink;
    Seek                : Boolean;
    F_IncrementalSearch : Boolean;
  protected
    { Protected declarations }
    Procedure   FillCombo;
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure   DataChange(Sender: TObject);
    Procedure   ActiveChange(Sender: TObject);
    procedure   Loaded; override;
    Procedure   OnSearchClick(Sender: TObject);
    Procedure   OnEditChange(Sender: TObject);
    Property    SearchEdit   : TEdit     Read F_SEdit   Write F_SEdit;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
    Property    SearchLabel  : TLabel    Read F_SLabel  Write F_SLabel;
    Property    SearchCombo  : TComboBox Read F_SCombo  Write F_SCombo;
    Property    SearchButton : TButton   Read F_SButton Write F_SButton;
  published
    { Published declarations }
    Property    IncrementalSearch  : Boolean     Read F_IncrementalSearch  Write F_IncrementalSearch;
    Property    DataSource         : TDataSource Read F_Get_DataSource     Write F_Set_DataSource;
  end;

procedure Register;

implementation
Const
  LeftStart = 10;
  TopStart  = 10;
  Spacing   = 5;

Constructor TKADaoSearch.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];
  BevelInner   := bvLowered;
  BevelInner   := bvLowered;
  BevelOuter   := bvRaised;

  F_SLabel     := TLabel.Create(Self);
  F_SCombo     := TComboBox.Create(Self);
  F_SEdit      := TEdit.Create(Self);
  F_SButton    := TButton.Create(Self);

  F_SLabel.Parent  := Self;
  F_SCombo.Parent  := Self;
  F_SEdit.Parent   := Self;
  F_SButton.Parent := Self;

  F_SLabel.Left    := LeftStart;
  F_SLabel.Top     := TopStart+((F_SCombo.Height-F_SLabel.Height) div 2);
  F_SLabel.Caption := 'Field :';

  F_SCombo.Left    := F_SLabel.Left+F_SLabel.Width+Spacing;
  F_SCombo.Top     := TopStart;
  F_SCombo.Style   := csDropDownList;

  F_SEdit.Left       := F_SCombo.Left+F_SCombo.Width+Spacing;
  F_SEdit.Top        := TopStart;
  F_SEdit.OnChange   := OnEditChange;

  F_SButton.Left      := F_SEdit.Left+F_SEdit.Width+Spacing;
  F_SButton.Top       := TopStart-((F_SButton.Height-F_SEdit.Height) div 2);
  F_SButton.Caption   := 'Search';
  F_SButton.Default   := True;
  F_SButton.OnClick   := OnSearchClick;

  Width               := F_SButton.Left+F_SButton.Width+Spacing+BevelWidth*6;
  Height              := F_SButton.Height+(TopStart)+(BevelWidth*6);

  F_DataLink                 := TFieldDataLink.Create;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnActiveChange  := ActiveChange;

  F_IncrementalSearch        := False;
End;

Destructor TKADaoSearch.Destroy;
Begin
  F_SLabel.Free;
  F_SCombo.Free;
  F_SEdit.Free;
  F_SButton.Free;
  F_DataLink.Free;
  Inherited Destroy;
End;

Procedure TKADaoSearch.FillCombo;
Var
  DaoTable : TKADaoTable;
  X        : Integer;
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
  F_SCombo.Items.Clear;
  if NOT Seek Then
    Begin
     For X := 0 to DaoTable.FieldCount-1 do
      Begin
       if (DaoTable.Fields[X].FieldKind=fkData) And (DaoTable.Fields[X].Visible)  Then
         Begin
          F_SCombo.Items.Add(DaoTable.Fields[X].FieldName);
         End;
      End;
      F_SCombo.ItemIndex := 0;
    End
 Else
    Begin
     For X := 0 to DaoTable.IndexFieldCount-1 do
      Begin
       if (DaoTable.IndexFields[X].FieldKind=fkData)  And (DaoTable.IndexFields[X].Visible) Then
         Begin
          F_SCombo.Items.Add(DaoTable.IndexFields[X].FieldName);
         End;
      End;
      F_SCombo.ItemIndex := 0;
      if F_SCombo.ItemIndex = -1 Then F_SCombo.ItemIndex:=0;
    End;
End;

procedure TKADaoSearch.OnSearchClick(Sender: TObject);
Var
 KeyFields : string;
 KeyValues : Variant;
 Options   : TLocateOptions;
 DaoTable  : TKADaoTable;
 OK        : Boolean;
 OAFI      : Boolean;
begin
 KeyFields := F_SCombo.Items.Strings[F_SCombo.ItemIndex];
 KeyValues := F_SEdit.Text;
 Options   := [loCaseInsensitive];
 DaoTable  := TKADaoTable(F_DataLink.Dataset);
 if NOT Seek Then
    Begin
      Try
        OK := DaoTable.Find_First(KeyFields, KeyValues, Options);
        if Not OK Then
           Begin
             Options := Options+[loPartialKey];
             DaoTable.Find_First(KeyFields, KeyValues, Options);
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
             DaoTable.Locate(KeyFields, KeyValues, Options);
           End;
      Except
      End;
      DaoTable.AutoFindIndex := OAFI;
      VarClear(KeyValues);
    End;
end;

procedure TKADaoSearch.OnEditChange(Sender: TObject);
Begin
 if F_IncrementalSearch Then F_SButton.Click;
End;

Function TKADaoSearch.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoSearch.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
        FillCombo;
      End
  Else
      Begin
        Enabled := False;
      End;
end;

Procedure TKADaoSearch.ActiveChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
        FillCombo;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoSearch.DataChange(Sender: TObject);
Begin
if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

Procedure TKADaoSearch.Loaded;
Begin
  Inherited Loaded;
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Enabled := True;
        FillCombo;
      End
  Else
      Begin
        Enabled := False;
      End;
End;

procedure TKADaoSearch.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
end;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoSearch]);
end;

end.
