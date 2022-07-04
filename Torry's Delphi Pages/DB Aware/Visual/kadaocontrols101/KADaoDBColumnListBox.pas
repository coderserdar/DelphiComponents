unit KADaoDBColumnListBox;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, DB, DBCommon, DBCtrls, StdCtrls, Dialogs;

type
  TKADaoDBColumnListBox = class(TListBox)
  private
    { Private declarations }
    F_DataLink       : TFieldDataLink;
    F_MasterLink     : TFieldDataLink;
    F_InGetting      : Boolean;
    F_Unique         : Boolean;                                  
    F_AutoPost       : Boolean;
    F_ReadOnly       : Boolean;
    F_Bookmarks      : TStringList;
    F_GetData        : Boolean;

    Procedure          DataChange(Sender: TObject);
    Procedure          UpdateData(Sender: TObject);
    Procedure          EditingChange(Sender: TObject);
    Procedure          ActiveChange(Sender: TObject);

    Procedure          ML_DataChange(Sender: TObject);
    Procedure          ML_UpdateData(Sender: TObject);
    Procedure          ML_EditingChange(Sender: TObject);
    Procedure          ML_ActiveChange(Sender: TObject);

    Procedure          FillData;
  protected
    { Protected declarations }
    Function    F_Get_DataField: string;
    Procedure   F_Set_DataField(const Value: string);
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);
    Function    F_Get_MasterSource: TDataSource;
    Procedure   F_Set_MasterSource(Value: TDataSource);
    Procedure   F_Set_Unique(Value: Boolean);
    Procedure   F_Set_Bookmarks(Value : TStringList);
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure   Click; override;
    Procedure   Loaded; override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
    Procedure   Resync;
    Function    IndexOf(Bookmark:String):Integer;
    Property    Bookmarks        : TStringList  Read F_Bookmarks        Write F_Set_Bookmarks;
  published
    { Published declarations }
    Property    AutoPost          : Boolean     Read F_AutoPost         Write F_AutoPost;
    Property    DataField         : String      Read F_Get_DataField    Write F_Set_DataField;
    Property    DataSource        : TDataSource Read F_Get_DataSource   Write F_Set_DataSource;
    Property    MasterSource      : TDataSource Read F_Get_MasterSource Write F_Set_MasterSource;
    Property    ReadOnly          : Boolean     Read F_ReadOnly         Write F_ReadOnly;
    Property    UniqueDataOnly    : Boolean     Read F_Unique           Write F_Set_Unique;
  end;

procedure Register;

implementation

Constructor TKADaoDBColumnListBox.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  F_AutoPost                   := False;
  F_ReadOnly                   := True;
  F_InGetting                  := False;
  F_GetData                    := False;
  F_Unique                     := False;
  F_Bookmarks                  := TStringList.Create;
  F_MasterLink                 := TFieldDataLink.Create;
  F_MasterLink.Control         := Self;
  F_MasterLink.OnDataChange    := ML_DataChange;
  F_MasterLink.OnEditingChange := ML_EditingChange;
  F_MasterLink.OnUpdateData    := ML_UpdateData;
  F_MasterLink.OnActiveChange  := ML_ActiveChange;

  F_DataLink                 := TFieldDataLink.Create;
  F_DataLink.Control         := Self;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnEditingChange := EditingChange;
  F_DataLink.OnUpdateData    := UpdateData;
  F_DataLink.OnActiveChange  := ActiveChange;
End;

Destructor TKADaoDBColumnListBox.Destroy;
Begin
 F_Bookmarks.Free;
 F_MasterLink.Free;
 F_DataLink.Free;
 F_DataLink := nil;
 inherited Destroy;
End;

Function  TKADaoDBColumnListBox.IndexOf(Bookmark:String):Integer;
Var
  X : Integer;
Begin
  Result := -1;
  For X := 0 To F_Bookmarks.Count-1 do
      Begin
        if F_Bookmarks.Strings[X]=Bookmark Then
           Begin
             Result := X;
             Exit;
           End;
      End;
End;

procedure TKADaoDBColumnListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then
      Begin
        DataSource := nil;
      End;
  if (Operation = opRemove) and (F_MasterLink <> nil) and (AComponent = MasterSource) then
      Begin
        MasterSource := nil;
      End;
end;

Procedure TKADaoDBColumnListBox.Click;
Begin
  Inherited Click;
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (NOT F_InGetting)
  And (Assigned(F_DataLink.Field))
  And (NOT F_DataLink.Field.IsBlob)
  And (NOT F_UNIQUE)
  And (F_ReadOnly) Then
      Begin
       if ItemIndex > -1 Then F_DataLink.Dataset.Bookmark := F_BookMarks.Strings[ItemIndex];
      End;

  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (NOT F_InGetting)
  And (Assigned(F_DataLink.Field))
  And (NOT F_DataLink.Field.IsBlob)
  And (NOT F_ReadOnly)
  And (F_DataLink.CanModify)
  And (F_DataLink.Edit)  Then
      Begin
       F_DataLink.Modified;
       F_DataLink.UpdateRecord;
       if F_AutoPost Then F_DataLink.DataSet.Post;
      End;
End;


Procedure TKADaoDBColumnListBox.Loaded;
Begin
 Inherited Loaded;
 Items.Clear;
 F_Bookmarks.Clear;
End;

Procedure TKADaoDBColumnListBox.FillData;
Var
  BK : TBookmarkStr;
  S  : String;
Begin
  Items.BeginUpdate;
  Try
    Items.Clear;
    F_Bookmarks.Clear;
    If  (Assigned(F_DataLink.DataSource))
    And (F_DataLink.Active)
    And (F_DataLink.FieldName<> '')
    And (F_DataLink.DataSet.State=dsBrowse)
    And (NOT F_DataLink.DataSet.IsEmpty)
    And (NOT F_InGetting)
    And (Assigned(F_DataLink.Field))
    And (NOT F_DataLink.Field.IsBlob) Then
      Begin
        F_InGetting := True;
        BK := F_DataLink.DataSet.Bookmark;
        F_DataLink.DataSet.DisableControls;
        Try
         F_DataLink.DataSet.First;
         While NOT F_DataLink.DataSet.EOF do
           Begin
             S := F_DataLink.DataSet.FieldByName(F_DataLink.FieldName).AsString;
             if F_Unique Then
                Begin
                  if Items.IndexOf(S) = -1 Then
                     Begin
                       Items.Add(S);
                       F_Bookmarks.Add(F_DataLink.DataSet.Bookmark);
                     End;
                End
             Else
                Begin
                  Items.Add(S);
                  F_Bookmarks.Add(F_DataLink.DataSet.Bookmark);
                End;
             F_DataLink.DataSet.Next;
           End;
        Finally
          Try
            F_DataLink.DataSet.Bookmark := BK;
          Except
          End;  
          ItemIndex := IndexOf(BK);
          F_DataLink.DataSet.EnableControls;
          F_InGetting := False;
        End;
      End;
  Finally
    Items.EndUpdate;
  End;
End;

Procedure TKADaoDBColumnListBox.ActiveChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet.Active) Then
      Begin
        FillData;
        Enabled := True;
      End
  Else
      Begin
        Items.Clear;
        F_Bookmarks.Clear;
        Enabled := False;
      End;
End;

Procedure TKADaoDBColumnListBox.Resync;
Var
 S  : String;
Begin
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (NOT F_InGetting)
  And (NOT MultiSelect)
  And (Assigned(F_DataLink.Field))
  And (NOT F_DataLink.Field.IsBlob)
  And (NOT F_DataLink.DataSet.ControlsDisabled)
  And (F_DataLink.Dataset.State = dsBrowse) Then
      Begin
        if F_Unique Then
            Begin
             S := F_DataLink.DataSet.FieldByName(F_DataLink.FieldName).AsString;
             ItemIndex := Items.IndexOf(S)
            End
        Else
            Begin
             ItemIndex := IndexOf(F_DataLink.Dataset.Bookmark);
            End;
      End;     
End;

Procedure TKADaoDBColumnListBox.DataChange(Sender: TObject);
Begin
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (NOT F_InGetting)
  And (Assigned(F_DataLink.Field))
  And (NOT F_DataLink.Field.IsBlob)
  And (F_DataLink.Dataset.State = dsBrowse) Then
      Begin
       if F_GetData Then FillData;
       F_GetData := False;
       Resync;
      End;

  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (NOT F_InGetting)
  And (Assigned(F_DataLink.Field))
  And (NOT F_DataLink.Field.IsBlob)
  And (F_DataLink.Dataset.State <> dsBrowse) Then
      Begin
        F_GetData := True;
      End;
End;

Procedure TKADaoDBColumnListBox.UpdateData(Sender: TObject);
Var
  S : String;
Begin
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (NOT F_InGetting)
  And (Assigned(F_DataLink.Field))
  And (NOT F_DataLink.Field.IsBlob)
  And (F_DataLink.CanModify) Then
      Begin
       S := Items.Strings[ItemIndex];
       F_DataLink.Dataset.FieldByName(F_DataLink.FieldName).AsString := S;
      End;
End;

Procedure TKADaoDBColumnListBox.EditingChange(Sender: TObject);
Begin
  FillData;
End;

Function TKADaoDBColumnListBox.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoDBColumnListBox.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if csLoading in ComponentState then Exit;
  FillData;
end;

Procedure TKADaoDBColumnListBox.F_Set_Unique(Value: Boolean);
Begin
 F_Unique := Value;
 if CsLoading in ComponentState Then Exit;
 FillData;
End;

Procedure TKADaoDBColumnListBox.F_Set_Bookmarks(Value : TStringList);
Begin
 //*********************************** Read Only
End;

Function TKADaoDBColumnListBox.F_Get_DataField: string;
begin
  Result := F_DataLink.FieldName;
end;

Procedure TKADaoDBColumnListBox.F_Set_DataField(const Value: string);
begin
  F_DataLink.FieldName := Value;
  if csLoading in ComponentState then Exit;
  FillData;
end;

Function TKADaoDBColumnListBox.F_Get_MasterSource: TDataSource;
Begin
 Result := F_MasterLink.DataSource;
End;

Procedure TKADaoDBColumnListBox.F_Set_MasterSource(Value: TDataSource);
Begin
 F_MasterLink.DataSource := Value;
 if Value <> nil then Value.FreeNotification(Self);
 if csLoading in ComponentState then Exit;
 FillData;
End;

Procedure TKADaoDBColumnListBox.ML_DataChange(Sender: TObject);
Begin
  FillData;
End;

Procedure TKADaoDBColumnListBox.ML_UpdateData(Sender: TObject);
Begin
  FillData;
End;

Procedure TKADaoDBColumnListBox.ML_EditingChange(Sender: TObject);
Begin
  FillData;
End;

Procedure TKADaoDBColumnListBox.ML_ActiveChange(Sender: TObject);
Begin
  FillData;
End;


procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoDBColumnListBox]);
end;

end.
