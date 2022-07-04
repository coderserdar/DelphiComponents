unit KADaoDBStringList;

interface

uses
  Windows, Messages, SysUtils, Classes, DB, DBCtrls, Controls;

type
  TKADaoDBStringList = class(TComponent)
  private
    { Private declarations }
    F_DataLink       : TFieldDataLink;
    F_AutoPost       : Boolean;
    F_ReadOnly       : Boolean;
    F_InChange       : Boolean;
    F_List           : TStringList;
    Procedure          DataChange(Sender: TObject);
    Procedure          UpdateData(Sender: TObject);
    Procedure          EditingChange(Sender: TObject);
    Procedure          ActiveChange(Sender: TObject);
    Procedure          ListChanged(Sender: TObject);
  protected
    { Protected declarations }
    Function    F_Get_DataField: string;
    Procedure   F_Set_DataField(const Value: string);
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);
    Procedure   F_Set_List(Value: TStringList);

    Function    BlobToString(Field:TField):String;
    Procedure   StringToBlob(Field:TField;S:String);
    Procedure   Loaded; override;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
  published
    { Published declarations }
    Property    AutoPost          : Boolean     Read F_AutoPost       Write F_AutoPost;
    Property    DataField         : String      Read F_Get_DataField  Write F_Set_DataField;
    Property    DataSource        : TDataSource Read F_Get_DataSource Write F_Set_DataSource;
    Property    DBStringList      : TStringList Read F_List           Write F_Set_List;
    Property    ReadOnly          : Boolean     Read F_ReadOnly       Write F_ReadOnly;
  end;

procedure Register;

implementation

Constructor TKADaoDBStringList.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  F_AutoPost                 := False;
  F_ReadOnly                 := False;
  F_InChange                 := False;
  F_DataLink                 := TFieldDataLink.Create;
  F_List                     := TStringList.Create;
  F_List.OnChange            := ListChanged;
  F_DataLink.Control         := Self;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnEditingChange := EditingChange;
  F_DataLink.OnUpdateData    := UpdateData;
  F_DataLink.OnActiveChange  := ActiveChange;
End;

Destructor TKADaoDBStringList.Destroy;
Begin
 F_List.Free;
 F_DataLink.Free;
 F_DataLink := nil;
 inherited Destroy;
End;

procedure TKADaoDBStringList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then
      Begin
        DataSource := nil;
      End;
end;

Procedure TKADaoDBStringList.Loaded;
Begin
 Inherited Loaded;
 F_List.Clear;
End;

Function TKADaoDBStringList.BlobToString(Field:TField):String;
Var
  MS : TMemoryStream;
Begin
  Result := '';
  MS := TMemoryStream.Create;
  Try
    (Field AS TBlobField).SaveToStream(MS);
    MS.Position := 0;
    SetString(Result,PChar(MS.Memory),MS.Size);
  Finally
    MS.Free;
  End;
End;

Procedure TKADaoDBStringList.StringToBlob(Field:TField;S:String);
Var
  MS : TMemoryStream;
  P  : PChar;
Begin
  MS := TMemoryStream.Create;
  Try
    P:=PChar(S);
    MS.Position:=0;
    MS.Write(P^,Length(S));
    MS.Position:=0;
    (F_DataLink.Field As TBlobField).LoadFromStream(MS);
  Finally
    MS.Free;
  End;
End;

Procedure TKADaoDBStringList.ListChanged(Sender: TObject);
Begin
 if  F_InChange Then Exit;
 If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (Assigned(F_DataLink.Field))
  And (F_DataLink.Field.IsBlob)
  And (NOT F_ReadOnly)
  And (F_DataLink.CanModify)
  And (F_DataLink.Edit)  Then
      Begin
       F_DataLink.Modified;
       F_DataLink.UpdateRecord;
       if F_AutoPost Then F_DataLink.DataSet.Post;
      End;
End;


Procedure TKADaoDBStringList.ActiveChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet.Active) Then
      Begin

      End
  Else
      Begin
        F_List.Clear;
      End;
End;

Procedure TKADaoDBStringList.DataChange(Sender: TObject);
Var
 MS : TMemoryStream;
Begin
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (Assigned(F_DataLink.Field))
  And (F_DataLink.Field.IsBlob)
  And (F_DataLink.Dataset.State = dsBrowse) Then
      Begin
        F_InChange := True;
        F_List.Clear;
        MS := TMemoryStream.Create;
        Try
         MS.Position:=0;
         (F_DataLink.Field AS TBlobField).SaveToStream(MS);
         MS.Position:=0;
         F_List.LoadFromStream(MS);
        Finally
         MS.Free;
         F_InChange := False;
        End;
      End;
End;

Procedure TKADaoDBStringList.UpdateData(Sender: TObject);
Var
  MS : TMemoryStream;
Begin
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (Assigned(F_DataLink.Field))
  And (F_DataLink.Field.IsBlob)
  And (F_DataLink.CanModify) Then
      Begin
        MS := TMemoryStream.Create;
        Try
         MS.Position:=0;
         F_List.SaveToStream(MS);
         MS.Position:=0;
         (F_DataLink.Field AS TBlobField).LoadFromStream(MS);
        Finally
         MS.Free;
        End;
      End;
End;

Procedure TKADaoDBStringList.EditingChange(Sender: TObject);
Begin

End;

Procedure TKADaoDBStringList.F_Set_List(Value: TStringList);
Begin
  F_List.BeginUpdate;
  F_List.Assign(Value);
  F_List.EndUpdate;
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (Assigned(F_DataLink.Field))
  And (F_DataLink.Field.IsBlob)
  And (NOT F_ReadOnly)
  And (F_DataLink.CanModify)
  And (F_DataLink.Edit)  Then
      Begin
       F_DataLink.Modified;
       F_DataLink.UpdateRecord;
       if F_AutoPost Then F_DataLink.DataSet.Post;
      End;
End;

Function TKADaoDBStringList.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoDBStringList.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

Function TKADaoDBStringList.F_Get_DataField: string;
begin
  Result := F_DataLink.FieldName;
end;

Procedure TKADaoDBStringList.F_Set_DataField(const Value: string);
begin
  F_DataLink.FieldName := Value;
end;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoDBStringList]);
end;

end.
