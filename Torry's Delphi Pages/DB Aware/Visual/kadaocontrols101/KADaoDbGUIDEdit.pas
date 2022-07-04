unit KADaoDbGUIDEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, DB, DBCtrls, Controls,
  StdCtrls, KDaoTable, Dialogs;

type
  TKADaoDbGUIDEdit = class(TEdit)
  private
    { Private declarations }
    F_DataLink       : TFieldDataLink;
    F_AutoPost       : Boolean;
    InSetting        : Boolean;
    Procedure          DataChange(Sender: TObject);
    Procedure          UpdateData(Sender: TObject);
    Procedure          EditingChange(Sender: TObject);
    Procedure          ActiveChange(Sender: TObject);
  protected
    { Protected declarations }
    Function    F_Get_DataField: string;
    Procedure   F_Set_DataField(const Value: string);
    Function    F_Get_DataSource: TDataSource;
    Procedure   F_Set_DataSource(Value: TDataSource);

    Procedure   Change; override;
    Procedure   Loaded; override;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    Function    CheckValidGUIDChars(S:String) : Boolean;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
    Function    CheckValidGUID(S:String) : Boolean;
  published
    { Published declarations }
    Property    AutoPost          : Boolean     Read F_AutoPost       Write F_AutoPost;
    Property    DataField         : String      Read F_Get_DataField  Write F_Set_DataField;
    Property    DataSource        : TDataSource Read F_Get_DataSource Write F_Set_DataSource;
  end;

procedure Register;

implementation

Function TKADaoDbGUIDEdit.CheckValidGUIDChars(S:String) : Boolean;
Var
  L : Integer;
  X : Integer;
Begin
  Result := False;
  L      := Length(S);
  if L=0 Then Exit;
  For X := 1 To L Do
     Begin
       if NOT (S[X] In GUID_VALID_CHARS) Then Exit;
     End;
  Result := True;
End;

Function TKADaoDbGUIDEdit.CheckValidGUID(S:String) : Boolean;
Var
  L : Integer;
Begin
 Result := True;
 L      := Length(S);
 if L=0 Then Exit;
 Result := False;
 if L <> 38 Then Exit;
 if S[1] <> '{' Then Exit;
 if S[L] <> '}' Then Exit;
 if S[10] <> '-' Then Exit;
 if S[15] <> '-' Then Exit;
 if S[20] <> '-' Then Exit;
 if S[25] <> '-' Then Exit;
 Result := CheckValidGUIDChars(S);
End;

Constructor TKADaoDbGUIDEdit.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  F_AutoPost                 := False;
  InSetting                  := False;
  F_DataLink                 := TFieldDataLink.Create;
  F_DataLink.Control         := Self;
  F_DataLink.OnDataChange    := DataChange;
  F_DataLink.OnEditingChange := EditingChange;
  F_DataLink.OnUpdateData    := UpdateData;
  F_DataLink.OnActiveChange  := ActiveChange;
  Width                      := Inherited Width*2;    
End;

Destructor TKADaoDbGUIDEdit.Destroy;
Begin
 F_DataLink.Free;
 F_DataLink := nil;
 inherited Destroy;
End;

procedure TKADaoDbGUIDEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (F_DataLink <> nil) and (AComponent = DataSource) then
      Begin
        DataSource := nil;
      End;
end;

Procedure TKADaoDbGUIDEdit.Loaded;
Begin
  Inherited Loaded;
 //*****************************************************************************
End;


Procedure TKADaoDbGUIDEdit.ActiveChange(Sender: TObject);
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet.Active) Then
      Begin

      End
  Else
      Begin
        Text := '';
      End;
End;

Procedure TKADaoDbGUIDEdit.DataChange(Sender: TObject);
Var
  SGUID : String;
Begin
  ReadOnly := NOT F_DataLink.CanModify;
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (Assigned(F_DataLink.Field))
  And (F_DataLink.Field.DataType=ftBytes)
  And (F_DataLink.Field.ValidChars = GUID_VALID_CHARS)
  And (F_DataLink.DataSet is TKAdaoTable)
  And (F_DataLink.Dataset.State = dsBrowse) Then
      Begin
        SGUID      := F_DataLink.Field.AsString;
        InSetting  := True;
        Try
          if F_DataLink.Field.IsNull Then
             Text := ''
          Else
             Text  := TKAdaoTable(F_DataLink.DataSet).GetGUIDAsString(SGUID);
        Finally
          InSetting := False;
        End;
      End;
End;

Procedure TKADaoDbGUIDEdit.UpdateData(Sender: TObject);
Var
  SGUID : String;
Begin
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (Assigned(F_DataLink.Field))
  And (F_DataLink.Field.DataType=ftBytes)
  And (F_DataLink.Field.ValidChars = GUID_VALID_CHARS)
  And (F_DataLink.DataSet is TKAdaoTable)
  And (F_DataLink.CanModify) Then                                
      Begin
        if (Length(Text) = 38) Or (Length(Text) = 0) Then
           Begin
            SGUID := '';
            if (Length(Text) = 38) Then SGUID := TKAdaoTable(F_DataLink.DataSet).PutGUIDInString(Text);
            F_DataLink.Field.AsString := SGUID;
           End;
      End;
End;

Procedure TKADaoDbGUIDEdit.Change;
Begin
  If  NOT (CheckValidGUID(Text)) Then Exit;
  If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (Assigned(F_DataLink.Field))
  And (F_DataLink.Field.DataType=ftBytes)
  And (F_DataLink.Field.ValidChars = GUID_VALID_CHARS)
  And (F_DataLink.DataSet is TKAdaoTable)
  And (NOT ReadOnly)
  And (NOT InSetting)
  And (F_DataLink.CanModify)
  And (F_DataLink.Edit) Then
      Begin
       F_DataLink.Modified;
       F_DataLink.UpdateRecord;
       if F_AutoPost Then F_DataLink.DataSet.Post;
      End;
End;

Procedure TKADaoDbGUIDEdit.EditingChange(Sender: TObject);
Begin
 If  (Assigned(F_DataLink.DataSource))
  And (F_DataLink.Active)
  And (F_DataLink.FieldName<> '')
  And (Assigned(F_DataLink.Field))
  And (F_DataLink.Field.DataType=ftBytes)
  And (F_DataLink.Field.ValidChars = GUID_VALID_CHARS)
  And (F_DataLink.DataSet is TKAdaoTable)
  And (F_DataLink.DataSet.State = dsInsert) Then Text := '';
End;

Function TKADaoDbGUIDEdit.F_Get_DataSource: TDataSource;
begin
  Result := F_DataLink.DataSource;
end;

Procedure TKADaoDbGUIDEdit.F_Set_DataSource(Value: TDataSource);
begin
  F_DataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

Function TKADaoDbGUIDEdit.F_Get_DataField: string;
begin
  Result := F_DataLink.FieldName;
end;

Procedure TKADaoDbGUIDEdit.F_Set_DataField(const Value: string);
begin
  F_DataLink.FieldName := Value;
end;

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoDbGUIDEdit]);
end;

end.
