unit o_GTDBLookUpEdit;

interface
uses
   Classes
  ,Controls
  ,DB
  ,DBCtrls
  ,DBGrids
  ,StdCtrls
  ,Buttons
  ,Messages
  ,Menus
  ,Graphics
  ;
{$I ../GTDIRECTIVES.inc}
type
{------------------------------------------------------------------------------}
  TgtDBLookUpEdit = class;
{------------------------------------------------------------------------------}
  TgtDBLookUpEdit = class(TCustomControl)
  private
    FDisplayField1: string;
    FDisplayField2: string;
    FListSource: TDataSource;
    FOnButtonClick: TNotifyEvent;
    FEditor1Width: Integer;
    FListKeyField: string;
    FAutoDropDown: Boolean;
    FDropDownHeight: Integer;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetListSource(const Value: TDataSource);
    function GetField: TField;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetDataField: string;
    function GetButtonPopUpMenu: TPopUpMenu;
    procedure SetButtonPopUpMenu(const Value: TPopUpMenu);
    procedure SetEditor1Width(const Value: Integer);
    function GetButtonGlyph: Graphics.TBitmap;
    procedure SetButtonGlyph(const Value: TBitmap);
    function GetButtonWidth: Integer;
    procedure SetButtonWidth(const Value: Integer);
    { Private declarations }
  protected
    FEdit1    : TEdit;
    FEdit2    : TEdit;
    FButton   : TSpeedButton;
    FDataLink : TFieldDataLink;
    Fld1      : TField;
    Fld2      : TField;
    FDBGrid   : TDBGrid;
  protected
    { Protected declarations }
    procedure Paint;override;
    procedure Notification(AComponent : TComponent ; Operation : TOperation);override;
    procedure EditorsKeyDown (Sender : TObject ; var Key: Word; Shift: TShiftState);
    procedure EditorsKeyPress(Sender : TObject ; var Key : Char);
    procedure EditorsOnChange(Sender : TObject);
    procedure EditorsOnExit  (Sender : TObject);
    procedure InternalOnDataChange  (Sender : TObject);
    procedure InternalOnActiveChange(Sender : TObject);
    procedure InternalOnButtonClick (Sender : TObject);
    procedure InternalOnGridKeyPress(Sender : TObject; var Key : Char);
    procedure InternalOnGridExit    (Sender : TObject);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;    
  protected
    procedure ShowGrid;
    procedure HideGrid;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Field : TField read GetField;
  published
    { Published declarations}
    property DataSource      : TDataSource     read GetDataSource        write SetDataSource;
    property DataField       : string          read GetDataField         write SetDataField;
    property DisplayField1   : string          read FDisplayField1       write FDisplayField1;
    property DisplayField2   : string          read FDisplayField2       write FDisplayField2;
    property ListSource      : TDataSource     read FListSource          write SetListSource;
    property ListKeyField    : string          read FListKeyField        write FListKeyField;
    property ReadOnly        : Boolean         read GetReadOnly          write SetReadOnly;
    property AutoDropDown    : Boolean         read FAutoDropDown        write FAutoDropDown;
    property ButtonPopUpMenu : TPopUpMenu      read GetButtonPopUpMenu   write SetButtonPopUpMenu;
    property ButtonGlyph     : TBitmap         read GetButtonGlyph       write SetButtonGlyph;
    property ButtonWidth     : Integer         read GetButtonWidth       write SetButtonWidth;
    property DrowDownHeight  : Integer         read FDropDownHeight      write FDropDownHeight;
  published
    property Editor1Width  : Integer       read FEditor1Width  write SetEditor1Width;
  published
    property OnButtonClick : TNotifyEvent  read FOnButtonClick write FOnButtonClick;
  published
    property Align;
    property PopUpMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFNDEF DELPHI6}
      property ParentBackground default True;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property Hint;
    property HelpContext;
    property HelpKeyWord;
    property HelpType;
    property Enabled;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Constraints;
    property Anchors;
  end;
{------------------------------------------------------------------------------}


implementation
uses
   SysUtils
  ,Types
  ,Forms
  ,Windows
  ,Variants
  ;


type
  _TControl = class(TControl);

{ TgtDBLookUpEdit }
{------------------------------------------------------------------------------}
constructor TgtDBLookUpEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height          := 21;
  Width           := 240;
  FEditor1Width   := 40;
  FDropDownHeight := 80;
  FAutoDropDown   := False;

  FDataLink                := TFieldDataLink.Create;
  FDataLink.OnDataChange   := InternalOnDataChange;
  FDataLink.OnActiveChange := InternalOnActiveChange;

  FButton          := TSpeedButton.Create(Self);
  FButton.Parent   := Self;
  FButton.Align    := alRight;
  FButton.OnClick  := InternalOnButtonClick;



  FEdit1           := TEdit.Create(Self);
  FEdit1.Tag       := 1;
  FEdit1.OnKeyPress:= EditorsKeyPress;
  FEdit1.OnChange  := EditorsOnChange;
  FEdit1.OnKeyDown := EditorsKeyDown;
  FEdit1.OnExit    := EditorsOnExit;
  FEdit1.Align     := alLeft;
  FEdit1.Width     := 40;
  FEdit1.Parent    := Self;



  FEdit2           := TEdit.Create(Self);
  FEdit2.Tag       := 2;
  FEdit2.OnKeyPress:= EditorsKeyPress;
  FEdit2.OnChange  := EditorsOnChange;
  FEdit2.OnKeyDown := EditorsKeyDown;
  FEdit2.OnExit    := EditorsOnExit;
  FEdit2.Align     := alClient;
  FEdit2.Parent    := Self;

  FDBGrid            := TDBGrid.Create(Self);
  FDBGrid.OnExit     := InternalOnGridExit;
  FDBGrid.OnKeyPress := InternalOnGridKeyPress;
  FDBGrid.Options    := FDBGrid.Options - [dgEditing,dgTitles];

end;
{------------------------------------------------------------------------------}
destructor TgtDBLookUpEdit.Destroy;
begin
  FDataLink.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FListSource then
      ListSource := nil;
  end;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then
  begin
    FEdit1.Enabled  := True;
    FEdit2.Enabled  := True;
    FButton.Enabled := True;
  end
  else
  begin
    FEdit1.Enabled  := False;
    FEdit2.Enabled  := False;
    FButton.Enabled := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.EditorsKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
  case Key of
    VK_DOWN :
      begin
        if FDBGrid.Visible then
          FDBGrid.SetFocus;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.EditorsKeyPress(Sender: TObject; var Key: Char);
begin
  if (Self.ReadOnly) or (not Assigned(FDataLink.DataSource)) or (not Assigned(FDataLink.DataSet)) or
  (not(FDataLink.DataSet.State in [dsInsert,dsEdit])) then
     Key := #0
  else
  begin
    if (FAutoDropDown) and (Key <> #27) then
      ShowGrid;
    case Key of
      #13 :
        begin
          FDBGrid.Visible := False;
          if (Length(Trim(FEdit1.Text)) = 0) or (Length(Trim(FEdit2.Text)) = 0) then
            Field.Value   := null
          else
            Field.Value     := FListSource.DataSet.FieldByName(FListKeyField).Value;
        end;
      #27 :
        begin
          if FDataLink.DataSet.State in [dsInsert,dsEdit] then
          begin
            HideGrid;
          end;
        end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.EditorsOnChange(Sender: TObject);
begin
  if Assigned(FDataLink.DataSet) then
  begin
    if FDataLink.DataSet.State in [dsInsert,dsEdit] then
    begin
      case TComponent(Sender).Tag of
        1 : //Editor1
          begin
            if FListSource.DataSet.Locate(FDisplayField1,FEdit1.Text,[]) then
              FEdit2.Text := Fld2.AsString
            {else
            begin
              FEdit2.Text := '';
            end;}
          end;
        2 : //Editor2
          begin
            if FListSource.DataSet.Locate(FDisplayField2,FEdit2.Text,[]) then
              FEdit1.Text := Fld1.AsString
            {else
            begin
              FEdit1.Text := '';
            end;}
          end;
      end;
    end
    else
    begin
      case TComponent(Sender).Tag of
        1 : //Editor1
          begin
            FEdit1.Text := Fld1.AsString
          end;
        2 : //Editor2
          begin
            FEdit2.Text := Fld2.AsString;
          end;
      end;
    end;
  end
  else
    TEdit(Sender).Text := '';
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.EditorsOnExit(Sender: TObject);
begin
  if Assigned(FDataLink.DataSet) and (FDataLink.DataSet.Active) then
  begin
    if (Length(Trim(FEdit1.Text)) = 0) or (Length(Trim(FEdit2.Text)) = 0) then
    begin
      if FDataLink.DataSet.State in [dsInsert,dsEdit] then
      begin
        Field.Value     := null;
        FEdit1.Text     := '';
        FEdit2.Text     := '';
      end;
    end
    else
    begin
      if FDataLink.DataSet.State in [dsInsert,dsEdit] then
        Field.Value     := FListSource.DataSet.FieldByName(FListKeyField).Value;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.Paint;
var
  R : TRect;
begin
  R := Self.GetClientRect;
  Self.Canvas.FillRect(R);
  if Assigned(Self.Parent) then
    Self.Color := _TControl(Self.Parent).Color;
  FEdit1.Width := Self.Editor1Width;
  if Assigned(FDataLink.DataSet) then
    if not (FDataLink.DataSet.State in [dsInsert,dsEdit]) then
      HideGrid;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.InternalOnDataChange(Sender: TObject);
begin
  FEdit1.Text := '';
  FEdit2.Text := '';
  if (not Assigned(Fld1)) or (not Assigned(Fld2)) then
  begin
    if Assigned(FListSource) then
      if Assigned(FListSource.DataSet) then
      begin
        Fld1 := FListSource.DataSet.FindField(FDisplayField1);
        Fld2 := FListSource.DataSet.FindField(FDisplayField2);
        if not  FListSource.DataSet.Active then
           FListSource.DataSet.Active := True;
        if FListSource.DataSet.Locate(FListKeyField,Field.AsInteger,[]) then
        begin
          if Assigned(Fld1) then
            FEdit1.Text := Fld1.AsString;
          if Assigned(Fld2) then
            FEdit2.Text := Fld2.AsString;
        end;
      end;
  end
  else
  begin
    if FListSource.DataSet.Locate(FListKeyField,Field.AsInteger,[]) then
    begin
      FEdit1.Text := Fld1.AsString;
      FEdit2.Text := Fld2.AsString;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.InternalOnActiveChange(Sender: TObject);
begin
  if Assigned(FListSource) then
    if Assigned(FListSource.DataSet) then
    begin
      Fld1 := FListSource.DataSet.FindField(FDisplayField1);
      Fld2 := FListSource.DataSet.FindField(FDisplayField2);
      if not  FListSource.DataSet.Active then
         FListSource.DataSet.Active := True;
      if FListSource.DataSet.Locate(FListKeyField,Field.AsInteger,[]) then
      begin
        if Assigned(Fld1) then
          FEdit1.Text := Fld1.AsString;
        if Assigned(Fld2) then
          FEdit2.Text := Fld2.AsString;
      end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.InternalOnButtonClick(Sender: TObject);
begin
  FDBGrid.Visible := False;
  if Assigned(FOnButtonClick) then
    FOnButtonClick(FButton);
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.ShowGrid;
var
  Cln : TColumn;
begin
  FDBGrid.DataSource := ListSource;
  FDBGrid.Columns.Clear;
  Cln := FDBGrid.Columns.Add;
  Cln.FieldName  := Self.FDisplayField1;
  Cln.Title.Caption := '';
  Cln.Width      := FEdit1.Width;
  Cln.ReadOnly   := True;
  Cln            := FDBGrid.Columns.Add;
  Cln.FieldName  := Self.FDisplayField2;
  Cln.Title.Caption := '';
  Cln.Width      := FEdit2.Width - 20;
  Cln.ReadOnly   := True;
  FDBGrid.Parent := Self.Parent;
  FDBGrid.Left   := Self.Left;
  FDBGrid.Width  := Self.Width;
  FDBGrid.Top    := Self.Top + Self.Height + 5;
  FDBGrid.Height := FDropDownHeight;
  FDBGrid.Visible := True;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.HideGrid;
begin
  FDBGrid.Visible := False;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.InternalOnGridKeyPress(Sender: TObject;var Key : Char);
begin
  if not (FDataLink.DataSet.State in [dsInsert,dsEdit]) then
    HideGrid;
  if Key = #13 then
  begin
    Field.Value     := FListSource.DataSet.FieldByName(FListKeyField).Value;
    HideGrid;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.InternalOnGridExit(Sender: TObject);
begin
  HideGrid;
end;
{------------------------------------------------------------------------------}







{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.SetListSource(const Value: TDataSource);
begin
  if Assigned(FListSource) then
    FListSource.RemoveFreeNotification(Self);

  FListSource := Value;

  if Assigned(FListSource) then
    FListSource.FreeNotification(Self);
end;
{------------------------------------------------------------------------------}
function TgtDBLookUpEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{------------------------------------------------------------------------------}
function TgtDBLookUpEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{------------------------------------------------------------------------------}
function TgtDBLookUpEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{------------------------------------------------------------------------------}
function TgtDBLookUpEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;
{------------------------------------------------------------------------------}
function TgtDBLookUpEdit.GetButtonPopUpMenu: TPopUpMenu;
begin
  Result := FButton.PopupMenu;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.SetButtonPopUpMenu(const Value: TPopUpMenu);
begin
  FButton.PopupMenu := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.SetEditor1Width(const Value: Integer);
begin
  FEditor1Width := Value;
  FEdit1.Width  := FEditor1Width;
end;
{------------------------------------------------------------------------------}
function TgtDBLookUpEdit.GetButtonGlyph: Graphics.TBitmap;
begin
  Result := FButton.Glyph;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.SetButtonGlyph(const Value: Graphics.TBitmap);
begin
  FButton.Glyph := Value;
end;
{------------------------------------------------------------------------------}
function TgtDBLookUpEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpEdit.SetButtonWidth(const Value: Integer);
begin
  FButton.Width := Value;
end;
{------------------------------------------------------------------------------}












































end.
