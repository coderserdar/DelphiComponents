{ Copyright (C) 1998-2008, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  In this unit I included two components (a-la DBCombobox) which allow:
    TSMDBComboBox - additionally to standard Items property you can use a new Values property.
                    So you can display strings from Values but to store strings from Items.
                    For example, to display AE instead America Express, MS instead Master Card etc

    TSMDBFilterComboBox - this is an extention with possibility to display
                    a custom top item which is not stored in dataset
                    (additionally to records from dataset).
                    The default value is "All records" but you can change it.

                    Personally I use this component for visual data filtering
                    by existing values (it's a reason of name for this component)
                    When data will be changed, the values will be refreshed

    PS must add in the future:
      - when "additional custom rows" are added, I must add spces for max.length
        which can be displayed in combobox
      - after every Post for dataset I must check - the visible fields are changed or not
      - after every Post for dataset now I rebuild a list of combobox but I must
        refresh a changed record only (how can I find it? to store a key fields?
        but the key field could be change too...
}
unit SMDBComb;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, DBCtrls, DB;

{$IFDEF VER100}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

type
  TSMDBComboBox = class(TCustomComboBox)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FPaintControl: TPaintControl;
    FValues: TStrings;
    FEnableValues: Boolean;

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    procedure SetComboItems(Value: TStrings);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    function GetComboText: string; virtual;
    procedure SetComboText(const Value: string); virtual;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure SetEnableValues(Value: Boolean);
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
  protected
    { Protected declarations }
    procedure Change; override;
    procedure Click; override;

    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
      ComboProc: Pointer); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetStyle(Value: TComboBoxStyle); {$IFDEF WIN32} override {$ELSE} virtual {$ENDIF};
    procedure WndProc(var Message: TMessage); override;
    property ComboText: string read GetComboText write SetComboText;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
    property Text;
  published
    { Published declarations }
    property Style; {Must be published before Items}
    property Color;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property EnableValues: Boolean read FEnableValues write SetEnableValues;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items write SetComboItems;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Values: TStrings read FValues write SetValues;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
  end;

type
  TSMDBFilterComboBox = class;

  TSMFilterDataLink = class(TDataLink)
  private
    FFilterControl: TSMDBFilterComboBox;
  protected
    { Protected declarations }
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    { Public declarations }
    constructor Create(AControl: TSMDBFilterComboBox);
  end;

  TSMDBFilterComboBox = class(TCustomComboBox)
  private
    FFreeItem: string;
    FFreeValue: string;
    FNeedRebuild: Boolean;

    FDataLink: TSMFilterDataLink;
    FFieldDisplay: string;
    FFieldValue: string;

    FOnBuildEnter: TNotifyEvent;
    FOnBuildExit: TNotifyEvent;
    function GetDataSource: TDataSource;
    procedure DataLinkActiveChanged;
    procedure SetDataSource(Value: TDataSource);
    procedure SetFieldDisplay(const Value: string);
    procedure SetFieldValue(const Value: string);

    procedure SetFreeItem(Value: string);
    procedure SetFreeValue(Value: string);

    function GetMaxLen: Integer;
    function ReplicateChar(chFill: Char; intLen: Integer): string;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CenterStr(strSource: string; chFill: Char; intLen: Integer): string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildItems;
  published
    { Published declarations }
    property Style; {Must be published before Items}
    property Color;
    property Ctl3D;
    property OnBuildEnter: TNotifyEvent read FOnBuildEnter write FOnBuildEnter;
    property OnBuildExit: TNotifyEvent read FOnBuildExit write FOnBuildExit;
    property FreeItem: string read FFreeItem write SetFreeItem;
    property FreeValue: string read FFreeValue write SetFreeValue;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property FieldDisplay: string read FFieldDisplay write SetFieldDisplay;
    property FieldValue: string read FFieldValue write SetFieldValue;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
  end;

procedure Register;

implementation
uses SysUtils,
     {$IFDEF SMForDelphi6} VDBConsts {$ELSE} DBConsts {$ENDIF};

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBComboBox, TSMDBFilterComboBox]);
end;

{ TSMDBComboBox }
constructor TSMDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  FPaintControl := TPaintControl.Create(Self, 'COMBOBOX');

  FValues := TStringList.Create;
  TStringList(FValues).OnChange := ValuesChanged;
  EnableValues := False;
end;

destructor TSMDBComboBox.Destroy;
begin
  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;

  TStringList(FValues).OnChange := nil;
  FValues.Free;

  inherited Destroy;
end;

procedure TSMDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and
     (FDataLink <> nil) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TSMDBComboBox.CreateWnd;
begin
  inherited CreateWnd;

  SetEditReadOnly;
end;

procedure TSMDBComboBox.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    SetComboText(FDataLink.Field.Text)
  else
    if csDesigning in ComponentState then
      SetComboText(Name)
    else
      SetComboText('');
end;

procedure TSMDBComboBox.UpdateData(Sender: TObject);
var
  s: string;
begin
  if FDataLink.Field <> nil then
  begin
    s := ComboText;
    FDataLink.Field.Text := s;
  end;
end;

procedure TSMDBComboBox.SetComboText(const Value: string);
var i: Integer;
    Redraw: Boolean;
begin
  if Value <> ComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then
          i := -1
        else
          if FEnableValues then
            i := Values.IndexOf(Value)
          else
            i := Items.IndexOf(Value);
        if i >= Items.Count then
          i := -1;
        ItemIndex := i;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if i >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then
      Text := Value;
  end;
end;

function TSMDBComboBox.GetComboText: string;
var
  i: Integer;
begin
  if (Style in [csDropDown, csSimple]) and
     (not FEnableValues) then
    Result := Text
  else
  begin
    i := ItemIndex;
    if (i < 0) or (FEnableValues and (FValues.Count < i + 1)) then
      Result := ''
    else
      if FEnableValues then
        Result := FValues[i]
      else
        Result := Items[i];
  end;
end;

procedure TSMDBComboBox.Change;
begin
  FDataLink.Edit;
  inherited Change;
  FDataLink.Modified;
end;

procedure TSMDBComboBox.Click;
begin
  FDataLink.Edit;
  inherited Click;
  FDataLink.Modified;
end;

function TSMDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TSMDBComboBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TSMDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TSMDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TSMDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TSMDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TSMDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TSMDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then
    if not FDataLink.Edit and (Key in [VK_UP, VK_DOWN]) then
      Key := 0;
end;

procedure TSMDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

  if {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}[#32..#255]) and
     (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255: FDataLink.Edit;
    #27: begin
           FDataLink.Reset;
           SelectAll;
           Key := #0;
         end;
  end;
end;

procedure TSMDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

procedure TSMDBComboBox.SetEditReadOnly;
begin
  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.Editing), 0);
end;

procedure TSMDBComboBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_COMMAND: if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
                    if not FDataLink.Edit then
                    begin
                      if Style <> csSimple then
                        PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
                      Exit;
                    end;
      CB_SHOWDROPDOWN: if Message.WParam <> 0 then
                         FDataLink.Edit
                       else
                         if not FDataLink.Editing then
                           DataChange(Self); {Restore text}
      WM_CREATE,
      WM_WINDOWPOSCHANGED,
      CM_FONTCHANGED: FPaintControl.DestroyHandle;
    end;

  inherited WndProc(Message);
end;

procedure TSMDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN: if (Style = csSimple) and (ComboWnd <> EditHandle) then
                        if not FDataLink.Edit then Exit;
    end;
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
end;

procedure TSMDBComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TSMDBComboBox.CMGetDatalink(var Message: TMessage);
begin
  Message.Result := Longint(FDataLink);
end;

procedure TSMDBComboBox.WMPaint(var Message: TWMPaint);
var
  S: string;
  R: TRect;
  P: TPoint;
  Child: HWND;
begin
  if csPaintCopy in ControlState then
  begin
    if FDataLink.Field <> nil then
      S := FDataLink.Field.Text
    else
      S := '';

    if Style = csDropDown then
    begin
      SendMessage(FPaintControl.Handle, WM_SETTEXT, 0, Longint(PChar(S)));
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
      Child := GetWindow(FPaintControl.Handle, GW_CHILD);
      if Child <> 0 then
      begin
        Windows.GetClientRect(Child, R);
        Windows.MapWindowPoints(Child, FPaintControl.Handle, R.TopLeft, 2);
        GetWindowOrgEx(Message.DC, P);
        SetWindowOrgEx(Message.DC, P.X - R.Left, P.Y - R.Top, nil);
        IntersectClipRect(Message.DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
        SendMessage(Child, WM_PAINT, Message.DC, 0);
      end;
    end
    else
    begin
      SendMessage(FPaintControl.Handle, CB_RESETCONTENT, 0, 0);
      if Items.IndexOf(S) <> -1 then
      begin
        SendMessage(FPaintControl.Handle, CB_ADDSTRING, 0, Longint(PChar(S)));
        SendMessage(FPaintControl.Handle, CB_SETCURSEL, 0, 0);
      end;
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
    end;
  end
  else
    inherited;
end;

procedure TSMDBComboBox.SetComboItems(Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

procedure TSMDBComboBox.SetStyle(Value: TComboBoxStyle);
begin
  if (Value in [csSimple, csDropDown]) and FEnableValues then
    Value := csDropDownList;

  if (Value = csSimple) and
     Assigned(FDatalink) and
     FDatalink.DatasourceFixed then
    DataBaseError(SNotReplicatable);

  inherited SetStyle(Value);
end;

procedure TSMDBComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then
    DataChange(Self);
end;

procedure TSMDBComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
  begin
    if Value and (Style in [csDropDown, csSimple]) then
      Style := csDropDownList;
    FEnableValues := Value;
    DataChange(Self);
  end;
end;

procedure TSMDBComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;


{ TSMFilterDataLink }
constructor TSMFilterDataLink.Create(AControl: TSMDBFilterComboBox);
begin
  inherited Create;
  FFilterControl := AControl;
end;

procedure TSMFilterDataLink.ActiveChanged;
begin
  inherited;

  if FFilterControl <> nil then
    FFilterControl.DataLinkActiveChanged;
end;

procedure TSMFilterDataLink.LayoutChanged;
begin
  if FFilterControl <> nil then
    FFilterControl.DataLinkActiveChanged;
end;

procedure TSMFilterDataLink.RecordChanged(Field: TField);
begin
  if (FFilterControl <> nil) and
     (Field <> nil) and
     ((Pos(Field.FieldName, FFilterControl.FieldDisplay) > 0) or
      (Pos(Field.FieldName, FFilterControl.FieldValue) > 0)) then
    FFilterControl.DataLinkActiveChanged;
end;

{ TSMDBFilterComboBox }
constructor TSMDBFilterComboBox.Create(AOwner: TComponent);
begin
  inherited;

  FDataLink := TSMFilterDataLink.Create(Self);

//  FFreeItem := 'все записи';
//  FFreeItem := 'all records';
  FFreeValue := '-1';
  FFieldDisplay := '';
  FFieldValue := '';
  Style := csDropDownList;
  FNeedRebuild := True;
end;

destructor TSMDBFilterComboBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  inherited;
end;

procedure TSMDBFilterComboBox.DataLinkActiveChanged;
begin
//  FNeedRebuild := True;
  BuildItems;
end;

function TSMDBFilterComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TSMDBFilterComboBox.SetDataSource(Value: TDataSource);
begin
  if Value = FDatalink.DataSource then Exit;
  FDataLink.DataSource := Value;
//  FNeedRebuild := (Value <> nil);
end;

procedure TSMDBFilterComboBox.SetFieldDisplay(const Value: string);
begin
  if (Value <> FFieldDisplay) then
  begin
    FFieldDisplay := Value;
//    FNeedRebuild := True;
  end;
end;

procedure TSMDBFilterComboBox.SetFieldValue(const Value: string);
begin
  if (Value <> FFieldValue) then
  begin
    FFieldValue := Value;
//    FNeedRebuild := True;
  end;
end;

procedure TSMDBFilterComboBox.SetFreeItem(Value: string);
begin
  if (FFreeItem <> Value) then
  begin
    FFreeItem := Value;
    BuildItems;
  end;
end;

procedure TSMDBFilterComboBox.SetFreeValue(Value: string);
begin
  if (FFreeValue <> Value) then
  begin
    FFreeValue := Value;
    BuildItems;
  end;
end;

function TSMDBFilterComboBox.GetMaxLen: Integer;
begin
  Result := Width;
//  Result := DataSource.DataSet.FieldByName(FDataField).DisplayWidth;
end;

function TSMDBFilterComboBox.ReplicateChar(chFill: Char; intLen: Integer): string;
begin
  SetLength(Result, intLen);
  FillChar(Result[1], intLen, chFill);
end;

function TSMDBFilterComboBox.CenterStr(strSource: string; chFill: Char; intLen: Integer): string;
begin
  Result := strSource;
  while (Length(Result) < intLen) do
    Result := chFill + Result + chFill;
end;

procedure TSMDBFilterComboBox.BuildItems;
const chSeparator = 'Ч';
var i, intOldIndex, intMaxLen: Integer;
    ABookmark: TBookmark;

  function PadRSpace(strStr: string; intLen: Integer): string;
  begin
    FmtStr(Result, '%-*s', [intLen, strStr]);
  end;

  function GetFieldValues(sFieldName: string): string;
  var j: Integer;
      lstFields: TList;
      curField: TField;
  begin
    if Pos(';', sFieldName) <> 0 then
    begin
      lstFields := TList.Create;
      try
        with DataSource.DataSet do
        begin
          GetFieldList(lstFields, sFieldName);
          Result := '';
          for j := 0 to lstFields.Count - 1 do
          begin
            curField := TField(lstFields[j]);
            Result := Result + PadRSpace(curField.AsString, curField.DisplayWidth) + ' ';
          end;
        end;
      finally
        lstFields.Free;
      end;
    end
    else
      Result := DataSource.DataSet.FieldByName(sFieldName).AsString;
  end;

begin
  intMaxLen := 0;
  intOldIndex := ItemIndex;
  if intOldIndex < 0 then
    intOldIndex := 0;

  if FNeedRebuild then
  begin
    if Assigned(FOnBuildEnter) then
      FOnBuildEnter(Self);

    for i := 0 to Items.Count-1 do
      DisposeStr(PAnsiString(Items.Objects[i]));
    Items.Clear;
    if (csDesigning in ComponentState) or
       (csLoading in ComponentState) or
       (not Assigned(FDataLink.DataSource)) or
       (not Assigned(FDataLink.DataSource.DataSet)) or
       (not FDataLink.Active) or
       ((FFieldDisplay = '') and (FFieldValue = '')) then Exit;

    if (FFreeItem <> '') then
    begin
      intMaxLen := GetMaxLen;
      if (FFieldValue = '') then
//        Items.Add(CenterStr(FFreeItem, chSeparator, intMaxLen));
        Items.Add(FFreeItem)
      else
        Items.AddObject(FFreeItem, TObject(LongInt(NewStr(FFreeValue))));
    end;
    FNeedRebuild := False;

    if Assigned(FOnBuildExit) then
      FOnBuildExit(Self);
  end
  else
    Exit;

  with DataSource.DataSet do
  begin
    DisableControls;
    ABookmark := GetBookmark;
    First;
    if (FFreeItem <> '') and not EOF then
      if (FFieldValue = '') then
        Items.Add(ReplicateChar(chSeparator, intMaxLen))
      else
        Items.AddObject(ReplicateChar(chSeparator, intMaxLen), TObject(LongInt(NewStr(FFreeValue))));

    while not EOF do
    begin
      if (FFieldValue = '') then
        Items.Add(GetFieldValues(FFieldDisplay))
      else
        Items.AddObject(GetFieldValues(FFieldDisplay), TObject(LongInt(NewStr(GetFieldValues(FFieldValue)))));
      Next;
    end;
    try
      GotoBookmark(ABookmark);
    except
    end;
    FreeBookmark(ABookmark);
    EnableControls;
  end;
  ItemIndex := intOldIndex;
end;

procedure TSMDBFilterComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (FDataLink <> nil) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TSMDBFilterComboBox.Change;
begin
  inherited;

  if (FFreeItem <> '') and (ItemIndex = 1) then //separator line
    ItemIndex := 0;
end;

end.
