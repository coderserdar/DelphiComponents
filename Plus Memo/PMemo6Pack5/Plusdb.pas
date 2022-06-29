unit Plusdb;

{ TDBPlusMemo component: Data aware TPlusMemo descendant
  Works with PlusMemo v6 }
{ © Electro-Concept Mauricie, 1997-2003 }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

interface
uses PlusMemo, Messages, Classes, Controls, DB, dbCtrls;

type
  TDBPlusMemo = class(TPlusMemo)
    private
      FDataLink: TFieldDataLink;
      FAutoDisplay: Boolean;
      FFocused: Boolean;
      FMemoLoaded: Boolean;
      FInDataChange, fInChange: Boolean;
      fInLoadMemo: Boolean;
      FPaintControl: TPlusMemo;
      procedure DataChange(Sender: TObject);
      procedure EditingChange(Sender: TObject);
      function GetDataField: string;
      function GetDataSource: TDataSource;
      function GetField: TField;
      function GetReadOnly: Boolean;
      procedure SetDataField(const Value: string);
      procedure SetDataSource(Value: TDataSource);
      procedure SetReadOnly(Value: Boolean);
      procedure SetAutoDisplay(Value: Boolean);
      procedure SetFocused(Value: Boolean);
      procedure UpdateData(Sender: TObject);
      procedure InitPaintControl;
      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
      procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
      procedure CMExit(var Message: TCMExit); message CM_EXIT;
      procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
      procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    protected
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure KeyPress{UCONVERT}(var Key: Char);{/UCONVERT} override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure LoadMemo; virtual;
      procedure LoadFromStream(stream: TStream); override;
      procedure SetSelTextBuf(t: PChar); override;
      property Field: TField read GetField;
    published
      property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
      property DataField: string read GetDataField write SetDataField;
      property DataSource: TDataSource read GetDataSource write SetDataSource;
      property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    end;

procedure Register;

implementation

uses WinTypes, WinProcs, SysUtils, Forms;

{ TDBPlusMemo }

constructor TDBPlusMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDBPlusMemo.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  if FPaintControl<>nil then
    begin
      FPaintControl.Keywords:= nil;
      FPaintControl.StartStopKeys:= nil;
      FPaintControl.Free
    end;
  inherited Destroy;
end;

procedure TDBPlusMemo.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  if fInDataChange or fInLoadMemo then Exit;
  fInChange:= True;
  FDataLink.Edit;
  FDataLink.Modified;
  FMemoLoaded := True;
  fInChange:= False
end;

procedure TDBPlusMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBPlusMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FMemoLoaded then
    begin
      if (Key = VK_DELETE) or (Key = VK_BACK) or ((Key = VK_INSERT) and (ssShift in Shift)) then
        begin
          fInChange:= True;
          FDataLink.Edit;
          fInChange:= False
        end
    end
  else Key := 0;

  inherited KeyDown(Key, Shift);
end;

procedure TDBPlusMemo.KeyPress{UCONVERT}(var Key: Char);{/UCONVERT}
begin
if FMemoLoaded then
  begin
    if (Key in [#32..#255]) and (FDataLink.Field <> nil) and not FDataLink.Field.IsValidChar(Key) then
      begin
        MessageBeep($FFFF);
        Key := #0;
      end;
    case Key of
      ^B, ^F, ^H, ^E, ^I, ^J, ^M, ^T, ^U, ^V, ^X, #32..#255:
          begin
            fInChange:= True;
            FDataLink.Edit;
            fInChange:= False;
          end;
      #27: FDataLink.Reset;
      end
    end

else
  begin
    if Key = #13 then LoadMemo;
    Key := #0;
  end;

inherited KeyPress(Key)
end;

procedure TDBPlusMemo.SetSelTextBuf(t: PChar);
begin
  inherited SetSelTextBuf(t);
  if fInDataChange then Exit;
  fInChange:= True;
  FDataLink.Edit;
  FDataLink.Modified;
  FMemoLoaded := True;
  fInChange:= False
end;

function TDBPlusMemo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPlusMemo.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TDBPlusMemo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBPlusMemo.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDBPlusMemo.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBPlusMemo.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDBPlusMemo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBPlusMemo.LoadMemo;
begin
  if not FMemoLoaded and (FDataLink.Field<>nil) and (FDataLink.Field is TBlobField) then
    begin
      fInLoadMemo:= True;
      BeginUpdate;
      Lines.Text := FDataLink.Field.AsString;
      EndUpdate;
      SelStart:= 0;
      FMemoLoaded:= True;
      fInLoadmemo:= False;
      EditingChange(Self)
    end
end;

procedure TDBPlusMemo.DataChange(Sender: TObject);
var tmpstr: string;
begin
  if fInChange then Exit;
  fInDataChange:= True;

  if FDataLink.Field <> nil then
    if FDataLink.Field is TBlobField then
      begin
        Clear;
        if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
          begin
            FMemoLoaded := False;
            LoadMemo;
          end
        else
          begin
            Paragraphs[0]:= '(' + FDataLink.Field.DisplayLabel + ')';
            FMemoLoaded := False;
          end;
      end
    else
      begin
        if FFocused and FDataLink.CanModify then tmpstr := FDataLink.Field.Text
                                            else tmpstr := FDataLink.Field.DisplayText;
        if tmpstr<>Paragraphs.Text then
          begin
            BeginUpdate;
            Paragraphs.Text:= tmpstr;
            SelStart:= 0;
            EndUpdate
          end;
        FMemoLoaded := True;
      end

  else
    begin
      Clear;   
      if csDesigning in ComponentState then Paragraphs[0] := Name;
      FMemoLoaded := False;
    end;

  inherited Change;
  fInDataChange:= False;
end;

procedure TDBPlusMemo.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TDBPlusMemo.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsString := Lines.Text;
end;

procedure TDBPlusMemo.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
    begin
      FFocused := Value;
      if not (FDataLink.Field is TBlobField) then FDataLink.Reset
    end
end;

procedure TDBPlusMemo.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited
end;

procedure TDBPlusMemo.CMExit(var Message: TCMExit);
begin
  {if not (FDataLink.Field is TBlobField) then  Commented June 7, 2001}
    try
      FDataLink.UpdateRecord;
    except
      SetFocus;
      raise;
    end;
  SetFocused(False);
  inherited;
end;

procedure TDBPlusMemo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TDBPlusMemo.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
    begin
      FAutoDisplay := Value;
      if Value then LoadMemo;
    end;
end;

procedure TDBPlusMemo.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure Register;
begin
  RegisterComponents('ECM', [TDBPlusMemo])
end;


procedure TDBPlusMemo.WMPaint(var Message: TWMPaint);
var S: string;
begin
  if not (csPaintCopy in ControlState) then inherited
  else
    begin
      InitPaintControl;
      FPaintControl.Highlighter:= Highlighter;
      if FDataLink.Field <> nil then
        if FDataLink.Field is TBlobField then
          begin
          if FAutoDisplay then
            S := AdjustLineBreaks(FDataLink.Field.AsString) else
            S := Format('(%s)', [FDataLink.Field.DisplayLabel]);
          end
        else
          S := FDataLink.Field.DisplayText;
      FPaintControl.Lines.Text:= S;
      FPaintControl.Parent:= GetParentForm(Self);
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
      FPaintControl.Parent:= nil;
      FPaintControl.Highlighter:= nil
    end;
end;

procedure TDBPlusMemo.InitPaintControl;
begin
  if FPaintControl=nil then
    begin
      FPaintControl:= TPlusMemo.Create(nil);
      with FPaintControl do
        begin
          UpdateMode:= umOnNeed;
          Visible:= False
        end
    end;

  FPaintControl.LineHeight:= LineHeight;
  FPaintControl.Alignment:= Alignment;
  FPaintControl.BorderStyle:= BorderStyle;
  FPaintControl.WordWrap:= WordWrap;
  FPaintControl.AltFont:= AltFont;
  FPaintControl.Font:= Font;
  FPaintControl.ApplyKeywords:= ApplyKeywords;
  FPaintControl.ApplyStartStopKeys:= ApplyStartStopKeys;
  FPaintControl.BackgroundBmp:= BackgroundBmp;
  FPaintControl.EndOfTextMark:= EndOfTextMark;
  FPaintControl.HighlightBackgnd:= HighlightBackgnd;
  FPaintControl.HighlightColor:= HighlightColor;
  FPaintControl.Justified:= Justified;
  FPaintControl.LeftMargin:= LeftMargin;
  FPaintControl.NullReplacement:= NullReplacement;
  FPaintControl.RightMargin:= RightMargin;
  FPaintControl.Delimiters:= Delimiters;
  FPaintControl.StaticFormat:= StaticFormat;
  FPaintControl.Keywords:= Keywords;
  FPaintControl.StartStopKeys:= StartStopKeys;
  FPaintControl.TabStops:= TabStops;
  FPaintControl.Color:= Color;
  FPaintControl.Ctl3D:= Ctl3D;
  FPaintControl.ScrollBars:= ScrollBars;
  FPaintControl.Width:= Width;
  FPaintControl.Height:= Height
end;

end.
