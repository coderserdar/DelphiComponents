unit DXMapEditProperties;
//(c)2007 Jaro Benes
//All Rights Reserved

{
Complex application for users of unDelphiX as component editor:

Supported:
 a) set up or change for each chip parameters by drawing or collision brick.
 b) integrated into map editor.

}
interface

{$INCLUDE DelphiXcfg.inc}              

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, DXSprite, DXDraws;

type
  {injected class}
  TEdit = class(StdCtrls.TEdit)
  private
    FButton: TSpeedButton;
    FEditorEnabled: Boolean;
    FOnBtnClick: TNotifyEvent;
    procedure SetGlyph(Pic: TBitmap);
    function GetGlyph: TBitmap;
    procedure SetNumGlyphs(ANumber: Integer);
    function GetNumGlyphs: Integer;
    //function GetMinHeight: Integer;
    procedure SetEditRect;
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    function GetAsFloat: Double;
    procedure SetAsFloat(const Value: Double);
    function GetBtnVisible: Boolean;
    procedure SetBtnVisible(const Value: Boolean);
  protected
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure aClick(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TSpeedButton read FButton;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  published
    property BtnVisible: Boolean read GetBtnVisible write SetBtnVisible default False;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs;
    property OnBtnClick: TNotifyEvent read FOnBtnClick write FOnBtnClick;
  end;

  {  TDelphiXMapEditPropertiesForm  }

  TDelphiXMapEditPropertiesForm = class(TForm)
    Panel1: TPanel;
    LAlpha: TLabel;
    LAnimCount: TLabel;
    LAnimSpeed: TLabel;
    LAnimStart: TLabel;
    LAnimPos: TLabel;
    EAlpha: TEdit;
    EAnimCount: TEdit;
    EAnimSpeed: TEdit;
    EAnimStart: TEdit;
    EAnimPos: TEdit;
    Panel2: TPanel;
    chbCollisioned: TCheckBox;
    chbAnimated: TCheckBox;
    rgBlendMode: TRadioGroup;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    GroupBox1: TGroupBox;
    chbFlip: TCheckBox;
    chbMirror: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    LocalMapType: TMapType;
    FCol, FRow: Integer;
  public
    { Public declarations }
    procedure LoadCellToForm(MapType: TMapType; ACol, ARow: Integer);
    function SaveCellFromForm(ACol, ARow: Integer): TMapType;
    property Col: Integer read FCol write FCol;
    property Row: Integer read FRow write FRow;
  end;

implementation

{$R *.dfm}

uses DXMapEdit;

{  TEdit  }

constructor TEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TSpeedButton.Create(Self);
  FButton.Align := alRight;
  FButton.Caption := '...';
  //FButton.Height := Self.Height-4;
  FButton.Width := FButton.Height div 2;
//  if csDesigning in ComponentState then
//    FButton.Visible := True
//  else FButton.Visible := False;
  FButton.Parent := Self;
  FButton.OnClick := aClick;
  FButton.Cursor := crArrow;
  ControlStyle := ControlStyle - [csSetCaption];
  FButton.Visible := False; {button is not visible as default}
  FEditorEnabled := True;
end;

destructor TEdit.Destroy;
begin
  FButton.Free; FButton := nil;
  inherited Destroy;
end;

procedure TEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

procedure TEdit.SetGlyph(Pic: TBitmap);
begin
  FButton.Glyph.Assign(Pic);
end;

function TEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TEdit.SetNumGlyphs(ANumber: Integer);
begin
  FButton.NumGlyphs := ANumber;
end;

function TEdit.GetNumGlyphs: Integer;
begin
  Result := FButton.NumGlyphs;
end;

procedure TEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

function TEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := True;
end;

procedure TEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN {and not WS_BORDER};
end;

procedure TEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TEdit.SetEditRect;
var
  Loc: TRect;
  W: Integer;
begin
  W := FButton.Width;
  if not FButton.Visible then W := 0;
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1; {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - W - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc)); {debug}
end;

procedure TEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := 5;
  { text edit bug: if size to less than minheight, then edit ctrl does not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else
  if Assigned(FButton) and FButton.Visible then
  begin
    FButton.Width := FButton.Height;
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - 5, 0, FButton.Width, Height - 5)
    else FButton.SetBounds(Width - FButton.Width, 1, FButton.Width, Height - 1);
    SetEditRect;
  end
  else SetEditRect;
end;
{
function TEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;
}
procedure TEdit.aClick(Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else if Assigned(FOnBtnClick) then FOnBtnClick(Self);
end;

procedure TEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TEdit.CMExit(var Message: TCMExit);
begin
  //FButton.Visible := False;
  inherited;
end;

procedure TEdit.CMEnter(var Message: TCMGotFocus);
begin
  //FButton.Visible := True;
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

function TEdit.GetAsInteger: Integer;
begin
  try
    Result := StrToInt(Self.Text);
  except
    Result := 0;
  end;
end;

function TEdit.GetBtnVisible: Boolean;
begin
  Result := FButton.Visible
end;

procedure TEdit.SetAsInteger(const Value: Integer);
begin
  Self.Text := IntToStr(Value)
end;

procedure TEdit.SetBtnVisible(const Value: Boolean);
begin
  FButton.Visible := Value;
end;

function TEdit.GetAsFloat: Double;
begin
  try
    Result := StrToFloat(Self.Text);
  except
    Result := 0;
  end;
end;

procedure TEdit.SetAsFloat(const Value: Double);
begin
  Self.Text := FloatToStr(Value)
end;

{  TDelphiXMapEditPropertiesForm  }

procedure TDelphiXMapEditPropertiesForm.LoadCellToForm(MapType: TMapType; ACol, ARow: Integer);
begin
  LocalMapType := MapType;
  Panel2.Caption := Format('Chip (%d, %d)', [ACol, ARow]);
  chbCollisioned.Checked := MapType.CollisionChip;
  chbAnimated.Checked := MapType.AnimLooped;
  EAnimStart.AsInteger := MapType.AnimStart;
  EAnimCount.AsInteger := MapType.AnimCount;
  EAnimSpeed.AsFloat := MapType.AnimSpeed;
  EAnimPos.AsFloat := MapType.AnimPos;
  rgBlendMode.ItemIndex := Ord(MapType.Rendered);
  EAlpha.AsInteger := MapType.Alpha;
  chbFlip.Checked := (rmfFlip in MapType.MirrorFlip);
  chbMirror.Checked := (rmfMirror in MapType.MirrorFlip);
end;

function TDelphiXMapEditPropertiesForm.SaveCellFromForm(ACol, ARow: Integer): TMapType;
begin
  Result := LocalMapType;
  Result.CollisionChip := chbCollisioned.Checked;
  Result.AnimLooped := chbAnimated.Checked;
  Result.AnimStart := EAnimStart.AsInteger;
  Result.AnimCount := EAnimCount.AsInteger;
  Result.AnimSpeed := EAnimSpeed.AsInteger;
  Result.AnimPos := EAnimPos.AsInteger;
  Result.Rendered := TRenderType(rgBlendMode.ItemIndex);
  Result.Alpha := EAlpha.AsInteger;
  Result.MirrorFlip := [];
  if chbFlip.Checked then Result.MirrorFlip := Result.MirrorFlip + [rmfFlip];
  if chbMirror.Checked then Result.MirrorFlip := Result.MirrorFlip + [rmfMirror];
end;

procedure TDelphiXMapEditPropertiesForm.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Panel2.Color := {$IFDEF VER6UP}clMoneyGreen{$ELSE}clGreen{$ENDIF};
  DelphiXMapEditForm.DXBackgroundSprite.Map[FCol, FRow] := SaveCellFromForm(FCol, FRow);
  DelphiXMapEditForm.MapArea.Invalidate;
  btnCancelClick(Sender);
  Hide;
end;

procedure TDelphiXMapEditPropertiesForm.btnCancelClick(Sender: TObject);
begin
  Tag := 0;
  Panel2.Color := {$IFDEF VER6UP}clSkyBlue{$ELSE}clBlue{$ENDIF};
end;

end.
