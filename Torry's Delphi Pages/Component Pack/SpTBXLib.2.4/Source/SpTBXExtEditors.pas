unit SpTBXExtEditors;

{==============================================================================
Version 2.4

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the theme changes and adjustments are marked with '[Theme-Change]'.

To Do:
  - Rotated caption painting.

Known Issues:
  -

History:
17 January 2009 - version 2.4
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, ExtCtrls, ActnList,
  {$IFNDEF UNICODE}
  TntClasses, TntControls, TntStdCtrls, TntSysUtils,
  {$ENDIF}
  TB2Toolbar, TB2Item, TB2ExtItems,
  SpTBXSkins, SpTBXItem, SpTBXControls, SpTBXEditors;

type
  { TSpTBXColorEditButton }

  TSpTBXColorEditButton = class(TSpTBXEditButton)
  private
    FSelectedColor: TColor;
    procedure SetSelectedColor(const Value: TColor);
  protected
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    function GetInternalDropDownMenu: TPopupMenu; override;
  public
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
  end;

  { TSpTBXColorEdit }

  TSpTBXColorEdit = class(TSpTBXEdit)
  private
    FColorButton: TSpTBXColorEditButton;
    FSelectedFormat: TSpTBXColorTextType;
    FOnSelectedColorChanged: TNotifyEvent;
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedFormat(const Value: TSpTBXColorTextType);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoSelectedColorChanged; virtual;
    procedure UpdateTextFromValue;
    procedure UpdateValueFromText(RevertWhenInvalid: Boolean = True);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ColorButton: TSpTBXColorEditButton read FColorButton;
  published
    property Text stored False;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property SelectedFormat: TSpTBXColorTextType read FSelectedFormat write SetSelectedFormat default cttIdentAndHTML;
    property OnSelectedColorChanged: TNotifyEvent read FOnSelectedColorChanged write FOnSelectedColorChanged;
  end;

  { TSpTBXFontComboBox }

  TSpTBXFontComboBoxPreview = class(TCustomControl)
  private
    FPreviewPanel: TPanel;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PreviewPanel: TPanel read FPreviewPanel;
  end;

  TSpTBXFontComboBox = class(TSpTBXComboBox)
  private
    FFontPreview: Boolean;
    FFontNamePreview: Boolean;
    FMaxMRUItems: Integer;
    FMRUCount: Integer;
    FPreviewWindow: TSpTBXFontComboBoxPreview;
    FSelectedFont: TFontName;
    FOnFontPreview: TSpTBXEditGetTextEvent;
    procedure UpdateSelectedFont(AddMRU: Boolean);
    procedure SetFontNamePreview(const Value: Boolean);
    procedure SetSelectedFont(const Value: TFontName);
    procedure SetMaxMRUItems(Value: Integer);
    procedure SetFontPreview(const Value: Boolean);
  protected
    procedure CreateWnd; override;
    procedure Click; override;
    procedure CloseUp; override;
    procedure DropDown; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MRUCount: Integer read FMRUCount;
  published
    property AutoItemHeight default False;
    property FontPreview: Boolean read FFontPreview write SetFontPreview default True;
    property FontNamePreview: Boolean read FFontNamePreview write SetFontNamePreview default True;
    property MaxMRUItems: Integer read FMaxMRUItems write SetMaxMRUItems default 5;
    property SelectedFont: TFontName read FSelectedFont write SetSelectedFont;
    property OnFontPreview: TSpTBXEditGetTextEvent read FOnFontPreview write FOnFontPreview;
  end;

{ Helpers }
procedure SpFillFontNames(ADest: TStrings);
procedure SpCalcMaxDropDownWidth(Combo: TSpTBXComboBox);

{ Painting helpers }
procedure SpDrawChequeredBackground(ACanvas: TCanvas; ARect: TRect);
procedure SpDrawColorDropDownButton(ACanvas: TCanvas; ARect: TRect; Pushed: Boolean; AColor: TColor; CheckeredBkgndWhenTransparent: Boolean = True);

var
  FontGlyphImgList: TImageList = nil;

implementation

uses
  Themes, UxTheme,
  {$IFNDEF UNICODE} TntActnList, TntWindows, {$ENDIF}
  Math, TB2Common, SpTBXColorPickerForm;

type
  TTBViewAccess = class(TTBView);
  TCustomEditAccess = class(TCustomEdit);

var
  DefaultColorPickerDropDownMenu: TSpTBXColorEditPopupMenu = nil;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpFillFontNames(ADest: TStrings);
// This will only work on Windows 2000 and above, more info on:
// http://www.delphipraxis.net/post712587.html&sid=945c12fa9fb826d76e51c80b42109a21#712587

  function EnumFontsProcWin9x(EnumLogFontEx: PEnumLogFontEx; NewTextMetric: PNewTextMetric;
    FontType: DWORD; LParam: LPARAM): Integer; stdcall;
  var
    S: string;
    GlyphIndex: Integer;
    L: TStrings;
  const
    NTM_PS_OPENTYPE = $00020000;
    NTM_TT_OPENTYPE = $00040000;
  begin
    L := TStrings(LParam);
    if FontType = TRUETYPE_FONTTYPE then
      GlyphIndex := 1
    else
      GlyphIndex := 0;

    S := EnumLogFontEx.elfLogFont.lfFaceName;

    if (S[1] <> '@') then
      if (L.Count = 0) or not SameText(S, L[L.Count - 1]) then
        L.AddObject(S, Pointer(GlyphIndex));

    Result := 1;
  end;

  function EnumFontsProc(EnumLogFontExDV: PEnumLogFontExDV; EnumTextMetric: PEnumTextMetric;
    FontType: DWORD; LParam: LPARAM): Integer; stdcall;
  var
    S: string;
    GlyphIndex: Integer;
    L: TStrings;
  const
    NTM_PS_OPENTYPE = $00020000;
    NTM_TT_OPENTYPE = $00040000;
  begin
    L := TStrings(LParam);
    GlyphIndex := 0;
    if ((EnumTextMetric.etmNewTextMetricEx.ntmTm.ntmFlags and NTM_TT_OPENTYPE) = NTM_TT_OPENTYPE) or
       ((EnumTextMetric.etmNewTextMetricEx.ntmTm.ntmFlags and NTM_PS_OPENTYPE) = NTM_PS_OPENTYPE) then
      GlyphIndex := 2
    else
      if FontType = TRUETYPE_FONTTYPE then
        GlyphIndex := 1;

    S := EnumLogFontExDV.elfEnumLogfontEx.elfLogFont.lfFaceName;

    if (S[1] <> '@') then
      if (L.Count = 0) or not SameText(S, L[L.Count - 1]) then
        L.AddObject(S, Pointer(GlyphIndex));

    Result := 1;
  end;

var
  DC: HDC;
  LFont: TLogFont;
  L: TStringList;
begin
  L := TStringList.Create;
  DC := GetDC(0);
  try
    FillChar(LFont, SizeOf(LFont), 0);
    LFont.lfCharset := DEFAULT_CHARSET;
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
      EnumFontFamiliesEx(DC, LFont, @EnumFontsProcWin9x, LongInt(L), 0)
    else
      EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, LongInt(L), 0);
    L.Sort;
    ADest.Assign(L);
  finally
    ReleaseDC(0, DC);
    L.Free;
  end;
end;

procedure SpCalcMaxDropDownWidth(Combo: TSpTBXComboBox);
var
  I, MaxWidth: Integer;
  Sz: TSize;
  C: TControlCanvas;
begin
  MaxWidth := 0;

  C := TControlCanvas.Create;
  try
    C.Control := Combo;
    C.Font.Assign(Combo.Font);
    for I := 0 to Combo.Items.Count - 1 do begin
      Sz := SpGetTextSize(C.Handle, Combo.Items[I], False);
      if Sz.cx > MaxWidth then MaxWidth := Sz.cx;
    end;

    MaxWidth := Sz.cx + GetSystemMetrics(SM_CXVSCROLL) + 150;
    if Combo.Width < MaxWidth then
      SendMessage(Combo.Handle, CB_SETDROPPEDWIDTH, MaxWidth, 0);
  finally
    C.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Painting helpers }

procedure SpDrawChequeredBackground(ACanvas: TCanvas; ARect: TRect);
// Draws a 2x2 white and silver checkered background
var
  R: TRect;
  I, J, HCount, WCount: Integer;
const
  Size = 2;
begin
  WCount := (ARect.Right - ARect.Left) div Size;
  HCount := (ARect.Bottom - ARect.Top) div Size;
  for J := 0 to HCount do
    for I := 0 to WCount do begin
      R := Bounds(ARect.Left + (I * Size), ARect.Top + (J * Size), Size, Size);
      if R.Right > ARect.Right then
        R.Right := ARect.Right;
      if R.Bottom > ARect.Bottom then
        R.Bottom := ARect.Bottom;
      if (I + J) mod 2 = 0 then
        ACanvas.Brush.Color := clWhite
      else
        ACanvas.Brush.Color := clSilver;
      ACanvas.FillRect(R);
    end;
end;

procedure SpDrawColorDropDownButton(ACanvas: TCanvas; ARect: TRect;
  Pushed: Boolean; AColor: TColor; CheckeredBkgndWhenTransparent: Boolean);
// Draws a button used for color editboxes
var
  R: TRect;
begin
  R := ARect;

  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(R);
  if not Pushed then
    SpDrawRectangle(ACanvas, R, 0, clBtnHighlight, clBtnShadow);

  InflateRect(R, -2, -2);
  if (AColor = clNone) and CheckeredBkgndWhenTransparent then begin
    // Draw a checkered background when clNone is used
    SpDrawChequeredBackground(ACanvas, R);
  end
  else begin
    ACanvas.Brush.Color := AColor;
    ACanvas.FillRect(R);
  end;
  SpDrawRectangle(ACanvas, R, 0, clBtnShadow, clBtnHighlight);

  R := ARect;
  R.Left := R.Right - 9;
  R.Top := R.Bottom - 7;
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(R);
  if Pushed then
    SpDrawRectangle(ACanvas, R, 0, clBtnHighlight, clBtnFace)
  else
    SpDrawRectangle(ACanvas, R, 0, clBtnHighlight, clBtnShadow);
  SpDrawArrow(ACanvas, R.Left + (R.Right - R.Left) div 2, R.Top + (R.Bottom - R.Top) div 2 - 1, clBlack, True, False, 2);

  R := ARect;
  InflateRect(R, -1, -1);
  SpDrawRectangle(ACanvas, R, 0, clBtnFace, clBtnFace);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXColorEditButton }

function TSpTBXColorEditButton.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
begin
  if (PaintStage = pstPrePaint) and not BitmapValid then begin
    Result := True;
    if Assigned(OnDraw) then OnDraw(Self, ACanvas, ARect, PaintStage, Result);
    if Result then
      SpDrawColorDropDownButton(ACanvas, ARect, Pushed, FSelectedColor);
  end
  else
    Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
end;

function TSpTBXColorEditButton.GetInternalDropDownMenu: TPopupMenu;
begin
  if Assigned(DropDownMenu) then
    Result := DropDownMenu
  else
    Result := DefaultColorPickerDropDownMenu;
end;

procedure TSpTBXColorEditButton.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then begin
    FSelectedColor := Value;
    Invalidate;
    if Owner is TSpTBXColorEdit then
      TSpTBXColorEdit(Owner).UpdateTextFromValue;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXColorEdit }

constructor TSpTBXColorEdit.Create(AOwner: TComponent);
begin
  inherited;

  FSelectedFormat := cttIdentAndHTML;

  FColorButton := TSpTBXColorEditButton.Create(Self);
  FColorButton.Parent := Self;
  FColorButton.FreeNotification(Self);
  FColorButton.Align := alRight;
  FColorButton.Width := 19;
  UpdateEditRect;

  Text := 'clBlack';
end;

destructor TSpTBXColorEdit.Destroy;
begin
  FreeAndNil(FColorButton);
  inherited;
end;

procedure TSpTBXColorEdit.DoSelectedColorChanged;
begin
  if Assigned(FOnSelectedColorChanged) then FOnSelectedColorChanged(Self);
end;

procedure TSpTBXColorEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    Key := #0;
    UpdateValueFromText;
  end;
end;

procedure TSpTBXColorEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FColorButton) and (Operation = opRemove) then
    FColorButton := nil;
end;

function TSpTBXColorEdit.GetSelectedColor: TColor;
begin
  Result := FColorButton.SelectedColor;
end;

procedure TSpTBXColorEdit.SetSelectedColor(const Value: TColor);
begin
  FColorButton.SelectedColor := Value;
end;

procedure TSpTBXColorEdit.SetSelectedFormat(const Value: TSpTBXColorTextType);
begin
  if FSelectedFormat <> Value then begin
    FSelectedFormat := Value;
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXColorEdit.UpdateTextFromValue;
begin
  if (SelectedColor = clNone) or (SelectedColor = clDefault) then
    Text := ColorToString(SelectedColor)
  else
    Text := SpColorToString(SelectedColor, FSelectedFormat);
  SelStart := Length(Text);
end;

procedure TSpTBXColorEdit.UpdateValueFromText(RevertWhenInvalid: Boolean = True);
var
  WS: WideString;
  PrevValue, NewValue, C: TColor;
begin
  PrevValue := SelectedColor;
  NewValue := SelectedColor;
  WS := Text;

  // Try to parse the text to get the value
  WS := Trim(WS);
  if SpStringToColor(WS, C) then
    NewValue := C;

  if RevertWhenInvalid or (NewValue <> PrevValue) then begin
    SetSelectedColor(NewValue);
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXColorEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  UpdateValueFromText;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXFontComboBoxPreview }

constructor TSpTBXFontComboBoxPreview.Create(AOwner: TComponent);
begin
  inherited;

  Visible := False;
  SetBounds(0, 0, 0, 0);
  Color := clWindow;
  FPreviewPanel := TPanel.Create(Self);
  FPreviewPanel.Parent := Self;
  FPreviewPanel.Color := clWindow;
  FPreviewPanel.BevelOuter := bvNone;
  FPreviewPanel.Align := alClient;
end;

procedure TSpTBXFontComboBoxPreview.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  with Params do begin
    Style := (Style and not (WS_CHILD or WS_GROUP or WS_TABSTOP)) or WS_POPUP;
    ExStyle := ExStyle or WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    if IsWindowsXP then
      WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
  end;
end;

destructor TSpTBXFontComboBoxPreview.Destroy;
begin
  FreeAndNil(FPreviewPanel);

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXFontComboBox }

constructor TSpTBXFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFontNamePreview := True;
  FFontPreview := True;
  FMaxMRUItems := 5;
  AutoItemHeight := False;
  ItemHeight := 23;
end;

procedure TSpTBXFontComboBox.CreateWnd;
begin
  inherited;
  FMRUCount := 0;
  {$IFNDEF UNICODE}
  SpFillFontNames(Items.AnsiStrings);
  {$ELSE}
  SpFillFontNames(Items);
  {$ENDIF}
  SpCalcMaxDropDownWidth(Self);
end;

procedure TSpTBXFontComboBox.Click;
begin
  UpdateSelectedFont(False);
  inherited;
end;

procedure TSpTBXFontComboBox.CloseUp;
begin
  UpdateSelectedFont(True);
  inherited;
  FreeAndNil(FPreviewWindow);
end;

procedure TSpTBXFontComboBox.DropDown;
var
  W: Integer;
  P: TPoint;
  Sz: TSize;
  WS: WideString;
begin
  inherited;

  if FFontPreview then begin
    WS := 'AaBbYyZz';
    FPreviewWindow := TSpTBXFontComboBoxPreview.Create(Self);
    FPreviewWindow.ParentWindow := Application.Handle;
    FPreviewWindow.PreviewPanel.Font.Size := 14;

    if Assigned(FOnFontPreview) then FOnFontPreview(Self, WS);

    FPreviewWindow.PreviewPanel.Caption := WS;
    Sz := SpGetControlTextSize(FPreviewWindow.PreviewPanel, FPreviewWindow.PreviewPanel.Font, WS);
    Inc(Sz.cx, 100);
    Inc(Sz.cy, 20);

    W := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
    P := Parent.ClientToScreen(Point(Left, Top));

    if P.X + W + Sz.cx > Screen.Width then
      Dec(P.X, Sz.cx)
    else
      Inc(P.X, W);
    if P.Y + Height + Sz.cy > Screen.Height then
      Dec(P.Y, Sz.cy)
    else
      Inc(P.Y, Height);
    FPreviewWindow.SetBounds(P.X, P.Y, Sz.cx, Sz.cy);
    FPreviewWindow.Visible := True;
    ShowWindow(FPreviewWindow.Handle, SW_SHOWNA);
  end;
end;

destructor TSpTBXFontComboBox.Destroy;
begin
  FreeAndNil(FPreviewWindow);
  inherited;
end;

procedure TSpTBXFontComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Flags, ImageIndex: Integer;
  R: TRect;
const
  Spacing = 4;
begin
  if Assigned(OnDrawItem) then
    inherited
  else begin
    // Draw the item glyph if the font is TrueType/OpenType
    R := Rect;
    R.Left := Spacing;
    R.Top := R.Top + ((R.Bottom - R.Top) - FontGlyphImgList.Height) div 2;
    ImageIndex := Integer(Items.Objects[Index]) - 1;
    if ImageIndex > -1 then
      FontGlyphImgList.Draw(Canvas, R.Left, R.Top, ImageIndex);

    // Draw the item text
    R := Rect;
    R.Left := Spacing + FontGlyphImgList.Width + Spacing;
    if FFontNamePreview then
      Canvas.Font.Name := Items[Index];
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    SpDrawXPText(Canvas, Items[Index], R, Flags);

    // Draw the MRU separator line
    if FMaxMRUItems > 0 then begin
      if Index = MRUCount - 1 then
        SpDrawLine(Canvas, Rect.Left, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1, $C0C0C0);
      if Index = MRUCount then
        SpDrawLine(Canvas, Rect.Left, Rect.Top, Rect.Right, Rect.Top, $C0C0C0);
    end;
  end;

  // Show Font Preview Window
  if Assigned(FPreviewWindow) and (odSelected in State) then
    FPreviewWindow.PreviewPanel.Font.Name := Items[Index];
end;

procedure TSpTBXFontComboBox.SetMaxMRUItems(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxMRUItems <> Value then begin
    FMaxMRUItems := Value;
    while FMRUCount > FMaxMRUItems do begin
      Items.Delete(FMRUCount);
      Dec(FMRUCount);
    end;
  end;
end;

procedure TSpTBXFontComboBox.SetFontNamePreview(const Value: Boolean);
begin
  if FFontNamePreview <> Value then begin
    FFontNamePreview := Value;
    Invalidate;
  end;
end;

procedure TSpTBXFontComboBox.SetFontPreview(const Value: Boolean);
begin
  FFontPreview := Value;
end;

procedure TSpTBXFontComboBox.SetSelectedFont(const Value: TFontName);
var
  I: Integer;
begin
  I := Items.IndexOf(Value);
  if ItemIndex <> I then
    ItemIndex := I;
  UpdateSelectedFont(True);

  // If the Value is not valid clear the text and call the click events
  if I = -1 then begin
    Click;
    Select;
  end;
end;

procedure TSpTBXFontComboBox.UpdateSelectedFont(AddMRU: Boolean);
var
  I: Integer;
begin
  I := ItemIndex;
  if I > -1 then begin
    FSelectedFont := Items[I];
    // Add the selected font name to the MRU list
    if AddMRU and (Items.Count > FMRUCount) and (FMaxMRUItems > 0) then begin
      // Exit if it's already on the list
      for I := 0 to FMRUCount - 1 do
        if SameText(FSelectedFont, Items[I]) then Exit;
      // Add the font to the top and delete the last MRU item if necessary
      Items.InsertObject(0, FSelectedFont, Items.Objects[ItemIndex]);
      if FMRUCount >= FMaxMRUItems then
        Items.Delete(FMRUCount)
      else
        Inc(FMRUCount);
    end;
  end
  else
    FSelectedFont := '';
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Stock Objects }

procedure InitializeStock;
begin
  Screen.Cursors[crSpTBXEyeDropper] := LoadCursor(HInstance, 'CZEYEDROPPER');

  FontGlyphImgList := TImageList.CreateSize(12, 12);
  FontGlyphImgList.ResInstLoad(HInstance, rtBitmap, 'SPTBXTRUETYPE', clFuchsia);
  FontGlyphImgList.ResInstLoad(HInstance, rtBitmap, 'SPTBXOPENTYPE', clFuchsia);

  DefaultColorPickerDropDownMenu := TSpTBXColorEditPopupMenu.Create(nil);
end;

procedure FinalizeStock;
begin
  FreeAndNil(FontGlyphImgList);
  FreeAndNil(DefaultColorPickerDropDownMenu);
end;

initialization
  InitializeStock;

finalization
  FinalizeStock;

end.
