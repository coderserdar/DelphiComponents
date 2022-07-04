unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, ImgList, StdCtrls, ExtCtrls, ComCtrls, TBX, TB2Dock, TB2Toolbar,
  TB2Item, TB2Common, TB2ExtItems, TBXMDI, TBXExtItems, TBXSwitcher,
  TBXLists, TBXDkPanels, TBXToolPals, TBXStatusBars, TB2MDI,
  TBXStripesTheme, TBXOfficeXPTheme, TBXAluminumTheme, TB2ToolWindow,
  Buttons, XPMan, TBXControls, TBXGraphics;

type
  TForm1 = class(TForm)
    TBDock1: TTBXDock;
    TBToolbar1: TTBXToolbar;
    TBXSubmenuItem1: TTBXSubmenuItem;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXItem3: TTBXItem;
    TBXItem4: TTBXItem;
    mnEdit: TTBXSubmenuItem;
    TBXItem5: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem6: TTBXItem;
    ImageList1: TTBXImageList;
    TBXItem7: TTBXItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    TBXItem8: TTBXItem;
    TBXSubmenuItem3: TTBXSubmenuItem;
    TBXSubmenuItem4: TTBXSubmenuItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    TBXItem9: TTBXItem;
    TBXItem10: TTBXItem;
    TBXItem11: TTBXItem;
    TBXItem15: TTBXItem;
    TBDock2: TTBXDock;
    TBDock3: TTBXDock;
    TBDock4: TTBXDock;
    ImageList2: TTBXImageList;
    TBXSeparatorItem11: TTBXSeparatorItem;
    TBXItem29: TTBXItem;
    TBXItem28: TTBXItem;
    TBXItem38: TTBXItem;
    TBXToolbar2: TTBXToolbar;
    TBXItem40: TTBXItem;
    TBXItem41: TTBXItem;
    TBXItem42: TTBXItem;
    TBXSeparatorItem12: TTBXSeparatorItem;
    TBXItem43: TTBXItem;
    TBXItem44: TTBXItem;
    TBXSeparatorItem13: TTBXSeparatorItem;
    TBXItem45: TTBXItem;
    TBXItem46: TTBXItem;
    TBXItem47: TTBXItem;
    TBXSeparatorItem14: TTBXSeparatorItem;
    mnUndoItems: TTBXSubmenuItem;
    TBXSubmenuItem9: TTBXSubmenuItem;
    TBXItem12: TTBXItem;
    TBXItem13: TTBXItem;
    TBXSeparatorItem6: TTBXSeparatorItem;
    bColorItem: TTBXColorItem;
    TBXSubmenuItem6: TTBXSubmenuItem;
    ColorDialog: TColorDialog;
    TBXSubmenuItem2: TTBXSubmenuItem;
    TBXItem16: TTBXItem;
    TBXItem17: TTBXItem;
    TBXItem18: TTBXItem;
    TBXItem19: TTBXItem;
    TBXSeparatorItem9: TTBXSeparatorItem;
    TBXItem20: TTBXItem;
    TBXItem21: TTBXItem;
    TBXSeparatorItem10: TTBXSeparatorItem;
    TBXItem22: TTBXItem;
    TBXMDIHandler1: TTBXMDIHandler;
    TBXSubmenuItem5: TTBXSubmenuItem;
    TBXMDIWindowItem1: TTBXMDIWindowItem;
    TBXItem23: TTBXItem;
    TBXItem26: TTBXItem;
    TBXSeparatorItem16: TTBXSeparatorItem;
    TBXSwitcher: TTBXSwitcher;
    TBXPopupMenu1: TTBXPopupMenu;
    TBXItem36: TTBXItem;
    TBXItem37: TTBXItem;
    TBXSubmenuItem7: TTBXSubmenuItem;
    TBXItem39: TTBXItem;
    TBXItem55: TTBXItem;
    TBXSeparatorItem17: TTBXSeparatorItem;
    TBXItem56: TTBXItem;
    TBXItem57: TTBXItem;
    TBXItem58: TTBXItem;
    TBXItem59: TTBXItem;
    TBXToolbar3: TTBXToolbar;
    TBXSeparatorItem18: TTBXSeparatorItem;
    TBXItem61: TTBXItem;
    TBXItem62: TTBXItem;
    TBXItem64: TTBXItem;
    TBXItem65: TTBXItem;
    TBXItem66: TTBXItem;
    TBXSeparatorItem19: TTBXSeparatorItem;
    TBXSubmenuItem8: TTBXSubmenuItem;
    TBXSeparatorItem20: TTBXSeparatorItem;
    MoreColors: TTBXItem;
    TBXSeparatorItem5: TTBXSeparatorItem;
    TBXLabelItem1: TTBXLabelItem;
    TBToolbar3: TTBXToolbar;
    be1: TTBXSubmenuItem;
    TBXItem24: TTBXItem;
    TBXItem25: TTBXItem;
    TBXItem30: TTBXItem;
    be2: TTBXSubmenuItem;
    be3: TTBXItem;
    be4: TTBXItem;
    TBXSeparatorItem8: TTBXSeparatorItem;
    be5: TTBXItem;
    be6: TTBXItem;
    be7: TTBXSubmenuItem;
    TBXItem27: TTBXItem;
    TBXItem31: TTBXItem;
    TBXEditItem2: TTBXEditItem;
    TBXSeparatorItem7: TTBXSeparatorItem;
    UndoList: TTBXUndoList;
    UndoLabel: TTBXLabelItem;
    lstFonts: TTBXComboBoxItem;
    TBXComboList1: TTBXComboBoxItem;
    ColorCombo: TTBXDropDownItem;
    TBXSubmenuItem10: TTBXSubmenuItem;
    TBXList1: TTBXStringList;
    TBXSeparatorItem15: TTBXSeparatorItem;
    DockablePanel: TTBXDockablePanel;
    TBXPageScroller: TTBXPageScroller;
    TBXLabel1: TTBXLabel;
    TBXLabel2: TTBXLabel;
    TBXLabel3: TTBXLabel;
    TBXAlignmentPanel2: TTBXAlignmentPanel;
    ListBox1: TListBox;
    TBXLabel4: TTBXLabel;
    TBXAlignmentPanel4: TTBXAlignmentPanel;
    TBXToolbar1: TTBXToolbar;
    TBXSubmenuItem11: TTBXSubmenuItem;
    TBXSeparatorItem21: TTBXSeparatorItem;
    TBXComboList2: TTBXComboBoxItem;
    ClrDefault: TTBXColorItem;
    ColorPalette: TTBXColorPalette;
    TBXSeparatorItem26: TTBXSeparatorItem;
    ToolPalette: TTBXToolPalette;
    TBXCheckBox1: TTBXCheckBox;
    TBXRadioButton1: TTBXRadioButton;
    TBXRadioButton2: TTBXRadioButton;
    TBXRadioButton3: TTBXRadioButton;
    TBXCheckBox2: TTBXCheckBox;
    TBXLink1: TTBXLink;
    TBXLabel5: TTBXLabel;
    TBXStatusBar: TTBXStatusBar;
    Edit1: TEdit;
    bColorButton: TTBXSubmenuItem;
    TBXSeparatorItem22: TTBXSeparatorItem;
    TBXVisibilityToggleItem1: TTBXVisibilityToggleItem;
    TBXSpinEditItem1: TTBXSpinEditItem;
    TBXSeparatorItem23: TTBXSeparatorItem;
    TBXComboList3: TTBXComboBoxItem;
    TBXMultiDock1: TTBXMultiDock;
    TBXMultiDock2: TTBXMultiDock;
    TBXMultiDock3: TTBXMultiDock;
    TBXMultiDock4: TTBXMultiDock;
    TBXDockablePanel1: TTBXDockablePanel;
    TBXDockablePanel2: TTBXDockablePanel;
    TBXDockablePanel3: TTBXDockablePanel;
    TBXLabel6: TTBXLabel;
    TBXRadioButton4: TTBXRadioButton;
    TBXRadioButton5: TTBXRadioButton;
    TBXRadioButton6: TTBXRadioButton;
    TBXLabel7: TTBXLabel;
    TBXCheckBox3: TTBXCheckBox;
    TBXCheckBox4: TTBXCheckBox;
    TBXToolWindow1: TTBXToolWindow;
    TBXSpinEditItem2: TTBXSpinEditItem;
    TBXComboBoxItem1: TTBXComboBoxItem;
    TBXButton1: TTBXButton;
    TBXButton2: TTBXButton;
    TBXButton3: TTBXButton;
    TBXPageScroller1: TTBXPageScroller;
    TBXButton4: TTBXButton;
    TBXButton5: TTBXButton;
    TBXButton6: TTBXButton;
    TBXSeparatorItem24: TTBXSeparatorItem;
    TBXItem14: TTBXItem;
    TBXItem32: TTBXItem;
    ImageList3: TImageList;
    procedure TBXItem31Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure MakeNewWindow(Sender: TObject);
    procedure TBXItem23Click(Sender: TObject);
    procedure TBXItem26Click(Sender: TObject);
    procedure TBXItem27Click(Sender: TObject);
    procedure mnUndoItemsPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure UndoListChange(Sender: TObject);
    procedure lstFontsMeasureHeight(Sender: TTBXCustomList;
      ACanvas: TCanvas; var AHeight: Integer);
    procedure lstFontsDrawItem(Sender: TTBXCustomList; ACanvas: TCanvas;
      ARect: TRect; AIndex, AHoverIndex: Integer; var DrawDefault: Boolean);
    procedure TBXItem60Click(Sender: TObject);
    procedure MoreColorsClick(Sender: TObject);
    procedure ColorPaletteChange(Sender: TObject);
    procedure ClrDefaultClick(Sender: TObject);
    procedure TBXRadioButton1Change(Sender: TObject);
    procedure TBXRadioButton2Change(Sender: TObject);
    procedure TBXRadioButton3Change(Sender: TObject);
    procedure TBXCheckBox1Change(Sender: TObject);
    procedure TBXStatusBarPanelClick(Sender: TTBXCustomStatusBar;
      Panel: TTBXStatusPanel);
    procedure bColorButtonDrawImage(Item: TTBCustomItem;
      Viewer: TTBItemViewer; Canvas: TCanvas; ImageRect: TRect;
      ImageOffset: TPoint; StateFlags: Integer);
    procedure TBXSpinEditItem1TextToValue(Sender: TTBXCustomSpinEditItem;
      const AText: WideString; out AValue: Extended; var CanConvert: Boolean);
    procedure TBXSpinEditItem1ValueToText(Sender: TTBXCustomSpinEditItem;
      const AValue: Extended; var Text: WideString);
    procedure TBXSpinEditItem1Convert(Sender: TTBXCustomSpinEditItem;
      const APrefix, APostfix: WideString; var AValue: Extended;
      var CanConvert: Boolean);
    procedure TBXComboList3Change(Sender: TObject);
    procedure TBXRadioButton4Click(Sender: TObject);
    procedure TBXCheckBox3Change(Sender: TObject);
    procedure TBXCheckBox4Change(Sender: TObject);
    procedure TBXColorSet1GetColorInfo(Sender: TTBXCustomColorSet; Col,
      Row: Integer; var Color: TColor; var Name: String);
  end;

var
  Form1: TForm1;

implementation

uses Unit2, TBXUtils, TBXThemes;

{$R *.DFM}

procedure TForm1.TBXItem31Click(Sender: TObject);
begin
  TTBXItem(Sender).Checked := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  C: HCURSOR;
begin
  if not IsWindowsXP then
  begin
    TBXLabel1.Enabled := False;
    TBXCheckBox1.Enabled := False;
  end;
  GetAvailableTBXThemes(ListBox1.Items);
  lstFonts.Lines.Assign(Screen.Fonts);

  { Replace Borland's hand cursor with windows default one, if available }
  C := LoadCursor(0, IDC_HAND);
  if C <> 0 then Screen.Cursors[crHandPoint] := C;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  TBXSetTheme(ListBox1.Items[ListBox1.ItemIndex]);
end;

procedure TForm1.MakeNewWindow(Sender: TObject);
begin
  TForm2.Create(Self);
end;

procedure TForm1.TBXItem23Click(Sender: TObject);
begin
  Cascade;
end;

procedure TForm1.TBXItem26Click(Sender: TObject);
begin
  Tile;
end;

procedure TForm1.TBXItem27Click(Sender: TObject);
begin
  TTBXItem(Sender).Checked := True;
end;

type
  TTBViewAccess = class(TTBView);

procedure TForm1.mnUndoItemsPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  UndoList.ItemIndex := 0;
end;

procedure TForm1.UndoListChange(Sender: TObject);
const
  Actns: array [Boolean] of string = (' Action', ' Actions');
begin
  with UndoList do
    UndoLabel.UpdateCaption('Undo ' + IntToStr(ItemIndex + 1) + Actns[ItemIndex <> 0]);
end;

procedure TForm1.lstFontsMeasureHeight(Sender: TTBXCustomList;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := AHeight * 3 div 2;
end;

procedure TForm1.lstFontsDrawItem(Sender: TTBXCustomList; ACanvas: TCanvas;
  ARect: TRect; AIndex, AHoverIndex: Integer; var DrawDefault: Boolean);
var
  S: string;
begin
  S := lstFonts.Lines[AIndex];
  ACanvas.Font.Size := 12;
  ACanvas.Font.Name := S;
end;

procedure TForm1.TBXItem60Click(Sender: TObject);
begin
  DockablePanel.Visible := True;
end;

procedure TForm1.MoreColorsClick(Sender: TObject);
begin
  with ColorDialog do
  begin
    Color := ColorPalette.Color;
    if Execute then ColorPalette.Color := Color;
  end;
end;

procedure TForm1.ColorPaletteChange(Sender: TObject);
begin
  ClrDefault.Checked := ColorPalette.Color = clNone;
  ColorCombo.Text := ColorPalette.ColorToString(ColorPalette.Color);
  bColorItem.Color := ColorPalette.Color;
  bColorItem.Caption := 'Current Color: ' + ColorPalette.ColorToString(ColorPalette.Color);
  bColorButton.Invalidate;
end;

procedure TForm1.ClrDefaultClick(Sender: TObject);
begin
  ColorPalette.Color := clNone;
  ClrDefault.Checked := True;
end;

procedure TForm1.TBXRadioButton1Change(Sender: TObject);
begin
  TBXSwitcher.FlatMenuStyle := fmsAuto;
end;

procedure TForm1.TBXRadioButton2Change(Sender: TObject);
begin
  TBXSwitcher.FlatMenuStyle := fmsEnable;
end;

procedure TForm1.TBXRadioButton3Change(Sender: TObject);
begin
  TBXSwitcher.FlatMenuStyle := fmsDisable;
end;

procedure TForm1.TBXCheckBox1Change(Sender: TObject);
begin
  TBXSwitcher.EnableXPStyles := TBXCheckBox1.Checked;
end;

procedure TForm1.TBXStatusBarPanelClick(Sender: TTBXCustomStatusBar; Panel: TTBXStatusPanel);
begin
  if Panel.Index in [2, 4] then Panel.Enabled := not Panel.Enabled;
end;

procedure TForm1.bColorButtonDrawImage(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Canvas: TCanvas; ImageRect: TRect;
  ImageOffset: TPoint; StateFlags: Integer);
var
  DC: HDC;
  Color: TColor;
begin
  DC := Canvas.Handle;
  if not Boolean(StateFlags and ISF_DISABLED) then
  begin
    Color := ColorPalette.Color;
    OffsetRect(ImageRect, ImageOffset.X, ImageOffset.Y);
    ImageRect.Top := ImageRect.Bottom - 4;
    if Color <> clNone then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ImageRect);
    end
    else
    begin
      FrameRectEx(DC, ImageRect, clBtnShadow, True);
      DitherRect(DC, ImageRect, clBtnFace, clBtnShadow);
    end;
  end;
end;

procedure TForm1.TBXSpinEditItem1TextToValue(
  Sender: TTBXCustomSpinEditItem; const AText: WideString;
  out AValue: Extended; var CanConvert: Boolean);
begin
  if WideCompareText(AText, 'Auto') = 0 then
  begin
    AValue := 0;
    CanConvert := True;
  end
  else if WideCompareText(AText, 'pi') = 0 then
  begin
    AValue := 3.14;
    CanConvert := True;
  end;
end;

procedure TForm1.TBXSpinEditItem1ValueToText(
  Sender: TTBXCustomSpinEditItem; const AValue: Extended;
  var Text: WideString);
begin
  if AValue <= 0 then Text := 'Auto';
end;

procedure TForm1.TBXSpinEditItem1Convert(Sender: TTBXCustomSpinEditItem;
  const APrefix, APostfix: WideString; var AValue: Extended;
  var CanConvert: Boolean);
var
  S: string;
begin
  S := APostfix;

  { use current units if user did not type in units explicitly }
  if Length(S) = 0 then S := Sender.Postfix;

  { convert everything to mm }
  if CompareText(S, 'in') = 0 then AValue := AValue * 25.4
  else if CompareText(S, 'cm') = 0 then AValue := AValue * 10;

  { convert mm to current units }
  if CompareText(Sender.Postfix, 'in') = 0 then AValue := AValue / 25.4
  else if CompareText(Sender.Postfix, 'cm') = 0 then AValue := AValue * 0.1;
end;

procedure TForm1.TBXComboList3Change(Sender: TObject);
begin
  TBXSpinEditItem1.Postfix := TBXComboList3.Text;
end;

procedure TForm1.TBXRadioButton4Click(Sender: TObject);
begin
  TBXDockablePanel1.CaptionRotation := TDPCaptionRotation((Sender as TComponent).Tag);
end;

procedure TForm1.TBXCheckBox3Change(Sender: TObject);
var
  B: Boolean;
begin
  B := (Sender as TTBXCheckBox).Checked;
  DockablePanel.SmoothDrag := B;
  TBXDockablePanel1.SmoothDrag := B;
  TBXDockablePanel2.SmoothDrag := B;
  TBXDockablePanel3.SmoothDrag := B;
end;

procedure TForm1.TBXCheckBox4Change(Sender: TObject);
var
  B: Boolean;
begin
  B := (Sender as TTBXCheckBox).Checked;
  DockablePanel.SmoothDockedResize := B;
  TBXDockablePanel1.SmoothDockedResize := B;
  TBXDockablePanel2.SmoothDockedResize := B;
  TBXDockablePanel3.SmoothDockedResize := B;
end;

procedure TForm1.TBXColorSet1GetColorInfo(Sender: TTBXCustomColorSet; Col,
  Row: Integer; var Color: TColor; var Name: String);
begin
  Color := clRed;
  Name := 'Red';
end;

end.
