unit Options;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, KControls, KHexEditor, Basic;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    BUOk: TButton;
    BuCancel: TButton;
    PCMain: TPageControl;
    TSEditorOptions: TTabSheet;
    TSColors: TTabSheet;
    LiBColors: TListBox;
    LBAttributes: TLabel;
    GBGeneral: TGroupBox;
    CBDropFiles: TCheckBox;
    CBUndoAfterSave: TCheckBox;
    GBColors: TGroupBox;
    LBHighFG: TLabel;
    TSEditorFont: TTabSheet;
    CoBFontName: TComboBox;
    LBFontName: TLabel;
    PNFontSample: TPanel;
    LBSample: TLabel;
    BUDefault: TButton;
    CBGroupUndo: TCheckBox;
    RGAddressMode: TRadioGroup;
    RGDisabledDrawStyle: TRadioGroup;
    GBAppearance: TGroupBox;
    CBShowAddress: TCheckBox;
    CBShowDigits: TCheckBox;
    CBShowText: TCheckBox;
    CBShowHorzLines: TCheckBox;
    CBShowVertLines: TCheckBox;
    CBShowSeparators: TCheckBox;
    CBShowInactiveCaret: TCheckBox;
    BUColorChange: TButton;
    LBAddressPrefix: TLabel;
    EDAddressPrefix: TEdit;
    EDAddressSize: TEdit;
    LBAddressSize: TLabel;
    LBCharSpacing: TLabel;
    EDCharSpacing: TEdit;
    LBDigitGrouping: TLabel;
    EDDigitGrouping: TEdit;
    EDLineSize: TEdit;
    LBLineSize: TLabel;
    LBbytes: TLabel;
    LBpoints: TLabel;
    LBbytes2: TLabel;
    LBLineHeightPercent: TLabel;
    EDLineHeightPercent: TEdit;
    LBpercent: TLabel;
    LBUndoLimit: TLabel;
    EDUndoLimit: TEdit;
    LBFontSize: TLabel;
    EDFontSize: TEdit;
    SHColor: TShape;
    CBFontStyleBold: TCheckBox;
    CBFontStyleItalic: TCheckBox;
    CDChange: TColorDialog;
    procedure CoBFontNameClick(Sender: TObject);
    procedure BUDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EDFontSizeExit(Sender: TObject);
    procedure LiBColorsClick(Sender: TObject);
    procedure BUColorChangeClick(Sender: TObject);
    procedure LiBColorsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure LiBColorsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FColors: TKColorArray;
    procedure UpdateFontSample;
  public
    procedure SetData(const Data: TEnvironment; AColors: TKColorArray);
    procedure GetData(var Data: TEnvironment; AColors: TKColorArray);
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses
  Math;

{ TOptionsForm }

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  I: TKHexEditorColorIndex;
begin
  PCMain.ActivePageIndex := 0;
  InitColors(FColors);
  for I := 0 to Length(FColors) - 1 do
    LiBColors.Items.Add(GetColorSpec(I).Name);
end;

procedure TOptionsForm.GetData(var Data: TEnvironment; AColors: TKColorArray);
begin
  with Data do
  begin
    P.DropFiles := CBDropFiles.Checked;
    P.GroupUndo := CBGroupUndo.Checked;
    P.UndoAfterSave := CBUndoAfterSave.Checked;
    P.ShowAddress := CBShowAddress.Checked;
    P.ShowDigits := CBShowDigits.Checked;
    P.ShowText := CBShowText.Checked;
    P.ShowHorzLines := CBShowHorzLines.Checked;
    P.ShowVertLines := CBShowVertLines.Checked;
    P.ShowSeparators := CBShowSeparators.Checked;
    P.ShowInactiveCaret := CBShowInactiveCaret.Checked;
    P.AddressMode := RGAddressMode.ItemIndex;
    AddressPrefix := EDAddressPrefix.Text;
    P.AddressSize := StrToIntDef(EDAddressSize.Text, P.AddressSize);
    P.CharSpacing := StrToIntDef(EDCharSpacing.Text, P.CharSpacing);
    P.DigitGrouping := StrToIntDef(EDDigitGrouping.Text, P.DigitGrouping);
    P.DisabledDrawStyle := RGDisabledDrawStyle.ItemIndex;
    P.LineHeightPercent := StrToIntDef(EDLineHeightPercent.Text, P.LineHeightPercent);
    P.LineSize := StrToIntDef(EDLineSize.Text, P.LineSize);
    P.UndoLimit := StrToIntDef(EDUndoLimit.Text, P.UndoLimit);
    FontName := CoBFontName.Text;
    P.FontSize := StrToIntDef(EDFontSize.Text, P.FontSize);
    P.FontStyle := [];
    if CBFontStyleBold.Checked then Include(P.FontStyle, fsBold);
    if CBFontStyleItalic.Checked then Include(P.FontStyle, fsItalic);
    CopyColors(FColors, AColors);
  end;
end;

procedure TOptionsForm.SetData(const Data: TEnvironment; AColors: TKColorArray);
begin
  with Data do
  begin
    CBDropFiles.Checked := P.DropFiles;
    CBGroupUndo.Checked := P.GroupUndo;
    CBUndoAfterSave.Checked := P.UndoAfterSave;
    CBShowAddress.Checked := P.ShowAddress;
    CBShowDigits.Checked := P.ShowDigits;
    CBShowText.Checked := P.ShowText;
    CBShowHorzLines.Checked := P.ShowHorzLines;
    CBShowVertLines.Checked := P.ShowVertLines;
    CBShowSeparators.Checked := P.ShowSeparators;
    CBShowInactiveCaret.Checked := P.ShowInactiveCaret;
    RGAddressMode.ItemIndex := P.AddressMode;
    EDAddressPrefix.Text := AddressPrefix;
    EDAddressSize.Text := IntToStr(P.AddressSize);
    EDCharSpacing.Text := IntToStr(P.CharSpacing);
    EDDigitGrouping.Text := IntToStr(P.DigitGrouping);
    RGDisabledDrawStyle.ItemIndex := P.DisabledDrawStyle;
    EDLineHeightPercent.Text := IntToSTr(P.LineHeightPercent);
    EDLineSize.Text := IntToStr(P.LineSize);
    EDUndoLimit.Text := IntToStr(P.UndoLimit);
    CobFontName.Items.Clear;
    AddFontsToList(Canvas.Handle, CoBFontName.Items, fpFixed);
    CoBFontName.ItemIndex := Max(CoBFontName.Items.IndexOf(FontName), Min(0, CobFontName.Items.Count - 1));
    EDFontSize.Text := IntToStr(P.FontSize);
    CBFontStyleBold.Checked := fsBold in P.FontStyle;
    CBFontStyleItalic.Checked := fsItalic in P.FontStyle;
    UpdateFontSample;
    CopyColors(AColors, FColors);
    LiBColors.Invalidate;
    if LiBColors.ItemIndex < 0 then LiBColors.ItemIndex := 0;
    LiBColorsClick(nil);
  end;
end;

procedure TOptionsForm.UpdateFontSample;
var
  Ok: Boolean;
  FontStyle: TFontStyles;
begin
  if CoBFontName.Text <> '' then
  begin
    Ok := True;
    with PNFontSample.Font do
    begin
      Name := CoBFontName.Text;
      Size := EditStrToInt(Self.Handle, EDFontSize, cFontSizeMin, cFontSizeMax, cFontSizeDef, Ok);
      FontStyle := [];
      if CBFontStyleBold.Checked then Include(FontStyle, fsBold);
      if CBFontStyleItalic.Checked then Include(FontStyle, fsItalic);
      Style := FontStyle;
    end;
  end;
end;

procedure TOptionsForm.CoBFontNameClick(Sender: TObject);
begin
  UpdateFontSample;
end;

procedure TOptionsForm.EDFontSizeExit(Sender: TObject);
begin
  UpdateFontSample;
end;

procedure TOptionsForm.BUDefaultClick(Sender: TObject);
var
  AEnv: TEnvironment;
  AColors: TKColorArray;
begin
  InitEnvironment(AEnv);
  InitColors(AColors);
  SetData(AEnv, AColors);
end;

procedure TOptionsForm.BUColorChangeClick(Sender: TObject);
begin
  CDChange.Color := FColors[TKHexEditorColorIndex(LiBColors.ItemIndex)];
  if CDChange.Execute then
  begin
    FColors[TKHexEditorColorIndex(LiBColors.ItemIndex)] := CDChange.Color;
    LiBColors.Invalidate;
    LiBColorsClick(nil);
  end;
end;

procedure TOptionsForm.LiBColorsClick(Sender: TObject);
begin
  SHColor.Brush.Color := FColors[TKHexEditorColorIndex(LiBColors.ItemIndex)];
end;

procedure TOptionsForm.LiBColorsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox).Canvas do
  begin
    FillRect(Rect);
    Brush.Color := FColors[TKHexEditorColorIndex(Index)];
    Pen.Color := clWindowText;
    Rectangle(Rect.Left + 2, Rect.Top + 1, Rect.Left + 16, Rect.Bottom - 1);
    SetBKMode(Handle, TRANSPARENT);
    TextOut(Rect.Left + 20, Rect.Top, LiBColors.Items[Index]);
    // to ensure coming DrawFocusRect will be painted correctly:
    SetBkColor(Handle, ColorToRGB(clWindow));
    SetTextColor(Handle, ColorToRGB(clWindowText));
  end;
end;

procedure TOptionsForm.LiBColorsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
    LiBColorsClick(nil);
end;

procedure TOptionsForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Ok: Boolean;
begin
  if ModalResult = mrOk then
  begin
    Ok := True;
    EditStrToInt(Handle, EDAddressSize, cAddressSizeMin, cAddressSizeMax, cAddressSizeDef, Ok);
    EditStrToInt(Handle, EDCharSpacing, cCharSpacingMin, cCharSpacingMax, cCharSpacingDef, Ok);
    EditStrToInt(Handle, EDDigitGrouping, cDigitGroupingMin, cDigitGroupingMax, cDigitGroupingDef, Ok);
    EditStrToInt(Handle, EDLineHeightPercent, cLineHeightPercentMin, cLineHeightPercentMax, cLineHeightPercentDef, Ok);
    EditStrToInt(Handle, EDLineSize, cLineSizeMin, cLineSizeMax, cLineSizeDef, Ok);
    EditStrToInt(Handle, EDUndoLimit, cUndoLimitMin, cUndoLimitMax, cUndoLimitDef, Ok);
    EditStrToInt(Handle, EDFontSize, cFontSizeMin, cFontSizeMax, cFontSizeDef, Ok);
    CanClose := Ok;
  end;
end;

{$IFDEF FPC}
initialization
  {$i options.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.

