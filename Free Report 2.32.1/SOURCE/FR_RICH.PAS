
{*****************************************}
{                                         }
{             FastReport v2.3             }
{           Rich Add-In Object            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Rich;

interface

{$I FR.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Menus,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ClipBrd,
  FR_DBRel, FR_Class, RichEdit, FR_Ctrls;

type
  TfrRichObject = class(TComponent)  // fake component
  end;

  TfrRichView = class(TfrStretcheable)
  private
    CurChar, LastChar, CharFrom: Integer;
    procedure GetRichData(lnum: Integer);
    function DoCalcHeight: Integer;
    procedure ShowRich(Render: Boolean);
  public
    RichEdit: TRichEdit;
    TextOnly: Boolean;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(From: TfrView); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure GetBlob(b: TfrTField); override;
    function CalcHeight: Integer; override;
    function MinHeight: Integer; override;
    function RemainHeight: Integer; override;
  end;

  TfrRichForm = class(TfrObjEditorForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SpeedBar: TPanel;
    OpenButton: TfrSpeedButton;
    SaveButton: TfrSpeedButton;
    UndoButton: TfrSpeedButton;
    Ruler: TPanel;
    FontDialog1: TFontDialog;
    FirstInd: TLabel;
    LeftInd: TLabel;
    RulerLine: TBevel;
    RightInd: TLabel;
    BoldButton: TfrSpeedButton;
    FontName: TComboBox;
    ItalicButton: TfrSpeedButton;
    LeftAlign: TfrSpeedButton;
    CenterAlign: TfrSpeedButton;
    RightAlign: TfrSpeedButton;
    UnderlineButton: TfrSpeedButton;
    BulletsButton: TfrSpeedButton;
    RichEdit1: TRichEdit;
    SpeedButton1: TfrSpeedButton;
    CancBtn: TfrSpeedButton;
    OkBtn: TfrSpeedButton;
    SpeedButton2: TfrSpeedButton;
    Image1: TImage;
    Bevel1: TBevel;
    E1: TEdit;
    Panel8: TPanel;
    SB1: TfrSpeedButton;
    SB2: TfrSpeedButton;
    HelpBtn: TfrSpeedButton;

    procedure SelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure SelectFont(Sender: TObject);
    procedure RulerResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BoldButtonClick(Sender: TObject);
    procedure AlignButtonClick(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure BulletsButtonClick(Sender: TObject);
    procedure RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RulerItemMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RightIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CancBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SB1Click(Sender: TObject);
    procedure SB2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FUpdating: Boolean;
    FDragOfs: Integer;
    FDragging: Boolean;
    function CurrText: TTextAttributes;
    procedure GetFontNames;
    procedure SetupRuler;
    procedure SetEditRect;
  public
    procedure ShowEditor(t: TfrView); override;
  end;


implementation

uses FR_Var, FR_Pars, FR_Intrp, FR_Utils, FR_Const, Printers;

const
  RulerAdj = 4/3;
  GutterWid = 6;

{$R *.DFM}


var
  SRichEdit: TRichEdit;        // temporary rich used during TRichView drawing
  frRichForm: TfrRichForm;

procedure AssignRich(Rich1, Rich2: TRichEdit);
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  Rich2.Lines.SaveToStream(st);
  st.Position := 0;
  Rich1.Lines.LoadFromStream(st);
  st.Free;
end;

{----------------------------------------------------------------------------}
constructor TfrRichView.Create;
begin
  inherited Create;
  RichEdit := TRichEdit.Create(nil);
  RichEdit.Parent := frRichForm;
  RichEdit.Visible := False;
  BaseName := 'Rich';
  Typ := gtAddIn;
end;

destructor TfrRichView.Destroy;
begin
  if frRichForm <> nil then RichEdit.Free;
  inherited Destroy;
end;

procedure TfrRichView.Assign(From: TfrView);
begin
  inherited Assign(From);
  AssignRich(RichEdit, (From as TfrRichView).RichEdit);
  TextOnly := (From as TfrRichView).TextOnly;
end;

procedure TfrRichView.GetRichData(lnum: Integer);
var
  i, j, sum: Integer;
  s, s1, s2: String;
begin
  s := SRichEdit.Lines[lnum];
  sum := 0;
  for i := 0 to lnum - 1 do
  begin
    j := Length(SRichEdit.Lines[i]);
    sum := sum + j + 2;
  end;
  i := 1;
  repeat
    while (i < Length(s)) and (s[i] <> '[') do Inc(i);
    s1 := GetBrackedVariable(s, i, j);
    if i <> j then
    begin
      Delete(s, i, j - i + 1);
      s2 := '';
      CurReport.InternalOnGetValue(s1, s2);
      SRichEdit.SelStart := sum + i - 1;
      SRichEdit.SelLength := j - i + 1;
      SRichEdit.SelText := s2;
      Insert(s2, s, i);
      Inc(i, Length(s2));
    end;
  until i = j;
end;

function TfrRichView.DoCalcHeight: Integer;
var
  Range: TFormatRange;
  LastChar, MaxLen, LogX, LogY, NewDY, LowDy, HighDy: Integer;
  StopRender, Fit: Boolean;
begin
  FillChar(Range, SizeOf(TFormatRange), 0);
  with Range do
  begin
    hdc := GetDC(0);
    hdcTarget := hdc;
    LogX := Screen.PixelsPerInch;
    LogY := LogX;

    LowDy := 0;
    HighDY := 1000000;
    while HighDy - LowDy > 1 do
    begin
      NewDY := LowDy + (HighDy - LowDy) div 2;
      rc := Rect(0, 0, Round(DX * 1440 / LogX), Round(NewDY * 1440.0 / LogY));
      rcPage := rc;
      LastChar := CharFrom;
      MaxLen := SRichEdit.GetTextLen;
      chrg.cpMax := -1;
      repeat
        chrg.cpMin := LastChar;
        LastChar := SRichEdit.Perform(EM_FORMATRANGE, 0, Integer(@Range));
        Fit := (LastChar >= MaxLen) or (LastChar = -1) or (LastChar = 0);
        StopRender := ((LastChar < MaxLen) and (LastChar <> -1)) or Fit;
      until StopRender;

      if Fit then
        HighDy := NewDY else
        LowDy := NewDY;
    end;
    ReleaseDC(0, hdc);
  end;
  SRichEdit.Perform(EM_FORMATRANGE, 0, 0);
  if HighDy < 2 then HighDy := 8;
  Result := HighDy;
end;

{$WARNINGS OFF}
procedure TfrRichView.ShowRich(Render: Boolean);
var
  Range: TFormatRange;
  MaxLen, LogX, LogY: Integer;
  StopRender: Boolean;
  EMF: TMetafile;
  EMFCanvas: TMetafileCanvas;
  re: TRichEdit;
begin
  if Render then
    re := RichEdit else
    re := SRichEdit;
  FillChar(Range, SizeOf(TFormatRange), 0);
  with Range do
  begin
    if Render then
      hdc := Canvas.Handle else
      hdc := GetDC(0);
    if Render then
      if IsPrinting then
      begin
        LogX := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX);
        LogY := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY);
        rc := Rect(DRect.Left * 1440 div LogX, DRect.Top * 1440 div LogY,
                   DRect.Right * 1440 div LogX, DRect.Bottom * 1440 div LogY);
      end
      else
      begin
        LogX := Screen.PixelsPerInch;
        LogY := LogX;
        rc := Rect(0, 0, Round(SaveDX * 1440 / LogX), Round(SaveDY * 1440 / LogY));
        EMF := TMetafile.Create;
        EMF.Width := SaveDX;
        EMF.Height := SaveDY;
        EMFCanvas := TMetafileCanvas.Create(EMF, 0);
        EMFCanvas.Brush.Style := bsClear;
        hdc := EMFCanvas.Handle;
      end
    else
    begin
      LogX := Screen.PixelsPerInch;
      LogY := LogX;
      rc := Rect(0, 0, Round(DX * 1440 / LogX), Round(DY * 1440 / LogY));
    end;
    hdcTarget := hdc;
    rcPage := rc;
    LastChar := CharFrom;
    MaxLen := re.GetTextLen;
    chrg.cpMax := -1;
    repeat
      chrg.cpMin := LastChar;
      LastChar := re.Perform(EM_FORMATRANGE, Integer(Render), Integer(@Range));
      StopRender := ((LastChar < MaxLen) and (LastChar <> -1)) or
                    (LastChar >= MaxLen) or (LastChar = -1) or (LastChar = 0);
    until StopRender;
  end;
  re.Perform(EM_FORMATRANGE, 0, 0);
  if not Render then
    ReleaseDC(0, Range.hdc)
  else if not IsPrinting then
  begin
    EMFCanvas.Free;
    Canvas.StretchDraw(DRect, EMF);
    EMF.Free;
  end;
end;
{$WARNINGS ON}

procedure TfrRichView.Draw(Canvas: TCanvas);
begin
  BeginDraw(Canvas);
  CalcGaps;
  with Canvas do
  begin
    ShowBackground;
    Brush.Style := bsClear;
    CharFrom := 0;
    ShowRich(True);
    ShowFrame;
  end;
  RestoreCoord;
end;

procedure TfrRichView.Print(Stream: TStream);
var
  i: Integer;
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  AssignRich(SRichEdit, RichEdit);
  if not TextOnly then
    for i := 0 to SRichEdit.Lines.Count - 1 do
      GetRichData(i);

  if DrawMode = drPart then
  begin
    CharFrom := LastChar;
    ShowRich(False);
    SRichEdit.SelStart := LastChar;
    SRichEdit.SelLength := SRichEdit.GetTextLen - LastChar + 1;
    SRichEdit.SelText := '';

    SRichEdit.SelStart := 0;
    SRichEdit.SelLength := CurChar;
    SRichEdit.SelText := '';

    CurChar := LastChar;
  end;

  Stream.Write(Typ, 1);
  frWriteString(Stream, ClassName);
  SaveToStream(Stream);
end;

function TfrRichView.CalcHeight: Integer;
var
  i: Integer;
begin
  CurChar := 0;
  Result := 0;
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  AssignRich(SRichEdit, RichEdit);
  if not TextOnly then
    for i := 0 to SRichEdit.Lines.Count - 1 do
      GetRichData(i);
  CharFrom := 0;
  Result := DoCalcHeight;
end;

function TfrRichView.MinHeight: Integer;
begin
  Result := 8;
end;

function TfrRichView.RemainHeight: Integer;
var
  i: Integer;
begin
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  AssignRich(SRichEdit, RichEdit);
  if not TextOnly then
    for i := 0 to SRichEdit.Lines.Count - 1 do
      GetRichData(i);

  CharFrom := LastChar;
  Result := DoCalcHeight;
end;

procedure TfrRichView.LoadFromStream(Stream: TStream);
var
  b: Byte;
  n: Integer;
begin
  inherited LoadFromStream(Stream);
  Stream.Read(b, 1);
  Stream.Read(n, 4);
  if b <> 0 then RichEdit.Lines.LoadFromStream(Stream);
  Stream.Seek(n, soFromBeginning);
end;

procedure TfrRichView.SaveToStream(Stream: TStream);
var
  b: Byte;
  n, o: Integer;
  re: TRichEdit;
begin
  inherited SaveToStream(Stream);
  re := RichEdit;
  if StreamMode = smPrinting then
    re := SRichEdit;
  b := 0;
  if re.Lines.Count <> 0 then b := 1;
  Stream.Write(b, 1);
  n := Stream.Position;
  Stream.Write(n, 4);
  if b <> 0 then re.Lines.SaveToStream(Stream);
  o := Stream.Position;
  Stream.Seek(n, soFromBeginning);
  Stream.Write(o, 4);
  Stream.Seek(0, soFromEnd);
end;

procedure TfrRichView.GetBlob(b: TfrTField);
var
  s: TMemoryStream;
begin
  s := TMemoryStream.Create;
{$IFDEF IBO}
  TfrTBlobField(b).AssignTo(s);
{$ELSE}
  TfrTBlobField(b).SaveToStream(s);
{$ENDIF}
  s.Position := 0;
  RichEdit.Lines.LoadFromStream(s);
  s.Free;
end;


{------------------------------------------------------------------------}
procedure TfrRichForm.ShowEditor(t: TfrView);
begin
  AssignRich(RichEdit1, (t as TfrRichView).RichEdit);
  if ShowModal = mrOk then AssignRich((t as TfrRichView).RichEdit, RichEdit1);
  RichEdit1.Lines.Clear;
end;

procedure TfrRichForm.SelectionChange(Sender: TObject);
begin
  with RichEdit1.Paragraph do
  try
    FUpdating := True;
    FirstInd.Left := Trunc(FirstIndent * RulerAdj) - 4 + GutterWid;
    LeftInd.Left := Trunc((LeftIndent + FirstIndent) * RulerAdj) - 4 + GutterWid;
    RightInd.Left := Ruler.ClientWidth - 6 - Trunc((RightIndent + GutterWid) * RulerAdj);
    BoldButton.Down := fsBold in RichEdit1.SelAttributes.Style;
    ItalicButton.Down := fsItalic in RichEdit1.SelAttributes.Style;
    UnderlineButton.Down := fsUnderline in RichEdit1.SelAttributes.Style;
    BulletsButton.Down := Boolean(Numbering);
    E1.Text := IntToStr(RichEdit1.SelAttributes.Size);
    FontName.ItemIndex := FontName.Items.IndexOf(RichEdit1.SelAttributes.Name);
    case Ord(Alignment) of
      0: LeftAlign.Down := True;
      1: RightAlign.Down := True;
      2: CenterAlign.Down := True;
    end;
  finally
    FUpdating := False;
  end;
end;

function TfrRichForm.CurrText: TTextAttributes;
begin
  if RichEdit1.SelLength > 0 then
    Result := RichEdit1.SelAttributes else
    Result := RichEdit1.DefAttributes;
end;

procedure TfrRichForm.GetFontNames;
begin
  FontName.Items := Screen.Fonts;
  FontName.Sorted := True;
end;

procedure TfrRichForm.SetupRuler;
var
  I: Integer;
  S: String;
begin
  SetLength(S, 201);
  I := 1;
  while I < 200 do
  begin
    S[I] := #9;
    S[I+1] := '|';
    Inc(I, 2);
  end;
  Ruler.Caption := S;
end;

procedure TfrRichForm.SetEditRect;
var
  R: TRect;
begin
  with RichEdit1 do
  begin
    R := Rect(GutterWid, 0, ClientWidth - GutterWid, ClientHeight);
    SendMessage(Handle, EM_SETRECT, 0, Longint(@R));
  end;
end;

{ Event Handlers }

procedure TfrRichForm.FormResize(Sender: TObject);
begin
  SetEditRect;
  SelectionChange(Sender);
end;

procedure TfrRichForm.FormPaint(Sender: TObject);
begin
  SetEditRect;
end;

procedure TfrRichForm.FileOpen(Sender: TObject);
begin
  OpenDialog.Filter := LoadStr(SRTFFile) + ' (*.rtf)|*.rtf';
  if OpenDialog.Execute then
  begin
    RichEdit1.Lines.LoadFromFile(OpenDialog.FileName);
    RichEdit1.SetFocus;
    SelectionChange(Self);
  end;
end;

procedure TfrRichForm.FileSaveAs(Sender: TObject);
begin
  SaveDialog.Filter := LoadStr(SRTFFile) + ' (*.rtf)|*.rtf|' +
                       LoadStr(STextFile) + ' (*.txt)|*.txt';
  if SaveDialog.Execute then
    RichEdit1.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TfrRichForm.EditUndo(Sender: TObject);
begin
  with RichEdit1 do
    if HandleAllocated then SendMessage(Handle, EM_UNDO, 0, 0);
end;

procedure TfrRichForm.SelectFont(Sender: TObject);
begin
  FontDialog1.Font.Assign(RichEdit1.SelAttributes);
  if FontDialog1.Execute then
    CurrText.Assign(FontDialog1.Font);
  RichEdit1.SetFocus;
end;

procedure TfrRichForm.RulerResize(Sender: TObject);
begin
  RulerLine.Width := Ruler.ClientWidth - RulerLine.Left * 2;
end;

procedure TfrRichForm.BoldButtonClick(Sender: TObject);
var
  s: TFontStyles;
begin
  if FUpdating then Exit;
  s := [];
  if BoldButton.Down then s := s + [fsBold];
  if ItalicButton.Down then s := s + [fsItalic];
  if UnderlineButton.Down then s := s + [fsUnderline];
  CurrText.Style := s;
end;

procedure TfrRichForm.AlignButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  case TControl(Sender).Tag of
    312: RichEdit1.Paragraph.Alignment := taLeftJustify;
    313: RichEdit1.Paragraph.Alignment := taCenter;
    314: RichEdit1.Paragraph.Alignment := taRightJustify;
  end;
end;

procedure TfrRichForm.FontNameChange(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Name := FontName.Items[FontName.ItemIndex];
end;

procedure TfrRichForm.BulletsButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  RichEdit1.Paragraph.Numbering := TNumberingStyle(BulletsButton.Down);
end;

{ Ruler Indent Dragging }

procedure TfrRichForm.RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragOfs := (TLabel(Sender).Width div 2);
  TLabel(Sender).Left := TLabel(Sender).Left + X - FDragOfs;
  FDragging := True;
end;

procedure TfrRichForm.RulerItemMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging then
    TLabel(Sender).Left :=  TLabel(Sender).Left + X - FDragOfs
end;

procedure TfrRichForm.FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.FirstIndent :=
    Trunc((FirstInd.Left + FDragOfs - GutterWid) / RulerAdj);
  LeftIndMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TfrRichForm.LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.LeftIndent :=
    Trunc((LeftInd.Left + FDragOfs - GutterWid) / RulerAdj) - RichEdit1.Paragraph.FirstIndent;
  SelectionChange(Sender);
end;

procedure TfrRichForm.RightIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.RightIndent :=
    Trunc((Ruler.ClientWidth - RightInd.Left + FDragOfs - 2) / RulerAdj) - 2 * GutterWid;
  SelectionChange(Sender);
end;

procedure TfrRichForm.CancBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrRichForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrRichForm.SpeedButton2Click(Sender: TObject);
begin
  frVarForm := TfrVarForm.Create(nil);
  with frVarForm do
  if ShowModal = mrOk then
    if SelectedItem <> '' then
    begin
      ClipBoard.Clear;
      ClipBoard.AsText := '[' + SelectedItem + ']';
      RichEdit1.PasteFromClipboard;
    end;
  frVarForm.Free;
end;

procedure TfrRichForm.SB1Click(Sender: TObject);
var
  i: Integer;
begin
  i := StrToInt(E1.Text);
  Inc(i);
  E1.Text := IntToStr(i);
  CurrText.Size := i;
end;

procedure TfrRichForm.SB2Click(Sender: TObject);
var
  i: Integer;
begin
  i := StrToInt(E1.Text);
  Dec(i);
  if i <= 0 then i := 1;
  E1.Text := IntToStr(i);
  CurrText.Size := i;
end;

procedure TfrRichForm.FormActivate(Sender: TObject);
begin
  RichEdit1.SetFocus;
end;

procedure TfrRichForm.FormCreate(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  GetFontNames;
  SetupRuler;
  SelectionChange(Self);

  Caption := LoadStr(frRes + 560);
  OpenButton.Hint := LoadStr(frRes + 561);
  SaveButton.Hint := LoadStr(frRes + 562);
  UndoButton.Hint := LoadStr(frRes + 563);
  BoldButton.Hint := LoadStr(frRes + 564);
  ItalicButton.Hint := LoadStr(frRes + 565);
  LeftAlign.Hint := LoadStr(frRes + 566);
  CenterAlign.Hint := LoadStr(frRes + 567);
  RightAlign.Hint := LoadStr(frRes + 568);
  UnderlineButton.Hint := LoadStr(frRes + 569);
  BulletsButton.Hint := LoadStr(frRes + 570);
  SpeedButton1.Hint := LoadStr(frRes + 571);
  HelpBtn.Hint := LoadStr(frRes + 032);
  CancBtn.Hint := LoadStr(frRes + 572);
  OkBtn.Hint := LoadStr(frRes + 573);
  SpeedButton2.Caption := LoadStr(frRes + 574);
  SpeedButton2.Hint := LoadStr(frRes + 575);
  FontName.Hint := LoadStr(frRes + 576);
  E1.Hint := LoadStr(frRes + 577);
  BoldButton.Glyph.Handle := LoadBitmap(hInstance, 'FR_BOLD');
  ItalicButton.Glyph.Handle := LoadBitmap(hInstance, 'FR_ITALIC');
  UnderlineButton.Glyph.Handle := LoadBitmap(hInstance, 'FR_UNDRLINE');
end;

type
  THackBtn = class(TfrSpeedButton)
  end;

procedure TfrRichForm.HelpBtnClick(Sender: TObject);
begin
  Screen.Cursor := crHelp;
  SetCapture(Handle);
  THackBtn(HelpBtn).FMouseInControl := False;
  HelpBtn.Invalidate;
end;

procedure TfrRichForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  c: TControl;
begin
  HelpBtn.Down := False;
  Screen.Cursor := crDefault;
  c := frControlAtPos(Self, Point(X, Y));
  if (c <> nil) and (c <> HelpBtn) then
    Application.HelpCommand(HELP_CONTEXTPOPUP, c.Tag);
end;


initialization
  frRichForm := TfrRichForm.Create(nil);
  SRichEdit := TRichEdit.Create(nil);
  with SRichEdit do
  begin
    Parent := frRichForm;
    Visible := False;
    WordWrap := False;
    Width := 1024;
  end;
  frRegisterObject(TfrRichView, frRichForm.Image1.Picture.Bitmap,
    LoadStr(SInsRichObject), frRichForm);

finalization
  SRichEdit.Free;
  frRichForm.Free;
  frRichForm := nil;


end.

