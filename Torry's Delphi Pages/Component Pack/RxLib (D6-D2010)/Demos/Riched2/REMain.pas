unit REMain;

{$I RX.INC}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls, ClipBrd,
  RxRichEd, rxSpeedBar, RXSpin, RXCombos, rxClipMon, RxMenus, rxAppEvent, ImgList,
  Mask;

type
  TMainForm = class(TForm)
    MainMenu: TRxMainMenu;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    FilePrintItem: TMenuItem;
    FileExitItem: TMenuItem;
    EditUndoItem: TMenuItem;
    EditCutItem: TMenuItem;
    EditCopyItem: TMenuItem;
    EditPasteItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    Ruler: TPanel;
    FontDialog: TFontDialog;
    FirstInd: TLabel;
    LeftInd: TLabel;
    RulerLine: TBevel;
    RightInd: TLabel;
    Editor: TRxRichEdit;
    StatusBar: TStatusBar;
    ToolbarImages: TImageList;
    InsertMenu: TMenuItem;
    InsertImageItem: TMenuItem;
    InsertObjectItem: TMenuItem;
    EditRedoItem: TMenuItem;
    FormatMenu: TMenuItem;
    FormatFontItem: TMenuItem;
    FormatParagraphItem: TMenuItem;
    N3: TMenuItem;
    EditFindItem: TMenuItem;
    EditReplaceItem: TMenuItem;
    ColorMenu: TRxPopupMenu;
    EditPasteSpecial: TMenuItem;
    SpeedBar: TSpeedBar;
    FormatBar: TSpeedBar;
    SpeedbarSection1: TSpeedbarSection;
    SpeedbarSection2: TSpeedbarSection;
    NewBtn: TSpeedItem;
    OpenBtn: TSpeedItem;
    SaveBtn: TSpeedItem;
    PrintBtn: TSpeedItem;
    CutBtn: TSpeedItem;
    CopyBtn: TSpeedItem;
    PasteBtn: TSpeedItem;
    UndoBtn: TSpeedItem;
    RedoBtn: TSpeedItem;
    FindBtn: TSpeedItem;
    FontName: TFontComboBox;
    FontSize: TRxSpinEdit;
    SpeedbarSection3: TSpeedbarSection;
    BoldBtn: TSpeedItem;
    ItalicBtn: TSpeedItem;
    UnderlineBtn: TSpeedItem;
    ColorBtn: TSpeedItem;
    LeftBtn: TSpeedItem;
    CenterBtn: TSpeedItem;
    RightBtn: TSpeedItem;
    SubscriptBtn: TSpeedItem;
    SuperscriptBtn: TSpeedItem;
    BulletsBtn: TSpeedItem;
    N5: TMenuItem;
    EditObjPropsItem: TMenuItem;
    EditPopupMenu: TRxPopupMenu;
    CutItm: TMenuItem;
    CopyItm: TMenuItem;
    PasteItm: TMenuItem;
    BackgroundBtn: TSpeedItem;
    BackgroundMenu: TRxPopupMenu;
    N6: TMenuItem;
    ProtectedItem: TMenuItem;
    DisabledItem: TMenuItem;
    HiddenItem: TMenuItem;
    EditFindNextItem: TMenuItem;
    EditSelectAllItem: TMenuItem;
    FileSaveSelItem: TMenuItem;
    App: TAppEvents;
    procedure SelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FilePrint(Sender: TObject);
    procedure FileExit(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure SelectFont(Sender: TObject);
    procedure RulerResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BoldButtonClick(Sender: TObject);
    procedure ItalicButtonClick(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure AlignButtonClick(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure UnderlineButtonClick(Sender: TObject);
    procedure BulletsButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
    procedure FormShow(Sender: TObject);
    procedure RichEditChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InsertObject(Sender: TObject);
    procedure EditRedo(Sender: TObject);
    procedure InsertBitmap(Sender: TObject);
    procedure FormatParaAttributes(Sender: TObject);
    procedure EditorURLClick(Sender: TObject; const URLText: String;
      Button: TMouseButton);
    procedure FormActivate(Sender: TObject);
    procedure EditFindClick(Sender: TObject);
    procedure EditReplaceClick(Sender: TObject);
    procedure MainMenuGetImageIndex(Sender: TMenu; Item: TMenuItem;
      State: TMenuOwnerDrawState; var ImageIndex: Integer);
    procedure MainMenuMeasureItem(Sender: TMenu; Item: TMenuItem;
      var Width, Height: Integer);
    procedure ColorMenuPopup(Sender: TObject);
    procedure ColorMenuDrawItem(Sender: TMenu; Item: TMenuItem;
      Rect: TRect; State: TMenuOwnerDrawState);
    procedure SubscriptClick(Sender: TObject);
    procedure EditPasteSpecialClick(Sender: TObject);
    procedure EditObjPropsItemClick(Sender: TObject);
    procedure EditPopupMenuGetImageIndex(Sender: TMenu; Item: TMenuItem;
      State: TMenuOwnerDrawState; var ImageIndex: Integer);
    procedure BackgroundMenuPopup(Sender: TObject);
    procedure ProtectedItemClick(Sender: TObject);
    procedure DisabledItemClick(Sender: TObject);
    procedure EditorProtectChange(Sender: TObject; StartPos,
      EndPos: Integer; var AllowChange: Boolean);
    procedure HiddenItemClick(Sender: TObject);
    procedure EditFindNextItemClick(Sender: TObject);
    procedure EditorTextNotFound(Sender: TObject; const FindText: string);
    procedure EditSelectAll(Sender: TObject);
    procedure FileSaveSelected(Sender: TObject);
  private
    FFileName: string;
    FUpdating: Boolean;
    FDragOfs: Integer;
    FLineOfs: Integer;
    FLineDC: HDC;
    FLinePen: HPen;
    FLineVisible: Boolean;
    FDragging: Boolean;
    FProtectChanging: Boolean;
    FClipboardMonitor: TClipboardMonitor;
    FOpenPictureDialog: TOpenDialog;
    function IndentToRuler(Indent: Integer; IsRight: Boolean): Integer;
    function RulerToIndent(RulerPos: Integer; IsRight: Boolean): Integer;
    procedure DrawLine;
    procedure CalcLineOffset(Control: TControl);
    function CurrText: TRxTextAttributes;
    procedure SetFileName(const FileName: String);
    procedure EditFindDialogClose(Sender: TObject; Dialog: TFindDialog);
    procedure ColorItemClick(Sender: TObject);
    procedure BackgroundItemClick(Sender: TObject);
    procedure CheckFileSave;
    procedure SetupRuler;
    procedure SetEditRect;
    procedure UpdateCursorPos;
    procedure FocusEditor;
    procedure ClipboardChanged(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure PerformFileOpen(const AFileName: string);
    procedure SetModified(Value: Boolean);
  end;

var
  MainForm: TMainForm;

implementation

uses
  RxGIF, {$IFDEF RX_D3} ExtDlgs, {$IFNDEF CBUILDER} Jpeg, {$ENDIF}{$ENDIF}
  RxShell, rxMaxMin, RichEdit, ShellAPI, rxVclUtils, ParaFmt;

{$R *.DFM}

const
  RulerAdj = 4/3;
  GutterWid: Integer = 6;
  UndoNames: array[TUndoName] of string =
    ('', 'typing', 'delete', 'drag and drop', 'cut', 'paste');
  ColorValues: array [0..16] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
    clWindowText);
  BackValues: array [0..16] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
    clWindow);

function ColorName(Color: TColor): string;
begin
  if (Color = clWindowText) or (Color = clWindow) or (Color = clDefault) then
    Result := 'Automatic'
  else begin
    Result := ColorToString(Color);
    if Pos('cl', Result) = 1 then Delete(Result, 1, 2);
  end;
end;

function TMainForm.IndentToRuler(Indent: Integer; IsRight: Boolean): Integer;
var
  R: TRect;
  P: TPoint;
begin
  Indent := Trunc(Indent * RulerAdj);
  with Editor do begin
    SendMessage(Handle, EM_GETRECT, 0, Longint(@R));
    if IsRight then begin
      P := R.BottomRight;
      P.X := P.X - Indent;
    end
    else begin
      P := R.TopLeft;
      P.X := P.X + Indent;
    end;
    P := ClientToScreen(P);
  end;
  with Ruler do P := ScreenToClient(P);
  Result := P.X;
end;

function TMainForm.RulerToIndent(RulerPos: Integer; IsRight: Boolean): Integer;
var
  R: TRect;
  P: TPoint;
begin
  P.Y := 0; P.X := RulerPos;
  with Ruler do P := ClientToScreen(P);
  with Editor do begin
    P := ScreenToClient(P);
    SendMessage(Handle, EM_GETRECT, 0, Longint(@R));
    if IsRight then
      Result := R.BottomRight.X - P.X
    else
      Result := P.X - R.TopLeft.X;
  end;
  Result := Trunc(Result / RulerAdj);
end;

procedure TMainForm.FocusEditor;
begin
  with Editor do if CanFocus then SetFocus;
end;

procedure TMainForm.SelectionChange(Sender: TObject);
begin
  with Editor.Paragraph do
  try
    FUpdating := True;
    FirstInd.Left := IndentToRuler(FirstIndent, False) - (FirstInd.Width div 2);
    LeftInd.Left := IndentToRuler(LeftIndent + FirstIndent, False) - (LeftInd.Width div 2);
    RightInd.Left := IndentToRuler(RightIndent, True) - (RightInd.Width div 2);
    BoldBtn.Down := fsBold in CurrText.Style;
    ItalicBtn.Down := fsItalic in CurrText.Style;
    UnderlineBtn.Down := fsUnderline in CurrText.Style;
    BulletsBtn.Down := Boolean(Numbering);
    SuperscriptBtn.Down := CurrText.SubscriptStyle = ssSuperscript;
    SubscriptBtn.Down := CurrText.SubscriptStyle = ssSubscript;
    FontSize.AsInteger := CurrText.Size;
    FontName.FontName := CurrText.Name;
    ProtectedItem.Checked := CurrText.Protected;
    DisabledItem.Checked := CurrText.Disabled;
    HiddenItem.Checked := CurrText.Hidden;
    case Ord(Alignment) of
      0: LeftBtn.Down := True;
      1: RightBtn.Down := True;
      2: CenterBtn.Down := True;
    end;
    UpdateCursorPos;
  finally
    FUpdating := False;
  end;
end;

function TMainForm.CurrText: TRxTextAttributes;
begin
  if Editor.SelLength > 0 then Result := Editor.SelAttributes
  else Result := Editor.WordAttributes;
end;

procedure TMainForm.SetFileName(const FileName: String);
begin
  FFileName := FileName;
  Editor.Title := ExtractFileName(FileName);
  Caption := Format('%s - %s', [ExtractFileName(FileName), Application.Title]);
end;

procedure TMainForm.CheckFileSave;
var
  SaveResp: Integer;
begin
  if not Editor.Modified then Exit;
  SaveResp := MessageDlg(Format('Save changes to %s?', [FFileName]),
    mtConfirmation, mbYesNoCancel, 0);
  try
    case SaveResp of
      mrYes: FileSave(Self);
      mrNo: {Nothing};
      mrCancel: Abort;
    end;
  finally
    FocusEditor;
  end;
end;

procedure TMainForm.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(0, 0);
  Inc(P.X, FLineOfs);
  with P, Editor do begin
    MoveToEx(FLineDC, X, Y, nil);
    LineTo(FLineDC, X, Y + ClientHeight);
  end;
end;

procedure TMainForm.SetupRuler;
var
  I: Integer;
  S: String;
begin
  SetLength(S, 201);
  I := 1;
  while I < 200 do begin
    S[I] := #9;
    S[I + 1] := '|';
    Inc(I, 2);
  end;
  Ruler.Caption := S;
end;

procedure TMainForm.SetEditRect;
var
  R: TRect;
  Offs: Integer;
begin
  with Editor do begin
    if SelectionBar then Offs := 3 else Offs := 0;
    R := Rect(GutterWid + Offs, 0, ClientWidth - GutterWid, ClientHeight);
    SendMessage(Handle, EM_SETRECT, 0, Longint(@R));
  end;
end;

{ Event Handlers }

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
const
{$IFDEF RX_D3}
 {$IFNDEF CBUILDER}
  SPictureFilter = '%s|%s|%s|%s';
 {$ELSE}
  SPictureFilter = '%s|%s|%s';
 {$ENDIF}
{$ELSE}
  SPictureFilter = '%s|%s|%s';
{$ENDIF}
begin
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  SetFileName('Untitled');
  SetupRuler;
  HandleNeeded;
  SelectionChange(Self);
{$IFDEF RX_D3}
  Editor.OnCloseFindDialog := EditFindDialogClose;
  FOpenPictureDialog := TOpenPictureDialog.Create(Self);
{$ELSE}
  FOpenPictureDialog := TOpenDialog.Create(Self);
{$ENDIF}
  with FOpenPictureDialog do
    Filter := Format(SPictureFilter, [
      GraphicFilter(TBitmap),
      GraphicFilter(TMetafile),
{$IFDEF RX_D3}
  {$IFNDEF CBUILDER}
      GraphicFilter(TJPEGImage),
  {$ENDIF}
{$ENDIF}
      GraphicFilter(TGIFImage)
    ]);
  with ColorMenu.Items do begin
    while Count > 0 do Items[Count - 1].Free;
    for I := 0 to 16 do begin
      Item := NewItem(ColorName(ColorValues[I]), scNone,
        False, True, ColorItemClick, 0, '');
      Item.RadioItem := True;
      Item.Tag := ColorValues[I];
      Add(Item);
    end;
  end;
  with BackgroundMenu.Items do begin
    while Count > 0 do Items[Count - 1].Free;
    for I := 0 to 16 do begin
      Item := NewItem(ColorName(BackValues[I]), scNone,
        False, True, BackgroundItemClick, 0, '');
      Item.RadioItem := True;
      Item.Tag := BackValues[I];
      Add(Item);
    end;
  end;
  FClipboardMonitor := TClipboardMonitor.Create(Self);
  FClipboardMonitor.OnChange := ClipboardChanged;
  SuperscriptBtn.Enabled := RichEditVersion >= 2;
  SubscriptBtn.Enabled := RichEditVersion >= 2;
  BackgroundBtn.Enabled := RichEditVersion >= 2;
  DisabledItem.Enabled := RichEditVersion >= 2;
  HiddenItem.Enabled := RichEditVersion >= 2;
end;

procedure TMainForm.ShowHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then begin
    StatusBar.SimplePanel := True;
    StatusBar.SimpleText := Application.Hint;
  end
  else StatusBar.SimplePanel := False;
end;

procedure TMainForm.FileNew(Sender: TObject);
begin
  CheckFileSave;
  SetFileName('Untitled');
  FProtectChanging := True;
  try
    Editor.Lines.Clear;
    Editor.Modified := False;
    Editor.ReadOnly := False;
    SetModified(False);
    with Editor do begin
      DefAttributes.Assign(Font);
      SelAttributes.Assign(Font);
    end;
    SelectionChange(nil);
  finally
    FProtectChanging := False;
  end;
end;

procedure TMainForm.PerformFileOpen(const AFileName: string);
begin
  FProtectChanging := True;
  try
    Editor.Lines.LoadFromFile(AFileName);
  finally
    FProtectChanging := False;
  end;
  SetFileName(AFileName);
  Editor.SetFocus;
  Editor.Modified := False;
  SetModified(False);
end;

procedure TMainForm.FileOpen(Sender: TObject);
begin
  CheckFileSave;
  if OpenDialog.Execute then begin
    PerformFileOpen(OpenDialog.FileName);
    Editor.ReadOnly := ofReadOnly in OpenDialog.Options;
  end;
end;

procedure TMainForm.FileSave(Sender: TObject);
begin
  if FFileName = 'Untitled' then
    FileSaveAs(Sender)
  else begin
    Editor.Lines.SaveToFile(FFileName);
    Editor.Modified := False;
    SetModified(False);
    RichEditChange(nil);
  end;
end;

procedure TMainForm.FileSaveAs(Sender: TObject);
begin
  if SaveDialog.Execute then begin
    Editor.Lines.SaveToFile(SaveDialog.FileName);
    SetFileName(SaveDialog.FileName);
    Editor.Modified := False;
    SetModified(False);
    RichEditChange(nil);
  end;
  FocusEditor;
end;

procedure TMainForm.FileSaveSelected(Sender: TObject);
begin
  if SaveDialog.Execute then begin
    with Editor do
    try
      StreamMode := [smSelection];
      Lines.SaveToFile(SaveDialog.FileName);
    finally
      StreamMode := [];
    end;
    RichEditChange(nil);
  end;
  FocusEditor;
end;

procedure TMainForm.FilePrint(Sender: TObject);
begin
  if PrintDialog.Execute then Editor.Print(FFileName);
end;

procedure TMainForm.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditUndo(Sender: TObject);
begin
  Editor.Undo;
  RichEditChange(nil);
  SelectionChange(nil);
end;

procedure TMainForm.EditRedo(Sender: TObject);
begin
  Editor.Redo;
  RichEditChange(nil);
  SelectionChange(nil);
end;

procedure TMainForm.EditCut(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TMainForm.EditCopy(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TMainForm.EditPaste(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TMainForm.EditPasteSpecialClick(Sender: TObject);
begin
  try
    Editor.PasteSpecialDialog;
  finally
    FocusEditor;
  end;
end;

procedure TMainForm.HelpAbout(Sender: TObject);
begin
  WinAbout('RichEdit 2.0 Control Demo', 'RX Library Demo Program');
  FocusEditor;
end;

procedure TMainForm.SelectFont(Sender: TObject);
begin
  FontDialog.Font.Assign(Editor.SelAttributes);
  if FontDialog.Execute then CurrText.Assign(FontDialog.Font);
  FocusEditor;
end;

procedure TMainForm.RulerResize(Sender: TObject);
begin
  RulerLine.Width := Ruler.ClientWidth - (RulerLine.Left * 2);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  SetEditRect;
  SelectionChange(Sender);
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  SetEditRect;
end;

procedure TMainForm.BoldButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if BoldBtn.Down then
    CurrText.Style := CurrText.Style + [fsBold]
  else
    CurrText.Style := CurrText.Style - [fsBold];
end;

procedure TMainForm.ItalicButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if ItalicBtn.Down then
    CurrText.Style := CurrText.Style + [fsItalic]
  else
    CurrText.Style := CurrText.Style - [fsItalic];
end;

procedure TMainForm.UnderlineButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if UnderlineBtn.Down then
    CurrText.Style := CurrText.Style + [fsUnderline]
  else
    CurrText.Style := CurrText.Style - [fsUnderline];
end;

procedure TMainForm.SubscriptClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if SuperscriptBtn.Down then
    CurrText.SubscriptStyle := ssSuperscript
  else if SubscriptBtn.Down then
    CurrText.SubscriptStyle := ssSubscript
  else
    CurrText.SubscriptStyle := ssNone;
end;

procedure TMainForm.FontSizeChange(Sender: TObject);
begin
  if FUpdating then Exit;
  if FontSize.AsInteger > 0 then CurrText.Size := FontSize.AsInteger;
end;

procedure TMainForm.AlignButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  Editor.Paragraph.Alignment := TParaAlignment(TComponent(Sender).Tag);
end;

procedure TMainForm.FontNameChange(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Name := FontName.FontName;
end;

procedure TMainForm.BulletsButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  Editor.Paragraph.Numbering := TRxNumbering(BulletsBtn.Down);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  try
    CheckFileSave;
  except
    CanClose := False;
  end;
end;

{ Ruler Indent Dragging }

procedure TMainForm.CalcLineOffset(Control: TControl);
var
  P: TPoint;
begin
  with Control do P := ClientToScreen(Point(0, 0));
  P := Editor.ScreenToClient(P);
  FLineOfs := P.X + FDragOfs;
end;

procedure TMainForm.RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragOfs := (TControl(Sender).Width div 2);
  TControl(Sender).Left := Max(0, TControl(Sender).Left + X - FDragOfs);
  FLineDC := GetDCEx(Editor.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  FLinePen := SelectObject(FLineDC, CreatePen(PS_DOT, 1, ColorToRGB(clWindowText)));
  SetROP2(FLineDC, R2_XORPEN);
  CalcLineOffset(TControl(Sender));
  DrawLine;
  FDragging := True;
end;

procedure TMainForm.RulerItemMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging then begin
    DrawLine;
    TControl(Sender).Left := Min(Max(0, TControl(Sender).Left + X - FDragOfs),
      Ruler.ClientWidth - FDragOfs * 2);
    CalcLineOffset(TControl(Sender));
    DrawLine;
  end;
end;

procedure TMainForm.FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  Editor.Paragraph.FirstIndent := Max(0, RulerToIndent(FirstInd.Left + FDragOfs,
    False));
  LeftIndMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TMainForm.LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  if FLineVisible then DrawLine;
  DeleteObject(SelectObject(FLineDC, FLinePen));
  ReleaseDC(Editor.Handle, FLineDC);
  Editor.Paragraph.LeftIndent := Max(-Editor.Paragraph.FirstIndent,
    RulerToIndent(LeftInd.Left + FDragOfs, False) -
    Editor.Paragraph.FirstIndent);
  SelectionChange(Sender);
end;

procedure TMainForm.RightIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  if FLineVisible then DrawLine;
  DeleteObject(SelectObject(FLineDC, FLinePen));
  ReleaseDC(Editor.Handle, FLineDC);
  Editor.Paragraph.RightIndent := Max(0, RulerToIndent(RightInd.Left + FDragOfs,
    True));
  SelectionChange(Sender);
end;

procedure TMainForm.UpdateCursorPos;
var
  CharPos: TPoint;
begin
  CharPos := Editor.CaretPos;
  StatusBar.Panels[0].Text := Format('Line: %3d  Col: %3d',
    [CharPos.Y + 1, CharPos.X + 1]);
  { update the status of the cut and copy command }
  CopyBtn.Enabled := Editor.SelLength > 0;
  EditCopyItem.Enabled := CopyBtn.Enabled;
  CopyItm.Enabled := CopyBtn.Enabled;
  CutBtn.Enabled := EditCopyItem.Enabled;
  CutItm.Enabled := CutBtn.Enabled;
  FileSaveSelItem.Enabled := CopyBtn.Enabled;
  EditCutItem.Enabled := EditCopyItem.Enabled;
  EditObjPropsItem.Enabled := Editor.SelectionType = [stObject];
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  Res: TResourceStream;
begin
  UpdateCursorPos;
  DragAcceptFiles(Handle, True);
  RichEditChange(nil);
  FocusEditor;
  ClipboardChanged(nil);
  { check if we should load a file from the command line }
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    PerformFileOpen(ParamStr(1))
  else begin
    StartWait;
    try
      Res := TResourceStream.Create(HInstance, 'RX.RTF', 'RTF');
      try
        Editor.Lines.LoadFromStream(Res);
        SetFileName('Untitled');
        Editor.Modified := False;
        SetModified(False);
      finally
        Res.Free;
      end;
    except
      { ignore exceptions }
    end;
    StopWait;
  end;
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then begin
      Application.BringToFront;
      CheckFileSave;
      PerformFileOpen(CFileName);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TMainForm.RichEditChange(Sender: TObject);
begin
  SetModified(Editor.Modified);
  { Undo }
  UndoBtn.Enabled := Editor.CanUndo;
  EditUndoItem.Enabled := UndoBtn.Enabled;
  EditUndoItem.Caption := '&Undo ' + UndoNames[Editor.UndoName];
  { Redo }
  EditRedoItem.Enabled := Editor.CanRedo;
  RedoBtn.Enabled := EditRedoItem.Enabled;
  EditRedoItem.Caption := '&Redo ' + UndoNames[Editor.RedoName];
end;

procedure TMainForm.SetModified(Value: Boolean);
begin
  if Value then StatusBar.Panels[1].Text := 'Modified'
  else StatusBar.Panels[1].Text := '';
end;

procedure TMainForm.ClipboardChanged(Sender: TObject);
var
  E: Boolean;
begin
  { check to see if we can paste what's on the clipboard }
  E := Editor.CanPaste;
  PasteBtn.Enabled := E;
  EditPasteItem.Enabled := E;
  EditPasteSpecial.Enabled := E;
  PasteItm.Enabled := E;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  { remove ourselves from the viewer chain }
  FClipboardMonitor.Free;
end;

procedure TMainForm.InsertObject(Sender: TObject);
begin
  Editor.InsertObjectDialog;
end;

procedure TMainForm.EditObjPropsItemClick(Sender: TObject);
begin
  Editor.ObjectPropertiesDialog;
end;

procedure TMainForm.InsertBitmap(Sender: TObject);
var
  Pict: TPicture;
begin
  with FOpenPictureDialog do begin
    if Execute then begin
      Pict := TPicture.Create;
      try
        Pict.LoadFromFile(FileName);
        Clipboard.Assign(Pict);
        Editor.PasteFromClipboard;
      finally
        Pict.Free;
      end;
    end;
  end;
end;

procedure TMainForm.FormatParaAttributes(Sender: TObject);
begin
  FormatParagraph(Editor.Paragraph);
  FocusEditor;
end;

procedure TMainForm.EditorURLClick(Sender: TObject; const URLText: String;
  Button: TMouseButton);
begin
  if Button = mbLeft then
    ShellExecute(Handle, nil, PChar(URLText), nil, nil, SW_SHOW);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  FocusEditor;
end;

procedure TMainForm.EditFindClick(Sender: TObject);
begin
  with Editor do FindDialog(SelText);
end;

procedure TMainForm.EditFindNextItemClick(Sender: TObject);
begin
  if not Editor.FindNext then Beep;
  FocusEditor;
end;

procedure TMainForm.EditReplaceClick(Sender: TObject);
begin
  with Editor do ReplaceDialog(SelText, '');
end;

procedure TMainForm.EditorTextNotFound(Sender: TObject;
  const FindText: string);
begin
  MessageDlg(Format('Text "%s" not found.', [FindText]), mtWarning,
    [mbOk], 0);
end;

procedure TMainForm.EditFindDialogClose(Sender: TObject; Dialog: TFindDialog);
begin
  FocusEditor;
end;

procedure TMainForm.MainMenuGetImageIndex(Sender: TMenu; Item: TMenuItem;
  State: TMenuOwnerDrawState; var ImageIndex: Integer);
begin
  if Item.Tag >= 0 then ImageIndex := Item.Tag;
end;

procedure TMainForm.MainMenuMeasureItem(Sender: TMenu; Item: TMenuItem;
  var Width, Height: Integer);
begin
  if Item.Caption <> '-' then Height := 19;
end;

procedure TMainForm.EditPopupMenuGetImageIndex(Sender: TMenu;
  Item: TMenuItem; State: TMenuOwnerDrawState; var ImageIndex: Integer);
begin
  if (Item = CutItm) or (Item = CopyItm) or (Item = PasteItm) then
    ImageIndex := Item.Tag;
end;

procedure TMainForm.ColorMenuPopup(Sender: TObject);
var
  I: Integer;
  C: TColor;
begin
  C := CurrText.Color;
  for I := 0 to 16 do
    ColorMenu.Items[I].Checked := (C = ColorValues[I]);
end;

procedure TMainForm.BackgroundMenuPopup(Sender: TObject);
var
  I: Integer;
  C: TColor;
begin
  C := CurrText.BackColor;
  for I := 0 to 16 do
    BackgroundMenu.Items[I].Checked := (C = BackValues[I]);
end;

procedure TMainForm.ColorItemClick(Sender: TObject);
begin
  with Sender as TMenuItem do begin
    Checked := True;
    CurrText.Color := Tag;
  end;
end;

procedure TMainForm.BackgroundItemClick(Sender: TObject);
begin
  with Sender as TMenuItem do begin
    Checked := True;
    CurrText.BackColor := Tag;
  end;
end;

procedure TMainForm.ColorMenuDrawItem(Sender: TMenu; Item: TMenuItem;
  Rect: TRect; State: TMenuOwnerDrawState);
begin
  TRxPopupMenu(Sender).DefaultDrawItem(Item, Rect, State);
  Inc(Rect.Left, LoWord(GetMenuCheckMarkDimensions) + 6);
  Rect.Right := Rect.Left + 20;
  InflateRect(Rect, 0, -3);
  with TRxPopupMenu(Sender).Canvas do begin
    Brush.Color := clMenuText;
    FrameRect(Rect);
    InflateRect(Rect, -1, -1);
    Brush.Color := Item.Tag;
    FillRect(Rect);
  end;
end;

procedure TMainForm.ProtectedItemClick(Sender: TObject);
begin
  if FUpdating then Exit;
  FProtectChanging := True;
  try
    CurrText.Protected := not CurrText.Protected;
    ProtectedItem.Checked := CurrText.Protected;
  finally
    FProtectChanging := False;
  end;
end;

procedure TMainForm.DisabledItemClick(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Disabled := not CurrText.Disabled;
  DisabledItem.Checked := CurrText.Disabled;
end;

procedure TMainForm.HiddenItemClick(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Hidden := not CurrText.Hidden;
  HiddenItem.Checked := CurrText.Hidden;
end;

procedure TMainForm.EditorProtectChange(Sender: TObject; StartPos,
  EndPos: Integer; var AllowChange: Boolean);
begin
  AllowChange := FProtectChanging;
end;

procedure TMainForm.EditSelectAll(Sender: TObject);
begin
  Editor.SelectAll;
end;

end.