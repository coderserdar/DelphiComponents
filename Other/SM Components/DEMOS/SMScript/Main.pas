unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynHighlighterGeneral, SynHighlighterJScript, SynEditHighlighter,
  SynHighlighterVBScript, SynEdit, ExtCtrls, SMScript, ComCtrls, Buttons,
  StdCtrls, SynCompletionProposal, AngleLbl, TreeNT,
  SMPanel, Menus;

type
  TfrmPackage = class(TForm)
    pnlToolbar: TPanel;
    Editor: TSynEdit;
    SynVBScriptSyn: TSynVBScriptSyn;
    SynJScriptSyn: TSynJScriptSyn;
    SynGeneralSyn: TSynGeneralSyn;
    SMScriptExecutor: TSMScriptExecutor;
    StatusBar: TStatusBar;
    btnRunScript: TSpeedButton;
    Splitter: TSplitter;
    SynAutoComplete: TSynAutoComplete;
    imgGutter: TImageList;
    SynCompletionProposal: TSynCompletionProposal;
    SplitterFunc: TSplitter;
    imgScript: TImageList;
    imgProcFunc: TImageList;
    pnlErrors: TSMFramePanel;
    lblErrors: TAngleLabel;
    lbErrors: TListBox;
    pnlTemplate: TSMFramePanel;
    pcTemplates: TPageControl;
    tsFunctions: TTabSheet;
    lbScriptFuncs: TListBox;
    pmEditor: TPopupMenu;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    miSepar1: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSepar2: TMenuItem;
    miSelectAll: TMenuItem;
    miSepar3: TMenuItem;
    miSaveAs: TMenuItem;
    miLoad: TMenuItem;
    miSepar4: TMenuItem;
    miShowErrors: TMenuItem;
    miShowTemplate: TMenuItem;
    cbLanguage: TComboBox;
    procedure EditorStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure btnRunScriptClick(Sender: TObject);
    procedure EditorSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    function SynCompletionProposalPaintItem(AKey: String; ACanvas: TCanvas;
      X, Y: Integer): Boolean;
    procedure SynCompletionProposalCodeCompletion(var Value: String;
      Shift: TShiftState);
    procedure lbScriptFuncsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure EditorDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure EditorDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbScriptFuncsDblClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure pmEditorPopup(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure pnlToolbarResize(Sender: TObject);
  private
    { Private declarations }
    CodeIsParsed: Boolean;

    procedure LoadScriptFuncs;
    procedure DrawScriptFunc(IsSelected: Boolean; AKey: string; ACanvas: TCanvas; Obj, X, Y: Integer);
    function GetScriptFunc(Value: string): string;
  public
    { Public declarations }
  end;

var
  frmPackage: TfrmPackage;

implementation

{$R *.DFM}

procedure TfrmPackage.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  ptCaret: TPoint;
begin
  inherited;

  ptCaret := Editor.CaretXY;
  if (ptCaret.X > 0) and (ptCaret.Y > 0) then
    StatusBar.Panels[0].Text := Format(' %6d:%3d ', [ptCaret.Y, ptCaret.X])
  else
    StatusBar.Panels[0].Text := '';
  if Editor.Modified then
    StatusBar.Panels[1].Text := 'Изменен'
  else
    StatusBar.Panels[1].Text := '';
  if Editor.InsertMode then
    StatusBar.Panels[2].Text := 'Вставка'
  else
    StatusBar.Panels[2].Text := 'Замена';
  StatusBar.Panels[3].Text := SMScriptExecutor.LanguageStr
end;

procedure TfrmPackage.btnRunScriptClick(Sender: TObject);
var
  err: TSMSEError;
  strProcName: string;
  IsCanceled: Boolean;
begin
  btnRunScript.Enabled := False;

  inherited;

  lbErrors.Items.Clear;
  Editor.InvalidateLine(SMScriptExecutor.LastError.Line);

  try
    if not CodeIsParsed then
    begin
      SMScriptExecutor.AddCode(Editor.Lines.Text);
      SMScriptExecutor.Prepare;
      SMScriptExecutor.ParseModules;
      CodeIsParsed := True;
    end;

    if CodeIsParsed then
    begin
      strProcName := 'main';

      IsCanceled := False;

      if not IsCanceled then
        SMScriptExecutor.Run(strProcName, NULL);//ExecuteStatement(Editor.Lines.Text);
    end
  except
    on E: Exception do
    begin
      lbErrors.Items.Add(E.Message);
      err := SMScriptExecutor.LastError;
      if Assigned(err) then
      begin
        lbErrors.Items.Add('Error in line ' + IntToStr(err.Line) + ', col ' + intToStr(err.Column));
        lbErrors.Items.Add(err.Number + ' ' + err.Source);
        lbErrors.Items.Add('Description: ' + err.Description);
        lbErrors.Items.Add('Message: ' + err.Text);

        Editor.InvalidateLine(SMScriptExecutor.LastError.Line);
        Editor.CaretXY := Point(err.Column+1, err.Line);
      end
    end;
  end;

  btnRunScript.Enabled := True;
end;

procedure TfrmPackage.EditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  err: TSMSEError;
begin
  inherited;

  err := SMScriptExecutor.LastError;
  if Assigned(err) then
    if (err.Line = Line) then
    begin
      Special := True;
      BG := clRed
    end
end;

procedure TfrmPackage.FormShow(Sender: TObject);
begin
  inherited;

  case SMScriptExecutor.Language of
    slVBScript: Editor.Highlighter := SynVBScriptSyn;
    slJavaScript: Editor.Highlighter := SynJScriptSyn;
  else
    Editor.Highlighter := SynGeneralSyn;
  end;

  if (lbScriptFuncs.Items.Count = 0) then
    LoadScriptFuncs;

//  Editor.SetFocus
end;

procedure TfrmPackage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;

  if (Key = VK_F9) then
    btnRunScript.Click 
end;

procedure TfrmPackage.FormCreate(Sender: TObject);
begin
  inherited;

  CodeIsParsed := False;
end;

procedure TfrmPackage.EditorChange(Sender: TObject);
begin
  inherited;

  CodeIsParsed := False;
end;

procedure TfrmPackage.DrawScriptFunc(IsSelected: Boolean; AKey: string; ACanvas: TCanvas; Obj, X, Y: Integer);
var
  i, j, intWidth: Integer;
  s: string;
begin
  {draw icon}
  if (SMScriptExecutor.Language = slCustom) then
    i := 3
  else
    i := Ord(SMScriptExecutor.Language);
  imgScript.Draw(ACanvas, X, Y, i);
  intWidth := imgScript.Width + 2;

  if IsSelected then
    ACanvas.Font.Color := clHighlightText
  else
    ACanvas.Font.Color := clBlack;

  {draw a "function" word}
  i := Pos(' ', AKey);
  if (i > 0) then
  begin
    s := Copy(AKey, 1, i);
    ACanvas.TextOut(X + intWidth, Y, s);
    intWidth := intWidth + ACanvas.TextWidth(s);
    AKey := Copy(AKey, i, Length(AKey));
  end;

  {draw a name of function}
  ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];

  i := Pos('(', AKey);
  j := Pos(#9, AKey);
  if (j <= 0) then
    j := Length(AKey)+1;

  if (i <= 0) then
    i := j;
  s := Copy(AKey, 1, i-1);
  ACanvas.TextOut(X + intWidth, Y, s);
  Inc(intWidth, ACanvas.TextWidth(s));

  ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];

  {draw function parameters}
  s := Copy(AKey, i, j-i);
  ACanvas.TextOut(X + intWidth, Y, s);
  Inc(intWidth, ACanvas.TextWidth(s));

  {draw function help string}
  if IsSelected then
    ACanvas.Font.Color := clYellow
  else
    ACanvas.Font.Color := clBlue;
  ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic];
  ACanvas.TextOut(X + intWidth + 10, Y, Copy(AKey, j+1, Length(AKey)-j));
  ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
end;

function TfrmPackage.SynCompletionProposalPaintItem(AKey: String;
  ACanvas: TCanvas; X, Y: Integer): Boolean;
var
  i: Integer;
begin
  inherited;

  with SynCompletionProposal do
  begin
    i := ItemList.IndexOf(AKey);
    DrawScriptFunc((Position = i), AKey, ACanvas, LongInt(lbScriptFuncs.Items.Objects[i]), X, Y);
  end;

  Result := True;
end;

function TfrmPackage.GetScriptFunc(Value: string): string;
var
  i: Integer;
begin
  i := Pos(' ', Value);
  if (i <= 0) then
    i := 0;
  Result := Copy(Value, i+1, Length(Value));

  i := Pos('(', Result);
  if (i <= 0) then
    i := Length(Result)+1;
  Result := Copy(Result, 1, i-1);
end;

procedure TfrmPackage.SynCompletionProposalCodeCompletion(
  var Value: String; Shift: TShiftState);
begin
  inherited;

  Value := GetScriptFunc(Value);
end;

procedure TfrmPackage.LoadScriptFuncs;
var
  i: Integer;
  strDescr: string;
begin
  inherited;

  if (SMScriptExecutor.LanguageStr = 'JAVASCRIPT') then
    strDescr := 'JScript'
  else
  if (SMScriptExecutor.LanguageStr = 'RSLSCRIPT') then
    strDescr := 'RSLSrvt'
  else
    strDescr := SMScriptExecutor.LanguageStr;
  SMScriptExecutor.LoadScriptFunctions(strDescr + '.dll', lbScriptFuncs.Items, True);

  for i := 0 to lbScriptFuncs.Items.Count-1 do
    lbScriptFuncs.Items[i] := 'function ' + lbScriptFuncs.Items[i];

  SynCompletionProposal.ItemList.Clear;
  SynCompletionProposal.ItemList.AddStrings(lbScriptFuncs.Items);
end;

procedure TfrmPackage.lbScriptFuncsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  inherited;

  lbScriptFuncs.Canvas.FillRect(Rect);
  DrawScriptFunc((odSelected in State),
                 lbScriptFuncs.Items[Index],
                 lbScriptFuncs.Canvas,
                 LongInt(lbScriptFuncs.Items.Objects[Index]),
                 Rect.Left,
                 Rect.Top);
end;

procedure TfrmPackage.EditorDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;

  Accept := (not Editor.ReadOnly) and
            (Source = lbScriptFuncs)
end;

procedure TfrmPackage.EditorDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  inherited;

  Editor.CaretXY := Editor.PixelsToRowColumn(Point(X, Y));
  lbScriptFuncsDblClick(Source);
end;

procedure TfrmPackage.lbScriptFuncsDblClick(Sender: TObject);
begin
  inherited;

  if (Sender = lbScriptFuncs) then
    Editor.SelText := GetScriptFunc(lbScriptFuncs.Items[lbScriptFuncs.ItemIndex])
end;

procedure TfrmPackage.miUndoClick(Sender: TObject);
const
  strAllWildCard = 'All files (*.*)|*.*';
begin
  inherited;

  case TMenuItem(Sender).Tag of
    1: Editor.Undo;
    2: Editor.Redo;
    3: Editor.CutToClipboard;
    4: Editor.CopyToClipboard;
    5: Editor.PasteFromClipboard;
    6: Editor.Lines.Clear;
    7: Editor.SelectAll;
    8: begin
         with TSaveDialog.Create(Application) do
           try
             Filter := strAllWildCard;

             if Execute then
               Editor.Lines.SaveToFile(FileName);
           finally
             Free
           end;
       end;
    9: begin
         with TOpenDialog.Create(Application) do
           try
             Filter := strAllWildCard;

             if Execute then
             begin
               Editor.Lines.LoadFromFile(FileName);
               Editor.Modified := True;
             end
           finally
             Free
           end
       end;
   10: pnlErrors.Visible := (Sender as TMenuItem).Checked;
   11: pnlTemplate.Visible := (Sender as TMenuItem).Checked;
  end;
end;

procedure TfrmPackage.pmEditorPopup(Sender: TObject);
begin
  inherited;

  miShowErrors.Checked := pnlErrors.Visible;
  miShowTemplate.Checked := pnlTemplate.Visible;
end;

procedure TfrmPackage.cbLanguageChange(Sender: TObject);
begin
  SMScriptExecutor.LanguageStr := UpperCase(cbLanguage.Text);

  lbScriptFuncs.Items.Clear;
  FormShow(Sender);
end;

procedure TfrmPackage.pnlToolbarResize(Sender: TObject);
begin
  btnRunScript.Left := pnlToolbar.ClientRect.Right - btnRunScript.Width - 10
end;

end.
