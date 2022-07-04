unit PlusLns;

{ TPlusMemo.Lines property editor
  TPlusLinesProperty is a property editor for design time entry of formatted text
  in a TPlusMemo (v6)
  This file must be accompanied by PlusLns.dfm, which is the form
  resource file for this property editor (TFrmPlusLines) }

{ © Electro-Concept Mauricie, 1997-2002 }

{$DEFINE PlusLns}

{UCONVERT}
  {$IFDEF PlusLnsClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

uses
{$IFDEF pmClx}
  QDialogs, QMenus, QTypes, QControls, QStdCtrls, QExtCtrls, QButtons, QForms, QGraphics, PlusMemoClx,

{$ELSE}
  Windows, Messages, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Menus, ExtCtrls, PlusMemo, Buttons,
{$ENDIF}
  Classes;

type
  TFrmPlusLines = class(TForm)
    MainMenu1: TMainMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Save1: TMenuItem;
    PanToolbar: TPanel;
    SpeedBtnUnderline: TSpeedButton;
    SpeedBtnItalic: TSpeedButton;
    SpeedBtnBold: TSpeedButton;
    SpeedBtnPaste: TSpeedButton;
    SpeedBtnCopy: TSpeedButton;
    SpeedBtnCut: TSpeedButton;
    SpeedBtnSearch: TSpeedButton;
    SpeedBtnSave: TSpeedButton;
    SpeedBtnOpen: TSpeedButton;
    Edit1: TMenuItem;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    SaveAs1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    Search1: TMenuItem;
    Setup1: TMenuItem;
    SpeedBtnHighlight: TSpeedButton;
    PanStatus: TPanel;
    LblOffset: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    Toolbar1: TMenuItem;
    SpeedBtnAltFont: TSpeedButton;
    FindDialog1: TFindDialog;
    Undo1: TMenuItem;
    N8: TMenuItem;
    BtnUndo: TSpeedButton;
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    LblPar: TLabel;
    BtnApply: TBitBtn;
    BtnEnd: TBitBtn;
    BtnStart: TBitBtn;
    BtnRedo: TSpeedButton;
    Redo1: TMenuItem;
    N1: TMenuItem;
    Bold1: TMenuItem;
    Italic1: TMenuItem;
    Underline1: TMenuItem;
    Highlight1: TMenuItem;
    Alternatefont1: TMenuItem;
    FontDialog1: TFontDialog;
    MnuFont: TMenuItem;
    MnuAltFont: TMenuItem;
    N3: TMenuItem;
    procedure Cut1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure SpeedBtnBoldClick(Sender: TObject);
    procedure SpeedBtnItalicClick(Sender: TObject);
    procedure SpeedBtnUnderlineClick(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure SpeedBtnHighlightClick(Sender: TObject);
    procedure PlusMemo1StyleChange(Sender: TObject);
    procedure PlusMemo1SelMove(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Toolbar1Click(Sender: TObject);
    procedure SpeedBtnAltFontClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FindNext1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure PlusMemo1Change(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnEndClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnRedoClick(Sender: TObject);
    procedure Bold1Click(Sender: TObject);
    procedure Italic1Click(Sender: TObject);
    procedure Underline1Click(Sender: TObject);
    procedure Highlight1Click(Sender: TObject);
    procedure Alternatefont1Click(Sender: TObject);
    procedure MnuFontClick(Sender: TObject);
    procedure MnuAltFontClick(Sender: TObject);
  private
    { Private-déclarations }
    EditFile: AnsiString;
    FontChanged, AltFontChanged: Boolean;
    OldFontChanged, OldAltFontChanged: TNotifyEvent;
    procedure PlusMemo1FontChange(Sender: TObject);
    procedure PlusMemo1AltFontChange(Sender: TObject);
 public
    MyMemo   : TPlusMemo;
    PlusMemo1: TPlusMemo;
    OrgTxt   : string;
    procedure LoadFile(const fname: AnsiString);
    { Public-déclarations }
  end;

var
  FrmPlusLines: TFrmPlusLines;

implementation

{$R *.DFM}
{$IFDEF pmClx}
uses PMSupportClx, SysUtils;
{$ELSE}
uses PMSupport, SysUtils;
{$ENDIF}

const maxtextlen = 32767;

var FileStream: TFileStream;
procedure TFrmPlusLines.LoadFile(const fname: AnsiString);
begin
  EditFile:= fname;
  FileStream:= TFileStream.Create(fname, fmOpenRead);
  try
    PlusMemo1.Lines.LoadFromStream(FileStream);
  finally
    FileStream.Destroy
    end;
  PlusMemo1SelMove(Self);
  PlusMemo1StyleChange(Self)
end;


procedure TFrmPlusLines.Cut1Click(Sender: TObject);
begin
  PlusMemo1.CutToClipboard
end;

procedure TFrmPlusLines.Paste1Click(Sender: TObject);
begin
  PlusMemo1.PasteFromClipboard
end;

procedure TFrmPlusLines.Copy1Click(Sender: TObject);
begin
  PlusMemo1.CopyToClipboard
end;

procedure TFrmPlusLines.Save1Click(Sender: TObject);
begin
  if EditFile='' then SaveAs1Click(Save1)
  else
    with PlusMemo1 do
      begin
        try
          FileStream:= TFileStream.Create(EditFile, fmCreate);
          Lines.SaveToStream(FileStream);
        finally
          FileStream.Destroy
        end;
      end
end;

procedure TFrmPlusLines.SaveAs1Click(Sender: TObject);
var ext: string;
begin
  if SaveDialog1.Execute then
    begin
      EditFile:= SaveDialog1.FileName;
      ext:= UpperCase(ExtractFileExt(EditFile));
      if (ext='') and (EditFile[Length(EditFile)]<>'.') then
          EditFile:= EditFile+'.TXT';
      Save1Click(SaveAs1)
    end
end;

procedure TFrmPlusLines.New1Click(Sender: TObject);
begin
  PlusMemo1.Clear;
  EditFile:= '';
end;

procedure TFrmPlusLines.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then  LoadFile(OpenDialog1.FileName);
end;

procedure TFrmPlusLines.SpeedBtnBoldClick(Sender: TObject);
begin
  PlusMemo1.SetBold;
  SpeedBtnBold.Down:= fsBold in PlusMemo1.SelStyle;
end;

procedure TFrmPlusLines.SpeedBtnItalicClick(Sender: TObject);
begin
  PlusMemo1.SetItalic;
  SpeedBtnItalic.Down:= fsItalic in PlusMemo1.SelStyle;
end;

procedure TFrmPlusLines.SpeedBtnUnderlineClick(Sender: TObject);
begin
  PlusMemo1.SetUnderline;
  SpeedBtnUnderline.Down:= fsUnderline in PlusMemo1.SelStyle;
end;

procedure TFrmPlusLines.SpeedBtnHighlightClick(Sender: TObject);
begin
  PlusMemo1.SetHighlight;
  SpeedBtnHighlight.Down:= fsHighlight in TPlusFontStyles(PlusMemo1.SelStyle)
end;

procedure TFrmPlusLines.SpeedBtnAltFontClick(Sender: TObject);
begin
  PlusMemo1.SetAltFont;
  SpeedBtnAltFont.Down:= fsAltFont in TPlusFontStyles(PlusMemo1.SelStyle)
end;

procedure TFrmPlusLines.SelectAll1Click(Sender: TObject);
begin
  PlusMemo1.SelectAll
end;

procedure TFrmPlusLines.PlusMemo1StyleChange(Sender: TObject);
var fs: TExtFontStyles;
begin
  fs:= PlusMemo1.SelStyle;
  SpeedBtnBold.Down:= fsBold in fs;
  SpeedBtnItalic.Down:= fsItalic in fs;
  SpeedBtnUnderline.Down:= fsUnderline in fs;
  SpeedBtnHighlight.Down:= fsHighlight in TPlusFontStyles(PlusMemo1.SelStyle);
  SpeedBtnAltFont.Down:= fsAltFont in TPlusFontStyles(PlusMemo1.SelStyle)
end;

procedure TFrmPlusLines.PlusMemo1SelMove(Sender: TObject);
begin
  with PlusMemo1 do
    begin
      LblOffset.Caption:= IntToStr(SelStart);
      LblPar.Caption   := IntToStr(SelPar+1);
    end
end;

procedure TFrmPlusLines.Find1Click(Sender: TObject);
begin
FindDialog1.Execute
end;


procedure TFrmPlusLines.Toolbar1Click(Sender: TObject);
begin
with PanToolbar do
  begin
    Visible:= not Visible;
    Toolbar1.Checked:= Visible
  end
end;

procedure TFrmPlusLines.FindDialog1Find(Sender: TObject);
begin
  Screen.Cursor:= crHourGlass;
  with FindDialog1 do
    if PlusMemo1.FindTxt(FindText, frDown in Options, frMatchCase in Options, frWholeWord in Options, False) then
        PlusMemo1.ScrollInView
    else ShowMessage('Search string not found');
  Screen.Cursor:= crDefault;
  SetFocus;
  FindNext1.Enabled:= True
end;

procedure TFrmPlusLines.FindNext1Click(Sender: TObject);
begin
  FindDialog1Find(FindNext1)
end;

procedure TFrmPlusLines.Undo1Click(Sender: TObject);
begin
  PlusMemo1.Undo
end;

procedure TFrmPlusLines.Edit1Click(Sender: TObject);
begin
  Undo1.Enabled:= PlusMemo1.CanUndo;
  Redo1.Enabled:= PlusMemo1.CanRedo
end;


procedure TFrmPlusLines.PlusMemo1Change(Sender: TObject);
begin
  BtnApply.Enabled:= True;
  BtnUndo.Enabled:= PlusMemo1.CanUndo;
  BtnRedo.Enabled:= PlusMemo1.CanRedo
end;

procedure TFrmPlusLines.BtnApplyClick(Sender: TObject);
begin
  if FontChanged then MyMemo.Font:= PlusMemo1.Font;
  if AltFontChanged then MyMemo.AltFont:= PlusMemo1.AltFont;
  FontChanged:= False;
  AltFontChanged:= False;
  BtnApply.Enabled:= False;
  MyMemo.BeginUpdate;  // so that it won't scroll to end
  MyMemo.Text:= PlusMemo1.Text;
  MyMemo.EndUpdate;
  //PlusMemo1.SetFocus
end;

procedure TFrmPlusLines.FormShow(Sender: TObject);
begin
  Caption:= 'Plus Lines Editor';
  OrgTxt:= MyMemo.Text;
  with PlusMemo1 do
    begin
      { we steal OnChange handlers of font, so save them to call later }
      OldFontChanged:= Font.OnChange;
      OldAltFontChanged:= AltFont.OnChange;

      Font.Assign(MyMemo.Font);
      AltFont.Assign(MyMemo.AltFont);
      SelStart:= 0;
      HighlightColor:= MyMemo.HighlightColor;
      HighlightBackgnd:= MyMemo.HighlightBackgnd;
      Color:= MyMemo.Color;
      Alignment:= MyMemo.Alignment;
      Justified:= MyMemo.Justified;
      Options:= MyMemo.Options;
      StaticFormat:= MyMemo.StaticFormat;
      if not StaticFormat then
        begin
          SpeedBtnBold.Enabled:= False;
          SpeedBtnItalic.Enabled:= False;
          SpeedBtnUnderline.Enabled:= False;
          SpeedBtnHighlight.Enabled:= False;
          SpeedBtnAltFont.Enabled:= False
        end;

      Text:= OrgTxt;
      ScrollTime:= 0;
      Font.OnChange:= PlusMemo1FontChange;
      AltFont.OnChange:= PlusMemo1AltFontChange;
      Modified:= False;
    end;
  BtnApply.Enabled:= False;
  PlusMemo1SelMove(Self);
  PlusMemo1StyleChange(Self)
end;

procedure TFrmPlusLines.BtnOkClick(Sender: TObject);
begin
  if BtnApply.Enabled then BtnApplyClick(BtnOk);
end;

procedure TFrmPlusLines.FormCreate(Sender: TObject);
begin
  PlusMemo1:= TPlusMemo.Create(Self);
  with PlusMemo1 do
    begin
      Parent:= Self;
      Align:= alClient;
      ActiveControl:= PlusMemo1;
      OnChange:= PlusMemo1Change;
      OnSelMove:= PlusMemo1SelMove;
      OnStyleChange:= PlusMemo1StyleChange;
      ScrollBars:= ssVertical
    end;
  {$IFDEF Linux}
  OpenDialog1.Filter:= 'Text files (*.txt)|All files (*.*)'  // Under Linux Filter does not work the same...
  {$ENDIF}
end;

procedure TFrmPlusLines.BtnCancelClick(Sender: TObject);
begin
  MyMemo.BeginUpdate;  // so that it won't scroll to end
  MyMemo.Text:= OrgTxt;
  MyMemo.EndUpdate;
end;

procedure TFrmPlusLines.BtnEndClick(Sender: TObject);
begin
with PlusMemo1 do
  begin
    SelLength:= 0;
    SelStart:= CharCount;
    ScrollInView;
    SetFocus
  end
end;

procedure TFrmPlusLines.BtnStartClick(Sender: TObject);
begin
  with PlusMemo1 do
    begin
      SelLength:= 0;
      SelStart:= 0;
      ScrollInView;
      SetFocus
    end
end;

procedure TFrmPlusLines.BtnRedoClick(Sender: TObject);
begin
  PlusMemo1.Redo
end;

procedure TFrmPlusLines.Bold1Click(Sender: TObject);
begin
  PlusMemo1.SetBold
end;

procedure TFrmPlusLines.Italic1Click(Sender: TObject);
begin
  PlusMemo1.SetItalic
end;

procedure TFrmPlusLines.Underline1Click(Sender: TObject);
begin
  PlusMemo1.SetUnderline
end;

procedure TFrmPlusLines.Highlight1Click(Sender: TObject);
begin
  PlusMemo1.SetHighlight
end;

procedure TFrmPlusLines.Alternatefont1Click(Sender: TObject);
begin
  PlusMemo1.SetAltFont
end;

procedure TFrmPlusLines.PlusMemo1FontChange(Sender: TObject);
begin
  FontChanged:= True;
  BtnApply.Enabled:= True;
  if Assigned(OldFontChanged) then OldFontChanged(Sender)
end;

procedure TFrmPlusLines.PlusMemo1AltFontChange(Sender: TObject);
begin
  AltFontChanged:= True;
  BtnApply.Enabled:= True;
  if Assigned(OldAltFontChanged) then OldAltFontChanged(Sender)
end;

procedure TFrmPlusLines.MnuFontClick(Sender: TObject);
begin
  FontDialog1.Font:= PlusMemo1.Font;
  if FontDialog1.Execute then PlusMemo1.Font:= FontDialog1.Font
end;

procedure TFrmPlusLines.MnuAltFontClick(Sender: TObject);
begin
  FontDialog1.Font:= PlusMemo1.AltFont;
  if FontDialog1.Execute then PlusMemo1.AltFont:= FontDialog1.Font
end;

end.

