unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus,
  SVFontCombos,
  RVStyle, RVScroll, RichView, RVEdit;

{==============================================================================}
{ RichEditor Demo
  Menu items disabling/enabling are not implemented here.

  The main idea: new styles are created and added to rvs.TextStyles when needed.
  The right place for this - rve.OnStyleConversion.
  Documents a saved as pair:
    FileName.rdf - RVF (RichViewFormat)
    FileName.rdf.Styles - ini-file with styles for this document.
{==============================================================================}

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    rve: TRichViewEdit;
    rvs: TRVStyle;
    cmbFont: TComboBox;
    cmbTextStyles: TComboBox;
    btnBold: TSpeedButton;
    btnItalic: TSpeedButton;
    btnUnderline: TSpeedButton;
    btnFont: TSpeedButton;
    btnLeft: TSpeedButton;
    btnCenter: TSpeedButton;
    btnRight: TSpeedButton;
    btnJustify: TSpeedButton;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    btnSaveAs: TSpeedButton;
    btnNew: TSpeedButton;
    cmbFontSize: TFontSizeComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mitOpen: TMenuItem;
    mitSave: TMenuItem;
    mitNew: TMenuItem;
    mitSaveAs: TMenuItem;
    N1: TMenuItem;
    mitExit: TMenuItem;
    Edit1: TMenuItem;
    mitUndo: TMenuItem;
    mitRedo: TMenuItem;
    N2: TMenuItem;
    mitCut: TMenuItem;
    mitCopy: TMenuItem;
    mitPaste: TMenuItem;
    mitDelete: TMenuItem;
    Format1: TMenuItem;
    mitBold: TMenuItem;
    mitItalic: TMenuItem;
    mitUnderline: TMenuItem;
    mitFont: TMenuItem;
    N3: TMenuItem;
    od: TOpenDialog;
    sd: TSaveDialog;
    fd: TFontDialog;
    procedure mitNewClick(Sender: TObject);
    procedure mitOpenClick(Sender: TObject);
    procedure mitSaveClick(Sender: TObject);
    procedure mitSaveAsClick(Sender: TObject);
    procedure mitExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure rveChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rveCurTextStyleChanged(Sender: TObject);
    procedure rveCurParaStyleChanged(Sender: TObject);
    procedure cmbFontClick(Sender: TObject);
    procedure cmbTextStylesClick(Sender: TObject);
    procedure rveStyleConversion(Sender: TRichViewEdit; StyleNo,
      UserData: Integer; AppliedToText: Boolean; var NewStyleNo: Integer);
    procedure mitUndoClick(Sender: TObject);
    procedure mitRedoClick(Sender: TObject);
    procedure mitCutClick(Sender: TObject);
    procedure mitCopyClick(Sender: TObject);
    procedure mitPasteClick(Sender: TObject);
    procedure mitDeleteClick(Sender: TObject);
    procedure mitBoldClick(Sender: TObject);
    procedure mitItalicClick(Sender: TObject);
    procedure mitUnderlineClick(Sender: TObject);
    procedure mitFontClick(Sender: TObject);
    procedure btnApplyParaClick(Sender: TObject);
    procedure cmbFontSizeClick(Sender: TObject);
    procedure cmbFontSizeKeyPress(Sender: TObject; var Key: Char);
    procedure cmbFontSizeExit(Sender: TObject);
  private
    { Private declarations }
    Modified: Boolean;
    FileName, FontName: String;
    IgnoreChanges: Boolean;
    FontSize: Integer;
    function SaveIfNeeded: Boolean;
    function Save: Boolean;
    function SaveAs: Boolean;
    procedure Open;
    procedure New;
    procedure UpdateFontButtons;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

// Parsmeters for ApplyStyleConversion
const
  DEMO_MAKEBOLD       = 1;
  DEMO_MAKEITALIC     = 2;
  DEMO_MAKEUNDERLINE  = 3;
  DEMO_APPLYFONTNAME  = 4;
  DEMO_APPLYFONT      = 5;
  DEMO_CLEARBOLD      = 6;
  DEMO_CLEARITALIC    = 7;
  DEMO_CLEARUNDERLINE = 8;
  DEMO_APPLYFONTSIZE  = 9;

{$R *.DFM}
{------------------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Filling text styles combobox
  cmbTextStyles.Items.Assign(rvs.TextStyles);
  // Filling font names combobox
  cmbFont.Items.Assign(Screen.Fonts);
  New;
end;
{------------------------------------------------------------------------------}
// data in editor were changed
procedure TForm1.rveChange(Sender: TObject);
begin
  Modified := True;
  StatusBar1.Panels[0].Text := 'Modified';
end;
{------------------------------------------------------------------------------}
// current text style was changed
procedure TForm1.rveCurTextStyleChanged(Sender: TObject);
var fi: TFontInfo;
begin
  IgnoreChanges := True;
  StatusBar1.Panels[1].Text := 'Style : '+IntToStr(rve.CurTextStyleNo);
  // Changing selection on combobox with styles:
  if rve.CurTextStyleNo<cmbTextStyles.Items.Count then
    cmbTextStyles.ItemIndex := rve.CurTextStyleNo
  else
    cmbTextStyles.ItemIndex := -1;
  // Changing selection on combobox with font names:
  fi := rvs.TextStyles[rve.CurTextStyleNo];
  cmbFont.ItemIndex :=
    cmbFont.Items.IndexOf(fi.FontName);
  // Changing combobox with font sizes:
  cmbFont.OnClick(nil);
  cmbFontSize.Text := IntToStr(fi.Size);
  // Checking font buttons and menu
  mitBold.Checked      := fsBold      in fi.Style;
  mitItalic.Checked    := fsItalic    in fi.Style;
  mitUnderline.Checked := fsUnderline in fi.Style;
  UpdateFontButtons;
  IgnoreChanges := False;
end;
{------------------------------------------------------------------------------}
procedure TForm1.UpdateFontButtons;
begin
  btnBold.Down      := mitBold.Checked;
  btnItalic.Down    := mitItalic.Checked;
  btnUnderline.Down := mitUnderline.Checked;
end;
{------------------------------------------------------------------------------}
// current paragraph style was changed
procedure TForm1.rveCurParaStyleChanged(Sender: TObject);
begin
  case rve.CurParaStyleNo of
    0:
      btnLeft.Down := True;
    1:
      btnCenter.Down := True;
    2:
      btnRight.Down := True;
    3:
      btnJustify.Down := True;    
  end;
end;
{------------------------------------------------------------------------------}
// applying standard text style
procedure TForm1.cmbTextStylesClick(Sender: TObject);
begin
  if not IgnoreChanges and (cmbTextStyles.ItemIndex<>-1) then
      rve.ApplyTextStyle(cmbTextStyles.ItemIndex);
  if Visible then
    rve.SetFocus;
end;
{------------------------------------------------------------------------------}
// applying font name
procedure TForm1.cmbFontClick(Sender: TObject);
begin
  if (cmbFont.ItemIndex<>-1) then begin
    cmbFontSize.FontName := cmbFont.Items[cmbFont.ItemIndex];
    if not IgnoreChanges then begin
      FontName := cmbFont.Items[cmbFont.ItemIndex];
      rve.ApplyStyleConversion(DEMO_APPLYFONTNAME);
    end;
  end;
  if Visible then
    rve.SetFocus;
end;
{------------------------------------------------------------------------------}
// applying font size
procedure TForm1.cmbFontSizeClick(Sender: TObject);
begin
  if (cmbFontSize.Text<>'') and not IgnoreChanges then begin
      FontSize := StrToIntDef(cmbFontSize.Text, 10);
      rve.ApplyStyleConversion(DEMO_APPLYFONTSIZE);
  end;
  if Visible then
    rve.SetFocus;
end;
{------------------------------------------------------------------------------}
// bold
procedure TForm1.mitBoldClick(Sender: TObject);
begin
  mitBold.Checked := not mitBold.Checked;
  if mitBold.Checked then
    rve.ApplyStyleConversion(DEMO_MAKEBOLD)
  else
    rve.ApplyStyleConversion(DEMO_CLEARBOLD)
end;
{------------------------------------------------------------------------------}
// italic
procedure TForm1.mitItalicClick(Sender: TObject);
begin
  mitItalic.Checked := not mitItalic.Checked;
  if mitItalic.Checked then
    rve.ApplyStyleConversion(DEMO_MAKEITALIC)
  else
    rve.ApplyStyleConversion(DEMO_CLEARITALIC)
end;
{------------------------------------------------------------------------------}
// underline
procedure TForm1.mitUnderlineClick(Sender: TObject);
begin
  mitUnderline.Checked := not mitUnderline.Checked;
  if mitUnderline.Checked then
    rve.ApplyStyleConversion(DEMO_MAKEUNDERLINE)
  else
    rve.ApplyStyleConversion(DEMO_CLEARUNDERLINE)
end;
{------------------------------------------------------------------------------}
// applying font
procedure TForm1.mitFontClick(Sender: TObject);
begin
  fd.Font.Assign(rvs.TextStyles[rve.CurTextStyleNo]);
  if fd.Execute then begin
    rve.ApplyStyleConversion(DEMO_APPLYFONT);
  end;
end;
{------------------------------------------------------------------------------}
// The heart of this demo: rve.OnStyleConversion
procedure TForm1.rveStyleConversion(Sender: TRichViewEdit; StyleNo,
  UserData: Integer; AppliedToText: Boolean; var NewStyleNo: Integer);
  {.......................................}
  function GetFontStyle(UserData: Integer): TFontStyle;
  begin
    Result := fsBold; // <-- avoiding warnings
    case UserData of
      DEMO_MAKEBOLD, DEMO_CLEARBOLD:
        Result := fsBold;
      DEMO_MAKEITALIC, DEMO_CLEARITALIC:
        Result := fsItalic;
      DEMO_MAKEUNDERLINE, DEMO_CLEARUNDERLINE:
        Result := fsUnderline;
    end;
  end;
  {.......................................}
var fs: TFontStyle;
begin
  case UserData of
    DEMO_MAKEBOLD,DEMO_MAKEITALIC,DEMO_MAKEUNDERLINE:
      begin
        fs := GetFontStyle(UserData);
        NewStyleNo := rvs.TextStyles.FindStyleWithFontStyle(StyleNo, [fs],[fs]);
        if NewStyleNo=-1 then begin
          rvs.TextStyles.Add;
          NewStyleNo := rvs.TextStyles.Count-1;
          rvs.TextStyles[NewStyleNo].Assign(rvs.TextStyles[StyleNo]);
          rvs.TextStyles[NewStyleNo].Style := rvs.TextStyles[NewStyleNo].Style+[fs];
        end;
      end;
    DEMO_CLEARBOLD,DEMO_CLEARITALIC,DEMO_CLEARUNDERLINE:
      begin
        fs := GetFontStyle(UserData);
        NewStyleNo := rvs.TextStyles.FindStyleWithFontStyle(StyleNo, [],[fs]);
        if NewStyleNo=-1 then begin
          rvs.TextStyles.Add;
          NewStyleNo := rvs.TextStyles.Count-1;
          rvs.TextStyles[NewStyleNo].Assign(rvs.TextStyles[StyleNo]);
          rvs.TextStyles[NewStyleNo].Style := rvs.TextStyles[NewStyleNo].Style-[fs];
        end;
      end;
    DEMO_APPLYFONTNAME:
      begin
        NewStyleNo := rvs.TextStyles.FindStyleWithFontName(StyleNo, FontName);
        if NewStyleNo=-1 then begin
          rvs.TextStyles.Add;
          NewStyleNo := rvs.TextStyles.Count-1;
          rvs.TextStyles[NewStyleNo].Assign(rvs.TextStyles[StyleNo]);
          rvs.TextStyles[NewStyleNo].FontName := FontName;
        end;
      end;
    DEMO_APPLYFONTSIZE:
      begin
        NewStyleNo := rvs.TextStyles.FindStyleWithFontSize(StyleNo, FontSize);
        if NewStyleNo=-1 then begin
          rvs.TextStyles.Add;
          NewStyleNo := rvs.TextStyles.Count-1;
          rvs.TextStyles[NewStyleNo].Assign(rvs.TextStyles[StyleNo]);
          rvs.TextStyles[NewStyleNo].Size := FontSize;
        end;
      end;
    DEMO_APPLYFONT:
      begin
        NewStyleNo := rvs.TextStyles.FindStyleWithFont(StyleNo, fd.Font);
        if NewStyleNo=-1 then begin
          rvs.TextStyles.Add;
          NewStyleNo := rvs.TextStyles.Count-1;
          rvs.TextStyles[NewStyleNo].Assign(rvs.TextStyles[StyleNo]);
          rvs.TextStyles[NewStyleNo].Assign(fd.Font);
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
// applying paragraph style
procedure TForm1.btnApplyParaClick(Sender: TObject);
begin
  rve.ApplyParaStyle(TSpeedButton(Sender).Tag);
end;
{------------------------------------------------------------------------------}
procedure TForm1.cmbFontSizeKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(Key)=VK_RETURN then begin
    Key := #0;
    cmbFontSizeClick(nil);
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.cmbFontSizeExit(Sender: TObject);
begin
  cmbFontSizeClick(nil);
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitUndoClick(Sender: TObject);
begin
  rve.Undo;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitRedoClick(Sender: TObject);
begin
  rve.Redo;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitCutClick(Sender: TObject);
begin
  rve.CutDef;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitCopyClick(Sender: TObject);
begin
  rve.CopyDef;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitPasteClick(Sender: TObject);
begin
  rve.Paste;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitDeleteClick(Sender: TObject);
begin
  rve.DeleteSelection;
end;
{------------------------------------------------------------------------------}
function TForm1.SaveIfNeeded: Boolean;
begin
  Result := True;
  if Modified then
    case Application.MessageBox('Save file now?','File was modified',
                                MB_ICONQUESTION or MB_YESNOCANCEL) of
      IDYES:
        Result := Save;
      IDNO:
        Result := True;
      IDCANCEL:
        Result := False;
    end;
end;
{------------------------------------------------------------------------------}
function TForm1.Save: Boolean;
begin
  if FileName='' then
    Result := SaveAs
  else begin
    rve.SaveRVF(FileName, False);
    rvs.SaveINI(FileName+'.styles', 'Styles');
    Modified := False;
    StatusBar1.Panels[0].Text := '';
    Result := True;
  end;
end;
{------------------------------------------------------------------------------}
function TForm1.SaveAs: Boolean;
begin
  if sd.Execute then begin
    FileName := sd.FileName;
    Result := Save;
    if Result then
      Caption := ExtractFileName(FileName) + '- RDemo';
    end
  else
    Result := False;
end;
{------------------------------------------------------------------------------}
procedure TForm1.Open;
begin
  if not SaveIfNeeded then exit;
  if od.Execute then begin
    FileName := od.FileName;
    rvs.LoadINI(FileName+'.styles', 'Styles');
    rve.LoadRVF(FileName);
    rve.Format;
    rveCurTextStyleChanged(nil);
    rveCurParaStyleChanged(nil);
    Modified := False;
    StatusBar1.Panels[0].Text := '';
    Caption := ExtractFileName(FileName) + '- RDemo';
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.New;
begin
  if not SaveIfNeeded then exit;
  FileName := '';
  Modified := False;
  StatusBar1.Panels[0].Text := '';
  Caption := 'Unnamed - RDemo';
  rve.Clear;
  rve.Format;
  // you can delete non default styles here...
  rveCurTextStyleChanged(nil);
  rveCurParaStyleChanged(nil);  
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitNewClick(Sender: TObject);
begin
  New;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitOpenClick(Sender: TObject);
begin
  Open;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitSaveClick(Sender: TObject);
begin
  Save;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitSaveAsClick(Sender: TObject);
begin
  SaveAs;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitExitClick(Sender: TObject);
begin
  Close;
end;
{------------------------------------------------------------------------------}
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := SaveIfNeeded;
end;

end.
