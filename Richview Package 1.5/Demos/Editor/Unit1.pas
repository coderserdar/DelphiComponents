unit Unit1;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle, RVScroll, RichView, RVEdit, PtblRV, RVMisc, CtrlImg, RVUndoStr, RVUni,
  {$IFDEF RICHVIEWDEF3}
  jpeg,
  {$ENDIF}
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  Clipbrd, StdCtrls, ExtCtrls, ComCtrls, Menus, OleCtnrs,
  RVTable, Buttons, CRVData,CRVFData, RVERVData, RVItem;

type
  TForm1 = class(TForm)
    RichViewEdit1: TRichViewEdit;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    mpdInsert: TMenuItem;
    mitPicture: TMenuItem;
    mpdComponent: TMenuItem;
    mitButtonComp: TMenuItem;
    mitEditBoxComp: TMenuItem;
    mitBreak: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    cmbText: TComboBox;
    cmbPara: TComboBox;
    mpdFile: TMenuItem;
    N1: TMenuItem;
    mitExit: TMenuItem;
    mpdBullet: TMenuItem;
    mitHelpIcon: TMenuItem;
    mitHelpSearchIcon: TMenuItem;
    mitPropertiesIcon: TMenuItem;
    mitSave: TMenuItem;
    SaveDialog1: TSaveDialog;
    N2: TMenuItem;
    mitClear: TMenuItem;
    mpdEdit: TMenuItem;
    mitCopy: TMenuItem;
    mitPaste: TMenuItem;
    mitCut: TMenuItem;
    mitDelete: TMenuItem;
    PasteAs1: TMenuItem;
    mitPasteAsText: TMenuItem;
    mitPasteAsMetafile: TMenuItem;
    mitPasteAsBitmap: TMenuItem;
    mitPasteAsRVF: TMenuItem;
    N3: TMenuItem;
    mitEditCheckpoint: TMenuItem;
    mitEditProps: TMenuItem;
    PopupMenu1: TPopupMenu;
    mitEditProp1: TMenuItem;
    mitEditCheckpoint1: TMenuItem;
    mpdHotspot: TMenuItem;
    mitAddImageHS: TMenuItem;
    mitAddTextHS: TMenuItem;
    mitSelectAll: TMenuItem;
    mpdMisc: TMenuItem;
    N4: TMenuItem;
    mitPrint: TMenuItem;
    FindDialog1: TFindDialog;
    N6: TMenuItem;

    mitSearch: TMenuItem;
    mitPasteAsOLE: TMenuItem;
    N8: TMenuItem;
    mitPreview: TMenuItem;
    mitSelectCurrentWord: TMenuItem;
    SpeedButton1: TSpeedButton;
    RVPrint1: TRVPrint;
    mpdRVFSaveOptions: TMenuItem;
    mpdRVFLoadOptions: TMenuItem;
    mitRVFSavePictureBodies: TMenuItem;
    mitRVFSaveControlBodies: TMenuItem;
    mitRVFSaveBinary: TMenuItem;
    mitRVFSaveBackground: TMenuItem;
    mitRVFIgnoreUnknownPictureFormats: TMenuItem;
    mitRVFIgnoreUnknownControls: TMenuItem;
    mitRVFConvertUnknownStylesto0: TMenuItem;
    mitRVFConvertTooLargeImageIndicesto0: TMenuItem;
    mitRVFLoadBackground: TMenuItem;
    mpdBackground: TMenuItem;
    mitBackNoBitmap: TMenuItem;
    mitBackStretched: TMenuItem;
    mitBackTiled: TMenuItem;
    mitBackTiledandScrolled: TMenuItem;
    mitBackCentered: TMenuItem;
    N5: TMenuItem;
    mitCheckpointList: TMenuItem;
    N9: TMenuItem;
    mitRemovePageBreak: TMenuItem;
    mitInsertPageBreak: TMenuItem;
    N10: TMenuItem;
    mitUndo: TMenuItem;
    mitRedo: TMenuItem;
    mitInsertFile: TMenuItem;
    mitPasteAsUnicodeText: TMenuItem;
    mitLoad: TMenuItem;
    mpdTable: TMenuItem;
    mitInserttable1: TMenuItem;
    mitInsertTable2: TMenuItem;
    N7: TMenuItem;
    mitMergeCells: TMenuItem;
    N13: TMenuItem;
    mitUmRows: TMenuItem;
    mitUmCols: TMenuItem;
    mitUmRowsandCols: TMenuItem;
    Insert1: TMenuItem;
    mitRowsAbove: TMenuItem;
    mitRowsBelow: TMenuItem;
    N14: TMenuItem;
    mitColsLeft: TMenuItem;
    mitColsRight: TMenuItem;
    Delete1: TMenuItem;
    mitDelRows: TMenuItem;
    mitDelColumns: TMenuItem;
    Unmerge1: TMenuItem;
    Split1: TMenuItem;
    mitSplitVertically: TMenuItem;
    mitSplitHorizontally: TMenuItem;
    mitInsertTable3: TMenuItem;
    mitInsertTable4: TMenuItem;
    psd: TPrinterSetupDialog;
    RVStyle1: TRVStyle;
    mitPasteAsRTF: TMenuItem;
    il: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure mitPictureClick(Sender: TObject);
    procedure mitButtonCompClick(Sender: TObject);
    procedure mitEditBoxCompClick(Sender: TObject);
    procedure RichViewEdit1CurParaStyleChanged(Sender: TObject);
    procedure RichViewEdit1CurTextStyleChanged(Sender: TObject);
    procedure cmbParaClick(Sender: TObject);
    procedure cmbTextClick(Sender: TObject);
    procedure mitBreakClick(Sender: TObject);
    procedure mitExitClick(Sender: TObject);
    procedure mitInsertBulletClick(Sender: TObject);
    procedure mitSaveClick(Sender: TObject);
    procedure mitClearClick(Sender: TObject);
    procedure mpdEditClick(Sender: TObject);
    procedure RichViewEdit1Select(Sender: TObject);
    procedure mitPasteAsBitmapClick(Sender: TObject);
    procedure mitPasteAsMetafileClick(Sender: TObject);
    procedure mitPasteAsTextClick(Sender: TObject);
    procedure mitPasteClick(Sender: TObject);
    procedure mitDeleteClick(Sender: TObject);
    procedure mitCutClick(Sender: TObject);
    procedure mitCopyClick(Sender: TObject);
    procedure mitEditCheckpointClick(Sender: TObject);
    procedure RichViewEdit1RVRightClickEx(Sender: TRichView; LineNo, Style,
      X, Y: Integer);
    procedure mitAddHSClick(Sender: TObject);
    procedure mitSelectAllClick(Sender: TObject);
    procedure mitEditPropsClick(Sender: TObject);
    procedure mitPrintClick(Sender: TObject);
    procedure mitRVFOptionsClick(Sender: TObject);
    procedure RichViewEdit1RVFPictureNeeded(Sender: TRichView; Name: String; Tag: Integer;
      var gr: TGraphic);
    procedure RichViewEdit1RVFControlNeeded(Sender: TRichView; Name: String; Tag: Integer;
      var ctrl: TControl);
    procedure RichViewEdit1RVFImageListNeeded(Sender: TRichView; ImageListTag: Integer;
      var il: TImageList);
    procedure mitSearchClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure mitCheckPointListClick(Sender: TObject);
    procedure mitPasteAsRVFClick(Sender: TObject);
    procedure mitPasteAsOLEClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mitPreviewClick(Sender: TObject);
    procedure RVPrint1PrintComponent(Sender: TRVPrint;
      PrintMe: TControl; var ComponentImage: TBitmap);
    procedure mitBackClick(Sender: TObject);
    procedure RichViewEdit1SaveComponentToFile(Sender: TRichView;
      Path: String; SaveMe: TPersistent; SaveFormat: TRVSaveFormat;
      var OutStr: String);
    procedure mitSelectCurrentWordClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure RichViewEdit1Jump(Sender: TObject; id: Integer);
    procedure RichViewEdit1StyleConversion(Sender: TRichViewEdit; StyleNo,
      UserData: Integer; AppliedToText: Boolean; var NewStyleNo: Integer);
    procedure RichViewEdit1Change(Sender: TObject);
    procedure mpdBackgroundClick(Sender: TObject);
    procedure mpdRVFSaveOptionsClick(Sender: TObject);
    procedure mpdRVFLoadOptionsClick(Sender: TObject);
    procedure mitInsertPageBreakClick(Sender: TObject);
    procedure mitRemovePageBreakClick(Sender: TObject);
    procedure RichViewEdit1URLNeeded(Sender: TRichView; id: Integer;
      var url: string);
    procedure mitUndoClick(Sender: TObject);
    procedure mitRedoClick(Sender: TObject);
    procedure mitInsertFileClick(Sender: TObject);
    procedure mitPasteAsUnicodeTextClick(Sender: TObject);
    procedure mitLoadClick(Sender: TObject);
    procedure mitInserttable1Click(Sender: TObject);
    procedure mitInsertTable2Click(Sender: TObject);
    procedure mitCellsOperationClick(Sender: TObject);
    procedure mpdTableClick(Sender: TObject);
    procedure mitInsertTable3Click(Sender: TObject);
    procedure mitInsertTable4Click(Sender: TObject);
    procedure RichViewEdit1RVMouseMove(Sender: TObject; id: Integer);
    procedure mitPasteAsRTFClick(Sender: TObject);
  private
    { Private declarations }
    procedure OnOleResize(Sender: TObject);
    procedure OnOleActions(Sender: TObject);
    procedure WMDisplayChange(var Message: TMessage{TWMDisplayChange}); message WM_DISPLAYCHANGE;
    procedure UpdateUndoMenu;
    procedure DisplayUnicodeWarning;
    procedure DisplayRTFWarning;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CPFrm, PropFrm, ListFrm, PreviewFrm;

{$R *.DFM}

{ This demo uses conditional defines from RV_Defs.inc (see include
  directive at the beginnning of this file)
  RICHVIEWDEF3 is defined, if there is Delphi3 or later or C++Builder 3 or later
  RICHVIEWDEF4 is defined, if there is Delphi4 or later
}

procedure TForm1.FormCreate(Sender: TObject);
begin
   Randomize;

   // Win2000 has good unicode font 'Arial Unicode MS'
   // If it does not exists in system, using Arial as Unicode font
   if Screen.Fonts.IndexOf('Arial Unicode MS')=-1 then
     RVStyle1.TextStyles[11].FontName := 'Arial';

   // This demo program is saved in Delphi 2, where Charset property is not available.
   // So assigning non-default charset here:
   {$IFDEF RICHVIEWCBDEF3} // For Delphi3+, C++Builder 3+
   RVStyle1.TextStyles[6].Charset := SYMBOL_CHARSET;
   {$ENDIF}

   // Item can have associated "tags" - integers or strings.
   // Comment next line to use integer tags
   RichViewEdit1.Options := RichViewEdit1.Options+[rvoTagsArePChars];

   cmbPara.Items.Assign(RVStyle1.ParaStyles);
   cmbText.Items.Assign(RVStyle1.TextStyles);


   RichViewEdit1.LoadRVF(ExtractFilePath(Application.ExeName)+'Readme.rvf');
   RichViewEdit1.Format;
   cmbPara.ItemIndex := RichViewEdit1.CurParaStyleNo;
   cmbText.ItemIndex := RichViewEdit1.CurTextStyleNo;
   UpdateUndoMenu;

end;
{---------------------------------------------------------------------}
procedure TForm1.UpdateUndoMenu;
var UndoType : TRVUndoType;
begin
  UndoType := RichViewEdit1.UndoAction;
  mitUndo.Enabled := UndoType<>rvutNone;
  if UndoType=rvutCustom then
    mitUndo.Caption := 'Undo '+RichViewEdit1.UndoName
  else
    mitUndo.Caption := 'Undo '+RVUndoTypeNamesEn[UndoType];

  UndoType := RichViewEdit1.RedoAction;
  mitRedo.Enabled := UndoType<>rvutNone;
  if UndoType=rvutCustom then
    mitRedo.Caption := 'Redo '+RichViewEdit1.RedoName
  else
    mitRedo.Caption := 'Redo '+RVUndoTypeNamesEn[UndoType];
end;
{---------------------------------------------------------------------}
procedure TForm1.DisplayUnicodeWarning;
var wasclear: Boolean;
begin
  wasclear := RichViewEdit1.ItemCount=0;
  // This method is called before loading Unicode
  // (when inserting Unicode, editor automatically switches to Unicode style,
  // according to RVStyle1.DefUnicodeStyle, if necessary)
  if not RVStyle1.TextStyles[RichViewEdit1.CurTextStyleNo].Unicode then
    Application.MessageBox('Loading/Inserting Unicode data using non-Unicode text style.'#13+
                           'Text will be converted.'#13+
                           'Choose "Unicode" style in combo to use Unicode text style',
                           'Warning', MB_OK or MB_ICONEXCLAMATION);
  if wasclear then
    RichViewEdit1.Clear;
end;
{------------------------------------------------------------------------------}
procedure TForm1.DisplayRTFWarning;
var s: String;
    wasclear: Boolean;
begin
  wasclear := RichViewEdit1.ItemCount=0;
  s := '';
  case RichViewEdit1.RTFReadProperties.TextStyleMode of
    rvrsUseSpecified:
      s := 'All text will be imported without formatting.';
    rvrsUseClosest:
      s := 'Mapping RTF formatting to the most similar RichView style.';
    rvrsAddIfNeeded:
      s := 'Extracting all text formatting from RTF. Warning: new text styles will be added to the collection.';
  end;
  case RichViewEdit1.RTFReadProperties.ParaStyleMode of
    rvrsUseSpecified:
      s := s+#13#13'Properties of paragraphs in RTF will be ignored.';
    rvrsUseClosest:
      s := s+#13#13'Mapping RTF paragraph properties to the most similar RichView style.';
    rvrsAddIfNeeded:
      s := s+#13#13'Extracting all paragraph properties from RTF. Warning: new paragraph styles will be added to the collection.';
  end;
  s := s+#13#13'(See RichViewEdit1.RTFReadProperties)';
  Application.MessageBox(PChar(s),
                       'RTF Import Mode', MB_OK or MB_ICONINFORMATION);
  if wasclear then
    RichViewEdit1.Clear;
end;
{======================================================================}
{ Font and paragraph combos                                            }
{======================================================================}
procedure TForm1.RichViewEdit1CurParaStyleChanged(Sender: TObject);
begin
  if RichViewEdit1.CurParaStyleNo<cmbPara.Items.Count then
    cmbPara.ItemIndex := RichViewEdit1.CurParaStyleNo
  else
    cmbPara.ItemIndex := -1;
end;
{----------------------------------------------------------------------}
procedure TForm1.RichViewEdit1CurTextStyleChanged(Sender: TObject);
begin
  if RichViewEdit1.CurTextStyleNo<cmbText.Items.Count then
    cmbText.ItemIndex := RichViewEdit1.CurTextStyleNo
  else
    cmbText.ItemIndex := -1;
end;
{----------------------------------------------------------------------}
procedure TForm1.cmbParaClick(Sender: TObject);
begin
   RichViewEdit1.ApplyParaStyle(cmbPara.ItemIndex);
   RichViewEdit1.SetFocus;
end;
{----------------------------------------------------------------------}
procedure TForm1.cmbTextClick(Sender: TObject);
begin
   RichViewEdit1.ApplyTextStyle(cmbText.ItemIndex);
   RichViewEdit1.SetFocus;
end;
{ Event: raised in ApplyStyleConversion -------------------------------}
procedure TForm1.RichViewEdit1StyleConversion(Sender: TRichViewEdit;
  StyleNo, UserData: Integer; AppliedToText: Boolean;
  var NewStyleNo: Integer);
begin
  // Simple example of custom text style conversion
  case UserData of
    0:
      begin
        if StyleNo=Sender.Style.TextStyles.Count-1 then
          NewStyleNo := 0
        else
          NewStyleNo := StyleNo + 1;
      end;
  end;
end;
{----------------------------------------------------------------------}
procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  RichViewEdit1.ApplyStyleConversion(0);
end;
{======================================================================}
{ Main menu: "File"                                                    }
{======================================================================}
{ File|Load... --------------------------------------------------------}
procedure TForm1.mitLoadClick(Sender: TObject);
var CurTextStyleNo, CurParaStyleNo: Integer;
    r: Boolean;
begin
  OpenDialog1.Title := 'Loading & Import';
  OpenDialog1.Filter := 'RichView Format Files(*.rvf)|*.rvf|'+
                        'RTF Files (*.rtf)|*.rtf|'+
                        'Text Files - autodetect (*.txt)|*.txt|'+
                        'ANSI Text Files (*.txt)|*.txt|'+
                        'Unicode Text Files (*.txt)|*.txt';
  if OpenDialog1.Execute then begin
    Screen.Cursor := crHourglass;
    CurTextStyleNo := RichViewEdit1.CurTextStyleNo;
    CurParaStyleNo := RichViewEdit1.CurParaStyleNo;
    RichViewEdit1.Clear;
    RichViewEdit1.CurTextStyleNo := CurTextStyleNo;
    RichViewEdit1.CurParaStyleNo := CurParaStyleNo;
    case OpenDialog1.FilterIndex of
      1: // RVF
        begin
          r := RichViewEdit1.LoadRVF(OpenDialog1.FileName);
          if rvfwConvUnknownStyles in RichViewEdit1.RVFWarnings then
            Application.MessageBox('This file uses incompatible set of styles.'#13+
                                   'Probably this file was converted from RTF.'#13+
                                   'This demo cannot save and load styles to files',
                                   'Warning',
                                   MB_OK or MB_ICONWARNING)
        end;
      2: // RTF
        begin
          DisplayRTFWarning;
          r := RichViewEdit1.LoadRTF(OpenDialog1.FileName);
        end;
      3: // Text
        if RV_TestFileUnicode(OpenDialog1.FileName)=rvutYes then begin
          DisplayUnicodeWarning;
          r := RichViewEdit1.LoadTextW(OpenDialog1.FileName,CurTextStyleNo,CurParaStyleNo,False)
          end
        else
          r := RichViewEdit1.LoadText(OpenDialog1.FileName,CurTextStyleNo,CurParaStyleNo,False);
      4: // ANSI text
        r := RichViewEdit1.LoadText(OpenDialog1.FileName,CurTextStyleNo,CurParaStyleNo,False);
      5: // Unicode text
        begin
          DisplayUnicodeWarning;
          r := RichViewEdit1.LoadTextW(OpenDialog1.FileName,CurTextStyleNo,CurParaStyleNo,False)
        end;
      else
        r := False;
    end;
    Screen.Cursor := crDefault;
    if not r then
      Application.MessageBox('Error during loading', 'Error', 0);
    RichViewEdit1.Format;
    cmbPara.ItemIndex := RichViewEdit1.CurParaStyleNo;
    cmbText.ItemIndex := RichViewEdit1.CurTextStyleNo;
    UpdateUndoMenu;
  end;
end;
{ Event: picture needed while reading from RVF ------------------------}
procedure TForm1.RichViewEdit1RVFPictureNeeded(Sender: TRichView; Name: String;
                                               Tag: Integer; var gr: TGraphic);
begin
  gr := TBitmap.Create;
  gr.LoadFromFile(ExtractFilePath(Application.ExeName)+'default.bmp');
end;
{ Event: control needed while reading from RVF ------------------------}
procedure TForm1.RichViewEdit1RVFControlNeeded(Sender: TRichView; Name: String;
                                               Tag: Integer; var ctrl: TControl);
begin
  ctrl := TButton.Create(RichViewEdit1);
  TButton(ctrl).Caption := 'from file';
end;
{ Event: imagelist needed while reading from RVF ----------------------}
procedure TForm1.RichViewEdit1RVFImageListNeeded(Sender: TRichView;
                                                 ImageListTag: Integer;
                                                  var il: TImageList);
begin
  il := Self.il;
end;
{ File|Save... --------------------------------------------------------}
procedure TForm1.mitSaveClick(Sender: TObject);
  var r: Boolean;
begin
  SaveDialog1.Title := 'Save & Export';
  SaveDialog1.Filter := 'RichView Format files(*.rvf)|*.rvf|'+
                        'RTF Files (*.rtf)|*.rtf|'+
                        'Text (*.txt)|*.txt|'+
                        'Unicode Text (*.txt)|*.txt|'+
                        'Plain HTML (*.htm;*.html)|*.htm;*.html|'+
                        'HTML with CSS (*.htm;*.html)|*.htm;*.html';
  SaveDialog1.DefaultExt := 'rvf';
  if SaveDialog1.Execute then begin
    Screen.Cursor := crHourglass;
    case SaveDialog1.FilterIndex of
      1: // RVF
        r := RichViewEdit1.SaveRVF(SaveDialog1.FileName, False);
      2: // RTF
        r := RichViewEdit1.SaveRTF(SaveDialog1.FileName, False);
      3: // ANSI Text (byte per character)
        r := RichViewEdit1.SaveText(SaveDialog1.FileName, 80);
      4: // Unicode Text (2 bytes per character)
        r := RichViewEdit1.SaveTextW(SaveDialog1.FileName, 80);
      5: // HTML
        r := RichViewEdit1.SaveHTML(SaveDialog1.FileName,'Demo File','img', []);
      6: // HTML with CSS
        r := RichViewEdit1.SaveHTMLEx(SaveDialog1.FileName,'Demo File','img',
                                      '','','',[]);
      else
        r := False;
    end;
    Screen.Cursor := crDefault;
    if not r then
      Application.MessageBox('Error during saving', 'Error', 0);
  end;
end;
{ Event: saving controls in HTML --------------------------------------}
// Note: code below works normally in Internet Explorer
// Netscape does not support <INPUT> tags outside <FORM></FORM> tags
// (not tested with the latest versions of Netscape, though)
procedure TForm1.RichViewEdit1SaveComponentToFile(Sender: TRichView;
  Path: String; SaveMe: TPersistent; SaveFormat: TRVSaveFormat;
  var OutStr: String);
begin
  case SaveFormat of
   rvsfText:
      begin
        OutStr := '('+SaveMe.ClassName+')';
      end;
   rvsfHTML:
       begin
         if SaveMe is TButton then begin
           OutStr := '<INPUT type="button" value="'+TButton(SaveMe).Caption+'" '+
                     'onClick="alert(''Just a demo'')">';
           exit;
         end;
         if SaveMe is TEdit then begin
           OutStr := '<INPUT type="text" value="'+TEdit(SaveMe).Text+'">';
           exit;
         end;
       end;
   rvsfRTF:
      begin
        OutStr := '\plain\b ('+SaveMe.ClassName+')';
      end;
   end;
end;
{ Event: saving URLs in HTML ------------------------------------------}
procedure TForm1.RichViewEdit1URLNeeded(Sender: TRichView; id: Integer;
  var url: string);
begin
  url := '#'+IntToStr(id);
end;
{ File|Clear ----------------------------------------------------------}
procedure TForm1.mitClearClick(Sender: TObject);
begin
   RichViewEdit1.Clear;
   RichViewEdit1.Format;
   cmbPara.ItemIndex := RichViewEdit1.CurParaStyleNo;
   cmbText.ItemIndex := RichViewEdit1.CurTextStyleNo;
   UpdateUndoMenu;
end;
{ File|Print Preview --------------------------------------------------}
procedure TForm1.mitPreviewClick(Sender: TObject);
begin
  RVPrint1.AssignSource(RichViewEdit1);
  RVPrint1.FormatPages(rvdoALL);
  if RVPrint1.PagesCount>0 then begin
    frmPreview.rvpp.RVPrint := RVPrint1;
    frmPreview.Button1Click(nil); //  Show First Page
    frmPreview.ShowModal;
  end;
end;
{ File|Print on Default Printer ---------------------------------------}
procedure TForm1.mitPrintClick(Sender: TObject);
var PrintIt: Boolean;
begin
  {$IFDEF RICHVIEWDEF3}
  PrintIt := psd.Execute;
  {$ELSE}
  PrintIt := True;
  {$ENDIF}
  if PrintIt then begin
    RVPrint1.AssignSource(RichViewEdit1);
    RVPrint1.FormatPages(rvdoALL);
    if RVPrint1.PagesCount>0 then
      RVPrint1.Print('RichView Edit Demo',1,False);
  end;
end;
{ Event: making image of  controls for printing -----------------------}
procedure TForm1.RVPrint1PrintComponent(Sender: TRVPrint;
  PrintMe: TControl; var ComponentImage: TBitmap);
var r: TRect;
begin
  if PrintMe is TButton then
    ComponentImage := DrawButton(PrintMe as TButton)
  else if PrintMe is TEdit then
    ComponentImage := DrawEdit(PrintMe as TEdit)
  else if PrintMe is TOleContainer then begin
    ComponentImage := TBitmap.Create;
    ComponentImage.Width  := PrintMe.Width;
    ComponentImage.Height := PrintMe.Height;
    r := Bounds(0,0,PrintMe.Width,PrintMe.Height);
    with r do
      ComponentImage.Canvas.Rectangle(Left,Top,Right,Bottom);
    InflateRect(r,-1,-1);
    DrawText(ComponentImage.Canvas.Handle, PChar(TOleContainer(PrintMe).OleClassName),-1,
             r, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;
{ File|Exit -----------------------------------------------------------}
procedure TForm1.mitExitClick(Sender: TObject);
begin
  Close;
end;
{======================================================================}
{ Main menu: "Insert"                                                  }
{======================================================================}
{ Insert|File... ------------------------------------------------------}
procedure TForm1.mitInsertFileClick(Sender: TObject);
var r: Boolean;
begin
  OpenDialog1.Title := 'Inserting File';
  OpenDialog1.Filter := 'RichView Format Files(*.rvf)|*.rvf|'+
                        'RTF Files(*.rtf)|*.rtf|'+
                        'Text Files - autodetect (*.txt)|*.txt|'+
                        'ANSI Text Files (*.txt)|*.txt|'+
                        'Unicode Text Files (*.txt)|*.txt|'+
                        'OEM Text Files (*.txt)|*.txt';
  if OpenDialog1.Execute then begin
    Screen.Cursor := crHourglass;
    case OpenDialog1.FilterIndex of
      1: // RVF
        r := RichViewEdit1.InsertRVFFromFileEd(OpenDialog1.FileName);
      2: // RTF
        begin
          DisplayRTFWarning;
          r := RichViewEdit1.InsertRTFFromFileEd(OpenDialog1.FileName);
        end;
      3: // Text
        begin
          if RV_TestFileUnicode(OpenDialog1.FileName)=rvutYes then
            r := RichViewEdit1.InsertTextFromFileW(OpenDialog1.FileName)
          else
            r := RichViewEdit1.InsertTextFromFile(OpenDialog1.FileName);
        end;
      4: // ANSI Text
        r := RichViewEdit1.InsertTextFromFile(OpenDialog1.FileName);
      5: // Unicode Text
        r := RichViewEdit1.InsertTextFromFileW(OpenDialog1.FileName);
      6: // OEM Text
        r := RichViewEdit1.InsertOEMTextFromFile(OpenDialog1.FileName);
      else
        r := False;
    end;
    Screen.Cursor := crDefault;
    if not r then
      Application.MessageBox('Error reading file', 'Error',
                             MB_OK or MB_ICONSTOP);
  end;
end;
{ Insert|Picture... ---------------------------------------------------}
procedure TForm1.mitPictureClick(Sender: TObject);
var gr: TGraphic;
    ext: String;
begin
  OpenDialog1.Title := 'Inserting Image';
  {$IFDEF RICHVIEWDEF3}
  OpenDialog1.Filter := 'Graphics(*.bmp;*.wmf;*.emf;*.ico;*.jpg)|*.bmp;*.wmf;*.emf;*.ico;*.jpg|All(*.*)|*.*';
  {$ELSE}
  OpenDialog1.Filter := 'Graphics(*.bmp;*.wmf;*.emf;*.ico)|*.bmp;*.wmf;*.emf;*.ico|All(*.*)|*.*';
  {$ENDIF}
  if OpenDialog1.Execute then begin
    gr := nil;
    ext := UpperCase(ExtractFileExt(OpenDialog1.FileName));
    {$IFDEF RICHVIEWDEF3}
    if (ext='.JPG') or (ext='.JPEG') then
      gr := TJPegImage.Create
    else
   {$ENDIF}
    if (ext='.BMP')or(ext='.DIB') then
      gr := TBitmap.Create
    else if ext='.ICO' then
      gr := TIcon.Create
    else if (ext='.WMF') or (ext='.EMF') then
      gr := TMetafile.Create
    else
      Application.MessageBox(PChar('Format "'+ext+'" is not supported'), 'Error',
                             MB_OK or MB_ICONSTOP);
    if gr<>nil then begin
      gr.LoadFromFile(OpenDialog1.FileName);
      RichViewEdit1.InsertPicture('',gr,rvvaBaseLine);
    end;
  end;
end;
{ Insert|Component|Button ---------------------------------------------}
procedure TForm1.mitButtonCompClick(Sender: TObject);
var btn: TButton;
const Captions: array[0..9] of String =
       (
       'Help','Exit','Cancel','Ok','Close','Run','Options...','Minimize',
       'Hide','Show'
       );
begin
  btn := TButton.Create(Self);
  btn.Caption := Captions[Random(10)];
  RichViewEdit1.InsertControl('',btn,rvvaBaseline);
end;
{ Insert|Component|Edit Box -------------------------------------------}
procedure TForm1.mitEditBoxCompClick(Sender: TObject);
var edt: TEdit;
const Captions: array[0..9] of String =
       (
       '0','Hello','1','$0','2x2=4','really cool!','x<y','don'' turn around!!!',
       '(empty)','(full)'
       );
begin
  edt := TEdit.Create(Self);
  edt.Text := Captions[Random(10)];
  RichViewEdit1.InsertControl('',edt,rvvaBaseline);
end;

{ Insert|Bullet|"XXX" -------------------------------------------------}
procedure TForm1.mitInsertBulletClick(Sender: TObject);
begin
  RichViewEdit1.InsertBullet(TMenuItem(Sender).Tag, il);
end;
{ Insert|Hot Spot|"XXX" -----------------------------------------------}
procedure TForm1.mitAddHSClick(Sender: TObject);
begin
  RichViewEdit1.InsertHotSpot(TMenuItem(Sender).Tag, TMenuItem(Sender).Tag+2, il);
end;
{ Insert|Break --------------------------------------------------------}
procedure TForm1.mitBreakClick(Sender: TObject);
begin
   RichViewEdit1.InsertBreak(1, rvbsLine, clNone);
end;
{======================================================================}
{ Main menu : "Edit"                                                   }
{======================================================================}
{ Edit ----------------------------------------------------------------}
procedure TForm1.mpdEditClick(Sender: TObject);
begin
  mitPasteAsRTF.Enabled      := RichViewEdit1.CanPasteRTF;
  mitPasteAsText.Enabled     := Clipboard.HasFormat(CF_TEXT);
  mitPasteAsUnicodeText.Enabled := Clipboard.HasFormat(CF_UNICODETEXT);
  mitPasteAsMetafile.Enabled := Clipboard.HasFormat(CF_METAFILEPICT);
  mitPasteAsBitmap.Enabled   := Clipboard.HasFormat(CF_BITMAP);
  mitPasteAsRVF.Enabled      := RichViewEdit1.CanPasteRVF;
  mitPaste.Enabled           := RichViewEdit1.CanPaste;

  mitRemovePageBreak.Enabled := RichViewEdit1.PageBreaksBeforeItems[RichViewEdit1.CurItemNo];

  // You can edit properties only for item with caret.
  // We disable this item because otherwise user can think what he will
  // edit properties of all selected items.
  // More smart programs can determine if there is only one item is selected
  // and do not disable this item in this case
  mitEditProps.Enabled       := not RichViewEdit1.SelectionExists;
end;
{ Edit|Undo------------------------------------------------------------}
procedure TForm1.mitUndoClick(Sender: TObject);
begin
  RichViewEdit1.Undo;
end;
{ Edit|Redo -----------------------------------------------------------}
procedure TForm1.mitRedoClick(Sender: TObject);
begin
  RichViewEdit1.Redo;
end;
{ Edit|Cut ------------------------------------------------------------}
procedure TForm1.mitCutClick(Sender: TObject);
begin
  RichViewEdit1.CutDef;
end;
{ Edit|Copy -----------------------------------------------------------}
procedure TForm1.mitCopyClick(Sender: TObject);
begin
  RichViewEdit1.CopyDef;
end;
{ Edit|Paste ----------------------------------------------------------}
procedure TForm1.mitPasteClick(Sender: TObject);
begin
  RichViewEdit1.Paste;
end;
{ Edit|Paste As|RTF ---------------------------------------------------}
procedure TForm1.mitPasteAsRTFClick(Sender: TObject);
begin
  DisplayRTFWarning;
  RichViewEdit1.PasteRTF;
end;
{ Edit|Paste As|Text --------------------------------------------------}
procedure TForm1.mitPasteAsTextClick(Sender: TObject);
begin
  RichViewEdit1.PasteText;
end;
{ Edit|Paste As|Unicode Text ------------------------------------------}
procedure TForm1.mitPasteAsUnicodeTextClick(Sender: TObject);
begin
  RichViewEdit1.PasteTextW;
end;
{ Edit|Paste As|Bitmap ------------------------------------------------}
procedure TForm1.mitPasteAsBitmapClick(Sender: TObject);
begin
  RichViewEdit1.PasteBitmap(False);
end;
{ Edit|Paste As|Metafile ----------------------------------------------}
procedure TForm1.mitPasteAsMetafileClick(Sender: TObject);

begin
  RichViewEdit1.PasteMetafile(False);
end;
{ Edit|Paste As|RichView Format ---------------------------------------}
procedure TForm1.mitPasteAsRVFClick(Sender: TObject);
begin
  RichViewEdit1.PasteRVF;
end;
{ Edit|Delete ---------------------------------------------------------}
procedure TForm1.mitDeleteClick(Sender: TObject);
begin
  // Shortcut to this item is Ctrl+Del
  // If you make it Del, you will be unable to use del key in editor
  RichViewEdit1.DeleteSelection;
end;
{ Edit|Select All -----------------------------------------------------}
procedure TForm1.mitSelectAllClick(Sender: TObject);
begin
  { warning: SelectAll moves caret to the end of the text }
  RichViewEdit1.SelectAll;
  RichViewEdit1.SetFocus;
  RichViewEdit1.Invalidate;
end;
{ Another clipboard-related action ------------------------------------}
procedure TForm1.RichViewEdit1Select(Sender: TObject);
begin
  mitCopy.Enabled := RichViewEdit1.SelectionExists;
  mitCut.Enabled := mitCopy.Enabled;
  mitDelete.Enabled := mitCopy.Enabled;
end;
{ Edit| Insert Page Break----------------------------------------------}
procedure TForm1.mitInsertPageBreakClick(Sender: TObject);
begin
  RichViewEdit1.InsertPageBreak;
end;
{ Edit| Remove Page Break----------------------------------------------}
procedure TForm1.mitRemovePageBreakClick(Sender: TObject);
begin
  RichViewEdit1.RemoveCurrentPageBreak;
end;
{----------------------------------------------------------------------}
// This function is used in mitEditCheckpointClick() and mitEditPropsClick()
// to convert tag to string
function GetTagStr(Tag: Integer): String;
begin
  if (rvoTagsArePChars in Form1.RichViewEdit1.Options) then
    if Tag = 0 then
      Result := ''
    else
      Result := PChar(Tag)
  else
    Result := IntToStr(Tag);
end;
// This function is used in mitEditCheckpointClick() and mitEditPropsClick()
// to create valid tags from string TagStr (user input in edit box)
// If tags are PChars, it allocates new ansiz string and replaces all
// spaces with '_'.
// If tags are Integers, it convert TagStr to integer.
function MakeTag(TagStr: String): Integer;
var i: Integer;
    r: PChar;
begin
   if (TagStr<>'0') and (TagStr<>'') and (rvoTagsArePChars in Form1.RichViewEdit1.Options) then begin
     r := StrNew(PChar(TagStr));
     for i := 0 to StrLen(r)-1 do
       if r[i]=' ' then r[i]:='_';
       Result := Integer(r);
     end
   else
     Result := StrToIntDef(TagStr,0);
end;
{ Edit|Checkpoint... --------------------------------------------------}
procedure TForm1.mitEditCheckpointClick(Sender: TObject);
var CpNo, Tag: Integer;
    Name: String;
    CheckPointData: TCheckPointData;
    RaiseEvent: Boolean;
begin
  CheckPointData := RichViewEdit1.GetCurrentCheckpoint;
  if CheckPointData<>nil then begin
    RichViewEdit1.GetCheckpointInfo(CheckPointData,Tag,Name,RaiseEvent);
    CpNo := RichViewEdit1.GetCheckpointNo(CheckPointData);
    frmCp.lblStatus.Caption := 'Editing checkpoint #'+IntToStr(CpNo);
    frmCp.txtName.Text := Name;
    frmCp.txtTag.Text := GetTagStr(Tag);
    frmCp.btnOk.Caption := 'OK';
    frmCp.btnDelete.Enabled := True;
    end
  else begin
    frmCp.lblStatus.Caption := 'Checkpoint does not exist';
    frmCp.txtName.Text := '';
    frmCp.txtTag.Text := GetTagStr(0);
    frmCp.btnOk.Caption := 'Add';
    frmCp.btnDelete.Enabled := False;
  end;
  case frmCP.ShowModal of
    mrOk: { add new checkpoint or modify existed one }
      RichViewEdit1.SetCurrentCheckpointInfo(MakeTag(frmCp.txtTag.Text),
                                             frmCp.txtName.Text,False);
    mrYes: { delete checkpoint }
      RichViewEdit1.RemoveCurrentCheckpoint;
  end;
end;
{ Edit|Search... -------------------------------------}
procedure TForm1.mitSearchClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;
{ Edit|Select Current Word -------------------------------------}
procedure TForm1.mitSelectCurrentWordClick(Sender: TObject);
begin
  RichViewEdit1.SelectCurrentWord;
  // now you can do something with current word:
  // translate or spell check, for example...
end;
{ Edit|Current Item Properties... -------------------------------------}
procedure TForm1.mitEditPropsClick(Sender: TObject);
var s: String;
    Tag, Index: Integer;
    VAlign: TRVVAlign;
    ImageList: TImageList;
    gr: TGraphic;
    ctrl: TControl;
    BreakColor: TColor;
    BreakStyle: TRVBreakStyle; // <- not implemented
    BreakWidth: Byte;
begin
  frmProp.PageControl1.Visible := True;
  frmProp.tsBullet.TabVisible := False;
  frmProp.tsHotSpot.TabVisible := False;
  frmProp.tsPicture.TabVisible := False;
  frmProp.tsText.TabVisible := False;
  frmProp.tsComponent.TabVisible := False;
  frmProp.tsBreak.TabVisible := False;
  frmProp.txtName.Enabled := True;
  case RichViewEdit1.CurItemStyle of
    rvsBullet:
      begin
        RichViewEdit1.GetCurrentBulletInfo(s, Index, ImageList, Tag);
        frmProp.tsBullet.TabVisible := True;
        frmProp.rgBullet.ItemIndex := Index;
        frmProp.txtName.Text := s;
        frmProp.txtTag.Text := GetTagStr(Tag);
      end;
    rvsHotspot:
      begin
        // you can use GetCurrentBulletInfo or GetCurrentHotspotInfo
        // to receive info about hotspot in caret position.
        // in this demo we need not HotImageIndex, because here
        // HotImageIndex = ImageIndex+2
        // and so we can use GetCurrentBulletInfo
        RichViewEdit1.GetCurrentBulletInfo(s, Index, ImageList, Tag);
        frmProp.tsHotspot.TabVisible := True;
        frmProp.rgHotspot.ItemIndex := Index-3;
        frmProp.txtName.Text := s;
        frmProp.txtTag.Text := GetTagStr(Tag);
      end;
    rvsPicture:
      begin
        RichViewEdit1.GetCurrentPictureInfo(s, gr, VAlign, Tag);
        frmProp.tsPicture.TabVisible := True;
        frmProp.Image1.Picture.Graphic := gr;
        frmProp.txtName.Text := s;
        frmProp.txtTag.Text := GetTagStr(Tag);
        frmProp.rgPicVAlign.ItemIndex := Integer(VAlign);
      end;
    rvsComponent:
      begin
        RichViewEdit1.GetCurrentControlInfo(s, ctrl, VAlign, Tag);
        frmProp.tsComponent.TabVisible := True;
        frmProp.txtWidth.Text := IntToStr(ctrl.Width);
        frmProp.txtHeight.Text := IntToStr(ctrl.Height);
        frmProp.txtName.Text := s;
        frmProp.lblComponent.Caption := ctrl.ClassName;
        frmProp.txtTag.Text := GetTagStr(Tag);
        frmProp.rgCtrlVAlign.ItemIndex := Integer(VAlign);
      end;
    rvsBreak:
      begin
        frmProp.tsBreak.TabVisible := True;
        RichViewEdit1.GetCurrentBreakInfo(BreakWidth, BreakStyle, BreakColor, Tag);
        frmProp.txtBreakWidth.Text := IntToStr(BreakWidth);
        case BreakColor of
          clNone:
            frmProp.rgBreakColor.ItemIndex := 0;
          clRed:
            frmProp.rgBreakColor.ItemIndex := 1;
          clGreen:
            frmProp.rgBreakColor.ItemIndex := 2;
          clBlue:
            frmProp.rgBreakColor.ItemIndex := 3;
        end;
        frmProp.txtName.Text := '(not available for breaks)';
        frmProp.txtName.Enabled := False;
        frmProp.txtTag.Text := GetTagStr(Tag);
      end;
    rvsTable:
      begin
        frmProp.txtName.Text := RichViewEdit1.GetCurrentItemText;
        frmProp.txtTag.Text := GetTagStr(RichViewEdit1.GetCurrentTag);
        frmProp.PageControl1.Visible := False;
      end;
    else
      begin
        RichViewEdit1.GetCurrentTextInfo(s, Tag);
        if RVStyle1.TextStyles[RichViewEdit1.CurTextStyleNo].Unicode then
          s := RVU_UnicodeToAnsi(CP_ACP, s); // function from RVUni.pas
        frmProp.tsText.TabVisible := True;
        frmProp.txtName.Text := '(not available for text)';
        frmProp.txtName.Enabled := False;
        frmProp.lblText.Caption := s;
        frmProp.txtTag.Text := GetTagStr(Tag);
      end;
  end;
  if frmProp.ShowModal=mrOk then
  case RichViewEdit1.CurItemStyle of
    rvsBullet:
      begin
        RichViewEdit1.SetCurrentBulletInfo(
          frmProp.txtName.Text,
          frmProp.rgBullet.ItemIndex,
          nil,
          MakeTag(frmProp.txtTag.Text));
      end;
    rvsHotspot:
      begin
        RichViewEdit1.SetCurrentHotspotInfo(
          frmProp.txtName.Text,
          frmProp.rgHotspot.ItemIndex+3,
          frmProp.rgHotspot.ItemIndex+3+2,
          nil,
          MakeTag(frmProp.txtTag.Text));
      end;
    rvsPicture:
      begin
        { first we need to create a copy of image ...}
        gr := TGraphic(frmProp.Image1.Picture.Graphic.ClassType.Create);
        gr.Assign(frmProp.Image1.Picture.Graphic);
        RichViewEdit1.SetCurrentPictureInfo(
          frmProp.txtName.Text,
          gr,
          TRVVAlign(frmProp.rgPicVAlign.ItemIndex),
          MakeTag(frmProp.txtTag.Text));
      end;
    rvsComponent:
      begin
        // we wish these setting to be undone as one action,
        // so we use BeginUndoGroup, SetUndoGroupMode(True), settings, SetUndoGroupMode(False)
        RichViewEdit1.BeginUndoGroup(rvutModifyItem);
        // you can use BeginUndoCustomGroup instead of BeginUndoGroup
        // example:
        // RichViewEdit1.BeginUndoCustomGroup('modifying control');
        // In this case undo type will be rvutCustom
        // (look at TForm1.UpdateUndoMenu in this file)
        RichViewEdit1.SetUndoGroupMode(True);
        RichViewEdit1.SetCurrentControlInfo(
          frmProp.txtName.Text,
          TRVVAlign(frmProp.rgCtrlVAlign.ItemIndex),
          MakeTag(frmProp.txtTag.Text));
        RichViewEdit1.ResizeCurrentControl(
          StrToIntDef(frmProp.txtWidth.Text, ctrl.Width),
          StrToIntDef(frmProp.txtHeight.Text, ctrl.Height));
        RichViewEdit1.SetUndoGroupMode(False);
      end;
    rvsBreak:
      begin
        case frmProp.rgBreakColor.ItemIndex of
          -1,0:
            BreakColor := clNone;
          1:
            BreakColor := clRed;
          2:
            BreakColor := clGreen;
          3:
            BreakColor := clBlue;
        end;
        BreakWidth := StrToIntDef(frmProp.txtBreakWidth.Text,1);
        RichViewEdit1.SetCurrentBreakInfo(BreakWidth,BreakStyle,BreakColor,
                                          MakeTag(frmProp.txtTag.Text));
      end;
    rvsTable:
      begin
        RichViewEdit1.BeginUndoGroup(rvutModifyItem);
        RichViewEdit1.SetUndoGroupMode(True);
        RichViewEdit1.SetCurrentItemText(frmProp.txtName.Text);
        RichViewEdit1.SetCurrentTag(MakeTag(frmProp.txtTag.Text));
        RichViewEdit1.SetUndoGroupMode(False);
      end;
    else
      begin
        RichViewEdit1.SetCurrentTag(MakeTag(frmProp.txtTag.Text));
      end;
  end;
end;
{======================================================================}
{ Main menu : "View"                                                   }
{======================================================================}
{ View | Checkpoint list... -------------------------------------------}
procedure TForm1.mitCheckPointListClick(Sender: TObject);
var X,Y,Tag: Integer;
    Name: String;
    CheckpointData: TCheckpointData;
    RaiseEvent: Boolean;
    s: String;
begin
  frmList.lst.Items.Clear;
  CheckpointData := RichViewEdit1.GetFirstCheckPoint;
  while CheckpointData<>nil do begin
    RichViewEdit1.GetCheckpointInfo(CheckpointData,Tag,Name,RaiseEvent);
    RichViewEdit1.GetCheckpointXY(CheckpointData,X,Y);
    s := Format('(X:%d,Y:%d) Name:"%s" Tag:"%s"', [X,Y,Name,GetTagStr(Tag)]);
    frmList.lst.Items.Add(s);
    CheckpointData := RichViewEdit1.GetNextCheckpoint(CheckpointData);
  end;
  if frmList.ShowModal=mrOk then
    with RichViewEdit1 do
      ScrollTo(GetCheckPointY(frmList.lst.ItemIndex));
end;
{======================================================================}
{ Main menu : "Misc"                                                   }
{======================================================================}
{ Misc | RVF Save Options Submenu popups ------------------------------}
procedure TForm1.mpdRVFSaveOptionsClick(Sender: TObject);
begin
  // Checking selected RVFOptions in submenu...
  mitRVFSavePictureBodies.Checked  := rvfoSavePicturesBody in RichViewEdit1.RVFOptions;
  mitRVFSaveControlBodies.Checked  := rvfoSaveControlsBody in RichViewEdit1.RVFOptions;
  mitRVFSaveBinary.Checked         := rvfoSaveBinary       in RichViewEdit1.RVFOptions;
  mitRVFSaveBackground.Checked     := rvfoSaveBack         in RichViewEdit1.RVFOptions;
  // There is one more option, affecting saving - rvfoUseStyleNames.
end;
{ Misc | RVF Load Options Submenu popups ------------------------------}
procedure TForm1.mpdRVFLoadOptionsClick(Sender: TObject);
begin
  // Checking selected RVFOptions in submenu...
  mitRVFIgnoreunknownpictureformats.Checked    := rvfoIgnoreUnknownPicFmt     in RichViewEdit1.RVFOptions;
  mitRVFIgnoreunknowncontrols.Checked          := rvfoIgnoreUnknownCtrls      in RichViewEdit1.RVFOptions;
  mitRVFConvertunknownstylesto0.Checked        := rvfoConvUnknownStylesToZero in RichViewEdit1.RVFOptions;
  mitRVFConvertTooLargeImageIndicesto0.Checked := rvfoConvLargeImageIdxToZero in RichViewEdit1.RVFOptions;
  mitRVFLoadBackground.Checked                 := rvfoLoadBack                in RichViewEdit1.RVFOptions;
end;
{ Misc | Background Submenu popups ------------------------------------}
procedure TForm1.mpdBackgroundClick(Sender: TObject);
begin
  // Displaying RichViewEdit1.BackgroundStyle as checkmark in submenu...
  mitBackNoBitmap.Checked         := RichViewEdit1.BackgroundStyle=bsNoBitmap;
  mitBackCentered.Checked         := RichViewEdit1.BackgroundStyle=bsCentered;
  mitBackStretched.Checked        := RichViewEdit1.BackgroundStyle=bsStretched;
  mitBackTiledAndScrolled.Checked := RichViewEdit1.BackgroundStyle=bsTiledAndScrolled;
  mitBackTiled.Checked            := RichViewEdit1.BackgroundStyle=bsTiled;
end;
{ Misc | RVF Save and Load Options ------------------------------------}
procedure TForm1.mitRVFOptionsClick(Sender: TObject);
begin
  with Sender as TMenuItem do begin
    Checked := not Checked;
    if Checked then
      RichViewEdit1.RVFOptions := RichViewEdit1.RVFOptions + [TRVFOption(Tag)]
    else
      RichViewEdit1.RVFOptions := RichViewEdit1.RVFOptions - [TRVFOption(Tag)];
  end;
end;
{ Misc | Background options -------------------------------------------}
procedure TForm1.mitBackClick(Sender: TObject);
begin
  RichViewEdit1.BackgroundStyle := TBackgroundStyle(TMenuItem(Sender).Tag);
end;
{======================================================================}
{ Displaying popup menu -----------------------------------------------}
procedure TForm1.RichViewEdit1RVRightClickEx(Sender: TRichView; LineNo,
  Style, X, Y: Integer);
begin
   { Here you can work with clicked item.
     LineNo, Style are for clicked item.
     Special case : LineNo=-1, Style=rvsBack }
   { But with editor, you should work not with clicked item,  but
     with "active" item (which has caret). So it has no meaning, where
     user right-click the component. So you can use not OnRVRightClickEx
     event, but PopupMenu property.
   }
   { Old OnRVRightClick event still works }
   PopupMenu1.Popup(X,Y);
end;
{ On Popup -------------------------------------------------------------}
procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  mitEditProp1.Enabled := not RichViewEdit1.SelectionExists;
end;
{-----------------------------------------------------------------------}
{OnChange event handler. You can, for example, set here some "Modified" flag}
{-----------------------------------------------------------------------}
procedure TForm1.RichViewEdit1Change(Sender: TObject);
begin
   UpdateUndoMenu;
end;
{-----------------------------------------------------------------------}
procedure TForm1.FindDialog1Find(Sender: TObject);
begin
  if not RichViewEdit1.SearchText(FindDialog1.FindText,
                           GetRVESearchOptions(FindDialog1.Options)) then
   Application.MessageBox('Can''t find', 'Search complete', MB_OK or MB_ICONEXCLAMATION);
end;
{-----------------------------------------------------------------------}
procedure TForm1.mitPasteAsOLEClick(Sender: TObject);
var oc: TOleContainer;
begin
  oc := TOleContainer.Create(nil);
  if oc.CanPaste then begin
    oc.Visible := False;
    oc.Parent := RichViewEdit1;
    oc.SizeMode := smAutoSize;
    oc.Paste;
    RichViewEdit1.InsertControl('', oc,rvvaBaseline);
    oc.OnResize := OnOleResize;
    oc.OnActivate := OnOleActions;
    oc.OnDeactivate := OnOleActions;
    oc.Visible := True;
    end
  else
    oc.Free;
end;
{-----------------------------------------------------------------------}
procedure TForm1.OnOleResize(Sender: TObject);
begin
  RichViewEdit1.AdjustControlPlacement2(TControl(Sender));
end;
{-----------------------------------------------------------------------}
procedure TForm1.OnOleActions(Sender: TObject);
begin
  RichViewEdit1.AdjustControlPlacement2(TControl(Sender));
end;
{-----------------------------------------------------------------------}
// You should manually update palette info when user changes color mode
// without restarting Windows
procedure TForm1.WMDisplayChange(var Message: TMessage{TWMDisplayChange});
begin
  RichViewEdit1.UpdatePaletteInfo;
  RVPrint1.UpdatePaletteInfo;
end;
{-----------------------------------------------------------------------}
{ Event: OnJump (when user clicks hypertext item with pressed Ctrl key   }
procedure TForm1.RichViewEdit1Jump(Sender: TObject; id: Integer);
var RVData: TCustomRVFormattedData;
    ItemNo: Integer;
begin
  // NOTE: OnJump is called after the caret is repositioned to clicked item
  // But warning: a clicked event is not necessarily an active item
  // (when clicking on left part of picture or left part of first character in text item,
  // caret moves before item and previous item becomes active!)
  RichViewEdit1.GetJumpPointLocation(id, RVData, ItemNo);
  Application.MessageBox(PChar(Format('Clicked on the %d-th jump!'#13'(the %d-th item in %s object)',
                                      [id, ItemNo, RVData.ClassName])),
                         'Control-Click', MB_OK or MB_ICONINFORMATION);


end;
{------------------------------------------------------------------------------}
{ Event: OnRVMouseMove (when user moves mouse above hypertext item with pressed Ctrl key   }
procedure TForm1.RichViewEdit1RVMouseMove(Sender: TObject; id: Integer);
var RVData: TCustomRVFormattedData;
    ItemNo: Integer;
begin
  if id=-1 then
    StatusBar1.SimpleText := ''
  else begin
    RichViewEdit1.GetJumpPointLocation(id, RVData, ItemNo);
    StatusBar1.SimpleText := Format('Mouse is over jump #%d      (the %d-th item in %s object)',[id, ItemNo,RVData.ClassName]);
  end;
end;
{------------------------------------------------------------------------------}

procedure TForm1.mitInserttable1Click(Sender: TObject);
var table: TRVTableItemInfo;
    r,c: Integer;
begin
  table := TRVTableItemInfo.CreateEx(4,3, RichViewEdit1.RVData);

  table.BorderStyle := rvtbRaisedColor;
  table.CellBorderStyle := rvtbLoweredColor;
  table.BorderLightColor := $00FAF1C9;
  table.BorderColor := $00A98E10;
  table.CellBorderLightColor := $00FAF1C9;
  table.CellBorderColor := $00A98E10;
  table.Color := $00EAC724;
  table.BorderWidth := 5;
  table.CellBorderWidth := 2;
  table.CellPadding := 5;
  table.CellVSpacing := 1;
  table.CellHSpacing := 1;
  table.BorderVSpacing := 1;
  table.BorderHSpacing := 1;

  for r := 0 to table.Rows.Count-1 do
    for c := 0 to table.Rows[r].Count-1 do
      table.Cells[r,c].BestWidth := 100;

  table.MergeCells(0,0,3,1, False);
  table.MergeCells(1,0,1,3, False);
  with table.Cells[0,0] do begin
    Color := clInfoBk;
    Clear;
    AddBulletEx(  '',0,il,2);
    AddNL(' Example 1 ',1,-1);
    AddBulletEx(  '',0,il,-1);
    AddNL('All cells have 100 pixels width, width of table itself is calculated basing on width of cells.',0,0);
  end;

  if RichViewEdit1.InsertItem('', table) then begin
  end;
end;

procedure TForm1.mitInsertTable2Click(Sender: TObject);
var table: TRVTableItemInfo;
    btn: TButton;
begin
  table := TRVTableItemInfo.CreateEx(10,6, RichViewEdit1.RVData);
  table.Color := clWhite;

  table.BorderStyle := rvtbRaisedColor;
  table.CellBorderStyle := rvtbLoweredColor;
  table.BorderLightColor := clWhite;
  table.BorderColor := clBlack;
  table.CellBorderLightColor := clWhite;
  table.CellBorderColor := clBlack;

  table.BorderWidth := 2;
  table.BorderVSpacing := 0;
  table.BorderHSpacing := 0;
  table.CellBorderWidth := 2;
  table.CellPadding := 3;
  table.CellVSpacing := 0;
  table.CellHSpacing := 0;
  table.Cells[0,0].BestWidth := -16;
  table.Cells[0,1].BestWidth := -16;
  table.Cells[0,2].BestWidth := -16;
  table.Cells[0,3].BestWidth := -16;
  table.Cells[0,4].BestWidth := -16;
  table.Cells[0,5].BestWidth := -16;
//  table.Rows.MergeCells(1,0,6,1);
  table.MergeCells(2,0,2,8, False);
  with table.Cells[2,0] do begin
    Clear;
    AddNL('Another example.',0,0);
    btn := TButton.Create(nil);
    btn.Caption := 'With button inside';
    btn.Width := 150;
    AddControlEx('',btn,2,rvvaBaseline);
    AddNL('Width of table = 90% of document width. Widths of cells = 16%',0,0);
  end;
//  table.Rows.MergeCells(2,4,2,8);
  table.BestWidth := -90;
  if RichViewEdit1.InsertItem('', table) then begin
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitInsertTable3Click(Sender: TObject);
var table: TRVTableItemInfo;
    r,c: Integer;
begin
  table := TRVTableItemInfo.CreateEx(5,6, RichViewEdit1.RVData);

  table.Color := $00A5CCE7;
  table.BorderStyle := rvtbColor;
  table.CellBorderStyle := rvtbColor;
  table.BorderColor := $002E1234;
  table.CellBorderColor := $002E1234;

  table.BorderWidth := 2;
  table.BorderVSpacing := 2;
  table.BorderHSpacing := 2;
  table.CellBorderWidth := 1;
  table.CellPadding := 3;
  table.CellVSpacing := 0;
  table.CellHSpacing := 0;

  for c := 0 to table.Rows[0].Count-1 do
   table.Cells[0,c].Color := $00A5E1F8;

  for r := 1 to table.Rows.Count-1 do
   table.Cells[r,0].Color := $00A5E1F8;

  for r := 1 to table.Rows.Count-1 do
    for c := 1 to table.Rows[r].Count-1 do begin
      table.Cells[r,c].Color := $007AB4DA;
      if c>1 then
        table.Cells[r,c].VisibleBorders.Left := False;
      if c<table.Rows[r].Count-1 then
        table.Cells[r,c].VisibleBorders.Right := False;
    end;

  table.BestWidth := 400;
  RichViewEdit1.InsertText('Third example: width of table = 400 pixels, widths of cells - unspecified.');
  if RichViewEdit1.InsertItem('', table) then begin
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitInsertTable4Click(Sender: TObject);
var table: TRVTableItemInfo;
    r,c: Integer;
begin
  table := TRVTableItemInfo.CreateEx(3,3, RichViewEdit1.RVData);

  table.Color := clNone;
  table.BorderStyle := rvtbColor;
  table.CellBorderStyle := rvtbColor;

  table.BorderWidth := 1;
  table.BorderVSpacing := 2;
  table.BorderHSpacing := 2;
  table.CellBorderWidth := 1;
  table.CellPadding := 3;
  table.CellVSpacing := 5;
  table.CellHSpacing := 5;
  table.VRuleWidth := 1;
  table.HRuleWidth := 1;
  //table.VOutermostRule := True;
  //table.HOutermostRule := True;
  for r := 0 to table.Rows.Count-1 do
    for c := 0 to table.Rows[r].Count-1 do begin
      table.Cells[r,c].BestWidth := 40;
      table.Cells[r,c].Clear;
      table.Cells[r,c].AddFmt('%d,%d',[r,c],0,0);
      table.Cells[r,c].Color := clWhite;
    end;
  //table.MergeCells(0,0,table.Rows[0].Count,1,True);
  //table.MergeCells(1,0,1,table.Rows.Count-1,True);
  RichViewEdit1.InsertText('Transparent table with rules');
  if RichViewEdit1.InsertItem('', table) then begin
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mpdTableClick(Sender: TObject);
var item: TCustomRVItemInfo;
    table: TRVTableItemInfo;
    r,c,cs,rs: Integer;
    rve: TRichViewEdit;
    Selected, SelectionRectangular: Boolean;
begin
  if not RichViewEdit1.GetCurrentItemEx(TRVTableItemInfo, rve, item) then begin
    mitRowsAbove.Enabled         := False;
    mitRowsBelow.Enabled         := False;
    mitColsLeft.Enabled          := False;
    mitColsRight.Enabled         := False;
    mitDelRows.Enabled           := False;
    mitDelColumns.Enabled        := False;
    mitMergeCells.Enabled        := False;
    mitUmRows.Enabled            := False;
    mitUmCols.Enabled            := False;
    mitUmRowsAndCols.Enabled     := False;
    mitSplitVertically.Enabled   := False;
    mitSplitHorizontally.Enabled := False;
    exit;
  end;
  table := TRVTableItemInfo(item);
  Selected := table.GetNormalizedSelectionBounds(True,r,c,cs,rs);
  mitRowsAbove.Enabled         := Selected;
  mitRowsBelow.Enabled         := Selected;
  mitColsLeft.Enabled          := Selected;
  mitColsRight.Enabled         := Selected;
  mitDelRows.Enabled           := Selected;
  mitDelColumns.Enabled        := Selected;
  mitMergeCells.Enabled        := table.CanMergeSelectedCells(True);
  SelectionRectangular := Selected and
                          (table.CanMergeSelectedCells(True) or
                           (table.GetEditedCell(r,c)<>nil));
  mitSplitVertically.Enabled   := SelectionRectangular;
  mitSplitHorizontally.Enabled := SelectionRectangular;
  mitUmRows.Enabled            := SelectionRectangular;
  mitUmCols.Enabled            := SelectionRectangular;
  mitUmRowsAndCols.Enabled     := SelectionRectangular;
end;
{------------------------------------------------------------------------------}
procedure TForm1.mitCellsOperationClick(Sender: TObject);
var item: TCustomRVItemInfo;
    table: TRVTableItemInfo;
    Data: Integer;
    r,c,cs,rs: Integer;
    s: String;
    rve: TRichViewEdit;
    ItemNo: Integer;
begin
  if not RichViewEdit1.CanChange or
     not RichViewEdit1.GetCurrentItemEx(TRVTableItemInfo, rve, item) then
    exit;
  table := TRVTableItemInfo(item);
  ItemNo := rve.GetItemNo(table);
  rve.BeginItemModify(ItemNo, Data);
  case TMenuItem(Sender).Tag of
    1:
      table.InsertRowsAbove(1);
    2:
      table.InsertRowsBelow(1);
    3:
      table.InsertColsLeft(1);
    4:
      table.InsertColsRight(1);
    5:
      begin
        table.GetNormalizedSelectionBounds(True,r,c,cs,rs);
        if rs=table.Rows.Count then begin
          rve.SetSelectionBounds(ItemNo,0,ItemNo,1);
          rve.DeleteSelection;
          exit;
        end;
        rve.BeginUndoGroup(rvutModifyItem);
        rve.SetUndoGroupMode(True);
        table.DeleteSelectedRows;
        // it's possible all-nil rows/cols appear after deleting
        table.DeleteEmptyRows;
        table.DeleteEmptyCols;
        rve.SetUndoGroupMode(False);
      end;
    6:
      begin
        table.GetNormalizedSelectionBounds(True,r,c,cs,rs);
        if cs=table.Rows[0].Count then begin
          rve.SetSelectionBounds(ItemNo,0,ItemNo,1);
          rve.DeleteSelection;
          exit;
        end;
        rve.BeginUndoGroup(rvutModifyItem);
        rve.SetUndoGroupMode(True);
        table.DeleteSelectedCols;
        // it's possible all-nil rows/cols appear after deleting
        table.DeleteEmptyRows;
        table.DeleteEmptyCols;
        rve.SetUndoGroupMode(False);
      end;
    7:
      begin
        // 3 methods: MergeSelectedCells, DeleteEmptyRows, DeleteEmptyCols
        // must be undone as one action.
        // So using BeginUndoGroup - SetUndoGroupMode(True) - ... - SetUndoGroupMode(False)
        rve.BeginUndoGroup(rvutModifyItem);
        rve.SetUndoGroupMode(True);
        table.MergeSelectedCells(True);
        table.DeleteEmptyRows;
        table.DeleteEmptyCols;
        rve.SetUndoGroupMode(False);
        // table.MergeSelectedCells(False) will not allow to create empty columns
        // or rows
      end;
    8:
      table.UnmergeSelectedCells(True, False);
    9:
      table.UnmergeSelectedCells(False, True);
    10:
      table.UnmergeSelectedCells(True, True);
    11:
      begin
        s := '2';
        if InputQuery('Split Vertically','Columns (in each selected cell):',s) then begin
          table.SplitSelectedCellsVertically(StrToIntDef(s,0));
        end;
      end;
    12:
      begin
        s := '2';
        if InputQuery('Split Horizontally','Rows (in each selected cell):',s) then begin
          table.SplitSelectedCellsHorizontally(StrToIntDef(s,0));
        end;
      end;
  end;
  rve.EndItemModify(ItemNo, Data);
  rve.Change;
end;

initialization
  // We need to register classes in order to load them from rvf files
  RegisterClasses([TButton, TEdit, TOleContainer]);
end.
