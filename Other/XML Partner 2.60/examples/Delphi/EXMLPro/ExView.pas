(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* XMLPartner Professional: ExView.PAS 2.57              *}
{*********************************************************}
{* XMLPartner: XSL Preview window for XML editor example *}
{*********************************************************}

unit ExView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, XpvFlHTM, XpvFlBas, XpvFlRTF, XpvFlPrt,
  XpvXSLPr, XpDOM, XpBase, XpvFlXML, XpvXSLT, ToolWin, ImgList;

type
  { This type describes the interface for a method that implements the
    display of a transformed XML document. }
  TXpFilterDisplayMethod = procedure(aFilter : TXpFilterBase) of object;

  TfrmPreview = class(TForm)
    pnlTop: TPanel;
    pbRender: TButton;
    pbBrowse: TButton;
    cboStylesheet: TComboBox;
    cboFilter: TComboBox;
    lblFilter: TLabel;
    lblStylesheet: TLabel;
    pgPreview: TPageControl;
    tabRTF: TTabSheet;
    tabPrinter: TTabSheet;
    RichEdit: TRichEdit;
    pnlPreviewContainer: TPanel;
    filtPrinter: TXpFilterPrint;
    PrinterHScroll: TScrollBar;
    tabXML: TTabSheet;
    memXML: TMemo;
    tabText: TTabSheet;
    memText: TMemo;
    dlgOpen: TOpenDialog;
    xslProcessor: TXpXSLProcessor;
    filtHTML: TXpFilterHTML;
    filtRTF: TXpFilterRTF;
    filtXML: TXpFilterXML;
    filtText: TXpFilterText;
    tbPrint: TToolBar;
    pbFirstPage: TToolButton;
    pbPrevPage: TToolButton;
    pbNextPage: TToolButton;
    pbLastPage: TToolButton;
    imMain: TImageList;
    lblSpacer: TLabel;
    pnlPage: TPanel;
    lblSpacer2: TLabel;
    PrinterVScroll: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure cboFilterChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbRenderClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure pbUpClick(Sender: TObject);
    procedure pbDownClick(Sender: TObject);
    procedure PrinterVScrollChange(Sender: TObject);
    procedure PrinterHScrollChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure filtPrinterResize(Sender: TObject);
    procedure pbPrevPageClick(Sender: TObject);
    procedure pbNextPageClick(Sender: TObject);
    procedure pbFirstPageClick(Sender: TObject);
    procedure pbLastPageClick(Sender: TObject);
  private
    { Private declarations }

    FCurSheet : string;
      { Used by SaveStylesheetSelection and RestoreStylesheetSelection. }

    FPrinterFilter : TXpFilterPrint;
      { The printer filter. For use by the printer page controls. }
    FDOM : TXpObjModel;
      { The DOM containing the XML that is to be transformed. }
    FFilename : string;
      { The name of the document being viewed. }
    FLastPage : Integer;
      { The last visible notebook page. }

    procedure DisplayHTML(aFilter : TXpFilterBase);
    procedure DisplayPrinter(aFilter : TXpFilterBase);
    procedure DisplayRTF(aFilter : TXpFilterBase);
    procedure DisplayText(aFilter : TXpFilterBase);
    procedure DisplayXML(aFilter : TXpFilterBase);
    function GetStyleSheet : DOMString;
    procedure RestoreStylesheetSelection;
    procedure SaveStylesheetSelection;
    procedure ShowTab;
    procedure UpdatePrinterUI;
  public
    { Public declarations }

    procedure AddStylesheet(const sName : string; oChildWin : TForm);
      { Add a stylesheet to this window's list of stylesheets. }

    procedure CopyToClipboard;
      { Copies text from the output control currently being viewed. }

    procedure RemoveStylesheet(const sName : string);
      { Remove a stylesheet from this window's list of stylesheets. }

    procedure RenameStylesheet(const sOldName, sNewName : string);
      { Rename a stylesheet in this window's list of stylesheets. }

    property DOM : TXpObjModel read FDOM write FDOM;
      { The DOM containing the XML that is to be transformed. }

    property Filename : string read FFilename write FFilename;
      { The name of the XML document being viewed. }

  end;

var
  frmPreview: TfrmPreview;

implementation

uses
{$IFDEF WIN32}
  ShellAPI,
{$ENDIF}
  ExChildw,
  ExErr;

{$R *.DFM}

const
  ciTabNone = -1;
  ciTabRTF = 0;
  ciTabPrinter = 1;
  ciTabXML = 2;
  ciTabText = 3;

type
  { This type is used to store filter-specific data. One instance of this
    class is associated with each entry in the filter combobox. }
  TXpFilterDetail = class
  public
    Display : TXpFilterDisplayMethod;
      { The method used to display the rendered XML document. }
    Filter : TXpFilterBase;
      { The filter component to be used to render the XML document. }
    TabIndex : Integer;
      { The notebook tab associated with the filter. If set to ciTabNone then
        the filter does not have a tab. }
  end;

procedure TfrmPreview.FormShow(Sender: TObject);
var
  aFilter : TXpFilterBase;
  aFilterDetail : TXpFilterDetail;
  Inx : Integer;
begin
  FLastPage := ciTabNone;
  { Find the filters on the form & populate the Filter combobox. }
  for Inx := 0 to pred(ComponentCount) do begin
    if Components[Inx] is TXpFilterBase then begin
      aFilter := TXpFilterBase(Components[Inx]);
      aFilterDetail := TXpFilterDetail.Create;
      aFilterDetail.Filter  := aFilter;
      if Components[Inx].ClassType = TXpFilterHTML then begin
        aFilterDetail.Display := DisplayHTML;
        aFilterDetail.TabIndex := ciTabNone;
      end
      else if Components[Inx].ClassType = TXpFilterPrint then begin
        aFilterDetail.Display := DisplayPrinter;
        aFilterDetail.TabIndex := ciTabPrinter;
      end
      else if Components[Inx].ClassType = TXpFilterRTF then begin
        aFilterDetail.Display := DisplayRTF;
        aFilterDetail.TabIndex := ciTabRTF;
      end
      else if Components[Inx].ClassType = TXpFilterXML then begin
        aFilterDetail.Display := DisplayXML;
        aFilterDetail.TabIndex := ciTabXML;
      end
      else begin
        aFilterDetail.Display := DisplayText;
        aFilterDetail.TabIndex := ciTabText;
      end;
      cboFilter.Items.AddObject(aFilter.DisplayName, Pointer(aFilterDetail));
    end;
  end;

  { Position to the first filter. }
  if cboFilter.Items.Count > 0 then
    cboFilter.ItemIndex := 0
  else begin
    cboFilter.Items.Add('<None available>');
    cboFilter.ItemIndex := 0;
    cboFilter.Enabled := False;
  end;

  { Position to the first stylesheet. }
  if cboStyleSheet.Items.Count > 0 then
    cboStyleSheet.ItemIndex := 0;

  { Set to the appropriate notebook tab. }
  ShowTab;

end;

procedure TfrmPreview.cboFilterChange(Sender: TObject);
begin
  { Display the tab associated with the selected filter. }
  ShowTab;
end;

procedure TfrmPreview.CopyToClipboard;
begin
  if pgPreview.ActivePage = tabRTF then
    RichEdit.CopyToClipboard
  else if pgPreview.ActivePage = tabXML then
    memXML.CopyToClipboard
  else if pgPreview.ActivePage = tabText then
    memText.CopyToClipboard;
end;

procedure TfrmPreview.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Inx : Integer;
begin
  { Free the filter detail objects associated with the items in the combobox. }
  for Inx := pred(cboFilter.Items.Count) downto 0 do
    cboFilter.Items.Objects[Inx].Free;
end;

procedure TfrmPreview.ShowTab;
var
  aFilterDetail : TXpFilterDetail;
begin

  aFilterDetail := TXpFilterDetail(cboFilter.Items.Objects[cboFilter.ItemIndex]);

  if FLastPage = aFilterDetail.TabIndex then
    Exit;

  { Hide the previously shown page. }
  if FLastPage <> ciTabNone then
    pgPreview.Pages[FLastPage].TabVisible := False;

  FLastPage := aFilterDetail.TabIndex;
  if FLastPage <> ciTabNone then begin
    pgPreview.ActivePage := pgPreview.Pages[FLastPage];
    pgPreview.Pages[FLastPage].TabVisible := True;
    pgPreview.Pages[FLastPage].SetFocus;
    if FLastPage = ciTabPrinter then
      UpdatePrinterUI;
  end;
end;

procedure TfrmPreview.pbRenderClick(Sender: TObject);
var
  aFilterDetail : TXpFilterDetail;
  aFilter : TXpFilterBase;
  savCursor : TCursor;
begin
  { Get the filter. }
  aFilterDetail := TXpFilterDetail(cboFilter.Items.Objects[cboFilter.ItemIndex]);
  aFilter := TXpFilterBase(aFilterDetail.Filter);
  with xslProcessor do begin
    Filter := aFilter;
    XMLObjModel := FDOM;
    { Did the user specify an external stylesheet? }
    if cboStyleSheet.ItemIndex < 0 then begin
      { Yes. Get the name of the file. }
      if cboStylesheet.Text <> '' then
        StyleURL := cboStyleSheet.Text;
    end
    else
      { No. Obtain the data for the stylesheet. }
      StyleData := GetStyleSheet;

    savCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      if ApplyStyle then
        { Display the file using the method specific to the filter. }
        aFilterDetail.Display(aFilter)
      else begin
        Screen.Cursor := savCursor;
        with TfrmErrors.Create(Self) do
          try
            Errors := xslProcessor.Errors;
            Caption := 'XSL Errors';
            IntroMsg := 'applying the stylesheet';
            ShowModal;
          finally
            Free;
          end;
      end;
    finally
      Screen.Cursor := savCursor;
    end;
  end;
end;

function TfrmPreview.GetStyleSheet : DOMString;
var
  bIsBEorLE : Boolean;
  oForm : TXmlChild;
  sXML : DOMString;
begin
  oForm := TXmlChild(cboStyleSheet.Items.Objects[cboStyleSheet.ItemIndex]);
  sXML := oForm.XMLData;
  bIsBEorLE := ( (sXML[1] = #$FEFF) or
                 (sXML[1] = #$FFFE) );
  { Marked as big or little endian? }
  if not bIsBEorLE then begin
   { No. Must force it to be recognized as little endian. }
   Result := DOMString(#$FEFF) + sXML;
  end
  else
    Result := sXML;
end;

procedure TfrmPreview.DisplayHTML(aFilter : TXpFilterBase);
resourcestring
  EX_Error = 'Unable to start web browser. Make sure you have it properly ' +
             'set up on your system.';
var
  sFile : string;
begin
  if FFileName = '' then
    FFileName := 'temp';
  sFile := XpMakeFilePath(ExtractFilePath(Application.ExeName),
                          ChangeFileExt(ExtractFileName(FFileName),'.htm'));
  TXpFilterHTML(aFilter).OutputEncoding := FDOM.OutCharSet;
  aFilter.SaveToFile(sFile);
{$IFDEF WIN32}
  if ShellExecute(0, 'open', PChar(sFile), '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage(EX_Error);
{$ENDIF}

{$IFDEF LINUX}
{$ENDIF}
end;

procedure TfrmPreview.DisplayPrinter(aFilter : TXpFilterBase);
begin
  FPrinterFilter := TXpFilterPrint(aFilter);
  UpdatePrinterUI;
end;

procedure TfrmPreview.DisplayRTF(aFilter : TXpFilterBase);
var
  sFile : string;
begin
  sFile := XpMakeFilePath(ExtractFilePath(Application.ExeName),
                          ChangeFileExt(ExtractFileName(FFileName),'.rtf'));
  aFilter.SaveToFile(sFile);
  RichEdit.Clear;
  RichEdit.Lines.LoadFromFile(sFile);
end;

procedure TfrmPreview.DisplayText(aFilter : TXpFilterBase);
var
  sFile : string;
begin
  sFile := XpMakeFilePath(ExtractFilePath(Application.ExeName),
                          ChangeFileExt(ExtractFileName(FFileName),'.txt'));
  TXpFilterText(aFilter).OutputEncoding := FDOM.OutCharSet;
  aFilter.SaveToFile(sFile);
  memText.Lines.Clear;
  memText.Lines.LoadFromFile(sFile);
end;

procedure TfrmPreview.DisplayXML(aFilter : TXpFilterBase);
var
  sFile : string;
begin
  sFile := XpMakeFilePath(ExtractFilePath(Application.ExeName),
                          ChangeFileExt(ExtractFileName(FFileName),'.out'));
  TXpFilterXML(aFilter).OutputEncoding := FDOM.OutCharSet;
  aFilter.SaveToFile(sFile);
  memXML.Lines.Clear;
  memXML.Lines.LoadFromFile(sFile);
end;

procedure TfrmPreview.pbBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then begin
    cboStylesheet.ItemIndex := -1;
    cboStylesheet.Text := dlgOpen.FileName;
  end;
end;

procedure TfrmPreview.pbUpClick(Sender: TObject);
begin
  if FPrinterFilter.PageCurrent <= 1 then begin
    MessageBeep(MB_OK);
    exit;
  end;

  FPrinterFilter.PageCurrent := FPrinterFilter.PageCurrent - 1;
  UpdatePrinterUI;
end;

procedure TfrmPreview.pbDownClick(Sender: TObject);
begin
  if FPrinterFilter.PageCurrent >= FPrinterFilter.PageCount then begin
    MessageBeep(MB_OK);
    exit;
  end;

  FPrinterFilter.PageCurrent := FPrinterFilter.PageCurrent + 1;
  UpdatePrinterUI;
end;

procedure TfrmPreview.UpdatePrinterUI;
var
  bFirstPage,
  bHasPages,
  bLastPage : Boolean;
begin
  if Assigned(FPrinterFilter) then begin
    pnlPage.Caption := Format('Page %d of %d',
                                   [FPrinterFilter.PageCurrent,
                                    FPrinterFilter.PageCount]);
    bHasPages := (FPrinterFilter.PageCount > 0);
  end
  else
    bHasPages := False;

  if bHasPages then begin
    PrinterVScroll.Position := 0;
    PrinterHScroll.Position := 0;
    bFirstPage := (FPrinterFilter.PageCurrent = 1);
    bLastPage := (FPrinterFilter.PageCurrent = FPrinterFilter.PageCount);
    pbFirstPage.Enabled := (not bFirstPage);
    pbPrevPage.Enabled := (not bFirstPage);
    pbNextPage.Enabled := (not bLastPage);
    pbLastPage.Enabled := (not bLastPage);
  end
  else begin
    pbFirstPage.Enabled := False;
    pbPrevPage.Enabled := False;
    pbNextPage.Enabled := False;
    pbLastPage.Enabled := False;
  end;
end;

procedure TfrmPreview.PrinterVScrollChange(Sender: TObject);
begin
  FPrinterFilter.VSlidePosition := PrinterVScroll.Position;
end;

procedure TfrmPreview.PrinterHScrollChange(Sender: TObject);
begin
  FPrinterFilter.HSlidePosition := PrinterHScroll.Position;
end;

procedure TfrmPreview.FormCreate(Sender: TObject);
var
  Inx : Integer;
begin
  { Make sure the tab pages are invisible. }
  for Inx := 0 to pred(pgPreview.PageCount) do
    pgPreview.Pages[Inx].TabVisible := False;
end;

procedure TfrmPreview.filtPrinterResize(Sender : TObject);
begin
  PrinterVScroll.Max := filtPrinter.VSlideFactor;
  PrinterVScroll.Position := filtPrinter.VSlidePosition;

  PrinterHScroll.Max := filtPrinter.HSlideFactor;
  PrinterHScroll.Position := filtPrinter.HSlidePosition;
end;

procedure TfrmPreview.SaveStylesheetSelection;
begin
  if cboStylesheet.ItemIndex >= 0 then
    FCurSheet := cboStylesheet.Items[cboStylesheet.ItemIndex]
  else
    FCurSheet := '';
end;

procedure TfrmPreview.RestoreStylesheetSelection;
var
  wInx : Integer;
begin
  if FCurSheet <> '' then begin
    wInx := cboStylesheet.Items.IndexOf(FCurSheet);
    { Is the stylesheet still in the list? }
    if wInx >= 0 then
      { Yes. Position combobox to that stylesheet. }
      cboStylesheet.ItemIndex := wInx
    else begin
      { No. Clear out the combobobox & the XSL Processor's style data. }
      cboStylesheet.Text := '';
      xslProcessor.StyleData := '';
    end;
  end;
end;

procedure TfrmPreview.AddStylesheet(const sName : string; oChildWin : TForm);
begin
  SaveStylesheetSelection;
  try
    cboStylesheet.Items.AddObject(sName, oChildWin);
  finally
    RestoreStylesheetSelection;
  end;
end;

procedure TfrmPreview.RemoveStylesheet(const sName : string);
var
  wInx : Integer;
begin
  SaveStylesheetSelection;
  try
    wInx := cboStylesheet.Items.IndexOf(sName);
    if wInx >= 0 then
      cboStylesheet.Items.Delete(wInx);
  finally
    RestoreStylesheetSelection;
  end;
end;

procedure TfrmPreview.RenameStylesheet(const sOldName, sNewName : string);
var
  wInx : Integer;
begin
  SaveStylesheetSelection;
  try
    if FCurSheet = sOldName then
      FCurSheet := sNewName;
    wInx := cboStylesheet.Items.IndexOf(sOldName);
    if wInx >= 0 then
      cboStylesheet.Items[wInx] := sNewName;
  finally
    RestoreStylesheetSelection;
  end;
end;

procedure TfrmPreview.pbPrevPageClick(Sender: TObject);
begin
  if FPrinterFilter.PageCurrent <= 1 then begin
    MessageBeep(MB_OK);
    exit;
  end;

  FPrinterFilter.PageCurrent := FPrinterFilter.PageCurrent - 1;
  UpdatePrinterUI;
end;

procedure TfrmPreview.pbNextPageClick(Sender: TObject);
begin
  if FPrinterFilter.PageCurrent >= FPrinterFilter.PageCount then begin
    MessageBeep(MB_OK);
    exit;
  end;

  FPrinterFilter.PageCurrent := FPrinterFilter.PageCurrent + 1;
  UpdatePrinterUI;
end;

procedure TfrmPreview.pbFirstPageClick(Sender: TObject);
begin
  FPrinterFilter.PageCurrent := 1;
  UpdatePrinterUI;
end;

procedure TfrmPreview.pbLastPageClick(Sender: TObject);
begin
  FPrinterFilter.PageCurrent := FPrinterFilter.PageCount;
  UpdatePrinterUI;
end;
end.
