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

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, 
  OleCtrls, SHDocVw, XpBase, XpDOM, XpvFlBas, XpvFlPrt, XpvXSLT, XpvXSLPr,
  XpvFlXML, XpvFlHTM, XpvFlRTF;

type
  TWorkOrders = class(TForm)
    Pages: TPageControl;
    Query: TTabSheet;
    Confirm: TTabSheet;
    Status: TTabSheet;
    Report: TTabSheet;
    Panel1: TPanel;
    rgSearchType: TRadioGroup;
    gbCriteria: TGroupBox;
    statusBar: TStatusBar;
    cmbTechs: TComboBox;
    pnlCatCrit: TPanel;
    rbLev1: TRadioButton;
    rbLev3: TRadioButton;
    rbLev2: TRadioButton;
    memWorkOrder: TMemo;
    Panel2: TPanel;
    edtExpression: TEdit;
    btnExecute: TButton;
    webBrowser: TWebBrowser;
    DOM: TXpObjModel;
    DateEdit: TDateTimePicker;
    lbNodes: TListView;
    Panel3: TPanel;
    btnConfirm: TButton;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    memConfirm: TMemo;
    PrintFilter: TXpFilterPrint;
    XSLProcessor: TXpXSLProcessor;
    wrkDOM: TXpObjModel;
    TextFilter: TXpFilterText;
    Panel8: TPanel;
    btnRefresh: TButton;
    HTMLFilter: TXpFilterHTML;
    lblStatusOrderBy: TLabel;
    cmbStatusOrderBy: TComboBox;
    cbStatusAscending: TCheckBox;
    rtfMemo: TRichEdit;
    Panel9: TPanel;
    RTFFilter: TXpFilterRTF;
    pbRefresh: TButton;
    btnOpen: TButton;
    btnPrint: TButton;
    procedure rgSearchTypeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbTechsChange(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure DateEditChange(Sender: TObject);
    procedure lbNodesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure rbLev1Click(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
  private
    { Private declarations }
    FXPathNodes : TXpNodeList;
    procedure BuildExpression;
  public
    { Public declarations }
  end;

var
  WorkOrders: TWorkOrders;

implementation

uses
  ShellAPI;

{$R *.DFM}

procedure TWorkOrders.rgSearchTypeClick(Sender: TObject);
begin
  case rgSearchType.ItemIndex of
    { Category }
    0 :
      begin
        cmbTechs.Visible := False;
        DateEdit.Visible := False;
        pnlCatCrit.Visible := True;
      end;
    { Date }
    1 :
      begin
        cmbTechs.Visible := False;
        pnlCatCrit.Visible := False;
        DateEdit.Visible := True;
      end;
    { Technician }
    2 :
      begin
        pnlCatCrit.Visible := False;
        DateEdit.Visible := False;
        cmbTechs.Visible := True;
        if cmbTechs.ItemIndex < 0 then
          cmbTechs.ItemIndex := 0;
      end;
  end;
  BuildExpression;
end;

procedure TWorkOrders.FormCreate(Sender: TObject);
begin
  rgSearchType.OnClick(Sender);
  if (not DOM.LoadDataSource('workorder.xml')) then
    ShowMessage('Unable to load WorkOrder database.');
end;

procedure TWorkOrders.BuildExpression;
var
  sName : string;
begin
  case rgSearchType.ItemIndex of
    0 :
      begin
        edtExpression.Text := '/WorkOrders/WorkOrder[Category="';
        if (rbLev1.Checked) then
          edtExpression.Text := edtExpression.Text + 'Technical"]'
        else if (rbLev2.Checked) then
          edtExpression.Text := edtExpression.Text + 'Admin"]'
        else
          edtExpression.Text := edtExpression.Text + 'Network"]';
      end;
    1 :
      edtExpression.Text := '/WorkOrders/WorkOrder[RequestDate="' +
                            DateToStr(DateEdit.Date) + '"]';
    2 :
      begin
        if cmbTechs.ItemIndex = 0 then
          sName := ''
        else
          sName := cmbTechs.Text;
        edtExpression.Text := '/WorkOrders/WorkOrder[AssignedTo="' +
                              sName + '"]';
      end;
  end;
end;

procedure TWorkOrders.cmbTechsChange(Sender: TObject);
begin
  BuildExpression;
end;

procedure TWorkOrders.btnExecuteClick(Sender: TObject);
const
  csNodesReturned = '%d nodes found';
  csNodeReturned = '1 node found';
var
  i          : Integer;
  oItem : TListItem;
begin
  lbNodes.Items.Clear;
  if FXPathNodes <> nil then
    FXPathNodes.Empty;
  lbNodes.Items.Clear;
  FXPathNodes := DOM.Document.SelectNodes(edtExpression.Text);
  if (FXPathNodes = nil) then
    StatusBar.Panels.Items[1].Text := Format(csNodesReturned, [0])
  else begin
    if FXPathNodes.Length = 1 then
      StatusBar.Panels.Items[1].Text := csNodeReturned
    else
      StatusBar.Panels.Items[1].Text := Format(csNodesReturned,
                                               [FXPathNodes.Length]);
    for i := 0 to Pred(FXPathNodes.Length) do begin
      oItem := lbNodes.Items.Add;
      oItem.Caption := FXPathNodes.Item(i).NodeName;
    end;
    if lbNodes.Items.Count > 0 then begin
      lbNodes.SetFocus;
      lbNodes.Selected := lbNodes.Items[0];
    end;
  end;

end;

procedure TWorkOrders.DateEditChange(Sender: TObject);
begin
  BuildExpression;
end;

procedure TWorkOrders.lbNodesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  oItem : TListItem;
begin
  if lbNodes.Items.Count > 0 then begin
    memWorkOrder.Lines.Clear;
    oItem := lbNodes.Selected;
    if oItem <> nil then
      memWorkOrder.Text := FXPathNodes.Item(oItem.Index).XmlDocument;
  end;  { if }
end;

procedure TWorkOrders.btnConfirmClick(Sender: TObject);
var
  oItem : TListItem;
  sText : string;
begin
  { If work order selected then prepare hardcopy printout and email
    confirmation. }
  oItem := lbNodes.Selected;
  if oItem <> nil then begin
    Pages.ActivePage := Confirm;
    sText := memWorkOrder.Text;
    wrkDOM.LoadMemory(sText[1], Length(sText));
    XSLProcessor.XmlObjModel := wrkDOM;
    XSLProcessor.RaiseErrors := False;
    { Generate the email confirmation. }
    XSLProcessor.Filter := TextFilter;
    XSLProcessor.StyleURL := 'confirm.xsl';
    if XSLProcessor.ApplyStyle then
      memConfirm.Text := TextFilter.Text
    else
      ShowMessage('Error generating confirm: ' + XSLProcessor.Errors[0]);

    { Generate the print preview of the hardcopy. }
    XSLProcessor.Filter := PrintFilter;
    XSLProcessor.StyleURL := 'workorder.xsl';
    if not XSLProcessor.ApplyStyle then
      ShowMessage('Error generating hardcopy: ' + XSLProcessor.Errors[0]);
  end;
end;

procedure TWorkOrders.FormShow(Sender: TObject);
begin
  Pages.ActivePage := Query;
  cmbStatusOrderBy.ItemIndex := 1;
    { Order by work number by default }
end;

type
  TXpXSLProcessorCracker = class(TXpXSLProcessor);
    { Gives us access to the stylesheet DOM. }

procedure TWorkOrders.btnRefreshClick(Sender: TObject);
var
  oList : TXpNodeList;
  oSort : TXpElement;
  oStyleDoc : TXpDocument;
  sDataType : string;
  sOrder : string;
  sSelect : string;
begin
  XSLProcessor.XmlObjModel := DOM;
  XSLProcessor.RaiseErrors := False;
  { Generate HTML page. }
  XSLProcessor.Filter := HTMLFilter;
  XSLProcessor.StyleURL := 'status.xsl';

  { We need to change the sort order dynamically. Our strategy is
    to find the xsl:sort element in the stylesheet DOM and change
    its select, order, and data-type attributes. }
  oStyleDoc := TXpXSLProcessorCracker(XSLProcessor).FStyleDOM.Document;
  oList := oStyleDoc.GetElementsByTagName('xsl:sort');
  { Did we find the xsl:sort element? }
  if oList.Length = 1 then begin
    { Yes. Change its attributes. }
    oSort := TXpElement(oList.Item(0));
    if cbStatusAscending.Checked then
      sOrder := 'ascending'
    else
      sOrder := 'descending';
    oSort.SetAttribute('order', sOrder);

    case cmbStatusOrderBy.ItemIndex of
      0 :
        begin
          sSelect := 'Category';
          sDataType := 'text';
        end;
      1 :
        begin
          sSelect := '@number';
          sDataType := 'number';
        end;
      2 :
        begin
          sSelect := 'AssignedTo';
          sDataType := 'text';
        end;
    end;  { case }
    oSort.SetAttribute('select', sSelect);
    oSort.SetAttribute('data-type', sDataType);

    if XSLProcessor.ApplyStyle then begin
      HTMLFilter.SaveToFile('c:\status.htm');
      webBrowser.Navigate('file://c:\status.htm');
    end
    else
      ShowMessage('Error generating status: ' + XSLProcessor.Errors[0]);
  end
  else
    ShowMessage('Could not find xsl:sort element in stylesheet.');
end;

procedure TWorkOrders.rbLev1Click(Sender: TObject);
begin
  BuildExpression;
end;

procedure TWorkOrders.pbRefreshClick(Sender: TObject);
begin
  XSLProcessor.XmlObjModel := DOM;
  XSLProcessor.RaiseErrors := False;
  { Generate the email confirmation. }
  XSLProcessor.Filter := RTFFilter;
  XSLProcessor.StyleURL := 'wklyrpt.xsl';
  if XSLProcessor.ApplyStyle then begin
    RTFFilter.SaveToFile('c:\wklyrpt.rtf');
    RTFMemo.Lines.LoadFromFile('c:\wklyrpt.rtf');
  end
  else
    ShowMessage('Error generating weekly report: ' + XSLProcessor.Errors[0]);
end;

procedure TWorkOrders.btnOpenClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'c:\wklyrpt.rtf', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage('Could not open RTF document using registered application');
end;

procedure TWorkOrders.btnPrintClick(Sender: TObject);
begin
  PrintFilter.Print('');
end;

end.
