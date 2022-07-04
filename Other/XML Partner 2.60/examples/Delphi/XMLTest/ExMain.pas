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
{* XMLPartner: ExMain.PAS 2.57                        *}
{*********************************************************}
{* XMLPartner: EXML Test example main file               *}
{*********************************************************}

unit ExMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus, XpDom, XpBase, XpParser, XpExcept;

type
  TMainForm = class(TForm)
    odFile: TOpenDialog;
    Parser: TXpParser;
    DOM: TXpObjModel;
    PageControl1: TPageControl;
    ParserTab: TTabSheet;
    ObjectModelTab: TTabSheet;
    Panel1: TPanel;
    lbData: TListBox;
    btnParse: TButton;
    Label1: TLabel;
    edtParse: TEdit;
    btnBrowse: TButton;
    Panel2: TPanel;
    memNodes: TMemo;
    lblFile: TLabel;
    ObjModelEdit: TEdit;
    btnBrowse2: TButton;
    btnDOMLoad: TButton;
    lblXMLRender: TLabel;
    lblXQLExpr: TLabel;
    edtSearch: TEdit;
    btnSearch: TButton;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    lblNumNodes: TLabel;
    NodesLabel: TLabel;
    procedure ParserStartDocument(Sender: TObject);
    procedure ParserEndDocument(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnBrowse2Click(Sender: TObject);
    procedure btnDOMLoadClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ParserAttribute(oOwner: TObject; sName,
      sValue: WideString; bSpecified: Boolean);
    procedure ParserCDATASection(oOwner: TObject; sValue: WideString);
    procedure ParserCharData(oOwner: TObject; sValue: WideString);
    procedure ParserComment(oOwner: TObject; sValue: WideString);
    procedure ParserDocTypeDecl(oOwner: TObject; sDecl, sId0,
      sId1: WideString);
    procedure ParserEndElement(oOwner: TObject; sValue: WideString);
    procedure ParserNonXMLEntity(oOwner: TObject; sEntityName,
      sPublicId, sSystemId, sNotationName: WideString);
    procedure ParserProcessingInstruction(oOwner: TObject; sName,
      sValue: WideString);
    procedure ParserStartElement(oOwner: TObject; sValue: WideString);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.ParserStartDocument(Sender : TObject);
begin
  lbData.Items.Add('Start document');
end;
{--------}
procedure TMainForm.ParserEndDocument(Sender : TObject);
begin
  lbData.Items.Add('End document');
end;
{--------}
procedure TMainForm.btnParseClick(Sender : TObject);
begin
  if edtParse.Text = '' then begin
    ShowMessage('You have not specified a file name.');
    exit;
  end;

  Screen.Cursor := crHourglass;
  try
    lbData.Items.BeginUpdate;
    try
      lbData.Clear;
      try
        if Parser.ParseDataSource(edtParse.Text) then
          ShowMessage('Parse succeeded!')
        else
          ShowMessage('Parse failed!');
      except
        on E: EXpParserError do
          ShowMessageFmt('Error on line: %d position: %d abs at: %d error msg: %s',
                         [E.Line, E.LinePos, E.FilePos, E.Reason]);
      end;
    finally
      lbData.Items.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;
{--------}
procedure TMainForm.btnBrowseClick(Sender : TObject);
begin
  if odFile.Execute then
    edtParse.Text := odFile.FileName;
end;
{--------}
procedure TMainForm.btnBrowse2Click(Sender : TObject);
begin
  if odFile.Execute then
    ObjModelEdit.Text := odFile.FileName;
end;
{--------}
procedure TMainForm.btnDOMLoadClick(Sender : TObject);
begin
  if ObjModelEdit.Text = '' then begin
    ShowMessage('You have not specified a file name.');
    Exit;
  end;

  Screen.Cursor := crHourglass;
  try
    memNodes.Lines.BeginUpdate;
    try
      memNodes.Clear;
      try
        if DOM.LoadDataSource(ObjModelEdit.Text) then begin
          memNodes.Text := DOM.XmlDocument;
          NodesLabel.Caption := '';
          ShowMessage('Load succeeded!');
        end else
          ShowMessage('Load failed!');
      except
        on X: EXpParserError do
          ShowMessageFmt('Error on line: %d position: %d abs at: %d error msg: %s',
                         [X.Line, X.LinePos, X.FilePos, X.Reason]);
      end;
    finally
      memNodes.Lines.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;
{--------}
procedure TMainForm.btnSearchClick(Sender : TObject);
var
  oList : TXpNodeList;
begin
  if (edtSearch.Text <> '') and
     (DOM.Document.DocumentElement <> nil) then begin
    oList := DOM.Document.DocumentElement.SelectNodes(edtSearch.Text);
    try
      NodesLabel.Caption := IntToStr(oList.Length);
      memNodes.Clear;
      memNodes.Text := oList.XmlDocument;
    finally
      oList.Free;
    end;
  end;
end;
{--------}
procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;
{--------}
procedure TMainForm.ParserAttribute(oOwner     : TObject;
                                    sName,
                                    sValue     : WideString;
                                    bSpecified : Boolean);
begin
  lbData.Items.Add(Format('Attribute = %s:%s', [sName, sValue]));
end;
{--------}
procedure TMainForm.ParserCDATASection(oOwner : TObject;
                                       sValue : WideString);
begin
  lbData.Items.Add(Format('CDATA Section = [%s]', [sValue]));
end;
{--------}
procedure TMainForm.ParserCharData(oOwner: TObject; sValue : WideString);
begin
  lbData.Items.Add(Format('Char data = [%s]', [sValue]));
end;
{--------}
procedure TMainForm.ParserComment(oOwner : TObject;
                                  sValue : WideString);
begin
  lbData.Items.Add('Comment = ' + sValue);
end;
{--------}
procedure TMainForm.ParserDocTypeDecl(oOwner : TObject;
                                      sDecl,
                                      sId0,
                                      sId1   : WideString);
begin
  lbData.Items.Add(Format('Doc type decl = %s:%s:%s',
                          [sDecl, sId0, sId1]));
end;
{--------}
procedure TMainForm.ParserEndElement(oOwner : TObject;
                                     sValue : WideString);
begin
  lbData.Items.Add('End element = ' + sValue);
end;
{--------}
procedure TMainForm.ParserNonXMLEntity(oOwner        : TObject;
                                       sEntityName,
                                       sPublicId,
                                       sSystemId,
                                       sNotationName : WideString);
begin
  lbData.Items.Add(Format('NON-XML = %s:%s:%s:%s',
                          [sEntityName,
                           sPublicID,
                           sSystemID,
                           sNotationName]));
end;
{--------}
procedure TMainForm.ParserProcessingInstruction(oOwner : TObject;
                                                sName,
                                                sValue : WideString);
begin
  lbData.Items.Add(Format('Processing Instruction = %s:%s',
                          [sName, sValue]));
end;
{--------}
procedure TMainForm.ParserStartElement(oOwner : TObject;
                                       sValue : WideString);
begin
  lbData.Items.Add('Start element = ' + sValue);
end;

end.
