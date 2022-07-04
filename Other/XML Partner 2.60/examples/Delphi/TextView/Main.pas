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
  XpParser, StdCtrls, ComCtrls, XpBase;

type
  TfrmMain = class(TForm)
    btnParse: TButton;
    memo: TRichEdit;
    edtFile: TEdit;
    fodXMLDoc: TOpenDialog;
    btnOpenFile: TButton;
    Label1: TLabel;
    Parser: TXpParser;
    procedure btnParseClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure ParserAttribute(oOwner: TObject; sName, sValue: WideString;
      bSpecified: Boolean);
    procedure ParserStartElement(oOwner: TObject; sValue: WideString);
    procedure ParserEndElement(oOwner: TObject; sValue: WideString);
    procedure ParserCharData(oOwner: TObject; sValue: WideString);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.btnParseClick(Sender: TObject);
begin
  Memo.Lines.Clear;
  if edtFile.Text <> '' then begin
    if not Parser.ParseDataSource(edtFile.Text) then
      Memo.Lines.Assign(Parser.Errors);
  end else
    ShowMessage('You must select an XML document to parse.');
end;

procedure TfrmMain.btnOpenFileClick(Sender: TObject);
begin
  fodXMLDoc.Execute;
  edtFile.Text := fodXMLDoc.FileName;
end;

procedure TfrmMain.ParserAttribute(oOwner: TObject; sName,
  sValue: WideString; bSpecified: Boolean);
begin
  if sName = 'FontName' then
    Memo.SelAttributes.Name := sValue
  else if sName = 'PointSize' then
    Memo.SelAttributes.Size := StrToInt(sValue)
  else if sName = 'Color' then
    Memo.SelAttributes.Color := StrToInt(sValue);
end;

procedure TfrmMain.ParserStartElement(oOwner: TObject; sValue: WideString);
begin
  if sValue = 'B' then
    Memo.SelAttributes.Style := Memo.SelAttributes.Style + [fsBold];
  if sValue = 'I' then
    Memo.SelAttributes.Style := Memo.SelAttributes.Style + [fsItalic];
  if sValue = 'U' then
    Memo.SelAttributes.Style := Memo.SelAttributes.Style + [fsUnderline];
end;

procedure TfrmMain.ParserEndElement(oOwner: TObject; sValue: WideString);
begin
  if sValue = 'B' then
    Memo.SelAttributes.Style := Memo.SelAttributes.Style - [fsBold];
  if sValue = 'I' then
    Memo.SelAttributes.Style := Memo.SelAttributes.Style - [fsItalic];
  if sValue = 'U' then
    Memo.SelAttributes.Style := Memo.SelAttributes.Style - [fsUnderline];
end;

procedure TfrmMain.ParserCharData(oOwner: TObject; sValue: WideString);
begin
  Memo.SelLength := 0;
  Memo.SelText := sValue;
end;

end.
