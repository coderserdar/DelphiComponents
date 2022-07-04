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
  XpDOM, StdCtrls, XpBase, XpvXSLT, XpvXSLPr;

type
  TfrmMain = class(TForm)
    btnConvert: TButton;
    CSVFile: TMemo;
    DOM: TXpObjModel;
    XSLProc: TXpXSLProcessor;
    procedure XslProcApplyStyleStart(Sender : TObject);
    procedure XslProcApplyStyleEnd(Sender : TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure XSLProcElementEnd(oOwner: TObject; sValue: WideString);
    procedure XSLProcElementStart(oOwner: TObject; sValue: WideString);
    procedure XSLProcText(oOwner: TObject; sValue: WideString);
  private
    { Private declarations }
    CurrentRecord : string;
    XMLDocument : string;
    XSLDocument : string;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.XslProcApplyStyleStart(Sender : TObject);
begin
  {Ensure data file is empty}
  CSVFile.Clear;
end;

procedure TfrmMain.XslProcApplyStyleEnd(Sender : TObject);
begin
  {All records processed. Save CSV file to disk.}
  CSVFile.Lines.SaveToFile('DailyWeather.txt');
end;

procedure TfrmMain.btnConvertClick(Sender: TObject);
begin
  XMLDocument := '..\..\xmldocs\weather.xml';
  XSLDocument := 'csvdemo.xsl';
  if (DOM.LoadDataSource(XMLDocument)) then begin
    XSLProc.StyleURL := XSLDocument;
    if (not XSLProc.ApplyStyle) then
      ShowMessage('Unable to transform weather report')
  end else
    ShowMessage('Unable to parse weather report: ' +
                DOM.GetErrorMsg(0));
end;

procedure TfrmMain.XSLProcElementEnd(oOwner : TObject;
                                     sValue : WideString);
begin
  {Add the current record to the new file.}
  CSVFile.Lines.Add(CurrentRecord);
end;

procedure TfrmMain.XSLProcElementStart(oOwner : TObject;
                                       sValue : WideString);
begin
  {Clear out the current record to prepare for new city's data}
  CurrentRecord := '';
end;

procedure TfrmMain.XSLProcText(oOwner: TObject; sValue: WideString);
begin
  {Is this the first field in the record?}
  if (CurrentRecord = '') then
    {Yes. No need to separate new field with a comma.}
    CurrentRecord := sValue
  else
    {No. Add a comma delimiter and the new data to the current record.}
    CurrentRecord := CurrentRecord + ',' + sValue;
end; 

end.
