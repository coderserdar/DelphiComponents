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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * Eivind Bakkestuen
 * Used with permission.
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit fmFRFFEEngine;

interface

uses
  SysUtils, Classes, fflllog, ffdb, DB, ffdbbase, ffllcomm, fflllgcy,
  ffllbase, ffllcomp, fflleng, ffsrintm, ffclreng, FR_Class, FR_Desgn,
  FR_DSet, FR_DBSet, FR_PTabl, FR_FFDB;

type
  TdmFRFFEEngine = class(TDataModule)
    ffRemoteEngine: TFFRemoteServerEngine;
    ffLegacyTransport: TffLegacyTransport;
    ffClient: TffClient;
    ffSession: TffSession;
    ffDatabase: TffDatabase;
    ffEventLog: TffEventLog;
    frDesigner: TfrDesigner;
    frReport: TfrReport;
    frPrintTable: TfrPrintTable;
    frFFComponents: TfrFFComponents;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmFRFFEEngine: TdmFRFFEEngine;

implementation

{$R *.dfm}

Uses
  Forms;

procedure TdmFRFFEEngine.DataModuleCreate(Sender: TObject);
begin
  ffEventLog.FileName := ExtractFilePath(Application.ExeName)+'\ffe.log';
  ffLegacyTransport.Enabled := False;
  ffClient.Active := False;
  ffClient.AutoClientName := True;
  ffSession.ClientName := ffClient.ClientName;
  ffSession.AutoSessionName := True;
  ffDatabase.SessionName := ffSession.SessionName;
end;

end.
