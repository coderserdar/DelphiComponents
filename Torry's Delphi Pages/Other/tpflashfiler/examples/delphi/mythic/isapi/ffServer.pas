{*********************************************************}
{* FlashFiler: ISAPI example server data module          *}
{*********************************************************}

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
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit ffServer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ffllcomm, fflllgcy, ffsqlbas, ffsqleng, ffclreng, ffllbase, ffllcomp,
  fflleng, ffsrintm, ffsreng, ffllprot;

{Comment out the following define to use an embedded server engine. }
{$DEFINE UseRemoteServer}

{$IFDEF UseRemoteServer}
const
  aProtocol = ptRegistry;
  aServerName = 'FF2@127.0.0.1';
{$ENDIF}

type
  TdmServer = class(TDataModule)
    embeddedServer: TffServerEngine;
    remoteServer: TFFRemoteServerEngine;
    embeddedSQL: TffSqlEngine;
    tranMain: TffLegacyTransport;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    {$IFDEF UseRemoteServer}
    property ServerEngine : TffRemoteServerEngine read remoteServer;
    {$ELSE}
    property ServerEngine : TffServerEngine read embeddedServer;
    {$ENDIF}
  end;

var
  dmServer: TdmServer;

implementation

{$R *.DFM}

procedure TdmServer.DataModuleCreate(Sender: TObject);
begin
  {$IFDEF UseRemoteServer}
  tranMain.Protocol := aProtocol;
  tranMain.ServerName := aServerName;
  {$ENDIF}
end;

end.
