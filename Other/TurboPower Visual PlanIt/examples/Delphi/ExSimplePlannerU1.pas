{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

unit ExSimplePlannerU1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VpDlg, VpResEditDlg, StdCtrls, VpTaskList, VpMonthView, VpBase, VpBaseDS,
  VpDayView, VpDBDS, VpBDEDS;

type
  TFrmSimplePlanner = class(TForm)
    VpBDEDataStore1: TVpBDEDataStore;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpMonthView1: TVpMonthView;
    VpTaskList1: TVpTaskList;
    Button1: TButton;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpResourceCombo1: TVpResourceCombo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSimplePlanner: TFrmSimplePlanner;

implementation

{$R *.DFM}

procedure TFrmSimplePlanner.Button1Click(Sender: TObject);
begin
  VpResourceEditDialog1.AddNewResource;
end;

end.
 
