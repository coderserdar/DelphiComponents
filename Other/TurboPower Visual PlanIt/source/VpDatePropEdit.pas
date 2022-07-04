{*********************************************************}
{*                 VPDATEPROPEDIT.PAS 1.03               *}
{*********************************************************}

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

unit VpDatePropEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VpEdPop, VpDateEdit, VpBase, VpBaseDS, VpCalendar;

type
  TfrmDatePropertyEditor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    VpCalendar1: TVpCalendar;
  private
    { Private declarations }
  public
    function Execute : Boolean;
    { Public declarations }
  end;

var
  frmDatePropertyEditor: TfrmDatePropertyEditor;

implementation

{$R *.DFM}

function TfrmDatePropertyEditor.Execute : Boolean;
begin
  if ShowModal = mrOk then
    Result := True
  else
    Result := False;
end;

end.
  
