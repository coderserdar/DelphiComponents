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

unit ExVpRptSetup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,

  VpBaseDS, VpPrtFmtCBox, VpEdPop, VpDateEdit;

type
  TReportDataRec = record
    StartDate, EndDate : TDateTime;
    Format : string;
  end;

  TfrmReportSetup = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    VpDateEdit1: TVpDateEdit;
    VpDateEdit2: TVpDateEdit;
    VpPrintFormatComboBox1: TVpPrintFormatComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure SaveData;

    function GetControlLink: TVpControlLink;
    procedure SetControlLink(const Value: TVpControlLink);
    function GetDate(Index: Integer): TDateTime;
    procedure SetDate(Index: Integer; Value: TDateTime);
    { Private declarations }
  public
    { Public declarations }
    function Execute : Boolean;
    property ControlLink : TVpControlLink
      read GetControlLink write SetControlLink;
    property StartDate : TDateTime index 1
      read GetDate write SetDate;
    property EndDate : TDateTime index 2
      read GetDate write SetDate;
  end;

var
  frmReportSetup: TfrmReportSetup;
  ReportData : TReportDataRec;

implementation

{$R *.DFM}

{ TfrmReportSetup }

function TfrmReportSetup.Execute : Boolean;
begin
  Result := ShowModal = mrOk;
end;
{=====}
procedure TfrmReportSetup.Button1Click(Sender: TObject);
begin
  SaveData;
  ModalResult := mrOk;
end;
{=====}
procedure TfrmReportSetup.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{=====}
procedure TfrmReportSetup.SaveData;
begin
  ReportData.StartDate := VpDateEdit1.Date;
  ReportData.EndDate := VpDateEdit2.Date;
  ReportData.Format := VpPrintFormatComboBox1.Text;
end;
{=====}
function TfrmReportSetup.GetControlLink: TVpControlLink;
begin
  Result := VpPrintFormatComboBox1.ControlLink;
end;
{=====}
procedure TfrmReportSetup.SetControlLink(const Value: TVpControlLink);
begin
  VpPrintFormatComboBox1.ControlLink := Value;
end;
{=====}
function TfrmReportSetup.GetDate(Index: Integer) : TDateTime;
begin
  Result := 0.0;
  case Index of
    1: Result := VpDateEdit1.Date;
    2: Result := VpDateEdit2.Date;
  end;
end;
{=====}
procedure TfrmReportSetup.SetDate(Index: Integer;
  Value: TDateTime);
begin
  case Index of
    1: VpDateEdit1.Date := Value;
    2: VpDateEdit2.Date := Value;
  end;
end;
{=====}
end.
 
