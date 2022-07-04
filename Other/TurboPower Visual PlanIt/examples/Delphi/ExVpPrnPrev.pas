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

unit ExVpPrnPrev;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,

  VpBaseDS, VpBase, VpPrtPrv, VpPrtFmt, VpPrtFmtCBox;

type
  TfrmPrnPreview = class(TForm)
    VpPrintPreview1: TVpPrintPreview;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    VpPrintFormatComboBox1: TVpPrintFormatComboBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    function GetControlLink: TVpControlLink;
    procedure SetControlLink(const Value: TVpControlLink);
    procedure SetSize(const SizeStr: string);
    function GetDate(Index: Integer) : TDateTime;
    procedure SetDate(Index: Integer; Value: TDateTime);
    function GetDayLimit(Index: Integer): TVpHours;
    procedure SetDayLimit(Index: Integer; Value: TVpHours);
    { Private declarations }
  public
    { Public declarations }
    property ControlLink : TVpControlLink
      read GetControlLink write SetControlLink;

    property StartDate : TDateTime index 1
      read GetDate write SetDate;
    property EndDate : TDateTime index 2
      read GetDate write SetDate;

    property DayStart : TVpHours index 1
      read GetDayLimit write SetDayLimit;
    property DayEnd : TVpHours index 2
      read GetDayLimit write SetDayLimit;


    procedure Execute;
  end;

var
  frmPrnPreview: TfrmPrnPreview;


implementation

{$R *.DFM}

{TfrmPrintPreview}

procedure TfrmPrnPreview.FormCreate(Sender: TObject);
begin
  VpPrintPreview1.Align := alClient;
  SetSize('Actual Size');
end;

procedure TfrmPrnPreview.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmPrnPreview.Execute;
begin
  ShowModal;
end;

function TfrmPrnPreview.GetControlLink: TVpControlLink;
begin
  Result := VpPrintPreview1.ControlLink;
end;

procedure TfrmPrnPreview.SetControlLink(const Value: TVpControlLink);
begin
  VpPrintPreview1.ControlLink := Value;
  VpPrintFormatComboBox1.ControlLink := Value;
end;

procedure TfrmPrnPreview.SetSize(const SizeStr : string);
begin
  if SizeStr = 'Actual Size' then VpPrintPreview1.ZoomFactor :=  zfActualSize;
  if SizeStr = 'Fit to Control' then VpPrintPreview1.ZoomFactor := zfFitToControl;
  if SizeStr = '25%' then VpPrintPreview1.ZoomFactor := zf25Percent;
  if SizeStr = '33%' then VpPrintPreview1.ZoomFactor := zf33Percent;
  if SizeStr = '50%' then VpPrintPreview1.ZoomFactor := zf50Percent;
  if SizeStr = '67%' then VpPrintPreview1.ZoomFactor := zf67Percent;
  if SizeStr = '75%' then VpPrintPreview1.ZoomFactor := zf75Percent;
  ComboBox1.Text := SizeStr;
end;

procedure TfrmPrnPreview.ComboBox1Change(Sender: TObject);
begin
  SetSize(ComboBox1.Items[ComboBox1.ItemIndex]);
end;

function TfrmPrnPreview.GetDate(Index: Integer) : TDateTime;
begin
  Result := 0.0;
  case Index of
    1: Result := VpPrintPreview1.StartDate;
    2: Result := VpPrintPreview1.EndDate;
  end;
end;

procedure TfrmPrnPreview.SetDate(Index: Integer;
  Value: TDateTime);
begin
  case Index of
    1: VpPrintPreview1.StartDate := Value;
    2: VpPrintPreview1.EndDate := Value;
  end;
end;

function TfrmPrnPreview.GetDayLimit(Index: Integer) : TVpHours;
begin
  Result := h_00;
  case Index of
    1: Result := VpPrintPreview1.ControlLink.Printer.DayStart;
    2: Result := VpPrintPreview1.ControlLink.Printer.DayEnd;
  end;
end;

procedure TfrmPrnPreview.SetDayLimit(Index: Integer; Value: TVpHours);
begin
  case Index of
    1: VpPrintPreview1.ControlLink.Printer.DayStart := Value;
    2: VpPrintPreview1.ControlLink.Printer.DayEnd := Value;
  end;
end;

end.
 
