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

unit ExFF2SimplePlannerU1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VpDlg, VpResEditDlg, StdCtrls, VpTaskList, VpMonthView, VpBase, VpBaseDS,
  VpDayView, VpDBDS, VpFF2DS, ComCtrls, ExtCtrls, VpContactGrid,
  VpTaskEditDlg, VpData, VpCalendar, VpBDEDS;

type
  TFrmSimplePlanner = class(TForm)
    VpControlLink1: TVpControlLink;
    VpResourceEditDialog1: TVpResourceEditDialog;
    Panel1: TPanel;
    VpDayView1: TVpDayView;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Panel3: TPanel;
    VpResourceCombo1: TVpResourceCombo;
    Button1: TButton;
    TrackBar1: TTrackBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VpTaskList1: TVpTaskList;
    VpContactGrid1: TVpContactGrid;
    VpTaskEditDialog1: TVpTaskEditDialog;
    Button2: TButton;
    Button3: TButton;
    TabSheet3: TTabSheet;
    VpCalendar1: TVpCalendar;
    GranularityCombo: TComboBox;
    CheckBox1: TCheckBox;
    VpMonthView1: TVpMonthView;
    VpFF2DataStore1: TVpFF2DataStore;
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure GranularityComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
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

procedure TFrmSimplePlanner.TrackBar1Change(Sender: TObject);
begin
  VpDayView1.NumDays := TrackBar1.Position;
end;

procedure TFrmSimplePlanner.Button2Click(Sender: TObject);
begin
  VpTaskEditDialog1.Execute(VpFF2DataStore1.Resource.Tasks.GetTask(VpTaskList1.TaskIndex));
end;

procedure TFrmSimplePlanner.Button3Click(Sender: TObject);
var
  Task: TVpTask;

begin
  Task := VpFF2DataStore1.Resource.Tasks.AddTask(VpFF2DataStore1.GetNextID('Tasks'));
  if VpTaskEditDialog1.Execute(Task) then
    VpFF2DataStore1.PostTasks;
end;

procedure TFrmSimplePlanner.GranularityComboChange(Sender: TObject);
begin
  case GranularityCombo.ItemIndex of
  0: VpDayView1.Granularity := GR05Min;
  1: VpDayView1.Granularity := GR06Min;
  2: VpDayView1.Granularity := GR10Min;
  3: VpDayView1.Granularity := GR15Min;
  4: VpDayView1.Granularity := GR20Min;
  5: VpDayView1.Granularity := GR30Min;
  6: VpDayView1.Granularity := GR60Min;
  end;
end;

procedure TFrmSimplePlanner.FormCreate(Sender: TObject);
begin
  GranularityCombo.ItemIndex := 5;
end;

procedure TFrmSimplePlanner.CheckBox1Click(Sender: TObject);
begin
  VpDayView1.IncludeWeekends := CheckBox1.Checked;
end;

end.
 
