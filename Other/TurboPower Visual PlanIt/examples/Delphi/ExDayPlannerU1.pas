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

unit ExDayPlannerU1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus, Buttons, ImgList, Printers, TypInfo,

  VpBase, VpMisc, VpNavBar, VpContactGrid, VpBaseDS, VpTaskList, VpDBDS, VpBDEDS,
  VpMonthView, VpDayView, VpDlg, VpResEditDlg, VpTaskEditDlg, VpContactEditDlg,
  VpEvntEditDlg, VpData, VpLEDLabel, VpClock, VpWeekView, VpPrtPrvDlg, VpPrtFmt,
  VpPrtFmtCBox, VpEdPop, VpDateEdit, VpPrtFmtDlg, VpContactButtons;

type
  TMainForm = class(TForm)
    Bevel1: TBevel;
    BtnAddRes: TSpeedButton;
    BtnApplyRes: TSpeedButton;
    BtnDelRes: TSpeedButton;
    BtnUndoRes: TSpeedButton;
    EditResDescr: TEdit;
    EditResID: TEdit;
    HeaderImage: TImage;
    ImageList1: TImageList;
    LabelContacts: TLabel;
    LabelContactsLED: TVpLEDLabel;
    LabelDayCount: TLabel;
    LabelDescription: TLabel;
    LabelEventsLED: TVpLEDLabel;
    LabelHeader: TLabel;
    LabelLoadedEvents: TLabel;
    LabelNotes: TLabel;
    LabelResID: TLabel;
    LabelResource2: TLabel;
    LabelResource: TLabel;
    LabelTasks: TLabel;
    LabelTasksLED: TVpLEDLabel;
    MainMenu: TMainMenu;
    MemoResNotes: TMemo;
    mnuAddScheduleItem: TMenuItem;
    mnuDeleteScheduleItem: TMenuItem;
    mnuDeleteTask: TMenuItem;
    mnuEditScheduleItem: TMenuItem;
    mnuEditTask: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFilePrint: TMenuItem;
    mnuFilePrintPreview: TMenuItem;
    mnuFilePrintSetup: TMenuItem;
    mnuFileReportSetup: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuFileLoadPrintFormats: TMenuItem;
    mnuMaintenance: TMenuItem;
    mnuMaintResources: TMenuItem;
    mnuNewTask: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelBase: TPanel;
    PanelCalendar: TPanel;
    PanelCalSplit1: TPanel;
    PanelCalSplit2: TPanel;
    PanelContacts: TPanel;
    PanelDayCount: TPanel;
    PanelDisplay: TPanel;
    PanelFunction: TPanel;
    PanelHeader: TPanel;
    PanelTasks: TPanel;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    SchedulePopup: TPopupMenu;
    Sep1: TMenuItem;
    Sep2: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    TaskPopup: TPopupMenu;
    TrackBar1: TTrackBar;
    VpBDEDataStore1: TVpBDEDataStore;
    VpClock1: TVpClock;
    VpContactGrid1: TVpContactGrid;
    VpControlLink1: TVpControlLink;
    VpDayView1: TVpDayView;
    VpEventEditDialog1: TVpEventEditDialog;
    VpMonthView1: TVpMonthView;
    VpNavBar1: TVpNavBar;
    VpPrintFormatEditDialog1: TVpPrintFormatEditDialog;
    VpPrintPreviewDialog1: TVpPrintPreviewDialog;
    VpResourceCombo1: TVpResourceCombo;
    VpResourceCombo2: TVpResourceCombo;
    VpResourceEditDialog1: TVpResourceEditDialog;
    VpTaskEditDialog1: TVpTaskEditDialog;
    VpTaskList1: TVpTaskList;
    VpTaskList2: TVpTaskList;
    mnuFileEditPrintFormats: TMenuItem;
    PanelMaintResources: TPanel;
    VpContactButtonBar1: TVpContactButtonBar;

    procedure FormCreate(Sender: TObject);

    procedure BtnAddResClick(Sender: TObject);
    procedure BtnDelResClick(Sender: TObject);
    procedure EditResDescrChange(Sender: TObject);

    procedure mnuAddScheduleItemClick(Sender: TObject);
    procedure mnuDeleteScheduleItemClick(Sender: TObject);
    procedure mnuDeleteTaskClick(Sender: TObject);
    procedure mnuEditScheduleItemClick(Sender: TObject);
    procedure mnuEditTaskClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuFileLoadPrintFormatsClick(Sender: TObject);
    procedure mnuFilePrintClick(Sender: TObject);
    procedure mnuFilePrintPreviewClick(Sender: TObject);
    procedure mnuFilePrintSetupClick(Sender: TObject);
    procedure mnuFileReportSetupClick(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuMaintResourcesClick(Sender: TObject);
    procedure mnuNewTaskClick(Sender: TObject);

    procedure ResChange(Sender: TObject);

    procedure TrackBar1Change(Sender: TObject);

    procedure VpBDEDataStore1ResourceChange(Sender: TObject; Resource: TVpResource);
    procedure VpNavBar1FolderChange(Sender: TObject; Index: Integer; var AllowChange: Boolean; Dragging: Boolean);
    procedure VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; Index: Integer);
    procedure mnuFileEditPrintFormatsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    procedure DoPrintCurrent;
    procedure SetActivePage(Page: Integer);
    procedure LoadMaintResFields;
    procedure LoadFormats;
    procedure MakeDefaultFormats;
  public

  end;

var
  MainForm: TMainForm;

const
  CalendarPage      = 0;
  ContactsPage      = 1;
  TasksPage         = 2;
  MaintResourcePage = 10;

implementation

uses
  ExVpRptSetup, ExVpAbout;


{$R *.DFM}
procedure TMainForm.FormCreate(Sender: TObject);
var
  Bmp : TBitmap;
begin
  { Load ImageList from the resource }
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := LoadBitmap(hInstance, 'ROLODEX');
    ImageList1.AddMasked(Bmp, clOlive);
    Bmp.Handle := LoadBitmap(hInstance, 'TODOLIST');
    ImageList1.AddMasked(Bmp, clOlive);
    Bmp.Handle := LoadBitmap(hInstance, 'JOURNAL');
    ImageList1.AddMasked(Bmp, clOlive);
    Bmp.Handle := LoadBitmap(hInstance, 'CALENDAR');
    ImageList1.AddMasked(Bmp, clOlive);
    Bmp.Handle := LoadBitmap(hInstance, 'PEOPLE');
    ImageList1.AddMasked(Bmp, clOlive);
  finally
    Bmp.Free;
  end;

  SetActivePage(CalendarPage);
  MakeDefaultFormats;
  HeaderImage.AutoSize := True;
end;
{=====}
procedure TMainForm.BtnAddResClick(Sender: TObject);
begin
  VpResourceEditDialog1.AddNewResource;
end;
{=====}
procedure TMainForm.BtnDelResClick(Sender: TObject);
begin
  if VpBDEDataStore1.Resource <> nil then begin
    if MessageDlg('This will permanently delete the resource and all of '
      + 'its Event, Contact and Task data.'#13#10'Are you sure?', mtConfirmation,
      [mbYes, mbNo], 0) = mrYes
    then begin
      VpBDEDataStore1.PurgeResource(VpBDEDataStore1.Resource);
      LoadMaintResFields;
    end;
  end;
end;
{=====}
procedure TMainForm.DoPrintCurrent;
begin
  if PrintDialog1.Execute then begin
    Printer.BeginDoc;
    VpControlLink1.Printer.CurFormat := VpControlLink1.Printer.Find(ReportData.Format);
    VpControlLink1.Printer.Print(Printer, ReportData.StartDate, ReportData.EndDate);
    Printer.EndDoc;
  end;
end;
{=====}
procedure TMainForm.EditResDescrChange(Sender: TObject);
begin
  BtnUndoRes.Enabled := True;
  BtnApplyRes.Enabled:= True;
end;
{=====}
procedure TMainForm.LoadFormats;
begin
  if OpenDialog1.Execute then begin
    VpControlLink1.Printer.LoadFromFile(OpenDialog1.FileName, False);
  end;
end;
{=====}
procedure TMainForm.LoadMaintResFields;
begin
  if VpBDEDataStore1.Resource = nil then begin
    EditResDescr.Enabled := False;
    MemoResNotes.Enabled := False;
    EditResDescr.Color := clBtnFace;
    MemoResNotes.Color := clBtnFace;
    EditResID.Text :=  '';
    EditResDescr.Text := '';
    MemoResNotes.Text := '';
  end else begin
    EditResDescr.Enabled := True;
    MemoResNotes.Enabled := True;
    EditResDescr.Color := clWindow;
    MemoResNotes.Color := clWindow;
    EditResID.Text :=  IntToStr(VpBDEDataStore1.ResourceID);
    EditResDescr.Text := VpBDEDataStore1.Resource.Description;
    MemoResNotes.Text := VpBDEDataStore1.Resource.Notes;
  end;
  BtnUndoRes.Enabled := False;
  BtnApplyRes.Enabled:= False;
end;
{=====}
procedure TMainForm.MakeDefaultFormats;
var
  NewFormat : TVpPrintFormatItem;
  NewElement : TVpPrintFormatElementItem;
  it : TVpItemType;
  Nm : string;
begin
  for it := Low(TVpItemType) to High(TVpItemType) do begin
    { portrait layout }
    NewFormat := TVpPrintFormatItem(VpControlLink1.Printer.PrintFormats.Add);
    Nm := GetEnumName(TypeInfo(TVpItemType), Ord(it));
    NewFormat.FormatName := Copy(Nm, 3, Length(Nm) - 2) + ' Format Test (portrait)';
    NewElement := TVpPrintFormatElementItem(NewFormat.Elements.Add);
    NewElement.ElementName := 'Element 1';
    NewElement.ItemType := it;

    { landscape layout }
    NewFormat := TVpPrintFormatItem(VpControlLink1.Printer.PrintFormats.Add);
    Nm := GetEnumName(TypeInfo(TVpItemType), Ord(it));
    NewFormat.FormatName := Copy(Nm, 3, Length(Nm) - 2) + ' Format Test (landscape)';
    NewElement := TVpPrintFormatElementItem(NewFormat.Elements.Add);
    NewElement.ElementName := 'Element 1';
    NewElement.ItemType := it;
    NewElement.Rotation := ra270;
  end;
end;
{=====}
procedure TMainForm.mnuAddScheduleItemClick(Sender: TObject);
var
  StartTime, EndTime : TDateTime;
begin
  StartTime := LineToStartTime(vpDayView1.ActiveRow, vpDayView1.Granularity);
  EndTime := LineToStartTime(vpDayView1.ActiveRow + 1, vpDayView1.Granularity);
  VpEventEditDialog1.AddNewEvent(StartTime, EndTime);
end;
{=====}
procedure TMainForm.mnuDeleteScheduleItemClick(Sender: TObject);
begin
  if (VpDayView1.ActiveRow > -1) and (VpDayView1.ActiveCol > -1) then
    VpDayView1.DeleteActiveEvent(True)
  else
    ShowMessage('You must select a schedule item to delete');
end;
{=====}
procedure TMainForm.mnuDeleteTaskClick(Sender: TObject);
begin
  if Assigned(VpTaskList2.ActiveTask) then
    VpTaskList2.DeleteActiveTask(True)
  else
    ShowMessage('You must select a task to delete');
end;
{=====}
procedure TMainForm.mnuEditScheduleItemClick(Sender: TObject);
var
  StartTime, EndTime : TDateTime;
begin
  StartTime := LineToStartTime(VpDayView1.ActiveRow, VpDayView1.Granularity);
  EndTime := LineToStartTime(VpDayView1.ActiveRow + 1, VpDayView1.Granularity);

  if Assigned(VpDayView1.ActiveEvent) then
    VpEventEditDialog1.Execute(VpDayView1.ActiveEvent)
  else
    VpEventEditDialog1.AddNewEvent(StartTime, EndTime);
end;
{=====}
procedure TMainForm.mnuEditTaskClick(Sender: TObject);
begin
  if Assigned(VpTaskList2.ActiveTask) then
    VpTaskEditDialog1.Execute(VpTaskList2.ActiveTask)
  else
    VpTaskEditDialog1.AddNewTask;
end;
{=====}
procedure TMainForm.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;
{=====}
procedure TMainForm.mnuFileLoadPrintFormatsClick(Sender: TObject);
begin
  LoadFormats;
end;
{=====}
procedure TMainForm.mnuFilePrintClick(Sender: TObject);
begin
  DoPrintCurrent;
end;
{=====}
procedure TMainForm.mnuFilePrintPreviewClick(Sender: TObject);
begin
  if VpControlLink1.Printer.PrintFormats.Count > 0 then begin
    VpPrintPreviewDialog1.StartDate := VpMonthView1.Date;
    VpPrintPreviewDialog1.EndDate := VpMonthView1.Date + 30;
    if VpPrintPreviewDialog1.Execute then
      DoPrintCurrent;
  end
  else
    ShowMessage('No print format defined');
end;
{=====}
procedure TMainForm.mnuFilePrintSetupClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;
{=====}
procedure TMainForm.mnuFileReportSetupClick(Sender: TObject);
begin
  frmReportSetup.StartDate := VpMonthView1.Date;
  frmReportSetup.EndDate := VpMonthView1.Date;
  frmReportSetup.ControlLink := VpControlLink1;
  frmReportSetup.Execute;
end;
{=====}
procedure TMainForm.mnuHelpAboutClick(Sender: TObject);
begin
  // Display about box
  frmAbout.Execute;
end;
{=====}
procedure TMainForm.mnuMaintResourcesClick(Sender: TObject);
begin
  SetActivePage(10);
  VpNavBar1.ActiveFolder := 1;
end;
{=====}
procedure TMainForm.mnuNewTaskClick(Sender: TObject);
begin
  VpTaskEditDialog1.AddNewTask;
end;
{=====}
procedure TMainForm.ResChange(Sender: TObject);
begin
  BtnUndoRes.Enabled := False;
  BtnApplyRes.Enabled:= False;

  if Sender = BtnUndoRes then begin
    LoadMaintResFields;
  end

  else begin
    VpBDEDataStore1.Resource.Description := EditResDescr.Text;
    VpBDEDataStore1.Resource.Notes := MemoResNotes.Text;
    VpBDEDataStore1.PostResources;
  end;
end;
{=====}
procedure TMainForm.SetActivePage(Page: Integer);
var
  Bmp: TBitmap;
begin
  PanelCalendar.Visible := False;
  PanelCalendar.Align := alClient;

  PanelContacts.Visible := False;
  PanelContacts.Align := alClient;

  PanelTasks.Visible := False;
  PanelTasks.Align := alClient;

  PanelMaintResources.Visible := False;
  PanelMaintResources.Align := alClient;

  Bmp := TBitmap.Create;
  try

    case Page of
      CalendarPage : begin
        PanelCalendar.Visible := True;
        Bmp.Handle := LoadBitmap(hInstance, 'CALENDAR');
        LabelHeader.Caption := 'Calendar';
      end;

      ContactsPage : begin
        PanelContacts.Visible := True;
        Bmp.Handle := LoadBitmap(hInstance, 'ROLODEX');
        LabelHeader.Caption := 'Contacts';
      end;

      TasksPage    : begin
        PanelTasks.Visible := True;
        Bmp.Handle := LoadBitmap(hInstance, 'TODOLIST');
        LabelHeader.Caption := 'Task List';
      end;

      MaintResourcePage: begin
        PanelMaintResources.Visible := True;
        Bmp.Handle := LoadBitmap(hInstance, 'PEOPLE');
        LabelHeader.Caption := 'Resource Maintenance';
        BtnAddRes.Glyph.Handle := LoadBitmap(hInstance, 'VPPLUS');
        if VpBDEDataStore1.Resource <> nil then
          LoadMaintResFields
        else begin
          EditResDescr.Enabled := False;
          EditResDescr.Color := clBtnFace;
          MemoResNotes.Enabled := False;
          MemoResNotes.Color := clBtnFace;
        end;

      end;

    end;
    { paint the image in the corner }
    HeaderImage.Canvas.Brush.Color := clBtnFace;
    HeaderImage.Canvas.BrushCopy(Rect(0, 0, 32, 32), Bmp, Rect(0, 0, 32, 32),
      clOlive);
  finally
    Bmp.Free;
  end;
end;
{=====}
procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  VpDayView1.NumDays := TrackBar1.Position;
end;
{=====}
procedure TMainForm.VpBDEDataStore1ResourceChange(Sender: TObject;
  Resource: TVpResource);
begin
  LoadMaintResFields;

  if (VpNavBar1.ActiveFolder = 2)
  and (VpBDEDataStore1.Resource <> nil) then begin
    LabelEventsLED.Caption := IntToStr(
      VpBDEDataStore1.Resource.Schedule.EventCount);
    LabelContactsLED.Caption := IntToStr(
      VpBDEDataStore1.Resource.Contacts.Count);
    LabelTasksLED.Caption := IntToStr(
      VpBDEDataStore1.Resource.Tasks.Count);
  end else begin
    LabelEventsLED.Caption := '0';
    LabelContactsLED.Caption := '0';
    LabelTasksLED.Caption := '0';
  end;
end;
{=====}
procedure TMainForm.VpNavBar1FolderChange(Sender: TObject; Index: Integer;
  var AllowChange: Boolean; Dragging: Boolean);
begin
  if Index = 2 then begin
    if VpBDEDataStore1.Resource <> nil then begin
      LabelEventsLED.Caption := IntToStr(
        VpBDEDataStore1.Resource.Schedule.EventCount);
      LabelContactsLED.Caption := IntToStr(
        VpBDEDataStore1.Resource.Contacts.Count);
      LabelTasksLED.Caption := IntToStr(
        VpBDEDataStore1.Resource.Tasks.Count);
    end else begin
      LabelEventsLED.Caption := '0';
      LabelContactsLED.Caption := '0';
      LabelTasksLED.Caption := '0';
    end;
  end;
end;
{=====}
procedure TMainForm.VpNavBar1ItemClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; Index: Integer);
begin
  if VpNavBar1.ActiveFolder = 0 then
    SetActivePage(Index)
  else if VpNavBar1.ActiveFolder = 1 then
    SetActivePage(Index + 10);
end;
{=====}
procedure TMainForm.mnuFileEditPrintFormatsClick(Sender: TObject);
begin
//  ShowMessage(IntToStr(VpControlLink1.Printer.PrintFormats.Count));
  VpPrintFormatEditDialog1.Execute;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if VpBDEDataStore1.Resources.Count > 0 then
    VpBDEDataStore1.Resource := VpBDEDataStore1.Resources.Items[0];
end;

end.


