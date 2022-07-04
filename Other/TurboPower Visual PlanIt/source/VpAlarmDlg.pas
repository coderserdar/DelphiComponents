{*********************************************************}
{*                  VPALARMDLG.PAS 1.03                  *}
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

{$I Vp.INC}

unit VpAlarmDlg;
  { Alarm Notification Dialog }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VpDlg, VpData, ExtCtrls, StdCtrls, VpBase, VpEvntEditDlg, VpBaseDS, VpConst,
  VpMisc;

type
  { forward declarations }
  TVpNotificationDialog = class;

  TAlarmNotifyForm = class(TForm)
    DismissBtn: TButton;
    SnoozeBtn: TButton;
    OpenItemBtn: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    SubjectCaption: TLabel;
    NotesCaption: TLabel;
    SnoozeCaption: TLabel;
    SubjectEdit: TEdit;
    NotesMemo: TMemo;
    SnoozeCombo: TComboBox;
    EventDialog: TVpEventEditDialog;
    procedure SnoozeComboChange(Sender: TObject);
    procedure SnoozeBtnClick(Sender: TObject);
    procedure DismissBtnClick(Sender: TObject);
    procedure OpenItemBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    SnoozeDelay: TDateTime;
    ShowTime   : TDateTime;

    procedure CalcSnooze;
  public
    Event: TVpEvent;
    DataStore: TVpCustomDataStore;
    procedure PopulateSelf;
  end;

  TVpNotificationDialog = class(TVpBaseDialog)
  protected {private}
    FBGColor   : TColor;
    ceEditDlg  : TAlarmNotifyForm;
    ceTask     : TVpTask;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Execute(Event: TVpEvent); reintroduce;
  published
    {properties}
    property BackgroundColor: TColor
      read FBGColor write FBGColor default clInfoBk;
    property DataStore;
    property Placement;
  end;

implementation

{$R *.DFM}

uses VpSR;

{ TVpNotificationDialog }

constructor TVpNotificationDialog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FBGColor := clInfoBk;
  FPlacement.Position := mpCustom;
  FPlacement.Width := 412; 
end;
{=====}


procedure TVpNotificationDialog.Execute(Event: TVpEvent);
var
  AlarmNotifyForm: TAlarmNotifyForm;
begin
  if (Event <> nil) and (not Event.AlertDisplayed) then begin
    Application.CreateForm(TAlarmNotifyForm, AlarmNotifyForm);
    try
      DoFormPlacement(AlarmNotifyForm);
      AlarmNotifyForm.Color := BackgroundColor;
      AlarmNotifyForm.DataStore := DataStore;
      AlarmNotifyForm.Event := Event;
      AlarmNotifyForm.PopulateSelf;
      Event.AlertDisplayed := true;
      try
        AlarmNotifyForm.ShowModal;
      finally
        Event.AlertDisplayed := false;
      end;
      if Event.Changed then
        DataStore.PostEvents;
    finally
      AlarmNotifyForm.Release;
    end;
  end;
end;
{=====}

{ TAlarmNotifyForm }

procedure TAlarmNotifyForm.PopulateSelf;
begin
  if Event <> nil then begin
    Caption := RSReminder;
    SubjectCaption.Caption := RSSubjectCaption;
    NotesCaption.Caption := RSNotesCaption;
    SnoozeCaption.Caption := RSSnoozeCaption;
    DismissBtn.Caption := RSDismissBtn;
    SnoozeBtn.Caption := RSSnoozeBtn;
    OpenItemBtn.Caption := RSOpenItemBtn;
    NotesMemo.Text := Event.Note;
    SubjectEdit.Text := Event.Description;

    if Now > Event.StartTime then
      Self.Caption := RSOverdue + ' : '
    else
      Self.Caption := RSReminder + ' : ';

    Self.Caption := Self.Caption + FormatDateTime(ShortDateFormat + ' '
      + ShortTimeFormat, Event.StartTime);

    SnoozeCombo.Items.Clear;
    SnoozeCombo.Items.Add(RS5Minutes);
    SnoozeCombo.Items.Add(RS10Minutes);
    SnoozeCombo.Items.Add(RS15Minutes);
    SnoozeCombo.Items.Add(RS30Minutes);
    SnoozeCombo.Items.Add(RS45Minutes);
    SnoozeCombo.Items.Add(RS1Hour);
    SnoozeCombo.Items.Add(RS2Hours);
    SnoozeCombo.Items.Add(RS3Hours);
    SnoozeCombo.Items.Add(RS4Hours);
    SnoozeCombo.Items.Add(RS5Hours);
    SnoozeCombo.Items.Add(RS6Hours);
    SnoozeCombo.Items.Add(RS7Hours);
    SnoozeCombo.Items.Add(RS8Hours);
    SnoozeCombo.Items.Add(RS1Days);
    SnoozeCombo.Items.Add(RS2Days);
    SnoozeCombo.Items.Add(RS3Days);
    SnoozeCombo.Items.Add(RS4Days);
    SnoozeCombo.Items.Add(RS5Days);
    SnoozeCombo.Items.Add(RS6Days);
    SnoozeCombo.Items.Add(RS1Week);
    SnoozeCombo.ItemIndex := 0;
    SnoozeDelay := 5 / MinutesInDay;
    ShowTime := Now;
  end;
end;
{=====}


procedure TAlarmNotifyForm.SnoozeComboChange(Sender: TObject);
begin
  case SnoozeCombo.ItemIndex of
    0 : SnoozeDelay :=  5  / MinutesInDay; { 5 minutes }
    1 : SnoozeDelay := 10  / MinutesInDay; {10 Minutes }
    2 : SnoozeDelay := 15  / MinutesInDay; {15 Minutes }
    3 : SnoozeDelay := 30  / MinutesInDay; {30 Minutes }
    4 : SnoozeDelay := 45  / MinutesInDay; {45 Minutes }
    5 : SnoozeDelay := 60  / MinutesInDay; {1 Hour     }
    6 : SnoozeDelay := 120 / MinutesInDay; {2 Hours    }
    7 : SnoozeDelay := 180 / MinutesInDay; {3 Hours    }
    8 : SnoozeDelay := 240 / MinutesInDay; {4 Hours    }
    9 : SnoozeDelay := 300 / MinutesInDay; {5 Hours    }
    10: SnoozeDelay := 360 / MinutesInDay; {6 Hours    }
    11: SnoozeDelay := 420 / MinutesInDay; {7 Hours    }
    12: SnoozeDelay := 480 / MinutesInDay; {8 Hours    }
    13: SnoozeDelay := 1.0;                {1 day      }
    14: SnoozeDelay := 2.0;                {2 day      }
    15: SnoozeDelay := 3.0;                {3 day      }
    16: SnoozeDelay := 4.0;                {4 day      }
    17: SnoozeDelay := 5.0;                {5 day      }
    18: SnoozeDelay := 6.0;                {6 day      }
    19: SnoozeDelay := 7.0;                {1 week     }
  end;
end;
{=====}

procedure TAlarmNotifyForm.SnoozeBtnClick(Sender: TObject);
begin
  CalcSnooze;
  Close;
end;
{=====}

procedure TAlarmNotifyForm.DismissBtnClick(Sender: TObject);
begin
  Event.AlarmSet := false;
  Close;
end;
{=====}

procedure TAlarmNotifyForm.OpenItemBtnClick(Sender: TObject);
begin
  Self.Hide;
  EventDialog.DataStore := DataStore;
  EventDialog.Execute(Event);
  Close;
end;
{=====}

procedure TAlarmNotifyForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    CalcSnooze;
    Close;
  end;
end;
{=====}

procedure TAlarmNotifyForm.CalcSnooze;
begin
  Event.SnoozeTime := Now + SnoozeDelay;
end;
{=====}
procedure TAlarmNotifyForm.FormShow(Sender: TObject);
begin
  OpenItemBtn.SetFocus;
end;

end.
 
