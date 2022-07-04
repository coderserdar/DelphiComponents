{*********************************************************}
{*                VPTASKEDITDLG.PAS 1.03                 *}
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

unit VpTaskEditDlg;
  { default task editing dialog }

interface

uses
  Windows, Messages, SysUtils,
  {$IFDEF VERSION6} Variants, {$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, VpData, StdCtrls, ExtCtrls,
  VpEdPop, VpDateEdit, VpBase, VpSR, VpDlg, ComCtrls;

type
  { forward declarations }
  TVpTaskEditDialog = class;

  TTaskEditForm = class(TForm)
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    PageControl1: TPageControl;
    tabTask: TTabSheet;
    DescriptionEdit: TEdit;
    DueDateLbl: TLabel;
    DueDateEdit: TVpDateEdit;
    CompleteCB: TCheckBox;
    CreatedOnLbl: TLabel;
    CompletedOnLbl: TLabel;
    DetailsMemo: TMemo;
    ResourceNameLbl: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    imgCalendar: TImage;
    imgCompleted: TImage;
    procedure FormCreate(Sender: TObject);
    procedure OnChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FReturnCode: TVpEditorReturnCode;
    FTask: TVpTask;
    FResource: TVpResource;
  public
    procedure PopulateSelf;
    procedure DePopulateSelf;
    property Task: TVpTask
      read FTask write FTask;
    property Resource: TVpResource
      read FResource write FResource;
    property ReturnCode: TVpEditorReturnCode
      read FReturnCode;
  end;

  TVpTaskEditDialog = class(TVpBaseDialog)
  protected {private}
    teEditDlg         : TTaskEditForm;
    teTask            : TVpTask;
  public
    constructor Create(AOwner : TComponent); override;
    function Execute(Task: TVpTask): Boolean; reintroduce;
    function AddNewTask: Boolean;
  published
    {properties}
    property DataStore;
    property Options;
    property Placement;
  end;

implementation

{$R *.dfm}

{ TTaskEditForm }

procedure TTaskEditForm.FormCreate(Sender: TObject);
begin
  FReturnCode := rtAbandon;
end;
{=====}

procedure TTaskEditForm.DePopulateSelf;
begin
  Task.Description := DescriptionEdit.Text;
  Task.DueDate := DueDateEdit.Date;
  Task.Details := DetailsMemo.Text;
  Task.Complete := CompleteCB.Checked;
  DueDateLbl.Caption := RSDueDate;
end;
{=====}

procedure TTaskEditForm.PopulateSelf;
begin
  ResourceNameLbl.Caption := Resource.Description;
  DueDateLbl.Caption := RSDueDate;
  OKBtn.Caption := RSOKBtn;
  CancelBtn.Caption := RSCancelBtn;

  DescriptionEdit.Text := Task.Description;
  DueDateEdit.Date := Task.DueDate;
  DetailsMemo.Text := Task.Details;
  CompleteCB.Checked := Task.Complete;
  if Task.CompletedOn <> 0 then
    CompletedOnLbl.Caption := RSCompletedOn + ' ' +
                              FormatDateTime(ShortDateFormat, Task.CompletedOn)
  else
    CompletedOnLbl.Visible := False;
  CompletedOnLbl.Visible := CompleteCB.Checked;
  CreatedOnLbl.Caption := RSCreatedOn + ' ' +
                          FormatDateTime(ShortDateFormat, Task.CreatedOn);
end;
{=====}

procedure TTaskEditForm.OnChange(Sender: TObject);
begin
  Task.Changed := true;
end;
{=====}

procedure TTaskEditForm.OKBtnClick(Sender: TObject);
begin
  FReturnCode := rtCommit;
  Close;
end;
{=====}

procedure TTaskEditForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;
{=====}

procedure TTaskEditForm.FormShow(Sender: TObject);
begin
  DescriptionEdit.SetFocus;
end;
{=====}

{ TVpTaskEditDialog }

constructor TVpTaskEditDialog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FPlacement.Height   := 340;
  FPlacement.Width    := 545;
end;

function TVpTaskEditDialog.Execute(Task: TVpTask): Boolean;
var
  TaskEditForm: TTaskEditForm;
begin
  Result := false;
  teTask := Task;
  if (teTask <> nil) and (DataStore <> nil) and
     (DataStore.Resource <> nil) then begin
    Application.CreateForm(TTaskEditForm, TaskEditForm);
    try
      DoFormPlacement(TaskEditForm);
      SetFormCaption(TaskEditForm, Task.Description, RSDlgTaskEdit);
      TaskEditForm.Task := Task;
      TaskEditForm.Resource := DataStore.Resource;
      TaskEditForm.PopulateSelf;
      TaskEditForm.ShowModal;
      Result := (TaskEditForm.ReturnCode = rtCommit);
      Task.Changed := Result;
      if Result then begin
        TaskEditForm.DePopulateSelf;
        DataStore.PostTasks;
        DataStore.NotifyDependents;
      end;
    finally
      TaskEditForm.Release;
    end;
  end;
end;
{=====}

function TVpTaskEditDialog.AddNewTask: Boolean;
begin
  result := false;
  if DataStore <> nil then begin
    teTask := DataStore.Resource.Tasks.AddTask(DataStore.GetNextID('Tasks'));
    if teTask <> nil then begin
      Result := Execute(teTask);
      if not Result then
        teTask.Free;
    end;
  end;
end;
{=====}

end.
  
