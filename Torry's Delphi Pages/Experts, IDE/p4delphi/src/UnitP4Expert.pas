{**************************************************************************************************}
{                                                                                                  }
{ Perforce for Delphi plugin (P4Delphi)                                                            }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Chris Fairall. Portions created by                 }
{ Chris Fairall are Copyright (C) Chris Fairall (cfairall at bigpond dot net dot au)               }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit UnitP4Expert;

{----------------------------------------------------------------------------

   Unit Name     :  UnitP4Expert
   Date Created  :  23 January 2002
   Author        :  Chris Fairall
   Description   :  The main controlling unit of the Perforce expert for
                    Delphi.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/UnitP4Expert.pas $
   $Revision: #15 $
   $DateTime: 2007/04/09 19:53:02 $
   $Author: Chris $

 ----------------------------------------------------------------------------}

interface

uses
  Windows, SysUtils, Graphics, Classes, Menus, ActnList, ToolsAPI, Dialogs, Forms,
  UnitP4Misc, ComCtrls, contnrs;

{$R P4Buttons.res}
{$I P4Define.inc}

const
  PERFORCE_CAT = 'Perforce';
  MOD_QUESTION = 'This file is currently marked as modified. Diff can only be ' +
                 'performed against the file as it exists on disk, and so, may ' +
                 'not reflect all current changes.'#13#10#13#10 +
                 'Do you wish to save the file before performing the Diff?';

type
  TP4FileListType = (fltAdd, fltSync, fltEdit, fltOther);

  TSavedBookmarks = Array[0..9] of TOTACharPos;

  TPerforceExpert = class(TObject)
  private
    FMenuPerforce,
    FMenuAdd,
    FMenuEdit,
    FMenuSubmit,
    FMenuRevert,
    FMenuSync,
    FMenuLock,
    FMenuUnlock,
    FMenuDiff,
    FMenuDiffHead,
    FMenuFileInfo,
    FMenuOptions,
    FMenuAddAll,
    FMenuP4VisualClient,
//    FMenuRevisionHistory,
    FMenuLogin,
    FMenuLogout,
    FMenuCheckServer,
    FMenuInfo        : TMenuItem;
    FActionAdd,
    FActionEdit,
    FActionSubmit,
    FActionRevert,
    FActionSync,
    FActionLock,
    FActionUnlock,
    FActionDiff,
    FActionDiffHead,
    FActionFileInfo,
    FActionOptions,
    FActionAddAll,
//    FActionRevisionHistory,
    FActionP4VisualClient,
    FActionLogin,
    FActionLogout,
    FActionCheckServer,
    FActionInfo      : TAction;
    FFileInfo        : TStringList;
    FFileName        : String;
    FLastChecked     : TDateTime;
    FOptions         : TP4Options;
    FNotifier        : IOTAIDENotifier;
    FNotifyIndex     : Integer;
    procedure LogMessages(pslLines : TStrings);
    procedure LogLine(psLine : String);
    procedure LogLastP4Transaction;
    function GetTempFile: string;
    procedure DelTempFile;
    { "Current" file functions }
    function ModuleFileName : String;
    function CurrentEditorFileName : String;
    procedure ListFiles(pslFileList : TStrings; pType : TP4FileListType);
    function AddFileToList(psFileName : String; pType : TP4FileListType) : Boolean;
    procedure ReloadFiles;
    function GetFileInfo(psFileName : String; pslInfo : TStrings; pbForce : Boolean = false) : Boolean;
    function GetModuleFileInfo(pslInfo : TStrings; pbForce : Boolean = false) : Boolean;
    procedure ForceFileInfoUpdate;
    function Modified : Boolean;
    function GetDiffProg : String;
    function BuildFileList(AProject : IOTAProject; AList : TStrings) : Boolean;
    procedure RemoveAction(AAction : TAction; AToolbar : TToolbar);
    function LatestVersions(pType : TP4FileListType) : Boolean;
    function StoreBookmarks(var AStorage : TSavedBookmarks) : Boolean;
    procedure RestoreBookmarks(AStorage : TSavedBookmarks);
    function GetSourceEditor : IOTASourceEditor;
    procedure DoDiff(AHeadRevision: boolean);
  protected
    function AddAction(psCaption, psHint, psName, psImageRes, psImageName : String;
                       ExecuteEvent, UpdateEvent : TNotifyEvent) : TAction;
    function AddMenu(pAction : TAction) : TMenuItem;
    procedure RemoveActionFromToolbar(AAction : TAction);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    { Action Event Handlers }
    procedure InfoUpdate(Sender: TObject);
    procedure InfoExecute(Sender : TObject);
    procedure OpenForEditUpdate(Sender : TObject);
    procedure OpenForEditExecute(Sender : TObject);
    procedure AddUpdate(Sender : TObject);
    procedure AddExecute(Sender : TObject);
    procedure SubmitUpdate(Sender : TObject);
    procedure SubmitExecute(Sender : TObject);
    procedure RevertUpdate(Sender : TObject);
    procedure RevertExecute(Sender : TObject);
    procedure DiffUpdate(Sender : TObject);
    procedure DiffExecute(Sender : TObject);
    procedure DiffHeadUpdate(Sender : TObject);
    procedure DiffHeadExecute(Sender : TObject);
    procedure LockUpdate(Sender : TObject);
    procedure LockExecute(Sender : TObject);
    procedure OptionsExecute(Sender : TObject);
    procedure UnlockUpdate(Sender : TObject);
    procedure UnlockExecute(Sender : TObject);
    procedure FileInfoUpdate(Sender : TObject);
    procedure FileInfoExecute(Sender : TObject);
    procedure SyncUpdate(Sender : TObject);
    procedure SyncExecute(Sender : TObject);
    procedure AddAllUpdate(Sender : TObject);
    procedure AddAllExecute(Sender : TObject);
    procedure LoginUpdate(Sender: TObject);
    procedure LoginExecute(Sender: TObject);
    procedure LogoutUpdate(Sender: TObject);
    procedure LogoutExecute(Sender: TObject);
    procedure CheckServerUpdate(Sender : TObject);
    procedure CheckServerExecute(Sender : TObject);
//    procedure RevisionHistoryUpdate(Sender : TObject);
//    procedure RevisionHistoryExecute(Sender : TObject);
    procedure P4VisualClientExecute(Sender : TObject);
    property Options : TP4Options read FOptions;
    procedure RefreshShortcuts;
  end;

var
  PerforceExpert : TPerforceExpert;

implementation

uses
  UnitP4Engine, FormAddEditDialog, Controls, FormSubmitDialog,
  FormAbout, FormFileProps, FormSyncDialog, FormAddAllDialog, UnitP4Notify,
  FormBaseAddEditDlg, FormLogin;

type
  TShortcutHolder = class(TObject)
  private
    FAction: TAction;
    FShortcut: TShortCut;
  public
    constructor Create(Action : TAction; Shortcut : TShortCut);
    property Action : TAction read FAction write FAction;
    property Shortcut : TShortCut read FShortcut write FShortcut;
  end;

{ TPerforceExpert }

constructor TPerforceExpert.Create;
var
  Service : INTAServices;
  iPos    : Integer;
  miAfter : TMenuItem;
begin
  inherited Create;

  FFileInfo := TStringList.Create;

  FFileName := '';
  FLastChecked := 0;

  FOptions := TP4Options.Create;

  Service := (BorlandIDEServices as INTAServices);

  FNotifier := TPerforceNotifier.Create;

  with BorlandIDEServices as IOTAServices do
    FNotifyIndex := AddNotifier(FNotifier);

  { Main menu item }
  FMenuPerforce := TMenuItem.Create(Service.MainMenu);
  FMenuPerforce.Caption := 'Perf&orce';
  FMenuPerforce.AutoHotkeys := maAutomatic;
  miAfter := Service.MainMenu.Items.Find('&Window');
  if miAfter = nil then
    iPos := 9
  else
    iPos := miAfter.MenuIndex;

  Service.MainMenu.Items.Insert(iPos, FMenuPerforce);

  { "Add New Archive" menu item }
  FActionAdd := AddAction('&Add New Archive...', 'Add To Perforce|',
                           'PerforceAddArchive', 'P4_BUTTON_ADD', 'PerforceAdd',
                           AddExecute, AddUpdate);
  FMenuAdd := AddMenu(FActionAdd);

  { "Open For Edit" menu item }
  FActionEdit := AddAction('&Open for Edit...', 'Open for Edit|.', 'PerforceOpenForEdit',
                            'P4_BUTTON_EDIT', 'PerforceEdit', OpenForEditExecute,
                            OpenForEditUpdate);
  FMenuEdit := AddMenu(FActionEdit);
  //SetActionShortcut(FActionEdit, ShortCut(Ord('K'), [ssAlt, ssCtrl]));

  { "Revert" menu item }
  FActionRevert := AddAction('&Revert Changes...', 'Perforce Revert|', 'PerforceRevert',
                             'P4_BUTTON_REVERT', 'PerforceRevert', RevertExecute,
                             RevertUpdate);
  FMenuRevert := AddMenu(FActionRevert);

  { "Submit" menu item }
  FActionSubmit := AddAction('&Submit Changes...', 'Perforce Submit|', 'PerforceSubmitChanges',
                             'P4_BUTTON_SUBMIT', 'PerforceSubmit', SubmitExecute,
                             SubmitUpdate);
  FMenuSubmit := AddMenu(FActionSubmit);

  FMenuPerforce.InsertNewLineAfter(FMenuSubmit);

  { "Add All Files In Project" menu item }
  FActionAddAll := AddAction('Add All Files In Project...', 'Perforce Add All|',
                             'PerforceAddAll', '', '', AddAllExecute, AddAllUpdate);
  FMenuAddAll := AddMenu(FActionAddAll);
  FMenuPerforce.InsertNewLineAfter(FMenuAddAll);

  { "Sync" menu item }
  FActionSync := AddAction('S&ync...', 'Perforce Sync|', 'PerforceSync',
                           'P4_BUTTON_SYNC', 'PerforceSync', SyncExecute,
                           SyncUpdate);
  FMenuSync := AddMenu(FActionSync);
  FMenuPerforce.InsertNewLineAfter(FMenuSync);

  { "Lock" menu item }
  FActionLock := AddAction('&Lock...', 'Perforce Lock|', 'PerforceLockFile',
                           'P4_BUTTON_LOCK', 'PerforceLock', LockExecute,
                           LockUpdate);
  FMenuLock := AddMenu(FActionLock);

  { "Unlock" menu item }
  FActionUnlock := AddAction('&Unlock...', 'Perforce Unlock|', 'PerforceUnlockFile',
                             'P4_BUTTON_UNLOCK', 'PerforceUnlock', UnlockExecute,
                             UnlockUpdate);
  FMenuUnlock := AddMenu(FActionUnlock);
  FMenuPerforce.InsertNewLineAfter(FMenuUnlock);

  { "Diff" menu item }
  FActionDiff := AddAction('&Diff Against Current Revision...', 'Diff Against Current Revision|',
                           'PerforceDiff', 'P4_BUTTON_DIFF', 'PerforceDiff',
                           DiffExecute, DiffUpdate);
  FMenuDiff := AddMenu(FActionDiff);

  { "Diff Against Head Revision" menu item }
  FActionDiffHead := AddAction('Diff Against &Head Revision...', 'Diff Against Head Revision|',
                               'PerforceDiffHead', 'P4_BUTTON_DIFF', 'PerforceDiff',
                               DiffHeadExecute, DiffHeadUpdate);
  FMenuDiffHead := AddMenu(FActionDiffHead);
  FMenuPerforce.InsertNewLineAfter(FMenuDiffHead);

  { Revision History Menu Item }
  {
  FActionRevisionHistory := AddAction('Re&vision History...', 'File Revision History|', 'PerforceRevisionHistory',
                               'P4_BUTTON_REVISION_HISTORY', 'PerforceRevisionHistory', RevisionHistoryExecute, RevisionHistoryUpdate);
  FMenuRevisionHistory := AddMenu(FActionRevisionHistory);
  }

  FActionP4VisualClient := AddAction('Perforce &Visual client', 'Perforce Visual Client|', 'PerforceVisualClient',
                               'P4_BUTTON_VISUAL_CLIENT', 'PerforceVisualClient', P4VisualClientExecute, nil);
  FMenuP4VisualClient := AddMenu(FActionP4VisualClient);

  { File Info Menu Item }
  FActionFileInfo := AddAction('&File Properties...', 'File Properties|', 'PerforceFileProp',
                               '', '', FileInfoExecute, FileInfoUpdate);
  FMenuFileInfo := AddMenu(FActionFileInfo);
  FMenuPerforce.InsertNewLineAfter(FMenuFileInfo);

  { Login Menu Item }
  FActionLogin := AddAction('Login...', 'Login|', 'PerforceLogin', '', '', LoginExecute, LoginUpdate);
  FMenuLogin := AddMenu(FActionLogin);

  { Logout Menu Item }
  FActionLogout := AddAction('Logout', 'Logout|', 'PerforceLogout', '', '', LogoutExecute, LogoutUpdate);
  FMenuLogout := AddMenu(FActionLogout);

  FMenuPerforce.InsertNewLineAfter(FMenuLogout);

  { Check Server Status menu item }
  FActionCheckServer := AddAction('&Check Server Status', 'Check Server Status|', 'PerforceCheckServerStatus',
                               '', '', CheckServerExecute, CheckServerUpdate);
  FMenuCheckServer := AddMenu(FActionCheckServer);
  FMenuPerforce.InsertNewLineAfter(FMenuCheckServer);

  { Options Menu Item }
  FActionOptions := AddAction('&Options...', 'Perforce Expert Options', 'PerforceDelphiOptions',
                              '', '', OptionsExecute, nil);
  FMenuOptions := AddMenu(FActionOptions);
  FMenuPerforce.InsertNewLineAfter(FMenuOptions);

  { Info Menu Item }
  FActionInfo := AddAction('Perforce &Info...', 'Perforce Info|', 'PerforceInfo',
                           '', '', InfoExecute, InfoUpdate);
  FMenuInfo := AddMenu(FActionInfo);

end;

destructor TPerforceExpert.Destroy;
var
  Service : INTAServices;
begin
  Service := (BorlandIDEServices as INTAServices);

  { Destroy the menu items }
  { Sub-menu items are freed by their owner - FMenuPerforce when it is freed... }
  Service.MainMenu.Items.Remove(FMenuPerforce);
  FMenuPerforce.Free;

  { Remove actions from any toolbars the user may have added them to. }
  RemoveActionFromToolbar(FActionAdd);
  RemoveActionFromToolbar(FActionEdit);
  RemoveActionFromToolbar(FActionRevert);
  RemoveActionFromToolbar(FActionSubmit);
  RemoveActionFromToolbar(FActionSync);
  RemoveActionFromToolbar(FActionLock);
  RemoveActionFromToolbar(FActionUnlock);
  RemoveActionFromToolbar(FActionDiff);
  RemoveActionFromToolbar(FActionDiffHead);
//  RemoveActionFromToolbar(FActionRevisionHistory);
  RemoveActionFromToolbar(FActionP4VisualClient);
  RemoveActionFromToolbar(FActionFileInfo);
  RemoveActionFromToolbar(FActionOptions);
  RemoveActionFromToolbar(FActionInfo);
  RemoveActionFromToolbar(FActionAddAll);
  RemoveActionFromToolbar(FActionLogin);
  RemoveActionFromToolbar(FActionLogout);
  RemoveActionFromToolbar(FActionCheckServer);

  { Destroy actions }
  FActionAdd.Free;
  FActionEdit.Free;
  FActionRevert.Free;
  FActionSubmit.Free;
  FActionSync.Free;
  FActionLock.Free;
  FActionUnlock.Free;
  FActionDiff.Free;
  FActionDiffHead.Free;
  FActionP4VisualClient.Free;
  // FActionRevisionHistory.Free;
  FActionFileInfo.Free;
  FActionOptions.Free;
  FActionInfo.Free;
  FActionAddAll.Free;
  FActionLogin.Free;
  FActionLogout.Free;
  FActionCheckServer.Free;

  with BorlandIDEServices as IOTAServices do
    RemoveNotifier(FNotifyIndex);
  FNotifier := nil;

  FFileInfo.Free;

  FOptions.Free;

  inherited Destroy;
end;

procedure TPerforceExpert.InfoUpdate(Sender: TObject);
begin
  FActionInfo.Enabled := P4Engine.IsServerUp;
end;

procedure TPerforceExpert.InfoExecute(Sender: TObject);
begin
  with TfrmAbout.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TPerforceExpert.OpenForEditExecute(Sender: TObject);
var
  slFiles  : TStrings;
begin
  with TfrmAddEditDialog.Create(nil) do
    try
      Caption := 'Open Archive For Edit';
      AutoLock := FOptions.AutoLock;
      lblWarning.Visible := not LatestVersions(fltEdit);
      if (not FOptions.OpenForEditPrompt) or Execute then
        begin
          slFiles := TStringList.Create;
          try
            ListFiles(slFiles, fltEdit);

            { Sync first }
            if SyncFirst then
              begin
                P4Engine.Sync(slFiles, false);
                LogLastP4Transaction;
              end;

            { Open for Edit }
            P4Engine.OpenForEdit(slFiles, ChangeList);
            LogLastP4Transaction;

            { Lock files }
            if AutoLock then
              begin
                P4Engine.LockFiles(slFiles);
                LogLastP4Transaction;
              end;
          finally
            slFiles.Free;
          end;

          ReloadFiles;
          ForceFileInfoUpdate;
        end;
    finally
      Free;
    end;
end;

procedure TPerforceExpert.SubmitExecute(Sender: TObject);
var
  slResults,
  slFileInfo     : TStringList;
  sChangeList    : String;
  iChangeList    : Integer;
begin
  slFileInfo := TStringList.Create;
  try
    GetModuleFileInfo(slFileInfo);
    sChangeList := slFileInfo.Values['change'];
    with TfrmSubmitDialog.Create(nil) do
      try
        try
          iChangeList := StrToInt(sChangeList);
          P4Engine.ChangeListInfo(iChangeList, Description, nil, Jobs);
        except
        end;
        if Execute(sChangeList) then
          begin
            Screen.Cursor := crHourGlass;
            slResults := TStringList.Create;
            try
              slResults.Text := P4Engine.Command('-s submit -i', 'c:\', FormFile);
              LogMessages(slResults);
            finally
              slResults.Free;
              Screen.Cursor := crDefault;
            end;
            ReloadFiles;
          end;
      finally
        Free;
      end;
  finally
    slFileInfo.Free;
  end;
end;

procedure TPerforceExpert.RevertExecute(Sender: TObject);
var
  slFiles  : TStrings;
begin
  if MessageDlg('Are you sure you want to revert this file?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      slFiles := TStringList.Create;
      try
        ListFiles(slFiles, fltOther);
        P4Engine.RevertFiles(slFiles);
        LogLastP4Transaction;
      finally
        slFiles.Free;
      end;

      ReloadFiles;
      ForceFileInfoUpdate;
    end;
end;

procedure TPerforceExpert.AddExecute(Sender: TObject);
var
  Files    : TStrings;
begin
  with TfrmAddEditDialog.Create(nil) do
    try
      Caption := 'Add New Perforce Archive';
      chbxAutoLock.Visible := false;
      chbxSyncFirst.Visible := false;
      if Execute then
        begin
          Files := TStringList.Create;
          try
            ListFiles(Files, fltAdd);
            P4Engine.AddArchive(Files, ChangeList);
            LogLastP4Transaction;
          finally
            Files.Free;
          end;
          ForceFileInfoUpdate;
        end;
    finally
      Free;
    end;
end;

procedure TPerforceExpert.SyncExecute(Sender: TObject);
var
  bForce        : Boolean;
  sAddOn        : String;
  slFileList      : TStringList;
  i           : Integer;
begin
  with TfrmSyncDialog.Create(nil) do
    try
      if Execute(sAddOn, bForce) then
        begin
          slFileList := TStringList.Create;
          try
            ListFiles(slFileList, fltSync);
            for i := 0 to slFileList.Count - 1 do
              slFileList[i] := slFileList[i] + sAddOn;
            P4Engine.Sync(slFileList, bForce);
          finally
            slFileList.Free;
          end;

          ReloadFiles;
        end;
    finally
      Free;
    end;
end;

procedure TPerforceExpert.LockExecute(Sender: TObject);
var
  Files      : TStrings;
begin
  Screen.Cursor := crHourGlass;
  try
    Files := TStringList.Create;
    try
      ListFiles(Files, fltOther);
      P4Engine.LockFiles(Files);
      LogLastP4Transaction;
    finally
      Files.Free;
    end;
    ForceFileInfoUpdate;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TPerforceExpert.UnlockExecute(Sender: TObject);
var
  Files      : TStrings;
begin
  Screen.Cursor := crHourGlass;
  try
    Files := TStringList.Create;
    try
      ListFiles(Files, fltOther);
      P4Engine.UnlockFiles(Files);
      LogLastP4Transaction;
    finally
      Files.Free;
    end;
    ForceFileInfoUpdate;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TPerforceExpert.FileInfoExecute(Sender: TObject);
begin
  with TfrmFileProperties.Create(nil) do
    try
      Execute(CurrentEditorFileName);
    finally
      Free;
    end;
end;

procedure TPerforceExpert.DoDiff(AHeadRevision: boolean);
var
  Serv        : IOTAModuleServices;
  sCmd,
  sPath,
  sRevision,
  sFileName   : String;
  bDoDiff     : Boolean;
begin
  sFileName := '';
  bDoDiff := false;

  Serv := (BorlandIDEServices as IOTAModuleServices);
  if (Serv.CurrentModule <> nil) and (Serv.CurrentModule.CurrentEditor <> nil) then
    begin
      bDoDiff := true;
      if Serv.CurrentModule.CurrentEditor.Modified then
      begin
        case MessageDlg(MOD_QUESTION, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
          mrYes: Serv.CurrentModule.Save(False, True);
          mrNo:;//Nothing to do
          mrCancel: bDoDiff := False;
        end;
      end;
      sFileName := Serv.CurrentModule.CurrentEditor.FileName;
      if bDoDiff then
        bDoDiff := FileExists(sFileName);
      sPath := ExtractFilePath(sFileName);
    end;

  if bDoDiff then
    begin
      DelTempFile;
      if AHeadRevision then
        sRevision := ''
      else
        sRevision := FFileInfo.Values['haveRev'];
      sCmd := 'print -o "' + GetTempFile + '" -q "' + sFileName;
      if sRevision <> '' then
        sCmd := sCmd + '#' + sRevision;
      sCmd := sCmd + '"';
      P4Engine.Command(sCmd, sPath);

      sCmd := GetDiffProg + ' "' + GetTempFile + '" "' + sFileName + '"';
      RunProgram(sCmd, woUntilStart);
    end;
end;

procedure TPerforceExpert.DiffExecute(Sender: TObject);
begin
  DoDiff(false);
end;

procedure TPerforceExpert.DiffHeadExecute(Sender: TObject);
begin
  DoDiff(true);
end;

function TPerforceExpert.ModuleFileName: String;
var
  Serv            : IOTAModuleServices;
begin
  Result := '';

  Serv := (BorlandIDEServices as IOTAModuleServices);
  if Serv.CurrentModule <> nil then
    Result := Serv.CurrentModule.FileName;
end;

function TPerforceExpert.CurrentEditorFileName: String;
var
  Serv            : IOTAModuleServices;
begin
  Result := '';

  Serv := (BorlandIDEServices as IOTAModuleServices);
  if (Serv.CurrentModule <> nil) and (Serv.CurrentModule.CurrentEditor <> nil) then
    Result := Serv.CurrentModule.CurrentEditor.FileName;
end;

function TPerforceExpert.AddFileToList(psFileName: String;
  pType: TP4FileListType): Boolean;
begin
  case pType of
    fltAdd:
      begin
        Result := (not P4Engine.FileArchived(psFileName)) and (FileExists(psFileName));
        if (Uppercase(ExtractFileExt(psFileName)) = '.DDP') and Result then
          Result := FOptions.AddDDPFiles;
        if (Uppercase(ExtractFileExt(psFileName)) = '.TODO') and Result then
          Result := FOptions.AddTodoFiles;
      end;
    fltSync, fltEdit:
      Result := P4Engine.FileArchived(psFileName) and FileExists(psFileName);
    fltOther:
      Result := FileExists(psFileName);
  else
    Result := false;
  end;
end;

procedure TPerforceExpert.ListFiles(pslFileList: TStrings; pType : TP4FileListType);
var
  Serv            : IOTAModuleServices;
  iCounter        : Integer;
  sFile           : String;
begin
  pslFileList.Clear;
  Serv := (BorlandIDEServices as IOTAModuleServices);
  if Serv.CurrentModule <> nil then
    begin
      for iCounter := 0 to Serv.CurrentModule.ModuleFileCount - 1 do
        begin
          sFile := Serv.CurrentModule.ModuleFileEditors[iCounter].FileName;

          if AddFileToList(sFile, pType) then
            pslFileList.Add(sFile);

          { Diagram (DDP) files }
          sFile := ChangeFileExt(sFile, '.ddp');
          if AddFileToList(sFile, pType) and (pslFileList.IndexOf(sFile) < 0) then
            pslFileList.Add(sFile);

          { Additional Project files }
          sFile := ChangeFileExt(sFile, '.dof');
          if AddFileToList(sFile, pType) and (pslFileList.IndexOf(sFile) < 0) then
            pslFileList.Add(sFile);

          sFile := ChangeFileExt(sFile, '.cfg');
          if AddFileToList(sFile, pType) and (pslFileList.IndexOf(sFile) < 0) then
            pslFileList.Add(sFile);

          sFile := ChangeFileExt(sFile, '.todo');
          if AddFileToList(sFile, pType) and (pslFileList.IndexOf(sFile) < 0) then
            pslFileList.Add(sFile);

          sFile := ChangeFileExt(sFile, '.bdsproj');
          if AddFileToList(sFile, pType) and (pslFileList.IndexOf(sFile) < 0) then
            pslFileList.Add(sFile);

          sFile := ChangeFileExt(sFile, '.dproj');
          if AddFileToList(sFile, pType) and (pslFileList.IndexOf(sFile) < 0) then
            pslFileList.Add(sFile);

        end;
    end;
end;

procedure TPerforceExpert.LogLine(psLine: String);
var
  iCounter    : Integer;
begin
  if FOptions.ShowMessages then
    begin
      (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(psLine);

      for iCounter := 0 to Screen.FormCount - 1 do
        if CompareText(Screen.Forms[iCounter].ClassName, 'TMessageViewForm') = 0 then
          Screen.Forms[iCounter].Visible := true;
    end;
end;

procedure TPerforceExpert.LogMessages(pslLines: TStrings);
var
  iCounter  : Integer;
  slError  : TStringList;
begin
  if FOptions.ShowMessages then
    for iCounter := 0 to pslLines.Count -1 do
      LogLine(pslLines[iCounter]);

  { Display any error messages }
  slError := TStringList.Create;
  try
    for iCounter := 0 to pslLines.Count - 1 do
      if Uppercase(Copy(pslLines[iCounter], 1, 6)) = 'ERROR:' then
        slError.Add(Trim(Copy(pslLines[iCounter], 7, Length(pslLines[iCounter]))));

    if slError.Count > 0 then
      MessageDlg(slError.Text, mtError, [mbOK], 0);
  finally
    slError.Free;
  end;
end;

procedure TPerforceExpert.LogLastP4Transaction;
var
  output: TStrings;
begin
  LogLine(P4Engine.LastCmd);
  output := TStringList.Create;
  try
    output.Text := P4Engine.LastOutput;
    LogMessages(output);
  finally
    output.Free;
  end;
end;

procedure TPerforceExpert.ReloadFiles;
var
  Serv        : IOTAModuleServices;
  ActSrv      : IOTAActionServices;
  sFile       : String;
  BookArray   : TSavedBookmarks;
  SelSaved,
  BookSaved   : Boolean;
  BlockStart,
  BlockEnd    : TOTACharPos;
  BlockType   : TOTABlockType;
  Editor      : IOTASourceEditor;
begin
  BookSaved := FOptions.PreserveBookmarks and StoreBookmarks(BookArray);

  SelSaved := false;

  Editor := GetSourceEditor;
  if (Editor <> nil) and FOptions.PreserveSelection then
    begin
      BlockStart := Editor.BlockStart;
      BlockEnd := Editor.BlockAfter;
      BlockType := Editor.BlockType;
      SelSaved := true;
      Editor := nil;
    end
  else
    BlockType := btUnknown;

  if Supports(BorlandIDEServices, IOTAModuleServices, Serv) and
     Supports(BorlandIDEServices, IOTAActionServices, ActSrv) then
    begin
      if Serv.CurrentModule <> nil then
        begin
          sFile := Serv.CurrentModule.FileName;
          ActSrv.ReloadFile(sFile);

          if BookSaved then
            RestoreBookmarks(BookArray);

          if SelSaved then
            begin
              Editor := GetSourceEditor;
              if Editor <> nil then
                begin
                  Editor.BlockVisible := false;
                  try
                    Editor.BlockStart := BlockStart;
                    Editor.BlockAfter := BlockEnd;
                    Editor.BlockType := BlockType;
                  finally
                    Editor.BlockVisible := true;
                  end;
                  Editor := nil;
                end;
            end;
        end;
    end;
end;

procedure TPerforceExpert.AddUpdate(Sender: TObject);
var
  bAddEnable      : Boolean;
  slInfo          : TStringList;
begin
  bAddEnable := false;

  slInfo := TStringList.Create;
  try
    P4Engine.CheckServerStatus;
    if P4Engine.IsServerUp then
      begin
        GetModuleFileInfo(slInfo);
        if WildcardCompare('*no such file*', slInfo.Text) then
          begin
            if FileExists(ModuleFileName) then
              bAddEnable := true;
          end;
      end;
  finally
    slInfo.Free;
  end;

  FActionAdd.Enabled := bAddEnable;
end;

procedure TPerforceExpert.OpenForEditUpdate(Sender: TObject);
var
  bEditEnable     : Boolean;
  slInfo          : TStringList;
begin
  bEditEnable := false;

  slInfo := TStringList.Create;
  try
    if P4Engine.IsServerUp and GetModuleFileInfo(slInfo) {and (not Modified)} then
      begin
        if (slInfo.Values['action'] = '') and (slInfo.Values['haveRev'] <> '') and
            (slInfo.IndexOf('otherLock') < 0) then
          bEditEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionEdit.Enabled := bEditEnable;
end;

procedure TPerforceExpert.RevertUpdate(Sender: TObject);
var
  bRevertEnable   : Boolean;
  slInfo          : TStringList;
begin
  bRevertEnable := false;

  slInfo := TStringList.Create;
  try
    if P4Engine.IsServerUp and GetModuleFileInfo(slInfo) then
      begin
        if ((slInfo.Values['action'] = 'edit') or (slInfo.Values['action'] = 'add')) then
          bRevertEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionRevert.Enabled := bRevertEnable;
end;

procedure TPerforceExpert.SubmitUpdate(Sender: TObject);
var
  bSubmitEnable   : Boolean;
  slInfo          : TStringList;
begin
  bSubmitEnable := false;

  slInfo := TStringList.Create;
  try
    if GetModuleFileInfo(slInfo) and (not Modified) then
      begin
        if ((slInfo.Values['action'] = 'edit') or (slInfo.Values['action'] = 'add')) then
          bSubmitEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionSubmit.Enabled := bSubmitEnable;
end;

procedure TPerforceExpert.LockUpdate(Sender: TObject);
var
  bLockEnable   : Boolean;
  slInfo        : TStringList;
begin
  bLockEnable := false;

  slInfo := TStringList.Create;
  try
    if P4Engine.IsServerUp and GetModuleFileInfo(slInfo) then
      begin
        if ((slInfo.Values['action'] = 'edit') or (slInfo.Values['action'] = 'add')) and
           (slInfo.IndexOf('ourLock') < 0) and (slInfo.IndexOf('otherLock') < 0) then
          bLockEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionLock.Enabled := bLockEnable;
end;

procedure TPerforceExpert.UnlockUpdate(Sender: TObject);
var
  bUnlockEnabled   : Boolean;
  slInfo           : TStringList;
begin
  bUnlockEnabled := false;

  slInfo := TStringList.Create;
  try
    if P4Engine.IsServerUp and GetModuleFileInfo(slInfo) then
      begin
        if ((slInfo.Values['action'] = 'edit') or (slInfo.Values['action'] = 'add')) and
           (slInfo.IndexOf('ourLock') >= 0) then
          bUnlockEnabled := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionUnlock.Enabled := bUnlockEnabled;
end;

procedure TPerforceExpert.FileInfoUpdate(Sender: TObject);
var
  bInfoEnable   : Boolean;
  slInfo        : TStringList;
begin
  bInfoEnable := false;

  slInfo := TStringList.Create;
  try
    if P4Engine.IsServerUp and GetModuleFileInfo(slInfo) then
      begin
        if not WildcardCompare('*no such file*', slInfo.Text) then
          bInfoEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionFileInfo.Enabled := bInfoEnable;
end;

procedure TPerforceExpert.DiffUpdate(Sender: TObject);
var
  bDiffEnable   : Boolean;
  slInfo        : TStringList;
begin
  bDiffEnable := false;

  slInfo := TStringList.Create;
  try
    if P4Engine.IsServerUp and GetModuleFileInfo(slInfo) then
      begin
        if (slInfo.Values['action'] = 'edit') then
          bDiffEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionDiff.Enabled := bDiffEnable;
end;

procedure TPerforceExpert.DiffHeadUpdate(Sender: TObject);
var
  bDiffEnable   : Boolean;
  slInfo        : TStringList;
begin
  bDiffEnable := false;

  slInfo := TStringList.Create;
  try
    if P4Engine.IsServerUp and GetModuleFileInfo(slInfo) then
      begin
        if (slInfo.Values['action'] <> 'add') and (slInfo.Values['haveRev'] <> '') then
          bDiffEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionDiffHead.Enabled := bDiffEnable;
end;

procedure TPerforceExpert.SyncUpdate(Sender: TObject);
var
  bSyncEnable   : Boolean;
  slInfo        : TStringList;
begin
  bSyncEnable := false;

  slInfo := TStringList.Create;
  try
    if P4Engine.IsServerUp and GetModuleFileInfo(slInfo) then
      begin
        if (slInfo.Values['action'] = '') then
          bSyncEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionSync.Enabled := bSyncEnable;
end;

function TPerforceExpert.GetFileInfo(psFileName: String; pslInfo: TStrings;
  pbForce: Boolean): Boolean;
var
  bGotoPerforce  : Boolean;
  dtNow          : TDateTime;
  wHour,
  wMinute,
  wSec,
  wMSec          : Word;
begin
  if P4Engine.IsServerUp then
    begin
      Result := true;
      bGotoPerforce := false;
      if (psFileName <> FFileName) or (FFileInfo.Count = 0) then
        bGotoPerforce := true;

      if not (bGotoPerforce or pbForce) then
        begin
          dtNow := Now - FLastChecked;
          DecodeTime(dtNow, wHour, wMinute, wSec, wMSec);
          if (wHour > 0) or (wMinute > 0) or (wSec > 5) then
            bGotoPerforce := true;
        end;

      if bGotoPerforce or pbForce then
        begin
          FFileName := psFileName;
          Result := P4Engine.GetFileInfo(FFileName, FFileInfo);
          FLastChecked := Now;
        end
      else
        begin
          if WildcardCompare('*no such file*', FFileInfo.Text) or
             WildcardCompare('*(P4PASSWD) invalid or unset*', FFileInfo.Text) or
             WildcardCompare('*is not under client''s root*', FFileInfo.Text) or
             WildcardCompare('*Failed to connect to Peforce server*', FFileInfo.Text) then
            begin
              Result := false;
            end;
        end;
      pslInfo.Assign(FFileInfo);
    end
  else
    Result := false;
end;

procedure TPerforceExpert.ForceFileInfoUpdate;
var
  slTemp   : TStringList;
begin
  if P4Engine.IsServerUp and (FFileName <> '') then
    begin
      slTemp := TStringList.Create;
      try
        GetModuleFileInfo(slTemp, true);
      finally
        slTemp.Free;
      end;
    end;
end;

function TPerforceExpert.Modified: Boolean;
var
  Serv            : IOTAModuleServices;
begin
  Result := false;

  Serv := (BorlandIDEServices as IOTAModuleServices);
  if Serv.CurrentModule <> nil then
    Result := Serv.CurrentModule.CurrentEditor.Modified;
end;

function TPerforceExpert.GetModuleFileInfo(pslInfo: TStrings; pbForce: Boolean): Boolean;
var
  Serv            : IOTAModuleServices;
begin
  Result := false;

  Serv := (BorlandIDEServices as IOTAModuleServices);
  {$IFDEF DELPHI2005}
  if P4Engine.IsServerUp and (Serv.CurrentModule <> nil) and (Serv.CurrentModule.FileName <> 'default.htm') then
  {$ELSE}
  if P4Engine.IsServerUp and (Serv.CurrentModule <> nil) then
  {$ENDIF}
    begin
      Result := GetFileInfo(Serv.CurrentModule.FileName, pslInfo, pbForce);
    end;
end;

function TPerforceExpert.GetTempFile: string;
var
  PathName: array[0..MAX_PATH] of Char;
begin
  Windows.GetTempPath(MAX_PATH, @PathName);
  Result := string(PathName);
  if AnsiLastChar(Result)^ <> '\' then
    Result := Result + '\';
  Result := Result + 'DelphiP4\DP4.tmp';
end;

procedure TPerforceExpert.DelTempFile;
begin
  if FileExists(GetTempFile) then
    begin
      {$IFDEF MSWINDOWS}
      {$WARN SYMBOL_PLATFORM OFF}
      FileSetAttr(GetTempFile, faArchive); // Make sure it's not read-only.
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}
      DeleteFile(GetTempFile);
    end;
end;

procedure TPerforceExpert.OptionsExecute(Sender: TObject);
begin
  if FOptions.ShowDialogue then
    RefreshShortcuts;
end;

procedure TPerforceExpert.P4VisualClientExecute(Sender: TObject);
begin
  P4Engine.ShowVisualClient;
end;

function TPerforceExpert.GetDiffProg: String;
begin
  if FOptions.UseP4DiffProg or (not FileExists(FOptions.DiffProg)) then
    // does not work with P4 Visual client installed, only with P4Win
    // Result := 'p4diff'
    Result := 'p4merge'
  else
    begin
      Result := FOptions.DiffProg;
      if Pos(' ', Result) > 0 then
        Result := '"' + Result + '"';
    end;
end;

procedure TPerforceExpert.AddAllExecute(Sender: TObject);
var
  Serv     : IOTAModuleServices;
  Proj     : IOTAProject;
  slFiles  : TStringList;
  ProjGrp  : IOTAProjectGroup;
  iCounter : Integer;
begin
  Serv := (BorlandIDEServices as IOTAModuleServices);

  slFiles := TStringList.Create;
  try
    Proj := nil;

    for iCounter := 0 to Serv.ModuleCount - 1 do
      begin
        if Supports(Serv.Modules[iCounter], IOTAProjectGroup, ProjGrp) then
          Proj := ProjGrp.ActiveProject;
        if Proj = nil then
          Supports(Serv.Modules[iCounter], IOTAProject, Proj);
      end;

    if Proj <> nil then
      begin
        Screen.Cursor := crHourGlass;
        try
          BuildFileList(Proj, slFiles);
        finally
          Screen.Cursor := crDefault;
        end;
        if slFiles.Count = 0 then
          MessageDlg('All files in this project are currently archived in Perforce.', mtInformation, [mbOK], 0)
        else
          begin
            with TfrmAddAllDialog.Create(nil) do
              try
                SetFiles(slFiles);
                if Execute then
                  begin
                    Screen.Cursor := crHourGlass;
                    try
                      GetFiles(slFiles);
                      P4Engine.AddArchive(slFiles, ChangeList);
                    finally
                      Screen.Cursor := crDefault;
                    end;
                  end;
              finally
                Free;
              end;
          end;
    end;
  finally
    slFiles.Free;
  end;
end;

procedure TPerforceExpert.AddAllUpdate(Sender: TObject);
var
  Serv            : IOTAModuleServices;
  bEnable         : Boolean;
begin
  bEnable := false;

  Serv := (BorlandIDEServices as IOTAModuleServices);
  if P4Engine.IsServerUp and (Serv.CurrentModule <> nil) and P4Engine.LoggedIn then
    bEnable := true;

  FActionAddAll.Enabled := bEnable;
end;

function TPerforceExpert.BuildFileList(AProject: IOTAProject; AList: TStrings): Boolean;
var
  iCounter   : Integer;
  sName      : String;
  Module     : IOTAModuleInfo;
begin
  Result := false;

  AList.Clear;

  for iCounter := 0 to AProject.ModuleFileCount - 1 do
    begin
      sName := AProject.ModuleFileEditors[iCounter].FileName;
      if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
        AList.Add(sName);
      sName := ChangeFileExt(sName, '.dof');
      if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
        AList.Add(sName);
      sName := ChangeFileExt(sName, '.cfg');
      if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
        AList.Add(sName);
      sName := ChangeFileExt(sName, '.res');
      if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
        AList.Add(sName);
      {$IFDEF DELPHI2005}
      sName := ChangeFileExt(sName, '.bdsproj');
      if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
        AList.Add(sName);
      {$ENDIF}
    end;

  for iCounter := 0 to AProject.GetModuleCount - 1 do
    begin
      Module := AProject.GetModule(iCounter);
      sName := Module.FileName;
      if FileExists(sName) and (not (Module.ModuleType in [omtObj,omtPackageImport,omtLib])) and (not P4Engine.FileArchived(sName)) then
        begin
          if AList.IndexOf(sName) < 0 then
            AList.Add(sName);
          sName := ChangeFileExt(sName, '.dfm');
          if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
            AList.Add(sName);
          sName := ChangeFileExt(sName, '.bdsproj');
          if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
            AList.Add(sName);
          if FOptions.AddDDPFiles then
            begin
              sName := ChangeFileExt(sName, '.ddp');
              if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
                AList.Add(sName);
            end;
          if FOptions.AddTodoFiles then
            begin
              sName := ChangeFileExt(sName, '.todo');
              if FileExists(sName) and (AList.IndexOf(sName) < 0) and (not P4Engine.FileArchived(sName)) then
                AList.Add(sName);
            end;
        end;
    end;
end;

procedure TPerforceExpert.RemoveAction(AAction: TAction;
  AToolbar: TToolbar);
var
  iCounter    : Integer;
  btnTool     : TToolButton;
begin
  for iCounter := AToolbar.ButtonCount - 1 downto 0 do
    begin
      btnTool := AToolbar.Buttons[iCounter];
      if btnTool.Action = AAction then
        begin
          AToolbar.Perform(CM_CONTROLCHANGE, WParam(btnTool), 0);
          btnTool.Free;
        end;
    end;
end;

procedure TPerforceExpert.RemoveActionFromToolbar(AAction: TAction);
var
  Services   : INTAServices;
begin
  Services := (BorlandIDEServices as INTAServices);

  RemoveAction(AAction, Services.ToolBar[sCustomToolBar]);
  RemoveAction(AAction, Services.ToolBar[sDesktopToolBar]);
  RemoveAction(AAction, Services.ToolBar[sStandardToolBar]);
  RemoveAction(AAction, Services.ToolBar[sDebugToolBar]);
  RemoveAction(AAction, Services.ToolBar[sViewToolBar]);
//  RemoveAction(AAction, Services.ToolBar['InternetToolBar']);
end;

function TPerforceExpert.LatestVersions(pType : TP4FileListType): Boolean;
var
  slFileList      : TStringList;
  iCounter        : Integer;
begin
  Result := true;
  slFileList := TStringList.Create;
  try
    ListFiles(slFileList, pType);
    iCounter := 0;
    while (iCounter < slFileList.Count) and Result do
      begin
        Result := Result and P4Engine.HaveHeadRevision(slFileList[iCounter]);
        Inc(iCounter);
      end;
  finally
    slFileList.Free;
  end;
end;

function TPerforceExpert.AddAction(psCaption, psHint, psName, psImageRes,
  psImageName: String; ExecuteEvent, UpdateEvent : TNotifyEvent): TAction;
var
  Service : INTAServices;
  bmpButton  : TBitmap;
begin
  Service := (BorlandIDEServices as INTAServices);

  Result := TAction.Create(Service.ActionList);
  with Result do
    begin
      ActionList := Service.ActionList;
      Category := PERFORCE_CAT;
      Caption := psCaption;
      Hint := psHint;
      Name := psName;
      if (psImageRes <> '') and (psImageName <> '') then
        begin
          bmpButton := TBitmap.Create;
          try
            try
              bmpButton.LoadFromResourceName(HInstance, psImageRes);
              ImageIndex := Service.AddMasked(bmpButton, clSilver, psImageName);
            except
              on E: Exception do
                MessageDlg(E.Message, mtError, [mbOK], 0);
            end;
          finally
            bmpButton.Free;
          end;
        end;
      OnExecute := ExecuteEvent;
      OnUpdate := UpdateEvent;
    end;
end;

function TPerforceExpert.AddMenu(pAction: TAction): TMenuItem;
begin
  Result := TMenuItem.Create(FMenuPerforce);

  Result.Action := pAction;
  FMenuPerforce.Add(Result);
end;

procedure TPerforceExpert.RestoreBookmarks(AStorage: TSavedBookmarks);
var
  Editor    : IOTASourceEditor;
  iCounter  : Integer;
  MovePos,
  CurPos    : TOTAEditPos;
begin
  Editor := GetSourceEditor;
  if Editor <> nil then
    begin
      CurPos := Editor.EditViews[0].CursorPos;
      for iCounter := 0 to 9 do
        begin
          if (AStorage[iCounter].Line > 0) then
            begin
              MovePos.Col := AStorage[iCounter].CharIndex + 1;
              MovePos.Line := AStorage[iCounter].Line;
              Editor.EditViews[0].CursorPos := MovePos;
              Editor.EditViews[0].BookmarkRecord(iCounter);
            end;
        end;
      Editor.EditViews[0].CursorPos := CurPos;
      Editor.EditViews[0].MoveViewToCursor;
    end;
end;

function TPerforceExpert.StoreBookmarks(var AStorage: TSavedBookmarks): Boolean;
var
  Editor    : IOTASourceEditor;
  iCounter  : Integer;
begin
  Result := false;
  Editor := GetSourceEditor;

  if (Editor <> nil) and (Editor.EditViewCount > 0) then
    begin
      Result := true;
      for iCounter := 0 to 9 do
        AStorage[iCounter] := Editor.EditViews[0].BookmarkPos[iCounter];
    end;
end;

function TPerforceExpert.GetSourceEditor: IOTASourceEditor;
var
  Serv      : IOTAModuleServices;
  iCounter  : Integer;
begin
  Result := nil;
  if Supports(BorlandIDEServices, IOTAModuleServices, Serv) then
    begin
      iCounter := 0;
      while (iCounter < Serv.CurrentModule.ModuleFileCount) and (Result = nil) do
        begin
          if not Supports(Serv.CurrentModule.ModuleFileEditors[iCounter], IOTASourceEditor, Result) then
            Inc(iCounter);
        end;
    end;
end;

procedure TPerforceExpert.RefreshShortcuts;
begin
  FActionAdd.ShortCut := FOptions.SC_AddNewArchive;

  FActionEdit.Shortcut := FOptions.SC_OpenForEdit;
  FActionSubmit.Shortcut := FOptions.SC_Submit;
  FActionRevert.Shortcut := FOptions.SC_Revert;
  FActionSync.Shortcut := FOptions.SC_Sync;
  FActionLock.Shortcut := FOptions.SC_Lock;
  FActionUnlock.Shortcut := FOptions.SC_Unlock;
  FActionDiff.Shortcut := FOptions.SC_DiffCurrent;
  FActionDiffHead.Shortcut := FOptions.SC_DiffHead;
  // FActionRevisionHistory.ShortCut := FOptions.SC_RevisionHistory;
  FActionP4VisualClient.ShortCut := FOptions.SC_VisualClient;
  FActionFileInfo.Shortcut := FOptions.SC_Properties;
  FActionOptions.Shortcut := FOptions.SC_Options;
  FActionAddAll.Shortcut := FOptions.SC_AddAll;
  FActionInfo.Shortcut := FOptions.SC_About;
end;

procedure TPerforceExpert.LoginUpdate(Sender: TObject);
begin
  FActionLogin.Enabled := P4Engine.IsServerUp and not P4Engine.LoggedIn;
end;

procedure TPerforceExpert.LoginExecute(Sender: TObject);
begin
  with TFrmLogin.Create(nil) do
  try
    EdtPassword.Text := FOptions.Password;
    ChkRemember.Checked := Length(EdtPassword.Text) > 0;

    if ShowModal = mrOk then
    begin
      if ChkRemember.Checked then
        FOptions.Password := EdtPassword.Text
      else
        FOptions.Password := '';

      FOptions.SaveToReg(REG_KEY);

      P4Engine.Login(EdtPassword.Text);
      LogLastP4Transaction;
    end;
  finally
    Free;
  end;
end;

procedure TPerforceExpert.LogoutUpdate(Sender: TObject);
begin
  FActionLogout.Enabled := P4Engine.IsServerUp and P4Engine.LoggedIn;
end;

procedure TPerforceExpert.LogoutExecute(Sender: TObject);
begin
  P4Engine.Logout;
  LogLastP4Transaction;
end;

procedure TPerforceExpert.CheckServerExecute(Sender: TObject);
begin
  if P4Engine.IsServerUp then
    MessageDlg('Server is up', mtInformation, [mbOK], 0)
  else
    MessageDlg('Server is currently down.', mtInformation, [mbOK], 0);
end;

procedure TPerforceExpert.CheckServerUpdate(Sender: TObject);
begin
//  P4Engine.CheckServerStatus;
end;

{

procedure TPerforceExpert.RevisionHistoryExecute(Sender: TObject);
begin
  P4Engine.ShowRevisionHistory(CurrentEditorFileName);
end;

procedure TPerforceExpert.RevisionHistoryUpdate(Sender: TObject);
var
  bRevisionHistoryEnable   : Boolean;
  slInfo        : TStringList;
begin
  bRevisionHistoryEnable := false;

  slInfo := TStringList.Create;
  try
    if GetModuleFileInfo(slInfo) then
      begin
        if not WildcardCompare('*no such file*', slInfo.Text) then
          bRevisionHistoryEnable := true;
      end;
  finally
    slInfo.Free;
  end;

  FActionRevisionHistory.Enabled := bRevisionHistoryEnable;
end;
}

{ TShortcutHolder }

constructor TShortcutHolder.Create(Action: TAction; Shortcut: TShortCut);
begin
  FAction := Action;
  FShortcut := Shortcut;
end;

initialization
  PerforceExpert := TPerforceExpert.Create;

finalization
  PerforceExpert.Free;

end.
