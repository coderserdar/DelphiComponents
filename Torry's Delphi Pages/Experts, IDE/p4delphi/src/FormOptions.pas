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
unit FormOptions;

{----------------------------------------------------------------------------

   Unit Name     :  FormOptions
   Date Created  :  23 January 2002
   Author        :  Chris Fairall
   Description   :  Delphi-specific options for the Perforce interface.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/FormOptions.pas $
   $Revision: #6 $
   $DateTime: 2007/04/09 19:53:02 $
   $Author: Chris $

 ----------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UnitP4Misc, Buttons, ComCtrls, ExtCtrls;

type
  TfrmOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    chbxShowMsg: TCheckBox;
    chbxAddDDP: TCheckBox;
    rbtnP4Diff: TRadioButton;
    rbtnOtherDiff: TRadioButton;
    edtDiffProg: TEdit;
    btnBrowse: TSpeedButton;
    dlgOpen: TOpenDialog;
    chbxAutoLock: TCheckBox;
    chkLatestVersion: TCheckBox;
    chkPreserveBkmk: TCheckBox;
    chkPreserveSel: TCheckBox;
    chbxAddTodoFiles: TCheckBox;
    pgcOptions: TPageControl;
    tsOptions: TTabSheet;
    tsShortcuts: TTabSheet;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    hkAddNewArchive: THotKey;
    hkOpenForEdit: THotKey;
    hkRevertChanges: THotKey;
    hkSubmitChanges: THotKey;
    hkAddFilesInProject: THotKey;
    hkSync: THotKey;
    hkLock: THotKey;
    hkUnlock: THotKey;
    hkDiffCurrent: THotKey;
    hkDiffHead: THotKey;
    hkProperties: THotKey;
    hkOptions: THotKey;
    hkAbout: THotKey;
    grpDiffTool: TGroupBox;
    chkAutoCheckServer: TCheckBox;
    edtServerStatusTime: TEdit;
    udServerStatusTime: TUpDown;
    Label14: TLabel;
    Image10: TImage;
    Label15: TLabel;
    hkRevisionHistory: THotKey;
    chkPromptOpenForEdit: TCheckBox;
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Execute(pOptions : TP4Options) : Boolean;
  end;

implementation

{$R *.dfm}

{ TfrmOptions }

function TfrmOptions.Execute(pOptions: TP4Options): Boolean;
begin
  chbxShowMsg.Checked := pOptions.ShowMessages;
  chbxAddDDP.Checked := pOptions.AddDDPFiles;
  chbxAddTodoFiles.Checked := pOptions.AddTodoFiles;
  chbxAutoLock.Checked := pOptions.AutoLock;
  chkLatestVersion.Checked := pOptions.CheckForLatest;
  chkPreserveBkmk.Checked := pOptions.PreserveBookmarks;
  chkPreserveSel.Checked := pOptions.PreserveSelection;
  chkAutoCheckServer.Checked := pOptions.AutoCheckServer;
  udServerStatusTime.Position := pOptions.CheckServerTime;
  chkPromptOpenForEdit.Checked := pOptions.OpenForEditPrompt;
  { Shortcuts }
  hkAddNewArchive.HotKey := pOptions.SC_AddNewArchive;
  hkOpenForEdit.HotKey := pOptions.SC_OpenForEdit;
  hkRevertChanges.HotKey := pOptions.SC_Revert;
  hkSubmitChanges.HotKey := pOptions.SC_Submit;
  hkAddFilesInProject.HotKey := pOptions.SC_AddAll;
  hkSync.HotKey := pOptions.SC_Sync;
  hkLock.HotKey := pOptions.SC_Lock;
  hkUnlock.HotKey := pOptions.SC_Unlock;
  hkDiffCurrent.HotKey := pOptions.SC_DiffCurrent;
  hkDiffHead.HotKey := pOptions.SC_DiffHead;
  hkRevisionHistory.HotKey := pOptions.SC_RevisionHistory;
  hkProperties.HotKey := pOptions.SC_Properties;
  hkOptions.HotKey := pOptions.SC_Options;
  hkAbout.HotKey := pOptions.SC_About;

  if pOptions.UseP4DiffProg then
    rbtnP4Diff.Checked := true
  else
    rbtnOtherDiff.Checked := true;
  edtDiffProg.Text := pOptions.DiffProg;

  Result := (ShowModal = mrOK);
  if Result then
    begin
      pOptions.ShowMessages := chbxShowMsg.Checked;
      pOptions.AddDDPFiles := chbxAddDDP.Checked;
      pOptions.AddTodoFiles := chbxAddTodoFiles.Checked;
      pOptions.UseP4DiffProg := rbtnP4Diff.Checked or (not FileExists(edtDiffProg.Text));
      pOptions.DiffProg := edtDiffProg.Text;
      pOptions.AutoLock := chbxAutoLock.Checked;
      pOptions.CheckForLatest := chkLatestVersion.Checked;
      pOptions.PreserveBookmarks := chkPreserveBkmk.Checked;
      pOptions.PreserveSelection := chkPreserveSel.Checked;
      pOptions.AutoCheckServer := chkAutoCheckServer.Checked;
      pOptions.CheckServerTime := udServerStatusTime.Position;
      pOptions.OpenForEditPrompt := chkPromptOpenForEdit.Checked;
      { Shortcuts }
      pOptions.SC_AddNewArchive := hkAddNewArchive.HotKey;
      pOptions.SC_OpenForEdit := hkOpenForEdit.HotKey;
      pOptions.SC_Revert := hkRevertChanges.HotKey;
      pOptions.SC_Submit := hkSubmitChanges.HotKey;
      pOptions.SC_AddAll := hkAddFilesInProject.HotKey;
      pOptions.SC_Sync := hkSync.HotKey;
      pOptions.SC_Lock := hkLock.HotKey;
      pOptions.SC_Unlock := hkUnlock.HotKey;
      pOptions.SC_DiffCurrent := hkDiffCurrent.HotKey;
      pOptions.SC_DiffHead := hkDiffHead.HotKey;
      pOptions.SC_RevisionHistory := hkRevisionHistory.HotKey;
      pOptions.SC_Properties := hkProperties.HotKey;
      pOptions.SC_Options := hkOptions.HotKey;
      pOptions.SC_About := hkAbout.HotKey;
    end;
end;

procedure TfrmOptions.btnBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtDiffProg.Text := dlgOpen.FileName;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  pgcOptions.ActivePageIndex := 0;
end;

end.
