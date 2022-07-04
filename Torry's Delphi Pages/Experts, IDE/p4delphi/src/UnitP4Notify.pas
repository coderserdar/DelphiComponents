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
unit UnitP4Notify;

interface

uses
  Windows, SysUtils, Controls, Graphics, Classes, Menus, ActnList, ToolsAPI,
  Dialogs, Forms;

type
  TPerforceNotifier = class(TNotifierObject, IOTAIDENotifier)
  private
    procedure ListFiles(slList : TStrings; sFileName : String);
    function InValidExtensions(sFileName : String) : Boolean;
  public
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: String; var Cancel: Boolean);
  end;

implementation

uses
  UnitP4Engine, UnitP4Expert;

{ TPerforceNotifier }

procedure TPerforceNotifier.AfterCompile(Succeeded: Boolean);
begin
  // Do nothing
end;

procedure TPerforceNotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  // Do nothing
end;

procedure TPerforceNotifier.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: String;
  var Cancel: Boolean);
const
  WARN_MSG = '     "%s";  You have revision: %s; Current head revision: %s';
var
  slData,
  slFiles   : TStringList;
  sMsg      : String;
  iCounter  : Integer;
begin
  PerforceExpert.RefreshShortcuts;
  if (NotifyCode = ofnFileOpening) and PerforceExpert.Options.CheckForLatest then
    begin
      slFiles := TStringList.Create;
      slData := TStringList.Create;
      try
        ListFiles(slFiles, FileName);
        sMsg := '';
        for iCounter := 0 to slFiles.Count - 1 do
          begin
            if P4Engine.GetFileInfo(slFiles[iCounter], slData) then
              begin
                if slData.Values['headRev'] <> slData.Values['haveRev'] then
                  begin
                    if sMsg <> '' then
                      sMsg := sMsg + #13#10;
                    sMsg := sMsg + Format(WARN_MSG, [ExtractFileName(slFiles[iCounter]), slData.Values['haveRev'], slData.Values['headRev']]);
                  end;
              end;
          end;
        if sMsg <> '' then
          begin
            sMsg := 'You do not have the head revision of the following files:'#13#10#13#10 + sMsg +
                    #13#10#13#10'Would you like to sync to head revision first?';
            case MessageDlg(sMsg, mtWarning, [mbYes, mbNo, mbCancel], 0) of
              mrCancel:
                Cancel := true;
              mrYes:
                begin
                  P4Engine.Sync(slFiles, false);
                end;
              end;
          end;
      finally
        slFiles.Free;
        slData.Free;
      end;
    end;
end;

function TPerforceNotifier.InValidExtensions(sFileName: String): Boolean;
const
  VALID_EXT : Array[0..10] of String =
              ('.pas', '.dfm', '.ddp', '.dpr', '.dpk', '.inc',
               '.dof', '.cfg', '.res', '.rc', '.bdsproj');
var
  sExt : String;
  iCounter  : Integer;
begin
  sExt := Lowercase(ExtractFileExt(sFileName));
  iCounter := 0;
  Result := false;
  while (not Result) and (iCounter <= High(VALID_EXT)) do
    begin
      if sExt = VALID_EXT[iCounter] then
        Result := true
      else
        Inc(iCounter);
    end;
end;

procedure TPerforceNotifier.ListFiles(slList: TStrings; sFileName: String);
var
  sr  : TSearchRec;
begin
  slList.Clear;
  sFileName := ChangeFileExt(sFileName, '.*');

  if FindFirst(sFileName, faAnyFile, sr) = 0 then
    begin
      repeat
        if InValidExtensions(sr.Name) then
          slList.Add(ExtractFilePath(sFileName) + sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
end;

end.
