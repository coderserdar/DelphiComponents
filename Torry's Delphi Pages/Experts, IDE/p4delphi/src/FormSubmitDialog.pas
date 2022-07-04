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
unit FormSubmitDialog;

{----------------------------------------------------------------------------

   Unit Name     :  FormSubmitDialog
   Date Created  :  21 May 2002
   Author        :  Chris Fairall
   Description   :  Dialogue used to submit changes to Perforce.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/FormSubmitDialog.pas $
   $Revision: #2 $
   $DateTime: 2004/03/14 20:05:33 $
   $Author: fairallc $

 ----------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Menus;

type
  TfrmSubmitDialog = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    memChangeDesc: TMemo;
    Label2: TLabel;
    clbxFiles: TCheckListBox;
    popCheckList: TPopupMenu;
    miCheckAll: TMenuItem;
    miCheckNone: TMenuItem;
    chbxReopen: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EnableOKButton(Sender: TObject);
    procedure CheckMenuOption(Sender: TObject);
  private
    FDesc: TStrings;
    FFiles: TStrings;
    FJobs: TStrings;
    FFormFile: TStrings;
    procedure SetDesc(Value: TStrings);
    procedure SetFiles(Value: TStrings);
    procedure SetJobs(Value: TStrings);
    procedure BuildFormFile(psChangeList : String);
    procedure SetFormFile(Value: TStrings);
    { Private declarations }
  public
    { Public declarations }
    function Execute(var psChangeList : String) : Boolean;
    property Files : TStrings read FFiles write SetFiles;
    property Jobs : TStrings read FJobs write SetJobs;
    property Description : TStrings read FDesc write SetDesc;
    property FormFile : TStrings read FFormFile write SetFormFile;
  end;

implementation

uses
  UnitP4Engine, UnitP4Misc;

{$R *.dfm}

{ TfrmSubmitDialog }

function TfrmSubmitDialog.Execute(var psChangeList: String): Boolean;
var
  iWidth,
  iHash,
  iCounter   : Integer;
  sExtra,
  sWork      : String;
begin
  memChangeDesc.Lines.Assign(Description);
  { List all files opened by the specified changelist. }
  FFiles.Text := P4Engine.Command('opened -c ' + psChangeList);
  clbxFiles.Items.Assign(Files);

  for iCounter := 0 to clbxFiles.Items.Count - 1 do
    begin
      clbxFiles.Checked[iCounter] := true;
      iWidth := clbxFiles.Canvas.TextWidth(clbxFiles.Items[iCounter]) + 20;
      if iWidth > clbxFiles.ScrollWidth then
        clbxFiles.ScrollWidth := iWidth;
    end;

  EnableOKButton(nil);

  Result := (ShowModal = mrOK);

  if Result then
    begin
      FFiles.Clear;
      for iCounter := 0 to clbxFiles.Items.Count - 1 do
        if clbxFiles.Checked[iCounter] then
          begin
            { Strip off the extra info }
            sWork := clbxFiles.Items[iCounter];
            iHash := Pos('#', sWork);
            if iHash > 0 then
              begin
                sExtra := Copy(sWork, iHash, Length(sWork));
                Delete(sWork, Pos('#', sWork), Length(sWork));
                if WildcardCompare('*add*', sExtra) then
                  sWork := sWork + #9'#add'
                else if WildcardCompare('*edit*', sExtra) then
                  sWork := sWork + #9'#edit'
                else
                  sWork := '';
              end;
            if sWork <> '' then
              FFiles.Add(sWork);
          end;
      FDesc.Assign(memChangeDesc.Lines);
      BuildFormFile(psChangeList);
    end;
end;

procedure TfrmSubmitDialog.SetDesc(Value: TStrings);
begin
  if Value = nil then
    FDesc.Clear
  else
    FDesc.Assign(Value);
end;

procedure TfrmSubmitDialog.SetFiles(Value: TStrings);
begin
  if Value = nil then
    FFiles.Clear
  else
    FFiles.Assign(Value);
end;

procedure TfrmSubmitDialog.SetJobs(Value: TStrings);
begin
  if Value = nil then
    FJobs.Clear
  else
    FJobs.Assign(Value);
end;

procedure TfrmSubmitDialog.FormCreate(Sender: TObject);
begin
  FFiles := TStringList.Create;
  FJobs := TStringList.Create;
  FDesc := TStringList.Create;
  FFormFile := TStringList.Create;
  btnOK.Enabled := false;
end;

procedure TfrmSubmitDialog.FormDestroy(Sender: TObject);
begin
  FFiles.Free;
  FJobs.Free;
  FDesc.Free;
  FFormFile.Free;
end;

procedure TfrmSubmitDialog.BuildFormFile(psChangeList : String);
var
  iCounter   : Integer;
begin
  FFormFile.Clear;
  if psChangeList = 'default' then
    psChangeList := 'new';
  FFormFile.Add('Change:'#9 + psChangeList);
  FFormFile.Add('Description:');
  for iCounter := 0 to FDesc.Count - 1 do
    FFormFile.Add(#9 + FDesc[iCounter]);
  FFormFile.Add('');
  FFormFile.Add('Files:');
  for iCounter := 0 to FFiles.Count - 1 do
    FFormFile.Add(#9 + FFiles[iCounter]);
end;

procedure TfrmSubmitDialog.SetFormFile(Value: TStrings);
begin
  if Value = nil then
    FFormFile.Clear
  else
    FFormFile.Assign(Value);
end;

procedure TfrmSubmitDialog.EnableOKButton(Sender: TObject);
var
  iCounter     : Integer;
  bOneChecked  : Boolean;
begin
  iCounter := 0;
  bOneChecked := false;
  while (not bOneChecked) and (iCounter < clbxFiles.Items.Count) do
    begin
      if clbxFiles.Checked[iCounter] then
        bOneChecked := true;
      Inc(iCounter);
    end;

  btnOK.Enabled := (Trim(memChangeDesc.Lines.Text) <> '') and bOneChecked;
end;

procedure TfrmSubmitDialog.CheckMenuOption(Sender: TObject);
var
  iCounter   : Integer;
begin
  for iCounter := 0 to clbxFiles.Items.Count - 1 do
    clbxFiles.Checked[iCounter] := (Sender = miCheckAll);
end;

end.
