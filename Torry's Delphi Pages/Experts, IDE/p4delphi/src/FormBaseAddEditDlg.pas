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
unit FormBaseAddEditDlg;

{----------------------------------------------------------------------------

   Unit Name     :  FormBaseAddEditDlg
   Date Created  :  21 May 2002
   Author        :  Chris Fairall
   Description   :  Basic add/edit dialogue, from which the standard Add/Edit
                    dialogue, and the Add All Files dialogue are descended.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/FormBaseAddEditDlg.pas $
   $Revision: #3 $
   $DateTime: 2004/03/17 18:55:36 $
   $Author: fairallc $

 ----------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmBaseAddEditDlg = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    lblDescription: TLabel;
    cbxChangeLists: TComboBox;
    memDescription: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure cbxChangeListsChange(Sender: TObject);
    procedure memDescriptionChange(Sender: TObject);
  private
    FChangeList: Integer;
    function GetChangelistNumber(const AChangelistStr: string): Integer;
    procedure SetChangeList(const Value: Integer);
  protected
    function EnableOKButton : Boolean; virtual;
  public
    function Execute : Boolean; virtual;
    property ChangeList : Integer read FChangeList write SetChangeList;    { -1 denotes default changelist }
  end;

implementation

uses
  UnitP4Expert, UnitP4Misc, UnitP4Engine;

{$R *.dfm}

procedure TfrmBaseAddEditDlg.FormCreate(Sender: TObject);
var
  slLists : TStringList;
begin
  FChangeList := -1;
  slLists := TStringList.Create;
  try
    P4Engine.PendingChangeLists(slLists);
    cbxChangeLists.Items.AddStrings(slLists);
  finally
    slLists.Free;
  end;
end;

procedure TfrmBaseAddEditDlg.cbxChangeListsChange(Sender: TObject);
var
  iChangeList   : Integer;
  sChangeList   : String;
begin
  memDescription.Lines.Clear;
  if cbxChangeLists.ItemIndex = 0 then
    begin
      memDescription.ReadOnly := true;
      memDescription.Color := clBtnFace;
    end
  else
    begin
      memDescription.ReadOnly := false;
      memDescription.Color := clWindow;
      if cbxChangeLists.ItemIndex > 1 then
        begin
          sChangeList := cbxChangeLists.Text;
          Delete(sChangeList, Pos(' ', sChangeList), Length(sChangeList));
          iChangeList := StrToInt(sChangeList);
          P4Engine.ChangeListInfo(iChangeList, memDescription.Lines, nil, nil);
          memDescription.Modified := false;
        end;
    end;
  btnOK.Enabled := EnableOKButton;
end;

function TfrmBaseAddEditDlg.EnableOKButton: Boolean;
begin
  Result := true;
  if (cbxChangeLists.ItemIndex = 1) and (Trim(memDescription.Lines.Text) = '') then
    Result := false;
end;

procedure TfrmBaseAddEditDlg.memDescriptionChange(Sender: TObject);
begin
  btnOK.Enabled := EnableOKButton;
end;

function TfrmBaseAddEditDlg.Execute: Boolean;
var
  sLine  : String;
begin
  if PerforceExpert <> nil then
    SetChangeList(PerforceExpert.Options.LastChangeList);

  Result := (ShowModal = mrOK);
  if Result then
    begin
      if cbxChangeLists.ItemIndex = 0 then
        FChangeList := -1
      else if cbxChangeLists.ItemIndex = 1 then
        begin
          FChangeList := P4Engine.CreateChangeList(memDescription.Text);
        end
      else
        begin
          sLine := cbxChangeLists.Text;
          Delete(sLine, Pos(' ', sLine), Length(sLine));
          try
            FChangeList := StrToInt(sLine);
          except
            ShowMessage('Could not translate text to integer "' + sLine + '".');
            Result := false;
          end;
          if memDescription.Modified then
            begin
              P4Engine.UpdateChangeList(FChangeList, memDescription.Text);
            end;
        end;
      if PerforceExpert <> nil then
        begin
          PerforceExpert.Options.LastChangeList := ChangeList;
          PerforceExpert.Options.SaveToReg(REG_KEY);
        end;
    end;
end;

procedure TfrmBaseAddEditDlg.SetChangeList(const Value: Integer);
var
  i: Integer;
begin
  if FChangeList <> Value then
  begin
    if Value = -1 then
      cbxChangeLists.ItemIndex := 0
    else
    begin
      for i := 2 to cbxChangeLists.Items.Count-1 do
      begin
        if Value = GetChangelistNumber(cbxChangeLists.Items[i]) then
        begin
          cbxChangeLists.ItemIndex := i;
          Break;
        end;
      end;
    end;
    cbxChangeListsChange(nil);
  end;
end;

function TfrmBaseAddEditDlg.GetChangelistNumber(const AChangelistStr: string):
    Integer;
var
  sLine  : String;
begin
  sLine := AChangelistStr;
  Delete(sLine, Pos(' ', sLine), Length(sLine));
  Result := StrToInt(sLine);
end;

end.
