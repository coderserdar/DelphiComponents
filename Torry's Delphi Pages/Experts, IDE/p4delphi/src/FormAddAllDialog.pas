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
unit FormAddAllDialog;

{----------------------------------------------------------------------------

   Unit Name     :  FormAddAllDialog
   Date Created  :  21 May 2002
   Author        :  Chris Fairall
   Description   :  Add all files not currently in the Perforce archive in
                    the project.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/FormAddAllDialog.pas $
   $Revision: #2 $
   $DateTime: 2004/03/14 20:05:33 $
   $Author: fairallc $

 ----------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormBaseAddEditDlg, Menus, StdCtrls, CheckLst;

type
  TfrmAddAllDialog = class(TfrmBaseAddEditDlg)
    chlbxFiles: TCheckListBox;
    popFiles: TPopupMenu;
    mntmSelectAll: TMenuItem;
    mntmInvertSel: TMenuItem;
    Label2: TLabel;
    procedure chlbxFilesClickCheck(Sender: TObject);
  private
  protected
    function EnableOKButton : Boolean; override;
  public
    procedure SetFiles(pslFileList : TStrings);
    procedure GetFiles(pslFileList : TStrings);
  end;

implementation

{$R *.dfm}

procedure TfrmAddAllDialog.chlbxFilesClickCheck(Sender: TObject);
begin
  btnOK.Enabled := EnableOKButton;
end;

function TfrmAddAllDialog.EnableOKButton: Boolean;
var
  iCounter     : Integer;
begin
  Result := inherited EnableOKButton;

  if Result then
    begin
      Result := false;
      for iCounter := 0 to chlbxFiles.Items.Count - 1 do
        if chlbxFiles.Checked[iCounter] then
          Result := true;
    end;
end;

procedure TfrmAddAllDialog.GetFiles(pslFileList: TStrings);
var
  iCounter    : Integer;
begin
  pslFileList.Clear;
  for iCounter := 0 to chlbxFiles.Items.Count - 1 do
    if chlbxFiles.Checked[iCounter] then
      pslFileList.Add(chlbxFiles.Items[iCounter]);
end;

procedure TfrmAddAllDialog.SetFiles(pslFileList: TStrings);
var
  iMaxWidth,
  iWidth,
  iCounter    : Integer;
begin
  chlbxFiles.Items.Assign(pslFileList);
  iMaxWidth := 0;
  for iCounter := 0 to chlbxFiles.Items.Count - 1 do
    begin
      chlbxFiles.Checked[iCounter] := true;
      iWidth := chlbxFiles.Canvas.TextWidth(chlbxFiles.Items[iCounter]);
      if iWidth > iMaxWidth then
        iMaxWidth := iWidth;
    end;
  chlbxFiles.ScrollWidth := iMaxWidth;
end;

end.
