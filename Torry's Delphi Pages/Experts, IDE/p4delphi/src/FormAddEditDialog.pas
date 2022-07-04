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
unit FormAddEditDialog;

{----------------------------------------------------------------------------

   Unit Name     :  FormAddEditDialog
   Date Created  :  21 May 2002
   Author        :  Chris Fairall
   Description   :  Open file for Add or Edit. Only gets user input. The
                    actual interaction with Perforce is done by the calling
                    routine.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/FormAddEditDialog.pas $
   $Revision: #2 $
   $DateTime: 2004/03/14 20:05:33 $
   $Author: fairallc $

 ----------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormBaseAddEditDlg, StdCtrls;

type
  TfrmAddEditDialog = class(TfrmBaseAddEditDlg)
    chbxAutoLock: TCheckBox;
    chbxSyncFirst: TCheckBox;
    lblWarning: TLabel;
  private
    function GetAutoLock: Boolean;
    procedure SetAutoLock(Value: Boolean);
    function GetSync: Boolean;
    procedure SetSync(Value: Boolean);
    { Private declarations }
  public
    { Properties }
    property AutoLock : Boolean read GetAutoLock write SetAutoLock;
    property SyncFirst : Boolean read GetSync write SetSync;
  end;

implementation

{$R *.dfm}

{ TfrmAddEditDialog }

function TfrmAddEditDialog.GetAutoLock: Boolean;
begin
  Result := chbxAutoLock.Checked;
end;

function TfrmAddEditDialog.GetSync: Boolean;
begin
  Result := chbxSyncFirst.Checked;
end;

procedure TfrmAddEditDialog.SetAutoLock(Value: Boolean);
begin
  chbxAutoLock.Checked := Value;
end;

procedure TfrmAddEditDialog.SetSync(Value: Boolean);
begin
  chbxSyncFirst.Checked := Value;
end;

end.
