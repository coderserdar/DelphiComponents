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
unit FormFileProps;

{----------------------------------------------------------------------------

   Unit Name     :  FormFileProps
   Date Created  :  21 May 2002
   Author        :  Chris Fairall
   Description   :  Displays file properties from Perforce. Populated by the
                    calling routine before it displays it.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/FormFileProps.pas $
   $Revision: #3 $
   $DateTime: 2005/02/19 18:04:40 $
   $Author: fairallc $

 ----------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmFileProperties = class(TForm)
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
    edtDepotPath: TEdit;
    edtClientPath: TEdit;
    edtFileType: TEdit;
    edtHeadRev: TEdit;
    edtHeadAction: TEdit;
    edtHaveRev: TEdit;
    edtHeadChange: TEdit;
    edtLastModTime: TEdit;
    edtLockedBy: TEdit;
    btnOK: TButton;
    lbxOpenedBy: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Execute(psFileName : String);
  end;

implementation

uses
  UnitP4Engine, UnitP4Misc;

{$R *.dfm}

{ TfrmFileProperties }

procedure TfrmFileProperties.Execute(psFileName: String);
var
  slInfo    : TStringList;
  sLine,
  sCmd,
  sUser     : String;
  dtModTime : TDateTime;
  iPos,
  iCounter,
  iModTime  : Integer;
  bLocked   : Boolean;
begin
  slInfo := TStringList.Create;
  try
    P4Engine.GetFileInfo(psFileName, slInfo);

    { Get info from p4 fstat }
    edtDepotPath.Text := slInfo.Values['depotFile'];
    edtClientPath.Text := slInfo.Values['clientFile'];
    edtFileType.Text := slInfo.Values['headType'];
    edtHeadRev.Text := slInfo.Values['headRev'];
    edtHeadAction.Text := slInfo.Values['headAction'];
    edtHaveRev.Text := slInfo.Values['haveRev'];
    edtHeadChange.Text := slInfo.Values['headChange'];
    if slInfo.Values['headTime'] <> '' then
      begin
        iModTime := StrToInt(slInfo.Values['headTime']);
        dtModTime := P4Engine.P4DateToDateTime(iModTime);
        edtLastModTime.Text := FormatDateTime('dddd, d mmmm yyyy  h:nn:ss AM/PM', dtModTime);
      end
    else
      edtLastModTime.Text := '';

    { Get additional opening information }
    sCmd := 'opened -a "' + ExtractFileName(psFileName) + '"';
    psFileName := ExtractFilePath(psFileName);
    slInfo.Text := P4Engine.Command(sCmd, ExtractFilePath(psFileName));
    for iCounter := 0 to slInfo.Count - 1 do
      begin
        sLine := slInfo[iCounter];
        iPos := Pos('#', sLine);
        iPos := PosFrom(' - ', sLine, iPos);
        Delete(sLine, 1, iPos + 2);

        bLocked := Pos('*locked*', sLine) > 0;

        iPos := Pos(' by ', sLine);
        sUser := Copy(sLine, iPos + 4, Length(sLine));
        Delete(sLine, iPos, Length(sLine));
        iPos := Pos(' ', sUser);
        if iPos > 0 then
          Delete(sUser, iPos, Length(sUser));
        if bLocked then
          edtLockedBy.Text := sUser;
        sLine := sUser + ' - ' + sLine;

        lbxOpenedBy.Items.Add(sLine);
      end;

    ShowModal;
  finally
    slInfo.Free;
  end;
end;

end.
