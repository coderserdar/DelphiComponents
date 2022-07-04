{*********************************************************}
{*                   VPWAVPE.PAS 1.03                    *}
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

unit VpWavPE;
  {Wav File Property Editor }

interface

uses
  {$IFDEF VERSION6}
    DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
    DsgnIntf,
  {$ENDIF}
  VpBase, VpWavDlg, Forms;

type
  {TWavFileProperty}
  TWavFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue : string; override;
    procedure Edit; override;
  end;

implementation

(*****************************************************************************)
{ TWavFileProperty }

function TWavFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{=====}

function TWavFileProperty.GetValue : string;
begin
  Result := inherited GetValue;
end;
{=====}

procedure TWavFileProperty.Edit;
var
  SoundFinder : TFrmSoundDialog;
begin
  SoundFinder := TFrmSoundDialog.Create(Application);
  try
    SoundFinder.DingPath := Value;
    SoundFinder.Populate;
    SoundFinder.ShowModal;
    if SoundFinder.ReturnCode = rtCommit then begin
      if SoundFinder.CBDefault.Checked then
        Value := ''
      else
        Value := SoundFinder.FileListBox1.FileName;
    end;
  finally
    SoundFinder.Free;
  end;
end;
{=====}

end.
