{*********************************************************}
{* FlashFiler: Options configuration                     *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit frmOptions;

interface

uses
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TffVerifyOptions = class
  protected
    FOutputVersion : Longint;
    procedure Load;
  public
    constructor Create;
    procedure Save;

    property OutputVersion : Longint
      read FOutputVersion write FOutputVersion;
  end;

  TfrmOptionsConfig = class(TForm)
    pnlBottom: TPanel;
    pbOK: TButton;
    pbCancel: TButton;
    pnlClient: TPanel;
    lblVersion: TLabel;
    efVersion: TEdit;
    lblValidRange: TLabel;
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure efVersionKeyPress(Sender: TObject; var Key: Char);
    procedure efVersionChange(Sender: TObject);
  private
    { Private declarations }
    FOptions : TffVerifyOptions;
    
    function GetOutputVersion : Longint;
    procedure SetCtrlStates;
    function ValidVersion : Boolean;
  public
    { Public declarations }
    property OutputVersion : Longint
      read GetOutputVersion;
  end;

var
  frmOptionsConfig: TfrmOptionsConfig;

implementation

uses
  ffllbase,
  IniFiles;

{$R *.dfm}

const
  cIniFile = 'FFVerify.ini';
  cSect = 'Options';
  cVersion = 'OutputVersion';

{===TffVerifyOptions=================================================}
constructor TffVerifyOptions.Create;
begin
  inherited;
  Load;
end;
{--------}
procedure TffVerifyOptions.Load;
begin
  with TIniFile.Create(cIniFile) do
    try
      FOutputVersion := ReadInteger(cSect, cVersion, ffVersionNumber);
    finally
      Free;
    end;
end;
{--------}
procedure TffVerifyOptions.Save;
begin
  with TIniFile.Create(cIniFile) do
    try
      WriteInteger(cSect, cVersion, FOutputVersion);
    finally
      Free;
    end;
end;
{====================================================================}

procedure TfrmOptionsConfig.pbOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
  FOptions.OutputVersion := GetOutputVersion;
  FOptions.Save;
  FOptions.Free;
end;

procedure TfrmOptionsConfig.FormShow(Sender: TObject);
begin
  { Read the options from the INI file. }
  FOptions := TffVerifyOptions.Create;
  efVersion.Text := IntToStr(FOptions.OutputVersion);
  lblValidRange.Caption := Format('Valid range: %d to %d',
                                  [ffVersion2_10, ffVersionNumber]);
  SetCtrlStates;
  efVersion.SetFocus;
end;

function TfrmOptionsConfig.GetOutputVersion : Longint;
var
  TmpStr,
  VerStr : string;
  TmpLen,
  SrcInx,
  TgtInx : Integer;
begin
  { Strip out all decimal points. }
  TmpStr := efVersion.Text;
  TmpLen := Length(TmpStr);
  SetLength(VerStr, TmpLen);
  TgtInx := 1;
  for SrcInx := 1 to TmpLen do
    if TmpStr[SrcInx] in ['0'..'9'] then begin
      VerStr[TgtInx] := TmpStr[SrcInx];
      inc(TgtInx);
    end;
  SetLength(VerStr, Pred(TgtInx));
  Result := StrToInt(VerStr);
end;

function TfrmOptionsConfig.ValidVersion : Boolean;
var
  Version : Longint;
begin
  try
    Version := GetOutputVersion;
    { The version # is valid if it an integer between 21000 and the current
      FF version. }
    Result := (Version >= ffVersion2_10) and (Version <= ffVersionNumber);
  except
    Result := False;
  end;
end;

procedure TfrmOptionsConfig.efVersionKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9', '.']) then begin
    Beep;
    Key := #0;
  end;
end;

procedure TfrmOptionsConfig.SetCtrlStates;
begin
  pbOK.Enabled := ValidVersion;
end;

procedure TfrmOptionsConfig.efVersionChange(Sender: TObject);
begin
  SetCtrlStates;
end;

end.
