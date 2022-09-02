{*********************************************************}
{* Query options dialog                                  *}
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

unit dgsqlops;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TfrmSQLOps = class(TForm)
    gbExecute: TGroupBox;
    gbOther: TGroupBox;
    cbSyntaxOnly: TCheckBox;
    lblTimeout: TLabel;
    edtTimeout: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    cbLiveDS: TCheckBox;
    edtQueryName: TEdit;
    Label2: TLabel;
    btnFont: TButton;
    FontDialog: TFontDialog;
    procedure btnFontClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    procedure SetQueryName(aQueryName : string);
    procedure SetFont(aFont : TFont);
    procedure SetSyntaxOnly(aSetting : Boolean);
    procedure SetTimeout(aTimeout : Integer);
    procedure SetReqLiveDS(aSetting : Boolean);
    function  GetTimeout : Integer;
    function  GetQueryName : string;
    function  GetSyntaxOnly : Boolean;
    function  GetReqLiveDS : Boolean;
    function  GetFont : TFont;
  public
    { Public declarations }
    property SyntaxOnly : Boolean
      read  GetSyntaxOnly
      write SetSyntaxOnly;
    property Timeout : Integer
      read  GetTimeOut
      write SetTimeout;
    property RequestLiveDS : Boolean
      read  GetReqLiveDS
      write SetReqLiveDS;
    property QueryName : string
      read  GetQueryName
      write SetQueryName;
    property Font : TFont
      read  GetFont
      write SetFont;
  end;

var
  frmSQLOps: TfrmSQLOps;

implementation

{$R *.DFM}

{ TfrmSQLOps }

procedure TfrmSQLOps.SetFont(aFont : TFont);
begin
  FontDialog.Font := aFont;
end;

procedure TfrmSQLOps.SetQueryName(aQueryName : string);
begin
  edtQueryName.Text := aQueryName;
end;

procedure TfrmSQLOps.btnFontClick(Sender: TObject);
begin
  FontDialog.Execute;
end;

function TfrmSQLOps.GetTimeout: Integer;
begin
  Result := StrToInt(edtTimeout.Text);
end;

function TfrmSQLOps.GetQueryName: string;
begin
  result := edtQueryName.Text;
end;

function TfrmSQLOps.GetSyntaxOnly : Boolean;
begin
  Result := cbSyntaxOnly.Checked;
end;

function TfrmSQLOps.GetReqLiveDS : Boolean;
begin
  Result := cbLiveDS.Checked;
end;

function TfrmSQLOps.GetFont : TFont;
begin
  Result := FontDialog.Font;
end;

procedure TfrmSQLOps.SetReqLiveDS(aSetting : Boolean);
begin
  cbLiveDS.Checked := aSetting;
end;

procedure TfrmSQLOps.SetSyntaxOnly(aSetting : Boolean);
begin
  cbSyntaxOnly.Checked := aSetting;
end;

procedure TfrmSQLOps.SetTimeout(aTimeout : Integer);
begin
  edtTimeout.Text := IntToStr(aTimeout);
end;

procedure TfrmSQLOps.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Code : Integer;
  Int  : Integer;
begin
  Val(edtTimeout.Text, Int, Code);
  CanClose := (Code = 0) and (Int > -2);
  if not CanClose then begin
    MessageBeep(0);
    MessageDlg('Timeout must be an integer > -1.', mtError, [mbOk], 0);
  end;
end;

end.
