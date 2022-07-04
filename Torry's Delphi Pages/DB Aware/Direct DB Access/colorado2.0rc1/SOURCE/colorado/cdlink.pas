{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

The Original Code is colorADO Database Components.

The Initial Developer of the Original Code is Maciej Kujalowicz.
Portions created by Maciej Kujalowicz are Copyright (C) 2000-2003
Maciej Kujalowicz. All Rights Reserved.}

unit cdlink;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ActiveX;

type
  TCDataLink = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CONNECTIONSTRING: TCheckBox;
    CONNECTIONSTRINGEDIT: TEdit;
    FILENAME: TCheckBox;
    FILENAMEEDIT: TEdit;
    DSN: TCheckBox;
    CONNECTIONSTRINGBUTTON: TButton;
    FILENAMEBUTTON: TButton;
    DSNTYPE: TRadioGroup;
    DSNCOMBO: TComboBox;
    procedure FILENAMEClick(Sender: TObject);
    procedure CONNECTIONSTRINGClick(Sender: TObject);
    procedure DSNClick(Sender: TObject);
    procedure DSNTYPEClick(Sender: TObject);
    procedure CONNECTIONSTRINGBUTTONClick(Sender: TObject);
    procedure FILENAMEBUTTONClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure AddDSNsToCombo;
  public
    { Public declarations }
  end;

var
  CDataLink: TCDataLink;

implementation

{$R *.DFM}
uses OLEDBADM, cMSDASC, cADODB;

procedure TCDataLink.FILENAMEClick(Sender: TObject);
begin
  if (not CONNECTIONSTRING.Checked)
    and
     (not FILENAME.Checked)
    and
     (not DSN.Checked)
    then FILENAME.Checked := TRUE;
  if not FILENAME.Checked then
         begin
           FILENAMEEDIT.Text := '';
           FILENAMEEDIT.Enabled := FALSE;
           FILENAMEEDIT.Color := clBtnFace;
           FILENAMEBUTTON.Enabled := FALSE;
         end
           else
             begin
               FILENAMEEDIT.Enabled := TRUE;
               FILENAMEEDIT.Color := clWindow;
               FILENAMEBUTTON.Enabled := TRUE;
               CONNECTIONSTRING.Checked := FALSE;
               DSN.Checked := FALSE;
             end;
end;

procedure TCDataLink.CONNECTIONSTRINGClick(Sender: TObject);
begin
  if (not CONNECTIONSTRING.Checked)
    and
     (not FILENAME.Checked)
    and
     (not DSN.Checked)
    then CONNECTIONSTRING.Checked := TRUE;
  if not CONNECTIONSTRING.Checked then
         begin
           CONNECTIONSTRINGEDIT.Text := '';
           CONNECTIONSTRINGEDIT.Enabled := FALSE;
           CONNECTIONSTRINGEDIT.Color := clBtnFace;
           CONNECTIONSTRINGBUTTON.Enabled := FALSE;
         end
           else
             begin
               CONNECTIONSTRINGEDIT.Enabled := TRUE;
               CONNECTIONSTRINGEDIT.Color := clWindow;
               CONNECTIONSTRINGBUTTON.Enabled := TRUE;
               FILENAME.Checked := FALSE;
               DSN.Checked := FALSE;
             end;
end;

procedure TCDataLink.DSNClick(Sender: TObject);
begin
  if (not CONNECTIONSTRING.Checked)
    and
     (not FILENAME.Checked)
    and
     (not DSN.Checked)
    then DSN.Checked := TRUE;
  if not DSN.Checked then
         begin
           DSNCOMBO.Text := '';
           DSNCOMBO.Enabled := FALSE;
           DSNCOMBO.Color := clBtnFace;
           DSNTYPE.Enabled := FALSE;
         end
           else
             begin
               DSNCOMBO.Enabled := TRUE;
               DSNCOMBO.Color := clWindow;
               DSNTYPE.Enabled := TRUE;
               CONNECTIONSTRING.Checked := FALSE;
               FILENAME.Checked := FALSE;
               AddDSNsToCombo;
             end;
end;

procedure TCDataLink.AddDSNsToCombo;
var i           : Integer;
    FOLEDBAdmin : IOLEDBAdmin;
    FDSN        : WideString;
    FDSNCaption : string;
begin
  FOLEDBAdmin := nil;
  FDSNCaption := DSN.Caption;
  DSN.Caption := 'Please wait ...';
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  try
    DSNCOMBO.Items.Clear;
    FOLEDBAdmin := coOLEDBAdmin.Create;
    case DSNType.ItemIndex of
      0: FOLEDBAdmin.Open($345);
      1: FOLEDBAdmin.Open($346);
    end;
    for i := 0 to FOLEDBAdmin.GetDSNCount - 1 do
        begin
          FOLEDBAdmin.GetDSN(i, FDSN);
          DSNCOMBO.Items.Add(FDSN);
        end;

  finally
    FOLEDBAdmin := nil;
    Screen.Cursor := crArrow;
    DSN.Caption := FDSNCaption;
  end;
end;

procedure TCDataLink.DSNTYPEClick(Sender: TObject);
begin
  AddDSNsToCombo;
end;

procedure TCDataLink.CONNECTIONSTRINGBUTTONClick(Sender: TObject);
var FDataSourceLocator : IDataSourceLocator;
    FConnection: IADOConnection;
begin
  FDataSourceLocator := CoDataLinks.Create;
  FDataSourceLocator.hWnd := Application.Handle;
  if CONNECTIONSTRINGEDIT.Text = '' then
     begin
       FConnection := IADOConnection(FDataSourceLocator.PromptNew);
       if FConnection = nil then Exit;
       CONNECTIONSTRINGEDIT.Text := FConnection.ConnectionString;
       FConnection := nil;
     end else
           begin
             FConnection := CoConnection.Create;
             FConnection.ConnectionString := CONNECTIONSTRINGEDIT.Text;
             try
               if FDataSourceLocator.PromptEdit(IDispatch(FConnection)) then
                  begin
                    CONNECTIONSTRINGEDIT.Text := FConnection.ConnectionString;
                  end;
             finally
               FConnection := nil;
             end;
           end;
end;

procedure TCDataLink.FILENAMEBUTTONClick(Sender: TObject);
var FDataInitialize    : IDataInitialize;
    FDBPrompt          : IDBPromptInitialize;
    hr                 : HRESULT;
    WStr               : PWideChar;
begin
  hr := CoCreateInstance(CLASS_DataLinks, nil, CLSCTX_INPROC_SERVER, IID_IDataInitialize, FDataInitialize);
  if (hr <> S_OK) then Exit;
  FDBPrompt := nil;
  hr := FDataInitialize.QueryInterface(
         IID_IDBPromptInitialize,
         FDBPrompt);
  if (hr <> S_OK) then
     begin
       FDataInitialize := nil;
       Exit;
     end;
  if FDBPrompt.PromptFileName( Application.Handle, 0, nil, '*.udl', WStr) = S_OK then
     begin
       FILENAMEEDIT.Text := WideCharToString(WStr);
       CoTaskMemFree(WStr);
     end;
  FDBPrompt := nil;
  FDataInitialize := nil;
end;

procedure TCDataLink.FormShow(Sender: TObject);
var Style: LongInt;
begin
  Style := GetWindowLong(DSN.Handle, GWL_STYLE);
  Style := Style  or BS_FLAT;
  SetWindowLong(CONNECTIONSTRING.Handle, GWL_STYLE, Style);
  SetWindowLong(FILENAME.Handle, GWL_STYLE, Style);
  SetWindowLong(DSN.Handle, GWL_STYLE, Style);
end;

end.
