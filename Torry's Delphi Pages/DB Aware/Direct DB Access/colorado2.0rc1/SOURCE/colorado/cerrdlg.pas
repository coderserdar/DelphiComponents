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

unit cerrdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CADODB;

type
  TOLEDBProviderErrorsForm = class(TForm)
    BOk: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    EdSource: TEdit;
    EdDesc: TMemo;
    Label3: TLabel;
    EdErrNo: TEdit;
    Label4: TLabel;
    EdNatErr: TEdit;
    Label5: TLabel;
    EdSqlState: TEdit;
    BPrev: TButton;
    BNext: TButton;
    Bevel2: TBevel;
    BHelp: TButton;
    procedure BNextClick(Sender: TObject);
    procedure BPrevClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
  private
    { Private declarations }
    FErrors: IADOErrors;
    ErrorIndex: Integer;
    procedure ShowError;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; AErrors: IADOErrors); reintroduce; 
  end;

var
  OLEDBProviderErrorsForm: TOLEDBProviderErrorsForm;

implementation

{$R *.DFM}

{ TOLEDBProviderErrorsForm }

constructor TOLEDBProviderErrorsForm.Create(AOwner: TComponent;
  AErrors: IADOErrors);
begin
  inherited Create(AOwner);
  FErrors := AErrors;
  ErrorIndex := 0;
  if FErrors.Count > 1 then BNext.Enabled := TRUE;
  ShowError;
end;

procedure TOLEDBProviderErrorsForm.ShowError;
begin
  if FErrors.Count >= ErrorIndex + 1 then
     begin
       EdSource.Text := FErrors [ErrorIndex].Source;
       EdDesc.Text := FErrors [ErrorIndex].Description;
       EdErrNo.Text := IntToStr(FErrors [ErrorIndex].Number);
       EdNatErr.Text := IntToStr(FErrors [ErrorIndex].NativeError);
       EdSqlState.Text := FErrors [ErrorIndex].SqlState;
       if FErrors [ErrorIndex].HelpFile <> '' then BHelp.Enabled := TRUE else BHelp.Enabled := FALSE;
     end else
           begin
             EdSource.Text := '';
             EdDesc.Text := '';
             EdErrNo.Text := '';
             EdNatErr.Text := '';
             EdSqlState.Text := '';
             BHelp.Enabled := FALSE;
           end;
end;

procedure TOLEDBProviderErrorsForm.BNextClick(Sender: TObject);
begin
  if FErrors.Count < ErrorIndex + 2
     then ErrorIndex := FErrors.Count - 1
     else inc(ErrorIndex);
  if ErrorIndex < 0 then ErrorIndex := 0;
  if FErrors.Count < ErrorIndex + 2 then BNext.Enabled := FALSE;
  ShowError;
  if ErrorIndex > 0 then BPrev.Enabled := TRUE;
end;

procedure TOLEDBProviderErrorsForm.BPrevClick(Sender: TObject);
begin
  if FErrors.Count < ErrorIndex
     then ErrorIndex := FErrors.Count - 1
     else dec(ErrorIndex);
  if ErrorIndex < 0 then ErrorIndex := 0;
  if ErrorIndex = 0 then BPrev.Enabled := FALSE;
  ShowError;
  if FErrors.Count >= ErrorIndex + 2 then BNext.Enabled := TRUE;
end;

procedure TOLEDBProviderErrorsForm.BHelpClick(Sender: TObject);
begin
  if FErrors.Count >= ErrorIndex + 1 then
     WinHelp(Handle, PChar(FErrors[ErrorIndex].HelpFile), HELP_CONTEXT, FErrors[ErrorIndex].HelpContext);
end;

end.
