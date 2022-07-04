{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: AboutTranslator.pas,v 1.5 2001/12/04 13:24:03 laa Exp $ }

unit AboutTranslator;
{$i common.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TdlgAboutTranslator = class(TForm)
    pnlTop: TPanel;
    GroupBox: TGroupBox;
    btnOk: TBitBtn;
    Label1: TLabel;
    lblPolycon: TLabel;
    Label3: TLabel;
    lblLeif: TLabel;
    Label5: TLabel;
    lblTranslator: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    lblTranslatorHome: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnLabelClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Show(const Version : String);
  end;

var
  dlgAboutTranslator: TdlgAboutTranslator;

implementation

{$R *.DFM}

uses ShellApi;

{ TdlgAboutTranslator }

procedure TdlgAboutTranslator.Show(const Version: String);
begin
  Self.Caption := 'TTranslator v' + Version;
  ShowModal;
end;

procedure TdlgAboutTranslator.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TdlgAboutTranslator.OnLabelClick(Sender: TObject);
var
  zOperation, zFileName : array[0..79] of Char;
begin
  ShellExecute(Self.Handle,
               StrPCopy(zOperation, 'open'),
               StrPCopy(zFileName, (Sender as TLabel).Hint),
               nil,
               nil, 0);
end;

end.

