//------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Compiler                : Delphi 2010
// Description             : WebSite Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uWebSite;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormWebsite = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Image1: TImage;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
    procedure Label2MouseEnter(Sender: TObject);
    procedure Label2MouseLeave(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label6MouseEnter(Sender: TObject);
    procedure Label6MouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWebsite: TFormWebsite;

implementation

uses ShellAPI;

{$R *.dfm}

procedure TFormWebsite.Label1MouseEnter(Sender: TObject);
begin
   Label1.Font.Color := clBlue;
   Label1.Font.Style := Label1.Font.Style + [fsUnderline];
end;

procedure TFormWebsite.Label1MouseLeave(Sender: TObject);
begin
  Label1.Font.Color := clBlack;
  Label1.Font.Style := Label1.Font.Style - [fsUnderline];
end;

procedure TFormWebsite.Label2MouseEnter(Sender: TObject);
begin
  Label2.Font.Color := clBlue;
  Label2.Font.Style := Label2.Font.Style + [fsUnderline];
end;

procedure TFormWebsite.Label2MouseLeave(Sender: TObject);
begin
   Label2.Font.Color := clBlack;
   Label2.Font.Style := Label2.Font.Style - [fsUnderline];
end;

procedure TFormWebsite.Label1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(Label1.Caption), nil, nil, SW_SHOWNORMAL);
  Label1.Font.Color := clFuchsia;
end;

procedure TFormWebsite.Label2Click(Sender: TObject);
begin
  ShellExecute(0, nil, PChar('mailto:' + Label2.Caption), nil, nil, SW_NORMAL);
  Label2.Font.Color := clFuchsia;
end;

procedure TFormWebsite.Label6Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(Label6.Caption), nil, nil, SW_SHOWNORMAL);
  Label6.Font.Color := clFuchsia;
end;

procedure TFormWebsite.Label6MouseEnter(Sender: TObject);
begin
   Label6.Font.Color := clBlue;
   Label6.Font.Style := Label1.Font.Style + [fsUnderline];
end;

procedure TFormWebsite.Label6MouseLeave(Sender: TObject);
begin
  Label6.Font.Color := clBlack;
  Label6.Font.Style := Label1.Font.Style - [fsUnderline];
end;

end.
