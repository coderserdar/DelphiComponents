// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 03-23-2012
// Description             : uWebSite
// Compiler                : Delphi 2010
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------

unit uWebSite;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TWebsiteForm = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Image1: TImage;
    Label5: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    procedure Label2MouseEnter(Sender: TObject);
    procedure Label2MouseLeave(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label5MouseEnter(Sender: TObject);
    procedure Label5MouseLeave(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebsiteForm: TWebsiteForm;

implementation

uses ShellAPI;

{$R *.dfm}

procedure TWebsiteForm.Label2MouseEnter(Sender: TObject);
begin
  Label2.Font.Color := clRed;
  Label2.Font.Style := Label2.Font.Style + [fsUnderline];
end;

procedure TWebsiteForm.Label2MouseLeave(Sender: TObject);
begin
   Label2.Font.Color := clBlack;
   Label2.Font.Style := Label2.Font.Style - [fsUnderline];
end;

procedure TWebsiteForm.Label2Click(Sender: TObject);
begin
  ShellExecute(0, nil, PChar('mailto:' + Label2.Caption), nil, nil, SW_NORMAL);
  Label2.Font.Color := clFuchsia;
end;

procedure TWebsiteForm.Label5Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(Label5.Caption), nil, nil, SW_SHOWNORMAL);
  Label5.Font.Color := clFuchsia;
end;

procedure TWebsiteForm.Label5MouseEnter(Sender: TObject);
begin
   Label5.Font.Color := clBlue;
   Label5.Font.Style := Label5.Font.Style + [fsUnderline];
end;

procedure TWebsiteForm.Label5MouseLeave(Sender: TObject);
begin
  Label5.Font.Color := clBlack;
  Label5.Font.Style := Label5.Font.Style - [fsUnderline];
end;

procedure TWebsiteForm.Label1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(Label1.Caption), nil, nil, SW_SHOWNORMAL);
  Label1.Font.Color := clFuchsia;
end;

procedure TWebsiteForm.Label1MouseEnter(Sender: TObject);
begin
   Label1.Font.Color := clBlue;
   Label1.Font.Style := Label1.Font.Style + [fsUnderline];
end;

procedure TWebsiteForm.Label1MouseLeave(Sender: TObject);
begin
  Label1.Font.Color := clBlack;
  Label1.Font.Style := Label1.Font.Style - [fsUnderline];
end;

end.
