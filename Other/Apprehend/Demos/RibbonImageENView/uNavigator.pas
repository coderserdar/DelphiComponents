// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Compiler                : Delphi 2010
// Description             : Navigator Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------

unit uNavigator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ieview, imageenview, StdCtrls, ExtCtrls;

type
  TFormNavigator = class( TForm )
    ImageEnViewNavigator: TImageEnView;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure Button1Click( Sender: TObject );
    procedure Button2Click( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure FormHide( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNavigator: TFormNavigator;

implementation

uses uMain;
{$R *.dfm}

procedure TFormNavigator.FormCreate( Sender: TObject );
begin
  if ( not ( FormMain.WindowState = wsMaximized ) ) and ( FormMain.Left > Width + 10 ) then
  begin
    Left := FormMain.Left - Width - 10;
    Top := FormMain.Top + 5;
  end
  else
  begin
    Left := FormMain.Left + 16;
    Top := FormMain.Top + 200;
  end;
end;

procedure TFormNavigator.FormDestroy( Sender: TObject );
begin
  FormMain.ActionNavigator1.Checked := False;
end;

procedure TFormNavigator.FormHide( Sender: TObject );
begin
  FormMain.ActionNavigator1.Checked := False;
end;

procedure TFormNavigator.FormShow( Sender: TObject );
begin
  Button1.Hint := FloatToStr( TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom ) + '%';
  Button2.Hint := FloatToStr( TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom ) + '%';
  Caption := 'Navigator- ' + FloatToStr( TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom ) + '%';
end;

procedure TFormNavigator.Button1Click( Sender: TObject );
begin
  TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom := TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom + 10;
  Caption := 'Navigator- ' + FloatToStr( TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom ) + '%';
  Button1.Hint := FloatToStr( TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom ) + '%';
end;

procedure TFormNavigator.Button2Click( Sender: TObject );
begin
  TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom := TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom - 10;
  Caption := 'Navigator- ' + FloatToStr( TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom ) + '%';
  Button1.Hint := FloatToStr( TImageEnView( FormMain.PageControl1.ActivePage.Controls[ 0 ] ).Zoom ) + '%';
end;

end.
