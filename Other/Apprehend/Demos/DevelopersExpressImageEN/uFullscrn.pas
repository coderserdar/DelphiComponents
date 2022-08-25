//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright (c) 1986-2012 : Adirondack Software & Graphics
//  Last Modification       : 04-06-2012
//  Compiler                : Delphi 2010
//  Description             : FullScreen Unit
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uFullscrn;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, ComCtrls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ieview, imageenview;

type
  TFullScreenForm = class( TForm )
    ImageEnView1: TImageEnView;
    procedure ImageClick( Sender: TObject );
    procedure Image1KeyDown( Sender: TObject; var Key: word; Shift: TShiftState );
    procedure Image1Click( Sender: TObject );
    procedure FormKeyPress( Sender: TObject; var Key: char );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FullScreenForm: TFullScreenForm;

implementation

uses uMain;

{$R *.DFM}

procedure TFullScreenForm.ImageClick( Sender: TObject );
begin
  FullScreenForm.Close;
end;

procedure TFullScreenForm.Image1KeyDown( Sender: TObject; var Key: word; Shift: TShiftState );
begin
  FullScreenForm.Close;
end;

procedure TFullScreenForm.Image1Click( Sender: TObject );
begin
  FullScreenForm.Close;
end;

procedure TFullScreenForm.FormKeyPress( Sender: TObject; var Key: char );
begin
  FullScreenForm.Close;
end;

end.

