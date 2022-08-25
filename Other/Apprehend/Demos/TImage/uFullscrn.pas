// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 03-23-2012
// Description             : uFullScrn
// Compiler                : Delphi 2010
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------

unit uFullscrn;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Comctrls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TFullScreen = class ( TForm )
    ScrollBox1: TScrollBox;
    Image1: TImage;
    procedure ImageClick ( Sender: TObject );
    procedure Image1KeyDown ( Sender: TObject;var Key: Word;Shift: TShiftState );
    procedure Image1Click ( Sender: TObject );
    procedure FormKeyPress ( Sender: TObject;var Key: Char );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FullScreen: TFullScreen;

implementation

uses uMain;

{$R *.DFM}

procedure TFullScreen.ImageClick ( Sender: TObject );
begin
  FullScreen.Close;
end;

procedure TFullScreen.Image1KeyDown ( Sender: TObject;var Key: Word;Shift: TShiftState );
begin
  FullScreen.Close;
end;

procedure TFullScreen.Image1Click ( Sender: TObject );
begin
  FullScreen.Close;
end;

procedure TFullScreen.FormKeyPress ( Sender: TObject;var Key: Char );
begin
  FullScreen.Close;
end;

end.

