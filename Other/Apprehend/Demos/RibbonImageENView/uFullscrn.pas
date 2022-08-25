//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012 : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : Fullscrn Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uFullscrn;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Comctrls,
   Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ImageEnView, ImageEn, ieview;

type
   TFormFullScreen = class( TForm )
    ImageEnView1: TImageEnView;
      procedure ImageClick( Sender: TObject );
      procedure Image1KeyDown( Sender: TObject;var Key: Word;Shift: TShiftState );
      procedure FormKeyPress( Sender: TObject;var Key: Char );
    procedure FormCreate(Sender: TObject);
   private
    { Private declarations }
   public
    { Public declarations }
   end;

var
   FormFullScreen: TFormFullScreen;

implementation

{$R *.DFM}

procedure TFormFullScreen.ImageClick( Sender: TObject );
begin
   Close;
end;

procedure TFormFullScreen.Image1KeyDown( Sender: TObject;var Key: Word;Shift: TShiftState );
begin
   Close;
end;

procedure TFormFullScreen.FormKeyPress( Sender: TObject;var Key: Char );
begin
   Close;
end;

procedure TFormFullScreen.FormCreate(Sender: TObject);
begin
   ImageEnView1.SetChessboardStyle(6, bsSolid);
end;

end.

