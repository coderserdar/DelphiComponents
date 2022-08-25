//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012   : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : Rotate Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uRotate;

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   StdCtrls, ComCtrls, ImageEn, ExtCtrls, ImageEnView, ImageEnProc, ieview;

type
   TFormRotate = class( TForm )
      GroupBox1: TGroupBox;
      Button1: TButton;
      Button2: TButton;
      Label1: TLabel;
      Edit1: TEdit;
      UpDown1: TUpDown;
      CheckBox1: TCheckBox;
      ImageEnView1: TImageEnView;
      ImageEnProc1: TImageEnProc;
    Panel1: TPanel;
      procedure FormActivate( Sender: TObject );
      procedure Edit1Change( Sender: TObject );
   private
    { Private declarations }
   public
    { Public declarations }
   end;

var
   FormRotate: TFormRotate;

implementation

{$R *.DFM}

procedure TFormRotate.FormActivate( Sender: TObject );
begin
   Updown1.Position := 0;
   Checkbox1.Checked := false;
   ImageEnProc1.SaveUndo;
end;

// edit-change
procedure TFormRotate.Edit1Change( Sender: TObject );
begin
   ImageEnProc1.Undo;
   ImageEnProc1.rotate( strtointdef( edit1.text, 0 ), checkbox1.checked );
   ImageEnView1.Fit;
end;

end.

