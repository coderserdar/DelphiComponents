// ------------------------------------------------------------------------------
// Apprehend Version     : 5.1
// Copyright © 1986-2010 : Adirondack Software & Graphics
// Created               : 01-09-1992
// Last Modification     : 08-08-2011
// Compiler              : Delphi 2010
// Description           : Promptdialog Unit
// This file is copyright (C) W W Miller, 1986-2011.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------

unit promptdialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmPosition = class( TForm )
    Label1: TLabel;
    procedure FormShow( Sender: TObject );
    procedure FormKeyDown( Sender: TObject;var Key: Word; Shift: TShiftState );
    procedure FormKeyPress( Sender: TObject;var Key: Char );
  private
    { Private declarations }
  public
    { Public declarations }
    TMx: Integer;
  end;

var
  frmPosition: TfrmPosition;

implementation

{$R *.dfm}

procedure TfrmPosition.FormShow( Sender: TObject );
begin
  TMx := 0;
end;

procedure TfrmPosition.FormKeyDown( Sender: TObject;var Key: Word; Shift: TShiftState );
begin
  if ( Key = VK_Escape ) then
    Hide;
end;

procedure TfrmPosition.FormKeyPress( Sender: TObject;var Key: Char );
begin
  //Hide;
end;

end.


