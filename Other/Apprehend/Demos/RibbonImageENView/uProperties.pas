//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012   : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : Properties Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uProperties;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
  ExtCtrls, ImageEnView, ImageEn, ImageEnIO, IEView;
type
  TFormProperties = class( TForm )
    OKBtn: TButton;
    Bevel1: TBevel;
    txtPath: TLabel;
    txtFilename: TLabel;
    txtSize: TLabel;
    txtColors: TLabel;
    txtmem: TLabel;
    txtFileType: TLabel;
    txtDPI: TLabel;
    txtDPIY: TLabel;
    txtColorMapCount: TLabel;
    ImageEnView1: TImageEnView;
    Panel1: TPanel;
    procedure FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure FormCreate( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormProperties: TFormProperties;

implementation

{$R *.DFM}

procedure TFormProperties.FormCreate( Sender: TObject );
begin
  // permit trapping of escape key
  KeyPreview := True;
end;

procedure TFormProperties.FormKeyDown( Sender: TObject; var Key: Word;
  Shift: TShiftState );
begin
  if ( Key = vk_Escape ) then
    Close;
end;

end.

