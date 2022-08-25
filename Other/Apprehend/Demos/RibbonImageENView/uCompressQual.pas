//------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012 : Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Compiler                : Delphi 2010
// Description             : CompressQual Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uCompressQual;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   JPeg, StdCtrls, ExtCtrls, Spin;

type
   TFormQuality = class( TForm )
      GroupBox3: TGroupBox;
      CompressionQuality1: TSpinEdit;
      Button1: TButton;
      Button2: TButton;
      Label1: TLabel;
      Label2: TLabel;
    Panel1: TPanel;
      procedure Button2Click( Sender: TObject );
      procedure Button1Click( Sender: TObject );
      procedure FormCreate( Sender: TObject );
      procedure FormDestroy( Sender: TObject );
   private
    { Private declarations }
   public
    { Public declarations }
      Picture: TPicture;
   end;

var
   FormQuality: TFormQuality;

implementation

{$R *.DFM}

procedure TFormQuality.Button2Click( Sender: TObject );
var
   Temp: Boolean;
begin
   Temp := Picture.Graphic is TJPEGImage;
   if Temp then
      with TJPEGImage( Picture.Graphic ) do
         CompressionQuality := 100;
end;

procedure TFormQuality.Button1Click( Sender: TObject );
var
   Temp: Boolean;
begin
   Temp := Picture.Graphic is TJPEGImage;
   if Temp then
      with TJPEGImage( Picture.Graphic ) do
         CompressionQuality := CompressionQuality1.Value;
end;

procedure TFormQuality.FormCreate( Sender: TObject );
begin
   Picture := TPicture.Create;
end;

procedure TFormQuality.FormDestroy( Sender: TObject );
begin
   Picture.Free;
end;

end.

