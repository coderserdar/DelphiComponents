// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright (c) 1986-2012 : Copyright Adirondack Software & Graphics
// Last Modification       : 03-24-2012
// Compiler                : Delphi 2010
// Description             : NewImage Unit
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------
unit uNewImage;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxSpinEdit;

type
  TFormNewImage = class( TForm )
    GroupBox1: TGroupBox;
    RadioGroup1: TRadioGroup;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Label1: TLabel;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    Panel1: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox3: TGroupBox;
    Color1: TShape;
    Button1: TButton;
    ColorDialog1: TColorDialog;
    RadioButton8: TRadioButton;
    Label2: TLabel;
    GroupBox4: TGroupBox;
    FileType1: TComboBox;
    Width1: TcxSpinEdit;
    Height1: TcxSpinEdit;
    procedure RadioButton4Click( Sender: TObject );
    procedure RadioButton1Click( Sender: TObject );
    procedure RadioButton2Click( Sender: TObject );
    procedure RadioButton3Click( Sender: TObject );
    procedure OKBtnClick( Sender: TObject );
    procedure RadioButton5Click( Sender: TObject );
    procedure RadioButton6Click( Sender: TObject );
    procedure RadioButton7Click( Sender: TObject );
    procedure Button1Click( Sender: TObject );
    procedure RadioGroup1Click( Sender: TObject );
    procedure FileType1Change( Sender: TObject );
    procedure FormCreate( Sender: TObject );
  private
    procedure SetImageDescription;
    procedure SetFileType;
    procedure SetPixelFormat;
    procedure SetColor;
    procedure SetImageDimensions;
    { Private declarations }
  public
    { Public declarations }
    fImageWidth: integer;
    fImageHeight: integer;
    fPixelFormat: TPixelFormat;
    fColor: TColor;
    fFileType: string;
  end;

var
  FormNewImage: TFormNewImage;

implementation

uses StrUtils;
{$R *.dfm}

function ColorToPrettyName( const AColor: TColor ): string;
{ Return a string of a TColor }
var
  cs1: string;
begin
  result := '';
  cs1 := ColorToString( AColor );
  if AnsiContainsText( cs1, 'cl' ) then
    Delete( cs1, 1, 2 );
  result := cs1;
end;

procedure TFormNewImage.RadioButton1Click( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.RadioButton2Click( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.RadioButton3Click( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.RadioButton4Click( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.RadioButton5Click( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.RadioButton6Click( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.RadioButton7Click( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.RadioGroup1Click( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.Button1Click( Sender: TObject );
begin
  ColorDialog1.Color := Color1.Brush.Color;
  if ColorDialog1.Execute then
  begin
    Color1.Brush.Color := ColorDialog1.Color;
    SetFileType;
    SetPixelFormat;
    SetColor;
    SetImageDimensions;
    SetImageDescription;
  end;
end;

procedure TFormNewImage.FileType1Change( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

procedure TFormNewImage.FormCreate( Sender: TObject );
begin
  fImageWidth := 16;
  fImageHeight := 16;
  fPixelFormat := pf24bit;
  fColor := clWhite;
  fFileType := 'png';
end;

procedure TformNewImage.SetPixelFormat;
begin
  case RadioGroup1.ItemIndex of
    0:
      fPixelFormat := pf32bit;
    1:
      fPixelFormat := pf24bit;
    2:
      fPixelFormat := pf8bit;
    3:
      fPixelFormat := pf4bit;
    4:
      fPixelFormat := pf4bit;
  end;
end;

procedure TFormNewImage.SetColor;
begin
  fColor := Color1.Brush.Color;
end;

procedure TFormNewImage.SetImageDimensions;
begin
  if RadioButton1.Checked then
  begin
    fImageWidth := 16;
    fImageHeight := 16;
    Width1.Enabled := false;
    Height1.Enabled := false;
  end
  else if RadioButton2.Checked then
  begin
    fImageWidth := 32;
    fImageHeight := 32;
    Width1.Enabled := false;
    Height1.Enabled := false;
  end
  else if RadioButton3.Checked then
  begin
    fImageWidth := 48;
    fImageHeight := 48;
    Width1.Enabled := false;
    Height1.Enabled := false;
  end
  else if RadioButton4.Checked then
  begin
    fImageWidth := 64;
    fImageHeight := 64;
    Width1.Enabled := true;
    Height1.Enabled := true;
  end
  else if RadioButton5.Checked then
  begin
    fImageWidth := 72;
    fImageHeight := 72;
    Width1.Enabled := false;
    Height1.Enabled := false;
  end
  else if RadioButton6.Checked then
  begin
    fImageWidth := 128;
    fImageHeight := 128;
    Width1.Enabled := false;
    Height1.Enabled := false;
  end
  else if RadioButton7.Checked then
  begin
    fImageWidth := Width1.EditingValue;
    fImageHeight := Height1.EditingValue;
    Width1.Enabled := true;
    Height1.Enabled := true;
  end
  else if RadioButton8.Checked then
  begin
    fImageWidth := Screen.Width;
    fImageHeight := Screen.Height;
    Width1.Enabled := true;
    Height1.Enabled := true;
  end;
end;

procedure TFormNewImage.SetImageDescription;
begin
  case RadioGroup1.ItemIndex of
    0:
      begin
        Label2.Caption := '24-bit with alpha channel ' + Uppercase( fFileType ) + ' ' + IntToStr( fImageWidth ) + ' x ' + IntToStr( fImageHeight )
          + ' pixels ' + ColorToPrettyName( fColor );
        Label2.Invalidate;
      end;
    1:
      begin
        Label2.Caption := '24-bit (16 million colors) ' + Uppercase( fFileType ) + ' ' + IntToStr( fImageWidth ) + ' x ' + IntToStr( fImageHeight )
          + ' pixels ' + ColorToPrettyName( fColor );
        Label2.Invalidate;
      end;
    2:
      begin
        Label2.Caption := '8-bit (256 colors) ' + Uppercase( fFileType ) + ' ' + IntToStr( fImageWidth ) + ' x ' + IntToStr( fImageHeight )
          + ' pixels ' + ColorToPrettyName( fColor );
        Label2.Invalidate;
      end;
    3:
      begin
        Label2.Caption := '4-bit (16 colors) ' + Uppercase( fFileType ) + ' ' + IntToStr( fImageWidth ) + ' x ' + IntToStr( fImageHeight )
          + ' pixels ' + ColorToPrettyName( fColor );
        Label2.Invalidate;
      end;
    4:
      begin
        Label2.Caption := '1-bit (Monochrome) ' + Uppercase( fFileType ) + ' ' + IntToStr( fImageWidth ) + ' x ' + IntToStr( fImageHeight )
          + ' pixels ' + ColorToPrettyName( fColor );
        Label2.Invalidate;
      end;
  end;
end;

procedure TFormNewImage.SetFileType;
begin
  case FileType1.ItemIndex of
    0:
      fFileType := 'gif';
    1:
      fFileType := 'jpg';
    2:
      fFileType := 'bmp';
    3:
      fFileType := 'ico';
    4:
      fFileType := 'png';
    5:
      fFileType := 'wmf';
    6:
      fFileType := 'emf';
  end;
end;

procedure TFormNewImage.OKBtnClick( Sender: TObject );
begin
  SetFileType;
  SetPixelFormat;
  SetColor;
  SetImageDimensions;
  SetImageDescription;
end;

end.
