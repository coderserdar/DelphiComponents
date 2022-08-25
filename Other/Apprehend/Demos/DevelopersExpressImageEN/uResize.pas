// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 04-06-2012
// Compiler                : Delphi 2010
// Description             : Resize Form Unit
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------
unit uResize;
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
  ComCtrls, StdCtrls, ExtCtrls, Menus, IEView, ImageEnView, ImageEn, ImageEnIO, ImageEnProc,
  HYIEDefs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxMaskEdit, cxSpinEdit, cxGroupBox, cxRadioGroup, cxButtons, cxCheckBox,
  cxLabel, cxDropDownEdit;

type
  TFormResizeResample = class( TForm )
    GroupBox2: TcxGroupBox;
    AspectRatio1: TcxCheckBox;
    Label3: TLabel;
    ComboBoxResampleFilter1: TcxComboBox;
    GroupBox3: TcxGroupBox;
    Label1: TcxLabel;
    Label2: TcxLabel;
    ImageEnProc1: TImageEnProc;
    ImageEnIO1: TImageEnIO;
    Reset1: TcxButton;
    ImageEnView1: TImageEnView;
    Preview1: TcxButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Panel1: TPanel;
    Button1: TcxButton;
    Button2: TcxButton;
    LockPreview1: TcxCheckBox;
    OriginalWidth1: TcxLabel;
    OriginalHeight1: TcxLabel;
    NewHeight1: TcxLabel;
    NewWidth1: TcxLabel;
    EnableAlphaChannel1: TcxCheckBox;
    Background1: TcxComboBox;
    Label4: TcxLabel;
    RadioGroup1: TcxRadioGroup;
    Fit1: TcxButton;
    Zoom1: TcxButton;
    ImageEnView2: TImageEnView;
    Label5: TcxLabel;
    Label6: TcxLabel;
    Width1: TcxSpinEdit;
    Height1: TcxSpinEdit;
    cxLookAndFeelController1: TcxLookAndFeelController;
    procedure FormActivate( Sender: TObject );
    procedure Reset1Click( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure ImageEnView1MouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure Preview1Click( Sender: TObject );
    procedure Background1Change( Sender: TObject );
    procedure Fit1Click( Sender: TObject );
    procedure Zoom1Click( Sender: TObject );
    procedure ComboBox1Change( Sender: TObject );
    procedure cxSpinEdit1PropertiesChange( Sender: TObject );
    procedure Width1KeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure cxSpinEdit2PropertiesChange( Sender: TObject );
    procedure Height1KeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure RadioGroup1PropertiesChange( Sender: TObject );
    procedure EnableAlphaChannel1PropertiesChange( Sender: TObject );
    procedure LockPreview1KeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure LockPreview1PropertiesChange( Sender: TObject );
  private
    { Private declarations }
    DontChange: Boolean;
    OtherChanging: Boolean;
    procedure Preview;
  public
    { Public declarations }
    OrgWidth: Integer;
    OrgHeight: Integer;
    NewWidth: Integer;
    NewHeight: Integer;
    ResampleFilter: TResampleFilter;
    Resize: Boolean;
    Resample: Boolean;
  end;

var
  FormResizeResample: TFormResizeResample;

implementation

uses uMain;
{$R *.DFM}

procedure TFormResizeResample.FormCreate( Sender: TObject );
begin
  DontChange := False;
  OtherChanging := False;
  ImageEnView2.MouseInteract := [ miZoom, miScroll ];
  ImageEnView2.Cursor := 1779;
  ImageEnView2.ScrollBars := ssBoth;
  ImageEnView2.ZoomFilter := rfNone;
  ImageEnView2.BackGround := clWhite;
  ImageEnIO1.AttachedImageEn := ImageEnView2;
  ImageEnProc1.AttachedImageEn := ImageEnView2;
  ImageEnProc1.AttachedBitmap := ImageEnView2.Bitmap;
  ImageEnView1.SetChessboardStyle( 6 );
  ImageEnView2.SetChessboardStyle( 6 );
  ImageEnView1.IO.Params.BMP_HandleTransparency := True;
  ImageEnView2.IO.Params.BMP_HandleTransparency := True;
  ResampleFilter := TResampleFilter( ComboBoxResampleFilter1.ItemIndex );
end;

procedure TFormResizeResample.FormActivate( Sender: TObject );
begin
  if Resample then
    Caption := 'Resample Image (With Stretching)'
  else
    Caption := 'Resize Canvas (With Cropping)';
  OrgWidth := ImageEnView1.IEBitmap.Width;
  OrgHeight := ImageEnView1.IEBitmap.Height;
  ResampleFilter := TResampleFilter( ComboBoxResampleFilter1.ItemIndex );
  Width1.EditValue := OrgWidth;
  Height1.EditValue := OrgHeight;
  OriginalWidth1.Caption := 'Original Width: ' + IntToStr( OrgWidth ) + ' pixels';
  OriginalHeight1.Caption := 'Original Height: ' + IntToStr( OrgHeight ) + ' pixels';
  DontChange := False;
  ComboBoxResampleFilter1.ItemIndex := 0;
  if Width1.CanFocus then
    Width1.SetFocus;
  Width1.SelectAll;
end;

procedure TFormResizeResample.EnableAlphaChannel1PropertiesChange( Sender: TObject );
begin
  ImageEnView1.EnableAlphaChannel := EnableAlphaChannel1.Checked;
  ImageEnView2.EnableAlphaChannel := EnableAlphaChannel1.Checked;
end;

procedure TFormResizeResample.Background1Change( Sender: TObject );
begin
  ImageEnView1.BackgroundStyle := TIEBackgroundStyle( Background1.ItemIndex );
  ImageEnView2.BackgroundStyle := TIEBackgroundStyle( Background1.ItemIndex );
end;

procedure TFormResizeResample.ComboBox1Change( Sender: TObject );
begin
  DontChange := False;
  ImageEnView2.Assign( ImageEnView1 );
  ImageEnView2.Update;
  ResampleFilter := TResampleFilter( ComboBoxResampleFilter1.ItemIndex );
  OrgWidth := ImageEnView1.Bitmap.Width;
  OrgHeight := ImageEnView1.Bitmap.Height;
  OriginalWidth1.Caption := 'Original Width: ' + IntToStr( OrgWidth ) + ' pixels';
  OriginalHeight1.Caption := 'Original Height: ' + IntToStr( OrgHeight ) + ' pixels';
  NewWidth1.Caption := 'New Width:';
  NewHeight1.Caption := 'New Height:';
  Width1.SetFocus;
  ImageEnView2.Zoom := 100;
  Preview;
end;

procedure TFormResizeResample.Width1KeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
begin
  if ( Key = VK_RETURN ) and ( LockPreview1.EditValue ) then
    Preview;
end;

procedure TFormResizeResample.cxSpinEdit1PropertiesChange( Sender: TObject );
begin
  if AspectRatio1.Checked and not DontChange and not OtherChanging then
  begin
    DontChange := True;
    OrgWidth := ImageEnView1.Bitmap.Width;
    OrgHeight := ImageEnView1.Bitmap.Height;
    Height1.EditValue := ( Round( OrgHeight * Height1.EditValue / OrgWidth ) );
    if ( Width1.EditValue = 8 ) and ( Height1.EditValue = 8 ) then
      RadioGroup1.ItemIndex := 0
    else if ( Width1.EditValue = 12 ) and ( Height1.EditValue = 12 ) then
      RadioGroup1.ItemIndex := 1
    else if ( Width1.EditValue = 16 ) and ( Height1.EditValue = 16 ) then
      RadioGroup1.ItemIndex := 2
    else if ( Width1.EditValue = 24 ) and ( Height1.EditValue = 24 ) then
      RadioGroup1.ItemIndex := 3
    else if ( Width1.EditValue = 28 ) and ( Height1.EditValue = 28 ) then
      RadioGroup1.ItemIndex := 4
    else if ( Width1.EditValue = 32 ) and ( Height1.EditValue = 32 ) then
      RadioGroup1.ItemIndex := 5
    else if ( Width1.EditValue = 48 ) and ( Height1.EditValue = 48 ) then
      RadioGroup1.ItemIndex := 6
    else if ( Width1.EditValue = 64 ) and ( Height1.EditValue = 64 ) then
      RadioGroup1.ItemIndex := 7
    else if ( Width1.EditValue = 72 ) and ( Height1.EditValue = 72 ) then
      RadioGroup1.ItemIndex := 8
    else if ( Width1.EditValue = 128 ) and ( Height1.EditValue = 128 ) then
      RadioGroup1.ItemIndex := 9
    else if ( Width1.EditValue = 160 ) and ( Height1.EditValue = 120 ) then
      RadioGroup1.ItemIndex := 10
    else if ( Width1.EditValue = 320 ) and ( Height1.EditValue = 240 ) then
      RadioGroup1.ItemIndex := 11
    else if ( Width1.EditValue = 640 ) and ( Height1.EditValue = 480 ) then
      RadioGroup1.ItemIndex := 12
    else if ( Width1.EditValue = 1024 ) and ( Height1.EditValue = 768 ) then
      RadioGroup1.ItemIndex := 13
    else if ( Width1.EditValue = 2048 ) and ( Height1.EditValue = 1536 ) then
      RadioGroup1.ItemIndex := 14
    else
      RadioGroup1.ItemIndex := -1;
    if LockPreview1.EditValue then
      Preview;
    DontChange := False;
  end;
end;

procedure TFormResizeResample.Height1KeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
begin
  if ( Key = VK_RETURN ) and ( LockPreview1.EditValue ) then
    Preview;
end;

procedure TFormResizeResample.cxSpinEdit2PropertiesChange( Sender: TObject );
begin
  if AspectRatio1.Checked and not DontChange and not OtherChanging then
  begin
    DontChange := True;
    Width1.EditValue := Round( OrgWidth * Height1.EditValue / OrgHeight );
    if ( Width1.EditValue = 8 ) and ( Height1.EditValue = 8 ) then
      RadioGroup1.ItemIndex := 0
    else if ( Width1.EditValue = 12 ) and ( Height1.EditValue = 12 ) then
      RadioGroup1.ItemIndex := 1
    else if ( Width1.EditValue = 16 ) and ( Height1.EditValue = 16 ) then
      RadioGroup1.ItemIndex := 2
    else if ( Width1.EditValue = 24 ) and ( Height1.EditValue = 24 ) then
      RadioGroup1.ItemIndex := 3
    else if ( Width1.EditValue = 28 ) and ( Height1.EditValue = 28 ) then
      RadioGroup1.ItemIndex := 4
    else if ( Width1.EditValue = 32 ) and ( Height1.EditValue = 32 ) then
      RadioGroup1.ItemIndex := 5
    else if ( Width1.EditValue = 48 ) and ( Height1.EditValue = 48 ) then
      RadioGroup1.ItemIndex := 6
    else if ( Width1.EditValue = 64 ) and ( Height1.EditValue = 64 ) then
      RadioGroup1.ItemIndex := 7
    else if ( Width1.EditValue = 72 ) and ( Height1.EditValue = 72 ) then
      RadioGroup1.ItemIndex := 8
    else if ( Width1.EditValue = 128 ) and ( Height1.EditValue = 128 ) then
      RadioGroup1.ItemIndex := 9
    else if ( Width1.EditValue = 160 ) and ( Height1.EditValue = 120 ) then
      RadioGroup1.ItemIndex := 10
    else if ( Width1.EditValue = 320 ) and ( Height1.EditValue = 240 ) then
      RadioGroup1.ItemIndex := 11
    else if ( Width1.EditValue = 640 ) and ( Height1.EditValue = 480 ) then
      RadioGroup1.ItemIndex := 12
    else if ( Width1.EditValue = 1024 ) and ( Height1.EditValue = 768 ) then
      RadioGroup1.ItemIndex := 13
    else if ( Width1.EditValue = 2048 ) and ( Height1.EditValue = 1536 ) then
      RadioGroup1.ItemIndex := 14
    else
      RadioGroup1.ItemIndex := -1;
    if LockPreview1.EditValue then
      Preview;
    DontChange := False;
  end;
end;

procedure TFormResizeResample.Preview;
var
  ARGB: TRGB;
begin
  if ImageEnProc1.AttachedBitmap <> nil then
  begin
    ImageEnView2.Assign( ImageEnView1 );
    if AspectRatio1.Checked and not DontChange then
    begin
      DontChange := True;
      NewWidth := Width1.EditValue;
      NewHeight := Height1.EditValue;
      if ( NewWidth > 1 ) and ( NewHeight > 1 ) then
      begin
        if Resize then
          ImageEnProc1.ImageResize( NewWidth, NewHeight );
        ResampleFilter := TResampleFilter( ComboBoxResampleFilter1.ItemIndex );
        if Resample then
          ImageEnProc1.Resample( NewWidth, NewHeight, ResampleFilter );
        ImageEnView2.Update;
        NewWidth1.Caption := 'New Width: ' + IntToStr( ImageEnProc1.AttachedBitmap.Width ) + ' pixels';
        NewHeight1.Caption := 'New Height: ' + IntToStr( ImageEnProc1.AttachedBitmap.Height ) + ' pixels';
      end;
      DontChange := False;
    end
    else
    begin
      DontChange := True;
      NewWidth := Width1.EditValue;
      NewHeight := Height1.EditValue;
      if ( NewWidth > 1 ) and ( NewHeight > 1 ) then
      begin
        if Resize then
          ImageEnProc1.ImageResize( NewWidth, NewHeight );
        ResampleFilter := TResampleFilter( ComboBoxResampleFilter1.ItemIndex );
        if Resample then
          ImageEnProc1.Resample( NewWidth, NewHeight, ResampleFilter );
        ImageEnView2.Update;
        NewWidth1.Caption := 'New Width: ' + IntToStr( ImageEnProc1.AttachedBitmap.Width ) + ' pixels';
        NewHeight1.Caption := 'New Height: ' + IntToStr( ImageEnProc1.AttachedBitmap.Height ) + ' pixels';
      end;
      DontChange := False;
    end;
    if ImageEnView2.IO.Params.BitsPerSample * ImageEnView1.IO.Params.SamplesPerPixel = 32 then
      with ImageEnView2, ImageEnView2.Proc do
      begin
        ARGB := IEBitmap.Pixels[ 0, IEBitmap.Height - 1 ];
        SetTransparentColors( ARGB, ARGB, 0 );
      end;
    ImageEnView2.Zoom := 100;
    ImageEnView2.Update;
  end;
end;

procedure TFormResizeResample.RadioGroup1PropertiesChange( Sender: TObject );
begin
  if not DontChange then
  begin
    OtherChanging := True;
    OrgWidth := ImageEnView1.IEBitmap.Width;
    OrgHeight := ImageEnView1.IEBitmap.Height;
    Width1.EditValue := OrgWidth;
    Height1.EditValue := OrgHeight;
    case RadioGroup1.ItemIndex of
      0:
        begin
          Width1.EditValue := 8;
          Height1.EditValue := 8;
          AspectRatio1.Checked := True;
        end;
      1:
        begin
          Width1.EditValue := 12;
          Height1.EditValue := 12;
          AspectRatio1.Checked := True;
        end;
      2:
        begin
          Width1.EditValue := 16;
          Height1.EditValue := 16;
          AspectRatio1.Checked := True;
        end;
      3:
        begin
          Width1.EditValue := 24;
          Height1.EditValue := 24;
          AspectRatio1.Checked := True;
        end;
      4:
        begin
          Width1.EditValue := 28;
          Height1.EditValue := 28;
          AspectRatio1.Checked := True;
        end;
      5:
        begin
          Width1.EditValue := 32;
          Height1.EditValue := 32;
          AspectRatio1.Checked := True;
        end;
      6:
        begin
          Width1.EditValue := 48;
          Height1.EditValue := 48;
          AspectRatio1.Checked := True;
        end;
      7:
        begin
          Width1.EditValue := 64;
          Height1.EditValue := 64;
          AspectRatio1.Checked := True;
        end;
      8:
        begin
          Width1.EditValue := 72;
          Height1.EditValue := 72;
          AspectRatio1.Checked := True;
        end;
      9:
        begin
          Width1.EditValue := 128;
          Height1.EditValue := 128;
          AspectRatio1.Checked := True;
        end;
      10:
        begin
          Width1.EditValue := 160;
          Height1.EditValue := 120;
          AspectRatio1.Checked := False;
        end;
      11:
        begin
          Width1.EditValue := 320;
          Height1.EditValue := 240;
          AspectRatio1.Checked := False;
        end;
      12:
        begin
          Width1.EditValue := 640;
          Height1.EditValue := 480;
          AspectRatio1.Checked := False;
        end;
      13:
        begin
          Width1.EditValue := 1024;
          Height1.EditValue := 768;
          AspectRatio1.Checked := False;
        end;
      14:
        begin
          Width1.EditValue := 2048;
          Height1.EditValue := 1536;
          AspectRatio1.Checked := False;
        end;
    end; // case
    NewWidth := Width1.EditValue;
    NewHeight := Height1.EditValue;
    NewWidth1.Caption := 'New Width: ' + IntToStr( Width1.EditValue ) + ' pixels';
    NewHeight1.Caption := 'New Height: ' + IntToStr( Height1.EditValue ) + ' pixels';
    if LockPreview1.EditValue then
      Preview;
    ImageEnView2.Zoom := 100;
    OtherChanging := False;
  end;
end;

procedure TFormResizeResample.Reset1Click( Sender: TObject );
begin
  DontChange := False;
  ImageEnView2.Assign( ImageEnView1 );
  ImageEnView2.Update;
  ComboBoxResampleFilter1.ItemIndex := 0;
  RadioGroup1.ItemIndex := -1;
  OrgWidth := ImageEnView1.Bitmap.Width;
  OrgHeight := ImageEnView1.Bitmap.Height;
  OriginalWidth1.Caption := 'Original Width: ' + IntToStr( OrgWidth ) + ' pixels';
  OriginalHeight1.Caption := 'Original Height: ' + IntToStr( OrgHeight ) + ' pixels';
  NewWidth1.Caption := 'New Width:';
  NewHeight1.Caption := 'New Height:';
  Width1.SetFocus;
  ImageEnView2.Zoom := 100;
end;

procedure TFormResizeResample.Fit1Click( Sender: TObject );
begin
  ImageEnView2.Fit;
end;

procedure TFormResizeResample.Zoom1Click( Sender: TObject );
begin
  ImageEnView2.Zoom := 100;
end;

procedure TFormResizeResample.FormShow( Sender: TObject );
begin
  // save undo file
  ImageEnProc1.SaveUndo;
  ImageEnView2.Assign( ImageEnView1 );
  ImageEnView2.IO.Params.Assign( ImageEnView1.IO.Params );
  ImageEnView2.Update;
  Preview1.Enabled := not LockPreview1.Checked;
  if ImageEnView1.IO.Params.SamplesPerPixel * ImageEnView1.IO.Params.BitsPerSample = 32 then
  begin
    ImageEnView1.BackgroundStyle := iebsChessboard;
    ImageEnView1.BackGround := clBtnface;
    ImageEnView1.SetChessboardStyle( 4 );
    ImageEnView2.BackgroundStyle := iebsChessboard;
    ImageEnView2.BackGround := clBtnface;
    ImageEnView2.SetChessboardStyle( 4 );
  end
  else
  begin
    ImageEnView1.BackgroundStyle := iebsSolid;
    ImageEnView1.BackGround := clWhite;
    ImageEnView2.BackgroundStyle := iebsSolid;
    ImageEnView2.BackGround := clWhite;
  end;
end;

procedure TFormResizeResample.ImageEnView1MouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if ImageEnView2.MouseInteract = [ miZoom, miScroll ] then
    ImageEnView2.Cursor := 1779
  else
    ImageEnView2.Cursor := 1782;
end;

procedure TFormResizeResample.LockPreview1KeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
begin
  Preview1.Enabled := not LockPreview1.Checked;
end;

procedure TFormResizeResample.LockPreview1PropertiesChange( Sender: TObject );
begin
  Preview1.Enabled := not LockPreview1.EditValue;
end;

procedure TFormResizeResample.Preview1Click( Sender: TObject );
begin
  Preview;
end;

end.
