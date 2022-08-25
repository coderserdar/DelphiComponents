//------------------------------------------------------------------------------
//  DeveloperExpress Glyph Editor : 1.0
//  Copyright (c) 2008            : Copyright Adirondack Software & Graphics
//  Created                       : 12-09-2007
//  Last Modification             : 04-01-2008
//  Description                   : BrightnessContrastSaturation Unit
//------------------------------------------------------------------------------

unit uBrightnessContrastSaturation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvTrackBar, ieview, imageenview;

type
  TFormBrightnessContrastSaturation = class( TForm )
    Label1: TLabel;
    ImageEnView1: TImageEnView;
    Label2: TLabel;
    ImageEnView2: TImageEnView;
    Ok1: TButton;
    Cancel1: TButton;
    ResetAll1: TButton;
    Label3: TLabel;
    Edit1: TEdit;
    Label4: TLabel;
    Edit2: TEdit;
    Label5: TLabel;
    Edit3: TEdit;
    AdvTrackBar1: TAdvTrackBar;
    AdvTrackBar2: TAdvTrackBar;
    AdvTrackBar3: TAdvTrackBar;
    ApplyPreviewToMainImage1: TCheckBox;
    Reset1: TButton;
    Reset2: TButton;
    Reset3: TButton;
    Apply1: TButton;
    LockPreview1: TCheckBox;
    procedure AdvTrackBar1Change( Sender: TObject );
    procedure AdvTrackBar2Change( Sender: TObject );
    procedure AdvTrackBar3Change( Sender: TObject );
    procedure Reset1Click( Sender: TObject );
    procedure Reset2Click( Sender: TObject );
    procedure Reset3Click( Sender: TObject );
    procedure Edit1Change( Sender: TObject );
    procedure Edit2Change( Sender: TObject );
    procedure Edit3Change( Sender: TObject );
    procedure ResetAll1Click( Sender: TObject );
    procedure Apply1Click( Sender: TObject );
    procedure LockPreview1Click( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure ApplyPreviewToMainImage1Click( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure ImageEnView2FinishWork(Sender: TObject);
    procedure ImageEnView2Progress(Sender: TObject; per: Integer);
  private
    { Private declarations }
    Starting: boolean;
    Resetting: boolean;
    ImageENView: TImageENView;
  public
    { Public declarations }
    Brightness: integer;
    Contrast: integer;
    Saturation: integer;
  end;

var
  FormBrightnessContrastSaturation: TFormBrightnessContrastSaturation;

implementation

uses Unit1;

{$R *.dfm}

procedure TFormBrightnessContrastSaturation.AdvTrackBar1Change( Sender: TObject
  );
begin
  Edit1.Text := IntToStr( AdvTrackBar1.Position );
  if ( not Resetting ) and ( LockPreview1.Checked ) then
  begin
    Screen.Cursor := crHourglass;
    try
      ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
      Brightness := AdvTrackBar1.Position;
      Contrast := AdvTrackBar2.Position;
      Saturation := AdvTrackBar3.Position;
      ImageEnView2.Proc.IntensityRGBall(Brightness,Brightness,Brightness);
      ImageENView2.Update;
      if ApplyPreviewToMainImage1.Checked then
      begin
        if Assigned( Form1.PageControl1.ActivePage ) then
        begin
          ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
            ] );
          ImageENView.IEBitmap.Assign( ImageENView2.IEBitmap );
          ImageENView.Update;
        end;
      end;
    finally; Screen.Cursor := crDefault; end;
  end;
end;

procedure TFormBrightnessContrastSaturation.AdvTrackBar2Change( Sender: TObject
  );
begin
  Edit2.Text := IntToStr( AdvTrackBar2.Position );
  if ( not Resetting ) and ( LockPreview1.Checked ) then
  begin
    Screen.Cursor := crHourglass;
    try
      ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
      Brightness := AdvTrackBar1.Position;
      Contrast := AdvTrackBar2.Position;
      Saturation := AdvTrackBar3.Position;
      ImageEnView2.Proc.Contrast3 ( Contrast, 10, True, True, True);
      ImageENView2.Update;
      if ApplyPreviewToMainImage1.Checked then
      begin
        if Assigned( Form1.PageControl1.ActivePage ) then
        begin
          ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
            ] );
          ImageENView.IEBitmap.Assign( ImageENView2.IEBitmap );
          ImageENView.Update;
        end;
      end;
    finally; Screen.Cursor := crDefault; end;
  end;
end;

procedure TFormBrightnessContrastSaturation.AdvTrackBar3Change( Sender: TObject
  );
begin
  Edit3.Text := IntToStr( AdvTrackBar3.Position );
  if ( not Resetting ) and ( LockPreview1.Checked ) then
  begin
    Screen.Cursor := crHourglass;
    try
      ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
      Brightness := AdvTrackBar1.Position;
      Contrast := AdvTrackBar2.Position;
      Saturation := AdvTrackBar3.Position;
      ImageENView2.Proc.AdjustBrightnessContrastSaturation( 0, 0, Saturation );
      ImageENView2.Update;
      if ApplyPreviewToMainImage1.Checked then
      begin
        if Assigned( Form1.PageControl1.ActivePage ) then
        begin
          ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
            ] );
          ImageENView.IEBitmap.Assign( ImageENView2.IEBitmap );
          ImageENView.Update;
        end;
      end;
    finally; Screen.Cursor := crDefault; end;
  end;
end;

procedure TFormBrightnessContrastSaturation.Apply1Click( Sender: TObject );
begin
  Screen.Cursor := crHourglass;
  try
    ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
    Brightness := AdvTrackBar1.Position;
    Contrast := AdvTrackBar2.Position;
    Saturation := AdvTrackBar3.Position;
    ImageENView2.Proc.AdjustBrightnessContrastSaturation( Brightness, 0, 0 );
    ImageENView2.Update;
    if ApplyPreviewToMainImage1.Checked then
    begin
      if Assigned( Form1.PageControl1.ActivePage ) then
      begin
        ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
          ] );
          ImageEnView.Proc.SaveUndo;
        ImageENView.IEBitmap.Assign( ImageENView2.IEBitmap );
        ImageENView.Update;
      end;
    end;
  finally; Screen.Cursor := crDefault; end;
end;

procedure TFormBrightnessContrastSaturation.ApplyPreviewToMainImage1Click(
  Sender: TObject );
begin
  if (LockPreview1.Checked) and not ( Starting ) then
  begin
    Screen.Cursor := crHourglass;
    try
      ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
      Brightness := AdvTrackBar1.Position;
      Contrast := AdvTrackBar2.Position;
      Saturation := AdvTrackBar3.Position;
      ImageENView2.Proc.AdjustBrightnessContrastSaturation( Brightness, 0, 0 );
      ImageENView2.Update;
      if ApplyPreviewToMainImage1.Checked then
      begin
        if Assigned( Form1.PageControl1.ActivePage ) then
        begin
          ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
            ] );
            ImageEnView.Proc.SaveUndo;
          ImageENView.IEBitmap.Assign( ImageENView2.IEBitmap );
          ImageENView.Update;
        end;
      end;
    finally; Screen.Cursor := crDefault; end;
  end;
  if ( not ApplyPreviewToMainImage1.Checked ) and not ( Starting ) then
  begin
    ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
      ] );
    ImageEnView.Proc.SaveUndo;
    ImageEnView.IEBitmap.Assign( ImageENView2.IEBitmap );
    ImageEnView.Update;
  end;
end;

procedure TFormBrightnessContrastSaturation.Edit1Change( Sender: TObject );
begin
  AdvTrackBar1.Position := StrToIntDef( Edit1.Text, 0 );
end;

procedure TFormBrightnessContrastSaturation.Edit2Change( Sender: TObject );
begin
  AdvTrackBar2.Position := StrToIntDef( Edit2.Text, 0 );
end;

procedure TFormBrightnessContrastSaturation.Edit3Change( Sender: TObject );
begin
  AdvTrackBar3.Position := StrToIntDef( Edit3.Text, 0 );
end;

procedure TFormBrightnessContrastSaturation.FormCreate( Sender: TObject );
begin
  Resetting := False;
  Starting := True;
end;

procedure TFormBrightnessContrastSaturation.FormShow( Sender: TObject );
begin
  if LockPreview1.Checked then
  begin
    Screen.Cursor := crHourglass;
    try
    Starting := False;
    finally; Screen.Cursor := crDefault; end;
  end;
end;

procedure TFormBrightnessContrastSaturation.ImageEnView2FinishWork(
  Sender: TObject);
begin
  Form1.VistaProBar1.Position := 0;
  Application.ProcessMessages;
end;

procedure TFormBrightnessContrastSaturation.ImageEnView2Progress(
  Sender: TObject; per: Integer);
begin
  Form1.VistaProBar1.Position := per;
end;

procedure TFormBrightnessContrastSaturation.LockPreview1Click( Sender: TObject
  );
begin
  Apply1.Enabled := not LockPreview1.Checked;
  if LockPreview1.Checked then
  begin
    Screen.Cursor := crHourglass;
    try
      ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
      Brightness := AdvTrackBar1.Position;
      Contrast := AdvTrackBar2.Position;
      Saturation := AdvTrackBar3.Position;
      ImageENView2.Proc.AdjustBrightnessContrastSaturation( Brightness, 0, 0 );
      ImageENView2.Update;
      if ApplyPreviewToMainImage1.Checked then
      begin
        if Assigned( Form1.PageControl1.ActivePage ) then
        begin
          ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
            ] );
            ImageEnView.Proc.SaveUndo;
          ImageENView.IEBitmap.Assign( ImageENView2.IEBitmap );
          ImageENView.Update;
        end;
      end;
    finally; Screen.Cursor := crDefault; end;
  end;
end;

procedure TFormBrightnessContrastSaturation.ResetAll1Click( Sender: TObject );
begin
  Resetting := True;
  ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
  ImageENView2.Update;
  AdvTrackBar1.Position := 0;
  AdvTrackBar2.Position := 0;
  AdvTrackBar3.Position := 0;
  if Assigned( Form1.PageControl1.ActivePage ) then
  begin
    ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
      ] );
    ImageENView.IEBitmap.Assign( ImageENView1.IEBitmap );
    ImageEnView.Update;
    ImageEnView.Proc.ClearUndo;
    ImageEnView.Proc.ClearRedo;
    Resetting := False;
  end;
end;

procedure TFormBrightnessContrastSaturation.Reset3Click( Sender: TObject );
begin
  AdvTrackBar3.Position := 0;
  ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
  ImageEnView2.Update;
  if Assigned( Form1.PageControl1.ActivePage ) then
  begin
    ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
      ] );
    ImageENView.IEBitmap.Assign( ImageENView1.IEBitmap );
    ImageEnView.Update;
  end;
end;

procedure TFormBrightnessContrastSaturation.Reset2Click( Sender: TObject );
begin
  AdvTrackBar2.Position := 0;
  ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
  ImageEnView2.Update;
  if Assigned( Form1.PageControl1.ActivePage ) then
  begin
    ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
      ] );
    ImageENView.IEBitmap.Assign( ImageENView1.IEBitmap );
    ImageEnView.Update;
  end;
end;

procedure TFormBrightnessContrastSaturation.Reset1Click( Sender: TObject );
begin
  AdvTrackBar1.Position := 0;
  ImageENView2.IEBitmap.Assign( ImageENView1.IEBitmap );
  ImageEnView2.Update;
  if Assigned( Form1.PageControl1.ActivePage ) then
  begin
    ImageENView := TImageENView( Form1.PageControl1.ActivePage.Controls[ 0
      ] );
    ImageENView.IEBitmap.Assign( ImageENView1.IEBitmap );
    ImageEnView.Update;
  end;
end;

end.

