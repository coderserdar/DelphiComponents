// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright (c) 2012      : Copyright Adirondack Software & Graphics
// Last Modification       : 04-06-2012
// Description             : uRotate Unit
// Compiler                : Delphi 2010
// This file is copyright (C) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------
unit uRotate;
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ImageEn, ImageEnView, ImageEnProc, IEView,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxColorComboBox, cxLabel,
  cxSpinEdit;

type
  TFormRotate = class( TForm )
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ImageEnView1: TImageEnView;
    Preview1: TButton;
    LockPreview1: TCheckBox;
    Undo1: TButton;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Label1: TLabel;
    Reset1: TButton;
    cxColorComboBox1: TcxColorComboBox;
    Angle1: TcxSpinEdit;
    cxLabel1: TcxLabel;
    procedure FormShow( Sender: TObject );
    procedure Preview1Click( Sender: TObject );
    procedure LockPreview1Click( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure Undo1Click( Sender: TObject );
    procedure RadioGroup1Click( Sender: TObject );
    procedure RadioGroup2Click( Sender: TObject );
    procedure Reset1Click( Sender: TObject );
    procedure cxSpinEdit1PropertiesChange( Sender: TObject );
  private
    { Private declarations }
    ImageEnView: TImageEnView;
    procedure UpdateMenu;
  public
    { Public declarations }
    Angle: Integer;
    Antialias: Boolean;
    AntialiasMode: TIEAntialiasMode;
    BackgroundColor: TColor;
  end;

var
  FormRotate: TFormRotate;

implementation

{$R *.DFM}

uses uMain;

// Is a string a number.
function IsStrANumber( const S: string ): Boolean;
var
  P: PChar;
begin
  P := PChar( S );
  Result := False;
  while P^ <> #0 do
  begin
    if not CharInSet( P^, [ '0' .. '9' ] ) then
      Exit;
    Inc( P );
  end;
  Result := True;
end;

procedure TFormRotate.cxSpinEdit1PropertiesChange( Sender: TObject );
begin
  with ImageEnView1.Proc do
  begin
    SaveRedoCaptioned( UndoCaptions[ 0 ], ieuImage ); // saves in Redo list
    Undo;
    ClearUndo;
  end;
  UpdateMenu;
  Antialias := CheckBox1.Checked;
  case RadioGroup2.ItemIndex of
    0:
      AntialiasMode := ierFast;
    1:
      AntialiasMode := ierBilinear;
    2:
      AntialiasMode := ierBicubic;
  end;
  BackgroundColor := cxColorComboBox1.EditValue;
  Angle := Angle1.EditValue;
  if LockPreview1.Checked then
  begin
    //ImageEnView1.Proc.SaveUndoCaptioned( ImageEnView1.Proc.UndoCaptions[ 0 ], ieuImage ); // saves in Undo list
    ImageEnView1.Proc.Rotate( Angle, Antialias, AntialiasMode, BackgroundColor );
    ImageEnView1.Proc.ClearAllRedo;
    ImageEnView1.Update;
    UpdateMenu;
  end;
end;

procedure TFormRotate.FormCreate( Sender: TObject );
begin
  ImageEnView1.Proc.UndoLimit := 6;
  Angle := 0;
  Antialias := True;
  AntialiasMode := ierBicubic;
  BackgroundColor := clWhite;
end;

procedure TFormRotate.FormShow( Sender: TObject );
begin
  ImageEnView1.SetChessboardStyle( 6, bsSolid );
  Preview1.Enabled := not LockPreview1.Checked;
  if LockPreview1.Checked then
  begin
    ImageEnView1.Proc.SaveUndoCaptioned( ImageEnView1.Proc.UndoCaptions[ 0 ], ieuImage ); // saves in Undo list
    ImageEnView1.Proc.Rotate( Angle, Antialias, AntialiasMode, BackgroundColor );
    ImageEnView1.Proc.ClearAllRedo;
    ImageEnView1.Update;
    UpdateMenu;
  end;
end;

procedure TFormRotate.LockPreview1Click( Sender: TObject );
begin
  Preview1.Enabled := not LockPreview1.Checked;
end;

procedure TFormRotate.Preview1Click( Sender: TObject );
begin
  case RadioGroup1.ItemIndex of
    0:
      Angle := 0;
    1:
      Angle := 45;
    2:
      Angle := 90;
    3:
      Angle := 135;
    4:
      Angle := 180;
    5:
      Angle := 225;
    6:
      Angle := 270;
    7:
      Angle := -45;
    8:
      Angle := -90;
    9:
      Angle := -135;
    10:
      Angle := -180;
    11:
      Angle := -225;
    12:
      Angle := -270;
  end;
  Antialias := CheckBox1.Checked;
  case RadioGroup2.ItemIndex of
    0:
      AntialiasMode := ierFast;
    1:
      AntialiasMode := ierBilinear;
    2:
      AntialiasMode := ierBicubic;
  end;
  BackgroundColor := cxColorComboBox1.EditValue;
  if LockPreview1.Checked then
  begin
    ImageEnView1.Proc.SaveUndoCaptioned( ImageEnView1.Proc.UndoCaptions[ 0 ], ieuImage ); // saves in Undo list
    ImageEnView1.Proc.Rotate( Angle, Antialias, AntialiasMode, BackgroundColor );
    ImageEnView1.Proc.ClearAllRedo;
    ImageEnView1.Update;
    UpdateMenu;
  end;
  ImageEnView1.Proc.ClearAllRedo;
  UpdateMenu;
end;

procedure TFormRotate.RadioGroup1Click( Sender: TObject );
begin
  with ImageEnView1.Proc do
  begin
    SaveRedoCaptioned( UndoCaptions[ 0 ], ieuImage ); // saves in Redo list
    Undo;
    ClearUndo;
  end;
  UpdateMenu;
  case RadioGroup1.ItemIndex of
    0:
      Angle := 0;
    1:
      Angle := 45;
    2:
      Angle := 90;
    3:
      Angle := 135;
    4:
      Angle := 180;
    5:
      Angle := 225;
    6:
      Angle := 270;
    7:
      Angle := -45;
    8:
      Angle := -90;
    9:
      Angle := -135;
    10:
      Angle := -180;
    11:
      Angle := -225;
    12:
      Angle := -270;
  end;
  Antialias := CheckBox1.Checked;
  case RadioGroup2.ItemIndex of
    0:
      AntialiasMode := ierFast;
    1:
      AntialiasMode := ierBilinear;
    2:
      AntialiasMode := ierBicubic;
  end;
  BackgroundColor := cxColorComboBox1.EditValue;
  if LockPreview1.Checked then
  begin
    ImageEnView1.Proc.SaveUndoCaptioned( ImageEnView1.Proc.UndoCaptions[ 0 ], ieuImage ); // saves in Undo list
    ImageEnView1.Proc.Rotate( Angle, Antialias, AntialiasMode, BackgroundColor );
    ImageEnView1.Proc.ClearAllRedo;
    ImageEnView1.Update;
    UpdateMenu;
  end;
end;

procedure TFormRotate.RadioGroup2Click( Sender: TObject );
begin
  if RadioGroup1.ItemIndex <> 0 then
  begin
    with ImageEnView1.Proc do
    begin
      SaveRedoCaptioned( UndoCaptions[ 0 ], ieuImage ); // saves in Redo list
      Undo;
      ClearUndo;
    end;
    UpdateMenu;
  end;
  case RadioGroup1.ItemIndex of
    0:
      Angle := 0;
    1:
      Angle := 45;
    2:
      Angle := 90;
    3:
      Angle := 135;
    4:
      Angle := 180;
    5:
      Angle := 225;
    6:
      Angle := 270;
    7:
      Angle := -45;
    8:
      Angle := -90;
    9:
      Angle := -135;
    10:
      Angle := -180;
    11:
      Angle := -225;
    12:
      Angle := -270;
  end;
  Antialias := CheckBox1.Checked;
  case RadioGroup2.ItemIndex of
    0:
      AntialiasMode := ierFast;
    1:
      AntialiasMode := ierBilinear;
    2:
      AntialiasMode := ierBicubic;
  end;
  BackgroundColor := cxColorComboBox1.EditValue;
  if LockPreview1.Checked then
  begin
    ImageEnView1.Proc.SaveUndoCaptioned( ImageEnView1.Proc.UndoCaptions[ 0 ], ieuImage ); // saves in Undo list
    ImageEnView1.Proc.Rotate( Angle, Antialias, AntialiasMode, BackgroundColor );
    ImageEnView1.Proc.ClearAllRedo;
    ImageEnView1.Update;
    UpdateMenu;
  end;
end;

procedure TFormRotate.Reset1Click( Sender: TObject );
begin
  if FormMain.cxPageControl1.ActivePage <> nil then
  begin
    ImageEnView := TImageEnView( FormMain.cxPageControl1.ActivePage.Controls[ 0 ] );
    ImageEnView1.Assign( ImageEnView );
    RadioGroup1.ItemIndex := 0;
    RadioGroup2.ItemIndex := 2;
    cxColorComboBox1.EditValue := clWhite;
    ImageEnView1.Proc.ClearAllUndo;
    ImageEnView1.Proc.ClearAllRedo;
    UpdateMenu;
  end;
end;

procedure TFormRotate.Undo1Click( Sender: TObject );
begin
  with ImageEnView1.Proc do
  begin
    SaveRedoCaptioned( UndoCaptions[ 0 ], ieuImage ); // saves in Redo list
    Undo;
    ClearUndo;
  end;
  UpdateMenu;
  RadioGroup1.ItemIndex := -1;
end;

procedure TFormRotate.UpdateMenu;
begin
  with ImageEnView1.Proc do
  begin
    // Undo menu
    Undo1.Hint := '&Undo ';
    Undo1.Enabled := UndoCount > 0;
    if UndoCount > 0 then
      Undo1.Hint := '&Undo ' + UndoCaptions[ 0 ];
  end;
end;

end.
