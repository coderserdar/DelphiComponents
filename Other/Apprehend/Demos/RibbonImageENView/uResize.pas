//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012   : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : Resize Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uResize;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ImageEnView, ImageEn, ImageEnIO, ImageEnProc, HYIEDefs, ExtCtrls,
  IEView;

type
  TResizeForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    ComboBox1: TComboBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ImageEnProc1: TImageEnProc;
    ImageEnIO1: TImageEnIO;
    Edit1: TEdit;
    Edit2: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    ResetBtn: TButton;
    ImageEnView1: TImageEnView;
    PeviewButton: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Panel1: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageEnView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PeviewButtonClick(Sender: TObject);
  private
    { Private declarations }
    DontChange: boolean;
    procedure Preview;
  public
    { Public declarations }
    OrgWidth, OrgHeight: integer;
    Resize: Boolean;
    Resample: Boolean;
  end;

var
  ResizeForm: TResizeForm;

implementation

uses uMain;

{$R *.DFM}

procedure TResizeForm.FormCreate(Sender: TObject);
begin
  ImageEnView1.MouseInteract := [miZoom, miScroll];
  ImageEnView1.Cursor := 1779;
  ImageEnView1.Scrollbars := ssBoth;
  ImageEnView1.ZoomFilter := rfNone;
  ImageEnView1.BackgroundStyle := iebsChessboard;
  ImageEnView1.BackGround := clWhite;
  ImageEnIO1.AttachedImageEn := ImageEnView1;
  ImageEnProc1.AttachedImageEn := ImageEnView1;
end;

procedure TResizeForm.FormActivate(Sender: TObject);
begin
  Edit1.Text := IntToStr(OrgWidth);
  Edit2.Text := IntToStr(OrgHeight);
  DontChange := False;
  ComboBox1.ItemIndex := 0;
  Edit1.SetFocus;
end;

procedure TResizeForm.Edit1Change(Sender: TObject);
begin
  if CheckBox1.Checked and not DontChange then
  begin
    DontChange := True;
    Edit2.Text := IntToStr(Round(OrgHeight * StrToIntDef(Edit1.Text, 0) / OrgWidth));
    DontChange := False;
  end;
end;

procedure TResizeForm.Edit2Change(Sender: TObject);
begin
  if CheckBox1.Checked and not DontChange then
  begin
    DontChange := True;
    Edit1.Text := IntToStr(Round(OrgWidth * StrToIntDef(Edit2.Text, 0) / OrgHeight));
    DontChange := False;
  end;
end;

procedure TResizeForm.Preview;
var
  w, h: integer;
begin
  if CheckBox1.checked and not DontChange then
  begin
    DontChange := true;
    w := StrToIntDef(Edit1.Text, 0);
    h := StrToIntDef(Edit2.Text, 0);
    if (w > 0) and (h > 0) then
    begin
      if Resize then
        ImageEnProc1.ImageResize(w, h);
      if Resample then
        ImageEnProc1.Resample(w, h, TResampleFilter(ComboBox1.ItemIndex));
      ImageEnView1.Update;
    end;
    DontChange := False;
  end
  else
  begin
    DontChange := true;
    w := StrToIntDef(Edit1.Text, 0);
    h := StrToIntDef(Edit2.Text, 0);
    if (w > 0) and (h > 0) then
    begin
      if Resize then
        ImageEnProc1.ImageResize(w, h);
      if Resample then
        ImageEnProc1.Resample(w, h, TResampleFilter(ComboBox1.ItemIndex));
      ImageEnView1.Update;
    end;
    DontChange := False;
  end;
end;

procedure TResizeForm.ResetBtnClick(Sender: TObject);
begin
  ImageEnView1.Assign(TImageENView(FormMain.PageControl1.ActivePage.Controls[0]).Bitmap);
  Edit1.Text := IntToStr(OrgWidth);
  Edit2.Text := IntToStr(OrgHeight);
  DontChange := False;
  ComboBox1.ItemIndex := 0;
  OrgWidth := ImageENView1.Bitmap.Width;
  OrgHeight := ImageENView1.Bitmap.Height;
  Edit1.Text := IntToStr(OrgWidth);
  Edit2.Text := IntToStr(OrgHeight);
  Edit1.SetFocus;
end;

procedure TResizeForm.FormShow(Sender: TObject);
begin
  // save undo file
  ImageEnProc1.SaveUndo;
end;

procedure TResizeForm.ImageEnView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ImageEnView1.MouseInteract = [miZoom, miScroll] then
    ImageEnView1.Cursor := 1779
  else
    ImageEnView1.Cursor := 1782;
end;

procedure TResizeForm.PeviewButtonClick(Sender: TObject);
begin
  Preview;
end;

end.

