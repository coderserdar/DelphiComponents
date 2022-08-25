//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012 : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : Print Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uPrint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ImageEnView, ImageEnIO, ieview, ExtCtrls;

type
  TFormPrint = class(TForm)
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    GroupBox2: TGroupBox;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton1: TSpeedButton;
    GroupBox3: TGroupBox;
    ComboBox1: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    Edit5: TEdit;
    Label7: TLabel;
    Edit6: TEdit;
    Label8: TLabel;
    GroupBox4: TGroupBox;
    Edit7: TEdit;
    Label9: TLabel;
    Button1: TButton;
    Button2: TButton;
    GroupBox5: TGroupBox;
    ImageEnView1: TImageEnView;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure printpreview(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    io:TImageEnIO;
  end;

var
  FormPrint: TFormPrint;

implementation

uses Printers;

{$R *.DFM}

procedure TFormPrint.FormCreate(Sender: TObject);
begin
	ComboBox1.ItemIndex:=1;
end;

procedure TFormPrint.FormActivate(Sender: TObject);
begin
	printpreview(Sender);
end;

// Changes Size combobox
procedure TFormPrint.ComboBox1Change(Sender: TObject);
var
	en:boolean;
begin
	en:= ComboBox1.ItemIndex=3;	// true when Specified Sizes
   Label5.Enabled:=en;
   Label6.Enabled:=en;
   Label7.Enabled:=en;
   Label8.Enabled:=en;
   Edit5.Enabled:=en;
   Edit6.Enabled:=en;
   printpreview(Sender);
end;

// Preview
procedure TFormPrint.Button3Click(Sender: TObject);
begin
	printpreview(Sender);
end;

// OK (print)
procedure TFormPrint.Button1Click(Sender: TObject);
begin
   printpreview(Sender);
end;

function StrToFloatDef(s:string; Def:extended):extended;
var
	q:integer;
begin
	if not TextToFloat(pchar(s),result,fvExtended) then begin
   	q:=pos(',',s);
      if q>0 then
      	s[q]:='.'
      else begin
			q:=pos('.',s);
         if q>0 then
         	s[q]:=',';
      end;
      if not TextToFloat(pchar(s),result,fvExtended) then
      	result:=Def;
   end;
end;

// di preview or print
procedure TFormPrint.printpreview(Sender: TObject);
var
	VerticalPos:TIEVerticalPos;
   HorizontalPos:TIEHorizontalPos;
   Size:TIESize;
   MarginLeft,MarginTop,MarginRight,MarginBottom,SpecWidth,SpecHeight,GammaCorrection:double;
begin
   VerticalPos:=ievpCENTER;
   HorizontalPos:=iehpCENTER;
   if SpeedButton1.Down or SpeedButton2.Down or SpeedButton3.Down then
      VerticalPos:=ievpTOP;
   if SpeedButton7.Down or SpeedButton8.Down or SpeedButton9.Down then
      VerticalPos:=ievpBOTTOM;
   if SpeedButton1.Down or SpeedButton4.Down or SpeedButton7.Down then
      HorizontalPos:=iehpLEFT;
   if SpeedButton3.Down or SpeedButton6.Down or SpeedButton9.Down then
      HorizontalPos:=iehpRIGHT;
   Size:=iesFITTOPAGE;
   case ComboBox1.ItemIndex of
   	0: Size:=iesNORMAL;
      1: Size:=iesFITTOPAGE;
      2: Size:=iesFITTOPAGESTRETCH;
      3: Size:=iesSPECIFIEDSIZE;
   end;
   MarginLeft:=StrToFloatDef(Edit2.Text,0);
   MarginTop:=StrToFloatDef(Edit1.Text,0);
   MarginRight:=StrToFloatDef(Edit3.Text,0);
   MarginBottom:=StrToFloatDef(Edit4.text,0);
   SpecWidth:=StrToFloatDef(Edit5.Text,1);
   SpecHeight:=StrToFloatDef(Edit6.Text,1);
   GammaCorrection:=StrToFloatDef(Edit7.Text,1);
   if Sender=Button1 then begin
   	// print
      Printer.BeginDoc;
      io.PrintImage(Printer.Canvas,MarginLeft,MarginTop,MarginRight,MarginBottom,VerticalPos,HorizontalPos,
            Size,SpecWidth,SpecHeight,GammaCorrection);
      Printer.EndDoc;
   end else begin
   	// preview
      io.PreviewPrintImage(ImageEnView1.Bitmap,ImageEnView1.Width,ImageEnView1.Height,Printer,MarginLeft,MarginTop,MarginRight,MarginBottom,VerticalPos,HorizontalPos,
            Size,SpecWidth,SpecHeight,GammaCorrection);
      ImageEnView1.Update;
   end;
end;


end.
