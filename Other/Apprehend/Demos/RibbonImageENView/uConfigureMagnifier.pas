//------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012 : Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Compiler                : Delphi 2010
// Description             : ConfigureMagnifier Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uConfigureMagnifier;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls;

type
  TFormConfigureMagnifier = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MagniferBorders1: TCheckBox;
    ComboBoxMagnifierStyle1: TComboBox;
    Label4: TLabel;
    EditMagnifierSize1: TEdit;
    UpDownMagnifierSize1: TUpDown;
    EditMagnifierRate1: TEdit;
    EditMagnifierTransparency1: TEdit;
    UpDownMagnifierRate1: TUpDown;
    UpDownMagnifierTransparency1: TUpDown;
    Panel1: TPanel;
    procedure MagniferBorders1Click(Sender: TObject);
    procedure ComboBoxMagnifierStyle1Change(Sender: TObject);
    procedure UpDownMagnifierRate1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure UpDownMagnifierTransparency1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure UpDownMagnifierSize1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfigureMagnifier: TFormConfigureMagnifier;

implementation

Uses uMain, IEView, ImageENView, IEVect;

{$R *.dfm}

procedure TFormConfigureMagnifier.MagniferBorders1Click(Sender: TObject);
begin
   TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Layers[1].VisibleBox := MagniferBorders1.Checked;
   TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Update;
end;

procedure TFormConfigureMagnifier.UpDownMagnifierSize1Changing(
  Sender: TObject; var AllowChange: Boolean);
begin
  TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Layers[1].Width := UpDownMagnifierSize1.Position;
  TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Layers[1].Height := UpDownMagnifierSize1.Position;
  TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Update;
end;

procedure TFormConfigureMagnifier.ComboBoxMagnifierStyle1Change(Sender: TObject);
begin
	TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Layers[1].Magnify.Style := TIEMagnifyStyle( ComboBoxMagnifierStyle1.ItemIndex );
  TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Update;
end;

procedure TFormConfigureMagnifier.UpDownMagnifierRate1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
	TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Layers[1].Magnify.Rate:=UpDownMagnifierRate1.Position/10;
  TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Update;
end;

procedure TFormConfigureMagnifier.UpDownMagnifierTransparency1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
	TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Layers[1].Transparency := UpDownMagnifierTransparency1.Position;
  TImageENVect ( FormMain.PageControl1.ActivePage.Controls[0] ).Update;
end;

procedure TFormConfigureMagnifier.FormDeactivate(Sender: TObject);
begin
  //FormMain.OptionsMagnifier1.Checked := False;
end;

procedure TFormConfigureMagnifier.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //FormMain.Action72.Checked := False;
end;

end.
