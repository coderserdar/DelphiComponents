// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright (c) 1986-2012 : Copyright Adirondack Software & Graphics
// Last Modification       : 03-24-2012
// Compiler                : Delphi 2010
// Description             : Flip Unit
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
unit uFlip;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IEView, ImageENView, ImageENProc, StdCtrls, ExtCtrls, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  Menus, cxButtons, cxCheckBox, cxGroupBox, cxRadioGroup, cxLabel;

type
  TFormFlip = class( TForm )
    RadioGroup1: TcxRadioGroup;
    Button1: TcxButton;
    Button2: TcxButton;
    Label1: TcxLabel;
    ImageEnView1: TImageEnView;
    MergeAlpha1: TcxCheckBox;
    Panel1: TPanel;
    cxLookAndFeelController1: TcxLookAndFeelController;
    procedure RadioGroup1Click( Sender: TObject );
    procedure FormActivate( Sender: TObject );
    procedure FormCreate( Sender: TObject );
  private
    { Private declarations }
    fFlipDirection: TFlipDir;
  public
    { Public declarations }
    property FlipDirection: TFlipDir read fFlipDirection write fFlipDirection;
  end;

var
  FormFlip: TFormFlip;

implementation

{$R *.dfm}

uses uMain;

procedure TFormFlip.FormActivate( Sender: TObject );
begin
  // case RadioGroup1.ItemIndex of
  // 0: ImageEnView1.Proc.Flip( fdHorizontal );
  // 1: ImageEnView1.Proc.Flip( fdVertical );
  // end;
end;

procedure TFormFlip.FormCreate( Sender: TObject );
begin
  fFlipDirection := fdHorizontal;
end;

procedure TFormFlip.RadioGroup1Click( Sender: TObject );
begin
  // ImageEnView1.Assign( frmImageViewer.ImageEnView1 );
  case RadioGroup1.ItemIndex of
    0:
      begin
        ImageEnView1.Proc.Flip( fdHorizontal );
        fFlipDirection := fdHorizontal;
      end;
    1:
      begin
        ImageEnView1.Proc.Flip( fdVertical );
        fFlipDirection := fdVertical;
      end;
  end;
end;

end.
