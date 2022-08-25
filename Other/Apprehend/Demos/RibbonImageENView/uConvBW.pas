//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012 : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : ConvBW Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uConvBW;

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   Buttons, StdCtrls, ExtCtrls;

type
   TFormConvBW = class( TForm )
      RadioGroup1: TRadioGroup;
      Button1: TButton;
      Button2: TButton;
      GroupBox1: TGroupBox;
      Label1: TLabel;
      Edit1: TEdit;
      SpeedButton1: TSpeedButton;
    Panel1: TPanel;
      procedure RadioGroup1Click( Sender: TObject );
      procedure SpeedButton1Click( Sender: TObject );
   private
    { Private declarations }
   public
    { Public declarations }
   end;

var
   FormConvBW: TFormConvBW;

implementation

{$R *.DFM}

procedure TFormConvBW.RadioGroup1Click( Sender: TObject );
begin
   GroupBox1.Enabled := RadioGroup1.ItemIndex = 0;
end;

procedure TFormConvBW.SpeedButton1Click( Sender: TObject );
begin
   edit1.enabled := not SpeedButton1.Down;
   label1.enabled := not SpeedButton1.Down;
end;

end.

