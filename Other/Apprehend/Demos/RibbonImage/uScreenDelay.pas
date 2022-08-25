//------------------------------------------------------------------------------
//  Apprehend Version     : 6.0
//  Copyright © 1986-2011 : Adirondack Software & Graphics
//  Last Modification     : 03-25-2012
//  Compiler              : Delphi 2010
//  Operating System      : Windows 7
//  Description           : ScreenDelay Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uScreenDelay;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
  ComCtrls, ExtCtrls;

type
  TDelayForm = class( TForm )
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edScreenDelay: TEdit;
    UpDown1: TUpDown;
    Panel1: TPanel;
    procedure OKBtnClick( Sender: TObject );
    procedure CancelBtnClick( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DelayForm: TDelayForm;

implementation

{$R *.DFM}

procedure TDelayForm.OKBtnClick( Sender: TObject );
begin
  ModalResult := mrOk;
end;

procedure TDelayForm.CancelBtnClick( Sender: TObject );
begin
  ModalResult := mrCancel;
end;

end.

