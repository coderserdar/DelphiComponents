//------------------------------------------------------------------------------
// Apprehend Version     : 5.1
// Copyright © 1986-2012 : Adirondack Software & Graphics
// Last Modification     : 04-01-2012
// Description           : ScreenDelay Unit
// Compiler              : Delphi 2010
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uScreenDelay;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
  ComCtrls, ExtCtrls;

type
  TDelayForm = class ( TForm )
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edScreenDelay: TEdit;
    UpDown1: TUpDown;
    Panel1: TPanel;
    procedure OKBtnClick ( Sender: TObject );
    procedure CancelBtnClick ( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DelayForm: TDelayForm;

implementation

{$R *.DFM}

procedure TDelayForm.OKBtnClick ( Sender: TObject );
begin
  ModalResult := mrOk;
end;

procedure TDelayForm.CancelBtnClick ( Sender: TObject );
begin
  ModalResult := mrCancel;
end;

end.

