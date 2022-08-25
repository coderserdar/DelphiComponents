// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 03-23-2012
// Description             : uScreenDelay
// Compiler                : Delphi 2010
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------

unit uScreenDelay;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
  ComCtrls;

type
  TDelayDlg = class ( TForm )
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edScreenDelay: TEdit;
    UpDown1: TUpDown;
    procedure OKBtnClick ( Sender: TObject );
    procedure CancelBtnClick ( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DelayDlg: TDelayDlg;

implementation

{$R *.DFM}

procedure TDelayDlg.OKBtnClick ( Sender: TObject );
begin
  ModalResult := mrOK;
end;

procedure TDelayDlg.CancelBtnClick ( Sender: TObject );
begin
  ModalResult := mrCancel;
end;

end.

