(* ------------------------------------------------------------------------------
  Apprehend               : 6.0
  Copyright © 1986-2012   : Copyright Adirondack Software & Graphics
  Last Modification       : 04-01-2012
  Source File             : uScreenDelay.pas
  Compiler                : Delphi 2010
  Operating System        : Windows 7
  This file is copyright © W W Miller, 1986-2012.
  It may be used without restriction. This code distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
  ------------------------------------------------------------------------------ *)

unit uScreenDelay;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
  ComCtrls, ExtCtrls;

type
  TFormDelay = class( TForm )
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edScreenDelay: TEdit;
    UpDown1: TUpDown;
    Panel1: TPanel;
    Label2: TLabel;
    procedure OKBtnClick( Sender: TObject );
    procedure CancelBtnClick( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDelay: TFormDelay;

implementation

{$R *.DFM}

procedure TFormDelay.OKBtnClick( Sender: TObject );
begin
  ModalResult := mrOk;
end;

procedure TFormDelay.CancelBtnClick( Sender: TObject );
begin
  ModalResult := mrCancel;
end;

end.

