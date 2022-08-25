//------------------------------------------------------------------------------
//  Apprehend Version  : 4.3
//  Copyright (c) 2010 : Adirondack Software & Graphics
//  Created            : 1-09-1992
//  Last Modification  : 01-06-2010
//  Description        : ScreenDelay Unit
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

