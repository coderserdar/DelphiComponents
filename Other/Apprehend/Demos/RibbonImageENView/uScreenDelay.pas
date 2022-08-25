//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012 : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : ScreenDelay Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uScreenDelay;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
  ComCtrls, ExtCtrls;

type
  TFormDelay = class ( TForm )
    Label1: TLabel;
    edScreenDelay: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    UpDown1: TUpDown;
    Panel1: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDelay: TFormDelay;

implementation

{$R *.DFM}

end.

