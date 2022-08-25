// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 03-23-2012
// Description             : uAbout
// Compiler                : Delphi 2010
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------

unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TfrmAbout = class ( TForm )
    pnlClient: TPanel;
    Bevel1: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Button1: TButton;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Image2: TImage;
    TabSheet3: TTabSheet;
    RichEdit1: TRichEdit;
    procedure FormCreate ( Sender: TObject );
    procedure Label1Click ( Sender: TObject );
    procedure Label1MouseEnter ( Sender: TObject );
    procedure Label1MouseLeave ( Sender: TObject );
    procedure Label4Click ( Sender: TObject );
    procedure Label4MouseEnter ( Sender: TObject );
    procedure Label4MouseLeave ( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

const
  VERSION = '6.0';

implementation

uses ShellAPI;

{$R *.dfm}

procedure TfrmAbout.FormCreate ( Sender: TObject );
begin
  Label3.Caption := 'Version ' + VERSION;
  Label3.Invalidate;
end;

procedure TfrmAbout.Label1Click ( Sender: TObject );
begin
  Screen.Cursor := crHourglass;
  try
    ShellExecute ( Handle, 'open', PChar ( 'http://www.hicomponents.com/Apprehend' ), nil, nil, SW_SHOWNORMAL );
  finally; Screen.Cursor := crDefault; end;
end;

procedure TfrmAbout.Label1MouseEnter ( Sender: TObject );
begin
  Label1.Font.Color := clRed;
  Label1.Font.Style := [ fsUnderline ];
end;

procedure TfrmAbout.Label1MouseLeave ( Sender: TObject );
begin
  Label1.Font.Color := clBlack;
  Label1.Font.Style := [ ];
end;

procedure TfrmAbout.Label4Click ( Sender: TObject );
begin
  Screen.Cursor := crHourglass;
  try
    ShellExecute ( Handle, 'open', PChar ( 'http://www.hicomponents.com/Apprehend' ), nil, nil, SW_SHOWNORMAL );
  finally; Screen.Cursor := crDefault; end;
end;

procedure TfrmAbout.Label4MouseEnter ( Sender: TObject );
begin
  Label4.Font.Color := clRed;
  Label4.Font.Style := [ fsUnderline ];
end;

procedure TfrmAbout.Label4MouseLeave ( Sender: TObject );
begin
  Label4.Font.Color := clBlack;
  Label4.Font.Style := [ ];
end;

end.

