//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012   : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : About Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, jpeg, ASGCapture, hyieutils;

type
  TFormAbout = class ( TForm )
    pnlClient: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Button1: TButton;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    RichEdit1: TRichEdit;
    Panel1: TPanel;
    Label5: TLabel;
    procedure FormCreate ( Sender: TObject );
    procedure Label1Click ( Sender: TObject );
    procedure Label1MouseEnter ( Sender: TObject );
    procedure Label1MouseLeave ( Sender: TObject );
    procedure Label4Click ( Sender: TObject );
    procedure Label4MouseEnter ( Sender: TObject );
    procedure Label4MouseLeave ( Sender: TObject );
    procedure FormMainActionAbout1Execute ( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

uses ShellAPI, uMain;

{$R *.dfm}

procedure TFormAbout.FormCreate ( Sender: TObject );
begin

  Label3.Caption := 'Version ' + FormMain.ASGScreenCapture1.Version;
  Label3.Invalidate;

end;

procedure TFormAbout.FormMainActionAbout1Execute ( Sender: TObject );
begin

  FormAbout := TFormAbout.Create ( self );
  try

    FormAbout.ShowModal;

  finally
    FormAbout.Free;
  end;

end;

procedure TFormAbout.Label1Click ( Sender: TObject );
begin

  Screen.Cursor := crHourGlass;
  try

    ShellExecute ( Handle, 'open', PChar ( 'http://frontiernet.net/~w2m/index.html' ), nil, nil, SW_SHOWNORMAL );

  finally
    Screen.Cursor := crDefault;
  end;

end;

procedure TFormAbout.Label1MouseEnter ( Sender: TObject );
begin

  Label1.Font.Color := clRed;
  Label1.Font.Style := [ fsUnderLine ];

end;

procedure TFormAbout.Label1MouseLeave ( Sender: TObject );
begin

  Label1.Font.Color := clBlack;
  Label1.Font.Style := [ ];

end;

procedure TFormAbout.Label4Click ( Sender: TObject );
begin

  Screen.Cursor := crHourGlass;
  try

    ShellExecute ( Handle, 'open', PChar ( 'http://williamwmiller.wordpress.com/' ), nil, nil, SW_SHOWNORMAL );

  finally;
    Screen.Cursor := crDefault;
  end;

end;

procedure TFormAbout.Label4MouseEnter ( Sender: TObject );
begin

  Label4.Font.Color := clRed;
  Label4.Font.Style := [ fsUnderLine ];

end;

procedure TFormAbout.Label4MouseLeave ( Sender: TObject );
begin

  Label4.Font.Color := clBlack;
  Label4.Font.Style := [ ];

end;

end.

