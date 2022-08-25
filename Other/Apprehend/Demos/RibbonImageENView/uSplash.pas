(* ------------------------------------------------------------------------------
  ImageMaker              : 1.0
  Copyright © 1986-2012   : Copyright Adirondack Software & Graphics
  Created                 : 02-01-2012
  Last Modification       : 03-01-2012
  Source File             : uSplash.pas
  Compiler                : Delphi 2010
  Operating System        : Windows 7
  This file is copyright (C) W W Miller, 1986-2012.
  It may be used without restriction. This code distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
  ------------------------------------------------------------------------------ *)
unit uSplash;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, jpeg, dxGDIPlusClasses, GIFImg, dxBevel;

type
  TFormSplash = class( TForm )
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Label7: TLabel;
    tmMinDisplay: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    dxBevel1: TdxBevel;
    procedure Label7Click( Sender: TObject );
    procedure Label7MouseEnter( Sender: TObject );
    procedure Label7MouseLeave( Sender: TObject );
    procedure Label4Click( Sender: TObject );
    procedure Label4MouseEnter( Sender: TObject );
    procedure Label4MouseLeave( Sender: TObject );
    procedure FormClose( Sender: TObject; var Action: TCloseAction );
    procedure tmMinDisplayTimer( Sender: TObject );
    procedure pnlClientClick( Sender: TObject );
    procedure Image2Click( Sender: TObject );
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    fCloseRequested: Boolean;
    { Flag recording if RequestClose method has been called }
    fTimeOut: Boolean;
    { Flag recording if form's minimum display time has elapsed }
    fTryToCloseLock: Integer;
    { Lock on TryToClose to prevent simultaneous access from main code and timer }
    procedure TryToClose;
    { Closes form only if RequestClose method has been called and if minimum display time has elapsed. }
  public
    { Public declarations }
    procedure RequestClose;
    { Requests that form should close. If minimum display time has expired form
      will close, otherwise request will be noted and form will not close. }
  end;

var
  FormSplash: TFormSplash;

implementation

uses uMain, ShellAPI;
{$R *.dfm}

procedure TFormSplash.RequestClose;
{ Requests that form should close. If minimum display time has expired form
  will close, otherwise request will be noted and form will not close. }
begin
  fCloseRequested := True;
  TryToClose;
end;

procedure TFormSplash.tmMinDisplayTimer( Sender: TObject );
begin
  inherited;
  tmMinDisplay.Enabled := False; // prevent timer from firing again
  fTimeOut := True; // note that form has timed out
  TryToClose;
end;

procedure TFormSplash.TryToClose;
{ Closes form only if RequestClose method has been called and if minimum
  display time has elapsed.
}
begin
  // Wait until lock is cleared
  while fTryToCloseLock > 0 do
    Application.ProcessMessages;
  // Lock entry to method
  InterlockedIncrement( fTryToCloseLock );
  try
    // Close document if closure requested and time out reached
    if fCloseRequested and fTimeOut then
      FormSplash.Close;
  finally
    // Unlock method
    InterlockedDecrement( fTryToCloseLock );
  end;
end;

procedure TFormSplash.pnlClientClick( Sender: TObject );
begin
  tmMinDisplay.Enabled := False; // prevent timer from firing again
  fTimeOut := True; // note that form has timed out
  Close;
end;

procedure TFormSplash.FormShow(Sender: TObject);
begin
// Post a message to close the Splash Form
  FormSplash.RequestClose;
end;

procedure TFormSplash.Image2Click( Sender: TObject );
begin
  tmMinDisplay.Enabled := False; // prevent timer from firing again
  fTimeOut := True; // note that form has timed out
  Close;
end;

procedure TFormSplash.FormClose( Sender: TObject; var Action: TCloseAction );
begin
  inherited;
  Action := caFree;
  FormSplash := nil;
end;

procedure TFormSplash.Label7Click( Sender: TObject );
begin
  Screen.Cursor := crHourglass;
  try
    ShellExecute( Handle, 'open', PChar( 'http://frontiernet.net/~w2m/index.html' ), nil, nil, SW_SHOWNORMAL );
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormSplash.Label7MouseEnter( Sender: TObject );
begin
  Label7.Font.Color := clRed;
  Label7.Font.Style := [ fsUnderline ];
end;

procedure TFormSplash.Label7MouseLeave( Sender: TObject );
begin
  Label7.Font.Color := clBlack;
  Label7.Font.Style := [ ];
end;

procedure TFormSplash.Label4Click( Sender: TObject );
begin
  Screen.Cursor := crHourglass;
  try
    ShellExecute( Handle, 'open', PChar( 'http://williamwmiller.wordpress.com/' ), nil, nil, SW_SHOWNORMAL );
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormSplash.Label4MouseEnter( Sender: TObject );
begin
  Label4.Font.Color := clRed;
  Label4.Font.Style := [ fsUnderline ];
end;

procedure TFormSplash.Label4MouseLeave( Sender: TObject );
begin
  Label4.Font.Color := clBlack;
  Label4.Font.Style := [ ];
end;

end.
