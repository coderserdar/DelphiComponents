//------------------------------------------------------------------------------
//  Apprehend Version  : 4.3
//  Copyright (c) 2008 : Adirondack Software & Graphics
//  Created            : 1-09-1992
//  Last Modification  : 10-26-2008
//  Description        : Splash Unit
//------------------------------------------------------------------------------

unit usplash;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TFormSplash = class( TForm )
    pnlClient: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Label1: TLabel;
    Label5: TLabel;
    tmMinDisplay: TTimer;
    Label2: TLabel;
    procedure Label5MouseEnter( Sender: TObject );
    procedure Label5MouseLeave( Sender: TObject );
    procedure Label5Click( Sender: TObject );
    procedure Label1Click( Sender: TObject );
    procedure Label1MouseEnter( Sender: TObject );
    procedure Label1MouseLeave( Sender: TObject );
    procedure Label4Click( Sender: TObject );
    procedure Label4MouseEnter( Sender: TObject );
    procedure Label4MouseLeave( Sender: TObject );
    procedure FormClose( Sender: TObject; var Action: TCloseAction );
    procedure tmMinDisplayTimer( Sender: TObject );
    procedure pnlClientClick( Sender: TObject );
    procedure AdvReflectionImage1Click( Sender: TObject );
  private
    { Private declarations }
    fCloseRequested: Boolean;
    {Flag recording if RequestClose method has been called}
    fTimeOut: Boolean;
    {Flag recording if form's minimum display time has elapsed}
    fTryToCloseLock: Integer;
    {Lock on TryToClose to prevent simultaneous access from main code and timer}
    procedure TryToClose;
    {Closes form only if RequestClose method has been called and if minimum display time has elapsed.}
  public
    { Public declarations }
    procedure RequestClose;
    {Requests that form should close. If minimum display time has expired form
    will close, otherwise request will be noted and form will not close.}
  end;

var
  FormSplash: TFormSplash;

implementation

uses ShellAPI;

{$R *.dfm}

procedure TFormSplash.RequestClose;
{Requests that form should close. If minimum display time has expired form
will close, otherwise request will be noted and form will not close.}
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
{Closes form only if RequestClose method has been called and if minimum
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

procedure TFormSplash.Label5MouseEnter( Sender: TObject );
begin
  Label5.Font.Color := clRed;
  Label5.Font.Style := [ fsUnderline ];
end;

procedure TFormSplash.Label5MouseLeave( Sender: TObject );
begin
  Label5.Font.Color := clBlack;
  Label5.Font.Style := [ ];
end;

procedure TFormSplash.pnlClientClick( Sender: TObject );
begin
  tmMinDisplay.Enabled := False; // prevent timer from firing again
  fTimeOut := True; // note that form has timed out
  Close;
end;

procedure TFormSplash.Label5Click( Sender: TObject );
begin
  Screen.Cursor := crHourglass;
  try
    ShellExecute( 0, nil, PChar( 'mailto:' + Label5.Caption ), nil, nil, SW_NORMAL );
  finally; Screen.Cursor := crDefault; end;
end;

procedure TFormSplash.AdvReflectionImage1Click( Sender: TObject );
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

procedure TFormSplash.Label1Click( Sender: TObject );
begin
  Screen.Cursor := crHourglass;
  try
    ShellExecute( Handle, 'open', PChar( 'http://www.hi-components.com/Apprehend' ), nil, nil, SW_SHOWNORMAL );
  finally; Screen.Cursor := crDefault; end;
end;

procedure TFormSplash.Label1MouseEnter( Sender: TObject );
begin
  Label1.Font.Color := clRed;
  Label1.Font.Style := [ fsUnderline ];
end;

procedure TFormSplash.Label1MouseLeave( Sender: TObject );
begin
  Label1.Font.Color := clBlack;
  Label1.Font.Style := [ ];
end;

procedure TFormSplash.Label4Click( Sender: TObject );
begin
  Screen.Cursor := crHourglass;
  try
    ShellExecute( Handle, 'open', PChar( 'http://www.hi-components.com' ), nil, nil, SW_SHOWNORMAL );
  finally; Screen.Cursor := crDefault; end;
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

