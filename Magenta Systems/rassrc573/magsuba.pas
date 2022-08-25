unit magsuba;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off} 
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
Updated by Angus Robertson, Magenta Systems Ltd, England, 11th August 2010
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

7 May    2001 - added WrapCaption, added Sleep to sysDelay
7 Apr 2003 - check terminate in delay loop
20 Mar 2004 - added sysDelayNoSleep so other functions in same program don't stop
18 Apr 2005 - SysDelay using TestTrgTick which supports wrapping at 49 days
27 Mar 2006 - added sysDelayNoMess
1 March 2010 - split all service stuff to MagService


}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdctrls, MagSubs1 ;


function Confirm(Msg: string): Boolean;
procedure sysDelay(aMs: longword);
procedure sysDelayNoSleep (aMs: longword);
procedure sysDelayNoMess (aMs: longword);
procedure WrapCaption (CapText: string; CapLabel: TLabel) ;


implementation

// message dialogue box

function Confirm(Msg: string): Boolean;
begin
  Result := MessageDlg(Msg, mtConfirmation, mbYesNoCancel, 0) = mrYes;
end;

procedure sysDelay (aMs: longword);
var
    Trg: longword;
begin
    Trg := GetTrgMsecs (aMs) ;
    while True do
    begin
        Application.ProcessMessages;
        if Application.Terminated then break ;
        if TestTrgTick (Trg) then break ;
        Sleep(0);   // thread now stops for rest of time slice
    end ;
end;

procedure sysDelayNoSleep (aMs: longword);
var
    Trg: longword;
begin
    Trg := GetTrgMsecs (aMs) ;
    while True do
    begin
        Application.ProcessMessages;
        if Application.Terminated then break ;
        if TestTrgTick (Trg) then break ;
    end ;
end;

procedure sysDelayNoMess (aMs: longword);
var
    Trg: longword;
begin
    Trg := GetTrgMsecs (aMs) ;
    while True do
    begin
        if Application.Terminated then break ;
        if TestTrgTick (Trg) then break ;
        Sleep(0);   // thread now stops for rest of time slice
    end ;
end;

// word wrap a caption allowing special file name and URL symbols for breaking

procedure WrapCaption (CapText: string; CapLabel: TLabel) ;
var
    maxcol: integer ;
begin
    if CapText <> '' then
    begin
        maxcol := (CapLabel.Width * Length (CapText)) div
                                        CapLabel.Canvas.TextWidth (CapText) ;
        CapLabel.Caption := WrapText (CapText, #13#10,
                                 [' ', '-', #9, '/', '\', '.', '?'], maxcol) ;
    end
    else
        CapLabel.Caption := CapText ;
end ;

end.

