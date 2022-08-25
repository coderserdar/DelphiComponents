{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demo for a full blown multi-user server using TWSocket and
              console mode.
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Creation:     Feb 17, 1999
Version:      1.01
Support:      Use the mailing list twsocket@elists.org See website for details.
Legal issues: Copyright (C) 1996, 1997, 1998, 1999 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Sep 29, 1999 V1.01 Adapted for Delphi 5

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program OverbyteIcsConSrv1;
{$I OVERBYTEICSDEFS.INC}
{$IFDEF VER80}
  Bomb('Sorry but Delphi 1 doesn''t support console mode program');
{$ENDIF}
{$APPTYPE CONSOLE}
{$IFNDEF NOFORMS}
Bomb('This demo must be compiled with symbol NOFORMS defined.' +
       'Go to Delphi/Menu/Project/Options and in "Directories/Conditionals"' +
       'tab, add NOFORMS to the "define" edit box.');
{$ENDIF}

uses
  Messages,
  Windows,
  SysUtils,
  Classes,
  OverbyteIcsWSocket,
  WinSock,
  OverbyteIcsConSrv1S in 'OverbyteIcsConSrv1S.pas',
  OverbyteIcsConSrv1C in 'OverbyteIcsConSrv1C.pas';

const
    Version = 101;

type
    TKeyboardThread = class (TThread)
    public
        procedure Execute; override;
    end;

// Declare all standard functions and procedures
function  InitAplication : Boolean; forward;
procedure RunAplication; forward;
procedure CleanupAplication; forward;
procedure CleanupData; forward;
function  CtrlHandlerRoutine(CtrlType : DWORD) : DWORD; stdcall; forward;
function  MyWindowProc(ahWnd   : HWND;
                       auMsg   : Integer;
                       awParam : WPARAM;
                       alParam : LPARAM): Integer; stdcall; forward;
function  CreateEvent(var MsgRec : TMsg) : Integer; forward;
procedure ClientDisconnectedEvent(var MsgRec : TMsg); forward;

// Declare some global variables
var
    SrvObject     : TServerObject;
    Terminated    : Boolean;
    hWndMain      : HWND;
    KbdThread     : TKeyboardThread;
    MyWindowClass : TWndClass = (style         : 0;
                                 lpfnWndProc   : @MyWindowProc;
                                 cbClsExtra    : 0;
                                 cbWndExtra    : 0;
                                 hInstance     : 0;
                                 hIcon         : 0;
                                 hCursor       : 0;
                                 hbrBackground : 0;
                                 lpszMenuName  : nil;
                                 lpszClassName : 'MyWindowClass');


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Console mode applications do not receive keyboard messages as GUI apps.   }
{ We use a thread to wait for keyboard activity and generate keyboard       }
{ messages as in a GUI application.                                         }
procedure TKeyboardThread.Execute;
var
    hConsole    : THandle;
    Status      : DWORD;
    InputBuffer : TInputRecord;
    KeyEvent    : TKeyEventRecord;
    Count       : DWORD;
begin
    hConsole := GetStdHandle(STD_INPUT_HANDLE);
    while not Terminated do begin
        Status := WaitForSingleObject(hConsole, 1000);
        if Status = WAIT_OBJECT_0 then begin
            if ReadConsoleInput(hConsole, InputBuffer, 1, Count) then begin
                if InputBuffer.EventType = KEY_EVENT then begin
{$IFDEF VER90}  { Delphi 2 }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER93}  { Bcb 1    }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER100} { Delphi 3 }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER110} { Bcb 3    }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$ENDIF}
{ Starting from Delphi 4 and Bcb4, they changed definition }
                    KeyEvent := InputBuffer.Event.KeyEvent;
{$ENDIF}
{$ENDIF}
{$ENDIF}
                    if KeyEvent.bKeyDown then begin
                        PostMessage(hWndMain, WM_KEYDOWN,
                                    KeyEvent.wVirtualKeyCode,
                                    KeyEvent.wRepeatCount +
                                    (KeyEvent.wVirtualScanCode shl 16));
                    end
                    else begin
                        PostMessage(hWndMain, WM_KEYUP,
                                    KeyEvent.wVirtualKeyCode,
                                    KeyEvent.wRepeatCount +
                                    (KeyEvent.wVirtualScanCode shl 16));
                    end;
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is a callback routine called by windows when some events occurs.     }
{ We trap those events to close our application.                            }
function CtrlHandlerRoutine(CtrlType : DWORD) : DWORD; stdcall;
begin
    case CtrlType of
    CTRL_C_EVENT,            // User hit CTRL-C
    CTRL_BREAK_EVENT,        // User hit CTRL-BREAK
    CTRL_LOGOFF_EVENT,       // User log off his session
    CTRL_CLOSE_EVENT,        // Close signal
    CTRL_SHUTDOWN_EVENT :    // Window shutdown signal
        begin
            Result := 1;
            PostMessage(hWndMain, WM_QUIT, 0, 0);
        end;
    else
        Result := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ClientDisconnectedEvent(var MsgRec : TMsg);
var
    Client : TClientObject;
begin
    Client := TClientObject(MsgRec.lParam);
    if Assigned(SrvObject) and Assigned(Client) then
        SrvObject.DisconnectedClient(Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CreateEvent(var MsgRec : TMsg) : Integer;
begin
    try
        SetConsoleTitle(PChar('ConSrv V' + Format('%d.%2d',
                              [Version div 100,Version mod 100])));
        WriteLn('Hit CTRL-C to return to system.');
        SrvObject := TServerObject.Create;
        SrvObject.CtrlWindow := MsgRec.hwnd;
        KbdThread := TKeyboardThread.Create(FALSE);
        Result    := 0;  // Success
    except
        on E:Exception do begin
            WriteLn('CreateEvent failed.');
            WriteLn('Exception ' + E.ClassName + ': ' + E.Message);
            Result := -1; // Failure
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DisplayHelp;
begin
    WriteLn('F1      Display this help text');
    WriteLn('F2      Display user list');
    WriteLn('CTRL-C  Quit program');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure KeyDownEvent(MsgRec : TMsg);
var
    Key : Integer;
begin
    Key := MsgRec.wParam;
    case Key of
    VK_SHIFT,
    VK_CONTROL,
    VK_MENU:     { Ignore };
    VK_F1:
        DisplayHelp;
    VK_F2:
        SrvObject.DisplayClientList;
    else
        MessageBeep(MB_OK);
        WriteLn('Unknown keyboard command. Type F1 to get help.');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MyWindowProc(
    ahWnd   : HWND;
    auMsg   : Integer;
    awParam : WPARAM;
    alParam : LPARAM): Integer; stdcall;
var
    MsgRec : TMsg;
begin
    Result := 0;  // This means we handled the message
    try
        MsgRec.hwnd    := ahWnd;
        MsgRec.message := auMsg;
        MsgRec.wParam  := awParam;
        MsgRec.lParam  := alParam;

        case auMsg of
        WM_CLIENT_DISCONNECTED:
            ClientDisconnectedEvent(MsgRec);
        WM_CREATE:
            Result := CreateEvent(MsgRec);
        WM_CLOSE:
            begin
                WriteLn('Closing');
                DestroyWindow(ahWnd);
            end;
        WM_DESTROY:
            begin
                WriteLn('Destroying');
                CleanupData;
            end;
        WM_KEYDOWN: KeyDownEvent(MsgRec);
{       WM_KEYUP:   writeln('WM_KEYUP'); }
{       WM_CHAR:    writeln('WM_CHAR');  }
        else
            Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
        end;
    except
        on E:Exception do
            WriteLn('Exception ' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function InitAplication : Boolean;
begin
    Result := FALSE;
    if Windows.RegisterClass(MyWindowClass) = 0 then
        Exit;
    hWndMain := CreateWindowEx(WS_EX_TOOLWINDOW,
                               MyWindowClass.lpszClassName,
                               '',        { Window name   }
                               WS_POPUP,  { Window Style  }
                               0, 0,      { X, Y          }
                               0, 0,      { Width, Height }
                               0,         { hWndParent    }
                               0,         { hMenu         }
                               HInstance, { hInstance     }
                               nil);      { CreateParam   }
    if hWndMain = 0 then
        Exit;
    SetConsoleCtrlHandler(@CtrlHandlerRoutine, TRUE);
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CleanupData;
begin
    if Assigned(SrvObject) then begin
        SrvObject.Destroy;
        SrvObject := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CleanupAplication;
begin
    CleanupData;
    if hWndMain <> 0 then begin
        DestroyWindow(hWndMain);
        hWndMain := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RunAplication;
var
    MsgRec : TMsg;
begin
    { If GetMessage retrieves the WM_QUIT, the return value is FALSE and    }
    { the message loop is broken.                                           }
    while GetMessage(MsgRec, 0, 0, 0) do begin
        TranslateMessage(MsgRec);
        DispatchMessage(MsgRec)
    end;
    Terminated := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
    InitAplication;
    try
        RunAplication;
    finally
        CleanupAplication;
    end;
end.
