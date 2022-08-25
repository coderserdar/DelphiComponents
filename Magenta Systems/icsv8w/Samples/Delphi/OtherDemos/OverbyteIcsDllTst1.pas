{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 02, 2000
Description:  This is a demonstration program for IcsDll1.dll and IcsDll2.dll.
              It will dynamically load the DLL, get IcsDllDemo entry point and
              call it. Then display result from DLL call. Be aware that the two
              DLL use the same function name IcsDllDemo. In the program here,
              we use two different variables to point to two those entry points.
Version:      1.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2000-2010 by François PIETTE
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
Dec 12, 2004 V1.01 Added a few comments, beautified the code, delay DLL loading
                   until needed. Added code for IcsDll2.dll.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDllTst1;

{$IFDEF VER80}
'Sorry, this is a demo program calling a 32 bit DLL.'
'Upgrade to latest Delphi version to use it.'
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, Buttons;

type
  // This is the signature of the IcsDll1 exported function. It must mach the
  // declaration in IcsDll1.dpr otherwise the program will crash.
  TIcsDll1Demo = function (HostName : PAnsiChar;
                           Port     : PAnsiChar;
                           Buffer   : PAnsiChar;
                           BufSize  : PInteger): Integer; stdcall;

  // This is the signature of the IcsDll2 exported function. It must mach the
  // declaration in IcsDll1.dpr otherwise the program will crash.
  TIcsDll2Demo = function (Url      : PAnsiChar;
                           Buffer   : PAnsiChar;
                           BufSize  : PInteger): Integer; stdcall;

  TDllTestForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    HostnameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PortEdit: TEdit;
    CallDll1Button: TButton;
    CallDll2Button: TButton;
    Label3: TLabel;
    UrlEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CallDll1ButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CallDll2ButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    function  LoadIcsDll1: Boolean;
    procedure UnloadIcsDll1;
    function  LoadIcsDll2: Boolean;
    procedure UnloadIcsDll2;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  DllTestForm   : TDllTestForm;
  IcsDll1Handle : THandle;
  IcsDll1Demo   : TIcsDll1Demo;  // This will point to the function entry point
  IcsDll2Handle : THandle;
  IcsDll2Demo   : TIcsDll2Demo;  // This will point to the function entry point

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.FormCreate(Sender: TObject);
begin
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.FormDestroy(Sender: TObject);
begin
    UnloadIcsDll1;
    UnloadIcsDll2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        finally
            IniFile.Free;
        end;
        DisplayMemo.Clear;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDllTestForm.LoadIcsDll1 : Boolean;
begin
    Result := FALSE;
    // Dynamically load IcsDLL1.dll (Will be unloaded in FormDestroy)
    IcsDll1Handle := LoadLibrary('OverbyteIcsDLL1.dll');
    if IcsDll1Handle = 0 then begin
        Application.MessageBox('OverbyteIcsDLL1.dll not found', 'Error', MB_OK);
        Exit;
    end;

    // Locate IcsDllDemo entry point in the DLL
    IcsDll1Demo := GetProcAddress(IcsDll1Handle, 'IcsDllDemo');
    if @IcsDll1Demo = nil then begin
        Application.MessageBox('IcsDllDemo not found (OverbyteIcsDLL1.dll)',
                               'Error', MB_OK);
        Exit;
    end;

    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.UnloadIcsDll1;
begin
    // If we had loaded the DLL, then unload it
    if IcsDll1Handle <> 0 then begin
        FreeLibrary(IcsDll1Handle);
        IcsDll1Handle := 0;
        IcsDll1Demo    := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.CallDll1ButtonClick(Sender: TObject);
var
    Buffer  : AnsiString;
    BufSize : Integer;
    Status  : Integer;
begin
    // First we must make sure we already loaded the DLL
    if not Assigned(IcsDll1Demo) then begin
        if not LoadIcsDll1 then begin
            Display('OverbyteIcsDLL1.dll has not been loaded !');
            Exit;
        end;
    end;
    Display('Calling DLL1...');
    BufSize := 100;
    SetLength(Buffer, BufSize);
    Status := IcsDll1Demo(PAnsiChar(AnsiString(HostnameEdit.Text)),
                         PAnsiChar(AnsiString(PortEdit.Text)),
                         @Buffer[1], @BufSize);
    SetLength(Buffer, BufSize);
    if Status <> 0 then
        Display(string('Error #' + IntToStr(Status)));
    Display(string(Buffer));
    Display('Done with DLL1');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDllTestForm.LoadIcsDll2 : Boolean;
begin
    Result := FALSE;
    // Dynamically load IcsDll2.dll (Will be unloaded in FormDestroy)
    IcsDll2Handle := LoadLibrary('OverbyteIcsDll2.dll');
    if IcsDll2Handle = 0 then begin
        Application.MessageBox('OverbyteIcsDll2.dll not found', 'Error', MB_OK);
        Exit;
    end;

    // Locate IcsDllDemo entry point in the DLL
    IcsDll2Demo := GetProcAddress(IcsDll2Handle, 'IcsDllDemo');
    if @IcsDll2Demo = nil then begin
        Application.MessageBox('IcsDllDemo not found (OverbyteIcsDll2.dll)',
                               'Error', MB_OK);
        Exit;
    end;

    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.UnloadIcsDll2;
begin
    // If we had loaded the DLL, then unload it
    if IcsDll2Handle <> 0 then begin
        FreeLibrary(IcsDll2Handle);
        IcsDll2Handle := 0;
        IcsDll2Demo    := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.CallDll2ButtonClick(Sender: TObject);
var
    Buffer  : AnsiString;
    BufSize : Integer;
    Status  : Integer;
begin
    // First we must make sure we already loaded the DLL
    if not Assigned(IcsDll2Demo) then begin
        if not LoadIcsDll2 then begin
            Display('OverbyteIcsDll2.dll has not been loaded !');
            Exit;
        end;
    end;
    Display('Calling DLL2...');
    BufSize := 10000;
    SetLength(Buffer, BufSize);
    Status := IcsDll2Demo(PAnsiChar(AnsiString(UrlEdit.Text)),
                          @Buffer[1], @BufSize);
    // On return, a negative BufSize means the buffer was too small
    // a positive BufSize give the exact length of the received document
    if BufSize < 0 then
        Display('Buffer was too small !')
    else
        SetLength(Buffer, BufSize);
    // Status code 200 means "OK" in HTTP protocol. Anything else is an error
    if Status <> 200 then
        Display('Error #' + IntToStr(Status) + ' ' + String(Buffer))
    else
        Display(String(Buffer));      // No error, show the document
        
    Display('Done with DLL2');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
