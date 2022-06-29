{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 02, 2000
Description:  This is a demonstration program for IcsDll1.dll. It will
              dynamically load the DLL, get IcsDllDemo entry point and call
              it. Then display result from DLL.
Version:      1.00
EMail:        francois.piette@pophost.eunet.be    francois.piette@swing.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@swing.be>

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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit DllTst_1;

{$IFDEF VER80}
'Sorry, this is a demo program calling a 32 bit DLL.'
'Upgrade to latest Delphi version to use it.'
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ExtCtrls, Buttons;

type
  TIcsDllDemo = function (HostName : PChar;
                          Port     : PChar;
                          Buffer   : PChar;
                          BufSize  : PInteger): Integer; stdcall;

  TDllTestForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    HostnameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PortEdit: TEdit;
    CallDllButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CallDllButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  DllTestForm : TDllTestForm;
  DllHandle   : THandle;
  IcsDllDemo  : TIcsDllDemo;

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
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.FormDestroy(Sender: TObject);
begin
    if DllHandle <> 0 then begin
        FreeLibrary(DllHandle);
        DllHandle := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Destroy;
        DisplayMemo.Clear;

        DllHandle := LoadLibrary('IcsDLL1.dll');
        if DllHandle = 0 then begin
            Application.MessageBox('ICSDLL1.DLL not found', 'Error', MB_OK);
            Application.Terminate;
            Exit;
        end;

        IcsDllDemo := GetProcAddress(DllHandle, 'IcsDllDemo');
        if @IcsDllDemo = nil then begin
            Application.MessageBox('IcsDllDemo not found (ICSDLL1.DLL)',
                                   'Error', MB_OK);
            Application.Terminate;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDllTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.Destroy;
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
procedure TDllTestForm.CallDllButtonClick(Sender: TObject);
var
    Buffer  : String;
    BufSize : Integer;
    Status  : Integer;
begin
    Display('Calling DLL...');
    BufSize := 100;
    SetLength(Buffer, BufSize);
    Status := IcsDllDemo(PChar(HostnameEdit.Text),
                         PChar(PortEdit.Text),
                         @Buffer[1], @BufSize);
    SetLength(Buffer, BufSize);
    if Status <> 0 then
        Display('Error #' + IntToStr(Status));
    Display(Buffer);
    Display('Done with DLL');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
