{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This is a template for ICS demo projects.
Creation:     December 2009
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDemoTemplate1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  OverbyteIcsIniFiles;

type
  TTemplateForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    FInitialized : Boolean;
    FIniFile     : TIcsIniFile;
    procedure Display(const Msg: String);
  public
    property IniFile: TIcsIniFile read FIniFile;
  end;

var
  TemplateForm: TTemplateForm;

implementation

{$R *.dfm}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTemplateForm.FormCreate(Sender: TObject);
begin
{$IF RTLVersion >= 18}
    { Built-in memory leak detection and display since BDS2006 }
    { This is useful for debugging, however a bit slower.      }
    ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$IFEND}
    DisplayMemo.Clear;
    FIniFile := TIcsIniFile.Create(OverbyteIcsIniFiles.GetIcsIniFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTemplateForm.FormDestroy(Sender: TObject);
begin
    FIniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTemplateForm.FormShow(Sender: TObject);
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                               (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                               (Screen.Width  - Width)  div 2);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTemplateForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    try
        IniFile.WriteInteger(SectionWindow,   KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow,   KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow,   KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow,   KeyHeight,      Height);
        IniFile.UpdateFile;
    except
        on E: Exception do
            MessageDlg(E.ClassName + ' ' + E.Message, mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTemplateForm.Display(const Msg: String);
var
    I : Integer;
begin
    if not Assigned(DisplayMemo) then
        Exit;
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { This is much faster than deleting line by line of the memo }
            { however still slow enough to throttle ICS speed!           }
            with TStringList.Create do
            try
                BeginUpdate;
                Assign(DisplayMemo.Lines);
                for I := 1 to 50 do
                    Delete(0);
                DisplayMemo.Lines.Text := Text;
            finally
                Free;
            end;
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
