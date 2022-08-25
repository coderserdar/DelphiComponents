{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE (From a work done by Ed Hochman <ed@mbhsys.com>)
Creation:     Jan 13, 1998
Version:      1.02
Description:  HttpThrd is a demo program showing how to use THttpCli component
              in a multi-threaded program.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2011 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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

Updates:
Jun 19 2011 V1.01 Arno - Make use of an event object rather than
                  TThread.Suspend/Resume, both are deprecated since D2010.
Jun 20 2011 V1.02 Arno reworked it, was needed.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpThr1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsHttpProt, StdCtrls, OverbyteIcsIniFiles,
  OverbyteIcsHttpThr2;  // The thread class is defined there;

const
  WM_THREAD_RESULT   = WM_USER + 1;
  WM_LOG             = WM_USER + 2;

type
  THttpThreadForm = class(TForm)
    URLEdit: TEdit;
    ResultsMemo: TMemo;
    DoItButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Thread0Label: TLabel;
    Thread1Label: TLabel;
    Thread2Label: TLabel;
    Thread3Label: TLabel;
    Thread4Label: TLabel;
    Thread5Label: TLabel;
    ProgressListBox: TListBox;
    Label14: TLabel;
    ProxyEdit: TEdit;
    Label15: TLabel;
    procedure DoItButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    FInitialized : Boolean;
    FIniFileName : String;
    FTerminating : Boolean;
    procedure DisplayThreadState(Which : Integer; State: TThreadState);
  protected
    procedure WmLog(var Msg: TMessage); message WM_LOG;
    procedure WmThreadResult(var Msg: TMessage); message WM_THREAD_RESULT;
  public
    procedure ProcessResults(AThread: THTTPThread; Success : Boolean);
  end;

const
    SectionData   = 'Data';
    KeyURL        = 'URL';
    KeyProxy      = 'Proxy';
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';

var
    HttpThreadForm: THttpThreadForm;

implementation

{$R *.DFM}

uses
    OverbyteIcsStreams;

const
    MaxThreads    = 6;    { If you change this, change labels on the form }
var
    { The array with all our threads components }
    ThreadsObjects : array [0..MaxThreads - 1] of THTTPThread;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.FormCreate(Sender: TObject);
var
    i: Integer;
begin
    { Initialize the array with zeros (nil it) }
    FillChar(ThreadsObjects[0], SizeOf(ThreadsObjects), 0);
    FIniFileName := GetIcsIniFileName;    
    for i := Low(ThreadsObjects) to High(ThreadsObjects) do
        DisplayThreadState(i, tsInvalid);  //None created and ready yet.
    ResultsMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.FormDestroy(Sender: TObject);
var
    i: Integer;
begin
    FTerminating := True;
    { Free all THttpThread objects, this is blocking }
    for i := Low(ThreadsObjects) to High(ThreadsObjects) do begin
        if ThreadsObjects[i] <> nil then
            ThreadsObjects[i].Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized   := TRUE;
        IniFile        := TIcsIniFile.Create(FIniFileName);
        try
            URLEdit.Text   := IniFile.ReadString(SectionData, KeyURL,
                                             'http://www.overbyte.be');
            ProxyEdit.Text := IniFile.ReadString(SectionData, KeyProxy, '');
            Top            := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
            Left           := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
            Width          := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height         := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        finally
            IniFile.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteString(SectionData, KeyURL,       URLEdit.Text);
        IniFile.WriteString(SectionData, KeyProxy,     proxyEdit.Text);
        IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.DoItButtonClick(Sender: TObject);
var
    i: Integer;
    LThread : THTTPThread;
begin
    for i := Low(ThreadsObjects) to High(ThreadsObjects) do begin
        if ThreadsObjects[i] = nil then begin
            LThread := THTTPThread.Create(i);
            { Setting the State is safe since we read/write it only from main thread }
            LThread.State := tsReady;
            DisplayThreadState(i, tsReady);
        {$IF CompilerVersion >= 21}
            LThread.Start;
        {$ELSE}
            LThread.Resume;
        {$IFEND}
            ThreadsObjects[i] := LThread;
        end
        else
            LThread := ThreadsObjects[i];

        if LThread.State = tsReady then begin
            { Writing the next tree properties is safe only in State tsReady }
            LThread.URL   := UrlEdit.Text;
            LThread.Proxy := ProxyEdit.Text;
            LThread.State := tsBusy;
            DisplayThreadState(i, tsBusy);
            //get the page
            LThread.Wakeup;  //Signal the event in Execute method
            Exit;            //For now, only one per click
        end;
    end;
    MessageBeep(MB_OK);
    ShowMessage('No more threads available');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.DisplayThreadState(Which : Integer;
  State: TThreadState);
begin
    case which of
        0: Thread0Label.Caption := IntToStr(Ord(State));
        1: Thread1Label.Caption := IntToStr(Ord(State));
        2: Thread2Label.Caption := IntToStr(Ord(State));
        3: Thread3Label.Caption := IntToStr(Ord(State));
        4: Thread4Label.Caption := IntToStr(Ord(State));
        5: Thread5Label.Caption := IntToStr(Ord(State));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.WmLog(var Msg: TMessage);
var
    LThread: THTTPThread;
    LLogList: TStringList;
begin
    { A thread notification message that log data is available in the log list }
    if not FTerminating then begin
        { We can be sure that the thread object is valid }
        LThread := THTTPThread(Msg.WParam);
        { Access to the log list is protected by a critical section }
        LLogList := LThread.LockLogList;
        try
            ProgressListBox.Items.AddStrings(LLogList);
            LLogList.Clear;
        finally
            LThread.UnlockLogList;
        end;
    end;
    if ProgressListBox.Items.Count > 0 then
        ProgressListBox.ItemIndex := ProgressListBox.Items.Count -1
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.WmThreadResult(var Msg: TMessage);
begin
    { A thread notification message that GET has finished }
    if not FTerminating then
        { We can be sure that the thread object is valid }
        ProcessResults(THTTPThread(Msg.WParam), Boolean(Msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadMemoFromStream(Memo : TMemo; Stream : TStream);
var
    Reader : TIcsStreamReader;
    Line : String;
begin
    Memo.Lines.BeginUpdate;
    try
        Stream.Position := 0;
        Reader := TIcsStreamReader.Create(Stream, TRUE, CP_ACP);
        try
            while Reader.ReadLine(Line) do
                Memo.Lines.Add(Line);
        finally
            Reader.Free;
        end;
    finally
        Memo.Lines.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.ProcessResults(AThread : THTTPThread;
  Success : Boolean);
begin
    AThread.State := tsReady;
    DisplayThreadState(AThread.ThreadNumber, AThread.State);
    { We know this is safe since the thread currently does nothing, }
    { waiting for the event being signaled.                         }
    if Success then begin
        ResultsMemo.Lines.Add('* * * * * * THREAD ' +
                    IntToStr(AThread.ThreadNumber) + ' * * * * * *');
        LoadMemoFromStream(ResultsMemo, AThread.DataStream);
        ResultsMemo.Lines.Add('');
    end
    else begin
        { There was an error getting data.                          }
        ResultsMemo.Lines.Add('Nothing returned by thread: ' +
                              IntToStr(AThread.ThreadNumber));
    end;
    ResultsMemo.Lines.Add('* * * * * * * * * * * * * * * * * * * *');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

