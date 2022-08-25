{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
              Used code from Frank Neuhaus <neuhaus@cpa.de>.
Creation:     December 22, 1998
Version:      1.00
Object:       Demo for threaded TFtpCli use (not the best way to use the
              component, but some like to use threads).
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
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
Updates:

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsFtpThrd1;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    ExtCtrls, StdCtrls;

type
  TThrdFtpForm = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    StartButton: TButton;
    SaveButton: TButton;
    procedure StartButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  end;

  TTransferThread = class(TThread)
  private
     FMsg : String;
  public
     constructor CreateThread;
     procedure   AddToListBox;
     procedure   Display(Msg: String);
     procedure   CliDisplay(Sender: TObject; var Msg: String);
     procedure   Execute ; override;
  end;

var
  ThrdFtpForm: TThrdFtpForm;

implementation
{$R *.DFM}

uses
    OverbyteIcsFtpCli;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdFtpForm.StartButtonClick(Sender: TObject);
begin
     TTransferThread.CreateThread;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdFtpForm.SaveButtonClick(Sender: TObject);
begin
     ListBox1.Items.SaveToFile('FtpThrd.Log');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTransferThread.CreateThread;
begin
     FreeOnTerminate := TRUE;
     inherited Create(FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTransferThread.Execute;
var
    FtpClient1 : TFtpClient;
begin
    FtpClient1                    := TFtpClient.Create(nil);
    FtpClient1.OnDisplay          := CliDisplay;
    FtpClient1.OnProgress64       := nil;
    FtpClient1.OnRequestDone      := nil;
    FtpClient1.OnSessionConnected := nil;
    FtpClient1.OnSessionClosed    := nil;
    FtpClient1.OnStateChange      := nil;
    FtpClient1.Multithreaded      := TRUE;
    FtpClient1.Timeout            := 60; // default is 15

    FtpClient1.HostName           := 'localhost';
    FtpClient1.Port               := 'ftp';
    FtpClient1.UserName           := 'fpiette';
    FtpClient1.Password           := 'fp';
    FtpClient1.DisplayFileFlag    := TRUE;
    FtpClient1.LocalFileName      := 'c:\temp\data1.txt';
    FtpClient1.HostDirName        := 'c:\temp';
    FtpClient1.HostFileName       := 'tofile.txt';

    if not FtpClient1.Connect then begin
        Display('Connect failed');
        FtpClient1.Abort;
        Exit;
    end;
    if not FtpClient1.Put then begin
        Display('Put failed');
        FtpClient1.Abort;
        Exit;
    end;
    if not FtpClient1.Quit then begin
        Display('Quit failed');
        FtpClient1.Abort;
        Exit;
    end;
    Display('Finished ok.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTransferThread.CliDisplay(Sender: TObject; var Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTransferThread.Display(Msg : String);
begin
    FMsg := Msg;
    Synchronize(AddToListBox);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTransferThread.AddToListBox;
begin
    ThrdFtpForm.ListBox1.Items.Add(FMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

