{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  TIcsTimeServer and TIcsTimeClient, time server and client,
              supporting time protocol with UDP and TCP portions of RFC868,
              and SNTP v4 (Simple Network Time Protocol) to RFC2030.
              Note current SNTP is RFC4330 but not looked at it.
              Note that full NTP is not supported.
Creation:     Jan 2019
Updated:      Mar 2019
Version:      8.60
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
Legal issues: Copyright (C) 2002-2019 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

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



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTimeTst1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ExtCtrls, OverbyteIcsWinsock,
  OverbyteIcsWSocket, OverbyteIcsUtils, OverbyteIcsSntp,
  OverbyteIcsWndControl;


type
  TTimeDemoForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    doClose: TButton;
    IcsTimeServer1: TIcsTimeServer;
    IcsTimeClient1: TIcsTimeClient;
    GroupBox1: TGroupBox;
    ServerIP: TComboBox;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    ClientTimeServer: TComboBox;
    Label2: TLabel;
    doServerStart: TButton;
    doServerStop: TButton;
    ClientProtocol: TRadioGroup;
    doClientStart: TButton;
    ServerProtoSntp: TCheckbox;
    ServerProtoTcp: TCheckbox;
    ServerProtoUdp: TCheckbox;
    doClientAbort: TButton;
    doCorrect: TButton;
    LabelCorrection: TLabel;
    SockFamily: TRadioGroup;
    procedure doCloseClick(Sender: TObject);
    procedure IcsTimeClient1Time(Sender: TObject; DateTime: TDateTime);
    procedure IcsTimeServer1QueryDone(Sender: TObject; Error: Word);
    procedure IcsTimeServer1Start(Sender: TObject);
    procedure IcsTimeServer1Stop(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doServerStartClick(Sender: TObject);
    procedure IcsTimeServer1Query(Sender: TObject; SocketAddr: sockaddr_in;
      TimeProtocol: TTimeProtocol; var Continue: Boolean);
    procedure doClientStartClick(Sender: TObject);
    procedure doClientAbortClick(Sender: TObject);
    procedure doServerStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doCorrectClick(Sender: TObject);
    procedure IcsTimeClient1TimeInfo(Sender: TObject; Info: string);
  private
  end;

var
  TimeDemoForm: TTimeDemoForm;


implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeDemoForm.doCloseClick(Sender: TObject);
begin
    Close;
end;

procedure TTimeDemoForm.doServerStartClick(Sender: TObject);
begin
    IcsTimeServer1.Addr := ServerIP.Text; 
    IcsTimeServer1.TimeProtocol := [];
    if ServerProtoSntp.Checked then
        IcsTimeServer1.TimeProtocol := IcsTimeServer1.TimeProtocol + [tpSNTP];
    if ServerProtoTCP.Checked then
        IcsTimeServer1.TimeProtocol := IcsTimeServer1.TimeProtocol + [tpTCP];
    if ServerProtoUdp.Checked then
        IcsTimeServer1.TimeProtocol := IcsTimeServer1.TimeProtocol + [tpUDP];
    try
        IcsTimeServer1.Start;
    except
          DisplayMemo.Lines.Add ('Failed to start time server: ' + IcsGetExceptMess(ExceptObject));
    end;
end;

procedure TTimeDemoForm.doServerStopClick(Sender: TObject);
begin
    IcsTimeServer1.Stop;
end;

procedure TTimeDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    IcsTimeServer1.Stop;
end;

procedure TTimeDemoForm.FormCreate(Sender: TObject);
begin
    ServerIP.Items.Assign(LocalIPList);
end;

procedure TTimeDemoForm.doClientAbortClick(Sender: TObject);
begin
    IcsTimeClient1.Abort;
end;

procedure TTimeDemoForm.doClientStartClick(Sender: TObject);
begin
    doCorrect.Enabled := False;
    LabelCorrection.Caption := 'Correction Needed?';
    IcsTimeClient1.ServerAddr := ClientTimeServer.Text;
    IcsTimeClient1.SocketFamily := TSocketFamily(SockFamily.ItemIndex);
    IcsTimeClient1.TimeProtocol := TTimeProtocol(ClientProtocol.ItemIndex);
    IcsTimeClient1.TimeoutSecs := 5;
    DisplayMemo.Lines.Add ('Starting time request to: ' + IcsTimeClient1.ServerAddr);
    try
        if NOT IcsTimeClient1.GetTime then
            DisplayMemo.Lines.Add ('Time request failed')
        else begin
            doClientStart.Enabled := False;
        end;
    except
          DisplayMemo.Lines.Add ('Time request exception: ' + IcsGetExceptMess(ExceptObject));
    end;
end;


procedure TTimeDemoForm.doCorrectClick(Sender: TObject);
begin
    IcsTimeClient1.ServerAddr := ClientTimeServer.Text;
    IcsTimeClient1.TimeProtocol := TTimeProtocol(ClientProtocol.ItemIndex);
    IcsTimeClient1.TimeoutSecs := 10;
    IcsTimeClient1.SocketFamily := sfAnyIPv4; // prefer IPv4
    DisplayMemo.Lines.Add ('Starting correction request to: ' + IcsTimeClient1.ServerAddr);
    try
        if NOT IcsTimeClient1.GetTime then
            DisplayMemo.Lines.Add ('Time request failed')
        else begin
            doClientStart.Enabled := False;
        end;
    except
          DisplayMemo.Lines.Add ('Time request exception: ' + IcsGetExceptMess(ExceptObject));
    end;
end;

procedure TTimeDemoForm.IcsTimeClient1Time(Sender: TObject; DateTime: TDateTime);
var
    OldUtcDT, NewUtcDT: TDateTime;
begin
    with Sender as TIcsTimeClient do
    begin
        if DateTime = 0 then begin
            DisplayMemo.Lines.Add ('Unable to Get Network Time, Server: ' +
                        ServerAddr + ' [' + IpAddress + '] - ' + LastProgress);
            doCorrect.Enabled := False;
        end
        else
        begin
            DisplayMemo.Lines.Add ('Response from Server: ' + ServerAddr +
                        ' [' + IpAddress + '], Attempts ' + IntToStr (Attempts)) ;
            DisplayMemo.Lines.Add ('UTC/GMT ' + IcsDateTimeZZtoStr (DTTransmit));
            DisplayMemo.Lines.Add ('Difference: ' + SecsDiffDisp +
                 ' secs, Roundtrip: ' + IcsIntToCStr (MsecsRoundtrip) + 'ms');
            DisplayMemo.Lines.Add ('Version=' + IntToStr (ServVN) +
                 ', Mode=' + IntToStr (ServMode) +
                 ', Statum=' + IntToStr (SNTP.Statum) +
                 ', Precision=' + IntToStr (SNTP.Precision) +
                 ', Root Delay=' + IntToStr (SNTP.RootDelay)) ;
            DisplayMemo.Lines.Add ('Local PC: UTC/GMT ' + IcsDateTimeZZtoStr (IcsGetUTCTime)) ;
            LabelCorrection.Caption := 'Correction Needed: ' + SecsDiffDisp + ' secs';
            if IcsTimeClient1.TimeProtocol <> tpSNTP then
                DisplayMemo.Lines.Add ('Beware TIME protocol only accurate to one second')
            else begin

              // see if correcting real PC clock, requires administrator rights
                if (SecsDifference <> 0) and (MsecsRoundtrip < 500) then begin

                  // correct PC clock by time difference, not absolute  due to roundtrip delays
                    if doCorrect.Enabled then begin
                        doCorrect.Enabled := False;
                        OldUtcDT := IcsGetUTCTime;
                        NewUtcDT := IcsGetNewTime (OldUtcDT, DTDifference);
                        DisplayMemo.Lines.Add ('!!! Changing system time from UTC: ' +
                                          IcsDateTimeZZtoStr (OldUtcDT) +
                                              ' to ' + IcsDateTimeZZtoStr (NewUtcDT));
                        if NOT IcsSetUTCTime (NewUtcDT) then // update system time !!!!
                            DisplayMemo.Lines.Add ('Clock correction failed, requires administrator rights');
                        DisplayMemo.Lines.Add ('Local PC: UTC/GMT ' +
                                        IcsDateTimeZZtoStr (IcsGetUTCTime));
                    end
                    else
                        doCorrect.Enabled := True;  // correct next time
                end;
            end;
        end ;
    end ;
    doClientStart.Enabled := True;
    DisplayMemo.Lines.Add ('');
end;

procedure TTimeDemoForm.IcsTimeClient1TimeInfo(Sender: TObject; Info: string);
begin
    DisplayMemo.Lines.Add (Info); 
end;

procedure TTimeDemoForm.IcsTimeServer1Query(Sender: TObject;
  SocketAddr: sockaddr_in; TimeProtocol: TTimeProtocol; var Continue: Boolean);
var
    RemAddr, Prot: string ;
begin
    RemAddr := WSocketIPv4ToStr (SocketAddr.sin_addr.S_addr) ;
    if TimeProtocol = tpUDP then
        Prot := 'UDP'
    else if TimeProtocol = tpTCP then
        Prot := 'TCP'
    else if TimeProtocol = tpSNTP then
        Prot := 'SNTP'
    else
        prot := '??' ;
    DisplayMemo.Lines.Add ('Time Request from ' + RemAddr + ' using ' + Prot) ;
end;

procedure TTimeDemoForm.IcsTimeServer1QueryDone(Sender: TObject; Error: Word);
var
    RemAddr: string ;
begin
    if Error = 0 then exit ;
    RemAddr := WSocketIPv4ToStr ((Sender as TIcsTimeServer).SrcIPAddr.sin_addr.S_addr) ;
    DisplayMemo.Lines.Add ('Time Server Request Error ' + IntToStr (Error) + ' to ' + RemAddr) ;
end;

procedure TTimeDemoForm.IcsTimeServer1Start(Sender: TObject);
begin
    DisplayMemo.Lines.Add ('Time server started');
end;

procedure TTimeDemoForm.IcsTimeServer1Stop(Sender: TObject);
begin
    DisplayMemo.Lines.Add ('Time server stopped');
end;





{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

