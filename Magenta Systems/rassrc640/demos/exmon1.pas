unit exmon1;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas Monitor Example
=======================

Monitors for RAS connections started by other applications, and
allows hang-up.

Created by Angus Robertson, Magenta Systems Ltd, England
in 2000, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 20th August 2010 

To load this example, the TMagRas components need to have been previously
installed on the component palette. 

20 Aug 2010 - support RAS WideChar APIs and Unicode for Delphi 2009 and later
			  with new MagRasxxxW units from TMagRas v6 and later

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, magrasconw, magrasapiw;

type
  TForm1 = class(TForm)
    MagRasCon: TMagRasCon;
    doHangup: TButton;
    doExit: TButton;
    ConnLog: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    LabelConn: TLabel;
    LabelStat: TLabel;
    TimerStatus: TTimer;
    LabelOnline: TLabel;
    procedure doExitClick(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doHangupClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ConnHandle: HRasConn ;         	// handle for current connection
  ConnName: string ;                // name of current connection
  LastState: integer ;				// used to check if state has changed

implementation

{$R *.DFM}

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

// to monitor a RAS connection, you only need a timer event to check
// if there's an active RAS connection and then check it's state
// this timer is set for once per second, and may miss some state
// messages during dialling and authentication that happen very fast
// the timer internval could be shorter, but on Win9x this may overload RAS

procedure TForm1.TimerStatusTimer(Sender: TObject);
var
	newname: string ;
begin

// check for active connections - this version only returns one connection
// Win9x lists connection when it starts dialling
// WinNT/2K only list connection if it answers
	newname := MagRasCon.GetConnection ;

// no active connections, see if already closed down
	if newname = '' then
	begin
		if ConnHandle = 0 then exit ;
        ConnHandle := 0 ;
        doHangup.Enabled := false ;
        LabelStat.Caption := '' ;
        LabelOnline.Caption := 'Offline' ;
        ConnLog.Lines.Add ('Connection Offline') ;
        exit ;
    end ;

// handle has changed, that means a new call
	if ConnHandle <> MagRasCon.CurRASConn then
    begin
	    ConnHandle := MagRasCon.CurRASConn ;
    	ConnName := MagRasCon.CurConnName ;
	    LabelConn.Caption := ConnName ;
        doHangup.Enabled := true ;
        ConnLog.Lines.Add ('New Connection Found: ' + ConnName) ;
	end ;

// get state of current connection - if unchanged nothing more
    MagRasCon.CurrentStatusEx (ConnHandle, 0) ;
	if LastState = MagRasCon.ConnectState then exit ;
    LastState := MagRasCon.ConnectState ;
    ConnLog.Lines.Add (MagRasCon.StatusStr) ;

// something has changed, talk to user
// ConnectState can be checked against literals in MagRasApi
//    to determine current state of connection
	LabelStat.Caption := MagRasCon.StatusStr ;
    if (MagRasCon.ConnectState < RASCS_Connected) then
    									LabelOnline.Caption := 'Dialling' ;
	if (MagRasCon.ConnectState = RASCS_Connected) then
    									LabelOnline.Caption := 'Online' ;
	if (MagRasCon.ConnectState = RASCS_DisConnected) then
    									LabelOnline.Caption := 'Hang-Up' ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	LastState := 0 ;
	LabelConn.Caption := '' ;
    LabelStat.Caption := '' ;
    ConnLog.Lines.Clear ;

// see if RAS has been installed     
	if MagRasCon.TestRAS then
    begin
	    TimerStatusTimer (self) ;  // avoid waiting one second until timer expires
        TimerStatus.Enabled := true ;
	end
    else
    begin
	 	ConnLog.Lines.Add ('RAS is not installed') ;
    end ;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	// could check if still online and close connection
    Application.Terminate ;
end;

procedure TForm1.doHangupClick(Sender: TObject);
begin
    if ConnHandle = 0 then exit ;
	doHangup.Enabled := false ;

// disconnect, returns when done or after three seconds
	MagRasCon.DisconnectEx (ConnHandle, 0, 3000, false) ;
end;

end.
