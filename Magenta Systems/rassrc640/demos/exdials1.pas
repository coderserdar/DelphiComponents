unit exdials1;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas Dialling Simple Example
===============================

Dial, monitor and hang-up a RAS connection using information 
already specified in the phonebook entry. 

Created by Angus Robertson, Magenta Systems Ltd, England
in 2000, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 20th August 2010 

To load this example, the TMagRas components need to have been previously
installed on the component palette.

24 May 2007 - set PBLocation before dialling
            - use MagRasGetEntryList (in magrasent) instead of MagRasCon.GetPhoneBookEntries
20 Aug 2010 - support RAS WideChar APIs and Unicode for Delphi 2009 and later
			  with new MagRasxxxW units from TMagRas v6 and later


}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, 
  magrasconw, magrasentw, magrasapiw;

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
    ConnList: TListBox;
    Label3: TLabel;
    doDial: TButton;
    procedure doExitClick(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doHangupClick(Sender: TObject);
    procedure doDialClick(Sender: TObject);
    procedure MagRasConStateChanged(Sender: TObject);
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
  DialHandle: HRasConn ;         	// handle for dialled connection
  DialName: string ;                // name of dialled connection

implementation

{$R *.DFM}

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

// main event handler, while this is being processed RAS will wait
// called in response to CurrentStatusEx in the timer event, or
// while dialling a call (events much faster)

procedure TForm1.MagRasConStateChanged(Sender: TObject);
var
	info: string ;
begin

// check type of event
	info := '' ;
    case MagRasCon.StateEventSource of
    	SourceDial: info := 'Dial: ' ;
    	SourceStatus: info := 'Status: ' ;
    	SourceHangup: info := 'Hangup: ' ;
	end ;

// see if new event, else display it
	if LastState = MagRasCon.ConnectState then exit ;
    LastState := MagRasCon.ConnectState ;
    ConnLog.Lines.Add (info + MagRasCon.StatusStr
						      +	' (' + IntToStr (LastState) + ')') ;

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

// if dialling need to see what's happened
	if DialHandle <> 0 then
    begin

	// online OK, restart timer
		if (MagRasCon.ConnectState = RASCS_Connected) then
        begin
            ConnHandle := DialHandle ;
			DialHandle := 0 ;
    	    TimerStatus.Enabled := true ;
		end ;

	// dialling failed, either an error or disconnected
		if ((MagRasCon.ConnectState > RASBase) and
	            (MagRasCon.ConnectState < RASCS_Paused)) or
	             (MagRasCon.ConnectState = RASCS_Disconnected) then
		begin
	// disconnect, returns when done or after three seconds, no StateChanged
            ConnHandle := DialHandle ;
			DialHandle := 0 ;
			MagRasCon.DisconnectEx (ConnHandle, 0, 3000, false) ;
	        TimerStatus.Enabled := true ;
            // reset is done in timer event
		end ;
	end ;
end;

// to monitor a RAS connection, you only need a timer event to check
// if there's an active RAS connection and then check it's state
// this timer is set for once per second, and may miss some state
// messages during dialling and authentication that happen very fast
// the timer internval could be shorter, but on Win9x this may overload RAS

procedure TForm1.TimerStatusTimer(Sender: TObject);
begin

// check for active connections
// Win9x lists connection when it starts dialling
// WinNT/2K only list connection if it answers
	MagRasCon.GetConnections ;

// details of active connections are now available in Connections list
// no active connections, see if already closed down
	if MagRasCon.Connections.Count = 0 then
	begin
		if ConnHandle = 0 then exit ;
        ConnHandle := 0 ;
        DialName := '' ;
        doHangup.Enabled := false ;
        doDial.Enabled := true ;
        LabelStat.Caption := '' ;
        LabelOnline.Caption := 'Offline' ;
        ConnLog.Lines.Add ('Connection Offline') ;
        exit ;
    end ;

// connection list has changed, that means a new call
	if MagRasCon.ConnChangedFlag then
    begin
      // assume only a single connection (there may be more)
	    ConnHandle := MagRasCon.Connections.RasConn (0) ;
    	ConnName := MagRasCon.Connections.EntryName (0) ;
	    LabelConn.Caption := ConnName ;
        doHangup.Enabled := true ;
        if DialName <> ConnName then
	        ConnLog.Lines.Add ('New Connection Found: ' + ConnName) ;
	end ;

// get state of current connection
// calls StateChanged event where all checking is done
    MagRasCon.CurrentStatusEx (ConnHandle, 0) ;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
    I: integer ;
begin
	LastState := 0 ;
	LabelConn.Caption := '' ;
    LabelStat.Caption := '' ;
    ConnLog.Lines.Clear ;
// see if RAS has been installed
	if MagRasCon.TestRAS then
    begin
    // get list of phonebook entries
	//	MagRasCon.GetPhoneBookEntries ;   5.21 gone
    //  MagRasCon.PhoneBookEntries.Sort ;  4.60 already sorted
	//	ConnList.Items.Assign (MagRasCon.PhoneBookEntries) ;	 // display it  5.21 gone

    // 5.21 get connection entries
        ConnList.Items.Clear;
        I := MagRasGetEntryList ('') ;
        if (I <> 0) then
            ConnLog.Lines.Add ('Failed to Read Connection Entries - ' +
                                                MagRasCon.GetErrorString (I))
        else if (MagRasNumEntryRec = 0) then
            ConnLog.Lines.Add ('No Connection Entries Found')
        else
        begin
            for I := 0 to Pred (MagRasNumEntryRec) do
                ConnList.Items.Add (MagRasEntryRecs [I].EntryName) ;
        end ;
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
    if (DialHandle = 0) and (ConnHandle = 0) then exit ;
	doHangup.Enabled := false ;
    doDial.Enabled := true ;

// disconnect, returns when done or after three seconds, calls StateChanged
	if ConnHandle = 0 then ConnHandle := DialHandle ;
	MagRasCon.DisconnectEx (ConnHandle, 0, 3000, true) ;
end;

procedure TForm1.doDialClick(Sender: TObject);
var
    I, Location: integer ;
begin
	if ConnList.ItemIndex < 0 then exit ;

// entry to dial
    I := ConnList.ItemIndex ;
    DialName := MagRasEntryRecs [I].EntryName ;
    Location := MagRasEntryRecs [I].PBLocation ;  // 5.21 get phonebook location
    LabelConn.Caption := DialName ;

// stop timer since dialling creates events
    TimerStatus.Enabled := false ;
    doDial.Enabled := false ;

// set phonebook entry to dial
	MagRasCon.EntryName := DialName ;
    MagRasCon.PBLocation := Location ;   // 5.21 keep location
	MagRasCon.PhoneNumber :=	'' ;  // use the one in the phonebook
  	ConnLog.Lines.Add ('Starting to Dial Connection: ' + DialName) ;
    ConnLog.Lines.Add ('Phonebook Location: ' +
                             MasRasPBLocationStr [MagRasCon.PBLocation]) ;  // 5.21 show location

// start connection (sets handle)
	DialHandle := 0 ;
  	if MagRasCon.AutoConnectEx (DialHandle) <> 0 then
	begin
        // fails here is dialling did not even start
		ConnLog.Lines.Add  ('Dialling Failed - ' + MagRasCon.StatusStr) ;
		beep ;
        TimerStatus.Enabled := true ;
	    doDial.Enabled := true ;
        exit ;
	end ;

// dialling started OK
// dial connection or failure is checked in StateChanged event handler
	doHangup.Enabled := true ;
end;


end.
