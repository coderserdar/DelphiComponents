unit exdialc1;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas Dialling Complex Example
================================

Dial, monitor and hang-up a RAS connection, allowing the user to change
the telephone number and logon details, and dialling location.

Created by Angus Robertson, Magenta Systems Ltd, England
in 2001, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 20th August 2010 

To load this example, the TMagRas components need to have been previously
installed on the component palette.

3 Jan 2001 - replaced MagRasConStateChanged with MagRasConStateEvent
           - corrected some state events suppressed, particularly with multilink
           - display dial info for separate sub entries with multilink
30 Apr 2001 - no need to sort phonebook list
24 May 2007 - set PBLocation before dialling
            - use MagRasGetEntryList (in magrasent) instead of MagRasCon.GetPhoneBookEntries
4 Aug 2009  - tested with Delphi 2009 
20 Aug 2010 - support RAS WideChar APIs and Unicode for Delphi 2009 and later
			  with new MagRasxxxW units from TMagRas v6 and later

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, magrasconw, magrasentw, magrasapiw;

type
  TForm1 = class(TForm)
    MagRasCon: TMagRasCon;
    doHangup: TButton;
    doExit: TButton;
    ConnLog: TMemo;
    Label2: TLabel;
    LabelConn: TLabel;
    LabelStat: TLabel;
    TimerStatus: TTimer;
    LabelOnline: TLabel;
    ConnList: TListBox;
    Label3: TLabel;
    doDial: TButton;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label43: TLabel;
    Label45: TLabel;
    Label44: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label12: TLabel;
    LabelNumberDisp: TLabel;
    LabelNumberDial: TLabel;
    entUserName: TEdit;
    entPassword: TEdit;
    entUseCountryandAreaCodes: TCheckBox;
    entCountryCode: TEdit;
    entAreaCode: TEdit;
    entLocalNumber: TEdit;
    entCanonNumber: TEdit;
    doPropDial: TButton;
    LabeDialLocation: TLabel;
    LabelPBLocation: TLabel;
    procedure doExitClick(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doHangupClick(Sender: TObject);
    procedure doDialClick(Sender: TObject);
    procedure ConnListClick(Sender: TObject);
    procedure entUseCountryandAreaCodesClick(Sender: TObject);
    procedure NumberChange(Sender: TObject);
    procedure entCanonNumberChange(Sender: TObject);
    procedure doPropDialClick(Sender: TObject);
    procedure MagRasConStateEvent(Sender: TObject; CallState: TRasStateRec);
  private
    { Private declarations }
	procedure GetDialProps ;
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
  DialProps: TDialProps;       		// dialling properties
  DialLocation: TDialLocation;  	// default dialling location
  DialCard: TDialCard ;		 		// default dialling calling card
  DialCountry: TDialCountry ;		// default country dialling info

implementation

{$R *.DFM}

procedure TForm1.GetDialProps ;
begin
    MagRasCon.GetTransCaps (DialProps, DialLocation, DialCard, DialCountry) ;
    LabeDialLocation.Caption := 'Dialling from ' + DialLocation.LocationName ;
end ;

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

// main event handler, while this is being processed RAS will wait
// called in response to CurrentStatusEx in the timer event, or
// while dialling a call (events much faster)

procedure TForm1.MagRasConStateEvent(Sender: TObject; CallState: TRasStateRec);
var
	info: string ;
    teststate: integer ;
begin

// check type of event
	info := '' ;
    if CallState.StateSubEntry <= 0 then CallState.StateSubEntry := 1 ;
    case CallState.StateEventSource of
    	SourceDial: info := 'Dial (Ch ' +
                            IntToStr (CallState.StateSubEntry) + '): ' ;
    	SourceStatus: info := 'Status: ' ;
    	SourceHangup: info := 'Hangup: ' ;
	end ;

// see if new event from same subentry and source, else display it
// this test is so that the same status message is not displayed each
// second when the timer triggers status checking
    teststate := Succ (CallState.ConnectState) * CallState.StateSubEntry *
                                     Succ (Ord (CallState.StateEventSource)) ;
	if LastState = teststate then exit ;
    LastState := teststate ;
    ConnLog.Lines.Add (info + CallState.StatusStr
					      +	' (' + IntToStr (CallState.ConnectState) + ')') ;

// something has changed, talk to user
// ConnectState can be checked against literals in MagRasApi
//    to determine current state of connection
	LabelStat.Caption := MagRasCon.StatusStr ;
    if (CallState.ConnectState < RASCS_Connected) then
    									LabelOnline.Caption := 'Dialling' ;
	if (CallState.ConnectState = RASCS_Connected) then
    									LabelOnline.Caption := 'Online' ;
	if (CallState.ConnectState = RASCS_DisConnected) then
    									LabelOnline.Caption := 'Hang-Up' ;

// warning - this function does not handle multilink call failures, where
// one channel is OK, but the other fails - hangup will stop both 

// if dialling need to see what's happened
	if DialHandle <> 0 then
    begin

	// online OK, get IP addresses and restart timer
		if (CallState.ConnectState = RASCS_Connected) then
        begin
            ConnHandle := DialHandle ;
			DialHandle := 0 ;
			if MagRasCon.GetProtocolEx (ConnHandle) = 0 then
            begin
              	ConnLog.Lines.Add ('Protocol: ' + MagRasCon.ConnProtocol) ;
              	ConnLog.Lines.Add ('IP Address: ' +  MagRasCon.ClientIP +
				              					' > ' + MagRasCon.ServerIP) ;
        	  	ConnLog.Lines.Add ('PPP Reply Message : ' +
                                                   MagRasCon.PPPReplyMessage) ;
			end ;
    	    TimerStatus.Enabled := true ;
		end ;

	// dialling failed, either an error or disconnected
		if ((CallState.ConnectState > RASBase) and
	            (CallState.ConnectState < RASCS_Paused)) or
	             (CallState.ConnectState = RASCS_Disconnected) then
		begin
	// disconnect, returns when done or after three seconds, no events
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

	// get dialling properties, location, calling card, etc
		GetDialProps ;

	// clear fields
        entCanonNumber.Text := '' ;
	    entUserName.Text := '' ;
	    entPassword.Text := '' ;
    	entUseCountryandAreaCodes.Checked := false ;
	    entCountryCode.Text := '' ;
    	entAreaCode.Text := '' ;
	    entLocalNumber.Text := '' ;

	// start monitoring
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
	dispnum, dialnum: String ;
begin
	if ConnList.ItemIndex < 0 then exit ;
    if DialName = '' then exit ;
  	ConnLog.Lines.Add ('Starting to Dial Connection: ' + DialName) ;
    ConnLog.Lines.Add ('Phonebook Location: ' +
                             MasRasPBLocationStr [MagRasCon.PBLocation]) ;  // 5.21 show location
// note, already got all dial and entry stuff in ConnListClick

// stop timer since dialling creates events
    TimerStatus.Enabled := false ;
    doDial.Enabled := false ;

// dial params that may have been changed by user
// note the canonical number has already been assembled in the NumberChange event
	MagRasCon.TranslateAddr (0, entCanonNumber.Text, dispnum, dialnum) ;
   	with MagRasCon do
    begin
    	UserName := entUserName.Text ;
    	Password := entPassword.Text ;
		PhoneNumber := dialnum ;   // translated number
        SubEntry := 0 ;            // dial all subentries (if allowed)
 	end ;

// start connection (sets handle)
	DialHandle := 0 ;
  	if MagRasCon.ConnectEx (DialHandle) <> 0 then
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

procedure TForm1.ConnListClick(Sender: TObject);
var
	I, errcode, Location: integer ;
begin
	if ConnList.ItemIndex = -1 then exit ;
// entry to dial
    I := ConnList.ItemIndex ;
    DialName := MagRasEntryRecs [I].EntryName ;
    Location := MagRasEntryRecs [I].PBLocation ;  // 5.21 get phonebook location
    LabelConn.Caption := DialName ;
    LabelPBLocation.Caption := 'Phonebook Location: ' +
                           MasRasPBLocationStr [Location] ;  // 5.21 show location

// set phonebook entry to access
	MagRasCon.EntryName := DialName ;
    MagRasCon.PBLocation := Location ;   // 5.21 keep location

// read entry and dial properties
	errcode := MagRasCon.GetDialParams ;
    if errcode = 0 then errcode := MagRasCon.GetEntryProperties ;
    if errcode <> 0 then
    begin
		ConnLog.Lines.Add (MagRasCon.StatusStr) ;
        beep ;
        exit ;
    end ;

// set-up edit boxes
	with MagRasCon do
	begin

    // if not going to edit number, could get PhoneCanonical property instead of bits
   	// entCanonNumber.Text := PhoneCanonical ;

	// Location and phone number, including alternates
    // event handler sets canonical number
		entUseCountryAndAreaCodes.Checked := UseCountryAndAreaCodes ;
        entCountryCode.Text := IntToStr (CountryCode) ;
		entAreaCode.Text := AreaCode ;
		entLocalNumber.Text := LocalPhoneNumber ;

	// dial params
      	entUserName.Text := UserName ;
    	entPassword.Text := Password ;

	end ;
	entUseCountryandAreaCodesClick (self) ;   // set a few fields
end;

procedure TForm1.entUseCountryandAreaCodesClick(Sender: TObject);
begin
   	entCountryCode.Enabled := entUseCountryAndAreaCodes.Checked ;
   	entAreaCode.Enabled := entUseCountryAndAreaCodes.Checked ;
   	entCanonNumberChange (self) ;
end;

procedure TForm1.NumberChange(Sender: TObject);
begin
	if entCountryCode.Text = '' then entCountryCode.Text := '0' ;
	entCanonNumber.Text := MagRasCon.GetCanonical
     	 (entUseCountryAndAreaCodes.Checked, StrToInt (entCountryCode.Text),
			            			   entAreaCode.Text, entLocalNumber.Text) ;
   	entCanonNumberChange (self) ;
end;

procedure TForm1.entCanonNumberChange(Sender: TObject);
var
	DispNum, DialNum: String ;
begin
	MagRasCon.TranslateAddr (0, entCanonNumber.Text, DispNum, DialNum) ;
   	LabelNumberDisp.Caption := 'Display Number: ' + DispNum ;
   	LabelNumberDial.Caption := 'Dialable Number: ' + DialNum ;
end;

procedure TForm1.doPropDialClick(Sender: TObject);
begin
	MagRasCon.TranslateDialog (Handle, 0, '') ;
	GetDialProps ;
   	entCanonNumberChange (self) ;
end;


end.
