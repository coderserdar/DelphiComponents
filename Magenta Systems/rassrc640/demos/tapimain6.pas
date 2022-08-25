unit tapimain6;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// Unicode demo when compiled with Delphi 2009

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MagTapiW, MagTapiApiW, ComCtrls ;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    DevList: TListBox;
    doTAPIMon: TButton;
    doConfig: TButton;
    doTranslate: TButton;
    NumUser: TEdit;
    NumDisp: TEdit;
    Label5: TLabel;
    doTranAddr: TButton;
    Label6: TLabel;
    NumDial: TEdit;
    Status: TStatusBar;
    Label1: TLabel;
    LabelVersion: TLabel;
    doClose: TButton;
    Memo2: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    doListModems: TButton;
    doDumpCaps: TButton;
    SaveDump: TSaveDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure doTAPIMonClick(Sender: TObject);
    procedure doConfigClick(Sender: TObject);
    procedure doTranslateClick(Sender: TObject);
    procedure doTranAddrClick(Sender: TObject);
    procedure doCloseClick(Sender: TObject);
    procedure doListModemsClick(Sender: TObject);
    procedure doDumpCapsClick(Sender: TObject);
  private
    { Private declarations }
    function SelectedDevNr: integer ;
  public
    { Public declarations }
    procedure StatusEvent (state, devid, callh: DWORD;
                                    mess, info1, info2: string) ;
    procedure DiagEvent (mess: string) ;
  end;


var
  Form1: TForm1;
  TAPI: TMagTAPI ;

implementation

{$R *.DFM}

procedure TForm1.StatusEvent (state, devid, callh: DWORD;
											mess, info1, info2: string) ;
var
	temp: string ;
begin
	Memo2.Lines.Add ('Device ' + IntToStr (devid) + ': ' +  mess);
    if state = LINECALLSTATE_DIALING then
	begin
    	temp :=TAPI.Modems [devid].Name ;
        Memo2.Lines.Add ('Modem: ' +  temp + ' by: ' + info1);
	end ;
    if state = LINECALLSTATE_CONNECTED then
                                    Memo2.Lines.Add ('Speed: ' + info2);
end ;

procedure TForm1.DiagEvent (mess: string) ;
begin
    Memo1.Lines.Add (mess) ;
end ;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	TAPI.Destroy ;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  	TAPI := TMagTAPI.Create (self) ;
    TAPI.OnTAPIStatus := StatusEvent ;
	TAPI.OnTAPIDiag := DiagEvent ;
	TAPI.StartTAPI ;
    LabelVersion.Caption := LabelVersion.Caption + #13 + #10 + TAPI.Version ;
 	// default dialogs
	SaveDump.InitialDir := ExtractFileDir (Application.ExeName) ;
end;

procedure TForm1.doListModemsClick(Sender: TObject);
var
	I: integer ;
begin
    TAPI.GetModemList ;
    DevList.Items.Clear ;
    if TAPI.Count = 0 then exit ;
    for I := 0 to (TAPI.Count - 1) do
                            DevList.Items.Add (TAPI.Modems [I].Name) ;
end;

procedure TForm1.doTAPIMonClick(Sender: TObject);
var
    I: integer ;
begin
    if TAPI.Count = 0 then
    begin
		Status.SimpleText := 'No Modems to Monitor' ;
        beep ;
        exit ;
    end ;
    begin
        for I := 0 to (TAPI.Count - 1) do TAPI.MonitorOne (I) ;
    end ;
	TAPI.InitCalls ;
end;

function TForm1.SelectedDevNr: integer ;
begin
    result := -1 ;
    if DevList.ItemIndex >= 0 then
          result := TAPI.GetModemDevice (DevList.Items [DevList.ItemIndex]) ;
end ;

procedure TForm1.doConfigClick(Sender: TObject);
var
    devnr: integer ;
begin
    devnr := SelectedDevNr ;
    if devnr < 0 then
    begin
        beep ;
        exit ;
    end ;
    TAPI.ConfigDialog (Handle, devnr) ;
end;

procedure TForm1.doTranslateClick(Sender: TObject);
var
    devnr: integer ;
begin

    devnr := SelectedDevNr ;
    if devnr < 0 then
    begin
        beep ;
        exit ;
    end ;
    TAPI.TranslateDialog (Handle, devnr, '') ;
end;

procedure TForm1.doTranAddrClick(Sender: TObject);
var
    devnr: integer ;
    disnum, dialnum: String ;
begin
	Status.SimpleText := '' ;
    devnr := SelectedDevNr ;
    if devnr < 0 then
    begin
		Status.SimpleText := 'Must Select A Modem First' ;
        beep ;
        exit ;
    end ;
	NumDial.Text := '' ;
    NumDisp.Text := '' ;
	if TAPI.TranslateAddr (devnr, NumUser.Text, disnum, dialnum) then
    begin
		NumDial.Text := dialnum ;
    	NumDisp.Text := disnum ;
    end ;
end;

procedure TForm1.doCloseClick(Sender: TObject);
begin
	Close ;
end;


procedure TForm1.doDumpCapsClick(Sender: TObject);
var
	dumpfn: file ;
    count, devnr: integer ;
    LineCaps: TLineDevCaps ;
begin
	Status.SimpleText := '' ;
    devnr := SelectedDevNr ;
    if devnr < 0 then
    begin
		Status.SimpleText := 'Must Select A Modem First' ;
        beep ;
        exit ;
    end ;
	SaveDump.FileName := SaveDump.InitialDir + '\modem-' +
	    									IntToStr (devnr) + '.bin' ;
    FillChar (LineCaps, sizeof (LineCaps), 0) ;
	if SaveDump.Execute then
    begin
    	try
	        if TAPI.GetModemCaps (devnr, LineCaps) then
            begin
		      	AssignFile (dumpfn, SaveDump.Filename) ;
			    Rewrite (dumpfn, 1) ;
				BlockWrite (dumpfn, LineCaps, LineCaps.dwUsedSize, count) ;
	        		Status.SimpleText := 'Modem Capabilities Dumped to ' +
            								   LowerCase (SaveDump.Filename) ;
		        CloseFile (dumpfn) ;
			end
            else
		        Status.SimpleText := 'Failed to Read Modem Capabilities' ;
		except
	        Status.SimpleText := 'Failed to Dump Entry' ;
	        CloseFile (dumpfn) ;
		end ;
        beep ;
    end ;

end;

end.

