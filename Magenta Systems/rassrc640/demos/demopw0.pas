unit demopw0;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas Password Demo
=====================


Created by Angus Robertson, Magenta Systems Ltd, England
in 2010, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 11th August 2010

This demo shows how to list all RAS phonebook entries from the rasphone.pbk file
and then to find the RAS credentials (username and password) from the Local
Security Authority (LSA) protected subsystem of Windows that maintains
information about all aspects of local security on a system, saved in the registry
under the key: HKEY_LOCAL_MACHINE\SECURITY\Policy\Secrets\

WARNING - this demo displays RAS passwords as cleartext!!!!


}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls,
  MagRasConW, MagRasApiW, MagRasEntW, MagLsaPw, MagSubs1;

type
  TForm1 = class(TForm)
    doExit: TButton;
    Label3: TLabel;
    doRefreshEntries: TButton;
    doSave: TButton;
    Status: TStatusBar;
    entPassword: TEdit;
    Label45: TLabel;
    EntriesList: TListView;
    procedure doExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doRefreshEntriesClick(Sender: TObject);
    procedure doSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
	procedure RefreshConnList ;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  MagRasCon: TMagRasCon ;
  LSARasPw: TLSARasPw ;

implementation

{$R *.DFM}

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

procedure TForm1.RefreshConnList ;
var
    I, tot, errcode, LsaNr: integer ;
begin
    doRefreshEntries.Enabled := false ;
    try
    EntriesList.Items.Clear ;
    errcode := MagRasGetEntryRecs ('', false) ;
    if errcode <> 0 then
    begin
        beep ;
        exit ;
    end ;
    tot := MagRasNumEntryRec ;
    if tot = 0 then exit ;
    for I := 0 to Pred (tot) do
    begin
        with EntriesList.Items.Add, MagRasEntryRecs [I] do
        begin
            Caption := EntryName ;
        // find RAS credentials from LSA store, keyed on DialParamsUID
            LsaNr := LSARasPw.FindDialParamsUID (DialParamsUID) ;
            if LsaNr >= 0 then
            begin
                with LSARasPw.LsaRasCreds [LsaNr] do
                begin
                    SubItems.Add (UserName) ;
                    SubItems.Add (Password) ;
                end ;
            end
            else
            begin
                SubItems.Add ('') ;
                SubItems.Add ('') ;
            end ;
            SubItems.Add (CanonNum) ;
            SubItems.Add (DevName1) ;
            SubItems.Add (DevPort1) ;
            SubItems.Add (DevType1) ;
            SubItems.Add (DevName2) ;
            SubItems.Add (DevPort2) ;
            SubItems.Add (DevType2) ;
            if PBLocation > REN_AllUsers then PBLocation := 0 ;  // 5.20 make sure it's legal
            SubItems.Add (MasRasPBLocationStr [PBLocation]) ;    // 5.20 show literal
            SubItems.Add (Phonebook) ;
        end ;
    end ;
    finally
        doRefreshEntries.Enabled := true ;
    end ;
end ;

procedure TForm1.FormCreate(Sender: TObject);
begin
    if MagRasOSVersion < OSW2K then
    begin
	 	Status.SimpleText := 'This demo is Windows 2000 and later' ;
        beep ;
    end ;

// create RAS component     
    MagRasCon := TMagRasCon.Create (Self);

// see if RAS has been installed
	if MagRasCon.TestRAS then
    begin
     // get list of phonebook entries and credential from LSA
        LSARasPw := TLSARasPw.Create ;
	    RefreshConnList ;
	end
    else
    begin
	 	Status.SimpleText := 'RAS is not installed' ;
        beep ;
    end ;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	// could check if still online and close connection
    Application.Terminate ;
end;


procedure TForm1.doRefreshEntriesClick(Sender: TObject);
begin
    LSARasPw.GetLSAPasswords ;
    RefreshConnList ;
end ;

procedure TForm1.doSaveClick(Sender: TObject);
begin
 // could update username and password in LSA
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    FreeAndNil (LSARasPw) ;
    FreeAndNil (MagRasCon) ;
end;

end.
