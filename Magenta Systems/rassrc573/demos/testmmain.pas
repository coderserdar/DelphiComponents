unit testmmain;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas test large number of connections
========================================

Allows hundreds of connections to be created and deleted,
so that application handling of such large numbers can
be tested

Created by Angus Robertson, Magenta Systems Ltd, England
in 2002, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 19 Aug 2010

24 May 2007 - set PBLocation before creating and deleting entries
19 Aug 2010 - Removed cast warning for Delphi 2009 and later,

}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, magrasedt, magrascon, MagRasEnt, Spin, ExtCtrls;

type
  TForm1 = class(TForm)
    MagRasCon: TMagRasCon;
    MagRasEdt: TMagRasEdt;
    Entries: TListBox;
    doCreate: TButton;
    doDelete: TButton;
    doShowEntries: TButton;
    NameMask: TEdit;
    NumLow: TSpinEdit;
    NumHigh: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    doExit: TButton;
    LabelTot: TLabel;
    Label4: TLabel;
    LabelProgress: TLabel;
    Device: TComboBox;
    entPhoneBook: TRadioGroup;
    procedure doExitClick(Sender: TObject);
    procedure doShowEntriesClick(Sender: TObject);
    procedure doCreateClick(Sender: TObject);
    procedure doDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

procedure TForm1.doShowEntriesClick(Sender: TObject);
var
    I, ret: integer ;
begin
    Entries.Items.Clear ;
    ret := MagRasGetEntryList ('') ;
    if (ret <> 0) then exit ;
    if (MagRasNumEntryRec = 0) then exit ;
    LabelTot.Caption := 'Total Entries: ' + IntToStr (MagRasNumEntryRec) ;
    for I := 0 to Pred (MagRasNumEntryRec) do
            Entries.Items.Add (String (MagRasEntryRecs [I].EntryName)) ;
end;

procedure TForm1.doCreateClick(Sender: TObject);
var
    I, J, ret: integer ;
    newname: string ;
begin
    if Pos ('#', NameMask.Text) <= 0 then exit ;
    if NumLow.Value > NumHigh.Value then exit ;
    J := 0 ;
    for I := NumLow.Value to NumHigh.Value do
    begin
        newname := StringReplace (NameMask.Text, '#', IntToStr (I), []) ;
        LabelProgress.Caption := 'Creating Entry: ' + newname ;
        Application.ProcessMessages ;
        MagRasEdt.PPPDefault ;

    // phonebook
        MagRasEdt.PBLocation := entPhoneBook.ItemIndex ;  // 5.20 set phonebook file

    // silly telephone number
        MagRasEdt.LocalPhoneNumber := AnsiString ('1111' + IntToStr (I)) ;

	// device stuff - required, must match precisely name and type from lists
    	MagRasEdt.DeviceName := AnsiString (MagRasCon.DeviceNameList [Device.ItemIndex]) ;
    	MagRasEdt.DeviceType := AnsiString (MagRasCon.DeviceTypeList [Device.ItemIndex]) ;

    // create new entry
        ret := MagRasEdt.PutAllEntryProps (AnsiString (newname)) ;
        if ret <> 0 then
        begin
            LabelProgress.Caption := MagRasEdt.StatusStr ;
            exit ;
        end ;
        inc (J) ;
    end ;
    LabelProgress.Caption := 'Created New Entries OK, Total ' + IntToStr (J) ;
    doShowEntriesClick (self) ;
end;

procedure TForm1.doDeleteClick(Sender: TObject);
var
    I, J, ret: integer ;
begin
    if Entries.Items.Count = 0 then exit ;
    J := 0 ;
    for I := 0 to Pred (Entries.Items.Count) do
    begin
        if Entries.Selected [I] then
        begin
            LabelProgress.Caption := 'Deleting Entry: ' + Entries.Items [I] ;
            Application.ProcessMessages ;
            MagRasCon.EntryName := AnsiString (Entries.Items [I]) ;
            MagRasCon.PBLocation := MagRasEntryRecs [I].PBLocation ; // 5.20 set phonebook file
            ret := MagRasCon.DeletePhonebook ;
            if ret <> 0 then
            begin
                LabelProgress.Caption := MagRasCon.StatusStr ;
                exit ;
            end ;
            inc (J) ;
        end ;
    end ;
    LabelProgress.Caption := 'Deleted Entries OK, Total ' + IntToStr (J) ;
    doShowEntriesClick (self) ;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
    I: integer ;
begin
// see if RAS has been installed
	if MagRasCon.TestRAS then
    begin

	// get list of RAS capable modems and devices - don't sort list!
    	MagRasCon.GetDeviceList ;
        if MagRasCon.DeviceNameList.Count <> 0 then
        begin
        	for I := 0 to MagRasCon.DeviceNameList.Count - 1 do
            begin
            	Device.Items.Add (MagRasCon.DeviceNameList [I] +
	                ' (' + LowerCase (MagRasCon.DeviceTypeList [I]) + ')') ;
			end ;
            Device.ItemIndex := 0 ;
		end ;
        entPhoneBook.ItemIndex := MagRasEdt.PBLocation ;  // 5.20 set phonebook file
	end
    else
    begin
	 	LabelProgress.Caption := 'RAS is not installed' ;
        beep ;
    end
end;

end.
