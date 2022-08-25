unit demolsa0;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas LSA Demo
=====================


Created by Angus Robertson, Magenta Systems Ltd, England
in 2008, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 29th August 2008

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
  MagLsaPw, MagSubs1;

type
  TForm1 = class(TForm)
    doExit: TButton;
    doTest: TButton;
    Status: TStatusBar;
    Log: TMemo;
    procedure doExitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Application.Terminate ;
end;


procedure TForm1.doTestClick(Sender: TObject);
var
  LSA: TLSALocal;
  PrivateData: PLSA_UNICODE_STRING;
  S: StringArray;
  I: integer ;

   procedure ListKey (const key: string) ;
   var
        W: WideString ;
        S: string ;
        Err: integer ;
   begin
        if LSA.GetLsaData(POLICY_ALL_ACCESS, key, PrivateData) then
        begin
            SetLength (W, PrivateData.Length div 2) ;
            Move (PrivateData.Buffer^, W [1], PrivateData.Length) ;
            S := W ;
            Log.Lines.Add (key) ;
            Log.Lines.Add (ConvHexQuads (S)) ;
            StringTranCh (S, #0, '|') ;
            Log.Lines.Add ('Data: ' + S) ;
            LSA.LsaFree(PrivateData.Buffer);
        end
        else
        begin
            err := LSA.LsaNtStatusToWinError (LSA.LsaNtStatus) ;
            Log.Lines.Add (key + ': Error ' + SysErrorMessage (err)) ;
        end ;            
        Log.Lines.Add ('') ;
   end ;

begin

  LSA := TLSALocal.Create;
  if not LSA.Initialized then begin
    LSA.Free;
    Exit;
  end;

  {    StrArrayFromMultiSZ (PrivateData.Buffer, PrivateData.Length, S) ;
    if Length (S) > 0 then
    begin
        for I := 0 to Pred (Length (S)) do FLSAList.Add (S [I]);
    end ;    }

    ListKey ('L$_RasDefaultCredentials#0') ; // 9 strings per RAS entry
    ListKey ('DefaultPassword') ;  // single unterminated string
    ListKey ('0083343a-f925-4ed7-b1d6-d95d17a0b57b-RemoteDesktopHelpAssistantAccount') ;
    ListKey ('aspnet_WP_PASSWORD') ;
    ListKey ('DPAPI_SYSTEM') ;
    ListKey ('L$HYDRAENCKEY_28ada6da-d622-11d1-9cb9-00c04fb16e75') ;
    ListKey ('L$RTMTIMEBOMB_1320153D-8DA3-4e8e-B27B-0D888223A588') ;
    ListKey ('NL$KM') ;
    ListKey ('L${6B3E6424-AF3E-4bff-ACB6-DA535F0DDC0A}') ;
    ListKey ('G${ED8F4747-E13D-47bc-856B-5CEFE1A81A7F}') ;
    ListKey ('_SC_DUN Manager Service') ;  // did not work
    ListKey ('_SC_MSSQL$SQLEXPRESS') ;
    ListKey ('_SC_SAVService') ;
//    ListKey ('') ;
//    ListKey ('') ;
//    ListKey ('') ;

  LSA.Free;
end ;

end.
