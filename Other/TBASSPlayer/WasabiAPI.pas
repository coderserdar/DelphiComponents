//---------------------------------- Wasabi services API ---------------------------------
// This Delphi version of Wasabi services API is to support Winamp vis plug-ins only,
//  i.e., not for general purpose.
// Thanks to Bernhard(Barna) & Emil Weiss(BassFan), They gave me good advice for me
//  to write & to use this API.
// This API is tested with Milkdrop 2.0e which is (currently) the only visualization
//  plug-in uses Wasabi services API.
//
// Usage >>   ( how to drive Milkdrop 2.0e )
//  a. The exported function 'winampVisGetHeader' of new visualization plugin demands a
//    parameter, so modify the import function as follows.
//      getVisHeader : function(ParentHandle : HWND) : pointer; cdecl;
//         {=> ParentHandle : handle to the (simulated) winamp window which receives
//             Winamp IPC messages }
//                .
//      getVisHeader := GetProcAddress(VisDLLHandle, 'winampVisGetHeader');
//      Visheader := getVisHeader(WinampHandle);
//                .
//  b. Declare this unit in the "uses" clause of the unit which takes charge of driving
//    Winamp visualization plug-ins, then put a sentence calling WaAPIServiceEntry
//    which is the only function of this unit, in the Winamp IPC message handling routine
//    for the case Lparam = IPC_GET_API_SERVICE as follows.
//
//      if Msg.Msg = WM_WA_IPC then
//      begin
//                .
//                .
//      else if Msg.LParam = IPC_GET_API_SERVICE then   // IPC_GET_API_SERVICE = 3025;
//         Result := WaAPIServiceEntry;          //
//      else if Msg.LParam = IPC_...
//                .
//                .
//
//  That's all.
//
//                                  written by Silhwan Hyun     2008-07-19

unit WasabiAPI;

interface

// {$DEFINE DEBUG}

uses
   Windows, SysUtils;

const
  g_guid_waservice_language: TGUID = '{30AED4E5-EF10-4277-8D49-27AB5570E891}';

  API_LANGUAGE_GETSTRING = integer(10);
  API_LANGUAGE_GETSTRINGW = integer(11);

  API_LANGUAGE_GETSTRINGFROMGUID = integer(12);
  API_LANGUAGE_GETSTRINGFROMGUIDW = integer(13);

  API_LANGUAGE_GETHINSTANCEBYGUID = integer(20);
  API_LANGUAGE_GETHINSTANCEBYNAME = integer(21);
  API_LANGUAGE_GETHINSTANCEBYNAMEW = integer(22);

  API_LANGUAGE_STARTUP = integer(30);
  API_LANGUAGE_SHUTDOWN = integer(31);

  API_LANGUAGE_GETLANGUAGEFOLDER = integer(40);

  API_LANGUAGE_CREATELDIALOGPARAM = integer(50);
  API_LANGUAGE_LDIALOGBOXPARAM = integer(51);
  API_LANGUAGE_LOADLMENU = integer(52);
  API_LANGUAGE_CREATELDIALOGPARAMW = integer(53);
  API_LANGUAGE_LDIALOGBOXPARAMW = integer(54);
  API_LANGUAGE_LOADLMENUW = integer(55);

  API_LANGUAGE_GETLANGUAGEIDENTIFIER = integer(60);

  API_LANGUAGE_LOADRESOURCEFROMFILE = integer(70);
  API_LANGUAGE_LOADRESOURCEFROMFILEW = integer(71);

  API_LANGUAGE_LOADACCELERATORSA = integer(80);
  API_LANGUAGE_LOADACCELERATORSW = integer(81);

  WASERVICEFACTORY_GETINTERFACE = integer(300);

  API_SERVICE_SERVICE_REGISTER = integer(10);
  API_SERVICE_SERVICE_DEREGISTER = integer(20);
  API_SERVICE_SERVICE_GETNUMSERVICES = integer(30);
  API_SERVICE_SERVICE_ENUMSERVICE = integer(40);
  API_SERVICE_SERVICE_GETSERVICEBYGUID = integer(50);

type
  TPointerArray = array[Word] of Pointer;
  PPointerArray = ^TPointerArray;

 // TWasabiService is used just to supply data space for virtual function.
  TWasabiService = class
  private
    { Private declarations }
  public
    { Public declarations }
    function _dispatch( msg: integer; var retval: integer;
                        Pparams: PPointerArray;
                        nparam: integer ): integer; virtual; stdcall;
  end;

  function WaAPIServiceEntry : integer;


implementation


var
   WasabiService : TWasabiService;
   ServiceEntry : integer;
   WaAPIService: TMethod;
   WaLanguageService : TMethod;

function WaAPIServiceEntry : integer;
begin
   ServiceEntry := integer(@WaAPIService);
   result := integer(@ServiceEntry);
end;

function TWasabiService._dispatch( msg: integer; var retval: integer;
                                   Pparams: PPointerArray;
                                   nparam: integer ): integer; stdcall;
begin
   result := 0;   // nothing to do
end;


// Wasabi Language Service
function waService_language( msg: Integer; var retval: integer;
                             Pparams: PPointerArray;
                             nparam: integer ): integer; stdcall;
var
  params : array[0..5] of pointer;
  TestBuf : array[0..512] of char;
  TestBufW : array[0..512] of WideChar;
  i : integer;
  p : pAnsiChar;
  p1 : pChar;
  p2 : pWideChar;

begin
  Result := Ord(False);

  for i := 0 to 5 do
     params[i] := Pparams^[i];

  case msg of
    API_LANGUAGE_GETSTRING :
      begin
        // params : (DWORD) handle, (HINSTANCE) hInstance, (UINT) uID, (LPSTR) destBuf, (int) destSize
        if (nparam <> 5) then
           exit;
        p := @TestBuf[0];
        if (LoadStringA(DWORD(params[1]^), DWORD(params[2]^), p, SizeOf(TestBuf) - 1) = 0) then
           result := ord(FALSE)
        else
        begin
           retval := integer(@TestBuf[0]);
           result := ord(TRUE);
        end;
      end;
    API_LANGUAGE_GETSTRINGW :
      begin
        // params : (DWORD) handle, (HINSTANCE) hInstance, (UINT) uID, (LPSTR) destBuf, (int) destSize
        if (nparam <> 5) then
           exit;
        p2 := @TestBufW[0];
        if (LoadStringW(DWORD(params[1]^), DWORD(params[2]^), p2, SizeOf(TestBufW) - 1) = 0) then
           result := ord(FALSE)
        else
        begin
        //   PWideChar(retval) := PWideChar((@TestBufW[0]));
           retval := integer(@TestBufW[0]);
           result := ord(TRUE);
        end;
      end;
    API_LANGUAGE_STARTUP :
      begin
        // params : (HINSTANCE) hInstance, (GUID) pluginGuid
        if (nparam <> 2) then
           exit;
        DWORD(retval) := DWORD($DEADC0DE);
        result := ord(TRUE);
      end;
    API_LANGUAGE_CREATELDIALOGPARAM :
      begin
        // params : (DWORD) handle, (HINSTANCE) hInstance, (LPCSTR) lpTemplateName, (HWND) hWndParent, (DLGPROC) lpDialogFunc, (LPARAM) dwInitParam
        if (nparam <> 6) then
           exit;
        p := pAnsiChar(params[2]^);
        retval :=  CreateDialogParamA(DWORD(params[1]^), p, DWORD(params[3]^), pointer(params[4]^), integer(params[5]^));
        result := ord(TRUE);
      end;
    API_LANGUAGE_CREATELDIALOGPARAMW :
      begin
        // params : (DWORD) handle, (HINSTANCE) hInstance, (LPCSTR) lpTemplateName, (HWND) hWndParent, (DLGPROC) lpDialogFunc, (LPARAM) dwInitParam
        if (nparam <> 6) then
           exit;
        p2 := pWideChar(params[2]^);
        retval :=  CreateDialogParamW(DWORD(params[1]^), p2, DWORD(params[3]^), pointer(params[4]^), integer(params[5]^));
        result := ord(TRUE);
      end;
    API_LANGUAGE_LDIALOGBOXPARAM :
      begin
        // params : (DWORD) handle, (HINSTANCE) hInstance, (LPCSTR) lpTemplateName, (HWND) hWndParent, (DLGPROC) lpDialogFunc, (LPARAM) dwInitParam
        if (nparam <> 6) then
           exit;
        p := pAnsiChar(params[2]^);
        retval := DialogBoxParamA(DWORD(params[1]^), p, DWORD(params[3]^), pointer(params[4]^), integer(params[5]^));
        result := ord(TRUE);
      end;
    API_LANGUAGE_LDIALOGBOXPARAMW :
      begin
        // params : (DWORD) handle, (HINSTANCE) hInstance, (LPCSTR) lpTemplateName, (HWND) hWndParent, (DLGPROC) lpDialogFunc, (LPARAM) dwInitParam
        if (nparam <> 6) then
           exit;
        p2 := pWideChar(params[2]^);
        retval := DialogBoxParamW(DWORD(params[1]^), p2, DWORD(params[3]^), pointer(params[4]^), integer(params[5]^));
        result := ord(TRUE);
      end;
    API_LANGUAGE_LOADLMENU :
      begin
        if (nparam <> 3) then
           exit;
     //  params : (DWORD) handle, (HINSTANCE) hInstance, (LPCSTR) original
        p1 := pChar(params[2]^);
        retval := LoadMenu(DWORD(params[1]^), p1);
        result := ord(TRUE);
      end;
    WASERVICEFACTORY_GETINTERFACE :
      begin
        ServiceEntry := integer(@WaLanguageService);
        retval := integer(@ServiceEntry);
        result := ord(TRUE);
      end;
  end;

// * Following code is just for debugging purpose (to know unprocessed msg)  
{$IFDEF DEBUG}
  if Result = Ord(False) then
     MessageBox(0, pchar(intToStr(msg)), 'Confirm', MB_OK or MB_ICONINFORMATION);
{$ENDIF}
end;

// Wasabi API Service
function waService_api( msg: integer; var retval: integer;
                        Pparams: PPointerArray;
                        nparam: integer ): integer;  stdcall;
var
  P_GUID : PGUID;
begin
  result := ord(FALSE);

  case msg of
    API_SERVICE_SERVICE_GETSERVICEBYGUID: begin
           P_GUID := Pparams^[0];
           if (nparam = 1) and (P_GUID <> nil) then
           begin
              if CompareMem(P_GUID, @g_guid_waservice_language,
                                          sizeof(g_guid_waservice_language)) then
              begin
                 ServiceEntry := integer(@WaLanguageService);
                 retval := integer(@ServiceEntry);
                 result := ord(TRUE);
              end;
           end;  // end of if
        end;  // end of API_SERVICE_SERVICE_GETSERVICEBYGUID
   end;  // end of case
end;


initialization
   WasabiService := TWasabiService.Create;
   WaAPIService.Data := Pointer(WasabiService);
   WaAPIService.Code := Addr(waService_api);
   WaLanguageService.Data := Pointer(WasabiService);
   WaLanguageService.Code := Addr(waService_language);

finalization
   WasabiService.Free;

end.
