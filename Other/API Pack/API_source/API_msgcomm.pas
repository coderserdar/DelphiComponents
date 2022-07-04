unit API_msgcomm;

// component to handle application to application messages. also containing
// some helpers to build quite flexible communication converting some basic
// types to string and back.
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// 23082004, r1.00, ari pikivirta

interface

uses
  SysUtils, Windows, Classes, Messages, Controls, ExtCtrls, forms,
  WinProcs, WinTypes, graphics;

type
  msgcomm_onnewmessage = procedure (
    sender: tobject; const command, value: integer; const messagetext: string)
    of object;

  TAPI_msgcomm = class(tpaintbox)
  private
    fversion: string;
    fmessage: string;
    fcommand: integer;
    fvalue: integer;
    freceivedmessages: integer;
    fsentmessages: integer;
    fonnewmessage: msgcomm_onnewmessage;
    fmsg: cardinal;
    fmailbox: string;
    fmaphandle: thandle;
    fwindowhandle: thandle;
    procedure dummys(s: string);
    procedure dummyi(i: integer);
//    procedure dummyb(b: boolean);
    procedure dummyc(c: cardinal);
    function getmaphandle: thandle;
  protected
    procedure WndProc(var _message: TMessage); override;
    procedure Paint; override;
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    function Send( windowcaption: string; command, value: integer;
      texttosend: string; broadcast: boolean ): boolean;
  published
    property Version: string read fversion write dummys stored false;
    property Mailbox: string read fmailbox write fmailbox;
    property MessageId: cardinal read fmsg write dummyc stored false;
    property ReceivedMessages: integer read freceivedmessages write dummyi stored false;
    property SentMessages: integer read fsentmessages write dummyi stored false;
    property LastCommand: integer read fcommand write dummyi stored false;
    property LastMessage: string read fmessage write dummys stored false;
    property OnMessage: msgcomm_onnewmessage read fonnewmessage write fonnewmessage;
  end;

procedure Register;

implementation

{$R *.RES}

const
  versioninfostring: string = 'r1.00/ari.pikivirta{at}kolumbus.fi';
  fmsgconstant = 'TAPI_MSGCOMM_CUSTOM_MESSAGE';
  fmapconstant = 'TAPI_MSGCOMM_CUSTOM_MAP';
  fmaxmaplength = 2000;

//------------------------------------------------------------------------------
procedure TAPI_msgcomm.dummys(s: string); begin end;
procedure TAPI_msgcomm.dummyi(i: integer); begin end;
//procedure TAPI_msgcomm.dummyb(b: boolean); begin end;
procedure TAPI_msgcomm.dummyc(c: cardinal); begin end;

//------------------------------------------------------------------------------
constructor TAPI_msgcomm.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfostring;
  fwindowhandle:= classes.AllocateHWnd(WndProc);
  fcommand:= 0;
  fmessage:= '';
  freceivedmessages:= 0;
  fsentmessages:= 0;
  fmailbox:= application.Title;
  fmsg:= registerwindowmessage( fmsgconstant );
  fmaphandle:= getmaphandle;

  visible:= false;
end;

//------------------------------------------------------------------------------
destructor TAPI_msgcomm.destroy;
begin
  classes.DeallocateHWnd(fWindowHandle);
  CloseHandle(fmaphandle);
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_msgcomm.getmaphandle: thandle;
begin
  result:= createfilemapping(
    $FFFFFFFF,
    nil,
    PAGE_READWRITE,
    0,
    fmaxmaplength,
    fmapconstant);
  if result<>0 then
    if getlasterror = ERROR_ALREADY_EXISTS then
    begin
      closehandle( result );
      result:= openfilemapping(
        FILE_MAP_WRITE,
        false,
        fmapconstant);
    end;
end;

//------------------------------------------------------------------------------
procedure TAPI_msgcomm.Paint;
begin
  if csdesigning in componentstate then
  begin
    // designing or empty
    canvas.Brush.Color:=clsilver;
    canvas.FillRect(canvas.ClipRect);
    canvas.brush.Color:=clyellow;
    canvas.FrameRect(canvas.ClipRect);
  end else
  begin
    // do nothing..
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_msgcomm.WndProc(var _message: TMessage);
var
  p: pchar;
  i: integer;
  tempstr: string;
  okmessage: boolean;
  recipient: string;
begin
  if _message.Msg = fmsg then
  begin
    okmessage:=False;

    // get string
    if fmaphandle<>0 then
    begin
      p:= mapviewoffile( fmaphandle, FILE_MAP_WRITE, 0, 0, 0 );
      tempstr:= p;
      unmapviewoffile(p);
    end else
      tempstr:= '';

    // get recipient
    i:= pos('|', tempstr);
    if i>0 then
    begin
      recipient:= copy(tempstr, 1, i-1);
      delete( tempstr, 1, i );
      if recipient<>'*' then
      begin
        if recipient= fmailbox then
          okmessage:=true;
      end else
        okmessage:=true;
    end;

    // if recipient was this
    if okmessage then
    begin
      fcommand:= _message.WParam;
      fvalue:= _message.LParam;
      fmessage:= tempstr;
      if assigned( fonnewmessage ) then
        fonnewmessage( self, fcommand, fvalue, fmessage );
      freceivedmessages:= freceivedmessages +1;
    end;
  end;
  inherited wndproc( _message );
end;

//------------------------------------------------------------------------------
function TAPI_msgcomm.Send( windowcaption: string; command, value: integer;
  texttosend: string; broadcast: boolean ): boolean;
var
  p: pchar;
  temptext: string;
begin
  p:= mapviewoffile( fmaphandle, FILE_MAP_WRITE, 0, 0, 0 );
  if broadcast then temptext:='*|'+texttosend else
    temptext:=windowcaption+'|'+texttosend;
  strplcopy( p, temptext, fmaxmaplength );
  SendMessage(HWND_BROADCAST, fmsg, command, value);
  unmapviewoffile(p);
  fsentmessages:= fsentmessages +1;
  result:= true;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_msgcomm]);
end;

end.
