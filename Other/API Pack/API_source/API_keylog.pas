unit API_keylog;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// r1.06/05062008, ari pikivirta
//  * processing messages until the application is either terminated or the
//    keys are released
//
// r1.05/03062008, ari pikivirta
//  * activate on designing state prevented
//
// r1.04/25022007, ari pikivirta
//  * removed windows tricks from the keylog component, they have been put into
//    new component API_windows
//
// r1.03/22022007, ari pikivirta
//  * added ForceForeground function (on all operating systems), also is
//    demonstrated in the example
//
// r1.02/19022007, ari pikivirta
//  * added FindWindowByTitle function (also exported)
//  * overloaded setactivewindow to be able to use handle too
//
// r1.01/12012007, ari pikivirta
//  * added keystate function to retrieve simultaneous keys (from keytable)
//  * added sendkey function

interface

uses
  Windows, SysUtils, Classes, Messages, extctrls, Forms, API_base;

const
  API_MINKEY = $01;
  API_MAXKEY = $FE+1;

type
  TAPI_onkey = procedure(sender: tobject; key: integer) of object;

  TAPI_keylog = class(TAPI_Custom_Component)
  private
    { Private declarations }
    factive     : boolean;
    fonkey      : TAPI_onkey;
    ftimer      : ttimer;
    fkeytable   : array[API_MINKEY..API_MAXKEY] of boolean;
    foldkeytable: array[API_MINKEY..API_MAXKEY] of boolean;
    procedure   TimerEvent(sender: tobject);
    procedure   SetActive(b: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: tcomponent); override;
    destructor  Destroy; override;
    procedure   SendKey(window: hwnd; virtualKey: word; shift, ctrl, menu: boolean; waitms: integer);    function KeyState(keynum: integer): boolean;
  published
    { Published declarations }
    property    Active: boolean read factive write setactive;
    property    OnKey: tapi_onkey read fonkey write fonkey;
  end;

procedure Register;

implementation

{$R *.RES}

//------------------------------------------------------------------------------
constructor TAPI_keylog.Create(AOwner: tcomponent);
begin
  inherited Create(AOwner);
  factive:= false;
  version:= 'r1.06/ari.pikivirta@kolumbus.fi';

  ftimer:= ttimer.create(self);
  ftimer.interval:= 1;
  ftimer.ontimer:= Timerevent;
  ftimer.enabled:= false;
end;

//------------------------------------------------------------------------------
destructor TAPI_keylog.Destroy;
begin
  ftimer.enabled:= false;
  ftimer.free;

  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_keylog.SendKey(window: hwnd; virtualKey: word; shift, ctrl, menu: boolean; waitms: integer);
const
  SHIFTKEY = $10;
  CTRLKEY = $11;
  ALTKEY = $12;
var
  lparam: dword;
  keyboardState: TKeyBoardState;
//  origw: hwnd;
begin
  // get current and activate target window
  (*
  origw:= windows.GetActiveWindow;
  window:= windows.SetActiveWindow(window);
  *)

  // keydown, press and keyup events
  lparam := $00000001;
  if menu then lparam:= lparam or $20000000;
  if shift or ctrl or menu then
  begin
    GetKeyBoardState(keyboardState);
    if menu then
    begin
      PostMessage(window, WM_SYSKEYDOWN, ALTKEY, lparam);
      keyboardState[ALTKEY]:= $81;
    end;
    if shift then
    begin
      PostMessage(window, WM_KEYDOWN, SHIFTKEY, lparam);
      keyboardState[SHIFTKEY]:= $81;
    end;
    if ctrl then
    begin
      PostMessage(window, WM_KEYDOWN, CTRLKEY, lparam);
      keyboardState[CTRLKEY]:= $81;
    end;
    SetKeyBoardState(keyboardState);
  end;

  if menu then PostMessage(window, WM_SYSKEYDOWN, virtualKey, lparam)
    else PostMessage(window, WM_KEYDOWN, virtualKey, lparam);

  if waitms>0 then
    sleep(waitms);

  lparam:= lparam or $D0000000;
  if menu then PostMessage(window, WM_SYSKEYUP, virtualKey, lparam)
    else PostMessage(window, WM_KEYUP, virtualKey, lparam);

  if shift or ctrl or menu then
  begin
    GetKeyBoardState(keyboardState);
    if ctrl then
    begin
      PostMessage(window, WM_KEYUP, CTRLKEY, lparam);
      keyboardState[CTRLKEY]:= $00;
    end;
    if shift then
    begin
      PostMessage(window, WM_KEYUP, SHIFTKEY, lparam);
      keyboardState[SHIFTKEY]:= $00;
    end;
    if menu then
    begin
      lparam:= lparam and $DFFFFFFF;
      PostMessage(window, WM_SYSKEYUP, ALTKEY, lparam);
      keyboardState[ALTKEY]:= $00;
    end;
    SetKeyBoardState(keyboardState);
  end;

  // activate the privious again
  (*
  windows.SetActiveWindow(origw);
  *)
end;

//------------------------------------------------------------------------------
procedure TAPI_keylog.SetActive(b: boolean);
begin
  if (factive<>b) and (not (csdesigning in componentstate)) then
  begin
    factive:= b;
    ftimer.enabled:= factive;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_keylog.TimerEvent(sender: TObject);
var
  key: integer;
  //keyboardstate: tkeyboardstate;
  nokeydown: boolean;
begin
  nokeydown:= true;
  while (nokeydown) and (not application.Terminated) do
  begin
    //GetKeyboardState(keyboardState); // get keyboard state every 1ms
    for key:=API_MINKEY to API_MAXKEY-1 do // go trought all needed keys..
    begin
      foldkeytable[key]:= fkeytable[key];
      fkeytable[key]:= //bool(keyboardstate[key]); //(keyboardstate and key); //GetAsyncKeyState(key));
        bool(GetAsyncKeyState(key));
      if (fkeytable[key]) and (fkeytable[key]<>foldkeytable[key]) then // if key pressed
      begin
        nokeydown:= false; // set no key down false to read keyboard again
        if assigned(fonkey) then // on key event (shouldn't be used if speed is needed!)
          fonkey(self, key);
      end;
    end;
    application.processmessages; // process messages
  end;
end;

//------------------------------------------------------------------------------
function TAPI_keylog.KeyState(keynum: integer): boolean;
begin
  result:= false;
  if (keynum>=API_MINKEY) and (keynum<API_MAXKEY) then
    result:= fkeytable[keynum];
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_keylog]);
end;

end.
