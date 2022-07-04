unit API_usb;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, Messages, Windows, Forms;

type
  TAPI_usb = class(TComponent)
  private
    { Private declarations }
    fversion: string;
    fwindowhandle: hwnd;
    fonusbarrive: tnotifyevent;
    fonusbremove: tnotifyevent;
    procedure dummys(s: string);
    procedure wndproc(var msg: tmessage);
    function usbregister: boolean;
  protected
    { Protected declarations }
    procedure wmdevicechange(var msg: tmessage); dynamic;
  public
    { Public declarations }
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
    property OnUSBArrive: tnotifyevent read fonusbarrive write fonusbarrive;
    property OnUSBRemove: tnotifyevent read fonusbremove write fonusbremove;
  end;

procedure Register;

implementation

{$R *.RES}

type
  PDevBroadcastHdr = ^DEV_BROADCAST_HDR;
  DEV_BROADCAST_HDR = packed record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;

  PDevBroadcastDeviceInterface = ^DEV_BROADCAST_DEVICEINTERFACE;
  DEV_BROADCAST_DEVICEINTERFACE = record
    dbcc_size: DWORD;
    dbcc_devicetype: DWORD;
    dbcc_reserved: DWORD;
    dbcc_classguid: TGUID;
    dbcc_name: short;
  end;

const
  GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  DBT_DEVICEARRIVAL = $8000;                  // system detected a new device
  DBT_DEVICEREMOVECOMPLETE = $8004;           // device is gone
  DBT_DEVTYP_DEVICEINTERFACE = $00000005;     // device interface class

//------------------------------------------------------------------------------
constructor TAPI_usb.Create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= 'r1.00/ari.pikivirta(at)kolumbus.fi';
  fwindowhandle:= classes.allocatehwnd(wndproc);
  usbregister;
end;

//------------------------------------------------------------------------------
destructor TAPI_usb.Destroy;
begin
  classes.deallocatehwnd(fwindowhandle);
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_usb.dummys(s: string);
begin
  // does absolutely nothing
end;

//------------------------------------------------------------------------------
procedure TAPI_usb.wndproc(var msg: tmessage);
begin
  if (Msg.Msg = WM_DEVICECHANGE) then
  try
    WMDeviceChange(Msg);
  except
    Application.HandleException(Self);
  end else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

//------------------------------------------------------------------------------
procedure TAPI_usb.wmdevicechange(var msg: tmessage);
var
  devtype: Integer;
  datos: PDevBroadcastHdr;
begin
  if (Msg.wParam = DBT_DEVICEARRIVAL) or (Msg.wParam = DBT_DEVICEREMOVECOMPLETE) then
  begin
    datos:= PDevBroadcastHdr(Msg.lParam);
    devtype:= datos^.dbch_devicetype;
    if devtype=DBT_DEVTYP_DEVICEINTERFACE then
      if Msg.wParam = DBT_DEVICEARRIVAL then
      begin
        if assigned(fonusbarrive) then
          fonusbarrive(self);
      end else
      begin
        if assigned(fonusbremove) then
          fonusbremove(self);
      end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_usb.usbregister: boolean;
var
  dbi: DEV_BROADCAST_DEVICEINTERFACE;
  size: integer;
  r: pointer;
begin
  size:= sizeof(DEV_BROADCAST_DEVICEINTERFACE);
  zeromemory(@dbi, size);
  dbi.dbcc_size:= size;
  dbi.dbcc_devicetype:= DBT_DEVTYP_DEVICEINTERFACE;
  dbi.dbcc_reserved:= 0;
  dbi.dbcc_classguid:= GUID_DEVINTERFACE_USB_DEVICE;
  dbi.dbcc_name:= 0;
  r:= RegisterDeviceNotification(FWindowHandle, @dbi, DEVICE_NOTIFY_WINDOW_HANDLE);
  result:= Assigned(r);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_usb]);
end;

end.
