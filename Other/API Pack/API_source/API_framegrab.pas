unit API_framegrab;

//------------------------------------------------------------------------------
// video capture component, which bases on someone others unit (i can't
// remember the name). just added some features like the panel and events.
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
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls,
  API_vfw, mmsystem, Graphics, API_base;

type
  TChannel = (Stereo, Mono);
  TFrequency = (f8000Hz, f11025Hz, f22050Hz, f44100Hz);
  TResolution  = (r8Bit, r16Bit);

  TCapStatusProc = procedure(Sender: TObject) of object;
  TCapStatusCallback = procedure (Sender: TObject; nID: integer; status: string) of object;
  TVideoStream = procedure (sender: TObject; lpVhdr: PVIDEOHDR) of object;
  TAudioStream = procedure (sender: TObject; lpWHdr: PWAVEHDR) of object;
  TCapError = procedure (sender: TObject; nID: integer; errorstr: string) of object;
  TAPIGrabError = procedure (sender: tObject; errorstr: string) of object;

  TAudioFormat = class (TPersistent)
   private
    FChannels :TChannel;
    FFrequency:TFrequency;
    FRes      :TResolution;
  private
    procedure SetAudio(handle:Thandle);
  public
    constructor create;
  published
    property Channels: TChannel read FChannels write Fchannels default Mono;
    property Frequency: TFrequency read FFrequency write fFrequency default f8000Hz;
    property Resolution : TResolution read FRes write FRes default r8Bit;
  end;

  TAPI_framegrab = class(TAPI_Custom_Panel)
  private
    fversion: string;

    fdriverlist: tstringlist;
    fdriverIndex: integer;
    fVideoDriverName: string;
    fhCapWnd: THandle;
    fpDrivercaps: PCapDriverCaps;
    fpDriverStatus: pCapStatus;
    fscale: boolean;
    fprop: boolean;
    fpreviewrate: word;
    fmicrosecpframe: cardinal;
    fCapVideoFileName: string;
    fTempFileName: String;
    fTempFileSize: word;

    fCapSingleImageFileName: string;
    fcapAudio: boolean;
    fcapTimeLimit: word;
    fIndexSize: cardinal;
    fcapToFile: boolean;
    fAudioFormat: TAudioFormat;
    fCapStatusProcedure: TCapStatusProc;
    fcapStatusCallBack: TCapStatusCallback;
    fcapVideoStream: TVideoStream;
    fcapAudioStream: TAudiostream;
    fcapFrameCallback: TVideoStream;
    fcapError: TCapError;
    fonError: TAPIGrabError;

    procedure setsize(var msg: TMessage); message WM_SIZE;
    function GetDriverCaps: boolean;
    procedure DeleteDriverProps;
    procedure CreateTmpFile(drvopn: boolean);

    function GetDriverStatus(callback: boolean): boolean;
    Procedure SetDriverOpen(value: boolean);
    function GetDriverOpen: boolean;
    function GetPreview: boolean;
    function GetOverlay: Boolean;
    procedure SizeCap;
    procedure Setprop(value: boolean);
    procedure SetMicroSecPerFrame(value: cardinal);
    procedure setFrameRate(value: word);
    function  GetFrameRate: word;

    procedure SetDriverIndex(value: integer);
    function CreateCapWindow: boolean;
    procedure DestroyCapwindow;
    function GetCapWidth: word;
    function GetCapHeight: word;
    function GetHasDlgVFormat: Boolean;
    function GetHasDlgVDisplay: Boolean;
    function GetHasDlgVSource: Boolean;
    function GetHasVideoOverlay: Boolean;
    procedure Setoverlay(value: boolean);
    procedure SetPreview(value: boolean);
    procedure SetScale(value: Boolean);
    procedure SetpreviewRate(value: word);
    function GetCapInProgress: boolean;
    procedure SetIndexSize(value: cardinal);
    function GetBitMapInfoNP: TBITMAPINFO;
    function GetBitmapHeader: TBitmapInfoHeader;
    procedure SetBitmapHeader(Header: TBitmapInfoHeader);
    procedure SetBufferFileSize(value: word);

    procedure SetStatCallBack(value: TCapStatusCallback);
    procedure SetCapVideoStream(value: TVideoStream);
    procedure SetCapAudioStream(value: TAudioStream);
    procedure SetCapFrameCallback(value: TVideoStream);
    procedure SetCapError(value: TCapError);

    procedure dummysl(sl: tstringlist);
    procedure dummys(s: string);

  protected
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

    procedure SetDriverName(value:String);

    property HasDlgFormat: Boolean read GetHasDlgVFormat;
    property HasDlgDisplay: Boolean read GetHasDlgVDisplay;
    property HasDlgSource: Boolean read GetHasDlgVSource;
    property HasVideoOverlay: boolean read GetHasVideoOverlay;
    property CapWidth: word read GetCapWidth;
    property CapHeight: word read GetCapHeight;
    property CapInProgess: boolean read getCapinProgress;
    property BitMapInfo:TBitmapinfo read GetBitmapInfoNP;

    //Header of the Bitmapinfo
    function DlgVFormat: Boolean;
    function DlgVDisplay: boolean;
    function DlgVSource: boolean;
    function DlgVCompression: Boolean;
    function GrabFrame: boolean;
    function GrabFrameNoStop: boolean;
    function SaveAsDIB: Boolean;
    function SaveToClipboard: Boolean;
    function StartCapture: Boolean;
    function StopCapture: Boolean;
    function GetBitmapInfo(var p: Pointer): integer;
    procedure SetBitmapInfo(p: Pointer; size: integer);
    property BitMapInfoHeader: TBitmapInfoHeader read GetBitmapHeader write SetBitmapHeader;
    function SaveCap: boolean;
    function CapSingleFramesOpen: boolean;
    function CapSingleFramesClose: boolean;
    function CapSingleFrame: boolean;

    Function GetDriverList:TStringList;

  published
    property Version: string read fversion write dummys stored false;

    property DriverList: tstringlist read fdriverlist write dummysl stored false;

    property DriverOpen: boolean read getDriveropen write setDriverOpen;
    property DriverIndex:integer read fdriverindex write SetDriverIndex;
    property DriverName: string read fVideoDriverName write SetDrivername;
    property VideoOverlay:boolean read GetOverlay write SetOverlay;
    property VideoPreview:boolean read GetPreview write SetPreview;
    property PreviewScaleToWindow:boolean read fscale write Setscale;
    property PreviewScaleProportional:boolean read  fprop write Setprop;
    property PreviewRate:word read fpreviewrate write SetpreviewRate;
    property MicroSecPerFrame:cardinal read  fmicrosecpframe write SetMicroSecPerFrame;
    property FrameRate:word read  getFramerate write setFrameRate;
    Property CapAudio:Boolean read fcapAudio write fcapAudio;
    property VideoFileName:string read fCapVideoFileName write fCapVideoFileName;
    property SingleImageFile:string read FCapSingleImageFileName write FCapSingleImageFileName;
    property CapTimeLimit:word read fCapTimeLimit write fCapTimeLimit;
    property CapIndexSize:cardinal read findexSize write setIndexSize;
    property CapToFile:boolean read fcaptoFile write fcapToFile;
    property CapAudioFormat:TAudioformat read FAudioformat write FAudioFormat;
    property BufferFileSize:word read ftempfilesize write SetBufferFileSize;

    property OnStatus: TCapStatusProc read fCapStatusProcedure write FCapStatusProcedure;
    property OnStatusCallback: TCapStatusCallback read fcapStatuscallback write SetStatCallback;
    property OnVideoStream: TVideoStream read fcapVideoStream write SetCapVideoStream;
    property OnFrameCallback: TVideoStream read FcapFramecallback write SetCapFrameCallback;
    property OnAudioStream: TAudioStream read fcapAudioStream write SetCapAudioStream;
    property OnCapError: TCapError read fcapError write SetCapError;
    property OnError: TAPIGrabError read fonerror write fonerror;

  end;

procedure Register;

implementation

{$r *.res}

const
  versioninfostring: string = 'r1.00/ari.pikivirta{at}kolumbus.fi';

//------------------------------------------------------------------------------
procedure TAPI_framegrab.dummys(s: string); begin end;
procedure TAPI_framegrab.dummysl(sl: tstringlist);  begin end;

//------------------------------------------------------------------------------
// Callback for status of video captures
function StatusCallbackProc(hWnd : HWND; nID : Integer; lpsz : Pchar): LongInt; stdcall;
var
  Control: TAPI_framegrab;
begin
  control:=TAPI_framegrab(capGetUserData(hwnd));
  if assigned(control) then
    if assigned(control.fcapStatusCallBack) then
      control.fcapStatusCallBack(control,nId,strPas(lpsz));
  result:= 1;
end;

//------------------------------------------------------------------------------
// Callback for video stream
function VideoStreamCallbackProc(hWnd:Hwnd; lpVHdr:PVIDEOHDR):longint; stdcall;
var
  Control: TAPI_framegrab;
begin
  control:= TAPI_framegrab(capGetUserData(hwnd));
  if assigned(control) then
    if assigned(control.fcapVideoStream ) then
      control.fcapVideoStream(control,lpvHdr);
  result:= 1;
end;

//------------------------------------------------------------------------------
//Callback for Frames during Preview
function FrameCallbackProc(hwnd:Hwnd; lpvhdr:PVideoHdr):longint;stdcall;
var
  Control: TAPI_framegrab;
begin
  control:= TAPI_framegrab(capGetUserData(hwnd));
  if assigned(control) then
    if assigned(control.fcapFrameCallback ) then
      control.fcapFrameCallback(control,lpvHdr);
  result:= 1;
end;

//------------------------------------------------------------------------------
// Callback for audio stream
function AudioStreamCallbackProc(hwnd:HWND;lpWHdr:PWaveHdr):longInt; stdcall;
var
  control: TAPI_framegrab;
begin
  control:= TAPI_framegrab(capGetUserData(hwnd));
  if assigned(control) then
    if assigned(control.fcapAudioStream) then
      control.fcapAudioStream(control,lpwhdr);
  result:= 1;
end;

//------------------------------------------------------------------------------
// Callback for Error
function ErrorCallbackProc(hwnd:HWND;nId:integer;lzError:Pchar):longint;stdcall;
var
  Control: TAPI_framegrab;
begin
  control:= TAPI_framegrab(capGetUserData(hwnd));
  if assigned(control) then
    if assigned(control.fcaperror) then
      control.fcapError(control,nId,StrPas(lzError));
  result:= 1;
end;

//------------------------------------------------------------------------------
// New Window-Procedure for CaputreWindow to post messages like WM_MouseMove to Component
function WCapproc(hw:THandle;messa:DWord; w:wParam; l:lParam):integer;stdcall;
var
  oldwndProc:Pointer;
  parentWnd:Thandle;
begin
  oldwndproc:=Pointer(GetWindowLong(hw,GWL_USERDATA));
  case Messa of
    WM_MOUSEMOVE,
    WM_LBUTTONDBLCLK,
    WM_LBUTTONDOWN,WM_RBUTTONDOWN,WM_MBUTTONDOWN ,
    WM_LBUTTONUP,WM_RBUTTONUP,WM_MBUTTONUP:
      begin
        ParentWnd:=Thandle(GetWindowLong(hw,GWL_HWNDPARENT));
        sendMessage(ParentWnd,messa,w,l);
        result := integer(true);
      end else
        result:= callWindowProc(oldwndproc,hw,messa,w,l);
  end;
end;

//------------------------------------------------------------------------------
constructor TAPI_framegrab.Create(aowner:TComponent);
begin
  inherited create(aowner);
  fversion:= versioninfostring;
  height:= 100;
  width:= 100;
  Color:= clblack;
  fVideoDriverName:= '';
  fdriverindex:= -1 ;
  fhCapWnd:= 0;
  fCapVideoFileName:= 'Video.avi';
  //fCapSingleImageFileName:= 'Capture.bmp';
  fscale:= false;
  fprop:= false;
  fpreviewrate:= 30;
  fmicrosecpframe:= 66667;
  fpDrivercaps:= nil;
  fpDriverStatus:= nil;
  fcapToFile:= true;
  findexSize:= 0;
  ftempFileSize:= 0;
  fCapStatusProcedure:= nil;
  fcapStatusCallBack:= nil;
  fcapVideoStream:= nil;
  fcapAudioStream:= nil;
  FAudioformat:=TAudioFormat.Create;
  fdriverlist:=tstringlist.create;
  fdriverlist:=getdriverlist;
end;

//------------------------------------------------------------------------------
destructor TAPI_framegrab.destroy;
begin
  DestroyCapWindow;
  deleteDriverProps;
  fAudioformat.free;
  fdriverlist.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
// Messagehandler for sizing the capture window
procedure TAPI_framegrab.SetSize(var msg:TMessage);
begin
  if (fhCapWnd <> 0) and (Fscale) then
    if msg.msg = WM_SIZE then SizeCap;
end;

//------------------------------------------------------------------------------
// Sizing capture window
procedure TAPI_framegrab.SizeCap;
var
  h: integer;
  w: integer;
  f: single;
  cf: single;
begin
  if not fscale then
  begin
    MoveWindow(fhcapWnd,0,0,Capwidth,capheight,true);
  end else
  begin
    if fprop then
    begin
      f:= Width/height;
      cf:= CapWidth/CapHeight;
      if f >  cf then
      begin
        h:= height;
        w:= round(h*cf);
      end else
      begin
        w:= width;
        h:= round(w*1/cf);
      end
    end else
    begin
      h:= height;
      w:= Width;
    end;
    MoveWindow(fhcapWnd,0,0,w, h,true);
  end;
end;

//------------------------------------------------------------------------------
// Delete driver infos
procedure TAPI_framegrab.DeleteDriverProps;
begin
  if assigned(fpDrivercaps) then
  begin
    dispose(fpDrivercaps);
    fpDriverCaps:= nil;
  end;
  if assigned(fpDriverStatus) then
  begin
    dispose(fpDriverStatus);
    fpDriverStatus:= nil;
  end;
end;

//------------------------------------------------------------------------------
// Buffer File
procedure TAPI_framegrab.CreateTmpFile(drvOpn:boolean);
var
  s,f: array [0..MAX_PATH] of char;
  size: word;
  ok: boolean;
begin
  if (ftempFileName ='') and (ftempFileSize = 0) then exit;
  if drvOpn then Size := ftempFileSize else size:=0;
  if fTempFileName = '' then
  begin
    GetTempPath(sizeof(s),@s);
    GetTempFileName(s,'cap',0,f);
    ftempfilename := f;
  end;
  if size <> 0 then
  begin
    capFileSetCaptureFile(fhCapWnd,strpCopy(f,ftempfilename));
    ok:=capFileAlloc(fhcapWnd,1024*1024* ftempFileSize);
    if not ok then
      if assigned(fonerror) then
        fonerror(self, 'Could not create tmp file');
  end else
  begin
    capFileSetCaptureFile(fhCapWnd,strpCopy(f, fCapVideoFileName));
    DeleteFile(fTempfileName);
    fTempFileName:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetBufferFileSize(Value:word);
begin
  if value = fTempFilesize then exit;
  ftempFileSize:=value;
  if DriverOpen Then CreateTmpFile(true);
end;

//------------------------------------------------------------------------------
// Capitilies of the Driver
function TAPI_framegrab.GetDriverCaps: boolean;
var
  savestat : integer;
begin
  result:= false;
  if assigned(fpDrivercaps) then
  begin
    result:= true;
    exit;
  end;
  if fdriverIndex = -1 then exit;
  savestat := fhCapwnd;
  if fhCapWnd = 0 then CreateCapWindow;
  if fhCapWnd = 0 then exit;
  new(fpDrivercaps);
  if capDriverGetCaps(fhCapWnd, fpDriverCaps, sizeof(TCapDriverCaps)) then
  begin
    result:= true;
    if savestat = 0 then destroyCapWindow;
    exit;
  end else
  begin
    dispose(fpDriverCaps);
    fpDriverCaps := nil;
    if savestat = 0 then destroyCapWindow;
    if assigned(fonerror) then
      fonerror(self, 'Failed to open capture driver');
  end;
end;

//------------------------------------------------------------------------------
// BitmapInfo without a Palette
function TAPI_framegrab.GetBitMapInfoNp:TBitmapinfo;
begin
  if driveropen then
  begin
    capGetVideoFormat(fhcapWnd, @result,sizeof(TBitmapInfo));
  end else
  begin
    fillchar(result,sizeof(TBitmapInfo),0);
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
  end;
end;

//------------------------------------------------------------------------------
// Whole BitmapInfo
function TAPI_framegrab.GetBitMapInfo(var p:Pointer): integer;
var
  size: integer;
begin
  p:=nil;
  result:=0;

  if driverOpen then
  begin
    size:= capGetVideoFormat(fhcapWnd,p,0);
    getmem(p,size);
    capGetVideoFormat(fhcapwnd,p,size);
    result:=size;
  end else
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
  end;
end;

//------------------------------------------------------------------------------
// Setting whole BitmapInfo
procedure TAPI_framegrab.SetBitmapInfo(p:Pointer;size:integer);
var
  supported: boolean;
begin
  if driverOpen then
  begin
    supported:=capSetVideoFormat(fhcapWnd,p,size);
    if not supported then
    begin
      if assigned(fonerror) then
        fonerror(self, 'Not supported frame format');
    end;
  end else
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
  end;
end;

//------------------------------------------------------------------------------
// Only Header of BitmapInfo
function TAPI_framegrab.GetBitMapHeader:TBitmapinfoHeader;
begin
  if driveropen then
  begin
    capGetVideoFormat(fhcapWnd, @result,sizeof(TBitmapInfoHeader));
    exit;
  end else
  begin
    fillchar(result,sizeof(TBitmapInfoHeader),0);
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetBitMapHeader(header:TBitmapInfoHeader);
begin
  if driveropen then
  begin
    if not capSetVideoFormat(fhcapWnd,@header,sizeof(TBitmapInfoHeader)) then
    begin
      if assigned(fonerror) then
        fonerror(self, 'Not supported frame format');
    end else
      exit;
  end else
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
  end;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.getDriverStatus(callback:boolean):boolean;
begin
  result := false;
  if fhCapWnd <> 0 then
  begin
    if not assigned(fpDriverstatus) then new(fpDriverStatus);
    if capGetStatus(fhCapWnd,fpdriverstatus, sizeof(TCapStatus)) then
      result:= true;
  end;
  if assigned(fCapStatusProcedure)and callback then fcapStatusProcedure ( self);
end;

//------------------------------------------------------------------------------
// Setting name of driver
procedure TAPI_framegrab.SetDrivername(value:string);
var
  i: integer;
  name: array[0..80] of char;
  ver: array[0..80] of char;
begin
  if fVideoDrivername = value then exit;
  for i:= 0 to 9 do
    if capGetDriverDescription( i,name,80,ver,80) then
      if strpas(name) = value then
      begin
        fVideoDriverName := value;
        Driverindex:= i;
        exit;
      end;
  fVideoDrivername:= '';
  DriverIndex:= -1;
  if assigned(fonerror) then
    fonerror(self, 'Driver with specified name not found');
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetDriverIndex(value: integer);
var
  name: array[0..80] of char;
  ver: array[0..80] of char;
begin
  if value = fdriverindex then exit;
  destroyCapWindow;
  deleteDriverProps;
  if value > -1 then
  begin
    if capGetDriverDescription(value,name,80,ver,80) then fVideoDriverName:= StrPas(name)
      else value:= -1;
  end;
  if value = -1 then
  begin
    fvideoDriverName:= '';
    if assigned(fonerror) then
      fonerror(self, 'Driver with specified index not found');
  end;
  fdriverindex:= value;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.CreateCapWindow: boolean;
var
  savewndproc:integer;
begin
  result:=false;

  if fhCapWnd <> 0 then
  begin
    result:= true;
    exit;
  end;

  if fdriverIndex = -1 then
  begin
    GetDriverStatus(true);
    if assigned(fonerror) then
      fonerror(self, 'No capture driver selected');
    exit;
  end;

  fhCapWnd := capCreateCaptureWindow( PChar(Name), WS_CHILD or WS_VISIBLE , 0, 0,
    Width, Height, Handle, 5001);

  if fhCapWnd =0 then
  begin
    GetDriverStatus(true);
    if assigned(fonerror) then
      fonerror(self, 'Failed to create capture window');
    exit;
  end;

  capSetUserData(fhCapwnd,integer(self));
  savewndproc:=SetWindowLong(fhcapWnd,GWL_WNDPROC,integer(@WCapProc));
  SetWindowLong(fhcapWnd,GWL_USERDATA,savewndProc);

  // Setting callbacks as events
  if assigned(fcapStatusCallBack ) then
    capSetCallbackOnStatus(fhcapWnd ,StatusCallbackProc);
  if assigned(fcapFrameCallback) then
    capSetCallbackOnFrame(fhcapWnd,FrameCallbackProc);
  if assigned(fcapError) then
    capSetCallbackOnError(fhcapWnd,ErrorCallBackProc);
  if assigned(fcapVideoStream) then
    capSetCallbackOnVideoStream(fhcapwnd,VideoStreamCallbackProc);
  if assigned(fcapAudioStream) then
    capSetCallbackOnWaveStream(fhcapWnd,AudioStreamCallbackProc);

  if not capDriverConnect(fhCapWnd, fdriverIndex) then
  begin
    Destroycapwindow;
    GetDriverStatus(true);
    if assigned(fonerror) then
      fonerror(self, 'Failed to connect capture window to driver');
    exit;
  end;

  CreateTmpFile(True);
  capPreviewScale(fhCapWnd, fscale);
  capPreviewRate(fhCapWnd, round( 1/fpreviewrate*1000));
  GetDriverStatus(true);
  Sizecap;
  result:= true;
end;

//------------------------------------------------------------------------------
// Setting callbacks as events
procedure TAPI_framegrab.SetStatCallBack(value: TCapStatusCallback);
begin
  fcapStatusCallBack := value;
  if DriverOpen then
    if assigned(fcapStatusCallBack) then
      capSetCallbackOnStatus(fhcapWnd ,StatusCallbackProc)
      else capSetCallbackOnStatus(fhcapWnd ,nil);
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetCapVideoStream(value:TVideoStream);
begin
  fcapVideoStream:= value;
  if DriverOpen then
    if assigned(fcapVideoStream) then
      capSetCallbackOnVideoStream(fhcapwnd,VideoStreamCallbackProc)
      else capSetCallbackOnVideoStream(fhcapwnd, nil);
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetCapFrameCallback(value:TVideoStream);
begin
  fcapframeCallback:= value;
  if DriverOpen then
    if assigned(fcapFrameCallback) then
      capSetCallbackOnFrame(fhcapwnd,FrameCallBackProc)
      else capSetCallbackOnFrame(fhcapwnd, nil);
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetCapAudioStream(value:TAudioStream);
begin
  fcapAudioStream:= value;
  if DriverOpen then
    if assigned(fcapAudioStream) then
      capSetCallbackOnWaveStream(fhcapWnd,AudioStreamCallbackProc)
      else capSetCallbackOnWaveStream(fhcapWnd,nil);
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetCapError(value: TCapError);
begin
  fcapError:= value;
  if DriverOpen then
    if assigned(fcapError) then
      capSetCallbackOnError(fhcapWnd,ErrorCallbackProc)
      else capSetCallbackOnError(fhcapWnd,nil);
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.DestroyCapWindow;
begin
  if fhCapWnd = 0 then exit;
  CreateTmpFile(False);
  CapDriverDisconnect(fhCapWnd);
  SetWindowLong(fhcapWnd,GWL_WNDPROC,GetWindowLong(fhcapwnd,GWL_USERDATA));
  DestroyWindow( fhCapWnd ) ;
  fhCapWnd := 0;
end;

//------------------------------------------------------------------------------
function  TAPI_framegrab.GetHasVideoOverlay:Boolean;
begin
  if getDriverCaps then Result := fpDriverCaps^.fHasOverlay
    else result:= false;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetHasDlgVFormat:Boolean;
begin
  if getDriverCaps then Result := fpDriverCaps^.fHasDlgVideoFormat
    else result:= false;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetHasDlgVDisplay : Boolean;
begin
  if getDriverCaps then  Result := fpDriverCaps^.fHasDlgVideoDisplay
    else result:= false;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetHasDlgVSource  : Boolean;
begin
  if getDriverCaps then Result := fpDriverCaps^.fHasDlgVideoSource
    else result:= false;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.DlgVFormat:boolean;
var
  savestat: integer;
begin
  result:= false;
  if fdriverIndex = -1 then exit;
  savestat := fhCapwnd;
  if fhCapWnd = 0 then
  if not CreateCapWindow then exit;
  result :=capDlgVideoFormat(fhCapWnd);
  if result then GetDriverStatus(true);
  if savestat = 0 then destroyCapWindow;
  if result then
  begin
    Sizecap;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.DlgVDisplay:boolean;
var
  savestat: integer;
begin
  result:= false;
  if fdriverIndex = -1 then exit;
  savestat := fhCapwnd;
  if fhCapWnd = 0 then
    if not CreateCapWindow then exit;
  result:=capDlgVideoDisplay(fhCapWnd) ;
  if result then GetDriverStatus(true);
  if savestat = 0 then destroyCapWindow;
  if result then
  begin
    SizeCap;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.DlgVSource:boolean;
var
  savestat: integer;
begin
  result:= false;
  if fdriverIndex = -1 then exit;
  savestat := fhCapwnd;
  if fhCapWnd = 0 then
    if not createCapWindow then exit;
  result:= capDlgVideoSource(fhCapWnd);
  if result then GetDriverStatus(true);
  if savestat = 0 then destroyCapWindow;
  if result then
  begin
    SizeCap;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.DlgVCompression;
var
  savestat : integer;
begin
  result:= false;
  if fdriverIndex = -1 then exit;
  savestat := fhCapwnd;
  if fhCapWnd = 0 then
    if not createCapWindow then exit;
  result:=capDlgVideoCompression(fhCapWnd);
  if savestat = 0 then destroyCapWindow;
end;

//------------------------------------------------------------------------------
// Single Frame Grabbling
function TAPI_framegrab.GrabFrame:boolean;
begin
  result:= false;
  if not DriverOpen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;
  Result:= capGrabFrame(fhcapwnd);
  if result then GetDriverStatus(true);
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GrabFrameNoStop:boolean;
begin
  result:= false;
  if not DriverOpen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;
  Result:= capGrabFrameNoStop(fhcapwnd);
  if result then GetDriverStatus(true);
end;

//------------------------------------------------------------------------------
// save frame as DIP
function TAPI_framegrab.SaveAsDIB:Boolean;
var
  s: array[0..MAX_PATH] of char;
begin
  result:= false;
  if not DriverOpen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;
  result := capFileSaveDIB(fhcapwnd,strpCopy(s,fCapSingleImageFileName));
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.SaveToClipboard:boolean;
begin
  result:= false;
  if not Driveropen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;
  result:= capeditCopy(fhcapwnd);
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.Setoverlay(value:boolean);
begin
  if value = GetOverlay then exit;
  if gethasVideoOverlay = false then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver has no overlay mode');
    exit;
  end;
  if value = true then
  begin
    if fhcapWnd = 0 then  CreateCapWindow;
    GrabFrame;
  end;
  capOverlay(fhCapWnd,value);
  GetDriverStatus(true);
  invalidate;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetOverlay:boolean;
begin
  if fhcapWnd = 0 then result := false
    else result:= fpDriverStatus^.fOverlayWindow;
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetPreview(value:boolean);
begin
  if value = GetPreview then exit;
  if value = true then
    if fhcapWnd = 0 then  CreateCapWindow;
  capPreview(fhCapWnd,value);
  GetDriverStatus(true);
  invalidate;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetPreview:boolean;
begin
  if fhcapWnd = 0 then result := false
    else result:= fpDriverStatus^.fLiveWindow;
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetPreviewRate(value:word);
begin
  if value = fpreviewrate then exit;
  if value < 1 then value := 1;
  if value > 30 then value := 30;
  fpreviewrate:= value;
  if DriverOpen then capPreviewRate(fhCapWnd, round( 1/fpreviewrate*1000));
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetMicroSecPerFrame(value:cardinal);
begin
  if value = fmicrosecpframe then exit;
  if value < 33333 then value := 33333;
  fmicrosecpframe := value;
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.setFrameRate(value:word);
begin
  if value <> 0 then fmicrosecpframe:= round(1.0/value*1000000.0);
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetFrameRate:word;
begin
  if fmicrosecpFrame > 0   then
    result:= round(1./ fmicrosecpframe * 1000000.0)
    else result:= 0;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.StartCapture;
var
  CapParms: TCAPTUREPARMS;
  name: array[0..MAX_PATH] of char;
begin
  result := false;
  if not DriverOpen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;

  capCaptureGetSetup(fhCapWnd, @CapParms, sizeof(TCAPTUREPARMS));
  if ftempfilename='' then capFileSetCaptureFile(fhCapWnd,strpCopy(name, fCapVideoFileName));

  CapParms.dwRequestMicroSecPerFrame:= fmicrosecpframe;
  CapParms.fLimitEnabled:= BOOL(FCapTimeLimit);
  CapParms.wTimeLimit:= fCapTimeLimit;
  CapParms.fCaptureAudio:= fCapAudio;
  CapParms.fMCIControl:= FALSE;
  CapParms.fYield:= TRUE;
  CapParms.vKeyAbort:= VK_ESCAPE;
  CapParms.fAbortLeftMouse:= FALSE;
  CapParms.fAbortRightMouse:= FALSE;
  if CapParms.fLimitEnabled then
  begin
    CapParms.dwIndexSize:= frameRate*FCapTimeLimit;
    If fCapAudio then CapParms.dwIndexSize := CapParms.dwIndexSize + 5*FCapTimeLimit;
  end else
  begin
    If CapParms.dwIndexSize = 0 then  CapParms.DwIndexSize := 100000
      else CapParms.dwIndexSize := findexSize;
  end;
  if CapParms.dwIndexSize < 1800 then CapParms.dwIndexSize:= 1800;
  If CapParms.dwIndexSize > 324000 then CapParms.dwIndexSize:= 324000;
  capCaptureSetSetup(fhCapWnd, @CapParms, sizeof(TCAPTUREPARMS));
  if fCapAudio then FAudioformat.SetAudio(fhcapWnd);
  if CapToFile then result:= capCaptureSequence(fhCapWnd)
    else result := capCaptureSequenceNoFile(fhCapWnd);
  GetDriverStatus(true);
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.StopCapture;
begin
  result:=false;
  if not DriverOpen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;
  result:=CapCaptureStop(fhcapwnd);
  GetDriverStatus(true);
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.SaveCap:Boolean;
var
  name: array[0..MAX_PATH] of char;
begin
  result := capFileSaveAs(fhcapwnd,strPCopy(name,fCapVideoFileName));
end;

//------------------------------------------------------------------------------
procedure TAPI_framegrab.SetIndexSize(value:cardinal);
begin
  if value = 0 then
  begin
    findexSize:= 0;
    exit;
  end;
  if value < 1800 then value := 1800;
  if value > 324000 then value := 324000;
  findexsize:= value;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetCapInProgress:boolean;
begin
  result:= false;
  if not DriverOpen then exit;
  GetDriverStatus(false);
  result:= fpDriverStatus^.fCapturingNow ;
end;

//------------------------------------------------------------------------------
Procedure TAPI_framegrab.SetScale(value:boolean);
begin
  if value = fscale then  exit;
  fscale:= value;
  if DriverOpen then
  begin
    capPreviewScale(fhCapWnd, fscale);
    SizeCap;
  end;
  Repaint;
end;

//------------------------------------------------------------------------------
Procedure TAPI_framegrab.Setprop(value:Boolean);
begin
  if value = fprop then exit;
  fprop:=value;
  if DriverOpen then Sizecap;
  Repaint;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetCapWidth;
begin
  if assigned(fpDriverStatus) then
    result:= fpDriverStatus^.uiImageWidth
    else result:= 0;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetCapHeight;
begin
  if assigned(fpDriverStatus) then
    result:= fpDriverStatus^.uiImageHeight
    else result:= 0;
end;

//------------------------------------------------------------------------------
Procedure TAPI_framegrab.SetDriverOpen(value:boolean);
begin
  if value = GetDriverOpen then exit;
  if value = false then DestroyCapWindow;
  if value = true then CreateCapWindow;
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.GetDriverOpen:boolean;
begin
  result := fhcapWnd <> 0;
end;

//------------------------------------------------------------------------------
// Singele frame Capturing
function TAPI_framegrab.CapSingleFramesOpen:boolean;
var
  name: array [0..MAX_PATH] of char;
  CapParms: TCAPTUREPARMS;
begin
  result := false;
  if not DriverOpen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;

  capCaptureGetSetup(fhCapWnd, @CapParms, sizeof(TCAPTUREPARMS));
  if ftempfilename='' then capFileSetCaptureFile(fhCapWnd,strpCopy(name, fCapVideoFileName));

  CapParms.dwRequestMicroSecPerFrame := fmicrosecpframe;
  CapParms.fLimitEnabled:= BOOL(0);
  CapParms.fCaptureAudio:= false;
  CapParms.fMCIControl:= FALSE;
  CapParms.fYield:= TRUE;
  CapParms.vKeyAbort:= VK_ESCAPE;
  CapParms.dwIndexSize:= findexSize;
  if CapParms.dwIndexSize < 1800 then CapParms.dwIndexSize:= 1800;
  If CapParms.dwIndexSize > 324000 then CapParms.dwIndexSize:= 324000;
  capCaptureSetSetup(fhCapWnd, @CapParms, sizeof(TCAPTUREPARMS));

  result:= capCaptureSingleFrameOpen(fhcapWnd);
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.CapSingleFramesClose: boolean;
begin
  result:=false;
  if not driverOpen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;
  result:= CapCaptureSingleFrameClose(fhcapWnd);
end;

//------------------------------------------------------------------------------
function TAPI_framegrab.CapSingleFrame:boolean;
begin
  result:=false;
  if not driverOpen then
  begin
    if assigned(fonerror) then
      fonerror(self, 'Driver is not open');
    exit;
  end;
  result:= CapCaptureSingleFrame(fhcapWnd);
end;

//------------------------------------------------------------------------------
constructor TAudioFormat.create;
begin
  inherited create;
  FChannels:=Mono;
  FFrequency:=f8000Hz;
  Fres:=r8Bit;
end;

//------------------------------------------------------------------------------
procedure TAudioFormat.SetAudio(handle:Thandle);
Var
  WAVEFORMATEX: TWAVEFORMATEX;
begin
  if handle= 0 then exit;
  capGetAudioFormat(handle,@WAVEFORMATEX, SizeOf(TWAVEFORMATEX));
  case FFrequency of
    f8000hz: WAVEFORMATEX.nSamplesPerSec:=8000;
    f11025Hz: WAVEFORMATEX.nSamplesPerSec:=11025;
    f22050Hz: WAVEFORMATEX.nSamplesPerSec:=22050;
    f44100Hz: WAVEFORMATEX.nSamplesPerSec:=44100;
  end;
  WAVEFORMATEX.nAvgBytesPerSec:= WAVEFORMATEX.nSamplesPerSec;
  if FChannels=Mono then WAVEFORMATEX.nChannels:=1
    else WAVEFORMATEX.nChannels:=2;
  if FRes=r8Bit then WAVEFORMATEX.wBitsPerSample:=8
    else WAVEFORMATEX.wBitsPerSample:=16;
  capSetAudioFormat(handle,@WAVEFORMATEX, SizeOf(TWAVEFORMATEX));
end;

//------------------------------------------------------------------------------
// Creating a list with capture drivers
Function TAPI_framegrab.GetDriverList:TStringList;
var
  i: integer;
  name: array[0..80] of char;
  ver: array[0..80] of char;
begin
  result:= TStringList.Create;
  result.Capacity:= 10;
  result.Sorted:= false;
  for i:= 0 to 9 do
    if capGetDriverDescription( i,name,80,ver,80) then result.Add(StrPas(name)+ ' '+strpas(ver))
      else break;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_framegrab]);
end;

end.

