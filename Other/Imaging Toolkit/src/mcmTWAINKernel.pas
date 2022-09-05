// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  15898: mcmTWAINKernel.pas
//
//    Rev 1.19    2014-03-28 17:52:54  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.18    2014-01-15 13:41:58  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.17    2013-12-04 23:16:14  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.16    2013-11-27 16:27:46  mcm
// Eliminated use of TThread.Resume & Suspend. As an upside, the TmcmTWAIN
// threads only runs while communicating with TWAIN and drivers.
//
//    Rev 1.15    08-01-2009 21:09:24  mcm    Version: DT 3.8
// Added support for Delphi 2009
//
//   Rev 1.14    11-06-2005 09:49:22  mcm    Version: DT 3.5
// Solved double "." error in the filename.

//
//   Rev 1.13    15-05-2005 21:53:26  mcm    Version: DT 3.5

//
//   Rev 1.12    19-02-2005 00:31:46  mcm    Version: DT 3.4
// Added support for configuration mode of data sources.

//
//   Rev 1.11    28-10-2004 20:04:12  mcm    Version: DT 3.2
// Moved code to InitializeThreads and TerminateThreads. 
// Added extra checks of FTWAINThread member variable before use.

//
//   Rev 1.10    16-09-2004 20:44:52  mcm
// Modified shut-down mechanisme to avoid dead-lock if application has been idle
// for approximately 15 min.

//
//   Rev 1.9    05-09-2004 19:56:18  mcm    Version: DT 3.2
// Minor change to ensure that string members on FAppIdentity never exceeds
// maximum size in the Create method.

//
//   Rev 1.8    03-01-2004 14:19:56  mcm
// Remove un-necessary PostMessage().

//
//   Rev 1.7    10-12-2003 15:29:58  mcm    Version: DT3.0
// Solution below is only supported in Delphi 6 and later.

//
//   Rev 1.6    10-12-2003 13:42:16  mcm    Version: DT3.0
// Modified to enable incorporation in a DLL file by adding a Hook checking for
// idle messages which then synchronizes calls from TmcmTWAINThread to the
// Application.
// To ensure an idle message (WM_NULL) is sent WakeMainThread is assigned the
// procedure DoWakeMainThread.

//
//   Rev 1.5    05-12-2003 11:47:58  mcm
// Replaced Parent.Handle with GetForegroundWindow.
// Removed un-necessary code (WinPos - TRect).

//
//   Rev 1.4    12-11-2003 14:01:24  mcm    Version: DT3.0
// Changed calls in OpenDS, EnableDS and SelectDS to include Parent.Handle.

//
//   Rev 1.2.1.10    04-11-2003 23:13:46  mcm    Version: DT3.0
// Moved code to the TmcmTWAINThread and TmcmTWAINQueue threads.

//
//   Rev 1.2.1.6    06-07-2003 11:13:10  mcm    Version: DT 2.5
// Modified logging to include pre-calls to Data source close, disable, enable
// and open.

//
//   Rev 1.2.1.5    14-06-2003 10:17:02  mcm    Version: DT 2.4
// Updated version information.

//
//   Rev 1.2.1.4    17-05-2003 12:56:22  mcm
// Added support for SPIFF and EXIF.

//
//   Rev 1.2.1.3    16-05-2003 20:53:58  mcm
// Internal revision (ActiveTWAIN).

//
//   Rev 1.2.1.2    15-04-2003 10:52:00  mcm    Version: DT 2.3
// Added call to LogTriplet before calling the DSM Entry procedure when Msg =
// MSG_SET to show the value(s) sent to the data source.
// Modified DSMEntry to close a data source if an exception occur. The data
// source (DLL file) is simply unloaded from memory.

//
//   Rev 1.2.1.1    06-03-2003 10:46:06  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.
// Fixed missing implementation of ImageHeight/Width.

//
//   Rev 1.2.1.0    07-10-2002 15:57:10  mcm    Version: DT2.1

//
//   Rev 1.2    18-01-2002 15:11:56  mcm    Version: DT 2.0
// Access to all property members are through function/procedures.

//
//   Rev 1.1    11-01-2002 15:18:36  mcm    Version: DT 2.0
// Added compiler directive to exclude uses mcmTWAINLog when TWNDEBUG is not set.

//
//   Rev 1.0    04-12-2001 16:49:08  mcm    Version: DT 2.0

unit mcmTWAINKernel;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, Vcl.Graphics, Vcl.Controls, Vcl.Forms, System.SysUtils,
     System.Classes, WinApi.Messages,
     {$IFDEF GE_DXE4}
     System.AnsiStrings,
     {$ENDIF}
     {$ELSE}
     Windows, Messages, Classes, SysUtils, Controls, Graphics, Forms,
     {$ENDIF}
     twain, twainproc, mcmTWAINThread, mcmTWAINQueue;

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

type
  TmcmTWAINKernel = class(TCustomControl)
  private
    { Private declarations }
    FOnCloseSource  : TNotifyEvent;      // Data source closed event
  protected
    { Protected declarations }
    FParentWnd      : hWnd;
    FAppHandle      : hWnd;
    FIsDemoVer      : boolean;           // Is demo version.
    FTWAINThread    : TmcmTWAINThread;   //
    FAppIdentity    : TW_IDENTITY;       // Storage for App identification
    FSourceID       : TW_IDENTITY;       // Storage for DS (Source) identification
    FSourceVersion  : word;
    FAllFormats     : TTwnFileFmts;      // Image (file) formats supported by application.

    FXferMech       : TTwnXferType;      // Transfer method [Native, Memory, file]
    FHandleType     : TTwnHdlType;       // Dib handle to return to application.
    FFileName       : string;            // File name of stored image
    FPrefFormat     : TTwnFileFmt;       // File format to use for file transfer.
    FFileAsDIB      : boolean;           // Return bitmap file image in Dib handle
    FMemFlipBmp     : boolean;           // Flip RGB image if transfered via Memory.

    FMsgLevel       : TTwnErrorLevel;    // Filter which messages to save.
    FLogToFile      : boolean;           // On True Save messages to .\APPTWN.LOG
    FLogFileName    : string;            // The LOG file name

    // Event Handlers
    FOnDisableMenus : TNotifyEvent;      // App should disable TWAIN menus.
    FOnEnableMenus  : TNotifyEvent;      // App that it can enable TWAIN menus.
    FOnFailure      : TFailureEvent;     // Event - An error occured.
    FOnImageReady   : TImageEvent;       // Image is acquired from TWAIN driver.
    FOnMemXferBuf   : TMemXferBufEvent;  // An image chunk is acquired from the
                                         // TWAIN driver.
                                         // Only fired when Transfer method is
                                         // TWFX_Memory and DIBHandleType is
                                         // THDT_MEMPTR.
    FOnMemXferSize  : TMemXferSizeEvent; // Negotiate Memory buffer size.
                                         // Only fired when Transfer method is
                                         // TWFX_Memory and DIBHandleType is
                                         // THDT_MEMPTR.
    FOnStateChanged : TStateChanged;     // State change event
    FOnXferNext     : TXferNextEvent;    // Source reports more images.
    FOnXferReady    : TNotifyEvent;      // Source reports, image transfer ready.

    FPageCount      : integer;

    procedure   DoWakeMainThread(Sender : TObject);
    procedure   InitializeThreads;
    procedure   TerminateThreads;

    procedure   CloseConnection;
    function    CloseDS : TW_UINT16;
    function    CloseDSM : TW_UINT16;
    function    ConfigureDS      (var ShowUI        : boolean;
                                  var ModalUI       : boolean) : TW_UINT16;
    function    CreateFileName   (    AFileName     : string;
                                      AFileExt      : string) : string;
    function    DisableDS : TW_UINT16;
    procedure   DoOnCloseSource  (    Sender : TObject);
    procedure   DoOnEnableMenus  (    Sender : TObject);
    procedure   DoOnFailure      (    Sender    : TObject;
                                      DG        : longint;
                                      DAT       : word;
                                      CAP       : word;
                                      MSG       : word;
                                      Error     : integer;
                                      Status    : integer);
    procedure   DoOnImageReady   (    Sender    : TObject;
                                      pBmp      : pointer;
                                      pBmpInfo  : PBitmapInfo;
                                      hImage    : hBitmap;
                                      FilePath  : string);
    procedure   DoOnMemXferBuf   (    Sender       : TObject;
                                      pBmpInfo     : PBitmapInfo;
                                      BytesPerRow  : integer;
                                      Rows         : integer;
                                      DataSize     : integer;
                                  var pData        : pointer);
    procedure   DoOnMemXferSize  (    Sender       : TObject;
                                      MinSize      : integer;
                                      MaxSize      : integer;
                                  var BufSize      : integer;
                                      pBmpInfo     : PBitmapInfo);
    procedure   DoOnDeviceEvent  (    Sender    : TObject); virtual; abstract;
    procedure   DoOnXferReady    (    Sender    : TObject);
    procedure   DoOnXferNext     (    Sender    : TObject;
                                  var NumImages : Integer;
                                  var SkipNext  : boolean);

    function    DSMEntry         (    pOrigin       : pTW_IDENTITY;
                                      pDest         : pTW_IDENTITY;
                                      DG            : TW_UINT32;
                                      DAT           : TW_UINT16;
                                      MSG           : TW_UINT16;
                                      pData         : TW_MEMREF) : TW_UINT16;

    function    EnableDS         (var ShowUI        : boolean;
                                  var ModalUI       : boolean) : TW_UINT16;
    function    FileFormat2Ext   (    twnFormat     : TTwnFileFmt) : string;
    function    FileName2Format  (    FileName      : string) : TTwnFileFmt;

    function    GetAcqFlag : TW_INT16;
    function    GetDSResult : word;
    function    GetDSStatus : word;
    function    GetFileFormat : TTwnFileFmt;
    function    GetFilename : string;
    procedure   SetFilename(Value : string);
    function    GetHandleType : TTwnHdlType;
    function    GetImageHeight : integer;
    function    GetImageWidth : integer;
    function    GetIsDSMOpen : bool;
    function    GetIsDSOpen : bool;
    function    GetIsDSEnabled : bool;
    function    GetLogFileName : string;
    function    GetLogToFile : boolean;
    function    GetMessageLevel : TTwnErrorLevel;
    function    GetOnCloseSource : TNotifyEvent;
    function    GetOnDisableMenus : TNotifyEvent;
    function    GetOnEnableMenus : TNotifyEvent;
    function    GetOnFailure : TFailureEvent;
    function    GetOnImageReady : TImageEvent;
    function    GetOnMemXferBuf : TMemXferBufEvent;
    function    GetOnMemXferSize : TMemXferSizeEvent;
    function    GetOnXferNext : TXferNextEvent;
    function    GetOnXferReady : TNotifyEvent;
    function    GetReturnHandle : boolean;
    function    GetSourceID : TW_IDENTITY;
    function    GetState : word;
    function    GetStatus        (var Status        : TW_UINT16;
                                      pSourceID     : pTW_IDENTITY) : TW_UINT16;
    function    GetSwapMemRGB : boolean;
    function    GetWinHandle : THandle;
    function    GetXferMech : TTwnXferType;
    function    ImageInfoA       (    pImageInfo    : pTW_IMAGEINFO) : TW_UINT16;
    function    ImageLayoutA     (    pImageLayout  : pTW_IMAGELAYOUT;
                                      Msg           : TW_UINT16) : TW_UINT16;

    procedure   LogMessage       (    LogStr        : string);
    function    OpenDS  : TW_UINT16;
    function    OpenDSM : TW_UINT16;
    function    Palette8         (    Palette       : pLogPalette;
                                  var PalType       : word;
                                      Msg           : TW_UINT16) : TW_UINT16;
    function    RemoveExt        (    Value         : string) : string;
    function    SelectDS  : TW_UINT16;
    procedure   SetAcqFlag       (    Value         : TW_INT16);
    procedure   SetFileFormat    (    Value         : TTwnFileFmt);
    procedure   SetHandleType    (    Value         : TTwnHdlType);
    procedure   SetLogFileName   (    Value         : string);
    procedure   SetLogToFile     (    Value         : boolean);
    procedure   SetMessageLevel  (    Value         : TTwnErrorLevel);
    procedure   SetOnCloseSource(Value : TNotifyEvent);
    procedure   SetOnDisableMenus(Value : TNotifyEvent);
    procedure   SetOnEnableMenus (    Value         : TNotifyEvent);
    procedure   SetOnFailure     (    Value         : TFailureEvent);
    procedure   SetOnImageReady  (    Value         : TImageEvent);
    procedure   SetOnMemXferBuf  (    Value         : TMemXferBufEvent);
    procedure   SetOnMemXferSize (    Value         : TMemXferSizeEvent);
    procedure   SetOnXferNext    (    Value         : TXferNextEvent);
    procedure   SetOnXferReady(Value : TNotifyEvent);
    procedure   SetReturnHandle  (    Value         : boolean);
    procedure   SetSourceID      (    Value         : TW_IDENTITY);
    procedure   SetState         (    Value         : word);
    procedure   SetSwapMemRGB    (    Value         : boolean);
    procedure   SetXferMech      (    Value         : TTwnXferType);

    property    AcqFlag : TW_INT16
      read      GetAcqFlag
      write     SetAcqFlag;
    property    DIBHandleType : TTwnHdlType
      read      GetHandleType
      write     SetHandleType default THDT_DIBHANDLE;
    property    FileFormat : TTwnFileFmt
      read      GetFileFormat
      write     SetFileFormat default TWFF_BMP;
    property    Filename : string
      read      GetFilename
      write     SetFilename;
    property    ImageWidth : integer
      read      GetImageWidth;
    property    ImageHeight : integer
      read      GetImageHeight;
    property    IsDSMOpen : bool
      read      GetIsDSMOpen;
    property    IsDSOpen : bool
      read      GetIsDSOpen;
    property    IsDSEnabled : bool
      read      GetIsDSEnabled;
    property    SourceID : TW_IDENTITY
      read      GetSourceID
      write     SetSourceID;
    property    State : word
      read      GetState
      write     SetState;
    property    WinHandle : THandle
      read      GetWinHandle;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    procedure   SetParentWnd(Wnd : hWnd);

    property    DSResult : word
      read      GetDSResult;
    property    DSStatus : word
      read      GetDSStatus;
    property    LogFilename : string
      read      GetLogFileName
      write     SetLogFileName;
    property    LogToFile : boolean // Save error messages to .\APPTWN.LOG
      read      GetLogToFile
      write     SetLogToFile default False;
    property    MessageLevel : TTwnErrorLevel
      read      GetMessageLevel
      write     SetMessageLevel default ML_NONE;

    property    ReturnHandle : boolean
      read      GetReturnHandle
      write     SetReturnHandle default True;
    property    SwapMemRGB : boolean
      read      GetSwapMemRGB
      write     SetSwapMemRGB default True;
    property    XferMech : TTwnXferType
      read      GetXferMech
      write     SetXferMech default TWFX_NATIVE;

    property    OnCloseSource  : TNotifyEvent
      read      GetOnCloseSource
      write     SetOnCloseSource;
    property    OnDisableMenus : TNotifyEvent
      read      GetOnDisableMenus
      write     SetOnDisableMenus;
    property    OnEnableMenus  : TNotifyEvent
      read      GetOnEnableMenus
      write     SetOnEnableMenus;
    property    OnFailure : TFailureEvent
      read      GetOnFailure
      write     SetOnFailure;
    property    OnImageReady : TImageEvent
      read      GetOnImageReady
      write     SetOnImageReady;
    property    OnMemXferBuffer : TMemXferBufEvent
      read      GetOnMemXferBuf
      write     SetOnMemXferBuf;
    property    OnMemXferSize : TMemXferSizeEvent
      read      GetOnMemXferSize
      write     SetOnMemXferSize;
    property    OnXferNext : TXferNextEvent
      read      GetOnXferNext
      write     SetOnXferNext;
    property    OnXferReady : TNotifyEvent
      read      GetOnXferReady
      write     SetOnXferReady;
    published
    { Pulished declarations }
    {$IFDEF ACTIVETWAIN}
    property    AppHandle : hWnd // Internal use only
      read      FAppHandle
      write     FAppHandle;
    {$ENDIF}
  end;

implementation

{$IFDEF DCB3_5}
uses FileCtrl;
{$ENDIF}

{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}

{$IFNDEF DCB3_5}
var FMsgHook : HHook;

function mcmGetMsgHook(nCode: Integer; wParam: Longint; var Msg: TMsg): Longint; stdcall;
begin
  Result := CallNextHookEx(FMsgHook, nCode, wParam, Longint(@Msg));
  if (Msg.Message = WM_NULL)
  then if (GetCurrentThreadID = MainThreadID)
       then CheckSynchronize;
end; // mcmGetMsgHook.

{$ENDIF}

procedure TmcmTWAINKernel.DoWakeMainThread(Sender : TObject);
begin
  PostThreadMessage(MainThreadID, WM_NULL, 0, 0);
end; // TmcmTWAINKernel.DoWakeMainThread.


procedure TmcmTWAINKernel.InitializeThreads;
begin
  FTWAINThread := TmcmTWAINThread.Create(False);
  if (FTWAINThread <> Nil)
  then begin
       FTWAINThread.OnCloseSource := DoOnCloseSource;
       FTWAINThread.OnEnableMenus := DoOnEnableMenus;
       FTWAINThread.OnFailure := DoOnFailure;
       FTWAINThread.OnImageReady := DoOnImageReady;
       FTWAINThread.OnMemXferBuffer := DoOnMemXferBuf;
       FTWAINThread.OnMemXferSize := DoOnMemXferSize;
       FTWAINThread.OnDeviceEvent := DoOnDeviceEvent;
       FTWAINThread.OnXferNext := DoOnXferNext;
       FTWAINThread.OnXferReady := DoOnXferReady;

       FTWAINThread.ParentWindow := GetForegroundWindow;
       FTWAINThread.ApplicationID := @FAppIdentity;
       FTWAINThread.SourceID := @FSourceID;

       // Set-up TWAINThread properties.
       FTWAINThread.MessageLevel := FMsgLevel;
       FTWAINThread.LogToFile := FLogToFile;
       FTWAINThread.LogFilename := FLogFileName;

       FTWAINThread.ReturnHandle := FFileAsDIB;
       FTWAINThread.SwapMemRGB := FMemFlipBmp;
       FTWAINThread.DIBHandleType := FHandleType;
       FTWAINThread.XferMech := FXferMech;
       FTWAINThread.FileFormat := FPrefFormat;
       FTWAINThread.Filename := FFilename;
  end;
end; // TmcmTWAINKernel.InitializeThreads.


procedure TmcmTWAINKernel.TerminateThreads;
begin
  if (FTWAINThread <> Nil)
  then begin
       try
         FTWAINThread.ClearEventProcs;

         //FTWAINThread.Resume;
         //FTWAINThread.Suspended := False;
         //while FTWAINThread.Suspended
         //do FTWAINThread.Resume;
         FTWAINThread.QuitThread;
         FTWAINThread.WaitFor;
         FTWAINThread.Free;
       except
       end;
  end;
  FTWAINThread := Nil;
end; // TmcmTWAINKernel.TerminateThreads.


constructor TmcmTWAINKernel.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);

  FParentWnd := GetForegroundWindow;

 {$IFNDEF DCB3_5}
  WakeMainThread := DoWakeMainThread;
  FMsgHook := SetWindowsHookEx(WH_GETMESSAGE, @mcmGetMsgHook, 0, MainThreadID);
 {$ENDIF}

  // Set up the information about your application which you want to pass
  // to the SM in this call.
  // Move all these text strings off to a resource fill for a real
  // application. They are here for easier readablilty for the student.
  // init to 0, but Source Manager will assign real value
  FillChar(FAppIdentity, SizeOf(TW_IDENTITY), 0); // Clear content of FAppIdentity
  with FAppIdentity
  do begin
    Id               := 0;
    Version.MajorNum := 3;
    Version.MinorNum := 0;
    Version.Language := word(twain.TWLG_ENG);
    Version.Country  := TWCY_UNITEDKINGDOM;
    {$IFDEF GE_DXE4}
    System.AnsiStrings.StrPLCopy(Version.Info, 'ImagingToolbox.com, TWAIN Toolkit', 32);
    {$ELSE}
    StrPLCopy(Version.Info, 'ImagingToolbox.com, TWAIN Toolkit', 32);
    {$ENDIF}
    ProtocolMajor    := TWON_PROTOCOLMAJOR;
    ProtocolMinor    := TWON_PROTOCOLMINOR;
    SupportedGroups  := DG_IMAGE or DG_CONTROL;
  end;
  {$IFDEF GE_DXE4}
  System.AnsiStrings.StrPCopy(FAppIdentity.Manufacturer,  'ImagingToolbox.com');
  System.AnsiStrings.StrPCopy(FAppIdentity.ProductFamily, 'TWAIN Toolkit');
  System.AnsiStrings.StrPCopy(FAppIdentity.ProductName,   'TWAIN Toolkit');
  {$ELSE}
  StrPCopy(FAppIdentity.Manufacturer,  'ImagingToolbox.com');
  StrPCopy(FAppIdentity.ProductFamily, 'TWAIN Toolkit');
  StrPCopy(FAppIdentity.ProductName,   'TWAIN Toolkit');
  {$ENDIF}

  FillChar(FSourceID, SizeOf(TW_IDENTITY), 0); // Clear content of FSourceID
  FSourceID.Id := 0;
  FTWAINThread := Nil;

  // Initialise properties.
  FMsgLevel    := ML_NONE;
  FLogToFile   := False;
  FLogFileName := '.\APPTWN.LOG';

  FMemFlipBmp  := True;
  FFileAsDIB   := True;
  FXferMech    := TWFX_NATIVE;
  FHandleType  := THDT_DIBHANDLE;
  FFilename    := '';
  FPrefFormat  := TWFF_BMP;
  FAllFormats  := [TWFF_BMP];

  FOnCloseSource  := Nil;
  FOnDisableMenus := Nil;
  FOnEnableMenus  := Nil;
  FOnFailure      := Nil;
  FOnImageReady   := Nil;
  FOnMemXferBuf   := Nil;
  FOnMemXferSize  := Nil;
  FOnStateChanged := Nil;
  FOnXferNext     := Nil;
  FOnXferReady    := Nil; 
end; // TmcmTWAINKernel.Create.


destructor TmcmTWAINKernel.Destroy;
begin
  {$IFNDEF DCB3_5}
    if (FMsgHook <> 0)
    then UnhookWindowsHookEx(FMsgHook);
  {$ENDIF}
  Inherited Destroy;
end; // TmcmTWAINKernel.Destroy.


procedure TmcmTWAINKernel.SetParentWnd(Wnd : hWnd);
begin
  if (Wnd = 0)
  then FParentWnd := GetForegroundWindow
  else FParentWnd := Wnd;
end; // TmcmTWAINKernel.SetParentWnd.


function TmcmTWAINKernel.DSMEntry(pOrigin : pTW_IDENTITY;
                                  pDest   : pTW_IDENTITY;
                                  DG      : TW_UINT32;
                                  DAT     : TW_UINT16;
                                  MSG     : TW_UINT16;
                                  pData   : TW_MEMREF) : TW_UINT16;
begin
  if (FTWAINThread <> Nil)
  then Result := FTWAINThread.DSMEntry(pOrigin, pDest, DG, DAT, MSG, pData)
  else Result := TWRC_FAILURE;
  Sleep(0);
  if (Result <> TWRC_SUCCESS)
  then Sleep(0);
end; // TmcmTWAINKernel.DSMEntry.


function TmcmTWAINKernel.GetStatus(var Status    : TW_UINT16;
                                       pSourceID : pTW_IDENTITY) : TW_UINT16;
begin
  Status := TWCC_BUMMER;
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.GetStatus(Status, pSourceID)
  else Result := 0;
end; // TmcmTWAINKernel.GetStatus.


function TmcmTWAINKernel.GetIsDSMOpen : bool;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.IsDSMOpen
  else Result := False;
end; // TmcmTWAINKernel.GetIsDSMOpen.


function TmcmTWAINKernel.GetIsDSOpen : bool;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.IsDSOpen
  else Result := False;
end; // TmcmTWAINKernel.GetIsDSOpen.


function TmcmTWAINKernel.GetIsDSEnabled : bool;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.IsDSEnabled
  else Result := False;
end; // TmcmTWAINKernel.GetIsDSEnabled.


function TmcmTWAINKernel.GetState : word;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.State
  else Result := 0;
end; // TmcmTWAINKernel.GetState.


procedure TmcmTWAINKernel.SetState(Value : word);
begin
  if Assigned(FTWAINThread)
  then FTWAINThread.State := Value;
end; // TmcmTWAINKernel.SetState.


function TmcmTWAINKernel.GetDSResult : word;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.DSResult
  else Result := 0;
end; // TmcmTWAINKernel.GetDSResult.


function TmcmTWAINKernel.GetDSStatus : word;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.DSStatus
  else Result := 0;
end; // TmcmTWAINKernel.GetDSStatus.


function TmcmTWAINKernel.GetWinHandle : THandle;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.WinHandle
  else Result := 0;
end; // TmcmTWAINKernel.GetWinHandle.


function TmcmTWAINKernel.GetAcqFlag : TW_INT16;
begin
  Result := FTWAINThread.AcqFlag;
end; // TmcmTWAINKernel.GetAcqFlag.


procedure TmcmTWAINKernel.SetAcqFlag(Value : TW_INT16);
begin
  if Assigned(FTWAINThread)
  then FTWAINThread.AcqFlag := Value;
end; // TmcmTWAINKernel.SetAcqFlag.


function TmcmTWAINKernel.GetFileFormat : TTwnFileFmt;
begin
  if Assigned(FTWAINThread)
  then FPrefFormat := FTWAINThread.FileFormat;
  Result := FPrefFormat;
end; // TmcmTWAINKernel.GetFileFormat.


procedure TmcmTWAINKernel.SetFileFormat(Value : TTwnFileFmt);
var AName : string;
begin
  if Not(Value in [TWFF_PICT, TWFF_XBM])
  then begin
       FPrefFormat := Value;
       if Assigned(FTWAINThread)
       then FTWAINThread.FileFormat := Value;
       if (Length(FileName) > 0)
       then begin
            AName := lowercase(RemoveExt(FileName));
            FileName := AName + '.' + FileFormat2Ext(FileFormat);
       end;
  end;
end; // TmcmTWAINKernel.SetFileFormat.


function TmcmTWAINKernel.GetFilename : string;
begin
  if Assigned(FTWAINThread)
  then FFilename := FTWAINThread.Filename;
  Result := FFilename;
end; // TmcmTWAINKernel.GetFilename.


procedure TmcmTWAINKernel.SetFilename(Value : string);
var TmpFormat : TTwnFileFmt;
    FileExt   : string;
begin
  if (Length(Value) > 0)
  then begin
       Value := LowerCase(Value);
       TmpFormat := FileName2Format(Value);
       if (TmpFormat <> FileFormat)
       then begin
            // Compensate for GeTTwnFileFormat returning TWFF_TIFF when file
            // extension is '.TIF'.
            case FileFormat of
            TWFF_TIFF      : ;
            TWFF_TIFFMULTI : ;
            TWFF_JFIF      : ;
            TWFF_SPIFF     : ;
            TWFF_EXIF      : ;
            else SetFileFormat(TmpFormat);
            end;
            //if Not((TmpFormat = TWFF_TIFF) and (FPrefFormat = TWFF_TIFFMULTI))
            //then SetFileFormat(TmpFormat);
       end;

       // Validate that the filename is OK. If not a valid filename is created.
       FileExt := FileFormat2Ext(TmpFormat);
       FFilename := CreateFileName(Value, FileExt);
       if (FTWAINThread <> Nil)
       then FTWAINThread.Filename := FFilename;
  end
  else begin
       FTWAINThread.Filename  := '';
       FileFormat := TWFF_BMP;
       SetReturnHandle(True);
  end;
end; // TmcmTWAINKernel.SetFilename.


function TmcmTWAINKernel.GetHandleType : TTwnHdlType;
begin
  if Assigned(FTWAINThread)
  then FHandleType := FTWAINThread.DIBHandleType
  else FHandleType := THDT_DIBHANDLE;
  Result := FHandleType;
end; // TmcmTWAINKernel.GetHandleType.


procedure TmcmTWAINKernel.SetHandleType(Value : TTwnHdlType);
begin
  FHandleType := Value;
  if Assigned(FTWAINThread)
  then FTWAINThread.DIBHandleType := FHandleType;
end; // TmcmTWAINKernel.SetHandleType.


function TmcmTWAINKernel.GetReturnHandle : boolean;
begin
  if Assigned(FTWAINThread)
  then FFileAsDIB := FTWAINThread.ReturnHandle;
  Result := FFileAsDIB;
end; // TmcmTWAINKernel.GetReturnHandle.


procedure TmcmTWAINKernel.SetReturnHandle(Value : boolean);
begin
  FFileAsDIB := Value;
  if Assigned(FTWAINThread)
  then FTWAINThread.ReturnHandle := FFileAsDIB;
end; // TmcmTWAINKernel.SetReturnHandle.


function TmcmTWAINKernel.GetImageHeight : integer;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.ImageHeight
  else Result := 0;
end; // TmcmTWAINKernel.GetImageHeight.


function TmcmTWAINKernel.GetImageWidth : integer;
begin
  if Assigned(FTWAINThread)
  then Result := FTWAINThread.ImageWidth
  else Result := 0;
end; // TmcmTWAINKernel.GetImageWidth.


function TmcmTWAINKernel.GetSwapMemRGB : boolean;
begin
  if Assigned(FTWAINThread)
  then FMemFlipBmp := FTWAINThread.SwapMemRGB;
  Result := FMemFlipBmp;
end; // TmcmTWAINKernel.GetSwapMemRGB.


procedure TmcmTWAINKernel.SetSwapMemRGB(Value : boolean);
begin
  FMemFlipBmp := Value;
  if Assigned(FTWAINThread)
  then FTWAINThread.SwapMemRGB := FMemFlipBmp;
end; // TmcmTWAINKernel.SetSwapMemRGB.


function TmcmTWAINKernel.GetXferMech : TTwnXferType;
begin
  if Assigned(FTWAINThread)
  then FXferMech := FTWAINThread.XferMech;
  Result := FXferMech;
end; // TmcmTWAINKernel.GetXferMech.


procedure TmcmTWAINKernel.SetXferMech(Value : TTwnXferType);
begin
  FXferMech := Value;
  if Assigned(FTWAINThread)
  then FTWAINThread.XferMech := FXferMech;
end; // TmcmTWAINKernel.SetXferMech.


function TmcmTWAINKernel.ImageInfoA(pImageInfo : pTW_IMAGEINFO) : TW_UINT16;
begin
  Result := FTWAINThread.ImageInfo(pImageInfo);
end; // TmcmTWAINKernel.ImageInfoA.


function TmcmTWAINKernel.ImageLayoutA(pImageLayout : pTW_IMAGELAYOUT;
                                      Msg          : TW_UINT16) : TW_UINT16;
begin
  Result := FTWAINThread.ImageLayout(pImageLayout, Msg);
end; // TmcmTWAINKernel.ImageInfoA.


function TmcmTWAINKernel.Palette8(    Palette       : pLogPalette;
                                  var PalType       : word;
                                      Msg           : TW_UINT16) : TW_UINT16;
begin
  Result := FTWAINThread.Palette8(Palette, PalType, Msg);
end; // TmcmTWAINKernel.Palette8.


function TmcmTWAINKernel.GetLogFileName : string;
begin
  if (FTWAINThread <> Nil)
  then FLogFileName := FTWAINThread.LogFileName;
  Result := FLogFileName;
end; // TmcmTWAINKernel.GetLogFileName.


procedure TmcmTWAINKernel.SetLogFileName(Value : string);
begin
  FLogFileName := Value;
  if Assigned(FTWAINThread)
  then FTWAINThread.LogFileName := FLogFileName;
end; // TmcmTWAINKernel.SetLogFileName.


function TmcmTWAINKernel.GetLogToFile : boolean;
begin
  if (FTWAINThread <> Nil)
  then FLogToFile := FTWAINThread.LogToFile;
  Result := FLogToFile;
end; // TmcmTWAINKernel.GetLogToFile.


procedure TmcmTWAINKernel.SetLogToFile(Value : boolean);
begin
  FLogToFile := Value;
  if (FTWAINThread <> Nil)
  then FTWAINThread.LogToFile := FLogToFile;
end; // TmcmTWAINKernel.SetLogToFile.


function TmcmTWAINKernel.GetMessageLevel : TTwnErrorLevel;
begin
  if (FTWAINThread <> Nil)
  then FMsgLevel := FTWAINThread.MessageLevel;
  Result := FMsgLevel;
end; // TmcmTWAINKernel.GetMessageLevel.


procedure TmcmTWAINKernel.SetMessageLevel(Value : TTwnErrorLevel);
begin
  FMsgLevel := Value;
  if (FTWAINThread <> Nil)
  then FTWAINThread.MessageLevel := FMsgLevel;
end; // TmcmTWAINKernel.SetMessageLevel.


function TmcmTWAINKernel.OpenDSM : TW_UINT16;
begin
  InitializeThreads;
  Result := FTWAINThread.OpenDSM;
end; // TmcmTWAINKernel.OpenDSM.


function TmcmTWAINKernel.CloseDSM : TW_UINT16;
begin
  Result := FTWAINThread.CloseDSM;
  TerminateThreads;
end; // TmcmTWAINKernel.CloseDSM.


function TmcmTWAINKernel.OpenDS : TW_UINT16;
begin
  Result := FTWAINThread.OpenDS;
  case Result of
  TWRC_SUCCESS : begin
                   FSourceVersion := FSourceID.ProtocolMajor * 10 +
                                     FSourceID.ProtocolMinor;
                   if Assigned(FOnDisableMenus)
                   then FOnDisableMenus(Self);
                 end;
  else           begin
                   // Trouble opening the Source
			             // Determine Condition Code
  end;
  end;
end; // TmcmTWAINKernel.OpenDS.


function TmcmTWAINKernel.CloseDS : TW_UINT16;
begin
  Result := FTWAINThread.CloseDS;
end; // TmcmTWAINKernel.CloseDS.


function TmcmTWAINKernel.ConfigureDS(var ShowUI        : boolean;
                                     var ModalUI       : boolean) : TW_UINT16;
begin
  Result := FTWAINThread.ConfigureDS(GetForegroundWindow, ShowUI, ModalUI);
end; // TmcmTWAINKernel.ConfigureDS.


function TmcmTWAINKernel.EnableDS(var ShowUI  : boolean;
                                  var ModalUI : boolean) : TW_UINT16;
begin
  Result := FTWAINThread.EnableDS(GetForegroundWindow, ShowUI, ModalUI);
end; // TmcmTWAINKernel.EnableDS.


function TmcmTWAINKernel.DisableDS : TW_UINT16;
begin
  Result := FTWAINThread.DisableDS;
end; // TmcmTWAINKernel.DisableDS.


procedure TmcmTWAINKernel.CloseConnection;
//------------------------------------------------------------------------------
// CloseConnection -- Disables the data source UI, closes the data source,
// and closes the DSM
//------------------------------------------------------------------------------
begin
  // Always try to unload DSM and DS.
  if IsDSEnabled
  then DisableDS;
  if IsDSOpen
  then CloseDS;
  if IsDSMOpen
  then CloseDSM;
  AcqFlag := TWFG_NONE;
end; // TmcmTWAINKernel.CloseConnection.


function TmcmTWAINKernel.SelectDS : TW_UINT16;
//------------------------------------------------------------------------------
// FUNCTION: SelectDS
//
// ARGS:    none
//
// RETURNS: twRC TWAIN status return code
//
// NOTES:   1). call the Source Manager to:
//              - have the SM put up a list of the available Sources
//              - get information about the user selected Source from
//                NewDSIdentity, filled by Source
//------------------------------------------------------------------------------
begin
  InitializeThreads;
  if Assigned(FOnDisableMenus)
  then FOnDisableMenus(Self);
  Result := FTWAINThread.SelectDS(GetForegroundWindow);
  TerminateThreads;
end; // TmcmTWAINKernel.SelectDS.


function TmcmTWAINKernel.GetSourceID : TW_IDENTITY;
begin
  Result := FSourceID;
end; // TmcmTWAINKernel.GetSourceID.


procedure TmcmTWAINKernel.SetSourceID(Value : TW_IDENTITY);
begin
  FSourceID := Value;
end; // TmcmTWAINKernel.SetSourceID.


function TmcmTWAINKernel.RemoveExt(Value : string) : string;
var TempName : string;
    TempExt  : string;
    i        : integer;
begin
  TempName := lowercase(Value);
  TempExt  := ExtractFileExt(TempName);
  i := Pos(TempExt, TempName) - 1;
  if (i > 0)
  then TempName := Copy(TempName, 1, i);
  if (TempName[Length(TempName)] = '.')
  then TempName := Copy(TempName, 1, Length(TempName) - 1);
  Result := TempName;
end; // TmcmTWAINKernel.RemoveExt.


function TmcmTWAINKernel.FileName2Format(FileName : string) : TTwnFileFmt;
var TempExt : string;
begin
  TempExt := lowercase(ExtractFileExt(FileName));
  if (CompareStr('.tif', TempExt) = 0)
  then Result := TWFF_TIFF
  else if (CompareStr('.pct', TempExt) = 0)
       then Result := TWFF_PICT
       else if (CompareStr('.xbm', TempExt) = 0)
            then Result := TWFF_XBM
            else if (CompareStr('.bmp', TempExt) = 0)
                 then Result := TWFF_BMP
                 else if (CompareStr('.jpg', TempExt) = 0)
                      then Result := TWFF_JFIF
                      else if (CompareStr('.fpx', TempExt) = 0)
                           then Result := TWFF_FPX
                           else if (CompareStr('.tif', TempExt) = 0)
                                then Result := TWFF_TIFFMULTI
                                else if (CompareStr('.png', TempExt) = 0)
                                     then Result := TWFF_PNG
                                     else if (CompareStr('.jpg', TempExt) = 0)
                                          then Result := TWFF_SPIFF
                                          else if (CompareStr('.jpg', TempExt) = 0)
                                               then Result := TWFF_EXIF
                                               else Result := TTwnFileFmt(TWFF_NONE);
end; // TmcmTWAINKernel.FileName2Format.


function TmcmTWAINKernel.FileFormat2Ext(twnFormat : TTwnFileFmt) : string;
begin
  case twnFormat of
  TWFF_TIFF      : Result := 'tif'; // Tag Image File Format.
  TWFF_PICT      : Result := 'pct'; // Macintosh PICT - Not supported on PC.
  TWFF_BMP       : Result := 'bmp'; // Windows Bitmap.
  TWFF_XBM       : Result := 'xbm'; // X-Windows Bitmap - Not supported on PC.
  TWFF_JFIF      : Result := 'jpg'; // JPEG.
  TWFF_FPX       : Result := 'fpx'; // Kodak FlashPix.
  TWFF_TIFFMULTI : Result := 'tif'; // Multiple TIFF image.
  TWFF_PNG       : Result := 'png'; // Portable Network Graphics
  TWFF_SPIFF     : Result := 'jpg'; // Unknown ?
  TWFF_EXIF      : Result := 'jpg'; // Unknown ?
  else Result := '';
  end;
end; // TmcmTWAINKernel.FileFormat2Ext.


function TmcmTWAINKernel.CreateFileName(AFileName, AFileExt : string) : string;
// CreateFileName - Generates a random file name for file transfer.
var i       : integer;
    TmpFile : File;
    TmpName : array[0..512] of char;
    TmpPath : string;
begin
  i := LastDelimiter('\:', AFileName);
  TmpPath := Copy(AFilename, 1, i);
  if Not(DirectoryExists(TmpPath))
  then begin
       if (GetTempPath(SizeOf(TmpName), TmpName) = 0)
       then TmpPath := '.\'
       else TmpPath := StrPas(TmpName);
       GetTempFileName(Pchar(TmpPath), 'TWN', word(0), TmpName);

       // Remove temporaryly created file.
       AssignFile(TmpFile, TmpName);
       Reset(TmpFile);
       CloseFile(TmpFile);
       Erase(TmpFile);
       AFileName := StrPas(TmpName);
  end;
  AFileName := RemoveExt(AFileName);
  AFileName := AFileName + '.' + AFileExt;
  Result := AFileName;
end; // TmcmTWAINKernel.CreateFileName.


function TmcmTWAINKernel.GetOnCloseSource : TNotifyEvent;
begin
  Result := FOnCloseSource;
end; // TmcmTWAINKernel.GetOnCloseSource.


procedure TmcmTWAINKernel.SetOnCloseSource(Value : TNotifyEvent);
begin
  FOnCloseSource := Value;
end; // TmcmTWAINKernel.SetOnCloseSource.


function TmcmTWAINKernel.GetOnDisableMenus : TNotifyEvent;
begin
  Result := FOnDisableMenus;
end; // TmcmTWAINKernel.GetOnDisableMenus.


procedure TmcmTWAINKernel.SetOnDisableMenus(Value : TNotifyEvent);
begin
  FOnDisableMenus := Value;
end; // TmcmTWAINKernel.SetOnDisableMenus.


function TmcmTWAINKernel.GetOnEnableMenus : TNotifyEvent;
begin
  Result := FOnEnableMenus;
end; // TmcmTWAINKernel.GetOnEnableMenus.


procedure TmcmTWAINKernel.SetOnEnableMenus(Value : TNotifyEvent);
begin
  FOnEnableMenus := Value;
end; // TmcmTWAINKernel.SetOnEnableMenus.


function TmcmTWAINKernel.GetOnFailure : TFailureEvent;
begin
  Result := FOnFailure;
end; // TmcmTWAINKernel.GetOnFailure.


procedure TmcmTWAINKernel.SetOnFailure(Value : TFailureEvent);
begin
  FOnFailure := Value;
end; // TmcmTWAINKernel.SetOnFailure.


function TmcmTWAINKernel.GetOnImageReady : TImageEvent;
begin
  Result := FOnImageReady;
end; // TmcmTWAINKernel.GetOnImageReady.


procedure TmcmTWAINKernel.SetOnImageReady(Value : TImageEvent);
begin
  FOnImageReady := Value;
end; // TmcmTWAINKernel.SetOnImageReady.


function TmcmTWAINKernel.GetOnMemXferBuf : TMemXferBufEvent;
begin
  Result := FOnMemXferBuf;
end; // TmcmTWAINKernel.GetOnMemXferBuf.


procedure TmcmTWAINKernel.SetOnMemXferBuf(Value : TMemXferBufEvent);
begin
  FOnMemXferBuf := Value;
end; // TmcmTWAINKernel.SetOnMemXferBuf.


function TmcmTWAINKernel.GetOnMemXferSize : TMemXferSizeEvent;
begin
  Result := FOnMemXferSize;
end; // TmcmTWAINKernel.GetOnMemXferSize.


procedure TmcmTWAINKernel.SetOnMemXferSize(Value : TMemXferSizeEvent);
begin
  FOnMemXferSize := Value;
end; // TmcmTWAINKernel.SetOnMemXferSize.


function TmcmTWAINKernel.GetOnXferNext : TXferNextEvent;
begin
  Result := FOnXferNext;
end; // TmcmTWAINKernel.GetOnXferNext.


procedure TmcmTWAINKernel.SetOnXferNext(Value : TXferNextEvent);
begin
  FOnXferNext := Value;
end; // TmcmTWAINKernel.SetOnXferNext.


function TmcmTWAINKernel.GetOnXferReady : TNotifyEvent;
begin
  Result := FOnXferReady;
end; // TmcmTWAINKernel.GetOnXferReady.


procedure TmcmTWAINKernel.SetOnXferReady(Value : TNotifyEvent);
begin
  FOnXferReady := Value;
end; // TmcmTWAINKernel.SetOnXferReady.


procedure TmcmTWAINKernel.DoOnCloseSource(Sender : TObject);
begin
  if Assigned(FOnCloseSource)
  then FOnCloseSource(Sender);
end; // TmcmTWAINKernel.DoOnCloseSource.


procedure TmcmTWAINKernel.DoOnEnableMenus(Sender : TObject);
begin
  if Assigned(FOnEnableMenus)
  then FOnEnableMenus(Sender);
end; // TmcmTWAINKernel.DoOnEnableMenus.


procedure TmcmTWAINKernel.DoOnFailure(Sender    : TObject;
                                      DG        : longint;
                                      DAT       : word;
                                      CAP       : word;
                                      MSG       : word;
                                      Error     : integer;
                                      Status    : integer);
begin
  if Assigned(FOnFailure)
  then FOnFailure(Sender, DG, DAT, CAP, MSG, Error, Status);
end; // TmcmTWAINKernel.DoOnFailure.


procedure TmcmTWAINKernel.DoOnImageReady(Sender    : TObject;
                                         pBmp      : pointer;
                                         pBmpInfo  : PBitmapInfo;
                                         hImage    : hBitmap;
                                         FilePath  : string);
begin
  if Assigned(FOnImageReady)
  then FOnImageReady(Sender, pBmp, pBmpInfo, hImage, FilePath);
end; // TmcmTWAINKernel.DoOnImageReady.


procedure TmcmTWAINKernel.DoOnMemXferBuf(    Sender       : TObject;
                                             pBmpInfo     : PBitmapInfo;
                                             BytesPerRow  : integer;
                                             Rows         : integer;
                                             DataSize     : integer;
                                         var pData        : pointer);
begin
  if Assigned(FOnMemXferBuf)
  then FOnMemXferBuf(Sender, pBmpInfo, BytesPerRow, Rows, DataSize, pData);
end; // TmcmTWAINKernel.DoOnMemXferBuf.


procedure TmcmTWAINKernel.DoOnMemXferSize(    Sender       : TObject;
                                              MinSize      : integer;
                                              MaxSize      : integer;
                                          var BufSize      : integer;
                                              pBmpInfo     : PBitmapInfo);
begin
  if Assigned(FOnMemXferSize)
  then FOnMemXferSize(Sender, MinSize, MaxSize, BufSize, pBmpInfo);
end; // TmcmTWAINKernel.DoOnMemXferSize.


procedure TmcmTWAINKernel.DoOnXferReady(Sender : TObject);
begin
  if Assigned(FOnXferReady)
  then FOnXferReady(Sender);
end; // TmcmTWAINKernel.DoOnXferReady.


procedure TmcmTWAINKernel.DoOnXferNext(    Sender    : TObject;
                                       var NumImages : Integer;
                                       var SkipNext  : boolean);
begin
  if Assigned(FOnXferNext)
  then FOnXferNext(Sender, NumImages, SkipNext);
end; // TmcmTWAINKernel.DoOnXferNext.


procedure TmcmTWAINKernel.LogMessage(LogStr : string);
begin
  if (FTWAINThread.MessageLevel >= ML_FULL)
  then FTWAINThread.LogMessage(LogStr);
end; { End TmcmTWAINKernel.LogMessage.                                         }


{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}
{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.


