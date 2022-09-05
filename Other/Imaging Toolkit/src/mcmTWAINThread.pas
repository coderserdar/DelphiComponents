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
// $Log:  21892: mcmTWAINThread.pas 
//
//    Rev 1.27    2014-03-28 17:52:56  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.26    2014-01-15 13:42:00  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.25    2013-12-04 23:16:14  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.24    2013-11-27 16:27:46  mcm
// Eliminated use of TThread.Resume & Suspend. As an upside, the TmcmTWAIN
// threads only runs while communicating with TWAIN and drivers.
//
//    Rev 1.23    26-08-2009 22:39:52  mcm    Version: DT 3.9
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.22    08-01-2009 21:09:22  mcm
// Added support for Delphi 2009
//
//    Rev 1.21    28-12-2008 18:42:10  mcm    Version: DT 3.8
// Delphi 2009
//
//    Rev 1.20    11-08-2007 10:59:14  mcm    Version: DT 3.7
// Added support for Delphi 2007
//
//    Rev 1.19    19-07-2006 17:01:22  mcm
// Corrected shutdown problem when Windows is closing while having a TWAN driver
// open/scanning.
//
//    Rev 1.18    22-05-2006 20:33:16  mcm
// Corrected a problem occuring when a TWAIN drivers sends a Close Request
// message. 
//
//    Rev 1.17    23-10-2005 18:10:20  mcm
// Corrected if statement (FNumImages <> 0) when testing for more image count
// for abortion when an error has occured.
//
//   Rev 1.16    11-06-2005 09:45:44  mcm    Version: DT 3.5
// Added a correction for Fujitsu scanners missing index in retreived palettes.
// Resolved a slow shut down when some HP Scanners sends a Close Request when
// they are not supposed to. 

//
//   Rev 1.15    27-05-2005 23:09:40  mcm

//
//   Rev 1.14    15-05-2005 21:50:12  mcm    Version: DT 3.5
// Fixed setting properties.

//
//   Rev 1.13    19-02-2005 00:26:54  mcm    Version: DT 3.4
// Modified where and when the TmcmTWAINQueue class is instanciated and
// destroyed. This solution solves a problem where the TmcmTWAIN control did not
// close down after being active for more than 15 min. without activation.
// Added support for data source configuration - without acquisition.

//
//   Rev 1.12    20-12-2004 23:03:38  mcm
// Modified SelectDS to disregard IsDSMOpen check.

//
//   Rev 1.11    11-11-2004 19:52:38  mcm
// Reverted back - to not using FreeOnTerminate.

//
//   Rev 1.10    16-09-2004 20:44:52  mcm
// Modified shut-down mechanisme to avoid dead-lock if application has been idle
// for approximately 15 min.

//
//   Rev 1.9    04-03-2004 20:55:34  mcm    Version: DT3.0
// Fix problem where DisableAfterAcquire is False - the data source didn't close
// when the user requested this. Ref WM_ONCLOSEREQUEST, removed if statement.

//
//   Rev 1.8    31-01-2004 13:56:34  mcm    Version: DT3.0
// Added missing call to OnXferReady.

//
//   Rev 1.7    10-12-2003 13:43:10  mcm    Version: DT3.0
// Internal.

//
//   Rev 1.6    05-12-2003 11:46:36  mcm
// Added SendCommandWait that sends twain commands to TmcmTWAINQueue and waits
// for the reply.
// Removed un-necessary code (WinRect).
// Replaced Application.MainForm.Handle with FParentWindow,
// (GetForegroundWindow).
//

//
//   Rev 1.5    28-11-2003 00:02:22  mcm    Version: DT3.0
// Fixed problem in Windows 98 with some scanners that required an extra message
// loop running while the data source is opened.

//
//   Rev 1.4    21-11-2003 22:00:22  mcm    Version: DT3.0
// Solved Delphi 6 problem by posting a message to release the event procedures.
// Corrected interpretation and offset to image data in 8 bit palette images
// where biClrUsed is less than 256.

//
//   Rev 1.3    12-11-2003 18:22:48  mcm    Version: DT3.0
// Resolved a dead-lock between TWAIN's DDE communication and application
// message loop in Windows 95 and 98.

//
//   Rev 1.2    12-11-2003 14:07:06  mcm    Version: DT3.0
// Added fixes for Windows 95, 98 and Me.

//
//   Rev 1.1    06-11-2003 09:08:30  mcm    Version: DT3.0
// Fixed a periodic termination problem.

//
//   Rev 1.0    04-11-2003 20:12:58  mcm    Version: DT3.0
// Initial version. Threaded TWAIN interface

unit mcmTWAINThread;

{$INCLUDE mcmDefines.pas}

interface

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

uses {$IFDEF GE_DXE2}
     System.Classes, WinApi.Windows, Vcl.Controls, WinApi.Messages,
     System.SysUtils, Vcl.Forms,
     {$IFDEF GE_DXE4}
     System.AnsiStrings,
     {$ENDIF}
     {$ELSE}
     Classes, Windows, Controls, Messages, SysUtils, Forms,
     {$ENDIF}
     twain,
     twainproc,
     mcmTWAINQueue;

type
  TmcmGetContainerData = function : integer of object;

  TmcmTWAINThread = class(TThread)
  private
    { Private declarations }
    FParentWindow   : HWnd;
    FOldWin         : boolean;
  {$IFDEF MCMDEMO}
    pData           : Pointer;
  {$ENDIF}
    FLock           : TRTLCriticalSection;
    FTWAINQueue     : TmcmTWAINQueue;
    FWaitMsec       : longint;
    FRetries        : integer;

    FApplicationID  : pTW_IDENTITY;      // Storage for App identification
    FSourceID       : pTW_IDENTITY;      // Storage for DS (Source) identification
    FAcqFlag        : TW_INT16;          // Flag for acquisition state
    FState          : word;              // TWAIN transition state [1..7].
                                         // Never set FState directly, always
                                         // set using property State to ensure
                                         // OnStateChange event is fired.
    FResult         : word;              // Result from last call to DSM/DS
    FStatus         : word;              // Status from DS on failure.
    FThreadID       : DWord;

    FFailureData    : TDSMFailureData;
  protected
    { Protected declarations }
    FDibSec         : HBitmap;           // The HBITMAP returned in OnImageReady
    FDib            : Pointer;           // Bitmap pointer returned in OnImageReady
    FDibInfo        : PBitmapInfo;       // Bitmap header point returned in OnImageReady
    FImageWidth     : integer;           // Image width
    FImageHeight    : integer;           // Image Height

    FNumImages      : smallint;          // Number of images left to transfer.

    FXferMech       : TTwnXferType;      // Transfer method [Native, Memory, file]
    FMemSetup       : pTW_SETUPMEMXFER;
    FMemXfer        : pTW_IMAGEMEMXFER;
    FSaveCount      : integer;
    FSkipNext       : boolean;           // Next transfer should be skipped.

    FHandleType     : TTwnHdlType;       // Dib handle to return to application.
    FFileName       : string;            // File name of stored image
    FPrefFormat     : TTwnFileFmt;       // File format to use for file transfer.
    FFileAsDIB      : boolean;           // Return bitmap file image in Dib handle
    FMemFlipBmp     : boolean;           // Flip RGB image if transfered via Memory.

    FOnCloseSource  : TNotifyEvent;      // Data source closed event
    FOnDeviceEvent  : TNotifyEvent;      // Device event from data source.
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
    FOnStateChanged : TStateChanged;
    FOnXferNext     : TXferNextEvent;    // Source reports more images.
    FOnXferReady    : TNotifyEvent;      // Source reports, image transfer ready.

    //
    FUnits          : integer;
    FGetUnits       : TmcmGetContainerData;
    FPixelFlavor    : integer;
    FGetPixelFlavor : TmcmGetContainerData;

    // Log members
    FMsgLevel       : TTwnErrorLevel;    // Filter which messages to save.
    FLogToFile      : boolean;           // On True Save messages to .\APPTWN.LOG
    FLogFileName    : string;            // The LOG file name
    
    procedure   Lock;
    procedure   Unlock;
    procedure   CreateQueue;
    procedure   DestroyQueue;
    function    SendCommandWait  (    Command       : longint;
                                      wParam        : WPARAM;
                                      lParam        : LPARAM;
                                      WaitTime      : longint) : TW_UINT16;
    function    AbortXfer        (    Msg           : TW_UINT16;
                                  var Count         : smallint) : TW_UINT16;
    procedure   CreateDIB        (    pDibInfo      : PBitmapInfo;
                                  var pDib          : pointer;
                                  var hDib          : HBitmap);
    function    ConvertRaw2Sec   (var hBmpRaw       : THandle;
                                  var pDib          : pointer;
                                  var pDibInfo      : PBitmapInfo;
                                  var hDibSec       : HBitmap) : boolean;
    function    LoadDibRaw       (    Filename      : string;
                                  var hBmpRaw       : THandle) : boolean;
    function    LoadDibSec       (    Filename      : string;
                                  var pDib          : pointer;
                                  var pDibInfo      : PBitmapInfo;
                                  var hDibSec       : HBitmap) : boolean;

    procedure   DoFileTransfer;
    procedure   DoMemTransfer;
    procedure   DoNativeTransfer;

    procedure   DoOnMemXferBuf   (    pDibInfo      : PBitmapInfo;
                                      pMemXfer      : pTW_IMAGEMEMXFER);
    procedure   DoOnMemXferSize  (    pDibInfo      : PBitmapInfo;
                                      pMemSetup     : pTW_SETUPMEMXFER);
    procedure   DoOnXferNext;


    procedure   FillDibInfoPal   (    NumColors     : integer;
                                      Flavor        : word;
                                      pPalette      : PLogPalette;
                                      pDibInfo      : PBitmapInfo);
    procedure   FlipBitmap       (    pDib          : PVectorB;
                                      pDibInfo      : PBitmapInfo;
                                      PixType       : TW_INT16);
    function    ImageFileXfer : TW_UINT16;
    function    ImageMemXfer     (    pMemxFer      : pTW_IMAGEMEMXFER;
                                      pMem          : pointer;
                                      Size          : longint;
                                      bitCount      : longint;
                                      Width         : longint) : TW_UINT16;
    function    ImageNativeXfer  (var hBmp          : TW_MEMREF) : TW_UINT16;

    function    PendingXfers     (    Msg           : TW_UINT16;
                                  var Count         : smallint) : TW_UINT16;
    function    SetupMemXfer     (    pMemSetup     : pTW_SETUPMEMXFER) : TW_UINT16;
    function    SetupFileXfer    (    Msg           : TW_UINT16;
                                  var FileName      : string;
                                  var Format        : TTwnFileFmt) : TW_UINT16;
    function    SetupFileXferA   (    Msg           : TW_UINT16;
                                  var FileName      : string;
                                  var Format        : TTwnFileFmt) : TW_UINT16;
    function    SetupFileXferB   (    Msg           : TW_UINT16;
                                  var FileName      : string;
                                  var Format        : TTwnFileFmt) : TW_UINT16;
    procedure   TransferImage;

    function    GetDSResult : word;
    function    GetDSStatus : word;
    function    GetFileFormat : TTwnFileFmt;
    function    GetFilename : string;
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
    function    GetOnDeviceEvent : TNotifyEvent;
    function    GetOnEnableMenus : TNotifyEvent;
    function    GetOnFailure : TFailureEvent;
    function    GetOnXferReady : TNotifyEvent;
    function    GetReturnHandle : boolean;
    function    GetSourceID : pTW_IDENTITY;
    function    GetState : word;
    function    GetSwapMemRGB : boolean;
    function    GetWinHandle : THandle;
    function    GetXferMech : TTwnXferType;
    procedure   PeekMessage;

    procedure   SetApplicationID(Value : pTW_IDENTITY);
    procedure   SetFileFormat(Value : TTwnFileFmt);
    procedure   SetFilename(Value : string);
    procedure   SetHandleType(Value : TTwnHdlType);
    procedure   SetLogFileName(Value : string);
    procedure   SetLogToFile(Value : boolean);
    procedure   SetMessageLevel(Value : TTwnErrorLevel);
    procedure   SetRetries(Value : integer);
    procedure   SetSourceID(Value : pTW_IDENTITY);
    procedure   SetState(Value : word);
    procedure   SetOnCloseSource(Value : TNotifyEvent);
    procedure   SetOnDeviceEvent(Value : TNotifyEvent);
    procedure   SetOnEnableMenus(Value : TNotifyEvent);
    procedure   SetOnFailure(Value : TFailureEvent);
    procedure   SetOnXferReady(Value : TNotifyEvent);
    procedure   SetReturnHandle(Value : boolean);
    procedure   SetSwapMemRGB(Value : boolean);
    procedure   SetWaitMsec(Value : longint);
    procedure   SetXferMech(Value : TTwnXferType);

    procedure   SyncClearEventProcs;
    procedure   SyncGetUnits;
    procedure   SyncGetPixelFlavor;
    procedure   SyncOnCloseRequest;
    procedure   SyncOnCloseSource;
    procedure   SyncOnDeviceEvent;
    procedure   SyncOnFailure;
    procedure   SyncOnImageReady;
    procedure   SyncOnImageFileReady;
    procedure   SyncOnMemXferBuf;
    procedure   SyncOnMemXferSize;
    procedure   SyncOnStateChanged;
    procedure   SyncOnXferNext;
    procedure   SyncOnXferReady;

    // Enable code when running in "DEBUG" mode.
    // Logs TWAIN communication between application and data source.
    procedure   LogDelete;
    procedure   LogTriplet       (    Dest          : integer;
                                      DG            : integer;
                                      DAT           : integer;
                                      Cap           : pTW_Capability;
                                      MSG           : integer;
                                      Return        : integer;
                                      Status        : integer;
                                      Level         : TTwnErrorLevel);
  public
    { Public declarations }
    constructor Create(CreateSuspended : Boolean);
    destructor  Destroy; override;

    procedure   ClearEventProcs;
    procedure   CloseConnection;
    function    CloseDS : TW_UINT16;
    function    CloseDSM : TW_UINT16;
    function    ConfigureDS      (    ParentWnd     : Hwnd;
                                  var ShowUI        : boolean;
                                  var ModalUI       : boolean) : TW_UINT16;
    function    DisableDS : TW_UINT16;
    function    DSMEntry         (    pOrigin       : pTW_IDENTITY;
                                      pDest         : pTW_IDENTITY;
                                      DG            : TW_UINT32;
                                      DAT           : TW_UINT16;
                                      MSG           : TW_UINT16;
                                      pData         : TW_MEMREF) : TW_UINT16;
    function    EnableDS         (    ParentWnd     : Hwnd;
                                  var ShowUI        : boolean;
                                  var ModalUI       : boolean) : TW_UINT16;
    procedure   Execute; override;
    function    GetStatus        (var Status        : TW_UINT16;
                                      pSourceID     : pTW_IDENTITY) : TW_UINT16;
    function    ImageInfo        (    pImageInfo    : pTW_IMAGEINFO) : TW_UINT16;
    function    ImageLayout      (    pImageLayout  : pTW_IMAGELAYOUT;
                                      Msg           : TW_UINT16) : TW_UINT16;
    procedure   LogMessage       (    LogStr        : string);
    function    OpenDS : TW_UINT16;
    function    OpenDSM : TW_UINT16;
    function    Palette8         (    Palette       : pLogPalette;
                                  var PalType       : word;
                                      Msg           : TW_UINT16) : TW_UINT16;
    procedure   QuitThread;
    function    SelectDS         (    ParentWnd     : Hwnd)  : TW_UINT16;

    property    ApplicationID : pTW_IDENTITY
      read      FApplicationID
      write     SetApplicationID;
    property    AcqFlag : TW_INT16
      read      FAcqFlag
      write     FAcqFlag;
    property    DIBHandleType : TTwnHdlType
      read      GetHandleType
      write     SetHandleType default THDT_DIBHANDLE;
    property    DSResult : word
      read      GetDSResult;
    property    DSStatus : word
      read      GetDSStatus;
    property    FileFormat : TTwnFileFmt
      read      GetFileFormat
      write     SetFileFormat default TWFF_BMP;
    property    Filename : string
      read      GetFilename
      write     SetFilename;
    property    GetUnits : TmcmGetContainerData
      read      FGetUnits
      write     FGetUnits;
    property    GetPixelFlavor : TmcmGetContainerData
      read      FGetPixelFlavor
      write     FGetPixelFlavor;
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
    property    LogFilename : string
      read      GetLogFileName
      write     SetLogFileName;
    property    LogToFile : boolean // Save error messages to .\APPTWN.LOG
      read      GetLogToFile
      write     SetLogToFile default False;
    property    MessageLevel : TTwnErrorLevel
      read      GetMessageLevel
      write     SetMessageLevel default ML_NONE;
    property    ParentWindow : HWnd
      read      FParentWindow
      write     FParentWindow;
    property    Retries : integer
      read      FRetries
      write     SetRetries;
    property    ReturnHandle : boolean
      read      GetReturnHandle
      write     SetReturnHandle default True;
    property    SourceID : pTW_IDENTITY
      read      GetSourceID
      write     SetSourceID;
    property    State : word
      read      GetState
      write     SetState;
    property    SwapMemRGB : boolean
      read      GetSwapMemRGB
      write     SetSwapMemRGB default True;
    property    WaitMsec : longint
      read      FWaitMsec
      write     SetWaitMsec;
    property    WinHandle : THandle
      read      GetWinHandle;
    property    XferMech : TTwnXferType
      read      GetXferMech
      write     SetXferMech default TWFX_NATIVE;

    // Events
    property    OnCloseSource  : TNotifyEvent
      read      GetOnCloseSource
      write     SetOnCloseSource;
    property    OnEnableMenus  : TNotifyEvent
      read      GetOnEnableMenus
      write     SetOnEnableMenus;
    property    OnDeviceEvent : TNotifyEvent
      read      GetOnDeviceEvent
      write     SetOnDeviceEvent;
    property    OnFailure : TFailureEvent
      read      GetOnFailure
      write     SetOnFailure;
    property    OnImageReady : TImageEvent
      read      FOnImageReady
      write     FOnImageReady;
    property    OnMemXferBuffer : TMemXferBufEvent
      read      FOnMemXferBuf
      write     FOnMemXferBuf;
    property    OnMemXferSize : TMemXferSizeEvent
      read      FOnMemXferSize
      write     FOnMemXferSize;
    property    OnXferNext : TXferNextEvent
      read      FOnXferNext
      write     FOnXferNext;
    property    OnXferReady : TNotifyEvent
      read      GetOnXferReady
      write     SetOnXferReady;
  end;

implementation

uses mcmTWAINFix;

//------------------------------------------------------------------------------
// TmcmTWAINThread.
//------------------------------------------------------------------------------

constructor TmcmTWAINThread.Create(CreateSuspended : Boolean);
begin
  InitializeCriticalSection(FLock);

  if (Win32Platform = VER_PLATFORM_WIN32_NT)
  then FOldWin := False
  else FOldWin := True;

  FMsgLevel    := ML_NONE;
  FLogToFile   := False;
  FLogFileName := '.\APPTWN.LOG';

  FTWAINQueue := Nil;

  FState       := 1;

  // Init/Clear event procedures.
  FOnCloseSource  := Nil;
  FOnDeviceEvent  := Nil;
  FOnDisableMenus := Nil;
  FOnEnableMenus  := Nil;
  FOnFailure      := Nil;
  FOnImageReady   := Nil;
  FOnMemXferBuf   := Nil;
  FOnMemXferSize  := Nil;
  FOnStateChanged := Nil;
  FOnXferNext     := Nil;
  FOnXferReady    := Nil;

  FXferMech    := TWFX_NATIVE;
  FHandleType  := THDT_DIBHANDLE;
  FPrefFormat  := TWFF_BMP;
  FFilename    := '';
  FMemFlipBmp  := True;
  FFileAsDIB   := True;

  FDibSec      := 0;
  FDib         := Nil;
  FDibInfo     := Nil;
  FImageWidth  := 0;
  FImageHeight := 0;

  FUnits          := TWUN_PIXELS;
  FGetUnits       := Nil;
  FPixelFlavor    := TWPF_CHOCOLATE;
  FGetPixelFlavor := Nil;

  {$IFDEF MCMDEMO}
    pData := @pCR_LF;
  {$ENDIF}

  Inherited Create(false);
  Priority := tpNormal;
  FreeOnTerminate := False;
  FThreadID := ThreadID;

  WaitMsec := DefaultWait;
  Retries  := 1000;
end; // TmcmTWAINThread.Create.


destructor TmcmTWAINThread.Destroy;
begin
  if (Self <> Nil)
  then begin
       DeleteCriticalSection(FLock);
       Inherited Destroy;
  end;
end; // TmcmTWAINThread.Destroy.


procedure TmcmTWAINThread.CreateQueue;
begin
  if Not(Assigned(FTWAINQueue))
  then begin
       FTWAINQueue := TmcmTWAINQueue.Create(False);
       FTWAINQueue.FreeOnTerminate := False;
       FTWAINQueue.ApplicationID := FApplicationID;
       FTWAINQueue.SourceID      := FSourceID;
       FTWAINQueue.ReceiveHandle := ThreadID;

       FTWAINQueue.WaitMsec      := FWaitMsec;
       FTWAINQueue.Retries       := FRetries;

       // Log to file set-up.
       FTWAINQueue.LogFilename   := FLogFileName;
       FTWAINQueue.MessageLevel  := FMsgLevel;
       FTWAINQueue.LogToFile     := FLogToFile;

       // Now it's safe to start the thread, all parameters are initialised.
       SetEvent(FTWAINQueue.FStartEvent);
       Sleep(0);

       // Wait for FTWAINQueue to fully initialise.
       while Not(PostThreadMessage(FTWAINQueue.ThreadID, WM_STARTUP, 0, 0))
       do Sleep(10);
       WaitForSingleObject(FTWAINQueue.FWaitEvent, 10000);
  end;
end; // TmcmTWAINThread.CreateQueue.


procedure TmcmTWAINThread.DestroyQueue;
var TempQueue : TmcmTWAINQueue;
begin
  if Assigned(FTWAINQueue)
  then begin
       TempQueue := FTWAINQueue;
       FTWAINQueue := Nil;
       TempQueue.QuitThread;
       TempQueue.WaitFor;
       TempQueue.Free;
  end;
end; // TmcmTWAINThread.DestroyQueue.


procedure TmcmTWAINThread.ClearEventProcs;
begin
  if Assigned(FTWAINQueue)
  then PostThreadMessage(ThreadID, WM_CLEAREVENTS, 0, 0);
end; // TmcmTWAINThread.ClearEventProcs.


procedure TmcmTWAINThread.SyncClearEventProcs;
begin
  // Init/Clear event procedures.
  FGetUnits       := Nil;
  FGetPixelFlavor := Nil;

  FOnFailure      := Nil;
  FOnImageReady   := Nil;
  FOnMemXferBuf   := Nil;
  FOnMemXferSize  := Nil;
  FOnStateChanged := Nil;
  FOnXferNext     := Nil;
  FOnXferReady    := Nil;
end; // TmcmTWAINThread.SyncClearEventProcs.


procedure TmcmTWAINThread.Lock;
begin
  EnterCriticalSection(FLock);
end; // TmcmTWAINThread.Lock.


procedure TmcmTWAINThread.Unlock;
begin
  LeaveCriticalSection(FLock);
end; // TmcmTWAINThread.Unlock.


procedure TmcmTWAINThread.SetWaitMsec(Value : longint);
begin
  FWaitMsec := Value;
  if Assigned(FTWAINQueue)
  then FTWAINQueue.WaitMsec := FWaitMsec;
end; // TmcmTWAINThread.SetWaitMsec.


procedure TmcmTWAINThread.SetRetries(Value : integer);
begin
  FRetries := Value;
  if Assigned(FTWAINQueue)
  then FTWAINQueue.Retries := FRetries;
end; // TmcmTWAINThread.SetRetries.


//------------------------------------------------------------------------------
// Threads main message loop.
//------------------------------------------------------------------------------

procedure TmcmTWAINThread.Execute;
var Msg : TMsg;
begin
  while Not(Terminated)
  do begin
     try
       {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.WaitMessage;
       if {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(Msg, 0, WM_QUIT, WM_QUIT, PM_REMOVE)
       then Terminate
       else PeekMessage;
     except
       On E:Exception
       do ;
     end;
  end;
  {$IFDEF MCMDEMO}
    if (byte(pData^) <> $0D)
    then FTWAINQueue := TmcmTWAINQueue(integer(@FTWAINQueue) + 1);
  {$ENDIF}

  Retries  := 1;
  WaitMsec := 50;
  if Assigned(FTWAINQueue)
  then begin
       SetEvent(FTWAINQueue.FWaitEvent);
       SetEvent(FTWAINQueue.FMsgEvent);

       // Cancel pending transfers.
       if FTWAINQueue.IsDSOpen
       then begin
            FNumImages := 0;
            PendingXfers(MSG_GET, FNumImages);
            if (FNumImages <> 0)
            then AbortXfer(MSG_RESET, FNumImages);
       end;
  end;

  // Close connection if open.
  CloseConnection;
  DestroyQueue;
end; // TmcmTWAINThread.Execute.


procedure TmcmTWAINThread.PeekMessage;
var Msg : TMsg;
begin
  while Not(Terminated) and {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(Msg, 0, 0, 0, PM_REMOVE)
  do begin
     if (Msg.Message = WM_QUIT)
     then begin
          Terminate;
          Break;
     end;
     try
       if (Msg.hwnd = 0)
       then begin
            if FOldWin
            then Sleep(10);
            case Msg.Message of
            WM_ONCLOSEREQUEST : begin
                                  CloseConnection;
                                  // Should event be sent to application.
                                  if Assigned(FOnCloseSource)
                                  then Synchronize(SyncOnCloseRequest);
                                end;
            WM_ONCLOSESOURCE  : begin
                                  if Assigned(FOnEnableMenus)
                                  then Synchronize(SyncOnCloseSource);
                                end;
            WM_ONDEVICEEVENT  : begin
                                  if Assigned(FOnDeviceEvent)
                                  then Synchronize(SyncOnDeviceEvent);
                                end;
            WM_ONFAILURE      : begin
                                  if Assigned(FOnFailure)
                                  then begin
                                       FFailureData := PDSMFailureData(Msg.lParam)^;
                                       Synchronize(SyncOnFailure);
                                  end;
                                end;
            WM_ONSTATECHANGED : begin
                                  State := Msg.lParam;
                                end;
            WM_ONXFERREADY    : begin
                                  State := 6;
                                  if (FAcqFlag > TWFG_NONE)
                                  then begin
                                       Synchronize(SyncOnXferReady);
                                       TransferImage;
                                  end;
                                end;
            WM_ONFATALERROR   : begin
                                  DestroyQueue;
                                  State := 1;

                                  // Re-enable applications menus.
                                  if Assigned(FOnEnableMenus)
                                  then Synchronize(SyncOnCloseSource);
                                end;
            WM_NEXTXFER       : begin
                                  // This message is sent internally, to request
                                  // the next image (PendingXfers <> 0).
                                  TransferImage;
                                end;
            WM_CLEAREVENTS    : SyncClearEventProcs;
            end; // End message case
       end;
     except
       On E:Exception
       do ;
     end;
  end;
end; // TmcmTWAINThread.PeekMessage.


procedure TmcmTWAINThread.QuitThread;
begin
  WaitMsec := 50;
  Retries := 1;
  if Assigned(FTWAINQueue)
  then begin
       SetEvent(FTWAINQueue.FWaitEvent);
       SetEvent(FTWAINQueue.FMsgEvent);
  end;
  PostThreadMessage(ThreadId, WM_QUIT, 0, 0);
end; // TmcmTWAINThread.QuitThread.


function TmcmTWAINThread.GetWinHandle : THandle;
begin
  if Assigned(FTWAINQueue)
  then Result := FTWAINQueue.WinHandle
  else Result := 0;
end; // TmcmTWAINThread.GetWinHandle.


function TmcmTWAINThread.GetStatus(var Status    : TW_UINT16;
                                       pSourceID : pTW_IDENTITY) : TW_UINT16;
// Parameters:
// Status           -
// Identity         - if Identity = FSourceID the status of the Data Source is
//                    requested (State 4 - 7).
//                    If Identity = Nil the status of the Data Source Manager is
//                    requested (State 2 - 7).
var twStatus : TW_STATUS;
begin
  Status := TWCC_BUMMER;
  // Determine details of failure from DSM.
  Result := DSMEntry(FApplicationID, pSourceID,
                     DG_CONTROL,
                     DAT_STATUS,
                     MSG_GET,
                     TW_MEMREF(@twStatus));
  if (Result = TWRC_SUCCESS)
  then Status := twStatus.ConditionCode;
end; // TmcmTWAINThread.GetStatus.


procedure TmcmTWAINThread.CloseConnection;
//------------------------------------------------------------------------------
// CloseConnection -- Disables the data source UI, closes the data source,
// and closes the DSM
//------------------------------------------------------------------------------
begin
  // Unload DS and DSM.
  if IsDSEnabled
  then DisableDS;
  if IsDSOpen
  then CloseDS;
  if IsDSMOpen
  then CloseDSM;
  FAcqFlag := TWFG_NONE;
end; // TmcmTWAINThread.CloseConnection.


//------------------------------------------------------------------------------
// Image transfer methods.
//------------------------------------------------------------------------------


procedure TmcmTWAINThread.CreateDIB(    pDibInfo : PBitmapInfo;
                                    var pDib     : pointer;
                                    var hDib     : HBitmap);
var ScreenDC : HDC;
begin
  ScreenDC  := {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.GetDC(0);
  hDib := 0;
  try
    hDib := CreateDIBSection(ScreenDC,
                             pDibInfo^,
                             DIB_RGB_COLORS,
                             pDib, 0, 0);
  finally
    {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.ReleaseDC(0, ScreenDC);
  end;
end; // TmcmTWAINThread.CreateDIB.


function TmcmTWAINThread.ConvertRaw2Sec(var hBmpRaw  : THandle;
                                        var pDib     : pointer;
                                        var pDibInfo : PBitmapInfo;
                                        var hDibSec  : HBitmap) : boolean;
// ConvertRaw2Sec converts the bitmap handle returned by a DS in Native mode.
//
var StartS         : TLongType;
    ToAddrS        : TLongType;
    BitsS          : TLongType;
    StartT         : TLongType;
    ToAddrT        : TLongType;
    BitsT          : TLongType;
    Count          : longint;

    pMem           : pointer;     // Pointer to returned image.
    pBmpInfoHeader : pBitmapInfoHeader;
    pBmpih         : pBitmapInfoHeader;
    pBmpch         : pBitmapCoreHeader;
    PalSize        : longint;     // Palette size.
    RGBPalOfs      : longint;     // RGB (True color 24 bit) palette offset.
    NoBits         : word;        // Number of bits per color.
    HeaderSize     : longint;     // Bitmap header size.
    LongWidth      : longint;     // Long image line width.
begin
  pDib     := Nil;
  pDibInfo := Nil;
  if (hBmpRaw <> 0)
  then begin
       // Convert the hBmpRaw handle to a DIB Section (HBitmap).
       pMem := DSMMemLock(hBmpRaw);
       if (pMem <> Nil)
       then begin
            pBmpInfoHeader := pMem;
            pBmpih := pBitmapInfoHeader(pBmpInfoHeader);
            pBmpch := pBitmapCoreHeader(pBmpInfoHeader);

            // Get number of bit per pixel.
            if (pBmpih^.biSize <> SizeOf(TBitmapCoreHeader))
            then NoBits := pBmpih^.biBitCount * pBmpih^.biPlanes
            else NoBits := pBmpch^.bcBitCount;

            // Determine Palette size (number of colours).
            RGBPalOfs := 0;
            case NoBits of
             1 : PalSize := 2;
             4 : PalSize := 16;
             8 : begin
                   PalSize := 256;
                   if (pBmpInfoHeader^.biClrUsed > 0)
                   then PalSize := pBmpInfoHeader^.biClrUsed;
                 end;
            24 : begin
                   PalSize := 0;
                   if (pBmpInfoHeader^.biClrUsed > 0)
                   then RGBPalOfs := 4 * pBmpInfoHeader^.biClrUsed;
                   pBmpInfoHeader^.biClrUsed := 0;
                 end;
            else PalSize := 0;
            end;

            // Create bitmap info header.
            HeaderSize := SizeOf(TBitmapInfoHeader) + PalSize * SizeOf(TRGBQuad);
            GetMem(pDibInfo, HeaderSize);

            if (pDibInfo <> Nil)
            then begin
                 // Copy bitmap info header.
                 CopyMemory(pDibInfo, pMem, HeaderSize);

                 // Fix-up bitmap info header.
                 with pDibInfo^.bmiHeader
                 do begin
                    FImageWidth  := biWidth;
                    FImageHeight := biHeight;
                    biClrUsed := 0;

                    // Ensure proper resolution information.
                    // If resolution is "zero" set it to 300 dpi.
                    if (biXPelsPerMeter <= 0)
                    then if (biYPelsPerMeter > 0)
                         then biXPelsPerMeter := biYPelsPerMeter
                         else biXPelsPerMeter := round(30000.0 / 2.54);
                    if (biYPelsPerMeter <= 0)
                    then if (biXPelsPerMeter > 0)
                         then biYPelsPerMeter := biXPelsPerMeter
                         else biYPelsPerMeter := round(30000.0 / 2.54);

                    // Calc. Long Width of Line.
                    LongWidth := (((abs(biWidth) * biBitCount) + 31) div 32) * 4;
                    if (biSizeImage = 0)
                    then biSizeImage := LongWidth * abs(biHeight);
                 end;

                 // Move bitmap to top of memory array.
                 StartS.Long := 0;
                 BitsS.Ptr   := pMem;
                 inc(StartS.Long, HeaderSize);
                 inc(StartS.Long, RGBPalOfs);
                 ToAddrS.Long := BitsS.Long + StartS.Long;

                 // Create a DIBitmap that the canvas can show.
                 CreateDIB(pDibInfo, pDib, hDibSec);
                 if (hDibSec <> 0)
                 then begin
                      // Copy bitmap data.
                      BitsT.Ptr   := pDib;
                      StartT.Long := 0;
                      if (BitsT.Ptr <> nil) and (BitsS.Ptr <> nil)
                      then begin
                           Count := pDibInfo^.bmiHeader.biSizeImage;
                           while (Count > 0)
                           do begin
                              // Move upto 32K at a time.
                              if (Count  > $8000)
                              then Count := $8000;
                              ToAddrS.Long := BitsS.Long + StartS.Long;
                              ToAddrT.Long := BitsT.Long + StartT.Long;
                              CopyMemory(ToAddrT.Ptr, ToAddrS.Ptr, Count);
                              StartS.Long := StartS.Long + Count;
                              StartT.Long := StartT.Long + Count;
                              Count       := longint(pDibInfo^.bmiHeader.biSizeImage) - StartT.Long;
                           end;
                      end;
                 end;
            end;

            // Unlock returned bitmap handle.
            DSMMemUnlock(hBmpRaw);
       end;
       DSMMemFree(hBmpRaw);
       hBmpRaw := 0;
  end;
  Result := (hDibSec <> 0);
end; // TmcmTWAINThread.ConvertRaw2Sec.


function TmcmTWAINThread.LoadDibRaw(    Filename : string;
                                    var hBmpRaw  : THandle) : boolean;
// LoadDibRaw reads the bitmap from disk into a THandle.
var AFile          : File;
    FileHeader     : TBitmapFileHeader;
    BytesToRead    : integer;
    Count          : integer;
    Bits           : TLongType;
    Start          : TLongType;
    ToAddr         : TLongType;
begin
  Result := False;
  {$IFOPT I-} {$DEFINE IOCHECKS_OFF} {$I+} {$ENDIF}
  try
    AssignFile(AFile, FileName);
    try
      System.Reset(AFile, 1);
      if (IOResult = 0)
      then begin
           // Read bitmap image from disk.
           BlockRead(AFile, FileHeader, SizeOf(TBitmapFileHeader));
           hBmpRaw := GlobalAlloc(GHND, FileHeader.bfSize);
           if (hBmpRaw <> 0)
           then begin
                Bits.Ptr   := GlobalLock(hBmpRaw);
                Start.Long := 0;
                Count := FileHeader.bfSize - SizeOf(TBitmapFileHeader);
                while (Count > 0)
                do begin
                   ToAddr.Long := Bits.Long + Start.Long;
                   BytesToRead := $8000;
                   if (BytesToRead > Count)
                   then BytesToRead := Count;
                   BlockRead(AFile, ToAddr.Ptr^, BytesToRead);
                   Count      := Count - BytesToRead;
                   Start.Long := Start.Long + BytesToRead;
                end;
                GlobalUnlock(hBmpRaw);
                Result := True;
           end;
      end;
    except
      // Result := TWRC_EXCEPTION;
    end;
  finally
    CloseFile(AFile);
  end;
  {$IFDEF IOCHECKS_OFF} {$I-} {$UNDEF IOCHECKS_OFF} {$ENDIF}
end; // TmcmTWAINThread.LoadDibRaw.


function TmcmTWAINThread.LoadDibSec(    Filename : string;
                                    var pDib     : pointer;
                                    var pDibInfo : PBitmapInfo;
                                    var hDibSec  : HBitmap) : boolean;
var AFile          : File;
    FileHeader     : TBitmapFileHeader;
    BytesToRead    : longint;
    Count          : longint;
    Bits           : TLongType;
    Start          : TLongType;
    ToAddr         : TLongType;
    HeaderSize     : integer;
    bitCount       : integer;
    NumColors      : integer;
    LongWidth      : integer;
begin
  Result   := False;
  pDib     := Nil;
  pDibInfo := Nil;
  {$IFOPT I-} {$DEFINE IOCHECKS_OFF} {$I+} {$ENDIF}
  try
    AssignFile(AFile, FileName);
    try
      Reset(AFile, 1);
      if (IOResult = 0)
      then begin
           // Read bitmap image from disk.
           BlockRead(AFile, FileHeader, SizeOf(TBitmapFileHeader));

           HeaderSize := SizeOf(TBitmapInfo);
           GetMem(pDibInfo, HeaderSize);

           if (pDibInfo <> Nil)
           then begin
                BlockRead(AFile, pDibInfo^.bmiHeader, SizeOf(TBitmapInfoHeader));

                with pDibInfo^.bmiHeader
                do bitCount := biPlanes * biBitCount;
                NumColors := 0;
                if (bitCount <= 8)
                then NumColors := 1 shl bitCount
                else if (pDibInfo^.bmiHeader.biClrUsed > 0)
                     then NumColors := pDibInfo^.bmiHeader.biClrUsed;

                if (NumColors > 0)
                then begin
                     HeaderSize := SizeOf(TBitmapInfo) + NumColors * SizeOf(TRGBQuad);
                     ReallocMem(pDibInfo, HeaderSize);

                     // Read palette.
                     BlockRead(AFile, pDibInfo^.bmiColors, NumColors * SizeOf(TRGBQuad));
                end;

                with pDibInfo^.bmiHeader
                do begin
                   LongWidth := (((longint(abs(biWidth) * bitCount) + 31) div 32) * 4);
                   biSizeImage := LongWidth * abs(biHeight);
                end;

                CreateDIB(pDibInfo, pDib, hDibSec);
                if (hDibSec <> 0)
                then begin
                     // Go to beginning of bitmap data in file.
                     Seek(AFile, FileHeader.bfOffBits);
                     Bits.Ptr   := pDib;
                     Start.Long := 0;
                     Count := pDibInfo^.bmiHeader.biSizeImage;
                     while (Count > 0)
                     do begin
                        ToAddr.Long := Bits.Long + Start.Long;
                        BytesToRead := $8000;
                        if (BytesToRead > Count)
                        then BytesToRead := Count;
                        BlockRead(AFile, ToAddr.Ptr^, BytesToRead);
                        Start.Long := Start.Long + BytesToRead;
                        Count := longint(pDibInfo^.bmiHeader.biSizeImage) - Start.Long;
                     end;
                     Result := True;
                end;
           end;
      end;
    except
      // Result := TWRC_EXCEPTION;
    end;
  finally
    CloseFile(AFile);
  end;
  {$IFDEF IOCHECKS_OFF} {$I-} {$UNDEF IOCHECKS_OFF} {$ENDIF}
end; // TmcmTWAINThread.LoadDibSec.


procedure TmcmTWAINThread.FillDibInfoPal(NumColors : integer;
                                         Flavor    : word;
                                         pPalette  : PLogPalette;
                                         pDibInfo  : PBitmapInfo);
var i      : integer;
    Step   : integer;
    iColor : integer;
begin
  if (0 < NumColors) and (NumColors <= 256)
  then begin
       if (pPalette <> Nil)
       then begin
            for i := 0 to (NumColors - 1)
            do begin
               pDibInfo^.bmiColors[i].rgbRed      := pPalette.palPalEntry[i].peRed;
               pDibInfo^.bmiColors[i].rgbGreen    := pPalette.palPalEntry[i].peGreen;
               pDibInfo^.bmiColors[i].rgbBlue     := pPalette.palPalEntry[i].peBlue;
               pDibInfo^.bmiColors[i].rgbReserved := 0;
            end;
       end
       else begin
            case NumColors of
            2  : Step := 256;
            16 : Step := 17;
            else Step := 1;
            end;
            for i := 0 to (NumColors - 1)
            do begin
               iColor := i * Step;
               if (iColor > 1) and (Step > 1)
               then dec(iColor);
               if (Flavor = TWPF_VANILLA)
               then iColor := 255 - iColor;
               pDibInfo^.bmiColors[i].rgbRed      := iColor;
               pDibInfo^.bmiColors[i].rgbGreen    := iColor;
               pDibInfo^.bmiColors[i].rgbBlue     := iColor;
               pDibInfo^.bmiColors[i].rgbReserved := 0;
            end;
       end;
  end;
end; // TmcmTWAINThread.FillDibInfoPal.


procedure TmcmTWAINThread.FlipBitmap(pDib     : PVectorB;
                                     pDibInfo : PBitmapInfo;
                                     PixType  : TW_INT16);
{------------------------------------------------------------------------------}
{ FlipBitmap - Takes a memory transfer buffer and changes it to a DIB format   }
{                                                                              }
{    i.e.    Memory Format                                                     }
{                         1  2  3  4  5                                        }
{                         6  7  8  9 10                                        }
{                        11 12 13 14 15                                        }
{                                                                              }
{            DIB bitmap Format                                                 }
{                        11 12 13 14 15                                        }
{                         6  7  8  9 10                                        }
{                         1  2  3  4  5                                        }
{                                                                              }
{    Memory RGBQuad order: RGB                                                 }
{    Windows DIB RGBQuad order: BGR                                            }
{                                                                              }
{ MCM 210996, This procedure was re-written in order to use less memory while  }
{ flipping the bitmap.                                                         }
{                                                                              }
{------------------------------------------------------------------------------}
var hBmpT       : THandle;
    pBmpT       : PVectorB;
    hBmpB       : THandle;
    pBmpB       : PVectorB;
    Height      : longint;
    BitCount    : word;
    Offset      : longint;
    pixels      : TW_UINT16;
    i, j, y     : TW_UINT32;
    SaveRed     : byte;

    LongWidth   : longint;
    StartT      : TLongType;
    ToAddrT     : TLongType;
    BitsT       : TLongType;

    StartB      : TLongType;
    ToAddrB     : TLongType;
    BitsB       : TLongType;
begin
  with pDibInfo^.bmiHeader
  do begin
     Height     := biHeight;
     BitCount   := biBitCount * biPlanes;
     LongWidth  := (((biWidth * bitCount) + 31) div 32) * 4;
  end;

  hBmpT := GlobalAlloc(GHND, LongWidth);
  hBmpB := GlobalAlloc(GHND, LongWidth);
  if (hBmpT <> 0) and
     (hBmpB <> 0)
  then begin
       pBmpT     := GlobalLock(hBmpT);
       pBmpB     := GlobalLock(hBmpB);

       BitsT.Ptr := pDib;
       BitsB.Ptr := pDib;

       // Calculate Offset to start of the bitmap data.
       Offset := 0;

       StartT.Long := Offset;
       StartB.Long := Offset + (LongWidth * (Height - 1));

       if (PixType = TWPT_RGB)
       then begin
            Pixels := TW_UINT16(pDibInfo^.bmiHeader.biWidth);

            // Flip image vertically.
            for y := 0 to ((Height - 1) div 2)
            do begin
               ToAddrT.Long := BitsT.Long + StartT.Long;
               ToAddrB.Long := BitsB.Long + StartB.Long;

               CopyMemory(pBmpT, ToAddrT.Ptr, LongWidth);
               CopyMemory(pBmpB, ToAddrB.Ptr, LongWidth);

               case bitCount of
               15 : begin // 5 Bits per channel, MSB not used.
                    end;
               16 : begin // 5-6-5 Bits per channel
                    end;
               24 : begin // 8 Bits per channel
                      for i := 0 to (Pixels - 1)
                      do begin
                         // Switch Red byte and Blue byte.
                         j := i * 3;
                         {$IFOPT R+} {$DEFINE RANGE_CHECKING} {$R-} {$ENDIF}
                           SaveRed     := pBmpT^[j];
                           pBmpT^[j]   := pBmpT^[j+2];
                           pBmpT^[j+2] := SaveRed;

                           SaveRed     := pBmpB^[j];
                           pBmpB^[j]   := pBmpB^[j+2];
                           pBmpB^[j+2] := SaveRed;
                         {$IFDEF RANGE_CHECKING} {$R+} {$UNDEF RANGE_CHECKING} {$ENDIF}
                      end;
                    end;
               30 : begin // 10 Bits per channel
                    end;
               36 : begin // 12 Bits per channel
                    end;
               48 : begin // 16 Bits per channel
                    end;
               end;

               CopyMemory(ToAddrB.Ptr, pBmpT, LongWidth);
               CopyMemory(ToAddrT.Ptr, pBmpB, LongWidth);
               StartT.Long := StartT.Long + LongWidth;
               StartB.Long := StartB.Long - LongWidth;
            end;
       end
       else begin
            // Flip image vertically.
            for i := 0 to ((Height - 1) div 2)
            do begin
               ToAddrT.Long := BitsT.Long + StartT.Long;
               ToAddrB.Long := BitsB.Long + StartB.Long;

               CopyMemory(pBmpT, ToAddrT.Ptr, LongWidth);
               CopyMemory(pBmpB, ToAddrB.Ptr, LongWidth);

               CopyMemory(ToAddrB.Ptr, pBmpT, LongWidth);
               CopyMemory(ToAddrT.Ptr, pBmpB, LongWidth);

               StartT.Long := StartT.Long + LongWidth;
               StartB.Long := StartB.Long - LongWidth;
            end;
       end;

       // Unlock memory.
       GlobalUnlock(hBmpT);
       GlobalUnlock(hBmpB);
  end
  else begin
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Could not allocate enough memory to flip image');
       {$ENDIF}
  end;

  // Free allocated memory.
  if (hBmpT <> 0)
  then GlobalFree(hBmpT);
  if (hBmpB <> 0)
  then GlobalFree(hBmpB);
end; // TmcmTWAINThread.FlipBitmap.


function TmcmTWAINThread.Palette8(    Palette : pLogPalette;
                                  var PalType : word;
                                      Msg     : TW_UINT16) : TW_UINT16;
//
// If palette is CMY then Red = Cyan, Blue = Magenta and Blue = Yellow.
//
// MSG_GET        - To assume that the returned palette is correct, this
//                  function should be called immediately after an image
//                  transfer is complete. (STATE 4-6)
// MSG_GETDEFAULT - Request the DS to use its default palette. (STATE 4-6)
// MSG_RESET      - Request the DS to roll back to its default palette. (STATE 4)
// MSG_SET        - Request the DS to adopt the specified palette. (STATE 4)
var i, j      : integer;
    Pal       : TW_PALETTE8;
    InitIndex : boolean;
begin
  try
    if (Msg = MSG_SET)
    then begin
         FillChar(Pal, 256 * SizeOf(TW_ELEMENT8), 0);
         Pal.NumColors := 256;
         Pal.PaletteType := PalType;
         for i := 0 to (Palette.palNumEntries - 1)
         do begin
            Pal.Colors[i].Index := i;
            Pal.Colors[i].Channel1 := Palette.palPalEntry[i].peRed;
            Pal.Colors[i].Channel2 := Palette.palPalEntry[i].peGreen;
            Pal.Colors[i].Channel3 := Palette.palPalEntry[i].peBlue;
         end;
    end;
    Result := DSMEntry(FApplicationID,
                       FSourceID,
                       DG_IMAGE,
                       DAT_PALETTE8,
                       Msg,
                       TW_MEMREF(@pal));
    if (Result = TWRC_SUCCESS)
    then begin
         if (Msg in [MSG_GET, MSG_GETDEFAULT, MSG_RESET])
         then begin
              // Check if data source filled the index entries.
              InitIndex := True;
              j := Pal.Colors[0].Index;
              for i := 0 to (Pal.NumColors - 1)
              do if (Pal.Colors[i].Index <> j)
                 then begin
                      InitIndex := False;
                      Break;
                 end;
              if InitIndex
              then begin
                   // Data source didn't filled the index entries.
                   for i := 0 to (Pal.NumColors - 1)
                   do Pal.Colors[i].Index := i;
              end;

              PalType := Pal.PaletteType;
              FillChar(Palette.palPalEntry[0], 256 * SizeOf(TPaletteEntry), 0);
              Palette.palNumEntries := 256;
              for i := 0 to (Pal.NumColors - 1)
              do begin
                 j := Pal.Colors[i].Index;
                 Palette.palPalEntry[j].peRed   := Pal.Colors[i].Channel1;
                 Palette.palPalEntry[j].peGreen := Pal.Colors[i].Channel2;
                 Palette.palPalEntry[j].peBlue  := Pal.Colors[i].Channel3;
              end;
         end;
    end;
  except
    Result := TWRC_EXCEPTION;
  end;
end; // TmcmTWAINThread.Palette8.


function TmcmTWAINThread.PendingXfers(    Msg   : TW_UINT16;
                                      var Count : smallint) : TW_UINT16;
// case of Msg:
// MSG_ENDXFER    - Sent after complete transfer to mark end of transfer. (State 6-7)
//                  Used to cancel or terminate a transfer. When issued in state
//                  6 the next pending transfer is canceled (pending transfer
//                  count is decremented). In state 7 this call terminates the
//                  current transfer, discarding any data.
// MSG_GET        - DS returns number of images that DS has for transfer. (State 4-7)
// MSG_RESET      - Reset DS image count to zero. (State 6)
// MSG_STOPFEEDER - Stops DS automatic feeder. (State 6)
//
var TWPendingXfers : TW_PENDINGXFERS;
begin
  // Msg = [MSG_ENDXFER, MSG_GET, MSG_RESET]
  TWPendingXfers.Count := 0;
  TWPendingXfers.Reserved := 0;
  Result := DSMEntry(FApplicationID,
                     FSourceID,
                     DG_CONTROL,
                     DAT_PENDINGXFERS,
                     Msg,
                     TW_MEMREF(@TWPendingXfers));
  if (Result = TWRC_SUCCESS)
  then begin
       Count := smallint(TWPendingXfers.Count);
       if (TWPendingXfers.Count = 0)
       then State := 5
       else State := 6;
  end;
end; // TmcmTWAINThread.PendingXfers.


function TmcmTWAINThread.AbortXfer(    Msg   : TW_UINT16;
                                   var Count : smallint) : TW_UINT16;
// AbortXfer aborts either the next image transfer or all images available in DS.
// case of Msg:
// MSG_ENDXFER    - Sent after complete transfer to mark end of transfer. (State 6-7)
// MSG_GET        - DS returns number of images that DS has for transfer. (State 4-7)
// MSG_RESET      - Reset DS image count to zero. (State 6)
// MSG_STOPFEEDER - Stops DS automatic feeder. (State 6)
begin
  // If the next image is to be skipped, but subsequent images are still
  // to be acquired, the PendingXfers will receive the Msg = MSG_ENDXFER,
  // otherwise PendingXfers will receive Msg = MSG_RESET.
  Result := PendingXfers(Msg, Count);
end; // TmcmTWAINThread.AbortXfer.


function TmcmTWAINThread.SetupMemXfer(pMemSetup : pTW_SETUPMEMXFER) : TW_UINT16;
// Parameters:
// pMemSetup        - Data container hold the returned information.
// Msg
//   MSG_GET        - Source returnes information about the preferred, minimum
//                    and maximum memory size to use for buffered transfer.
//                    An application must use a buffer size within the limits
//                    specified by minimum and maximum.
//                    If the data source doesn't care about the buffer size it
//                    may specify TWON_DONTCARE32 in either of these fields.
begin
  pMemSetup^.MinBufSize := 0;
  pMemSetup^.MaxBufSize := 0;
  pMemSetup^.Preferred  := 0;
  Result := DSMEntry(FApplicationID,
                     FSourceID,
                     DG_CONTROL,
                     DAT_SETUPMEMXFER,
                     MSG_GET,
                     TW_MEMREF(pMemSetup));
end; // TmcmTWAINThread.SetupMemXfer.


function TmcmTWAINThread.SetupFileXferA(    Msg      : TW_UINT16;
                                        var FileName : string;
                                        var Format   : TTwnFileFmt) : TW_UINT16;
// Parameters:
// Msg
//   MSG_GET        - Source returnes information about the file into which
//                    image or audio has or will be acquired to. State 4 - 6
//   MSG_GETDEFAULT - Source returnes default information about the file into
//                    which image or audio has or will be acquired to. State 4 - 6
//   MSG_RESET      - Source resets the file information to default and returnes
//                    file information. State 4
//   MSG_SET        - App sets the file information to be used for the next
//                    image or audio transfer. App is responsible for verifying
//                    that the file name is valid.
//                    App is also responsible for requesting a format supported
//                    by the source. State 4 - 6
// Filename         - A valid file name to store acquired image or audio data to.
// Format           - The file format to be used for storing image or audio data.
var TWSetupFile  : TW_SETUPFILEXFER;
begin
  Result := TWRC_FAILURE;
  if (Msg in [MSG_GET, MSG_GETDEFAULT, MSG_RESET, MSG_SET])
  then begin
       TWSetupFile.Format  := TW_UINT16(Format);
       TWSetupFile.VRefNum := TW_INT16(TWON_DONTCARE16);
       {$IFDEF GE_DXE4}
        System.AnsiStrings.StrPCopy(TWSetupFile.FileName, AnsiString(FileName));
       {$ELSE}
        StrPCopy(TWSetupFile.FileName, AnsiString(FileName));
       {$ENDIF}

       Result := DSMEntry(FApplicationID,
                          FSourceID,
                          DG_CONTROL,
                          DAT_SETUPFILEXFER,
                          Msg,
                          TW_MEMREF(@TWSetupFile));

       if (Result = TWRC_SUCCESS) and (Msg <> MSG_SET)
       then begin
            Format   := TTwnFileFmt(TWSetupFile.Format);
            FileName := string(TWSetupFile.FileName);
            // := TWSetupFile.VRefNum; Mac ONLY.
       end;
  end;
end; // TmcmTWAINThread.SetupFileXferA.


function TmcmTWAINThread.SetupFileXferB(    Msg      : TW_UINT16;
                                        var FileName : string;
                                        var Format   : TTwnFileFmt) : TW_UINT16;
// DAT_SETUPFILEXFER2 (SetupFileXferB) is an replacement or rather an enhancement
// of DAT_SETUPFILEXFER (SetupFileXferA) and should be used by Applications and
// Sources that comply with the TWAIN protocol version 1.9.
// Parameters:
// Msg
//   MSG_GET        - Source returnes information about the file into which
//                    image or audio has or will be acquired to. State 4 - 6
//   MSG_GETDEFAULT - Source returnes default information about the file into
//                    which image or audio has or will be acquired to. State 4 - 6
//   MSG_RESET      - Source resets the file information to default and returnes
//                    file information. State 4
//   MSG_SET        - App sets the file information to be used for the next
//                    image or audio transfer. App is responsible for verifying
//                    that the file name is valid.
//                    App is also responsible for requesting a format supported
//                    by the source. State 4 - 6
// Filename         - A valid file name to store acquired image or audio data to.
// Format           - The file format to be used for storing image or audio data.
var TWSetupFile2 : TW_SETUPFILEXFER2;
begin
  Result := TWRC_FAILURE;
  if (Msg in [MSG_GET, MSG_GETDEFAULT, MSG_RESET, MSG_SET])
  then begin
       TWSetupFile2.FileNameType := TWTY_STR1024;
       TWSetupFile2.Format       := TW_UINT16(Format);
       TWSetupFile2.VRefNum      := TW_INT16(TWON_DONTCARE16);
       TWSetupFile2.parID        := TW_UINT32(TWON_DONTCARE16);
       GetMem(TWSetupFile2.FileName, SizeOf(TW_STR1024));
       try
         StrPCopy(TWSetupFile2.FileName, FileName);

         Result := DSMEntry(FApplicationID,
                            FSourceID,
                            DG_CONTROL,
                            DAT_SETUPFILEXFER2,
                            Msg,
                            TW_MEMREF(@TWSetupFile2));
         if (Result = TWRC_SUCCESS) and (Msg <> MSG_SET)
         then begin
              Format := TTwnFileFmt(TWSetupFile2.Format);
              case TWSetupFile2.FileNameType of
              TWTY_UNI512  : begin
                               {$IFDEF UNICODE}
                                 FileName := StrPas(PWideChar(TWSetupFile2.FileName));
                               {$ELSE}
                                 FileName := StrPas(PAnsiChar(TWSetupFile2.FileName));
                               {$ENDIF}
                             end;
              TWTY_STR1024 : begin
                               {$IFDEF UNICODE}
                                 FileName := string(PAnsiChar(TWSetupFile2.FileName)^);
                               {$ELSE}
                                 FileName := PAnsiChar(TWSetupFile2.FileName)^;
                               {$ENDIF}
                             end;
              end;
              // := TWSetupFile.VRefNum;
         end;
       finally
         FreeMem(TWSetupFile2.FileName, SizeOf(TW_STR1024));
       end;
  end;
end; // TmcmTWAINThread.SetupFileXferB.


function TmcmTWAINThread.SetupFileXfer(    Msg      : TW_UINT16;
                                       var FileName : string;
                                       var Format   : TTwnFileFmt) : TW_UINT16;
// Parameters:
// Msg
//   MSG_GET        - Source returnes information about the file into which
//                    image or audio has or will be acquired to. State 4 - 6
//   MSG_GETDEFAULT - Source returnes default information about the file into
//                    which image or audio has or will be acquired to. State 4 - 6
//   MSG_RESET      - Source resets the file information to default and returnes
//                    file information. State 4
//   MSG_SET        - App sets the file information to be used for the next
//                    image or audio transfer. App is responsible for verifying
//                    that the file name is valid.
//                    App is also responsible for requesting a format supported
//                    by the source. State 4 - 6
// Filename         - A valid file name to store acquired image or audio data to.
// Format           - The file format to be used for storing image or audio data.
var AFile     : File;
//mcm    Container : TTwnContainer;
    bFiles2   : boolean;
begin
  Result := TWRC_SUCCESS;
  if (Msg = MSG_SET)
  then begin
       // Create the file and close it. If the file cannot be created her, it is
       // assumed that it cannot be created by the Source either.
       // NOTE: A file with the same name will be overriden here!
      {$IFOPT I-} {$DEFINE IOCHECKS_OFF} {$I+} {$ENDIF}
       try
         AssignFile(AFile, FileName);
         Rewrite(AFile, 1);
         if (IOResult <> 0)
         then Result := TWRC_FAILURE;
         CloseFile(AFile);

         // Delete the file just created.
         if FileExists(FileName)
         then DeleteFile(FileName);
       except
         Result := TWRC_EXCEPTION;
       end;
       {$IFDEF IOCHECKS_OFF} {$I-} {$UNDEF IOCHECKS_OFF} {$ENDIF}
  end;

  if (Result = TWRC_SUCCESS)
  then begin
       bFiles2 := False;
       (*
       if (FTWAINQueue.SourceVersion >= 19)
       then begin
{mxm
            // Source is using protocol 1.9
            Container := FConList.Items[ICAP_XFERMECH];
            if Assigned(Container)
            then if (Container.CurrentValue = TWFX_FILES2)
                 then bFiles2 := True;
}
       end;
       *)
       if bFiles2
       then Result := SetupFileXferB(Msg, FileName, Format)
       else Result := SetupFileXferA(Msg, FileName, Format);
  end;
end; // TmcmTWAINThread.SetupFileXfer.


function TmcmTWAINThread.ImageFileXfer : TW_UINT16;
// Initiate image transfer via file.
// Valid call only in state 6. If call is successful transit to state 7 and
// remain until DAT_PENDINGXFERS / MSG_ENDXFER is sent.
begin
  Result := DSMEntry(FApplicationID,
                     FSourceID,
                     DG_IMAGE,
                     DAT_IMAGEFILEXFER,
                     MSG_GET,
                     TW_MEMREF(Nil));
  if (Result = TWRC_SUCCESS) or (Result = TWRC_XFERDONE)
  then State := 7
  else State := 6;
end; // TmcmTWAINThread.ImageFileXfer.


function TmcmTWAINThread.ImageMemXfer(pMemxFer : pTW_IMAGEMEMXFER;
                                      pMem     : pointer;
                                      Size     : longint;
                                      bitCount : longint;
                                      Width    : longint) : TW_UINT16;
type TLongType = record
                 case Word of
       	         0  : (Ptr  : Pointer);
                 1  : (Long : Longint);
	         2  : (Lo   : Word;
	               Hi   : Word);
                 end;
var Start      : TLongType;
    ToAddr     : TLongType;
    Bits       : TLongType;
    Descramble : TLongType;
    Scramble   : TLongType;
    ToLine     : TLongType;
    FromLine   : TLongType;
    i          : longint;
    LongWidth  : longint;
begin
  Result := TWRC_FAILURE;
  if Assigned(pMem)
  then begin
       if (Size > 0)
       then begin
            LongWidth := (((Longint(Width * BitCount) + 31) div 32) * 4);
            Bits.Ptr   := pMem;
            Start.Long := 0;
            pMemXfer.Memory.Flags  := TWMF_APPOWNS or TWMF_POINTER;
            pMemXfer.Memory.Length := Size;
            repeat
              ToAddr.Long := Bits.Long + Start.Long;
              pMemXfer.Memory.TheMem := ToAddr.Ptr;
              Result := DSMEntry(FApplicationID,
                                 FSourceID,
                                 DG_IMAGE,
                                 DAT_IMAGEMEMXFER,
                                 MSG_GET,
                                 TW_MEMREF(pMemXfer));

              if (Result = TWRC_SUCCESS) or
                 (Result = TWRC_XFERDONE)
              then begin
                   // Ensure that lines are aligned to 32 bit boundary.
                   if (LongWidth <> pMemXfer.BytesPerRow)
                   then begin
                        if (pMemXfer.Rows > 1)
                        then begin
                             FromLine.Long := pMemXfer.BytesWritten - pMemXfer.BytesPerRow;
                             ToLine.Long   := (pMemXfer.Rows - 1) * LongWidth;
                             for i := 0 to (pMemXfer.Rows - 2)
                             do begin
                                Scramble.Long   := ToAddr.Long + FromLine.Long;
                                Descramble.Long := ToAddr.Long + ToLine.Long;
                                CopyMemory(Descramble.Ptr, Scramble.Ptr, pMemXfer.BytesPerRow);
                                ToLine.Long   := ToLine.Long - LongWidth;
                                FromLine.Long := FromLine.Long - pMemXfer.BytesPerRow;
                             end;
                        end;
                        Start.Long := Start.Long + pMemXfer.Rows * LongWidth;
                   end
                   else Start.Long := Start.Long + pMemXfer.BytesWritten;
              end;
            until (Result <> TWRC_SUCCESS)
       end;
  end;
end; // TmcmTWAINThread.ImageMemXfer.


function TmcmTWAINThread.ImageNativeXfer(var hBmp : TW_MEMREF) : TW_UINT16;
begin
  Result := DSMEntry(FApplicationID,
                     FSourceID,
                     DG_IMAGE,
                     DAT_IMAGENATIVEXFER,
                     MSG_GET,
                     TW_MEMREF(@hBmp));
end; // TmcmTWAINThread.ImageNativeXfer.


procedure TmcmTWAINThread.DoOnMemXferBuf(pDibInfo : PBitmapInfo;
                                         pMemXfer : pTW_IMAGEMEMXFER);
begin
  if (pDibInfo <> Nil) and (pMemXfer <> Nil)
  then begin
       FDibInfo := pDibInfo;
       FMemXfer := pMemXfer;
       if Assigned(FOnMemXferBuf)
       then Synchronize(SyncOnMemXferBuf);
  end;
end; // TmcmTWAINThread.DoOnMemXferBuf.


procedure TmcmTWAINThread.DoOnMemXferSize(pDibInfo  : PBitmapInfo;
                                          pMemSetup : pTW_SETUPMEMXFER);
begin
  if (pMemSetup <> Nil)
  then begin
       FDibInfo := pDibInfo;
       FMemSetup := pMemSetup;
       if Assigned(FOnMemXferSize)
       then Synchronize(SyncOnMemXferSize);
  end;
end; // TmcmTWAINThread.DoOnMemXferSize.


procedure TmcmTWAINThread.DoOnXferNext;
begin
  // Notify app that there are more images.
  if Assigned(FOnXferNext)
  then begin
       FSaveCount := FNumImages;
       FSkipNext := False;
       Synchronize(SyncOnXferNext);
       if IsDSOpen
       then begin
            if FSkipNext
            then AbortXfer(MSG_ENDXFER, FNumImages)
            else begin
                 if (FNumImages <> FSaveCount)
                 then begin
                      if (FSaveCount = 0)
                      then begin
                           AbortXfer(MSG_RESET, FNumImages);
                           if (FAcqFlag = TWFG_CLOSE)
                           then CloseConnection;
                      end;
                 end;
            end;
       end;
  end;
end; // TmcmTWAINThread.DoOnXferNext.


procedure TmcmTWAINThread.DoNativeTransfer;
var twRC     : TW_UINT16;
    hBmpRaw  : THandle;   // handle to bitmap from Source to ret to App
    hDibSec  : HBitmap;
    pDib     : Pointer;
    pDibInfo : PBitmapInfo;
    pMem     : pointer;     // Pointer to returned image.
    pBmpih   : pBitmapInfoHeader;
    pBmpch   : pBitmapCoreHeader;
begin
  // Do until there are no more pending transfers
  // explicitly initialize the our flags
  FNumImages := 0;
  repeat
     FDib     := Nil;
     FDibInfo := Nil;
     FDibSec  := 0;
     pDib     := Nil;
     pDibInfo := Nil;
     hDibSec  := 0;
     hBmpRaw  := 0;
     twRC := ImageNativeXfer(TW_MEMREF(hBmpRaw));
     case twRC of
     TWRC_XFERDONE : begin
                       // Session is in State 7
                       // Acknowledge the end of the transfer.
                       // and transition to state 6/5.
                       PendingXfers(MSG_ENDXFER, FNumImages);

                       // Close the DSM and DS.
                       if (FNumImages = 0) and (FAcqFlag = TWFG_CLOSE)
                       then CloseConnection;

                       if (hBmpRaw >= THANDLE(TWN_VALID_HANDLE))
                       then begin
                            // If DIBHandleType is THDT_DIBHANDLE or THDT_DIBSEC
                            // convert the returned raw bitmap handle (memory
                            // stream).
                            case FHandleType of
                            THDT_DIBHANDLE : begin
                                               ConvertRaw2Sec(hBmpRaw, pDib, pDibInfo, hDibSec);
                                               FDib     := pDib;
                                               FDibInfo := pDibInfo;
                                               FDibSec  := hDibSec;
                                               Synchronize(SyncOnImageReady);
                                             end;
                            THDT_DIBSEC    : begin
                                               ConvertRaw2Sec(hBmpRaw, pDib, pDibInfo, hDibSec);
                                               FDib     := pDib;
                                               FDibInfo := pDibInfo;
                                               FDibSec  := hDibSec;
                                               Synchronize(SyncOnImageReady);
                                             end;
                            // THDT_DIBRAW
                            else begin
                                 pMem := DSMMemLock(hBmpRaw);
                                 if (pMem <> Nil)
                                 then begin
                                      pBmpih := pBitmapInfoHeader(pMem);
                                      pBmpch := pBitmapCoreHeader(pMem);
                                      if (pBmpch^.bcSize = SizeOf(TBitmapCoreHeader))
                                      then begin
                                           FImageWidth  := pBmpch^.bcWidth;
                                           FImageHeight := pBmpch^.bcHeight;
                                      end
                                      else begin
                                           FImageWidth  := pBmpih^.biWidth;
                                           FImageHeight := pBmpih^.biHeight;
                                      end;
                                      DSMMemUnlock(hBmpRaw);
                                 end;
                                 FDib     := Nil;
                                 FDibInfo := Nil;
                                 FDibSec  := hBmpRaw;
                                 Synchronize(SyncOnImageReady);
                            end;
                            end;
                       end
                       else Synchronize(SyncOnImageReady);

                       if (pDibInfo <> Nil)
                       then FreeMem(pDibInfo);
                       pDibInfo := Nil;
                     end;
     // The user canceled or wants to rescan the image something wrong, abort
     // the transfer and delete the image pass a Nil ptr back to App.
     TWRC_CANCEL   : begin
                       // Session is in State 7
                       // Source (or User) Canceled Transfer
                       // transistion to state 6/5.
                       PendingXfers(MSG_ENDXFER, FNumImages);

                       // Close the DSM and DS.
                       if (FNumImages = 0) and (FAcqFlag = TWFG_CLOSE)
                       then CloseConnection;
                       Synchronize(SyncOnImageReady);
                     end;
     // TWRC_FAILURE,
     else            begin
                       // Session is in State 6.
                       // The transfer failed.
                       // Abort the image.
                       PendingXfers(MSG_ENDXFER, FNumImages);
                       if (FNumImages <> 0)
                       then AbortXfer(MSG_RESET, FNumImages);

                       // Close the DSM and DS.
                       if (FNumImages = 0)
                       then CloseConnection;
                       Synchronize(SyncOnImageReady);
                     end;
     end;

     // Another image is ready for scanning. Notify app.
     DoOnXferNext;
     if (FNumImages = 0)
     then begin
          // Close the DSM and DS.
          if IsDSOpen and (FAcqFlag = TWFG_CLOSE)
          then CloseConnection;
     end;
//  until (FNumImages = 0) or Not(IsDSOpen);
  until True;
  if (FNumImages <> 0) and IsDSOpen
  then PostThreadMessage(ThreadId, WM_NEXTXFER, 0, 0);
end; // TmcmTWAINThread.DoNativeTransfer.


procedure TmcmTWAINThread.DoFileTransfer;
var twRC     : TW_UINT16;
    hBmpRaw  : THandle;
    hDibSec  : HBitmap;
    pDib     : Pointer;
    pDibInfo : PBitmapInfo;
begin
  // Do until there are no more pending transfers.
  // explicitly initialize the our flags
  FNumImages := 0;
  repeat
     hBmpRaw  := 0;
     hDibSec  := 0;
     pDib     := Nil;
     pDibInfo := Nil;
     FDibSec  := 0;
     FDib     := Nil;
     FDibInfo := Nil;
     // Setup file transfer.
     twRC := SetupFileXfer(MSG_SET, FFileName, FPrefFormat);

     // Initiate File Transfer.
     if (twRC = TWRC_SUCCESS)
     then twRC := ImageFileXfer;

     case twRC of
     TWRC_XFERDONE : begin
                       // Successful Transfer.
                       // Read the bitmap header and verify the transfer is a
                       // valid bmp and create a handle to that bitmap
                       if FFileAsDIB and (FPrefFormat = TWFF_BMP)
                       then begin
                            // Read bitmap image from disk.
                            case FHandleType of
                            THDT_DIBRAW    : LoadDibRaw(FFilename, hBmpRaw);
                            THDT_DIBSEC,
                            THDT_DIBHANDLE : LoadDibSec(FFilename, pDib, pDibInfo, hDibSec);
                            end;
                            // Delete returned file.
                            if ((hBmpRaw <> 0) or (hDibSec <> 0)) and FileExists(FFileName)
                            then DeleteFile(FFileName);
                       end;

                       // Acknowledge the end of the transfer and transition to
                       // state 6/5.
                       PendingXfers(MSG_ENDXFER, FNumImages);

                       // Close the DSM and DS.
                       if (FNumImages = 0) and (FAcqFlag = TWFG_CLOSE)
                       then CloseConnection;

                       if (hBmpRaw <> 0) or (hDibSec <> 0)
                       then begin
                            case FHandleType of
                            THDT_DIBRAW    : begin
                                               FDibSec  := hBmpRaw;
                                               Synchronize(SyncOnImageReady);
                                             end;
                            THDT_DIBSEC    : begin
                                               FDib     := pDib;
                                               FDibInfo := pDibInfo;
                                               FDibSec  := hDibSec;
                                               Synchronize(SyncOnImageReady);
                                             end;
                            THDT_DIBHANDLE : begin
                                               FDibSec  := hDibSec;
                                               Synchronize(SyncOnImageReady);
                                             end;
                            end;
                       end
                       else Synchronize(SyncOnImageFileReady);

                       if (pDibInfo <> Nil)
                       then FreeMem(pDibInfo);
                     end;
     // The user canceled or wants to rescan the image something wrong, abort
     // the transfer and delete the image pass a Nil ptr back to App.
     TWRC_CANCEL   : begin
                       // The Source is in state 7 transistion to state 6/5.
                       PendingXfers(MSG_ENDXFER, FNumImages);
                       if (FNumImages <> 0)
                       then AbortXfer(MSG_RESET, FNumImages);

                       // Close the DSM and DS.
                       if (FNumImages = 0) and (FAcqFlag = TWFG_CLOSE)
                       then CloseConnection;
                       Synchronize(SyncOnImageReady);
                     end;
     // TWRC_FAILURE,
     else            begin
                       // The transfer failed.

                       // Abort the image.
                       PendingXfers(MSG_ENDXFER, FNumImages);
                       if (FNumImages <> 0)
                       then AbortXfer(MSG_RESET, FNumImages);

                       // Close the DSM and DS.
                       if (FNumImages = 0)
                       then CloseConnection;
                       Synchronize(SyncOnImageReady);
                     end;
     end;

     // Another image is ready for scanning. Notify app.
     DoOnXferNext;
     if (FNumImages = 0)
     then begin
          // Close the DSM and DS.
          if IsDSOpen and (FAcqFlag = TWFG_CLOSE)
          then CloseConnection;
     end;
//  until (FNumImages = 0);
  until True;
  if (FNumImages <> 0) and IsDSOpen
  then PostThreadMessage(ThreadId, WM_NEXTXFER, 0, 0);
end; // TmcmTWAINThread.DoFileTransfer.


procedure TmcmTWAINThread.DoMemTransfer;
var twRC          : TW_UINT16;
    hBmpRaw       : THandle;
    hDibSec       : HBitmap;
    pDib          : Pointer;
    pDibInfo      : PBitmapInfo;
    info          : TW_IMAGEINFO;
    ImageSize     : TW_UINT32;
    MemSetup      : TW_SETUPMEMXFER;
    MemXfer       : TW_IMAGEMEMXFER;
    pPalette      : PLogPalette;
    PalType       : word;
    PixelType     : word;            // A TWPT_xxx
    PixelFlavor   : TW_UINT16;
    ResUnits      : TW_UINT16;
    XRes, YRes    : real;
    NoColors      : longint;
    LongWidth     : longint;
    Start         : TLongType;
    ToAddr        : TLongType;
    Bits          : TLongType;
    pRGBBit       : PAnsiChar;
    RGBbyte       : AnsiChar;
    i, j          : integer;
begin
  // Do until there are no more pending transfers explicitly initialize the our
  // flags.
  FNumImages := 0;
  hBmpRaw  := 0;
  hDibSec  := 0;
  pDib     := Nil;
  pDibInfo := Nil;
  FDibSec  := 0;
  FDib     := Nil;
  FDibInfo := Nil;

  // Get Image Information from source.
  twRC := ImageInfo(@Info);
  if (twRC = TWRC_SUCCESS)
  then begin
       FImageWidth  := info.ImageWidth;
       FImageHeight := info.ImageLength;
       PixelType    := Info.PixelType;

       // Limited to 256 colors in Palette.
       LongWidth := (((Longint(Info.ImageWidth * Info.BitsPerPixel) + 31) div 32) * 4);
       ImageSize := LongWidth * Info.ImageLength;
       if (Info.BitsPerPixel <= 8)
       then NoColors := 1 shl Info.BitsPerPixel
       else NoColors := 0;

       // Make the size an integral of the preferred transfer size.
       twRC := SetupMemXfer(@MemSetup);

       // Create DIB Header based on info from the data source.
       if (twRC = TWRC_SUCCESS)
       then begin
            try
              GetMem(pDibInfo, SizeOf(TBitmapInfoHeader) + NoColors * SizeOf(TRGBQUAD));
            except
              On E:EOutOfMemory
              do begin
                 pDibInfo := Nil;
              end;
            end;
       end;

       if (pDibInfo = Nil)
       then begin // GlobalAlloc Failed.
            {$IFDEF TWNDEBUG}
              LogMessage('#M DoMemTransfer, Failed allocating memory for image.');
            {$ENDIF}
       end
       else begin // Lock the Memory.
            // Fill in the image information.
            FillChar(pDibInfo^, SizeOf(TBitmapInfoHeader), 0);
            pDibInfo^.bmiHeader.biSize        := SizeOf(TBITMAPINFOHEADER);
            pDibInfo^.bmiHeader.biWidth       := Info.ImageWidth;
            pDibInfo^.bmiHeader.biHeight      := Info.ImageLength;
            pDibInfo^.bmiHeader.biPlanes      := 1; // Only 1 is supported.
            pDibInfo^.bmiHeader.biBitCount    := Info.BitsPerPixel;

            // This application does not support compression.
            pDibInfo^.bmiHeader.biCompression := BI_RGB;
            pDibInfo^.bmiHeader.biSizeImage   := ImageSize;

            // Get Units and calculate PelsPerMeter.
            ResUnits := FGetUnits;
            if (DSResult <> TWRC_SUCCESS)
            then begin
                 pDibInfo^.bmiHeader.biXPelsPerMeter := 0;
                 pDibInfo^.bmiHeader.biYPelsPerMeter := 0;
            end
            else begin
                 XRes := FIX32ToFloat(Info.XResolution);
                 YRes := FIX32ToFloat(Info.YResolution);
                 try
                   case ResUnits of
                   TWUN_INCHES      : with pDibInfo^.bmiHeader
                                      do begin
                                         biXPelsPerMeter := Round(XRes * 100.0 / 2.54);
                                         biYPelsPerMeter := Round(YRes * 100.0 / 2.54);
                                      end;
                   TWUN_CENTIMETERS : with pDibInfo^.bmiHeader
                                      do begin
                                         biXPelsPerMeter := Round(XRes * 100);
                                         biYPelsPerMeter := Round(YRes * 100);
                                      end;
                   TWUN_PICAS       : with pDibInfo^.bmiHeader
                                      do begin
                                         biXPelsPerMeter := Round((XRes * 100.0 / 2.54) * 6.0);
                                         biYPelsPerMeter := Round((YRes * 100.0 / 2.54) * 6.0);
                                      end;
                   TWUN_POINTS      : with pDibInfo^.bmiHeader
                                      do begin
                                         biXPelsPerMeter := Round((XRes * 100.0 / 2.54) * 72.0);
                                         biYPelsPerMeter := Round((YRes * 100.0 / 2.54) * 72.0);
                                      end;
                   TWUN_TWIPS       : with pDibInfo^.bmiHeader
                                      do begin
                                         biXPelsPerMeter := Round((XRes * 100.0 / 2.54) * 1440.0);
                                         biYPelsPerMeter := Round((YRes * 100.0 / 2.54) * 1440.0);
                                      end;
                   // TWUN_PIXELS
                   else               with pDibInfo^.bmiHeader
                                      do begin
                                         biXPelsPerMeter := Round(300.0 * 100.0 / 2.54);
                                         biYPelsPerMeter := Round(300.0 * 100.0 / 2.54);
                                      end;
                   end;
                 except
                   On E:Exception
                   do begin
                      with pDibInfo^.bmiHeader
                      do begin
                         biXPelsPerMeter := Round(300.0 * 100.0 / 2.54);
                         biYPelsPerMeter := Round(300.0 * 100.0 / 2.54);
                      end;
                   end;
                 end;
            end; // End DibInfo

            // Get CAP_PIXELFLAVOR to determine colors filled
            // in the palette information.

            // (PixelFlavor = TWPF_CHOCOLATE) -> Black = 0.
            // (PixelFlavor = TWPF_VANILLA)   -> White = 0.
            PixelFlavor := FGetPixelFlavor;
            if (DSResult <> TWRC_SUCCESS)
            then PixelFlavor := TWPF_CHOCOLATE;

            // Setup Palette -- if the palettes are B/W or shades of gray,
            // the color table is built here.  If the image is 8 bit color,
            // a call to the source is made to retrieve the correct set of
            // colors.  If the call fails, the color table is constructed
            // with 256 shades of gray inorder to provide some image
            // reference

            GetMem(pPalette, SizeOf(TLogPalette) + 256 * SizeOf(TPaletteEntry));
            try
              case PixelType of
              TWPT_BW      : begin
                               pDibInfo^.bmiHeader.biClrUsed := 2;
                               pDibInfo^.bmiHeader.biClrImportant := 0;
                               FillDibInfoPal(NoColors, PixelFlavor, Nil, pDibInfo);
                             end;
              TWPT_GRAY    : begin
                               pDibInfo^.bmiHeader.biClrUsed := NoColors;
                               FillDibInfoPal(NoColors, PixelFlavor, Nil, pDibInfo);
                             end;
              TWPT_RGB     : pDibInfo^.bmiHeader.biClrUsed := 0;
              TWPT_PALETTE : begin // (TWPT_PALETTE) fill the palette information.
                               if (Palette8(pPalette, PalType, MSG_GET) = TWRC_SUCCESS)
                               then FillDibInfoPal(NoColors, PixelFlavor, pPalette, pDibInfo)
                               else FillDibInfoPal(NoColors, PixelFlavor, Nil, pDibInfo);
                             end;
              TWPT_CMY     : ;
              TWPT_CMYK    : ;
              TWPT_YUV     : ;
              TWPT_YUVK    : ;
              TWPT_CIEXYZ  : ;
              else           begin
                             end;
              end; // end case (PixelType).
            finally
              FreeMem(pPalette);
            end;

            // blocks    := ImageSize div MemSetup.Preferred;
            // ImageSize := (blocks + 1) * MemSetup.Preferred;

            // Negotiate transfer block size. Note, this condition must be
            // meet (MinBufSize <= Preferred <= MaxBufSize)
            // Preferred equals the transfer buffer size.
            DoOnMemXferSize(pDibInfo, @MemSetup);

            // Allocate memory to hold the incomming image.
            case FHandleType of
            THDT_DIBSEC,
            THDT_DIBHANDLE : CreateDIB(pDibInfo, pDib, hDibSec);
            THDT_MEMPTR    : begin
                               // Send Message containing Image header (Info + Palette).
                               // The receiver must assign pDib to allocated
                               // memory, large enough to receive MemSetup.Preferred
                               // bytes.
                               FillChar(MemXfer, SizeOf(TW_IMAGEMEMXFER), 0);
                               DoOnMemXferBuf(pDibInfo, @MemXfer);

                               // pDib is used to test for a valid pointer below.
                               pDib := MemXfer.Memory.TheMem;
                             end;
            // THDT_DIBRAW
            else             begin
                               hBmpRaw := GlobalAlloc(GHND, TW_UINT32(pDibInfo^.bmiHeader.biSizeImage) +
                                                      SizeOf(TBitmapInfoHeader) +
                                                      NoColors * SizeOf(TRGBQUAD));
                               if (hBmpRaw <> 0)
                               then pDib := PBitmapInfo(GlobalLock(hBmpRaw));
                             end;
            end; // End case.
       end;

       if (pDib <> Nil) and (pDibInfo <> Nil)
       then begin
            if (FHandleType = THDT_MEMPTR)
            then begin // Transfer the data - loop until done or canceled.
                 MemXfer.Memory.Flags  := TWMF_APPOWNS or TWMF_POINTER;
                 MemXfer.Memory.Length := MemSetup.Preferred;
                 repeat
                   if (MemXfer.Memory.TheMem = Nil)
                   then twRC := TWRC_CANCEL
                   else begin
                        twRC := DSMEntry(FApplicationID,
                                         FSourceID,
                                         DG_IMAGE,
                                         DAT_IMAGEMEMXFER,
                                         MSG_GET,
                                         TW_MEMREF(@MemXfer));
                        if (MemXfer.BytesWritten > 0)
                        then begin
                             if FMemFlipBmp and (PixelType = TWPT_RGB)
                             then begin
                                  pRGBBit := MemXfer.Memory.TheMem;
                                  i := 0;
                                  while (i < MemXfer.BytesWritten - MemXfer.BytesPerRow)
                                  do begin
                                     j := 0;
                                     while (j < MemXfer.BytesPerRow)
                                     do begin
                                        RGBbyte := pRGBBit[j];
                                        pRGBBit[j] := pRGBBit[j+2];
                                        pRGBBit[j+2] := RGBbyte;
                                        inc(j, 3);
                                     end;
                                     inc(i, MemXfer.BytesPerRow);
                                     pRGBBit := pRGBBit + MemXfer.BytesPerRow;
                                  end;
                             end;
                             DoOnMemXferBuf(pDibInfo, @MemXfer);
                        end;
                   end;
                 until (twRC <> TWRC_SUCCESS);
                 // Used as indication of end of transfer.
                 MemXfer.Memory.Flags := 0;
                 MemXfer.Columns      := 0;
                 MemXfer.Rows         := 0;
                 MemXfer.BytesWritten := 0;
                 DoOnMemXferBuf(pDibInfo, @MemXfer);
            end
            else begin // Transfer the data.
                 // Locate the start of the bitmap data
                 Bits.Ptr := pDib;
                 if (FHandleType = THDT_DIBRAW)
                 then begin
                      Start.Long := SizeOf(TBITMAPINFOHEADER);
                      Start.Long := Start.Long + longint(pDibInfo^.bmiHeader.biClrUsed) * SizeOf(TRGBQUAD);
                 end
                 else Start.Long := 0;
                 ToAddr.Long := Bits.Long + Start.Long;
                 twRC := ImageMemXfer(@MemXfer,
                                      ToAddr.Ptr,
                                      MemSetup.Preferred,
                                      longint(Info.BitsPerPixel),
                                      longint(Info.ImageWidth));
            end;

            case twRC of
            TWRC_XFERDONE : begin // Successful Transfer.
                              // Acknowledge the end of the transfer
                              // and transition to state 6/5
                              PendingXfers(MSG_ENDXFER, FNumImages);

                              // Close the DSM and DS.
                              if (FNumImages = 0) and (FAcqFlag = TWFG_CLOSE)
                              then CloseConnection;

                              if (FHandleType <> THDT_MEMPTR)
                              then begin
                                   // A memory transfer from a source
                                   // will be the reverse (RGB image only)
                                   // and needs to be flipped.
                                   if FMemFlipBmp
                                   then begin
                                        Bits.Ptr := pDib;
                                        if (FHandleType <> THDT_DIBSEC) and
                                           (FHandleType <> THDT_DIBHANDLE)
                                        then begin
                                             Start.Long := SizeOf(TBITMAPINFOHEADER);
                                             Start.Long := Start.Long + longint(pDibInfo^.bmiHeader.biClrUsed) * SizeOf(TRGBQUAD);
                                        end
                                        else Start.Long := 0;
                                        ToAddr.Long := Bits.Long + Start.Long;

                                        FlipBitmap(ToAddr.Ptr, pDibInfo, PixelType);
                                   end;

                                   case FHandleType of
                                   THDT_DIBRAW    : begin
                                                      CopyMemory(pDib,
                                                                 pDibInfo,
                                                                 SizeOf(TBitmapInfoHeader) +
                                                                 NoColors * SizeOf(TRGBQUAD));
                                                      GlobalUnlock(hBmpRaw);
                                                      FDibSec  := hBmpRaw;
                                                      Synchronize(SyncOnImageReady);
                                                    end;
                                   THDT_DIBHANDLE : begin
                                                      FDibSec  := hDibSec;
                                                      Synchronize(SyncOnImageReady);
                                                    end;
                                   THDT_DIBSEC    : begin
                                                      FDib     := pDib;
                                                      FDibInfo := pDibInfo;
                                                      FDibSec  := hDibSec;
                                                      Synchronize(SyncOnImageReady);
                                                    end;
                                   end;
                              end;
                            end;
            TWRC_CANCEL   : begin
                              if (hBmpRaw <> 0)
                              then begin
                                   GlobalUnlock(hBmpRaw);
                                   GlobalFree(hBmpRaw);
                              end;
                              if (hDibSec <> 0)
                              then DeleteObject(hDibSec);

                              // The Source is in state 7
                              // transistion to state 6/5
                              PendingXfers(MSG_ENDXFER, FNumImages);

                              // Close the DSM and DS.
                              if (FNumImages = 0) and (FAcqFlag = TWFG_CLOSE)
                              then CloseConnection;
                              Synchronize(SyncOnImageReady);
                            end;
            TWRC_FAILURE  : begin
                              if (hBmpRaw <> 0)
                              then begin
                                   GlobalUnlock(hBmpRaw);
                                   GlobalFree(hBmpRaw);
                              end;
                              if (hDibSec <> 0)
                              then DeleteObject(hDibSec);

                              // The transfer failed.
                              // Enhancement: Check Condition Code
                              // and attempt recovery.
                              PendingXfers(MSG_ENDXFER, FNumImages);
                              if (FNumImages <> 0)
                              then AbortXfer(MSG_RESET, FNumImages);

                              // close the DSM and DS.
                              if (FNumImages = 0)
                              then CloseConnection;
                              Synchronize(SyncOnImageReady);
                            end;
            else            begin
                              if (hBmpRaw <> 0)
                              then begin
                                   GlobalUnlock(hBmpRaw);
                                   GlobalFree(hBmpRaw);
                              end;
                              if (hDibSec <> 0)
                              then DeleteObject(hDibSec);

                              // Abort the image.
                              PendingXfers(MSG_ENDXFER, FNumImages);
                              if (FNumImages <> 0)
                              then AbortXfer(MSG_RESET, FNumImages);

                              // Close the DSM and DS.
                              if (FNumImages = 0)
                              then CloseConnection;
                              Synchronize(SyncOnImageReady);
                            end;
            end;
       end;
       if (pDibInfo <> Nil)
       then FreeMem(pDibInfo);
  end
  else FNumImages := 0;

  // Another image is ready for scanning. Notify app.
  DoOnXferNext;
  if (FNumImages = 0)
  then begin
       // Close the DSM and DS.
       if IsDSOpen and (AcqFlag = TWFG_CLOSE)
       then CloseConnection;
  end;

  if (FNumImages <> 0) and IsDSOpen
  then PostThreadMessage(ThreadId, WM_NEXTXFER, 0, 0);
end; // TmcmTWAINThread.DoMemTransfer.


procedure TmcmTWAINThread.TransferImage;
begin
  FWaitMsec := -1;
  {$IFDEF MCMDEMO}
  if (byte(pData^) = $0D)
  then
  {$ENDIF}
  case FXferMech of
  TWFX_Memory : DoMemTransfer;
  TWFX_Files,
  TWFX_Files2 : DoFileTransfer;
  else DoNativeTransfer;
  end;
  FWaitMsec := DefaultWait;
end; // TmcmTWAINThread.TransferImage.


//------------------------------------------------------------------------------
// Thread interface methods.
//
//------------------------------------------------------------------------------


function TmcmTWAINThread.SendCommandWait(Command  : longint;
                                         wParam   : WPARAM;
                                         lParam   : LPARAM;
                                         WaitTime : longint) : TW_UINT16;
var i         : integer;
    CountDown : integer;
    WinMsg    : TMsg;
begin
  // The first message is sent from this point.
  // To ensure that the threads message queue is ready we'll loop until
  // PostThreadMessage returns true.
  if Not(Assigned(FTWAINQueue))
  then begin
       Result := TWRC_FAILURE;
       Exit;
  end;

  ResetEvent(FTWAINQueue.FWaitEvent);
  FTWAINQueue.MsgID := Command;
  FTWAINQueue.MsgReceived := 0;

  i := 0;
  while Not(PostThreadMessage(FTWAINQueue.ThreadId, Command, wParam, lParam)) and (i < 256)
  do begin
     inc(i);
     Sleep(0);
  end;

  // Solves dead-lock between TWAIN's DDE and applications message loop.
  if (WaitTime < 0)
  then begin
       while (WaitForSingleObject(FTWAINQueue.FWaitEvent, 20) <> WAIT_OBJECT_0)
       do begin
          if {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(WinMsg, 0, WM_QUIT, WM_QUIT, PM_REMOVE)
          then begin
               Result := TWRC_FAILURE;
               Terminate;
               Exit;
          end
          else begin
               if {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(WinMsg, FParentWindow, WM_PAINT, WM_PAINT, PM_REMOVE)
               then begin
                    TranslateMessage(WinMsg);
                    DispatchMessage(WinMsg);
               end;
          end;
          Sleep(0);
       end;
  end
  else begin
       CountDown := WaitTime;
       while (WaitForSingleObject(FTWAINQueue.FWaitEvent, 20) <> WAIT_OBJECT_0) and (CountDown > 0)
       do begin
          if {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(WinMsg, 0, WM_QUIT, WM_QUIT, PM_REMOVE)
          then begin
               Result := TWRC_FAILURE;
               Terminate;
               Exit;
          end
          else if {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(WinMsg, FParentWindow, WM_PAINT, WM_PAINT, PM_REMOVE)
               then begin
                    TranslateMessage(WinMsg);
                    DispatchMessage(WinMsg);
               end;
          dec(CountDown, 20);
       end;
  end;
  if (WaitForSingleObject(FTWAINQueue.FWaitEvent, 0) <> WAIT_OBJECT_0)
  then Result := TWRC_TIMEOUT
  else Result := FTWAINQueue.FWaitResult;
  SetEvent(FTWAINQueue.FMsgEvent);
end; // TmcmTWAINThread.SendCommandWait.


function TmcmTWAINThread.DSMEntry(pOrigin : pTW_IDENTITY;
                                  pDest   : pTW_IDENTITY;
                                  DG      : TW_UINT32;
                                  DAT     : TW_UINT16;
                                  MSG     : TW_UINT16;
                                  pData   : TW_MEMREF) : TW_UINT16;
var DSMData : TDSMEntryData;
    i, Wait : integer;
begin
  if Not(Assigned(FTWAINQueue))
  then begin
       Result := TWRC_FAILURE;
       Exit;
  end;
  Lock;
  Wait := WaitMsec;
  DSMData.pOrigin := pOrigin;
  if (DAT = DAT_IDENTITY) or (DAT = DAT_PARENT)
  then begin
       DSMData.pDest := Nil;
       if (MSG in [MSG_GETDEFAULT, MSG_GETFIRST, MSG_GETNEXT])
       then WaitMsec := -1;
  end
  else DSMData.pDest := pDest;
  DSMData.DG      := DG;
  DSMData.DAT     := DAT;
  DSMData.MSG     := MSG;
  DSMData.pData   := pData;

  // Try send the message. If DS window is being moved PostThreadMessage will
  // not succeed. Therefore we'll retry FRetries times before giving up.
  i := 0;
  ResetEvent(FTWAINQueue.FWaitEvent);
  FTWAINQueue.MsgID := DAT + MSG;
  FTWAINQueue.MsgReceived := 0;
  while (FTWAINQueue.MsgID <> FTWAINQueue.MsgReceived) and (i < FRetries)
  do begin
     inc(i);
     PostThreadMessage(FTWAINQueue.ThreadId, WM_DSMENTRY, 0, dword(@DSMData));
     WaitForSingleObject(FTWAINQueue.FWaitEvent, 20);
  end;

  if (WaitForSingleObject(FTWAINQueue.FWaitEvent, WaitMsec) <> WAIT_OBJECT_0)
  then Result := TWRC_TIMEOUT
  else Result := FTWAINQueue.FWaitResult;
  SetEvent(FTWAINQueue.FMsgEvent);

  if (WaitMsec <> Wait)
  then WaitMsec := Wait;
  Unlock;
end; // TmcmTWAINThread.DSMEntry.


function TmcmTWAINThread.OpenDSM : TW_UINT16;
begin
  CreateQueue;

  WaitMsec := DefaultWait;
  Result := SendCommandWait(WM_OPENDSM, 0, 0, WaitMsec);
end; // TmcmTWAINThread.OpenDSM.


function TmcmTWAINThread.CloseDSM : TW_UINT16;
begin
  if IsDSMOpen
  then begin
       Lock;
       if IsDSMOpen
       then begin
            WaitMsec := DefaultWait;
            Result := SendCommandWait(WM_CLOSEDSM, 0, 0, WaitMsec);
       end
       else Result := TWRC_FAILURE;
       
       DestroyQueue;
       Unlock;
  end
  else Result := TWRC_FAILURE;
end; // TmcmTWAINThread.CloseDSM.


function TmcmTWAINThread.OpenDS : TW_UINT16;
begin
  if Assigned(FTWAINQueue)
  then begin
       while Not(PostThreadMessage(FTWAINQueue.ThreadID, WM_MCMWINRECT, FParentWindow, 0))
       do Sleep(10);
       Result := SendCommandWait(WM_OPENDS, 0, 0, -1);
  end
  else Result := TWRC_FAILURE;
end; // TmcmTWAINThread.OpenDS.


function TmcmTWAINThread.CloseDS : TW_UINT16;
begin
  if IsDSOpen
  then begin
       Lock;
       if IsDSOpen
       then Result := SendCommandWait(WM_CLOSEDS, 0, 0, WaitMsec)
       else Result := TWRC_FAILURE;
       Unlock;
  end
  else Result := TWRC_FAILURE;
end; // TmcmTWAINThread.CloseDS.


function TmcmTWAINThread.ConfigureDS(    ParentWnd     : Hwnd;
                                     var ShowUI        : boolean;
                                     var ModalUI       : boolean) : TW_UINT16;
begin
  if Assigned(FTWAINQueue)
  then begin
       if ShowUI
       then begin
            while Not(PostThreadMessage(FTWAINQueue.ThreadID, WM_MCMWINRECT, ParentWnd, 0))
            do Sleep(50);
       end;
       Result := SendCommandWait(WM_CONFIGUREDS, integer(ShowUI), integer(ModalUI), -1);
       ShowUI  := FTWAINQueue.FShowUI;
       ModalUI := FTWAINQueue.FModalUI;
  end
  else Result := TWRC_FAILURE;
end; // TmcmTWAINThread.ConfigureDS.


function TmcmTWAINThread.EnableDS(    ParentWnd : Hwnd;
                                  var ShowUI    : boolean;
                                  var ModalUI   : boolean) : TW_UINT16;
begin
  if Assigned(FTWAINQueue)
  then begin
       if ShowUI
       then begin
            while Not(PostThreadMessage(FTWAINQueue.ThreadID, WM_MCMWINRECT, ParentWnd, 0))
            do Sleep(50);
       end;
       Result := SendCommandWait(WM_ENABLEDS, integer(ShowUI), integer(ModalUI), -1);
       ShowUI  := FTWAINQueue.FShowUI;
       ModalUI := FTWAINQueue.FModalUI;
  end
  else Result := TWRC_FAILURE;
end; // TmcmTWAINThread.EnableDS.


function TmcmTWAINThread.DisableDS : TW_UINT16;
begin
  if IsDSEnabled
  then begin
       Lock;
       if IsDSEnabled
       then Result := SendCommandWait(WM_DISABLEDS, 0, 0, WaitMsec)
       else Result := TWRC_FAILURE;
       Unlock;
  end
  else Result := TWRC_FAILURE;
end; // TmcmTWAINThread.DisableDS.


function TmcmTWAINThread.SelectDS(ParentWnd : Hwnd) : TW_UINT16;
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
var twRC          : TW_UINT16;
    NewDSIdentity : TW_IDENTITY;
    DSMwasOpen    : boolean;
    QueueWasOpen  : boolean;
begin
  Lock;
  QueueWasOpen := Assigned(FTWAINQueue);
  if Not(QueueWasOpen)
  then CreateQueue;
  if IsDSOpen or Not(Assigned(FTWAINQueue))
  then begin
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M A Source is already open.  Close Source before Selecting a New Source');
       {$ENDIF}
       twRC := TWRC_FAILURE;
  end
  else begin
       if IsDSMOpen
       then begin
            DSMwasOpen := True;
            twRC := TWRC_SUCCESS;
       end
       else begin
            DSMwasOpen := False;
            twRC := FTWAINQueue.OpenDSM(ParentWnd);
       end;
       if (twRC = TWRC_SUCCESS)
       then begin
            // I will settle for the system default.  Shouldn't I get a highlight
            // on system default without this call?
            twRC := FTWAINQueue.DSMEntry(FApplicationID,
                                         Nil,
                                         DG_CONTROL,
                                         DAT_IDENTITY,
                                         MSG_GETDEFAULT,
                                         TW_MEMREF(@NewDSIdentity));

            // This call performs one important function:
            // - should cause SM to put up dialog box of available Source's
            // - tells the SM which application, FApplicationID.id, is requesting,
            //   REQUIRED
            // - returns the SM assigned NewDSIdentity.id field, you check if
            //   changed (needed to talk to a particular Data Source)
            // - be sure to test return code, failure indicates SM did not close !!
            if (twRC = TWRC_SUCCESS) or (twRC = TWRC_ENDOFLIST) or True
            then twRC := FTWAINQueue.DSMEntry(FApplicationID,
                                              Nil,
                                              DG_CONTROL,
                                              DAT_IDENTITY,
                                              MSG_USERSELECT,
                                              TW_MEMREF(@NewDSIdentity));

            // Check if the user changed the Source and react as apporpriate.
            // - TWRC_SUCCESS, log in new Source
            // - TWRC_CANCEL,  keep the current Source
            // - default,      check the codes in a status message, display result
            case twRC of
            TWRC_SUCCESS : begin
                             FSourceID^ := NewDSIdentity;
                           end;
            TWRC_CANCEL  : begin
                           end;
            else           begin
                           end;
            end;

            if Not(DSMwasOpen)
            then FTWAINQueue.CloseDSM(ParentWnd);
       end;
  end;
  PostThreadMessage(FThreadID, WM_ONCLOSESOURCE, 0, 0);

  if Not(QueueWasOpen)
  then DestroyQueue;

  // Let the caller know what happened.
  Result := twRC;
  Unlock;
end; // TmcmTWAINThread.SelectDS.


function TmcmTWAINThread.GetIsDSMOpen : bool;
begin
  if Assigned(FTWAINQueue)
  then Result := FTWAINQueue.IsDSMOpen
  else Result := False;
end; // TmcmTWAINThread.GetIsDSMOpen.


function TmcmTWAINThread.GetIsDSOpen : bool;
begin
  if Assigned(FTWAINQueue)
  then Result := FTWAINQueue.IsDSOpen
  else Result := False;
end; // TmcmTWAINThread.GetIsDSOpen.


function TmcmTWAINThread.GetIsDSEnabled : bool;
begin
  if Assigned(FTWAINQueue)
  then Result := FTWAINQueue.IsDSEnabled
  else Result := False;
end; // TmcmTWAINThread.GetIsDSEnabled.


function TmcmTWAINThread.GetState : word;
begin
  Result := FState;
end; // TmcmTWAINThread.GetState.


procedure TmcmTWAINThread.SetState(Value : word);
begin
  FState := Value;
  if Assigned(FOnStateChanged)
  then Synchronize(SyncOnStateChanged);
end; // TmcmTWAINThread.SetState.


function TmcmTWAINThread.GetDSResult : word;
begin
  Result := FResult;
end; // TmcmTWAINThread.GetDSResult.


function TmcmTWAINThread.GetDSStatus : word;
begin
  Result := FStatus;
end; // TmcmTWAINThread.GetDSStatus.


function TmcmTWAINThread.GetSourceID : pTW_IDENTITY;
begin
  Result := FSourceID;
end; // TmcmTWAINThread.GetSourceID.


procedure TmcmTWAINThread.SetSourceID(Value : pTW_IDENTITY);
begin
  FSourceID := Value;
  if Assigned(FTWAINQueue)
  then FTWAINQueue.SourceID := FSourceID;
end; // TmcmTWAINThread.SetSourceID.


procedure TmcmTWAINThread.SetApplicationID(Value : pTW_IDENTITY);
begin
  FApplicationID := Value;
  if Assigned(FTWAINQueue)
  then FTWAINQueue.ApplicationID := FApplicationID;
end; // TmcmTWAINThread.SetApplicationID.


function TmcmTWAINThread.GetHandleType : TTwnHdlType;
begin
  Result := FHandleType;
end; // TmcmTWAINThread.GetHandleType.


procedure TmcmTWAINThread.SetHandleType(Value : TTwnHdlType);
begin
  if (Value = THDT_MEMPTR)
  then if (FXferMech <> TWFX_MEMORY)
       then FXferMech := TWFX_MEMORY;
  FHandleType := Value;
end; // TmcmTWAINThread.SetHandleType.


function TmcmTWAINThread.GetReturnHandle : boolean;
begin
  Result := FFileAsDIB;
end; // TmcmTWAINThread.GetReturnHandle.


procedure TmcmTWAINThread.SetReturnHandle(Value : boolean);
begin
  if (FPrefFormat = TWFF_BMP)
  then FFileAsDIB := Value
  else FFileAsDIB := False;
end; // TmcmTWAINThread.SetReturnHandle.


function TmcmTWAINThread.GetFileFormat : TTwnFileFmt;
begin
  Result := FPrefFormat;
end; // TmcmTWAINThread.GetFileFormat.


procedure TmcmTWAINThread.SetFileFormat(Value : TTwnFileFmt);
begin
  if Not(Value in [TWFF_PICT, TWFF_XBM])
  then FPrefFormat := Value;
end; // TmcmTWAINThread.SetFileFormat.


function TmcmTWAINThread.GetFilename : string;
begin
  Result := FFilename;
end; // TmcmTWAINThread.GetFilename.


procedure TmcmTWAINThread.SetFilename(Value : string);
begin
  FFileName := Value;
end; // TmcmTWAINThread.SetFilename.


function TmcmTWAINThread.GetImageHeight : integer;
begin
  Result := FImageHeight;
end; // TmcmTWAINThread.GetImageHeight.


function TmcmTWAINThread.GetImageWidth : integer;
begin
  Result := FImageWidth;
end; // TmcmTWAINThread.GetImageWidth.


function TmcmTWAINThread.GetSwapMemRGB : boolean;
begin
  Result := FMemFlipBmp;
end; // TmcmTWAINThread.GetSwapMemRGB.


procedure TmcmTWAINThread.SetSwapMemRGB(Value : boolean);
begin
  FMemFlipBmp := Value;
end; // TmcmTWAINThread.SetSwapMemRGB.


function TmcmTWAINThread.GetXferMech : TTwnXferType;
begin
  Result := FXferMech;
end; // TmcmTWAINThread.GetXferMech.


procedure TmcmTWAINThread.SetXferMech(Value : TTwnXferType);
begin
  FXferMech := Value;
  if (FXferMech <> TWFX_MEMORY)
  then if (FHandleType = THDT_MEMPTR)
       then DIBHandleType := THDT_DIBHANDLE;
  // EXPAND TO RUNTIME TOO !

  // WHAT ABOUT GETXferMech ? RETURN ACTUAL XferMech AT RUNTIME !

  // HOW TO DO !?!
end; // TmcmTWAINThread.SetXferMech.


function TmcmTWAINThread.ImageInfo(pImageInfo : pTW_IMAGEINFO) : TW_UINT16;
begin
  // Check ImageInfo information.
  Result := DSMEntry(FApplicationID,
                     FSourceID,
                     DG_IMAGE,
                     DAT_IMAGEINFO,
                     MSG_GET,
                     TW_MEMREF(pImageInfo));
end; // TmcmTWAINThread.ImageInfo.


function TmcmTWAINThread.ImageLayout(pImageLayout : pTW_IMAGELAYOUT;
                                     Msg          : TW_UINT16) : TW_UINT16;
// MSG_GET
// MSG_GETDEFAULT
// MSG_RESET
// MSG_SET
begin
  // Check ImageLayout information.
  Result := DSMEntry(FApplicationID,
                     FSourceID,
                     DG_IMAGE,
                     DAT_IMAGELAYOUT,
                     Msg,
                     TW_MEMREF(pImageLayout));
end; // TmcmTWAINThread.ImageLayout.


//------------------------------------------------------------------------------
// Synchronised Event procedures
//------------------------------------------------------------------------------

function TmcmTWAINThread.GetOnCloseSource : TNotifyEvent;
begin
  Result := FOnCloseSource;
end; // TmcmTWAINThread.GetOnCloseSource.


procedure TmcmTWAINThread.SetOnCloseSource(Value : TNotifyEvent);
begin
  FOnCloseSource := Value;
end; // TmcmTWAINThread.SetOnCloseSource.


function TmcmTWAINThread.GetOnDeviceEvent : TNotifyEvent;
begin
  Result := FOnDeviceEvent;
end; // TmcmTWAINThread.GetOnDeviceEvent.


procedure TmcmTWAINThread.SetOnDeviceEvent(Value : TNotifyEvent);
begin
  FOnDeviceEvent := Value;
end; // TmcmTWAINThread.SetOnDeviceEvent.


function TmcmTWAINThread.GetOnEnableMenus : TNotifyEvent;
begin
  Result := FOnEnableMenus;
end; // TmcmTWAINThread.GetOnEnableMenus.


procedure TmcmTWAINThread.SetOnEnableMenus(Value : TNotifyEvent);
begin
  FOnEnableMenus := Value;
end; // TmcmTWAINThread.SetOnEnableMenus.


function TmcmTWAINThread.GetOnFailure : TFailureEvent;
begin
  Result := FOnFailure;
end; // TmcmTWAINThread.GetOnFailure.


procedure TmcmTWAINThread.SetOnFailure(Value : TFailureEvent);
begin
  FOnFailure := Value;
end; // TmcmTWAINThread.SetOnFailure.


function TmcmTWAINThread.GetOnXferReady : TNotifyEvent;
begin
  Result := FOnXferReady;
end; // TmcmTWAINThread.GetOnXferReady.


procedure TmcmTWAINThread.SetOnXferReady(Value : TNotifyEvent);
begin
  FOnXferReady := Value;
end; // TmcmTWAINThread.SetOnXferReady.


procedure TmcmTWAINThread.SyncGetUnits;
begin
  if Assigned(FGetUnits)
  then FUnits := FGetUnits
  else FUnits := TWUN_PIXELS;
end; // TmcmTWAINThread.SyncGetUnits.


procedure TmcmTWAINThread.SyncGetPixelFlavor;
begin
  if Assigned(FGetPixelFlavor)
  then FPixelFlavor := FGetPixelFlavor
  else FPixelFlavor := TWPF_CHOCOLATE;
end; // TmcmTWAINThread.SyncGetPixelFlavor.


procedure TmcmTWAINThread.SyncOnCloseRequest;
begin
  if Assigned(FOnCloseSource)
  then FOnCloseSource(Self);
end; // TmcmTWAINThread.SyncOnCloseRequest.


procedure TmcmTWAINThread.SyncOnCloseSource;
begin
  if Assigned(FOnEnableMenus)
  then FOnEnableMenus(Self);
end; // TmcmTWAINThread.SyncOnCloseSource.


procedure TmcmTWAINThread.SyncOnDeviceEvent;
begin
  if Assigned(FOnDeviceEvent)
  then FOnDeviceEvent(Self);
end; // TmcmTWAINThread.SyncOnDeviceEvent.


procedure TmcmTWAINThread.SyncOnFailure;
begin
  if Assigned(FOnFailure)
  then with FFailureData
       do FOnFailure(Self, DG, DAT, CAP, MSG, Result, Status);
end; // TmcmTWAINThread.SyncOnFailure.


procedure TmcmTWAINThread.SyncOnImageReady;
begin
  if Assigned(FOnImageReady)
  then FOnImageReady(Self, FDib, FDibInfo, FDibSec, '')
  else begin
       // Need to clean up.
       if (FDibSec <> 0)
       then GlobalFree(FDibSec);
       FDibSec := 0;
       FNumImages := 0;
  end;
end; // TmcmTWAINThread.SyncOnImageReady.


procedure TmcmTWAINThread.SyncOnImageFileReady;
begin
  if Assigned(FOnImageReady)
  then FOnImageReady(Self, Nil, Nil, 0, FFileName)
  else begin
       FNumImages := 0;
  end;
end; // TmcmTWAINThread.SyncOnImageFileReady.


procedure TmcmTWAINThread.SyncOnMemXferBuf;
begin
  if Assigned(FOnMemXferBuf)
  then with FMemXfer^
       do FOnMemXferBuf(Self, FDibInfo, BytesPerRow, Rows, BytesWritten, Memory.TheMem);
end; // TmcmTWAINThread.SyncOnMemXferBuf.


procedure TmcmTWAINThread.SyncOnMemXferSize;
var PrefSize : integer;
begin
  if Assigned(FOnMemXferSize)
  then with FMemSetup^
       do begin
          PrefSize := Preferred;
          FOnMemXferSize(Self, MinBufSize, MaxBufSize, PrefSize, FDibInfo);
          // Validate preferred buffer size.
          if Not((MinBufSize = 0) or (MaxBufSize = 0))
          then begin
               // Validate preferred buffer size.
               if (PrefSize < MinBufSize)
               then PrefSize := MinBufSize;
               if (PrefSize > MaxBufSize) and (MaxBufSize <> -1)
               then PrefSize := MaxBufSize;
          end;
          Preferred := PrefSize;
       end;
end; // TmcmTWAINThread.SyncOnMemXferSize.


procedure TmcmTWAINThread.SyncOnStateChanged;
begin
  if Assigned(FOnStateChanged)
  then FOnStateChanged(Self, FState);
end; // TmcmTWAINThread.SyncOnStateChanged.


procedure TmcmTWAINThread.SyncOnXferNext;
begin
  if Assigned(FOnXferNext)
  then FOnXferNext(Self, FSaveCount, FSkipNext);
end; // TmcmTWAINThread.SyncOnXferNext.


procedure TmcmTWAINThread.SyncOnXferReady;
begin
  if Assigned(FOnXferReady)
  then FOnXferReady(Self);
end; // TmcmTWAINThread.SyncOnXferReady.


//------------------------------------------------------------------------------
// LOG event methods.
//------------------------------------------------------------------------------

function TmcmTWAINThread.GetLogFileName : string;
begin
  if Assigned(FTWAINQueue)
  then FLogFileName := FTWAINQueue.LogFileName;
  Result := FLogFileName;
end; // TmcmTWAINThread.GetLogFileName.


procedure TmcmTWAINThread.SetLogFileName(Value : string);
begin
  FLogFileName := Value;
  if Assigned(FTWAINQueue)
  then if (FTWAINQueue.LogFileName <> FLogFileName)
       then FTWAINQueue.LogFileName := FLogFileName;
end; // TmcmTWAINThread.SetLogFileName.


function TmcmTWAINThread.GetLogToFile : boolean;
begin
  if Assigned(FTWAINQueue)
  then FLogToFile := FTWAINQueue.LogToFile;
  Result := FLogToFile;
end; // TmcmTWAINThread.GetLogToFile.


procedure TmcmTWAINThread.SetLogToFile(Value : boolean);
begin
  FLogToFile := Value;
  if Assigned(FTWAINQueue)
  then FTWAINQueue.LogToFile := FLogToFile;
end; // TmcmTWAINThread.SetLogToFile.


function TmcmTWAINThread.GetMessageLevel : TTwnErrorLevel;
begin
  if Assigned(FTWAINQueue)
  then FMsgLevel := FTWAINQueue.MessageLevel;
  Result := FMsgLevel;
end; // TmcmTWAINThread.GetMessageLevel.


procedure TmcmTWAINThread.SetMessageLevel(Value : TTwnErrorLevel);
begin
  FMsgLevel := Value;
  if Assigned(FTWAINQueue)
  then FTWAINQueue.MessageLevel := FMsgLevel;
end; // TmcmTWAINThread.SetMessageLevel.


procedure TmcmTWAINThread.LogMessage(LogStr : string);
begin
  {$IFDEF TWNDEBUG}
  try
    if Assigned(FTWAINQueue)
    then FTWAINQueue.LogMessage(LogStr);
  except
  end;
  {$ENDIF}
end; // TmcmTWAINThread.LogMessage.



procedure TmcmTWAINThread.LogDelete;
begin
{$IFDEF TWNDEBUG}
  try
    if Assigned(FTWAINQueue)
    then FTWAINQueue.LogDelete;
  except
  end;
  {$ENDIF}
end; // TmcmTWAINThread.LogDelete.


procedure TmcmTWAINThread.LogTriplet(Dest     : integer;
                                     DG       : integer;
                                     DAT      : integer;
                                     Cap      : pTW_Capability;
                                     MSG      : integer;
                                     Return   : integer;
                                     Status   : integer;
                                     Level    : TTwnErrorLevel);
begin
  {$IFDEF TWNDEBUG}
  try
    if Assigned(FTWAINQueue)
    then FTWAINQueue.LogTriplet(Dest, DG, DAT, Cap, MSG, Return, Status, Level);
  except
  end;
  {$ENDIF}
end; // TmcmTWAINThread.LogTriplet.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

initialization
end.
