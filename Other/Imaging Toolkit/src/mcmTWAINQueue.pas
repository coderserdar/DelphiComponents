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
// $Log:  21890: mcmTWAINQueue.pas 
//
//    Rev 1.18    2014-03-28 17:52:56  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.17    2014-01-15 13:42:00  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.16    2013-12-04 23:16:14  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.15    2013-11-27 16:27:46  mcm
// Eliminated use of TThread.Resume & Suspend. As an upside, the TmcmTWAIN
// threads only runs while communicating with TWAIN and drivers.
//
//    Rev 1.14    08-01-2009 21:09:22  mcm    Version: DT 3.8
// Added support for Delphi 2009
//
//    Rev 1.13    11-08-2007 10:59:12  mcm    Version: DT 3.7
// Added support for Delphi 2007
//
//   Rev 1.12    11-06-2005 09:46:52  mcm    Version: DT 3.5
// Added comment.

//
//   Rev 1.11    27-05-2005 23:09:40  mcm

//
//   Rev 1.10    15-05-2005 20:59:10  mcm    Version: DT 3.5
// Additional ckeck in Destroy.

//
//   Rev 1.9    19-02-2005 00:23:08  mcm    Version: DT 3.4
// Modified where and when the hidden Window handle is created/destroyed.
// Added support for showing UI for configuration - not acquisition.

//
//   Rev 1.8    16-09-2004 20:44:52  mcm
// Modified shut-down mechanisme to avoid dead-lock if application has been idle
// for approximately 15 min.

//
//   Rev 1.7    05-09-2004 19:57:40  mcm    Version: DT 3.2
// Force FSourceID.Id to zero before opening a data source. 

//
//   Rev 1.6    25-03-2004 21:54:54  mcm
// Modified DestroyWindowHandle, to release the window handle before
// unregistering the window class.

//
//   Rev 1.5    05-12-2003 11:41:56  mcm
// Removed un-necessary code (WinRect).

//
//   Rev 1.4    21-11-2003 22:00:22  mcm    Version: DT3.0
// Solved Delphi 6 problem by posting a message to release the event procedures.
// Corrected interpretation and offset to image data in 8 bit palette images
// where biClrUsed is less than 256.

//
//   Rev 1.3    12-11-2003 14:04:50  mcm    Version: DT3.0
// Added fixes for Windows 95, 98 and Me.

//
//   Rev 1.2    06-11-2003 09:06:50  mcm    Version: DT3.0
// Minor demo twick

//
//   Rev 1.1    04-11-2003 23:14:52  mcm    Version: DT3.0
// Minor correction to pre-logging OpenDS etc in DSMEntry.

//
//   Rev 1.0    04-11-2003 20:12:58  mcm    Version: DT3.0
// Initial version. Threaded TWAIN interface

unit mcmTWAINQueue;

{$INCLUDE mcmDefines.pas}

interface

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

uses {$IFDEF GE_DXE2}
     System.Classes, WinApi.Windows, Vcl.Controls, WinApi.Messages, System.SysUtils, 
     {$ELSE}
     Classes, Windows, Controls, Messages, SysUtils,
     {$ENDIF}
     twain,
     twainproc,
     mcmTWAINLog;

type
  TDSMEntryData = record
  pOrigin : pTW_IDENTITY;
  pDest   : pTW_IDENTITY;
  DG      : TW_UINT32;
  DAT     : TW_UINT16;
  MSG     : TW_UINT16;
  pData   : TW_MEMREF;
  end;
  PDSMEntryData = ^TDSMEntryData;


  TDSMFailureData = record
  DG     : TW_UINT32;
  DAT    : TW_UINT16;
  CAP    : TW_UINT16;
  MSG    : TW_UINT16;
  Result : word;
  Status : word;
  end;
  PDSMFailureData = ^TDSMFailureData;

  TmcmTWAINQueue = class(TThread)
  private
    { Private declarations }
    FFailureData    : TDSMFailureData;
  protected
    { Protected declarations }
    FOldWin         : boolean;
    FParentWnd      : HWnd;
    FLock           : TRTLCriticalSection;
    FLogLock        : TRTLCriticalSection;
  {$IFDEF MCMDEMO}
    pData           : Pointer;
  {$ENDIF}

    FRetries        : integer;
    FWaitMsec       : longint;
    FMsgID          : integer;
    FMsgIDConfirm   : integer;

    FParams         : TCreateParams;
    FWinHandle      : THandle;
    FReceiveHandle  : THandle;

    FDSMMemAlloc    : TDSM_MemAllocate;
    FDSMMemFree     : TDSM_MemFree;
    FDSMMemLock     : TDSM_MemLock;
    FDSMMemUnLock   : TDSM_MemUnlock;

    FApplicationID  : pTW_IDENTITY;      // Storage for App identification
    FSourceID       : pTW_IDENTITY;      // Storage for DS (Source) identification
    FDSM_Entry      : TDSM_Entry_Proc;   // Entry point to the Source Manager
    FDSMDLL         : THandle;           // Handle to Source Manager

    FIsDSMOpen      : bool;              // Flag for an Open Source Manager
    FIsDSOpen       : bool;              // Flag for an Open Source
    FIsDSEnabled    : bool;              // Flag for an Enabled Source
    FSourceVersion  : integer;

    FResult         : word;              // Result from last call to DSM/DS
    FStatus         : word;              // Status from DS on failure.
    FState          : word;

    FMsgLevel       : TTwnErrorLevel;    // Filter which messages to save.
    FLogToFile      : boolean;           // On True Save messages to .\APPTWN.LOG
    mcmTWAINLog     : TmcmTWAINLog;

    procedure   CreateWindowHandle;
    procedure   DestroyWindowHandle;
    procedure   PeekMessage;
    function    ProcessMessage(lpMsg : PMSG) : bool;

    function    SetupDSMMemoryHandler : TW_UINT16;    
    
    function    GetDSM10Path : string;
    function    GetDSM20Path : string;

    function    GetIsDSMOpen : bool;
    function    GetIsDSOpen : bool;
    function    GetIsDSEnabled : bool;

    function    GetLogFileName : string;
    function    GetLogToFile : boolean;
    function    GetMessageLevel : TTwnErrorLevel;

    procedure   SetLogFileName(Value : string);
    procedure   SetLogToFile(Value : boolean);
    procedure   SetMessageLevel(Value : TTwnErrorLevel);
  public
    { Public declarations }
    FMsgEvent       : THandle;
    FWaitEvent      : THandle;
    FStartEvent     : THandle;
    FWaitResult     : TW_UINT16;

    FShowUI         : boolean;
    FModalUI        : boolean;

    constructor Create(CreateSuspended : Boolean);
    destructor  Destroy; override;
    procedure   Execute; override;

    procedure   Lock;
    procedure   Unlock;

    procedure   MoveWindow;
    procedure   ParentWindow(ParentWnd : HWnd);
    procedure   WndThreadMessage(var Message: TMessage);

    function    DSMEntry         (    pOrigin       : pTW_IDENTITY;
                                      pDest         : pTW_IDENTITY;
                                      DG            : TW_UINT32;
                                      DAT           : TW_UINT16;
                                      MSG           : TW_UINT16;
                                      pData         : TW_MEMREF) : TW_UINT16;

    function    CloseDS : TW_UINT16;
    function    CloseDSM(Handle : THandle) : TW_UINT16;
    function    ConfigureDS(var ShowUI : boolean; var ModalUI : boolean) : TW_UINT16;
    function    DisableDS : TW_UINT16;
    function    EnableDS(var ShowUI : boolean; var ModalUI : boolean) : TW_UINT16;
    function    OpenDS  : TW_UINT16;
    function    LoadDSM(Path : string) : THandle;
    function    OpenDSM(Handle : THandle) : TW_UINT16;

    // Enable code when running in "DEBUG" mode.
    // Logs TWAIN communication between application and data source.
    procedure   LogDelete;
    procedure   LogMessage       (    LogStr        : string);
    procedure   LogTriplet       (    Dest          : integer;
                                      DG            : integer;
                                      DAT           : integer;
                                      Cap           : pTW_Capability;
                                      MSG           : integer;
                                      Return        : integer;
                                      Status        : integer;
                                      Level         : TTwnErrorLevel);
    procedure   QuitThread;

    property    ApplicationID : pTW_IDENTITY
      read      FApplicationID
      write     FApplicationID;
    property    IsDSOpen : bool
      read      GetIsDSOpen;
    property    IsDSMOpen : bool
      read      GetIsDSMOpen;
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
    property    MsgID : integer
      read      FMsgID
      write     FMsgID;
    property    MsgReceived : integer
      read      FMsgIDConfirm
      write     FMsgIDConfirm;
    property    ReceiveHandle : THandle
      read      FReceiveHandle
      write     FReceiveHandle;
    property    Retries : integer
      read      FRetries
      write     FRetries;
    property    SourceID : pTW_IDENTITY
      read      FSourceID
      write     FSourceID;
    property    SourceVersion  : integer
      read      FSourceVersion;
    property    WaitMsec : longint
      read      FWaitMsec
      write     FWaitMsec;
    property    WinHandle : THandle
      read      FWinHandle;
end;


implementation

threadvar GlobalHwnd : THandle;

//------------------------------------------------------------------------------
// Default window handler
//------------------------------------------------------------------------------

function StdThreadWndProc(HWindow : HWnd; Message, WParam, LParam : Longint) : Longint; stdcall;
begin
  // Handle only messages pertaining to the window handle (HWindow).
  if (Message < WM_USER) and (GlobalHwnd = HWindow)
  then Result := DefWindowProc(HWindow, Message, WParam, LParam)
  else Result := 1;
end; // StdThreadWndProc.


//------------------------------------------------------------------------------
// TmcmTWAINQueue
//------------------------------------------------------------------------------

constructor TmcmTWAINQueue.Create(CreateSuspended : Boolean);
begin
  {$IFDEF MCMDEMO}
    pData := @pCR_LF;
  {$ENDIF}

  if (Win32Platform = VER_PLATFORM_WIN32_NT)
  then FOldWin := False
  else FOldWin := True;

  FParentWnd      := HWND_TOPMOST;
  FWinHandle      := 0;
  FReceiveHandle  := 0;

  FWaitMsec       := DefaultWait;
  FRetries        := 100;

  FDSMMemAlloc    := Nil;
  FDSMMemFree     := Nil;
  FDSMMemLock     := Nil;
  FDSMMemUnLock   := Nil;
  
  FDSM_Entry      := Nil;
  FDSMDLL         := 0;

  InitializeCriticalSection(FLock);
  InitializeCriticalSection(FLogLock);
  FWaitEvent := CreateEvent(Nil, True, False, 'EmcmTWAINQueueThreadEvent');
  ResetEvent(FWaitEvent);
  FMsgEvent  := CreateEvent(Nil, True, False, 'EmcmTWAINQueueMsgEvent');
  ResetEvent(FMsgEvent);

  FStartEvent := CreateEvent(Nil, True, False, 'EmcmTWAINQueueStartEvent');
  ResetEvent(FStartEvent);

  // Create a new log file.
  mcmTWAINLog := Nil;
  mcmTWAINLog := TmcmTWAINLog.Create(Nil);
  LogFileName := '.\APPTWN.LOG';
  // Delete previous log file.
  LogDelete;

  {$IFDEF TWNDEBUG}
  LogMessage('# TmcmTWAINQueue, Created thread');
  {$ENDIF}
  FLogToFile := False;
  FMsgLevel  := ML_NONE;

  Inherited Create(CreateSuspended);
  Priority := tpNormal;
  FreeOnTerminate := False;
end; // TmcmTWAINQueue.Create.


destructor TmcmTWAINQueue.Destroy;
begin
  if (Self <> Nil)
  then begin
       {$IFDEF TWNDEBUG}
         LogMessage('# TmcmTWAINQueue, Terminating thread');
       {$ENDIF}
       if Assigned(mcmTWAINLog)
       then mcmTWAINLog.Free;
       mcmTWAINLog := Nil;

       CloseHandle(FWaitEvent);
       CloseHandle(FMsgEvent);

       DeleteCriticalSection(FLock);
       DeleteCriticalSection(FLogLock);

       Inherited Destroy;
  end;
end; // TmcmTWAINQueue.Destroy.


procedure TmcmTWAINQueue.QuitThread;
begin
  while Not(PostThreadMessage(ThreadID, WM_MCMWINRECT, 0, 0))
  do Sleep(0);
  while Not(PostThreadMessage(ThreadID, WM_QUIT, 0, 0))
  do Sleep(0);
end; // TmcmTWAINQueue.QuitThread.


//------------------------------------------------------------------------------
// TWAIN Window - Hidden window used to "handle" data sources.
//------------------------------------------------------------------------------

procedure TmcmTWAINQueue.WndThreadMessage(var Message: TMessage);
begin
  Message.Result := DefWindowProc(FWinHandle, Message.Msg, Message.WParam, Message.LParam);
end; // TmcmTWAINQueue.WndThreadMessage.


procedure TmcmTWAINQueue.CreateWindowHandle;
var hinst             : dword;
    ClassIsRegistered : boolean;
begin
  // Register the window class (if not already registered)
  FParams.WindowClass.lpszClassName := 'TmcmTWAINQueueClass';
  ClassIsRegistered := GetClassInfo(HInstance, FParams.WindowClass.lpszClassName, FParams.WindowClass);

  if Not(ClassIsRegistered) or
     (FParams.WindowClass.lpfnWndProc <> @{$IFDEF GE_DXE2}WinApi.{$ENDIF}windows.DefWindowProc)
  then begin
       if ClassIsRegistered
       then UnregisterClass(FParams.WindowClass.lpszClassName, FParams.WindowClass.hInstance);
       FParams.Caption := 'TmcmTWAINQueue';
       FParams.WindowClass.style := CS_BYTEALIGNWINDOW or CS_GLOBALCLASS or CS_OWNDC;
       FParams.WindowClass.lpfnWndProc := @{$IFDEF GE_DXE2}WinApi.{$ENDIF}windows.DefWindowProc;
       FParams.WindowClass.cbClsExtra := 0;
       FParams.WindowClass.cbWndExtra := 0;
       FParams.WindowClass.hInstance := hInstance;
       FParams.WindowClass.hIcon := LoadIcon(0, IDI_APPLICATION);
       FParams.WindowClass.hCursor := LoadCursor(0, IDC_ARROW);
       FParams.WindowClass.hbrBackground := GetStockObject(WHITE_BRUSH);
       FParams.WindowClass.lpszMenuName :=  Nil;
       FParams.WinClassName := 'TmcmTWAINQueueClass';
       ClassIsRegistered := (RegisterClass(FParams.WindowClass) <> 0);
  end;

  if ClassIsRegistered
  then begin
       // Create the main window.
       hinst := hInstance;
       FWinHandle := CreateWindowEx(WS_EX_CONTROLPARENT or
                                    WS_EX_NOPARENTNOTIFY or
                                    WS_EX_TRANSPARENT or
                                    WS_EX_DLGMODALFRAME,
                                    FParams.WinClassName,
                                    FParams.Caption,
                                    WS_OVERLAPPED or
                                    WS_CLIPSIBLINGS or
                                    WS_CLIPCHILDREN or
                                    WS_TABSTOP,
                                    integer(CW_USEDEFAULT),
                                    integer(CW_USEDEFAULT),
                                    integer(CW_USEDEFAULT),
                                    integer(CW_USEDEFAULT),
                                    0, 0, hinst, Nil);
       GlobalHwnd := FWinHandle;
       // Subclass the window.
       SetWindowLong(FWinHandle, GWL_USERDATA, integer(Self));
       SetWindowLong(FWinHandle, GWL_WNDPROC, integer(@StdThreadWndProc));
  end;
end; // TmcmTWAINQueue.CreateWindowHandle.


procedure TmcmTWAINQueue.DestroyWindowHandle;
begin
  if (FWinHandle <> 0)
  then begin
       {$IFDEF GE_DXE2}
       SetWindowLong(FWinHandle, GWL_WNDPROC, integer(@WinApi.windows.DefWindowProc));
       {$ELSE}
       SetWindowLong(FWinHandle, GWL_WNDPROC, integer(@windows.DefWindowProc));
       {$ENDIF}
       DestroyWindow(FWinHandle);
  end;
  FWinHandle := 0;
  if (UnregisterClass(FParams.WindowClass.lpszClassName, FParams.WindowClass.hInstance) = False)
  then begin
       // We have a problem. Class did not un-register, and can therefore not
       // be registered again unless the "process" is freed.
  end;
end; // TmcmTWAINQueue.DestroyWindowHandle.


procedure TmcmTWAINQueue.ParentWindow(ParentWnd : HWnd);
begin
  if (FWinHandle <> 0)
  then begin
       if ((Win32MajorVersion >= 4) and (Win32MinorVersion > 10)) or
          (Win32MajorVersion >= 5) or
          (Win32Platform = VER_PLATFORM_WIN32_NT)
       then begin
            // Doesn't work in Windows 95 & 98.
            if ({$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.GetParent(FWinHandle) <> ParentWnd)
            then {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.SetParent(FWinHandle, 0);
            FParentWnd := ParentWnd;
       end
       else begin
            FParentWnd := HWND_TOPMOST;
            {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.SetParent(FWinHandle, 0);
       end;
  end;
end; // TmcmTWAINQueue.ParentWindow.


procedure TmcmTWAINQueue.MoveWindow;
begin
  if (FWinHandle <> 0)
  then SetForegroundWindow(FWinHandle);
end; // TmcmTWAINQueue.MoveWindow.


procedure TmcmTWAINQueue.Lock;
begin
//  EnterCriticalSection(FLock);
end; // TmcmTWAINQueue.Lock.


procedure TmcmTWAINQueue.Unlock;
begin
//  LeaveCriticalSection(FLock);
end; // TmcmTWAINQueue.Unlock.


//------------------------------------------------------------------------------
// Threads main message loop.
//------------------------------------------------------------------------------

procedure TmcmTWAINQueue.Execute;
var Msg : TMsg;
begin
  WaitForSingleObject(FStartEvent, 10000);

  CreateWindowHandle;
  SetEvent(FWaitEvent);
  while Not(Terminated)
  do begin
     try
       {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.WaitMessage;
       if {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(Msg, 0, WM_QUIT, WM_QUIT, PM_REMOVE)
       then begin
            // Flush message queue.
            while ({$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(Msg, 0, 0, 0, PM_NOYIELD or PM_REMOVE) and (Msg.message <> WM_PAINT))
            do begin
            end;
            Terminate;
       end
       else PeekMessage;
     except
       On E:Exception
       do ;
     end;
  end;
  {$IFDEF MCMDEMO}
    if (byte(pData^) <> $0D)
    then begin
         CloseHandle(FWaitEvent);
         CloseHandle(FMsgEvent);
    end;
  {$ENDIF}
  DestroyWindowHandle;
end; // TmcmTWAINQueue.Execute.


procedure TmcmTWAINQueue.PeekMessage;
var Msg     : TMsg;
    DSMData : TDSMEntryData;
begin
  while Not(Terminated) and {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(Msg, 0, 0, 0, PM_REMOVE)
  do begin
     if (Msg.Message = WM_QUIT)
     then begin
          Terminate;
          Break;
     end;
     try
       // Pass message to Data Source.
       if (Msg.hwnd = 0)
       then begin
            case Msg.Message of
            WM_OPENDSM      : begin
                                // CreateWindowHandle;
                                {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.SetParent(FWinHandle, 0);
                                FMsgIDConfirm := FMsgID;
                                FApplicationID.Id := 0;
                                FWaitResult := OpenDSM(FWinHandle);
                              end;
            WM_CLOSEDSM     : begin
                                FMsgIDConfirm := FMsgID;
                                FWaitResult := CloseDSM(FWinHandle);
                                // DestroyWindowHandle;
                              end;
            WM_OPENDS       : begin
                                FMsgIDConfirm := FMsgID;
                                FWaitResult := OpenDS;
                              end;
            WM_CLOSEDS      : begin
                                FMsgIDConfirm := FMsgID;
                                FWaitResult := CloseDS;
                              end;
            WM_ENABLEDS     : begin
                                FMsgIDConfirm := FMsgID;
                                FShowUI := boolean(msg.wParam);
                                FWaitResult := EnableDS(FShowUI, FModalUI);
                              end;
            WM_DISABLEDS    : begin
                                FMsgIDConfirm := FMsgID;
                                FWaitResult := DisableDS;
                              end;
            WM_CONFIGUREDS  : begin
                                FMsgIDConfirm := FMsgID;
                                FShowUI := boolean(msg.wParam);
                                FWaitResult := ConfigureDS(FShowUI, FModalUI);
                              end;
            WM_DSMENTRY     : begin
                                FMsgIDConfirm := FMsgID;
                                DSMData := PDSMEntryData(Msg.lParam)^;
                                with DSMData
                                do FWaitResult := DSMEntry(pOrigin, pDest,
                                                           DG, DAT, MSG, pData);
                                // As a WM_DSMENTRY message can be sent more
                                // than once, we'll have to empty the message
                                // queue for WM_DSMENTRY messages.
                                // Ref. Moving the data source window.
                                while {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(Msg, 0, WM_DSMENTRY, WM_DSMENTRY, PM_REMOVE)
                                do ;
                              end;
            WM_MCMWINRECT   : begin
                                ParentWindow(Hwnd(Msg.wParam));
                                MoveWindow;
                              end;
            end;

            if (WM_OPENDSM <= Msg.message) and (Msg.message < WM_NEXTXFER)
            then begin
                 ResetEvent(FMsgEvent);
                 SetEvent(FWaitEvent);
                 if (FWaitMsec = -1) 
                 then WaitForSingleObject(FMsgEvent, DWORD(-1))
                 else WaitForSingleObject(FMsgEvent, FWaitMsec); // div 10);
            end;
       end
       else begin
            if FIsDSOpen
            then begin
                 if Not(ProcessMessage(PMSG(@Msg)))
                 then begin
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                 end;
            end;
       end;
     except
       On E:Exception
       do ;
     end;
  end;
end; // TmcmTWAINQueue.PeekMessage.


function TmcmTWAINQueue.ProcessMessage(lpMsg : PMSG) : bool;
//------------------------------------------------------------------------------
// FUNCTION: ProcessMessage
//
// ARGS:    lpMsg  Pointer to Windows msg retrieved by GetMessage
//          hWnd   Application's main window handle
//
// RETURNS: True  if application should process message as usual
//          False if application should skip processing of this message
//
// NOTES:   1). be sure both Source Manager and Source are open
//          2). two way message traffic:
//              - relay windows messages down to Source's modeless dialog
//              - retrieve TWAIN messages from the Source
//
// COMMENT: ProcessSourceMessage is designed for applications that can only
// have one Source open at a time.  If you wish your application to have more
// than one Source open at a time please consult the TWAIN ToolKit for
// event handling instructions.
//------------------------------------------------------------------------------
var twRC    : TW_UINT16;
    twEvent : TW_EVENT;
begin
  twRC := TWRC_NOTDSEVENT;
  // Only ask Source Manager to process event if there is a Source connected.
  if (FIsDSMOpen and FIsDSOpen)
  then begin
       // A Source provides a modeless dialog box as its user interface.
       // The following call relays Windows messages down to the Source's
       // UI that were intended for its dialog box.  It also retrieves TWAIN
       // messages sent from the Source to our Application.
       twEvent.pEvent := TW_MEMREF(lpMsg);
       twRC := DSMEntry(FApplicationID,
                        FSourceID,
                        DG_CONTROL,
                        DAT_EVENT,
                        MSG_PROCESSEVENT,
                        TW_MEMREF(@twEvent));
       if (twRC <> TWRC_EXCEPTION)
       then begin
            case twEvent.TWMessage of
            MSG_XFERREADY   : begin
                                {$IFDEF MCMDEMO}
                                  if (byte(pCR_LF[0]) <> $0D)
                                  then PostThreadMessage(FReceiveHandle, WM_QUIT, 0, 0)
                                  else
                                {$ENDIF}
                                PostThreadMessage(FReceiveHandle, WM_ONXFERREADY, 0, 0);
                               end;
            MSG_CLOSEDSREQ,         // User closed ds, Disable, CloseDS, CloseDSM.
            MSG_CLOSEDSOK   : begin // Data source closed correctly.
                                PostThreadMessage(FReceiveHandle, WM_ONCLOSEREQUEST, 0, 0);
                              end;
            MSG_DEVICEEVENT : begin // Device event from data source.
                                PostThreadMessage(FReceiveHandle, WM_ONDEVICEEVENT, 0, 0);
                              end;
            MSG_NULL        : begin // No message from the Source to the App.
                              end;
            else              begin // Possible new message.
                              end;
            end; // End of Message switch.
       end;
  end; // end of if DS and DSM Open.

  // Tell the caller what happened.
  // Return True if message was for data source.
  Result := (twRC = TWRC_DSEVENT);
end; // TmcmTWAINQueue.ProcessMessage.


function TmcmTWAINQueue.DSMEntry(pOrigin : pTW_IDENTITY;
                                 pDest   : pTW_IDENTITY;
                                 DG      : TW_UINT32;
                                 DAT     : TW_UINT16;
                                 MSG     : TW_UINT16;
                                 pData   : TW_MEMREF) : TW_UINT16;
var StatResult : word;
    twStatus   : TW_STATUS;
begin
  Lock;
  FResult := TWRC_FAILURE;
  try
  {$IFDEF TWNDEBUG}
    if ((MSG = MSG_SET) and (DAT = DAT_CAPABILITY)) or
       ((DAT = DAT_IDENTITY) and (MSG <> MSG_GETFIRST) and (MSG <> MSG_GETNEXT)) or
       (DAT = DAT_USERINTERFACE)
    then LogTriplet(integer(pDest <> Nil), DG, DAT, pData, MSG, TWRC_CUSTOMBASE+99, TWCC_CUSTOMBASE+99, ML_INFO);
  {$ENDIF}
    if (@FDSM_Entry <> Nil)
    then FResult := FDSM_Entry(pOrigin, pDest, DG, DAT, MSG, pData);
  except
    FResult := TWRC_EXCEPTION;
  end;

  // Log call result.
  {$IFDEF TWNDEBUG}
    case FResult of
    TWRC_SUCCESS,
    TWRC_CANCEL,
    TWRC_XFERDONE,
    // After MSG_GETNEXT if nothing left
    TWRC_ENDOFLIST,
    TWRC_INFONOTSUPPORTED,
    TWRC_DATANOTAVAILABLE : LogTriplet(integer(pDest <> Nil), DG, DAT, pData, MSG, FResult, FStatus, ML_INFO);
    // App may get TW_STATUS for info on failure.
    TWRC_FAILURE,
    // "tried hard"; get status
    TWRC_CHECKSTATUS,
    // The source manager or source raised an exception.
    TWRC_EXCEPTION        : LogTriplet(integer(pDest <> Nil), DG, DAT, pData, MSG, FResult, FStatus, ML_ERROR);
    // Message loop.
    TWRC_DSEVENT          : if (pTW_EVENT(pData)^.TWMessage <> MSG_NULL)
                            then LogTriplet(integer(pDest <> Nil), DG, DAT, pData, MSG, FResult, FStatus, ML_INFO)
                            else LogTriplet(integer(pDest <> Nil), DG, DAT, pData, MSG, FResult, FStatus, ML_FULL);
    TWRC_NOTDSEVENT       : LogTriplet(integer(pDest <> Nil), DG, DAT, pData, MSG, FResult, FStatus, ML_FULL);
    end;
  {$ENDIF}

  // Get status from source manager / source.
  case FResult of
  TWRC_SUCCESS,
  TWRC_CANCEL,
  TWRC_DSEVENT,
  TWRC_NOTDSEVENT,
  TWRC_XFERDONE,
  TWRC_ENDOFLIST   : FStatus := TWCC_SUCCESS; // After MSG_GETNEXT if nothing left
  TWRC_EXCEPTION   : begin
                       // The source manager or source raised an exception.
                       // If call to obtain stauts fails we better close the DS.
                       if (DAT <> DAT_STATUS)
                       then begin
                            FIsDSOpen := False;
                            FIsDSMOpen := False;
                            // Explicitly free the DSM library.
                            if (FDSMDLL > 0)
                            then begin
                                 FreeLibrary(FDSMDLL);
                                 FDSMDLL := 0;
                                 // The data source id will no longer be valid after
                                 // twain is killed.  If the id is left around the
                                 // data source can not be found or opened
                                 FSourceID.Id := 0;
                                 FDSM_Entry := Nil;
                            end;
                       end;

                       // Notify app that it's time to re-enable TWAIN menus.
                       PostThreadMessage(FReceiveHandle, WM_ONFATALERROR, 0, 0);
                     end;
  else begin // Other case results.
       try
         FStatus := TWCC_BUMMER;

         // Determine details of failure from DSM.
         if (@FDSM_Entry <> Nil)
         then StatResult := FDSM_Entry(pOrigin, pDest, DG_CONTROL, DAT_STATUS, MSG_GET, TW_MEMREF(@twStatus))
         else StatResult := TWRC_FAILURE;

         if (StatResult = TWRC_SUCCESS)
         then FStatus := twStatus.ConditionCode;
       except
         // If an exception is raised when getting status from data source what
         // should we do: Close DS & DSM.
         On E:Exception
         do begin
            {$IFDEF TWNDEBUG}
              LogMessage('#E ' + E.Message);
            {$ENDIF}
         end;
       end;
  end;
  end; // End case

  // Report error to owner.
  if (FStatus <> TWRC_SUCCESS) and (DAT <> DAT_STATUS)
  then begin
       // Raise event with call result, status.
       // Extract Capability from pData.
       if (DAT = DAT_CAPABILITY) and (pData <> Nil)
       then FFailureData.CAP := pTW_Capability(pData)^.Cap
       else FFailureData.CAP := 0;
       FFailureData.DG  := DG;
       FFailureData.DAT := DAT;
       FFailureData.MSG := MSG;
       FFailureData.Result := FResult;
       FFailureData.Status := FStatus;
       PostThreadMessage(FReceiveHandle, WM_ONFAILURE, 0, dword(@FFailureData));
  end;
  Result := FResult;
  Unlock;
end; // TmcmTWAINQueue.DSMEntry.


//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

function TmcmTWAINQueue.GetIsDSMOpen : bool;
begin
  Result := FIsDSMOpen;
end; // TmcmTWAINQueue.GetIsDSMOpen.


function TmcmTWAINQueue.GetIsDSOpen : bool;
begin
  Result := FIsDSOpen;
end; // TmcmTWAINQueue.GetIsDSOpen.


function TmcmTWAINQueue.GetIsDSEnabled : bool;
begin
  Result := FIsDSEnabled;
end; // TmcmTWAINQueue.GetIsDSEnabled.


function TmcmTWAINQueue.SetupDSMMemoryHandler : TW_UINT16;
var twRC        : TW_UINT16;
    pEntryPoint : pTW_ENTRYPOINT;
begin
  twRC := TWRC_FAILURE;
  if ((FApplicationID.SupportedGroups and DF_DSM2) = DF_DSM2)
  then begin
       pEntryPoint := GlobalEntryPoint;
       twRC := DSMEntry(FApplicationID,
                        Nil,
                        DG_CONTROL,
                        DAT_ENTRYPOINT,
                        MSG_GET,
                        TW_MEMREF(pEntryPoint));
       if (twRC <> TWRC_SUCCESS)
       then ClearGlobalEntryPoint;
  end;
  Result := twRC;
end; // TmcmTWAINQueue.SetupDSM20MemoryHandler.


function TmcmTWAINQueue.GetDSM10Path : string;
const WINDIRPATHSIZE = 1024;
var BufSize : cardinal;
    pDir    : PChar;
begin
  BufSize := WINDIRPATHSIZE;
  GetMem(pDir, BufSize);
  BufSize := GetWindowsDirectory(pDir, WINDIRPATHSIZE);
  if (BufSize >= WINDIRPATHSIZE - 12)
  then begin
       FreeMem(pDir);
       GetMem(pDir, BufSize + 32);
       BufSize := GetWindowsDirectory(pDir, WINDIRPATHSIZE);
  end;

  if (BufSize > 0)
  then begin
       if (pDir[StrLen(pDir)-1] <> '\')
       then lstrcat(pDir, '\');
       Result := pDir;
       {$IFDEF WIN32}
         Result := Result + 'TWAIN_32.DLL';
       {$ELSE}
         Result := Result + 'TWAIN.DLL';
       {$ENDIF}
  end;
  FreeMem(pDir);

  if Not(FileExists(Result))
  then Result := '';
end; // TmcmTWAINQueue.GetDSM10Path.


function TmcmTWAINQueue.GetDSM20Path : string;
const WINDIRPATHSIZE = 1024;
var BufSize : cardinal;
    pDir    : PChar;
begin
  BufSize := WINDIRPATHSIZE;
  GetMem(pDir, BufSize);
  BufSize := GetSystemDirectory(pDir, WINDIRPATHSIZE);
  if (BufSize >= WINDIRPATHSIZE - 12)
  then begin
       FreeMem(pDir);
       GetMem(pDir, BufSize + 32);
       BufSize := GetSystemDirectory(pDir, WINDIRPATHSIZE);
  end;

  if (BufSize > 0)
  then begin
       if (pDir[StrLen(pDir)-1] <> '\')
       then lstrcat(pDir, '\');
       Result := pDir;
       Result := Result + 'TWAINDSM.DLL';
  end;
  FreeMem(pDir);

  if Not(FileExists(Result))
  then Result := '';
end; // TmcmTWAINQueue.GetDSM20Path.


function TmcmTWAINQueue.LoadDSM(Path : string) : THandle;
var DSMDLL : THandle;
begin
  DSMDLL := 0;
  if FileExists(Path)
  then begin
      DSMDLL := LoadLibrary(PChar(Path));
      if (DSMDLL >= TWN_VALID_HANDLE)
      then begin
           try
             FDSM_Entry := (GetProcAddress(DSMDLL, MAKEINTRESOURCE(1)));
           except
             FDSM_Entry := Nil;
           end;
      end
      else begin // Could not load library.
           {$IFDEF TWNDEBUG}
             if (MessageLevel >= ML_ERROR)
             then LogMessage('#M Could not load library.');
           {$ENDIF}
      end;
  end
  else begin
      {$IFDEF TWNDEBUG}
        if (MessageLevel >= ML_ERROR)
        then LogMessage('#M TWAIN DLL files may not exist.');
      {$ENDIF}
  end;
  Result := DSMDLL;
end; // TmcmTWAINQueue.LoadDSM.


function TmcmTWAINQueue.OpenDSM(Handle : THandle) : TW_UINT16;
//------------------------------------------------------------------------------
// FUNCTION: OpenDSM
//
// ARGS:    none
//
// RETURNS: current state of the Source Manager
//
// NOTES:     1). be sure SM is not already open
//            2). explicitly load the .DLL for the Source Manager
//            3). call Source Manager to:
//                - opens/loads the SM
//                - pass the handle to the app's window to the SM
//                - get the SM assigned AppIdentity.id field
//
//------------------------------------------------------------------------------
const WINDIRPATHSIZE = 1024;
var   twRC    : TW_UINT16;
      DSMPath : string;
begin
  Lock;
  twRC := TWRC_FAILURE;
  {$IFDEF TWNDEBUG}
    if FIsDSMOpen
    then LogMessage('#M OpenDSM, DSM already open');
  {$ENDIF}

  // Only open SM if currently closed
  if Not(FIsDSMOpen)
  then begin
       // Older TWAIN drivers/applications uses c:\windows\twain_32.dll
       // Current TWAIN drivers and applications uses c:\windows\system32\twaindsm.dll
       // If installing a 32 bit twaindsm.dll on a 64 bit windows install this 
       // file into c:\windows\syswow64\.
       DSMPath := GetDSM20Path;
       FDSMDLL := LoadDSM(DSMPath);
       
       if (FDSMDLL = 0) or (@FDSM_Entry = Nil)
       then begin
            DSMPath := GetDSM10Path;
            FDSMDLL := LoadDSM(DSMPath);
            
            FApplicationID^.ProtocolMajor := 1;
            FApplicationID^.ProtocolMajor := 9;
            FApplicationID^.SupportedGroups := FApplicationID^.SupportedGroups and DG_MASK;
       end
       else begin
            FApplicationID^.ProtocolMajor := 2;
            FApplicationID^.ProtocolMajor := 3;
            FApplicationID^.SupportedGroups := (FApplicationID^.SupportedGroups and DG_MASK) or DF_APP2;
       end;

       if (@FDSM_Entry <> Nil)
       then begin
            PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 2);
            // This call performs four important functions:
            // - opens/loads the SM
            // - passes the handle to the app's window to the SM
            // - returns the SM assigned AppIdentity.id field
            // - be sure to test the return code for SUCCESSful open of SM
            twRC := DSMEntry(FApplicationID,
                             Nil,
                             DG_CONTROL,
                             DAT_PARENT,
                             MSG_OPENDSM,
                             TW_MEMREF(@Handle));
            case twRC of
            // Needed for house keeping.  Do single open and do not
            // close SM which is not already open ....
            TWRC_SUCCESS : begin
                             FIsDSMOpen := True;
                             PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 3);

                             if ((FApplicationID^.SupportedGroups and DF_DSM2) = DF_DSM2)
                             then twRC := SetupDSMMemoryHandler;                             
                           end;
            // Trouble opening the SM, inform the user
            else FIsDSMOpen := False;
            end;
       end
       else begin // Could not obtain address to DSM_ENTRY.
            {$IFDEF TWNDEBUG}
              if (MessageLevel >= ML_ERROR)
              then LogMessage('#M Could not obtain address to DSM_ENTRY.');
            {$ENDIF}
       end;
  end;

  // Let the caller know what happened.
  Result := twRC;
  Unlock;
end; // TmcmTWAINQueue.OpenDSM.


function TmcmTWAINQueue.CloseDSM(Handle : THandle) : TW_UINT16;
//------------------------------------------------------------------------------
// FUNCTION: CloseDSM
//
// ARGS:    none
//
// RETURNS: current state of Source Manager
//
// NOTES:    1). be sure SM is already open
//           2). call Source Manager to:
//               - request closure of the Source identified by AppIdentity info
//
//------------------------------------------------------------------------------
var twRC : TW_UINT16;
begin
  Lock;
  twRC := TWRC_FAILURE;
  if FIsDSOpen
  then begin
       // A Source is Currently Open", "Cannot Close Source Manager.
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M CloseDSM, A Source is Currently Open. Cannot Close Source Manager.');
       {$ENDIF}
  end
  else begin
       // Only close something which is already open
       if FIsDSMOpen
       then begin
            // This call performs one important function:
            // - tells the SM which application, AppIdentity.id, is requesting
            //   SM to close.
            // - be sure to test return code, failure indicates SM did not close !!
            twRC := DSMEntry(FApplicationID,
                             Nil,
                             DG_CONTROL,
                             DAT_PARENT,
                             MSG_CLOSEDSM,
                             TW_MEMREF(@Handle));
            if (twRC = TWRC_SUCCESS)
            then begin
                 FIsDSMOpen := False;
                 PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 2);

                 ClearGlobalEntryPoint;

                 // Explicitly free the SM library.
                 if (FDSMDLL > 0)
                 then begin
                      FreeLibrary(FDSMDLL);
                      FDSMDLL := 0;
                      // The data source id will no longer be valid after
                      // twain is killed.  If the id is left around the
                      // data source can not be found or opened
                      FSourceID.Id := 0;
                      FDSM_Entry := Nil;
                      PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 1);
                 end;
            end
            else ; // Trouble closing the SM, inform the user.
       end
       else begin // Cannot close Source Manager, Source Manager is Closed.
            {$IFDEF TWNDEBUG}
              if (MessageLevel >= ML_ERROR)
              then LogMessage('#M Data Source Manager was not open');
            {$ENDIF}
       end;
  end;
  // Let the caller know what happened.
  Result := twRC;
  Unlock;
end; // TmcmTWAINQueue.CloseDSM.


function TmcmTWAINQueue.OpenDS : TW_UINT16;
//------------------------------------------------------------------------------
// FUNCTION: OpenDS
//
// ARGS:    none
//
// RETURNS: current state of select Source
//
// NOTES:    1). only attempt to open a source if it is currently closed
//           2). call Source Manager to:
//                - open the Source indicated by info in SourceID
//                - SM will fill in the unique .Id field
//------------------------------------------------------------------------------
var twRC : TW_UINT16;
begin
  Lock;
  twRC := TWRC_FAILURE;
  if Not(FIsDSMOpen)
  then begin
       // DSM not open - cannot open DS.
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M DSM not open, cannot open DS');
       {$ENDIF}
  end
  else begin
       // Source Manager is open.
       if Not(FIsDSOpen)
       then begin
            FSourceID^.Id := 0;

            // This will open the Source.
            twRC := DSMEntry(FApplicationID,
                             Nil,
                             DG_CONTROL,
                             DAT_IDENTITY,
                             MSG_OPENDS,
                             TW_MEMREF(FSourceID));
            case twRC of
            TWRC_SUCCESS : begin
                             FIsDSOpen := True;
                             PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 4);
                             FSourceVersion := FSourceID.ProtocolMajor * 10 +
                                               FSourceID.ProtocolMinor;
                           end;
            else           begin
			                     // Trouble opening the Source
			                     // Determine Condition Code
                           end;
            end;
       end
       else begin
            // Source is already open, TWAIN Information.
            {$IFDEF TWNDEBUG}
              if (MessageLevel >= ML_INFO)
              then LogMessage('#M Data Source already open');
            {$ENDIF}
       end;
  end;
  Result := twRC;
  Unlock;
end; // TmcmTWAINQueue.OpenDS.


function TmcmTWAINQueue.CloseDS : TW_UINT16;
//------------------------------------------------------------------------------
// FUNCTION: CloseDS
//
// ARGS:    none
//
// RETURNS: none
//
// NOTES:    1). only attempt to close an open Source
//           2). call Source Manager to:
//                - ask that Source identified by info in SourceID close itself
//------------------------------------------------------------------------------
var twRC : TW_UINT16;
begin
  Lock;
  twRC := TWRC_FAILURE;
  if FIsDSOpen
  then begin
       if FIsDSEnabled
       then begin
            // Source is Currently Enabled, Cannot Close Source.
            {$IFDEF TWNDEBUG}
              if (MessageLevel >= ML_ERROR)
              then LogMessage('#M Source is currently enabled. Cannot close source');
            {$ENDIF}
       end
       else begin
            // Close an open Source.
            twRC := DSMEntry(FApplicationID, Nil,
                             DG_CONTROL,
                             DAT_IDENTITY,
                             MSG_CLOSEDS,
                             TW_MEMREF(FSourceID));
            // Show error on close.
            if (twRC <> TWRC_SUCCESS)
            then begin
            end
            else begin
                 FIsDSOpen := False;
                 FSourceID.Id := 0;
                 FSourceID.ProductName[0] := #0;
                 PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 3);
                 // Notify app that it's time to re-enable TWAIN menus.
                 PostThreadMessage(FReceiveHandle, WM_ONCLOSESOURCE, 0, 0);
            end;
       end;
  end
  else begin
       // Cannot Close Source Source Not Open, Sequence Error.
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Sequence error. Cannot close source. Source not open.');
       {$ENDIF}
  end;
  Result := twRC;
  Unlock;
end; // TmcmTWAINQueue.CloseDS.


function TmcmTWAINQueue.ConfigureDS(var ShowUI : boolean;
                                    var ModalUI : boolean) : TW_UINT16;
//------------------------------------------------------------------------------
// FUNCTION: ConfigureDS
//
// NOTES:    1). only enable an open Source for configuration (no acquire).
//           2). call the Source Manager to:
//               - bring up the Source's User Interface in configuration mode.
//------------------------------------------------------------------------------
var twRC   : TW_UINT16;
    twUI   : TW_USERINTERFACE;
begin
  Lock;
  twRC := TWRC_FAILURE;

  // Only enable open Source's.
  if FIsDSOpen
  then begin
       if FIsDSEnabled
       then begin
            {$IFDEF TWNDEBUG}
              if (MessageLevel >= ML_ERROR)
              then LogMessage('#M Cannot enable source, already enabled');
            {$ENDIF}
       end
       else begin
            // This will display the Source User Interface. The Source should
            // only display a user interface that is compatible with the group
            // defined by FApplicationID.
            // SupportedGroups (in our case DG_IMAGE | DG_CONTROL)
            twUI.hParent := FWinHandle;
            twUI.ShowUI  := TW_BOOL(ShowUI);
            twUI.ModalUI := 0;
            twRC := DSMEntry(FApplicationID,
                             FSourceID,
                             DG_CONTROL,
                             DAT_USERINTERFACE,
                             MSG_ENABLEDSUIONLY,
                             TW_MEMREF(@twUI));
            case twRC of
            TWRC_SUCCESS,
            TWRC_CHECKSTATUS : begin
                                 // A data source may return TWRC_CHECKSTATUS if
                                 // twUI.ShowUI = false, but it cannot function
                                 // without it's own user interface. In this
                                 // case the data source is required to set
                                 // twUI.ShowUI = true, and continue normal
                                 // operation.

                                 // Result := True;
                                 PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 5);
                                 FIsDSEnabled := True;
                                 ShowUI  := boolean(twUI.ShowUI);
                                 ModalUI := boolean(twUI.ModalUI);
                                 {$IFDEF TWNDEBUG}
                                   if (twRC = TWRC_CHECKSTATUS) and Not(ShowUI)
                                   then LogMessage('#M Data Source does not support hidden UI.');
                                 {$ENDIF}
                               end;
            else               begin // Error
                               end;
            end;
       end;
  end
  else begin
       // Cannot Enable Source, No Source Open, TWAIN Error.
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Cannot enable source. No source open.');
       {$ENDIF}
  end;
  Result := twRC;
  Unlock;
end; // TmcmTWAINQueue.ConfigureDS.


function TmcmTWAINQueue.EnableDS(var ShowUI  : boolean;
                                 var ModalUI : boolean) : TW_UINT16;
//------------------------------------------------------------------------------
// FUNCTION: EnableDS
//
// NOTES:    1). only enable an open Source
//           2). call the Source Manager to:
//                - bring up the Source's User Interface
//------------------------------------------------------------------------------
var twRC   : TW_UINT16;
    twUI   : TW_USERINTERFACE;
begin
  Lock;
  twRC := TWRC_FAILURE;

  // Only enable open Source's.
  if FIsDSOpen
  then begin
       if FIsDSEnabled
       then begin
            {$IFDEF TWNDEBUG}
              if (MessageLevel >= ML_ERROR)
              then LogMessage('#M Cannot enable source, already enabled');
            {$ENDIF}
       end
       else begin
            // This will display the Source User Interface. The Source should
            // only display a user interface that is compatible with the group
            // defined by FApplicationID.
            // SupportedGroups (in our case DG_IMAGE | DG_CONTROL)
            twUI.hParent := FWinHandle;
            twUI.ShowUI  := TW_BOOL(ShowUI);
            twUI.ModalUI := 0;
            twRC := DSMEntry(FApplicationID,
                             FSourceID,
                             DG_CONTROL,
                             DAT_USERINTERFACE,
                             MSG_ENABLEDS,
                             TW_MEMREF(@twUI));
            case twRC of
            TWRC_SUCCESS,
            TWRC_CHECKSTATUS : begin
                                 // A data source may return TWRC_CHECKSTATUS if
                                 // twUI.ShowUI = false, but it cannot function
                                 // without it's own user interface. In this
                                 // case the data source is required to set
                                 // twUI.ShowUI = true, and continue normal
                                 // operation.

                                 // Result := True;
                                 PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 5);
                                 FIsDSEnabled := True;
                                 ShowUI  := boolean(twUI.ShowUI);
                                 ModalUI := boolean(twUI.ModalUI);
                                 {$IFDEF TWNDEBUG}
                                   if (twRC = TWRC_CHECKSTATUS) and Not(ShowUI)
                                   then LogMessage('#M Data Source does not support hidden UI.');
                                 {$ENDIF}
                               end;
            else               begin // Error
                               end;
            end;
       end;
  end
  else begin
       // Cannot Enable Source, No Source Open, TWAIN Error.
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Cannot enable source. No source open.');
       {$ENDIF}
  end;
  Result := twRC;
  Unlock;
end; // TmcmTWAINQueue.EnableDS.


function TmcmTWAINQueue.DisableDS : TW_UINT16;
//------------------------------------------------------------------------------
// FUNCTION: DisableDS
//
// ARGS:    none
//
// RETURNS: none
//
// NOTES:    1). only disable an open Source
//           2). call Source Manager to:
//                - ask Source to hide it's User Interface
//------------------------------------------------------------------------------
var twRC   : TW_UINT16;
    twUI   : TW_USERINTERFACE;
begin
  Lock;
  twRC := TWRC_FAILURE;
  if Not(FIsDSEnabled)
  then begin
       // Cannot Disable Source, Source Not Enabled, Sequence Error.
       {$IFDEF TWNDEBUG}
         if (MessageLevel >= ML_ERROR)
         then LogMessage('#M Cannot disable source. Source not enabled.');
       {$ENDIF}
  end
  else begin
       // Only disable open Source's.
       if FIsDSOpen
       then begin
            // Hide the Source UI.
            twUI.hParent := FWinHandle;
            twUI.ShowUI  := 0;
            twRC := DSMEntry(FApplicationID,
                             FSourceID,
                             DG_CONTROL,
                             DAT_USERINTERFACE,
                             MSG_DISABLEDS,
                             TW_MEMREF(@twUI));
            if (twRC = TWRC_SUCCESS)
            then begin
                 FIsDSEnabled := False;
                 PostThreadMessage(FReceiveHandle, WM_ONSTATECHANGED, 0, 4);
            end
            else begin // Error
            end;
       end;
  end;
  Result := twRC;
  Unlock;
end; // TmcmTWAINQueue.DisableDS.


//------------------------------------------------------------------------------
// Events
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Message logging
//------------------------------------------------------------------------------

function TmcmTWAINQueue.GetLogFileName : string;
begin
  Result := mcmTWAINLog.LogFileName;
end; // TmcmTWAINQueue.GetLogFileName.


procedure TmcmTWAINQueue.SetLogFileName(Value : string);
begin
  if Assigned(mcmTWAINLog)
  then begin
       if (mcmTWAINLog.LogFileName <> Value)
       then mcmTWAINLog.LogFileName := Value;
       mcmTWAINLog.DeleteLogFile;
  end;
end; // TmcmTWAINQueue.SetLogFileName.


function TmcmTWAINQueue.GetLogToFile : boolean;
begin
  Result := FLogToFile;
end; // TmcmTWAINQueue.GetLogToFile.


procedure TmcmTWAINQueue.SetLogToFile(Value : boolean);
begin
  FLogToFile := Value;
end; // TmcmTWAINQueue.SetLogToFile.


function TmcmTWAINQueue.GetMessageLevel : TTwnErrorLevel;
begin
  Result := FMsgLevel;
end; // TmcmTWAINQueue.GetMessageLevel.


procedure TmcmTWAINQueue.SetMessageLevel(Value : TTwnErrorLevel);
begin
  FMsgLevel := Value;
end; // TmcmTWAINQueue.SetMessageLevel.


procedure TmcmTWAINQueue.LogMessage(LogStr : string);
begin
  EnterCriticalSection(FLogLock);
  {$IFDEF TWNDEBUG}
  try
    if (FMsgLevel >= ML_FULL) or true
    then mcmTWAINLog.Str2File(LogStr);
  except
  end;
  {$ENDIF}
  LeaveCriticalSection(FLogLock);
end; // TmcmTWAINQueue.LogMessage.



procedure TmcmTWAINQueue.LogDelete;
begin
  EnterCriticalSection(FLogLock);
  {$IFDEF TWNDEBUG}
  try
    mcmTWAINLog.DeleteLogFile;
  except
  end;
  {$ENDIF}
  LeaveCriticalSection(FLogLock);
end; // TmcmTWAINQueue.LogDelete.


procedure TmcmTWAINQueue.LogTriplet(Dest     : integer;
                                     DG       : integer;
                                     DAT      : integer;
                                     Cap      : pTW_Capability;
                                     MSG      : integer;
                                     Return   : integer;
                                     Status   : integer;
                                     Level    : TTwnErrorLevel);
begin
  EnterCriticalSection(FLogLock);
  {$IFDEF TWNDEBUG}
  try
    if (FMsgLevel >= Level)
    then mcmTWAINLog.Triplet2Str(Dest, DG, DAT, Cap, MSG, Return, Status, Level);
  except
  end;
  {$ENDIF}
  LeaveCriticalSection(FLogLock);
end; // TmcmTWAINQueue.LogTriplet.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

initialization
end.
