//---------------------------------------------------------------------------
#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
#define WM_CUSTOM_1 (WM_USER_DDSERVICE + 1)

DEFINE_GUID(GUID_DEVINTERFACE_USB, 0xA5DCBF10L, 0x6530, 0x11D2, 0x90, 0x1F,
		   0x00, 0xC0, 0x4F, 0xB9, 0x51, 0xED);

TDDService1 *DDService1;
//---------------------------------------------------------------------------
__fastcall TDDService1::TDDService1(TComponent* Owner)
  : TDDService(Owner)
{
}

TServiceController __fastcall TDDService1::GetServiceController(void)
{
  return (TServiceController) ServiceController;
}

void __stdcall ServiceController(unsigned CtrlCode)
{
  DDService1->Controller(CtrlCode);
}

TServiceControllerEx __fastcall TDDService1::GetServiceControllerEx(void)
{
  return (TServiceControllerEx) ServiceControllerEx;
}

unsigned __stdcall ServiceControllerEx(unsigned CtrlCode, unsigned EventType,
  void * EventData, void * Context)
{
  return DDService1->ControllerEx(CtrlCode, EventType, EventData, Context);
}

TServiceConsoleCtrlHandler __fastcall TDDService1::GetConsoleCtrlHandler(void)
{
  return (TServiceConsoleCtrlHandler) ServiceConsoleCtrlHandler;
}
//---------------------------------------------------------------------------

bool __stdcall ServiceConsoleCtrlHandler(unsigned Ctrl)
{
  return DDService1->ConsoleCtrlHandler(Ctrl);
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::Cleanup()
{
	delete Timer1;
	if (DeviceHandle) {
		UnregisterDeviceNotification(DeviceHandle);
		DeviceHandle = NULL;
  	};
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServiceAfterInstall(TDDService *Sender)
{
	RegisterEventLogSource(Sender->ServiceName, ParamStr(0));
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServiceAfterUninstall(TDDService *Sender)
{
	UnRegisterEventLogSource(Sender->ServiceName);
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServiceConsoleEvent(TDDService *Sender, int CtrlCode,
		  bool &Handled)
{
	LogMessage("Console Ctrl: " + IntToStr(CtrlCode) + " - ThreadID: #" +
			 IntToStr(int(GetCurrentThreadId)), EVENTLOG_INFORMATION_TYPE);
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServiceContinue(TDDService *Sender, bool &Continued)

{
    if (Timer1) {
		Timer1->Enabled = true;
	}
	LogMessage("Continued", EVENTLOG_INFORMATION_TYPE);
}
//---------------------------------------------------------------------------
// W2K+
void __fastcall TDDService1::DDServiceDeviceEvent(TDDService *Sender, int EventType,
          TDDIntPtr EventData, int &MsgResult)
{
	LogMessage("DeviceEvent - EventType: $" + IntToHex(EventType, 8),
			 EVENTLOG_WARNING_TYPE);
}
//---------------------------------------------------------------------------

// W2K+  Never saw the OS sending this control code, a SCP however may use it
void __fastcall TDDService1::DDServiceNetBindChange(TDDService *Sender, int EventType)

{
	LogMessage("NetBindChange Type: #" + IntToStr(EventType),
			EVENTLOG_INFORMATION_TYPE);
}
//---------------------------------------------------------------------------

// W2K+  Never saw the OS sending this control code, a SCP however may use it
void __fastcall TDDService1::DDServiceParamChange(TDDService *Sender)
{
	LogMessage("ParamChange", EVENTLOG_INFORMATION_TYPE);
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServicePause(TDDService *Sender, bool &Paused)
{
	if (Timer1) {
		Timer1->Enabled = false;
	}
	//raise exception.Create('Test'); //Caught in DDServiceRunException
	LogMessage("Paused", EVENTLOG_INFORMATION_TYPE);
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServicePowerEvent(TDDService *Sender, int EventType,
		  TDDIntPtr EventData, int &MsgResult)
{
	LogMessage("PowerEvent - EventType: $" + IntToHex(EventType, 8),
             EVENTLOG_INFORMATION_TYPE);
  	// Note that Vista+ doesn't send PBT_APMQUERYSUSPEND but PBT_APMSUSPEND at once
	if (EventType == PBT_APMQUERYSUSPEND)
		MsgResult = BROADCAST_QUERY_DENY;  // Deny the request
}
//---------------------------------------------------------------------------

// Vista+
void __fastcall TDDService1::DDServicePreShutdown(TDDService *Sender)
{
	LogMessage("PreShutdown", EVENTLOG_INFORMATION_TYPE);
	Cleanup();
	// Test PreShutdown timeout - Vista+ will show the shutdown screen!
	int TimeOut = 1000 * 60 * 1;
	while (TimeOut > 0) {
		Sleep(1000);
		TimeOut += 1000;
		ReportStatus();
  	};
}
//---------------------------------------------------------------------------
/* This event is triggered when an unhandled exception occured after the
   service has been started. */
void __fastcall TDDService1::DDServiceRunException(TObject *Sender, Exception *E,
          bool &LogDefaultErrMsg, bool &CanAbort)
{
// Service will be aborted.
	CanAbort = true;
	// Set a service specific error code to enable service recovery actions
	ErrCode = 1;
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServiceSessionChange(TDDService *Sender, int EventType,
          int SessionID)
{
	LogMessage("SessionChange - EventType = " + IntToStr(EventType) +
			 " SessionID = " + IntToStr(SessionID), EVENTLOG_INFORMATION_TYPE);
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServiceShutdown(TDDService *Sender)
{
	LogMessage("Shutdown", EVENTLOG_INFORMATION_TYPE);
	Cleanup();
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServiceStart(TDDService *Sender, bool &Started)
{
	TDevBroadcastDeviceInterface dbi;
	LogMessage("Starting", EVENTLOG_INFORMATION_TYPE);
	// We may register additional device notifications, specify the
	// service _window as recipient. W2K+

	if ((Win32Platform == VER_PLATFORM_WIN32_NT) && (Win32MajorVersion >= 5))
	{
		ZeroMemory(&dbi, sizeof(dbi));
		dbi.dbcc_size = sizeof(dbi);
		dbi.dbcc_devicetype = DBT_DEVTYP_DEVICEINTERFACE;
		dbi.dbcc_classguid = GUID_DEVINTERFACE_USB;
		DeviceHandle = RegisterDeviceNotification(Sender->ServiceWindow, &dbi,
											   DEVICE_NOTIFY_WINDOW_HANDLE);
		if (!DeviceHandle)
			RaiseLastOSError();
	};

	// TTimer is not thread safe, do not use it in service threads in real life.
	Timer1 = new TTimer(NULL);
	Timer1->OnTimer = Timer1Timer;

	LogMessage("Started", EVENTLOG_INFORMATION_TYPE);

	// Test our custom message
	PostMessage(ServiceWindow, WM_CUSTOM_1, 0, 0);
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::DDServiceStop(TDDService *Sender, bool &Stopped)
{
	LogMessage("Stopped", EVENTLOG_INFORMATION_TYPE);
	Cleanup();
}
//---------------------------------------------------------------------------

void __fastcall TDDService1::Timer1Timer(TObject *Sender)
{
	Sysutils::Beep; // Does no longer work in Vista+ :(
}
//---------------------------------------------------------------------------
/* We may want to override service thread's WndProc.
   Note: Some Message-IDs in the range WM_USER + are used by TDDService!
   So define your custom messages beginning at WM_USER_DDSERVICE. */
void __fastcall TDDService1::WndProc(Messages::TMessage &MsgRec)
{
	if (MsgRec.Msg == WM_CUSTOM_1) {
		LogMessage("WM_CUSTOM_1 received", EVENTLOG_INFORMATION_TYPE);
	}
	else
		TDDService::WndProc(MsgRec);
}

//---------------------------------------------------------------------------
