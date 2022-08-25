//---------------------------------------------------------------------------
#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <DDSvcMgr.hpp>
#include <vcl.h>
#include <DDDbt.hpp>
#include <DDSvcUtils.hpp>
#include <guiddef.h>
#include <INITGUID.H>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TDDService1 : public TDDService
{
__published:    // IDE-managed Components
	void __fastcall DDServiceAfterInstall(TDDService *Sender);
	void __fastcall DDServiceAfterUninstall(TDDService *Sender);
	void __fastcall DDServiceConsoleEvent(TDDService *Sender, int CtrlCode, bool &Handled);
	void __fastcall DDServiceContinue(TDDService *Sender, bool &Continued);
	void __fastcall DDServiceDeviceEvent(TDDService *Sender, int EventType, TDDIntPtr EventData,
          int &MsgResult);
	void __fastcall DDServiceNetBindChange(TDDService *Sender, int EventType);
	void __fastcall DDServiceParamChange(TDDService *Sender);
	void __fastcall DDServicePause(TDDService *Sender, bool &Paused);
	void __fastcall DDServicePowerEvent(TDDService *Sender, int EventType, TDDIntPtr EventData,
          int &MsgResult);
	void __fastcall DDServicePreShutdown(TDDService *Sender);
	void __fastcall DDServiceRunException(TObject *Sender, Exception *E, bool &LogDefaultErrMsg,
          bool &CanAbort);
	void __fastcall DDServiceSessionChange(TDDService *Sender, int EventType, int SessionID);
	void __fastcall DDServiceShutdown(TDDService *Sender);
	void __fastcall DDServiceStart(TDDService *Sender, bool &Started);
	void __fastcall DDServiceStop(TDDService *Sender, bool &Stopped);	

protected:
	virtual void __fastcall WndProc(Messages::TMessage &MsgRec);
private:        // User declarations
	TTimer *Timer1;
	PVOID DeviceHandle;
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall Cleanup();
public:         // User declarations
 __fastcall TDDService1(TComponent* Owner);
 TServiceController __fastcall GetServiceController(void);

 TServiceControllerEx __fastcall GetServiceControllerEx(void);

 TServiceConsoleCtrlHandler __fastcall GetConsoleCtrlHandler(void);

 friend bool __stdcall ServiceConsoleCtrlHandler(unsigned Ctrl);

 friend void __stdcall ServiceController(unsigned CtrlCode);

 friend unsigned __stdcall ServiceControllerEx(unsigned CtrlCode,
   unsigned EventType, void * EventData, void * Context);
};
//---------------------------------------------------------------------------
extern PACKAGE TDDService1 *DDService1;
//---------------------------------------------------------------------------
#endif