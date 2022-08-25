//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("CaptureObjectHighlight.pas", Captureobjecthighlight, CaptureObjectHighlightForm);
USEFORMNS("CaptureTheRect.pas", Capturetherect, CaptureRectForm);
USEFORMNS("promptdialog.pas", Promptdialog, frmPosition);
USEFORMNS("CaptureFreehand.pas", Capturefreehand, CaptureFreehandForm);
USEFORMNS("CaptureTheDesktop.pas", Capturethedesktop, CaptureDesktopForm);
USEFORMNS("CaptureSpecificRect.pas", Capturespecificrect, CaptureSpecificRectForm);
USEFORMNS("CaptureTheObject.pas", Capturetheobject, CaptureObjectForm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
	return 1;
}
//---------------------------------------------------------------------------
