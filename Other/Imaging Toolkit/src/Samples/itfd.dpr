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
// $Log:  17525: Itfd.dpr
//
//    Rev 1.29    2014-02-02 21:09:52  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.28    26-08-2009 22:47:58  mcm
// Delphi 2009 support
//
//    Rev 1.27    11-01-2009 18:18:14  mcm
//
//    Rev 1.26    20-08-2007 20:28:20  mcm
// Added support for Delphi 2007
//
//    Rev 1.25    18-03-2006 18:32:04  mcm    Version: IMG 2.16
//
//    Rev 1.24    04-03-2006 18:17:36  mcm    Version: IMG 2.16
// Added forms TFormNew, TFormColorSelect, TFormPaintBox and TFormCanvasSize.
//
//    Rev 1.23    22/02/2006 00:13:04  mcm
//
//    Rev 1.22    19/02/2006 11:55:20  mcm    Version: IMG 2.15
//
//    Rev 1.21    29-01-2006 20:10:02  mcm    Version: IMG 2.14
//
//    Rev 1.20    29-01-2006 12:11:34  mcm    Version: IMG 2.14
// Version 2.14
//
//    Rev 1.19    22/11/2005 20:45:48  mcm    Version: IMG 2.10
//
//   Rev 1.17    24-07-2005 19:00:30  mcm

//
//   Rev 1.16    23-05-2005 22:07:36  mcm    Version: IMG 2.9
// Added Canny, Despeckle and Marr-Hildreth filters, 
// Added Trace threshold method.

//
//   Rev 1.15    25-02-2005 19:55:52  mcm

//
//   Rev 1.14    03-01-2005 18:32:22  mcm    Version: IMG 2.7
// Added support for Delphi 2005.

//
//   Rev 1.13    20-12-2004 23:01:32  mcm

//
//   Rev 1.12    09-07-2004 20:37:42  mcm    Version: IMG 2.5

//
//   Rev 1.11    14-03-2004 09:11:36  mcm    Version: IMG 2.4

//
//   Rev 1.10    30-01-2004 20:15:14  mcm    Version: IMG 2.3

//
//   Rev 1.9    17-11-2003 10:13:04  mcm    Version: IMG 2.0
// Included file association support.

//
//   Rev 1.8    29-09-2003 18:54:50  mcm    Version: IMG 1.6
// Added support for file association.
// Added code that will allow only one instance of itfd.exe.

//
//   Rev 1.7    25-09-2003 23:28:18  mcm    Version: IMG 1.5

//
//   Rev 1.6    11-03-2003 01:03:26  mcm    Version: IMG 1.3.2
// Release 1.3.2

//
//   Rev 1.5    17-02-2003 10:34:28  mcm    Version: IMG 1.3

//
//   Rev 1.4    05-02-03 20:27:12  mcm

//
//   Rev 1.3    05-02-03 16:54:52  mcm    Version: IMG 1.3
// Release 1.3

//
//   Rev 1.2    27-09-2002 13:34:38  mcm    Version: IMG 1.2

//
//   Rev 1.1    08-08-2002 15:34:20  mcm    Version: IMG 1.1

//
//   Rev 1.0    27-05-2002 16:21:54  mcm

program Itfd;

{$Include 'mcmDefines.pas'}

uses
  Windows,
  SysUtils,
  Registry,
  Forms,
  mcmImageTypeDef,
  uFormMain in 'uFormMain.pas' {FormMain},
  uchildwin in 'uchildwin.pas' {FormChild},
  uFormPrintPreview in 'uFormPrintPreview.pas' {FormPrintPreview},
  uFormMargin in 'uFormMargin.pas' {FormPageMargin},
  uFormBrowse in 'uFormBrowse.pas' {FormBrowse},
  uFormImageInfo in 'uFormImageInfo.pas' {FormImageInfo},
  uFormCombine2RGB in 'uFormCombine2RGB.pas' {FormCombineImage},
  uFormHistogram in 'uFormHistogram.pas' {FormHistogram},
  uFormStretch in 'uFormStretch.pas' {FormResize},
  uFormFilterBrowser in 'uFormFilterBrowser.pas' {FormFilterBrowser},
  uFormFilterUser in 'uFormFilterUser.pas' {FormFilterUser},
  uFormMath in 'uFormMath.pas' {FormMath},
  uFormThreshold in 'uFormThreshold.pas' {FormThreshold},
  uFormMorph in 'uFormMorph.pas' {FormMorph},
  uFormRotate in 'uFormRotate.pas' {FormRotate},
  uFormGamma in 'uFormGamma.pas' {FormGamma},
  uFormBrightContrast in 'uFormBrightContrast.pas' {FormBrightContrast},
  uFormAffine in 'uFormAffine.pas' {FormAffine},
  uFormFileAssociate in 'uFormFileAssociate.pas' {FormFormatAssociate},
  uFormPalette in 'uFormPalette.pas' {FormPalette},
  uFormViewProfile in 'uFormViewProfile.pas' {FormViewProfile},
  uFormCanvasSize in 'uFormCanvasSize.pas' {FormCanvasSize},
  uFormPaintBox in 'uFormPaintBox.pas' {FormPaintBox},
  uFormNew in 'uFormNew.pas' {FormNewImage},
  uFormColorSelect in 'uFormColorSelect.pas' {FormColorSelect};

{$R *.RES}
{$R itfd_Manifest.RES}
{$R mcmCursors.RES}

const // Windows register message.
      RegWinMsg = 'ImagingToolkit';

type  TBroadcastSystemMsgFunc = function(Flags      : dword;
                                         Recipients : pdword;
                                         uiMsg      : uint;
                                         wParam     : WPARAM;
                                         lParam     : LPARAM) : longint; stdcall;

const APIName        : array[boolean] of PChar = ('BroadcastSystemMessageA',
                                                  'BroadcastSystemMessage');
var  hMutex          : THandle;
     Reg             : TRegistry;
     BSMRecipients   : dword;
     IsInWin95       : boolean;
     BroadcastMsgPtr : TBroadcastSystemMsgFunc;
     FileName        : string;
begin
  // This is the "normal" look of your applications entry point
  (*
  Application.Initialize;
  Application.Title := 'Imaging Toolkit for Delphi';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
  *)

  //----------------------------------------------------------------------------
  // This is the modified application entry.

  // This will only allow the application to be loaded once.
  // We will retreive the file name passed in ParamStr(1) and pass
  // this on the first instance of the application.

  // MessageID is declared as a global variable in uMainForm.pas
  MessageID := RegisterWindowMessage(RegWinMsg);
  hMutex := OpenMutex(MUTEX_ALL_ACCESS, False, RegWinMsg);
  if (hMutex = 0)
  then begin // Mutex object has not been created. This is the first instance!
       hMutex := CreateMutex(Nil, False, RegWinMsg);

       // The "normal" look your applications entry point is moved into here!
       Application.Initialize;
       Application.Title := 'Imaging Toolkit for Delphi';
       Application.HelpFile := 'mcmImaging.hlp';
       Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormColorSelect, FormColorSelect);
  Application.Run;

       // Free mutex from system memory.
       if (hMutex <> 0)
       then CloseHandle(hMutex);
  end
  else begin
       // We are running as a second instance.
       // Determin Windows OS.
       IsInWin95 := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
                    (Win32MajorVersion = 4) and
                    (Win32MinorVersion = 0);

       // Get the pointer to right BroadcastSystemMessage function.
       @BroadcastMsgPtr := GetProcAddress(GetModuleHandle(Windows.User32), APIName[IsInWin95]);

       // Dont show main form.
       Application.ShowMainForm := False;

       // Post message to first instance to focus itself.
       BSMRecipients := BSM_APPLICATIONS;

       if (ParamCount > 0)
       then begin
            // we need to store the file name somewhere!
            FileName := ParamStr(1);
            Reg := TRegistry.Create;
            Reg.RootKey := HKEY_CURRENT_USER;
            // RegKey is define in our main unit as
            // const RegKey =  '\SOFTWARE\MCM DESIGN\mcmImaging\';
            if Reg.OpenKey(RegKey, True)
            then begin
                 Reg.WriteString('Shell', FileName);
                 Reg.CloseKey;
            end;
            Reg.Free;
       end
       else FileName := '';
       BroadcastMsgPtr(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE,
                       @BSMRecipients, MessageID, 0, integer(FileName <> ''));
       Application.Terminate;
  end;

end.
