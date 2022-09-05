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
// $Log:  32144: itfdxe.dpr 
//
//    Rev 1.0    2014-02-03 00:30:14  mcm    Version: IMG 4.0
program itfdxe;

{$Include 'mcmDefines.pas'}

uses
  {$IFNDEF GE_DXE2}
  Windows, SysUtils, Registry, Forms,
  {$ELSE}
  WinApi.Windows, System.SysUtils, System.Win.Registry, Vcl.Forms,
  {$ENDIF}
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
  uFormNew in 'uFormNew.pas' {FormNewImage};

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
       {$IFNDEF GE_DXE2}
       @BroadcastMsgPtr := GetProcAddress(GetModuleHandle(Windows.User32), APIName[IsInWin95]);
       {$ELSE}
       @BroadcastMsgPtr := GetProcAddress(GetModuleHandle(WinApi.Windows.User32), APIName[IsInWin95]);
       {$ENDIF}
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
