{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtMessageBox                                   }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTMessageBox;

interface
uses
    Classes
   ,Messages
  ;
type
{------------------------------------------------------------------------------}
//The Type of the dialog
 TgtMessageBoxType = (
                       mbtOk
                      ,mbtYesNo
                      ,mbtOkCancel
                      ,mbtAbortRetryIgnore
                      ,mbtYesNoCancel
                      ,mbtRetryCancel
                     );
{------------------------------------------------------------------------------}
{
The modality level of the dialog
mmlApplication,mmlTask = Modality level is confined with in the application
mmlSystem = Modality level is System wide meaning that the dialog will stay on top of all other
                   open windows.
}
 TgtMessageModalityLevel = (
                             mmlApplication
                            ,mmlSystem
                            ,mmlTask
                           );
{------------------------------------------------------------------------------}
//The icon of the dialog
 TgtMessageBoxIcon = (
                       mbiNone
                      ,mbiInformation
                      ,mbiQuestion
                      ,mbiWarning
                      ,mbiError
                      );
{------------------------------------------------------------------------------}
//Which button will have focus when the dialog is created
  TgtMessageDefButton = (
                             mdbButton1
                            ,mdbButton2
                            ,mdbButton3
                            ,mdbButton4
                            );
{------------------------------------------------------------------------------}
//The result of the dialog
  TgtMessageExecResult = (
                           merNone
                          ,merOk
                          ,merCancel
                          ,merAbort
                          ,merRetry
                          ,merIgnore
                          ,merYes
                          ,merNo
                          ,merClose
                          ,merTryAgain
                          ,merContinue
                         );
{------------------------------------------------------------------------------}
//This event will run after the execution of the dialog parsing the result
  TgtAfterExecuteEvent = procedure (Sender : TObject ; ExecutionResult : TgtMessageExecResult) of Object;
{------------------------------------------------------------------------------}
  TgtMessageBox = class(TComponent)
  private
    FMessageBoxIcon      : TgtMessageBoxIcon;
    FMessageCaption      : string;
    FMessageText         : string;
    FAfterExecute        : TgtAfterExecuteEvent;
    FMessageDefButton    : TgtMessageDefButton;
    FMessageBoxType      : TgtMessageBoxType;
    FMessageModalityLevel: TgtMessageModalityLevel;
    FOnHelpButtonClick: TNotifyEvent;
    FShowHelpButton: Boolean;
    FExecResult: TgtMessageExecResult;
  {Private Declarations}
  protected
  {Protected Declarations}
    FHandle : LongWord;
    procedure WndProc(var Message: TMessage);
  public
  {Public Declarations}
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  public
    procedure Execute;
  published
  {Published Declarations}
    property MessageBoxType       : TgtMessageBoxType        read FMessageBoxType       write FMessageBoxType;
    property MessageModalityLevel : TgtMessageModalityLevel  read FMessageModalityLevel write FMessageModalityLevel;
    property MessageBoxIcon       : TgtMessageBoxIcon        read FMessageBoxIcon       write FMessageBoxIcon   default mbiNone;
    property MessageDefButton     : TgtMessageDefButton      read FMessageDefButton     write FMessageDefButton default mdbButton1;
    property MessageText          : string                   read FMessageText          write FMessageText;
    property MessageCaption       : string                   read FMessageCaption       write FMessageCaption;
    property ShowHelpButton       : Boolean                  read FShowHelpButton       write FShowHelpButton;
    property ExecResult           : TgtMessageExecResult     read FExecResult;
  published
    property AfterExecute         : TgtAfterExecuteEvent     read FAfterExecute         write FAfterExecute;
    property OnHelpButtonClick    : TNotifyEvent             read FOnHelpButtonClick    write FOnHelpButtonClick;
  end;
{------------------------------------------------------------------------------}

implementation
uses
   Windows
  ,SysUtils
  ;

{ TgtMessageBox }
{------------------------------------------------------------------------------}
constructor TgtMessageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{------------------------------------------------------------------------------}
destructor TgtMessageBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtMessageBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_HELP :
      begin
       //Capturing the Help Button Click
        if Assigned(FOnHelpButtonClick) then
          FOnHelpButtonClick(Self);
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtMessageBox.Execute;
var
  ExecResult : TgtMessageExecResult;
  MsgIcon    : DWORD;
  MsgDefBtn  : DWORD;
  MsgType    : DWORD;
  MsgModLevel: DWORD;
begin
  MsgIcon     :=0;
  MsgDefBtn   :=0;
  MsgType     :=0;
  MsgModLevel :=0;
  case MessageBoxIcon of
    mbiNone        :;
    mbiInformation : MsgIcon := Windows.MB_ICONINFORMATION;
    mbiQuestion    : MsgIcon := Windows.MB_ICONQUESTION;
    mbiWarning     : MsgIcon := Windows.MB_ICONWARNING;
    mbiError       : MsgIcon := Windows.MB_ICONERROR;
  end;
  case MessageDefButton of
    mdbButton1 : MsgDefBtn := Windows.MB_DEFBUTTON1;
    mdbButton2 : MsgDefBtn := Windows.MB_DEFBUTTON2;
    mdbButton3 : MsgDefBtn := Windows.MB_DEFBUTTON3;
    mdbButton4 : MsgDefBtn := Windows.MB_DEFBUTTON4;
  end;
  case MessageBoxType of
    mbtOk                : MsgType := Windows.MB_OK;
    mbtYesNo             : MsgType := Windows.MB_YESNO;
    mbtOkCancel          : MsgType := Windows.MB_OKCANCEL;
    mbtAbortRetryIgnore  : MsgType := Windows.MB_ABORTRETRYIGNORE;
    mbtYesNoCancel       : MsgType := Windows.MB_YESNOCANCEL;
    mbtRetryCancel       : MsgType := Windows.MB_RETRYCANCEL;
  end;
  case MessageModalityLevel of
    mmlApplication : MsgModLevel := Windows.MB_APPLMODAL;
    mmlSystem      : MsgModLevel := Windows.MB_SYSTEMMODAL;
    mmlTask        : MsgModLevel := Windows.MB_TASKMODAL;
  end;
  if ShowHelpButton then
  try
    FHandle := Classes.AllocateHWnd(WndProc);
    ExecResult := TgtMessageExecResult(MessageBox(FHandle,PChar(MessageText),PChar(MessageCaption)
                                      ,MsgIcon+MsgDefBtn+MsgType+MsgModLevel+MB_HELP))
  finally
      Classes.DeallocateHWnd(FHandle);
  end
  else
    ExecResult := TgtMessageExecResult(MessageBox(HWND(nil),PChar(MessageText),PChar(MessageCaption)
                                      ,MsgIcon+MsgDefBtn+MsgType+MsgModLevel));
  if Assigned(FAfterExecute) then
    FAfterExecute(Self,ExecResult);
end;
{------------------------------------------------------------------------------}

end.

