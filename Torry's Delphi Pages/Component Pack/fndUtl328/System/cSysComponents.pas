{$INCLUDE ..\cDefines.inc}
unit cSysComponents;

interface

uses
  { Delphi }
  Classes,
  Graphics,
  StdCtrls,

  { Fundamentals }
  cLog;



{                                                                              }
{ TfndMemoLog                                                                  }
{                                                                              }
type
  TfndMemoLogGetLogColorEvent = procedure (Sender: TObject; LogClass: TLogClass;
      LogMsg: String; var Color: TColor) of object;
  TfndMemoLog = class(TLog)
  protected
    FLogToMemo     : TCustomMemo;
    FMaxMemoLines  : Integer;
    FOnGetLogColor : TfndMemoLogGetLogColorEvent;

    procedure Init; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    procedure TriggerLogMsg(const Sender: TObject; const LogClass: TLogClass;
              const LogMsg: String); override;

  published
    property  OnLog;
    property  OnEditMessage;
    property  OnLogFile;
    property  OnGetLogColor: TfndMemoLogGetLogColorEvent read FOnGetLogColor write FOnGetLogColor;
    property  LogFileName;
    property  LogOptions;
    property  LogTo;
    property  LogToMemo: TCustomMemo read FLogToMemo write FLogToMemo;
    property  MaxMemoLines: Integer read FMaxMemoLines write FMaxMemoLines default 1024;
  end;



{                                                                              }
{ Component Register                                                           }
{                                                                              }
procedure Register;



implementation

uses
  { Delphi }
  Messages,
  ComCtrls,

  { Fundamentals }
  cUtils,
  cWindows,
  cThreads;



{                                                                              }
{ TfndMemoLog                                                                  }
{                                                                              }
procedure TfndMemoLog.Init;
begin
  inherited Init;
  FMaxMemoLines := 1024;
end;

procedure TfndMemoLog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FLogToMemo then
      FLogToMemo := nil;
end;

procedure TfndMemoLog.TriggerLogMsg(const Sender: TObject;
    const LogClass: TLogClass; const LogMsg: String);
var Col : TColor;
    R : TCustomRichEdit;
    L : Integer;
begin
  inherited TriggerLogMsg(Sender, LogClass, LogMsg);
  if Assigned(FLogToMemo) then
    try
      // Log to memo
      if FLogToMemo is TCustomRichEdit then
        begin
          Col := clBlack;
          if Assigned(FOnGetLogColor) then
            FOnGetLogColor(Sender, LogClass, LogMsg, Col);
          R := TCustomRichEdit(FLogToMemo);
          L := Length(R.Text);
          R.SelStart := L;
          R.SelAttributes.Color := Col;
          R.SelText := iif(L > 0, #13#10, '') + LogMsg;
        end else
        begin
          L := Length(FLogToMemo.Text);
          FLogToMemo.SelStart := L;
          FLogToMemo.SelText := iif(L > 0, #13#10, '') + LogMsg;
        end;
      // Delete lines
      if FMaxMemoLines > 0 then
        While FLogToMemo.Lines.Count > FMaxMemoLines do
          FLogToMemo.Lines.Delete(0);
      // Scroll to bottom
      FLogToMemo.Perform(EM_LineScroll, 0, FLogToMemo.Lines.Count - 1);
    except
      if not (loIgnoreLogFailure in FLogOptions) then
        raise;
    end;
end;



{                                                                              }
{ Component Register                                                           }
{                                                                              }
procedure Register;
begin
  RegisterComponents('Fundamentals', [TfndWindowHandle, TfndTimerHandle,
      TfndLog, TfndMemoLog, TfndThread]);
end;



end.

