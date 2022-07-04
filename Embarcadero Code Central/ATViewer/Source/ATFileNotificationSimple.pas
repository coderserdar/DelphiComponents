{************************************************}
{                                                }
{  ATFileNotificationSimple Component            }
{  Copyright (C) 2007 Alexey Torgashin           }
{  http://atorg.net.ru                           }
{  support@uvviewsoft.com                        }
{                                                }
{************************************************}

{
ATFileNotificationSimple is a simple version of ATFileNotification,
the advantages are:

- It doesn't create any threads, it uses simple timer instead
- It doesn't use FindFirstChangeNotification API

Disadvantages are:

- It monitors file change only, not directory change
- It doesn't react to file change immediately (but with a timer delay)
- It polls disk repeatedly, may be slow on remote disks

Example of usage:

  procedure TFormMain.NotifyFile;
  begin
    with ATFileNotificationSimple1 do
    begin
      Timer.Enabled := False;
      Timer.Interval := StrToIntDef(edDelay.Text, Timer.Interval);
      FileName := edFileName.Text;
      Timer.Enabled := True;
    end;
  end;
}

{$BOOLEVAL OFF} //Short boolean evaluation.

unit ATFileNotificationSimple;

interface

uses
  Windows, SysUtils, Classes, ExtCtrls;

type
  TATFileSimpleRec = record
    FExist: Boolean;
    FSizeLow,
    FSizeHigh: DWORD;
    FTimeWr: TFileTime;
  end;

type
  TATFileNotificationSimple = class(TComponent)
  private
    { Private declarations }
    FFileName: WideString;
    FFileRec: TATFileSimpleRec;
    FTimer: TTimer;
    FTimerBusy: Boolean;
    FOnChanged: TNotifyEvent;
    procedure SetFileName(const AValue: WideString);
    procedure TimerTimer(Sender: TObject);
    procedure DoChanged;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property FileName: WideString read FFileName write SetFileName;
    property Timer: TTimer read FTimer;
  published
    { Published declarations }
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

procedure Register;


implementation

{ Helper functions }

procedure FGetFileRec(const FileName: WideString; var Rec: TATFileSimpleRec);
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
begin
  FillChar(Rec, SizeOf(Rec), 0);
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    h := FindFirstFileW(PWideChar(FileName), fdW);
    Rec.FExist := h <> INVALID_HANDLE_VALUE;
    if Rec.FExist then
    begin
      Rec.FSizeLow := fdW.nFileSizeLow;
      Rec.FSizeHigh := fdW.nFileSizeHigh;
      //Rec.FAttr := fdW.dwFileAttributes;
      Rec.FTimeWr := fdW.ftLastWriteTime;
      //Rec.FTimeCr := fdW.ftCreationTime;
      //Rec.FTimeAcc := fdW.ftLastAccessTime;
      Windows.FindClose(h);
    end;
  end
  else
  begin
    h := FindFirstFileA(PAnsiChar(AnsiString(FileName)), fdA);
    Rec.FExist := h <> INVALID_HANDLE_VALUE;
    if Rec.FExist then
    begin
      Rec.FSizeLow := fdA.nFileSizeLow;
      Rec.FSizeHigh := fdA.nFileSizeHigh;
      //Rec.FAttr := fdA.dwFileAttributes;
      Rec.FTimeWr := fdA.ftLastWriteTime;
      //Rec.FTimeCr := fdA.ftCreationTime;
      //Rec.FTimeAcc := fdA.ftLastAccessTime;
      Windows.FindClose(h);
    end;
  end;
end;

function FFileChanged(const FileName: WideString; var OldRec: TATFileSimpleRec): Boolean;
var
  NewRec: TATFileSimpleRec;
begin
  FGetFileRec(FileName, NewRec);

  Result :=
    ( OldRec.FExist <> NewRec.FExist ) or
    ( OldRec.FSizeLow <> NewRec.FSizeLow ) or 
    ( OldRec.FSizeHigh <> NewRec.FSizeHigh) or
    ( OldRec.FTimeWr.dwLowDateTime <> NewRec.FTimeWr.dwLowDateTime ) or 
    ( OldRec.FTimeWr.dwHighDateTime <> NewRec.FTimeWr.dwHighDateTime );

  if Result then
    Move(NewRec, OldRec, SizeOf(NewRec));
end;


{ TATFileNotificationSimple }

constructor TATFileNotificationSimple.Create(AOwner: TComponent);
begin
  inherited;
  FFileName := '';
  FillChar(FFileRec, SizeOf(FFileRec), 0);
  FTimer := TTimer.Create(Self);
  with FTimer do
  begin
    Enabled := False;
    Interval := 1000;
    OnTimer := TimerTimer;
  end;
  FTimerBusy := False;
end;

procedure TATFileNotificationSimple.SetFileName(const AValue: WideString);
var
  En: Boolean;
begin
  En := FTimer.Enabled;
  FTimer.Enabled := False;

  FFileName := AValue;
  FGetFileRec(FFileName, FFileRec);
  if not FFileRec.FExist then
    raise Exception.Create('File to watch doesn''t exist');

  FTimer.Enabled := En;
end;

procedure TATFileNotificationSimple.TimerTimer(Sender: TObject);
begin
  if not FTimerBusy then
    try
      FTimerBusy := True;
      if FFileChanged(FFileName, FFileRec) then
        DoChanged;
    finally
      FTimerBusy := False;
    end;
end;

procedure TATFileNotificationSimple.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;


{ Registration }

procedure Register;
begin
  RegisterComponents('Samples', [TATFileNotificationSimple]);
end;

end.
