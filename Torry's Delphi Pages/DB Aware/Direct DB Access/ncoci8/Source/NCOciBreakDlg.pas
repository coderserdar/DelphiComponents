{*******************************************************}
{File:      NCOciBreakDlg.pas                           }
{Revision:  0.02.01 / 16.12.2001                        }
{Comment:   NC OCI8 VCL: Oracle8 break query dialog     }
{Copyright: (c) 1999-2001, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciBreakDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, NCOci,
  NCOciDB, StdCtrls, Buttons, ExtCtrls;

type
  TNCOciBreakFrm = class(TForm)
    BitBtn1: TBitBtn;
    Timer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    FDatabase: TOCICustomDatabase;
    FRequestShow: Boolean;
    FWindowList : Pointer;
    procedure DoRequest(AShow: Boolean; ADatabase: TOCICustomDatabase);
  public
    { Public declarations }
    class procedure Execute(ADatabase: TOCICustomDatabase);
    class procedure Cancel(ADatabase: TOCICustomDatabase);
    class procedure ProcessMessages;
  end;

var
  NCOciBreakFrm: TNCOciBreakFrm;

implementation

{$R *.DFM}

class procedure TNCOciBreakFrm.Execute(ADatabase: TOCICustomDatabase);
begin
    if NCOciBreakFrm = nil then
        NCOciBreakFrm := TNCOciBreakFrm.Create(nil);
    NCOciBreakFrm.DoRequest(True, ADatabase);
end;

class procedure TNCOciBreakFrm.Cancel(ADatabase: TOCICustomDatabase);
begin
    if NCOciBreakFrm <> nil then
        NCOciBreakFrm.DoRequest(False, ADatabase);
end;

class procedure TNCOciBreakFrm.ProcessMessages;
var
    Msg: TMsg;
    
    function GetMouseMessageForm(const Msg: TMsg): TCustomForm;
    var
        ctrl: TControl;
    begin
        ctrl := FindControl(Msg.hwnd);
        if Ctrl <> nil then
            Result := GetParentForm(ctrl)
        else
            Result := nil;
    end;

begin
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do begin
        if not (
            (Msg.message >= WM_KEYDOWN) and (Msg.message <= WM_DEADCHAR) and
                (Screen.ActiveForm <> NCOciBreakFrm) or
            (Msg.message >= WM_MOUSEFIRST) and (Msg.message <= WM_MOUSELAST) and
                (GetMouseMessageForm(Msg) <> NCOciBreakFrm) or
            (Msg.message >= WM_SYSKEYDOWN) and (Msg.message <= WM_SYSDEADCHAR)
           ) then begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
        end;
    end;
end;

type
    __TOCICustomDatabase = class(TOCICustomDatabase)
    end;

procedure TNCOciBreakFrm.DoRequest(AShow: Boolean; ADatabase: TOCICustomDatabase);
begin
    FDatabase := ADatabase;
    if (FDatabase = nil) or (csDestroying in FDatabase.ComponentState) then begin
        if FWindowList <> nil then begin
          EnableTaskWindows(FWindowList);
          FWindowList := nil;
        end;
        Free;
        NCOciBreakFrm := nil;
    end
    else if (AShow <> FRequestShow) and (FDatabase <> nil) and
            (__TOCICustomDatabase(FDatabase).Connected or not AShow) then begin
        FRequestShow := AShow;
        Timer.Interval := WDelayBeforeSQLCrs;
        Timer.Enabled := True;
    end;
end;

procedure TNCOciBreakFrm.TimerTimer(Sender: TObject);
begin
    if (FRequestShow <> Visible)
     and not (csDestroying in ComponentState)then begin
        if FRequestShow then begin
            if GetCapture <> 0 then
                SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
            FWindowList := DisableTaskWindows(0);
            Show;
        end
        else begin
            if FWindowList <> nil then begin
              EnableTaskWindows(FWindowList);
              FWindowList := nil;
            end;
            Hide;
        end;
        Timer.Enabled := False;
        TNCOciBreakFrm.ProcessMessages;
    end;
end;

procedure TNCOciBreakFrm.Button1Click(Sender: TObject);
begin
    FDatabase.Break;
end;

procedure TNCOciBreakFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key = VK_ESCAPE then begin
        Button1Click(nil);
        Key := 0;
    end;
end;

initialization
    NCOciBreakFrm := nil;

finalization
    NCOciBreakFrm.Free;
    NCOciBreakFrm := nil;
    
end.
