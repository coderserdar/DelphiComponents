
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Progress dialog             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_progr;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FR_Class, FR_Const;

const
  CM_BeforeModal = WM_USER + 1;

type
  TfrProgressForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDoc: TfrReport;
    FOnBeforeModal: TNotifyEvent;
{$IFDEF Delphi9}
    procedure CMBeforeModal(Sender: TObject);
{$ELSE}
    procedure CMBeforeModal(var Message: TMessage); message CM_BeforeModal;
{$ENDIF}
  public
    { Public declarations }
    FirstCaption: String;
{$IFDEF Delphi9}
    Timer: TTimer;
{$ENDIF}
    property OnBeforeModal: TNotifyEvent read FOnBeforeModal write FOnBeforeModal;
    function Show_Modal(Doc: TfrReport): Word;
  end;

var
  frProgressForm: TfrProgressForm;

implementation

{$R *.DFM}

function TfrProgressForm.Show_Modal(Doc: TfrReport): Word;
begin
  FDoc := Doc;
{$IFDEF Delphi9}
  Timer := TTimer.Create(nil);
  Timer.Interval := 200;
  Timer.OnTimer := CMBeforeModal;
  Timer.Enabled := True;
{$ELSE}
  PostMessage(Handle, CM_BeforeModal, 0, 0);
{$ENDIF}
  Result := ShowModal;
{$IFDEF Delphi9}
  Timer.Free;
{$ENDIF}
end;

procedure TfrProgressForm.Button1Click(Sender: TObject);
begin
  FDoc.Terminated := True;
  ModalResult := mrCancel;
end;

{$IFDEF Delphi9}
procedure TfrProgressForm.CMBeforeModal(Sender: TObject);
begin
  Timer.Enabled := False;
  if Assigned(FOnBeforeModal) then
    FOnBeforeModal(Self);
end;
{$ELSE}
procedure TfrProgressForm.CMBeforeModal(var Message: TMessage);
begin
  if Assigned(FOnBeforeModal) then FOnBeforeModal(Self);
end;
{$ENDIF}

procedure TfrProgressForm.FormCreate(Sender: TObject);
begin
  Button1.Caption := LoadStr(SCancel);
end;

end.

