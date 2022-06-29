
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
  StdCtrls, FR_Class, FR_Const;

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
    procedure CMBeforeModal(var Message: TMessage); message CM_BeforeModal;
  public
    { Public declarations }
    FirstCaption: String;
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
  PostMessage(Handle, CM_BeforeModal, 0, 0);
  Result := ShowModal;
end;

procedure TfrProgressForm.Button1Click(Sender: TObject);
begin
  FDoc.Terminated := True;
  ModalResult := mrCancel;
end;

procedure TfrProgressForm.CMBeforeModal(var Message: TMessage);
begin
  if Assigned(FOnBeforeModal) then FOnBeforeModal(Self);
end;

procedure TfrProgressForm.FormCreate(Sender: TObject);
begin
  Button1.Caption := LoadStr(SCancel);
end;

end.

