unit Dialog_UGS;

interface

uses
  Forms, SysUtils, Classes, ExtCtrls, Dialogs;
  //TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  //TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
 type
   TMessOBJ = class (TObject)
   DLG : TForm;
   Timer:TTimer;
   procedure MyMessage(Captext : String; MsgText : String; ShowTime: Cardinal;
       MsgType : TMsgDlgType ; Buttons : TMsgDlgButtons;
       FontName : string; FontSize : Integer);
   procedure MyTimerHandler(Sender : TObject);
 end;

type
  TUGS_Dialog = class(TComponent)
  private
    { Private declarations }
    MessOBJ : TMessOBJ;
  protected
    { Protected declarations }
  public
    { Public declarations }
   procedure DisplayMessage(Captext : String; MsgText : String; ShowTime: Cardinal
       = 1000; MsgType : TMsgDlgType = mtInformation; Buttons : TMsgDlgButtons =
       []; FontName : string = 'Arial'; FontSize : Integer = 8);
    constructor Create;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('UGS Soft', [TUGS_Dialog]);
end;

{ TUGS_Dialog }

procedure TUGS_Dialog.DisplayMessage(Captext : String; MsgText : String;
    ShowTime: Cardinal = 1000; MsgType : TMsgDlgType = mtInformation; Buttons :
    TMsgDlgButtons = []; FontName : string = 'Arial'; FontSize : Integer = 8);
begin
  if not Assigned(MessOBJ)then MessOBJ := TMessOBJ.Create;
  MessOBJ.MyMessage(Captext, MsgText, ShowTime, MsgType, Buttons, FontName, FontSize);
end;

constructor TUGS_Dialog.Create;
begin
  MessOBJ := nil;
end;

destructor TUGS_Dialog.Destroy;
begin
  inherited;
  if Assigned(MessOBJ)then FreeAndNil(MessOBJ);
end;

{ TMesaj }

procedure TMessOBJ.MyMessage(Captext : String; MsgText : String; ShowTime:
    Cardinal; MsgType : TMsgDlgType; Buttons : TMsgDlgButtons;
    FontName : string; FontSize : Integer);
begin
  DLG   := CreateMessageDialog(MsgText, MsgType, Buttons);
  if Buttons = [] then begin
    Timer := TTimer.Create(DLG);
    Timer.Enabled := True;
    Timer.Interval := ShowTime;
    Timer.OnTimer  := MyTimerHandler;
  end else Timer := nil;

  with DLG do begin
    if Trim(Captext)=''then Captext:= Application.Title;
    Caption   := Captext;
    Font.Name := FontName;
    Font.Size := FontSize;
    if Buttons = [] then Show
    else ShowModal;
  end;
end;

procedure TMessOBJ.MyTimerHandler(Sender: TObject);
begin
  DLG.Close;
  if Assigned(Timer)then Timer.Free;
end;

end.
