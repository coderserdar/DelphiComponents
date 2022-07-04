{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtBenchMark                                    }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTInfoTrayDialog;

interface

uses
  SysUtils, Classes,Forms, ImgList, Controls, StdCtrls, ExtCtrls, Buttons;

type

  TInfoType = (itInfo,itHelp,itOk,itWarning,itError,itLock);

  TFrmInfoTrayDialog = class;

  TgtInfoTrayDialog = class(TComponent)
  private
    { Private declarations }
    FCaption       : String;
    FMessageText   : TStrings;
    FInformerForm  : TFrmInfoTrayDialog;
    FInfoType      : TInfoType;
    FStep          : Integer;
    FAutoClose     : Boolean;
    FCloseAfter    : Integer;
    FButtonCaption : String;
    FShowButton    : Boolean;
    FShowCountDown : Boolean;
    FOnButtonClick : TNotifyEvent;
    procedure SetInfoType(const Value: TInfoType);
    procedure SetCaption(const Value: String);
    procedure SetMessageText(const Value: TStrings);
    procedure SetAutoClose(const Value: Boolean);
    procedure SetCloseAfter(const Value: Integer);
    procedure SetButtonCaption(const Value: String);
    procedure SetShowButton(const Value: Boolean);
    procedure SetShowCountDown(const Value: Boolean);
  protected
    { Protected declarations }
    procedure PrepareAndShowForm;
    function GetTaskBarHeight:Integer;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    procedure   DisplayInfo;
  published
    { Published declarations }
    property InfoType      : TInfoType    read FInfoType       write SetInfoType;
    property Caption       : String       read FCaption        write SetCaption;
    property MessageText   : TStrings     read FMessageText    write SetMessageText;
    property AutoClose     : Boolean      read FAutoClose      write SetAutoClose;
    property CloseAfter    : Integer      read FCloseAfter     write SetCloseAfter;
    property ButtonCaption : String       read FButtonCaption  write SetButtonCaption;
    property ShowButton    : Boolean      read FShowButton     write SetShowButton;
    property ShowCountDown : Boolean      read FShowCountDown  write SetShowCountDown;
    property OnButtonClick : TNotifyEvent read FOnButtonClick  write FOnButtonClick;
  end;

 TFrmInfoTrayDialog = class(TForm)
    ImageList: TImageList;
    Image1: TImage;
    MessageTextMemo: TMemo;
    Bevel1: TBevel;
    BtnInformer: TBitBtn;
    CountDownTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure MessageTextMemoKeyPress(Sender: TObject; var Key: Char);
    procedure CountDownTimerTimer(Sender: TObject);
    procedure BtnInformerClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
 private
    { Private declarations }
    FCountDown : Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;
  

implementation
uses
 Windows;
{$R *.dfm}



{ TgtInfoTrayDialog }
{------------------------------------------------------------------------------}
constructor TgtInfoTrayDialog.Create(AOwner: TComponent);
begin
  inherited;
  FMessageText    := TStringList.Create;
  FButtonCaption  := 'Closing in %d';
end;
{------------------------------------------------------------------------------}
function TgtInfoTrayDialog.GetTaskBarHeight: Integer;
var
  TaskBarHandle : HWND;
  TaskRect      : TRect;
begin
  TaskBarHandle := FindWindow('Shell_TrayWnd', nil);
  GetWindowRect(TaskBarHandle,TaskRect);
  Result := TaskRect.Bottom - TaskRect.Top;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.DisplayInfo;
begin
  PrepareAndShowForm;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.PrepareAndShowForm;
var
  FinalTop : Integer;
begin
  if Assigned(FInformerForm) then
    FreeAndNil(FInformerForm);
  FStep := 30;
  FInformerForm                 := TFrmInfoTrayDialog.Create(Self);
  FInformerForm.Caption         := FCaption;
  FInformerForm.Left            := Screen.Width  - FInformerForm.Width;
  FinalTop                      := Screen.Height - FInformerForm.Height - GetTaskBarHeight;
  FInformerForm.Top             := Screen.Height;
  FInformerForm.MessageTextMemo.Lines.AddStrings(FMessageText);
  FInformerForm.Show;
  while FInformerForm.Top > FinalTop do
  begin
    FInformerForm.Top := FInformerForm.Top - FStep;
    Sleep(50);
  end;
  FInformerForm.MessageTextMemo.Visible := True;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.SetAutoClose(const Value: Boolean);
begin
  FAutoClose := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.SetButtonCaption(const Value: String);
begin
  FButtonCaption := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.SetCaption(const Value: String);
begin
  FCaption := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.SetShowButton(const Value: Boolean);
begin
  FShowButton := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.SetCloseAfter(const Value: Integer);
begin
  FCloseAfter := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.SetInfoType(const Value: TInfoType);
begin
  FInfoType := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.SetMessageText(const Value: TStrings);
begin
  FMessageText.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtInfoTrayDialog.SetShowCountDown(const Value: Boolean);
begin
  FShowCountDown := Value;
end;
{------------------------------------------------------------------------------}
{ TFrmInfoTrayDialog }
{------------------------------------------------------------------------------}
procedure TFrmInfoTrayDialog.FormCreate(Sender: TObject);
begin
  BtnInformer.Caption := TgtInfoTrayDialog(Owner).ButtonCaption;
  BtnInformer.Visible := TgtInfoTrayDialog(Owner).ShowButton;
  FCountDown := TgtInfoTrayDialog(Owner).CloseAfter;
  if FCountDown = 0 then FCountDown := 5;
  if TgtInfoTrayDialog(Owner).AutoClose then CountDownTimer.Enabled := True;
  case TgtInfoTrayDialog(Owner).InfoType of
    itInfo    : ImageList.GetIcon(0,Image1.Picture.Icon);
    itHelp    : ImageList.GetIcon(1,Image1.Picture.Icon);
    itOk      : ImageList.GetIcon(2,Image1.Picture.Icon);
    itWarning : ImageList.GetIcon(3,Image1.Picture.Icon);
    itError   : ImageList.GetIcon(4,Image1.Picture.Icon);
    itLock    : ImageList.GetIcon(5,Image1.Picture.Icon);
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmInfoTrayDialog.MessageTextMemoKeyPress(Sender: TObject;var Key: Char);
begin
  Key := #0;
end;
{------------------------------------------------------------------------------}
procedure TFrmInfoTrayDialog.CountDownTimerTimer(Sender: TObject);
begin
  Dec(FCountDown);
  if TgtInfoTrayDialog(Owner).ShowCountDown then
    BtnInformer.Caption := TgtInfoTrayDialog(Owner).ButtonCaption+Format(' (%d)',[FCountDown])
  else
    BtnInformer.Caption := TgtInfoTrayDialog(Owner).ButtonCaption;
  if FCountDown <= 0 then
    begin
      FreeAndNil(TgtInfoTrayDialog(Owner).FInformerForm);
    end;
end;
{------------------------------------------------------------------------------}
procedure TFrmInfoTrayDialog.BtnInformerClick(Sender: TObject);
begin
  if Assigned(TgtInfoTrayDialog(Owner).OnButtonClick) then
    TgtInfoTrayDialog(Owner).OnButtonClick(Sender);
end;
{------------------------------------------------------------------------------}
procedure TFrmInfoTrayDialog.FormClose(Sender: TObject;var Action: TCloseAction);
begin
  FreeAndNil(TgtInfoTrayDialog(Owner).FInformerForm);
end;
{------------------------------------------------------------------------------}

end.
