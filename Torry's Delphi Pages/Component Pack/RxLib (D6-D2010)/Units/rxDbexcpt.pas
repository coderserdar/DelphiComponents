{*******************************************************}
{                                                       }
{       Delphi VCL Extensions (RX)                      }
{                                                       }
{       TRxBdeErrorDlg based on sample form             }
{       DELPHI\DEMOS\DB\TOOLS\DBEXCEPT.PAS              }
{       Portions copyright (c) 1995, 1996 AO ROSNO      }
{       Portions copyright (c) 1997, 1998 Master-Bank   }
{                                                       }
{*******************************************************}

unit rxDBExcpt;

{$I RX.INC}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, {$IFDEF RX_D3} DBTables, {$ENDIF} RXCtrls;

type
  TDBErrorEvent = procedure (Error: TDBError; var Msg: string) of object;

  TRxBdeErrorDlg = class(TForm)
    BasicPanel: TPanel;
    ErrorText: TLabel;
    IconPanel: TPanel;
    IconImage: TImage;
    TopPanel: TPanel;
    RightPanel: TPanel;
    DetailsPanel: TPanel;
    DbMessageText: TMemo;
    DbResult: TEdit;
    DbCatSub: TEdit;
    NativeResult: TEdit;
    Back: TButton;
    Next: TButton;
    ButtonPanel: TPanel;
    DetailsBtn: TButton;
    OKBtn: TButton;
    BDELabel: TRxLabel;
    NativeLabel: TRxLabel;
    BottomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
  private
    CurItem: Integer;
    Details: Boolean;
    DetailsHeight: Integer;
    DbException: EDbEngineError;
    FPrevOnException: TExceptionEvent;
    FOnErrorMsg: TDBErrorEvent;
    procedure GetErrorMsg(Error: TDBError; var Msg: string);
    procedure ShowError;
    procedure SetShowDetails(Value: Boolean);
  public
    procedure ShowException(Sender: TObject; E: Exception);
    property OnErrorMsg: TDBErrorEvent read FOnErrorMsg write FOnErrorMsg;
  end;

const
  DbErrorHelpCtx: THelpContext = 0;

var
  DbEngineErrorDlg: TRxBdeErrorDlg;

procedure DbErrorIntercept;

implementation

uses
  Windows, BDE, Consts,
  RxDConst, RxCConst, rxVCLUtils;

{$R *.DFM}

procedure DbErrorIntercept;
begin
  if DbEngineErrorDlg <> nil then DbEngineErrorDlg.Free;
  DbEngineErrorDlg := TRxBdeErrorDlg.Create(Application);
end;

{ TRxBdeErrorDlg }

procedure TRxBdeErrorDlg.ShowException(Sender: TObject; E: Exception);
begin
  Screen.Cursor := crDefault;
  Application.NormalizeTopMosts;
  try
    if (E is EDbEngineError) and (DbException = nil)
      and not Application.Terminated then
    begin
      DbException := EDbEngineError(E);
      try
        ShowModal;
      finally
        DbException := nil;
      end;
    end
    else begin
      if Assigned(FPrevOnException) then FPrevOnException(Sender, E)
      else if NewStyleControls then Application.ShowException(E)
      else MessageDlg(E.Message + '.', mtError, [mbOk], 0);
    end;
  except
    { ignore any exceptions }
  end;
  Application.RestoreTopMosts;
end;

procedure TRxBdeErrorDlg.ShowError;
var
  BDEError: TDbError;
  S: string;
  I: Integer;
begin
  Back.Enabled := CurItem > 0;
  Next.Enabled := CurItem < DbException.ErrorCount - 1;
  BDEError := DbException.Errors[CurItem];
  { Fill BDE error information }
  BDELabel.Enabled := True;
  DbResult.Text := IntToStr(BDEError.ErrorCode);
  DbCatSub.Text := Format('[$%s] [$%s]', [IntToHex(BDEError.Category, 2),
    IntToHex(BDEError.SubCode,  2)]);
  { Fill native error information }
  NativeLabel.Enabled := BDEError.NativeError <> 0;
  if NativeLabel.Enabled then
    NativeResult.Text := IntToStr(BDEError.NativeError)
  else NativeResult.Clear;
  { The message text is common to both BDE and native errors }
  S := Trim(BDEError.Message);
  for I := 1 to Length(S) do
    if S[I] < ' ' then S[I] := ' ';
  {GetErrorMsg(BDEError, S);}
  DbMessageText.Text := Trim(S);
end;

procedure TRxBdeErrorDlg.SetShowDetails(Value: Boolean);
begin
  DisableAlign;
  try
    if Value then begin
      DetailsPanel.Height := DetailsHeight;
      ClientHeight := DetailsPanel.Height + BasicPanel.Height;
      DetailsBtn.Caption := '<< &' + LoadStr(SDetails);
      CurItem := 0;
      ShowError;
    end
    else begin
      ClientHeight := BasicPanel.Height;
      DetailsPanel.Height := 0;
      DetailsBtn.Caption := '&' + LoadStr(SDetails) + ' >>';
    end;
    DetailsPanel.Enabled := Value;
    Details := Value;
  finally
    EnableAlign;
  end;
end;

procedure TRxBdeErrorDlg.GetErrorMsg(Error: TDBError; var Msg: string);
begin
  if Assigned(FOnErrorMsg) then
  try
    FOnErrorMsg(Error, Msg);
  except
  end;
end;

procedure TRxBdeErrorDlg.FormCreate(Sender: TObject);
begin
  DetailsHeight := DetailsPanel.Height;
  Icon.Handle := LoadIcon(0, IDI_EXCLAMATION);
  IconImage.Picture.Icon := Icon;
  { Load string resources }
  Caption := LoadStr(SDBExceptCaption);
  BDELabel.Caption := LoadStr(SBDEErrorLabel);
  NativeLabel.Caption := LoadStr(SServerErrorLabel);
  Next.Caption := LoadStr(SNextButton) + ' >';
  Back.Caption := '< ' + LoadStr(SPrevButton);
  OKBtn.Caption := ResStr(SOKButton);
  { Set exception handler }
  FPrevOnException := Application.OnException;
  Application.OnException := ShowException;
end;

procedure TRxBdeErrorDlg.FormDestroy(Sender: TObject);
begin
  Application.OnException := FPrevOnException;
end;

procedure TRxBdeErrorDlg.FormShow(Sender: TObject);
var
  S: string;
  ErrNo: Integer;
begin
  if DbException.HelpContext <> 0 then
    HelpContext := DbException.HelpContext
  else HelpContext := DbErrorHelpCtx;
  CurItem := 0;
  if (DbException.ErrorCount > 1) and
    (DbException.Errors[1].NativeError <> 0) and
    ((DbException.Errors[0].ErrorCode = DBIERR_UNKNOWNSQL) or
    { General SQL error }
    (DbException.Errors[0].ErrorCode = DBIERR_INVALIDUSRPASS)) then
    { Unknown username or password }
    ErrNo := 1
  else ErrNo := 0;
  S := Trim(DbException.Errors[ErrNo].Message);
  GetErrorMsg(DbException.Errors[ErrNo], S);
  ErrorText.Caption := S;
  SetShowDetails(False);
  DetailsBtn.Enabled := DbException.ErrorCount > 0;
end;

procedure TRxBdeErrorDlg.DetailsBtnClick(Sender: TObject);
begin
  SetShowDetails(not Details);
end;

procedure TRxBdeErrorDlg.BackClick(Sender: TObject);
begin
  Dec(CurItem);
  ShowError;
end;

procedure TRxBdeErrorDlg.NextClick(Sender: TObject);
begin
  Inc(CurItem);
  ShowError;
end;

initialization
  DbEngineErrorDlg := nil;
end.
