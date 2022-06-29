unit UFormViewFindProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ATBinHex, ATViewer, ExtCtrls,
  TntStdCtrls,
  ATStreamSearch;

type
  TFormViewFindProgress = class(TForm)
    btnCancel: TButton;
    ProgressBar1: TProgressBar;
    labSearch: TTntLabel;
    Timer1: TTimer;
    labPercent: TLabel;
    Timer2: TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FViewer: TATViewer;
    FFindText: WideString;
    FFindTextOrig: WideString;
    FFindOptions: TATStreamSearchOptions;
    FFindFirst: boolean;
    FFindPrevious: boolean;
    FCancel: boolean;
    FShowNoErrorMsg: boolean;
    procedure StartSearch;
    procedure Progress(const ACurrentPos, AMaximalPos: Int64; var AContinue: boolean);
  end;

implementation

uses
  ATxSProc, ATxMsgProc, ATxMsg, ATxUtils,
  ATViewerMsg;

{$R *.DFM}

procedure TFormViewFindProgress.Progress(const ACurrentPos, AMaximalPos: Int64; var AContinue: boolean);
var
  Percent: integer;
begin
  if AMaximalPos > 0 then
    Percent:= ACurrentPos * 100 div AMaximalPos
  else
    Percent:= 0;
  labSearch.Caption:= SFormatW(MsgViewerSearchProgress, [FFindTextOrig]);
  labPercent.Caption:= Format('%d%%', [Percent]);
  ProgressBar1.Position:= Percent;
  Application.ProcessMessages;
  AContinue:= not FCancel;
end;

procedure TFormViewFindProgress.StartSearch;
begin
  ShowModal;
end;

procedure TFormViewFindProgress.FormCreate(Sender: TObject);
begin
  //Init fields
  FViewer:= nil;
  FFindText:= '';
  FFindTextOrig:= '';
  FFindOptions:= [];
  FFindFirst:= true;
  FFindPrevious:= false;
  FCancel:= false;
  FShowNoErrorMsg:= false;
end;

procedure TFormViewFindProgress.FormShow(Sender: TObject);
var
  En: boolean;
begin
  {$I Lang.FormViewFindProgress.inc}

  FViewer.OnTextSearchProgress:= Progress;
  FCancel:= false;

  labSearch.Caption:= SFormatW(MsgViewerSearchProgress, [FFindTextOrig]);
  labPercent.Caption:= '0%';
  ProgressBar1.Position:= 0;

  En:= FViewer.Mode in [vmodeText, vmodeBinary, vmodeHex, vmodeUnicode];
  ProgressBar1.Enabled:= En;
  btnCancel.Enabled:= En;

  Timer1.Enabled:= true;
  Timer2.Enabled:= true;
  AlphaBlendValue:= 0;
end;

procedure TFormViewFindProgress.btnCancelClick(Sender: TObject);
begin
  FCancel:= true;
end;

procedure TFormViewFindProgress.Timer1Timer(Sender: TObject);
var
  AResultOK,
  ASearchOK: boolean;
begin
  Timer1.Enabled:= false;

  ASearchOK:= true;

  try
    if FFindFirst then
      AResultOK:= FViewer.FindFirst(FFindText, FFindOptions)
    else
      AResultOK:= FViewer.FindNext(FFindPrevious);
  except
    on E: Exception do
      begin
      AResultOK:= false;
      ASearchOK:= false;
      if E.Message<>'' then
        MsgError(E.Message, Handle)
      else
        MsgError(MsgString(0124), Handle);
      end;
  end;

  Hide;

  if not ASearchOK then
    begin
    //Nothing currently
    end
  else
  if not AResultOK then
    begin
    if FShowNoErrorMsg then
      MessageBeep(MB_ICONEXCLAMATION)
    else
      MsgWarning(SFormatW(MsgViewerErrCannotFindText, [FFindTextOrig]), Handle);
    end;

  ModalResult:= mrOk;
end;


procedure TFormViewFindProgress.Timer2Timer(Sender: TObject);
var
  N: Integer;
begin
  if AlphaBlendValue < 5 then N := 1 else N := 50;
  AlphaBlendValue := AlphaBlendValue + N;
  if AlphaBlendValue >= 250 then
  begin
    Timer2.Enabled := false;
    AlphaBlendValue := 255;
  end;  
end;

end.
