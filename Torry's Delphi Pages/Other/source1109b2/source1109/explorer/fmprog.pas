{$I fsdefine.inc}

unit fmprog;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  fsllbase;

type
  TfrmRebuildStatus = class(TForm)
    lblProgress: TLabel;
    mtrPercentComplete: TProgressBar;
  private
    FCursor: TCursor;
  public
    procedure Hide;
    procedure ShowProgress(aAction, aTableName: string);
    procedure UpdateProgress(aCompleted: Boolean; aStatus: TffRebuildStatus);
  end;

var
  frmRebuildStatus: TfrmRebuildStatus;

implementation

{$R *.DFM}

procedure TfrmRebuildStatus.Hide;
begin
  Screen.Cursor := FCursor;
  inherited Hide;
end;

procedure TfrmRebuildStatus.ShowProgress(aAction, aTableName: string);
begin
  Caption := Format('%s Table %s', [aAction, aTableName]);
  lblProgress.Hide;
  FCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  mtrPercentComplete.Position := 0;
  inherited Show;
end;

procedure TfrmRebuildStatus.UpdateProgress(aCompleted: Boolean; aStatus: TffRebuildStatus);
begin
  with aStatus do begin
  if rsErrorCode <> 0 then
    ShowMessage(Format('%s', [rsErrorCode]));
    with lblProgress do begin
      Caption := Format('Processing record %d of %d', [rsRecsRead, rsTotalRecs]);
      Show;
      Application.ProcessMessages;
    end;
    mtrPercentComplete.Position := aStatus.rsPercentDone;
  end;
end;

end.
