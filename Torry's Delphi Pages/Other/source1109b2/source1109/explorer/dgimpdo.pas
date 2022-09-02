{$I fsdefine.inc}

unit dgimpdo;

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
  Buttons,
  ExtCtrls,
  fsclimex,
  fsllbase,
  uentity;

type
  TdlgImportProgress = class(TForm)
    lblProgress: TLabel;
    btnCancel: TBitBtn;
    lblFrom: TLabel;
    lblTo: TLabel;
    edtImportFilename: TEdit;
    edtTablename: TEdit;
    mtrProgress: TProgressBar;
    procedure btnCancelClick(Sender: TObject);
  private
  public
    FEngine: TffImportEngine;
    procedure ShowProgress(aImportFilename, aTableName: string);
    procedure UpdateProgress(aProgress: TffieProgressPacket);
  end;

function DoImport(aIE: TffImportEngine;
                  aImportFilename: TFilename;
                  aTableName: TfsTableName;
                  aTable: TffexpTable;
                  aBlockInserts: SmallInt;
                  aRangeError: boolean): Boolean;

var
  dlgImportProgress: TdlgImportProgress;

implementation

{$R *.DFM}

function DoImport(aIE: TffImportEngine;
                  aImportFilename: TFilename;
                  aTableName: TfsTableName;
                  aTable: TffexpTable;
                  aBlockInserts: SmallInt;
                  aRangeError: boolean): Boolean;
begin
  with TdlgImportProgress.Create(nil) do
    try                                                            {start !!.01}
      FEngine := aIE;
      ShowProgress(aImportFilename, aTableName);
      try
        FEngine.OnYield := UpdateProgress;
        FEngine.Import(aTable, aBlockInserts,aRangeError);
      finally
        Hide;
      end;
      Application.ProcessMessages;
      Result := not FEngine.Terminated;
    finally
      Free;
    end;                                                             {end !!.01}
end;

procedure TdlgImportProgress.ShowProgress(aImportFilename, aTableName: string);
begin
  edtImportFilename.Text := aImportFilename;
  edtTablename.Text := aTableName;
  lblProgress.Hide;
  mtrProgress.Position := 0;
  inherited Show;
  Application.ProcessMessages;
end;

procedure TdlgImportProgress.UpdateProgress(aProgress: TffieProgressPacket);
var
  Dividend: LongInt;
  Divisor: LongInt;
begin
  with aProgress do begin
    with lblProgress do begin
      Caption := Format('Processing record %d of %d', [ppNumRecs, ppTotalRecs]);
      Show;
    end;

    { Calculate % completed }
    if (ppNumRecs >= $1000000) then begin
      Dividend := (ppNumRecs shr 7) * 100;
      Divisor := ppTotalRecs shr 7;
    end
    else begin
      Dividend := ppNumRecs * 100;
      Divisor := ppTotalRecs;
    end;

    if Divisor <> 0 then
      mtrProgress.Position := Dividend div Divisor;

    if IsIconic(Application.Handle) then
      Application.Title := Format('Importing %d%% complete', [mtrProgress.Position]);
  end;
end;

procedure TdlgImportProgress.btnCancelClick(Sender: TObject);
begin
  if MessageDlg('Abort importing data?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    FEngine.Terminate;
end;

end.
