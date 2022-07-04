unit SqlitePassErrorLogDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, SqlitePassDbo;

type
  TSqlitePassErrorLogDlg = class(TForm)
    PanelIndexesToolBar: TPanel;
    PanelButtons: TPanel;
    SbOk: TSpeedButton;
    SbClear: TSpeedButton;
    SbSaveToFile: TSpeedButton;
    PanelErrorLogTitle: TPanel;
    LabelErrorLogTitle: TLabel;
    Image4: TImage;
    MemoErrorLog: TMemo;
    SaveDialog: TSaveDialog;
    procedure SbClearClick(Sender: TObject);
    procedure SbOkClick(Sender: TObject);
    procedure SbSaveToFileClick(Sender: TObject);
  private
    FDatabase: TSqlitePassDatabase;
  public
   constructor Create(AOwner: TComponent; Database: TSqlitePassDatabase); reintroduce;
  end;

var
  SqlitePassErrorLogDlg: TSqlitePassErrorLogDlg;

implementation

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}

constructor TSqlitePassErrorLogDlg.Create(AOwner: TComponent;
  Database: TSqlitePassDatabase);
begin
inherited Create(AOwner);
FDatabase := Database;
MemoErrorLog.Clear;
If Not FDatabase.Options.LogErrors
   then MemoErrorLog.Text := 'No error log available. Database.Options.LogErros is set to False'
   else begin
        LabelErrorLogTitle.Caption := IntToStr(FDatabase.DatabaseError.ErrorCount) + ' Logged Error(s)';
        MemoErrorLog.Lines.Assign(FDatabase.DatabaseError.ErrorList);
        end;

end;


procedure TSqlitePassErrorLogDlg.SbClearClick(Sender: TObject);
begin
FDatabase.DatabaseError.Clear;
MemoErrorLog.Clear;
LabelErrorLogTitle.Caption := IntToStr(FDatabase.DatabaseError.ErrorCount) + ' Logged Error(s)';

end;

procedure TSqlitePassErrorLogDlg.SbOkClick(Sender: TObject);
begin
ModalResult := mrOk;
end;

procedure TSqlitePassErrorLogDlg.SbSaveToFileClick(Sender: TObject);
begin
If SaveDialog.Execute
   then FDatabase.DatabaseError.SaveToFile(SaveDialog.FileName);
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassErrorLogDialog.lrs}
 {$ENDIF}
end.
