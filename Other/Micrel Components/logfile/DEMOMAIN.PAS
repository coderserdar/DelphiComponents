unit DemoMain;

{* Program : DemoMain.Pas
   Purpose : Demo for TLogFile

   Author            Last Changed
   ------            ------------
   Song Weng Sam     Aug. 14, 96
 *}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, LogFile, StdCtrls, Buttons, ExtCtrls, jbSupp;

type
  TfrmDemoTLogFile = class(TForm)
    LogFile1: TLogFile;
    edtLogInfo: TEdit;
    spdWriteToLogFile: TSpeedButton;
    Bevel1: TBevel;
    procedure spdWriteToLogFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDemoTLogFile: TfrmDemoTLogFile;

implementation

{$R *.DFM}

procedure TfrmDemoTLogFile.FormCreate(Sender: TObject);
begin
  LogFile1.LogFile := ChangeFileExt(Application.ExeName, '.log');
end;

procedure TfrmDemoTLogFile.spdWriteToLogFileClick(Sender: TObject);
begin
   if edtLogInfo.Text <> '' then
   begin
      LogFile1.LogToFile(edtLogInfo.Text);
      MessageDlg('Message "'+edtLogInfo.Text+'" has been logged to file.',
                  mtInformation, [mbOk], 0);
   end
   else
      MessageDlg('Please enter a message before proceeding.', mtError, [mbOk], 0);
end;

end.
