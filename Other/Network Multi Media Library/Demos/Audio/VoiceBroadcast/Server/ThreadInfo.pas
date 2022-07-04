unit ThreadInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmThreadInfo = class(TForm)
    lblClientGroupThreads: TLabel;
    lblWriteStreamThread: TLabel;
    ThreadInfoTimer: TTimer;
    lblClientHandles: TLabel;
    procedure ThreadInfoTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmThreadInfo: TfrmThreadInfo;

implementation

uses Main, NMMServerGlobals;

{$R *.dfm}

procedure TfrmThreadInfo.ThreadInfoTimerTimer(Sender: TObject);
begin
  lblClientGroupThreads.Caption:=
    'ClientGroupThreads= ' +
    IntToStr(frmMain.Server.ThreadInfo.ConnectionGroupThreads);

  lblWriteStreamThread.Caption:=
    'WriteStreamThread= ' +
    IntToStr(frmMain.Server.ThreadInfo.WriteStreamThreads);

  lblClientHandles.Caption:= 'ClientHandles= ' + IntToStr(GchInstances.Value);
end;

end.
