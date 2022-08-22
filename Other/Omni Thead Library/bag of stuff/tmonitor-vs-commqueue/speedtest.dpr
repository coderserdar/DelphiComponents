program speedtest;

uses
  Forms,
  speedtest1 in 'speedtest1.pas' {Form28},
  SimpleThreadedQueue in 'SimpleThreadedQueue.pas',
  SimpleThreadedQueueLL in 'SimpleThreadedQueueLL.pas',
  SimpleThreadedQueueNoWait in 'SimpleThreadedQueueNoWait.pas',
  SimpleThreadedQueueSem in 'SimpleThreadedQueueSem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm28, Form28);
  Application.Run;
end.
