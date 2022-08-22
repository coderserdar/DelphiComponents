unit DemoUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, BMDThread;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ProgressBar1: TProgressBar;
    ButtonStopAll: TButton;
    BMDThread1: TBMDThread;
    BMDThread2: TBMDThread;
    BMDThreadGroup1: TBMDThreadGroup;
    procedure BMDThread1Execute(Sender: TObject; Thread: TBMDExecuteThread;
      var Data: Pointer);
    procedure BMDThread1Start(Sender: TObject; Thread: TBMDExecuteThread;
      var Data: Pointer);
    procedure BMDThread1Terminate(Sender: TObject;
      Thread: TBMDExecuteThread; var Data: Pointer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonStopAllClick(Sender: TObject);
    procedure BMDThreadGroup1Start(Sender: TObject;
      Thread: TBMDExecuteThread; var Data: Pointer);
    procedure BMDThreadGroup1Terminate(Sender: TObject;
      Thread: TBMDExecuteThread; var Data: Pointer);
    procedure BMDThreadGroup1Update(Sender: TObject;
      Thread: TBMDExecuteThread; var Data: Pointer; Percent: Integer);
    procedure BMDThread2Execute(Sender: TObject; Thread: TBMDExecuteThread;
      var Data: Pointer);
    procedure BMDThread2Start(Sender: TObject; Thread: TBMDExecuteThread;
      var Data: Pointer);
    procedure BMDThread2Terminate(Sender: TObject;
      Thread: TBMDExecuteThread; var Data: Pointer);
    procedure SynchroEvent2 (Sender: TBMDThread; Thread: TBMDExecuteThread; var Data: Pointer);
    procedure SynchroEvent1 ( Sender: TObject );
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

  public
    { Public declarations }
    Value1 : Integer;
    Value2 : Integer;

  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BMDThread1Execute(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer);
var
  i : Integer;
  
begin
  Value1 := 0;
  for i := 0 to 1000 - 1 do
    begin
    Thread.PercentProgress := i div 10;
    SleepEx ( 20, false );
    if ( Thread.Terminated ) then
      break;

    Inc ( Value1 );
    Thread.Synchronize ( SynchroEvent1 );
    end;
end;

procedure TForm1.SynchroEvent1( Sender: TObject );
begin
  Label2.Caption := IntToStr ( Value1 );
end;
//---------------------------------------------------------------------------
procedure TForm1.SynchroEvent2(Sender: TBMDThread; Thread: TBMDExecuteThread; var Data: Pointer);
begin
  TLabel( Data ).Caption := IntToStr ( Value2 );
end;
//---------------------------------------------------------------------------

procedure TForm1.BMDThread1Start(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer);
begin
  Label1.Caption := 'Start';
  Button1.Enabled := false;
  Button2.Enabled := true;

  Data := Label2;
end;

procedure TForm1.BMDThread1Terminate(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer);
begin
  Label1.Caption := 'Stop';
  Button1.Enabled := true;
  Button2.Enabled := false;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BMDThread1.Start ();
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BMDThread1.Stop ();
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  BMDThread2.Start ();
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  BMDThread2.Stop ();
end;

procedure TForm1.ButtonStopAllClick(Sender: TObject);
begin
  BMDThreadGroup1.Stop();
end;

procedure TForm1.BMDThreadGroup1Start(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer);
begin
  ProgressBar1.Position := 0;
  Label5.Visible := true;
  ProgressBar1.Visible := true;
  ButtonStopAll.Enabled := true;
end;

procedure TForm1.BMDThreadGroup1Terminate(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer);
begin
  Label5.Visible := false;
  ProgressBar1.Visible := false;
  ProgressBar1.Position := 0;
  ButtonStopAll.Enabled := false;
end;

procedure TForm1.BMDThreadGroup1Update(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer; Percent: Integer);
begin
  if ( Sender = BMDThread1 ) then
    Label5.Caption := 'Thread 1 :'

  else
    Label5.Caption := 'Thread 2 :';

  ProgressBar1.Position := Percent;
end;

procedure TForm1.BMDThread2Execute(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer);
var
  i : Integer;
  
begin
  Value2 := 0;
  for i := 0 to 1000 - 1 do
    begin
    Thread.PercentProgress := i div 10;
    SleepEx ( 20, false );
    if ( Thread.Terminated ) then
      break;

    Inc ( Value2 );
    Thread.Synchronize ( SynchroEvent2, Data );
    end;
end;

procedure TForm1.BMDThread2Start(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer);
begin
  Label3.Caption := 'Start';
  Button3.Enabled := false;
  Button4.Enabled := true;

  Data := Label4;
end;

procedure TForm1.BMDThread2Terminate(Sender: TObject;
  Thread: TBMDExecuteThread; var Data: Pointer);
begin
  Label3.Caption := 'Stop';
  Button3.Enabled := true;
  Button4.Enabled := false;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BMDThread1.OnTerminate := NIL;
  BMDThread2.OnTerminate := NIL;
  BMDThreadGroup1.Stop();
end;

end.
