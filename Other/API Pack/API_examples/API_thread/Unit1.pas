unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_thread, ExtCtrls, API_timer, API_progressbar,
  API_base;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Timer1: TTimer;
    API_thread1: TAPI_thread;
    API_thread2: TAPI_thread;
    Bevel1: TBevel;
    Label5: TLabel;
    Label6: TLabel;
    API_thread3: TAPI_thread;
    API_progressbar1: TAPI_progressbar;
    API_thread4: TAPI_thread;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure API_thread1Execute(aMain: TAPI_thread;
      aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
    procedure API_thread2Execute(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread4Execute(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread3Execute(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
  private
    { Private declarations }
    counter4: integer;
    procedure DrawProgressbar;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  dateutils;

var
  counter1: integer;
  counter2: integer;
  counter3: integer;

//------------------------------------------------------------------------------
// form's original procedure that is called from the thread 3
// the thread is assigned to execute this procedure instead of
// event (running until terminated)
procedure TForm1.DrawProgressbar;
begin
  with api_progressbar1 do
  begin
    Position:= Position + 0.05;
    if Position=Max then
      Position:= 0;
  end;
  counter3:= counter3 + 1;
end;

//------------------------------------------------------------------------------
// start threads, second one is run only once
procedure TForm1.API_thread1Execute(aMain: TAPI_thread;
  aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
begin
  counter1:= counter1 + 1;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_thread2Execute(aMain: TAPI_thread;
  aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
begin
  amain.Lock;
  counter2:= counter2 + 1;
  amain.Release;
end;

procedure TForm1.API_thread3Execute(aMain: TAPI_thread;
  aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
begin
  // this event doesn't need to be done at all
  // unless you need to, thread3 was started so
  // that we assigned form's method to be executed
  // in the thread3 execute procedure..
end;

//------------------------------------------------------------------------------
procedure TForm1.API_thread4Execute(aMain: TAPI_thread;
  aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
begin
  tform1(data^).counter4:= tform1(data^).counter4 + 1;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
begin
  api_thread1.Start(false, nil);                    // run thread with default options
  api_thread2.Start(false, nil);
  api_thread3.start(form1.drawprogressbar, false);  // assign thread to form's method
  api_thread4.Start(false, @form1);                 // run thread, assign form pointer
end;

//------------------------------------------------------------------------------
// stop / terminate both threads
procedure TForm1.Button2Click(Sender: TObject);
begin
  api_thread1.stop;
  api_thread2.Stop;
  api_thread3.stop;
  api_thread4.stop;
end;

//------------------------------------------------------------------------------
// show active flag information
procedure TForm1.Timer1Timer(Sender: TObject);

  function BoolState(b: boolean): string;
  begin
    if b then result:= 'Running'
      else result:= 'Stopped';
  end;

var
  i1, i2, i3, i4: integer;
begin
  // all below variables can be accessed directly
  // because they were increase trough some sort of
  // synchronization (except counter4)
  i1:= counter1;
  i3:= counter3;
  i4:= counter4;

  // to access variables used in the unsynchronized thread
  // we need always to use threads lock (or some other
  // similar that you've created for this) to make it 100%
  // thread safe
  api_thread2.Lock;
  try
    i2:= counter2;
  finally
    api_thread2.Release;
  end;

  // now when the counter values are all in the temporary
  // variables we may do whatever we want to them without
  // caring about the threadsafe issues..
  label1.Caption:= inttostr(i1);
  label2.caption:= inttostr(i2);
  label7.Caption:= inttostr(i4);
  label5.Caption:= inttostr(i2-i1);
  label8.Caption:= inttostr(i2-i4);
  api_progressbar1.Caption:= inttostr(i3);

  // another function in the thread component is
  // IsActive which will return true if thread is running
  // this is also 100% threadsafe
  label3.caption:= BoolState(api_thread1.IsActive);
  label4.caption:= BoolState(api_thread2.IsActive);
  label10.caption:= BoolState(api_thread3.IsActive);
  
end;

end.
