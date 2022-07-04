unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_thread, ExtCtrls, API_progressbar, API_base;

type
  TForm1 = class(TForm)
    API_progressbar1: TAPI_progressbar;
    API_progressbar2: TAPI_progressbar;
    API_progressbar3: TAPI_progressbar;
    API_progressbar4: TAPI_progressbar;
    API_thread1: TAPI_thread;
    API_thread2: TAPI_thread;
    API_thread3: TAPI_thread;
    API_thread4: TAPI_thread;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    Timer1: TTimer;
    procedure API_thread1Execute(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; id: Integer);
    procedure API_thread2Execute(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread3Execute(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread4Execute(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure API_thread1Start(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: integer);
    procedure API_thread1Stop(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread2Start(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread2Stop(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread3Start(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: integer);
    procedure API_thread3Stop(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread4Start(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure API_thread4Stop(aMain: TAPI_thread; aThread: TAPIsimplethread;
      var Data: Pointer; Id: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure API_thread1Exception(aMain: TAPI_thread;
      aThread: TAPIsimplethread; e: Exception; Id: Integer);
  private
    { Private declarations }
    procedure DrawBar1;
    procedure DrawBar2;
    procedure DrawBar3;
    procedure DrawBar4;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  // thread variables, using thread component threadsafe floating
  // point value classes. there is defined following threadsafe
  // new classes there:
  // - TSInteger
  // - TSFloat
  // - TSString
  // - TSStringlist

  thread1pos: TSFloat;
  thread2pos: TSFloat;
  thread3pos: TSFloat;
  thread4pos: TSFloat;
  threadlist: TSStringlist;

  // note! all threadsafe classes have to be created to initialize
  // the critical sections they contain, after that they all have
  // minimum of the same possibilities than original types.

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
//  thread execute events
//  =====================
//  all four threads execute events are below. when ever they execute
//  they'll run the execute event contents. note that all four threads
//  are run unsynchronized and they'r using global variables - this
//  basically would not be threadsafe unless done as i've done here:
//  each variable is used only by the corresponding thread and no-one
//  other is accessing the variables -> and when accessed, it's done
//  via threads athread.sync on the forms method.
//------------------------------------------------------------------------------
procedure TForm1.API_thread1Exception(aMain: TAPI_thread;
  aThread: TAPIsimplethread; e: Exception; Id: Integer);
begin
  // as an example of how to deal with the exception on thread event..
  // exceptions can basicly only happen on the event that user is able
  // to write the code into; anyway, if any exception on the thread
  // run is spotted - this event is fired (actually, to save space
  // same event can be used to all components as well..)
  threadlist.Add('Thread1 ('+inttostr(id)+') Exception: '+e.Message);
end;

//------------------------------------------------------------------------------
// thread 1 main events
procedure TForm1.API_thread1Execute(aMain: TAPI_thread;
  aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
begin
  // if value below 100%
  if thread1pos.value<100 then                        // if variable is below 100
  begin
    thread1pos.add( 0.02 );                           // increase value by 0.02
    athread.Sync(form1.DrawBar1);                     // synchronize mainthreads method
  end else

  // value was above!
  begin
    thread1pos.value:= 0;                             // reset counter
    threadlist.add(timetostr(now)+'> Thread1 Overrun');

    // as an option you could use the amain
    // parameter there as follows if you want:
    //amain.Stop;                                     // stop the thread processing
  end;
end;

procedure TForm1.API_thread1Start(aMain: TAPI_thread; aThread: TAPIsimplethread;
  var Data: Pointer; Id: Integer);
begin
  threadlist.Add(timetostr(now)+'> Thread1 ('+inttostr(id)+') Started');
end;

procedure TForm1.API_thread1Stop(aMain: TAPI_thread; aThread: TAPIsimplethread;
  var Data: Pointer; Id: Integer);
begin
  threadlist.add(timetostr(now)+'> Thread1 ('+inttostr(id)+') Stopped');
end;

//------------------------------------------------------------------------------
// thread 2 events
procedure TForm1.API_thread2Execute(aMain: TAPI_thread;
  aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
begin
  if thread2pos.value<100 then
  begin
    thread2pos.value:= thread2pos.value + 0.02;
    athread.Sync(form1.DrawBar2);
  end else
  begin
    thread2pos.value:= 0;
    threadlist.add(timetostr(now)+'> Thread2 Overrun');
  end;
end;

procedure TForm1.API_thread2Start(aMain: TAPI_thread; aThread: TAPIsimplethread;
  var Data: Pointer; Id: Integer);
begin
  threadlist.add(timetostr(now)+'> Thread2 Started');
end;

procedure TForm1.API_thread2Stop(aMain: TAPI_thread; aThread: TAPIsimplethread;
  var Data: Pointer; Id: Integer);
begin
  threadlist.Add(timetostr(now)+'> Thread2 Stopped');
end;

//------------------------------------------------------------------------------
// thread 3 events
procedure TForm1.API_thread3Execute(aMain: TAPI_thread;
  aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
begin
  if thread3pos.value<100 then
  begin
    thread3pos.value:= thread3pos.value + 0.02;
    athread.Sync(form1.DrawBar3);
  end else
  begin
    thread3pos.value:= 0;
    threadlist.add(timetostr(now)+'> Thread3 Overrun');
  end;
end;

procedure TForm1.API_thread3Start(aMain: TAPI_thread; aThread: TAPIsimplethread;
  var Data: Pointer; Id: Integer);
begin
  threadlist.Add(timetostr(now)+'> Thread3 Started');
end;

procedure TForm1.API_thread3Stop(aMain: TAPI_thread; aThread: TAPIsimplethread;
  var Data: Pointer; Id: Integer);
begin
  threadlist.Add(timetostr(now)+'> Thread3 Stopped');
end;

//------------------------------------------------------------------------------
// thread 4 events
procedure TForm1.API_thread4Execute(aMain: TAPI_thread;
  aThread: TAPIsimplethread; var Data: Pointer; Id: Integer);
begin
  if thread4pos.value<100 then
  begin
    thread4pos.value:= thread4pos.value + 0.02;
    athread.Sync(form1.DrawBar4);
  end else
  begin
    thread4pos.value:= 0;
    threadlist.add(timetostr(now)+'> Thread4 Overrun');
  end;
end;

procedure TForm1.API_thread4Start(aMain: TAPI_thread; aThread: TAPIsimplethread;
  var Data: Pointer; Id: Integer);
begin
  threadlist.add(timetostr(now)+'> Thread4 Started');
end;

procedure TForm1.API_thread4Stop(aMain: TAPI_thread; aThread: TAPIsimplethread;
  var Data: Pointer; Id: Integer);
begin
  threadlist.add(timetostr(now)+'> Thread4 Stopped');
end;

//------------------------------------------------------------------------------
// start all threads
// -----------------
// all threads are started on button click
//------------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
var
  b: boolean;
begin
  // get the common status flag of threads running or not..
  b:= api_thread1.IsActive or api_thread2.IsActive
    or api_thread3.IsActive or api_thread4.IsActive;

  // according to the flag, we'll do some actions on this
  // button click
  if not b then
  begin
    //threadlist.Clear;                         // message history
    api_thread1.Start(false);              // start infinite loop(s)
    api_thread2.start(false);
    api_thread3.start(false);
    api_thread4.start(false);
    button1.Caption:= 'Stop The Threads';     // change button label
  end else
  begin
    api_thread1.Stop;                         // stop all threads
    api_thread2.stop;
    api_thread3.stop;
    api_thread4.stop;
    button1.caption:= 'Start All Threads';    // change button label
  end;
end;

//------------------------------------------------------------------------------
//  progressbar drawings
//  ====================
//  each thread will synchronize to these four draw methods of the mainthread,
//  and in these methods the visible progressbar positions are updated to the
//  variables that are used otherwise only by the unsynchronized thread.
//  if there would be variables to report to mainthread other purposes they
//  can be assigned here in synched event or done by using critical sections
//  or similar. note! each thread have lock/release function implemented for
//  easy access to the critical sections.
//------------------------------------------------------------------------------
procedure TForm1.DrawBar1;
begin
  // thread 1 synchorinized contents
  api_progressbar1.Position:= thread1pos.value;
end;

procedure TForm1.DrawBar2;
begin
  // thread 2 synchorinized contents
  api_progressbar2.Position:= thread2pos.value;
end;

procedure TForm1.DrawBar3;
begin
  // thread 3 synchorinized contents
  api_progressbar3.Position:= thread3pos.value;
end;

procedure TForm1.DrawBar4;
begin
  // thread 4 synchorinized contents
  api_progressbar4.Position:= thread4pos.value;
end;

//------------------------------------------------------------------------------
//  on form create and on destroy
//  =============================
//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  // we'll create some threadsafe variables here to be
  // used troughout the app - if used like this we don't
  // have to pay too much attention on threadsafe issues
  // at least on these following variables..
  thread1pos:= TSFloat.Create;
  thread2pos:= TSFloat.Create;
  thread3pos:= TSFloat.Create;
  thread4pos:= TSFloat.Create;
  threadlist:= TSStringlist.create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // we'll have to finish the threads
  // because we have the threadsafe variables
  // created that are used inside the threads
  // that are most probably not stopped yet..
  api_thread1.Stop;
  api_thread2.stop;
  api_thread3.stop;
  api_thread4.stop;

  // free the threadsafe variables created
  // on the form create,
  // note above stop of the threads in case they were running:
  // the thread component is destroyed after these and it would
  // cause access violation unless the thread were stopped first.
  thread1pos.Free;
  thread2pos.Free;
  thread3pos.free;
  thread4pos.free;
  threadlist.free;
end;

//------------------------------------------------------------------------------
//  timer event
//  ===========
//  on timer event, we'll just draw the threadsafe list contents to the
//  listbox visible on the screen.
//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  listbox1.items.Text:= threadlist.Items.text;
  listbox1.ItemIndex:= listbox1.items.Count-1;
end;

end.
