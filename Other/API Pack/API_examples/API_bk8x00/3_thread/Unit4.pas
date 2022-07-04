unit Unit4;

//------------------------------------------------------------------------------
// threading example with the API_BK8x00 component.
// UN-SYNCHRONIZED (better to minimize windows affects to the bk8x00 functionality)
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_edit, API_bk8x00, ExtCtrls, API_grbutton, API_ledgrid,
  API_chart;

type
  TForm1 = class(TForm)
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_bk8x001: TAPI_bk8x00;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    API_ledgrid1: TAPI_ledgrid;
    Label3: TLabel;
    API_ledgrid2: TAPI_ledgrid;
    Label4: TLabel;
    Timer2: TTimer;
    Bevel1: TBevel;
    ListBox1: TListBox;
    API_chart1: TAPI_chart;
    procedure Timer1Timer(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure API_ledgrid2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer2Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure API_bk8x001Error(Sender: TObject; ErrNo: Integer;
      ErrorMsg: string);
    procedure API_bk8x001ThreadEvent(thread: TThread);
  private
  public
    mainpath: string;
    counter: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  dateutils, inifiles, syncobjs;

//------------------------------------------------------------------------------
// following part will make the io handling threadsafe, which is MUST if
// bk8x00 thread is run unsynchronized

type
  // input and output structure
  TThreadIO = record
    lock: tcriticalsection;
    input: tbuffer;
    output: tbuffer;
    errtext: string;
  end;

  TIO = record
    input: tbuffer;
    output: tbuffer;
    errtext: string;
  end;

var
  ThreadIO: TThreadIO;    // thread will use this IO structure
  IO: TIO;                // main application is allowed to use this one

// initialize thread io record's critical section on application start
procedure InitIO;
begin
  ThreadIO.lock:= tcriticalsection.create;
end;

// free thread io record's critical section on application close
procedure KillIO;
begin
  ThreadIO.lock.free;
end;

// update thread's io to the similar io structure to be
// used from the main thread for example showing the values
// threadsafe manner (or to affect outputs from the mainthread)
procedure UpdateIO;
begin
  ThreadIO.lock.Acquire;
  try
    ThreadIO.output:= io.output;
    io.input:= ThreadIO.input;
    io.errtext:= threadio.errtext;
  finally
    ThreadIO.lock.Release;
  end;
end;

procedure ClearErrTexts;
begin
  threadio.lock.acquire;
  try
    threadio.errtext:= '';
    io.errtext:= '';
  finally
    threadio.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
  i,j: integer;
  threadtime: double;
begin
  // change led state according to the device
  // serial port open or not (automatically
  // opened on thread start)
  api_grbutton2.LedState:=api_bk8x001.Open;

  // show online time on the label
  if api_bk8x001.Starttime>0 then
  begin
    threadtime:= api_bk8x001.ThreadTime;

    // online time update
    label1.caption:=
      formatfloat('0.00',secondspan(now, api_bk8x001.StartTime))+'s online; '+
      formatfloat('0.00',threadtime)+'ms cycle';

    // update thread time chart
    api_chart1.Add(secondspan(now, api_bk8x001.starttime), ThreadTime);
    api_chart1.Repaint;

    // draw input(s) states (4 first bytes)
    for i:=0 to (api_bk8x001.InputLength*2)-1 do
      if i<4 then
        for j:=0 to 7 do
          api_ledgrid1.Boolean(j,i,BitIsSet(io.input[i],j));

    // draw output(s) states (4 first bytes)
    for i:=0 to (api_bk8x001.OutputLength*2)-1 do
      if i<4 then
        for j:=0 to 7 do
          api_ledgrid2.boolean(j,i,BitIsSet(io.output[i],j));

  end else
    label1.caption:='Not online';

  // update topmost line if errtext is changed
  // and something is assigned to the errtext from
  // the updateio cycle..
  if (io.errtext<>'') and (io.errtext<>listbox1.Items[0]) then
    listbox1.items.insert(0, io.errtext);

  // update status
  label2.caption:= api_bk8x001.GetStatus;
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer2Timer(Sender: TObject);
begin
  // this timer will update threadio structure
  // to io structure that should be used in all
  // signal processing and controlling via the
  // visible items on any form for example..
  UpdateIO;

  // if you've some plans to implement this
  // inputs and outputs to a some visible
  // process handling it might be good idea
  // to do the process structure here also
  // into the main thread instead of unsychronized
  // thread event, example:
  //
  // if (io.input[0] and 1>0) then        // if bit1 is set
  //   io.output[0]:= io.output[0] + 4;   // set output bit3
  //
  //  if (bitisset(input[0], 0) then      // if bit1 is set
  //    setbit(output[0], 2);             // set output bit3
  //
  // here's one example as binary counter:
  counter:= counter + 1;
  if counter>$ff then counter:= 0;
  io.output[0]:= counter;  
end;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x001ThreadEvent(thread: TThread);
begin
  // when thread event is now in this example run unsynchronized
  // we must enter critical section to update the io structure
  // items that is used to update mainthreads information..
  threadio.lock.Acquire;
  try
    api_bk8x001.readwrite(threadio.input, threadio.output);
  finally
    threadio.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  // show component's internal setting window
  // with just the minimal needed data to setup
  // before the bk8x00 is ready to use
  api_bk8x001.showsettings;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton2click(Sender: TObject);
var
  oldstate: boolean;
begin
  oldstate:= api_bk8x001.open;

  // change bk state to running, also
  // because thread is set to automatically
  // start on port open it will be run
  // in case port is succesfully opened
  // and also stopped in case the thread
  // was running on this change.
  api_bk8x001.Open:=not oldstate;
  api_chart1.ClearData;
  clearerrtexts;

  // here we check if the port was opened
  // if the last state was false to inform
  // user if operation failed
  if (oldstate = FALSE) and (not api_bk8x001.open) then
  begin
    messagedlg('Failed to open port.', mterror, [mbok], 0);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_ledgrid2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  col, row: integer;
begin
  // left click on output grid
  if button=mbleft then
  begin
    // locate actual col and row
    col:= api_ledgrid2.GetMousePosX(x);
    row:= api_ledgrid2.GetMousePosY(y);

    // next will set above clicked bit
    // on output array
    SetBit(io.output[row*8], col, not api_ledgrid2.Boolean(col,row));
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // before form is allowed to close we should
  // make sure that the bk thread is stopped and
  // port is closed..
  if api_bk8x001.open then
  begin
    api_bk8x001.Open:= false;
    sleep(200);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  // get application's folder
  // and open bk settings if
  // the file has been written
  // on last run
  mainpath:= extractfiledir(application.exename);
  api_bk8x001.OpenSettings(mainpath+'\bk8x001.settings');

  // initialize thread io buffers
  InitIO;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  api_bk8x001.SaveSettings(mainpath+'\bk8x001.settings');

  // free thread io buffer critical section
  KillIO;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x001Error(Sender: TObject; ErrNo: Integer;
  ErrorMsg: string);
begin
  threadio.lock.Acquire;
  try
    threadio.errtext:= timetostr(now)+'> '+errormsg;
  finally
    threadio.lock.Release;
  end;
end;

//------------------------------------------------------------------------------

end.
