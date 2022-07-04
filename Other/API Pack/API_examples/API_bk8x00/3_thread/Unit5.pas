unit Unit5;

//------------------------------------------------------------------------------
// threading example with the API_BK8x00 component.
// UN-SYNCHRONIZED (better to minimize windows affects to the bk8x00 functionality)
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_edit, API_bk8x00, ExtCtrls, API_grbutton, API_ledgrid,
  API_chart, API_progressbar, API_gradient, API_listbox;

type
  TForm1 = class(TForm)
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_bk8x001: TAPI_bk8x00;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Timer2: TTimer;
    Bevel1: TBevel;
    API_edit1: TAPI_edit;
    API_edit2: TAPI_edit;
    API_edit3: TAPI_edit;
    API_edit4: TAPI_edit;
    API_progressbar1: TAPI_progressbar;
    API_progressbar2: TAPI_progressbar;
    API_progressbar3: TAPI_progressbar;
    API_progressbar4: TAPI_progressbar;
    API_ledgrid1: TAPI_ledgrid;
    API_ledgrid2: TAPI_ledgrid;
    API_ledgrid3: TAPI_ledgrid;
    API_progressbar5: TAPI_progressbar;
    API_progressbar6: TAPI_progressbar;
    API_progressbar7: TAPI_progressbar;
    API_progressbar8: TAPI_progressbar;
    API_ledgrid4: TAPI_ledgrid;
    API_ledgrid5: TAPI_ledgrid;
    API_ledgrid6: TAPI_ledgrid;
    API_edit5: TAPI_edit;
    API_edit6: TAPI_edit;
    API_edit7: TAPI_edit;
    API_edit8: TAPI_edit;
    Label5: TLabel;
    Label6: TLabel;
    API_edit9: TAPI_edit;
    API_edit10: TAPI_edit;
    API_edit11: TAPI_edit;
    API_edit12: TAPI_edit;
    API_edit13: TAPI_edit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    API_listbox1: TAPI_listbox;
    Label7: TLabel;
    API_gradient1: TAPI_gradient;
    API_gradient2: TAPI_gradient;
    Label8: TLabel;
    Label3: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
const
  MAXCOUPLERS = 24;           // amount of couplers..

type
  TThreadIO = record
    lock: tcriticalsection;
    input: tbuffer;
    output: tbuffer;
  end;
  TIO = record
    input: tbuffer;
    output: tbuffer;
  end;

  TSmoothingDevice = record
    Active: boolean;          // active or not..
    StartTime: tdatetime;     // as timestamp
    TargetTime: double;       // as seconds
  end;
  TSmoothingDevices = record
    SomeActive: integer;
    DelayStart: tdatetime;    // off-delay start time
    DelayTime: double;        // off-delay target
    Dev: array[0..3] of TSmoothingDevice;
  end;

  TPolishingDevice = record
    Active: boolean;          // active or not
    StartTime: tdatetime;     // as timestamp
    TargetTime: double;       // as seconds
  end;
  TPolishingDevices = record
    SomeActive: integer;
    DelayStart: tdatetime;    // off-delay start time
    DelayTime: double;        // off-delay target
    Dev: array[0..3] of TPolishingDevice;
  end;

var
  IO, OldIO: array[0..MAXCOUPLERS-1] of TIo;
  ThreadIO: array[0..MAXCOUPLERS-1] of TThreadIo;
  Smooth: array[0..MAXCOUPLERS-1] of TSmoothingDevices;
  Polish: array[0..MAXCOUPLERS-1] of TPolishingDevices;

//------------------------------------------------------------------------------
// following part will make the io handling threadsafe, which is MUST if
// bk8x00 thread is run unsynchronized

// initialize thread io record's critical section on application start
procedure InitIO(index: integer);
begin
  ThreadIO[index].lock:= tcriticalsection.create;
end;

// free thread io record's critical section on application close
procedure KillIO(index: integer);
begin
  ThreadIO[index].lock.free;
end;

// update thread's io to the similar io structure to be
// used from the main thread for example showing the values
// threadsafe manner (or to affect outputs from the mainthread)
procedure UpdateIO(index: integer);
begin
  ThreadIO[index].lock.Acquire;
  try
    OldIO[index].output:= io[index].output;
    ThreadIO[index].output:= io[index].output;
    oldio[index].input:= io[index].input;
    io[index].input:= ThreadIO[index].input;
  finally
    ThreadIO[index].lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
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

  end else
    label1.caption:='Not online';

  // update status
  label2.caption:= api_bk8x001.GetStatus;
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer2Timer(Sender: TObject);

const
  SMOOTHINGDEVICE = 0;
  POLISHINGDEVICE = 1;
  SMOOTHIOADDR = 0;
  POLISHIOADDR = 1;

  // check that old key state was up and new one is
  // down (active down)
  function CheckKeyDown( coupler, devtype: byte; bit: integer ): boolean;
  begin
    if devtype = SMOOTHINGDEVICE then result:= ((not bitisset(oldio[coupler].input[SMOOTHIOADDR], bit) and (bitisset(io[coupler].input[SMOOTHIOADDR], bit ))))
      else result:= ((not bitisset(oldio[coupler].input[POLISHIOADDR], bit) and (bitisset(io[coupler].input[POLISHIOADDR], bit ))));
  end;

  // draw progress of one device
  procedure drawprogress( devtype: byte; dev: integer; act: boolean; start, target: tdatetime );
  var
    res: double;
  begin
    if act then
    begin
      if (now<>start) and (target<>0) then
        res:= 100* (secondspan(now, start) / target)
        else res:= 0;
      if devtype=SMOOTHINGDEVICE then
        case dev of
          0: api_progressbar1.Position:= res;
          1: api_progressbar2.position:= res;
          2: api_progressbar3.position:= res;
          3: api_progressbar4.position:= res;
        end else
        case dev of
          0: api_progressbar5.Position:= res;
          1: api_progressbar6.position:= res;
          2: api_progressbar7.position:= res;
          3: api_progressbar8.position:= res;
        end;
    end else
    begin
      if devtype=SMOOTHINGDEVICE then
        case dev of
          0: api_progressbar1.Position:= 0;
          1: api_progressbar2.position:= 0;
          2: api_progressbar3.position:= 0;
          3: api_progressbar4.position:= 0;
        end else
        case dev of
          0: api_progressbar5.Position:= 0;
          1: api_progressbar6.position:= 0;
          2: api_progressbar7.position:= 0;
          3: api_progressbar8.position:= 0;
        end;
    end;
  end;

  // read setting time for the device
  // that is currently written into the edit
  function gettime_smooth( dev: integer ): double;
  begin
    case dev of
      0: result:= api_edit1.asFloat;
      1: result:= api_edit2.asfloat;
      2: result:= api_edit3.asfloat;
      3: result:= api_edit4.asfloat;
      else result:= 0;
    end;
  end;

  function gettime_polish( dev: integer ): double;
  begin
    case dev of
      0: result:= api_edit5.asFloat;
      1: result:= api_edit6.asfloat;
      2: result:= api_edit7.asfloat;
      3: result:= api_edit8.asfloat;
      else result:= 0;
    end;
  end;

  // off-delay timers
  function getdelay_smooth: double;
  begin
    result:= api_edit13.asFloat;
  end;

  function getdelay_polish( dev: integer ): double;
  begin
    case dev of
      0: result:= api_edit9.asFloat;
      1: result:= api_edit10.asfloat;
      2: result:= api_edit11.asfloat;
      3: result:= api_edit12.asfloat;
      else result:= 0;
    end;
  end;

var
  coupler, viscoupler, i: integer;
begin
  // get visible coupler num
  viscoupler:= api_listbox1.ItemIndex;

  // show visible things (selected coupler)
  if viscoupler>-1 then
  begin

    // go trough 4 devices (2x8 input module, 2x8 output module)
    for i:=0 to 7 do
    begin
      // smooth
      api_ledgrid1.Boolean(0,i,bitisset(io[viscoupler].input[SMOOTHIOADDR],i));    // smooth inputs
      api_ledgrid3.Boolean(0,i,bitisset(io[viscoupler].output[SMOOTHIOADDR],i));   // smooth outputs
      // polish
      api_ledgrid4.Boolean(0,i,bitisset(io[viscoupler].input[POLISHIOADDR],i));    // polish inputs
      api_ledgrid6.Boolean(0,i,bitisset(io[viscoupler].output[POLISHIOADDR],i));   // polish outputs
    end;

    // draw items
    for i:=0 to 3 do
    begin
      // smooth
      drawprogress(SMOOTHINGDEVICE, i, smooth[viscoupler].dev[i].active, smooth[viscoupler].dev[i].StartTime, smooth[viscoupler].Dev[i].TargetTime);
      api_ledgrid2.Boolean(0,i,smooth[viscoupler].dev[i].Active);
      // polish
      drawprogress(POLISHINGDEVICE, i, polish[viscoupler].dev[i].active, polish[viscoupler].dev[i].StartTime, polish[viscoupler].Dev[i].TargetTime);
      api_ledgrid5.Boolean(0,i,polish[viscoupler].dev[i].Active);
    end;

  end; // ..end of visible coupler

  // go trough all couplers
  for coupler:=0 to MAXCOUPLERS-1 do
  begin

    // this timer will update threadio structure
    // to io structure that should be used in all
    // signal processing and controlling via the
    // visible items on any form for example..
    UpdateIO(coupler);

    // COMMON OUTPUT
    //===============
    // DO0.7  any smooth / polish operating
    setbit(io[coupler].output[0], 7, (smooth[coupler].SomeActive>-1) or (polish[coupler].SomeActive>-1));

    // SMOOTHING DEVICE(S)
    //=====================
    //
    // I/O List for smoothing device:
    //
    // DI0.0  Input push button 1
    // DI0.1  Input push button 2
    // DI0.2  Input push button 3
    // DI0.3  Input push button 4
    // DI0.4  (reserved)
    // DI0.5  (reserved)
    // DI0.6  Motor Direction A
    // DI0.7  Motor Direction B
    //
    // DO0.0  Valve Control for Flush
    // DO0.1  Device 1 Active
    // DO0.2  Device 2 Active
    // DO0.3  Device 3 Active
    // DO0.4  Device 4 Active
    // DO0.5  (reserved)
    // DO0.6  Air Cylinder
    // DO0.7  (reserved)  << for common

    // check if any of the devices is active and draw progress :)
    smooth[coupler].SomeActive:= -1;
    for i:=0 to 3 do
    begin
      if smooth[coupler].Dev[i].Active then smooth[coupler].SomeActive:= i;
      setbit(io[coupler].output[SMOOTHIOADDR], i+1, smooth[coupler].dev[i].Active);
    end;

    // washer after the cycle is finished for couple of seconds
    // (defined on the form)
    setbit(io[coupler].output[SMOOTHIOADDR], 0, (smooth[coupler].SomeActive>-1)
      or ((smooth[coupler].DelayStart>0) and (secondspan(smooth[coupler].DelayStart, now)<smooth[coupler].DelayTime)));

    // any device active
    if smooth[coupler].SomeActive>-1 then
    begin

      // shut down timer
      if (secondspan(smooth[coupler].Dev[smooth[coupler].SomeActive].Starttime, now)> smooth[coupler].dev[smooth[coupler].SomeActive].TargetTime)
        or (checkkeydown(coupler, SMOOTHINGDEVICE, smooth[coupler].someactive)) then
      begin
        smooth[coupler].Dev[smooth[coupler].SomeActive].Active:= false;
        smooth[coupler].DelayStart:= now;
        setbit(io[coupler].output[SMOOTHIOADDR], 6, FALSE);   // air cylinder valve   false
      end else

      // air valve control
      if (secondspan(now, smooth[coupler].dev[smooth[coupler].someactive].StartTime)>5) then
        setbit(io[coupler].output[SMOOTHIOADDR], 6, (not bitisset(io[coupler].input[SMOOTHIOADDR], 6)) and (not bitisset(io[coupler].input[SMOOTHIOADDR], 7)) );

    end else

    // none is active
    begin
      // check for new device activation
      for i:=0 to 3 do
        if (checkkeydown(coupler,SMOOTHINGDEVICE,i)) then
        begin
          smooth[coupler].dev[i].Active:= true;
          smooth[coupler].dev[i].StartTime:= now;
          smooth[coupler].dev[i].targettime:= gettime_smooth(i);
          smooth[coupler].DelayTime:= getdelay_smooth;
          setbit(io[coupler].output[SMOOTHIOADDR], 0, TRUE);     // smoothing valve to true
          setbit(io[coupler].output[SMOOTHIOADDR], 6, FALSE);   // air cylinder valve to false
        end;
    end;

    // POLISHING DEVICE(S)
    //=====================
    //
    // I/O List for smoothing device:
    //
    // DI1.0  Input push button 1
    // DI1.1  Input push button 2
    // DI1.2  Input push button 3
    // DI1.3  Input push button 4
    // DI1.4  (reserved)
    // DI1.5  (reserved)
    // DI1.6  Motor Direction A
    // DI1.7  Motor Direction B
    //
    // DO1.0  Polishing valve
    // DO1.1  Device 1 Active
    // DO1.2  Device 2 Active
    // DO1.3  Device 3 Active
    // DO1.4  Device 4 Active
    // DO1.5  (reserved)
    // DO1.6  Air cylinder output
    // DO1.7  (reserved)

    // check if any of the devices is active and draw progress :)
    polish[coupler].SomeActive:= -1;
    for i:=0 to 3 do
    begin
      if polish[coupler].Dev[i].Active then polish[coupler].SomeActive:= i;
      setbit(io[coupler].output[POLISHIOADDR], i+1, polish[coupler].dev[i].Active);
    end;

    // any device active
    if polish[coupler].SomeActive>-1 then
    begin

      // shutdown timer
      if (secondspan(polish[coupler].Dev[polish[coupler].SomeActive].Starttime, now)> polish[coupler].dev[polish[coupler].SomeActive].TargetTime)
        or (checkkeydown(coupler, POLISHINGDEVICE, polish[coupler].someactive)) then
      begin
        polish[coupler].Dev[ polish[coupler].SomeActive ].Active:= false;
        setbit(io[coupler].output[POLISHIOADDR], 0, FALSE);
        setbit(io[coupler].output[POLISHIOADDR], 6, FALSE);     // force air valve off
      end else

      // normal run
      begin
        // automatic polish valve shutdown
        if (bitisset(io[coupler].output[POLISHIOADDR], 0)) and (
          polish[coupler].dev[polish[coupler].someactive].TargetTime - secondspan(polish[coupler].dev[polish[coupler].someactive].starttime, now)
          < polish[coupler].DelayTime) then
        begin
          setbit(io[coupler].output[POLISHIOADDR], 0, FALSE);     // force polishing valve off
        end else
        // air valve control
        if (secondspan(now, polish[coupler].dev[polish[coupler].someactive].StartTime)>5) then
          setbit(io[coupler].output[POLISHIOADDR], 6, (not bitisset(io[coupler].input[POLISHIOADDR], 6)) and (not bitisset(io[coupler].input[POLISHIOADDR], 7)) );
      end;

    end else

    // none is active
    begin
      // check for new device activation
      for i:=0 to 3 do
        if (checkkeydown(coupler, POLISHINGDEVICE,i)) then
        begin
          polish[coupler].dev[i].Active:= true;
          polish[coupler].dev[i].StartTime:= now;
          polish[coupler].dev[i].targettime:= gettime_polish(i);
          polish[coupler].DelayTime:= getdelay_polish(i);          // get off delay
          setbit(io[coupler].output[POLISHIOADDR], 0, TRUE);       // polishing valve      true
          setbit(io[coupler].output[POLISHIOADDR], 6, FALSE);      // air cylinder valve   false
        end;
    end;

  end; // ..end of coupler loop

end;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x001ThreadEvent(thread: TThread);
var
  devindex: integer;
begin
  // when thread event is now in this example run unsynchronized
  // we must enter critical section to update the io structure
  // items that is used to update mainthreads information..
  devindex:= 0;     // first coupler!
  threadio[devindex].lock.Acquire;
  try
    api_bk8x001.readwrite(
      threadio[devindex].input, threadio[devindex].output
      );
  finally
    threadio[devindex].lock.Release;
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

  // here we check if the port was opened
  // if the last state was false to inform
  // user if operation failed
  if (oldstate = FALSE) and (not api_bk8x001.open) then
    messagedlg('Failed to open port', mterror, [mbok], 0);
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
var
  couplerindex: integer;
begin
  // get application's folder
  // and open bk settings if
  // the file has been written
  // on last run
  mainpath:= extractfiledir(application.exename);
  api_bk8x001.OpenSettings(mainpath+'\bk8x001.settings');

  // initialize thread io buffers
  api_listbox1.clear;
  for couplerindex:=0 to MAXCOUPLERS-1 do
  begin
    api_listbox1.items.add('Coupler (0x'+inttohex(couplerindex,2)+')');
    InitIO(couplerindex);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
var
  couplerindex: integer;
begin
  api_bk8x001.SaveSettings(mainpath+'\bk8x001.settings');

  // free thread io buffer critical section
  for couplerindex:=0 to MAXCOUPLERS-1 do
    KillIO(couplerindex);
end;

end.
