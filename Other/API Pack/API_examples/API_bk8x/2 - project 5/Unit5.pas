unit Unit5;

//------------------------------------------------------------------------------
// threading example with the API_BK8x00 component.
// UN-SYNCHRONIZED (better to minimize windows affects to the bk8x00 functionality)
//------------------------------------------------------------------------------
//
// 06052008, ari pikivirta
//  * added emergency stop buttons on to each device group
//

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_edit, API_bk8x00, ExtCtrls, API_grbutton, API_ledgrid,
  API_chart, API_progressbar, API_gradient, API_listbox, API_bk8x, API_base;

type
  TForm1 = class(TForm)
    API_grbutton2: TAPI_grbutton;
    UpdateVisuals: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    MachineControl: TTimer;
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
    API_bk8x1: TAPI_bk8x;
    API_grbutton1: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    API_grbutton5: TAPI_grbutton;
    API_grbutton6: TAPI_grbutton;
    API_grbutton7: TAPI_grbutton;
    API_grbutton8: TAPI_grbutton;
    API_grbutton9: TAPI_grbutton;
    API_grbutton10: TAPI_grbutton;
    API_grbutton11: TAPI_grbutton;
    procedure UpdateVisualsTimer(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MachineControlTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure API_bk8x1ThreadEvent(Sender: TThread; CycleMSec: Double;
      Inputs: TBuffer; var Outputs: TBuffer);
    procedure API_bk8x1ThreadError(Sender: TThread; ErrorMsg: string);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_edit9Change(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton5MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton6MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton7MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton7MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton8MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton8MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton9MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton10MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton10MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton11MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure API_grbutton11MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
  public
    mainpath: string;
    counter: integer;
    button1a, button1b, button2a, button2b, button3a, button3b, button4a, button4b,
      button1a_old, button1b_old, button2a_old, button2b_old,
      button3a_old, button3b_old, button4a_old, button4b_old: boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  dateutils, inifiles, syncobjs;

//------------------------------------------------------------------------------
const
  MAXCOUPLERS     = 24;       // amount of couplers..
  SMOOTHINGDEVICE = 0;
  POLISHINGDEVICE = 1;
  SMOOTHIOADDR    = 0;
  POLISHIOADDR    = 1;
  GENERALINPUT    = 2;

type
  TThreadIO = record
    lock: tcriticalsection;   //
    input: tbuffer;           // input buffer
    output: tbuffer;          // outputs
    cycletime: double;        // thread cycle time
  end;

  TIO = record
    input: tbuffer;
    output: tbuffer;
    cycletime: double;
  end;

  TSmoothingDevice = record
    Active: boolean;          // active or not..
    StartTime: tdatetime;     // as timestamp
    TargetTime: double;       // as seconds
  end;

  TSmoothingDevices = record
    EMS: boolean;             // emergency stop (06052008)
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
    EMS: boolean;             // emergency stop (06052008)
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
    // store old io states
    OldIO[index].output:= io[index].output;
    oldio[index].input:= io[index].input;
    // actual io transfer
    ThreadIO[index].output:= io[index].output;
    io[index].input:= ThreadIO[index].input;
    io[index].cycletime:= threadio[index].cycletime;
  finally
    ThreadIO[index].lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.UpdateVisualsTimer(Sender: TObject);

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

var
  i, viscoupler: integer;
begin
  // device running -> ledstate
  api_grbutton2.LedState:= api_bk8x1.Open;

  // show online time on the label
  if not api_bk8x1.open then
  begin
    label1.caption:= 'Not online';
  end else
  begin
    // get visible coupler num
    viscoupler:= api_listbox1.ItemIndex;
    if viscoupler>-1 then
    begin
      label1.caption:= formatfloat('0.00',secondspan(now, api_bk8x1.StartTime))+'s online ('+
        formatfloat('0.00', io[viscoupler].cycletime)+' ms)';

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

      // draw items (4 devices)
      for i:=0 to 3 do
      begin
        // smooth
        drawprogress(SMOOTHINGDEVICE, i, smooth[viscoupler].dev[i].active, smooth[viscoupler].dev[i].StartTime, smooth[viscoupler].Dev[i].TargetTime);
        api_ledgrid2.Boolean(0,i,smooth[viscoupler].dev[i].Active);
        // show ems state
        if smooth[viscoupler].EMS then
        begin
          api_grbutton1.Color:= clred;
          api_grbutton1.GradientEnd:= clyellow;
        end else
        begin
          api_grbutton1.Color:= clbtnface;
          api_grbutton1.GradientEnd:= clwhite;
        end;
        // polish
        drawprogress(POLISHINGDEVICE, i, polish[viscoupler].dev[i].active, polish[viscoupler].dev[i].StartTime, polish[viscoupler].Dev[i].TargetTime);
        api_ledgrid5.Boolean(0,i,polish[viscoupler].dev[i].Active);
        // show ems state
        if polish[viscoupler].EMS then
        begin
          api_grbutton2.Color:= clred;
          api_grbutton2.GradientEnd:= clyellow;
        end else
        begin
          api_grbutton2.Color:= clbtnface;
          api_grbutton2.GradientEnd:= clwhite;
        end;
      end;

    end else
      label1.caption:= formatfloat('0.00',secondspan(now, api_bk8x1.StartTime))+'s online, no coupler selected';
  end;
  // ..end of visible coupler
end;

//------------------------------------------------------------------------------
procedure TForm1.MachineControlTimer(Sender: TObject);
var
  coupler, i, viscoupler: integer;

  // check that old key state was up and new one is
  // down (active down)
  function CheckKeyDown( coupler, devtype: byte; bit: integer ): boolean;
  begin
    if devtype = SMOOTHINGDEVICE then
    begin
      result:= ((not bitisset(oldio[coupler].input[SMOOTHIOADDR], bit) and (bitisset(io[coupler].input[SMOOTHIOADDR], bit ))));
      if viscoupler>-1 then
      begin
        case (bit) of
          0: begin
              if (button1a) and (not button1a_old) then result:= true;
              button1a_old:= button1a;
            end;
          1: begin
              if (button2a) and (not button2a_old) then result:= true;
              button2a_old:= button2a;
            end;
          2: begin
              if (button3a) and (not button3a_old) then result:= true;
              button3a_old:= button3a;
            end;
          3: begin
              if (button4a) and (not button4a_old) then result:= true;
              button4a_old:= button4a;
            end;
        end;
      end;
    end else
    begin
      result:= ((not bitisset(oldio[coupler].input[POLISHIOADDR], bit) and (bitisset(io[coupler].input[POLISHIOADDR], bit ))));
      if viscoupler>-1 then
        case (bit) of
          0: begin
              if (button1b) and (not button1b_old) then result:= true;
              button1b_old:= button1b;
            end;
          1: begin
              if (button2b) and (not button2b_old) then result:= true;
              button2b_old:= button2b;
            end;
          2: begin
              if (button3b) and (not button3b_old) then result:= true;
              button3b_old:= button3b;
            end;
          3: begin
              if (button4b) and (not button4b_old) then result:= true;
              button4b_old:= button4b;
            end;
        end;
    end;
  end;

  // read setting time for the device
  // that is currently written into the edit
  function gettime( devtype: byte; dev: integer ): double;
  begin
    if devtype = SMOOTHINGDEVICE then
      case dev of
        0: result:= api_edit1.asFloat;
        1: result:= api_edit2.asfloat;
        2: result:= api_edit3.asfloat;
        3: result:= api_edit4.asfloat;
        else result:= 0;
      end else
      case dev of
        0: result:= api_edit5.asFloat;
        1: result:= api_edit6.asfloat;
        2: result:= api_edit7.asfloat;
        3: result:= api_edit8.asfloat;
        else result:= 0;
      end;
  end;

  // off-delay timers
  function getdelay( devtype: byte; dev: integer ): double;
  begin
    if devtype = SMOOTHINGDEVICE then
    begin
      result:= api_edit13.asFloat;
    end else
    case dev of
      0: result:= api_edit9.asFloat;
      1: result:= api_edit10.asfloat;
      2: result:= api_edit11.asfloat;
      3: result:= api_edit12.asfloat;
      else result:= 0;
    end;
  end;

begin
  viscoupler:= api_listbox1.itemindex;

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
    // DI0.0  emergency stop - NORMALLY CLOSED
    // DO0.7  any smooth / polish operating

    //GENERALINPUT    = 2;
    if not bitisset(io[coupler].input[GENERALINPUT], 0) then
    begin
      // shut down all outputs or drop dowm
      // main contactor or something..
    end;

    setbit(io[coupler].output[0], 7, (smooth[coupler].SomeActive>-1) or (polish[coupler].SomeActive>-1));

    // SMOOTHING DEVICE(S)
    //=====================

    // check if any of the devices is active and draw progress :)
    smooth[coupler].SomeActive:= -1;
    for i:=0 to 3 do
    begin
      if smooth[coupler].EMS then smooth[coupler].dev[i].Active:= false;        // emergency -> turn inactive
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
      // emergency -> do not allow activate
      if smooth[coupler].EMS then
      begin
        // shut down all outputs
        setbit(io[coupler].output[SMOOTHIOADDR], 0, FALSE);   // smoothing valve to false
        setbit(io[coupler].output[SMOOTHIOADDR], 6, FALSE);   // air cylinder valve to false
      end else
      // check for new device activation
      for i:=0 to 3 do
        if (checkkeydown(coupler,SMOOTHINGDEVICE,i)) then
        begin
          smooth[coupler].dev[i].Active:= true;
          smooth[coupler].dev[i].StartTime:= now;
          smooth[coupler].dev[i].targettime:= gettime(SMOOTHINGDEVICE, i);
          smooth[coupler].DelayTime:= getdelay(SMOOTHINGDEVICE, 0);
          setbit(io[coupler].output[SMOOTHIOADDR], 0, TRUE);     // smoothing valve to true
          setbit(io[coupler].output[SMOOTHIOADDR], 6, FALSE);   // air cylinder valve to false
        end;
    end;

    // POLISHING DEVICE(S)
    //=====================

    // check if any of the devices is active and draw progress :)
    polish[coupler].SomeActive:= -1;
    for i:=0 to 3 do
    begin
      if polish[coupler].EMS then polish[coupler].dev[i].Active:= false; // emergency -> turn inactive
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
      // emergency -> do not allow activate
      if polish[coupler].EMS then
      begin
        // shut down all outputs
        setbit(io[coupler].output[POLISHIOADDR], 0, FALSE);
        setbit(io[coupler].output[POLISHIOADDR], 6, FALSE);     // force air valve off
      end else
      // check for new device activation
      for i:=0 to 3 do
        if (checkkeydown(coupler, POLISHINGDEVICE,i)) then
        begin
          polish[coupler].dev[i].Active:= true;
          polish[coupler].dev[i].StartTime:= now;
          polish[coupler].dev[i].targettime:= gettime(POLISHINGDEVICE, i);
          polish[coupler].DelayTime:= getdelay(POLISHINGDEVICE, i);  // get off delay
          setbit(io[coupler].output[POLISHIOADDR], 0, TRUE);          // polishing valve      true
          setbit(io[coupler].output[POLISHIOADDR], 6, FALSE);         // air cylinder valve   false
        end;
    end;

  end; // ..end of coupler loop
end;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x1ThreadError(Sender: TThread; ErrorMsg: string);
begin
  label2.Caption:= errormsg;    // show error message
  api_bk8x1.Open:= false;       // stop on error
end;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x1ThreadEvent(Sender: TThread; CycleMSec: Double;
  Inputs: TBuffer; var Outputs: TBuffer);
var
  devindex: integer;
begin
  devindex:= 0;
  threadio[devindex].lock.Acquire;
  try
    threadio[devindex].input:= inputs;
    outputs:= threadio[devindex].output;
    threadio[devindex].cycletime:= cycleMsec;
  finally
    threadio[devindex].lock.Release;
  end;
end;

procedure TForm1.API_edit9Change(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton1Click(Sender: TObject);
var
  viscoupler: integer;
begin
  // change EMS state of current visible device
  viscoupler:= api_listbox1.ItemIndex;
  if viscoupler>-1 then
    smooth[viscoupler].EMS:= not smooth[viscoupler].ems;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton2click(Sender: TObject);
var
  oldstate: boolean;
begin
  oldstate:= api_bk8x1.open;
  label2.caption:= 'ok';
  api_bk8x1.Open:=not oldstate;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton3Click(Sender: TObject);
var
  viscoupler: integer;
begin
  // change EMS state of current visible device
  viscoupler:= api_listbox1.ItemIndex;
  if viscoupler>-1 then
    polish[viscoupler].EMS:= not polish[viscoupler].ems;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if api_bk8x1.open then
  begin
    api_bk8x1.Open:= false;
    sleep(200);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var
  couplerindex: integer;
begin
  mainpath:= extractfiledir(application.exename);
  // api_bk8x1.OpenSettings(mainpath+'\bk8x1.settings');

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
  api_bk8x1.SaveSettings(mainpath+'\bk8x1.settings');

  // free thread io buffer critical section
  for couplerindex:=0 to MAXCOUPLERS-1 do
    KillIO(couplerindex);
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton4MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button1a:= true;
end;

procedure TForm1.API_grbutton4MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button1a:= false;
end;

procedure TForm1.API_grbutton5MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button2a:= false;
end;

procedure TForm1.API_grbutton5MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button2a:= true;
end;

procedure TForm1.API_grbutton6MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button3a:= true;
end;

procedure TForm1.API_grbutton6MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button3a:= false;
end;

procedure TForm1.API_grbutton7MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button4a:= true;
end;

procedure TForm1.API_grbutton7MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button4a:= false;
end;

procedure TForm1.API_grbutton8MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button1b:= true;
end;

procedure TForm1.API_grbutton8MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button1b:= false;
end;

procedure TForm1.API_grbutton9MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button2b:= true;
end;

procedure TForm1.API_grbutton9MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button2b:= false;
end;

procedure TForm1.API_grbutton10MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button3b:= true;
end;

procedure TForm1.API_grbutton10MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button3b:= false;
end;

procedure TForm1.API_grbutton11MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button4b:= true;
end;

procedure TForm1.API_grbutton11MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  button4b:= false;
end;

end.
