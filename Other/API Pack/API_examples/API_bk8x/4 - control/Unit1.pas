unit Unit1;

//------------------------------------------------------------------------------
//
// API_bk8x example for the machine / controller type programming for the
// beckhoff bk8x00 couplers
//  * check the timer event
//  * check the thread event
//
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_bk8x, StdCtrls, API_listbox, API_edit, ExtCtrls, API_grbutton,
  API_ledgrid, API_base;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    API_edit1: TAPI_edit;
    API_edit2: TAPI_edit;
    API_edit3: TAPI_edit;
    API_listbox1: TAPI_listbox;
    Label4: TLabel;
    API_bk8x1: TAPI_bk8x;
    Label5: TLabel;
    API_edit4: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    Bevel1: TBevel;
    ScreenUpdateTimer: TTimer;
    Label6: TLabel;
    Label7: TLabel;
    API_ledgrid1: TAPI_ledgrid;
    API_ledgrid2: TAPI_ledgrid;
    ControlTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScreenUpdateTimerTimer(Sender: TObject);
    procedure API_bk8x1ThreadEvent(Sender: TThread; CycleMSec: Double;
      Inputs: TBuffer; var Outputs: TBuffer);
    procedure API_bk8x1ThreadError(Sender: TThread; ErrorMsg: string);
    procedure ControlTimerTimer(Sender: TObject);
  private
    { Private declarations }
    finputs: tbuffer;
    foutputs: tbuffer;
    fcycletime: double;
    fcounter: integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  syncobjs;

type
  TDataRecord = record
    lock: tcriticalsection;
    inputs, outputs: tbuffer;
    cycletime: double;
  end;

var
  Data: TDataRecord;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x1ThreadError(Sender: TThread; ErrorMsg: string);
begin
  if errormsg<>'' then
  begin
    api_listbox1.items.insert(0, timetostr(now)+'> '+errormsg);
    api_bk8x1.Open:= false;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x1ThreadEvent(Sender: TThread; CycleMSec: Double;
  Inputs: TBuffer; var Outputs: TBuffer);
begin
  // move input variables into the record and
  // read outputs from the record. this is all
  // we do on the thread event to keep the io
  // cycle running as fast as it's possible and
  // then have the machine / control part in
  // the timer instead because everything there
  // doesn't have to be 100% real time as in
  // the measuring example.

  // - - - - access record
  data.lock.Acquire;
  try
    data.inputs:= inputs;             // inputs -> data.inputs
    outputs:= data.outputs;           // data.outputs -> outputs
    data.cycletime:= cycleMSec;
  finally
    data.lock.release;
  end;
  // - - - - access record
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  if not api_bk8x1.open then
  begin
    api_listbox1.clear;
    // open
    api_bk8x1.Port:=        api_edit1.asInteger;
    api_bk8x1.Baudrate:=    api_edit2.asinteger;
    api_bk8x1.WriteLength:= api_edit3.asinteger;
    api_bk8x1.Address:=     api_edit4.asinteger;
    api_bk8x1.open:=        true;
  end else
    // close
    api_bk8x1.Open:= false;
end;

//------------------------------------------------------------------------------
procedure TForm1.ControlTimerTimer(Sender: TObject);
begin
  // this is the part containing the whole machine control
  // and also updating of all variables that has to do with
  // the inputs & outputs to minimize the accessing to the
  // record that we have to use critical section with for threadsafe

  // - - - - access record
  data.lock.Acquire;
  try
    finputs:= data.inputs;          // data.inputs -> finputs
    data.outputs:= foutputs;        // foutputs -> data.outputs
    fcycletime:= data.cycletime;
  finally
    data.lock.release;
  end;
  // - - - - access record

  // machine control part goes here below. we're running this
  // machine as 100ms cycle (that's where this timer is interval
  // is set) and also we can access all visuals or whatever
  // variables here without having to worry about threadsafe
  // issues etc. because all access to the io thread is done
  // already at the beginning of this and inside the thread
  // -> those can be left as they are in all this kind of projects :)

  // let's build binary counter, which will start
  // counting up every 0,5 seconds :)
  fcounter:= fcounter + 1;
  if fcounter>4 then
  begin
    foutputs[0]:= foutputs[0] + 1;
    fcounter:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  Data.lock:= tcriticalsection.create;
  if api_bk8x1.OpenSettings(extractfiledir(application.exename)+'\bk8x1.setting') then
  begin
    // load settings
    api_edit1.asInteger(api_bk8x1.Port);
    api_edit2.asinteger(api_bk8x1.Baudrate);
    api_edit3.asinteger(api_bk8x1.WriteLength);
    api_edit4.asInteger(api_bk8x1.Address);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  // save settings
  api_bk8x1.SaveSettings(extractfiledir(application.exename)+'\bk8x1.setting');
  Data.lock.release;
end;

//------------------------------------------------------------------------------
procedure TForm1.ScreenUpdateTimerTimer(Sender: TObject);
var
  i: integer;
begin
  // show cycletime
  if api_bk8x1.Open then
  begin
    label7.caption:= 'Running ('+timetostr(now-api_bk8x1.starttime)+')';
    label6.Caption:= floattostr(fcycletime)+' ms';
    api_grbutton1.Caption:= 'Stop';
  end else
  begin
    label7.caption:= 'Stopped';
    label6.caption:= '';
    api_grbutton1.Caption:= 'Start IO Thread';
  end;

  // draw ledgrids
  for i:=0 to 7 do
  begin
    api_ledgrid1.Boolean(0,i, bitisset(finputs[0], i));
    api_ledgrid2.Boolean(0,i, bitisset(foutputs[0], i));
  end;
end;

end.
