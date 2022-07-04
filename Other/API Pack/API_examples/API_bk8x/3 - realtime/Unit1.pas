unit Unit1;

//------------------------------------------------------------------------------
//
// API_bk8x real time control example (all control put into the thread event)
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
    Timer1: TTimer;
    Label6: TLabel;
    Label7: TLabel;
    API_ledgrid1: TAPI_ledgrid;
    API_ledgrid2: TAPI_ledgrid;
    procedure FormCreate(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure API_bk8x1ThreadEvent(Sender: TThread; CycleMSec: Double;
      Inputs: TBuffer; var Outputs: TBuffer);
    procedure API_bk8x1ThreadError(Sender: TThread; ErrorMsg: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TDataRecord = record
    lock: tmultireadexclusivewritesynchronizer;
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
  // - - - - write to record
  // move variables threadsafe into the data record
  // in this example "measuring device" is only throwing
  // the inputs and outputs and cycle time out from the
  // thread event to show up something on the form on demand
  data.lock.beginwrite;
  try
    data.inputs:= inputs;
    data.outputs:= outputs;
    data.cycletime:= cycleMSec;
  finally
    data.lock.endwrite;
  end;
  // - - - - write to record

  // this part is for doing the real time control
  // and all time critical things with the input and output
  // buffers

  // binary counter as example,
  // this is pretty fast though - because
  // binary counter is increased on every cycle :)
  outputs[0]:= outputs[0] + 1;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  if not api_bk8x1.open then
  begin
    // clear error history
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
procedure TForm1.FormCreate(Sender: TObject);
begin
  Data.lock:= tmultireadexclusivewritesynchronizer.create;
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
  Data.lock.Free;
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
  outputs, inputs: tbuffer;
  cycletime: double;
  i: integer;
begin
  // - - - - read from record
  // threadsafe get all data record items, note that this part
  // is only for reading the things from the record -> were are
  // putting things into this record only on the thread event
  // (using begin write), all other parts (visual etc.) is only reading that info
  data.lock.BeginRead;
  try
    inputs:= data.inputs;
    outputs:= data.outputs;
    cycletime:= data.cycletime;
  finally
    data.lock.EndRead;
  end;
  // - - - - read from record

  // show cycletime
  if api_bk8x1.Open then
  begin
    label7.caption:= 'Running ('+timetostr(now-api_bk8x1.starttime)+')';
    label6.Caption:= floattostr(cycletime)+' ms';
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
    api_ledgrid1.Boolean(0,i, bitisset(inputs[0], i));
    api_ledgrid2.Boolean(0,i, bitisset(outputs[0], i));
  end;
end;

end.
