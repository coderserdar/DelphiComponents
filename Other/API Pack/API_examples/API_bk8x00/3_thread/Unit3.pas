unit Unit3;

//------------------------------------------------------------------------------
// threading example with the API_BK8x00 component.
// SYNCHRONIZED
//------------------------------------------------------------------------------
// note! bk thread must be used as synchronized with the main thread
// on this example because input and output buffer is accessed inside
// the threadevent as well as from other places (from the mainthread)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_edit, API_bk8x00, ExtCtrls, API_grbutton, API_ledgrid,
  API_base;

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
    procedure Timer1Timer(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure API_bk8x001Error(Sender: TObject; ErrNo: Integer;
      const ErrText: string);
    procedure API_ledgrid2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_bk8x001ThreadEvent(thread: TThread);
  private
  public
    mainpath: string;
    input, output: tbuffer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  dateutils, inifiles;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
  i,j: integer;
begin
  // change led state according to the device
  // serial port open or not (automatically
  // opened on thread start)
  api_grbutton2.LedState:=api_bk8x001.Open;

  // show online time on the label
  if api_bk8x001.StartTime<>0 then
  begin
    // online time update
    label1.caption:=formatfloat('0.00',secondspan(now, api_bk8x001.StartTime))+'s online';

    // update status line, only
    // in case no error(s) and port
    // closing has been done
    label2.Caption:= api_bk8x001.GetStatus;

    // draw input(s) states (4 first bytes)
    for i:=0 to (api_bk8x001.InputLength*2)-1 do
      if i<4 then
        for j:=0 to 7 do
          api_ledgrid1.Boolean(j,i,BitIsSet(input[i],j));

    // draw output(s) states (4 first bytes)
    for i:=0 to (api_bk8x001.OutputLength*2)-1 do
      if i<4 then
        for j:=0 to 7 do
          api_ledgrid2.boolean(j,i,BitIsSet(output[i],j));

  end else
    label1.caption:='Not online';
end;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x001ThreadEvent(thread: TThread);
begin
  // component thread will only fire this
  // event and not do any reading or writing
  // the bk8x00 buffers yet. here as it is
  // simplest (do both at same time):
  api_bk8x001.readwrite(input, output);
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
procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  // change bk state to running, also
  // because thread is set to automatically
  // start on port open it will be run
  // in case port is succesfully opened
  // and also stopped in case the thread
  // was running on this change.
  api_bk8x001.Open:=not api_bk8x001.Open;
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
    SetBit(output[row*8], col, not api_ledgrid2.Boolean(col,row));
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
end;

//------------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  api_bk8x001.SaveSettings(mainpath+'\bk8x001.settings');
end;

//------------------------------------------------------------------------------
procedure TForm1.API_bk8x001Error(Sender: TObject; ErrNo: Integer;
  const ErrText: string);
begin
  // if error happens, we'll get into this
  // event and then refresh status label
  // information to show the error text
  label2.caption:= errtext;
end;

//------------------------------------------------------------------------------

end.
