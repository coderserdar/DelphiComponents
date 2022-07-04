unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, API_bk8x00, StdCtrls, syncobjs,
  API_gradient;

type
  TForm1 = class(TForm)
    API_bk8x001: TAPI_bk8x00;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
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
    API_gradient1: TAPI_gradient;
    API_grbutton3: TAPI_grbutton;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    API_grbutton5: TAPI_grbutton;
    API_grbutton6: TAPI_grbutton;
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton5Click(Sender: TObject);
    procedure API_grbutton6Click(Sender: TObject);
  private
  public
    calc: integer;
    outp: tbuffer;
    inp: tbuffer;
    fr: int64;
    c1: int64;
    c2: int64;
    readturn: boolean;
    cyclecount: cardinal;
    cycletime: real;
    starttime: tdatetime;
    procedure applyinoutcoloring;
    procedure messaging;
  end;

  Tbk8x00threadrecord = record
    active: boolean;
    thread: tthread;
  end;

  Tbk8x00thread = class(tthread)
  private
  protected
    procedure execute; override;
  public
  end;

var
  Form1: TForm1;
  bk8x00thread: tbk8x00threadrecord;

implementation

uses
  dateutils;

{$R *.dfm}

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  // show component integrated
  // settings form
  api_bk8x001.showsettings;
end;

procedure TForm1.applyinoutcoloring;
begin
  // apply inout coloring
  if api_bk8x001.inputlength<3 then label23.font.Color:=clred
    else label23.Font.color:=clgreen;
  if api_bk8x001.inputlength<2 then
  begin
    label18.Font.color:=clred;
    label17.font.color:=clred;
  end else
  begin
    label18.font.color:=clgreen;
    label17.font.color:=clgreen;
  end;
  if api_bk8x001.inputlength<1 then
  begin
    label16.Font.color:=clred;
    label13.Font.color:=clred;
  end else
  begin
    label16.Font.color:=clgreen;
    label13.font.color:=clgreen;
  end;
  if api_bk8x001.outputlength<3 then label22.Font.Color:=clred
    else label22.font.color:=clgreen;
  if api_bk8x001.outputlength<2 then
  begin
    label21.font.color:=clred;
    label20.font.color:=clred;
  end else
  begin
    label21.font.color:=clgreen;
    label20.Font.color:=clgreen;
  end;
  if api_bk8x001.outputlength<1 then
  begin
    label19.font.color:=clred;
    label14.font.color:=clred;
  end else
  begin
    label19.font.color:=clgreen;
    label14.font.color:=clgreen;
  end;
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  if api_bk8x001.open then
    if not bk8x00thread.active then
    begin
      // init variables
      cycletime:=0;
      cyclecount:=0;
      readturn:=False;
      starttime:=now;
      // init timer
      queryperformancefrequency(int64((@fr)^));
      queryperformancecounter(int64((@c1)^));
      // start thread
      bk8x00thread.thread:=tbk8x00thread.create(false);
    end else
    begin
      // terminate thread
      bk8x00thread.thread.Terminate;
    end;
end;

procedure Tbk8x00thread.execute;
begin
  bk8x00thread.active:=true;
  while not terminated do
    synchronize(form1.messaging);
  bk8x00thread.active:=false;
end;

procedure TForm1.messaging;
begin
    if API_bk8x001.readwrite(inp, outp) then
    begin
      // error occured
    end;

    // get cycletime (ms) and increase
    // cyclecounter
    queryperformancecounter(int64((@c2)^));
    cycletime:=(1000*(c2-c1)/fr);
    c1:=c2;
    cyclecount:=cyclecount+1;

    // DO YOUR INP AND OUTP HANDLING (LOGICS) AFTER THIS ------------------

    calc:=calc+1;
    if calc>256*256 then calc:=0;

    outp[0]:=byte(hi(calc));
    outp[1]:=byte(lo(calc));
    outp[2]:=outp[1];
    outp[3]:=outp[0];
    outp[4]:=byte(round(cyclecount/500));

    // program will run this while thread is running
    // without caring what else is happening there
    // so you don't need separated timers or anything..
    // windows messages will cause some delays because
    // this is synchronized with the main thread.

    // THIS WAS YOUR "LOGICS" PART ----------------------------------------
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  // open or close port
  // depending on current
  // state
  if api_bk8x001.open then
    api_bk8x001.open:=false else
    api_bk8x001.open:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // display information for user
  if api_bk8x001.open then
  begin
    label2.Color:=clgreen;
    label2.Caption:='open';
    applyinoutcoloring;
  end else
  begin
    label2.Color:=clred;
    label2.caption:='false';
  end;
  if bk8x00thread.active then
  begin
    label4.Color:=clgreen;
    label4.caption:='running';
    form1.Caption:='Online '+formatfloat('0.00',hourspan(now,starttime))+'h';
  end else
  begin
    label4.color:=clred;
    label4.caption:='stopped';
    form1.caption:='Example 3';
  end;

  label6.Caption:=formatfloat('0.00',cycletime)+'ms'; // note! windows sync
  label8.caption:=inttostr(cyclecount);
  label10.caption:=api_bk8x001.geterror(api_bk8x001.lasterror);

  // display input buffer values
  label13.Caption:='0x'+inttohex(inp[0],2);
  label16.caption:='0x'+inttohex(inp[1],2);
  label17.caption:='0x'+inttohex(inp[2],2);
  label18.caption:='0x'+inttohex(inp[3],2);
  label23.caption:='0x'+inttohex(inp[4],2);

  // display output buffer values
  label14.caption:='0x'+inttohex(outp[0],2);
  label19.caption:='0x'+inttohex(outp[1],2);
  label20.caption:='0x'+inttohex(outp[2],2);
  label21.caption:='0x'+inttohex(outp[3],2);
  label22.caption:='0x'+inttohex(outp[4],2);

  // display bk8x00 status byte (just for debugging) :)
  label15.caption:=inttostr(api_bk8x001.status);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  calc:=0;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  applyinoutcoloring;
end;

procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.API_grbutton5Click(Sender: TObject);
var
  mainpath: string;
  fname: string;
begin
  // get program running directory
  mainpath:=Extractfiledir(paramstr(0));

  fname:=mainpath+'\bussettings.cfg';
  if api_bk8x001.savesettings(fname) then
  begin
    messagedlg('Settings saved succesfully.',mtinformation,[mbok],0);
  end else
  begin
    messagedlg('Failed to save settings.',mterror,[mbok],0);
  end;
end;

procedure TForm1.API_grbutton6Click(Sender: TObject);
var
  mainpath: string;
  fname: string;
begin
  // get program running directory
  mainpath:=Extractfiledir(paramstr(0));

  fname:=mainpath+'\bussettings.cfg';
  if api_bk8x001.opensettings(fname) then
  begin
    messagedlg('Settings opened succesfully.',mtinformation,[mbok],0);
  end else
  begin
    messagedlg('Failed to open settings.',mterror,[mbok],0);
  end;
end;

end.
