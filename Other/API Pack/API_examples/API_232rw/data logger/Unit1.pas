unit Unit1;

//------------------------------------------------------------------------------
// 29112004, ari pikivirta
// * added comments for each function to clear the idea and how easy serial
//   communication really can be.
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, API_base, API_grbutton, API_chart,
  API_logfile;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Memo1: TMemo;
    Timer1: TTimer;
    Label2: TLabel;
    Label4: TLabel;
    ComboBox2: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    API_chart1: TAPI_chart;
    API_grbutton1: TAPI_grbutton;
    API_logfile1: TAPI_logfile;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  API_232rw;

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  // set defaults
  com_init;
  //com_setdefaults;
  com_seteofchar( #13 );
  com_setport( combobox1.ItemIndex + 1 );
  com_setbaudrate( strtoint( combobox2.text ) );
  com_setreadtimeout( 250 );

  api_logfile1.Filename:= extractfiledir(application.exename)+'\logging.txt';

  // try to open port
  (*
  if com_open then memo1.lines.add('COM'+inttostr(com_getport)+' port opened ('+inttostr(com_getbaudrate)+' bps).')
    else memo1.lines.add('Failed to open COM'+inttostr(com_getport)+' port.');
  *)
end;

//------------------------------------------------------------------------------
procedure TForm1.ComboBoxChange(Sender: TObject);
begin
  // close port if currently open
  if com_isopen then
    if com_close then
      memo1.lines.add('COM'+inttostr(com_getport)+' port closed.');

  // set new values
  com_setport( combobox1.ItemIndex + 1 );
  com_setbaudrate( strtoint( combobox2.Text ) );
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
  txt: string;
  s: string;
  i: integer;
begin
  if api_232rw.com_isopen then
  begin
    //$008080FF
    if api_grbutton1.GradientEnd<>$008080FF then
    begin
      api_grbutton1.GradientEnd:= $008080FF;
      api_grbutton1.Caption:= 'Stop Data Logging';
    end;
  end else
  begin
    //$0080FF80
    if api_grbutton1.GradientEnd<>$0080FF80 then
    begin
      api_grbutton1.GradientEnd:= $0080FF80;
      api_grbutton1.Caption:= 'Start Data Logging';
    end;
  end;

  // clear memo
  while memo1.lines.count > 500 do
    memo1.lines.Delete(0);

  // get last error message
  label6.Caption:= com_lasterror;

  // check for messages
  if com_isopen then
  begin
    // try to read line
    if (com_read( txt )) and (txt<>'') then
    begin
      memo1.lines.Text:= memo1.lines.text + s;
      api_logfile1.Add(s);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  // close port if open
  if com_isopen then com_close;
  com_kill;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  // activate / deactivate
  if com_isopen then
  begin
    // close port if currently open
    if com_close then
      memo1.lines.add('COM'+inttostr(com_getport)+' port closed.');
  end else
  begin
    // try to open port
    if com_open then memo1.lines.add('COM'+inttostr(combobox1.itemindex+1)+' port opened ('+combobox2.text+' bps).')
      else memo1.lines.add('Failed to open COM'+inttostr(combobox1.itemindex+1)+' port.');
  end;
end;

end.
