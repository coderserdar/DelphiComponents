unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, StdCtrls, API_edit, API_logfile, API_base,
  API_trayicon;

type
  TForm1 = class(TForm)
    API_logfile1: TAPI_logfile;
    API_edit1: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    Label1: TLabel;
    API_edit2: TAPI_edit;
    Label2: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    ListBox1: TListBox;
    Timer2: TTimer;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    procedure API_grbutton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure API_edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure API_logfile1LogFull(sender: TComponent;
      var DontRemoveLines: Boolean);
    procedure Timer2Timer(Sender: TObject);
    procedure API_logfile1Log(sender: TComponent; var Txt: string);
    procedure API_logfile1OpenFailed(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  (*
      here we will add new message into the log book
      and clear the text for entering new one
  *)
  if api_logfile1.filename<>api_edit2.text then
  begin
    api_logfile1.Filename:= api_edit2.Text; // set new filename
    api_logfile1.lines(listbox1.items); // load lines
  end;
  api_logfile1.Add(api_edit1.text); // add new message
  api_edit1.Text:= ''; // clear
  api_edit1.setfocus; // make sure edit is still focused
end;

//------------------------------------------------------------------------------
procedure TForm1.API_logfile1Log(sender: TComponent; var Txt: string);
begin
  listbox1.items.add(txt);
end;

//------------------------------------------------------------------------------
procedure TForm1.API_logfile1LogFull(sender: TComponent;
  var DontRemoveLines: Boolean);
var
  p1, p2: integer;
  AFilename, AExt, S: string;
  APart: integer;
begin
  if not checkbox1.checked then exit;

  // this happens when the max log lines happens
  // we want no lines to be removed from the log
  DontRemoveLines:= TRUE;
  // and we want to have new log file name for the
  // next logging parts..
  if api_logfile1.filename<>'' then
  begin
    AFilename:= api_logfile1.filename;
    AExt:= extractfileext(api_logfile1.filename);
    p1:= pos(AExt, AFilename);
    if (p1>0) then
      delete(AFilename, p1, length(AFilename)); // delete extension
    p2:= pos('.', AFilename); // get part if exists
    if (p2>0) then
    begin
      S:= copy(AFilename, p2+1, length(AFilename)); // get existing part num
      trystrtoint(S, APart);
      inc(APart);
      delete(AFilename, p2, length(AFilename)); // delete part info too
    end else
      APart:= 0;
    // we now should have new filename as below:
    api_logfile1.Filename:= AFilename + '.' + inttostr(Apart) + AExt;
    api_Edit2.Text:= api_logfile1.Filename;
    api_logfile1.Lines(listbox1.Items); // this basically clears the visible list
    api_edit1.SetFocus; // make sure edit is still focused
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_logfile1OpenFailed(Sender: TObject);
begin
  showmessage('Failed to open or create log file!');
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  (*
      we show up the log filename that we've defined
      onto the component's properties, also because
      there is loadonstart set to true, if the log file
      defined already exists, all the messages are loaded
      on startup -> so here we'll just put them into
      the visible listbox as well.
  *)
  api_logfile1.Filename:= api_edit2.Text;             // set filename
  api_logfile1.lines(listbox1.items);                 // load lines to listbox
  listbox1.itemindex:= listbox1.items.count-1;        // highlight last row
end;

//------------------------------------------------------------------------------
procedure TForm1.API_edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then api_grbutton1click(self);
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  label4.caption:= inttostr(api_logfile1.LineCount)+' of '+inttostr(api_logfile1.MaxLines)+' line(s)';
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer2Timer(Sender: TObject);
begin
  // this is for test purposes only
  // timer2 is disabled on this example by default
  api_logfile1.add(timetostr(now));
end;

end.
