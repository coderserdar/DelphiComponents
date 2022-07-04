unit Unit1;

//------------------------------------------------------------------------------
// if you have checked the bk8x00 documentation or otherwise have got into
// the details, you probably also need to modify the register values of the
// different type of modules for some special feature. this usually is done
// via the Beckhoff's KS2000, vbut also is possible using this example. 
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, API_bk8x00, StdCtrls, API_listbox;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    API_bk8x001: TAPI_bk8x00;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_listbox1: TAPI_listbox;
    Label2: TLabel;
    Edit1: TEdit;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    procedure API_grbutton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
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
  api_bk8x001.ShowSettings;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  api_bk8x001.OpenSettings(extractfiledir(application.exename)+'\bk8x001.cfg');

  api_listbox1.Clear;
  api_listbox1.Items.add('Wellcome to API_BK8x00 example');
  api_listbox1.items.add('');
  api_listbox1.items.add('This example will show you how to modify register');
  api_listbox1.items.add('values of the different Beckhoff modules.');
  api_listbox1.items.add('You could use this as a tool directly as well.');
end;

//------------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  api_bk8x001.SaveSettings(extractfiledir(application.exename)+'\bk8x001.cfg');
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton2Click(Sender: TObject);
var
  cbyte: integer;
  sbyte: integer;
  regis: integer;
  value: integer;
  i, o: tbuffer;

  function RW(var inp: tbuffer; const outp: tbuffer): boolean;
  begin
    result:= api_bk8x001.ReadWrite(inp, outp);
    if not result then
      api_listbox1.items.add('Error: ReadWrite failed.');
  end;

begin
  api_listbox1.clear;

  try
    cbyte:= strtoint(edit2.text);                 // get command byte address
    regis:= strtoint(edit3.text);                 // register index
    sbyte:= strtoint(edit1.text);                 // status byte address
    value:= strtoint(edit4.text);                 // get value to write
  except
    api_listbox1.Items.add('Error: Conversion.');
    exit;
  end;

  api_bk8x001.Open:= true;                        // open serial port
  if api_bk8x001.Open then                        // check if it was opened
  try
    fillchar(o,sizeof(tbuffer),0);                // clear output buffer
    if not RW(i,o) then exit;                     // init buffers

    // write password
    o[cbyte+0]:= 128 + 64 + 31;
    o[cbyte+1]:= $35;
    o[cbyte+2]:= $12;
    if not RW(i,o) then exit;                     // write outputs
    api_listbox1.items.add('Admin password written.');

    // verify password
    o[cbyte+0]:= 128 + 31;
    o[cbyte+1]:= 0;
    o[cbyte+2]:= 0;
    if not RW(i,o) then exit;                     // write outputs
    if not RW(i,o) then exit;                     // read inputs
    if (  (i[sbyte+1] <> $35) or
          (i[sbyte+2] <> $12) ) then
    begin
      api_listbox1.items.add('Error: Password verify failed ('+inttohex(i[sbyte+2]+i[sbyte+1]*256,4)+').');
      exit;
    end else
      api_listbox1.items.add('Password done ('+inttohex(i[sbyte+2]+i[sbyte+1]*256,4)+').');

    // write registry value
    o[cbyte+0]:= 128 + 64 + regis;
    o[cbyte+1]:= (value and $ff);
    o[cbyte+2]:= (value and $ff00) shr 8;
    if not RW(i,o) then exit;                     // write outputs
    api_listbox1.items.add('Registry value written.');

    // verify registry write
    o[cbyte+0]:= 128 + regis;
    o[cbyte+1]:= 0;
    o[cbyte+2]:= 0;
    if not RW(i,o) then exit;                     // write outputs
    if not RW(i,o) then exit;                     // read inputs
    if (  (i[sbyte+2] <> (value and $ff00) shr 8) or
          (i[sbyte+1] <> (value and $ff))          ) then
    begin
      api_listbox1.items.add('Error: Setting verify failed ('+inttohex(i[sbyte+2]+i[sbyte+1]*256,4)+').');
      exit;
    end else
      api_listbox1.items.add('Verify done ('+inttohex(i[sbyte+2]+i[sbyte+1]*256,4)+').');

    // clear setting outputs
    o[cbyte+0]:= 0;
    o[cbyte+1]:= 0;
    o[cbyte+2]:= 0;
    if not RW(i,o) then exit;                     // write empty
    api_listbox1.Items.add('Registry write completed.');

  finally                                         // finally
    api_bk8x001.Open:= false;                     // close serial port
  end else
    api_listbox1.Items.add('Error: Failed to open serial port.');
end;

end.
