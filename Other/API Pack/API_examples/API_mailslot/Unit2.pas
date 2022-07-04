unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_edit, API_listbox, ExtCtrls, API_grbutton,
  API_mailslot, API_base;

type
  TForm1 = class(TForm)
    API_mailslot1: TAPI_mailslot;
    API_grbutton1: TAPI_grbutton;
    API_listbox1: TAPI_listbox;
    API_listbox2: TAPI_listbox;
    API_edit2: TAPI_edit;
    API_edit1: TAPI_edit;
    API_grbutton2: TAPI_grbutton;
    Splitter1: TSplitter;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure API_edit1KeyPress(Sender: TObject; var Key: Char);
    procedure API_mailslot1NewMessage(sender: TObject; Origin, Nick, Date,
      Time, Text: ShortString);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure API_mailslot1LeaveMessage(sender: TObject; Origin, Nick,
      Date, Time, Text: ShortString);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then
  begin
    // get nick
    api_mailslot1.Nick:=api_edit2.text;
    // send message
    if api_mailslot1.broadcast(api_edit1.text,api_mailslot1.Mailbox) then
    begin
      // add own message
      api_Edit1.text:='';
      api_edit1.SetFocus;
    end else
    begin
      // add error message
      api_listbox2.Items.add(timetostr(now)+'||Error sending message');
    end;
    api_listbox2.TopIndex:=api_listbox2.items.count-1;    
  end;
end;

procedure TForm1.API_mailslot1NewMessage(sender: TObject; Origin, Nick,
  Date, Time, Text: ShortString);
var
  i, j: integer;
begin
  j:=-1;
  for i:=0 to api_listbox1.items.count-1 do
    if api_listbox1.Items.Names[i]=origin then
    begin
      j:=i;
      break;
    end;

  if j>-1 then
    api_listbox1.items[j]:=origin+'='+nick else
    api_listbox1.items.add(origin+'='+nick);

  api_listbox2.items.add(time+'||'+nick+'||'+text);
  api_listbox2.TopIndex:=api_listbox2.items.count-1;
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  // check for leave message
  if api_mailslot1.active then
    api_mailslot1.broadcast_leave('-',api_mailslot1.Mailbox);

  // change mailbox state
  api_mailslot1.Active:=not api_mailslot1.Active;
  api_grbutton1.LedState:= api_mailslot1.active;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // check for leave message
  if api_mailslot1.active then
  begin
    api_mailslot1.broadcast_leave('-',api_mailslot1.Mailbox);
    api_mailslot1.Active:=false;
  end;
end;

procedure TForm1.API_mailslot1LeaveMessage(sender: TObject; Origin, Nick,
  Date, Time, Text: ShortString);
var
  i: integer;
  j: integer;
begin
  j:=-1;
  for i:=0 to api_listbox1.items.count-1 do
    if api_listbox1.items.names[i]=origin then
    begin
      j:=i;
      break;
    end;

  // delete from list
  if j>-1 then
    api_listbox1.Items.Delete(j);

  // show leaving message
  api_listbox2.items.add(time+'||'+nick+'||Exit: '+text);
  api_listbox2.TopIndex:=api_listbox2.items.count-1;
end;

end.
