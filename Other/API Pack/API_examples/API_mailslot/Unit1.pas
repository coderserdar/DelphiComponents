unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_mailslot;

type
  TForm1 = class(TForm)
    tAPI_mailslot1: tAPI_mailslot;
    Edit1: TEdit;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    Edit3: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    procedure CheckBox1Click(Sender: TObject);
    procedure tAPI_mailslot1newmessage(sender: TObject; Origin, Date, Time,
      Text: ShortString);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if not tapi_mailslot1.active then
  begin
    tapi_mailslot1.mailbox:=edit1.text;
    tapi_mailslot1.header:=edit2.text;
    tapi_mailslot1.active:=True;
    edit1.enabled:=false;
    edit2.enabled:=false;
    button1.enabled:=true;
  end else
  begin
    tapi_mailslot1.active:=false;
    edit1.enabled:=true;
    edit2.enabled:=true;
    button1.enabled:=false;
  end;
end;

procedure TForm1.tAPI_mailslot1newmessage(sender: TObject; Origin, Date,
  Time, Text: ShortString);
begin
  listbox1.items.Insert(0,time+'>'+origin+'>'+text);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  recipient: string;
  text: string;
  mailbox: string;
begin
  recipient:=edit3.text;
  mailbox:=edit5.text;
  text:=edit6.Text;
  //
  tapi_mailslot1.sendmessage(recipient,mailbox,text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edit4.Text:=tapi_mailslot1.computer;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if tapi_mailslot1.active then
    tapi_mailslot1.active:=false;
end;

end.
