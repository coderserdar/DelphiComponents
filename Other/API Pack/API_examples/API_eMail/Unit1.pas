unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, API_eMail, API_base;

type
  TForm1 = class(TForm)
    tAPI_eMail1: tAPI_eMail;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    body: TMemo;
    Label3: TLabel;
    Button1: TButton;
    Label4: TLabel;
    attach: TListBox;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    Label5: TLabel;
    Label6: TLabel;
    Mailto: TMemo;
    Copyto: TMemo;
    BCopyto: TMemo;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label7: TLabel;
    Edit1: TEdit;
    Label8: TLabel;
    Edit3: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure tAPI_eMail1MapiError(Sender: TObject; ErrCode: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  // set input data
  tapi_email1.MailTo.Text:=mailto.Lines.Text;
  tapi_email1.CopyTo.text:=copyto.Lines.Text;
  tapi_email1.BlindCopyTo.text:=bcopyto.lines.text;
  tapi_email1.FromName:= edit1.text;
  tapi_email1.FromEmail:= edit1.text;
  tapi_email1.Subject:= edit2.text;
  tapi_email1.Body.Text:= body.Lines.Text;
  tapi_email1.Attachments.text:= attach.Items.Text;
  tapi_email1.Profilename:= edit1.text;
  tapi_email1.Password:= edit3.text;

  // send email
  try
    if tapi_email1.sendemail(handle) then showmessage('Message sent succesfully')
      else showmessage('Failed to send message');
  except
    showmessage('Exception during send.');
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  opendialog1.Filter:='All files (*.*)|*.*';
  if opendialog1.Execute then
  begin
    attach.items.add(opendialog1.FileName);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if attach.itemindex<>-1 then
    attach.items.Delete(attach.itemindex);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  mailto.Clear;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  copyto.clear;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  bcopyto.Clear;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  body.Clear;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  attach.clear;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.tAPI_eMail1MapiError(Sender: TObject; ErrCode: Integer);
var
  s: string;
begin
  case errcode of
    2: s:= 'MAPI_E_FAILURE';
    3: s:= 'MAPI_E_LOGON_FAILURE';
    4: s:= 'MAPI_E_DISK_FULL';
    5: s:= 'MAPI_E_INSUFFICIENT_MEMORY';
    6: s:= 'MAPI_E_ACCESS_DENIED';
    8: s:= 'MAPI_E_TOO_MANY_SESSIONS';
    9: s:= 'MAPI_E_TOO_MANY_FILES';
    10: s:= 'MAPI_E_TOO_MANY_RECIPIENTS';
    11: s:= 'MAPI_E_ATTACHMENT_NOT_FOUND';
    12: s:= 'MAPI_E_ATTACHMENT_OPEN_FAILURE';
    13: s:= 'MAPI_E_ATTACHMENT_WRITE_FAILURE';
    14: s:= 'MAPI_E_UNKNOWN_RECIPIENT';
    15: s:= 'MAPI_E_BAD_RECIPTYPE';
    16: s:= 'MAPI_E_NO_MESSAGES';
    17: s:= 'MAPI_E_INVALID_MESSAGE';
    18: s:= 'MAPI_E_TEXT_TOO_LARGE';
    19: s:= 'MAPI_E_INVALID_SESSION';
    20: s:= 'MAPI_E_TYPE_NOT_SUPPORTED';
    21: s:= 'MAPI_E_AMBIGUOUS_RECIPIENT';
    22: s:= 'MAPI_E_MESSAGE_IN_USE';
    23: s:= 'MAPI_E_NETWORK_FAILURE';
    24: s:= 'MAPI_E_INVALID_EDITFIELDS';
    25: s:= 'MAPI_E_INVALID_RECIPS';
    26: s:= 'MAPI_E_NOT_SUPPORTED';
    else s:= 'MAPI ERROR '+inttostr(ErrCode);
  end;
  messagedlg(s, mterror, [mbok], 0);
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  edit1.text:=tapi_email1.GetDefaultProfile;
end;

end.
