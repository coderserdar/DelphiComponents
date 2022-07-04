unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_grbutton, StdCtrls, API_edit, API_label, ExtCtrls, API_tcp,
  ComCtrls, API_richedit, API_tools;

type
  TForm1 = class(TForm)
    API_tcp1: TAPI_tcp;
    Panel1: TPanel;
    API_label1: TAPI_label;
    API_edit1: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    API_richedit1: TAPI_richedit;
    Panel2: TPanel;
    API_edit2: TAPI_edit;
    API_label2: TAPI_label;
    API_label3: TAPI_label;
    API_edit3: TAPI_edit;
    API_label4: TAPI_label;
    API_edit4: TAPI_edit;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    API_label5: TAPI_label;
    API_edit5: TAPI_edit;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    API_tools1: TAPI_tools;
    procedure FormActivate(Sender: TObject);
    procedure API_tcp1ServerPutFileComplete(sender: TObject; ipto,
      filename: string);
    procedure API_tcp1ServerGetFileComplete(sender: TObject; from,
      filename: string);
    procedure API_tcp1ServerPutFile(sender: TObject; ipto, filename: string;
      var Accept: Boolean);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_edit4KeyPress(Sender: TObject; var Key: Char);
    procedure API_tcp1ServerGetFile(sender: TObject; from: string; var filename: string;
      filesize: Int64; var Accept: Boolean);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_tcp1ServerGetText(sender: TObject; from, text: string);
    procedure API_tcp1ServerError(sender: TObject; from, text: string;
      stamp: TDateTime);
    procedure API_tcp1ServerClose(sender: TObject);
    procedure API_tcp1ServerStart(sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
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
procedure TForm1.API_edit4KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then
  begin
    key:= #0;                                   // no need to show as enter
    api_grbutton4click(self);                   // press send button internally
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  if not api_tcp1.Active then                   // if server is not active
  begin
    api_tcp1.ServerPort:= api_edit1.asInteger;  // set port to listen for
    api_tcp1.Active:= true;                     // try to activate server
  end else
    api_tcp1.Active:= false;                    // de-activate server
  api_grbutton1.LedState:= api_tcp1.Active;     // change led status according to state
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  opendialog1.Filter:= 'All files (*.*)|*.*';
  if opendialog1.Execute then
    api_edit3.Text:= opendialog1.filename;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  if not api_tcp1.SendFile(api_edit2.text, api_edit5.asInteger, api_edit3.text) then
  begin
    api_richedit1.SelAttributes.Color:= clblack;
    api_richedit1.SelText:= #13+timetostr(now)+'> ';
    api_richedit1.selattributes.color:= clred;
    api_richedit1.seltext:= 'Failed to send file: ';
    api_richedit1.selattributes.color:= clteal;
    api_richedit1.seltext:= api_edit4.text;    
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  if api_tcp1.SendText(api_edit2.text, api_edit5.asInteger, api_edit4.text) then
  begin
    api_richedit1.SelAttributes.Color:= clblack;
    api_richedit1.SelText:= #13+timetostr(now)+'> Message sent: ';
    api_richedit1.selattributes.color:= clgreen;
    api_richedit1.seltext:= api_edit4.text;
    api_edit4.Text:= '';
    api_edit4.SetFocus;
  end else
  begin
    api_richedit1.SelAttributes.Color:= clblack;
    api_richedit1.SelText:= #13+timetostr(now)+'> ';
    api_richedit1.SelAttributes.Color:= clred;
    api_richedit1.SelText:= 'Failed to send message!';
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_tcp1ServerClose(sender: TObject);
begin
  // we have to check if form is showing before we show any data here,
  // because this event is fired on when the api_tcp component is about
  // to be destroyed also - at that moment we don't have form anymore
  // to display anything in there.
  if showing then
  begin
    api_richedit1.SelAttributes.Color:= clblue;
    api_richedit1.SelText:= #13+'Server stopped.';
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_tcp1ServerError(sender: TObject; from, text: string;
  stamp: TDateTime);
begin
  api_richedit1.SelAttributes.Color:= clblack;
  api_richedit1.SelText:= #13+timetostr(now)+'> Server error: ';
  api_richedit1.selattributes.color:= clred;
  api_richedit1.seltext:= text;
  if from<>'' then api_richedit1.seltext:=' (ip '+from+')';
end;

//------------------------------------------------------------------------------
procedure TForm1.API_tcp1ServerGetFile(sender: TObject; from: string; var filename: string;
  filesize: Int64; var Accept: Boolean);
begin
  savedialog1.filename:= filename;
  if savedialog1.execute then
  begin
    api_richedit1.SelAttributes.Color:= clblack;
    api_richedit1.SelText:= #13+timetostr(now)+'> Receiving file: ';
    filename:= savedialog1.filename;
    accept:= true;
  end else
  begin
    api_richedit1.SelAttributes.Color:= clblack;
    api_richedit1.SelText:= #13+timetostr(now)+'> ';
    api_richedit1.SelAttributes.Color:= clred;
    api_richedit1.seltext:= 'Rejected file: ';
    accept:= false;
  end;
  api_richedit1.selattributes.color:= clgreen;
  api_richedit1.seltext:= filename;
  api_richedit1.selattributes.color:= clpurple;
  api_richedit1.seltext:=' ('+inttostr(filesize)+'b)';
  api_richedit1.selattributes.color:= clpurple;
  api_richedit1.seltext:=' (from '+from+')';
end;

//------------------------------------------------------------------------------
procedure TForm1.API_tcp1ServerGetFileComplete(sender: TObject; from,
  filename: string);
begin
  api_richedit1.SelAttributes.Color:= clblack;
  api_richedit1.SelText:= #13+timetostr(now)+'> File received: ';
  api_richedit1.selattributes.color:= clgreen;
  api_richedit1.seltext:= filename;
  api_richedit1.selattributes.color:= clpurple;
  api_richedit1.seltext:=' (from '+from+')';
end;

//------------------------------------------------------------------------------
procedure TForm1.API_tcp1ServerGetText(sender: TObject; from, text: string);
begin
  api_richedit1.SelAttributes.Color:= clblack;
  api_richedit1.SelText:= #13+timetostr(now)+'> Received text: ';
  api_richedit1.selattributes.color:= clgreen;
  api_richedit1.seltext:= text;
  if from<>'' then
  begin
    api_richedit1.selattributes.color:= clpurple;
    api_richedit1.seltext:=' (from '+from+')';
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_tcp1ServerPutFile(sender: TObject; ipto, filename: string;
  var Accept: Boolean);
begin
  api_richedit1.SelAttributes.Color:= clblack;
  api_richedit1.SelText:= #13+timetostr(now)+'> Sending file: ';
  api_richedit1.selattributes.color:= clgreen;
  api_richedit1.seltext:= filename;
  api_richedit1.selattributes.color:= clpurple;
  api_richedit1.seltext:=' (to '+ipto+')';
end;

//------------------------------------------------------------------------------
procedure TForm1.API_tcp1ServerPutFileComplete(sender: TObject; ipto,
  filename: string);
begin
  api_richedit1.SelAttributes.Color:= clblack;
  api_richedit1.SelText:= #13+timetostr(now)+'> File sent: ';
  api_richedit1.selattributes.color:= clgreen;
  api_richedit1.seltext:= filename;
  api_richedit1.selattributes.color:= clpurple;
  api_richedit1.seltext:=' (to '+ipto+')';
end;

//------------------------------------------------------------------------------
procedure TForm1.API_tcp1ServerStart(sender: TObject);
begin
  api_richedit1.SelAttributes.Color:= clblue;
  api_richedit1.SelText:= #13+'Server activated.';
end;

//------------------------------------------------------------------------------
procedure TForm1.FormActivate(Sender: TObject);
begin
  api_edit2.text:= api_tools1.IPAddress;
end;

end.
