unit Main;
{(C) Alex Demchenko(alex@ritlabs.com)}

interface

uses
  Windows, Classes, Forms, Controls, StdCtrls,
  ICQWorks,       //Unit with all constants and some nifty functions
  ICQClient;      //Unit with component

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    PasswordEdit: TEdit;
    UINEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LoginBtn: TButton;
    GroupBox2: TGroupBox;
    DestUINEdit: TEdit;
    Label3: TLabel;
    MessageMemo: TMemo;
    SendMsgBtn: TButton;
    GroupBox3: TGroupBox;
    EventMemo: TMemo;
    ICQClient1: TICQClient;
    procedure LoginBtnClick(Sender: TObject);
    procedure ICQClient1Login(Sender: TObject);
    procedure SendMsgBtnClick(Sender: TObject);
    procedure ICQClient1ConnectionFailed(Sender: TObject);
    procedure ICQClient1MessageRecv(Sender: TObject; Msg, UIN: String);
    procedure ICQClient1Error(Sender: TObject; ErrorType: TErrorType;
      ErrorMsg: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.LoginBtnClick(Sender: TObject);
begin
  //Check if Password & UIN was set
  if (StrToInt(UINEdit.Text) = 0) or (PasswordEdit.Text = '') then
  begin
    MessageBox(0, 'Please set your UIN & Password', 'Could not login', MB_ICONERROR);
    Exit;
  end;

  LoginBtn.Enabled := False;                      //Disable Login button to prevent login errors 

  //Theese parameters can be set in Properties tab of TICQClient component
  ICQClient1.ConvertToPlaintext := True;          //Convert RTF text to plain (when you don't use TRichEdit)
  ICQClient1.ICQServer := 'login.icq.com';        //Default login server
  ICQClient1.ICQPort := 5190;                     //Default login port
  ICQClient1.Password := PasswordEdit.Text;       //Set password
  ICQClient1.UIN := StrToInt(UINEdit.Text);       //Set UIN
  {-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}
  ICQClient1.Login;                               //Login to server!
  EventMemo.Lines.Add('Connecting to server...'); //Add a line showing that we've started connection to the server
end;

//This event called on a successfull logon
procedure TForm1.ICQClient1Login(Sender: TObject);
begin
  EventMemo.Lines.Add('Successfull logon! Try to send a message to someone now!');
end;

//This event called when connection with ICQ server lost or you cannot login in different reasons
procedure TForm1.ICQClient1ConnectionFailed(Sender: TObject);
begin
  EventMemo.Lines.Add('Connection with ICQ server failed');
  LoginBtn.Enabled := True;  
end;

//Called when TICQClient cannot connect to specified host(login.icq.com), possible wrong login host or network is down
procedure TForm1.SendMsgBtnClick(Sender: TObject);
begin
  //If we are online then send a message, otherwise show message requireing a login
  if (not ICQClient1.LoggedIn) then
  begin
    MessageBox(0, 'Please, login before sending a message', 'Could not send message', MB_ICONERROR);
    Exit;
  end;
  //Finally send the message
  ICQClient1.SendMessage(StrToInt(DestUINEdit.Text), MessageMemo.Text);
end;

//This event called when someone sends you a message
procedure TForm1.ICQClient1MessageRecv(Sender: TObject; Msg, UIN: String);
begin
  EventMemo.Lines.Add('You received a message from ' + UIN + ': ' + Msg);
end;

//This event is called when error in TICQClient is occured
procedure TForm1.ICQClient1Error(Sender: TObject; ErrorType: TErrorType;
  ErrorMsg: String);
begin
  if ErrorType = ERR_WARNING then
    EventMemo.Lines.Add('Warning: ' + ErrorMsg)
  else
    EventMemo.Lines.Add('Error: ' + ErrorMsg);
  LoginBtn.Enabled := True;
end;

end.
