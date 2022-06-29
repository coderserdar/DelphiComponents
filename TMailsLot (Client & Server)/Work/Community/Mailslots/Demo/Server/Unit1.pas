unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Mailslots;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    MailslotServer1: TMailslotServer;
    Label2: TLabel;
    procedure MailslotServer1MessageReceived(Sender: TObject;
      const Message: String);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

 procedure TForm1.MailslotServer1MessageReceived(Sender: TObject; const Message: String);
 Begin
   Memo1.Lines.Assign(MailslotServer1.Messages);
 End;

 Procedure TForm1.Button1Click(Sender: TObject);
 Begin
   If MailslotServer1.Active Then
      Begin
         Button1.Caption := '&Listen';
         MailslotServer1.Active := False;
      End
   Else
      Begin
         Button1.Caption := '&Stop';
         MailslotServer1.MailslotName := Edit1.Text;
         MailslotServer1.Active := True;
      End;
 End;

end.
