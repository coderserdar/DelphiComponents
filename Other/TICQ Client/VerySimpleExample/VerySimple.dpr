program VerySimple;
{(C) Alex Demchenko(alex@ritlabs.com)}

uses
  Forms,
  ICQClient in '..\Component\ICQClient.pas', //You can remove this line
  ICQWorks in '..\Component\ICQWorks.pas', //You can remove this line
  ICQSock in '..\Component\ICQSock.pas', //You can remove this line    
  ICQDirect2 in '..\Component\ICQDirect2.pas', //You can remove this line
  ICQLang in '..\Component\ICQLang.pas', //You can remove this line
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
