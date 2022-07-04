program LeDemo;

{
  TLabelEffect Demonstration.
  Show off the capabilities of the TLabelEffect component.

  Written by Keith Wood - 3 Jan 1997.
}

uses
  Forms,
  LeDemo1 in 'LEDEMO1.PAS' {Form1};

{$R *.RES}

begin
  Application.Title := 'TLabelEffect Demonstration';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
