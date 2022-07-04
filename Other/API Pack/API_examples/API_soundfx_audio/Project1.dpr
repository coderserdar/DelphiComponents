program Project1;

{%TogetherDiagram 'ModelSupport_Project1\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Project1\Project1\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Project1\Unit1\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Project1\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Project1\Project1\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Project1\Unit1\default.txvpck'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
