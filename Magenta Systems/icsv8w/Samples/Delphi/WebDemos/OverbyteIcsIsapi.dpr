library OverbyteIcsIsapi;

uses
  WebBroker,
  ISAPIApp,
  OverbyteIcsIsapi1 in 'OverbyteIcsIsapi1.pas' {IcsIsapiWebModule: TWebModule};

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TIcsIsapiWebModule, IcsIsapiWebModule);
  Application.Run;
end.
