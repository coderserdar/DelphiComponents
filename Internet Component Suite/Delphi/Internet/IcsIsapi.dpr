library IcsIsapi;

uses
  WebBroker,
  ISAPIApp,
  IcsIsap1 in 'IcsIsap1.pas' {IcsIsapiWebModule: TWebModule};

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
