program OverbyteIcsHttpPost;

uses
  Forms,
  OverbyteIcsHttpPost1 in 'OverbyteIcsHttpPost1.pas' {HttpPostForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpPostForm, HttpPostForm);
  Application.Run;
end.
