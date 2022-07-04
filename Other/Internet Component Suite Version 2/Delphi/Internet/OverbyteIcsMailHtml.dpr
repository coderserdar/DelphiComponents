program OverbyteIcsMailHtml;

uses
  Forms,
  OverbyteIcsMailHtm1 in 'OverbyteIcsMailHtm1.pas' {HtmlMailForm};

{$R *.res}

begin
  {$IFNDEF VER80}Application.Initialize;{$ENDIF}
  Application.CreateForm(THtmlMailForm, HtmlMailForm);
  Application.Run;
end.

