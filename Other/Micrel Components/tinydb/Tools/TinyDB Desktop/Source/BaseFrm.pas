unit BaseFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TBaseForm = class(TForm)
  private
  protected
    procedure DoCreate; override;
  public
    procedure TransLanguage; virtual;
  end;

var
  BaseForm: TBaseForm;

implementation

uses LangMgr;

{$R *.DFM}

{ TBaseForm }

procedure TBaseForm.DoCreate;
begin
  inherited;
  TransLanguage;
end;

procedure TBaseForm.TransLanguage;
begin
  AppLangMgr.Trans(Self);
end;

end.
