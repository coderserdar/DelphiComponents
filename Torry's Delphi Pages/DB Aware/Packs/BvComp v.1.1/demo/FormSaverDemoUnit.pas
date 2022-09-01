unit FormSaverDemoUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, bvFormSaver;

type
  TFormSaverDemo = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    bvFormSaver1: TbvFormSaver;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSaverDemo: TFormSaverDemo;

implementation

{$R *.DFM}

procedure TFormSaverDemo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=caFree
end;

end.
