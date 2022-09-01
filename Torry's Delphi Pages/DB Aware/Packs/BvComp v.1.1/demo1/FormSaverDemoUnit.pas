unit FormSaverDemoUnit;

interface


uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons,
{$else}
  QExtCtrls,
  QControls, QStdCtrls, QButtons,
  QForms,QDialogs,
  Qt,
  QGraphics,
{$endif}
  SysUtils, Classes,

  bvFormSaver;

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

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure TFormSaverDemo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=caFree
end;

end.
