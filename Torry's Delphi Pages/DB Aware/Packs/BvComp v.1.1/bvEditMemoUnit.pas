unit bvEditMemoUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Buttons,ExtCtrls,StdCtrls,
{$else}
  QGraphics,
  QControls,
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
  Qt,
  QDialogs,
  QComCtrls,
{$endif}


  bvLocalization, Classes;

type
  TEditMemoForm = class(TForm)
    Memo: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditMemoForm: TEditMemoForm;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure TEditMemoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=caFree
end;

procedure TEditMemoForm.FormCreate(Sender: TObject);
begin
   caption:=StrSeeList;
end;

end.
