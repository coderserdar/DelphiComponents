unit bvNewFieldUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
  QControls, 
{$endif}
  SysUtils,
  bvFormSaver,bvLocalization, Classes;

type
  TNewFieldForm = class(TForm)
    EditField: TComboBox;
    LabelChoiceField: TLabel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewFieldForm: TNewFieldForm;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}


procedure TNewFieldForm.FormCreate(Sender: TObject);
begin
  caption:=StrNewColumn;
  LabelChoiceField.caption:=StrChoiceField;
  BitBtnOk.caption:=StrYes;
  BitBtnCancel.caption:=StrCancel;
  restoreform(self);

end;

procedure TNewFieldForm.FormDestroy(Sender: TObject);
begin
  saveform(self);
end;

end.
