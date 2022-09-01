unit bvGetAlignmentUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, SysUtils, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QControls, 
{$endif}
  bvFormSaver,
  bvLocalization, Classes;

type
  TAlignmentForm = class(TForm)
    BitBtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    EditAlignment: TRadioGroup;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  AlignmentForm: TAlignmentForm;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}



procedure TAlignmentForm.FormDestroy(Sender: TObject);
begin
  saveform(self);
end;

procedure TAlignmentForm.FormCreate(Sender: TObject);
begin
  self.caption:=strAlignment;
  Self.EditAlignment.items[0]:=strLeftAlignment;
  Self.EditAlignment.items[1]:=strCenterAlignment;
  Self.EditAlignment.items[2]:=strRightAlignment;
  BitBtnOk.caption:=StrYes;
  BtnCancel.caption:=StrCancel;
  REstoreform(self);
end;

end.
