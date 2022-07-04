unit fb_ib_edt_ins_master_field_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type

  { TeditorInsertMasterFieldForm }

  TeditorInsertMasterFieldForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ListBox1: TListBox;
    procedure ListBox1DblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  editorInsertMasterFieldForm: TeditorInsertMasterFieldForm;

implementation

{ TeditorInsertMasterFieldForm }

procedure TeditorInsertMasterFieldForm.ListBox1DblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

initialization
  {$I fb_ib_edt_ins_master_field_unit.lrs}

end.

