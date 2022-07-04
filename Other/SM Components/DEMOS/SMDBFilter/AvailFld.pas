unit AvailFld;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, checklst;

type
  TfrmAvailFields = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    CheckListBox: TCheckListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAvailFields: TfrmAvailFields;

implementation

{$R *.DFM}

end.
