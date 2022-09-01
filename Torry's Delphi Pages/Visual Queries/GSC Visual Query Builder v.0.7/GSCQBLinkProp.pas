unit GSCQBLinkProp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TGSCQBLinkPropForm = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Table1: TEdit;
    Table2: TEdit;
    Field1: TEdit;
    Field2: TEdit;
    JoinOp: TComboBox;
    GroupBox1: TGroupBox;
    CBAllFrom1: TCheckBox;
    CBAllFrom2: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
  end;

implementation

{$R *.DFM}

end.
