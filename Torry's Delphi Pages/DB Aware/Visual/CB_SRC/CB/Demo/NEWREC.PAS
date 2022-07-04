unit Newrec;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls;

type
  TFormNewRec = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Bevel1: TBevel;
    Label1: TLabel;
    EditIDNo: TEdit;
    Label2: TLabel;
    EditCompany: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    EditYears: TEdit;
    EditOwner: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNewRec: TFormNewRec;

implementation

{$R *.DFM}

end.
