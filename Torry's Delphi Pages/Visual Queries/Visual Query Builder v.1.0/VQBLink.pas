unit VQBLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, VQBLocalize, ExtCtrls;

type
  TLinkDlg = class(TForm)
    Label1: TLabel;
    StaticText1: TStaticText;
    Label2: TLabel;
    StaticText2: TStaticText;
    Label3: TLabel;
    Label4: TLabel;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    btnOK: TButton;
    btnCancel: TButton;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
  end;

var
  LinkDlg: TLinkDlg;

implementation

{$R *.dfm}

{ TLinkForm }

constructor TLinkDlg.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= resLinkTitle;
  Label1.Caption:= resLinkTbl1;
  Label2.Caption:= resLinkFld1;
  Label3.Caption:= resLinkTbl2;
  Label4.Caption:= resLinkFld2;
  RadioGroup1.Caption:= resLinkJoinOp;
  btnOk.Caption:= resLinkOkBtn;
  btnCancel.Caption:= resLinkCancelBtn;
  RadioGroup2.Items.Add(resLinkJT1);
  RadioGroup2.Items.Add(resLinkJT2);
  RadioGroup2.Items.Add(resLinkJT3);
  RadioGroup2.Items.Add(resLinkJT4);
  RadioGroup2.ItemIndex:= 0;
end;

end.
