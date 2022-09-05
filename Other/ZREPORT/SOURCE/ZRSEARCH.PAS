unit ZRSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfmSearch = class(TForm)
    lbInvitation: TLabel;
    edSearchStr: TEdit;
    cbUseCase: TCheckBox;
    cbFromCurrent: TCheckBox;
    btCont: TBitBtn;
    btCancel: TBitBtn;
    btFind: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}
uses
  zrConst;

procedure TfmSearch.FormCreate(Sender: TObject);
begin
  Caption                    := LoadStr(szrSearchDlgCaption);
  lbInvitation .Caption      := LoadStr(szrSearchDlgInvitation);
  cbUseCase    .Caption      := LoadStr(szrSearchDlgIgnoreCase);
  cbFromCurrent.Caption      := LoadStr(szrSearchDlgFromCurrent);
  btFind       .Caption      := LoadStr(szrSearchDlgStartSearch);
  btCont       .Caption      := LoadStr(szrSearchDlgContinueSearch);
  btCancel     .Caption      := LoadStr(szrCancel);
end;

procedure TfmSearch.FormShow(Sender: TObject);
begin
  edSearchStr.SetFocus;
end;

end.

