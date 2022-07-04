unit UFormViewOptionsNextPrev;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormViewOptionsNextPrev = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    chkShowAll: TRadioButton;
    chkShowCurrent: TRadioButton;
    chkShowCustom: TRadioButton;
    chkTypeText: TCheckBox;
    chkTypeImages: TCheckBox;
    chkTypeMedia: TCheckBox;
    chkTypeWeb: TCheckBox;
    procedure chkShowAllClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormViewOptionsNextPrev.chkShowAllClick(Sender: TObject);
var
  en: boolean;
begin
  en:= chkShowCustom.Checked;
  chkTypeText.Enabled:= en;
  chkTypeImages.Enabled:= en;
  chkTypeMedia.Enabled:= en;
  chkTypeWeb.Enabled:= en;
end;

procedure TFormViewOptionsNextPrev.FormShow(Sender: TObject);
begin
  chkShowAllClick(Self);
end;

end.
