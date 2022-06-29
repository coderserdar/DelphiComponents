unit ListFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrmList = class(TForm)
    lst: TListBox;
    Bevel1: TBevel;
    Button1: TButton;
    procedure lstClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmList: TfrmList;

implementation

{$R *.DFM}

procedure TfrmList.lstClick(Sender: TObject);
begin
  Button1.Enabled := lst.ItemIndex<>-1;
end;

procedure TfrmList.FormShow(Sender: TObject);
begin
  Button1.Enabled := lst.ItemIndex<>-1;
end;

end.
