unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, ImgList, ActnList;

type
  TfrmDesign = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Button1: TButton;
    ListView1: TListView;
    ListBox1: TListBox;
    PopupMenu1: TPopupMenu;
    Shape1: TShape;
    PopupMenu2: TPopupMenu;
    N11: TMenuItem;
    N21: TMenuItem;
    ActionList1: TActionList;
    Action1: TAction;
    Action2: TAction;
    ImageList1: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDesign: TfrmDesign;

implementation

{$R *.dfm}

end.
