unit uFormJsonViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ActnList, StdActns, ImgList, Menus,
  ComCtrls, jsontreeview, jsondoc;

type
  TFormJsonViewer = class(TForm)
    Panel1: TPanel;
    ActionList1: TActionList;
    ImageList1: TImageList;
    EditPaste1: TEditPaste;
    EditClear1: TEditDelete;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ActionToggleVisibleChildrenCounts: TAction;
    ActionToggleVisibleByteSizes: TAction;
    PopupMenu1: TPopupMenu;
    VisibleChildrenCounts1: TMenuItem;
    VisibleByteSizes1: TMenuItem;
    JSONTreeView1: TJSONTreeView;
    JSONDocument1: TJSONDocument;
    procedure ActionToggleVisibleChildrenCountsExecute(Sender: TObject);
    procedure ActionToggleVisibleByteSizesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure EditClear1Execute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormJsonViewer: TFormJsonViewer;

implementation

uses Clipbrd;

{$R *.dfm}

procedure TFormJsonViewer.FormCreate(Sender: TObject);
begin
  ActionToggleVisibleByteSizes.Checked := JSONTreeView1.VisibleByteSizes;
  ActionToggleVisibleChildrenCounts.Checked := JSONTreeView1.VisibleChildrenCounts;
end;

procedure TFormJsonViewer.ActionToggleVisibleByteSizesExecute(Sender: TObject);
begin
  JSONTreeView1.VisibleByteSizes := not JSONTreeView1.VisibleByteSizes;
end;

procedure TFormJsonViewer.ActionToggleVisibleChildrenCountsExecute(Sender: TObject);
begin
  JSONTreeView1.VisibleChildrenCounts := not JSONTreeView1.VisibleChildrenCounts;
end;

procedure TFormJsonViewer.EditClear1Execute(Sender: TObject);
begin
  JSONDocument1.JsonText := '';
  JSONTreeView1.LoadJson;
end;

procedure TFormJsonViewer.EditPaste1Execute(Sender: TObject);
begin
  JSONDocument1.JsonText := Clipboard.AsText;

  JSONTreeView1.Items.BeginUpdate;
  JSONTreeView1.LoadJson;
  JSONTreeView1.Items.EndUpdate;
end;

end.
