unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ExtCtrls,
  { gettext }
  gnugettext,
  { TB2K }
  TB2Item, TB2Dock, TB2Toolbar, SpTBXItem, SpTBXControls, SpTBXDkPanels;

type
  TForm2 = class(TForm)
    SpTBXDock1: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    SpTBXButton1: TSpTBXButton;
    SpTBXButton2: TSpTBXButton;
    SpTBXButton3: TSpTBXButton;
    ImageList1: TImageList;
    tProperties: TSpTBXItem;
    tPageSetup: TSpTBXItem;
    tHistory: TSpTBXItem;
    tFavorites: TSpTBXItem;
    tCustomize: TSpTBXItem;
    ClientPanel: TPanel;
    LeftPanel: TPanel;
    TitlePanel: TSpTBXLabel;
    ButtonsPanel: TSpTBXPanel;
    procedure SideBarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  Unit1;

{$R *.dfm}

procedure TForm2.ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm2.SideBarClick(Sender: TObject);
var
  Item: TSpTBXItem;
begin
  if Sender is TSpTBXItem then begin
    Item := Sender as TSpTBXItem;
    Item.Checked := True;
    TitlePanel.Caption := Item.Caption;
    ClientPanel.Visible := Item = tCustomize;
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // We need to close the customizer form manually here
  Form1.SpTBXCustomizer1.Close;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  gnugettext.TranslateComponent(Self);
end;

end.
