unit fDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, ColorPresets, StdCtrls, ColorListBox, ComCtrls,
  CheckLst, ColorCheckListBox, Grids, ValEdit, ColorValueListEditor,
  ColorStringGrid, ColorComboBox, ColorDrawGrid, DBGrids, ColorDBGrid,
  DBCtrls, ColorDBComboBox, ColorDBListBox;

type
  TfmDemoMain = class(TForm)
    dsPeople: TDataSource;
    ADOConnection: TADOConnection;
    quAdresses: TADOQuery;
    quCity: TADOQuery;
    quPeople: TADOQuery;
    dsAdresses: TDataSource;
    dsCity: TDataSource;
    PageControl: TPageControl;
    tsListBox: TTabSheet;
    ColorListBox1: TColorListBox;
    ColorListBox2: TColorListBox;
    ColorPresets_Default: TColorPresets;
    ColorPresets_red: TColorPresets;
    ColorPresets_blue: TColorPresets;
    ColorPresets_green: TColorPresets;
    ColorPresets_user: TColorPresets;
    ColorListBox3: TColorCheckListBox;
    ColorListBox4: TColorListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    tsAnotherControls: TTabSheet;
    ColorValueListEditor1: TColorValueListEditor;
    Label5: TLabel;
    ColorStringGrid1: TColorStringGrid;
    Label6: TLabel;
    ColorDrawGrid1: TColorDrawGrid;
    Label7: TLabel;
    Color_ComboBox1: TColor_ComboBox;
    Color_ComboBox2: TColor_ComboBox;
    Color_ComboBox3: TColor_ComboBox;
    Color_ComboBox4: TColor_ComboBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    tsDBControls: TTabSheet;
    ColorDBGrid1: TColorDBGrid;
    ColorDBListBox1: TColorDBListBox;
    ColorDBComboBox1: TColorDBComboBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmDemoMain: TfmDemoMain;

implementation

{$R *.dfm}

procedure TfmDemoMain.FormCreate(Sender: TObject);
begin
  ColorStringGrid1.Cells[2, 3] := 'Example of text';
  ColorStringGrid1.Cells[4, 2] := 'Example of text';
  ColorStringGrid1.Cells[4, 3] := 'Example of text';
end;

end.
