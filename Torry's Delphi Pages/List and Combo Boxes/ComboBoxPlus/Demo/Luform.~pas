unit Luform;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, DBGrids,
  DBLookup, Tabs, ExtCtrls,  Spin, NewRec, DB, DBTables,
  DBCtrls, Grids, Buttons, Dblup1a, Dblup1b, Cbxbase;

type
  TForm1 = class(TForm)
    Notebook1: TNotebook;
    TabSet1: TTabSet;
    RadioGroupStylePg2: TRadioGroup;
    Table1: TTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Bevel1: TBevel;
    CheckBoxLUActive: TCheckBox;
    CheckBoxAutoDropDown: TCheckBox;
    CheckBoxHideBorder: TCheckBox;
    CheckBoxCtl3D: TCheckBox;
    CheckBoxSpeedButton: TCheckBox;
    CheckBoxAlignHorz: TCheckBox;
    CheckBoxAlignVert: TCheckBox;
    CheckBoxListTitles: TCheckBox;
    CheckBoxListColLines: TCheckBox;
    CheckBoxListRowLines: TCheckBox;
    SpinButton1: TSpinButton;
    FontDialog1: TFontDialog;
    ButtonFieldColor: TButton;
    ColorDialog1: TColorDialog;
    ButtonFieldFont: TButton;
    ButtonListFont: TButton;
    ButtonListColor: TButton;
    ButtonTitle: TButton;
    Memo1: TMemo;
    DBComboBoxPlus1: TDBComboBoxPlus;
    Bevel2: TBevel;
    SpinButton2: TSpinButton;
    ComboBoxPlus1: TComboBoxPlus;
    CheckBoxLUActive2: TCheckBox;
    RadioGroupStyle2: TRadioGroup;
    CheckBoxDropDown2: TCheckBox;
    CheckBoxBorder2: TCheckBox;
    CheckBoxCtrl3D2: TCheckBox;
    CheckBoxSpeedButton2: TCheckBox;
    CheckBoxHrozAlign2: TCheckBox;
    CheckBoxVertAlign2: TCheckBox;
    CheckBoxTitles2: TCheckBox;
    CheckBoxColLines2: TCheckBox;
    CheckBoxRowLines2: TCheckBox;
    ButtonFieldColor2: TButton;
    ButtonFieldFont2: TButton;
    ButtonListColor2: TButton;
    ButtonListFont2: TButton;
    ButtonTitleColor2: TButton;
    Memo2: TMemo;
    LabelValue: TLabel;
    Label2Value: TLabel;
    MemoPg0: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure TabSet1Click(Sender: TObject);
    procedure RadioGroupStylePg2Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure CheckBoxLUActiveClick(Sender: TObject);
    procedure CheckBoxAutoDropDownClick(Sender: TObject);
    procedure CheckBoxHideBorderClick(Sender: TObject);
    procedure CheckBoxCtl3DClick(Sender: TObject);
    procedure CheckBoxSpeedButtonClick(Sender: TObject);
    procedure CheckBoxAlignHorzClick(Sender: TObject);
    procedure CheckBoxAlignVertClick(Sender: TObject);
    procedure CheckBoxListTitlesClick(Sender: TObject);
    procedure CheckBoxListColLinesClick(Sender: TObject);
    procedure CheckBoxListRowLinesClick(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure ButtonFieldColorClick(Sender: TObject);
    procedure ButtonFieldFontClick(Sender: TObject);
    procedure ButtonListFontClick(Sender: TObject);
    procedure ButtonListColorClick(Sender: TObject);
    procedure ButtonTitleClick(Sender: TObject);
    procedure DBComboBoxPlus1NewLookupRec(Sender: TObject;
      var Cancelled: Boolean);
    procedure ComboBoxPlus1Change(Sender: TObject);
    procedure ComboBoxPlus1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBoxPlus1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    ComboBox : TComboBoxPlus;
    procedure LoadDropDownData(Combo: TComboBoxPlus; ColsOfData: Integer);
    procedure CreateDemoTable;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.LoadDropDownData(Combo : TComboBoxPlus; ColsOfData : Integer);
begin
  {Flush the old data}
  Combo.ClearGridData;
  {Make sure the allocated storage is big enough}
  Combo.RowCount := 20;
  Combo.ColCount := ColsOfData + 1;
  {Load the data}

  Combo.AddRow(['0',   'Company'               ,'Years'              ,'Owner']);
  Combo.AddRow(['1221','Kauai Dive Shoppe'     ,'12'                 ,'Paul' ]);
  Combo.AddRow(['1680','Island Finders'        ,''                   ,'Willie']);
  Combo.AddRow(['3158','Action Divers Supply'  ,'2'                  ,'Matt' ]);
  Combo.AddRow(['2135','Frank''s Divers Supply','8'                  ,'Frank']);
  Combo.AddRow(['5515','Ocean Adventures'      ,'234'                ,'Sally']);
  Combo.AddRow(['3042','Gold Coast Supply'     ,'4'                  ,'Lucy' ]);
  Combo.AddRow(['5432','Divers-for-Hire'       ,'6'                  ,'Laura']);
  Combo.AddRow(['5384','Tora Tora Tora'        ,'A very long time'   ,'Ben'  ]);
  Combo.AddRow(['1231','Unisco'                ,'4'                  ,'Susan']);
  Combo.AddRow(['6312','Aquatic Drama'         ,'26'                 ,'Gizmo']);
  Combo.AddRow(['1563','Blue Sports'           ,'13'                 ,'Jim'  ]);
  Combo.AddRow(['1351','Sight Diver'           ,'5'                  ,'Juice']);
  Combo.AddRow(['1384','VIP Divers Club'       ,'32'                 ,''     ]);

  {Now shring the grid so its just big enough for the data}
  Combo.SizeGridToData;
end;

procedure TForm1.CreateDemoTable;
begin
  with Table1 do
  begin
    Active := False;
    TableType := ttParadox;
    with FieldDefs do
      begin
      Clear;
      Add('Company', ftString, 50, True);
    end;
    CreateTable;
    Active := True;
    Insert;
    SetFields(['1221']);
    Post;
    Insert;
    SetFields(['5384']);
    Post;
    Insert;
    SetFields(['1563']);
    Post;
  end;
 end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  LoadDropDownData(DBComboBoxPlus1, 3);
  DBComboBoxPlus1.FixedRows := 1;
  LoadDropDownData(ComboBoxPlus1, 3);
  ComboBoxPlus1.FixedRows := 1;
  CreateDemoTable;
  ComboBox := DBComboBoxPlus1;
  Notebook1.PageIndex :=  0;
  TabSet1.TabIndex := 0;
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
  Table1.Active := False;
  Table1.DeleteTable;
end;

procedure TForm1.TabSet1Click(Sender: TObject);
begin
  Notebook1.PageIndex := TabSet1.TabIndex;
  case Notebook1.PageIndex of
   0 : exit;
   1 : ComboBox := DBComboBoxPlus1;
   2 : ComboBox := ComboBoxPlus1;
  end;
  ComboBox.SetFocus;
end;

procedure TForm1.RadioGroupStylePg2Click(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0 : ComboBox.Style := TComboPlusStyle(csIncSearch);
    1 : ComboBox.Style := TComboPlusStyle(csIncSrchEdit);
  end; {case}
  { get fresh data for the new drop down style }
  LoadDropDownData(ComboBox,3);
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxLUActiveClick(Sender: TObject);
begin
  ComboBox.LookupActive := TCheckBox(Sender).Checked;
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxAutoDropDownClick(Sender: TObject);
begin
  ComboBox.AutoDropDown := TCheckBox(Sender).Checked;
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxHideBorderClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    ComboBox.BorderStyle := bsNone
  else
    ComboBox.BorderStyle := bsSingle;
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxCtl3DClick(Sender: TObject);
begin
  ComboBox.Ctl3D := TCheckBox(Sender).Checked;
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxSpeedButtonClick(Sender: TObject);
begin
  ComboBox.ShowSpeedButton := TCheckBox(Sender).Checked;
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxAlignHorzClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    ComboBox.DropDownAlign := Cbxbase.Left
  else
    ComboBox.DropDownAlign := Cbxbase.Right;
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxAlignVertClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    ComboBox.DropDownTop := Cbxbase.Above
  else
    ComboBox.DropDownTop := Cbxbase.Below;
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxListTitlesClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    ComboBox.FixedRows := 1
  else
    ComboBox.FixedRows := 0;
  LoadDropDownData(ComboBox, 3);
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxListColLinesClick(Sender: TObject);
begin
  If TCheckBox(Sender).Checked then
    ComboBox.Options := ComboBox.Options + [TStringGridPOptions(loColLines)]
  else
    ComboBox.Options := ComboBox.Options - [TStringGridPOptions(loColLines)];
  ComboBox.SetFocus;
end;

procedure TForm1.CheckBoxListRowLinesClick(Sender: TObject);
begin
  If TCheckBox(Sender).Checked then
    ComboBox.Options := ComboBox.Options + [TStringGridPOptions(loRowLines)]
  else
    ComboBox.Options := ComboBox.Options - [TStringGridPOptions(loRowLines)];
  ComboBox.SetFocus;
end;

procedure TForm1.SpinButton1DownClick(Sender: TObject);
begin
  ComboBox.Height := ComboBox.Height + 1;
  Case TabSet1.TabIndex of
    1 : SpinButton1.Height := ComboBox.Height;
    2 : SpinButton2.Height := ComboBox.Height;
   end; {case}
end;

procedure TForm1.SpinButton1UpClick(Sender: TObject);
begin
  ComboBox.Height := ComboBox.Height - 1;
  Case TabSet1.TabIndex of
    1 : SpinButton1.Height := ComboBox.Height;
    2 : SpinButton2.Height := ComboBox.Height;
   end; {case}
end;

procedure TForm1.ButtonFieldColorClick(Sender: TObject);
begin
  ColorDialog1.Color := ComboBox.Color;
  If ColorDialog1.Execute then
    ComboBox.Color := ColorDialog1.Color;
  ComboBox.SetFocus;
end;

procedure TForm1.ButtonFieldFontClick(Sender: TObject);
begin
  FontDialog1.Font := ComboBox.Font;
  If FontDialog1.Execute then
    ComboBox.Font := FontDialog1.Font;
  ComboBox.SetFocus;
end;

procedure TForm1.ButtonListFontClick(Sender: TObject);
begin
  FontDialog1.Font := ComboBox.ListFont;
  If FontDialog1.Execute then
  begin
    ComboBox.ListFont := FontDialog1.Font;
    ComboBox.DefaultRowHeight := ComboBox.ListFont.Size + ComboBox.ListFont.Size div 3;
  end;
  ComboBox.SizeGridToData;
  ComboBox.SetFocus;
end;

procedure TForm1.ButtonListColorClick(Sender: TObject);
begin
  ColorDialog1.Color := ComboBox.ListColor;
  If ColorDialog1.Execute then
    ComboBox.ListColor := ColorDialog1.Color;
  ComboBox.SetFocus;
end;

procedure TForm1.ButtonTitleClick(Sender: TObject);
begin
  ColorDialog1.Color := ComboBox.TitleColor;
  If ColorDialog1.Execute then
    ComboBox.TItleColor := ColorDialog1.Color;
  ComboBox.SetFocus;
end;

procedure TForm1.DBComboBoxPlus1NewLookupRec(Sender: TObject;
  var Cancelled: Boolean);
begin
  Cancelled := False;                 {Set cancelled to false initially}
  {make room for new record}
  ComboBox.RowCount := ComboBox.RowCount + 1;
  {Fillin the company field}
  FormNewRec.EditCompany.Text := ComboBox.DisplayValue;

  FormNewRec.ShowModal;                { display the dialog box }
  if FormNewRec.ModalResult=mrOK then  {if user pressed OK then save the new SubCat}
  begin
    ComboBox.AddRow([FormNewRec.EditIDNo.Text,
                     FormNewRec.EditCompany.Text,
                     FormNewRec.EditYears.Text,
                     FormNewRec.EditOwner.Text]);
    ComboBox.DisplayValue := FormNewRec.EditCompany.Text;
  end
  else
  begin
    ComboBox.DisplayValue := '';
    Cancelled := True;
  end;
  {shrink the grid so it just fits the data}
  ComboBox.SizeGridToData;
end;

procedure TForm1.ComboBoxPlus1Change(Sender: TObject);
begin
  LabelValue.Caption := ComboBoxPlus1.Value;
end;

procedure TForm1.ComboBoxPlus1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  messagebeep(1);
end;

procedure TForm1.ComboBoxPlus1KeyPress(Sender: TObject; var Key: Char);
begin
  messagebeep(1);

end;

end.


