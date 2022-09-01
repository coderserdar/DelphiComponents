
{$IFDEF VER120}
  {$DEFINE USEINTERFACE}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE USEINTERFACE}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE USEINTERFACE}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE USEINTERFACE}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE USEINTERFACE}
{$ENDIF}

{$IFDEF VER140} //D6
{$DEFINE D6D7}
{$ENDIF}
{$IFDEF VER150} //D7
{$DEFINE D6D7}
{$ENDIF}

unit cellsdlg;
(* When this, cbx component's component editor, is activated the dd
properties of the control are assigned to the component editor's form
and to the test cbx control on the form. This test cbx control is then
updated as edits are made using this component editor form. When the OK
button is pressed the properties from the test cbx control are assigned
back to the cbx control that opened this editor.

The exception to this is the data in the dropdown data. The master copy of this
data is moved from the component directly into the grid when the this form is
opened and not into the test cbx. This is so the data can be kept in its unsorted
state.

  From Component to TestComponent in FormShow using CopyCBXProperties
  From TestComponent to Form in FormShow using field assignments
  From Form to Test Component in
    SpinButtonDownClick,
    SpinButtonUpClick,
    edExit,   & XXXX       using CopyFormtoCBX
  From Test Component to original componen in btnOKClick using UpDateFormDesigner *)
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Grids, Mask, Spin, 
{$IFDEF D6D7} //D6
   DesignIntf,
   //DesignEditors,
{$ELSE}
   DsgnIntf,
{$ENDIF}
  Dialogs, CBXBase, Dblup1a;

type
  TCellsForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edRowCount: TEdit;
    edCount: TEdit;
    cbAlign: TComboBox;
    cbTop: TComboBox;
    ckbSorted: TCheckBox;
    btnTitleColor: TButton;
    btnFont: TButton;
    btnColor: TButton;
    edWidth: TEdit;
    edDefaultRowHeight: TEdit;
    edFixedRows: TEdit;
    ckbDefaultDrawing: TCheckBox;
    ckbParentColor: TCheckBox;
    ckbParentFont: TCheckBox;
    edColCount: TEdit;
    GroupBox1: TGroupBox;
    ckbloColLines: TCheckBox;
    ckbloRowLines: TCheckBox;
    ckbloThumbTracking: TCheckBox;
    SpinButton1: TSpinButton;
    SpinButton2: TSpinButton;
    SpinButton3: TSpinButton;
    SpinButton4: TSpinButton;
    SpinButton5: TSpinButton;
    SpinButton6: TSpinButton;
    Label3: TLabel;
    cbCursor: TComboBox;
    btnOK: TButton;
    btnClear: TButton;
    StringGrid1: TStringGrid;
    btnCancel: TButton;
    ColorDialog1: TColorDialog;
    ColorDialog2: TColorDialog;
    FontDialog1: TFontDialog;
    Panel1: TPanel;
    Memo1: TMemo;
    CBXTest: TComboBoxPlus;
    procedure btnClearClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinButtonDownClick(Sender: TObject);
    procedure SpinButtonUpClick(Sender: TObject);
    procedure edKeyPress(Sender: TObject; var Key: Char);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Longint;
      const Value: String);
    procedure edExit(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure btnTitleColorClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
  private
    { Private declarations }

{$IFDEF USEINTERFACE}
   {$IFDEF D6D7}
    fDesigner : IDesigner;
   {$ELSE}
    fDesigner : IFormDesigner;
   {$ENDIF}
{$ELSE}
    fDesigner : TFormDesigner;
{$ENDIF}

    fComponent : TComboBoxPlus;
    procedure CopyCBXProperties(src, Dst  : TComboBoxPlus);
    procedure CopyFormtoCBX(CBX : TComboBoxPlus);
    procedure UpdateGrid;
{    procedure UpdateTestCBX; }
    procedure UpdateFormDesigner;
  public
    { Public declarations }

{$IFDEF USEINTERFACE}
   {$IFDEF D6D7}
    property DlgDesigner : IDesigner read fDesigner write fDesigner;
   {$ELSE}
    property DlgDesigner : IFormDesigner read fDesigner write fDesigner;
   {$ENDIF}
{$ELSE}
    property DlgDesigner : TFormDesigner read fDesigner write fDesigner;
{$ENDIF}
    property DlgComponent : TComboBoxPlus read fComponent write fComponent;
  end;

var
  CellsForm: TCellsForm;

implementation

{$R *.DFM}


procedure TCellsForm.CopyCBXProperties(src, Dst  : TComboBoxPlus);
{ basically an assign for dd properties }
(*
var
  C, R : Integer;
*)  
begin
  with src do
  begin
    Dst.ColCount := ColCount;
    Dst.RowCount := RowCount;
    Dst.DropDownCount    := DropDownCount;
    Dst.DropDownWidth    := DropDownWidth;
    Dst.DefaultRowHeight := DefaultRowHeight;
    Dst.FixedRows := FixedRows;
    Dst.DropDownAlign := DropDownAlign;
    Dst.DropDownTop := DropDownTop;
    Dst.Options := Options;
    Dst.ListCursor := ListCursor;
    Dst.ListColor := ListColor;
    Dst.TitleColor := TitleColor;
    Dst.ListParentColor := ListParentColor;
    Dst.ListFont.assign(ListFont);
    Dst.ListParentFont := ListParentFont;
    Dst.Sorted := Sorted;
    { Fill the grid with data }
    (*
    For R := 0 to (ddRowCount-1) do
      For C := 0 to (ddColCount-1) do
        Dst.Cells[C,R] := Cells[C,R];
     *)
  end;
end;


procedure TCellsForm.CopyFormtoCBX(CBX : TComboBoxPlus);
{ Sets properties in CBX parameter to values in this forms fields }
begin
  with CBX do
  begin
    ColCount := StrToInt(edColCount.text);
    RowCount := StrToInt(edRowCount.text);
    DropDownCount    := StrToInt(edCount.text);
    DropDownWidth    := StrToInt(edWidth.text);
    DefaultRowHeight := StrToInt(edDefaultRowHeight.text);
    FixedRows := StrToInt(edFixedRows.text);
    DropDownAlign    := TLeftRight(cbAlign.itemindex);
    DropDownTop      := TBelowAbove(cbTop.itemindex);
    ListCursor   := StringToCursor(cbCursor.text);
    ListColor    := ColorDialog1.Color;
    ListParentColor := ckbParentColor.checked;
    TitleColor := ColorDialog2.Color;
    ListFont := FontDialog1.Font;
    ListParentFont := ckbParentFont.checked;
    Sorted := ckbSorted.Checked;
    if ckbloColLines.Checked then
      Options := Options + [loColLines]
    else
      Options := Options - [loColLines];

    if ckbloRowLines.Checked then
      Options := Options + [loRowLines]
    else
      Options := Options - [loRowLines];

    if ckbloThumbTracking.Checked then
      Options := Options + [loThumbTracking]
    else
      Options := Options - [loThumbTracking]
  end;
end;

procedure TCellsForm.UpdateFormDesigner;
var
 R, C : Integer;

begin
  { UpDate FormDesigner }
  if assigned(fComponent) then
  begin
    CopyCBXProperties(CBXTest, fComponent);
    {now copy the data from the grid to the component}
    For R := 0 to (StringGrid1.RowCount-1) do
      For C := 0 to (StringGrid1.ColCount-1) do
        fComponent.Cells[C,R] := StringGrid1.Cells[C,R];
  end;
  if assigned(fDesigner) then fDesigner.Modified;

end;

procedure TCellsForm.UpdateGrid;
{Update the data entry grid component when ever the number of row, column or
 fixedrows changes}
begin
  { UpDate Grid if necessary }
  with StringGrid1 do
  begin
    ColCount := StrToInt(edColCount.text);
    RowCount := StrToInt(edRowCount.text);
  end;
end;

procedure TCellsForm.btnClearClick(Sender: TObject);
begin
{  messagebeep(1); }
end;

procedure TCellsForm.FormShow(Sender: TObject);
{ first copy data into the test component then from the test component into the
  form's fields }
var
  R,C : Integer;
begin
  { First make the Test controls font the same as the components font and color
    so the dd has the same parent font and color.}
  CbxTest.Color := fcomponent.color;
  CbxTest.font.assign(fcomponent.font);
  { from fComponent to cbxTest }
  CopyCBXProperties(fComponent, cbxTest);
  { from cbxTest to form fields }
    { Grid Column Count }
  StringGrid1.ColCount := cbxTest.ColCount;
  edColCount.text := IntToStr(StringGrid1.ColCount);
    { Grid Row Count }
  StringGrid1.RowCount := cbxTest.RowCount;
  edRowCount.text := IntToStr(StringGrid1.RowCount);
    { Grid Fixed Rows (these are the row(s)) }
  edFixedRows.text := IntToStr(cbxTest.FixedRows);
    { Grid Drop Down Count - (this number or records that display) }
  edCount.text    := IntToStr(cbxTest.DropDownCount);
    { Grid Width }
  edWidth.text    := IntToStr(cbxTest.DropDownWidth);
    { Grid Default Row Height }
  edDefaultRowHeight.text := IntToStr(cbxTest.DefaultRowHeight);
    { Alignment to the left or right of the edit }
  cbAlign.itemindex := ord(cbxTest.DropDownAlign);
    { Alignment to the top or bottom of the edit }
  cbTop.itemindex := ord(cbxTest.DropDownTop);
    { Grid Cursor property }
  cbCursor.itemIndex := cbCursor.items.IndexOf(CursorToString(cbxTest.ListCursor));
    { Grid Color }
  ColorDialog1.Color := cbxTest.ListColor;
  ckbParentColor.checked := CBXTest.ListParentColor;
    { Grid Title Color }
  ColorDialog2.Color := cbxTest.TitleColor;
    { Grid Parent Font }
  ckbParentFont.checked := CBXTest.ListParentFont;
    { Grid Font }
  FontDialog1.Font := cbxTest.ListFont;
    { Grid Options }
  ckbloColLines.Checked := (loColLines in cbxTest.Options);
  ckbloRowLines.Checked := (loRowLines in cbxTest.Options);
  ckbloThumbTracking.Checked := (loThumbTracking in cbxTest.options);
  { Sorted Property }
  ckbSorted.checked := CBXTest.Sorted;
  { Fill the grid with data from the component}
  For R := 0 to (StringGrid1.RowCount-1) do
    For C := 0 to (StringGrid1.ColCount-1) do
      StringGrid1.Cells[C,R] := fComponent.Cells[C,R];
end;

procedure TCellsForm.SpinButtonDownClick(Sender: TObject);
var
 I : Integer;
begin
  if Sender is TSpinButton then
  begin
    I := StrToInt(TEdit(TSpinButton(Sender).FocusControl).Text);
    If not (pred(I) < 0) then
      dec(I);
    TEdit(TSpinButton(Sender).FocusControl).Text := IntToStr(I);
  end;
  CopyFormToCBX(CBXTest);
  UpdateGrid;
end;

procedure TCellsForm.SpinButtonUpClick(Sender: TObject);
var
 I : Integer;
begin
  if Sender is TSpinButton then
  begin
    I := StrToInt(TEdit(TSpinButton(Sender).FocusControl).Text);
    If not (succ(I) >= MaxInt) then
      inc(I);
    TEdit(TSpinButton(Sender).FocusControl).Text := IntToStr(I);
  end;
  CopyFormToCBX(CBXTest);
  UpdateGrid;
end;

procedure TCellsForm.edKeyPress(Sender: TObject; var Key: Char);
{ Validate of integer fields }
begin {he he}
  case Key of
    #48..#57 : ;
    else
      Key := #0;
  end;
end;

procedure TCellsForm.StringGrid1SetEditText(Sender: TObject; ACol,
  ARow: Longint; const Value: String);
begin
  CBXTest.Cells[aCol,aRow] := StringGrid1.Cells[aCol,aRow];
  UpdateFormDesigner;
end;

procedure TCellsForm.edExit(Sender: TObject);
begin
  UpdateGrid;
  CopyFormToCBX(CBXTest);
end;

procedure TCellsForm.btnOKClick(Sender: TObject);
begin
  { First make sure that the columns sized etc }
{  CBXTest.SizeGridToData; }
  UpdateFormDesigner;
end;

procedure TCellsForm.FormCreate(Sender: TObject);
type
  TGetStrFunc = function(const Value: string): Integer of object;
var
  Proc : TGetStrFunc;
begin
  {pass the items.add function off to GetCursorValues (from controls unit) to
   fill the cbCursor controls list with valid cursors }
  Proc := cbCursor.Items.add;
  GetCursorValues(TGetStrProc(Proc));

  CBXTest.Visible := False;
end;

procedure TCellsForm.btnColorClick(Sender: TObject);
var
  C : TColor;
begin
  ColorDialog1.Color := cbxTest.ListColor;
  C := cbxTest.ListColor;
  If ColorDialog1.Execute then
  begin
    if C <> ColorDialog1.Color then
    begin
      CBXTest.ListColor := ColorDialog1.Color;
      ckbParentColor.Checked := False;
    end;
  end;
end;

procedure TCellsForm.btnTitleColorClick(Sender: TObject);
begin
  If ColorDialog2.Execute then
  CBXTest.TitleColor := ColorDialog2.Color;
end;

procedure TCellsForm.btnFontClick(Sender: TObject);
var
  F : TFont;
begin
  FontDialog1.Font := cbxTest.ListFont;
  F := TFont.Create;
  F.Assign(cbxTest.ListFont);
  If FontDialog1.Execute then
  begin
    if (F.name <> FontDialog1.Font.name) or
       (F.Color <> FontDialog1.Font.Color) or
       (F.Size <> FontDialog1.Font.Size) or
       (F.Style <> FontDialog1.Font.Style) then
    begin
      CBXTest.ListFont := FontDialog1.Font;
      ckbParentFont.Checked := False;
    end;
  end;
  F.Free;
end;

end.



