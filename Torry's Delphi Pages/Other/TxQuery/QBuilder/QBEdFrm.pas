unit QBEdFrm;

{$i xq_flag.inc}
interface

uses Windows, StdCtrls, ComCtrls, Controls, Classes, Buttons,
     ExtCtrls, QBuilder, Forms, ImgList, SysUtils,
     DB, Dialogs, xquery;

type
  TfrmEdQBField = class(TForm)
    OKBtn: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    BtnDn: TSpeedButton;
    BtnUp: TSpeedButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    CboShow: TComboBox;
    Label3: TLabel;
    CboSort: TComboBox;
    GroupBox1: TGroupBox;
    CboFilterAction: TComboBox;
    LblData1: TLabel;
    LblData2: TLabel;
    LblData3: TLabel;
    LblData4: TLabel;
    TreeView1: TTreeView;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    Label10: TLabel;
    EdCustom: TEdit;
    Button2: TButton;
    Button3: TButton;
    EdData1: TComboBox;
    EdData2: TComboBox;
    EdData3: TComboBox;
    EdData4: TComboBox;
    EdData5: TComboBox;
    procedure CboFilterActionChange(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    procedure BtnDnClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FOQBField, FSaveQBField: TOQBField;
    FCurrentFilter: Integer;
    FList : TList;
    procedure ShowFilter;
    procedure SaveCurrentFilter;
  public
    { Public declarations }
    function Enter(Query: TDataSet; OQBField: TOQBField; StartFilter: Integer): Word;
  end;

implementation

{$R *.DFM}

uses
   xqconsts;

function TfrmEdQBField.Enter(Query: TDataSet; OQBField: TOQBField;
   StartFilter: Integer): Word;
var
  DataSet: TDataSet;
  i,j : Integer;
  TreeNode : TTreeNode;
  s: string;
begin
   FOQBField := TOQBField.Create(nil);
   FSaveQBField := OQBField;
   FOQBField.Assign(OQBField);
   Caption := Format('Edit [%s.%s]',[OQBField.Table,OQBField.FieldName]);
   FList := TList.Create;
   FList.Add(EdData1);
   FList.Add(EdData2);
   FList.Add(EdData3);
   FList.Add(EdData4);
   FList.Add(EdData5);
   { show data }
   with OQBField do
   begin
      Edit1.Text := Alias;
      CboShow.ItemIndex:= Ord(ShowAction);
      CboSort.ItemIndex:= Ord(SortType);
      FCurrentFilter:= StartFilter;
      ShowFilter;
      {other page}
      { populate TreeView1 }
      for i := 0 to (Query as TxQuery).DataSets.Count - 1 do
      begin
          DataSet := (Query as TxQuery).DataSets[i].DataSet;
          if DataSet.Active then
          begin
             with TreeView1.Items do
             begin
               TreeNode := Add(nil, (Query as TxQuery).DataSets[i].Alias);
               for j := 0 to DataSet.FieldCount - 1 do
                  AddChild(TreeNode,DataSet.Fields[j].FieldName);
             end;
          end;
      end;
   end;

   Result := ShowModal;
end;

procedure TfrmEdQBField.ShowFilter;
begin
   GroupBox1.Caption:=Format('Filter %d of 5',[Succ(FCurrentFilter)]);
   CboFilterAction.ItemIndex:= Ord(FOQBField.Filters[fCurrentFilter].FilterAction);
   EdCustom.Text:= FOQBField.Filters[fCurrentFilter].CustomExpres;
   CboFilterAction.OnChange(nil);
end;

procedure TfrmEdQBField.CboFilterActionChange(Sender: TObject);
var
   i : Integer;
begin
   { visibility of controls }
   for i:= 1 to 5 do
      TComboBox(FList[i-1]).Text:= FOQBField.Filters[fCurrentFilter].Data[i];
   case TOQBFilterAction(CboFilterAction.ItemIndex) of
      faIsEqualTo, faIsLike, faIsNotEqualTo, faIsNotLike :
        begin
          EdData1.Visible :=True;
          EdData2.Visible :=True;
          EdData3.Visible :=True;
          EdData4.Visible :=True;
          EdData5.Visible :=True;
          LblData1.Visible:=True; LblData1.Caption:='Or';
          LblData2.Visible:=True;
          LblData3.Visible:=True;
          LblData4.Visible:=True;
        end;
      faIsBetween, faIsNotBetween :
        begin
          EdData1.Visible :=True;
          EdData2.Visible :=True;
          EdData3.Visible :=False;
          EdData4.Visible :=False;
          EdData5.Visible :=False;
          LblData1.Visible:=True; LblData1.Caption:='And';
          LblData2.Visible:=False;
          LblData3.Visible:=False;
          LblData4.Visible:=False;
        end;
      faIsGreaterThan, faIsGreaterEqualTo, faIsLessThan,
      faIsLessEqualTo :
        begin
          EdData1.Visible :=True;
          EdData2.Visible :=False;
          EdData3.Visible :=False;
          EdData4.Visible :=False;
          EdData5.Visible :=False;
          LblData1.Visible:=False;
          LblData2.Visible:=False;
          LblData3.Visible:=False;
          LblData4.Visible:=False;
        end;
   end;
end;

procedure TfrmEdQBField.ListBox1DblClick(Sender: TObject);
begin
   EdCustom.SelText := ListBox1.Items[ListBox1.ItemIndex];
end;

procedure TfrmEdQBField.BtnUpClick(Sender: TObject);
begin
   SaveCurrentFilter;
   if FCurrentFilter > 0 then
      Dec(FCurrentFilter);
   ShowFilter;
end;

procedure TfrmEdQBField.BtnDnClick(Sender: TObject);
begin
   SaveCurrentFilter;
   if FCurrentFilter < 4 then
      Inc(FCurrentFilter);
   ShowFilter;
end;

procedure TfrmEdQBField.TreeView1DblClick(Sender: TObject);
begin
   if (TreeView1.Selected=nil) or (TreeView1.Selected.Parent=nil) then Exit;
   EdCustom.SelText:= TreeView1.Selected.Parent.Text+'.'+TreeView1.Selected.Text;
end;

procedure TfrmEdQBField.OKBtnClick(Sender: TObject);
begin
  FOQBField.ShowAction:=TOQBShowAction(CboShow.ItemIndex);
  FOQBField.SortType:=TOQBSortType(CboSort.ItemIndex);
  FOQBField.Alias := Edit1.Text;
  SaveCurrentFilter;
  FSaveQBField.Assign(FOQBField);
end;

procedure TfrmEdQBField.SaveCurrentFilter;
var
   i: integer;
begin
  FOQBField.Filters[FCurrentFilter].FilterAction:= TOQBFilterAction(CboFilterAction.ItemIndex);
  FOQBField.Filters[FCurrentFilter].CustomExpres:= EdCustom.Text;
  for i:= 1 to 5 do
     FOQBField.Filters[FCurrentFilter].Data[i]:= TComboBox(FList[i-1]).Text;
end;

procedure TfrmEdQBField.FormDestroy(Sender: TObject);
begin
   FList.Free;
   FOQBField.Free;
end;

procedure TfrmEdQBField.Button2Click(Sender: TObject);
var
   j: Integer;
begin
   FOQBField.Filters[FCurrentFilter].FilterAction:=Low(TOQBFilterAction);
   for j:= 1 to 5 do
     FOQBField.Filters[fCurrentFilter].Data[j]:= '';
   //Edit1.Text:='';
   //CboShow.ItemIndex:=0;
   //CboSort.ItemIndex:=0;
   //EdCustom.Text:='';
   ShowFilter;
end;

end.
