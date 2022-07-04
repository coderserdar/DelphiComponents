unit fbcustomdatasetsqleditorl;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, SynCompletion,
  SynEditHighlighter, SynHighlighterSQL, jvuib, FBCustomDataSet,
  SynEditMiscClasses, SynEditSearch, LResources, SynEdit;

type

  { TFBCustomDataSetSQLEditor }

  TFBCustomDataSetSQLEditor = class(TForm)
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    FindDialog1: TFindDialog;
    JvUIBQuery1: TJvUIBQuery;
    ListBoxFields: TListBox;
    ListBoxRelations: TListBox;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    SpeedButton3: TSpeedButton;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SynSQLSyn1: TSynSQLSyn;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    edtSelectSQL: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    edtEditSql: TSynEdit;
    edtDeleteSQL: TSynEdit;
    edtRefreshSQL: TSynEdit;
    CheckBox1: TCheckBox;
    btnTest: TButton;
    TabSheet5: TTabSheet;
    edtInsertSQL: TSynEdit;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit2: TEdit;
    Panel4: TPanel;
    Memo1: TMemo;
    Label2: TLabel;
    Panel5: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    FontDialog1: TFontDialog;
    procedure btnTestClick(Sender: TObject);
    procedure edtSelectSQLStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ListBoxRelationsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ListBoxFieldsClick(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Edit2KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FDS:TFBDataSet;
    SynCompletion:TSynCompletion;
    FConfigFileName:string;
    procedure LoadTableList;
    procedure FillIdentifier;
    function ActiveEditor:TSynEdit;
    procedure ListBoxRelationsDblClick(Sender: TObject);
    //procedure ccComplete(var Value: ansistring; Shift: TShiftState);
    procedure ccExecute(Sender: TObject);
    procedure DoCfgLoad;
    procedure DoCfgStore;
    procedure DoSetEditorOpt(AFontName:string; AFontSize:integer;
         AColor:TColor; ACharSet:TFontCharSet);
    procedure DoUpdateStatus;
  public
    constructor CreateEditor(ADS:TFBDataSet);
  end;

var
  FBCustomDataSetSQLEditor: TFBCustomDataSetSQLEditor;

implementation
uses jvuibconst, fbcustomdatasetsqleditortestl, fb_ib_edt_ins_master_field_unit,
  SynEditTypes, LazIDEIntf, IniFiles;


{ TFBCustomDataSetSQLEditor }

{$I listboxrelationsdblclick.inc}

constructor TFBCustomDataSetSQLEditor.CreateEditor(ADS: TFBDataSet);
begin
  inherited Create(Application);
  FDS:=ADS;
  if Assigned(FDS) then
  begin
    edtSelectSQL.Lines.Text:=FDS.SQLSelect.Text;
    edtEditSql.Lines.Text:=FDS.SQLEdit.Text;
    edtDeleteSQL.Lines.Text:=FDS.SQLDelete.Text;
    edtRefreshSQL.Lines.Text:=FDS.SQLRefresh.Text;
    edtInsertSQL.Lines.Text:=FDS.SQLInsert.Text;
    JvUIBQuery1.DataBase:=FDS.DataBase;
    JvUIBQuery1.Transaction:=FDS.Transaction;
    LoadTableList;
    FillIdentifier;
    Caption:=Caption + ' : '+FDS.Name;
  end;
  PageControl1.ActivePageIndex:=0;
  ListBoxRelations.OnDblClick:=ListBoxRelationsDblClick;
  Button2.Enabled:=Assigned(JvUIBQuery1.DataBase);
  
  SynCompletion:=TSynCompletion.Create(Self);
  SynCompletion.AddEditor(edtSelectSQL);
  SynCompletion.AddEditor(edtEditSql);
  SynCompletion.AddEditor(edtDeleteSQL);
  SynCompletion.AddEditor(edtRefreshSQL);
  SynCompletion.AddEditor(edtInsertSQL);

  FConfigFileName:='fbdatasetoptions.ini';
  if Assigned(LazarusIDE) then
  begin
    FConfigFileName:=LazarusIDE.GetPrimaryConfigPath + DirectorySeparator + FConfigFileName;
  end;
end;

procedure TFBCustomDataSetSQLEditor.FillIdentifier;
var
  i:integer;
begin
  SynSQLSyn1.TableNames.Clear;
  for i:=0 to ListBoxRelations.Items.Count-1 do
  begin
    SynSQLSyn1.TableNames.Add(ListBoxRelations.Items[i]);
  end;
end;

procedure TFBCustomDataSetSQLEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (ModalResult=mrOk) and Assigned(FDS) then
  begin
    FDS.SQLSelect.Text:=edtSelectSQL.Lines.Text;
    FDS.SQLEdit.Text:=edtEditSql.Lines.Text;
    FDS.SQLDelete.Text:=edtDeleteSQL.Lines.Text;
    FDS.SQLRefresh.Text:=edtRefreshSQL.Lines.Text;
    FDS.SQLInsert.Text:=edtInsertSQL.Lines.Text;
  end;
  DoCfgStore;
end;

procedure TFBCustomDataSetSQLEditor.FormShow(Sender: TObject);
begin
  DoCfgLoad;
end;

procedure TFBCustomDataSetSQLEditor.ListBoxRelationsClick(Sender: TObject);
var
  ind:integer;
  S:string;
begin
  if (ListBoxRelations.ItemIndex > -1) and Assigned(JvUIBQuery1.DataBase)
    and Assigned(JvUIBQuery1.Transaction) then
  begin
    JvUIBQuery1.Sql.Clear;
    JvUIBQuery1.Sql.Add(Format(sqlSelectFields, [ListBoxRelations.Items[ListBoxRelations.ItemIndex]]));
    try
      JvUIBQuery1.Execute;
      ListBoxFields.Items.Clear;
      JvUIBQuery1.Next;
      while not JvUIBQuery1.Eof do
      begin
        ind:=ListBoxFields.Items.Add(trim(JvUIBQuery1.Fields.AsString[0]));
        JvUIBQuery1.ReadBlob(1, s);
        ListBoxFields.Items.Objects[ind]:=TFieldInfo.Create(s);
        JvUIBQuery1.Next;
      end;
    finally
      JvUIBQuery1.Close;
    end;
  end;
end;

function TFBCustomDataSetSQLEditor.ActiveEditor: TSynEdit;
begin
  case PageControl1.ActivePageIndex of
    0:Result:=edtSelectSQL;
    1:Result:=edtInsertSQL;
    2:Result:=edtEditSql;
    3:Result:=edtDeleteSQL;
    4:Result:=edtRefreshSQL;
  end;
end;


procedure TFBCustomDataSetSQLEditor.Button1Click(Sender: TObject);
begin
  ListBoxRelationsDblClick(nil);
end;

(*
procedure TFBCustomDataSetSQLEditor.SynCompletionProposal1Execute(
  Kind: SynCompletionType; Sender: TObject; var CurrentInput: String;
  var x, y: Integer; var CanExecute: Boolean);

function GetCurWord:string;
var
  S:string;
  i,j:integer;
begin
  Result:='';
  with ActiveEditor do
  begin
    S:=Trim(Copy(LineText, 1, CaretX));
    I:=Length(S);
    while (i>0) and (S[i]<>'.') do Dec(I);
    if (I>0) then
    begin
      J:=i-1;
      //Get table name
      while (j>0) and (S[j] in ['A'..'z','"']) do Dec(j);
      Result:=trim(Copy(S, j+1, i-j-1));
    end;
  end;
end;
var
  S:string;
begin
  S:=AnsiUpperCase(GetCurWord);
  if S<>'' then
  begin
    if Assigned(JvUIBQuery1.DataBase) and Assigned(JvUIBQuery1.Transaction) then
    begin
      JvUIBQuery1.Sql.Clear;
      JvUIBQuery1.Sql.Add('select RDB$RELATION_FIELDS.RDB$FIELD_NAME as FIELD_NAME from RDB$RELATION_FIELDS '+
                          'where RDB$RELATION_FIELDS.rdb$relation_name = '''+S+''' order by RDB$RELATION_FIELDS.RDB$FIELD_NAME');
      try
        JvUIBQuery1.Execute;
        SynCompletionProposal1.InsertList.Clear;
        SynCompletionProposal1.ItemList.Clear;
        JvUIBQuery1.Next;
        while not JvUIBQuery1.Eof do
        begin
          SynCompletionProposal1.InsertList.Add(trim(JvUIBQuery1.Fields.ByNameAsString['FIELD_NAME']));
          SynCompletionProposal1.ItemList.Add(trim(JvUIBQuery1.Fields.ByNameAsString['FIELD_NAME']));
          JvUIBQuery1.Next;
        end;
      finally
        JvUIBQuery1.Close;
      end;
    end;
  end;
end;
*)

procedure TFBCustomDataSetSQLEditor.ccExecute(Sender: TObject);
function GetCurWord:string;
var
  S:string;
  i,j:integer;
begin
  Result:='';
  with ActiveEditor do
  begin
    S:=Trim(Copy(LineText, 1, CaretX));
    I:=Length(S);
    while (i>0) and (S[i]<>'.') do Dec(I);
    if (I>0) then
    begin
      J:=i-1;
      while (j>0) and (S[j] in ['A'..'z','"']) do Dec(j);
      Result:=trim(Copy(S, j+1, i-j-1));
    end;
  end;
end;
var
  S:string;
begin
  S:=AnsiUpperCase(GetCurWord);
  if S<>'' then
  begin
    if Assigned(JvUIBQuery1.DataBase) and Assigned(JvUIBQuery1.Transaction) then
    begin
      JvUIBQuery1.Sql.Clear;
      JvUIBQuery1.Sql.Add('select RDB$RELATION_FIELDS.RDB$FIELD_NAME as FIELD_NAME from RDB$RELATION_FIELDS '+
                          'where RDB$RELATION_FIELDS.rdb$relation_name = '''+S+''' order by RDB$RELATION_FIELDS.RDB$FIELD_NAME');
      try
        JvUIBQuery1.Execute;
//        SynCompletionProposal1.InsertList.Clear;
        SynCompletion.ItemList.Clear;
        {$IFDEF FPC}
        SynCompletion.OnPaintItem;
        {$ELSE}
        {$ENDIF}
        JvUIBQuery1.Next;
        while not JvUIBQuery1.Eof do
        begin
//          SynCompletion.InsertList.Add(trim(JvUIBQuery1.Fields.ByNameAsString['FIELD_NAME']));
          SynCompletion.ItemList.Add(trim(JvUIBQuery1.Fields.ByNameAsString['FIELD_NAME']));
          JvUIBQuery1.Next;
        end;
      finally
        JvUIBQuery1.Close;
      end;
    end;
  end;
end;

procedure TFBCustomDataSetSQLEditor.DoCfgLoad;
var
  Ini:TIniFile;

  AFontName:string;
  AFontSize:integer;
  AColor:TColor;
  ACharSet:TFontCharSet;
begin
  Ini:=TIniFile.Create(FConfigFileName);
  Left:=Ini.ReadInteger('Position', 'Left', Left);
  Top:=Ini.ReadInteger('Position', 'Top', Top);
  Width:=Ini.ReadInteger('Position', 'Width', Width);
  Height:=Ini.ReadInteger('Position', 'Height', Height);

  AFontName:=Ini.ReadString('Font', 'Name', ActiveEditor.Font.Name);
  AFontSize:=Ini.ReadInteger('Font', 'Size', ActiveEditor.Font.Size);
  AColor:=Ini.ReadInteger('Font', 'Color', ActiveEditor.Font.Color);
  ACharSet:=Ini.ReadInteger('Font', 'CharSet', ActiveEditor.Font.CharSet);

  Ini.Free;

  DoSetEditorOpt(AFontName, AFontSize, AColor, ACharSet);
end;

procedure TFBCustomDataSetSQLEditor.DoCfgStore;
var
  Ini:TIniFile;
begin
  Ini:=TIniFile.Create(FConfigFileName);
  Ini.WriteInteger('Position', 'Left', Left);
  Ini.WriteInteger('Position', 'Top', Top);
  Ini.WriteInteger('Position', 'Width', Width);
  Ini.WriteInteger('Position', 'Height', Height);

  Ini.WriteString('Font', 'Name', ActiveEditor.Font.Name);
  Ini.WriteInteger('Font', 'Size', ActiveEditor.Font.Size);
  Ini.WriteInteger('Font', 'Color', ActiveEditor.Font.Color);
  Ini.WriteInteger('Font', 'CharSet', ActiveEditor.Font.CharSet);

  Ini.Free;
end;

procedure TFBCustomDataSetSQLEditor.DoSetEditorOpt(AFontName: string;
  AFontSize: integer; AColor:TColor;
  ACharSet:TFontCharSet);
  
procedure DoSetOpt(AEdt:TSynEdit);
begin
  AEdt.Font.Name:=AFontName;
  AEdt.Font.Size:=AFontSize;
  AEdt.Font.Color:=AColor;
  AEdt.Font.CharSet:=ACharSet;
end;

begin
  DoSetOpt(edtSelectSQL);
  DoSetOpt(edtInsertSQL);
  DoSetOpt(edtEditSql);
  DoSetOpt(edtDeleteSQL);
  DoSetOpt(edtRefreshSQL);
end;

procedure TFBCustomDataSetSQLEditor.DoUpdateStatus;
begin
  StatusBar1.Panels[0].Text:=Format('%d : %d', [ActiveEditor.CaretX, ActiveEditor.CaretY]);

  if ActiveEditor.Modified then
    StatusBar1.Panels[1].Text:='Modified'
  else
    StatusBar1.Panels[1].Text:='';
end;


procedure TFBCustomDataSetSQLEditor.PageControl1Change(Sender: TObject);
begin
  ActiveEditor.SetFocus;
  DoUpdateStatus;
end;

procedure TFBCustomDataSetSQLEditor.CheckBox2Click(Sender: TObject);
begin
  Edit2.Enabled:=CheckBox2.Checked;
  LoadTableList;
end;

procedure TFBCustomDataSetSQLEditor.ListBoxFieldsClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  if (ListBoxFields.ItemIndex>-1) and (ListBoxFields.ItemIndex<ListBoxFields.Items.Count) then
    if Assigned(ListBoxFields.Items.Objects[ListBoxFields.ItemIndex]) then
      Memo1.Text:=TFieldInfo(ListBoxFields.Items.Objects[ListBoxFields.ItemIndex]).Description;
end;

procedure TFBCustomDataSetSQLEditor.CheckBox3Click(Sender: TObject);
begin
  Panel4.Visible:=CheckBox3.Checked;
end;

procedure TFBCustomDataSetSQLEditor.LoadTableList;
begin
  ListBoxRelations.Items.Clear;
  if Assigned(JvUIBQuery1.DataBase) and Assigned(JvUIBQuery1.Transaction) then
  begin
    JvUIBQuery1.Sql.Clear;
    JvUIBQuery1.Sql.Add('select rdb$relations.rdb$relation_name as relation_name from rdb$relations where RDB$SYSTEM_FLAG=0 order by RDB$RELATION_NAME');
    try
      JvUIBQuery1.Execute;
      ListBoxRelations.Items.Clear;
      JvUIBQuery1.Next;
      while not JvUIBQuery1.Eof do
      begin
        if (CheckBox2.Checked) and (Edit2.Text<>'') then
        begin
          if Pos(UpperCase(Edit2.Text), JvUIBQuery1.Fields.ByNameAsString['relation_name'])<>0 then
            ListBoxRelations.Items.Add(trim(JvUIBQuery1.Fields.ByNameAsString['relation_name']));
        end
        else
          ListBoxRelations.Items.Add(trim(JvUIBQuery1.Fields.ByNameAsString['relation_name']));
        JvUIBQuery1.Next;
      end;
    finally
      JvUIBQuery1.Close;
    end;
  end;
end;


procedure TFBCustomDataSetSQLEditor.Edit2KeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  LoadTableList;
end;

procedure TFBCustomDataSetSQLEditor.Button2Click(Sender: TObject);
begin
  JvUIBQuery1.SQL.Text:=ActiveEditor.Lines.Text;
  try
    JvUIBQuery1.Prepare;
    ShowMessage('Check OK!');
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TFBCustomDataSetSQLEditor.SpeedButton1Click(Sender: TObject);
var
  SynSearchOptions:TSynSearchOptions;
begin
  if FindDialog1.Execute then
  begin
    ActiveEditor.SetFocus;
    SynSearchOptions:=[ssoEntireScope];
    
    if not (frDown in FindDialog1.Options) then
      Include(SynSearchOptions, ssoBackwards);

    if (frWholeWord in FindDialog1.Options) then
      Include(SynSearchOptions, ssoWholeWord);

    if (frMatchCase in FindDialog1.Options) then
      Include(SynSearchOptions, ssoMatchCase);

    ActiveEditor.SearchReplace(FindDialog1.FindText, '', SynSearchOptions);
  end;
end;

procedure TFBCustomDataSetSQLEditor.FindDialog1Find(Sender: TObject);
begin
{  SynEditSearch1.Pattern:=FindDialog1.FindText;
  SynEditSearch1.FindFirst('');}
end;

procedure TFBCustomDataSetSQLEditor.SpeedButton2Click(Sender: TObject);
begin
 with FontDialog1.Font do
 begin
   Name :=ActiveEditor.Font.Name;
   Size :=ActiveEditor.Font.Size;
   Color:=ActiveEditor.Font.Color;
   Style:=ActiveEditor.Font.Style;
   CharSet:=ActiveEditor.Font.CharSet;
 end;

 if FontDialog1.Execute then
 begin
   edtSelectSQL.Font:=FontDialog1.Font;
   edtInsertSQL.Font:=FontDialog1.Font;
   edtEditSql.Font:=FontDialog1.Font;
   edtDeleteSQL.Font:=FontDialog1.Font;
   edtRefreshSQL.Font:=FontDialog1.Font;
 end;
end;

procedure TFBCustomDataSetSQLEditor.SpeedButton3Click(Sender: TObject);
var
  i:integer;
begin
  if Assigned(FDS.DataSource) and Assigned(FDS.DataSource.DataSet) and (FDS.DataSource.DataSet.FieldCount>0) then
  begin
    editorInsertMasterFieldForm:=TeditorInsertMasterFieldForm.Create(Application);
    try
      editorInsertMasterFieldForm.ListBox1.Items.Clear;
      for i:=0 to FDS.DataSource.DataSet.FieldCount-1 do
        editorInsertMasterFieldForm.ListBox1.Items.Add(FDS.DataSource.DataSet.Fields[i].FieldName);
      if editorInsertMasterFieldForm.ListBox1.Items.Count>0 then
        editorInsertMasterFieldForm.ListBox1.ItemIndex:=0;
        
      if (editorInsertMasterFieldForm.ShowModal = mrOk) then
      begin
        if editorInsertMasterFieldForm.ListBox1.ItemIndex>=0 then
          ActiveEditor.SelText:=editorInsertMasterFieldForm.ListBox1.Items[editorInsertMasterFieldForm.ListBox1.ItemIndex];
      end;
    finally
      editorInsertMasterFieldForm.Free;
    end;
  end;
end;

procedure TFBCustomDataSetSQLEditor.edtSelectSQLStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  DoUpdateStatus;
end;

initialization
  {$i fbcustomdatasetsqleditorl.lrs}
end.
