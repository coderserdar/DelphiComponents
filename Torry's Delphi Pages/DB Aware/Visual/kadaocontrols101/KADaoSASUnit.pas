unit KADaoSASUnit;
{$I KADaoControlsCommonDirectives.pas}
{DEFINE USEXML} //Only if you have KADao DELUXE Manegement package
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBGrids, KDaoTable, StdCtrls, KAHRollForm, ExtCtrls, CheckLst {$IFDEF USEXML}, KADaoAdo{$ENDIF};

type
  TSAS = class(TForm)
    Label1: TLabel;                                         
    ListBox1: TListBox;
    Button2: TButton;
    Button1: TButton;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    SaveDialog1: TSaveDialog;
    CheckBox2: TCheckBox;
    GroupBox2: TGroupBox;
    CheckBox3: TCheckBox;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Label4: TLabel;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label5: TLabel;
    Edit3: TEdit;
    KAHRollForm1: TKAHRollForm;
    GroupBox3: TGroupBox;
    CheckListBox1: TCheckListBox;
    procedure ListBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
    Table : TKADaoTable;
    Grid  : TDBGrid;
    procedure WriteSchemaIni(FN:String);
    Function  FindColumn(Caption:String):TColumn;
    Function  FindCaption(Caption:String):TField;
    procedure DoExport;
  public
    { Public declarations }
    ExportDir : String;
    Function Execute(DaoTable : TDataset):Boolean;
    Function ExecutefromGrid(DaoTable : TDataset; DaoGrid: TDBGrid):Boolean;
  end;

var
  SAS: TSAS;

implementation
{$R *.dfm}

Function  TSAS.FindColumn(Caption:String):TColumn;
Var
 X : Integer;
Begin
 Result := Nil;
 For X := 0 To Grid.Columns.Count-1 do
     Begin
       If (Grid.Columns.Items[X].Title.Caption=Caption) Then
          Begin
            Result := Grid.Columns.Items[X];
            Exit;
          End;
     End;
End;


Function  TSAS.FindCaption(Caption:String):TField;
Var
 X : Integer;
Begin
 Result := Nil;
 For X := 0 To Table.FieldCount-1 do
     Begin
       if  (Table.Fields[X].Visible)
       And (Table.Fields[X].DisplayLabel=Caption) Then
          Begin
            Result := Table.Fields[X];
          End;
     End;
End;

Function TSAS.Execute(DaoTable : TDataset):Boolean;
Var
 X : Integer;
Begin
 Result := False;
 Table  := TKADaoTable(DaoTable);
 Grid   := Nil;
 ListBox1.Clear;

 if Table.Database.Version<>'12.0' Then
    Begin
      ListBox1.Items.Add('MS Excel 3.0');
      ListBox1.Items.Add('MS Excel 4.0');
    End
 Else
    Begin
      ListBox1.Items.Add('MS Excel 12.0');
      ListBox1.Items.Add('MS Excel 12.0 Xml');
    End;

 ListBox1.Items.Add('MS Excel 5.0');
 ListBox1.Items.Add('MS Excel 8.0');

 ListBox1.Items.Add('Paradox 3.X');
 ListBox1.Items.Add('Paradox 4.X');
 ListBox1.Items.Add('Paradox 5.X');

 if Table.Database.Version<>'3.5' Then
    ListBox1.Items.Add('Paradox 7.X')
 Else
    ListBox1.Items.Add('FoxPro 3.X');

 ListBox1.Items.Add('dBase III');
 ListBox1.Items.Add('dBase IV');
 ListBox1.Items.Add('dBase 5.0');

 ListBox1.Items.Add('HTML');
 ListBox1.Items.Add('Text');
 ListBox1.Items.Add('MS Access');

 {$IFDEF USEXML}
 ListBox1.Items.Add('XML');
 ListBox1.Items.Add('ADTG');
 {$ENDIF}
 ListBox1.ItemIndex:=0;

 ComboBox1.Clear;
 ComboBox1.Items.Add('ANSI');
 ComboBox1.Items.Add('OEM');
 ComboBox1.Items.Add(IntToStr(GetACP));
 ComboBox1.Items.Add(IntToStr(GetOEMCP));
 ComboBox1.ItemIndex:=0;

 ComboBox2.Clear;
 ComboBox2.Items.Add('Semicolon Delimited');
 ComboBox2.Items.Add('Tab Delimited');
 ComboBox2.Items.Add('CSV Delimited');
 ComboBox2.Items.Add('Fixed Length');
 ComboBox2.ItemIndex:=0;

 CheckListBox1.Clear;
 For X := 0 to DaoTable.FieldCount-1 do
     Begin
       if DaoTable.Fields[X].Visible Then
          Begin
            CheckListBox1.Items.Add(DaoTable.Fields[X].DisplayLabel);
            CheckListBox1.Checked[CheckListBox1.Items.Count-1]:=True;
          End;
     End;
 if Table.ExportMethod=AllFields Then
    KAHRollForm1.Enabled := False
 Else
    KAHRollForm1.Enabled := True;
 ShowModal;
 if ModalResult=mrOK Then Result := True;
End;

Function TSAS.ExecutefromGrid(DaoTable : TDataset; DaoGrid: TDBGrid):Boolean;
Var
 X : Integer;
Begin
 Result := False;
 Table  := TKADaoTable(DaoTable);
 Grid   := DaoGrid;
 ListBox1.Clear;
 if Table.Database.Version<>'12.0' Then
    Begin
      ListBox1.Items.Add('MS Excel 3.0');
      ListBox1.Items.Add('MS Excel 4.0');
    End
 Else
    Begin
      ListBox1.Items.Add('MS Excel 12.0');
      ListBox1.Items.Add('MS Excel 12.0 Xml');
    End;

 ListBox1.Items.Add('MS Excel 5.0');
 ListBox1.Items.Add('MS Excel 8.0');

 ListBox1.Items.Add('Paradox 3.X');
 ListBox1.Items.Add('Paradox 4.X');
 ListBox1.Items.Add('Paradox 5.X');

 if Table.Database.Version<>'3.5' Then
    ListBox1.Items.Add('Paradox 7.X')
 Else
    ListBox1.Items.Add('FoxPro 3.X');

 ListBox1.Items.Add('dBase III');
 ListBox1.Items.Add('dBase IV');
 ListBox1.Items.Add('dBase 5.0');

 ListBox1.Items.Add('HTML');
 ListBox1.Items.Add('Text');
 ListBox1.Items.Add('MS Access');

 {$IFDEF USEXML}
 ListBox1.Items.Add('XML');
 ListBox1.Items.Add('ADTG');
 {$ENDIF}
 ListBox1.ItemIndex:=0;

 ComboBox1.Clear;
 ComboBox1.Items.Add('ANSI');
 ComboBox1.Items.Add('OEM');
 ComboBox1.Items.Add(IntToStr(GetACP));
 ComboBox1.Items.Add(IntToStr(GetOEMCP));
 ComboBox1.ItemIndex:=0;

 ComboBox2.Clear;
 ComboBox2.Items.Add('Semicolon Delimited');
 ComboBox2.Items.Add('Tab Delimited');
 ComboBox2.Items.Add('CSV Delimited');
 ComboBox2.Items.Add('Fixed Length');
 ComboBox2.ItemIndex:=0;

 CheckListBox1.Clear;
 For X := 0 to Grid.Columns.Count-1 do
     Begin
       CheckListBox1.Items.Add(Grid.Columns[X].Title.Caption);
       {$IFDEF D4UP}
       if Grid.Columns[X].Visible Then
       {$ENDIF}
          CheckListBox1.Checked[CheckListBox1.Items.Count-1]:=True;
     End;
 if Table.ExportMethod=AllFields Then
    KAHRollForm1.Enabled := False
 Else
    KAHRollForm1.Enabled := True;
 ShowModal;
 if ModalResult=mrOK Then Result := True;
End;



procedure TSAS.ListBox1Click(Sender: TObject);
begin
  Label2.Enabled    := False;
  Edit2.Enabled     := False;
  Label5.Enabled    := False;
  Edit3.Enabled     := False;

  Label3.Enabled    := False;
  Label4.Enabled    := False;
  CheckBox3.Enabled := False;
  GroupBox2.Enabled := False;
  ComboBox1.Enabled := False;
  ComboBox2.Enabled := False;
  if (ListBox1.ItemIndex > -1) And (ListBox1.ItemIndex < 4) Then
     Begin
       Label2.Enabled := True;
       Edit2.Enabled  := True;
     End;
  if (ListBox1.ItemIndex = 13) Then
     Begin
       Label5.Enabled := True;
       Edit3.Enabled  := True;
     End;
  if (ListBox1.ItemIndex=11) Or (ListBox1.ItemIndex=12) Then
     Begin
      Label3.Enabled    := True;
      Label4.Enabled    := True;
      ComboBox1.Enabled := True;
      ComboBox2.Enabled := True;
      CheckBox3.Enabled := True;
      GroupBox2.Enabled := True;
     End;
end;

procedure TSAS.WriteSchemaIni(FN:String);
var
 IniFN       : String;
 IniSection  : String;
 II          : Integer;
 Format      : String;
 X           : Integer;
 Col         : String;
 Tmp         : Array[0..1000] of Char;
 S           : String;
Begin
 IniFN       := ExtractFilePath(FN)+'schema.ini';
 IniSection  := ExtractFileName(FN);
 II          := ComboBox2.ItemIndex;
 if ListBox1.ItemIndex=11 Then II:=4;
 Case II of
    0 : Format := 'Delimited(;)';
    1 : Format := 'TabDelimited';
    2 : Format := 'CSVDelimited';
    3 : Format := 'FixedLength';
    4 : Format := 'HTML';
 End;
 if CheckBox3.Checked Then
    WritePrivateProfileString(PChar(IniSection),'ColNameHeader','True',PChar(IniFN))
 Else
    WritePrivateProfileString(PChar(IniSection),'ColNameHeader','False',PChar(IniFN));
 WritePrivateProfileString(PChar(IniSection),'MaxScanRows','0',PChar(IniFN));
 WritePrivateProfileString(PChar(IniSection),'Format',PChar(Format),PChar(IniFN));
 WritePrivateProfileString(PChar(IniSection),'CharacterSet',PChar(ComboBox1.Items[ComboBox1.ItemIndex]),PChar(IniFN));

 if II=3 Then
    Begin
      For X := 0 to Table.FieldCount-1 do
          Begin
            Col := 'Col'+IntToStr(X+1);
            GetPrivateProfileString(PChar(IniSection),PChar(Col),'',Tmp,1000,PChar(IniFN));
            S := StrPas(Tmp);
            if S <> '' Then
               Begin
                 if Pos('width ',AnsiLowerCase(S))=0 Then
                    Begin
                      S := S+' Width '+Edit1.Text;
                      WritePrivateProfileString(PChar(IniSection),PChar(Col),PChar(S),PChar(IniFN));
                    End;
               End;
          End;
    End;
End;

procedure TSAS.DoExport;
Var
  II           : Integer;
  FileName     : String;
  SheetName    : String;
  ExportBlobs  : Boolean;
  DeleteOld    : Boolean;
  {$IFDEF USEXML}
  XML          : TKADaoXML;
  {$ENDIF}
begin
  II := ListBox1.ItemIndex;
  if II = -1  Then Exit;
  FileName   :=  SaveDialog1.FileName;
  if FileName = '' Then Exit;
  SheetName  := Edit2.Text;
  ExportBlobs:= CheckBox1.Checked;
  DeleteOld  := CheckBox2.Checked;

  if Table.Database.Version='3.5' Then
      if (ListBox1.ItemIndex > -1) And (ListBox1.ItemIndex < 5) Then
         ExportBlobs := False;

  Case II of
      0 : Begin
           if SheetName = '' Then Exit;
           if Table.Database.Version='12.0' Then
              Table.AccessExportToExcel(FileName, SheetName, 12, ExportBlobs, DeleteOld)
           Else
              Table.AccessExportToExcel(FileName, SheetName, 3, ExportBlobs, DeleteOld);
          End;
      1 : Begin
           if SheetName = '' Then Exit;
           if Table.Database.Version='12.0' Then
              Table.AccessExportToExcel(FileName, SheetName, 9, ExportBlobs, DeleteOld)
           Else
              Table.AccessExportToExcel(FileName, SheetName, 4, ExportBlobs, DeleteOld);
          End;
      2 : Begin
           if SheetName = '' Then Exit;
           Table.AccessExportToExcel(FileName, SheetName, 5, ExportBlobs, DeleteOld);
          End;
      3 : Begin
           if SheetName = '' Then Exit;
           Table.AccessExportToExcel(FileName, SheetName, 8, ExportBlobs, DeleteOld);
          End;
      4 : Begin
           Table.AccessExportToParadox(FileName, 3, ExportBlobs, DeleteOld);
          End;
      5 : Begin
            Table.AccessExportToParadox(FileName, 4, ExportBlobs, DeleteOld);
          End;
      6 : Begin
            Table.AccessExportToParadox(FileName, 5, ExportBlobs, DeleteOld);
          End;
      7 : Begin
            if Table.Database.Version<>'3.5' Then
               Table.AccessExportToParadox(FileName, 7, ExportBlobs, DeleteOld)
            Else
               Table.AccessExportToFoxPro(FileName, 30, ExportBlobs, DeleteOld);
          End;
      8 : Begin
            Table.AccessExportToDBase(FileName, 3, ExportBlobs, DeleteOld);
          End;
      9 : Begin
            Table.AccessExportToDBase(FileName, 4, ExportBlobs, DeleteOld);
          End;
      10: Begin
            Table.AccessExportToDBase(FileName, 5, ExportBlobs, DeleteOld);
          End;
      11: Begin
            Table.AccessExportToHTML(FileName, ExportBlobs, DeleteOld);
            WriteSchemaIni(FileName);
            Table.AccessExportToHTML(FileName, ExportBlobs, True);
          End;
      12: Begin
            Table.AccessExportToTXT(FileName, ExportBlobs, True);
            WriteSchemaIni(FileName);
            Table.AccessExportToTXT(FileName, ExportBlobs, DeleteOld);
          End;
      13: Begin
            if Not FileExists(FileName) Then
               Begin
                 Table.Database.CreateAccessDatabase(FileName);
                 DeleteOld := False;
               End;
            Table.AccessExportToMDB(FileName, Edit3.Text, ExportBlobs, DeleteOld);
          End;
      {$IFDEF USEXML}
      14: Begin
            XML := TKADaoXML.Create(Self);
            Try
              XML.Table        := Table;
              XML.FileName     := FileName;
              XML.IncludeBlobs := ExportBlobs;
              XML.DeleteOldFile:= DeleteOld;
              XML.ActionType   := atSaveToXML;
              XML.Activate     := True;
            Finally
              XML.Free;
            End;
          End;
      15: Begin
            XML := TKADaoXML.Create(Self);
            Try
              XML.Table        := Table;
              XML.FileName     := FileName;
              XML.IncludeBlobs := ExportBlobs;
              XML.DeleteOldFile:= DeleteOld;
              XML.ActionType   := atSaveToADTG;
              XML.Activate     := True;
            Finally
              XML.Free;
            End;
          End;
      {$ENDIF}
  End;
  ModalResult := mrOK;
end;

procedure TSAS.Button2Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

procedure TSAS.Button1Click(Sender: TObject);
Var
  II           : Integer;
  X            : Integer;
  FF           : TField;
  Column       : TColumn;
begin
  II := ListBox1.ItemIndex;
  if II = -1  Then Exit;
  Case II of
      0 : Begin
           SaveDialog1.DefaultExt:='xls';
           if Table.Database.Version='12.0' Then
               SaveDialog1.Filter:='MS Excel 12.0 files(*.xlsb)|*.xlsb|All files (*.*)|*.*'
           Else
               SaveDialog1.Filter:='MS Excel 3.0 files(*.xls)|*.xls|All files (*.*)|*.*';
           SaveDialog1.FilterIndex:=1;
          End;
      1 : Begin
           SaveDialog1.DefaultExt:='xls';
           if Table.Database.Version='12.0' Then
               SaveDialog1.Filter:='MS Excel 12.0 files(*.xlsx)|*.xlsx|All files (*.*)|*.*'
           Else
               SaveDialog1.Filter:='MS Excel 4.0 files (*.xls)|*.xls|All files (*.*)|*.*';
           SaveDialog1.FilterIndex:=1;
          End;
      2 : Begin
           SaveDialog1.DefaultExt:='xls';
           SaveDialog1.Filter:='MS Excel 5.0 files (*.xls)|*.xls|All files (*.*)|*.*';
           SaveDialog1.FilterIndex:=1;
          End;
      3 : Begin
           SaveDialog1.DefaultExt:='xls';
           SaveDialog1.Filter:='MS Excel 8.0 files (*.xls)|*.xls|All files (*.*)|*.*';
           SaveDialog1.FilterIndex:=1;
          End;
      4 : Begin
           SaveDialog1.DefaultExt:='db';
           SaveDialog1.Filter:='Paradox 3.0 files (*.db)|*.db|All files (*.*)|*.*';
           SaveDialog1.FilterIndex:=1;
          End;
      5 : Begin
            SaveDialog1.DefaultExt:='db';
           SaveDialog1.Filter:='Paradox 4.0 files (*.db)|*.db|All files (*.*)|*.*';
           SaveDialog1.FilterIndex:=1;
          End;
      6 : Begin
           SaveDialog1.DefaultExt:='db';
           SaveDialog1.Filter:='Paradox 5.0 files (*.db)|*.db|All files (*.*)|*.*';
           SaveDialog1.FilterIndex:=1;
          End;
      7 : Begin
            SaveDialog1.FilterIndex:=1;
            if Table.Database.Version<>'3.5' Then
               Begin
                SaveDialog1.DefaultExt:='db';
                SaveDialog1.Filter:='Paradox 7.0 files(*.db)|*.db|All files (*.*)|*.*'
               End
            Else
               Begin
                SaveDialog1.DefaultExt:='dbf';
                SaveDialog1.Filter:='FoxPro 3.0 files (*.dbf)|*.dbf|All files (*.*)|*.*';
               End;
          End;
      8 : Begin
            SaveDialog1.DefaultExt:='dbf';
            SaveDialog1.Filter:='dBase III files(*.dbf)|*.dbf|All files (*.*)|*.*';
            SaveDialog1.FilterIndex:=1;
          End;
      9 : Begin
            SaveDialog1.DefaultExt:='dbf';
            SaveDialog1.Filter:='dBase IV files(*.dbf)|*.dbf|All files (*.*)|*.*';
            SaveDialog1.FilterIndex:=1;
          End;
      10: Begin
            SaveDialog1.DefaultExt:='dbf';
            SaveDialog1.Filter:='dBase 5.0 files(*.dbf)|*.dbf|All files (*.*)|*.*';
            SaveDialog1.FilterIndex:=1;
          End;
      11: Begin
            SaveDialog1.DefaultExt:='htm';
            SaveDialog1.Filter:='HTML files(*.htm)|*.htm|All files (*.*)|*.*';
            SaveDialog1.FilterIndex:=1;
          End;
      12: Begin
            SaveDialog1.DefaultExt:='txt';
            SaveDialog1.Filter:='Text files(*.txt)|*.txt|All files (*.*)|*.*';
            SaveDialog1.FilterIndex:=1;
          End;
      13: Begin
            if Table.Database.Version='12.0' Then
               Begin
                 SaveDialog1.DefaultExt := 'accdb';
                 SaveDialog1.Filter     := 'MS Access 12.0 files(*.accdb)|*.accdb|All files (*.*)|*.*';
               End
            Else
               Begin
                 SaveDialog1.DefaultExt := 'mdb';
                 SaveDialog1.Filter     := 'MS Access files(*.mdb)|*.mdb|All files (*.*)|*.*';
               End;
            SaveDialog1.FilterIndex:=1;
          End;
      {$IFDEF USEXML}
      14: Begin
            SaveDialog1.DefaultExt:='xml';
            SaveDialog1.Filter:='XML files(*.xml)|*.xml|All files (*.*)|*.*';
            SaveDialog1.FilterIndex:=1;
          End;
      15: Begin
            SaveDialog1.DefaultExt:='adt';
            SaveDialog1.Filter:='ADTG files(*.adt)|*.adt|All files (*.*)|*.*';
            SaveDialog1.FilterIndex:=1;
          End;
      {$ENDIF}
  End;
  if ExportDir <> '' Then SaveDialog1.InitialDir := ExportDir;
  if SaveDialog1.Execute Then
     Begin
       Try
        For X := 0 to CheckListBox1.Items.Count-1 do
            Begin
              if NOT CheckListBox1.Checked[X] Then
                 Begin
                   if Grid <> Nil Then
                      Begin
                        Column := FindColumn(CheckListBox1.Items.Strings[X]);
                        if Column <> Nil Then Column.Field.Visible := False;
                      End
                   Else
                      Begin
                       FF := FindCaption(CheckListBox1.Items.Strings[X]);
                       if FF <> Nil Then FF.Visible:=False;
                      End;
                 End;
             End;
        DoExport;
       Finally
         For X := 0 to CheckListBox1.Items.Count-1 do                             
             Begin
              if Grid <> Nil Then
                 Begin
                   Column := FindColumn(CheckListBox1.Items.Strings[X]);
                   if Column <> Nil Then Column.Field.Visible := True;
                 End
              Else
                 Begin
                   FF := FindCaption(CheckListBox1.Items.Strings[X]);
                   if FF <> Nil Then FF.Visible:=True;
                 End;
             End;
       End;
     End;
end;

procedure TSAS.ComboBox2Change(Sender: TObject);
begin
 if ComboBox2.ItemIndex=3  Then Edit1.Visible:=True Else Edit1.Visible:=False;
end;

end.

