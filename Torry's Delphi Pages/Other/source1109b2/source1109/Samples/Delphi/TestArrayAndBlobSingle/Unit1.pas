Unit Unit1;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Stdctrls,
  Mask,
  DBCtrls,
  Db,
  fsexfield,
  Grids,
  DBGrids,
  fsllcomm,
  fslllgcy,
  fsdb,
  ComCtrls,
  fsdbbase,
  fsllbase,
  fsllcomp,
  fslleng,
  fssrintm,
  ExtCtrls,
  Spin, fsserverclass;

Type
  TForm1 = Class(TForm)
    FSClient1: TFSClient;
    FSSession1: TFSSession;
    FSTable1: TFSTable;
    DataSource1: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    Label7: TLabel;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    ComboBox1: TComboBox;
    Label6: TLabel;
    ComboBox2: TComboBox;
    Button3: TButton;
    Button4: TButton;
    GroupBox2: TGroupBox;
    Button2: TButton;
    Label8: TLabel;
    Button1: TButton;
    FSServer1: TFSServer;
    FSDatabase1: TFSDatabase;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    DBMemo1: TDBMemo;
    DBImage1: TDBImage;
    btnLoadGeneric: TButton;
    btnSaveGeneric: TButton;
    btnClearGeneric: TButton;
    SaveDialog1: TSaveDialog;
    Memo1: TMemo;
    ComboBox3: TComboBox;
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Label9: TLabel;
    DBNavigator1: TDBNavigator;
    SpinEdit1: TSpinEdit;
    Label10: TLabel;
    Button5: TButton;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure btnLoadGenericClick(Sender: TObject);
    Procedure btnSaveGenericClick(Sender: TObject);
    Procedure btnClearGenericClick(Sender: TObject);
    Procedure PageControl1Change(Sender: TObject);
    Procedure DataSource1DataChange(Sender: TObject; Field: TField);
    Procedure ComboBox3Change(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure DataSource1StateChange(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure SpinEdit1Change(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure CheckBox4Click(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

Var
  Form1: TForm1;

Implementation

{$R *.DFM}

Procedure TForm1.Button2Click(Sender: TObject);
Var
  v: variant;
  s: String;
  i: Integer;
Begin
  {s := '';
  v := fstable1.FieldByName(combobox1.text).AsVariant;
  For i := 0 To fstable1.FieldByName(combobox1.text).size - 1 Do
    s := s + FloatToStr(v[i]) + '  ';
  edit3.text := Trim(s);   }
  // or
  // get
  edit3.text := fstable1.FieldByName(combobox1.text).AsString;
  //set
  //fstable1.FieldByName(combobox1.text).AsString:= edit3.text;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  v: variant;
Begin
  v := TfsArrayField(fstable1.FieldByName(combobox1.text)).Value[combobox2.ItemIndex];
  edit3.text := v;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  v: variant;
Begin
  v := edit3.text;
  fstable1.Edit;
  TfsArrayField(fstable1.FieldByName(combobox1.text)).Value[combobox2.ItemIndex] := v;
  fstable1.Post;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  DecimalSeparator := '.';
  ComboBox1.ItemIndex := 3;
  ComboBox2.ItemIndex := 0;
  fsSession1.Active := False;
  fsClient1.Active := False;
  FSDatabase1.AliasName := ExtractFilePath(ParamStr(0));
  fsClient1.Active := True;
  ComboBox3.ItemIndex := 1;
  fstable1.BlobMode := bmAuto;
  SpinEdit1Change(Nil);
  If fsClient1.Active Then
    Begin
      FSDatabase1.Open;
      fstable1.Open;
    End;
  CheckBox1Click(Nil);
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  TfsArrayField(fstable1.FieldByName('fArrayUInt8')).ShowArray := CheckBox1.Checked;
  TfsArrayField(fstable1.FieldByName('fArrayUInt16')).ShowArray := CheckBox1.Checked;
  TfsArrayField(fstable1.FieldByName('fArrayInt32')).ShowArray := CheckBox1.Checked;
  TfsArrayField(fstable1.FieldByName('fArrayDouble')).ShowArray := CheckBox1.Checked;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  v: variant;
  s: String;
  i: Integer;
Begin
  {s := '';
  v := fstable1.FieldByName('combobox1.text').AsVariant;
  For i := 0 To fstable1.FieldByName('combobox1.text').size - 1 Do
    s := s + FloatToStr(v[i]) + '  ';
  edit3.text := Trim(s);   }
  // or
  // get
  //edit3.text := fstable1.FieldByName(combobox1.text).AsString;
  //set
  fstable1.Edit;
  fstable1.FieldByName(combobox1.text).AsString := edit3.text;
  fstable1.Post;
End;

Procedure TForm1.btnLoadGenericClick(Sender: TObject);
Var
  OpenDialog1: TOpenDialog;
Begin
  OpenDialog1 := TOpenDialog.Create(Nil);
  Try
    If opendialog1.Execute Then
      Begin
        FsTable1.Edit;
        TBlobField(fstable1.FieldByName('fblob')).LoadFromFile(opendialog1.FileName);
        PageControl1Change(Nil);
      End;
  Finally
    OpenDialog1.free;
  End;
End;

Procedure TForm1.btnSaveGenericClick(Sender: TObject);
Begin
  If savedialog1.Execute Then
    Begin
      FsTable1.Edit;
      TBlobField(fstable1.FieldByName('fblob')).savetoFile(savedialog1.FileName);
    End;
End;

Procedure TForm1.btnClearGenericClick(Sender: TObject);
Begin
  FsTable1.Edit;
  TBlobField(fstable1.FieldByName('fblob')).Clear;
  PageControl1Change(Nil);
End;

Procedure TForm1.PageControl1Change(Sender: TObject);
Const
  HexChar: Array[0..15] Of char = '0123456789ABCDEF';
Var
  Stream: tfsStream;
  BlobBuffer: Array[0..1024] Of char;

  Function GenerateHexLines(Buf: pointer; BufLen: TffMemSize): String;
  Const
    HexPos: Array[0..15] Of Byte =
    (1, 3, 5, 7, 10, 12, 14, 16, 19, 21, 23, 25, 28, 30, 32, 34);
  Var
    B: PffByteArray Absolute Buf;
    ThisWidth,
      i, j: Integer;
    Line: String[56];
    Work: Byte;
  Begin
    Result := '';
    If (BufLen = 0) Or (Buf = Nil) Then
      Exit
    Else
      Begin
        If (BufLen > 1024) Then
          Begin
            BufLen := 1024;
          End;
        For i := 0 To ((BufLen - 1) Shr 4) Do
          Begin
            FillChar(Line, 56, ' ');
            Line[0] := #55;
            Line[38] := '[';
            Line[55] := ']';
            If (BufLen >= 16) Then
              ThisWidth := 16
            Else
              ThisWidth := BufLen;
            For j := 0 To ThisWidth - 1 Do
              Begin
                Work := B^[(i Shl 4) + j];
                Line[HexPos[j]] := HexChar[Work Shr 4];
                Line[HexPos[j] + 1] := HexChar[Work And $F];
                If (Work < 32) Then
                  Work := ord('.');
                Line[39 + j] := char(Work);
              End;
            Result := Result + Line + ffcCRLF;
            dec(BufLen, ThisWidth);
          End;
      End;
  End;
Begin
  //if FsTable1.State in [dsedit,dsinsert] then exit;
  If PageControl1.ActivePageIndex = 1 Then
    Begin

      Stream := FsTable1.fsCreateBlobStream(FsTable1.FieldByName('fblob'), bmRead);
      Try
        Memo1.Text := '';
        Stream.Read(BlobBuffer, FFMinL(1024, Stream.Size));
        Memo1.Text :=
          GenerateHexLines(@BlobBuffer, FFMinL(1024, Stream.Size));
      Finally
        Stream.Free;
      End;
    End;
End;

Procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
Begin
  PageControl1Change(Nil);
  CheckBox3.Checked := FSDatabase1.InTransaction;
End;

Procedure TForm1.ComboBox3Change(Sender: TObject);
Begin
  fstable1.Close;
  fstable1.BlobMode := TfsBlobMode(ComboBox3.ItemIndex);
  fstable1.Open;
  CheckBox1Click(Nil);
End;

Procedure TForm1.CheckBox2Click(Sender: TObject);
Begin
  fstable1.BlobAutoStartTransaction := CheckBox2.Checked;
End;

Procedure TForm1.DataSource1StateChange(Sender: TObject);
Begin
  CheckBox3.Checked := FSDatabase1.InTransaction;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  If Not FSDatabase1.InTransaction Then
    FSDatabase1.StartTransaction;
  CheckBox3.Checked := FSDatabase1.InTransaction;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  If FSDatabase1.InTransaction Then
    FSDatabase1.Commit;
  CheckBox3.Checked := FSDatabase1.InTransaction;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  If FSDatabase1.InTransaction Then
    FSDatabase1.Rollback;
  CheckBox3.Checked := FSDatabase1.InTransaction;
End;

Procedure TForm1.SpinEdit1Change(Sender: TObject);
Begin
  fstable1.BlobChunkSize := SpinEdit1.Value;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  FsTable1.Edit;
  TBlobField(fstable1.FieldByName('fmemo')).Clear;
End;

Procedure TForm1.CheckBox4Click(Sender: TObject);
Begin
  fstable1.BlobModifiedError := CheckBox4.Checked;
End;

End.

