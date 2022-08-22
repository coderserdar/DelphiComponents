unit Testdbf1;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, jbDbf, Gauges, ExtCtrls, FileCtrl;


type
  TForm1 = class(TForm)
    Button1: TButton;
    DBF1: TjbDBF;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Gauge1: TGauge;
    Label1: TLabel;
    ListBox1: TListBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Edit2: TEdit;
    Label2: TLabel;
    Bevel1: TBevel;
    Edit3: TEdit;
    Edit4: TEdit;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure DBF1Found(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DBF1Progress(Sender: TObject; const Operace: string;
      Progress: Integer);
    procedure DBF1Query(Sender: TObject; const IdxName, IdxField,
      Key: string; var Accept, Cancel: Boolean);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure Edit3Exit(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure DBF1Opened(Sender: TObject; IsOpened: Boolean);
    procedure DBF1Closed(Sender: TObject);
  private
    { Private declarations }
    N: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses jbStrLst;

const
  ccRecNo = 100;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close
end;

const
  jmspoub = 'test_y.dbf';
  jmidxcislo = 'cislo';
  jmidxtest = 'test';

function RandomNejakeJmeno(PLen: Integer): string;
var
  str: string;
begin
  Randomize;
  str := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until (Length(Result) = PLen)
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  S: string;
  I, J: Integer;
  A: array[1..ccRecNo] of string[8];
  OK: Boolean;
begin
  {zrus predchozi}
  SysUtils.DeleteFile(Edit2.Text + jmspoub);
  SysUtils.DeleteFile(Edit2.Text + jmidxcislo + '.idx');
  SysUtils.DeleteFile(Edit2.Text + jmidxtest + '.idx');
  {vyrob novou tabulku s indexy}
  with DBF1 do
  begin
    ClearFields;
    MakeField(1, jmidxcislo, dbtNumber, 10, 0, jmidxcislo, dbfUnique, dbfDescending);
    MakeField(2, jmidxtest, dbtString, 50, 0, jmidxtest, dbfDuplicates, dbfAscending);
    CreateDB(Edit2.Text + jmspoub);
    FileName := Edit2.Text + jmspoub;
    {make some fields}
    for I := 1 to ccRecNo do
    begin
      repeat
        OK := True;
        S := RandomNejakeJmeno(8);
        for J := 1 to I - 1 do //jen jeden vyskyt
          if A[J] = S then
          begin
            OK := False;
            Break
          end
      until OK;
      A[I] := S //sup do pole
    end;
    if Open then
    begin
      Gauge1.MaxValue := ccRecNo;
      for I := 1 to ccRecNo do begin
        InternalSave(jmidxcislo, IntToStr(I));
        InternalSave(jmidxtest, {'Ahojky '} A[I] + ' ' + IntToStr(I));
        InternalAppend;
        Application.ProcessMessages;
        Gauge1.Progress := I;
      end;
      Gauge1.MaxValue := 100;
      {preindexuj}
      Reindex;
      {RemoveIndexes(1);}
      Gauge1.Progress := 0;
      Panel2.Caption := '';
      Close;
      Button3.Enabled := True; //je to priklad, nekontroluje predchozi existenci
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  N := 0;
  {find proc}
  with DBF1 do begin
    FileName := Edit2.Text + jmspoub;
    if Open then begin
      Find(jmidxcislo + '.idx', jmidxcislo); {hledej pres Found+Query}
      Gauge1.Progress := 0;
      Panel2.Caption := '';
      Close
    end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  {open DBF by file name}
  DBF1.FileName := Edit2.Text + jmspoub;
  {opening}
  if DBF1.Open then
  try
    {set to first record}
    DBF1.First;
    {go through to bottom table}
    while not DBF1.Eof do begin
      {special test, is beginning for character}
      if DBF1['test'].BeginString('b') then
        {add it to listox}
        listbox1.Items.Add(DBF1['test'].AsString);
      {and go next record}
      DBF1.Next;
    end;
  finally
    {afret thel close table}
    DBF1.Close;
  end;
  {and show results}
  listbox1.Visible := listbox1.Count > 0;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  {open DBF by file name}
  DBF1.FileName := Edit2.Text + jmspoub;
  {opening}
  if DBF1.Open then
  try
    {set to first record}
    DBF1.Last;
    {go through to bottom table}
    while not DBF1.Bof do begin
      {special test, is beginning for character}
      if DBF1['test'].BeginString('a') then
        {add it to listox}
        listbox1.Items.Add(DBF1['test'].AsString);
      {and go next record}
      DBF1.Prior;
    end;
  finally
    {afret thel close table}
    DBF1.Close;
  end;
  {and show results}
  listbox1.Visible := listbox1.Count > 0;
end;

procedure TForm1.DBF1Closed(Sender: TObject);
begin
  Panel2.Caption := 'Close';
end;

procedure TForm1.DBF1Found(Sender: TObject);
begin
  //find
  {standardni hlasky}
  if Radiobutton1.Checked then begin
    Label1.Caption := 'Found (' + DBF1.Load(jmidxcislo) + '):';
    Edit1.Text := DBF1.Load(jmidxtest);
  end
  else begin
    Inc(N);
    Label1.Caption := 'Found ' + IntToStr(N) + ' fields:';
    ListBox1.Items.Add(DBF1.Load(jmidxcislo) + ' -- ' + DBF1.Load(jmidxtest));
  end;
end;

procedure TForm1.DBF1Opened(Sender: TObject; IsOpened: Boolean);
begin
  Panel2.Caption := 'Open';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit2.Text := 'c:\_testdbf\'; {toto je cesta k souboru}
  //make directory when doesn't exists
  ForceDirectories(Edit2.Text);
  Edit1.Text := '';
  Label1.Caption := '';
  N := 0;
  ListBox1.Clear;
end;

procedure TForm1.DBF1Progress(Sender: TObject; const Operace: string;
  Progress: Integer);
begin
  Gauge1.Progress := Progress;
  Panel2.Caption := Operace;
  Application.ProcessMessages
end;

procedure TForm1.DBF1Query(Sender: TObject; const IdxName, IdxField,
  Key: string; var Accept, Cancel: Boolean);
var I, I1, I2: Integer;
begin
  //jen priprava promennych
  try
    I1 := StrToInt(Edit3.Text);
    if not (I1 in [1..100]) then I1 := 1;
  except
    I1 := 1;
  end;
  try
    I2 := StrToInt(Edit4.Text);
    if not (I2 in [1..100]) then I2 := 1;
  except
    I2 := 100
  end;
  if I1 > I2 then
  begin
    I := I1; I1 := I2; I2 := I;
  end;
  {zde realizace dotazu a accept/cancel}
  if Key <> '' then
    if Radiobutton1.Checked then begin
      if StrToInt(Key) = I1 then begin
        Accept := True; {tento si vezmi}
        Cancel := True; {ale dalsi jiz odmitni}
      end;
    end
    else begin
      if (StrToInt(Key) >= I1) and (StrToInt(Key) <= I2) then Accept := True;
    end;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  Edit4.Visible := not RadioButton1.Checked;
  Edit1.Visible := RadioButton1.Checked;
  Listbox1.Visible := not RadioButton1.Checked;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  Edit4.Visible := RadioButton2.Checked;
  Edit1.Visible := not RadioButton2.Checked;
  Listbox1.Visible := RadioButton2.Checked;
end;

procedure TForm1.Edit3Exit(Sender: TObject);

begin
  try
    TEdit(Sender).Text := IntToStr(StrToInt(Trim(TEdit(Sender).Text)))
  except
    TEdit(Sender).Text := '';
  end;
end;

end.

