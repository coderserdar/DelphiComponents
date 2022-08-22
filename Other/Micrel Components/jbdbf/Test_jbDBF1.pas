unit Test_jbDBF1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Gauges, ExtCtrls, StdCtrls, jbDbf;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Gauge1: TGauge;
    MainMenu1: TMainMenu;
    Database1: TMenuItem;
    Create1: TMenuItem;
    Reindex1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    est1: TMenuItem;
    Createtestdata1: TMenuItem;
    Deletetestdata1: TMenuItem;
    About1: TMenuItem;
    About2: TMenuItem;
    Panel4: TPanel;
    DBF_filename: TEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    N2: TMenuItem;
    Searchup1: TMenuItem;
    Searchdown1: TMenuItem;
    DBF1: TjbDBF;
    procedure About2Click(Sender: TObject);
    procedure Searchup1Click(Sender: TObject);
    procedure Searchdown1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Create1Click(Sender: TObject);
    procedure DBF1Closed(Sender: TObject);
    procedure DBF1Opened(Sender: TObject; IsOpened: Boolean);
    procedure DBF1Found(Sender: TObject);
    procedure DBF1Deleted(Sender: TObject);
    procedure DBF1Update(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

const
  ccRecNo = 100;

  jmspoub = 'test_y.dbf';
  jmidxcislo = 'cislo';
  jmidxtest = 'test';

procedure TfrmMain.About2Click(Sender: TObject);
begin
  MessageDlg('Test program for jbDBF advanced edition'#13#10'(c)2009 Jaro Benes', mtInformation, [mbOK], 0);
end;

function RandomAnyNameCreate(PLen: Integer): string;
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

procedure TfrmMain.Create1Click(Sender: TObject);
var
  S: string;
  I, J: Integer;
  A: array[1..ccRecNo] of string[8];
  OK: Boolean;
begin
  {zrus predchozi}
  Create1.Enabled := False;
  try
  SysUtils.DeleteFile(DBF_filename.Text + jmspoub);
  SysUtils.DeleteFile(DBF_filename.Text + jmidxcislo + '.idx');
  SysUtils.DeleteFile(DBF_filename.Text + jmidxtest + '.idx');
  {vyrob novou tabulku s indexy}
  with DBF1 do
  begin
    ClearFields;
    MakeField(1, jmidxcislo, dbtNumber, 10, 0, jmidxcislo, dbfUnique, dbfDescending);
    MakeField(2, jmidxtest, dbtString, 50, 0, jmidxtest, dbfDuplicates, dbfAscending);
    CreateDB(DBF_filename.Text + jmspoub);
    FileName := DBF_filename.Text + jmspoub;
    {make some fields}
    for I := 1 to ccRecNo do
    begin
      repeat
        OK := True;
        S := RandomAnyNameCreate(8);
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
    end;
  end;
  finally
    Create1.Enabled := True; //je to priklad, nekontroluje predchozi existenci
  end;
end;

procedure TfrmMain.DBF1Closed(Sender: TObject);
begin
  Panel2.Caption := 'Close';
end;

procedure TfrmMain.DBF1Deleted(Sender: TObject);
begin
  Panel2.Caption := 'Delete';
end;

procedure TfrmMain.DBF1Found(Sender: TObject);
begin
  Panel2.Caption := 'Finding...';
end;

procedure TfrmMain.DBF1Opened(Sender: TObject; IsOpened: Boolean);
begin
  Panel2.Caption := 'Open';
end;

procedure TfrmMain.DBF1Update(Sender: TObject);
begin
  Panel2.Caption := 'Update';
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  DBF1.Close;
  Close;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DBF_filename.Text := 'c:\_testdbf\'; {toto je cesta k souboru}
  //make directory when doesn't exists
  ForceDirectories(DBF_filename.Text);
  ListBox1.Clear;
end;

procedure TfrmMain.Searchdown1Click(Sender: TObject);
begin
  ListBox1.Clear;
  {open DBF by file name}
  DBF1.FileName := DBF_filename.Text + jmspoub;
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

procedure TfrmMain.Searchup1Click(Sender: TObject);
begin
  ListBox1.Clear;
  {open DBF by file name}
  DBF1.FileName := DBF_filename.Text + jmspoub;
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

end.