unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo;

type
  TFormMain = class(TForm)
    Memo: TMemo;
    ButtonTest1: TButton;
    ButtonTest2: TButton;
    ButtonTest3: TButton;
    procedure ButtonTest1Click(Sender: TObject);
    procedure ButtonTest2Click(Sender: TObject);
    procedure ButtonTest3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses System.IOUtils, Csv;

function GetFileName(const FileName: string): string;
begin
  Result := TPath.GetDocumentsPath + PathDelim + FileName;
end;

procedure TFormMain.ButtonTest1Click(Sender: TObject);
var i, j: Integer;
begin
  Memo.Text := '';

  with TCsv.Create do
    try
      LineCount := 10;
      for i := 0 to LineCount - 1 do
      begin
        FieldCount[i] := 3;
        for j := 0 to FieldCount[i] - 1 do
          Fields[i, j] := 'Field ' + IntToStr(i) + ' ' + IntToStr(j);
      end;

      Memo.Text := ToString;
    finally
      Free;
    end;
end;

procedure TFormMain.ButtonTest2Click(Sender: TObject);
begin
  Memo.Text := '';

  with TCsvWriter.Create(GetFileName('export.csv')) do
  try
    Write('Field 1 1');
    Write('Field 1 2');
    Write('Field 1 3');

    NextLine;

    Write('Field 2 1');
    Write('Field 2 2');
    Write('Field 2 3');

    NextLine;

    Write('Field 3 1');
    Write('Field 3 2');
    Write('Field 3 3');
  finally
    Free;
  end;

  with TCsv.Create do
  try
    LoadUtf8File(GetFileName('export.csv'));
    Memo.Text := ToString;
  finally
    Free;
  end;
end;

procedure TFormMain.ButtonTest3Click(Sender: TObject);
begin
  Memo.Text := '';

  with TCsv.Create do
  try
    LoadUtf8File(GetFileName('Country.csv'));
    Memo.Text := ToString;
  finally
    Free;
  end;
end;

end.
