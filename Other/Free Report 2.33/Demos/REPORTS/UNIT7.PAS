unit Unit7;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_DSet, FR_Class;

type
  TForm7 = class(TForm)
    frReport1: TfrReport;
    Dialog1: TOpenDialog;
    TextFileDataset: TFRUserDataset;
    procedure frReport1GetValue(const ParName: string;
      var ParValue: Variant);
    procedure frReport1EnterRect(Memo: TStringList; View: TfrView);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure StartReport;
  end;

var
  Form7: TForm7;

implementation

{$R *.DFM}

var FName: String;

procedure TForm7.StartReport;
begin
  with Dialog1 do
    if Execute then
    begin
      FName := FileName;
      frReport1.ShowReport;
    end;
end;

procedure TForm7.frReport1GetValue(const ParName: string; var ParValue: Variant);
begin
  if ParName = 'Файл' then
    ParValue := FName
end;

procedure TForm7.frReport1EnterRect(Memo: TStringList; View: TfrView);
begin
  if Memo.Count > 0 then
    if Memo[0] = '[Строка]' then
      Memo.LoadFromFile(FName);
end;

end.
