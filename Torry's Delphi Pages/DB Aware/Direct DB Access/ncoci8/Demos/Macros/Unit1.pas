unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOci, NCOciWrapper, NCOciDB, StdCtrls, NCOCIParams;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OCIQuery1: TOCIQuery;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    ComboBox2: TComboBox;
    Edit2: TEdit;
    Memo1: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    Memo2: TMemo;
    procedure Memo1Exit(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure ComboBox2Click(Sender: TObject);
    procedure Edit2Exit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateResult;
    function GetMacro: TOCIMacro;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Memo1Exit(Sender: TObject);
var
    i: Integer;
begin
    OCIQuery1.SQL.Assign(Memo1.Lines);
    ListBox1.Items.Clear;
    for i := 0 to OCIQuery1.MacroCount - 1 do
        ListBox1.Items.Add(OCIQuery1.Macros[i].Name);
    if ListBox1.Items.Count > 0 then
        ListBox1.ItemIndex := 0
    else
        ListBox1.ItemIndex := -1;
    UpdateResult;
end;

function TForm1.GetMacro: TOCIMacro;
begin
    if ListBox1.ItemIndex = -1 then
        Result := nil
    else
        Result := OCIQuery1.MacroByName(ListBox1.Items[ListBox1.ItemIndex]);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
    if ListBox1.ItemIndex = -1 then begin
        Edit1.Text := '';
        ComboBox1.ItemIndex := -1;
        ComboBox2.ItemIndex := -1;
        Edit2.Text := '';
    end
    else
        with GetMacro do begin
            Edit1.Text := Name;
            ComboBox1.ItemIndex := Integer(MacroType);
            ComboBox2.ItemIndex := Integer(DataType);
            Edit2.Text := AsString;
        end;
end;

procedure TForm1.Edit1Exit(Sender: TObject);
begin
    if ListBox1.ItemIndex <> -1 then begin
        GetMacro.Name := Edit1.Text;
        UpdateResult;
    end;
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
    if ListBox1.ItemIndex <> -1 then begin
        GetMacro.MacroType := TOCIMacroType(ComboBox1.ItemIndex);
        UpdateResult;
    end;
end;

procedure TForm1.ComboBox2Click(Sender: TObject);
begin
    if ListBox1.ItemIndex <> -1 then begin
        GetMacro.DataType := TOCIMacroDataType(ComboBox2.ItemIndex);
        UpdateResult;
    end;
end;

procedure TForm1.Edit2Exit(Sender: TObject);
begin
    if ListBox1.ItemIndex <> -1 then begin
        GetMacro.AsString := Edit2.Text;
        UpdateResult;
    end;
end;

procedure TForm1.UpdateResult;
begin
    ListBox1Click(nil);
    Memo2.Lines.Text := OCIQuery1.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Memo1Exit(nil);
end;

end.
