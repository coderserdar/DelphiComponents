unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TreeView, WinJson, FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    ButtonTest1: TButton;
    ButtonTest2: TButton;
    ButtonTest3: TButton;
    TreeView: TTreeView;
    ButtonTest4: TButton;
    procedure ButtonTest1Click(Sender: TObject);
    procedure ButtonTest2Click(Sender: TObject);
    procedure ButtonTest3Click(Sender: TObject);
    procedure ButtonTest4Click(Sender: TObject);
  private
    { Private declarations }
    procedure Show(Parent: TControl; const Prefix: string; JsonItem: TJson); overload;
    procedure Show(JsonItem: TJson); overload;
    procedure ShowFile(const FileName: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses System.IOUtils;

function GetFileName(const FileName: string): string;
begin
  Result := TPath.GetDocumentsPath + PathDelim + FileName;
end;

procedure TFormMain.ShowFile(const FileName: string);
var Json: TJson;
begin
  with TJsonParser.Create do
  try
    Json := ParseUtf8File(GetFileName(FileName));
    try
      Show(Json);
    finally
      FreeAndNil(Json);
    end;
  finally
    Free;
  end;
end;

procedure TFormMain.Show(JsonItem: TJson);
begin
  TreeView.BeginUpdate;
  try
    TreeView.Clear;
    Show(TreeView, '', JsonItem);

    // switch off checkboxes
    TreeView.ShowCheckboxes := True;
    TreeView.ShowCheckboxes := False;
  finally
    TreeView.EndUpdate;
  end;
end;

procedure TFormMain.Show(Parent: TControl; const Prefix: string; JsonItem: TJson);
var
  Child: TTreeViewItem;
  JsonArray: TJsonArray;
  JsonObject: TJsonObject;
  i: Integer;

  function AddChild(Parent: TControl; const Text: string): TTreeViewItem;
  begin
    Result := TTreeViewItem.Create(Parent);
    Result.Text := Text;
    Parent.AddObject(Result);
    Result.ExpandAll;
  end;

begin
  if JsonItem is TJsonNull then
    AddChild(Parent, Prefix + 'null')
  else if JsonItem is TJsonFalse then
    AddChild(Parent, Prefix + 'false')
  else if JsonItem is TJsonTrue then
    AddChild(Parent, Prefix + 'true')
  else if JsonItem is TJsonNumber then
    AddChild(Parent, Prefix + FloatToStr(TJsonNumber(JsonItem).Value))
  else if JsonItem is TJsonString then
    AddChild(Parent, Prefix + '"' + TJsonString(JsonItem).Value + '"')
  else if JsonItem is TJsonArray then
  begin
    Child := AddChild(Parent, Prefix + '[]');
    JsonArray := TJsonArray(JsonItem);
    for i := 0 to JsonArray.ElementCount - 1 do
      Show(Child, '[' + IntToStr(i) + '] ', JsonArray.Elements[i]);
  end
  else if JsonItem is TJsonObject then
  begin
    Child := AddChild(Parent, Prefix + '{}');
    JsonObject := TJsonObject(JsonItem);
    for i := 0 to JsonObject.MemberCount - 1 do
      Show(Child, JsonObject.MemberName[i] + ': ', JsonObject.MemberValue[i]);
  end;
end;

procedure TFormMain.ButtonTest1Click(Sender: TObject);
begin
  ShowFile('example1.json');
end;

procedure TFormMain.ButtonTest2Click(Sender: TObject);
var
  MyArray: TJsonArray;
  MyObject: TJsonObject;
begin
  with TJsonObject.Create do
  try
    SetNull('Null');
    SetTrue('True');
    SetFalse('False');
    SetNumber('Number', 1.23);
    SetString('String', 'hello');
    SetDateTime('DateTime', Now);

    MyArray := SetArray('Array', 3);
    MyArray.SetString(0, 'First item');
    MyArray.SetString(1, 'Second item');
    MyArray.SetNumber(2, 3.21);

    MyObject := SetObject('Object');
    MyObject.SetString('First Member', 'Hello');
    MyObject.SetString('Second Member', 'World');
    MyObject.SetFalse('Third Member');
    MyObject.SetTrue('Fourth Member');
    MyObject.SetNumber('Other Member', 1.11);

    ShowMessage(ToString);
    ToUtf8File(GetFileName('new.json'));

    ShowFile('new.json');
  finally
    Free;
  end;
end;

procedure TFormMain.ButtonTest3Click(Sender: TObject);
begin
  with TJsonWriter.Create(GetFileName('new.json')) do
  try
    BeginObject;
      WriteNull('Null');
      Write('True', True);
      Write('False', False);
      Write('Number', 1.23);
      Write('String', 'hello');
      Write('DateTime', Now, 0);

      BeginArray('Array');
        Write('First item');
        Write('Second item');
        Write(3.21);
      EndArray;

      BeginObject('Object');
        Write('First Member', 'Hello');
        Write('Second Member', 'World');
        Write('Third Member', False);
        Write('Fourth Member', True);
        Write('Other Member', 1.11);
      EndObject;
    EndObject;

    Check;
  finally
    Free;
  end;

  ShowFile('new.json');
end;

procedure TFormMain.ButtonTest4Click(Sender: TObject);
begin
  ShowFile('example2.json');
end;

end.