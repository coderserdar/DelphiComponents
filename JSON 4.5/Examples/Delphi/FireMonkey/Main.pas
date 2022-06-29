unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TreeView, WinJson;

type
  TFormMain = class(TForm)
    ButtonOpenFile: TButton;
    TreeView: TTreeView;
    OpenDialog: TOpenDialog;
    procedure ButtonOpenFileClick(Sender: TObject);
  private
    { Private declarations }
    procedure Show(Parent: TControl; const Prefix: string; JsonItem: TJson);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonOpenFileClick(Sender: TObject);
var JsonItem: TJson;
begin
  if OpenDialog.Execute then
  begin
    TreeView.BeginUpdate;
    try
      TreeView.Clear;
      with TJsonParser.Create do
      try
        JsonItem := ParseUtf8File(OpenDialog.FileName);
        try
          Show(TreeView, '', JsonItem);
        finally
          JsonItem.Free;
        end;
      finally
        Free;
      end;
    finally
      TreeView.EndUpdate;
    end;
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

end.
