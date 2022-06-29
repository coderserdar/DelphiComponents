unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XPMan, StdCtrls, ComCtrls, ExtCtrls, WinJson;

type
  TFormMain = class(TForm)
    XPManifest: TXPManifest;
    TreeView: TTreeView;
    Panel: TPanel;
    ButtonOpenFile: TButton;
    OpenDialog: TOpenDialog;
    procedure ButtonOpenFileClick(Sender: TObject);
  private
    { Private declarations }
    procedure Show(Parent: TTreeNode; const Prefix: string; JsonItem: TJson);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.ButtonOpenFileClick(Sender: TObject);
var JsonItem: TJson;
begin
  if OpenDialog.Execute then
  begin
    TreeView.Items.BeginUpdate;
    try
      TreeView.Items.Clear;
      with TJsonParser.Create do
      try
        JsonItem := ParseUtf8File(OpenDialog.FileName);
        try
          Show(nil, '', JsonItem);
          TreeView.FullExpand;
        finally
          JsonItem.Free;
        end;
      finally
        Free;
      end;
    finally
      TreeView.Items.EndUpdate;
    end;    
  end;
end;

procedure TFormMain.Show(Parent: TTreeNode; const Prefix: string; JsonItem: TJson);
var
  Child: TTreeNode;
  JsonArray: TJsonArray;
  JsonObject: TJsonObject;
  i: Integer;
begin
  with TreeView.Items do
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