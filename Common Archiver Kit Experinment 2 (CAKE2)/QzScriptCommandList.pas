unit QzScriptCommandList;

interface

uses
  SysUtils, Classes, Controls, ComCtrls, ScriptUtils;

type
  TQzScriptCommandList = class(TListView)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    procedure SclSelectItem(Sender : TObject; Item: TListItem; Selected: Boolean);
    procedure SclInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
    procedure AddScriptCmd2List(ID : integer);
    function GetScriptCmdHint(ID : integer) : string;
    procedure RefillScriptCmdList;
  published
    { Published declarations }
  end;

procedure Register;

implementation


constructor TQzScriptCommandList.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self.OnSelectItem := SclSelectItem;
  Self.OnInfoTip := SclInfoTip;
  ShowHint := true;
end;

procedure TQzScriptCommandList.SclSelectItem(Sender : TObject; Item: TListItem; Selected: Boolean);
begin
  if Assigned(Item) then
    Hint := GetScriptCmdHint(Item.StateIndex) else
    Hint := '';
end;

procedure TQzScriptCommandList.SclInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
begin
  if Assigned(Item) then
    InfoTip := GetScriptCmdHint(Item.StateIndex) else
    InfoTip := '';
end;

procedure TQzScriptCommandList.RefillScriptCmdList;
var i : integer;
begin
  Items.Clear;
  for i := 0 to MaxScript -1 do
    AddScriptCmd2List(i);
end;

function TQzScriptCommandList.GetScriptCmdHint(ID : integer) : string;
begin
  Result := ScriptCmdList[ID].CommandHelp;
end;

procedure TQzScriptCommandList.AddScriptCmd2List(ID : integer);
var aItem : TListItem;
begin
  with ScriptCmdList[ID] do
    begin
      aItem := Items.Add;
      aItem.ImageIndex := ImgIndex;
      aItem.StateIndex := ID;
      aItem.Caption := CommandName;
    end;
end;


procedure Register;
begin
  RegisterComponents('QZip', [TQzScriptCommandList]);
end;

end.
