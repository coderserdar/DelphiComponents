unit QzDirectoryCombo;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, CakComboBoxEx, ShellCtrls, Cakdir2;

type
  TQzDirectoryCombo = class(TCakComboBoxEx)
  private
    { Private declarations }
    TreeView1: TShellTreeView;
    CakDir : TCakdir2;
    aPanel : TPanel;
    procedure TreeView1Click(Sender: TObject);
    procedure SetDirectory(value :string);
    function GetDirectory : string;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure FormDropDown(Sender : TObject);
    procedure Paint; override;
  published
    { Published declarations }
    property Directory : string read GetDirectory write SetDirectory;
    property Cakdir2 : TCakdir2 read CakDir write CakDir;
    property TreeView : TShellTreeView read TreeView1 write TreeView1;
  end;

procedure Register;

implementation

constructor TQzDirectoryCombo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  OnDroppedDown := FormDropDown;

  TreeView1 := TShellTreeView.Create(DropDownForm);
  TreeView1.Parent := DropDownForm;
  TreeView1.Align := alClient;
  TreeView1.OnClick := TreeView1Click;
end;

destructor TQzDirectoryCombo.Destroy;
begin
  TreeView1.Free;
  inherited Destroy;
end;

procedure TQzDirectoryCombo.FormDropDown(Sender : TObject);
begin
//
end;

procedure TQzDirectoryCombo.paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,Width,Height));
  if Assigned(DropDownForm) then
  begin
      if Assigned(Cakdir) and (Cakdir.ReturnIconType('*DIR*') <> -1) then
        Cakdir.ImageS.Draw(Canvas,2,0,Cakdir.ReturnIconType('*DIR*'));
      Canvas.TextOut(20,2,Format('%s',[Directory]))
  end;
end;

procedure TQzDirectoryCombo.TreeView1Click(Sender: TObject);
begin
  //DropDownForm.Close;
  if Assigned(OnChanged) then
    OnChanged(Sender);
  Invalidate;
end;

procedure TQzDirectoryCombo.SetDirectory(value :string);
begin
  if Assigned(DropDownForm) then
    TreeView1.Path := value;
end;

function TQzDirectoryCombo.GetDirectory : string;
begin
  if Assigned(DropDownForm) then
    Result := TreeView1.Path;
end;

procedure Register;
begin
  RegisterComponents('QZip', [TQzDirectoryCombo]);
end;

end.
