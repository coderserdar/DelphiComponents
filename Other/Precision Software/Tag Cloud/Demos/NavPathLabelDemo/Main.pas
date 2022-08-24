unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, NavPathLabel, ExtCtrls;

type
  TfrmMain = class(TForm)
    pLeft: TPanel;
    pMain: TPanel;
    Splitter1: TSplitter;
    pPath: TPanel;
    lbPath: TNavPathLabel;
    tvTree: TTreeView;
    lbContent: TLabel;
    pOptions: TPanel;
    Label1: TLabel;
    cbNavPathDelim: TComboBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure tvTreeChange(Sender: TObject; Node: TTreeNode);
    procedure lbPathPathClick(Sender: TObject; const Path: string; const aRect: TRect);
    procedure cbNavPathDelimChange(Sender: TObject);
    procedure lbContentMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    function GetNodePath(Node:TTreeNode;const Delim:string):string;
    function FindNodeByText(ParentNode:TTreeNode;const Text:string):TTreeNode;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DesktopFont:=True;
  lbPath.Font.Color := clNavy;
  tvTree.FullExpand;
end;

function TfrmMain.GetNodePath(Node:TTreeNode;const Delim:string):string;
begin
  if Node=nil then
    Result:=''
  else
  begin
    Result:=Node.Text;
    Node:=Node.Parent;
    while Assigned(Node) do
    begin
      Result:=Node.Text+Delim+Result;
      Node:=Node.Parent;
    end;
  end;
end;

function TfrmMain.FindNodeByText(ParentNode:TTreeNode;const Text:string):TTreeNode;
var
  N:TTreeNode;
begin
  Result:=nil;
  if ParentNode=nil then
    N:=tvTree.Items.GetFirstNode
  else
    N:=ParentNode.getFirstChild;
  while Assigned(N) do
  begin
    if AnsiSameText(Text, N.Text) then
    begin
      Result:=N;
      Break;
    end;
    N:=N.getNextSibling;
  end;
end;

procedure TfrmMain.lbPathPathClick(Sender: TObject; const Path: string; const aRect: TRect);
var
  SN,N:TTreeNode;
  i:Integer;
begin
  SN:=nil;
  i:=0;
  N:=nil;
  while i<=lbPath.HoverPathIndex do
  begin
    N:=FindNodeByText(SN,lbPath.GetPathItemAtIndex(i));
    if N=nil then
      break
    else
      SN:=N;
    Inc(i);
  end;
  if Assigned(SN) then
  begin
    SN.Selected:=True;
    SN.Focused:=True;
  end;
end;

procedure TfrmMain.tvTreeChange(Sender: TObject; Node: TTreeNode);
begin
  lbPath.Caption:=GetNodePath(Node,lbPath.PathDelimiter);
  if Node=nil then
    lbContent.Caption:='No item selected!'
  else
    lbContent.Caption:='Currently selected node: '#13#10+Node.Text;
end;

procedure TfrmMain.cbNavPathDelimChange(Sender: TObject);
var
  tmp:string;
begin
  tmp:=cbNavPathDelim.Text;
  if AnsiSameText(tmp,'Arrow') then
    tmp:='a';
  if Length(tmp)<=2 then
    tmp:='  '+tmp+'  ';
  lbPath.NavPathDelimiter:=tmp;
end;

procedure TfrmMain.lbContentMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button=mbLeft then
    lbPath.Font.Size:=lbPath.Font.Size+1
  else
    lbPath.Font.Size:=lbPath.Font.Size-1
end;


end.
