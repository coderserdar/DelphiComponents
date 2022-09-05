unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, SpTBXFormPopupMenu;

type
  TForm3 = class(TForm)
    TreeView1: TTreeView;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    function AddIcon(Fill: TColor; Circular: Boolean = False): Integer;
    procedure CreateNodesProc(const S: string);
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

function TForm3.AddIcon(Fill: TColor; Circular: Boolean = False): Integer;
var
  B: TBitmap;
  R: TRect;
begin
  B := TBitmap.Create;
  try
    B.Width := 16;
    B.Height := 16;
    B.Canvas.Brush.Color := Fill;
    R := Rect(0, 0, B.Width, B.Height);
    InflateRect(R, -1, -1);
    if Circular then
      B.Canvas.Ellipse(R)
    else
      B.Canvas.Rectangle(R);
    Result := ImageList1.Add(B, nil);
  finally
    B.Free;
  end;
end;

procedure TForm3.CreateNodesProc(const S: string);
var
  ParentNode: TTreeNode;
  C: TColor;
begin
  // Add the color node
  C := StringToColor(S);
  ParentNode := TreeView1.Items.GetFirstNode;
  if C < 0 then
    ParentNode := ParentNode.getNextSibling;
  with TreeView1.Items.AddChildObject(ParentNode, S, Pointer(C)) do begin
    ImageIndex := AddIcon(C);
    SelectedIndex := ImageIndex;
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  I := AddIcon(clWhite, True);
  // Add the parent nodes to the TreeView
  with TreeView1.Items.Add(nil, 'Standard Colors') do begin
    ImageIndex := I;
    SelectedIndex := ImageIndex;
  end;
  with TreeView1.Items.Add(nil, 'System Colors') do begin
    ImageIndex := I;
    SelectedIndex := ImageIndex;
  end;
  // Add the color nodes
  GetColorValues(CreateNodesProc);
end;

procedure TForm3.TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if Button = mbLeft then begin
    Node := TreeView1.GetNodeAt(X, Y);
    if Assigned(Node) and Node.Selected and (Node.Level > 0) then begin
      // Inform the ActiveFormPopupMenu that a selection was made.
      if Assigned(ActiveFormPopupMenu) then
        ActiveFormPopupMenu.ClosePopup(True);
    end;
  end;
end;

end.
