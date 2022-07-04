

unit ReportCtrls;

{$I STD.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GraphUtils, ImgList, ComCtrls, ReportTools,
  ScrollCtrl;

{ TReportForm form }

type
  TReportForm = class(TForm)
    GroupBox: TGroupBox;
    TitleLabel: TLabel;
    AuthorLabel: TLabel;
    VersionLabel: TLabel;
    DescriptionLabel: TLabel;
    TextLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    Image: TImage;
    ImageBevel: TBevel;
    Images: TImageList;
    ParamBox: TGroupBox;
    ParamScrollList: TParamScrollList;
    TreeView: TTreeView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FReportsGroup: string;
    FReportsLength: Integer;
    FReports: array of IReport;
    FNode: TTreeNode;
    function GetActiveReport: IReport;
    procedure DoErrorInput(Sender: TObject);
  protected
    property ActiveReport: IReport read GetActiveReport;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshReports;
  end;

{ TReportDialog component }

  TReportDialog = class(TComponent)
  private
    FReport: IReport;
    FAutoPrint: Boolean;
    FReportGroup: string;
    FReportName: string;
  public
    function Execute: Boolean;
    property Report: IReport read FReport;
  published
    property AutoPrint: Boolean read FAutoPrint write FAutoPrint;
    property ReportGroup: string read FReportGroup write FReportGroup;
    property ReportName: string read FReportName write FReportName;
  end;

implementation

{$R *.DFM}

{ TReportForm }

constructor TReportForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClientWidth := GroupBox.Left * 2 + GroupBox.Width;
  ClientHeight := OKButton.Top + 32;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - (Height + ParamScrollList.Height)) div 2;
  RefreshReports;
end;

procedure EnumReportsProc(Report: IUnknown; Data: Pointer); stdcall;
const
  Delta = 10;
var
  Item: IReport;
  Form: TReportForm absolute Data;
begin
  Item := Report as IReport;
  with Form do
  begin
     if UpperCase(Item.Group) <> FReportsGroup then
     begin
       if FReportsLength mod Delta = 0 then
         SetLength(FReports, Length(FReports) + Delta);
       FReportsGroup := UpperCase(Item.Group);
       FReports[FReportsLength] := nil;
       Inc(FReportsLength);
       FNode := TreeView.Items.AddChild(nil, Item.Group);
       Fnode.ImageIndex := 0;
     end;
     if FReportsLength mod Delta = 0 then
       SetLength(FReports, Length(FReports) + Delta);
     FReports[FReportsLength] := Report as IReport;
     Inc(FReportsLength);
     with TreeView.Items.AddChild(FNode, Item.Name) do
     begin
       ImageIndex := -1;
       SelectedIndex := -1;
     end;
     FNode.Expand(True);
  end;
end;

procedure TReportForm.RefreshReports;
begin
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    FReportsGroup := '';
    FReportsLength := 0;
    FReports := nil;
    FNode := nil;
    EnumReports(EnumReportsProc, Self);
    if FReportsLength > 0 then
      SetLength(FReports, FReportsLength);
  finally
    TreeView.Items.EndUpdate;
  end;
end;

function TReportForm.GetActiveReport: IReport;
begin
  Result := nil;
  if TreeView.Selected <> nil then
    Result := FReports[TreeView.Selected.AbsoluteIndex];
end;

procedure TReportForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TReportForm.TreeViewCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  PriorColor: TColor;
  Rect: TRect;
  Index: Integer;
  S: string;
begin
  DefaultDraw := False;
  with Sender as TTreeView, Canvas  do
  begin
    if Node.Level = 0 then
      Font.Style := [fsBold]
    else
      Font.Style := [];
    Rect := Node.DisplayRect(False);
    FillRect(Rect);
    InflateRect(Rect, -2, 0);
    Index := Node.AbsoluteIndex;
    if FReports[Index] = nil then
    begin
      if cdsSelected in State then
        DrawNode(Canvas.Handle, Rect, Node.Expanded, COLOR_WINDOW)
      else
        DrawNode(Canvas.Handle, Rect, Node.Expanded);
      Images.Draw(Canvas, Rect.Left + 12, Rect.Top, 0);
      S := FReports[Index + 1].Group;
    end
    else
      S := FReports[Index].Name;
    Inc(Rect.Left, 32);
    DrawCaption(Handle, Rect, S, daLeft);
  end;
end;

procedure TReportForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if Node <> nil then
  begin
    try
      ParamScrollList.Report := FReports[Node.AbsoluteIndex];
    except
      TreeView.Selected := FNode;
      raise;
    end;
    OKButton.Enabled := FReports[Node.AbsoluteIndex] <> nil;
    if OKButton.Enabled then
      with FReports[Node.AbsoluteIndex] do
      begin
        TitleLabel.Caption := {'Title: ' +} Name;
        AuthorLabel.Caption := 'Author: ' + Author;
        DescriptionLabel.Caption := 'Description';
        VersionLabel.Caption := 'Version: ' + Format('%.2f',
          [Version]);
        TextLabel.Caption := Description;
      end
    else
    begin
      TitleLabel.Caption := 'Group: ' + FReports[Node.AbsoluteIndex + 1].Group;
      AuthorLabel.Caption := '';
      DescriptionLabel.Caption := '';
      VersionLabel.Caption := '';
      TextLabel.Caption := '';
    end;
    FNode := Node;
  end;
end;

procedure TReportForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if Button = mbLeft then
  begin
    Node := nil;
    Node := TreeView.GetNodeAt(X, Y);
    if (ParamScrollList.Valid) and (Node <> nil) then
      TreeView.Selected := Node;
  end;
end;

procedure TReportForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  ParamScrollList.ValidateSelection;
  CanClose := True;
end;

procedure TReportForm.DoErrorInput(Sender: TObject);
var
  Event: TTVChangedEvent;
begin
  Event := TreeView.OnChange;
  TreeView.OnChange := nil;
  TreeView.Selected := FNode;
  TreeView.OnChange := Event;
end;

{ TReportDialog }

function TReportDialog.Execute: Boolean;
var
  Form: TReportForm;

  procedure FindGroup;
  var
    S: string;
    I, J: Integer;
  begin
    S := UpperCase(ReportGroup);
    for I := 0 to Length(Form.FReports) - 1 do
      if (Form.FReports[I] <> nil) and (UpperCase(Form.FReports[I].Group) = S) then
      begin
        for J := I + 1 to Length(Form.FReports) - 1 do
          if Form.FReports[J] = nil then
            Break
          else if UpperCase(Form.FReports[J].Name) = UpperCase(ReportName) then
          begin
            // Form.ListBox.ItemIndex := J;
            Break;
          end;
        Break;
      end;
  end;

  procedure FindName;
  var
    S: string;
    I: Integer;
  begin
    S := UpperCase(ReportName);
    for I := 0 to Length(Form.FReports) do
      if (Form.FReports[I] <> nil) and (UpperCase(Form.FReports[I].Name) = S) then
      begin
        // Form.ListBox.ItemIndex := I;
        Break;
      end;
  end;

begin
  FReport := nil;
  Form := TReportForm.Create(nil);
  try
    if ReportGroup <> '' then
      FindGroup
    else if ReportName <> '' then
      FindName;
    Result := Form.ShowModal = mrOK;
    FReport := Form.ActiveReport;
  finally
    Form.Free;
  end;
  if FAutoPrint and (FReport <> nil) then
    FReport.Print;
end;

end.
