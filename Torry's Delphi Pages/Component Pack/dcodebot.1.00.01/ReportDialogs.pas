
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ReportDialogs;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GraphTools, ImgList, ComCtrls, ReportTools, ScrollCtrls,
  Grip, InspectCtrls, InspectEditors, ComObj, ActiveX;

{ TReports }

type
  TReports = class(TObject)
  private
    FModules: TList;
    FUnknown: IUnknown;
    FReportList: TInterfaceList;
    function GetCount: Integer;
    function GetItem(Index: Integer): IReport;
  protected
    property ReportList: TInterfaceList read FReportList;
  public
    constructor Create(const Path: string);
    destructor Destroy; override;
    function FindReport(const Identity: TGUID): IReport;
    procedure UpdateParameter(const ParamName, ParamValue: string);
    property Count: Integer read GetCount;
    property Item[Index: Integer]: IReport read GetItem; default;
  end;

{ TReportForm form }

  TReportForm = class(TGripForm)
    GroupBox: TGroupBox;
    TitleLabel: TLabel;
    AuthorLabel: TLabel;
    VersionLabel: TLabel;
    DescriptionLabel: TLabel;
    TextLabel: TLabel;
    PrintButton: TButton;
    CloseButton: TButton;
    Image: TImage;
    ImageBevel: TBevel;
    ParamBox: TGroupBox;
    TreeView: TTreeView;
    Inspector: TInspector;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeViewCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure InspectorEditorValidated(Sender: TObject;
      Editor: TInspectorEditor);
    procedure TreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure PrintButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConnection: OleVariant;
    FReports: TReports;
    FReportPath: string;
    function GetActiveReport: IReport;
    procedure SetConnection(const Value: OleVariant);
    procedure SetReportPath(const Value: string);
  protected
    property ActiveReport: IReport read GetActiveReport;
  public
    property Connection: OleVariant read FConnection write SetConnection;
    property Reports: TReports read FReports;
    property ReportPath: string read FReportPath write SetReportPath;
  end;

{ TReportDialog component }

  TReportDialog = class(TComponent)
  private
    FReport: IReport;
    FAutoPrint: Boolean;
    FReportGroup: string;
    FReportName: string;
    FReportPath: string;
  public
    function Execute: Boolean;
    property Report: IReport read FReport;
  published
    property AutoPrint: Boolean read FAutoPrint write FAutoPrint;
    property ReportGroup: string read FReportGroup write FReportGroup;
    property ReportName: string read FReportName write FReportName;
    property ReportPath: string read FReportPath write FReportPath;
  end;

implementation

{$R *.DFM}

{ TReports }

procedure EnumReportsProc(Report: IUnknown; Data: Pointer); stdcall;
var
  Reports: TReports absolute Data;
begin
  Reports.FReportList.Add(Report);
end;

constructor TReports.Create(const Path: string);

  procedure AddModule(const FileName: string);
  var
    Module: HMODULE;
    EnumReports: TEnumReports;
  begin
    Module := LoadLibrary(PChar(FileName));
    if Module <> 0 then
    begin
      @EnumReports := GetProcAddress(Module, 'EnumReports');
      if @EnumReports <> nil then
      begin
        FModules.Add(Pointer(Module));
        EnumReports(EnumReportsProc, Self);
      end;
    end;
  end;

var
  SearchRec: TSearchRec;
  SearchResult: Integer;
  S: string;
begin
  FModules := TList.Create;
  FReportList := TInterfaceList.Create;
  FUnknown := FReportList as IUnknown;
  S := Trim(Path);
  if S <> '' then
  begin
    if S[Length(S)] <> '\' then
      S := S + '\';
    SearchResult := FindFirst(S + '*.rpl', faAnyFile, SearchRec);
    while SearchResult = 0 do
    begin
      AddModule(S + SearchRec.Name);
      SearchResult := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  end;
end;

destructor TReports.Destroy;
var
  I: Integer;
begin
  FReportList.Clear;
  FUnknown := nil;
  for I := 0 to FModules.Count - 1 do
    if not FreeLibrary(HMODULE(FModules[I])) then
      RaiseLastWin32Error;
  FModules.Free;
  inherited Destroy;
end;

function TReports.FindReport(const Identity: TGUID): IReport;
var
  Report: IReport;
  ReportIdentity: TGUID;
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Report := Item[I];
    Report.GetIdentity(ReportIdentity);
    if IsEqualGUID(Identity, ReportIdentity) then
    begin
      Result := Report;
      Break;
    end;
  end;
end;

procedure TReports.UpdateParameter(const ParamName, ParamValue: string);
var
  Report: IReport;
  Params: IReportParameters;
  S: array[0..1024] of Char;
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Report := Item[I];
    if Supports(FReportList[I], IReportParameters, Params) then
    begin
      Params.GetCount(J);
      while J > 0 do
      begin
        Dec(J);
        Params.GetName(J, S, SizeOf(S));
        if UpperCase(S) = UpperCase(ParamName) then
        begin
          Params.SetValue(J, PChar(ParamValue));
          Break;
        end;
      end;
    end;
  end;
end;

function TReports.GetCount: Integer;
begin
  Result := FReportList.Count;
end;

function TReports.GetItem(Index: Integer): IReport;
begin
  Result := FReportList[Index] as IReport;
end;

function GetTitle(Unknown: IUnknown): string;
var
  S: array[0..1024] of Char;
begin
  Result := '';
  if Unknown <> nil then
  begin
    OleCheck((Unknown as IReport).GetTitle(S, SizeOf(S)));
    Result := S;
  end;
end;

function GetGroup(Unknown: IUnknown): string;
var
  S: array[0..1024] of Char;
begin
  Result := '';
  if Unknown <> nil then
  begin
    OleCheck((Unknown as IReport).GetGroup(S, SizeOf(S)));
    Result := S;
  end;
end;

{ TReportForm }

function TReportForm.GetActiveReport: IReport;
var
  Node: TTreeNode;
begin
  Result := nil;
  Node := TreeView.Selected;
  if (Node <> nil) and (Node.Data <> nil) then
    Result := IReport(Node.Data);
end;

procedure TReportForm.SetConnection(const Value: OleVariant);
var
  Params: IReportParameters;
  I: Integer;
begin
  FConnection := Value;
  if FReports <> nil then
  for I := 0 to FReports.Count - 1 do
    if Supports(FReports[I], IReportParameters, Params) then
      OleCheck(Params.SetConnectionInfo(FConnection));
end;

procedure TReportForm.SetReportPath(const Value: string);

  function ItemSignature(Index: Integer): string;
  var
    Item: IUnknown;
  begin
    Item := FReports[Index];
    Result := UpperCase(GetGroup(Item) + GetTitle(Item));
  end;

var
  L, R: Integer;
  Report: IReport;
  Params: IReportParameters;
  Group: string;
  Node: TTreeNode;
  S: string;
begin
  FReportPath := Value;
  Inspector.Editors.Clear;
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    FReports.Free;
    FReports := nil;
    FReports := TReports.Create(Value);
    L := 0;
    R := FReports.Count - 1;
    while L < R do
      if ItemSignature(L) > ItemSignature(L + 1) then
      begin
        FReports.ReportList.Exchange(L, L + 1);
        L := 0;
      end
      else
        Inc(L);
    L := 0;
    R := FReports.Count;
    Group := '';
    Node := nil;
    while L < R do
    begin
      Report := FReports[L];
      S := GetGroup(Report);
      if UpperCase(S) <> Group then
      begin
        Group := UpperCase(S);
        Node := TreeView.Items.AddChild(nil, S);
        Node.ImageIndex := 0;
      end;
      S := GetTitle(Report);
      with TreeView.Items.AddChild(Node, S) do
      begin
        ImageIndex := -1;
        SelectedIndex := -1;
        Data := Pointer(Report);
      end;
      if Supports(Report, IReportParameters, Params) then
        OleCheck(Params.SetConnectionInfo(FConnection));
      Inc(L);
      Node.Expand(True);
    end;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TReportForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TReportForm.InspectorEditorValidated(Sender: TObject;
  Editor: TInspectorEditor);
var
  Node: TTreeNode;
  Params: IReportParameters;
begin
  Node := TreeView.Selected;
  if (Node <> nil) and (Node.Data <> nil) and Supports(IReport(Node.Data),
    IReportParameters, Params) then
    with Editor do
      Params.SetValue(Index, PChar(Text));
end;

var
  CheckeredBitmap: TBitmap;
  CheckeredBrush: HBRUSH;

procedure TReportForm.TreeViewCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Rect: TRect;
  PriorColor: COLORREF;
  DC: HDC;
begin
  DefaultDraw := False;
  Rect := Node.DisplayRect(False);
  with Sender as TTreeView do
  begin
    Canvas.Font := Self.Font;
    DC := Canvas.Handle;
    if Node.Level = 0 then
    begin
      DrawFrame(DC, Rect, dfRaised);
      InflateRect(Rect, -1, -1);
      if cdsSelected in State then
        FillRect(DC, Rect, CheckeredBrush)
      else
        FillRect(DC, Rect, COLOR_BTNFACE + 1);
      Rect.Left := 25;
      PriorColor := SetTextColor(DC, ColorToRGB(clWindowText));
      DrawCaption(DC, Node.Text, Rect, drLeft);
      SetTextColor(DC, PriorColor);
      Rect.Left := 0;
      Rect.Right := HeightOf(Rect);
      DrawNode(DC, Rect, Node.Expanded);
    end
    else
    begin
      if cdsSelected in State then
        FillRect(DC, Rect, COLOR_HIGHLIGHT + 1)
      else
        FillRect(DC, Rect, COLOR_WINDOW + 1);
      Rect.Left := 25;
      if cdsSelected in State then
        PriorColor := SetTextColor(DC, GetSysColor(COLOR_HIGHLIGHTTEXT))
      else
        PriorColor := SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT));
      DrawCaption(DC, Node.Text, Rect, drLeft);
      SetTextColor(DC, PriorColor);
    end;
  end;
end;

procedure TReportForm.TreeViewCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  with Sender as TTreeView do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(ARect);
    Canvas.Brush.Color := clWindow;
  end;
end;

procedure TReportForm.TreeViewChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  try
    Inspector.Submit(Inspector.Selected, Inspector.Text);
  except
    on E: Exception do
    begin
      AllowChange := False;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TReportForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  Report: IReport;
  Params: IReportParameters;
  Version: Double;
  ParamCount: integer;
  Kind: Integer;
  Editor: TInspectorEditor;
  I: Integer;
  S: array[0..1024] of Char;
begin
  if Node <> nil then
  begin
    Inspector.Editors.Clear;
    PrintButton.Enabled := Node.Data <> nil;
    if PrintButton.Enabled then
    begin
      Report := IReport(Node.Data);
      OleCheck(Report.GetTitle(S, SizeOf(S)));
      TitleLabel.Caption := 'Title: ' + S;
      OleCheck(Report.GetAuthor(S, SizeOf(S)));
      AuthorLabel.Caption := 'Author: ' + S;
      DescriptionLabel.Caption := 'Description:';
      OleCheck(Report.GetVersion(Version));
      VersionLabel.Caption := 'Version: ' + Format('%.2f',
        [Version]);
      OleCheck(Report.GetDescription(S, SizeOf(S)));
      TextLabel.Caption := S;
      if Supports(Report, IReportParameters, Params) then
      begin
        OleCheck(Params.GetCount(ParamCount));
        for I := 0 to ParamCount - 1 do
        begin
          OleCheck(Params.GetKind(I, Kind));
          Editor := nil;
          case Kind of
            pkText: Editor := Inspector.Editors.Add(ekString);
            pkInteger: Editor := Inspector.Editors.Add(ekInteger);
            pkFloat: Editor := Inspector.Editors.Add(ekFloat);
            pkDate: Editor := Inspector.Editors.Add(ekDate);
            pkChoice:
              begin
                Editor := Inspector.Editors.Add(ekPicklist);
                OleCheck(Params.GetChoices(I, S, SizeOf(S)));
                TPickInspectorEditor(Editor).Nullable := True;
                TPickInspectorEditor(Editor).Items.CommaText := S;
              end;
          end;
          if Editor = nil then
            Continue;
          OleCheck(Params.GetName(I, S, SizeOf(S)));
          Editor.Name := S;
          OleCheck(Params.GetValue(I, S, SizeOf(S)));
          Editor.Text := S;
        end;
      end;
    end
    else
    begin
      TitleLabel.Caption := 'Group: ' + Node.Text;
      AuthorLabel.Caption := '';
      DescriptionLabel.Caption := '';
      VersionLabel.Caption := '';
      TextLabel.Caption := '';
    end;
  end;
end;

procedure TReportForm.PrintButtonClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView.Selected;
  if (Node <> nil) and (Node.Data <> nil) then
    IReport(Node.Data).Print;
end;

procedure TReportForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

{ TReportDialog }

function TReportDialog.Execute: Boolean;
var
  Form: TReportForm;
begin
  FReport := nil;
  Form := TReportForm.Create(nil);
  try
    Form.ReportPath := ReportPath;
    Result := Form.ShowModal = mrOK;
    FReport := Form.ActiveReport;
  finally
    Form.Free;
  end;
  if (FReport <> nil) then
    FReport.Print;
end;

procedure TReportForm.FormDblClick(Sender: TObject);
begin
  TreeView.Items.Clear;
  SetReportPath(ReportPath);
end;

procedure TReportForm.FormDestroy(Sender: TObject);
begin
  FReports.Free;
end;

initialization
  CheckeredBitmap := GetBitmap(clBtnFace, clBtnHighlight);
  CheckeredBrush := GetBrush(CheckeredBitmap);
finalization
  DeleteObject(CheckeredBrush);
  CheckeredBitmap.Free;
end.
