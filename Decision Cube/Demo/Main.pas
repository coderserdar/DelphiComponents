unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, FxPivSrc, ComCtrls, ToolWin,
  FxCommon, FxDb, FxExpr, FxStore, FxCache, FxBin, FxGrid, FxGraph, FxMap,
  DB, IBCustomDataSet, IBQuery, IBDatabase, Provider, ActnList,
  DBClient, DBLocal, DBLocalI, ImgList, FxDimPager, StdCtrls, CheckLst,
  ValEdit, TeeProcs, TeEngine, Chart, Series, Buttons, HTTPApp,
  ExtActns, ShellAPI, FxHtml;

type
  TMainForm = class(TForm,IFxProgress)
    ToolBar1: TToolBar;
    StatusBar: TStatusBar;
    Pivot: TFxPivot;
    DecisionGrid: TFxGrid;
    DecisionSource: TFxSource;
    btnOpen: TToolButton;
    btnDiv1: TToolButton;
    ImageList: TImageList;
    cbBin: TComboBox;
    panBar: TPanel;
    btnDiv2: TToolButton;
    btnBar: TToolButton;
    lbDims: TCheckListBox;
    vlProps: TValueListEditor;
    Splitter1: TSplitter;
    cbSummary: TSumCombo;
    PageControl: TPageControl;
    tabGrid: TTabSheet;
    tabChart: TTabSheet;
    btnChart: TToolButton;
    btnGrid: TToolButton;
    ActionList: TActionList;
    actActive: TAction;
    actChartDlg: TAction;
    ToolButton1: TToolButton;
    actSparceCols: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    actSparseRows: TAction;
    ToolButton4: TToolButton;
    actExportHtml: TAction;
    btnHtml: TToolButton;
    FxHtml: TFxHtml;
    ToolButton5: TToolButton;
    actMemoryUsage: TAction;
    tbDims: TToolBar;
    btnUpdateMap: TToolButton;
    actUpdateMap: TAction;
    procedure cbBinSelect(Sender: TObject);
    procedure btnBarClick(Sender: TObject);
    procedure vlPropsStringsChange(Sender: TObject);
    procedure btnChartClick(Sender: TObject);
    procedure btnGridClick(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure actActiveExecute(Sender: TObject);
    procedure actChartDlgExecute(Sender: TObject);
    procedure actChartDlgUpdate(Sender: TObject);
    procedure actSparceColsExecute(Sender: TObject);
    procedure actSparseRowsExecute(Sender: TObject);
    procedure actExportHtmlExecute(Sender: TObject);
    procedure actExportHtmlUpdate(Sender: TObject);
    procedure actMemoryUsageExecute(Sender: TObject);
    procedure actUpdateMapExecute(Sender: TObject);
    procedure lbDimsClickCheck(Sender: TObject);
  private
    { Private declarations }
    FActiveDim:TFxMapItem;
    FCancel:Boolean;
    FCancelBtn:TSpeedButton;
    FProgressBar:TProgressBar;
    FBlock:Boolean;
    FFixedRows:Integer;
    FChart:TFxChart;
    function GetCancel:Boolean;
    procedure CancelClick(Sender:TObject);
    procedure FinishProgress;
    procedure StartProgress(Max:Integer);
    procedure SetActiveDim(const Value: TFxMapItem);
    procedure SetText(const Value:WideString);
    procedure UpdateProgress;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    property ActiveDim:TFxMapItem read FActiveDim write SetActiveDim;
  end;

var
  MainForm: TMainForm;

implementation

uses StrUtils, Data, ChartDlg, MSData;

const
  SAlignment:array [0..2] of string=('Left','Right','Center');

{$R *.dfm}

function SafeInt(const Str:string; Default:Integer=10):Integer;
begin
  if Str='' then
    Result:=Default
  else
    try
      Result:=StrToInt(Str);
    except
      Result:=Default;
    end;
end;

procedure AddCombo(Prop:TItemProp; const S:array of string);
var
  I:Integer;
begin
  Prop.EditStyle:=esPickList;
  for I:=0 to High(S)do
    Prop.PickList.Add(S[I]);
end;

{ TMainForm }

procedure TMainForm.cbBinSelect(Sender: TObject);
begin
  DM.DecisionCube.DimensionMap[0].BinType:=TBinType(cbBin.ItemIndex);
end;

procedure TMainForm.btnBarClick(Sender: TObject);
begin
  with Sender as TToolButton do
    panBar.Visible:=Down;
end;

procedure TMainForm.vlPropsStringsChange(Sender: TObject);
var
  I:Integer;
begin
  if FBlock then Exit;
  with vlProps do begin
    case Row-FFixedRows of
    0:
      begin
        I:=ItemProps[0].PickList.IndexOf(Values['Dimension']);
        if I>=0 then
          ActiveDim:=ItemProps[0].PickList.Objects[I] as TFxMapItem;
      end;
    1:
      begin
        I:=AnsiIndexStr(Values['Alignment'],SAlignment);
        if FActiveDim<>nil then FActiveDim.Alignment:=TAlignment(I);
      end;
    2: if FActiveDim<>nil then FActiveDim.Format:=Values['Format'];
    3: if FActiveDim<>nil then FActiveDim.Caption:=Values['Caption'];
    4: if FActiveDim<>nil then FActiveDim.Width :=SafeInt(Values['Width'],FActiveDim.Width);
    end;
  end;
end;

procedure TMainForm.SetActiveDim(const Value: TFxMapItem);
begin
  if Value<>FActiveDim then begin
    FActiveDim := Value;
    with vlProps do begin
      FBlock:=True;
      try
        if ActiveDim<>nil then begin
          Values['Alignment']:=SAlignment[Ord(FActiveDim.Alignment)];
          Values['Format'   ]:=FActiveDim.Format;
          Values['Width'    ]:=IntToStr(FActiveDim.Width);
          Values['Caption'  ]:=FActiveDim.Caption;
        end;
      finally
        FBlock:=False;
      end;
    end;
  end;
end;

procedure TMainForm.btnChartClick(Sender: TObject);
begin
  PageControl.ActivePage:=tabChart;
  FChart.DecisionSource:=DecisionSource;
end;

procedure TMainForm.btnGridClick(Sender: TObject);
begin
  PageControl.ActivePage:=tabGrid;
  // chart-off (save chart building time)
  FChart.DecisionSource:=nil;
end;

// Create TSpeedButton and TProgressBar on StatusBar
// Create TDecisionChart in runtime (design time creation have
// some problem with series. I don't have code for TChart and
// can't fix it. In my applications I create DecisonChart from
// "chart.dat" (see destructor TDecisionChart.Destroy).

constructor TMainForm.Create(AOwner: TComponent);
var
  I:Integer;
begin
  inherited Create(AOwner);
  // you can uncomment this and line in destructor TDecisionChart.Destroy
  // then you can save runtime settings for TDecisionChart; 
  {
  if FileExists('chart.dat')then
    try
      FChart:=ReadComponentResFile('chart.dat',nil) as TDecisionChart;
      //Memo.Lines.Text :=ComponentToString(Chart);
    except
      FChart:=nil;
      DeleteFile('chart.dat');
    end;
  }
  if FChart=nil then
    FChart:=TFxChart.Create(tabChart);
  with FChart do begin
    Align:=alClient;
    Parent:=tabChart;
    FxProgress:=Self;
  end;
  FProgressBar:=TProgressBar.Create(StatusBar);
  with FProgressBar do begin
    Parent:=StatusBar;
    Min:=0;
    Step:=1;
    Smooth:=True;
  end;
  FCancelBtn:=TSpeedButton.Create(StatusBar);
  with FCancelBtn do begin
    Parent:=StatusBar;
    Caption:='Cancel';
    Flat:=True;
    Visible:=False;
    OnClick:=CancelClick;
  end;
  // Init controls
  cbBin.ItemIndex:=Ord(DM.DecisionCube.DimensionMap[0].BinType);
  with DM.DecisionCube do begin
    DimensionMap.GetDimList(lbDims.Items);
    for I:=0 to lbDims.Count-1 do
      lbDims.Checked[I]:=DimensionMap[I].ActiveFlag<>diInactive;
  end;
  for I:=0 to lbDims.Count-1 do
    lbDims.Checked[I]:=(lbDims.Items.Objects[I]as TFxMapItem).ActiveFlag<>diInactive;
  FFixedRows:=0;
  if doColumnTitles in vlPRops.DisplayOptions then
    Inc(FFixedRows);
  vlProps.ItemProps[0].EditStyle:=esPickList;
  DM.DecisionCube.DimensionMap.GetDimList(vlProps.ItemProps[0].PickList);
  AddCombo(vlProps.ItemProps[1],SAlignment);
end;

procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if Panel.Index=1 then
    FProgressBar.BoundsRect:=Rect
  else if Panel.Index=2 then
    FCancelBtn.BoundsRect:=Rect;
end;

//************************************************//
//             IFxProgress Implementation         //
//************************************************//

function TMainForm.GetCancel: Boolean;
begin
  Result:=FCancel;
end;

procedure TMainForm.SetText(const Value: WideString);
begin
  StatusBar.Panels[0].Text:=Value;
  Application.ProcessMessages;
end;

procedure TMainForm.StartProgress(Max: Integer);
begin
  btnGrid.Enabled:=False;
  FProgressBar.Max:=Max;
  FProgressBar.Position:=0;
  FCancel:=False;
  FCancelBtn.Visible:=True;
end;

procedure TMainForm.FinishProgress;
begin
  FProgressBar.Position:=0;
  FCancelBtn.Visible:=False;
  SetText('');
  btnGrid.Enabled:=True;
end;

procedure TMainForm.UpdateProgress;
begin
  FProgressBar.StepIt;
  Application.ProcessMessages;
end;

procedure TMainForm.CancelClick(Sender: TObject);
begin
  FCancel:=True;
end;

//************************************************//
//             Actions Implementation             //
//************************************************//

procedure TMainForm.actActiveExecute(Sender: TObject);
begin
  with Sender as TAction,dm.DecisionCube do begin
    Active:=not Checked;
    Checked:=Active;
    ImageIndex:=Ord(Checked);
  end;
end;

procedure TMainForm.actChartDlgExecute(Sender: TObject);
begin
  dlgChartOpt.Execute(FChart);
end;

procedure TMainForm.actChartDlgUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled:= PageControl.ActivePage=tabChart;
end;

procedure TMainForm.actSparceColsExecute(Sender: TObject);
begin
  with Sender as TAction do
    DecisionSource.SparseCols:=Checked;
end;

procedure TMainForm.actSparseRowsExecute(Sender: TObject);
begin
  with Sender as TAction do
    DecisionSource.SparseRows:=Checked;
end;

procedure TMainForm.actExportHtmlExecute(Sender: TObject);
var
  S:TStringStream;
  F:TFileStream;
begin
  S:=TStringStream.Create(FxHtml.PageContent);
  try
    F:=TFileStream.Create('demo.html',fmCreate);
    try
      F.CopyFrom(S,0);
    finally
      F.Free;
    end;
  finally
    S.Free;
  end;
  ShellExecute(0, nil, PChar('demo.html'), '', '', SW_SHOWNORMAL);
end;

procedure TMainForm.actExportHtmlUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled:= Assigned(DecisionSource) and DecisionSource.Ready;
end;

procedure TMainForm.actMemoryUsageExecute(Sender: TObject);
var
  I:Integer;
begin
  I:=dm.DecisionCube.DataCache.GetMemoryUsage;
  MessageDlg(IntToStr(I div 1024)+'kb',mtInformation,[mbOk],0);
end;

procedure TMainForm.actUpdateMapExecute(Sender: TObject);
begin
  case (Sender as TAction).Checked of
  False: DM.DecisionCube.DimensionMap.EndUpdate;
  True : DM.DecisionCube.DimensionMap.BeginUpdate;
  end;
end;

procedure TMainForm.lbDimsClickCheck(Sender: TObject);
var
  MapItem:TDimensionItem;
begin
  with DM.DecisionCube,Sender as TCheckListBox do begin
    MapItem:=DimensionMap.Find(Items[ItemIndex]);
    if MapItem=nil then Exit;
    if Checked[ItemIndex]then
      MapItem.ActiveFlag:=diAsNeeded
    else
      MapItem.ActiveFlag:=diInactive;
  end;
end;

end.
