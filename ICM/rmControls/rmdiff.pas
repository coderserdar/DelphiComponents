{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmDiff
Purpose  : This is used to do a textfile difference.
Date     : 12-29-2000
Author   : Ryan J. Mills
Version  : 1.90
Notes    : The original engine code came from the diff.c file found on the
           ftp.cdrom.com site.  The original source can be found here:
           ftp://ftp.cdrom.com/pub/algorithims/c/diff.c
           The DiffMap code originally came from Bernie Caudry and Guy LeMar.
           I've only modified the original DiffMap source to work with my rmDiff
           components.  I would like to go back and rewrite the drawing and
           mapping algorithms because I think they are unnecessarily complex.
           It also doesn't use any resources where it should.
================================================================================}

unit rmDiff;

interface

{$I CompilerDefines.INC}

uses windows, Messages, controls, classes, stdctrls, Grids, extctrls, sysutils, Graphics,
  rmListControl;

type
  EDiffException = Exception;

  TrmDiffBlock = record
    startLine: integer;
    EndLine: integer;
  end;

  TrmDiffObject = class(TObject)
  private
    fSource1, fSource2: TrmDiffBlock;
  public
    property Source1: TrmDiffBlock read fSource1 write fSource1;
    property Source2: TrmDiffBlock read fSource2 write fSource2;
  end;

  TrmDiffOption = (fdoCaseSensitive, fdoIgnoreCharacters, fdoIgnoreTrailingWhiteSpace, fdoIgnoreLeadingWhiteSpace);
  TrmDiffOptions = set of TrmDiffOption;

  TrmDiffProgressEvent = procedure(PercentComplete: integer) of object;
  TrmDiffMapClickEvent = procedure(Sender: TObject; IndicatorPos: integer) of object;

  TrmCustomDiffViewer = class;
  TrmDiffMap = class;

  TrmCharacterSet = set of char;

  TrmCustomDiffEngine = class(TComponent)
  private
  { Private }
    fSource1, fSource2: TStringList;
    fDiffSource1, fDiffSource2 : TStringList;
    fAttachedViewers : TList;
    fSource1Count, fSource2Count: integer;
    fBlankLines1, fBlankLines2: integer;
    fOptions: TrmDiffOptions;
    fOnDiffCompleted: TNotifyEvent;
    fOnProgress: TrmDiffProgressEvent;
    fMatchDepth: integer;
    fIgnoreChars: TrmCharacterSet;
    fMultiLineCommentOpen, fStringOpen: boolean;
    function AtEOF: boolean;
    procedure MoveDown(Amount1, Amount2: integer);
    procedure CompareData;
    procedure StartCompare;
    procedure SetMatchDepth(const Value: integer);
    procedure ClearData;
    function RemoveCharacters(st:string):string;
  protected
  { protected }
    function MatchLines(level1, level2, MatchDepth: integer): boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DiffStarted; virtual;
    procedure DiffFound(Source1, Source2: TrmDiffBlock); virtual;
    procedure DiffCompleted; virtual;

    procedure AttachViewer(viewer:TrmCustomDiffViewer);
    procedure RemoveViewer(viewer:TrmCustomDiffViewer);

    property MatchDepth: integer read fMatchDepth write SetMatchDepth default 3;
    property DiffOptions: TrmDiffOptions read fOptions write fOptions;
    property OnDiffCompleted: TNotifyEvent read fOnDiffCompleted write fOnDiffCompleted;
    property OnDiffProgress: TrmDiffProgressEvent read fOnProgress write fOnProgress;
    property DiffSource1 : TStringList read fDiffSource1;
    property DiffSource2 : TStringList read fDiffSource2;
    property IgnoreCharacters : TrmCharacterSet read fIgnoreChars write fIgnoreChars;
  public
  { Public }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CompareFiles(file1, file2: string);
    procedure CompareStreams(Strm1, Strm2: TStream);
  end;

  TrmDiffEngine = class(TrmCustomDiffEngine)
  public
    property DiffSource1;
    property DiffSource2;
    property IgnoreCharacters;
  published
  { Published }
    property MatchDepth;
    property DiffOptions;
    property OnDiffCompleted;
    property OnDiffProgress;
  end;

  TrmCustomDiffViewer = class(TCustomControl)
  private
  { Private }
    fDiff: TrmCustomDiffEngine;
    fDiffMap: TrmDiffMap;
    fEBGColor: TColor;
    fDBGColor: TColor;
    fSimpleViewer: boolean;
    fITColor: TColor;
    fDColor: TColor;
    fCTColor: TColor;

    procedure SetDBGColor(const Value: TColor);
    procedure SetEBGColor(const Value: TColor);
    procedure SetSimpleViewer(const Value: boolean);
    procedure SetCTColor(const Value: TColor);
    procedure SetDTColor(const Value: TColor);
    procedure SetITColor(const Value: TColor);
    procedure wmEraseBKGrnd(var msg: tmessage); message wm_erasebkgnd;
  protected
  { protected }
    function GetSrc1Label: string; virtual;
    function GetSrc2Label: string; virtual;
    procedure SetSrc1Label(const Value: string); virtual;
    procedure SetSrc2Label(const Value: string); virtual;
    procedure SetDiff(const Value: TrmCustomDiffEngine); virtual;
    procedure SetDiffMap(const Value: TrmDiffMap); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DiffEngine: TrmCustomDiffEngine read fDiff write SetDiff;
    function GetMapData: string;
    procedure MapClick(Sender: TObject; IndicatorPos: integer); virtual; Abstract;
    property EmptyBGColor: TColor read fEBGColor write SetEBGColor default clBtnFace;
    property DiffBGColor: TColor read fDBGColor write SetDBGColor default clYellow;
    property ChangedTextColor: TColor read fCTColor write SetCTColor default clWindowText;
    property DeletedTextColor: TColor read fDColor write SetDTColor default clWindowText;
    property InsertedTextColor: TColor read fITColor write SetITColor default clWindowText;
    property SimpleDiffViewer: boolean read fSimpleViewer write SetSimpleViewer default true;
    property DiffMap: TrmDiffMap read fDiffMap write SetDiffMap;
    property Src1Label : string read GetSrc1Label write SetSrc1Label;
    property Src2Label : string read GetSrc2Label write SetSrc2Label;
  public
  { Public }
    constructor Create(AOwner: TComponent); override;
    procedure DiffferenceCompleted; virtual;
  published
  { Published }
    property Align;
    property Font;
  end;

  TrmDiffViewer = class(TrmCustomDiffViewer)
  private
  { Private }
    fDrawing: boolean;
    fsource1: TrmListControl;
    fsource2: TrmListControl;
    fBevel: TBevel;
    fPanel: TPanel;
    fLabel1, fLabel2: TLabel;
    fScrollInProgress: boolean;
    fIHC: boolean;
    fLockSelIndex: boolean;
    procedure UpdateVScrollBar;
    procedure UpdateHScrollBar;
    procedure scrollChanged(Sender: TObject; ScrollBar: integer);
    procedure Drawing(Sender: TObject; Canvas: TCanvas; Selected: boolean; var str: string);
    procedure cmFontChanged(var Msg: TMessage); message cm_fontchanged;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure SetIHC(const Value: boolean);
    procedure SetLockSelIndex(const Value: boolean);
  protected
  { protected }
    function GetSrc1Label: string; override;
    function GetSrc2Label: string; override;
    procedure SetSrc1Label(const Value: string); override;
    procedure SetSrc2Label(const Value: string); override;
    procedure SetDiffMap(const Value: TrmDiffMap); override;
    procedure Resize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ResetIndex(Index, Pos: integer);
    procedure MapClick(Sender: TObject; IndicatorPos: integer); override;
  public
  { Public }
    procedure DiffferenceCompleted; override;

    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
  published
  { Published }
    property TabStop;
    property DiffEngine;
    property EmptyBGColor;
    property DiffBGColor;
    property ChangedTextColor;
    property DeletedTextColor;
    property InsertedTextColor;
    property DiffMap;
    property SimpleDiffViewer;
    property IndependantHorzControl: boolean read fIHC write SetIHC default False;
    property LockSelectedIndex:boolean read fLockSelIndex write SetLockSelIndex default true;
    property Src1Label;
    property Src2Label;
  end;

  TrmDiffMergeViewer = class(TrmCustomDiffViewer)
  private
  { Private }
    fDrawing: boolean;
    fsource1: TrmListControl;
    fsource2: TrmListControl;
    fMergeSource: TrmListControl;
    fBevel, fBevel2: TBevel;
    fPanel: TPanel;
    fLabel1, fLabel2, fLabel3: TLabel;
    fScrollInProgress: boolean;
    fD1BGColor: TColor;
    fD2BGColor: TColor;
    fIHC: boolean;
    fSourceSide : smallint;
    procedure UpdateVScrollBar;
    procedure UpdateHScrollBar;
    procedure scrollChanged(Sender: TObject; ScrollBar: integer);
    procedure Drawing(Sender: TObject; Canvas: TCanvas; Selected: boolean; var str: string);
    procedure cmFontChanged(var Msg: TMessage); message cm_fontchanged;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure SetD1BGColor(const Value: TColor);
    procedure SetD2BGColor(const Value: TColor);
    procedure SetIHC(const Value: boolean);
    procedure SetSourceSide(const Value: smallint);
  protected
  { protected }
    function GetSrc1Label: string; override;
    function GetSrc2Label: string; override;
    procedure SetSrc1Label(const Value: string); override;
    procedure SetSrc2Label(const Value: string); override;
    function GetMergeLable: string; virtual;
    procedure SetMergeLabel(const Value: string); virtual;
    procedure Resize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ResetIndex(Index, Pos: integer);
    procedure MapClick(Sender: TObject; IndicatorPos: integer); override;
  public
  { Public }
    procedure DiffferenceCompleted; override;

    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    function CanChangeSource: boolean;
    procedure CopySource(Source1: boolean);
    procedure ClearSource;
    procedure SaveMergeToFile(FileName: string);
    procedure SaveMergeToStream(Strm: TStream);
    property SelectedSource : smallint read FSourceSide write SetSourceSide default 0;
  published
  { Published }
    property DiffEngine;
    property TabStop;
    property IndependantHorzControl: boolean read fIHC write SetIHC default False;
    property EmptyBGColor;
    property ChangedTextColor;
    property DeletedTextColor;
    property InsertedTextColor;
    property DiffMap;
    property SimpleDiffViewer;
    property Source1DiffBGColor: TColor read fD1BGColor write SetD1BGColor default clAqua;
    property Source2DiffBGColor: TColor read fD2BGColor write SetD2BGColor default clYellow;
    property MergeSrcLabel:string read GetMergeLable write SetMergeLabel;
    property Src1Label;
    property Src2Label;
  end;

  TrmDiffMap = class(TCustomControl)
  private
    { Private declarations }
    FColorDeleted: TColor;
    FColorInserted: TColor;
    FColorModified: TColor;
    FShowIndicator: Boolean;
    FIndicatorPos: integer;
    FIndicator: TBitmap;
    FData: string;
    FOnMapClick: TrmDiffMapClickEvent;
    procedure DrawIndicator;
    procedure SetIndicatorPos(Value: integer);
    function MapHeight: integer;
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetData(Value: string);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property IndicatorPos: integer read FIndicatorPos write SetIndicatorPos;
  published
    { Published declarations }
    property Color;
    property Caption;
    property Align;
    property ColorDeleted: TColor read FColorDeleted write FColorDeleted;
    property ColorInserted: TColor read FColorInserted write FColorInserted;
    property ColorModified: TColor read FColorModified write FColorModified;
    property ShowIndicator: Boolean read FShowIndicator write FShowIndicator;
    property OnMapClick: TrmDiffMapClickEvent read FOnMapClick write FOnMapClick;
  end;

implementation

uses rmLibrary, math, Forms;

{$R *.RES}

const
  NormalLine = #1;
  DeletedLine = #2;
  InsertedLine = #3;
  ChangedLine = #4;
  EmptyLine = #5;
  MergeLine1 = #6;
  MergeLine2 = #7;

  TOP_MARGIN = 5;
  BOTTOM_MARGIN = 5;

{ TrmDiff }

constructor TrmCustomDiffEngine.Create;
begin
  inherited;
  fSource1 := TStringList.Create;
  fSource2 := TStringList.Create;
  fDiffSource1 := TStringList.create;
  fDiffSource2 := TStringList.create;
  fAttachedViewers := TList.create;
  fSource1Count := -1;
  fSource2Count := -1;
  fMatchDepth := 3;
  fIgnoreChars := [#0..#32];
  fMultiLineCommentOpen:=false;
  fStringOpen:=false;
end;

destructor TrmCustomDiffEngine.Destroy;
begin
  fSource1.Free;
  fSource2.Free;
  fDiffSource1.free;
  fDiffSource2.free;
  fAttachedViewers.Free;
  inherited;
end;

procedure TrmCustomDiffEngine.CompareFiles(file1, file2: string);
begin
  ClearData;   
  fSource1.LoadFromFile(File1);
  fSource2.LoadFromFile(File2);
  StartCompare;
end;

procedure TrmCustomDiffEngine.CompareStreams(Strm1, Strm2: TStream);
begin
  ClearData;   
  fSource1.LoadFromStream(Strm1);
  fSource2.LoadFromStream(Strm2);
  StartCompare;
end;

procedure TrmCustomDiffEngine.StartCompare;
begin
  fSource1Count := fSource1.Count;
  fSource2Count := fSource2.count;

  DiffStarted;

  while not AtEOF do
    CompareData;

  DiffCompleted;
end;

procedure TrmCustomDiffEngine.CompareData;
var
  depth, level, tmp: integer;
  db1, db2: TrmDiffBlock;
  wEOF: boolean;
  CheckDepth: integer;
begin
  while (not atEOF) and matchLines(0, 0, 0) do
    MoveDown(1, 1);

  if AtEOF then
    exit;

  wEOF := false;
  depth := 0;
  level := 0;

  if (fSource1Count = fSource1.count) and (fSource2Count = fSource2.count) then
    checkdepth := 1
  else
    CheckDepth := fMatchDepth;
  while true do
  begin
    try
      if MatchLines(level, depth, CheckDepth) then
        break;

      if (level <> depth) and (MatchLines(depth, level, CheckDepth)) then
        break;

      if (level < depth) then
        inc(level)
      else
      begin
        inc(depth);
        level := 0;
      end;
    except
      wEOF := true;
      level := fSource2.count - 1;
      depth := fSource1.count - 1;
    end;
  end;

  if not wEOF then
  begin
    if MatchLines(level, depth, CheckDepth) then
    begin
      tmp := level;
      level := depth;
      depth := tmp;
    end;
  end;

  db1.startLine := fSource1Count - fSource1.count;
  db1.endline := db1.startline + depth;

  db2.startLine := fSource2Count - fSource2.count;
  db2.endline := db2.startline + level;

  try
     DiffFound(db1, db2);
  except
      //Do Nothing
  end;
end;

function TrmCustomDiffEngine.AtEOF: boolean;
begin
  result := (fSource1.count = 0) or (fSource2.count = 0);
end;

function TrmCustomDiffEngine.MatchLines(level1, level2, MatchDepth: integer): boolean;
var
  s1, s2: string;
  loop: integer;
begin
  result := true;

  for loop := 0 to MatchDepth do
  begin
    if level1 + loop >= fSource1.count then
      s1 := ''
    else
      s1 := fSource1[level1 + loop];

    if level2 + loop >= fSource2.count then
      s2 := ''
    else
      s2 := fSource2[level2 + loop];

    if (fdoIgnoreCharacters in fOptions) and (fIgnoreChars <> []) then
    begin
      s1 := RemoveCharacters(s1);
      s2 := RemoveCharacters(s2);
    end;

    if ((fdoIgnoreTrailingWhiteSpace in fOptions) and
       (fdoIgnoreLeadingWhiteSpace in fOptions)) then
    begin
      s1 := Trim(s1);
      s2 := Trim(s2);
    end
    else
    if fdoIgnoreTrailingWhiteSpace in fOptions then
    begin
      s1 := TrimRight(s1);
      s2 := TrimRight(s2);
    end
    else if fdoIgnoreLeadingWhiteSpace in fOptions then
    begin
      s1 := TrimLeft(s1);
      s2 := TrimLeft(s2);
    end;

    if fdoCaseSensitive in fOptions then
      result := result and (compareStr(s1, s2) = 0)
    else
      result := result and (compareStr(lowercase(s1), lowercase(s2)) = 0);

    if not result then
      break;
  end;
end;

procedure TrmCustomDiffEngine.MoveDown(Amount1, Amount2: integer);
begin
  while Amount1 > 0 do
  begin
    dec(Amount1);
    try
       fDiffSource1.add(NormalLine + fSource1[0]);
       fSource1.delete(0);
    except
      amount1 := 0;
    end;
  end;

  while Amount2 > 0 do
  begin
    dec(Amount2);
    try
       fDiffSource2.add(NormalLine + fSource2[0]);
       fSource2.delete(0);
    except
      amount2 := 0;
    end;
  end;

  if assigned(fOnProgress) then
  begin
    try
      fOnProgress(round((((fSource1Count + fSource2Count) - (fSource1.Count + fSource2.count)) / (fSource1Count + fSource2Count)) * 100));
    except
         //Do Nothing...
    end;
  end;
end;

procedure TrmCustomDiffEngine.Notification(AComponent: TComponent;
  Operation: TOperation);
var
   Index : integer;
begin
  inherited;
  if operation = opremove then
  begin
    index := fAttachedViewers.indexof(AComponent);
    if index <> -1 then
       fAttachedViewers.Delete(index);
  end;
end;

procedure TrmCustomDiffEngine.SetMatchDepth(const Value: integer);
begin
  if (fMatchDepth <> value) and (value > 0) and (value < 100) then
    fMatchDepth := Value;
end;

{ TrmCustomDiffViewer }

constructor TrmCustomDiffViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 150;
  width := 250;

  ControlStyle := ControlStyle - [csAcceptsControls];

  fEBGColor := clBtnFace;
  fDBGColor := clYellow;

  fSimpleViewer := true;
end;

procedure TrmCustomDiffViewer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (operation = opRemove) then
  begin
    if (AComponent = fDiff) then
      fDiff := nil;
    if (AComponent = fDiffMap) then
      fDiffMap := nil;
  end;
end;

procedure TrmCustomDiffViewer.SetDiff(const Value: TrmCustomDiffEngine);
begin
  if value <> fDiff then
  begin
    if assigned(fDiff) then
      fDiff.RemoveViewer(self);

    fDiff := Value;
    
    if assigned(fDiff) then
    begin
      fDiff.FreeNotification(self);
      fDiff.AttachViewer(self);
    end
  end;
end;

procedure TrmCustomDiffViewer.SetDBGColor(const Value: TColor);
begin
  fDBGColor := Value;
  Invalidate;
end;

procedure TrmCustomDiffViewer.SetEBGColor(const Value: TColor);
begin
  fEBGColor := Value;
  Invalidate;
end;

procedure TrmCustomDiffViewer.SetSimpleViewer(const Value: boolean);
begin
  fSimpleViewer := Value;
  invalidate;
end;

procedure TrmCustomDiffViewer.SetCTColor(const Value: TColor);
begin
  fCTColor := Value;
  invalidate;
end;

procedure TrmCustomDiffViewer.SetDTColor(const Value: TColor);
begin
  fDColor := Value;
  invalidate;
end;

procedure TrmCustomDiffViewer.SetITColor(const Value: TColor);
begin
  fITColor := Value;
  invalidate;
end;

procedure TrmCustomDiffViewer.SetDiffMap(const Value: TrmDiffMap);
begin
  if fDiffMap <> Value then
  begin
    fDiffMap := value;
    if assigned(fDiffMap) then
    begin
      fDiffMap.FreeNotification(self);
      fDiffMap.SetData(GetMapData);
      fDiffMap.OnMapClick := MapClick;
    end;
  end;

end;

function TrmCustomDiffViewer.GetMapData: string;
var
  loop: integer;
  wstr: string;
begin
  wstr := '';
  if assigned(DiffEngine) and (DiffEngine.fDiffSource1.count > 0) then
  begin
     setlength(wStr, DiffEngine.fDiffSource1.count);
     for loop := 0 to DiffEngine.fDiffSource1.Count - 1 do
     begin
       case DiffEngine.fDiffSource1[loop][1] of
         NormalLine: wstr[loop + 1] := NormalLine;
         DeletedLine: wstr[loop + 1] := DeletedLine;
         ChangedLine: wstr[loop + 1] := ChangedLine;
         EmptyLine: wstr[loop + 1] := InsertedLine;
       end;
     end;
  end;
  result := wstr;
end;

procedure TrmCustomDiffViewer.DiffferenceCompleted;
begin
   if assigned(fDiffMap) then
      fDiffMap.SetData(GetMapData);  
end;

function TrmCustomDiffViewer.GetSrc1Label: string;
begin
   result := 'Source 1';
end;

function TrmCustomDiffViewer.GetSrc2Label: string;
begin
   result := 'Source 2';
end;

procedure TrmCustomDiffViewer.SetSrc1Label(const Value: string);
begin
   //do nothing...
end;

procedure TrmCustomDiffViewer.SetSrc2Label(const Value: string);
begin
   //do nothing...
end;

procedure TrmCustomDiffViewer.wmEraseBKGrnd(var msg: tmessage);
begin
  msg.result := 1;
end;

{ TrmDiffMergeViewer }

function TrmDiffMergeViewer.CanChangeSource: boolean;
begin
  result := (fMergeSource.items.count > 0) and (fMergeSource.Items[fMergeSource.itemindex][1] <> NormalLine);
end;

procedure TrmDiffMergeViewer.ClearSource;
var
  oldIndex, oldScrollPos: integer;
begin
  oldindex := fMergeSource.ItemIndex;
  oldScrollPos := fMergeSource.VScrollPos;
  try
    fMergeSource.Items[fMergeSource.ItemIndex] := EmptyLine + '';
  finally
    ResetIndex(OldIndex, OldScrollPos);
  end;
  invalidate;
end;

procedure TrmDiffMergeViewer.cmFontChanged(var Msg: TMessage);
begin
  inherited;
  fsource1.font.Assign(font);
  fsource2.font.Assign(font);
  fMergeSource.font.assign(font);
end;

procedure TrmDiffMergeViewer.CopySource(Source1: boolean);
var
  wstr: string;
  oldIndex, OldScrollPos, loop: integer;
  wSource : TrmListControl;
begin
  oldindex := fMergeSource.ItemIndex;
  oldScrollPos := fMergeSource.VScrollPos;

  if Source1 then
     wSource := fSource1
  else
     wSource := fSource2;

  try
    for loop := wSource.SelStart to (wSource.SelStart + wSource.SelCount) do
    begin
       wstr := wSource.Items[loop];
       if wstr[1] <> emptyline then
       begin
         delete(wstr, 1, 1);
         wstr := MergeLine1 + wstr;
       end
       else
         wstr := EmptyLine + '';
       fMergeSource.Items[loop] := wstr;
    end;
  finally
    ResetIndex(oldIndex, OldScrollPos);
    invalidate;
  end;
end;

constructor TrmDiffMergeViewer.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls];

  BevelEdges := [beLeft, beTop, beRight, beBottom];
  BevelInner := bvLowered;
  BevelOuter := bvLowered;
  BevelKind := bkTile;

  fD1BGColor := clAqua;
  fD2BGColor := clYellow;

  fDrawing := false;
  fScrollInProgress := false;
  fIHC := false;
  fSourceSide := 0;

  fPanel := TPanel.create(self);
  with fPanel do
  begin
    ControlStyle := ControlStyle - [csAcceptsControls];
    Parent := self;
    align := altop;
    BevelInner := bvlowered;
    BevelOuter := bvraised;
  end;

  fLabel1 := TLabel.create(fPanel);
  with fLabel1 do
  begin
    parent := fPanel;
    align := alLeft;
    AutoSize := false;
    Caption := 'Source 1';
    Alignment := taCenter;
  end;

  fLabel2 := TLabel.create(fPanel);
  with fLabel2 do
  begin
    parent := fPanel;
    align := alLeft;
    AutoSize := false;
    Caption := 'Merged Source';
    Alignment := taCenter;
  end;

  fLabel3 := TLabel.create(fPanel);
  with fLabel3 do
  begin
    parent := fPanel;
    align := alClient;
    AutoSize := false;
    Caption := 'Source 2';
    Alignment := taCenter;
  end;

  fSource1 := TrmListControl.create(self);
  with fSource1 do
  begin
    name := 'SourceList1';
    parent := self;
    align := alLeft;
    font.assign(self.font);
    enabled := false;
    OnFormatDrawing := Drawing;
    ScrollBars := ssNone;
    Multiselect := true;
    Tag := 1;
  end;

  fBevel := TBevel.Create(self);
  with fBevel do
  begin
    parent := self;
    align := alLeft;
    width := 2;
  end;

  fMergeSource := TrmListControl.create(self);
  with fMergeSource do
  begin
    name := 'MergedSourceList';
    parent := self;
    align := alLeft;
    font.assign(self.font);
    TabStop := true;
    OnScroll := ScrollChanged;
    OnFormatDrawing := Drawing;
    ScrollBars := ssNone;
    Multiselect := true;
    Tag := 0;
  end;

  fBevel2 := TBevel.Create(self);
  with fBevel2 do
  begin
    parent := self;
    align := alLeft;
    width := 2;
  end;

  fSource2 := TrmListControl.create(self);
  with fSource2 do
  begin
    name := 'SourceList2';
    parent := self;
    align := alLeft;
    enabled := false;
    font.assign(self.font);
    OnFormatDrawing := Drawing;
    ScrollBars := ssNone;
    Multiselect := true;
    Tag := 2;
  end;
end;

procedure TrmDiffMergeViewer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.style := Params.style or WS_VSCROLL;
  if not fIHC then
    Params.style := Params.style or WS_HSCROLL;
end;

procedure TrmDiffMergeViewer.DiffferenceCompleted;
var
  loop, wLongLine: integer;
begin
  inherited;
  fSource1.Items.BeginUpdate;
  fSource2.Items.BeginUpdate;
  fMergeSource.Items.BeginUpdate;
  try
    fMergeSource.Items.Clear;
    fSource1.Items.assign(DiffEngine.DiffSource1);
    fSource2.Items.assign(DiffEngine.DiffSource2);
    for loop := 0 to fSource1.Items.count - 1 do
    begin
      if fSource1.items[loop][1] = NormalLine then
        fMergeSource.Items.Add(fSource1.items[loop])
      else
        fMergeSource.Items.Add(Emptyline + '')
    end;
    UpdateVScrollBar;
    UpdateHScrollBar;
  finally
    fSource1.Items.EndUpdate;
    fSource2.Items.EndUpdate;
    fMergeSource.Items.EndUpdate;
  end;

  wLongLine := max(fSource1.LongestLineLength, fSource2.LongestLineLength);
  wLongLine := max(wLongLine, fMergeSource.LongestLineLength);

  fSource1.LongestLineLength := wLongLine;
  fSource2.LongestLineLength := wLongLine;
  fMergeSource.LongestLineLength := wLongLine;

  invalidate;
end;

procedure TrmDiffMergeViewer.Drawing(Sender: TObject; Canvas: TCanvas; Selected: boolean;
  var str: string);
var
  status: char;
begin
  status := str[1];
  delete(str, 1, 1);

  Canvas.Font.Color := clWindowText;
  case status of
    NormalLine: Canvas.brush.color := clWindow;
    ChangedLine, DeletedLine, InsertedLine:
      begin
        if Sender = fsource1 then
          Canvas.Brush.Color := fD1BGColor
        else if Sender = fsource2 then
          Canvas.Brush.Color := fD2BGColor;

        Canvas.Font.Color := ChangedTextColor;

        if not SimpleDiffViewer then
        begin
          case status of
            DeletedLine: Canvas.Font.Color := DeletedTextColor;
            InsertedLine: Canvas.Font.Color := InsertedTextColor;
          end;
        end;
      end;
    EmptyLine: Canvas.Brush.Color := fEBGColor;
    MergeLine1: Canvas.Brush.Color := fD1BGColor;
    MergeLine2: Canvas.Brush.Color := fD2BGColor;
  end;

  if (Selected) then
  begin
    Canvas.font.color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end;
end;

function TrmDiffMergeViewer.GetMergeLable: string;
begin
   result := fLabel2.caption;
end;

function TrmDiffMergeViewer.GetSrc1Label: string;
begin
   result := flabel1.caption;
end;

function TrmDiffMergeViewer.GetSrc2Label: string;
begin
   result := flabel3.caption;
end;

procedure TrmDiffMergeViewer.Loaded;
begin
  inherited;
  fsource1.left := 0;
  fBevel.left := fsource1.width;
  fMergeSource.left := fBevel.left + fBevel.width;
  fBevel2.left := fMergeSource.left + fMergeSource.width;
  fSource2.left := fBevel2.left + fBevel2.width;
  fPanel.height := Canvas.TextHeight('X') + 4;
  Resize;
end;

procedure TrmDiffMergeViewer.MapClick(Sender: TObject;
  IndicatorPos: integer);
begin
   ResetIndex(IndicatorPos, IndicatorPos);
end;

procedure TrmDiffMergeViewer.ResetIndex(Index, Pos: integer);
begin
  fMergeSource.VScrollPos := pos;
  fMergeSource.ItemIndex := Index;

  fSource1.VScrollPos := pos;
  fSource1.ItemIndex := Index;

  fSource2.VScrollPos := pos;
  fSource2.ItemIndex := Index;
  UpdateVScrollBar;
end;

procedure TrmDiffMergeViewer.Resize;
var
  wcw: integer;
begin
  inherited;
  wcw := ClientWidth div 3;
  fLabel1.width := wcw;
  fLabel2.width := wcw;
  fLabel3.width := wcw;

  wcw := ClientWidth - (fBevel.width * 2);
  fSource1.Width := wcw div 3;
  fMergeSource.width := fSource1.Width;
  fSource2.Width := fSource1.Width + (wcw mod 3);

  UpdateVScrollBar;
  UpdateHScrollBar;
end;

procedure TrmDiffMergeViewer.SaveMergeToFile(FileName: string);
var
  fstrm: TFileStream;
begin
  fstrm := TFileStream.create(filename, fmCreate);
  try
    SaveMergeToStream(fStrm);
  finally
    fstrm.free;
  end;
end;

procedure TrmDiffMergeViewer.SaveMergeToStream(Strm: TStream);
var
  wstr: string;
  loop: integer;
begin
  if assigned(strm) then
  begin
    strm.Position := 0;
    for loop := 0 to fMergeSource.Items.count - 1 do
    begin
      wstr := fMergeSource.Items[loop] + #13#10;
      if wstr[1] <> emptyline then
      begin
        delete(wstr, 1, 1);
        Strm.Write(wstr, length(wstr));
      end;
    end;
  end
  else
    raise EStreamError.create('Stream not open for writing');
end;

procedure TrmDiffMergeViewer.scrollChanged(Sender: TObject; ScrollBar: integer);
begin
  if fScrollInProgress then exit;
  fScrollInProgress := true;
  try
    if fIHC and (ScrollBar = sb_Horz) then
      exit;

    if ScrollBar = SB_VERT then
    begin
      if fSource1.ItemIndex = fMergeSource.ItemIndex then
      begin
         fSource1.VScrollPos := fMergeSource.VScrollPos;
         fSource2.VScrollPos := fMergeSource.VScrollPos;
      end
      else
      begin
         fSource1.ItemIndex := fMergeSource.ItemIndex;
         fSource1.SelStart := fMergeSource.SelStart;
         fSource1.SelCount := fMergeSource.SelCount;
         fSource2.ItemIndex := fMergeSource.ItemIndex;
         fSource2.SelStart := fMergeSource.SelStart;
         fSource2.SelCount := fMergeSource.SelCount;
      end;
      UpdateVScrollBar;
      if assigned(DiffMap) then
         DiffMap.IndicatorPos := fMergeSource.ItemIndex;
    end
    else
    begin
      fSource1.HScrollPos := fMergeSource.HScrollPos;
      fSource2.HScrollPos := fMergeSource.HScrollPos;
      UpdateHScrollBar;
    end;
  finally
    fScrollInProgress := false;
  end;
end;

procedure TrmDiffMergeViewer.SetD1BGColor(const Value: TColor);
begin
  fD1BGColor := Value;
  invalidate;
end;

procedure TrmDiffMergeViewer.SetD2BGColor(const Value: TColor);
begin
  fD2BGColor := Value;
  invalidate;
end;

procedure TrmDiffMergeViewer.SetIHC(const Value: boolean);
begin
  if fIHC <> Value then
  begin
    fIHC := Value;
    if fIHC then
    begin
       fsource1.ScrollBars := ssHorizontal;
       fsource2.ScrollBars := ssHorizontal;
       fMergeSource.ScrollBars := ssNone;
    end
    else
    begin
       fsource1.ScrollBars := ssNone;
       fsource2.ScrollBars := ssNone;
       fMergeSource.ScrollBars := ssNone;
    end;
    RecreateWnd;
  end;
end;

procedure TrmDiffMergeViewer.SetMergeLabel(const Value: string);
begin
   fLabel2.caption := value;
end;

procedure TrmDiffMergeViewer.SetSourceSide(const Value: smallint);
begin
  if (value >= 0) or (value <= 2) then
  begin
     FSourceSide := Value;
     case fSourceSide of
       0: begin
             fsource1.HideSelection := false;
             fsource2.HideSelection := false;
          end;
       1: begin
             fsource1.HideSelection := false;
             fsource2.HideSelection := true;
          end;
       2: begin
             fsource1.HideSelection := true;
             fsource2.HideSelection := false;
          end;
     end;
  end;
end;

procedure TrmDiffMergeViewer.SetSrc1Label(const Value: string);
begin
   flabel1.caption := value;
end;

procedure TrmDiffMergeViewer.SetSrc2Label(const Value: string);
begin
   flabel3.caption := value;
end;

procedure TrmDiffMergeViewer.UpdateHScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  if fIHC then
    exit;

  with wScrollInfo do
  begin
    cbSize := sizeof(TScrollInfo);
    fMask := SIF_POS or SIF_RANGE {or SIF_DISABLENOSCROLL};
    nMin := 0;
    nMax := fMergeSource.HScrollSize;
    nPos := fMergeSource.HScrollPos;
  end;

  SetScrollInfo(Handle, SB_HORZ, wScrollInfo, True);
end;

procedure TrmDiffMergeViewer.UpdateVScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  with wScrollInfo do
  begin
    cbSize := sizeof(TScrollInfo);
    fMask := SIF_POS or SIF_RANGE {or SIF_DISABLENOSCROLL};
    nMin := 0;
    nMax := fMergeSource.VScrollSize;
    nPos := fMergeSource.VScrollPos;
  end;

  SetScrollInfo(Handle, SB_VERT, wScrollInfo, True);
end;

procedure TrmDiffMergeViewer.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  fMergeSource.Dispatch(msg);
  UpdateHScrollBar;
end;

procedure TrmDiffMergeViewer.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  fMergeSource.Dispatch(msg);
  UpdateVScrollBar;
end;

{ TrmDiffViewer }

procedure TrmDiffViewer.cmFontChanged(var Msg: TMessage);
begin
  inherited;
  fsource1.font.Assign(font);
  fsource2.font.Assign(font);
end;

constructor TrmDiffViewer.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls];

  BevelEdges := [beLeft, beTop, beRight, beBottom];
  BevelInner := bvLowered;
  BevelOuter := bvLowered;
  BevelKind := bkTile;

  fDrawing := false;
  fScrollInProgress := false;
  fIHC := false;
  fLockSelIndex := true;

  fPanel := TPanel.create(self);
  with fPanel do
  begin
    ControlStyle := ControlStyle - [csAcceptsControls];
    Parent := self;
    align := altop;
    BevelInner := bvlowered;
    BevelOuter := bvraised;
  end;

  fLabel1 := TLabel.create(fPanel);
  with fLabel1 do
  begin
    parent := fPanel;
    align := alLeft;
    AutoSize := false;
    Caption := 'Source 1';
    Alignment := taCenter;
  end;

  fLabel2 := TLabel.create(fPanel);
  with fLabel2 do
  begin
    parent := fPanel;
    align := alClient;
    AutoSize := false;
    Caption := 'Source 2';
    Alignment := taCenter;
  end;

  fSource1 := TrmListControl.create(self);
  with fSource1 do
  begin
    name := 's1';
    parent := self;
    align := alLeft;
    font.assign(self.font);
    TabStop := true;
    OnScroll := ScrollChanged;
    OnFormatDrawing := Drawing;
    ScrollBars := ssNone;
  end;

  fBevel := TBevel.Create(self);
  with fBevel do
  begin
    parent := self;
    align := alLeft;
    width := 2;
  end;

  fSource2 := TrmListControl.create(self);
  with fSource2 do
  begin
    name := 's2';
    parent := self;
    align := alLeft;
    font.assign(self.font);
    TabStop := true;
    OnScroll := ScrollChanged;
    OnFormatDrawing := Drawing;
    ScrollBars := ssNone;
  end;
end;

procedure TrmDiffViewer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.style := Params.style or WS_VSCROLL;
  if not fIHC then
    Params.style := Params.style or WS_HSCROLL;
end;

procedure TrmDiffViewer.DiffferenceCompleted;
var
   wLongLine : integer;
begin
  inherited;
  fSource1.Items.BeginUpdate;
  fSource2.Items.BeginUpdate;
  try
     fSource1.Items.assign(DiffEngine.DiffSource1);
     fSource2.Items.assign(DiffEngine.DiffSource2);
    UpdateVScrollBar;
    UpdateHScrollBar;
  finally
    fSource1.Items.EndUpdate;
    fSource2.Items.EndUpdate;
  end;
  wLongLine := max(fSource1.LongestLineLength, fSource2.LongestLineLength);
  fSource1.LongestLineLength := wLongLine;
  fSource2.LongestLineLength := wLongLine;
  invalidate;
end;

procedure TrmDiffViewer.Drawing(Sender: TObject; Canvas: TCanvas;
  Selected: boolean; var str: string);
var
  status: char;
begin
  status := str[1];
  delete(str, 1, 1);

  Canvas.Font.Color := clWindowText;
  case status of
    NormalLine: Canvas.brush.color := clWindow;
    ChangedLine, DeletedLine, InsertedLine:
      begin
        Canvas.Brush.Color := fDBGColor;
        Canvas.Font.Color := ChangedTextColor;
        if not SimpleDiffViewer then
        begin
          case status of
            DeletedLine: Canvas.Font.Color := DeletedTextColor;
            InsertedLine: Canvas.Font.Color := InsertedTextColor;
          end;
        end;
      end;
    EmptyLine: Canvas.Brush.Color := fEBGColor;
  end;

  if (Selected) then
  begin
    Canvas.font.color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end;
end;

function TrmDiffViewer.GetSrc1Label: string;
begin
   result := fLabel1.Caption;
end;

function TrmDiffViewer.GetSrc2Label: string;
begin
   result := flabel2.caption;
end;

procedure TrmDiffViewer.Loaded;
begin
  inherited;
  fsource1.left := 0;
  fBevel.left := fsource1.width;
  fSource2.left := fBevel.left + fBevel.width;
  fPanel.height := Canvas.TextHeight('X') + 4;
  Resize;
end;

procedure TrmDiffViewer.MapClick(Sender: TObject; IndicatorPos: integer);
begin
   ResetIndex(IndicatorPos, IndicatorPos);
end;

procedure TrmDiffViewer.ResetIndex(Index, Pos: integer);
begin
  fSource1.VScrollPos := pos;
  fSource1.ItemIndex := Index;
  fSource2.VScrollPos := pos;
  fSource2.ItemIndex := Index;
  UpdateVScrollBar;
end;

procedure TrmDiffViewer.Resize;
var
  wcw: integer;
begin
  inherited;
  wcw := ClientWidth div 2;
  fLabel1.width := wcw;
  fLabel2.width := wcw;

  wcw := ClientWidth - fBevel.width;
  fSource1.Width := wcw div 2;
  fSource2.Width := fSource1.Width + (wcw mod 2);

  UpdateVScrollBar;
  UpdateHScrollBar;
end;

procedure TrmDiffViewer.scrollChanged(Sender: TObject; ScrollBar: integer);
begin
  if fScrollInProgress then exit;
  fScrollInProgress := true;
  try
    if fIHC and (ScrollBar = sb_Horz) then
      exit;
    if (sender = fsource1) then
    begin
      if ScrollBar = SB_VERT then
      begin
        if fLockSelIndex then
        begin
           fSource2.ItemIndex := fSource1.ItemIndex;
           fSource2.SelCount := fsource1.SelCount;
        end;
        fSource2.VScrollPos := fSource1.VScrollPos;
      end
      else
        fSource2.HScrollPos := fSource1.HScrollPos;
    end
    else if (sender = fSource2) then
    begin
      if ScrollBar = SB_VERT then
      begin
        if fLockSelIndex then
        begin
           fSource1.ItemIndex := fSource2.ItemIndex;
           fSource1.SelCount := fSource2.SelCount;
        end;
        fSource1.VScrollPos := fSource2.VScrollPos;
      end
      else
        fSource1.HScrollPos := fSource2.HScrollPos;
    end;
    if ScrollBar = SB_VERT then
      UpdateVScrollBar
    else
      UpdateHScrollBar;
  finally
    fScrollInProgress := false;
  end;
end;

procedure TrmDiffViewer.SetDiffMap(const Value: TrmDiffMap);
begin
  inherited;
  if assigned(DiffMap) then
     DiffMap.ShowIndicator := false;
end;

procedure TrmDiffViewer.SetIHC(const Value: boolean);
begin
  if fIHC <> Value then
  begin
    fIHC := Value;
    if fIHC then
    begin
         fsource1.ScrollBars := ssHorizontal;
         fSource2.ScrollBars := ssHorizontal;
    end
    else
    begin
         fsource1.ScrollBars := ssNone;
         fSource2.ScrollBars := ssNone;
    end;
    RecreateWnd;
  end;
end;

procedure TrmDiffViewer.SetLockSelIndex(const Value: boolean);
begin
  if fLockSelIndex <> Value then
  begin
     fLockSelIndex := Value;
     if fLockSelIndex then
     begin
        fsource2.VScrollPos := fsource1.VScrollPos;
        fsource2.ItemIndex := fsource1.itemIndex;
     end;
  end;
end;

procedure TrmDiffViewer.SetSrc1Label(const Value: string);
begin
   fLabel1.caption := value;
end;

procedure TrmDiffViewer.SetSrc2Label(const Value: string);
begin
   fLabel2.caption := value;
end;

procedure TrmDiffViewer.UpdateHScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  if fIHC then
    exit;

  with wScrollInfo do
  begin
    cbSize := sizeof(TScrollInfo);
    fMask := SIF_POS or SIF_RANGE {or SIF_DISABLENOSCROLL};
    nMin := 0;
    nMax := fsource1.HScrollSize;
    nPos := fsource1.HScrollPos;
  end;

  SetScrollInfo(Handle, SB_HORZ, wScrollInfo, True);
end;

procedure TrmDiffViewer.UpdateVScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  with wScrollInfo do
  begin
    cbSize := sizeof(TScrollInfo);
    fMask := SIF_POS or SIF_RANGE {or SIF_DISABLENOSCROLL};
    nMin := 0;
    nMax := fsource1.VScrollSize;
    nPos := fsource1.VScrollPos;
  end;

  SetScrollInfo(Handle, SB_VERT, wScrollInfo, True);
end;

procedure TrmDiffViewer.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if not fIHC then
  begin
    fsource1.Dispatch(msg);
    UpdateHScrollBar;
  end;
end;

procedure TrmDiffViewer.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  fsource1.Dispatch(msg);
  UpdateVScrollBar;
end;

{ TrmDiffMap }

constructor TrmDiffMap.Create(AOwner: TComponent);
begin
  inherited;
  width := 25;
  height := 200;

  FIndicator := TBitmap.Create;
  FIndicator.LoadFromResourceName(HInstance,'DIFFMAP_INDICATOR');
  FIndicator.Transparent := True;

  FColorDeleted := clRed;
  FColorInserted := clLime;
  FColorModified := clYellow;

  FShowIndicator := True;

  IndicatorPos := 0;
end;

destructor TrmDiffMap.Destroy;
begin
  FIndicator.Free;
  inherited;
end;

procedure TrmDiffMap.SetData(Value: string);
begin
  FData := copy(Value, 1, length(Value));
  refresh;
end;

procedure TrmDiffMap.SetIndicatorPos(Value: integer);
begin
  FIndicatorPos := Value;
  Refresh;
end;

procedure TrmDiffMap.Paint;
var
  i: Integer;
  j: Integer;
  NrOfDataRows: Integer;
  Ht: Integer;
  Ct: Integer;
  CurrIndex: Integer;
  PixelPos: Integer;
  PixelHt: Double; // amount of pixel height for each row - could be a rather small number
  PixelFrac: Double; // Faction part of pixel - left over from previous
  PixelPrevHt: Double; // logical height of previous mapped pixel (eg. .92)
  NrOfPixelRows: Double; // Number of rows that the current pixel is to represent.
  ExtraPixel: Double; // Left over pixel from when calculating number of rows for the next pixel.
                               // eg. 1/.3 = 3 rows, .1 remaining, next 1.1/.3 = 3 rows, .2 remain, next 1.2/.3 = 4 rows.

  DrawIt: Boolean; // Drawing flag for each column
  RowModified: Boolean;
  RowDeleted: Boolean;
  RowInserted: Boolean;

  ExitLoop: Boolean; // loop control

   // Draws the line between two points on the horizonatal line of i.
  procedure DrawLine(X1, X2: integer);
  var
    k: integer;
  begin
      // What colour?  Black or Background?
    if DrawIt then
    begin
      if RowModified then
      begin
        Canvas.Pen.Color := ColorModified;
      end
      else
      begin
        if RowInserted then
        begin
          Canvas.Pen.Color := ColorInserted;
        end
        else
        begin
          if RowDeleted then
          begin
            Canvas.Pen.Color := ColorDeleted;
          end;
        end;
      end;
    end
    else
    begin
      Canvas.Pen.Color := Color;
    end;
      // Draw the pixels for the map here
    for k := 0 to Round(NrOfPixelRows) - 1 do
    begin
      Canvas.MoveTo(X1, PixelPos + k);
      Canvas.LineTo(X2, PixelPos + k);
    end;
  end;

begin
  inherited;
  if csDesigning in ComponentState then
     exit;

  Ht := MapHeight;
  Ct := length(fData);

  if Ct < 1 then
  begin
    Ct := 1;
  end;
  PixelHt := Ht / Ct;
  CurrIndex := 1;
  NrOfPixelRows := 0.0;
  PixelPrevHt := 0.0;
  PixelPos := 5;
  ExtraPixel := 0.0;

  j := CurrIndex;
  while j < Ct do
  begin
    NrOfDataRows := 0;
    PixelPrevHt := PixelPrevHt - NrOfPixelRows; // remainder from prevous pixel row (+ or -)
    PixelFrac := frac(PixelPrevHt); // We want just the fractional part!

      // Calculate how high the pixel line is to be
    if PixelHt < 1.0 then
    begin
      NrOfPixelRows := 1.0; // Each Pixel line represents one or more rows of data
    end
    else
    begin
      NrOfPixelRows := Int(PixelHt + ExtraPixel); // We have several pixel lines for each row of data.
      ExtraPixel := frac(PixelHt + ExtraPixel); // save frac part for next time
    end;

      // Calculate the nr of data rows to be represented by the Pixel Line about to be drawn.
    ExitLoop := False;
    repeat
         // the '.../2.0' checks if half a Pixel Ht will fit, else leave remainder for next row.
      if (PixelFrac + PixelHt <= NrOfPixelRows) or
        (PixelFrac + PixelHt / 2.0 <= NrOfPixelRows) then
      begin
        PixelFrac := PixelFrac + PixelHt;
        inc(NrOfDataRows);
      end
      else
      begin
        ExitLoop := True;
      end;
    until (PixelFrac >= NrOfPixelRows) or (ExitLoop);

      // go through each data row, check if a file has been modified.
      // if any file has been modified then we add to the Mapping.
    if NrOfDataRows > 0 then
    begin
      DrawIt := False;
    end;

    RowModified := False;
    RowInserted := False;
    RowDeleted := False;

    for i := j to j + NrOfDataRows - 1 do
    begin
      if i < ct then
      begin
        case fData[i] of
          ChangedLine:
            begin
              DrawIt := True;
              RowModified := True;
            end;
          InsertedLine:
            begin
              DrawIt := True;
              RowInserted := True;
            end;
          DeletedLine:
            begin
              DrawIt := True;
              RowDeleted := True;
            end;
        end;
      end;
      inc(j);
    end;


      // Mapping is drawn here
    if ShowIndicator then
    begin
      DrawLine(FIndicator.Width, Width - FIndicator.Width);
    end
    else
    begin
      DrawLine(0, Width);
    end;
    inc(PixelPos, Trunc(NrOfPixelRows)); // the pixel pos on the map.
    PixelPrevHt := int(PixelPrevHt) + PixelFrac;
  end;

  if ShowIndicator then
     DrawIndicator;
end;

procedure TrmDiffMap.DrawIndicator;
var
  Y: integer;
begin
  Canvas.Pen.Color := clBlack;
  if length(fData) <> 0 then
  begin
    Y := TOP_MARGIN + Trunc((IndicatorPos / length(fData)) * MapHeight);
    Canvas.Draw(0, Y - (FIndicator.Height div 2), FIndicator);
  end;
end;

function TrmDiffMap.MapHeight: integer;
begin
  Result := Height - TOP_MARGIN - BOTTOM_MARGIN;
  if result < 1 then
     result := 1; //Map calculation correction...if ht < 1 then causes FP overflow...very bad!!!
end;

procedure TrmDiffMap.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewRow: Integer;
  PixelHt: Double;
begin
  inherited;
  if Assigned(OnMapClick) and (length(fData) > 0) then
  begin
    PixelHt := MapHeight / length(fData); // This is how much of a pixel (or how many pixels), each row represents.
      // (the pixel clicked) + Half the Pixel Height + 1.0 / the pixel height
      // eg. Of 1000 rows, in the pixel area height of 500, then pixelHt = .5 (500/1000)
      //     Therefore, if pixel 100 is clicked we get - (100 + .25 + 1) / .5 = 51
      // Y-5, we subtract 5 as we start 5 pixels from the top of lblQuickPickArea.
    NewRow := Round(((Y - TOP_MARGIN) * 1.0 + PixelHt / 2.0 + 1.0) / PixelHt);

    if NewRow > length(fData) - 1 then
      NewRow := length(fData) - 1
    else
    begin
      if NewRow < 1 then
         NewRow := 1;
    end;
    OnMapClick(self, NewRow);
    IndicatorPos := NewRow;
  end;
  //This is a new line...
end;

procedure TrmCustomDiffEngine.ClearData;
begin
  fDiffSource1.Clear;
  fDiffSource2.Clear;
  fSource1.Clear;
  fSource2.Clear;
end;

procedure TrmCustomDiffEngine.DiffStarted;
begin
  fBlankLines1 := 0;
  fBlankLines2 := 0;
end;

procedure TrmCustomDiffEngine.DiffFound(Source1, Source2: TrmDiffBlock);
var
  s1diff, s2diff: integer;
begin
  Source1.startline := source1.startline + fBlankLines1;
  Source1.Endline := source1.EndLine + fBlankLines1;

  Source2.startline := source2.startline + fBlankLines2;
  Source2.Endline := source2.Endline + fBlankLines2;

  s1diff := source1.endline - source1.startline;
  s2diff := source2.endline - source2.startline;

  if (s1diff = 0) and (s2diff > 0) then
  begin
    while s2diff > 0 do
    begin
      fDiffSource1.Add(EmptyLine + ' ');
      Inc(fBlankLines1);

      fDiffSource2.Add(InsertedLine + fSource2[0]);
      fSource2.delete(0);
      dec(s2Diff);
    end;
  end
  else if (s1diff > 0) and (s2diff = 0) then
  begin
    while s1diff > 0 do
    begin
      fDiffSource1.Add(DeletedLine + fSource1[0]);
      fSource1.delete(0);

      fDiffSource2.Add(Emptyline + '');
      Inc(fBlankLines2);
      dec(s1Diff);
    end;
  end
  else if (s1diff > 0) and (s2diff > 0) then
  begin
    if s1diff > s2diff then
    begin
      while s2diff > 0 do
      begin
        fDiffSource1.Add(ChangedLine + fSource1[0]);
        fSource1.delete(0);

        fDiffSource2.Add(ChangedLine + fSource2[0]);
        fSource2.delete(0);
        dec(s2Diff);
        dec(s1Diff);
      end;

      while s1Diff > 0 do
      begin
        fDiffSource1.Add(DeletedLine + fSource1[0]);
        fSource1.delete(0);

        fDiffSource2.Add(EmptyLine + ' ');
        Inc(fBlankLines2);
        dec(s1Diff);
      end;
    end
    else
    begin
      while s1diff > 0 do
      begin
        fDiffSource1.Add(ChangedLine + fSource1[0]);
        fSource1.delete(0);

        fDiffSource2.Add(ChangedLine + fSource2[0]);
        fSource2.delete(0);
        dec(s1Diff);
        dec(s2Diff);
      end;

      while s2Diff > 0 do
      begin
        fDiffSource2.Add(InsertedLine + fSource2[0]);
        fSource2.delete(0);

        fDiffSource1.Add(EmptyLine + ' ');
        Inc(fBlankLines1);
        dec(s2Diff);
      end;
    end;
  end
  else
    raise Exception.create('This should never happen');
end;

procedure TrmCustomDiffEngine.DiffCompleted;
var
   loop : integer;
begin
  while fSource1.count > 0 do
  begin
    fDiffSource1.add(NormalLine + fSource1[0]);
    fSource1.delete(0);
  end;

  while fSource2.count > 0 do
  begin
    fDiffSource2.add(NormalLine + fSource2[0]);
    fSource2.delete(0);
  end;

  while fDiffSource1.count < fDiffSource2.count do
    fDiffSource1.Add(EmptyLine + ' ');

  while fDiffSource2.count < fDiffSource1.count do
    fDiffSource2.Add(EmptyLine + ' ');

  for loop := 0 to fAttachedViewers.count-1 do
     TrmCustomDiffViewer(fattachedViewers[loop]).DiffferenceCompleted;

  if assigned(fOnDiffCompleted) then
     fOnDiffCompleted(self);
end;

procedure TrmCustomDiffEngine.AttachViewer(viewer: TrmCustomDiffViewer);
begin
   if fAttachedViewers.indexof(viewer) = -1 then
      fAttachedViewers.add(viewer);
end;

procedure TrmCustomDiffEngine.RemoveViewer(viewer: TrmCustomDiffViewer);
var
   index : integer;
begin
   index := fAttachedViewers.indexof(viewer);
   if index <> -1 then
      fAttachedViewers.Delete(index);
end;

function TrmCustomDiffEngine.RemoveCharacters(st: string): string;
var
   loop, len : integer;
begin
   len := length(st);
   result := '';
   loop := 0;
   while loop < len do
   begin
      inc(loop);
      if not (st[loop] in fIgnoreChars) then
         result := result + st[loop];
   end;
end;

end.
