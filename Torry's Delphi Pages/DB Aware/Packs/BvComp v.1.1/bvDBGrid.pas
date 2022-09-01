unit bvDBGrid;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QForms,
  QGraphics,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QControls,
  QGrids,
  Types,
  QImgList,
  Qt,
{$endif}
  Classes,
  SysUtils,
  DB,
  bvdbGridSaver,bvFindUnit,
  {$ifndef LINUX}
  bvdbTableSaver,{bvdbTablePrinter,}
  {$endif}
  bvGridPopupUnit,bvBookMark,bvdbGridDrawType;

type
  TGetCellParamsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; Highlight: Boolean) of object;


type
  TbvDBGrid = class(TDBGrid)
  private
    { Private declarations }
    FInCheckTitle:boolean;

    FOnGetCellParams: TGetCellParamsEvent;
    FSaver:TDbGridSaver;
    FFinder:bv_Find;
    FbvBookMark:TBvBookMark;
    {$ifndef LINUX}
    FTableSaver:TdbTableSaver;
    {FTablePrinter:TdbTablePrinter;}
    {$endif}
    FFixedCols: Integer;
    FTitleMinHeight: Integer;
    FLines3dV:Lines3dType;
    FLines3dH:Lines3dType;

    FENTER2Tab:boolean;
    FHintCells:boolean;

    FPrinterAutoPopup:boolean;

    FIsLoaded:boolean;

    FAfterRestore:TNotifyEvent;

    FStrippedRows:integer;
    FStrippedColor:TColor;

    FCellHint:THintWindow;

    FCellHeights:integer;

    FCheckTitle:boolean;

    procedure SetCellHeights(Value:integer);
    function  GetCellHeights:integer;

    procedure CloseHINT;

    {$ifndef LINUX}
    function GetAutoPopupTableSaver:boolean;
    procedure SetAutoPopupTableSaver(Value:boolean);

    function GetAutoPopupTablePrinter:boolean;
    procedure SetAutoPopupTablePrinter(Value:boolean);
    {$endif}

    function GetEnabledSaver:boolean;
    procedure SetEnabledSaver(Value:boolean);
    function GetAutoPopupSaver:boolean;
    procedure SetAutoPopupSaver(Value:boolean);
    function GetSaveWidthOnly:boolean;
    procedure SetSaveWidthOnly(Value:boolean);

    function GetAutoPopupFinder:boolean;
    procedure SetAutoPopupFinder(Value:boolean);

    function GetAutoPopupBookMark:boolean;
    procedure SetAutoPopupBookMark(Value:boolean);

    procedure SetFixedCols(Value: Integer);
    function GetFixedCols: Integer;

    procedure bvSetSelectedIndex(Value: Integer);


    procedure setStrippedRows(Value:integer);
    procedure SEtStrippedColor(Value:TColor);

    procedure SetTitleMinHeight(Value:integer);

    procedure WMSCROLLMOUSE(var Message: TMESSAGE); message $20A;
    //procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    //procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure OnHintTimer(Sender:tObject);

    procedure SetCheckTitle(Value:boolean);

    function GetSaverCannotAddColumns:boolean;
    procedure SetSaverCannotAddColumns(value:boolean);

  protected
    { Protected declarations }

    {$ifndef LINUX}
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    {$else}
    procedure MouseLeave(AControl:TControl); override;
    {$endif}

    procedure DblClick;override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    function NeededRECT(aText:string;aWidth:integer;
                        Alignment: TAlignment; ARightToLeft: Boolean):TRECT;

    procedure RowHeightsChanged; override;
    procedure ColWidthsChanged; override;


  public
    { Public declarations }


    property CheckTitle:boolean read FCheckTitle write SetCheckTitle;

    procedure SetColumnAttributes; override;

    procedure LayoutChanged; override;

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure SelectAll;
    procedure UnselectAll;
    procedure loaded; override;
    procedure GotoSelection(Index: Longint);
    property LeftCol;
    property Saver:TDBGridsaver read FSaver;
    property Finder:bv_Find read FFinder;
    property BvBookMark:TbvBookMark read FBvBookMark;
{$ifndef LINUX}
    property TableSaver:TDbTableSaver read FTableSaver;
    {property TablePrinter:TdbTablePrinter read FTablePrinter;}
{$endif}
    function CalcLeftColumn: Integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;


    procedure CheckTitleHeight;

    procedure SetLines3dV(value:Lines3dTYPE);
    procedure SetLines3dH(value:Lines3dTYPE);

    procedure SetLinesV(Value:boolean);
    procedure SetLinesH(value:boolean);

    function  GetLinesV:boolean;
    function  GetLinesH:Boolean;

    function GetSTDdefaultRowHeight:integer;

    function SelectedCoord:TRect;

    procedure PrintClick(Sender:tobject);

  published
    { Published declarations }
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;

    property SaverEnabled:boolean read GetEnabledSaver write SetEnabledSaver default true;
    property SaverAutoPopup:boolean read GetAutoPopupSaver write SetAutoPopupSaver default true;
    property SaverWidthOnly:boolean read GetSaveWidthOnly write SetSaveWidthOnly default false;

{$ifndef LINUX}
    property TableSaverAutoPopup:boolean read GetAutoPopupTableSaver write SetAutoPopupTableSaver default true;
    property TablePrinterAutoPopup:boolean read GetAutoPopupTablePrinter write SetAutoPopupTablePrinter default true;
{$endif}
    property FinderAutoPopup:boolean read GetAutoPopupFinder write SetAutoPopupFinder default true;
    property BookMarkAutoPopup:boolean read GetAutoPopupBookMark write SetAutoPopupBookMark default true;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property FixedCols: Integer read GetFixedCols write SetFixedCols default 0;

    property SelectedIndex write bvSetSelectedIndex;

    property StrippedRows:integer read FStrippedRows write SetStrippedRows default 0;
    property StrippedColor:TColor read FStrippedColor write SetStrippedColor;

    property CellHeights:integer read GetCellHeights write SetCellHeights;

    property TitleMinHeight:integer read FTitleMinHeight write SetTitleMinHeight;
    property Enter2Tab:boolean read FEnter2Tab write FEnter2Tab default false;

    property Lines3dV:Lines3dType read FLines3dV write SETLines3dV default c3dNONE;
    property Lines3dH:Lines3dType read FLines3dH write SETLines3dH default c3dNONE;
    property LinesV:boolean read GetLinesV write SETLinesV stored false;
    property LinesH:boolean read GetLinesH write SETLinesH stored false;
    property HintCells:boolean read FHintCells write FHintCells default true;
    property AfterRestore:TNotifyEvent read FAfterRestore write FAfterREstore;
    property PrinterAutoPopup:boolean read FPrinterAutoPopup write FPrinterAutopopup;

    property SaverCannotAddColumns:boolean read GetSaverCannotAddColumns write SetSaverCannotAddColumns;
  end;


type
    TbvGridPopupMenu = class(TbvCommonGridPopupMenu);

//var quickPrintEnabled:boolean;


implementation

uses math;

{$R bvdbgrid.res}

const
  bmArrow = 'BVDBGARROW';
  bmEdit = 'BVDBEDIT';
  bmInsert = 'BVDBINSERT';
  bmMultiDot = 'BVDBMULTIDOT';
  bmMultiArrow = 'BVDBMULTIARROW';

var
  FIndicators: TImageList=nil;



var drawBitmap:TBitmap;

{$ifdef LINUX}
const
  CXHSCROLL = 12;
  CYHSCROLL = 12;
  CXVSCROLL = 18;
{$endif}

const
  AlignFlags : array [TAlignment] of   Integer =
{$ifndef LINUX}
    ( DT_LEFT {or DT_WORDBREAK} or DT_EXPANDTABS or DT_NOPREFIX ,
      DT_RIGHT {or DT_WORDBREAK} or DT_EXPANDTABS or DT_NOPREFIX ,
      DT_CENTER {or DT_WORDBREAK} or DT_EXPANDTABS or DT_NOPREFIX  );
{$else}
    ( integer(AlignmentFlags_AlignLeft) {or DT_WORDBREAK} or integer(AlignmentFlags_ExpandTabs) {or AlignmentFlags_NOPREFIX} ,
      integer(AlignmentFlags_AlignRIGHT) {or DT_WORDBREAK} or integer(AlignmentFlags_EXPANDTABS) {or AlignmentFlags_NOPREFIX} ,
      integer(AlignmentFlags_AlignCENTER) {or DT_WORDBREAK} or integer(AlignmentFlags_EXPANDTABS) {or AlignmentFlags_NOPREFIX}  );

    //AlignmentFlags_ShowPrefix = 512 { $200 },
{$endif}



  RTL: array [Boolean] of Integer = (0,
               {$ifndef LINUX}
               DT_RTLREADING
               {$ELSE} $20000
               {$endif}
               );


procedure bvWriteText(Canvas:TCanvas;ARect: TRect;
                          const Text: string;
                          Alignment: TAlignment;
                          ARightToLeft: Boolean);
var
  TW,TH:integer;       
var
  B: TRect;
  LayoutFormat:integer;
begin
  begin                       { Use ExtTextOut for solid colors }

{$ifndef LINUX}
    if (Canvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft)
    then  ChangeBiDiModeAlignment(Alignment);
{$ENDIF}    

    Canvas.FillRect(arect);

    inflaterect(arect,-2,0);

    TW:=Canvas.TextWidth(Text);

    if TW<=ARECT.Right-arect.left  then begin
       LayoutFormat:=
           {$ifndef LINUX}
           DT_VCENTER
           {$else}
           integer(AlignmentFlags_AlignVCENTER)
           {$endif}
           or
           {$ifndef LINUX}
           dt_SingleLine
           {$else}
           integer(AlignmentFlags_SingleLine)
           {$endif}
           ;
    end
    else begin
      TH:=CAnvas.textheight('Hg');

      //DrawText(Acanvas.Handle, PChar(Text), Length(Text), DREct,
        // AlignFlags[Alignment] or RTL[ARightToLeft] or dt_WORDBREAK or DT_CALCRECT);

      if th*2>ARECT.bottom-Arect.top
      then begin
         LayoutFormat:=
           {$ifndef LINUX}
           DT_VCENTER
           {$else}
           integer(AlignmentFlags_AlignVCENTER)
           {$endif}
           or
           {$ifndef LINUX}
           dt_SingleLine
           {$else}
           integer(AlignmentFlags_SingleLine)
           {$endif}
           ;
      end
      else  begin
         LayoutFormat:=
           {$ifndef LINUX}
            DT_WORDBREAK
           {$else}
           integer(AlignmentFlags_WORDBREAK)
           {$endif}
           {$ifndef LINUX}
           or
            DT_NOPREFIX
           {$else}
           //integer(AlignmentFlags_SHOWPREFIX)
           {$endif}
           ;


         B:=ARECT;
         b.bottom:=b.top;
         {$ifndef LINUX}
         DrawText(canvas.Handle, PChar(Text), Length(Text), B,AlignFlags[Alignment] or  RTL[ARightToLeft] or LayoutFormat or DT_CALCRECT);
         {$else}
         Canvas.TextExtent(Text, B,AlignFlags[Alignment] or  RTL[ARightToLeft] or LayoutFormat);
         {$endif}
         if (B.bottom-b.top)<(arect.bottom-arect.top)
         then begin
           ARECT.Top:=((arect.bottom+arect.top)-(B.Bottom-b.top)) div 2
         end;
      end;
    end;


    {$ifndef LINUX}
    DrawText(canvas.Handle, PChar(Text), Length(Text), AREct,
        AlignFlags[Alignment] or RTL[ARightToLeft] or LayoutFormat);
    {$ELSE}
      Canvas.TextRect( ARect, ARect.left,ARect.top{0, 0}, Text,AlignFlags[Alignment] or RTL[ARightToLeft] or LayoutFormat);
    {$endif}
  end
end;

constructor TbvDbGrid.Create(AOwner:TComponent);
begin

  inherited;

  FLines3dV:=c3dNONE;
  FLines3dH:=c3dNONE;

  FInCheckTitle:=false;
  FCheckTitle:=true;

  FIsLoaded:=false;
  FFinder:=bv_Find.Create(self);
{$ifndef LINUX}
  FTableSaver:=TDBTableSaver.create(Self);
  {FTablePrinter:=TdbTablePrinter.create(self);}
{$endif}
  FSaver:=TdbGridSaver.Create(Self);
  FbvBookMark:=TbvBookMark.create(Self);
  FFixedCols:=0;
  FPrinterAutoPopup:=true;

  FStrippedRows:=0;
  FStrippedColor:=$00F4F7F4;

  FCellHint:=THintWindow.Create(Self);
  FCellHint.Color:= clInfoBK; //GetSysColor(COLOR_INFOBK);
  {$ifndef LINUX}
  FCellHInt.Canvas.Brush.Color:= clInfoBK; //GetSysColor(COLOR_INFOBK);
  FCellHInt.Canvas.Font.Color:= clInfoText; //GetSysColor(COLOR_INFOTEXT);
  FCellHInt.Canvas.Pen.Color:= clBlack;
  {$else}
  FCellHInt.Brush.Color:= clInfoBK; //GetSysColor(COLOR_INFOBK);
  // ??? in LINUX? FCellHInt.Font.Color:= clInfoText; //GetSysColor(COLOR_INFOTEXT);
  // ??? in LINUX? FCellHInt.Pen.Color:= clBlack;
  {$endif}

  FHintCells:=true;

  FCellHeights:=100;
  FTitleMinHeight:=100;
end;

destructor TbvdbGrid.Destroy;
begin
  if Assigned(FSaver) then begin
      if FSaver.Enabled then begin
        FSaver.SaveGrid;
        FSaver.Enabled:=false;
      end;
      FSaver.Free;
      FSaver:=nil;
  end;
  if Assigned(FFinder) then begin
      FFinder.Free;
      FFinder:=nil;
  end;
  if Assigned(FbvBookMark) then begin
      FbvBookMark.Free;
      FbvBookMark:=nil;
  end;
{$ifndef LINUX}
  if Assigned(FTablesaver) then begin
      FTableSaver.Free;
      FTableSaver:=nil;
  end;
  {if assigned(FTablePrinter) then begin
      FTablePrinter.free;
      FTablePrinter:=nil;
  end;}
{$endif}

  if Assigned(PopupMenu)
  then  begin
    if PopupMenu is TbvGridPopupMenu
    then PopupMenu.free
    {else begin
       i:=0;
       while i<PopupMenu.Items.Count do
         if (PopupMenu.Items[i] is TbvPrintMenuItem)
            and (PopupMenu.Items[i].Owner=Self)
         then begin
            //if PopupMenu.items.items.count>0 then PopupMenu.items.items.free;
            PopupMenu.Items.Delete(i)
         end
         else inc(i);
    end;}
  end;

  FCellHint.free;
  inherited;
end;

procedure TbvDBgrid.DblClick;
{$ifndef LINUX}
var i,ic:integer;
{$endif}
begin
  inherited;
  {$ifndef LINUX}
  if not Assigned(OndblClicK) and
     Assigned(popupmenu)
  then begin
     ic:=PopupMenu.Items.count;
     for i:=0 to IC-1 do begin
        if (PopupMenu.items[i].visible) and
           (PopupMenu.items[i].enabled) and
           (PopupMenu.items[i].Default)
        then begin
           PopupMenu.items[i].Click;
           break
        end;
     end;
  end
  {$endif}

end;

function TbvDBGrid.GetAutoPopupSaver:boolean;
begin
  if Assigned(FSaver) then  Result:=FSaver.AutoPopup
  else Result:=false;
end;

procedure TbvDBGrid.SetAutoPopupSaver(Value:boolean);
begin
  if Assigned(FSaver) then  FSaver.AutoPopup:=Value;
end;

{$ifndef LINUX}

function TbvDBGrid.GetAutoPopupTableSaver:boolean;
begin
  if Assigned(FTableSaver) then  Result:=FTableSaver.AutoPopup
  else Result:=false;
end;

function TbvDBGrid.GetAutoPopupTablePrinter:boolean;
begin
  REsult:=true;
  {if Assigned(FTablePrinter) then result:=FTablePrinter.AutoPopup
  else Result:=false}
end;

procedure TbvDBGrid.SetAutoPopupTableSaver(Value:boolean);
begin
  if Assigned(FTableSaver) then  FTableSaver.AutoPopup:=Value;
end;

procedure TbvDBGrid.SetAutoPopupTablePrinter(Value:boolean);
begin
  {if assigned(FTablePrinter) then FTablePrinter.AutoPopup:=value;}
end;
{$endif}

function TbvDBGrid.GetSaveWidthOnly:boolean;
begin
  if Assigned(FSaver) then  Result:=FSaver.SaveWidthOnly
  else Result:=false;
end;

procedure TbvDBGrid.SetSaveWidthOnly(Value:boolean);
begin
  if Assigned(FSaver) then  FSaver.SaveWidthOnly:=Value;
end;

procedure TbvDBGrid.Loaded;
//var ThItem:TbvTabPrinterMenuItem;
begin
  inherited;
  CellHeights:=cellHeights;
  if not FIsLoaded then begin
     if Assigned(FFinder) then FFinder.Loaded;
     if Assigned(FbvBookMark) then FbvBookMark.Loaded;
{$ifndef LINUX}
     if Assigned(FTableSaver) then FTableSaver.Loaded;
{$endif}
     if Assigned(FSaver) then FSaver.Loaded;
     //if assigned(FTablePrinter) then FTablePrinter.loaded;

     (*
     if quickPrintEnabled
        and PrinterAutoPopup and not (csDesigning in ComponentState)
     then begin
         if (PopupMenu=nil) then begin
            PopupMenu:=tbvGridpopupmenu.Create(Self);
   //               (ThisGrid.PopupMenu as tbvFinderPopupMenu).DrawText:='Таблица';
         end
         else if
              not (PopupMenu is TbvCommonGridPopupMenu)
              {and not (PopupMenu is TbvFinderPopupMenu)
              and not (PopupMenu is TbvTabSaverPopupMenu)
              and not (PopupMenu is TbvBookMarkPopupMenu)
              and not (PopupMenu is TbvGridPopupMenu)}
              and (popupmenu.items.count>0) and
              not ((popupmenu.items[popupmenu.items.count-1] is TbvCommonGridMenuItem)
                  )
         then begin
            ThItem:=tbvTabPrinterMenuItem.Create(self);
            ThItem.Caption:='-';
            PopupMenu.items.Add(ThItem);
         end;
         ThItem:=tbvTabPrinterMenuItem.Create(self);
         ThItem.Caption:='Быстрая печать';
         ThItem.OnClick:=PrintClick;
   //      thItem.ShortCut:=ShortCut(word('F'),[ssCtrl]);
         ThItem.bitmap.Handle:=LoadBitmap(Hinstance,'rsprint');
         ThItem.HelpContext:=0;
//         thItem.Enabled:=quickPrintEnabled;
         PopupMenu.items.Add(ThItem);   //resource discarded
   //            SetMenuItemBitmaps(FThisGrid.popupmenu.Handle,ThItem.Command,MF_BYCOMMAND,LoadBitmap(Hinstance,'bv_Finder'),LoadBitmap(Hinstance,'bv_Finder'));
     end;
     *)
     FIsLoaded:=true;
   end
end;

function TbvDBGrid.GetEnabledSaver:boolean;
begin
  if Assigned(FSaver) then Result:=FSaver.Enabled
  else result:=false;
end;

procedure TbvDBGrid.SetEnabledSaver(Value:boolean);
begin
  if Assigned(fSaver) then FSaver.Enabled:=Value;
end;


procedure TbvDBGrid.SelectAll;
var
  ABookmark: TBookmark;
begin
  if  (dgMultiSelect in Options) and DataLink.Active then begin //TDBGrid
    with Datalink.Dataset do begin
      if (BOF and EOF) then Exit;
      DisableControls;
      try
        ABookmark := GetBookmark;
        try
          First;
          while not EOF do begin
            SelectedRows.CurrentRowSelected := True;
            Next;
          end;
        finally
          try
            if Assigned(ABookMark) and BookMarkValid(ABookMark)
            then  GotoBookmark(ABookmark);
          except
          end;
          if Assigned(ABookMark) then FreeBookmark(ABookmark);
        end;
      finally
        EnableControls;
      end;
    end;
  end;
end;

procedure TbvDBGrid.UnselectAll;
begin
  if dgMultiSelect in Options then begin
    SelectedRows.Clear;
  end;
end;

procedure TbvDBGrid.GotoSelection(Index: Longint);
begin
  if (dgMultiSelect in Options) and DataLink.Active and (Index < SelectedRows.Count) and
    (Index >= 0) then
    Datalink.DataSet.GotoBookmark(Pointer(SelectedRows[Index]));
end;



function TbvDBGrid.GetAutoPopupFinder:boolean;
begin
  if Assigned(FFinder) then  Result:=FFinder.AutoPopup
  else Result:=false;
end;

procedure TbvDBGrid.SetAutoPopupFinder(Value:boolean);
begin
  if Assigned(FFinder) then FFinder.AutoPopup:=Value;
end;

function TbvDBGrid.GetAutoPopupBookmark:boolean;
begin
  if Assigned(FbvBookMark) then  Result:=FbvBookMark.AutoPopup
  else Result:=false;
end;

procedure TbvDBGrid.SetAutoPopupBookMark(Value:boolean);
begin
  if Assigned(FbvBookMark) then FbvBookMark.AutoPopup:=Value;
end;

procedure TbvDBGrid.SetFixedCols(Value: Integer);
var
  FixCount, I: Integer;
begin
  FixCount := Max(Value, 0) + IndicatorOffset;
  if DataLink.Active and not (csLoading in ComponentState) and
    (ColCount > IndicatorOffset + 1) then
  begin
    FixCount := Min(FixCount, ColCount - 1);
    inherited FixedCols := FixCount;
    for I := 1 to Min(FixedCols, ColCount - 1) do
      TabStops[I] := False;
  end;
  FFixedCols := FixCount - IndicatorOffset;
end;

function TbvDBGrid.GetFixedCols: Integer;
begin
  if DataLink.Active then Result := inherited FixedCols - IndicatorOffset
  else Result := FFixedCols;
end;

procedure TbvDBGrid.SetColumnAttributes;
begin
  inherited SetColumnAttributes;
  SetFixedCols(FFixedCols);
  CheckTitleHeight;
end;


procedure tbvdbgrid.ColWidthsChanged;
begin
  inherited; //ColWidthsChanged;
end;

procedure TbvDBGrid.LayoutChanged;
var
  ACol: Longint;
begin
  ACol := Col;
  inherited LayoutChanged;
  if Datalink.Active and (FixedCols > 0) then
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
end;

function TbvDBGrid.CalcLeftColumn: Integer;
begin
  Result := FixedCols + IndicatorOffset;
  while (Result < ColCount) and (ColWidths[Result] <= 0) do
    Inc(Result);
end;

{
procedure TbvDBGrid.WMChar(var Msg: TWMChar);
begin
  if FEnter2Tab and (Msg.CharCode=vk_RETURN)
  then begin
       MSG.charCode:=vk_TAB;
  end;
  inherited;
end;
}
procedure TbvDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyDownEvent: TKeyEvent;

begin
  CloseHint;

  if FEnter2Tab and (KEY={$ifndef LINUX} vk_RETURN {$else}key_return{$endif})
  then begin
      Key:={$ifndef LINUX} vk_TAB {$else}key_tab{$endif};
      KeyDown(key,shift);
      exit;
  end;

  KeyDownEvent := OnKeyDown;
  if Assigned(KeyDownEvent) then KeyDownEvent(Self, Key, Shift);

  if not Datalink.Active or not CanGridAcceptKey(Key, Shift) then Exit;

  with Datalink.DataSet do
    if ssCtrl in Shift then begin
      case Key of
        {$ifndef LINUX} vk_LEFT {$else}key_left{$endif}:
          if FixedCols > 0 then begin
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
            Exit;
          end;
      end
    end
    else begin
      case Key of
        {$ifndef LINUX} vk_LEFT {$else}key_left{$endif}:
          if (FixedCols > 0) and not (dgRowSelect in Options) then begin
            if SelectedIndex <= CalcLeftColumn - IndicatorOffset then
              Exit;
          end;
        {$ifndef LINUX} vk_HOME {$else}key_home{$endif}:
          if (FixedCols > 0) and (ColCount <> IndicatorOffset + 1) and
            not (dgRowSelect in Options) then
          begin
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
            Exit;
          end;
      end;
    end;
  OnKeyDown := nil;
  try
    inherited KeyDown(Key, Shift);
  finally

    OnKeyDown := KeyDownEvent;
  end;
end;


procedure TbvDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

var
  Cell: TGridCoord;
  TitleOffset:integer;
begin
   if (Owner is TCustomForm)
      and
      not ((Owner as TcustomForm).activeControl=Self)
   then
      (Owner as TcustomForm).activeControl:=Self;


   CloseHINT;

   Cell := MouseCoord(X, Y);
   if dgTitles in options then TitleOffSet:=1 else TitleOffset:=0;

    if (Cell.X < FixedCols + IndicatorOffset) and Datalink.Active then begin
      if (dgIndicator in Options) then
        inherited MouseDown(Button, Shift, 1, Y)
      else if Cell.Y >= TitleOffset then
        if Cell.Y - Row <> 0 then Datalink.Dataset.MoveBy(Cell.Y - Row);
    end
    else inherited MouseDown(Button, Shift, X, Y);
end;

procedure tbvdbGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  FrameOffs: Byte;

  function RowIsMultiSelected: Boolean;
  var
    Index: Integer;
  begin
    Result := (dgMultiSelect in Options) and Datalink.Active and
      SElectedROWS.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  end;

  procedure DrawTitleCell(ACol, ARow: Integer; Column: TColumn; var AState: TGridDrawState);
  const
    {$ifndef LINUX}
    ScrollArrows: array [Boolean, Boolean] of Integer =
      ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
    {$else}
    ScrollArrows: array [Boolean] of ArrowType =
      (ArrowType_RightArrow, ArrowType_LeftArrow);
    {$endif}

  var
    MasterCol: TColumn;
    TitleRect, TextRect, ButtonRect: TRect;
    I,k: Integer;
    {$ifndef LINUX}
    InBiDiMode: Boolean;
    {$endif}

    RECT1:TRECT;

    {$ifdef LINUX}
    Style: QStyleH;
    cg: QColorGroupH;
    {$endif}

  begin
    MasterCol:=nil;

    if (ACol<FixedCols) then
    begin
       TitleRect := CellRect(0,0);// CalcTitleRect(Columns[acol], ARow, MasterCol);



       if (ACol=0) and not (dgIndicator in Options)
          and Columns[0].Visible
       then begin
         MasterCol:=Columns[0];
       end;

       for i:=1 to ACol+Indicatoroffset do
       begin
            if dgIndicator in Options then k:=i-1 else k:=i;
            if Columns[k].Visible then begin
              TitleRect.Left:=TitleRect.Right;
              if dgColLines in Options then TitleRect.Left:=TitleRect.Left+1;
              TitleRect.right:=TitleRect.Left+Columns[k].Width;
              MasterCol:=Columns[k];
            end;
       end;
    end
    else
      TitleRect := CalcTitleRect(Column, ARow, MasterCol);

    //Canvas.Font := MasterCol.Title.Font;

    Canvas.FillRect(ARect);
    if MasterCol = nil then
    begin
      Exit;
    end;


    TextRect := TitleRect;
    I :=
       {$ifndef LINUX}
           GetSystemMetrics(SM_CXHSCROLL);
       {$else}
           CXHSCROLL;
       {$endif}
    if ((TextRect.Right - TextRect.Left) > I) and MasterCol.Expandable then
    begin
      Dec(TextRect.Right, I);
      ButtonRect := TitleRect;
      ButtonRect.Left := TextRect.Right;

      {$ifndef LINUX}
      I := SaveDC(Canvas.Handle);
      try
      {$endif}
        Canvas.FillRect(ButtonRect);
        {$ifndef LINUX}
          { DrawFrameControl doesn't draw properly when orienatation has changed.
            It draws as ExtTextOut does. }
          InflateRect(ButtonRect, -1, -1);
          IntersectClipRect(Canvas.Handle, ButtonRect.Left,
            ButtonRect.Top, ButtonRect.Right, ButtonRect.Bottom);
          InflateRect(ButtonRect, 1, 1);
          InBiDiMode := Canvas.CanvasOrientation = coRightToLeft;
          if InBiDiMode then { stretch the arrows box }
            Inc(ButtonRect.Right, GetSystemMetrics(SM_CXHSCROLL) + 4);
          DrawFrameControl(Canvas.Handle, ButtonRect, DFC_SCROLL,
            ScrollArrows[InBiDiMode, MasterCol.Expanded] or DFCS_FLAT);
        {$else}
          InflateRect(ButtonRect, -1, -1);
          Style := QApplication_style;
          if Enabled then
            cg := Palette.ColorGroup(cgActive)
          else
            cg := Palette.ColorGroup(cgDisabled);
          with ButtonRect do
          QStyle_drawArrow(Style, Canvas.Handle, ScrollArrows[MasterCol.Expanded], false,
            Left, Top, Right - Left, Bottom - Top, cg, True, nil);

          InflateRect(ButtonRect, 1, 1);
        {$endif}

      {$ifndef LINUX}
      finally
        RestoreDC(Canvas.Handle, I);
      end;
      {$endif}
    end;

    RECT1:=NeededRECT(
             masterCol.Title.Caption,
             TextRECT.right-textRect.left-4,
             Mastercol.title.Alignment,
             UseRightToLeftAlignmentForField(MasterCol.Field, Mastercol.title.Alignment)
             );


    if Rect1.Bottom<(TextRect.bottom-TextrEct.top)
    then inflaterect(textrect,0,-(TextRect.bottom-TextrEct.top-REct1.bottom) div 2);

    bvWriteText(canvas,  TEXTRECT,
       mastercol.title.Caption,
       Mastercol.title.Alignment,
       UseRightToLeftAlignmentForField(MasterCol.Field, Mastercol.title.Alignment));
  end;

var
  OldActive: Integer;
  Indicator: Integer;
  Highlight: Boolean;
  Value: string;
  DrawColumn: TColumn;
  MultiSelected: Boolean;
  ALeft: Integer;

  {$ifndef LINUX}
  Type3d:integer;
  {$else}
  Type3dInner,Type3dOuter:tEdgeStyle;
  {$endif}


  NewBackground:TColor;
begin
  //inherited;


  if csLoading in ComponentState then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Exit;
  end;

  Dec(ARow, FixedROWS);
  Dec(ACol, IndicatorOffset);

  if  (gdFixed in AState)
  then begin
     Canvas.Brush.Color:=FixedColor;
  end;

  if (gdFixed in AState) {and ([dgRowLines, dgColLines] * Options =
    [dgRowLines, dgColLines])} then
  begin
    InflateRect(ARect, -1, -1);
    FrameOffs := 1;
  end
  else
    FrameOffs := 2;

  if (gdFixed in AState) and (ACol < 0) then
  begin
    Canvas.Brush.Color := FixedColor;

    Canvas.FillRect(ARect);
    if Assigned(DataLink) and DataLink.Active  then
    begin
      MultiSelected := False;
      if ARow >= 0 then
      begin
        OldActive := DataLink.ActiveRecord;
        try
          Datalink.ActiveRecord := ARow;
          MultiSelected := RowIsMultiselected;
        finally
          Datalink.ActiveRecord := OldActive;
        end;
      end;
      if (ARow = DataLink.ActiveRecord) or MultiSelected then
      begin
        Indicator := 0;
        if DataLink.DataSet <> nil then
          case DataLink.DataSet.State of
            dsEdit: Indicator := 1;
            dsInsert: Indicator := 2;
            dsBrowse:
              if MultiSelected then
                if (ARow <> Datalink.ActiveRecord) then
                  Indicator := 3
                else
                  Indicator := 4;  // multiselected and current row
          end;
        FIndicators.BkColor := FixedColor;
        ALeft := ARect.Right - FIndicators.Width - FrameOffs;
        {$ifndef LINUX}
          if Canvas.CanvasOrientation = coRightToLeft then Inc(ALeft);
          FIndicators.Draw( Canvas, ALeft,
            (ARect.Top + ARect.Bottom - FIndicators.Height) shr 1, Indicator, True);
        {$else}
          FIndicators.Draw(Canvas, ALeft,
            (ARect.Top + ARect.Bottom - FIndicators.Height) shr 1, Indicator, itImage, True);
        {$endif} // LINUX ???


      end;
    end;
  end
  else with Canvas do
  begin
    DrawColumn := Columns[ACol];
    if not DrawColumn.Showing then Exit;


    if not (gdFixed in AState) then
    begin
      Brush.Color := DrawColumn.Color;
    end;

    if ARow < 0 then begin

      if cvtitlecolor in DrawColumn.assignedValues
      then   Canvas.Brush.Color := Drawcolumn.Title.Color;

      if cvTitleFont in DrawColumn.assignedValues then Font := DrawColumn.title.Font
      else Font:=self.titleFont;

      DrawTitleCell(ACol, ARow + FixedROWS, DrawColumn, AState);
    end
    else if (DataLink = nil) or not DataLink.Active then
      FillRect(ARect)
    else
    begin

      if cvFont in DrawColumn.assignedValues then Font := DrawColumn.Font
      else Font:=self.font;

      Value := '';
      OldActive := DataLink.ActiveRecord;
      try
        DataLink.ActiveRecord := ARow;
        if Assigned(DrawColumn.Field) then
          Value := DrawColumn.Field.DisplayText;
        Highlight := HighlightCell(ACol, ARow, Value, AState);
        if Highlight then
        begin
          Brush.Color := clHighlight;
          Font.Color := clHighlightText;
        end
        else
        if
           (FStrippedRows>=2)
           and not (gdFixed in AState)
           and (brush.color=Color)
           and assigned(DrawColumn.field)
           and assigned(DrawColumn.field.dataset)
           and (DrawColumn.Field.DataSet.RecNo mod FStrippedRows =0)
        then begin
          Brush.Color:= FStrippedColor;
        end;

        if not Enabled then
          Font.Color := clGrayText;

        if Assigned(FOnGetCellParams) and not (gdFixed in AState)
        then begin
          NewBackGround:=Brush.Color;
          FOnGetCellParams(Self, DrawColumn.Field, Font, NewbAckGround, Highlight);
          Brush.Color:=NewBackGround;
        end;

        if DefaultDrawing then
          bvWriteText(canvas,ARect,
                      Value, DrawColumn.Alignment,
                      UseRightToLeftAlignmentForField(DrawColumn.Field, DrawColumn.Alignment));
        if Columns.State = csDefault then
          DrawDataCell(ARect, DrawColumn.Field, AState);
        DrawColumnCell(ARect, ACol, DrawColumn, AState);
      finally
        DataLink.ActiveRecord := OldActive;
      end;
      if DefaultDrawing and (gdSelected in AState)
        and ((dgAlwaysShowSelection in Options) or Focused)
        and not (csDesigning in ComponentState)
        and not (dgRowSelect in Options)
        and (UpdateLock = 0)
        and (ValidParentForm(Self).ActiveControl = Self) then
         //Windows.DrawFocusRect(Handle, ARect);
        DrawFocusRect(ARect);
      //selectedindex
    end;
  end;
  if (gdFixed in AState) {and ([dgRowLines, dgColLines] * Options =
    [dgRowLines, dgColLines])} then
  begin
    InflateRect(ARect, 1, 1);
    {$ifndef LINUX}
      DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT);
    {$else}
      QGraphics.DrawEdge(Canvas, ARect, esNone,esRaised,[ebTop,ebLeft]);

      {if [dgRowLines] * Options =[dgRowLines]
      then
      QGraphics.DrawEdge(Canvas, ARect, esRaised,esNone,[ebBottom])
      else}
      QGraphics.DrawEdge(Canvas, ARect, esNone,esraised,[ebBottom]);

      {if [dgColLines] * Options <>[dgColLines]
      then}
      QGraphics.DrawEdge(Canvas, ARect, esNone,esRaised,[ebRight]);
      //else QGraphics.DrawEdge(Canvas, ARect, esNone,esLow,[ebRight]);
    {$endif}
    //DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
  end
  else
  if  not (gdSelected in astate)
  then begin
    if (Lines3dV<>c3dNONE)
    then begin

        {$ifndef LINUX}
          if Lines3dV<>c3dOUTER then type3d:=BDR_RAISEDINNER
          else Type3d:=BDR_SUNKENOUTER;
          DrawEdge(Canvas.Handle, ARect, TYPE3D, BF_Left);
        {$else}
          if Lines3dV<>c3dOUTER then begin
                     type3dInner:=esNone; //1 esRaised;
                     type3dOuter:=esRaised; //1 esNone;
          end
          else begin
             Type3dInner:=esNone;
             Type3dOuter:=esLowered; //esRaised;
          end;
          QGraphics.DrawEdge(Canvas, ARect, type3dInner,type3dOuter,[ebLeft]);
        {$endif}


        if  dgColLines in Options
        then begin
           InflateRect( ARect, 1,0 );
        end;



        {$ifndef LINUX}
          DrawEdge(Canvas.Handle, ARect, TYPE3D, BF_RIGHT);
        {$else}
          QGraphics.DrawEdge(Canvas, ARect, type3dInner,type3dOuter,[ebRight]);
        {$endif}

    end;
    if (Lines3dH<>c3dNONE)
    then begin

        {$ifndef LINUX}
          if Lines3dV<>c3dOUTER then type3d:=BDR_RAISEDINNER
          else Type3d:=BDR_SUNKENOUTER;
        {$else}
          if Lines3dV<>c3dOUTER then begin
                     type3dInner:=esNone; //esRaised;
                     type3dOuter:=esRaised;
          end
          else begin
             Type3dInner:=esNone;
             Type3dOuter:=esLowered;
          end;
        {$endif}

        if (Lines3dH=c3dInner)
           or  (ARow>0)
        then begin
          {$ifndef LINUX}
          DrawEdge(Canvas.Handle, ARect, TYPE3D, BF_Top);
          {$else}
          QGraphics.DrawEdge(Canvas, ARect, type3dInner,type3dOuter,[ebTop]);
          {$endif}
        end;

        if  dgRowLines in Options
        then
        begin
           InflateRect( ARect, 0,1 );
        end;
        {$ifndef LINUX}
          DrawEdge(Canvas.Handle, ARect, TYPE3D, BF_BOTTOM);
        {$else}
          QGraphics.DrawEdge(Canvas, ARect, type3dInner,type3dOuter,[ebBottom]);
        {$endif}
    end;
  end;


end;


procedure tbvdbGrid.bvSetSelectedIndex(Value: Integer);
begin
 if Value>=FixedCols // + IndicatorOffset
 then inherited SelectedIndex:=Value
 else inherited SelectedIndex:=FixedCols //+ IndicatorOffset;
end;

procedure tbvdbGrid.SetStrippedRows(Value: Integer);
begin
  if (Value<0) then FStrippedRows:=0
  else if  (Value=1) then FStrippedRows:=2
  else FStrippedRows:=Value;
  Repaint;
end;

procedure tbvdbGrid.SetStrippedColor(Value: TColor);
begin
  FStrippedColor:=Value;
  Repaint;
end;


procedure TbvdbGrid.checkTitleHeight;
var i:integer;
    ROWH:integer;
    ARECT:TRECT;
//    UFormat:integer;
begin
  if FCheckTitle and  not FIncheckTitle then
  try
    FInCheckTitle:=true;

    if dgTitles in OPTIONS then begin
       with CellRect(0,0) do begin
          ROWH:=Bottom-Top;
       end;
       for i:=0 to Columns.Count-1 do
       with Columns[i] do if visible then
       begin
          //ARECT:=RECT( 0,0,Columns[i].width,ROWH);
          //inflaterect(arect,-1,-1);
          if cvTitleFont in assignedvalues
          then  canvas.Font:=Title.font
          else  canvas.Font:=self.TitleFont;

          ARECT:=NeededRECT(
             Title.caption,
             width-4,
             title.Alignment,
             UseRightToLeftAlignmentForField(Field, title.Alignment)
             );


          if ROWH<ARECT.Bottom+2 then ROWH:=ARECT.BOttom+2;
          if ROWH<FTitleMinHeight*GetSTDdefaultRowHeight div 100
          then ROWH:=FTitleMinHeight*GetSTDdefaultRowHeight div 100;
       end;
       if rowheights[0]<>ROWH then begin
          rowheights[0]:=ROWH;
          {$ifndef LINUX}
          perform(wm_size,0,0);
          {$endif}
       end
    end
    else begin
       if rowheights[0]<>DefaultRowHeight then begin
         rowheights[0]:=DefaultRowHeight;
         {$ifndef LINUX}
         perform(WM_sIzE,0,0);
         {$endif}
       end
    end;
  finally
    FInCheckTitle:=false;
  end;
end;





procedure tbvDBGrid.WMSCROLLMOUSE(var Message: TMESSAGE);
begin
 inherited;
 CloseHINT;
 if assigned(datasource) and assigned(datasource.dataset)
    and datasource.dataset.active
 then begin
   {$ifndef LINUX}
   if ((Message.wparamhi div $100) >=$F0)
   then begin
      datasource.dataset.next;
   end
   else { if (((Message.wparamHI)  in [$78,$F0])
             or
            (Message.wparamhi = $1E0)
           )
   then} begin
      datasource.dataset.Prior;
   end;
   {$endif}
   refresh;
 end;
end;

procedure tbvdbGrid.SetTitleMinHeight(Value:integer);
begin
  if Value<1 then FTitleMinHeight:=1
  else FTitleMinHeight:=Value;
  CheckTitleHeight;
end;


function TbvDBGrid.NeededRECT(aText:string;aWidth:integer;
                        Alignment: TAlignment; ARightToLeft: Boolean):TRECT;
var
    //OldFont:TFont;
    uFormat:integer;
begin
   Result.Left:=0;
   Result.Top:=0;
   Result.right:=aWidth;
   Result.Bottom:=0;

   //OldFont:=tfont.Create;
   //OldFont:=canvas.font;
   try
     //OldFont.assign(canvas.font);
     //if aFont<>nil
     //then Canvas.Font:=aFont;

     //UFormat:=DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX  or AlignFlags[Alignment] or  RTL[ARightToLeft];

     UFormat:=
       {$ifndef LINUX}
       DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX
       {$else}
       integer(AlignmentFlags_WORDBREAK)
       {$endif}
       or AlignFlags[Alignment] or  RTL[ARightToLeft]
       ;




     {$ifndef LINUX}
     drawtext( canvas.handle,pchar(aText),
               length(aText),Result,UFormat);
     {$else}
     Canvas.TextExtent(aText, Result,UFormat);
     {$endif}

   finally
     //if aFont<>nil then  Canvas.font:=OldFont;
     //OldFont.Free;
   end;

end;

procedure TbvDBGrid.SetLines3dV(Value:Lines3dType);
begin
   FLines3dV:= Value;
   //(Self as TCustomGrid).PaintLines:= Value=c3dNONE;
   if Visible then refresh;
end;

procedure TbvDBGrid.SetLines3dH(Value:Lines3dType);
begin
   FLines3dH:= Value;
   //(Self as TCustomGrid).PaintLines:= Value=c3dNONE;
   if Visible then refresh;
end;

procedure TbvDBGrid.SetLinesV(Value:boolean);
begin
   if value
   then Options:=Options+[dgColLines]
   else Options:=Options-[dgColLines];

   if Visible then refresh;
end;

procedure TbvDBGrid.SetLinesH(Value:boolean);
begin
   if value
   then Options:=Options+[dgROWLines]
   else Options:=Options-[dgROWLines];

   if Visible then refresh;
end;

function TbvDBGrid.GetLinesH:boolean;
begin
   REsult:=dgROWLines in options;
end;

function TbvDBGrid.GetLinesV:boolean;
begin
   REsult:=dgCOLLines in options;
end;

procedure TbvDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
const OldROW:integer=0;
      OLDCOL:integer=0;
      //oldx:integer=0;
      //oldY:integer=0;
var
      CELL:TGridCoord;
      CRECT,Drect:TRECT;
      HintColumn:tColumn;
      OldActive: Integer;
      Value:string;
//      CurPoint:TPoint;
      HW,HH:integer;
      OkDRAW:boolean;
begin
  inherited;
  if FHintCells
     and assigned(datasource) and assigned(datasource.dataset)
     and datasource.dataset.active
  then
  try

    Cell:=mousecoord(x,y);
    if (Cell.x>-1) and (Cell.y>-1)
    then begin
         if (Cell.X<>OldCol)
            or (Cell.Y<>OldROW)
         then begin
            CRECT:=cellRECT(Cell.x,Cell.y);

            CloseHINT;

            if not ((dgTitles in Options) and (CELL.y=0))
               and not ((dgIndicator in Options) and (CELL.X=0))
            then begin

              if (dgIndicator in Options) then HintColumn:=Columns[CELL.X-1]
              else HintColumn:=Columns[CELL.X];

              Value := '';

              if assigned(HintColumn)
              then begin
                OldActive := DataLink.ActiveRecord;
                try
                  if dgTitles in Options then DataLink.ActiveRecord := CELL.Y-1
                  else DataLink.ActiveRecord := CELL.Y;
                  if Assigned(HintColumn.Field) then
                    Value := HintColumn.Field.DisplayText;
                finally
                  DataLink.ActiveRecord := OldActive;
                end;
                if value>'' then begin
                  canvas.Font:=HINTCOLUMN.Font;
                  HW:=canvas.textwidth(Value);
                  HH:=canvas.textheight('Hg');
                  if (hw>crect.Right-crect.left-5) or (HH>Crect.bottom-crect.top)
                  then begin

                     if  (hh*2>Crect.bottom-crect.top)
                     then OkDRAW:=true
                     else begin
                       drect:=crect;
                       inc(DRECT.top,2);
                       inc(drect.left,2);
                       dec(drect.right,3);

                       {$ifndef LINUX}
                       DrawText(canvas.Handle, PChar(Value), Length(Value), dRECT,
                               DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX or dt_CALCRECT);
                       {$else}
                       Canvas.TextExtent(Value, dRect,
                           integer(AlignmentFlags_WORDBREAK) or
                           integer(AlignmentFlags_EXPANDTABS)
                           //DT_NOPREFIX or dt_CALCRECT
                           );
                       {$endif}


                       OkDraw:= drect.Bottom-drect.top >CRECT.Bottom-crect.top-2

                     end;
                     //GetCursorPos(CurPoint);
                     //FCellHint.Hint:=value; //}  inttostr(Cell.x)+' : '+inttostr(Cell.y)+' ( '+inttostr(cRECT.left)+' : '+inttostr(cRECT.top)+' ) - '+inttostr(clienttoscreen(point( CRECT.Left,cRECT.Top)).x)+' : '+inttostr(clienttoscreen(point( CRECT.Left,cRECT.Top)).y)+' mouse: '+inttostr(CurPoint.x)+' : '+inttostr(CurPoint.y);

                     if okDraw then begin
                       CloseHINT;
                       {$ifndef LINUX}
                       FCellHINT.Canvas.Font:= self.Font;
                       {$endif}

                       if hintcolumn.Alignment=taRightJustify
                       then
                       with clienttoscreen(point( CRECT.right,cRECT.Top)) do begin
                         FCellHint.ActivateHint(rect( x-
                                   {$ifndef LINUX}
                                   FCellHINT.Canvas.textwidth(VAlue)
                                   {$else}
                                   Canvas.textwidth(VAlue)
                                   {$endif}
                                   -6,y-1, x+1,y+
                                   {$ifndef LINUX}
                                   FCellHINT.Canvas.textHeight('Hg')
                                   {$else}
                                   Canvas.textHeight('Hg')
                                   {$endif}
                                   +4-1),value);
                       end
                       else
                       with clienttoscreen(point( CRECT.Left,cRECT.Top)) do begin
                         FCellHint.ActivateHint(rect( x-1,y-1,x+
                            {$ifndef LINUX}
                            FCellHINT.Canvas.textwidth(VAlue)
                            {$else}
                            Canvas.textwidth(VAlue)
                            {$endif}

                            +11,y+
                            {$ifndef LINUX}
                            FCellHINT.Canvas.textHeight('Hg')
                            {$else}
                            Canvas.textHeight('Hg')
                            {$endif}
                            +4-1),value);
                       end;
                       with TTimer.create(self) do begin
                          OnTimer:=self.OnHintTimer;
                          Interval:=5000;
                          Enabled:=true;
                          //SetTimer( handle,777,5000,nil);
                       end
                     end;
                  end;
                end;
              end
            end;


            OldCOL:=Cell.X;
            OldROW:=Cell.Y;
         end
    end;
  except
  end;
end;

procedure tbvdbGrid.onHintTimer(Sender:TObject); //WMTimer(var Msg: TWMTimer);
begin
  CloseHint;
  Sender.free;
  {
  inherited;

  if msg.TimerID=777
  then begin
     CloseHINT;
  end;}
end;

procedure tbvdbGrid.CloseHINT;
begin
     FCellHint.releasehandle;
     //KillTimer(handle,777);
end;

procedure TbvDBGrid.SetCellHeights(Value:integer);
var H:integer;
begin
  if Value<1 then Value:=1;
  FCellHeights:=Value;
  if not (csLoading in componentstate) then begin
    H:=FCellHeights*GetSTDdefaultRowHeight div 100;
    if DefaultROWHeight<>H then DefaultRowHeight:=H;
  end;
end;

function TbvDBGrid.GetCellHeights:integer;
begin
  REsult:=FCellHeights;
end;

function TbvdbGrid.GetSTDdefaultRowHeight:integer;
var CanvFont:TFont;
begin
   CanvFont:=Canvas.font;
   try
     Canvas.Font := Font;
     REsult := Canvas.TextHeight('Wg') + 3;
     if dgRowLines in Options then
       Inc(REsult, GridLineWidth);
   finally
     Canvas.font:=CanvFont;
   end;
end;

procedure TbvDBGrid.RowHeightsChanged;
begin
  CellHeights:=cellHeights;
  CheckTitleHeight;
  inherited
end;

procedure TbvdbGrid.SetCheckTitle(Value:boolean);
begin
  FCheckTitle:=Value;
  if Value then checktitleheight;
end;


function TbvdbGrid.SElectedCoord:TRect;
begin
  Result:=CellRect(Col,Row);
end;

{$ifndef LINUX}
procedure TbvDBGrid.CMMouseLeave(var Message: TMessage);
{$else}
procedure TbvDBGrid.MouseLeave(AControl:TControl);
{$endif}
begin
  inherited;
  CloseHint;
end;


procedure tbvdbGrid.PrintClick(Sender:tobject);
begin
  {with TDataSetPrint.Create(Self) do
  try
     grid:=Self;
     Preview;
  finally
     free;
  end;
  }
  //ShowMessage('In work!');
end;

procedure TbvdbGrid.SetSaverCannotAddColumns(value:boolean);
begin
  if assigned(Saver) then saver.cannotAddColumns:=value;
end;

function TbvdbGrid.getSaverCannotAddColumns:boolean;
begin
  Result := assigned(Saver) and saver.cannotaddcolumns;
end;


var
  Bmp: TBitmap;    

initialization
  DrawBitmap:=TBitmap.create;


  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(HInstance, bmArrow);
    FIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmEdit);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmInsert);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmMultiDot);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmMultiArrow);
    FIndicators.AddMasked(Bmp, clWhite);
  finally
    Bmp.Free;
  end;



finalization
  //QuickPrintEnabled:=true;
  DrawBitmap.free;

  if assigned(FIndicators) then FIndicators.free;
end.
