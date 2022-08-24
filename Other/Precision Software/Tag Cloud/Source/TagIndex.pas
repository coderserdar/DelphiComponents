{------------------------------------------------------------------------------
  TagIndex.pas

  TTagIndex control, a part of "TagCloud for VCL" package

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    An Index control for Delphi developers,
              implemented as a descendant of TCustomTagCloud component

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. The complete source code remains property of the author
  and may not be distributed, published, given or sold in any form as such.
  No parts of the source code can be included in any other component
  or application without written authorization of the author.

  Copyright (c) 2008-2014  Precision software & consulting
  All rights reserved

------------------------------------------------------------------------------}

{ Change log:

  Version 2.1 (2014-09-29)
  - added: Delphi XE6/XE7 support

  Version 2.0 (2013-11-16)
  - added: Delphi XE4/XE5 support

  Version 1.9.5 (2013-01-01)
  - added: Delphi XE3 support
  - and other minor improvements

  Version 1.9 (13.7.2012)
  - improved: Missing default values have been assigned to properties

  Version 1.5 (16.3.2011)
  - The first release of TTagIndex component
}

{ This unit contains TTagIndex component declaration and implementation. }
unit TagIndex;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, TagCloud;

type

  { A helper class for holding some specific data about the index labels and page number tabs (or buttons).
    The data are used for drawing the labels and page numbers. }
  TTIGuideMetrics = class
  private
    FSize:TSize;
    FRect: TRect;
    FRectFrame: TRect;
    // Indicates the existance of items, that match the FCustomFilter
    FHasData: Boolean;
    // Custom filter for labels
    FCustomFilter: string;
    // This label serves as a separator between labels
    FIsSeparator: Boolean;
  public
    constructor Create;
  end;

  // Defines the style of page number tabs.
  TTIPageNumberStyle = (pnsTabs,pnsButtons);
  // Defines the positioning of index labels inside TTagIndex control
  TTILabelsPosition = (tlpTop,tlpLeft,tlpRight);

  { Index control implementation. }
  TTagIndex = class(TCustomTagCloud)
  private
    FAutoCheckLabels: Boolean;
    FShowPageNumbers: Boolean;
    FLabelsRect: TRect;
    FPagesRect: TRect;
    FPageTabSize: TSize;
    FPaddingRect: TRect;
    FLabels: TStringList;
    FPages: TStringList;
    FCustomLabels: TStrings;
    FSelLabelIndex: Integer;
    FHoverLabelIndex: Integer;
    FHoverPageIndex: Integer;
    FPageNumberStyle: TTIPageNumberStyle;
    FLabelsPosition: TTILabelsPosition;
    FLblCursor: TCursor;

    FLabelFont:TFont;
    FSelLabelColor: TColor;
    FSelLabelStyle: TFontStyles;
    FLabelFrame: TTagItemFrame;
    FSelLabelFrame: TTagItemFrame;
    FOnFilterLabel: TTagFilterEvent;
    procedure ClearLabels;
    procedure ClearPages;
    procedure SetAutoCheckLabels(Value: Boolean);
    procedure SetShowPageNumbers(Value: Boolean);
    procedure SetCustomLabels(Value: TStrings);
    procedure SetLabelFont(Value:TFont);
    procedure SetSelLabelStyle(const Value: TFontStyles);
    function GetSelectedLabel:string;
    procedure SetSelectedLabel(const Value:string);
    procedure SetLabelFrame(Value: TTagItemFrame);
    procedure SetSelLabelColor(const Value: TColor);
    procedure SetSelLabelFrame(Value: TTagItemFrame);
    procedure SetHoverLabelIndex(Value: Integer);
    procedure SetHoverPageIndex(Value: Integer);
    procedure SetPageNumberStyle(Value: TTIPageNumberStyle);
    procedure SetLabelsPosition(Value: TTILabelsPosition);
    // Returns the label index at specified position
    function GetLabelAt(X, Y:Integer):Integer;
    function GetPageTabAt(X, Y:Integer):Integer;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    { This method returns the drawing rectangle for tag cloud items. In TTagIndex it is modified to place the index items
      and page tabs into the control. }
    function GetItemsRect:TRect; override;
    { Overrides the standard OnFilter event handler of the ancestor (TCustomTagCloud), so the tag items are filtered by
      currently selected index label. See also OnFilterLabel event. }
    procedure DoFilterLabel(Sender:TObject; Item:TTagCloudItem; var Allowed:Boolean);
    // All the labels and page numbers drawing takes the place inside this method.
    procedure DoDrawItems(aCanvas:TCanvas; Rect: TRect); override;
    // This method is triggered on the changes in Items collection, so the labels can be automatically recreated.
    procedure DoItemsChanged(Sender: TObject);
    // Handles the LabelFont changes
    procedure LabelFontChange(Sender: TObject);
    // Handles the LabelFrame changes
    procedure LabelFrameChanged(Sender:TObject; Param: Integer);
    // Handles mouse movements, so the item hover event can be processed. Controls also the hovered labels and hovered page tabs.
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    // Handles mouse down events, so the item clicks can be processed. Controls also the clicks on labels and page tabs
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { This method is a core for creating the labels, that are displayed in TTagIndex control. You can define your own CustomLabels,
      or you can let the control to create the labels automatically. }
    procedure RebuildIndex(FullRebuild: Boolean = True); virtual;
    // Holds the index of currently hovered index label (label under the mouse)
    property HoverLabelIndex: Integer read FHoverLabelIndex write SetHoverLabelIndex;
    // Holds the index of currently hovered page number tab (or button)
    property HoverPageIndex: Integer read FHoverPageIndex write SetHoverPageIndex;
    // This property is hidden in TTagIndex, because we handle the Index as a multipage list. Maybe in the future we will find some utilization for that.
    property AutoSize;
  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
  published
    // Align in the parent control.
    property Align;
    { See @inherited }
    property Alignment default taCenter;
    // Component anchors
    property Anchors;
    { When set to True, the presence of items is checked for each label. If label has no data (no item is valid for label's filter),
      the label is drawn with disabled color. }
    property AutoCheckLabels: Boolean read FAutoCheckLabels write SetAutoCheckLabels default True;
    { See @inherited }
    property AutoScaleFont default False;
    // Bidirectional text drawing mode
    property BiDiMode;
    { See @inherited }
    property Color nodefault;
    { See @inherited }
    property ColSpacing;
    // Defines the component size constraints.
    property Constraints;
    { Custom index labels. Normally, the index labels are alphabetic characters from A to Z.
      You can override these labels (and filters) by filling this list.@br
      Syntax: Label[|Filter]@br
      Example: All items|*@br
      As a separator you can use the | (pipe) only. }
    property CustomLabels: TStrings read FCustomLabels write SetCustomLabels;
    { See @inherited }
    property Direction default tcdVertical;
    // Defines the drag cursor.
    property DragCursor;
    // Defines the drag kind for component
    property DragKind;
    // Defines the drag mode for component
    property DragMode;
    // Enabling or disabling the component. If the component is disabled, the items are painted with clGrayText color.
    property Enabled;
    { See @inherited }
    property FixedColCount;
    { See @inherited }
    property FixedColFullFrame;
    { See @inherited }
    property FixedColWidth default 100;
    { See @inherited }
    property Font;
    { See @inherited }
    property GlowSize;
    { See @inherited }
    property HoverColor;
    { See @inherited }
    property HoverCursor;
    { See @inherited }
    property HoverStyle;
    { See @inherited }
    property HoverEnlarge;
    { See @inherited }
    property HoverColSpace;
    { See @inherited }
    property ItemFrame;
    { See @inherited }
    property HoverFrame;
    { See @inherited }
    property SelectedFrame;
    { See @inherited }
    property Items;
    // Index labels font
    property LabelFont:TFont read FLabelFont Write SetLabelFont;
    // Defines the frame style for index labels
    property LabelFrame: TTagItemFrame read FLabelFrame write SetLabelFrame;
    // By setting this property you can set up your desired position of index labels (on top, at left, at right)
    property LabelsPosition: TTILabelsPosition read FLabelsPosition write SetLabelsPosition default tlpTop;
    {$IF CompilerVersion>=18}
    // Component margins
    property Margins;
    { See @inherited }
    property Padding;
    // OnMouseActivate event
    property OnMouseActivate;
    // OnMouseEnter event
    property OnMouseEnter;
    // OnMouseLeave event
    property OnMouseLeave;
    {$IFEND}
    { See @inherited }
    property SelectedColor;
    // Set or get the current index label with the help of this property
    property SelectedLabel: string read GetSelectedLabel write SetSelectedLabel;
    // Defines the color for selected labels
    property SelLabelColor: TColor read FSelLabelColor write SetSelLabelColor default {$IF CompilerVersion>=15} clDefault {$ELSE} clNone {$IFEND};
    // Defines the frame style for selected and hovered index labels
    property SelLabelFrame: TTagItemFrame read FSelLabelFrame write SetSelLabelFrame;
    // Defines the font style for selected index label
    property SelLabelStyle: TFontStyles read FSelLabelStyle write SetSelLabelStyle;
    // Shows or hides the page numbers at the bottom of the tag index control (see also PageNumberStyle)
    property ShowPageNumbers: Boolean read FShowPageNumbers write SetShowPageNumbers default True;
    { See @inherited }
    property Styler;
    { See @inherited }
    property PageIndex;
    // Controls the style of page numbers, that are displayed at bottom of the tag index control, if the tags do not fit.
    property PageNumberStyle: TTIPageNumberStyle read FPageNumberStyle write SetPageNumberStyle default pnsTabs;
    // Takes the same bidirectional text drawing mode as its parent control
    property ParentBiDiMode;
    { See @inherited }
    property ParentColor;
    { See @inherited }
    property ParentFont default True;
    // Takes the ShowHint property value from its parent control
    property ParentShowHint;
    // Popup menu for the component
    property PopupMenu;
    { See @inherited }
    property RowSpacing;
    // If True, the help hint will be shown for hovered tag items.
    property ShowHint;
    { See @inherited }
    property Transparent;
    { See @inherited }
    property VerticalAlignment default taAlignTop;
    // Visibility of the control
    property Visible;
    // OnClick event
    property OnClick;
    // OnContextPopup event
    property OnContextPopup;
    // OnDblClick event
    property OnDblClick;
    // OnDragDrop event
    property OnDragDrop;
    // OnDragOver event
    property OnDragOver;
    // OnEndDock event
    property OnEndDock;
    // OnEndDrag event
    property OnEndDrag;
    { This event allows you to customize the filtering of tag items by current label.
     You can use Filter or SelectedLabel properties to obtain the current filter (resp. selected label) }
    property OnFilterLabel: TTagFilterEvent read FOnFilterLabel write FOnFilterLabel;
    // OnMouseDown event
    property OnMouseDown;
    // OnMouseMove event
    property OnMouseMove;
    // OnMouseUp event
    property OnMouseUp;
    // OnStartDock event
    property OnStartDock;
    // OnStartDrag event
    property OnStartDrag;
    { See @inherited }
    property OnAfterPaint;
    { See @inherited }
    property OnBeforePaint;
    { See @inherited }
    property OnCompareItems;
    { See @inherited }
    property OnCustomDrawItem;
    { See @inherited }
    property OnAdvancedCustomDrawItem;
    { See @inherited }
    property OnHoverChange;
    // OnCanResize event
    property OnCanResize;
    // Resize event
    property OnResize;
    { See @inherited }
    property OnShow;
    { See @inherited }
    property OnTagClick;
    { See @inherited }
    property OnTagDblClick;
    { See @inherited }
    property OnTagColor;
    { See @inherited }
    property OnTagHint;
    { See @inherited }
    property OnTagPositioning;
  end;

implementation

uses
  {$IF CompilerVersion >= 24} System.Types, System.UITypes, {$IFEND}
  SysUtils;

constructor TTIGuideMetrics.Create;
begin
  inherited;
  FSize.cx:=0;
  FSize.cy:=0;
  SetRectEmpty(FRect);
  SetRectEmpty(FRectFrame);
  FHasData := False;
  FCustomFilter := '';
  FIsSeparator := False;
end;

constructor TTagIndex.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetRectEmpty(FPaddingRect);
  SetRectEmpty(FLabelsRect);
  SetRectEmpty(FPagesRect);
  FLabelFrame := TTagItemFrame.Create;
  FLabelFrame.OnChange := LabelFrameChanged;
  FSelLabelFrame := TTagItemFrame.Create;
  FSelLabelFrame.OnChange := LabelFrameChanged;
  FLabels := TStringList.Create;
  FPages := TStringList.Create;
  FLabelFont:=TFont.Create;
  FLabelFont.OnChange:=LabelFontChange;
  FLabelFont.Assign(Font);
  FSelLabelStyle := FLabelFont.Style+[fsBold];
  FCustomLabels := TStringList.Create;
  FSelLabelIndex := -1;
  FHoverLabelIndex := -1;
  FHoverPageIndex := -1;
  FPageNumberStyle := pnsTabs;
  FLabelsPosition := tlpTop;

  TStringList(FCustomLabels).OnChange:=LabelFontChange;
  {$IF CompilerVersion>=15}
  FSelLabelColor := clDefault;
  {$ELSE}
  FSelLabelColor := clNone;
  {$IFEND}
  FAutoCheckLabels := True;
  FShowPageNumbers := True;
  FLblCursor := Cursor;
  FPageTabSize.cx := 0;
  FPageTabSize.cy := 0;

  OnFilter := DoFilterLabel;
  if UseRightToLeftAlignment then
    Alignment := taRightJustify
  else
    Alignment := taLeftJustify;
  AutoSize := False;
  AutoShrinkRows := True;
  FixedColFullFrame := False;
  MaxFontSize := Font.Size;
  ShrinkDiacritic := False;
  FixedColCount := 0;
  FixedColWidth := 100;
  Direction := tcdVertical;
  Sorted := True;
  OnItemsChanged := DoItemsChanged;
end;

destructor TTagIndex.Destroy;
begin
  OnItemsChanged := nil;
  ClearLabels;
  ClearPages;
  FLabels.Free;
  FPages.Free;
  FLabelFont.Free;
  FCustomLabels.Free;
  FLabelFrame.Free;
  FSelLabelFrame.Free;
  inherited;
end;

procedure TTagIndex.ClearLabels;
var
  i:Integer;
begin
  for i:=0 To FLabels.Count-1 do
    TTIGuideMetrics(FLabels.Objects[i]).Free;
  FLabels.Clear;
end;

procedure TTagIndex.ClearPages;
var
  i:Integer;
begin
  for i:=0 To FPages.Count-1 do
    TTIGuideMetrics(FPages.Objects[i]).Free;
  FPages.Clear;
end;

procedure TTagIndex.SetAutoCheckLabels(Value: Boolean);
var
  i:Integer;
begin
  if FAutoCheckLabels<>Value then
  begin
    FAutoCheckLabels:=Value;
    if not FAutoCheckLabels then
    begin
      for i:=0 To FLabels.Count-1 do
        TTIGuideMetrics(FLabels.Objects[i]).FHasData:=True;
    end
    else
      RebuildIndex(False);
    CheckInvalidateRect(FLabelsRect);
  end;
end;

procedure TTagIndex.SetCustomLabels(Value: TStrings);
begin
  FCustomLabels.Assign(Value);
end;

procedure TTagIndex.SetLabelFont(Value:TFont);
begin
  FLabelFont.Assign(Value);
end;

procedure TTagIndex.SetSelLabelColor(const Value: TColor);
begin
  if FSelLabelColor<>Value then
  begin
    FSelLabelColor:=Value;
    if FSelLabelIndex>=0 then
    begin
      if (Transparent or (GlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered then
        Invalidate
      else
      if (not (csDestroying in ComponentState)) and Assigned(Parent) then
        CheckInvalidateRect(TTIGuideMetrics(FLabels.Objects[FSelLabelIndex]).FRectFrame);
    end;
  end;
end;

procedure TTagIndex.SetSelLabelStyle(const Value: TFontStyles);
begin
  if FSelLabelStyle<>Value then
  begin
    FSelLabelStyle:=Value;
    if ((fsBold in FSelLabelStyle) and (not (fsBold in FLabelFont.Style))) or
      ((fsBold in FLabelFont.Style) and (not (fsBold in FSelLabelStyle))) then
    begin
      RebuildIndex(True);
      Rebuild;
    end
    else
      CheckInvalidateRect(FLabelsRect);
  end;
end;

procedure TTagIndex.SetLabelFrame(Value: TTagItemFrame);
begin
  FLabelFrame.Assign(Value);
  Rebuild;
end;

procedure TTagIndex.SetSelLabelFrame(Value: TTagItemFrame);
begin
  FSelLabelFrame.Assign(Value);
  Rebuild;
end;

procedure TTagIndex.LabelFontChange(Sender: TObject);
begin
  RebuildIndex(True);
  Rebuild;
end;

procedure TTagIndex.LabelFrameChanged(Sender:TObject; Param: Integer);
begin
  Rebuild;
end;

function TTagIndex.GetSelectedLabel:string;
begin
  if (FSelLabelIndex>=0) and (FSelLabelIndex<FLabels.Count) then
    Result:=FLabels[FSelLabelIndex]
  else
    Result:=Filter;
end;

procedure TTagIndex.SetSelectedLabel(const Value:string);
var
  i:Integer;
  flt:string;
  GM:TTIGuideMetrics;
begin
  flt:='';
  FSelLabelIndex:=-1;
  if Length(Value)>0 then
  begin
    // check labels
    for i:=0 To FLabels.Count-1 do
      if SameText(Value,FLabels[i]) then
      begin
        GM:=TTIGuideMetrics(FLabels.Objects[i]);
        flt:=GM.FCustomFilter;
        FSelLabelIndex:=i;
        break;
      end;
    // check filters
    if FSelLabelIndex=-1 then
      for i:=0 To FLabels.Count-1 do
      begin
        GM:=TTIGuideMetrics(FLabels.Objects[i]);
        if SameText(Value,GM.FCustomFilter) then
        begin
          flt:=Value;
          FSelLabelIndex:=i;
          break;
        end;
      end;
  end;
  Filter:=flt;
end;

procedure TTagIndex.SetPageNumberStyle(Value: TTIPageNumberStyle);
begin
  if FPageNumberStyle<>Value then
  begin
    FPageNumberStyle:=Value;
    Invalidate;
  end;
end;

procedure TTagIndex.SetLabelsPosition(Value: TTILabelsPosition);
begin
  if FLabelsPosition<>Value then
  begin
    FLabelsPosition:=Value;
    Rebuild;
  end;
end;

procedure TTagIndex.SetShowPageNumbers(Value: Boolean);
begin
  if FShowPageNumbers<>Value then
  begin
    FShowPageNumbers:=Value;
    Rebuild;
  end;
end;

procedure TTagIndex.DoFilterLabel(Sender:TObject; Item:TTagCloudItem; var Allowed:Boolean);
begin
  if Assigned(FOnFilterLabel) then
    FOnFilterLabel(Sender, Item, Allowed)
  else
    Allowed:=SameText(Filter,Copy(Item.Caption,1,Length(Filter)));
end;

function TTagIndex.GetLabelAt(X, Y:Integer):Integer;
var
  i:Integer;
  p:TPoint;
  GM:TTIGuideMetrics;
begin
  Result:=-1;
  p.x:=x; p.y:=y;
  if PtInRect(FLabelsRect,p) then
    for i:=0 To FLabels.Count-1 do
    begin
      GM:=TTIGuideMetrics(FLabels.Objects[i]);
      if (not GM.FIsSeparator) and PtInRect(GM.FRectFrame,p) then
      begin
        Result:=i;
        break;
      end
    end;
end;

function TTagIndex.GetPageTabAt(X, Y:Integer):Integer;
var
  lbMg,w,i:Integer;
  p:TPoint;
  R:TRect;
begin
  Result:=-1;
  p.x:=x; p.y:=y;
  if (PageCount>1) and FShowPageNumbers and (PtInRect(FPagesRect,p)) then
  begin
    lbMg:=FLabelFrame.FrameMargin;
    if FSelLabelFrame.FrameMargin>lbMg then
      lbMg:=FSelLabelFrame.FrameMargin;
    if FPageNumberStyle=pnsTabs then  // tabs
    begin
      x:=FPagesRect.Left+FPaddingRect.Left;
      y:=FPagesRect.Top;
      for i:=1 To PageCount do
      begin
        w:=FPageTabSize.cx+(Length(IntToStr(i))-1)*(FPageTabSize.cx div 4)+lbMg*2;
        if x+w>=FPagesRect.Right-FPaddingRect.Right then
          break;
        SetRect(R,x,y,x+w,y+FPageTabSize.cy+lbMg+1);
        if PtInRect(R,p) then
        begin
          Result:=i-1;
          break;
        end;
        Inc(x,w);
      end;
    end
    else
    begin // buttons
      x:=FPagesRect.Left+FPaddingRect.Left;
      y:=FPagesRect.Top+FPaddingRect.Bottom;
      for i:=1 To PageCount do
      begin
        w:=FPageTabSize.cx+(Length(IntToStr(i))-1)*(FPageTabSize.cx div 4)+lbMg*2;
        if x+w>=FPagesRect.Right-FPaddingRect.Right then
          break;
        SetRect(R,x,y,x+w,y+FPageTabSize.cy+lbMg*2);
        if PtInRect(R,p) then
        begin
          Result:=i-1;
          break;
        end;
        Inc(x,w+ColSpacing);
      end;
    end;
  end;
end;

procedure TTagIndex.SetHoverLabelIndex(Value: Integer);
var
  R:TRect;
  GM:TTIGuideMetrics;
begin
  if FHoverLabelIndex<>Value then
  begin
    if (FHoverLabelIndex>=0) and (FHoverLabelIndex<FLabels.Count) then
      GM:=TTIGuideMetrics(FLabels.Objects[FHoverLabelIndex])
    else
      GM:=nil;
    if Assigned(GM) and (not (csDestroying in ComponentState))
      and (not ((Transparent or (GlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered)) then
    begin
      R:=GM.FRectFrame;
      FHoverLabelIndex:=Value;
      CheckInvalidateRect(R);
    end
    else
      FHoverLabelIndex:=Value;
    if (FHoverLabelIndex>=0) and (FHoverLabelIndex<FLabels.Count)
      and (not TTIGuideMetrics(FLabels.Objects[FHoverLabelIndex]).FIsSeparator) then // exclude the separators from hovering
      GM:=TTIGuideMetrics(FLabels.Objects[FHoverLabelIndex])
    else
    begin
      GM:=nil;
      FHoverLabelIndex:=-1;
    end;

    if Assigned(GM) then
    begin
      if Cursor<>HoverCursor then
        FLblCursor:=Cursor;
      Cursor:=HoverCursor;
    end
    else
      Cursor:=FLblCursor;
    if (Transparent or (GlowSize>0)) and Assigned(Parent) and Parent.DoubleBuffered then
      Invalidate
    else
    if Assigned(GM) and (not (csDestroying in ComponentState)) and Assigned(Parent) then
      CheckInvalidateRect(GM.FRectFrame);
  end;
end;

procedure TTagIndex.SetHoverPageIndex(Value: Integer);
begin
  if FHoverPageIndex<>Value then
  begin
    FHoverPageIndex:=Value;
    if FHoverPageIndex>=0 then
    begin
      if Cursor<>HoverCursor then
        FLblCursor:=Cursor;
      Cursor:=HoverCursor;
    end
    else
      Cursor:=FLblCursor;
  end;
end;

procedure TTagIndex.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  GM:TTIGuideMetrics;
begin
  inherited;
  if HoverItem=nil then
  begin
    if (FHoverLabelIndex>=0) and (FHoverLabelIndex<FLabels.Count) then
      GM:=TTIGuideMetrics(FLabels.Objects[FHoverLabelIndex])
    else
      GM:=nil;
    if (GM=nil) or (not PtInRect(GM.FRectFrame,Point(x,y))) then
      HoverLabelIndex := GetLabelAt(X,Y);
    if FHoverLabelIndex=-1 then
    begin
      if PtInRect(FPagesRect,point(x,y)) then
        HoverPageIndex:=GetPageTabAt(x,y)
      else
        HoverPageIndex:=-1
    end;
  end
end;

procedure TTagIndex.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button=mbLeft) and (HoverItem=nil) then
  begin
    if (FHoverLabelIndex>=0) and (FHoverLabelIndex<FLabels.Count) then
      SelectedLabel:=FLabels[FHoverLabelIndex]
    else
    if (FHoverPageIndex>=0) and (FHoverPageIndex<PageCount) then
      PageIndex:=FHoverPageIndex
  end;
end;

procedure TTagIndex.CMMouseLeave(var Msg: TMessage);
begin
  if FHoverLabelIndex>=0 then
    HoverLabelIndex:=-1
  else
  if FHoverPageIndex>=0 then
    HoverPageIndex:=-1;
  inherited;
end;

// This method is called just before the tag cloud internal rebuild, so we need only to assign FHasData fields
procedure TTagIndex.DoItemsChanged(Sender: TObject);
begin
  RebuildIndex(False);
end;

procedure TTagIndex.RebuildIndex(FullRebuild: Boolean = True);
var
  i,j,k:Integer;
  idx:string;
  lastFilter:string;
  GM:TTIGuideMetrics;
  S1:TSize;
begin
  if (Parent=nil) or (csLoading in ComponentState) then
    Exit; // parent is not assigned - no calculation performed

  if FullRebuild or (FLabels.Count=0) then
  begin
    lastFilter:=Filter;
    FSelLabelIndex:=-1;
    ClearLabels;
    if FCustomLabels.Count>0 then
    begin
      // custom labels handling ...
      // Labels syntax: Label[|Filter string]
      for i:=0 To FCustomLabels.Count-1 do
      begin
        GM:=TTIGuideMetrics.Create;
        idx:=FCustomLabels[i];
        k:=Pos('|',idx);
        if k>0 then
        begin
          if idx='|||' then
          begin
            GM.FCustomFilter:='|';
            idx:='|';
          end
          else
          begin
            GM.FCustomFilter:=Copy(idx,k+1,maxint);
            Delete(idx,k,maxint);
          end;
          GM.FIsSeparator:=GM.FCustomFilter='|';
        end
        else
          GM.FCustomFilter:=idx;
        FLabels.AddObject(idx,GM);
      end;
    end
    else
    begin
      for i:=Ord('A') To Ord('Z') do
      begin
        GM:=TTIGuideMetrics.Create;
        GM.FCustomFilter:=Chr(i);
        FLabels.AddObject(Chr(i),GM);
      end;
    end;

    // get optimal width for one char labels
    Canvas.Font:=FLabelFont;
    if (fsBold in FSelLabelStyle) then
      Canvas.Font.Style:=FLabelFont.Style+FSelLabelStyle;
    GetTextExtentPoint32(Canvas.Handle, 'gM', 2, S1);
    for i:=0 To FLabels.Count-1 do
    begin
      idx:=FLabels[i];
      GM:=TTIGuideMetrics(FLabels.Objects[i]);
      if Length(idx)=1 then
        GM.FSize:=S1
      else
        {$IF CompilerVersion>=20}
        GetTextExtentPoint32(Canvas.Handle, idx, Length(idx), GM.FSize);
        {$ELSE}
        GetTextExtentPoint32(Canvas.Handle, PChar(idx), Length(idx), GM.FSize);
        {$IFEND}
      if (FSelLabelIndex=-1) and SameText(lastFilter,GM.FCustomFilter) then
        FSelLabelIndex:=i;
    end;
    if FHoverLabelIndex>=FLabels.Count then
      FHoverLabelIndex:=-1;

    // get the basic page tab size
    Canvas.Font:=Font;
    GetTextExtentPoint32(Canvas.Handle, 'gM', 2, FPageTabSize);

  end;

  if FAutoCheckLabels then
    for i:=0 To FLabels.Count-1 do
    begin
      idx:=FLabels[i];
      GM:=TTIGuideMetrics(FLabels.Objects[i]);
      if (Length(GM.FCustomFilter)=0) or GM.FIsSeparator then
        GM.FHasData:=True
      else
      begin
        GM.FHasData:=False;
        if Assigned(FOnFilterLabel) then
          for j:=0 To Items.Count-1 do
          begin
            FOnFilterLabel(Self,Items[j],GM.FHasData);
            if GM.FHasData then
              break;
          end
        else
        begin
          k:=Length(idx);
          for j:=0 To Items.Count-1 do
            if SameText(idx,Copy(Items[j].Caption,1,k)) then
            begin
              GM.FHasData:=True;
              break;
            end;
        end;
      end;
    end;
end;

function TTagIndex.GetItemsRect:TRect;
var
  x,y,maxx,tmaxx,miny:Integer;
  i:Integer;
  GM,LastGM:TTIGuideMetrics;
  lbMg,lbLen2:Integer;
  R:TRect;
begin
  if (Parent=nil) or (csLoading in ComponentState) then
    Exit; // parent is not assigned - no calculation performed

  Result:=inherited GetItemsRect;
  R:=ClientRect;
  FPaddingRect.Left:=Result.Left-R.Left;
  FPaddingRect.Top:=Result.Top-R.Top;
  FPaddingRect.Bottom:=R.Bottom-Result.Bottom;
  FPaddingRect.Right:=R.Right-Result.Right;
  lbMg:=FLabelFrame.FrameMargin;
  if FSelLabelFrame.FrameMargin>lbMg then
    lbMg:=FSelLabelFrame.FrameMargin;

  if tcsRebuilding in State then
  begin
    // calculate index pages rectangle
    if FShowPageNumbers then
    begin
      FPagesRect:=Result;
      FPagesRect.Top:=FPagesRect.Bottom-FPageTabSize.cy-lbMg*2-4;
    end
    else
      SetRect(FPagesRect,Result.Left,Result.Bottom,Result.Right,Result.Bottom);

    // calculate index labels rectangle
    if FLabels.Count=0 then
      RebuildIndex(True);

    FLabelsRect:=Result;
    FLabelsRect.Bottom:=FPagesRect.Top;

    case FLabelsPosition of
      tlpLeft,tlpRight:
        begin
          // labels at left
          x:=FLabelsRect.Left+lbMg;
          y:=FLabelsRect.Top;
          i:=0;
          maxx:=0;
          tmaxx:=0;
          miny:=FLabelsRect.Top;
          LastGM:=nil;
          while i<FLabels.Count do
          begin
            GM:=TTIGuideMetrics(FLabels.Objects[i]);
            lbLen2:=Length(FLabels[i]);
            if lbLen2>=2 then lbLen2:=2 else if lbLen2<=1 then lbLen2:=0;
            if (y>miny) and (LastGM<>nil) and (y+GM.FSize.cy+lbMg*2>=FLabelsRect.Bottom) then
            begin
              Inc(x,maxx+ColSpacing);
              y:=miny;
              maxx:=0;
            end;

            SetRect(GM.FRect,x,y+lbMg,x+GM.FSize.cx+(lbMg+lbLen2),y+GM.FSize.cy+lbMg);
            GM.FRectFrame:=GM.FRect;
            InflateRect(GM.FRectFrame,(lbMg+lbLen2),lbMg);
            if GM.FRectFrame.Right-GM.FRectFrame.Left>maxx then
              maxx:=GM.FRectFrame.Right-GM.FRectFrame.Left;
            if GM.FRectFrame.Right>tmaxx then
              tmaxx:=GM.FRectFrame.Right;
            Inc(y,GM.FRectFrame.Bottom-GM.FRectFrame.Top+RowSpacing);
            if GM.FIsSeparator then
            begin // top items separator
              maxx:=0;
              if Length(trim(FLabels[i]))=0 then
                Dec(y,(GM.FRectFrame.Bottom-GM.FRectFrame.Top) div 2);
              miny:=y;
            end;
            LastGM:=GM;

            Inc(i);
          end;

          if FLabelsPosition=tlpLeft then
          begin
            if tmaxx>0 then
              FLabelsRect.Right:=tmaxx+FPaddingRect.Left*2
            else
              FLabelsRect.Right:=FLabelsRect.Left+FPaddingRect.Left*2;
            if FShowPageNumbers then
              FPagesRect.Left:=FLabelsRect.Right-FPaddingRect.Left;
          end
          else
          begin
            if tmaxx=0 then
              FLabelsRect.Left:=FLabelsRect.Right-FPaddingRect.Right*2
            else
            begin
              maxx:=FLabelsRect.Right-tmaxx;
              for i:=0 To FLabels.Count-1 do
              begin
                GM:=TTIGuideMetrics(FLabels.Objects[i]);
                offsetRect(GM.FRect,maxx,0);
                offsetRect(GM.FRectFrame,maxx,0);
              end;
              FLabelsRect.Left:=FLabelsRect.Right-tmaxx-FPaddingRect.Right;
              if FShowPageNumbers then
                FPagesRect.Right:=FLabelsRect.Left+FPaddingRect.Right+1;
            end;
          end;
        end;
    else
        begin
          // labels at top
          x:=FLabelsRect.Left;
          y:=FLabelsRect.Top+lbMg;
          i:=0;
          LastGM:=nil;
          while i<FLabels.Count do
          begin
            GM:=TTIGuideMetrics(FLabels.Objects[i]);
            lbLen2:=Length(FLabels[i]);
            if lbLen2>=2 then lbLen2:=2 else if lbLen2<=1 then lbLen2:=0;
            if (x>Result.Left) and (LastGM<>nil) and (x+GM.FSize.cx+(lbMg+lbLen2)*2>=Result.Right) then
            begin
              Inc(y,LastGM.FRectFrame.Bottom-LastGM.FRectFrame.Top+RowSpacing);
              x:=Result.Left;
            end;

            SetRect(GM.FRect,x+(lbMg+lbLen2),y,x+GM.FSize.cx+(lbMg+lbLen2),y+GM.FSize.cy);
            GM.FRectFrame:=GM.FRect;
            InflateRect(GM.FRectFrame,(lbMg+lbLen2),lbMg);

            Inc(x,GM.FRectFrame.Right-GM.FRectFrame.Left+ColSpacing);
            LastGM:=GM;
            Inc(i);
          end;

          if Assigned(LastGM) then
            FLabelsRect.Bottom:=LastGM.FRectFrame.Bottom+FPaddingRect.Top*2
          else
            FLabelsRect.Bottom:=FLabelsRect.Top+FPaddingRect.Top*2;
        end;
    end;

    if FPagesRect.Top<FLabelsRect.Bottom then
      SetRect(FPagesRect,Result.Left,Result.Bottom,Result.Right,Result.Bottom);
  end;

  if FShowPageNumbers then
    Result.Bottom:=FPagesRect.Top-FPaddingRect.Bottom-1;

  case FLabelsPosition of
    tlpLeft:Result.Left:=FLabelsRect.Right+FPaddingRect.Left+1;
    tlpRight:Result.Right:=FLabelsRect.Left-1;
  else
      begin
        // labels at top
        Result.Top:=FLabelsRect.Bottom+FPaddingRect.Top+1;
        Inc(Result.Left,lbMg+FPaddingRect.Left);
        Dec(Result.Right,lbMg+FPaddingRect.Right);
      end;
  end;
end;

procedure TTagIndex.DoDrawItems(aCanvas:TCanvas; Rect: TRect);
var
  R,tmpRect: TRect;
  i,x,y,w: Integer;
  GM: TTIGuideMetrics;
  Flags: Cardinal;
  asDisabled:Boolean;
  lbMg:Integer;
  ptc:string;
  nTabClr,selTabClr:TColor;
  selTabP:TPoint;

  procedure DrawLabel(const LCaption:string; LGM:TTIGuideMetrics; asSelected:boolean);
  var
    bc,fc:TColor;
    fw,rc:Integer;
    fs:TPenStyle;
    bcd:Boolean;
  begin
    fw:=FLabelFrame.FrameSize;
    rc:=FLabelFrame.RoundedSize;
    fs:=FLabelFrame.FrameStyle;
    if asDisabled then
    begin
      aCanvas.Font.Color:=clGrayText;
      aCanvas.Font.Style:=FLabelFont.Style;
      bc:=clNone;
      fc:=clGrayText;
    end
    else
    begin
      aCanvas.Font.Style:=FLabelFont.Style;
      if LGM.FHasData then
        aCanvas.Font.Color:=FLabelFont.Color
      else
        aCanvas.Font.Color:=clGrayText;
      bc:=FLabelFrame.BackColor;
      fc:=FLabelFrame.FrameColor;
      if asSelected then
      begin
        aCanvas.Font.Style:=FSelLabelStyle;
        if (FSelLabelColor<>clNone) {$IF CompilerVersion>=15} and (FSelLabelColor<>clDefault) {$IFEND} then
          aCanvas.Font.Color:=FSelLabelColor;
        if (FSelLabelFrame.BackColor<>clNone) {$IF CompilerVersion>=15} and (FSelLabelFrame.BackColor<>clDefault) {$IFEND} then
          bc:=FSelLabelFrame.BackColor;
        fw:=FSelLabelFrame.FrameSize;
        if fw>0 then
        begin
          if (FSelLabelFrame.FrameColor<>clNone) {$IF CompilerVersion>=15} and (FSelLabelFrame.FrameColor<>clDefault) {$IFEND} then
            fc:=FSelLabelFrame.FrameColor;
          fs:=FSelLabelFrame.FrameStyle;
          rc:=FSelLabelFrame.RoundedSize;
        end;
      end;
    end;

    bcd:=(bc<>clNone) {$IF CompilerVersion>=15} and (bc<>clDefault) {$IFEND};
    if bcd then
    begin
      aCanvas.Brush.Color:=bc;
      aCanvas.Brush.Style:=bsSolid;
    end
    else
      aCanvas.Brush.Style:=bsClear;
    if fw>0 then
    begin
      aCanvas.Pen.Width:=fw;
      if (fc=clNone) {$IF CompilerVersion>=15} or (fc=clDefault) {$IFEND} then
        aCanvas.Pen.Color:=aCanvas.Font.Color
      else
        aCanvas.Pen.Color:=fc;
      aCanvas.Pen.Style:=fs;
    end
    else
      aCanvas.Pen.Style:=psClear;

    if bcd or (fw>0) then
    begin
      if rc>0 then
        RoundRect(aCanvas.Handle,LGM.FRectFrame.Left,LGM.FRectFrame.Top,LGM.FRectFrame.Right,LGM.FRectFrame.Bottom,rc,rc)
      else
      if fw>0 then
        Rectangle(aCanvas.Handle,LGM.FRectFrame.Left,LGM.FRectFrame.Top,LGM.FRectFrame.Right,LGM.FRectFrame.Bottom)
      else
        FillRect(aCanvas.Handle,LGM.FRectFrame,aCanvas.Brush.Handle);
    end;
    aCanvas.Brush.Style:=bsClear;
    FDrawTextProc(aCanvas.Handle,LCaption,LGM.FRect,Flags);
  end;

  procedure DrawPageTab(Idx:Integer;const TCap:string; TR:TRect; AsButton:Boolean);
  var
    bc,fc:TColor;
    fw,rc:Integer;
    fs:TPenStyle;
    bcd:Boolean;
  begin
    if AsButton then
    begin
      fw:=FLabelFrame.FrameSize;
      rc:=FLabelFrame.RoundedSize;
      fs:=FLabelFrame.FrameStyle;
      if asDisabled then
      begin
        aCanvas.Font.Color:=clGrayText;
        aCanvas.Font.Style:=FLabelFont.Style;
        bc:=clNone;
        fc:=clGrayText;
      end
      else
      begin
        aCanvas.Font.Style:=FLabelFont.Style;
        aCanvas.Font.Color:=FLabelFont.Color;
        bc:=FLabelFrame.BackColor;
        fc:=FLabelFrame.FrameColor;
        if Idx=PageIndex+1 then
        begin
          aCanvas.Font.Style:=FSelLabelStyle;
          if (FSelLabelColor<>clNone) {$IF CompilerVersion>=15} and (FSelLabelColor<>clDefault) {$IFEND} then
            aCanvas.Font.Color:=FSelLabelColor;
          if (FSelLabelFrame.BackColor<>clNone) {$IF CompilerVersion>=15} and (FSelLabelFrame.BackColor<>clDefault) {$IFEND} then
            bc:=FSelLabelFrame.BackColor;
          fw:=FSelLabelFrame.FrameSize;
          if fw>0 then
          begin
            if (FSelLabelFrame.FrameColor<>clNone) {$IF CompilerVersion>=15} and (FSelLabelFrame.FrameColor<>clDefault) {$IFEND} then
              fc:=FSelLabelFrame.FrameColor;
            fs:=FSelLabelFrame.FrameStyle;
          end;
          rc:=FSelLabelFrame.RoundedSize;
        end;
      end;

      bcd:=(bc<>clNone) {$IF CompilerVersion>=15} and (bc<>clDefault) {$IFEND};
      if bcd then
      begin
        aCanvas.Brush.Color:=bc;
        aCanvas.Brush.Style:=bsSolid;
      end
      else
        aCanvas.Brush.Style:=bsClear;
      if fw>0 then
      begin
        aCanvas.Pen.Width:=fw;
        if (fc=clNone) {$IF CompilerVersion>=15} or (fc=clDefault) {$IFEND} then
          aCanvas.Pen.Color:=aCanvas.Font.Color
        else
          aCanvas.Pen.Color:=fc;
        aCanvas.Pen.Style:=fs;
      end
      else
        aCanvas.Pen.Style:=psClear;

      if bcd or (fw>0) then
      begin
        if rc>0 then
          RoundRect(aCanvas.Handle,TR.Left,TR.Top,TR.Right,TR.Bottom,rc,rc)
        else
        if fw>0 then
          Rectangle(aCanvas.Handle,TR.Left,TR.Top,TR.Right,TR.Bottom)
        else
          FillRect(aCanvas.Handle,TR,aCanvas.Brush.Handle);
      end;

      aCanvas.Brush.Style:=bsClear;
      FDrawTextProc(aCanvas.Handle,TCap,TR,Flags);
    end
    else
    begin
      if asDisabled then
        aCanvas.Pen.Color:=clGrayText
      else
      if Idx=PageIndex+1 then
        aCanvas.Pen.Color:=selTabClr
      else
        aCanvas.Pen.Color:=nTabClr;
      if Idx=PageIndex+1 then
      begin
        selTabP.x:=TR.Left; selTabP.y:=TR.Right;
        if FSelLabelFrame.RoundedSize>0 then
          aCanvas.Polyline([
            point(TR.Left,TR.Top),point(TR.Left,TR.Bottom-1),
            point(TR.Left,TR.Bottom-1),point(TR.Left+1,TR.Bottom),
            point(TR.Left+1,TR.Bottom),point(TR.Right-1,TR.Bottom),
            point(TR.Right-1,TR.Bottom),point(TR.Right,TR.Bottom-1),
            point(TR.Right,TR.Bottom-1),point(TR.Right,TR.Top)])
        else
          aCanvas.Polyline([
            point(TR.Left,TR.Top),point(TR.Left,TR.Bottom),
            point(TR.Right,TR.Bottom),point(TR.Right,TR.Top)]);
      end
      else
        aCanvas.Polyline([point(TR.Right,TR.Top+3),point(TR.Right,TR.Bottom-1)]);
      offsetrect(TR,0,1);
      FDrawTextProc(aCanvas.Handle,TCap,TR,Flags);
    end;
  end;

  function _MidColor(Color1, Color2: TColor): TColor;
  begin
    Result := ((ColorToRGB(Color1) shr 1) and $7F7F7F7F) + ((ColorToRGB(Color2) shr 1) and $7F7F7F7F);
  end;

begin
  inherited;

  Flags := DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or DT_CENTER;
  Flags := DrawTextBiDiModeFlags(Flags);
  asDisabled := not Enabled;

  // draw labels
  if IntersectRect(tmpRect,Rect,FLabelsRect) then
  begin
    aCanvas.Font:=FLabelFont;
    for i:=0 To FLabels.Count-1 do
    begin
      GM:=TTIGuideMetrics(FLabels.Objects[i]);
      if IntersectRect(tmpRect,Rect,GM.FRectFrame) then
        DrawLabel(FLabels[i],GM,(i=FSelLabelIndex) or (i=FHoverLabelIndex));
    end;

    // labels header line
    aCanvas.Pen.Style:=psSolid;
    if (FSelLabelFrame.FrameColor=clNone) {$IF CompilerVersion>=15} or (FSelLabelFrame.FrameColor=clDefault) {$IFEND} then
      aCanvas.Pen.Color:=Font.Color
    else
      aCanvas.Pen.Color:=FSelLabelFrame.FrameColor;
    if FSelLabelFrame.FrameSize<=0 then
      aCanvas.Pen.Width:=1
    else
      aCanvas.Pen.Width:=FSelLabelFrame.FrameSize;
    case FLabelsPosition of
      tlpLeft:
        aCanvas.PolyLine([point(FLabelsRect.Right-FPaddingRect.Left,FLabelsRect.Top),point(FLabelsRect.Right-FPaddingRect.Left,FLabelsRect.Bottom)]);
      tlpRight:
        aCanvas.PolyLine([point(FLabelsRect.Left+FPaddingRect.Right,FLabelsRect.Top),point(FLabelsRect.Left+FPaddingRect.Right,FLabelsRect.Bottom)]);
    else // labels at top
      aCanvas.PolyLine([point(FLabelsRect.Left,FLabelsRect.Bottom-FPaddingRect.Top),point(FLabelsRect.Right,FLabelsRect.Bottom-FPaddingRect.Top)]);
    end;
  end;

  // draw pages
  if FShowPageNumbers and IntersectRect(tmpRect,Rect,FPagesRect) then
  begin
    // pages footer line
    aCanvas.Font:=Font;
    aCanvas.Pen.Style:=psSolid;
    if (FSelLabelFrame.FrameColor=clNone) {$IF CompilerVersion>=15} or (FSelLabelFrame.FrameColor=clDefault) {$IFEND} then
      selTabClr:=Font.Color
    else
      selTabClr:=FSelLabelFrame.FrameColor;
    aCanvas.Pen.Color:=selTabClr;
    if Transparent then
      nTabClr:=_MidColor(selTabClr,clBtnFace)
    else
      nTabClr:=_MidColor(selTabClr,Color);
    if FSelLabelFrame.FrameSize<=0 then
      aCanvas.Pen.Width:=1
    else
      aCanvas.Pen.Width:=FSelLabelFrame.FrameSize;

    if PageCount>1 then
    begin
      lbMg:=FLabelFrame.FrameMargin;
      if FSelLabelFrame.FrameMargin>lbMg then
        lbMg:=FSelLabelFrame.FrameMargin;

      if asDisabled then
        aCanvas.Font.Color:=clGrayText;
      if FPageNumberStyle=pnsTabs then  // tabs
      begin
        aCanvas.Font.Style:=aCanvas.Font.Style-[fsUnderline,fsStrikeOut];
        selTabP.x:=-1; selTabP.y:=-1;
        aCanvas.Brush.Style:=bsClear;
        x:=FPagesRect.Left+FPaddingRect.Left;
        y:=FPagesRect.Top;
        for i:=1 To PageCount do
        begin
          ptc:=IntToStr(i);
          w:=FPageTabSize.cx+(Length(ptc)-1)*(FPageTabSize.cx div 4)+lbMg*2;
          if x+w>=FPagesRect.Right-FPaddingRect.Right then
            break;
          SetRect(R,x,y,x+w,y+FPageTabSize.cy+lbMg+1);
          if IntersectRect(tmpRect,Rect,R) then
            DrawPageTab(i,ptc,R,False);
          Inc(x,w);
        end;
        aCanvas.Pen.Color:=selTabClr;
        if selTabP.x=-1 then
          aCanvas.PolyLine([FPagesRect.TopLeft,point(FPagesRect.Right,FPagesRect.Top)])
        else
        begin
          aCanvas.PolyLine([FPagesRect.TopLeft,point(selTabP.x,FPagesRect.Top)]);
          aCanvas.PolyLine([point(selTabP.y,FPagesRect.Top),point(FPagesRect.Right,FPagesRect.Top)]);
        end;
      end
      else
      begin // buttons
        aCanvas.PolyLine([FPagesRect.TopLeft,point(FPagesRect.Right,FPagesRect.Top)]);
        x:=FPagesRect.Left+FPaddingRect.Left;
        y:=FPagesRect.Top+FPaddingRect.Bottom;
        for i:=1 To PageCount do
        begin
          ptc:=IntToStr(i);
          w:=FPageTabSize.cx+(Length(ptc)-1)*(FPageTabSize.cx div 4)+lbMg*2;
          if x+w>=FPagesRect.Right-FPaddingRect.Right then
            break;
          SetRect(R,x,y,x+w,y+FPageTabSize.cy+lbMg*2);
          if IntersectRect(tmpRect,Rect,R) then
            DrawPageTab(i,ptc,R,True);
          Inc(x,w+ColSpacing);
        end;
      end;
    end
    else
      aCanvas.PolyLine([FPagesRect.TopLeft,point(FPagesRect.Right,FPagesRect.Top)]);
  end;
end;

end.

