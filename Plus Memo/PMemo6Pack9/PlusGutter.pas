unit PlusGutter;

{ PlusGutter 6.2
  Works with TPlusMemo version 6.1 or later

  © Electro-Concept Mauricie, 1998-2004

  TPlusGutter is a graphic control that can be placed next to a TPlusMemo component and be linked to it.
  It then automatically shows line numbers at the appropriate vertical position.

  It also allows to bookmark lines using Shift+Ctrl+0..9, and to jump from any point to one of
  the bookmarks using Ctrl+0..9. It also shows (optionally) line numbers or paragraph numbers. }

{ ISSUE: no bookmarks in Clx version yet }

{$DEFINE PlusGutter}
{UCONVERT}
  {$IFDEF PlusGutterClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

{$R PlusGutter.res}

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

interface

{$IFDEF pmClx}
uses
  Classes, QControls, QGraphics, PlusMemoClx, PMSupportClx;
{$ELSE}
uses
  Messages, Classes, Controls, Graphics, PlusMemo, PMSupport;
{$ENDIF}

type
  TBookmarkRange = 0..9;

  TpgDrawItemEvent = procedure (Sender: TObject; LineIndex: Integer; var Text: string; var Graphic: TGraphic) of object;
  TpgBookmarkEvent = procedure (Sender: TObject; BookmarkIndex: TBookmarkRange; Navigator: TPlusNavigator;
                                var Accept: Boolean) of object;

  TPlusGutter = class(TGraphicControl, IpmsNotify)
  private
    { Fields corresponding to public or published properties }
    fAlignment      : TAlignment;
    fPlusMemo       : TPlusMemo;
    fLineNumbers    : Boolean;
    fBookmarks      : Boolean;
    fParNumbers     : Boolean;
    fIgnoreLast     : Boolean;
    fKeepBookmarks  : Boolean;
    fRightBorder    : Boolean;
    fOnDrawItem     : TpgDrawItemEvent;
    fOnSetBookmark, fOnClearBookmark : TpgBookmarkEvent;

    { Property access methods }
    procedure SetAlignment(al: TAlignment);
    procedure SetLineNumbers(ln: Boolean);
    procedure SetPlusMemo(Value: TPlusMemo);
    procedure setParNumbers(const Value: Boolean);
    function getBookmarks(BookmarkIndex: TBookmarkRange): Integer;
    function getBookmarkList(BookmarkIndex: TBookmarkRange): TPlusNavigator;
    procedure setIgnoreLast(const Value: Boolean);
    procedure setKeepBookmarks(Value: Boolean);
    procedure setRightBorder(Value: Boolean);
    procedure setOnDrawItem(const Value: TpgDrawItemEvent);

    { Event handler for bookmark navigators OnFree }
    procedure BookMarkFree(Sender: TObject);

    { IpmsNotify }
    procedure Notify(Sender: TComponent; Events: TpmEvents);

  protected
    { these fields put in protected part to support for descendant classes that
      provide their own painting code or other effects }

    { last saved values from PlusMemo we are attached }
    fTopY           : Integer;
    fLineCount      : Integer;
    fParCount       : Integer;
    fWordWrap       : Boolean;
    fBookmarkValues : array[TBookmarkRange] of Integer;

    { internal working fields }
    fTrackNav  : TPlusNavigator;
    fBookmarkList   : array[TBookmarkRange] of TPlusNavigator;

    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MemoWndProc(var Msg: TMessage);
    procedure Scroll(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    BookmarkLine(ALine: Integer; BookmarkIndex: TBookmarkRange): Boolean; virtual;
    procedure   BookmarkPos(Pos: Integer; BookmarkIndex: TBookmarkRange); virtual;
    procedure   ClearBookmarks;
    procedure   JumpToBookmark(BookmarkIndex: TBookmarkRange);

    function GetBookmarkFromLine(ALine: Integer): SmallInt;
    property BookmarksArray[BookmarkIndex: TBookmarkRange]: Integer read getBookmarks;
    property BookmarkList[BookmarkIndex: TBookmarkRange]: TPlusNavigator read getBookmarkList;

  published
    { Genuine TPlusGutter properties }
    property Alignment: TAlignment read fAlignment write SetAlignment default taCenter;
    property Bookmarks: Boolean read FBookmarks write FBookmarks default True;
    property IgnoreLastLineIfEmpty: Boolean read fIgnoreLast write setIgnoreLast;
    property KeepBookmarks: Boolean read fKeepBookmarks write setKeepBookmarks default True;
    property LineNumbers: Boolean read fLineNumbers write SetLineNumbers default True;
    property ParagraphNumbers: Boolean read fParNumbers write setParNumbers default False;
    property PlusMemo: TPlusMemo read FPlusMemo write SetPlusMemo;
    property RightBorder: Boolean read fRightBorder write setRightBorder default True;
    property OnDrawItem: TpgDrawItemEvent read fOnDrawItem write setOnDrawItem;
    property OnSetBookmark: TpgBookmarkEvent read fOnSetBookmark write fOnSetBookmark;
    property OnClearBookmark: TpgBookmarkEvent read fOnClearBookmark write fOnClearBookmark;

    { Exposition from TGraphicControl }
    {$IFNDEF pmClx}
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragMode;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

{$IFDEF pmClx}
uses SysUtils, Qt, Types, QForms, QImgList;
{$ELSE}
uses Windows, SysUtils, Forms, ImgList;
{$ENDIF}

var gBookmarkIcons: TImageList;

procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TPlusGutter]);
end;

const
  HotKeys   : set of AnsiChar = ['0'..'9'];
  IcnHeight = 10;
  IcnWidth  = 10;


constructor TPlusGutter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle:= ControlStyle + [csOpaque];

  { Initialize inherited properties }
  Width := 30;
  Height:= 50;

  { Initialize new properties }
  fLineNumbers:= True;
  fAlignment  := taCenter;
  fBookmarks  := True;
  fRightBorder:= True;
  fKeepBookmarks:= True;

  { Initialize the list of bitmaps }
  if gBookmarkIcons=nil then
    begin
      gBookmarkIcons := TImageList.CreateSize(IcnWidth, IcnHeight);
      {$IFNDEF pmClx}
        gBookmarkIcons.DrawingStyle := dsTransparent;
        gBookmarkIcons.ResInstLoad(HInstance, rtBitmap,
                                   {UCONVERT}
                                   {$IFDEF PLUSGUTTERU}  'PGU_BOOKMARKICONS1',
                                   {$ELSE} 'PG_BOOKMARKICONS1',
                                   {$ENDIF}
                                   clWhite)
                                   {/UCONVERT};
      {$ENDIF}
    end
end;

destructor TPlusGutter.Destroy;
var i: Integer;
begin
  SetPlusMemo(nil);
  fTrackNav.Free;
  for i:= 0 to High(TBookmarkRange) do
    if fBookmarkList[i]<>nil then fBookmarkList[i].OnFree:= nil;
  inherited Destroy;
end;


procedure TPlusGutter.setAlignment(al: TAlignment);
begin
  if al<>fAlignment then
    begin
      fAlignment:= al;
      Invalidate
    end
end;

procedure TPlusGutter.setIgnoreLast(const Value: Boolean);
begin
  if Value<>fIgnoreLast then
    begin
      fIgnoreLast := Value;
      Invalidate
    end
end;

procedure TPlusGutter.SetLineNumbers(ln: Boolean);
begin
  if ln<>fLineNumbers then
    begin
      fLineNumbers:= ln;
      Invalidate
    end
end;

procedure TPlusGutter.SetPlusMemo(Value: TPlusMemo);
var i: Integer; sif: IpmsNotify;
begin
  if fPlusMemo=Value then Exit;

  { Clear the old bookmarks }
  for i := 0 to 9 do FreeAndNil(fBookmarkList[i]);
  sif:= Self;
  if fPlusMemo <> nil then
    begin
      fPlusMemo.MsgList.Remove(Pointer(sif));
      fPlusMemo.NotifyList.Remove(Pointer(sif))
    end;
  fPlusMemo := Value;
  if fPlusMemo <> nil then
    begin
      fPlusMemo.MsgList.Add(Pointer(sif));
      fPlusMemo.NotifyList.Add(Pointer(sif));
      fPlusMemo.FreeNotification(Self);
      if fIgnoreLast then
        begin
          fLineCount:= fPlusMemo.Lines.Count;
          fParcount:= fPlusMemo.Paragraphs.Count
        end
      else
        begin
          fLineCount:= fPlusMemo.LineCount;
          fParCount:= fPlusMemo.ParagraphCount
        end
    end
  else
    begin
      fLineCount:= 0;
      fParCount:= 0
    end;
  Invalidate
end;

procedure TPlusGutter.setParNumbers(const Value: Boolean);
begin
  if Value<>fParNumbers then
    begin
      fParNumbers := Value;
      Invalidate
    end
end;

procedure TPlusGutter.Scroll(Sender: TObject);
var currenttopy : Longint;
    r           : TRect;
begin
  if (Parent<>nil) and Parent.HandleAllocated then
    begin
      r:= BoundsRect;
      if (fPlusMemo.LineCount<>fLineCount) or (fPlusMemo.ParagraphCount<>fParCount) then Invalidate
      else
        begin
          currenttopy:= fPlusMemo.TopOrigin;
          ScrollWindow(Parent.{$IFDEF pmClx} ChildHandle {$ELSE} Handle {$ENDIF}, 0, fTopY-currenttopy, @r, @r);
          fTopY  := currenttopy
        end
    end
end;

procedure TPlusGutter.Paint;
var
  scanvas    : TCanvas;    { local Canvas reference }
  clRect     : TRect;      { local value of clip rectangle }
  sclient    : TRect;      { local value of ClientRect }
  DrawY      : Integer;    { Y value where we are drawing }
  DrawLine   : Integer;    { line index being drawn }
  DrawIndex  : Integer;    { bookmark index of current line being drawn }
  LHeight    : Integer;    { local value of line height }
  i          : Integer;
  DigitWidth : Integer;    { width of a digit, used only if LineNumbers=True or with OnDrawItem }
  CWidth     : Integer;    { local value of ClientWidth, minus border }
  t          : string;     { temporary string used if LineNumbers = True }
  LeftText   : Integer;    { left coordinate of text or bookmark icon }
  po         : TPoint;
  mt         : Integer;
  IconYAdd   : Integer;    { used in centering the bookmark icon vertically }
  sgraph     : TGraphic;

begin
  sclient:= ClientRect;
  scanvas:= Canvas;
  clRect:= scanvas.ClipRect;
  sgraph:= nil;
  IntersectRect(clRect, sclient, clRect);

  {$IFDEF PMDEBUG}
    OutputDebugString(PAnsiChar('PG: Painting ' + IntToStr(clRect.Left) + ',' + IntToStr(clRect.Top) + ',' +
                                        IntToStr(clRect.Right) + ',' + IntToStr(clRect.Bottom)));
  {$ENDIF}
  if fRightBorder then CWidth:= sclient.Right - 2
                  else CWidth:= sclient.Right;

  { Fill with color }
  scanvas.Brush.Color := Color;
  scanvas.Brush.Style:= bsSolid;
  scanvas.FillRect(clRect);
  scanvas.Brush.Style:= bsClear;

  { Draw the right border, which is 2 pixels wide }
  if clRect.Right > CWidth then
    begin
      scanvas.Pen.Color := clBtnHighlight;
      scanvas.MoveTo(CWidth, clRect.Top);
      scanvas.LineTo(CWidth, clRect.Bottom);
      scanvas.Pen.Color := clBtnShadow;
      scanvas.MoveTo(CWidth+1, clRect.Top);
      scanvas.LineTo(CWidth+1, clRect.Bottom);
    end;

  { Draw the bookmarks and line numbers }
  if (fPlusMemo<>nil) and (fPlusMemo.HandleAllocated) then
    begin
      if fTrackNav=nil then fTrackNav:= TPlusNavigator.Create(nil);
      fTrackNav.fPMemo:= fPlusMemo;
      try
        fTrackNav.Assign(fPlusMemo.DisplayStartNav);
        fTopY   := fPlusMemo.TopOrigin;
        LHeight:= fPlusMemo.LineHeightRT;

        for i:= Low(TBookmarkRange) to High(TBookmarkRange) do
            if fBookmarkList[i]<>nil then fBookmarkValues[i]:= fBookmarkList[i].TrueLineNumber
                                     else fBookmarkValues[i]:= -1;

        po:= ClientOrigin;
        mt:= fPlusMemo.ClientToScreen(Point(0,0)).Y;
        DrawLine:= (po.Y+clRect.Top-mt+FTopY) div lheight;
        if DrawLine<0 then DrawLine:= 0;

        if fIgnoreLast then
          begin
            fLineCount:= fPlusMemo.Lines.Count;
            fParCount:= fPlusMemo.Paragraphs.Count
          end
         else
          begin
            fLineCount:= fPlusMemo.IParList.fVisibleLineCount; //  LineCount;
            fParCount:= fPlusMemo.ParagraphCount
          end;

        fWordWrap:= fPlusMemo.WordWrap;

        DigitWidth:= 0;
        if LineNumbers or ParagraphNumbers or Assigned(fOnDrawItem) then
          begin
            scanvas.Font:= Self.Font;
            {$IFDEF pmClx}
            scanvas.TextAlign:= taBottom;
            {$ELSE}
            SetTextAlign(scanvas.Handle, ta_baseline);
            {$ENDIF}
            if Alignment<>taLeftJustify then digitwidth:= scanvas.TextWidth('0')
                                        else digitwidth:= 0;
          end;

        DrawY    := DrawLine*LHeight - fTopY + mt - po.Y;
        LeftText := 0;

        IconYAdd := (LHeight - IcnHeight) div 2;
        if IconYAdd+IcnHeight<fPlusMemo.LineBase then IconYAdd:= fPlusMemo.LineBase-IcnHeight;

        while (DrawY<clRect.Bottom) and (DrawLine<fLineCount) do
          begin
            if Assigned(fOnDrawItem) then
              begin
                fOnDrawItem(Self, DrawLine, t, sgraph);
                case Alignment of
                  taLeftJustify:  lefttext:= 0;
                  taRightJustify: lefttext:= CWidth - scanvas.TextWidth(t);
                  taCenter:       lefttext:= (CWidth - scanvas.TextWidth(t)) div 2
                  end;
                scanvas.TextOut(lefttext, DrawY + fPlusMemo.LineBase, t);
                if sgraph<>nil then scanvas.Draw(0, DrawY, sgraph);
              end
            else
              begin
                DrawIndex:= GetBookmarkFromLine(DrawLine);
                if DrawIndex>=0 then
                    begin
                      case Alignment of
                        taLeftJustify: lefttext:= 2;
                        taRightJustify: lefttext:= CWidth - IcnWidth;
                        taCenter      : lefttext:= (CWidth - IcnWidth) div 2
                        end;
                      gBookmarkIcons.Draw(scanvas, lefttext, DrawY + iconyadd, DrawIndex)
                    end

                else
                  if LineNumbers or ParagraphNumbers then
                    begin
                      fTrackNav.VisibleLineNumber:= DrawLine;
                      if ParagraphNumbers then
                          if fTrackNav.ParLine=0 then t:= IntToStr(fTrackNav.ParNumber+1)
                                                 else t:= ''
                      else t:= IntToStr(fTrackNav.TrueLineNumber+1);
                      if t<>'' then
                        begin
                          case Alignment of
                            taLeftJustify:  lefttext:= 0;
                            taRightJustify: lefttext:= CWidth - Length(t)*DigitWidth;
                            taCenter:       lefttext:= (CWidth - Length(t)*DigitWidth) div 2
                            end;
                          scanvas.TextOut(lefttext, DrawY + fPlusMemo.LineBase, t)
                        end
                    end
              end;

            Inc(DrawLine);
            Inc(DrawY, LHeight)
          end;  // while DrawLine<FLineCount
      finally
        fTrackNav.fPMemo:= nil
        end
    end;   // FPlusMemo <> nil
end;  // TPlusGutter.Paint

procedure TPlusGutter.Notification(AComponent: TComponent; Operation: TOperation);
var i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (Assigned(FPlusMemo)) and (AComponent = FPlusMemo) then
    begin
      for i:= 0 to 9 do fBookmarkList[i] := nil;
      fPlusMemo := nil;
    end
end;

procedure TPlusGutter.MemoWndProc(var Msg: TMessage);
{$IFNDEF pmClx}
var
  ss: TShiftState;
  ch: AnsiChar;
  ln: Integer;
{$ENDIF}
begin
  {$IFDEF pmClx}
  case QEvent_type(QEventH(Msg.Msg)) of
  {$ELSE}
  case Msg.Msg of
  {$ENDIF}
    {$IFNDEF pmClx}
    //QEventType_KeyPress:
    WM_KEYDOWN: // check for trying to reach or set a bookmark
      begin
        ss := KeyDataToShiftState(Msg.LParam);
        ch := AnsiChar(Msg.WParam);
        ln := Msg.WParam - Ord('0');

        if fBookmarks and (ch in HotKeys) then
            if (ssShift in ss) and (ssCtrl in ss) and not (ssAlt in ss) then
              begin  // set a bookmark
                BookmarkPos(fPlusMemo.SelStart, ln);
              end
            else
              if (ssCtrl in ss) and (ss*[ssShift, ssAlt]=[]) and (fBookmarkList[ln] <> nil) then
                begin  // jump to bookmark
                  fPlusMemo.SelStart := FBookmarkList[ln].Pos;
                  fPlusMemo.ScrollInView;
                end;
        end;

     {$ENDIF}

     {$IFDEF pmClx}
     QEventType_Paint:
     {$ELSE}
     WM_PAINT: if (fWordWrap<>fPlusMemo.WordWrap) or (fTopY<>fPlusMemo.TopOrigin) or
                  (fLineCount<>fPlusMemo.IParList.fVisibleLineCount) then Invalidate
     {$ENDIF}
     end;   // case Msg
end;   // MemoWndProc

function TPlusGutter.BookmarkLine(ALine: Longint; BookmarkIndex: TBookmarkRange): Boolean;
var CurrentBIndex: Integer; saccept: Boolean;
begin
  Result := False;  // Return False if the line is already bookmarked
  CurrentBIndex:= GetBookmarkFromLine(ALine);
  saccept:= True;

  if CurrentBIndex = BookmarkIndex then
    begin      // Calling BookmarkLine on a line with already the same bookmark index: remove the bookmark
      if Assigned(fOnClearBookmark) then
          fOnClearBookmark(Self, BookmarkIndex, fBookmarkList[BookmarkIndex], saccept);
      if not saccept then Exit;
      Result := True;
      fBookmarkList[BookmarkIndex].Free;
      fBookmarkList[BookmarkIndex]:= nil
    end
  else
      if CurrentBIndex<0 then
        begin    // Add the bookmark
          fBookmarkList[BookmarkIndex] := TPlusNavigator.Create(fPlusMemo);
          fBookmarkList[BookmarkIndex].TrueLineNumber:= ALine;
          if Assigned(fOnSetBookmark) then fOnSetBookmark(Self, BookmarkIndex, fBookmarkList[BookmarkIndex], saccept);
          if saccept then
            begin
              fBookmarkList[BookmarkIndex].AdjustRight:= fBookmarkList[BookmarkIndex].ParOffset=0;
              if not KeepBookmarks then
                begin
                  fBookmarkList[BookmarkIndex].FreeOnDelete:= True;
                  fBookmarkList[BookmarkIndex].OnFree:= BookmarkFree
                end;
              Result := True
            end
          else
              FreeAndNil(fBookmarkList[BookmarkIndex])
        end;

  if Result then Invalidate
end;

procedure TPlusGutter.BookmarkPos(Pos: Integer; BookmarkIndex: TBookmarkRange);
var sbm: TPlusNavigator; saccept: Boolean;
begin
  sbm:= fBookmarkList[BookmarkIndex];
  if sbm=nil then
    begin
      saccept:= True;
      sbm:= TPlusNavigator.Create(fPlusMemo);
      sbm.Pos:= Pos;
      if Assigned(fOnSetBookmark) then fOnSetBookmark(Self, BookmarkIndex, sbm, saccept);
      if saccept then
        begin
          fBookmarkList[BookmarkIndex]:= sbm;
          if not KeepBookmarks then
            begin
              sbm.FreeOnDelete:= True;
              sbm.OnFree:= BookmarkFree
            end
        end
      else
        begin
          sbm.Free;
          Exit
        end
    end;

  sbm.Pos:= Pos;
  if not KeepBookmarks then sbm.Col:= 0;  // place it at beginning of line
  sbm.AdjustRight:= sbm.ParOffset=0;
  Invalidate
end;


function TPlusGutter.GetBookmarkFromLine(ALine: Longint): SmallInt;
var i: Integer;
begin
  { Iterate through bookmarks and return the bookmark number for ALine or -1 }
  Result := -1;
  for i := Low(TBookmarkRange) to High(TBookmarkRange) do
    if (fBookmarkList[i] <> nil) and (fBookmarkList[i].VisibleLineNumber = ALine) then
      begin
        Result := i;
        Break
      end;
end;


function TPlusGutter.getBookmarks(BookmarkIndex: TBookmarkRange): Longint;
begin
  if Assigned(fBookmarkList[BookmarkIndex]) then Result:= fBookmarkList[BookmarkIndex].TrueLineNumber
                                            else Result:= -1
end;

function TPlusGutter.getBookmarkList(BookmarkIndex: TBookmarkRange): TPlusNavigator;
begin
  Result:= fBookmarkList[BookmarkIndex]
end;

procedure TPlusGutter.ClearBookmarks;
var i: Integer;
begin
  for i:= Low(TBookmarkRange) to High(TBookmarkRange) do
      if BookmarksArray[i]>=0 then
        BookmarkLine(BookmarksArray[i], i)
end;

procedure TPlusGutter.JumpToBookmark(BookmarkIndex: TBookmarkRange);
begin
  if Assigned(fPlusMemo) and (BookmarksArray[BookmarkIndex]>=0) then
    begin
      fPlusMemo.SelStart:= fBookmarkList[BookmarkIndex].Pos;
      fPlusMemo.ScrollInView
    end
end;


procedure TPlusGutter.Notify(Sender: TComponent; Events: TpmEvents);
  function SameBookmarks: Boolean;
    var i: Integer;
    begin
      Result:= True;
      for i:= Low(TBookmarkRange) to High(TBookmarkRange) do
        if (fBookmarkList[i]<>nil) and (fBookmarkValues[i]<>fBookmarkList[i].TrueLineNumber) then
          begin
            Result:= False;
            Exit
          end
    end;

var ln, slinecount, sparcount: Integer;
begin
  if pmeAfterMessage in Events then MemoWndProc(fPlusMemo.WinMsg);
  if pmeFontChanged in Events then Invalidate;   // to replace lineheight and other internal values
  if pmeChange in Events then
    begin
      for ln:= 0 to High(TBookmarkRange) do
        if fBookmarkList[ln]<>nil then
          begin
            fBookmarkList[ln].AdjustRight:= fBookmarkList[ln].ParOffset=0;
            if not fKeepBookmarks then fBookmarkList[ln].Col:= 0 // reset it to start of line
          end;

      if fIgnoreLast then
         begin
           slinecount:= fPlusMemo.Lines.Count;
           sparcount:= fPlusMemo.Paragraphs.Count
         end
       else
         begin
           slinecount:= fPlusMemo.IParList.fVisibleLineCount;  //LineCount;
           sparcount:= fPlusMemo.ParagraphCount
         end;

       if (slinecount<>fLineCount) or (sparcount<>fParCount) or (not SameBookmarks) then
         begin
           fLineCount:= slinecount;
           fParCount:= sparcount;
           Invalidate
         end
     end;
  if pmeVScroll in Events then Scroll(Self)
end;

procedure TPlusGutter.setOnDrawItem(const Value: TpgDrawItemEvent);
begin
  fOnDrawItem := Value;
  if Assigned(fPlusMemo) then Invalidate
end;

procedure TPlusGutter.setKeepBookmarks(Value: Boolean);
var i: Integer;
begin
  if Value<>fKeepBookmarks then
    begin
      fKeepBookmarks:= Value;
      for i:= 0 to High(TBookmarkRange) do
          if fBookmarkList[i]<>nil then
            begin
              fBookmarkList[i].FreeOnDelete:= not Value;
              if not Value then
                begin
                  fBookmarkList[i].Col:= 0;  // adjust it to start of line
                  fBookmarkList[i].OnFree:= BookmarkFree
                end
              else
                  fBookmarkList[i].OnFree:= nil
            end
    end
end;

procedure TPlusGutter.setRightBorder(Value: Boolean);
begin
  if Value<>fRightBorder then
    begin
      fRightBorder:= Value;
      Invalidate
    end
end;

    { Event handler for bookmark navigators OnFree }
procedure TPlusGutter.BookMarkFree(Sender: TObject);
var i: Integer;
begin
  for i:= 0 to High(TBookmarkRange) do
    if fBookmarkList[i]=Sender then
      begin
        fBookmarkList[i]:= nil;
        Break
      end
end;

initialization
gBookmarkIcons:= nil;

finalization
gBookmarkIcons.Free;

end.
