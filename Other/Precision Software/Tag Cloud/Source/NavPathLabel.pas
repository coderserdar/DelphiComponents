{------------------------------------------------------------------------------
  NavPathLabel.pas

  NavPathLabel for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Simple path info and path navigation component with basic
              interactivity support. Derived from TCustomLabel.

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

  Version 1.8 (15.3.2012)
  - first public release
}

{ Implementation of TNavPathLabel component. }
unit NavPathLabel;

interface

uses
  Classes, Graphics, Windows, Messages, Controls, StdCtrls;

type
  { Event handler that allows you to perform custom actions when user clicks the path item.
    Path argument contains the path from start to clicked item, aRect is pre-filled with drawing rectangle
    of that item. Inside the event handler you can use also other TNavPathLabel properties and functions
    to handle clicked item, like HoverPathIndex, GetPathItemAtIndex, etc.}
  TNavPathClickEvent = procedure(Sender:TObject; const Path:string; const aRect:TRect) of object;

  { TNavPathLabel class implementation. Simple path info and path navigation component with basic interactivity support. }
  TNavPathLabel = class(TCustomLabel)
  private
    FPathDelimiter: string;
    FNavPathDelimiter: string;
    FHoverPathIdx: Integer;
    FPathItems: TStringList;
    FNavPathColor: TColor;
    FHoverColor: TColor;
    FHoverCursor: TCursor;
    FCmpCursor: TCursor;
    FHoverStyle: TFontStyles;
    FOnPathClick: TNavPathClickEvent;
    FRebuilding: Boolean;
    FAlignmentOld: TAlignment;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure SetHoverPathIdx(Value:Integer);
    procedure SetHoverStyle(const Value: TFontStyles);
    procedure SetNavPathColor(const Value: TColor);
    procedure SetPathDelimiter(const Value:string);
    procedure SetNavPathDelimiter(const Value:string);
  protected
    // Used internally to clear the array of path items
    procedure ClearPathItems;
    // General method to draw the path items
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    // Returns an index of path item at XY client coordinats. Used internally.
    function GetPathIndexAt(X, Y:Integer):Integer;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    // This method prepares path items and their internall properties from defined Caption and PathDelimiter.
    procedure Rebuild(Redraw:Boolean=True); virtual;
    procedure Resize; override;
  public
    // Component constructor
    constructor Create(AOwner: TComponent); override;
    // Component destructor
    destructor Destroy; override;
    // Returns the part of current path from beginning to the given path item index
    function GetPathAtIndex(Index:Integer):string;
    // Returns the part of current path from beginning to path item that is displayed at XY client coordinates
    function GetPathAtXY(X, Y:Integer):string;
    // Returns the path item by given index
    function GetPathItemAtIndex(Index:Integer):string;
    // Contains an index of currently hovered path item.
    property HoverPathIndex: Integer read FHoverPathIdx write SetHoverPathIdx;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    { Use this property to get or set the current path. Individual path items must be delimited by PathDelimiter.
      Displayed delimiter is controled by NavPathDelimiter property. }
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IF CompilerVersion>=17}
    property EllipsisPosition;
    {$IFEND}
    property Enabled;
    property FocusControl;
    property Font;
    {$IF CompilerVersion>=20}
    property GlowSize;
    {$IFEND}
    // Color of hovered path items
    property HoverColor: TColor read FHoverColor write FHoverColor default clNone;
    // Mouse cursor of hovered path items
    property HoverCursor: TCursor read FHoverCursor write FHoverCursor default crHandPoint;
    // Font style of hovered path items
    property HoverStyle: TFontStyles read FHoverStyle write SetHoverStyle;
    { Color of displayed path delimiter. You can control the style of displayed path delimiter by NavPathDelimiter property.
      Another property, PathDelimiter, is used to determine path items from the given path, defined in Caption property. }
    property NavPathColor: TColor read FNavPathColor write SetNavPathColor default clNone;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    { PathDelimiter, is used to determine path items from the given path, defined in Caption property. }
    property PathDelimiter: string read FPathDelimiter write SetPathDelimiter;
    { By this property you can control the style of displayed path delimiters. You can set any string value here,
      but the following characters are reserved for internally supported delimiter styles:
        'a' - filled arrow,
        #187 - double arrow (&raquo;),
        #171 - mirrored double arrow (&laquo),
        #155 - arrow (&rsaquo),
        #139 - mirrored arrow (&lsaquo).
      Please, do not confuse NavPathDelimiter with PathDelimiter property, that is used to determine path items from the given path,
      defined in Caption property. }
    property NavPathDelimiter: string read FNavPathDelimiter write SetNavPathDelimiter;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IF CompilerVersion>=17}
    property OnMouseActivate;
    {$IFEND}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IF CompilerVersion>=18}
    property OnMouseEnter;
    property OnMouseLeave;
    {$IFEND}
    { Event handler that allows you to perform custom actions when user clicks the path item.
      Path argument contains the path from start to clicked item, aRect is pre-filled with drawing rectangle
      of that item. Inside the event handler you can use also other TNavPathLabel properties and functions
      to handle clicked item, like HoverPathIndex, GetPathItemAtIndex, etc.}
    property OnPathClick: TNavPathClickEvent read FOnPathClick write FOnPathClick;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  SysUtils;

constructor TNavPathLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPathDelimiter := '\';
  FNavPathDelimiter := '  a  '; // arrow
  FPathItems := TStringList.create;
  FHoverPathIdx := -1;
  FHoverColor := clNone;
  FNavPathColor := clNone;
  FHoverCursor := crHandPoint;
  FHoverStyle := Font.Style + [fsUnderline];
  FCmpCursor := Cursor;
  FRebuilding := False;
  FAlignmentOld := Alignment;
end;

destructor TNavPathLabel.Destroy;
begin
  ClearPathItems;
  FPathItems.Free;
  inherited;
end;

procedure TNavPathLabel.CMTextChanged(var Message: TMessage);
begin
  Rebuild;
  inherited;
end;

procedure TNavPathLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Rebuild;
end;

procedure TNavPathLabel.CMBiDiModeChanged(var Message: TMessage);
begin
  Rebuild;
  inherited;
end;

procedure TNavPathLabel.Loaded;
begin
  inherited Loaded;
  Rebuild;
end;

procedure TNavPathLabel.SetHoverStyle(const Value: TFontStyles);
begin
  if FHoverStyle<>Value then
  begin
    FHoverStyle:=Value;
    Rebuild;
  end;
end;

procedure TNavPathLabel.SetNavPathColor(const Value: TColor);
begin
  if FNavPathColor<>Value then
  begin
    FNavPathColor:=Value;
    Invalidate;
  end;
end;

procedure TNavPathLabel.SetPathDelimiter(const Value:string);
begin
  if Value <> FPathDelimiter then
  begin
    FPathDelimiter := Value;
    Rebuild;
  end;
end;

procedure TNavPathLabel.SetNavPathDelimiter(const Value:string);
begin
  if Value <> FNavPathDelimiter then
  begin
    FNavPathDelimiter := Value;
    Rebuild;
  end;
end;

procedure TNavPathLabel.Resize;
begin
  Rebuild;
  inherited;
end;

procedure TNavPathLabel.ClearPathItems;
var
  i:Integer;
begin
  FHoverPathIdx := -1;
  for i:=0 To FPathItems.Count-1 do
    dispose(PRect(FPathItems.Objects[i]));
  FPathItems.Clear;
end;

procedure TNavPathLabel.Rebuild(Redraw:Boolean=True);
var
  piRect: PRect;
  Rect: TRect;
  p,l,nw,fw: Integer;
  tmp,itext: string;
  rtl:Boolean;
begin
  if (Parent=nil) or (csLoading in ComponentState) or FRebuilding then
    Exit;
  try
    FRebuilding := True;
    ClearPathItems;
    if FPathDelimiter <> '' then
    begin
      rtl:=UseRightToLeftAlignment;
      fw := 0;
      l := Length(FPathDelimiter) - 1;
      Canvas.Font:=Font;
      nw := Canvas.TextWidth(FNavPathDelimiter); // nav.delimiter width
      Rect := ClientRect;
      tmp := Caption;
      p := Pos(FPathDelimiter, tmp);
      while p>0 do
      begin
        itext:=Copy(tmp,1,p-1);
        New(piRect);
        piRect^:=Rect;
        if rtl then
        begin
          piRect^.Left:=piRect^.Right-Canvas.TextWidth(itext);
          Rect.Right:=piRect^.Left - nw;
        end
        else
        begin
          piRect^.Right:=piRect^.Left+Canvas.TextWidth(itext);
          Rect.Left:=piRect^.Right + nw;
        end;
        fw:=fw+(piRect^.Right-piRect^.Left)+nw;
        FPathItems.AddObject(itext,TObject(piRect));
        Delete(tmp, 1, p+l);
        p := Pos(FPathDelimiter, tmp);
      end;
      if Length(tmp)>0 then
      begin
        New(piRect);
        piRect^:=Rect;
        if rtl then
          piRect^.Left:=piRect^.Right-Canvas.TextWidth(tmp)
        else
          piRect^.Right:=piRect^.Left+Canvas.TextWidth(tmp);
        fw:=fw+piRect^.Right-piRect^.Left;
        FPathItems.AddObject(tmp,TObject(piRect));
      end;
      if Alignment in [taCenter,taRightJustify] then
      begin
        Rect:=ClientRect;
        fw:=Rect.Right-Rect.Left-fw;
        if Alignment=taCenter then
          fw:=fw div 2;
        if rtl then
          fw:=-fw;

        for p:=0 To FPathItems.Count-1 do
          OffsetRect(PRect(FPathItems.Objects[p])^,fw,0);
      end;
    end;
  finally
    if Redraw then
      Invalidate;
    FRebuilding := False;
  end;
end;

procedure TNavPathLabel.CMMouseLeave(var Msg: TMessage);
begin
  if FHoverPathIdx >= 0 then
    HoverPathIndex := -1;
  inherited;
end;

procedure TNavPathLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FHoverPathIdx = -1) or (not PtInRect(PRect(FPathItems.Objects[FHoverPathIdx])^,Point(x,y))) then
    HoverPathIndex := GetPathIndexAt(X,Y);
end;

procedure TNavPathLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
  if (Button=mbLeft) and Assigned(FOnPathClick) and (FHoverPathIdx>=0) then
    FOnPathClick(Self,GetPathAtIndex(FHoverPathIdx),PRect(FPathItems.Objects[FHoverPathIdx])^);
end;

function TNavPathLabel.GetPathIndexAt(X, Y:Integer):Integer;
var
  i:Integer;
  p:TPoint;
begin
  Result:=-1;
  if FPathItems.Count>0 then
  begin
    p.x:=x; p.y:=y;
    for i:=0 To FPathItems.Count-1 do
      if PtInRect(PRect(FPathItems.Objects[i])^,p) then
      begin
        Result:=i;
        break;
      end;
  end;
end;

function TNavPathLabel.GetPathAtIndex(Index:Integer):string;
var
  y: Integer;
begin
  if Length(FPathDelimiter)=0 then
    Result:=Caption
  else
  begin
    Result:='';
    if (Index>=0) and (Index<FPathItems.Count) then
      for y:=0 To Index do
        Result:=Result+FPathItems[y]+FPathDelimiter;
  end;
end;

function TNavPathLabel.GetPathAtXY(X, Y:Integer):string;
begin
  Result:=GetPathAtIndex(GetPathIndexAt(x,y));
end;

function TNavPathLabel.GetPathItemAtIndex(Index:Integer):string;
begin
  if Length(FPathDelimiter)=0 then
    Result:=Caption
  else
  begin
    Result:='';
    if (Index>=0) and (Index<FPathItems.Count) then
      Result:=FPathItems[Index];
  end;
end;

procedure TNavPathLabel.SetHoverPathIdx(Value:Integer);
begin
  if Value<>FHoverPathIdx then
  begin
    FHoverPathIdx:=Value;
    if FHoverPathIdx>=0 then
    begin
      if Cursor<>FHoverCursor then
        FCmpCursor:=Cursor;
      Cursor:=FHoverCursor;
    end
    else
      Cursor:=FCmpCursor;
    Invalidate;
  end;
end;

procedure TNavPathLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: string;
  i,nw: Integer;
  iRect: TRect;
  grNavDelim: smallint; // nav type: 0 - string, 1 - arrow, 2..X - special string
  sNavPD:char;
  rtl:Boolean;

  procedure _DrawNavDelimiter;
  var
    R: TRect;
    dh: Integer;
  begin
    if rtl then
      setRect(R,iRect.Left-nw-1,iRect.Top,iRect.Left,iRect.Bottom)
    else
      setRect(R,iRect.Right,iRect.Top,iRect.Right+nw+1,iRect.Bottom);
    if grNavDelim=0 then
    begin
      if FNavPathColor <> clNone then
        Canvas.Font.Color := FNavPathColor;
      {$IF CompilerVersion>=20}
      Windows.DrawTextW(Canvas.Handle, FNavPathDelimiter, Length(FNavPathDelimiter), R, DT_CENTER or DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE);
      {$ELSE}
      Windows.DrawText(Canvas.Handle, PChar(FNavPathDelimiter), Length(FNavPathDelimiter), R, DT_CENTER or DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE);
      {$IFEND}
      if FNavPathColor <> clNone then
        Canvas.Font.Color := Font.Color;
    end
    else
    begin
      if FNavPathColor = clNone then
        Canvas.Pen.Color := Canvas.Font.Color
      else
        Canvas.Pen.Color := FNavPathColor;
      case grNavDelim of
       -1:begin
            dh:=(abs(Canvas.Font.Height) div 2 + 1) div 2;
            R.Top:=(R.Bottom-R.Top+1) div 2 - dh;
            R.Right:=R.Right-(nw div 2)+(dh div 2)-1;
            Canvas.Brush.Color := Canvas.Pen.Color;
            Canvas.Polygon([Point(R.Right, R.Top), Point(R.Right-dh, R.Top+dh), Point(R.Right, R.Top+dh*2)]);
            Canvas.Brush.Style:=bsClear;
          end;
        2:begin
            Canvas.Font.Size:=Canvas.Font.Size+5;
            if FNavPathColor <> clNone then
              Canvas.Font.Color := FNavPathColor;
            Dec(r.Top,abs(Canvas.Font.Height-Font.Height) div 2 - 1);
            {$IF CompilerVersion>=20}
            Windows.DrawTextW(Canvas.Handle, sNavPD, 1, R, DT_CENTER or DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE);
            {$ELSE}
            Windows.DrawText(Canvas.Handle, PChar(sNavPD), 1, R, DT_CENTER or DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE);
            {$IFEND}
            Canvas.Font.Size:=Canvas.Font.Size-5;
            if FNavPathColor <> clNone then
              Canvas.Font.Color := Font.Color;
          end;
      else
        begin
          dh:=(abs(Canvas.Font.Height) div 2 + 1) div 2;
          if dh>=2 then
          begin
            R.Top:=(R.Bottom-R.Top+1) div 2 - dh;
            R.Left:=R.Left+(nw div 2)-(dh div 2)-1;
            Canvas.Brush.Color := Canvas.Pen.Color;
            Canvas.Polygon([Point(R.Left, R.Top), Point(R.Left+dh, R.Top+dh), Point(R.Left, R.Top+dh*2)]);
            Canvas.Brush.Style:=bsClear;
          end;
        end;
      end;
    end;
  end;

begin
  if (Length(FPathDelimiter)=0) or (Flags and DT_CALCRECT <> 0) then
    inherited DoDrawText(Rect, Flags)
  else
  begin
    if FAlignmentOld<>Alignment then
    begin
      FAlignmentOld := Alignment;
      Rebuild(False);
    end;
    Rect := ClientRect;
    Canvas.Font := Font;
    Flags := Flags or DT_NOCLIP or DT_VCENTER or DT_SINGLELINE or DT_LEFT or DT_NOPREFIX and not DT_EXPANDTABS;
    Flags := DrawTextBiDiModeFlags(Flags);
    rtl := UseRightToLeftAlignment;

    grNavDelim:=0;
    Text:=lowercase(trim(FNavPathDelimiter));
    if Length(Text)=1 then
    begin
      sNavPD:=Text[1];
      case sNavPD of
        'a':if rtl then grNavDelim:=-1 else grNavDelim:=1;
        #187:begin grNavDelim:=2; if rtl then sNavPD:=#171; end;  // &raquo;
        #171:begin grNavDelim:=2; if rtl then sNavPD:=#187; end;  // &laquo;
        #155:begin grNavDelim:=2; if rtl then sNavPD:=#139; end;  // &rsaquo;
        #139:begin grNavDelim:=2; if rtl then sNavPD:=#155; end;  // &lsaquo;
      end;
    end;

    nw := Canvas.TextWidth(FNavPathDelimiter);
    for i:=0 To FPathItems.Count-1 do
    begin
      Text := FPathItems[i];
      if Text <> '' then
      begin
        if i=FHoverPathIdx then
        begin
          Canvas.Font.Style:=FHoverStyle;
          if FHoverColor <> clNone then
            Canvas.Font.Color:=FHoverColor;
        end;
        iRect := PRect(FPathItems.Objects[i])^;
        if not Enabled then
        begin
          OffsetRect(iRect, 1, 1);
          Canvas.Font.Color := clBtnHighlight;
          {$IF CompilerVersion>=20}
          Windows.DrawTextW(Canvas.Handle, Text, Length(Text), iRect, Flags);
          {$ELSE}
          Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), iRect, Flags);
          {$IFEND}
          OffsetRect(iRect, -1, -1);
          Canvas.Font.Color := clBtnShadow;
          {$IF CompilerVersion>=20}
          Windows.DrawTextW(Canvas.Handle, Text, Length(Text), iRect, Flags);
          {$ELSE}
          Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), iRect, Flags);
          {$IFEND}
        end
        else
          {$IF CompilerVersion>=20}
          Windows.DrawTextW(Canvas.Handle, Text, Length(Text), iRect, Flags);
          {$ELSE}
          Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), iRect, Flags);
          {$IFEND}
        if i=FHoverPathIdx then
        begin
          Canvas.Font.Style:=Font.Style;
          if FHoverColor <> clNone then
            Canvas.Font.Color:=Font.Color;
        end;
        if i<FPathItems.Count-1 then
          _DrawNavDelimiter;
      end;
    end;
  end;
end;

end.

