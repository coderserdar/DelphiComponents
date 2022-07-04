{   Component(s):
    tcyColorGrid

    Description:
    It's a selection palette color control with selected color and hot color !!!
    
    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyColorGrid;

interface

uses Classes, Types, Controls, Graphics, Messages, Windows;

type
  TcyCursorRender=class(TPersistent)
  private
    FFrameWidth: Integer;
    FColor: TColor;
    FFrameColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameWidth(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property FrameColor: TColor read FFrameColor write SetFrameColor default clNavy;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth default 1;
    property Color: TColor read FColor write SetColor default $00FF8E8E;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBoxState = set of (bsHot, bsSelected);
  TProcOnPaintBox = procedure (Sender: TObject; aRect: TRect; aState: TBoxState; aRow: integer; aCol: integer; aColor: TColor) of object;
  TProcOnBoxClick = procedure (Sender: TObject; aRow: integer; aCol: integer; aColor: TColor) of object;
  TProcOnBoxDblClick = procedure (Sender: TObject; aRow: integer; aCol: integer; aColor: TColor) of object;

  TcyColorGrid = class(TGraphicControl)
    FColorList: TStrings;
  private
    FNeedFullRepaint: Boolean;
    FPartialPaintIndex: Integer;
    FptMouseUp: TPoint;
    FHot: Boolean;
    FHideSelection: Boolean;
    FBackground: TColor;
    FBoxWidth: integer;
    FBoxHeight: integer;
    FBoxRows: integer;
    FBoxCols: integer;
    FBoxSpacingWidth: integer;
    FBoxSpacingHeight: integer;
    FBoxFrameWidth: integer;
    FBoxFrameColor: TColor;
    FOnCustomDrawBox: TProcOnPaintBox;
    FOnAfterPaint: TNotifyEvent;
    FTransparent: Boolean;
    FOnCustomDrawBkgnd: TNotifyEvent;
    FOnBoxClick: TProcOnBoxClick;
    FReadOnly: Boolean;
    FSelectionRender: TcyCursorRender;
    FHotRender: TcyCursorRender;
    FOnBoxDblClick: TProcOnBoxDblClick;
    FSelectionIndex: Integer;
    FHotColorIndex: Integer;
    FPartialPaint: Boolean;
    FBorderWidth: Word;
    FOnChange: TNotifyEvent;
    procedure ColorListChange(Sender: TObject);
    procedure SetHot(const Value: Boolean);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetSelection(const Value: TColor);
    procedure SetBackground(const Value: TColor);
    procedure SetBoxWidth(const Value: integer);
    procedure SetBoxHeight(const Value: integer);
    procedure SetBoxRows(const Value: integer);
    procedure SetBoxCols(const Value: integer);
    procedure SetBoxSpacingWidth(const Value: integer);
    procedure SetBoxSpacingHeight(const Value: integer);
    procedure SetColorList(const Value: Tstrings);
    procedure SetBoxFrameWidth(const Value: integer);
    procedure SetBoxFrameColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetHotColor(const Value: TColor);
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetHotRender(const Value: TcyCursorRender);
    procedure SetSelectionRender(const Value: TcyCursorRender);
    procedure SubPropertiesChanged(Sender: TObject);
    function GetSelection: TColor;
    procedure SetSelectionIndex(const Value: Integer);
    function GetHotColor: TColor;
    procedure SetHotColorIndex(const Value: Integer);
    function GetIndexColors(Index: Integer): TColor;
    procedure SetIndexColors(Index: Integer; const Value: TColor);
    function GetBoxColors(aRow, aCol: Integer): TColor;
    procedure SetBoxColors(aRow, aCol: Integer; const Value: TColor);
    procedure SetBorderWidth(const Value: Word);
  protected
    procedure DblClick; override;
    procedure Loaded; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseUp procedure ...
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    // Old ... Replaced by property PartialPaint ... function  CanPartialPaint: Boolean; virtual;
    procedure DoDrawBkgnd;
    procedure DoDrawBoxes;
    procedure DoDrawBox(const BoxColor: TColor; BoxState: TBoxState; BoxRect: TRect; const Row, Column: Integer);
    procedure RedefineSize; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property HotColor: TColor read GetHotColor write SetHotColor;
    property HotColorIndex: Integer read FHotColorIndex write SetHotColorIndex default -1;
    property Colors[Index: Integer]: TColor read GetIndexColors write SetIndexColors; default;
    property BoxColors[aRow, aCol: Integer]: TColor read GetBoxColors write SetBoxColors;
    //
    function  AddColor(const Color: TColor; const CheckIfAlreadyExists: Boolean = false): Integer;
    procedure AddColors(const fromColor, toColor: TColor; const Count: Integer);
    procedure DefaultDrawBkgnd;
    //
    procedure DefaultDrawBox(aRect: TRect; const aState: TBoxState; const aRow, aCol: integer; const aColor: TColor);
    function  GeBoxState(const aRow, aCol: integer): TBoxState;
    function  PointInBox(const aPoint: TPoint; var aRow: Integer; var aCol: Integer): Boolean;
    function  GetBoxAtPos(const aPoint: TPoint; var aRow: Integer; var aCol: Integer; ExactPos: Boolean): Boolean;
    //
    function  GetColorFromIndex(aIndex: Integer; var aColor: TColor): Boolean;
    function  GetIndexFromColor(const aColor: TColor; const SearchFromIndex: Word = 0): Integer;
    //
    function  GetBoxFromIndex(const aIndex: Integer; var aRow, aCol: Integer): Boolean;
    function  GetIndexFromBox(aRow: Integer; aCol: Integer): integer;
    //
    function  GetBoxFromColor(const aColor: TColor; var aRow: Integer; var aCol: Integer): Boolean;
    function  GetColorBox(const aRow: Integer; aCol: integer; var aColor: TColor): Boolean;
    function  SetColorBox(const aRow: Integer; aCol: integer; aColor: TColor): Boolean;
  published
    property Align;
    property Autosize default true;
    property Anchors;
    property Constraints;
    property Enabled;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Background: TColor read FBackground write SetBackground default clWindow;
    property BorderWidth: Word read FBorderWidth write SetBorderWidth default 6;
    property BoxHeight: integer read FBoxHeight write SetBoxHeight default 12;
    property BoxWidth: integer read FBoxWidth write SetBoxWidth default 12;
    property BoxRows: integer read FBoxRows write SetBoxRows default 5;
    property BoxCols: integer read FBoxCols write SetBoxCols default 8;
    property BoxFrameColor: TColor read FBoxFrameColor write SetBoxFrameColor default clGray;
    property BoxFrameWidth: integer read FBoxFrameWidth write SetBoxFrameWidth default 1;
    property BoxSpacingWidth: integer read FBoxSpacingWidth write SetBoxSpacingWidth default 6;
    property BoxSpacingHeight: integer read FBoxSpacingHeight write SetBoxSpacingHeight default 6;
    property ColorList: Tstrings read FColorList write SetColorList;
    property Hot: Boolean read FHot write SetHot default false;
    property HotRender: TcyCursorRender read FHotRender write SetHotRender;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default false;
    property PartialPaint: Boolean read FPartialPaint write FPartialPaint default true;
    property Selection: TColor read GetSelection write SetSelection default clBlack;
    property SelectionIndex: Integer read FSelectionIndex write SetSelectionIndex default -1;
    property SelectionRender: TcyCursorRender read FSelectionRender write SetSelectionRender;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property OnCustomDrawBox: TProcOnPaintBox read FOnCustomDrawBox write FOnCustomDrawBox;
    property OnCustomDrawBkgnd: TNotifyEvent read FOnCustomDrawBkgnd write FOnCustomDrawBkgnd;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
    property OnBoxClick: TProcOnBoxClick read FOnBoxClick write FOnBoxClick;
    property OnBoxDblClick: TProcOnBoxDblClick read FOnBoxDblClick write FOnBoxDblClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TcyCursorRender }
constructor TcyCursorRender.Create(AOwner: TComponent);
begin
  FFrameColor := clNavy;
  FFrameWidth := 1;
  FColor      := $00FF8E8E;
end;

procedure TcyCursorRender.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyCursorRender.SetFrameColor(const Value: TColor);
begin
  FFrameColor := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyCursorRender.SetFrameWidth(const Value: Integer);
begin
  FFrameWidth := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TcyColorGrid }
constructor TcyColorGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorList := TStringList.Create;
  FColorList.BeginUpdate;
  FColorList.Add('clBlack');
  FColorList.Add('clMaroon');
  FColorList.Add('clGreen');
  FColorList.Add('clOlive');
  FColorList.Add('clNavy');
  FColorList.Add('clPurple');
  FColorList.Add('clTeal');
  FColorList.Add('clGray');
  FColorList.Add('clSilver');
  FColorList.Add('clRed');
  FColorList.Add('clLime');
  FColorList.Add('clYellow');
  FColorList.Add('clBlue');
  FColorList.Add('clFuchsia');
  FColorList.Add('clAqua');
  FColorList.Add('clWhite');
  FColorList.Add('clMoneyGreen');
  FColorList.Add('clSkyBlue');
  FColorList.Add('clCream');
  FColorList.Add('clMedGray');
  FColorList.Add('clActiveBorder');
  FColorList.Add('clActiveCaption');
  FColorList.Add('clAppWorkSpace');
  FColorList.Add('clBackground');
  FColorList.Add('clBtnFace');
  FColorList.Add('clBtnHighlight');
  FColorList.Add('clBtnShadow');
  FColorList.Add('clBtnText');
  FColorList.Add('clCaptionText');
  FColorList.Add('clDefault');
  FColorList.Add('clGrayText');
  FColorList.Add('clHighlight');
  FColorList.Add('clHighlightText');
  FColorList.Add('clInactiveBorder');
  FColorList.Add('clInactiveCaption');
  FColorList.Add('clInactiveCaptionText');
  FColorList.Add('clInfoBk');
  FColorList.Add('clInfoText');
  FColorList.Add('clMenu');
  FColorList.Add('clMenuText');
  FColorList.Add('clNone');
  FColorList.Add('clScrollBar');
  FColorList.Add('cl3DDkShadow');
  FColorList.Add('cl3DLight');
  FColorList.Add('clWindow');
  FColorList.Add('clWindowFrame');
  FColorList.Add('clWindowText');
  FColorList.EndUpdate;
  TStringList(FColorList).OnChange := ColorListChange;
  FHot := false;
  FHideSelection := false;
  FSelectionIndex := 0;
  FHotColorIndex := -1;
  FPartialPaint := true;
  FBackground := clWindow;
  FBoxHeight := 12;
  FBoxWidth := 12;
  FBoxRows := 5;
  FBoxCols := 8;
  FBorderWidth := 6;
  FBoxSpacingWidth := 6;
  FBoxSpacingHeight := 6;
  FBoxFrameWidth := 1;
  FBoxFrameColor := clGray;
  FReadOnly := false;
  FTransparent := false;
  FSelectionRender := TcyCursorRender.Create(self);
  FSelectionRender.OnChange := SubPropertiesChanged;
  FHotRender := TcyCursorRender.Create(self);
  FHotRender.OnChange := SubPropertiesChanged;
  Autosize:= true;
  RedefineSize;
end;

destructor TcyColorGrid.Destroy;
begin
  FSelectionRender.Free;
  FHotRender.Free;
  FColorList.Free;
  inherited Destroy;
end;

procedure TcyColorGrid.Loaded;         // !!! Not called for Dynamic controls !!!
begin
  Inherited;
  FHotColorIndex := -1;
end;

procedure TcyColorGrid.SubPropertiesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyColorGrid.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    inherited SetAutoSize(Value);  // Change Autosize value ...
    RedefineSize;
  end;
end;

procedure TcyColorGrid.RedefineSize;
begin
  if AutoSize then
  begin
    Width  := FBorderWidth * 2 + FBoxCols * FBoxWidth + (FBoxCols-1) * FBoxSpacingWidth;
    Height := FBorderWidth * 2 + FBoxRows * FBoxHeight + (FBoxRows-1) * FBoxSpacingHeight;
  end;
end;

procedure TcyColorGrid.SetBackground(const Value: TColor);
begin
  FBackground := Value;
  if (not FTransparent) and (FBorderWidth <> 0) then
    Invalidate;
end;

procedure TcyColorGrid.SetHot(const Value: Boolean);
begin
  FHot := Value;
  Invalidate;
end;

procedure TcyColorGrid.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxCols(const Value: integer);
begin
  FBoxCols := Value;

  RedefineSize;
  Invalidate;

  if FSelectionIndex <> -1 then
    if FSelectionIndex > (FBoxRows * FBoxCols) - 1 then
      SelectionIndex := -1;

  if FHotColorIndex <> -1 then
    if FHotColorIndex > (FBoxRows * FBoxCols) - 1 then
      HotColorIndex := -1;
end;

procedure TcyColorGrid.SetBoxFrameColor(const Value: TColor);
begin
  FBoxFrameColor := Value;
  if FBoxFrameWidth <> 0 then
    Invalidate;
end;

procedure TcyColorGrid.SetBoxFrameWidth(const Value: integer);
begin
  FBoxFrameWidth := Value;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxHeight(const Value: integer);
begin
  FBoxHeight := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxSpacingWidth(const Value: integer);
begin
  FBoxSpacingWidth := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxSpacingHeight(const Value: integer);
begin
  FBoxSpacingHeight := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxRows(const Value: integer);
begin
  FBoxRows := Value;

  RedefineSize;
  Invalidate;

  if FSelectionIndex <> -1 then
    if FSelectionIndex > (FBoxRows * FBoxCols) - 1 then
      SelectionIndex := -1;

  if FHotColorIndex <> -1 then
    if FHotColorIndex > (FBoxRows * FBoxCols) - 1 then
      HotColorIndex := -1;
end;

procedure TcyColorGrid.SetBoxWidth(const Value: integer);
begin
  FBoxWidth := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetColorList(const Value: Tstrings);
begin
  if Assigned(FColorList) then
    FColorList.Assign(Value)
  else
    FColorList := Value;
end;

procedure TcyColorGrid.SetHideSelection(const Value: Boolean);
begin
  FHideSelection := Value;
  Invalidate;
end;

procedure TcyColorGrid.ColorListChange(Sender: TObject);
begin
  // Set SelectionIndex HotColorIndex :
  if FSelectionIndex > FColorList.Count-1 then SelectionIndex := -1;
  if FHotColorIndex > FColorList.Count-1 then HotColorIndex := -1;

  Invalidate;
end;

function TcyColorGrid.AddColor(const Color: TColor; const CheckIfAlreadyExists: Boolean = false): Integer;
begin
  if CheckIfAlreadyExists then
    if GetIndexFromColor(Color) <> -1 then
    begin
      Result := -1;    // Returning -1 (instead of color index) allows to know if color was added !
      Exit;
    end;

  if FPartialPaint and Visible then
  begin
    TStringList(FColorList).OnChange := Nil;
    FColorList.Add( ColorToString(Color) );
    Result := FColorList.Count-1;
    TStringList(FColorList).OnChange := ColorListChange;
    FNeedFullRepaint := false;
    FPartialPaintIndex := Result;
    DoDrawBoxes;
  end
  else begin
    FColorList.Add( ColorToString(Color) );
    Result := FColorList.Count-1;
  end;
end;

procedure TcyColorGrid.Paint;
begin
  FNeedFullRepaint := true;
  DoDrawBkgnd;
  DoDrawBoxes;
  if Assigned(FOnAfterPaint) then FOnAfterPaint(Self);
end;

procedure TcyColorGrid.DoDrawBkgnd;
begin
  if Assigned(FOnCustomDrawBkgnd)
  then FOnCustomDrawBkgnd(Self)
  else DefaultDrawBkgnd;
end;

procedure TcyColorGrid.DefaultDrawBkgnd;
begin
  if not FTransparent then
  begin
    Canvas.Brush.Color := FBackground;
    Canvas.FillRect(classes.Rect(0, 0, Width, Height));
  end;
end;

procedure TcyColorGrid.DoDrawBoxes;
var
  curIndex, r, c, xPos, yPos: integer;
  BoxRect: TRect;
  aColor: TColor;
  StrColor: ShortString;
begin
  curIndex := -1;
  yPos := FBorderWidth;

  for r := 0 to FBoxRows -1 do
  begin
    xPos := FBorderWidth;

    for c := 0 to FBoxCols -1 do
    begin
      inc(curIndex, 1);

      if curIndex <= FColorList.Count-1 then
      begin
        StrColor := FColorList[CurIndex];

        if StrColor <> '' then
        begin
          aColor  := StringToColor(StrColor);

          if (FNeedFullRepaint) or (FPartialPaintIndex = curIndex) then
          begin
            BoxRect := classes.Rect(xPos, yPos, xPos + FBoxWidth, yPos + FBoxHeight);
            DoDrawBox(aColor, GeBoxState(r, c), BoxRect, r, c);
          end;
        end;
      end;

      Inc(xPos, FBoxWidth + FBoxSpacingWidth);
    end;

    Inc(yPos, FBoxHeight + FBoxSpacingHeight);
  end;
end;

procedure TcyColorGrid.DoDrawBox(const BoxColor: TColor; BoxState: TBoxState; BoxRect: TRect; const Row, Column: Integer);

        procedure DrawCellBkgnd;
        var
          BgndRect: TRect;
          cursorRender: TcyCursorRender;
          ShowSelection, ShowHot: Boolean;
          Spacing: Integer;
        begin
          BgndRect := BoxRect;
          ShowSelection := (bsSelected in BoxState) and (not FHideSelection);
          ShowHot := (bsHot in BoxState) and (FHot);

          // Check if we need to reduce BoxRect in order to show selection/hot state :
          if ShowSelection or ShowHot then
          begin
            if FBoxSpacingWidth > FBorderWidth
            then Spacing := FBorderWidth
            else Spacing := FBoxSpacingWidth;

            if Spacing <= 0
            then InflateRect(BoxRect, -1, 0)                // Reduce BoxRect
            else InflateRect(BgndRect, Spacing div 2, 0);   // Increase BgndRect

            if FBoxSpacingHeight > FBorderWidth
            then Spacing := FBorderWidth
            else Spacing := FBoxSpacingHeight;

            if Spacing <= 0
            then InflateRect(BoxRect, 0, -1)               // Reduce BoxRect
            else InflateRect(BgndRect, 0, Spacing div 2);  // Increase BgndRect

            if bsSelected in BoxState
            then cursorRender := FSelectionRender
            else cursorRender := FHotRender;

            if cursorRender.FFrameWidth > 0 then
            begin
              Canvas.Brush.Color := cursorRender.FrameColor;
              Canvas.FillRect(BgndRect);
              InflateRect(BgndRect, -cursorRender.FFrameWidth, -cursorRender.FFrameWidth);
            end;

            Canvas.Brush.Color := cursorRender.Color;
            Canvas.FillRect(BgndRect);
          end
          else begin
            InflateRect(BgndRect, FBoxSpacingWidth div 2, FBoxSpacingHeight div 2);
            Canvas.Brush.Color := FBackground;
            Canvas.FillRect(BgndRect);
          end;
        end;

begin
  // Draw cell background:
  if not FNeedFullRepaint then
    DrawCellBkgnd  // BoxState changed, always to be done!
  else
    if ((bsSelected in BoxState) and (not FHideSelection)) or ((bsHot in BoxState) and (FHot)) then
      DrawCellBkgnd;

  // Draw box color :
  if Assigned(FOnCustomDrawBox)
  then FOnCustomDrawBox(self, BoxRect, BoxState, Row, Column, BoxColor)
  else DefaultDrawBox(BoxRect, BoxState, Row, Column, BoxColor);
end;

procedure TcyColorGrid.DefaultDrawBox(aRect: TRect; const aState: TBoxState; const aRow, aCol: integer; const aColor: TColor);
var
  ShowSelection, ShowHot: Boolean;
begin
  if (FBoxHeight <= 2) or (FBoxWidth <= 2) then   // No space to draw color if selection/hot background ...
  begin
    ShowSelection := (bsSelected in aState) and (not FHideSelection);
    ShowHot := (bsHot in aState) and (FHot);

    if ShowSelection or ShowHot then
      exit;
  end;

  if FBoxHeight = 1 then
  begin
    if FBoxWidth = 1 then
    begin
      // Draw a point without frame :
      Canvas.Pixels[aRect.Left, aRect.Top] := aColor;
    end
    else begin
      // Draw horizontal line without frame :
      Canvas.Brush.Color := aColor;
      Canvas.FillRect(aRect);
    end;
  end
  else begin
    if FBoxWidth = 1 then
    begin
      // Draw vertical line without frame :
      Canvas.Brush.Color := aColor;
      Canvas.FillRect(aRect);
    end
    else
      case FBoxFrameWidth of
        0 : begin
              Canvas.Brush.Color := aColor;
              Canvas.FillRect(aRect);
            end;

        1 : begin
              Canvas.Brush.Color := aColor;
              Canvas.Pen.Width := FBoxFrameWidth;
              Canvas.Pen.Color := FBoxFrameColor;
              Canvas.Rectangle(aRect);
            end;

            else begin
              // Draw outer Rect :
              Canvas.Brush.Color := FBoxFrameColor;
              Canvas.FillRect(aRect);

              // Draw inner Rect :
              InflateRect(aRect, -FBoxFrameWidth, -FBoxFrameWidth);
              Canvas.Brush.Color := aColor;
              Canvas.FillRect(aRect);
            end;
      end;
  end;
end;

function TcyColorGrid.GetHotColor: TColor;
begin
  Result := clNone;
  if FHotColorIndex <> -1 then
    if FHotColorIndex <= FColorList.Count-1 then
      if FColorList[FHotColorIndex] <> '' then
        Result := StringToColor(FColorList[FHotColorIndex]);
end;

function TcyColorGrid.GetSelection: TColor;
begin
  Result := clNone;
  if FSelectionIndex <> -1 then
    if FSelectionIndex <= FColorList.Count-1 then
      if FColorList[FSelectionIndex] <> '' then
        Result := StringToColor(FColorList[FSelectionIndex]);
end;

function TcyColorGrid.GeBoxState(const aRow, aCol: integer): TBoxState;
begin
  Result := [];

  if (GetIndexFromBox(aRow, aCol) = FSelectionIndex) and (FSelectionIndex <> -1) then
    Include(Result, bsSelected);

  if (GetIndexFromBox(aRow, aCol) = FHotColorIndex) and (FHotColorIndex <> -1) then
    Include(Result, bsHot);
end;

procedure TcyColorGrid.SetSelectionIndex(const Value: Integer);
begin
  FPartialPaintIndex := FSelectionIndex;
  FSelectionIndex := Value;

  if not FHideSelection then
    if FPartialPaint and Visible then                // Avoid full repaint with blinking effect ...
    begin
      FNeedFullRepaint := false;
      DoDrawBoxes;                         // Repaint old SelectionIndex ...

      FPartialPaintIndex := FSelectionIndex;
      DoDrawBoxes;                         // Paint new SelectionIndex ...
    end
    else
      Invalidate;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TcyColorGrid.SetHotColorIndex(const Value: Integer);
begin
  FPartialPaintIndex := FHotColorIndex;
  FHotColorIndex := Value;

  if FHot then
    if FPartialPaint and Visible then      // Avoid full repaint with blinking effect ...
    begin
      FNeedFullRepaint := false;

      if FPartialPaintIndex <> -1 then
        DoDrawBoxes;

      FPartialPaintIndex := FHotColorIndex;
      DoDrawBoxes;
    end
    else
      Invalidate;
end;

procedure TcyColorGrid.SetSelection(const Value: TColor);
begin
  if Value <> Selection then
    SelectionIndex := GetIndexFromColor(Value);
end;

procedure TcyColorGrid.SetHotColor(const Value: TColor);
begin
  if (Value <> HotColor) or (not FHotColorIndex = -1) then
    HotColorIndex := GetIndexFromColor(Value);
end;

procedure TcyColorGrid.SetSelectionRender(const Value: TcyCursorRender);
begin
  FSelectionRender := Value;
end;

procedure TcyColorGrid.SetHotRender(const Value: TcyCursorRender);
begin
  FHotRender := Value;
end;

procedure TcyColorGrid.SetIndexColors(Index: Integer; const Value: TColor);
var
  aRow, aCol: Integer;
begin
  if GetBoxFromIndex(Index, aRow, aCol) then
    SetColorBox(aRow, aCol, Value);
end;

procedure TcyColorGrid.SetBorderWidth(const Value: Word);
begin
  FBorderWidth := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxColors(aRow, aCol: Integer; const Value: TColor);
begin
  SetColorBox(aRow, aCol, Value);
end;

procedure TcyColorGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  aRow, aCol: integer;
begin
  if FHot then
    if GetBoxAtPos(Point(X, Y), aRow, aCol, true)
    then HotColorIndex := GetIndexFromBox(aRow, aCol)
    else HotColorIndex := -1;

  inherited;
end;

procedure TcyColorGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aRow, aCol: integer;
  aColor: TColor;
begin
  FptMouseUp := Point(X, Y);

  // Moved here in order to call FOnBoxClick() from where we release mouse button :
  if GetBoxAtPos(FptMouseUp, aRow, aCol, true) then
  begin
    if not FReadOnly then
      SelectionIndex := GetIndexFromBox(aRow, aCol);

    if Assigned(FOnBoxClick) then
      if GetColorFromIndex(FSelectionIndex, aColor) then
        FOnBoxClick(Self, aRow, aCol, aColor);
  end;


  inherited;
end;

procedure TcyColorGrid.DblClick;
var
  aRow, aCol: integer;
  aColor: TColor;
begin
  if GetBoxAtPos(FptMouseUp, aRow, aCol, true) then
  begin
    if not FReadOnly then
      SelectionIndex := GetIndexFromBox(aRow, aCol);

    if Assigned(FOnBoxDblClick) then
      if GetColorFromIndex(FSelectionIndex, aColor) then
        FOnBoxDblClick(Self, aRow, aCol, aColor);
  end;

  inherited;
end;

procedure TcyColorGrid.CmMouseLeave(var Msg: TMessage);
begin
  if FHotColorIndex <> -1 then
    HotColorIndex := -1;

  inherited;
end;

function  TcyColorGrid.GetColorFromIndex(aIndex: Integer; var aColor: TColor): Boolean;
begin
  Result := false;

  if aIndex >= 0 then
    if aIndex < FColorList.Count then
      if FColorList[aIndex] <> '' then
        try
          aColor := StringToColor(FColorList[aIndex]);
          Result := true;
        finally
        end;
end;

function  TcyColorGrid.GetIndexFromColor(const aColor: TColor; const SearchFromIndex: Word = 0): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := SearchFromIndex to FColorList.Count-1 do
    if FColorList[i] <> '' then
      if aColor = StringToColor(FColorList[i]) then  // Only compare TColor because clRed can be written in different ways
      begin
        Result := i;
        Break;
      end;
end;

function TcyColorGrid.GetBoxFromColor(const aColor: TColor; var aRow, aCol: Integer): Boolean;
var
  i: Integer;
begin
  Result := false;

  for i := 0 to FColorList.Count-1 do
    if FColorList[i] <> '' then
      if aColor = StringToColor(FColorList[i]) then  // Only compare TColor because clRed can be written in different ways
      begin
        Result := true;
        aRow   := i div FBoxCols;
        aCol   := i - (aRow * FBoxCols);
      end;
end;

function TcyColorGrid.GetBoxFromIndex(const aIndex: Integer; var aRow, aCol: Integer): Boolean;
begin
  Result := false;

  if (aIndex >= 0) and (aIndex <= FColorList.Count - 1) then
  begin
    Result := true;
    aRow   := aIndex div FBoxCols;
    aCol   := aIndex - (aRow * FBoxCols);
  end;
end;

function TcyColorGrid.GetIndexColors(Index: Integer): TColor;
begin
  GetColorFromIndex(Index, Result);
end;

function TcyColorGrid.GetBoxColors(aRow, aCol: Integer): TColor;
begin
  GetColorBox(aRow, aCol, Result);
end;

function  TcyColorGrid.GetIndexFromBox(aRow: Integer; aCol: Integer): integer;
begin
  Result := -1;

  if (ARow <= FBoxRows-1) and (ACol <= FBoxCols-1) then
    Result := (ARow * FBoxCols) + ACol;
end;

function  TcyColorGrid.GetColorBox(const aRow: Integer; aCol: integer; var aColor: TColor): Boolean;
begin
  Result := GetColorFromIndex( GetIndexFromBox(aRow, aCol), aColor );
end;

function  TcyColorGrid.SetColorBox(const aRow: Integer; aCol: integer; aColor: TColor): Boolean;
var
  ColorListIndex, i, x, y: integer;
  BoxRect: TRect;
begin
  Result := false;
  ColorListIndex := GetIndexFromBox(aRow, aCol);

  if ColorListIndex <> -1 then
  begin
    TStringList(FColorList).OnChange := nil;

    for i := (FColorList.Count-1) to ColorListIndex do
      FColorList.Add('');

    if FPartialPaint and Visible then
    begin
      FColorList[ColorListIndex] := ColorToString(aColor);
      x := FBorderWidth + (FBoxWidth + FBoxSpacingWidth) * aCol;
      y := FBorderWidth + (FBoxHeight + FBoxSpacingHeight) * aRow;
      BoxRect := classes.Rect(x, y, x + FBoxWidth, y + FBoxHeight);
      FNeedFullRepaint := false;
      DoDrawBox(aColor, GeBoxState(aRow, aCol), BoxRect, aRow, aCol);
    end
    else begin
      FColorList[ColorListIndex] := ColorToString(aColor);
      Invalidate;
    end;

    TStringList(FColorList).OnChange := ColorListChange;
    Result := true;
  end;
end;

function TcyColorGrid.PointInBox(const aPoint: TPoint; var aRow: Integer; var aCol: Integer): Boolean;
begin
  Result := false;

  if aPoint.X >= FBorderWidth + (FBoxWidth + FBoxSpacingWidth) * aCol then
    if aPoint.X <= FBorderWidth + (FBoxWidth + FBoxSpacingWidth) * (aCol + 1) then
      if aPoint.Y >= FBorderWidth + (FBoxHeight + FBoxSpacingHeight) * aRow then
        if aPoint.Y <= FBorderWidth + (FBoxHeight + FBoxSpacingHeight) * (aRow + 1) then
          Result := true;
end;

function  TcyColorGrid.GetBoxAtPos(const aPoint: TPoint; var aRow: Integer; var aCol: Integer; ExactPos: Boolean): Boolean;

    function VerifyIntervals: Boolean;
    begin
      Result := false;

      if (aCol >= 0) and (aCol < FBoxCols) then
        if (aRow >= 0) and (aRow < FBoxRows) then
          Result := true;
    end;

begin
  Result := false;
  aCol := (aPoint.X - FBorderWidth + FBoxSpacingWidth div 2) div (FBoxWidth + FBoxSpacingWidth);
  aRow := (aPoint.Y - FBorderWidth + FBoxSpacingHeight div 2) div (FBoxHeight + FBoxSpacingHeight);

  if ExactPos then
  begin
    if PointInBox(aPoint, aRow, aCol) then
      Result := VerifyIntervals;
  end
  else
    Result := VerifyIntervals;
end;

procedure TcyColorGrid.AddColors(const fromColor, toColor: TColor; const Count: Integer);
var
  i: Integer;
  fromColorRGB, toColorRGB: Integer;
  Arr_StartRGB: Array[0..2] of Byte;
  Arr_DifRGB  : Array[0..2] of integer;
  Arr_CurRGB  : Array[0..2] of Byte;
begin
  fromColorRGB := ColorToRGB(fromColor);
  toColorRGB   := ColorToRGB(toColor);

  Arr_StartRGB[0] := GetRValue(fromColorRGB);
  Arr_StartRGB[1] := GetGValue(fromColorRGB);
  Arr_StartRGB[2] := GetBValue(fromColorRGB);

  Arr_DifRGB[0] := GetRValue(toColorRGB) - Arr_StartRGB[0] ;
  Arr_DifRGB[1] := GetGValue(toColorRGB) - Arr_StartRGB[1] ;
  Arr_DifRGB[2] := GetBValue(toColorRGB) - Arr_StartRGB[2] ;

  for i := 1 to Count do
  begin
    Arr_CurRGB[0] := Arr_StartRGB[0] + MulDiv(i, Arr_DifRGB[0], Count);
    Arr_CurRGB[1] := Arr_StartRGB[1] + MulDiv(i, Arr_DifRGB[1], Count);
    Arr_CurRGB[2] := Arr_StartRGB[2] + MulDiv(i, Arr_DifRGB[2], Count);

    AddColor(RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]), false);
  end;
end;

end.
