// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  22490: mcmColorGrid.pas 
//
//    Rev 1.3    2014-02-02 21:09:52  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.2    30-01-2004 19:56:00  mcm    Version: IMG 2.3
// Included additional alignment options and brush property to control
// color-cells border. Improved design-time presentation. 

//
//   Rev 1.1    22-12-2003 16:00:42  mcm

//
//   Rev 1.0    05-12-2003 16:27:16  mcm    Version: IMG 2.2

unit mcmColorGrid;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, Classes, Graphics, Forms, Controls, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Controls, Vcl.Graphics,
      Vcl.Forms, Vcl.ExtCtrls,
     {$ENDIF}
     mcmImage;

const
  NumPaletteEntries = 256;

  RedVals : array[0..256 - 1] of Byte =
    (  0,  26,  51,  77, 102, 128, 153, 179,
     204, 230, 255,   0, 127, 255, 255, 255,
     255, 255, 127,   0,   0,   0,   0,  60,
      51, 102, 102, 102, 102,  51,   0,   0,

       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,

      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,

      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,

      77,  77,  77,  77,  77,  77,  77,  77,
      77,  77,  77,  77,  77,  77,  77,  77,
     179, 179, 179, 179, 179, 179, 179, 179,
     179, 179, 179, 179, 179, 179, 179, 179,

      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,

     204, 204, 204, 204, 204, 204, 204, 204,
     102, 102, 102, 102, 102, 102, 102, 102,
     204, 204, 204, 204, 204, 204, 204, 204,
     102, 102, 102, 102, 102, 102, 102, 102,

     204, 204, 204, 204, 204, 204, 204, 204,
     102, 102, 102, 102, 102, 102, 102, 102,
     204, 204, 204, 204, 204, 204, 204, 204,
     102, 102, 102, 102, 102, 102, 102, 102);


  GreenVals : array[0..256 - 1] of Byte =
    (  0,  26,  51,  77, 102, 128, 153, 179,
     204, 230, 255,   0,   0,   0,   0,   0,
     127, 255, 255, 255, 255, 255, 127,  30,
       0,   0,   0,  51, 102, 102, 102, 102,

      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,

       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,

     204, 204, 204, 204, 204, 204, 204, 204,
     153, 153, 153, 153, 153, 153, 153, 153,
     102, 102, 102, 102, 102, 102, 102, 102,
      51,  51,  51,  51,  51,  51,  51,  51,

      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,

     204, 204, 204, 204, 204, 204, 204, 204,
     102, 102, 102, 102, 102, 102, 102, 102,
     204, 204, 204, 204, 204, 204, 204, 204,
     102, 102, 102, 102, 102, 102, 102, 102,

      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,

      77,  77,  77,  77,  77,  77,  77,  77,
      77,  77,  77,  77,  77,  77,  77,  77,
     179, 179, 179, 179, 179, 179, 179, 179,
     179, 179, 179, 179, 179, 179, 179, 179);



  BlueVals : array[0..256 - 1] of Byte =
    (  0,  26,  51,  77, 102, 128, 153, 179,
     204, 230, 255, 255, 255, 255, 127,   0,
       0,   0,   0,   0, 127, 255, 255,  30,
     102, 102,  51,   0,   0,   0,  51, 102,

     204, 204, 204, 204, 204, 204, 204, 204,
     153, 153, 153, 153, 153, 153, 153, 153,
     102, 102, 102, 102, 102, 102, 102, 102,
      51,  51,  51,  51,  51,  51,  51,  51,

     204, 204, 204, 204, 204, 204, 204, 204,
     153, 153, 153, 153, 153, 153, 153, 153,
     102, 102, 102, 102, 102, 102, 102, 102,
      51,  51,  51,  51,  51,  51,  51,  51,

       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,

     204, 204, 204, 204, 204, 204, 204, 204,
     102, 102, 102, 102, 102, 102, 102, 102,
     204, 204, 204, 204, 204, 204, 204, 204,
     102, 102, 102, 102, 102, 102, 102, 102,

      77,  77,  77,  77,  77,  77,  77,  77,
      77,  77,  77,  77,  77,  77,  77,  77,
     179, 179, 179, 179, 179, 179, 179, 179,
     179, 179, 179, 179, 179, 179, 179, 179,

      77,  77,  77,  77,  77,  77,  77,  77,
      77,  77,  77,  77,  77,  77,  77,  77,
     179, 179, 179, 179, 179, 179, 179, 179,
     179, 179, 179, 179, 179, 179, 179, 179,

      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230,
      51,  77, 102, 128, 153, 179, 204, 230);

type
  TmcmGridAlignment = (GA2x1, GA1x2, GA4x4, GA16x1, GA1x16, GA16x16, GA8x32, GA256x1, GA1x256);

  TOnPaletteIndex = procedure(Sender   : TObject;
                              NewIndex : integer) of object;


  TmcmColorGrid = class(TCustomControl)
  private
    FBrush             : TBrush;
    FAutoUpdate        : boolean;
    FCellBorder        : boolean;
    FNumEntries        : integer;
    FPaletteEntries    : array[0..NumPaletteEntries - 1] of TPaletteEntry;
    FImage             : TmcmImage;
    FShowSelection     : boolean;
    FSelected          : integer;
    FCellXSize         : integer;
    FCellYSize         : integer;
    FNumXSquares       : integer;
    FNumYSquares       : integer;
    FGridAlignment     : TmcmGridAlignment;
    FHasFocus          : boolean;
    FUsePaletteSize    : boolean;
    FOnPaletteIndex    : TOnPaletteIndex;
    FOnChange          : TNotifyEvent;
    FButton            : TMouseButton;
    FButtonDown        : boolean;
    procedure   DrawSquare           (    Which        : integer;
                                          ShowSelector : boolean);
    procedure   DrawFgBg;
    procedure   SetGridAlignment     (    Value        : TmcmGridAlignment);
    function    GetSelectedColor : TColor;
    procedure   SetSelected          (    Value        : integer);
    procedure   SetSelectedColor     (    Value        : TColor);
    procedure   StyleChanged         (    Sender       : TObject);
    procedure   UpdateCellSizes      (    DoRepaint    : boolean);
    procedure   WMSetFocus           (var Message      : TWMSetFocus);   message WM_SETFOCUS;
    procedure   WMKillFocus          (var Message      : TWMKillFocus);  message WM_KILLFOCUS;
    procedure   WMGetDlgCode         (var Message      : TWMGetDlgCode); message WM_GETDLGCODE;
    procedure   WMSize               (var Message      : TWMSize);       message WM_SIZE;
    procedure   CMCtl3DChanged       (var Message      : TMessage);      message CM_CTL3DCHANGED;
  protected
    procedure   Change;                                                  dynamic;
    procedure   CreateWnd;                                               override;
    procedure   KeyDown              (var Key          : Word;
                                          Shift        : TShiftState);   override;
    procedure   MouseDown            (    Button       : TMouseButton;
                                          Shift        : TShiftState;
                                          X, Y         : integer);       override;
    procedure   MouseMove            (    Shift        : TShiftState;
                                          X, Y         : integer);       override;
    procedure   MouseUp              (    Button       : TMouseButton;
                                          Shift        : TShiftState;
                                          X, Y         : integer);       override;
    procedure   Paint;                                                   override;
    procedure   SetBrush             (    Value        : TBrush);
    procedure   SetCellBorder        (    Value        : boolean);
    procedure   SetImage             (    Value        : TmcmImage);
    function    SquareFromPos        (    X, Y         : integer) : integer;
  public
    constructor Create               (    AOwner       : TComponent);    override;
    destructor  Destroy; override;

    function    GetColor             (    Index        : word) : TColor;
    function    GetPaletteEntry      (    Index        : word) : TPaletteEntry;
    procedure   SetColor             (    Index        : word;
                                          NewColor     : TColor);
    procedure   SetPaletteEntry      (    Index        : word;
                                          PalEntry     : TPaletteEntry);
    property    Image : TmcmImage
      read      FImage
      write     SetImage;
    property    SelectedColor : TColor
       read     GetSelectedColor
       write    SetSelectedColor;
  published
    property    AutoUpdate : boolean
      read      FAutoUpdate
      write     FAutoUpdate default False;
    property    Brush : TBrush
      read      FBrush
      write     SetBrush;
    property    CellBorder : boolean
      read      FCellBorder
      write     SetCellBorder default True;
    property    Ctl3D;
//    property    DragCursor;
//    property    DragMode;
    property    Enabled;
//    property    Font;
    property    GridAlignment : TmcmGridAlignment
       read     FGridAlignment
       write    SetGridAlignment default ga16x16;
    property    ParentCtl3D;
    property    ParentFont;
    property    ParentShowHint;
    property    Selected : integer
       read     FSelected
       write    SetSelected default 0;
    property    ShowHint;
    property    TabOrder;
    property    TabStop;
    property    UsePaletteSize : boolean
      read      FUsePaletteSize
      write     FUsePaletteSize default False;
    property    Visible;

    property    OnChange : TNotifyEvent
       read     FOnChange
       write    FOnChange;
    property    OnClick;
//    property    OnDragDrop;
//    property    OnDragOver;
//    property    OnEndDrag;
    property    OnEnter;
    property    OnExit;
    property    OnKeyDown;
    property    OnKeyPress;
    property    OnKeyUp;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnPaletteIndex : TOnPaletteIndex
      read      FOnPaletteIndex
      write     FOnPaletteIndex;
  end;

implementation

uses {$IFNDEF GE_DXE2}
      SysUtils, Consts, StdCtrls,
     {$ELSE}
      System.UITypes, System.SysUtils, Vcl.Consts, Vcl.StdCtrls,
     {$ENDIF}
     mcmImageTypeDef;


constructor TmcmColorGrid.Create(AOwner : TComponent);
var i : integer;
begin
  inherited Create(AOwner);

  FCellBorder        := True;
  FOnPaletteIndex    := Nil;
  FAutoUpdate        := False;
  ControlStyle       := ControlStyle + [csOpaque];
  FUsePaletteSize    := False;
  FGridAlignment     := ga16x16;
  FNumXSquares       := 16;
  FNumYSquares       := 16;
  FShowSelection     := True;
  Color              := clBtnFace;

  FBrush := TBrush.Create;
  FBrush.Color := clBtnFace;
  FBrush.Style := bsSolid;
  FBrush.OnChange := StyleChanged;
  Canvas.Pen.Color   := clBlack;
  SetBounds(0, 0, 100, 100);

  FNumEntries := NumPaletteEntries;
  for i := 0 to (NumPaletteEntries - 1)
  do begin
     with FPaletteEntries[i]
     do begin
        peRed   := RedVals[i];
        peGreen := GreenVals[i];
        peBlue  := BlueVals[i];
        peFlags := 0;
     end;
  end;
  FImage := Nil;
end; // TmcmColorGrid.Create.


destructor TmcmColorGrid.Destroy;
begin
  if Assigned(FBrush)
  then FBrush.Free;
  FBrush := Nil;
  Inherited Destroy;
end; // TmcmColorGrid.Destroy.


procedure TmcmColorGrid.CreateWnd;
begin
  inherited CreateWnd;
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or WS_CLIPSIBLINGS);
end; // TmcmColorGrid.CreateWnd.


procedure TmcmColorGrid.SetBrush(Value : TBrush);
begin
  FBrush.Assign(Value);
end; // TmcmColorGrid.SetBrush.


procedure TmcmColorGrid.SetCellBorder(Value : boolean);
begin
  if (FCellBorder <> Value)
  then begin
       FCellBorder := Value;
       InvalidateRect(Handle, Nil, False);
  end;
end; // TmcmColorGrid.SetCellBorder.


procedure TmcmColorGrid.DrawSquare(Which : Integer; ShowSelector : Boolean);
var
  WinTop   : integer;
  WinLeft  : Integer;
  PalIndex : Integer;
  CellRect : TRect;
begin
  if (Which >= 0) and (Which <= 255)
  then begin
       PalIndex := Which;
       WinTop   := (Which div FNumXSquares) * FCellYSize;
       WinLeft  := (Which mod FNumXSquares) * FCellXSize;
       CellRect := Bounds(WinLeft, WinTop, FCellXSize, FCellYSize);

       Canvas.Pen.Color := Brush.Color;
       Canvas.Pen.Width := 1;

       if Ctl3D
       then begin
            with CellRect
            do Canvas.Rectangle(Left, Top, Right, Bottom);
            InflateRect(CellRect, -1, -1);
            Frame3D(Canvas, CellRect, clBtnShadow, clBtnHighlight, 2);
       end;

       with FPaletteEntries[PalIndex]
       do begin
          Canvas.Brush.Color := TColor(RGB(peRed, peGreen, peBlue));
          if Ctl3D or Not(FCellBorder)
          then Canvas.Pen.Color := TColor(RGB(peRed, peGreen, peBlue));
       end;
       if not ShowSelector
       then with CellRect
            do Canvas.Rectangle(Left, Top, Right, Bottom)
       else with CellRect
            do begin
               if Ctl3D
               then begin
                    Canvas.Rectangle(Left, Top, Right, Bottom);
                    InflateRect(CellRect, -1, -1);
                    DrawFocusRect(Canvas.Handle, CellRect);
               end
               else with Canvas
                    do begin
                       Pen.Color := clBlack;
                       Pen.Mode := pmNot;
                       Rectangle(Left, Top, Right, Bottom);
                       Pen.Mode := pmCopy;
                       Rectangle(Left + 2, Top + 2, Right - 2, Bottom - 2);
                    end;
            end;
  end;
end; // TmcmColorGrid.DrawSquare.


procedure TmcmColorGrid.DrawFgBg;
var InvColor : TPaletteEntry;
    PalIndex  : Integer;
    OldBkMode : Integer;
    R         : TRect;

    function TernaryOp(Test : Boolean; ResultTrue, ResultFalse : Integer) : Integer;
    begin
      if Test
      then Result := ResultTrue
      else Result := ResultFalse;
    end; // TernaryOp.

begin
  if FShowSelection
  then begin
       OldBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
       with R
       do begin
          left   := (FSelected mod FNumXSquares) * FCellXSize;
          right  := left + FCellXSize;
          top    := (FSelected div FNumXSquares) * FCellYSize;
          bottom := top + FCellYSize;
       end;

       PalIndex := FSelected;
       InvColor := FPaletteEntries[PalIndex];
       with InvColor
       do begin
          peRed   := TernaryOp(peRed   >= $80, $0, $80);
          peGreen := TernaryOp(peGreen >= $80, $0, $80);
          peBlue  := TernaryOp(peBlue  >= $80, $0, $80);
          Canvas.Pen.Color := TColor(RGB(peRed, peGreen, peBlue));
       end;
       Canvas.Brush.Style := BSCLEAR;
       Canvas.Pen.Color  := TColor(RGB(92, 92, 92));
       Canvas.Rectangle(R.Left + 1, R.Top + 1, R.Right, R.Bottom);

       Canvas.Pen.Color  := TColor(RGB(255, 255, 255));
       Canvas.Rectangle(R.Left, R.Top, R.Right - 1, R.Bottom - 1);
       SetBkMode(Canvas.Handle, OldBkMode);
  end;
end; // TmcmColorGrid.DrawFgBg.


procedure TmcmColorGrid.WMSetFocus(var Message : TWMSetFocus);
begin
  FHasFocus := True;
  DrawSquare(FSelected, True);
  DrawFgBg;
  inherited;
end; // TmcmColorGrid.WMSetFocus.


procedure TmcmColorGrid.WMKillFocus(var Message : TWMKillFocus);
begin
  FHasFocus := False;
  DrawSquare(FSelected, False);
  DrawFgBg;
  inherited;
end; // TmcmColorGrid.WMKillFocus.


procedure TmcmColorGrid.KeyDown(var Key : Word; Shift : TShiftState);
var NewSelection : Integer;
    Range        : Integer;
begin
  inherited KeyDown(Key, Shift);
  Range := FNumXSquares * FNumYSquares;
  case Key of
  VK_HOME  : NewSelection := 0;
  VK_UP    : begin
               if (FSelected >= FNumXSquares)
               then NewSelection := FSelected - FNumXSquares
               else if (FSelected <> 0)
                    then NewSelection := Range - FNumXSquares + FSelected - 1
                    else NewSelection := Range - 1;
               NewSelection := NewSelection mod FNumEntries;
             end;
  VK_LEFT  : begin
               if (FSelected <> 0)
               then NewSelection := FSelected - 1
               else NewSelection := Range - 1;
               NewSelection := NewSelection mod FNumEntries;
             end;
  VK_DOWN  : begin
               if (FSelected + FNumXSquares < Range)
               then begin
                    NewSelection := FSelected + FNumXSquares;
                    if (NewSelection >= FNumEntries)
                    then NewSelection := NewSelection mod FNumEntries + 1;
                    if (NewSelection >= FNumEntries)
                    then NewSelection := 0;
               end
               else if (FSelected <> Range - 1)
                    then NewSelection := FSelected mod FNumXSquares + 1
                    else NewSelection := 0;
             end;
  VK_RIGHT : begin
               if (FSelected <> Range - 1)
               then NewSelection := FSelected + 1
               else NewSelection := 0;
               NewSelection := NewSelection mod FNumEntries;
             end;
  VK_END   : NewSelection := FNumEntries - 1;
  else       inherited KeyDown(Key, Shift);
             Exit;
  end;
  Key := 0;

  DrawSquare(FSelected, FHasFocus);
  if (FSelected <> NewSelection)
  then SetSelected(NewSelection);
end; // TmcmColorGrid.KeyDown.


procedure TmcmColorGrid.WMGetDlgCode(var Message : TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end; // TmcmColorGrid.WMGetDlgCode.


procedure TmcmColorGrid.StyleChanged(Sender : TObject);
begin
  Invalidate;
end; // TmcmColorGrid.StyleChanged.


procedure TmcmColorGrid.WMSize(var Message : TWMSize);
begin
  inherited;
  UpdateCellSizes(False);
end; // TmcmColorGrid.WMSize.


procedure TmcmColorGrid.CMCtl3DChanged(var Message : TMessage);
begin
  inherited;
  Invalidate;
end; // TmcmColorGrid.CMCtl3DChanged.


procedure TmcmColorGrid.MouseDown(Button : TMouseButton;
                                  Shift  : TShiftState;
                                  X, Y   : Integer);
var Square : Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  FButton     := Button;
  FButtonDown := True;
  Square      := SquareFromPos(X, Y);
  if (Square < FNumEntries)
  then SetSelected(Square);
  if TabStop
  then SetFocus;
end; // TmcmColorGrid.MouseDown.


procedure TmcmColorGrid.MouseMove(Shift : TShiftState; X, Y : Integer);
var Square : Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FButtonDown
  then begin
       Square := SquareFromPos(X, Y);
       if (Square < FNumEntries)
       then SetSelected(Square);
  end;
end; // TmcmColorGrid.MouseMove.


procedure TmcmColorGrid.MouseUp(Button : TMouseButton;
                                Shift  : TShiftState;
                                X, Y   : Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FButtonDown := False;
  if (FButton = mbRight)
  then MouseCapture := False;
end; // TmcmColorGrid.MouseUp.


procedure TmcmColorGrid.Paint;
var Row         : integer;
    Col         : integer;
    wEntryIndex : Integer;
begin
  Canvas.Brush.Assign(FBrush);
  //Canvas.Font := Font;
  for Row := 0 to FNumYSquares
  do for Col := 0 to FNumXSquares
     do begin
        wEntryIndex := Row * FNumXSquares + Col;
        DrawSquare(wEntryIndex, False);
     end;
  DrawSquare(FSelected, FHasFocus);
  DrawFgBg;
end; // TmcmColorGrid.Paint.


procedure TmcmColorGrid.SetGridAlignment(Value : TmcmGridAlignment);
begin
  if (FGridAlignment = Value)
  then Exit;
  FGridAlignment := Value;
  case FGridAlignment of
  ga2x1   : begin
              FNumXSquares  := 2;
              FNumYSquares  := 1;
            end;
  ga1x2   : begin
              FNumXSquares  := 1;
              FNumYSquares  := 2;
            end;
  ga4x4   : begin
              FNumXSquares  := 4;
              FNumYSquares  := 4;
            end;
  ga16x1  : begin
              FNumXSquares  := 16;
              FNumYSquares  := 1;
            end;
  ga1x16  : begin
              FNumXSquares  := 1;
              FNumYSquares  := 16;
            end;
  ga16x16 : begin
              FNumXSquares  := 16;
              FNumYSquares  := 16;
            end;
  ga8x32  : begin
              FNumXSquares  := 8;
              FNumYSquares  := 32;
            end;
  ga256x1 : begin
              FNumXSquares  := 256;
              FNumYSquares  := 1;
            end;
  ga1x256 : begin
              FNumXSquares  := 1;
              FNumYSquares  := 256;
            end;
  end;
  UpdateCellSizes(True);
end; // TmcmColorGrid.SetGridAlignment.


procedure TmcmColorGrid.SetSelected(Value : Integer);
begin
  if (FSelected = Value)
  then Exit;
  DrawSquare(FSelected, False);
  FSelected := Value;
  DrawSquare(FSelected, FHasFocus);
  DrawFgBg;
  if Assigned(FOnPaletteIndex)
  then FOnPaletteIndex(Self, FSelected);
end; // TmcmColorGrid.SetSelected.


function TmcmColorGrid.SquareFromPos(X, Y : Integer) : Integer;
begin
  if (X > Width - 1)
  then X := Width - 1
  else if (X < 0)
       then X := 0;
  if (Y > Height - 1)
  then Y := Height - 1
  else if (Y < 0)
       then Y := 0;
  Result := (Y div FCellYSize) * FNumXSquares + (X div FCellXSize);
end; // TmcmColorGrid.SquareFromPos.


procedure TmcmColorGrid.UpdateCellSizes(DoRepaint : Boolean);
var NewWidth  : integer;
    NewHeight : Integer;
begin
  NewWidth   := (Width div FNumXSquares) * FNumXSquares;
  NewHeight  := (Height div FNumYSquares) * FNumYSquares;
  BoundsRect := Bounds(Left, Top, NewWidth, NewHeight);
  FCellXSize := Width div FNumXSquares;
  FCellYSize := Height div FNumYSquares;
  if DoRepaint
  then Invalidate;
end; // TmcmColorGrid.UpdateCellSizes.


procedure TmcmColorGrid.Change;
begin
  if Assigned(FOnChange)
  then FOnChange(Self);
end; // TmcmColorGrid.Change.


function TmcmColorGrid.GetSelectedColor : TColor;
begin
  Result := GetColor(FSelected);
end; // TmcmColorGrid.GetSelectedColor.


procedure TmcmColorGrid.SetSelectedColor(Value : TColor);
begin
  SetColor(FSelected, Value);
end; // TmcmColorGrid.SetSelectedColor.


function TmcmColorGrid.GetColor(Index : word) : TColor;
begin
  with FPaletteEntries[Index]
  do Result := TColor(RGB(peRed, peGreen, peBlue));
end; // TmcmColorGrid.GetColor.


procedure TmcmColorGrid.SetColor(Index    : word;
                                 NewColor : TColor);
var r, g, b : byte;
begin
  if (Index < FNumEntries)
  then begin
       r := GetRValue(NewColor);
       g := GetGValue(NewColor);
       b := GetBValue(NewColor);

       with FPaletteEntries[Index]
       do begin
          if (peRed   <> r) or (peGreen <> g) or (peBlue  <> b)
          then begin
               peRed   := r;
               peGreen := g;
               peBlue  := b;
               peFlags := 0;

               DrawSquare(Index, FHasFocus and (Index = FSelected));
               if (Index = FSelected)
               then DrawFgBg;
               if FAutoUpdate
               then begin
                    if Assigned(FImage)
                    then FImage.SetPaletteEntry(Index, @FPaletteEntries[Index]);
               end;
          end;
       end;
  end;
end; // TmcmColorGrid.SetColor.


function TmcmColorGrid.GetPaletteEntry(Index : word) : TPaletteEntry;
begin
  if (Index < FNumEntries)
  then Result := FPaletteEntries[Index];
end; // TmcmColorGrid.GetPaletteEntry.


procedure TmcmColorGrid.SetPaletteEntry(Index    : word;
                                        PalEntry : TPaletteEntry);
begin
  if (Index < FNumEntries)
  then begin
       if (FPaletteEntries[Index].peRed <> PalEntry.peRed) or
          (FPaletteEntries[Index].peGreen <> PalEntry.peGreen) or
          (FPaletteEntries[Index].peBlue <> PalEntry.peBlue)
       then begin
            FPaletteEntries[Index] := PalEntry;
            DrawSquare(Index, FHasFocus and (Index = FSelected));
            if (Index = FSelected)
            then DrawFgBg;
            if FAutoUpdate
            then begin
                 if Assigned(FImage)
                 then begin
                      FImage.SetPaletteEntry(Index, @FPaletteEntries[Index]);
                      FImage.Modified := True;
                 end;
            end;
       end;
  end;
end; // TmcmColorGrid.SetPaletteEntry.


procedure TmcmColorGrid.SetImage(Value : TmcmImage);
var i       : integer;
    r, g, b : byte;
begin
  if (Value.ImageFormat < IF_RGB15)
  then begin
       FImage := Value;
       FNumEntries := 1 shl FImage.BitCount;
       r := GetRValue(ColorToRGB(Brush.Color));
       g := GetGValue(ColorToRGB(Brush.Color));
       b := GetBValue(ColorToRGB(Brush.Color));
       for i := FNumEntries to 255
       do begin
          FPaletteEntries[i].peRed   := r;
          FPaletteEntries[i].peGreen := g;
          FPaletteEntries[i].peBlue  := b;
       end;
       for i := 0 to (FNumEntries - 1)
       do FImage.GetPaletteEntry(i, @FPaletteEntries[i]);

       if FUsePaletteSize
       then begin
            case FNumEntries of
            2   : GridAlignment := ga2x1;
            16  : GridAlignment := ga4x4;
            256 : GridAlignment := ga16x16;
            end;
       end;
       FSelected := -1;
       Selected := 0;
  end;
end; // TmcmColorGrid.SetImage.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.







