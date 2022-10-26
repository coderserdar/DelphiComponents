{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit rxClipview;

interface

{$I RX.INC}

uses
  SysUtils, Windows,
  Messages, Classes, Graphics, Controls, Clipbrd, Forms, StdCtrls,
  ExtCtrls, Menus;

type

{ TCustomClipboardViewer }

  TClipboardViewFormat = (cvDefault, cvEmpty, cvUnknown, cvText, cvBitmap,
    cvMetafile, cvPalette, cvOemText, cvPicture, cvComponent, cvIcon);

  TCustomClipboardViewer = class(TScrollBox)
  private
    { Private declarations }
    FWndNext: HWnd;
    FChained: Boolean;
    FPaintControl: TComponent;
    FViewFormat: TClipboardViewFormat;
    FOnChange: TNotifyEvent;
    function IsEmptyClipboard: Boolean;
    procedure ForwardMessage(var Message: TMessage);
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure WMDestroyClipboard(var Message: TMessage); message WM_DESTROYCLIPBOARD;
    procedure WMChangeCBChain(var Message: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Message: TMessage); message WM_DRAWCLIPBOARD;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure SetViewFormat(Value: TClipboardViewFormat);
    function GetClipboardFormatNames(Index: Integer): string;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure DestroyWindowHandle; override;
    procedure Change; dynamic;
    procedure CreatePaintControl; virtual;
    function GetDrawFormat: TClipboardViewFormat; virtual;
    function ValidFormat(Format: TClipboardViewFormat): Boolean; dynamic;
    property ViewFormat: TClipboardViewFormat read FViewFormat write
      SetViewFormat stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    class function CanDrawFormat(ClipboardFormat: Word): Boolean;
    property ClipboardFormatNames[Index: Integer]: string read GetClipboardFormatNames;
  published
    property Color default clWindow;
    property ParentColor default False;
  end;

  TClipboardViewer = class(TCustomClipboardViewer)
  published
{$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
    property ViewFormat;
    property OnChange;
{$IFDEF RX_D5}
    property OnContextPopup;
{$ENDIF}
    property OnStartDrag;
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

function ClipboardFormatToView(Value: Word): TClipboardViewFormat;

implementation

uses
  Grids,
  rxClipIcon, rxMaxMin, RxTConst, rxVCLUtils;

{ Utility routines }

function ClipboardFormatName(Format: Word): string;
var
  Buffer: array[0..255] of Char;
begin
  SetString(Result, Buffer, GetClipboardFormatName(Format, Buffer, 255));
  if Result = '' then
    case Format of
      CF_BITMAP: Result := 'Bitmap';
      CF_DIB: Result := 'DIB Bitmap';
      CF_DIF: Result := 'DIF';
      CF_METAFILEPICT: Result := 'Metafile Picture';
      CF_ENHMETAFILE: Result := 'Enhanced Metafile';
      CF_OEMTEXT: Result := 'OEM Text';
      CF_PALETTE: Result := 'Palette';
      CF_PENDATA: Result := 'Pen Data';
      CF_RIFF: Result := 'RIFF File';
      CF_SYLK: Result := 'SYLK';
      CF_TEXT: Result := 'Text';
      CF_TIFF: Result := 'Tag Image';
      CF_WAVE: Result := 'Wave';
    end;
end;

function ViewToClipboardFormat(Value: TClipboardViewFormat): Word;
begin
  case Value of
    cvDefault, cvUnknown, cvEmpty: Result := 0;
    cvText: Result := CF_TEXT;
    cvBitmap: Result := CF_BITMAP;
    cvMetafile: Result := CF_METAFILEPICT;
    cvPalette: Result := CF_PALETTE;
    cvOemText: Result := CF_OEMTEXT;
    cvPicture: Result := CF_PICTURE; { CF_BITMAP, CF_METAFILEPICT }
    cvComponent: Result := CF_COMPONENT; { CF_TEXT }
    cvIcon: Result := CF_ICON; { CF_BITMAP }
  else
    Result := 0;
  end;
end;

function ClipboardFormatToView(Value: Word): TClipboardViewFormat;
begin
  if Value = CF_TEXT then Result := cvText
  else if Value = CF_BITMAP then Result := cvBitmap
  else if Value = CF_METAFILEPICT then Result := cvMetafile
  else if Value = CF_ENHMETAFILE then Result := cvMetafile
  else if Value = CF_PALETTE then Result := cvPalette
  else if Value = CF_OEMTEXT then Result := cvOemText
  else if Value = CF_PICTURE then Result := cvPicture { CF_BITMAP, CF_METAFILEPICT }
  else if Value = CF_COMPONENT then Result := cvComponent { CF_TEXT }
  else if Value = CF_ICON then Result := cvIcon { CF_BITMAP }
  else Result := cvDefault;
end;

procedure ComponentToStrings(Instance: TComponent; Text: TStrings);
var
  Mem, Out: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.WriteComponent(Instance);
    Mem.Position := 0;
    Out := TMemoryStream.Create;
    try
      ObjectBinaryToText(Mem, Out);
      Out.Position := 0;
      Text.LoadFromStream(Out);
    finally
      Out.Free;
    end;
  finally
    Mem.Free;
  end;
end;

{ TPaletteGrid }

const
  NumPaletteEntries = 256;

type
  TPaletteGrid = class(TDrawGrid)
  private
    FPaletteEntries: array[0..NumPaletteEntries - 1] of TPaletteEntry;
    FPalette: HPALETTE;
    FCount: Integer;
    FSizing: Boolean;
    procedure SetPalette(Value: HPALETTE);
    procedure UpdateSize;
    function CellColor(ACol, ARow: Longint): TColor;
    procedure DrawSquare(CellColor: TColor; CellRect: TRect; ShowSelector: Boolean);
  protected
    function GetPalette: HPALETTE; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Palette: HPALETTE read FPalette write SetPalette;
  end;

function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogSize: Integer;
  LogPalette: PLogPalette;
begin
  Result := 0;
  if Palette = 0 then
    Exit;
  GetObject(Palette, SizeOf(PaletteSize), @PaletteSize);
  LogSize := SizeOf(TLogPalette) + (PaletteSize - 1) * SizeOf(TPaletteEntry);
  GetMem(LogPalette, LogSize);
  try
    with LogPalette^ do
    begin
      palVersion := $0300;
      palNumEntries := PaletteSize;
      GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
    end;
    Result := CreatePalette(LogPalette^);
  finally
    FreeMem(LogPalette, LogSize);
  end;
end;

constructor TPaletteGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultColWidth := 20;
  DefaultRowHeight := 20;
  Options := [];
  GridLineWidth := 0;
  FixedCols := 0;
  FixedRows := 0;
  ColCount := 0;
  RowCount := 0;
  DefaultDrawing := False;
  ScrollBars := ssVertical;
end;

destructor TPaletteGrid.Destroy;
begin
  if FPalette <> 0 then
    DeleteObject(FPalette);
  inherited Destroy;
end;

procedure TPaletteGrid.UpdateSize;
var
  Rows: Integer;
begin
  if FSizing then
    Exit;
  FSizing := True;
  try
    ColCount := (ClientWidth - GetSystemMetrics(SM_CXVSCROLL)) div
      DefaultColWidth;
    Rows := FCount div ColCount;
    if FCount mod ColCount > 0 then
      Inc(Rows);
    RowCount := Max(1, Rows);
    ClientHeight := DefaultRowHeight * RowCount;
  finally
    FSizing := False;
  end;
end;

function TPaletteGrid.GetPalette: HPALETTE;
begin
  if FPalette <> 0 then
    Result := FPalette
  else
    Result := inherited GetPalette;
end;

procedure TPaletteGrid.SetPalette(Value: HPALETTE);
var
  I: Integer;
  ParentForm: TCustomForm;
begin
  if FPalette <> 0 then
    DeleteObject(FPalette);
  FPalette := CopyPalette(Value);
  FCount := Min(PaletteEntries(FPalette), NumPaletteEntries);
  GetPaletteEntries(FPalette, 0, FCount, FPaletteEntries);
  for I := FCount to NumPaletteEntries - 1 do
    FillChar(FPaletteEntries[I], SizeOf(TPaletteEntry), $80);
  UpdateSize;
  if Visible and (not (csLoading in ComponentState)) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
      PostMessage(ParentForm.Handle, WM_QUERYNEWPALETTE, 0, 0);
  end;
end;

function TPaletteGrid.CellColor(ACol, ARow: Longint): TColor;
var
  PalIndex: Integer;
begin
  PalIndex := ACol + (ARow * ColCount);
  if PalIndex <= FCount - 1 then
    with FPaletteEntries[PalIndex] do
      Result := TColor(RGB(peRed, peGreen, peBlue))
  else
    Result := clNone;
end;

procedure TPaletteGrid.DrawSquare(CellColor: TColor; CellRect: TRect;
  ShowSelector: Boolean);
var
  SavePal: HPalette;
begin
  Canvas.Pen.Color := clBtnFace;
  with CellRect do
    Canvas.Rectangle(Left, Top, Right, Bottom);
  InflateRect(CellRect, -1, -1);
  Frame3D(Canvas, CellRect, clBtnShadow, clBtnHighlight, 2);
  SavePal := 0;
  if FPalette <> 0 then
  begin
    SavePal := SelectPalette(Canvas.Handle, FPalette, False);
    RealizePalette(Canvas.Handle);
  end;
  try
    Canvas.Brush.Color := CellColor;
    Canvas.Pen.Color := CellColor;
    with CellRect do
      Canvas.Rectangle(Left, Top, Right, Bottom);
  finally
    if FPalette <> 0 then
      SelectPalette(Canvas.Handle, SavePal, True);
  end;
  if ShowSelector then
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.Pen.Color := Self.Color;
    InflateRect(CellRect, -1, -1);
    Canvas.DrawFocusRect(CellRect);
  end;
end;

function TPaletteGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := ((ACol = 0) and (ARow = 0)) or (CellColor(ACol, ARow) <> clNone);
end;

procedure TPaletteGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  Color: TColor;
begin
  Color := CellColor(ACol, ARow);
  if Color <> clNone then
    DrawSquare(PaletteColor(Color), ARect, gdFocused in AState)
  else
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(ARect);
  end;
end;

procedure TPaletteGrid.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateSize;
end;

{ TCustomClipboardViewer }

constructor TCustomClipboardViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  FWndNext := 0;
  FPaintControl := nil;
  FViewFormat := cvDefault;
  ParentColor := False;
  Color := clWindow;
  ControlState := ControlState - [csCreating];
end;

procedure TCustomClipboardViewer.ForwardMessage(var Message: TMessage);
begin
  if FWndNext <> 0 then
    with Message do
      SendMessage(FWndNext, Msg, WParam, LParam);
end;

procedure TCustomClipboardViewer.CreateWnd;
begin
  inherited CreateWnd;
  if Handle <> 0 then
  begin
    FWndNext := SetClipboardViewer(Handle);
    FChained := True;
  end;
end;

procedure TCustomClipboardViewer.DestroyWindowHandle;
begin
  if FChained then
  begin
    ChangeClipboardChain(Handle, FWndNext);
    FChained := False;
  end;
  FWndNext := 0;
  inherited DestroyWindowHandle;
end;

procedure TCustomClipboardViewer.CreatePaintControl;
var
  Icon: TIcon;
  Format: TClipboardViewFormat;
  Instance: TComponent;
begin
  if csDesigning in ComponentState then
    Exit;
  FPaintControl.Free;
  FPaintControl := nil;
  if IsEmptyClipboard then
    Exit;
  Format := GetDrawFormat;
  if not ValidFormat(Format) then
    Format := cvUnknown;
  case Format of
    cvText, cvOemText, cvUnknown, cvDefault, cvEmpty:
      begin
        FPaintControl := TMemo.Create(Self);
        with TMemo(FPaintControl) do
        begin
          BorderStyle := bsNone;
          Parent := Self;
          Left := 0;
          Top := 0;
          ScrollBars := ssBoth;
          Align := alClient;
          if Format = cvOemText then
          begin
            ParentFont := False;
            Font.Name := 'Terminal';
          end;
          Visible := True;
          if Clipboard.HasFormat(CF_TEXT) then
            PasteFromClipboard
          else
          if (Format = cvText) and Clipboard.HasFormat(CF_COMPONENT) then
          begin
            Instance := Clipboard.GetComponent(Self, Self);
            try
              ComponentToStrings(Instance, Lines);
            finally
              Instance.Free;
            end;
          end
          else
          if IsEmptyClipboard then
            Text := LoadStr(SClipbrdEmpty)
          else
            Text := LoadStr(SClipbrdUnknown);
          ReadOnly := True;
        end;
      end;
    cvPicture, cvMetafile, cvBitmap, cvIcon:
      begin
        FPaintControl := TImage.Create(Self);
        with TImage(FPaintControl) do
        begin
          Parent := Self;
          AutoSize := True;
          Left := 0;
          Top := 0;
          Visible := True;
          if Format = cvIcon then
          begin
            if Clipboard.HasFormat(CF_ICON) then
            begin
              Icon := CreateIconFromClipboard;
              try
                Picture.Icon := Icon;
              finally
                Icon.Free;
              end;
            end;
          end
          else
          if ((Format = cvBitmap) and Clipboard.HasFormat(CF_BITMAP))
            or ((Format = cvMetafile) and (Clipboard.HasFormat(CF_METAFILEPICT))
            or Clipboard.HasFormat(CF_ENHMETAFILE))
            or ((Format = cvPicture) and Clipboard.HasFormat(CF_PICTURE)) then
            Picture.Assign(Clipboard);
        end;
        CenterControl(TImage(FPaintControl));
      end;
    cvComponent:
      begin
        Instance := Clipboard.GetComponent(Self, Self);
        FPaintControl := Instance;
        if FPaintControl is TControl then
        begin
          with TControl(FPaintControl) do
          begin
            Left := 1;
            Top := 1;
            Parent := Self;
          end;
          CenterControl(TControl(FPaintControl));
        end
        else
        begin
          FPaintControl := TMemo.Create(Self);
          try
            with TMemo(FPaintControl) do
            begin
              BorderStyle := bsNone;
              Parent := Self;
              Left := 0;
              Top := 0;
              ScrollBars := ssBoth;
              Align := alClient;
              ReadOnly := True;
              ComponentToStrings(Instance, Lines);
              Visible := True;
            end;
          finally
            Instance.Free;
          end;
        end;
      end;
    cvPalette:
      begin
        FPaintControl := TPaletteGrid.Create(Self);
        with TPaletteGrid(FPaintControl) do
        try
          BorderStyle := bsNone;
          Parent := Self;
          Ctl3D := False;
          Align := alClient;
          Clipboard.Open;
          try
            Palette := GetClipboardData(CF_PALETTE);
          finally
            Clipboard.Close;
          end;
        except
          FPaintControl.Free;
          raise;
        end;
      end;
  end;
end;

function TCustomClipboardViewer.GetClipboardFormatNames(Index: Integer): string;
begin
  Result := '';
  if Index < Clipboard.FormatCount then
    Result := ClipboardFormatName(Clipboard.Formats[Index]);
end;

procedure TCustomClipboardViewer.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomClipboardViewer.WMSize(var Message: TMessage);
begin
  inherited;
  if (FPaintControl <> nil) and (FPaintControl is TControl) then
    CenterControl(TControl(FPaintControl));
end;

procedure TCustomClipboardViewer.WMChangeCBChain(var Message: TWMChangeCBChain);
begin
  if Message.Remove = FWndNext then
    FWndNext := Message.Next
  else
    ForwardMessage(TMessage(Message));
  inherited;
end;

procedure TCustomClipboardViewer.WMNCDestroy(var Message: TWMNCDestroy);
begin
  if FChained then
  begin
    ChangeClipboardChain(Handle, FWndNext);
    FChained := False;
    FWndNext := 0;
  end;
  inherited;
end;

procedure TCustomClipboardViewer.WMDrawClipboard(var Message: TMessage);
var
  Format: Word;
begin
  ForwardMessage(Message);
  Format := ViewToClipboardFormat(ViewFormat);
  if IsEmptyClipboard then
    FViewFormat := cvEmpty
  else
    if not Clipboard.HasFormat(Format) then
      FViewFormat := cvDefault;
  Change;
  DisableAlign;
  try
    CreatePaintControl;
  finally
    EnableAlign;
  end;
  inherited;
end;

procedure TCustomClipboardViewer.WMDestroyClipboard(var Message: TMessage);
begin
  FViewFormat := cvEmpty;
  Change;
  CreatePaintControl;
end;

function TCustomClipboardViewer.IsEmptyClipboard: Boolean;
begin
  Result := (Clipboard.FormatCount = 0);
end;

procedure TCustomClipboardViewer.SetViewFormat(Value: TClipboardViewFormat);
var
  Format: Word;
begin
  if Value <> ViewFormat then
  begin
    Format := ViewToClipboardFormat(Value);
    if (Clipboard.HasFormat(Format) and ValidFormat(Value)) then
      FViewFormat := Value
    else
      FViewFormat := cvDefault;
    CreatePaintControl;
  end;
end;

function TCustomClipboardViewer.GetDrawFormat: TClipboardViewFormat;

  function DefaultFormat: TClipboardViewFormat;
  begin
    if Clipboard.HasFormat(CF_TEXT) then Result := cvText
    else if Clipboard.HasFormat(CF_OEMTEXT) then Result := cvOemText
    else if Clipboard.HasFormat(CF_BITMAP) then Result := cvBitmap
    else if (Clipboard.HasFormat(CF_METAFILEPICT))
      or (Clipboard.HasFormat(CF_ENHMETAFILE))
      then Result := cvMetafile
    else if Clipboard.HasFormat(CF_ICON) then Result := cvIcon
    else if Clipboard.HasFormat(CF_PICTURE) then Result := cvPicture
    else if Clipboard.HasFormat(CF_COMPONENT) then Result := cvComponent
    else if Clipboard.HasFormat(CF_PALETTE) then Result := cvPalette
    else Result := cvUnknown;
  end;

begin
  if IsEmptyClipboard then
    Result := cvEmpty
  else
  begin
    Result := ViewFormat;
    if Result = cvDefault then
      Result := DefaultFormat;
  end;
end;

class function TCustomClipboardViewer.CanDrawFormat(ClipboardFormat: Word): Boolean;
begin
  Result := ClipboardFormatToView(ClipboardFormat) <> cvUnknown;
end;

function TCustomClipboardViewer.ValidFormat(Format: TClipboardViewFormat): Boolean;
begin
  Result := (Format in [cvDefault, cvEmpty, cvUnknown]);
  if not Result then
    if Clipboard.HasFormat(ViewToClipboardFormat(Format)) then
      Result := True;
end;

end.