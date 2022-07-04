unit TBXImgListEdit;

interface

uses
  Windows, Messages, Types, SysUtils, Controls, Menus, Forms, StdCtrls, TBXGraphics;

const
  SOKButtonText = 'OK';
  SCancelButtonText = 'Cancel';
  SAddButtonText = '&Add...';
  SRemoveButtonText = '&Remove';
  SMoveUpButtonText = 'Move &up';
  SMoveDownButtonText = 'Move &down';
  SExportButtonText = '&Export...';

  SSelectAllMenuText = 'Select &All';
  SSelectNoneMenuText = 'Select &None';
  SAddMenuText = '&Add...';
  SRemoveMenuText = '&Remove';

  CMD_SELECTALL  = 1;
  CMD_SELECTNONE = 2;
  CMD_ADD        = 3;
  CMD_REMOVE     = 4;
  CMD_MOVEUP     = 5;
  CMD_MOVEDOWN   = 6;
  CMD_EXPORT     = 7;

type
  TTBXImageListDlg = class(TForm)
  private
    FDIBList: TDIBList32;
    procedure SetDIBList(Value: TDIBList32);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    ImageSize: TPoint;
    procedure AddImage(DIB: TDIB32);
    procedure AddImages;
    procedure ArrangeControls;
    procedure CmdClick(Sender: TObject);
    procedure ExportImages;
    function  GetSelectionInfo(var SelFirst, SelLast: Integer; var Continuous: Boolean): Boolean;
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxClick(Sender: TObject);
    procedure MoveImages(Up: Boolean);
    procedure RemoveImages;
    procedure UpdateStates;
  public
    ListBox: TListBox;
    OkButton: TButton;
    CancelButton: TButton;
    AddButton: TButton;
    RemoveButton: TButton;
    MoveUpButton: TButton;
    MoveDownButton: TButton;
    ExportButton: TButton;
    Popup: TPopupMenu;
    constructor Create; reintroduce;
    procedure AddImagesFromDIB(DIB: TDIB32; UseColorKeyTransparency: Boolean);
    procedure AddImagesFromFile(const FileName: WideString);
    property DIBList: TDIBList32 read FDIBList write SetDIBList;
  end;

implementation

uses
  RTLConsts, Graphics, TB2Common, TBXUtils, Classes, Dialogs, CommCtrl, CommDlg;

{ TTBXImageListDlg }

procedure TTBXImageListDlg.AddImage(DIB: TDIB32);
var
  Idx, Delta: Integer;
  R: TRect;
begin
  DIBList.ImportImage(DIB);
  Idx := ListBox.Items.Add(IntToStr(ListBox.Items.Count));
  ListBox.Selected[Idx] := True;

  { scroll to the new item }
  R := ListBox.ItemRect(Idx);
  Delta := R.Bottom - ClientHeight;
  if Delta > 0 then
  begin
    Delta := (Delta + ListBox.ItemHeight - 1) div ListBox.ItemHeight;
    ListBox.TopIndex := ListBox.TopIndex + Delta;
  end;
end;

procedure TTBXImageListDlg.AddImages;
var
  OFN: OpenFileNameW;
  FileName: WideString;
  ActiveWindow: HWnd;
  WindowList: Pointer;
  FPUControlWord: Word;
  FocusState: TFocusState;
  Res: Boolean;
  Filters: WideString;
begin
  ZeroMemory(@OFN, SizeOf(OFN));
  OFN.lStructSize := SizeOf(OFN);
  OFN.hWndOwner := Handle;
  OFN.hInstance := SysInit.HInstance;
  SetLength(FileName, MAX_PATH + 2);
  FillChar(FileName[1], (MAX_PATH + 2) * SizeOf(WideChar), 0);
  OFN.lpstrFile := PWideChar(FileName);
  OFN.nMaxFile := MAX_PATH;
  Filters :=
    'All supported formats (*.png; *.bmp; *.ico; *.cur)'#0'*.png;*.bmp;*.ico;*.cur'#0 +
    'Portable Network Graphics (*.png)'#0'*.png'#0 +
    'Windows Bitmap (*.bmp)'#0'*.bmp'#0 +
    'Icon (*.ico)'#0'*.ico'#0 +
    'Cursor (*.cur)'#0'*.cur'#0 +
    'All files (*.*)'#0'*.*'#0;
  OFN.lpstrFilter := PWideChar(Filters);
  OFN.nFilterIndex := 0;
  OFN.lpstrInitialDir := '.';
  OFN.Flags := OFN_ENABLESIZING or OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST or OFN_SHOWHELP;

  ActiveWindow := {$IFDEF JR_D7}Application.ActiveFormHandle{$ELSE}GetActiveWindow{$ENDIF};
  WindowList := DisableTaskWindows(0);
  FocusState := SaveFocusState;
  try
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Res := GetOpenFileNameW(OFN);
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    RestoreFocusState(FocusState);
  end;
  if Res then
    AddImagesFromFile(FileName);
end;

procedure TTBXImageListDlg.AddImagesFromDIB(DIB: TDIB32; UseColorKeyTransparency: Boolean);
var
  D: TDIB32;
  Rows, Cols, R, C: Integer;

  procedure MakeTransparent(D: TDIB32);
  var
    C: Cardinal;
    I: Integer;
  begin
    C := D.Bits[(D.Height - 1) * D.RowStride];
    for I := 0 to D.Width * D.Height - 1 do
      if D.Bits[I] = C then D.Bits[I] := D.Bits[I] and $00FFFFFF;
  end;

begin
  ListBox.ClearSelection;
  if (DIB.Width > DIBList.Width) or (DIB.Height > DIBList.Height) then
  begin
    Cols := (DIB.Width + DIBList.Width - 1) div DIBList.Width;
    Rows := (DIB.Height + DIBList.Height - 1) div DIBList.Height;

    case MessageDlg('Image dimensions are larger than Image list dimensions. Separate into ' +
      IntToStr(Rows * Cols) + ' images?', mtInformation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          D := TDIB32.Create;
          try
            D.SetSize(DIBList.Width, DIBList.Height);
            for R := 0 to Rows - 1 do
              for C := 0 to Cols - 1 do
              begin
                D.Clear(0);
                DIB.CopyTo(D, 0, 0,
                  Rect(C * DIBList.Width, R * DIBList.Height,
                    (C + 1) * DIBList.Width, (R + 1) * DIBList.Height));
                if UseColorKeyTransparency then MakeTransparent(D);
                AddImage(D);
              end;
          finally
            D.Free;
          end;
        end;

      mrNo:
        begin
          D := TDIB32.Create;
          try
            D.SetSize(DIBList.Width, DIBList.Height);
            if UseColorKeyTransparency then MakeTransparent(DIB);
            DIB.StretchTo(D, D.ContentRect, DIB.ContentRect, PBR_COPY or PBR_LINEAR);
            AddImage(D);
          finally
            D.Free;
          end;
        end;

      mrCancel: Exit;
    end;
  end
  else AddImage(DIB);
  UpdateStates;
end;

procedure TTBXImageListDlg.AddImagesFromFile(const FileName: WideString);
var
  FileHandle: THandle;
  HandleStream: THandleStream;
  ImgFormat: Integer;
  DIB: TDIB32;
  ColorKeyTransparency: Boolean;
begin
  if not FileExists(FileName) then raise EFOpenError.CreateResFmt(@SFOpenError, [FileName]);

  FileHandle := CreateFileW(PWideChar(FileName), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then EFOpenError.CreateResFmt(@SFOpenError, [FileName]);
  HandleStream := THandleStream.Create(FileHandle);
  try
    ImgFormat := CheckGraphicStream(HandleStream);
    DIB := TDIB32.Create;
    try
      ColorKeyTransparency := False;
      case ImgFormat of
        GS_BMP: begin LoadBMPGraphic(HandleStream, DIB); ColorKeyTransparency := True; end;
        GS_PNG: LoadPNGGraphic(HandleStream, DIB);
        GS_ICO: LoadICOGraphic(HandleStream, DIB);
      else
        raise EFOpenError.CreateResFmt(@SFOpenError, [FileName]);
      end;
      AddImagesFromDIB(DIB, ColorKeyTransparency);
    finally
      DIB.Free;
    end;
  finally
    HandleStream.Free;
    FileClose(FileHandle);
  end;
end;

procedure TTBXImageListDlg.ArrangeControls;
var
  CR, R: TRect;
  I: Integer;
  BtnSize: TPoint;
  DialogUnits: TPoint;
  TextSz, BtnGap, Margins: TPoint;
  Buffer: array [0..51] of Char;
begin
  { calculate dialog units }
  for I := 0 to 25 do Buffer[I] := Chr(I + Integer('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Integer('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(DialogUnits));
  DialogUnits.X := (DialogUnits.X + 26) div 52;

  { calculate button size }
  TextSz.Y := DialogUnits.Y;
  TextSz.X := GetTextWidth(Canvas.Handle, SOKButtonText, True);
  TextSz.X := Max(TextSz.X, GetTextWidth(Canvas.Handle, SCancelButtonText, True));
  TextSz.X := Max(TextSz.X, GetTextWidth(Canvas.Handle, SAddButtonText, True));
  TextSz.X := Max(TextSz.X, GetTextWidth(Canvas.Handle, SRemoveButtonText, True));
  BtnSize := TextSz;
  BtnSize.X := Max(BtnSize.X, TextSz.X);
  BtnSize.Y := Max(BtnSize.Y, TextSz.Y);
  Inc(BtnSize.X, 12);
  Inc(BtnSize.Y, 4);

  BtnSize.X := Max(BtnSize.X, (50 * DialogUnits.X + 2) div 4);
  BtnSize.Y := Max(BtnSize.Y, (14 * DialogUnits.Y + 4) div 8);
  BtnGap.X := (4 * DialogUnits.X + 2) div 4;
  BtnGap.Y := (3 * DialogUnits.Y + 4) div 8;

  { calculate window margins }
  Margins.X := (7 * DialogUnits.X + 2) div 4;
  Margins.Y := (7 * DialogUnits.Y + 4) div 8;

  CR := ClientRect;
  InflateRect(CR, -Margins.X, -Margins.Y);

  R := CR;
  Dec(R.Right, BtnSize.X + BtnGap.X);
  ListBox.BoundsRect := R;

  R := CR;
  R.Left := R.Right - BtnSize.X;
  R.Bottom := R.Top + BtnSize.Y;
  OkButton.BoundsRect := R;

  R.Top := R.Bottom + BtnGap.Y;
  R.Bottom := R.Top + BtnSize.Y;
  CancelButton.BoundsRect := R;

  R.Top := R.Bottom + BtnGap.Y * 2;
  R.Bottom := R.Top + BtnSize.Y;
  AddButton.BoundsRect := R;

  R.Top := R.Bottom + BtnGap.Y;
  R.Bottom := R.Top + BtnSize.Y;
  RemoveButton.BoundsRect := R;

  R.Top := R.Bottom + BtnGap.Y * 2;
  R.Bottom := R.Top + BtnSize.Y;
  MoveUpButton.BoundsRect := R;

  R.Top := R.Bottom + BtnGap.Y;
  R.Bottom := R.Top + BtnSize.Y;
  MoveDownButton.BoundsRect := R;

  R.Top := R.Bottom + BtnGap.Y * 2;
  R.Bottom := R.Top + BtnSize.Y;
  ExportButton.BoundsRect := R;
end;

procedure TTBXImageListDlg.CmdClick(Sender: TObject);
begin
  if Sender is TComponent then
    case TComponent(Sender).Tag of
      CMD_SELECTALL: ListBox.SelectAll;
      CMD_SELECTNONE: ListBox.ClearSelection;
      CMD_ADD: AddImages;
      CMD_REMOVE: RemoveImages;
      CMD_MOVEUP: MoveImages(True);
      CMD_MOVEDOWN: MoveImages(False);
      CMD_EXPORT: ExportImages;
    end;
  UpdateStates;
end;

constructor TTBXImageListDlg.Create;

  procedure MakePopupItem(const Caption: string; Tag: Integer);
  var
    M: TMenuItem;
  begin
    M := TMenuItem.Create(Self);
    M.Caption := Caption;
    M.Tag := Tag;
    M.OnClick := CmdClick;
    Popup.Items.Add(M);
  end;

begin
  inherited CreateNew(Application);
  Caption := 'ImageList Editor';
  AutoScroll := False;
  BorderIcons := BorderIcons - [biMinimize];
//  BorderStyle := bsDialog;

  Popup := TPopupMenu.Create(Self);
  MakePopupItem(SSelectAllMenuText, CMD_SELECTALL);
  MakePopupItem(SSelectNoneMenuText, CMD_SELECTNONE);
  MakePopupItem('-', 0);
  MakePopupItem(SAddMenuText, CMD_ADD);
  MakePopupItem(SRemoveMenuText, CMD_REMOVE);

  ListBox := TListBox.Create(Self);
  ListBox.Parent := Self;
  ListBox.MultiSelect := True;
  ListBox.Style := lbOwnerDrawFixed;
  ListBox.PopupMenu := Popup;
  ListBox.OnClick := ListBoxClick;
  ListBox.OnDrawItem := ListBoxDrawItem;

  OkButton := TButton.Create(Self);
  OkButton.Parent := Self;
  OkButton.Caption := SOkButtonText;
  OkButton.Default := True;
  OkButton.ModalResult := mrOk;

  CancelButton := TButton.Create(Self);
  CancelButton.Parent := Self;
  CancelButton.Caption := SCancelButtonText;
  CancelButton.Cancel := True;
  CancelButton.ModalResult := mrCancel;

  AddButton := TButton.Create(Self);
  AddButton.Parent := Self;
  AddButton.Caption := SAddButtonText;
  AddButton.Tag := CMD_ADD;
  AddButton.OnClick := CmdClick;

  RemoveButton := TButton.Create(Self);
  RemoveButton.Parent := Self;
  RemoveButton.Caption := SRemoveButtonText;
  RemoveButton.Tag := CMD_REMOVE;
  RemoveButton.OnClick := CmdClick;

  MoveUpButton := TButton.Create(Self);
  MoveUpButton.Parent := Self;
  MoveUpButton.Caption := SMoveUpButtonText;
  MoveUpButton.Tag := CMD_MOVEUP;
  MoveUpButton.OnClick := CmdClick;

  MoveDownButton := TButton.Create(Self);
  MoveDownButton.Parent := Self;
  MoveDownButton.Caption := SMoveDownButtonText;
  MoveDownButton.Tag := CMD_MOVEDOWN;
  MoveDownButton.OnClick := CmdClick;

  ExportButton := TButton.Create(Self);
  ExportButton.Parent := Self;
  ExportButton.Caption := SExportButtonText;
  ExportButton.Tag := CMD_EXPORT;
  ExportButton.OnClick := CmdClick;

  Width := 320;
  Height := 320;
  ImageSize := Point(16, 16);

  ArrangeControls;
end;

procedure TTBXImageListDlg.ExportImages;
var
  ExportAll: Boolean;
  I, N: Integer;
  DIB: TDIB32;
  OFN: OpenFileNameW;
  FileName, DefExt: WideString;
  ActiveWindow: HWnd;
  WindowList: Pointer;
  FPUControlWord: Word;
  FocusState: TFocusState;
  Res: Boolean;
  Filters: WideString;
begin
  if ListBox.Count = 0 then Exit;
  ExportAll := True;
  if (ListBox.SelCount > 0) and (ListBox.SelCount <> ListBox.Count) then
  begin
    case MessageDlg('Export all images?'#13#10'Click No to export only selected images.', mtConfirmation, mbYesNoCancel, 0) of
      mrNo: ExportAll := False;
      mrCancel: Exit;
    end;
  end;

  if ExportAll then N := ListBox.Count
  else
  begin
    N := 0;
    for I := 0 to ListBox.Count - 1 do Inc(N, Ord(ListBox.Selected[I]));
  end;
  DIB := TDIB32.Create;
  try
    DIB.SetSize(N * DIBList.Width, DIBList.Height);
    N := 0;
    for I := 0 to ListBox.Count - 1 do
      if ExportAll or ListBox.Selected[I] then
      begin
        DIBList.DIB.CopyTo(DIB, N * DIBList.Width, 0, DIBList.GetImageRect(I));
        Inc(N);
      end;

    ZeroMemory(@OFN, SizeOf(OFN));
    OFN.lStructSize := SizeOf(OFN);
    OFN.hWndOwner := Handle;
    OFN.hInstance := SysInit.HInstance;
    SetLength(FileName, MAX_PATH + 2);
    FillChar(FileName[1], (MAX_PATH + 2) * SizeOf(WideChar), 0);
    DefExt := 'png';
    OFN.lpstrFile := PWideChar(FileName);
    OFN.lpstrDefExt := PWideChar(DefExt);
    OFN.nMaxFile := MAX_PATH;
    OFN.lpstrInitialDir := '.';
    Filters := 'Portable Network Graphics (*.png)'#0'*.png'#0;
    OFN.lpstrFilter := PWideChar(Filters);
    OFN.nFilterIndex := 0;
    OFN.Flags := OFN_ENABLESIZING or OFN_CREATEPROMPT or OFN_OVERWRITEPROMPT or OFN_SHOWHELP;

    ActiveWindow := {$IFDEF JR_D7}Application.ActiveFormHandle{$ELSE}GetActiveWindow{$ENDIF};
    WindowList := DisableTaskWindows(0);
    FocusState := SaveFocusState;
    try
      asm
        FNSTCW  FPUControlWord
      end;
      try
        Res := GetSaveFileNameW(OFN);
      finally
        asm
          FNCLEX
          FLDCW FPUControlWord
        end;
      end;
    finally
      EnableTaskWindows(WindowList);
      SetActiveWindow(ActiveWindow);
      RestoreFocusState(FocusState);
    end;
    if Res then
      SavePNGGraphic(FileName, DIB);

  finally
    DIB.Free;
  end;
end;

function TTBXImageListDlg.GetSelectionInfo(var SelFirst, SelLast: Integer; var Continuous: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ListBox.SelCount < 1 then Exit;
  for I := 0 to ListBox.Count - 1 do
    if ListBox.Selected[I] then
    begin
      SelFirst := I;
      Break;
    end;
  if ListBox.SelCount > 1 then
  begin
    for I := ListBox.Count - 1 downto SelFirst + 1 do
      if ListBox.Selected[I] then
      begin
        SelLast := I;
        Break;
      end;
    Continuous := True;
    for I := SelFirst + 1 to SelLast - 1 do
      if not ListBox.Selected[I] then
      begin
        Continuous := False;
        Break;
      end;
  end
  else
  begin
    SelLast := SelFirst;
    Continuous := True;
  end;
  Result := True;
end;

procedure TTBXImageListDlg.ListBoxClick(Sender: TObject);
begin
  UpdateStates;
end;

procedure TTBXImageListDlg.ListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  GridColors: array [0..1] of Cardinal = ($FF9F9F9F, $FFEFEFEF);
var
  DC: HDC;
  R: TRect;
  Bg, GutterTextClr: TColor;
  Flags: Integer;
  B: HBRUSH;
  S: string;
  OldBkMode: Integer;
  OldTextColor: TColorRef;
  GutterWidth, ImageIndent: Integer;
  Buffer: TDIB32;
  X, Y, I: Integer;
begin
  DC := ListBox.Canvas.Handle;
  Flags := DFCS_BUTTONPUSH;
  Bg := ListBox.Color;
  GutterTextClr := clBtnText;
  if Index and $1 <> 0 then Bg := MixColors(clHighlight, Bg, $8);
  if odSelected in State then
  begin
    Bg := clHighlight;
    Flags := Flags or DFCS_FLAT;
  end;

  if ListBox.Count > 99 then GutterWidth := GetTextWidth(DC, '999', False)
  else if ListBox.Count > 9 then GutterWidth := GetTextWidth(DC, '99', False)
  else GutterWidth := GetTextWidth(DC, '9', False);
  Inc(GutterWidth, 8);

  ImageIndent := 8;

  { Gutter }
  R := Rect;
  R.Right := R.Left + GutterWidth;
  Windows.DrawFrameControl(DC, R, DFC_BUTTON, Flags);
  S := IntToStr(Index);
  InflateRect(R, -2, -2);
  OldBkMode := SetBkMode(DC, TRANSPARENT);
  OldTextColor := SetTextColor(DC, ColorToRGB(GutterTextClr));
  Windows.DrawText(DC, PChar(S), Length(S), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  SetTextColor(DC, OldTextColor);
  SetBkMode(DC, OldBkMode);

  { Image }
  R := Rect;
  R.Left := R.Left + GutterWidth + ImageIndent;
  R.Right := R.Left + ImageSize.X;
  R.Top := (R.Top + R.Bottom - ImageSize.Y) div 2;
  R.Bottom := R.Top + ImageSize.Y;
  InflateRect(R, 2, 2);
  FrameRectEx(DC, R, clBtnFace, True);
  Buffer := TDIB32.Create;
  try
    Buffer.SetSize(ImageSize.X + 2, ImageSize.Y + 2);
    I := 0;
    for Y := 1 to Buffer.Height do
      for X := 1 to Buffer.Width do
      begin
        Buffer.Bits[I] := GridColors[(X shr 2 and $1) xor (Y shr 2 and $1)];
        Inc(I);
      end;
    DIBList.APIDrawEx(Index, Buffer.DC, 1, 1, ImageSize.X, ImageSize.Y, CLR_NONE, CLR_DEFAULT, ILD_TRANSPARENT);
    Buffer.CopyTo(DC, R, Buffer.ContentRect);

    InflateRect(R, 1, 1);
    SaveDC(DC);
    try
      with R do ExcludeClipRect(DC, Left, Top, Right, Bottom);
    
      R := Rect;
      R.Left := R.Left + GutterWidth;
      R.Top := R.Bottom - 1;
      B := CreateDitheredBrush(clBtnFace, ListBox.Color);
      Windows.FillRect(DC, R, B);
      DeleteObject(B);

      R := Rect;
      R.Left := R.Left + GutterWidth;
      R.Right := Rect.Right;
      Dec(R.Bottom);
      FillRectEx(DC, R, Bg);
    finally
      RestoreDC(DC, -1);
    end;
  finally
    Buffer.Free;
  end;
end;

procedure TTBXImageListDlg.MoveImages(Up: Boolean);
var
  I: Integer;
  SelFirst, SelLast, SelCount, Ref: Integer;
  Continuous: Boolean;

  procedure MakeSelectionVisible;
  var
    R: TRect;
    Delta: Integer;
    I: Integer;
  begin
    SelFirst := -1;
    for I := 0 to ListBox.Items.Count - 1 do
      if ListBox.Selected[I] then
      begin
        if SelFirst < 0 then
        begin
          R := ListBox.ItemRect(I);
          SelFirst := I;
        end
        else
        begin
          UnionRect(R, R, ListBox.ItemRect(I));
        end;
      end;
    Delta := 0;
    if R.Bottom > ListBox.ClientHeight then Delta := R.Bottom - ListBox.ClientHeight;
    if R.Top + Delta < 0 then Delta := -R.Top;
    if Delta > 0 then
      ListBox.TopIndex := ListBox.TopIndex + (Delta + ListBox.ItemHeight - 1) div ListBox.ItemHeight
    else if Delta < 0 then
      ListBox.TopIndex := ListBox.TopIndex - (Delta - ListBox.ItemHeight - 1) div ListBox.ItemHeight;
  end;

begin
  if not GetSelectionInfo(SelFirst, SelLast, Continuous) then Exit;
  SelCount := ListBox.SelCount;
  ListBox.Items.BeginUpdate;
  try
    if Up then
    begin
      if SelFirst = 0 then Exit;
      if Continuous then Ref := SelFirst - 1
      else Ref := SelFirst;
      for I := 0 to ListBox.Count - 1 do
        if ListBox.Selected[I] then
        begin
          ListBox.Items.Move(I, Ref);
          DIBList.Move(I, Ref);
          ListBox.Selected[Ref] := True;
          Inc(Ref);
        end;
    end
    else // moving down
    begin
      if Continuous then
      begin
        if SelLast = ListBox.Count - 1 then Exit;
        Ref := SelLast + 1;
        for I := 0 to SelCount - 1 do
        begin
          ListBox.Items.Move(SelFirst, Ref);
          DIBList.Move(SelFirst, Ref);
          ListBox.Selected[Ref] := True;
        end;
      end
      else
      begin
        Ref := SelLast;
        for I := SelLast downto SelFirst do
        begin
          if ListBox.Selected[I] then
          begin
            ListBox.Items.Move(I, Ref);
            DIBList.Move(I, Ref);
            ListBox.Selected[Ref] := True;
            Dec(Ref);
          end;
        end;
      end;
    end;
  finally
    ListBox.Items.EndUpdate;
  end;
  MakeSelectionVisible;
  UpdateStates;
end;

procedure TTBXImageListDlg.RemoveImages;
var
  I: Integer;
  LastIndex: Integer;
begin
  LastIndex := -1;
  for I := DIBList.Count - 1 downto 0 do
    if ListBox.Selected[I] then
    begin
      DIBList.Delete(I);
      LastIndex := I;
    end;
  ListBox.DeleteSelected;
  if LastIndex < 0 then LastIndex := 0;
  if LastIndex >= DIBList.Count then LastIndex := DIBList.Count - 1;
  if LastIndex >= 0 then
    ListBox.Selected[LastIndex] := True;
  UpdateStates;
end;

procedure TTBXImageListDlg.SetDIBList(Value: TDIBList32);
var
  I: Integer;
begin
  if FDIBList = Value then Exit;
  ListBox.Items.Clear;
  FDIBList := Value;
  if Assigned(FDIBList) then
  begin
    for I := 0 to FDIBList.Count - 1 do ListBox.Items.Add(IntToStr(I));
    ImageSize.X := DIBList.Width;
    ImageSize.Y := DIBList.Height;
    ListBox.ItemHeight := ImageSize.Y + 6;
  end;
  UpdateStates;
end;

procedure TTBXImageListDlg.UpdateStates;
var
  SelCount: Integer;
  SelFirst, SelLast: Integer;
  Continuous: Boolean;

  procedure SetEnabled(Cmd: Integer; State: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to Popup.Items.Count - 1 do
      with Popup.Items[I] do
        if Tag = Cmd then Enabled := State;
    for I := 0 to Self.ControlCount - 1 do
      with Controls[I] do
        if Tag = Cmd then Enabled := State;
  end;

begin
  SelCount := ListBox.SelCount;

  if not GetSelectionInfo(SelFirst, SelLast, Continuous) then
  begin
    SetEnabled(CMD_MOVEUP, False);
    SetEnabled(CMD_MOVEDOWN, False);
  end
  else if not Continuous then
  begin
    SetEnabled(CMD_MOVEUP, True);
    SetEnabled(CMD_MOVEDOWN, True);
  end
  else
  begin
    SetEnabled(CMD_MOVEUP, SelFirst > 0);
    SetEnabled(CMD_MOVEDOWN, SelLast < ListBox.Count - 1);
  end;

  SetEnabled(CMD_SELECTALL, SelCount < ListBox.Count);
  SetEnabled(CMD_SELECTNONE, SelCount > 0);
  SetEnabled(CMD_REMOVE, SelCount > 0);
  SetEnabled(CMD_EXPORT, ListBox.Count > 0);
end;

procedure TTBXImageListDlg.WMNCHitTest(var Message: TWMNCHitTest);
var
  R: TRect;
  Pt: TPoint;
begin
  inherited;
  R := ClientRect;
  R.Left := R.Right - GetSystemMetrics(SM_CXHSCROLL);
  R.Top := R.Bottom - GetSystemMetrics(SM_CYVSCROLL);
  Pt := SmallPointToPoint(Message.Pos);
  Windows.ScreenToClient(Handle, Pt);
  if PtInRect(R, Pt) then Message.Result := HTBOTTOMRIGHT;
end;

procedure TTBXImageListDlg.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  CR, R: TRect;
begin
  inherited;
  DC := Canvas.Handle;
  CR := ClientRect;
  R := CR;
  FillRectEx(DC, R, Color);
  R.Left := R.Right - GetSystemMetrics(SM_CXHSCROLL);
  R.Top := R.Bottom - GetSystemMetrics(SM_CYVSCROLL);
  DrawFrameControl(DC, R, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
end;

procedure TTBXImageListDlg.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeControls;
end;

end.
