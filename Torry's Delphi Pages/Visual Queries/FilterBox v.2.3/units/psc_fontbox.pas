{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_fontbox;

interface
{$I psc_defines.inc}

Uses
  dialogs,
  StdCtrls,

  Printers,
  winapi.messages,
  classes,
  controls,
  winapi.windows,
  forms,
  graphics,

  myla_system,
  myla_interfaces,

  psc_edit,
  psc_procs,
  psc_wrapper,
  psc_const;

{------------------------------}

Const
  SPSCCharSetExample_Symbols : String = 'AbCdEfGhIj'; //don't resource
  SPSCCharSetExample_Hebrew  : String = 'אבגדהוזחטי'; //don't resource
  SPSCCharSetExample_Arabic  : String = 'אבגדהוזחטי'; //don't resource

  cDefaultFontPopupWidth = 265;

Type
  TPSCCustomObjListBox = Class(TCustomListBox)
  private
    FObjects: TCollection;
    FSaveObjects: Boolean;
    FScrollTimer: Cardinal;
    FSmoothScroll: Boolean;

    Procedure SetObjects(Value: TCollection);
    Procedure KillTimer;
    Procedure WMTimer(Var Message: TWMTimer); message WM_TIMER;
    Procedure LBAddString(Var Message: TMessage); message LB_ADDSTRING;
    Procedure LBInsertString(Var Message: TMessage); message LB_INSERTSTRING;
    Procedure LBDeleteString(Var Message: TMessage); message LB_DELETESTRING;
    Procedure LBResetContent(Var Message: TMessage); message LB_RESETCONTENT;
    Procedure WMGetDlgCode(Var Message: TMessage); message WM_GETDLGCODE;
  protected
    Procedure CreateWnd; override;
    Procedure DestroyWnd; override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Function CreateObjects: TCollection; virtual; abstract;
    Procedure InsertItem(Var Index: LRESULT); virtual;
    Procedure DeleteItem(Index: Integer); virtual;
    Procedure ResetItems; virtual;
    Procedure MoveItemIndex(Delta: Integer);

    Property Objects: TCollection read FObjects write SetObjects;
    Property SmoothScroll: Boolean read FSmoothScroll write FSmoothScroll
      default false;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure DefaultHandler(Var Message); override;
  End;

  TPSCSelectionShowing = Set Of (ssShowFocusRect,ssShowSelection);

  TPSCCustomImgListBox = Class(TPSCCustomObjListBox)
  private
    FImages: TPSCImageList;
    FShowImages: Boolean;
    FSelectionShowing: TPSCSelectionShowing;

    Procedure SetImages(Value: TPSCImageList);
    Procedure SetShowImages(Value: Boolean);
    Procedure SeTPSCSelectionShowing(Value: TPSCSelectionShowing);
    Procedure CNDrawItem(Var Message: TWMDrawItem); message CN_DRAWITEM;
  protected
    Procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    Procedure CreateParams(Var Params: TCreateParams); override;
    Function CreateObjects: TCollection; override;
    Function GetImageSize(Var Size: TSize): Boolean; virtual;
    Procedure DrawImage(Index: Integer; Const Rect: TRect); virtual;

    Property Images: TPSCImageList read FImages write SetImages;
    Property ShowImages: Boolean read FShowImages write SetShowImages
      default false;
    Property SelectionShowing: TPSCSelectionShowing read FSelectionShowing
      write SeTPSCSelectionShowing default [ssShowFocusRect,ssShowSelection];
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  End;

  TPSCImgListBoxItem = Class(TCollectionItem)
  private
    FImageIndex: TPSCImageIndex;
    Procedure SetImageIndex(Value: TPSCImageIndex);
  public
    Constructor Create(Collection: TCollection); override;
    Procedure Assign(Source: TPersistent); override;
  published
    Property ImageIndex: TPSCImageIndex read FImageIndex write SetImageIndex;
  End;

  TPSCImgListBoxItems = Class(TOwnedCollection)
  protected
    Procedure Update(Item: TCollectionItem); override;
  End;

  TPSCFontBoxOptions = Set Of (fbFontFamiliesOnly,fbAnsiOnly,fbTrueTypeOnly,
    fbFixedPitchOnly,fbNoOEMFonts,fbOEMFontsOnly,fbScalableOnly,
    fbNoVectorFonts,fbWysiwyg);

  TPSCFontBoxDevice = (fbScreen,fbPrinter,fbBoth);

  TPSCFontKind = (fkSystem,fkDoc,fkDocAlias,fkAlias,fkFont);

  TPSCShowFontEvent = Procedure(Sender: TObject; Const AFontName: String;
    AFontType: Byte; APitchAndFamily: Byte; ACharset: TFontCharset;
    Var ACanShow: Boolean) Of Object;

  TPSCFontBoxItem = Class;

  TPSCCustomFontBox = Class(TPSCCustomImgListBox)
  private
    FUpdateCount: Integer;
    FModified: Boolean;
    FImageIndex: TPSCImageIndex;
    FOptions: TPSCFontBoxOptions;
    FDevice: TPSCFontBoxDevice;
    FDrawFonts: Boolean;
    FShowFontAliases: Boolean;
    FSortedDocs: Boolean;
    FOnShowFont: TPSCShowFontEvent;
    FShowFonts: TStrings;
    FFontAliases: TStrings;
    FDocFonts: TStrings;
    FSystemFonts: TStrings;

    Procedure SetSortedDocs(Value: Boolean);
    Procedure SetOptions(Value: TPSCFontBoxOptions);
    Procedure SetDevice(Value: TPSCFontBoxDevice);
    Procedure SetDocFonts(Value: TStrings);
    Procedure SetFontAliases(Value: TStrings);
    Procedure SetShowFonts(Value: TStrings);
    Procedure SetDrawFonts(Value: Boolean);
    Procedure SetFontName(const Value: String);
    Function GetFontName: String;
    Function GetFontNameKind: TPSCFontKind;
    Procedure SetImageIndex(Value: TPSCImageIndex);
    Procedure SetShowFontAliases(Value: Boolean);
    Function GetFontItemsCount: Integer;
    Function GetFontItems(Index: Integer): TPSCFontBoxItem;
    Procedure RemeasureItems(Index: Integer);
    Procedure FontsChanged(Sender: TObject);
    Procedure RebuildSystemFonts(Rebuild: Boolean);
    Procedure CMFontChange(Var Message: TMessage); message CM_FONTCHANGE;
    Procedure CNDrawItem(Var Message: TWMDrawItem); message CN_DRAWITEM;
    Procedure CNMeasureItem(Var Message: TWMMeasureItem);
      message CN_MEASUREITEM;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FONTCHANGED;
    Procedure WMEraseBkgnd(Var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    Procedure CMHintShow(Var Message: TMessage); message CM_HINTSHOW;
  protected
    Function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    Function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    Procedure ReadState(Reader: TReader); override;
    Procedure CreateParams(Var Params: TCreateParams); override;
    Procedure CreateWnd; override;
    Function CreateObjects: TCollection; override;
    Procedure InsertItem(Var Index: LRESULT); override;
    Function GetImageSize(Var Size: TSize): Boolean; override;
    Procedure DrawImage(Index: Integer; Const Rect: TRect); override;
    Procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    Procedure BuildFonts(Items: TStrings);
    Procedure SelectItem(Const AFontName: String; AFontNameKind: TPSCFontKind);

    Property SystemFonts: TStrings read FSystemFonts;
    Property UpdateCount: Integer read FUpdateCount;

    Property Sorted default true;
    Property SortedDocs: Boolean read FSortedDocs write SetSortedDocs
      default false;
    Property SelectionShowing default [ssShowSelection];
    Property Options: TPSCFontBoxOptions read FOptions write SetOptions
      default [fbFontFamiliesOnly];
    Property Device: TPSCFontBoxDevice read FDevice write SetDevice
      default fbBoth;
    Property ShowFontAliases: Boolean read FShowFontAliases
      write SetShowFontAliases default true;
    Property DrawFonts: Boolean read FDrawFonts write SetDrawFonts default true;
    Property ShowImages default true;
    Property ImageIndex: TPSCImageIndex read FImageIndex write SetImageIndex
      default -1;
    Property OnShowFont: TPSCShowFontEvent read FOnShowFont write FOnShowFont;
    Property ShowFonts: TStrings read FShowFonts write SetShowFonts;
    Property FontAliases: TStrings read FFontAliases write SetFontAliases;
    Property DocFonts: TStrings read FDocFonts write SetDocFonts;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure BeginUpdate;
    Procedure EndUpdate;
    Procedure Rebuild;

    Property FontName: String read GetFontName write SetFontName;
    Property FontNameKind: TPSCFontKind read GetFontNameKind;
    Property FontItemsCount: Integer read GetFontItemsCount;
    Property FontItems[Index: Integer]: TPSCFontBoxItem read GetFontItems;
  End;

  TPSCFontBox = Class(TPSCCustomFontBox)
  published
    Property Align;
    Property Anchors;
    Property BiDiMode;
    Property Constraints;
    Property DragKind;
    Property ParentBiDiMode;
    Property BorderStyle;
    Property Color;
    Property DragCursor;
    Property DragMode;
    Property Enabled;
    Property ExtendedSelect;
    Property Font;
    Property ImeMode;
    Property ImeName;
    Property ItemHeight;
    Property MultiSelect;
    Property ParentColor;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property Sorted;
    Property Style;
    Property TabOrder;
    Property TabStop;
    Property TabWidth;
    Property Visible;
    Property OnClick;
    Property OnContextPopup;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnDrawItem;
    Property OnEndDock;
    Property OnStartDock;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMeasureItem;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;

    Property SortedDocs;
    Property SmoothScroll;
    Property SelectionShowing;
    Property Images;
    Property ShowImages;
    Property ImageIndex;
    Property DrawFonts;
    Property OnShowFont;
    Property Options;
    Property Device;
    Property ShowFontAliases;
    Property ShowFonts;
    Property FontAliases;
    Property DocFonts;
  End;

  TPSCFontBoxItem = Class(TCollectionItem)
  private
    FFontType: Byte;
    FPitchAndFamily: Byte;
    FCharset: TFontCharset;
    FKind: TPSCFontKind;

    Procedure SetFontType(Value: Byte);
    Procedure SetPitchAndFamily(Value: Byte);
    Procedure SetCharset(Value: TFontCharset);
    Procedure SetKind(Value: TPSCFontKind);
  public
    Procedure Assign(Source: TPersistent); override;
  published
    Property FontType: Byte read FFontType write SetFontType;
    Property PitchAndFamily: Byte read FPitchAndFamily write SetPitchAndFamily;
    Property Charset: TFontCharset read FCharset write SetCharset;
    Property Kind: TPSCFontKind read FKind write SetKind;
  End;

  TPSCFontBoxPopup = Class(TPSCPopupForm)
  private
    FFontBox: TPSCFontBox;
    Procedure DoFontBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
  public
    Constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    Property FontBox: TPSCFontBox read FFontBox;
  End;

  TPSCFontEdit = Class(TPSCCustomContainerEdit)
  private
    FDropDownWidth: Integer;
    FDropDownCount: Integer;

    Function GetFontName: String;
    Function GetFontBox: TPSCFontBox;
    Function GetDocFonts: TStrings;
    Function GetFBDevice: TPSCFontBoxDevice;
    Function GetFBDrawFonts: Boolean;
    Function GetFBFontAliases: TStrings;
    Function GetFBImageIndex: TPSCImageIndex;
    Function GetFBImages: TPSCImageList;
    Function GetFBOptions: TPSCFontBoxOptions;
    Function GetFBSelectionShowing: TPSCSelectionShowing;
    Function GetFBSmoothScroll: Boolean;
    Function GetFBShowFontAliases: Boolean;
    Function GetFBShowFonts: TStrings;
    Function GetFBShowImages: Boolean;
    Function GetFBSorted: Boolean;
    Function GetFBSortedDocs: Boolean;
    Function GetOnShowFont: TPSCShowFontEvent;

    Procedure SetDropDownWidth(Value: Integer);
    Procedure SetDropDownCount(Value: Integer);
    Procedure SetFontName(const Value: String);
    Procedure SetDocFonts(Value: TStrings);
    Procedure SetFBDevice(Value: TPSCFontBoxDevice);
    Procedure SetFBDrawFonts(Value: Boolean);
    Procedure SetFBFontAliases(Value: TStrings);
    Procedure SetFBImageIndex(Value: TPSCImageIndex);
    Procedure SetFBImages(Value: TPSCImageList);
    Procedure SetFBOptions(Value: TPSCFontBoxOptions);
    Procedure SeFBSelectionShowing(Value: TPSCSelectionShowing);
    Procedure SetFBSmoothScroll(Value: Boolean);
    Procedure SetFBShowFontAliases(Value: Boolean);
    Procedure SetFBShowFonts(Value: TStrings);
    Procedure SetFBShowImages(Value: Boolean);
    Procedure SetFBSorted(Value: Boolean);
    Procedure SetFBSortedDocs(Value: Boolean);
    Procedure SetOnShowFont(Value: TPSCShowFontEvent);
  protected
    Procedure PopupCloseEvent(Sender: TObject; Canceled: Boolean);override;
    Function CreatePopup: TPSCPopupForm; override;
  public
    Procedure UpdatePopup;override;
    Constructor Create(AOwner: TComponent); override;

    Property FontBox: TPSCFontBox read GetFontBox;
  published
    Property PopupColor;
    Property Align;
    Property Anchors;
    Property AutoSize;
    Property BiDiMode;
    Property Constraints;
    Property DragKind;
    Property ParentBiDiMode;
    Property DragCursor;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property FontName: String read GetFontName write SetFontName;
    Property Visible;
    Property OnClick;
    Property OnContextPopup;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnStartDock;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnStartDrag;
    Property OnDropDown;
    Property OnCloseUp;

    Property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth
      default cDefaultFontPopupWidth;
    Property DropDownCount: Integer read FDropDownCount write SetDropDownCount
      default 8;
    Property DocFonts: TStrings read GetDocFonts write SetDocFonts
      stored PopupCreated;
    Property OnChange;

    Property Device: TPSCFontBoxDevice read GetFBDevice write SetFBDevice
      default fbBoth;
    Property DrawFonts: Boolean read GetFBDrawFonts write SetFBDrawFonts
      default true;
    Property FontAliases: TStrings read GetFBFontAliases write SetFBFontAliases
      stored PopupCreated;
    Property ImageIndex: TPSCImageIndex read GetFBImageIndex write SetFBImageIndex
      default -1;
    Property Images: TPSCImageList read GetFBImages write SetFBImages
      stored PopupCreated;
    Property Options: TPSCFontBoxOptions read GetFBOptions write SetFBOptions
      default [fbFontFamiliesOnly];
    Property SelectionShowing: TPSCSelectionShowing read GetFBSelectionShowing
      write SeFBSelectionShowing default [ssShowSelection];
    Property SmoothScroll: Boolean read GetFBSmoothScroll
      write SetFBSmoothScroll default true;
    Property ShowFontAliases: Boolean read GetFBShowFontAliases
      write SetFBShowFontAliases default true;
    Property ShowFonts: TStrings read GetFBShowFonts write SetFBShowFonts
      stored PopupCreated;
    Property ShowImages: Boolean read GetFBShowImages write SetFBShowImages
      default true;
    Property Sorted: Boolean read GetFBSorted write SetFBSorted
      default true;
    Property SortedDocs: Boolean read GetFBSortedDocs write SetFBSortedDocs
      default false;
    Property OnShowFont: TPSCShowFontEvent read GetOnShowFont
      write SetOnShowFont stored PopupCreated;
  End;

Function PSCTrueTypeBitmap: TPSCBitmap;

{------------------------------}

implementation

{-------------------------------------}

Constructor TPSCCustomObjListBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FObjects := CreateObjects
End;

{-------------------------------------}

Destructor TPSCCustomObjListBox.Destroy;
Begin
  If HandleAllocated Then
    DestroyWindowHandle;
  FObjects.Free;
  FObjects := Nil;
  Inherited Destroy
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.CreateWnd;
Begin
  Inherited CreateWnd;
  FSaveObjects := false
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.DestroyWnd;
Begin
  FSaveObjects := true;
  Inherited DestroyWnd;
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.DefaultHandler(Var Message);
Begin
  With TMessage(Message) Do
    If Not FSmoothScroll Or Multiselect Or
      (Msg <> WM_LBUTTONDOWN) And (Msg <> WM_LBUTTONUP) Then
      Inherited DefaultHandler(Message)
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.KillTimer;
Begin
  If FScrollTimer <> 0 Then
    Begin
      winapi.Windows.KillTimer(Handle,FScrollTimer);
      FScrollTimer := 0
    End
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.WMTimer(Var Message: TWMTimer);
Var
  P: TPoint;
Begin
  If Message.TimerID <> 1 Then
    Inherited
  Else
    If Not MouseCapture Then
      KillTimer
    Else
      Begin
        GetCursorPos(P);
        P := ScreenToClient(P);
        If P.y < 0 Then
          MoveItemIndex(-1)
        Else
          If P.y >= ClientHeight Then
            MoveItemIndex(1)
          Else
            KillTimer
      End
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X,Y: Integer);
Var
  Item: Integer;
Begin
  Inherited MouseDown(Button,Shift,X,Y);
  If FSmoothScroll And Not Multiselect And
    Not (csDesigning In ComponentState) And CanFocus Then
    Begin
      SetFocus;
      If (Button = mbLeft) And Not (ssDouble In Shift) Then
        Begin
          MouseCapture := true;
          Item := ItemAtPos(Point(X,Y),true);
          If Item <> -1 Then
            ItemIndex := Item
        End
    End
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X,Y: Integer);
Begin
  Inherited MouseUp(Button,Shift,X,Y);
  If FSmoothScroll And Not Multiselect And (Button = mbLeft) Then
    Begin
      MouseCapture := false;
      KillTimer;
      SendMessage(GetParentHandle,WM_COMMAND,
        MakeLong(Handle,LBN_SELCHANGE),Handle)
    End
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.MouseMove(Shift: TShiftState; X,Y: Integer);
Var
  Index: Integer;
Begin
  Inherited MouseMove(Shift,X,Y);
  If FSmoothScroll And Not Multiselect And (csLButtonDown In ControlState) Then
    If (Y >= 0) And (Y < ClientHeight) Then
      Begin
        KillTimer;
        Index := ItemAtPos(Point(0,Y),true);
        If Index <> -1 Then
          ItemIndex := Index
      End
    Else
      If FScrollTimer = 0 Then
        FScrollTimer := SetTimer(Handle,1,50,Nil)
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.SetObjects(Value: TCollection);
Begin
  If FObjects <> Value Then
    FObjects.Assign(Value)
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.LBAddString(Var Message: TMessage);
Begin
  Inherited;
  With Message Do
    If (Result <> LB_ERR) And (Result <> LB_ERRSPACE) And Not FSaveObjects Then
      InsertItem(Result)
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.LBInsertString(Var Message: TMessage);
Begin
  Inherited;
  With Message Do
    If (Result <> LB_ERR) And (Result <> LB_ERRSPACE) And Not FSaveObjects Then
      InsertItem(Result)
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.LBDeleteString(Var Message: TMessage);
Begin
  Inherited;
  With Message Do
    If (Result <> LB_ERR) And (Result <> LB_ERRSPACE) And Not FSaveObjects Then
      DeleteItem(WParam)
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.LBResetContent(Var Message: TMessage);
Begin
  Inherited;
  If Not FSaveObjects Then
    ResetItems
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.WMGetDlgCode(Var Message: TMessage);
Begin
  Inherited;
  With Message Do
    Result := Result Or DLGC_WANTARROWS
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.InsertItem(Var Index: LRESULT);
Begin
  FObjects.Add.Index := Index
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.DeleteItem(Index: Integer);
Begin
  FObjects.Items[Index].Free
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.ResetItems;
Begin
  FObjects.Clear
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.KeyDown(Var Key: Word; Shift: TShiftState);

  Procedure Move(Delta: Integer);
  Begin
    SendMessage(GetParentHandle,WM_COMMAND,
      MakeLong(Handle,LBN_SELCHANGE),Handle);
    MoveItemIndex(Delta);
    Key := 0
  End;

Begin
  Inherited KeyDown(Key,Shift);
  If (ItemIndex > -1) And FSmoothScroll And Not MultiSelect Then
    Case Key Of
      VK_UP: Move(-1);
      VK_DOWN: Move(1)
    End
End;

{-------------------------------------}

Procedure TPSCCustomObjListBox.MoveItemIndex(Delta: Integer);
Var
  Index: Integer;

  Procedure Move;
  Var
    ScrollInfo: TScrollInfo;
    Y,dy: Integer;
    Rect: TRect;
  Begin
    SendMessage(Handle,WM_SETREDRAW,0,0);
    Try
      Rect := ClientRect;
      With ItemRect(Index) Do
        If Delta > 0 Then
          Begin
            Y := Rect.Bottom;
            Rect.Bottom := Top
          End
        Else
          Begin
            Y := Rect.Top;
            Rect.Top := Bottom;
          End;
      ItemIndex := Index + Delta;
      With ItemRect(Index) Do
        If Delta > 0 Then
          dy := Top - Rect.Bottom
        Else
          dy := Bottom - Rect.Top;
      ScrollWindow(Handle,0,dy,@Rect,Nil);
    Finally
      SendMessage(Handle,WM_SETREDRAW,1,0)
    End;
    With Rect Do
      If Delta > 0 Then
        Begin
          Top := Bottom;
          Bottom := Y
        End
      Else
        Begin
          Bottom := Top;
          Top := Y
        End;
    InvalidateRect(Handle,@Rect,true);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := 0;
    SetScrollInfo(Handle,SB_VERT,ScrollInfo,true)
  End;

Begin
  Index := ItemIndex;
  If (Index + Delta < 0) Or (Index + Delta >= Items.Count) Then
    Exit;
  If Delta < 0 Then
    If Index + Delta >= TopIndex Then
      ItemIndex := Index + Delta
    Else
      Move
  Else
    If ItemRect(Index + Delta).Bottom <= ClientHeight Then
      ItemIndex := Index + Delta
    Else
      Move
End;

{-------------------------------------}

Constructor TPSCImgListBoxItem.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  FImageIndex := -1
End;

{-------------------------------------}

Procedure TPSCImgListBoxItem.Assign(Source: TPersistent);
Begin
  If Source Is TPSCImgListBoxItem Then
    With TPSCImgListBoxItem(Source) Do
      Self.FImageIndex := FImageIndex
  Else
    Inherited Assign(Source)
End;

{-------------------------------------}

Procedure TPSCImgListBoxItem.SetImageIndex(Value: TPSCImageIndex);
Begin
  If FImageIndex <> Value Then
    Begin
      FImageIndex := Value;
      Changed(false)
    End
End;

{-------------------------------------}

Procedure TPSCImgListBoxItems.Update(Item: TCollectionItem);
Var
  Rect: TRect;
Begin
  With TPSCCustomImgListBox(GetOwner) Do
    If FShowImages And (FImages <> Nil) And HandleAllocated Then
      If Item = Nil Then
        Invalidate
      Else
        Begin
          Rect := ItemRect(Item.Index);
          InvalidateRect(Handle,@Rect,true)
        End
End;

{-------------------------------------}

Constructor TPSCCustomImgListBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FSelectionShowing := [ssShowFocusRect,ssShowSelection]
End;

{-------------------------------------}

Destructor TPSCCustomImgListBox.Destroy;
Begin
  Inherited;
End;

{-------------------------------------}

Procedure TPSCCustomImgListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  If (Operation = opRemove) And (AComponent = FImages) Then
    Images := Nil
End;

{-------------------------------------}

Procedure TPSCCustomImgListBox.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
    If (Self.Style = lbStandard) And FShowImages Then
      Style := Style Or LBS_OWNERDRAWFIXED
End;

{-------------------------------------}

Function TPSCCustomImgListBox.CreateObjects: TCollection;
Begin
  Result := TPSCImgListBoxItems.Create(Self,TPSCImgListBoxItem)
End;

{-------------------------------------}

Procedure TPSCCustomImgListBox.SetImages(Value: TPSCImageList);
Begin
  If FImages <> Value Then
    Begin
      If FImages <> Nil Then
        Begin
          RemoveFreeNotification(FImages)
        End;
      FImages := Value;
      If FImages <> Nil Then
        Begin
          FreeNotification(FImages)
        End;
      If FShowImages Then
        Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCCustomImgListBox.SetShowImages(Value: Boolean);
Begin
  If FShowImages <> Value Then
    Begin
      FShowImages := Value;
      RecreateWnd
    End
End;

{-------------------------------------}

Procedure
  TPSCCustomImgListBox.SeTPSCSelectionShowing(Value: TPSCSelectionShowing);
Var
  Index: Integer;
  Rect: TRect;
Begin
  If FSelectionShowing <> Value Then
    Begin
      FSelectionShowing := Value;
      If HandleAllocated Then
        Begin
          Index := ItemIndex;
          If Index >= 0 Then
            Begin
              Rect := ItemRect(Index);
              InvalidateRect(Handle,@Rect,true)
            End
        End
    End
End;

{-------------------------------------}

Function TPSCCustomImgListBox.GetImageSize(Var Size: TSize): Boolean;
Begin
  Result := FImages <> Nil;
  If Result Then
    Begin
      Size.cx := FImages.Width;
      Size.cy := FImages.Height;
    End
End;

{-------------------------------------}

Procedure TPSCCustomImgListBox.DrawImage(Index: Integer; Const Rect: TRect);
Begin
  Canvas.FillRect(Rect);
  If FImages <> Nil Then
    With FImages,Rect Do
      Draw(Canvas,(Left + Right - Width) Div 2,(Top + Bottom - Height) Div 2,
        TPSCImgListBoxItem(Objects.Items[Index]).ImageIndex);
End;

{-------------------------------------}

Procedure TPSCCustomImgListBox.CNDrawItem(Var Message: TWMDrawItem);
Var
  State: TOwnerDrawState;
  Rect: TRect;
  Size: TSize;
  ShowImage: BOolean;
Begin
  With Message.DrawItemStruct^ Do
    Begin
      State := TOwnerDrawState(TPSCLongRec(itemState).Lo);
      If Not (ssShowFocusRect In SelectionShowing) Then
        State := State - [odFocused];
      If Not (ssShowSelection In SelectionShowing) Then
        State := State - [odSelected];
      Canvas.Handle := hDC;
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      If (Integer(itemID) >= 0) And (odSelected In State) Then
        Begin
          Canvas.Brush.Color := clPSCHighlight;
          Canvas.Font.Color := clPSCHighlightText
        End;
      If Integer(itemID) >= 0 Then
        Begin
          ShowImage := FShowImages And GetImageSize(Size);
          If ShowImage Then
            Begin
              With rcItem Do
                Rect := Classes.Rect(Left,Top,Left + Size.cx + 2,Bottom);
              DrawImage(itemID,Rect);
              rcItem.Left := Rect.Right
            End;
          DrawItem(itemID,rcItem,State)
        End
      Else
        Begin
          ShowImage := false;
          Canvas.FillRect(rcItem)
        End;
      If odFocused In State Then
        Begin
          If ShowImage Then
            rcItem.Left := Rect.Left;
          DrawFocusRect(hDC,rcItem)
        End;
      Canvas.Handle := 0
    End
End;

{-------------------------------------}

Procedure TPSCFontBoxItem.SetFontType(Value: Byte);
Begin
  If FFontType <> Value Then
    Begin
      FFontType := Value;
      Changed(false)
    End
End;

{-------------------------------------}

Procedure TPSCFontBoxItem.SetPitchAndFamily(Value: Byte);
Begin
  If FPitchAndFamily <> Value Then
    Begin
      FPitchAndFamily := Value;
      Changed(false)
    End
End;

{-------------------------------------}

Procedure TPSCFontBoxItem.SetCharset(Value: TFontCharset);
Begin
  If FCharset <> Value Then
    Begin
      FCharset := Value;
      Changed(false)
    End
End;

{-------------------------------------}

Procedure TPSCFontBoxItem.SetKind(Value: TPSCFontKind);
Begin
  If FKind <> Value Then
    Begin
      FKind := Value;
      Changed(false)
    End
End;

{-------------------------------------}

Procedure TPSCFontBoxItem.Assign(Source: TPersistent);
Begin
  If Source Is TPSCFontBoxItem Then
    With TPSCFontBoxItem(Source) Do
      Begin
        Self.FFontType := FFontType;
        Self.FPitchAndFamily := FPitchAndFamily;
        Self.FCharset := FCharset;
        Self.FKind := FKind
      End
  Else
    Inherited Assign(Source)
End;

{-------------------------------------}

Type
  TFontBoxHint = Class(THintWindow)
    Procedure ActivateHintData(Rect: TRect; Const AHint: String;
      AData: Pointer); override;
  End;

  PEnumFontRec = ^TEnumFontRec;
  TEnumFontRec = Record
    FontBox: TPSCCustomFontBox;
    Fonts: TStrings;
    Items: TStrings;
  End;

  TFontParams = Packed Record
    Case Boolean Of
      false: (
        FontType: Byte;
        PitchAndFamily: Byte;
        Charset: TFontCharset);
      true: (Value: Integer)
  End;

  {-------------------------------------}

Function EnumFontFamProc(Var EnumLogFont: TEnumLogFont;
  Var TextMetric: TNewTextMetric; FontType: Integer;
  Data: Longint): Integer; stdcall;
Var
  FaceName: String;
  CanShow: Boolean;
  FontParams: TFontParams;

  Function FontExists: Boolean;
  Var
    I: Integer;
  Begin
    With PEnumFontRec(Data)^ Do
      Begin
        I := Items.IndexOf(FaceName);
        Result := I <> -1;
        If Result Then
          Begin
            FontParams := TFontParams(Items.Objects[I]);
            FontParams.FontType := FontParams.FontType Or FontType;
            Items.Objects[I] := TObject(FontParams)
          End
      End
  End;

Begin
  FaceName := EnumLogFont.elfLogFont.lfFaceName;

  If (FaceName<>'') and (FaceName[1]='@') then
    Delete(FaceName,1,1);

  With PEnumFontRec(Data)^ Do
    Begin
      If Not FontExists And
        (Not (fbFontFamiliesOnly In FontBox.FOptions) Or
        (Fonts = Nil) Or (Fonts.IndexOf(FaceName) <> -1)) And
        (Not (fbAnsiOnly In FontBox.FOptions) Or
        (EnumLogFont.elfLogFont.lfCharSet <> SYMBOL_CHARSET)) And
        (Not (fbNoOEMFonts In FontBox.FOptions) Or
        (EnumLogFont.elfLogFont.lfCharSet <> OEM_CHARSET)) And
        (Not (fbTrueTypeOnly In FontBox.FOptions) Or
        (FontType And TRUETYPE_FONTTYPE <> 0)) And
        (Not (fbScalableOnly In FontBox.FOptions) Or
        (FontType And RASTER_FONTTYPE = 0)) And
        (Not (fbNoVectorFonts In FontBox.FOptions) Or
        (FontType And (TRUETYPE_FONTTYPE Or RASTER_FONTTYPE) <> 0)) And
        (Not (fbOEMFontsOnly In FontBox.FOptions) Or
        (EnumLogFont.elfLogFont.lfCharSet = OEM_CHARSET)) And
        (Not (fbFixedPitchOnly In FontBox.FOptions) Or
        (EnumLogFont.elfLogFont.lfPitchAndFamily And FIXED_PITCH <> 0)) Then
        Begin
          CanShow := true;
          If Assigned(FontBox.OnShowFont) Then
            FontBox.OnShowFont(FontBox,FaceName,FontType,
              EnumLogFont.elfLogFont.lfPitchAndFamily,
              EnumLogFont.elfLogFont.lfCharSet,CanShow);
          If CanShow Then
            Begin
              FontParams.FontType := Byte(FontType);
              FontParams.PitchAndFamily :=
                EnumLogFont.elfLogFont.lfPitchAndFamily;
              FontParams.Charset := EnumLogFont.elfLogFont.lfCharSet;

              Items.AddObject(FaceName,TObject(FontParams.Value))
            End
        End;
      Result := 1
    End
End;

{-------------------------------------}

Constructor TPSCCustomFontBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FSystemFonts := TStringList.Create;
  TStringList(FSystemFonts).Sorted := true;
  TStringList(FSystemFonts).OnChange := FontsChanged;
  FDocFonts := TStringList.Create;
  TStringList(FDocFonts).OnChange := FontsChanged;
  FFontAliases := TStringList.Create;
  TStringList(FFontAliases).OnChange := FontsChanged;
  FShowFonts := TStringList.Create;
  TStringList(FShowFonts).OnChange := FontsChanged;
  ShowImages := true;
  FDrawFonts := true;
  FShowFontAliases := true;
  FImageIndex := -1;
  ItemHeight := 25;
  FOptions := [fbFontFamiliesOnly];
  FDevice := fbBoth;
  Sorted := true;
  SelectionShowing := [ssShowSelection]
End;

{-------------------------------------}

Destructor TPSCCustomFontBox.Destroy;
Begin
  If HandleAllocated Then
    DestroyHandle;
  FShowFonts.Free;
  FFontAliases.Free;
  FDocFonts.Free;
  FSystemFonts.Free;
  Inherited Destroy
End;

{-------------------------------------}

Function TPSCCustomFontBox.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
Begin
  If Not Inherited DoMouseWheelDown(Shift,MousePos) Then
    TopIndex := TopIndex + 1;
  Result := true
End;

{-------------------------------------}

Function TPSCCustomFontBox.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
Begin
  If Not Inherited DoMouseWheelUp(Shift,MousePos) Then
    TopIndex := TopIndex - 1;
  Result := true
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.ReadState(Reader: TReader);
Begin
  BeginUpdate;
  Try
    Inherited ReadState(Reader);
    RebuildSystemFonts(true)
  Finally
    EndUpdate
  End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
    Style := Style And Not (LBS_OWNERDRAWFIXED Or LBS_SORT) Or
      LBS_OWNERDRAWVARIABLE
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.CreateWnd;
Begin
  Inherited CreateWnd;
  RebuildSystemFonts(false)
End;

{-------------------------------------}

Function TPSCCustomFontBox.CreateObjects: TCollection;
Begin
  Result := TPSCImgListBoxItems.Create(Self,TPSCFontBoxItem)
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.InsertItem(Var Index: LRESULT);
Var
  SysIndex: Integer;
  FontParams: TFontParams;
Begin
  Inherited InsertItem(Index);
  SysIndex := FSystemFonts.IndexOf(Items[Index]);
  If SysIndex <> -1 Then
    With FontItems[Index] Do
      Begin
        FontParams.Value := Integer(FSystemFonts.Objects[SysIndex]);
        FontType := FontParams.FontType;
        PitchAndFamily := FontParams.PitchAndFamily;
        Charset := FontParams.Charset
      End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.RemeasureItems(Index: Integer);
Var
  I: Integer;
  MeasureItemMsg: TWMMeasureItem;
  MeasureItemStruct: TMeasureItemStruct;

  Procedure DoMeasureItem(Index: Integer);
  Begin
    MeasureItemStruct.itemID := Index;
    MeasureItemStruct.itemData := Longint(Pointer(Items.Objects[Index]));
    MeasureItemStruct.itemWidth := Width;
    MeasureItemStruct.itemHeight := ItemHeight;
    Dispatch(MeasureItemMsg);
    Perform(LB_SETITEMHEIGHT,Index,
      MakeLParam(MeasureItemStruct.itemHeight,0))
  End;

Begin
  If HandleAllocated Then
    Begin
      MeasureItemMsg.Msg := CN_MEASUREITEM;
      MeasureItemMsg.IDCtl := Handle;
      MeasureItemMsg.MeasureItemStruct := @MeasureItemStruct;
      MeasureItemStruct.CtlType := ODT_LISTBOX;
      MeasureItemStruct.CtlID := Handle;
      If Index <> -1 Then
        DoMeasureItem(Index)
      Else
        For I := 0 To Items.Count - 1 Do
          DoMeasureItem(I);
      SendMessage(Handle,LB_SETTOPINDEX,TopIndex,0)
    End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.BuildFonts(Items: TStrings);
Var
  I: Integer;
  DC: HDC;
  EnumFontRec: TEnumFontRec;
  TmpItems: TStrings;
Begin
  EnumFontRec.FontBox := Self;
  EnumFontRec.Items := Items;
  Items.BeginUpdate;
  Try
    If (FDevice = fbBoth) And (fbWysiwyg In FOptions) Then
      TmpItems := TStringList.Create
    Else
      TmpItems := Items;
    Try
      Items.Clear;
      If FDevice In [fbScreen,fbBoth] Then
        Begin
          DC := GetDC(0);
          Try
            If fbFontFamiliesOnly In FOptions Then
              EnumFontRec.Fonts := Screen.Fonts;
            EnumFontFamilies(DC,Nil,@EnumFontFamProc,Longint(@EnumFontRec))
          Finally
            ReleaseDC(0,DC)
          End
        End;
      If FDevice In [fbPrinter,fbBoth] Then
        With Printer Do
          If Printers.Count > 0 Then
            Begin
              If fbFontFamiliesOnly In FOptions Then
              Try
                EnumFontRec.Fonts := Fonts
              Except
                On EPrinter Do
                  ;
              Else
                Raise;
              End;
              EnumFontFamilies(Handle,Nil,@EnumFontFamProc,Longint
                (@EnumFontRec))
            End;
      If TmpItems <> Items Then
        Begin
          TStringList(TmpItems).Sorted := true;
          I := 0;
          While I < Items.Count Do
            Begin
              If TmpItems.IndexOf(Items[I]) = -1 Then
                Items.Delete(I)
              Else
                I := I + 1
            End
        End
    Finally
      If TmpItems <> Items Then
        TmpItems.Free
    End
  Finally
    Items.EndUpdate
  End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.RebuildSystemFonts(Rebuild: Boolean);
Begin
  If Not Rebuild And (csReading In ComponentState) Then
    FModified := true
  Else
    Begin
      BeginUpdate;
      Try
        TStringList(FSystemFonts).Sorted := false;
        BuildFonts(FSystemFonts);
        TStringList(FSystemFonts).Sorted := true
      Finally
        EndUpdate
      End
    End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.BeginUpdate;
Begin
  Objects.BeginUpdate;
  FUpdateCount := FUpdateCount + 1
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.EndUpdate;
Begin
  FUpdateCount := FUpdateCount - 1;
  If (FUpdateCount = 0) And FModified Then
    Rebuild;
  Objects.EndUpdate
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.Rebuild;
Var
  I: Integer;
  AFontName,Font: String;
  AFontNameKind: TPSCFontKind;
  ATopIndex: Integer;
  TmpItems: TStringList;
  ShowFonts,TmpItems2: TStrings;
Begin
  FModified := true;
  If FUpdateCount > 0 Then
    Exit;
  Items.BeginUpdate;
  Try
    ATopIndex := TopIndex;
    AFontName := FontName;
    AFontNameKind := FontNameKind;
    BeginUpdate;
    Try
      If Sorted Or FSortedDocs Then
        TmpItems := TStringList.Create
      Else
        TmpItems := Nil;
      Try
        Clear;
        If FShowFonts.Count > 0 Then
          ShowFonts := FShowFonts
        Else
          ShowFonts := FSystemFonts;
        If Not FSortedDocs Then
          TmpItems2 := FDocFonts
        Else
          Begin
            TmpItems.Assign(FDocFonts);
            TmpItems.Sorted := true;
            TmpItems2 := TmpItems
          End;
        For I := 0 To TmpItems2.Count - 1 Do
          Begin
            Font := TmpItems2[I];
            If (ShowFonts.IndexOf(Font) <> -1) And
              ((ShowFonts <> FShowFonts) Or (FSystemFonts.IndexOf(Font) <> -1))
                Then
              FontItems[Items.Add(Font)].Kind := fkDoc
            Else
              If FShowFontAliases And (FFontAliases.IndexOf(Font) <> -1) Then
                FontItems[Items.Add(Font)].Kind := fkDocAlias
          End;
        If Sorted Then
          Begin
            TmpItems.Sorted := false;
            TmpItems.Clear;
            If FShowFontAliases Then
              For I := 0 To FFontAliases.Count - 1 Do
                TmpItems.AddObject(FFontAliases[I],TObject(fkAlias));
            For I := 0 To ShowFonts.Count - 1 Do
              Begin
                Font := ShowFonts[I];
                If FSystemFonts.IndexOf(Font) <> -1 Then
                  TmpItems.AddObject(Font,TObject(fkFont))
              End;
            TmpItems.Sorted := true;
            For I := 0 To TmpItems.Count - 1 Do
              With FontItems[Items.Add(TmpItems[I])] Do
                Kind := TPSCFontKind(TmpItems.Objects[I])
          End
        Else
          Begin
            If FShowFontAliases Then
              For I := 0 To FFontAliases.Count - 1 Do
                FontItems[Items.Add(FFontAliases[I])].Kind := fkAlias;
            For I := 0 To ShowFonts.Count - 1 Do
              Begin
                Font := ShowFonts[I];
                If FSystemFonts.IndexOf(Font) <> -1 Then
                  FontItems[Items.Add(Font)].Kind := fkFont
              End
          End
      Finally
        TmpItems.Free
      End;
      FModified := false
    Finally
      EndUpdate
    End;
    RemeasureItems(-1);
    SelectItem(AFontName,AFontNameKind);
    TopIndex := ATopIndex
  Finally
    Items.EndUpdate
  End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.FontsChanged(Sender: TObject);
Begin
  Rebuild
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetSortedDocs(Value: Boolean);
Begin
  If FSortedDocs <> Value Then
    Begin
      FSortedDocs := Value;
      Rebuild
    End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetOptions(Value: TPSCFontBoxOptions);
Begin
  If FOptions <> Value Then
    Begin
      FOptions := Value;
      RebuildSystemFonts(false)
    End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetDevice(Value: TPSCFontBoxDevice);
Begin
  If FDevice <> Value Then
    Begin
      FDevice := Value;
      RebuildSystemFonts(false)
    End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetDocFonts(Value: TStrings);
Begin
  If FDocFonts <> Value Then
    FDocFonts.Assign(Value)
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetFontAliases(Value: TStrings);
Begin
  If FFontAliases <> Value Then
    FFontAliases.Assign(Value)
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetShowFontAliases(Value: Boolean);
Begin
  If FShowFontAliases <> Value Then
    Begin
      FShowFontAliases := Value;
      Rebuild
    End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetShowFonts(Value: TStrings);
Begin
  If FShowFonts <> Value Then
    FShowFonts.Assign(Value)
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetDrawFonts(Value: Boolean);
Begin
  If FDrawFonts <> Value Then
    Begin
      FDrawFonts := Value;
      Items.BeginUpdate;
      Try
        RemeasureItems(-1)
      Finally
        Items.EndUpdate
      End
    End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetFontName(const Value: String);
Var
  Index: Integer;
Begin
  Index := Items.IndexOf(Value);
  If Index <> -1 Then
    ItemIndex := Index
End;

{-------------------------------------}

Function TPSCCustomFontBox.GetFontName: String;
Var
  Index: Integer;
Begin
  Index := ItemIndex;
  If (Index < 0) Or (Index >= Items.Count) Then
    Result := ''
  Else
    Result := Items[Index]
End;

{-------------------------------------}

Function TPSCCustomFontBox.GetFontNameKind: TPSCFontKind;
Var
  Index: Integer;
Begin
  Index := ItemIndex;
  If (Index <= 0) Or (Index > Items.Count) Then
    Result := fkSystem
  Else
    Result := FontItems[Index].Kind
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SelectItem(Const AFontName: String;
  AFontNameKind: TPSCFontKind);
Var
  I: Integer;
Begin
  For I := 0 To Items.Count - 1 Do
    If ((AFontNameKind = fkSystem) Or (FontItems[I].Kind = AFontNameKind)) And
      (PSCCompareText(Items[I],AFontName) = 0) Then
      Begin
        ItemIndex := I;
        Break
      End
End;

{-------------------------------------}

Function TPSCCustomFontBox.GetFontItemsCount: Integer;
Begin
  Result := Objects.Count
End;

{-------------------------------------}

Function TPSCCustomFontBox.GetFontItems(Index: Integer): TPSCFontBoxItem;
Begin
  Result := TPSCFontBoxItem(Objects.Items[Index])
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.SetImageIndex(Value: TPSCImageIndex);
Begin
  If FImageIndex <> Value Then
    Begin
      FImageIndex := Value;
      If ShowImages And (Images <> Nil) Then
        Invalidate
    End
End;

{-------------------------------------}

Function TPSCCustomFontBox.GetImageSize(Var Size: TSize): Boolean;
Begin
  If (Images <> Nil) And (FImageIndex <> -1) Then
    Begin
      Size.cx := Images.Width;
      Size.cy := Images.Height;
      Result := true
    End
  Else
    Begin
      Size.cx := PSCTrueTypeBitmap.Width;
      Size.cy := PSCTrueTypeBitmap.Height;
      Result := true
    End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.DrawImage(Index: Integer; Const Rect: TRect);
Begin
  Canvas.FillRect(Rect);
  With FontItems[Index] Do
    If (FontType And TRUETYPE_FONTTYPE <> 0) And (Kind <> fkAlias) Then
      If (Images <> Nil) And (FImageIndex <> -1) Then
        With Images,Rect Do
          Draw(Canvas,(Left + Right - Width) Div 2,(Top + Bottom - Height) Div
            2,
            FImageIndex)
      Else
        With Rect Do
          Canvas.Draw((Left + Right - PSCTrueTypeBitmap.Width) Div 2,
            (Top + Bottom - PSCTrueTypeBitmap.Height) Div 2,PSCTrueTypeBitmap);
End;

{-------------------------------------}

type
  THackCanvas=class(TPSCCanvas)
  end;

Procedure TPSCCustomFontBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
Var
  FontName: String;
  Caption: String;
  L: Integer;
  Size: TSize;
Begin
  If Assigned(OnDrawItem) Or (Index >= Items.Count) Then
    Inherited DrawItem(Index,Rect,State)
  Else
    Begin
      FontName := Items[Index];
      Caption := FontName;
      With FontItems[Index] Do
        Begin
          If Not FDrawFonts Or (Kind In [fkDocAlias,fkAlias]) Then
            Begin
              Size := Canvas.TextExtent(FontName);
              Canvas.TextRect(Rect,Rect.Left + 2,
                (Rect.Top + Rect.Bottom - Size.cy) Div 2,FontName)
            End
          Else
            Begin
              If (CharSet In [SYMBOL_CHARSET,HEBREW_CHARSET,ARABIC_CHARSET])
              Then
                Begin
                  Canvas.Font.CharSet := DEFAULT_CHARSET;
                  Caption := Caption + ' ';
                  Size := Canvas.TextExtent(Caption);
                  L := Rect.Right;
                  Rect.Right := Rect.Left + Size.cx;
                  Canvas.TextRect(Rect,Rect.Left + 2,
                    (Rect.Top + Rect.Bottom - Size.cy) Div 2,Caption);
                  Rect.Left := Rect.Right;
                  Rect.Right := L;
                  Case Charset Of
                    SYMBOL_CHARSET: Caption := SPSCCharSetExample_Symbols;
                    HEBREW_CHARSET: Caption := SPSCCharSetExample_Hebrew;
                    ARABIC_CHARSET: Caption := SPSCCharSetExample_Arabic;
                  else
                    Caption := '';
                  End
                End;
              Canvas.Font.Name := FontName;
              Canvas.Font.CharSet := Charset;
              If Caption<>'' then
              begin
                Size := Canvas.TextExtent(Caption);
                Canvas.TextRect(Rect,Rect.Left + 2,
                  (Rect.Top + Rect.Bottom - Size.cy) Div 2,Caption);
              end;
            End;
        End;
    End;
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.WMEraseBkgnd(Var Message: TWMEraseBkgnd);
Var
  Rect: TRect;
Begin
  If Columns = 0 Then
    Begin
      Rect := ClientRect;
      Rect.Top := ItemRect(Items.Count - 1).Bottom;
      If Rect.Top < Rect.Bottom Then
        FillRect(Message.DC,Rect,Brush.Handle);
      Message.Result := 1
    End
  Else
    Inherited
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.CMFontChange(Var Message: TMessage);
Begin
  Inherited;
  RebuildSystemFonts(false)
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.CNDrawItem(Var Message: TWMDrawItem);
Var
  Rect: TRect;
  DrawLine: Boolean;
Begin
  With Message.DrawItemStruct^ Do
    Begin
      DrawLine := (Integer(itemID) >= 0) And (Integer(itemID) < Items.Count -
        1)
        And
        (FontItems[Integer(itemID)].Kind In [fkDoc,fkDocAlias]) And
      Not (FontItems[Integer(itemID) + 1].Kind In [fkDoc,fkDocAlias]);
      If DrawLine Then
        With rcItem Do
          Begin
            Rect := Classes.Rect(Left,Bottom - 3,Right,Bottom);
            Bottom := Bottom - 3
          End
    End;
  Inherited;
  If DrawLine Then
    With Message.DrawItemStruct^ Do
      Begin
        FillRect(hDC,Rect,Canvas.Brush.Handle);
        DrawEdge(hDC,Rect,BDR_RAISEDOUTER,BF_BOTTOM Or BF_TOP Or BF_FLAT)
      End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.CNMeasureItem(Var Message: TWMMeasureItem);
Var
  DC: HDC;
  Ht: Integer;
  FontName: String;
  Size: TSize;
Begin
  If FUpdateCount <> 0 Then
    Exit;
  Inherited;
  With Message.MeasureItemStruct^ Do
    If (Integer(ItemID) >= 0) And (Integer(ItemID) < Items.Count) Then
      Begin
        If ShowImages And GetImageSize(Size) Then
          itemHeight := Size.cy
        Else
          itemHeight := 0;
        If Style <> lbOwnerDrawVariable Then
          Begin
            If Integer(itemHeight) < Self.ItemHeight Then
              itemHeight := Self.ItemHeight
          End
        Else
          Begin
            DC := GetDC(0);
            Try
              Canvas.Handle := DC;
              Try
                Canvas.Font := Font;
                FontName := Items[ItemID];
                With FontItems[Integer(ItemID)] Do
                  If FDrawFonts And Not (Kind In [fkDocAlias,fkAlias]) Then
                    Begin
                      Canvas.Font.Name := FontName;
                      Canvas.Font.Charset := Charset;

                      If Charset = SYMBOL_CHARSET Then  //!!
                        Begin
                          Ht := Canvas.TextHeight(SPSCCharSetExample_Symbols) + 2;
                          If Integer(itemHeight) < Ht Then
                            itemHeight := Ht;
                          Canvas.Font := Font
                        End;

                    End;
                Ht := Canvas.TextHeight(FontName) + 2
              Finally
                Canvas.Handle := 0
              End
            Finally
              ReleaseDC(0,DC)
            End;
            If Integer(itemHeight) < Ht Then
              itemHeight := Ht
          End;
        If Style = lbOwnerDrawVariable Then
          MeasureItem(itemID,Integer(itemHeight))
      End
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.CMFontChanged(Var Message: TMessage);
Begin
  Inherited;
  { Work around of list box bug }
  RemeasureItems(-1)
End;

{-------------------------------------}

Procedure TPSCCustomFontBox.CMHintShow(Var Message: TMessage);
Var
  Item: Integer;
Begin
  Inherited;
  With TCMHintShow(Message).HintInfo^ Do
    Begin
      Item := ItemAtPos(CursorPos,false);
      If Item = -1 Then
        Message.Result := 1;
      CursorRect := ItemRect(Item);
      HintWindowClass := TFontBoxHint
    End
End;

{-------------------------------------}

var
  FPSCTrueTypeBitmap: TPSCBitmap = Nil;

Function PSCTrueTypeBitmap: TPSCBitmap;
Begin
  If FPSCTrueTypeBitmap = Nil Then
    Begin
      FPSCTrueTypeBitmap := TPSCBitmap.Create;
      PSCLoadBitmapFromResource(FPSCTrueTypeBitmap,SPSCResName_Img_TrueType);
      FPSCTrueTypeBitmap.Transparent := true;
    End;
  Result := FPSCTrueTypeBitmap;
End;

{-------------------------------------}

Procedure TFontBoxHint.ActivateHintData(Rect: TRect; Const AHint: String;
  AData: Pointer);
Var
  P: TPoint;
  Control: TControl;
  Item: Integer;
  CalcRect: TRect;
Begin
  GetCursorPos(P);
  Control := FindDragTarget(P,false);
  If Control Is TPSCCustomFontBox Then
    Begin
      P := Control.ScreenToClient(P);
      Item := TPSCCustomFontBox(Control).ItemAtPos(P,true);
      If Item <> -1 Then
        Begin
          With TPSCCustomFontBox(Control) Do
            Begin
              Self.Canvas.Font.CharSet := FontItems[Item].Charset;
              Self.Canvas.Font.Name := Items[Item]
            End;
          CalcRect := CalcHintRect(Screen.Width,AHint,AData);
          With Rect Do
            Begin
              With TPSCCustomFontBox(Control) Do
                TopLeft := ClientToScreen(Point(Width,ItemRect(Item).Top))
                  ;
              Right := Left + CalcRect.Right;
              Bottom := Top + CalcRect.Bottom;
              If (Right > Screen.Width) And (Right - Left <= Left -
                Control.Width) Then
                OffsetRect(Rect,Left - Control.Width - Right,0)
            End;
          Inherited ActivateHintData(Rect,AHint,AData)
        End
    End
End;

{-------------------------------------}

Procedure TPSCFontBoxPopup.DoFontBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X,Y: Integer);
Var
  Index: Integer;
Begin
  If Button = mbLeft Then
    Begin
      Index := FFontBox.ItemAtPos(Point(X,Y),true);
      ClosePopup(Index = -1,True);
    End;
End;

{-------------------------------------}

Constructor TPSCFontBoxPopup.CreateNew(AOwner: TComponent; Dummy: Integer);
Begin
  Inherited;
  FFontBox := TPSCFontBox.Create(Self);
  With FFontBox Do
    Begin
      Parent := Self;
      Align := alClient;
      SmoothScroll := true;
      BorderStyle := bsNone;
      OnMouseUp:=DoFontBoxMouseUp;
      ParentColor:=True;
    End
End;

{-------------------------------------}

Constructor TPSCFontEdit.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  Text := Font.Name;
  ControlStyle := ControlStyle - [csSetCaption,csDoubleClicks];
  FDropDownWidth := cDefaultFontPopupWidth;
  FDropDownCount := 8;
  ButtonsVisible:=True;
End;

{-------------------------------------}

Function TPSCFontEdit.GetFontBox: TPSCFontBox;
Begin
  With Popup As TPSCFontBoxPopup Do
    Result := FontBox
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetDocFonts(Value: TStrings);
Begin
  FontBox.DocFonts := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetDocFonts: TStrings;
Begin
  Result := FontBox.DocFonts
End;

{-------------------------------------}

Function TPSCFontEdit.CreatePopup: TPSCPopupForm;
Begin
  Result := TPSCFontBoxPopup.CreateNew(nil,0);
  With TPSCFontBoxPopup(Result) Do
    Begin
      ClientWidth := FDropDownWidth;
      ClientHeight := FDropDownCount * FontBox.ItemHeight;
      With FontBox Do
        Begin
          OnMouseUp := DoFontBoxMouseUp;
        End
    End
End;

{-------------------------------------}

Procedure TPSCFontEdit.UpdatePopup;
Begin
  inherited;
  If PopupCreated then
    begin
      TPSCFontBoxPopup(Popup).FontBox.FontName:=Self.FontName;
    end;
End;

{-------------------------------------}

Procedure TPSCFontEdit.PopupCloseEvent(Sender: TObject; Canceled: Boolean);
Begin
  inherited;
  If not Canceled Then
    With TPSCFontBoxPopup(Popup).FontBox Do
      Self.FontName:=FontName;
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetDropDownWidth(Value: Integer);
Begin
  If FDropDownWidth <> Value Then
    Begin
      FDropDownWidth := Value;
      If PopupCreated Then
        Popup.ClientWidth := FDropDownWidth
    End
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetDropDownCount(Value: Integer);
Begin
  If FDropDownCount <> Value Then
    Begin
      FDropDownCount := Value;
      If PopupCreated Then
        Popup.ClientHeight := FDropDownCount * FontBox.ItemHeight
    End
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFontName(const Value: String);
Begin
  Text := Value;
End;

{-------------------------------------}

Function TPSCFontEdit.GetFontName: String;
Begin
  Result := Text;
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBDevice: TPSCFontBoxDevice;
Begin
  Result := FontBox.Device
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBDevice(Value: TPSCFontBoxDevice);
Begin
  FontBox.Device := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBDrawFonts: Boolean;
Begin
  Result := FontBox.DrawFonts
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBDrawFonts(Value: Boolean);
Begin
  FontBox.DrawFonts := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBFontAliases: TStrings;
Begin
  Result := FontBox.FontAliases
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBFontAliases(Value: TStrings);
Begin
  FontBox.FontAliases := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBImageIndex: TPSCImageIndex;
Begin
  Result := FontBox.ImageIndex
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBImageIndex(Value: TPSCImageIndex);
Begin
  FontBox.ImageIndex := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBImages: TPSCImageList;
Begin
  Result := FontBox.Images;
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBImages(Value: TPSCImageList);
Begin
  FontBox.Images := Value;
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBOptions: TPSCFontBoxOptions;
Begin
  Result := FontBox.Options
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBOptions(Value: TPSCFontBoxOptions);
Begin
  FontBox.Options := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBSelectionShowing: TPSCSelectionShowing;
Begin
  Result := FontBox.SelectionShowing
End;

{-------------------------------------}

Procedure TPSCFontEdit.SeFBSelectionShowing(Value: TPSCSelectionShowing);
Begin
  FontBox.SelectionShowing := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBSmoothScroll: Boolean;
Begin
  Result := FontBox.SmoothScroll
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBSmoothScroll(Value: Boolean);
Begin
  FontBox.SmoothScroll := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBShowFontAliases: Boolean;
Begin
  Result := FontBox.ShowFontAliases
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBShowFontAliases(Value: Boolean);
Begin
  FontBox.ShowFontAliases := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBShowFonts: TStrings;
Begin
  Result := FontBox.ShowFonts
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBShowFonts(Value: TStrings);
Begin
  FontBox.ShowFonts := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBShowImages: Boolean;
Begin
  Result := FontBox.ShowImages
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBShowImages(Value: Boolean);
Begin
  FontBox.ShowImages := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBSorted: Boolean;
Begin
  Result := FontBox.Sorted
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBSorted(Value: Boolean);
Begin
  FontBox.Sorted := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetFBSortedDocs: Boolean;
Begin
  Result := FontBox.SortedDocs
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetFBSortedDocs(Value: Boolean);
Begin
  FontBox.SortedDocs := Value
End;

{-------------------------------------}

Function TPSCFontEdit.GetOnShowFont: TPSCShowFontEvent;
Begin
  Result := FontBox.FOnShowFont
End;

{-------------------------------------}

Procedure TPSCFontEdit.SetOnShowFont(Value: TPSCShowFontEvent);
Begin
  FontBox.FOnShowFont := Value;
End;

{-------------------------------------}

initialization
finalization
  FPSCTrueTypeBitmap.Free;
  FPSCTrueTypeBitmap:=nil;
end.
