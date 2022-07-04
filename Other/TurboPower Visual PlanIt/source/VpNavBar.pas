{*********************************************************}
{*                  VPNAVBAR.PAS 1.03                    *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpNavBar;

interface

uses
  Windows, Controls, Graphics, Forms, Buttons, Messages, SysUtils, MMSystem,
  StdCtrls, Classes, ExtCtrls, VpBase, VpConst, VpMisc, VpSR;

type
  {Forward Declaration}
  TVpNavFolder = class;
  TVpCustomNavBar = class;

  TVpIconSize = (isLarge, isSmall);
  TVpBackgroundMethod = (bmNone, bmNormal, bmStretch, bmTile);
  TVpFolderDrawingStyle = (dsDefButton, dsEtchedButton, dsCoolTab,
    dsStandardTab);
  TVpFolderType = (ftDefault, ftContainer);

  TVpFolderContainer = class(TPanel)
  protected{Private}
    FNavBar    : TVpCustomNavBar;
    FIndex         : Integer;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override; 

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Index: Integer Read FIndex;
    property NavBar: TVpCustomNavBar read FNavBar;
  end;

  TVpNavBtnItem = class(TVpCollectionItem)
  protected {private}
    {property variables}
    FFolder      : TVpNavFolder;
    FCaption     : string;
    FDescription : String;
    FIconIndex   : Integer;
    FIconRect    : TRect;
    FLabelRect   : TRect;
    FTag         : Integer;
    {internal variables}
    liDisplayName : string;
    {property methods}
    procedure SetCaption(const Value : string);
    procedure SetIconIndex(Value : Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Folder: TVpNavFolder read FFolder;
    procedure Assign(Source: TPersistent); override;
    property IconRect : TRect read FIconRect;
    property LabelRect : TRect read FLabelRect;
  published
    property Caption : string
      read FCaption write SetCaption;
    property Description : string
      read FDescription write FDescription;
    property IconIndex : Integer
      read FIconIndex write SetIconIndex;
    property Name;
    property Tag: Integer
      read FTag write FTag;
  end;

  TVpNavFolder = class(TVpCollectionItem)
  protected {private}
    {property variables}
    FNavBar     : TVpCustomNavBar;
    FCaption        : string;
    FEnabled        : Boolean;
    FIconSize       : TVpIconSize;
    FFolderType     : TVpFolderType;
    FContainerIndex : Integer;
    FItems          : TVpCollection;
    {internal variables}
    lfDisplayName   : string;
    lfRect          : TRect;
    FTag            : Integer;
    {property methods}
    function GetItem(Index : Integer) : TVpNavBtnItem;
    function GetItemCount : Integer;
    procedure SetCaption(const Value : string);
    procedure SetEnabled(Value : Boolean);
    procedure SetFolderType(Value: TVpFolderType);
    function CreateContainer: Integer;
    procedure SetIconSize(Value : TVpIconSize);
    procedure SetItem(Index : Integer; Value : TVpNavBtnItem);
    procedure lfGetEditorCaption(var Caption : string);
    procedure lfItemChange(Sender : TObject);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadIndex(Reader: TReader);
    procedure WriteIndex(Writer: TWriter);
  public
    constructor Create(Collection : TCollection); override;
    destructor Destroy; override;

    function GetContainer: TVpFolderContainer;

    property Items[Index : Integer] : TVpNavBtnItem
      read GetItem;
    property ItemCount : Integer
      read GetItemCount;
    property ContainerIndex: Integer
      read FContainerIndex write FContainerIndex;
  published
    property Caption : string
      read FCaption write SetCaption;
    property Enabled : Boolean
      read FEnabled write SetEnabled;
    property FolderType: TVpFolderType
      read FFolderType write SetFolderType;
    property ItemCollection : TVpCollection
      read FItems write FItems;
    property IconSize : TVpIconSize
      read FIconSize write SetIconSize;
    property Name;
    property Tag: Integer
      read FTag write FTag;
  end;

  TVpRenameEdit = class(TCustomMemo)
  private
  protected
    procedure KeyPress(var Key: Char); override;
  public
    FolderIndex : Integer;
    ItemIndex   : Integer;
    constructor Create(AOwner : TComponent); override;
  end;

  {NavBar Events}
  TVpFolderClickEvent =
    procedure(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      Index : Integer) of object;
  TVpItemClickEvent =
    procedure(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      Index : Integer) of object;
  TVpFolderChangeEvent =
    procedure(Sender : TObject; Index : Integer; var AllowChange : Boolean;
      Dragging : Boolean) of object;
  TVpFolderChangedEvent =
    procedure(Sender : TObject; Index : Integer) of object;
  TVpNABDragOverEvent =
    procedure(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var AcceptFolder, AcceptItem: Boolean) of object;
  TVpNABDragDropEvent =
    procedure(Sender, Source: TObject; X, Y: Integer;
      FolderIndex, ItemIndex : Integer) of object;
  TVpMouseOverItemEvent =
    procedure(Sender : TObject; Item : TVpNavBtnItem) of object;


  TVpCustomNavBar = class(TVpCustomControl)
  protected {private}
    {property variables}
    FActiveFolder       : Integer;
    FActiveItem         : Integer;
    FAllowRearrange     : Boolean;
    FBackgroundColor    : TColor;
    FBackgroundImage    : TBitmap;
    FBackgroundMethod   : TVpBackgroundMethod;
    FBorderStyle        : TBorderStyle;
    FButtonHeight       : Integer;
    FContainers         : TVpContainerList;
    FDrawingStyle       : TVpFolderDrawingStyle;
    FFolders            : TVpCollection;
    FHotFolder          : Integer;
    FImages             : TImageList;
    FItemFont           : TFont;
    FItemSpacing        : Word;
    FPreviousFolder     : Integer;
    FPreviousItem       : Integer;
    FPlaySounds         : Boolean;
    FSelectedItem       : Integer;
    FSelectedItemFont   : TFont;
    FScrollDelta        : Integer;
    FShowButtons        : Boolean;
    FSoundAlias         : string;
    FLoadingFolder      : Integer;

    {event variables}
    FOnArrange          : TNotifyEvent;
    FOnDragDrop         : TVpNABDragDropEvent;
    FOnDragOver         : TVpNABDragOverEvent;
    FOnFolderChange     : TVpFolderChangeEvent;
    FOnFolderChanged    : TVpFolderChangedEvent;
    FOnFolderClick      : TVpFolderClickEvent;
    FOnItemClick        : TVpItemClickEvent;
    FOnMouseOverItem    : TVpMouseOverItemEvent;

    {internal variables}
    nabChanging         : Boolean;
    nabEdit             : TVpRenameEdit;
    nabTopItem          : Integer;
    nabExternalDrag     : Boolean;
    nabDragFromItem     : Integer;
    nabDragFromFolder   : Integer;
    nabDragToItem       : Integer;
    nabDragToFolder     : Integer;
    nabDropY            : Integer;
    nabHitTest          : TPoint;     {location of mouse cursor}
    nabItemsRect        : TRect;
    nabMouseDown        : Boolean;
    nabOverButton       : Boolean;
    nabScrollDownBtn    : TSpeedButton;
    nabScrollUpBtn      : TSpeedButton;
    nabTimer            : Integer;    {timer-pool handle}
    nabExternalDragItem : Integer;
    nabFolderAccept     : Boolean;
    nabItemAccept       : Boolean;
    nabCursorOverItem   : Boolean;
    nabAcceptAny        : Boolean;
    nabLastMouseOverItem: Integer;

    {property methods}
    function GetFolder(Index : Integer) : TVpNavFolder;
    function GetFolderCount : Integer;
    function GetContainer(Index: Integer):TVpFolderContainer;
    procedure SetActiveFolder(Value : Integer);
    procedure SetBackgroundColor(Value : TColor);
    procedure SetBackgroundImage(Value : TBitmap);
    procedure SetBackgroundMethod(Value : TVpBackgroundMethod);
    procedure SetDrawingStyle(Value : TVpFolderDrawingStyle);
    procedure SetBorderStyle(const Value : TBorderStyle);
    procedure SetButtonHeight(Value : Integer);
    procedure SetImages(Value : TImageList);
    procedure SetItemFont(Value : TFont);
    procedure SetItemSpacing(Value : Word);
    procedure SetSelectedItemFont(Value : TFont);
    procedure SetScrollDelta(Value : Integer);

    {internal methods}
    function nabButtonRect(Index : Integer) : TRect;
    procedure nabCommitEdit(Sender : TObject);
    procedure DragOver(Source: TObject;
                       X, Y: Integer;
                       State: TDragState;
                   var Accept: Boolean); override;
    function nabDropHitTest(X, Y : Integer) : Boolean;
    procedure nabFolderChange(Sender : TObject);
    procedure nabFolderSelected(Sender : TObject; Index : Integer);
    procedure nabFontChanged(Sender : TObject);
    procedure nabGetEditorCaption(var Caption : string);
    function nabGetFolderArea(Index : Integer) : TRect;
    procedure nabGetHitTest(X, Y : Integer;
                        var FolderIndex : Integer;
                        var ItemIndex : Integer);
    procedure nabImagesChanged(Sender : TObject);
    procedure nabRecalcDisplayNames;
    procedure nabScrollDownBtnClick(Sender : TObject);
    procedure nabScrollUpBtnClick(Sender : TObject);
    function nabShowScrollUp : Boolean;
    function nabShowScrollDown : Boolean;
    procedure nabTimerEvent(Sender : TObject;
                            Handle : Integer;
                            Interval : Cardinal;
                            ElapsedTime : LongInt);

    {VCL message methods}
    procedure CMCtl3DChanged(var Msg : TMessage); message CM_CTL3DCHANGED;
    procedure CMDesignHitTest(var Msg : TCMDesignHitTest);
      message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentColorChanged(var Message: TMessage);
      message CM_PARENTCOLORCHANGED;

    {windows message response methods}
    procedure WMEraseBkGnd(var Msg : TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMNCHitTest(var Msg : TWMNCHitTest);  message WM_NCHITTEST;
    procedure WMSetCursor(var Msg : TWMSetCursor); message WM_SETCURSOR;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;

    {Compound component streaming methods}
    procedure Loaded; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
    function AddContainer(Container: TVpFOlderContainer): Integer;
    procedure RemoveContainer(Container: TVpFolderContainer);

    procedure MouseDown(Button : TMouseButton;
                        Shift : TShiftState;
                        X, Y : Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button : TMouseButton;
                      Shift : TShiftState;
                      X, Y : Integer); override;
    procedure Notification(AComponent : TComponent;
                           Operation : TOperation); override;
    procedure Paint; override;
    procedure DoArrange;
    procedure DoFolderChange(Index : Integer; var AllowChange : Boolean);
    procedure DoFolderChanged(Index : Integer);
    procedure DoFolderClick(Button : TMouseButton;
                            Shift : TShiftState;
                            Index : Integer);
    procedure DoItemClick(Button : TMouseButton;
                          Shift : TShiftState;
                          Index : Integer);
    procedure DoMouseOverItem(X, Y, ItemIndex : Integer);

    {properties}
    property ActiveFolder : Integer
      read FActiveFolder write SetActiveFolder;
    property AllowRearrange : Boolean
      read FAllowRearrange write FAllowRearrange;
    property BackgroundColor : TColor
      read FBackgroundColor write SetBackgroundColor;
    property BackgroundImage : TBitmap
      read FBackgroundImage write SetBackgroundImage;
    property BackgroundMethod : TVpBackgroundMethod
      read FBackgroundMethod write SetBackgroundMethod;
    property BorderStyle : TBorderStyle
      read FBorderStyle write SetBorderStyle;
    property ButtonHeight : Integer
      read FButtonHeight write SetButtonHeight;
    property DrawingStyle : TVpFolderDrawingStyle
      read FDrawingStyle write SetDrawingStyle;
    property FolderCollection : TVpCollection
      read FFolders write FFolders;
    property Images : TImageList
      read FImages write SetImages;
    property ItemFont : TFont
      read FItemFont write SetItemFont;
    property ItemSpacing : Word
      read FItemSpacing write SetItemSpacing;
    property PlaySounds : Boolean
      read FPlaySounds write FPlaySounds;
    property ScrollDelta : Integer
      read FScrollDelta write SetScrollDelta default 2;
    property SelectedItem : Integer
      read FSelectedItem write FSelectedItem;
    property SelectedItemFont : TFont
      read FSelectedItemFont write SetSelectedItemFont;
    property ShowButtons : Boolean
      read FShowButtons write FShowButtons;
    property SoundAlias : string
      read FSoundAlias write FSoundAlias;
{    property Storage : TOvcAbstractStore
      read FStorage write SetStorage;}

    {inherited Events}
    property AfterEnter;
    property AfterExit;
    property OnMouseWheel;

    {events}
    property OnArrange : TNotifyEvent
      read FOnArrange write FOnArrange;
    property OnDragDrop : TVpNABDragDropEvent
      read FOnDragDrop write FOnDragDrop;
    property OnDragOver : TVpNABDragOverEvent
      read FOnDragOver write FOnDragOver;
    property OnFolderClick : TVpFolderClickEvent
      read FOnFolderClick write FOnFolderClick;
    property OnItemClick : TVpItemClickEvent
      read FOnItemClick write FOnItemClick;
    property OnFolderChange : TVpFolderChangeEvent
      read FOnFolderChange write FOnFolderChange;
    property OnFolderChanged : TVpFolderChangedEvent
      read FOnFolderChanged write FOnFolderChanged;
    property OnMouseOverItem : TVpMouseOverItemEvent
      read FOnMouseOverItem write FOnMouseOverItem;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer); override;
    { Declared public because TControl's DragDrop is public. }
    procedure DragDrop(Source: TObject; X, Y: Integer); override;

    procedure BeginUpdate;
    procedure ItemChanged(FolderIndex, ItemIndex: Integer);
    procedure FolderChanged(FolderIndex: Integer);
    procedure EndUpdate;
    function GetFolderAt(X, Y : Integer) : Integer;
    function GetItemAt(X, Y : Integer) : Integer;
    function Container: TVpFolderContainer;
    procedure InsertFolder(const ACaption : string; AFolderIndex : Integer);
    procedure AddFolder(const ACaption : string);
    procedure RemoveFolder(AFolderIndex : Integer);
    procedure RenameFolder(AFolderIndex : Integer);
    procedure InsertItem(const ACaption : string; AFolderIndex, AItemIndex,
      AIconIndex : Integer);
    procedure AddItem(const ACaption : string; AFolderIndex,
      AIconIndex : Integer);
    procedure RemoveItem(AFolderIndex, AItemIndex : Integer);
    procedure InvalidateItem(FolderIndex, ItemIndex : Integer);
    procedure RenameItem(AFolderIndex, AItemIndex : Integer);
    property ActiveItem : Integer
      read FActiveItem;
    property Containers[Index: Integer]: TVpFolderContainer
      read GetContainer;
    property Folders[Index : Integer] : TVpNavFolder
      read GetFolder;
    property FolderCount : Integer
      read GetFolderCount;
    property PreviousFolder  : Integer
      read FPreviousFolder;
    property PreviousItem  : Integer
      read FPreviousItem;
  end;


  TVpNavBar = class(TVpCustomNavBar)
  published
    property ActiveFolder;
    property AllowRearrange;
    property BackgroundColor;
    property BackgroundImage;
    property BackgroundMethod;
    property BorderStyle;
    property ButtonHeight;
    property DrawingStyle;
    property FolderCollection;
    property Images;
    property ItemFont;
    property ItemSpacing;
    property PlaySounds;
    property ScrollDelta;
    property SelectedItem;
    property SelectedItemFont;
    property ShowButtons;
    property SoundAlias;
//    property Storage;

    {inherited Events}
    property AfterEnter;
    property AfterExit;
    property OnMouseWheel;

    {events}
    property OnArrange;
    property OnDragDrop;
    property OnDragOver;
    property OnFolderClick;
    property OnItemClick;
    property OnFolderChange;
    property OnFolderChanged;
    property OnMouseOverItem;

    {inherited properties}
    {$IFDEF VERSION4}
    property Anchors;
    property Constraints;
    property DragKind;
    {$ENDIF}
    property Align;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
(*
    The following properties are not published to avoid conflicts with
    OnFolderClick and OnItemClick.
    property OnClick;
    property OnDblClick;
*)
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;


implementation

const
  nabTimerInterval = 200;

{DrawNavTab - returns the usable text area inside the tab rect.}
function DrawNavTab(Canvas: TCanvas;
                  const Client: TRect;
                        BevelWidth: Integer;
                        TabColor: TColor;
                        TabNumber: Integer;
                        CoolTab,
                        IsFocused,
                        IsMouseOver: Boolean): TRect;
var
  R: TRect;
  {$IFNDEF VERSION4}
    Points: array[1..5] of TPoint;
  {$ENDIF}
begin
  R := Client;

  with Canvas do begin
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    Pen.Color := TabColor;

    {fill the tab area}
    Polygon([Point(R.Left, R.Bottom),
             Point(R.Left, R.Top),
             Point(R.Right, R.Top),
             Point(R.Right, R.Bottom)]);

    if CoolTab then
    {Draw Cool Tabs}
    begin

      Pen.Color := clBlack;

      {Draw the bottom, left line}
      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Left + 5, R.Bottom - 1);

      {Draw the bottom, left curve}
      {$IFNDEF VERSION4}
      Points[1] := Point(R.Left + 5,  R.Bottom - 1);
      Points[2] := Point(R.Left + 11, R.Bottom - 2);
      Points[3] := Point(R.Left + 12, R.Bottom - 7);
      Points[4] := Point(R.Left + 13, R.Bottom - 9);
        {$IFDEF CBuilder}
          Canvas.PolyBezier(Points);
        {$ELSE}
          Canvas.Polyline(Points);
        {$ENDIF}
      {$ELSE}
      PolyBezier([Point(R.Left + 5,    R.Bottom - 1),   {StartPoint}
                  Point(R.Left + 11,   R.Bottom - 2),   {ControlPoint}
                  Point(R.Left + 12,   R.Bottom - 7),   {ControlPoint}
                  Point(R.Left + 13,   R.Bottom - 9)]); {EndPoint}
      {$ENDIF}

      {Draw the left side of the tab}
      MoveTo(R.Left + 13, R.Bottom - 9);
      LineTo(R.Left + 13, R.Top + 9);

      {Draw the top, left corner of the tab}
      {$IFNDEF VERSION4}
      Points[1] := Point(R.Left + 13,   R.Top + 9);
      Points[2] := Point(R.Left + 14,   R.Top + 7);
      Points[3] := Point(R.Left + 15,   R.Top + 2);
      Points[4] := Point(R.Left + 21,   R.Top + 1);
        {$IFDEF CBuilder}
          Canvas.PolyBezier(Points);
        {$ELSE}
          Canvas.Polyline(Points);
        {$ENDIF}
      {$ELSE}
      PolyBezier([Point(R.Left + 13,   R.Top + 9),     {StartPoint}
                  Point(R.Left + 14,   R.Top + 7),     {ControlPoint}
                  Point(R.Left + 15,   R.Top + 2),     {ControlPoint}
                  Point(R.Left + 21,   R.Top + 1)]);   {EndPoint}
      {$ENDIF}

      {Draw the top of the tab}
      MoveTo(R.Left + 21,   R.Top + 1);
      LineTo(R.Right - 16,  R.Top + 1);

      {Draw the Top, Right corner of the tab}
      {$IFNDEF VERSION4}
      Points[1] := Point(R.Right - 16,   R.Top + 1);
      Points[2] := Point(R.Right - 10,   R.Top + 2);
      Points[3] := Point(R.Right -  9,   R.Top + 7);
      Points[4] := Point(R.Right -  8,   R.Top + 9);
        {$IFDEF CBuilder}
          Canvas.PolyBezier(Points);
        {$ELSE}
          Canvas.Polyline(Points);
        {$ENDIF}
      {$ELSE}
      PolyBezier([Point(R.Right - 16,   R.Top + 1),     {StartPoint}
                  Point(R.Right - 10,   R.Top + 2),     {ControlPoint}
                  Point(R.Right -  9,   R.Top + 7),     {ControlPoint}
                  Point(R.Right -  8,   R.Top + 9)]);   {EndPoint}
      {$ENDIF}

      {Draw the right side of the tab}
      MoveTo(R.Right - 8, R.Top + 9);
      LineTo(R.Right - 8, R.Bottom - 9);

      {Draw the bottom, Right curve of the tab which should finish against the
       right side.}
      {$IFNDEF VERSION4}
      Points[1] := Point(R.Right - 8,  R.Bottom - 9);
      Points[2] := Point(R.Right - 7,  R.Bottom - 7);
      Points[3] := Point(R.Right - 6,  R.Bottom - 2);
      Points[4] := Point(R.Right,      R.Bottom - 1);
        {$IFDEF CBuilder}
          Canvas.PolyBezier(Points);
        {$ELSE}
          Canvas.Polyline(Points);
        {$ENDIF}
      {$ELSE}
      PolyBezier([Point(R.Right - 8,   R.Bottom - 9),   {StartPoint}
                  Point(R.Right - 7,   R.Bottom - 7),   {ControlPoint}
                  Point(R.Right - 6,   R.Bottom - 2),   {ControlPoint}
                  Point(R.Right,       R.Bottom - 1)]); {EndPoint}
      {$ENDIF}

    end else begin
    {Draw Standard Tabs}

      if TabNumber > 0 then begin
        Brush.Color := TabColor;
        Brush.Style := bsSolid;
        Pen.Color := TabColor;

        {fill the tab area}
        Polygon([Point(R.Left, R.Bottom),
                 Point(R.Left, R.Top),
                 Point(R.Right, R.Top),
                 Point(R.Right, R.Bottom)]);
      end;

      Brush.Color := TabColor;
      Brush.Style := bsSolid;

      {Draw Tab}
      Pen.Color := TabColor;
      Polygon([Point(R.Left + 10,       R.Bottom - 1),
               Point(R.Left + 10,       R.Top + 3),
               Point(R.Left + 12,       R.Top + 1),
               Point(R.Right-4,    R.Top+1),
               Point(R.Right-2,    R.Top+3),
               Point(R.Right-2,    R.Bottom-1)]);

      {highlight tab}
      Pen.Color := clBtnHighlight;
      PolyLine([Point(R.Left,          R.Bottom - 2),
                Point(R.Left + 8,      R.Bottom - 2),
                Point(R.Left + 9,      R.Bottom - 3),
                Point(R.Left + 9,      R.Top + 3),
                Point(R.Left + 11,     R.Top + 1),
                Point(R.Right - 1,     R.Top + 1)]);

      {draw border}
      Pen.Color := clBlack;
      PolyLine([Point(R.Left,       R.Bottom - 1),
                Point(R.Left + 9,   R.Bottom - 1),
                Point(R.Left + 10,  R.Bottom - 2),
                Point(R.Left + 10,  R.Top + 4),
                Point(R.Left + 11,  R.Top + 3),
                Point(R.Left + 12,  R.Top + 2),
                Point(R.Right-2,    R.Top + 2),
                Point(R.Right-1,    R.Top + 3),
                Point(R.Right-1,    R.Bottom-1)]);

      {draw shadow}
    end;
  end;

  Result := Rect(Client.Left + 1, Client.Top + 1,
    Client.Right - 2, Client.Bottom - 2);
  if IsFocused then OffsetRect(Result, 1, 1);
end;

{===== TVpFolderContainer ===========================================}
constructor TVpFolderContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNavBar := TVpCustomNavBar(AOwner);
  Width := 0;
  Height := 0;
  Visible := false;
  {Add self to container list}
  FIndex := FNavBar.AddContainer(Self);
end;
{=====}

destructor TVpFolderContainer.Destroy;
begin
  {FComponentList.Free;}
  inherited;
end;
{=====}

function TVpFolderContainer.GetChildOwner: TComponent;
begin
  Result := Owner.Owner;
end;
{=====}

procedure TVpFolderContainer.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  C: TControl;
begin
  inherited GetChildren(Proc, Self);
  for I := 0 to ControlCount - 1 do begin
    C := Controls[I];
    C.Parent := Self;
    Proc(C);
  end;
end;

{===== TRenameEdit ===================================================}

constructor TVpRenameEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  Ctl3D := False;
  Visible := False;
  WantReturns := False;
  FolderIndex := -1;
  ItemIndex   := -1;
end;
{=====}

procedure TVpRenameEdit.KeyPress(var Key: Char);
begin
  if Key = #13 then begin
    Key := #0;
    DoExit;
  end else if Key = #27 then begin
    FolderIndex := -1;
    ItemIndex   := -1;
    Key := #0;
    DoExit;
  end;
end;

{===== Miscellaneous routines ========================================}

function RectWidth(Rect : TRect) : Integer;
begin
  Result := Rect.Right - Rect.Left;
end;
{=====}

function RectHeight(Rect : TRect) : Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;


{===== TVpNavBtnItem ===============================================}

constructor TVpNavBtnItem.Create(Collection : TCollection);
begin
  inherited Create(Collection);
  FFolder := TVpNavFolder((TVpCollection(Collection)).GetOwner);
  FIconIndex := -1;
  Name := 'Item' + IntToStr(FFolder.Index) + '-' + IntToStr(Index);
  FFolder.FNavBar.Invalidate;
end;
{=====}

destructor TVpNavBtnItem.Destroy;
var
  NaBar: TVpCustomNavBar;
  FolderIndex: Integer;
begin
  NaBar := FFolder.FNavBar;
  FolderIndex := FFolder.Index;
  inherited Destroy;
  NaBar.FolderChanged(FolderIndex);
end;
{=====}

procedure TVpNavBtnItem.Assign(Source: TPersistent);
begin
  if Source is TVpNavBtnItem then begin
    Caption := TVpNavBtnItem(Source).Caption;
    Description := TVpNavBtnItem(Source).Description;
    IconIndex := TVpNavBtnItem(Source).IconIndex;
    Tag := TVpNavBtnItem(Source).Tag;
  end else
    inherited Assign(Source);
end;
{=====}

procedure TVpNavBtnItem.SetCaption(const Value : string);
begin
  if Value <> FCaption then begin
    FCaption := Value;
    Changed(false);
    FFolder.FNavBar.ItemChanged(FFolder.Index, Index);
  end;
end;
{=====}

procedure TVpNavBtnItem.SetIconIndex(Value : Integer);
begin
  if Value <> FIconIndex then begin
    FIconIndex := Value;
    Changed(false);
    FFolder.FNavBar.ItemChanged(FFolder.Index, Index);
  end;
end;

{===== TVpNavBtnFolder =============================================}

constructor TVpNavFolder.Create(Collection : TCollection);
begin
  inherited Create(Collection);
  RegisterClass(TVpFolderContainer);
  FNavBar := TVpCustomNavBar(TVpCollection(Collection).GetOwner);
  FNavBar.ActiveFolder := Index;
  FItems := TVpCollection.Create(Self, TVpNavBtnItem);
  Name := 'NavFolder' + IntToStr(Index);
  FEnabled := True;
  FIconSize := isLarge;
end;
{=====}

destructor TVpNavFolder.Destroy;
begin
  {Change the Active Folder to one that will still exist}
  if not(csDestroying in FNavBar.ComponentState) then begin
    if Index > 0 then
      FNavBar.ActiveFolder := Index - 1
    else if Collection.Count > 1 then
      FNavBar.ActiveFolder := 0
    else
      FNavBar.ActiveFolder := -1;

    FNavBar.FolderChanged(Index);
  end;

  FItems.Free;
  FItems := nil;
  inherited Destroy;
end;
{=====}

function TVpNavFolder.GetItem(Index : Integer) : TVpNavBtnItem;
begin
  Result := TVpNavBtnItem(FItems[Index]);
end;
{=====}

function TVpNavFolder.GetItemCount : Integer;
begin
  Result := FItems.Count;
end;
{=====}

function TVpNavFolder.GetContainer: TVpFolderContainer;
begin
  if FolderType = ftContainer then
    result := FNavBar.FContainers[FContainerIndex]
  else
    result := nil;
end;
{=====}

procedure TVpNavFolder.lfGetEditorCaption(var Caption : string);
begin
  Caption := RSEditingItems;
end;
{=====}

procedure TVpNavFolder.lfItemChange(Sender : TObject);
begin
  if (TVpCollection(Collection).GetOwner is TComponent) then
    if not (csDestroying in
      TComponent(TVpCollection(Collection).GetOwner).ComponentState)
    then begin
      TVpNavBar(TVpCollection(Collection).GetOwner).nabRecalcDisplayNames;
      TVpNavBar(TVpCollection(Collection).GetOwner).Invalidate;
    end;
end;
{=====}

procedure TVpNavFolder.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('ContainerIndex', ReadIndex, WriteIndex,
    FFolderType = ftContainer);
end;
{=====}

procedure TVpNavFolder.ReadIndex(Reader: TReader);
begin
  ContainerIndex := trunc(Reader.ReadFloat);
end;
{=====}

procedure TVpNavFolder.WriteIndex(Writer: TWriter);
begin
  Writer.WriteFloat(ContainerIndex);
end;
{=====}

procedure TVpNavFolder.SetCaption(const Value : string);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    Changed(false);
    FNavBar.FolderChanged(Index);
  end;
end;
{=====}

procedure TVpNavFolder.SetEnabled(Value : Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    Changed(false);
    FNavBar.FolderChanged(Index);
  end;
end;
{=====}

procedure TVpNavFolder.SetFolderType(Value: TVpFolderType);
begin
  if Value <> FFolderType then begin
    FFolderType := Value;

    if not (csLoading in FNavBar.ComponentState) then begin
      if FFolderType = ftContainer then
        ContainerIndex := CreateContainer
      else begin
        FNavBar.FContainers.Delete(FContainerIndex);
        FContainerIndex := -1;
      end;
    FNavBar.FolderChanged(Index);
    end;
  end;
end;
{=====}

function TVpNavFolder.CreateContainer: Integer;
var
  New: TVpFolderContainer;
begin
  New := TVpFolderContainer.Create(FNavBar);
  New.Parent := FNavBar;
  result := New.Index;
  New.Name := 'Container' + IntToStr(Result);
  New.Caption := '';
  New.BevelOuter := bvNone;
  New.BevelInner := bvNone;
  New.Color := FNavBar.FBackgroundColor;
end;
{=====}

procedure TVpNavFolder.SetIconSize(Value : TVpIconSize);
begin
  if FIconSize <> Value then begin
    FIconSize := Value;
    Changed(false);
    FNavBar.FolderChanged(Index);
  end;
end;
{=====}

procedure TVpNavFolder.SetItem(Index : Integer; Value : TVpNavBtnItem);
begin
  SetItem(Index, Value);
end;

{===== TVpNavBar ================================================}
constructor TVpCustomNavBar.Create(AOwner : TComponent);
var
  HSnd : THandle;
begin
  inherited Create(AOwner);

  FContainers := TVpContainerList.Create(Self);

  FLoadingFolder := -1;
  FShowButtons := True;

  if Classes.GetClass(TVpNavFolder.ClassName) = nil then
    Classes.RegisterClass(TVpNavFolder);
  if Classes.GetClass(TVpNavBtnItem.ClassName) = nil then
    Classes.RegisterClass(TVpNavBtnItem);

  FFolders := TVpCollection.Create(Self, TVpNavFolder);
  FFolders.OnChanged := nabFolderChange;
  FFolders.OnGetEditorCaption := nabGetEditorCaption;
  FFolders.OnItemSelected := nabFolderSelected;

  FItemFont := TFont.Create;
  FItemFont.Name := Font.Name;
  FItemFont.OnChange := nabFontChanged;
  FItemFont.Color := clWhite;
  FItemSpacing := abs(FItemFont.Height) + 3;

  FSelectedItemFont := TFont.Create;
  FSelectedItemFont.Name := Font.Name;
  FSelectedItemFont.OnChange := nabFontChanged;
  FSelectedItemFont.Color := FItemFont.Color;
  FSelectedItemFont.Style := FItemFont.Style;
  FSelectedItemFont.Size  := FItemFont.Size;

  {force drivers to load by playing empty wave data}
  HSnd := FindResource(HInstance, 'VPEMPTYWAVE', RT_RCDATA);
  if HSnd > 0 then begin
    HSnd := LoadResource(HInstance, HSnd);
    if HSnd > 0 then begin
      sndPlaySound(LockResource(HSnd), SND_ASYNC or SND_MEMORY);
      FreeResource(HSnd);
    end;
  end;

  nabScrollUpBtn := TSpeedButton.Create(Self);
  with nabScrollUpBtn do begin
    Visible := False;
    Parent := Self;
    OnClick := nabScrollUpBtnClick;
    Glyph.Handle := LoadBaseBitmap('VPUPARROW');
    NumGlyphs := 1;
    Left := -20;
    Height := 15;
    Width := 17;
  end;

  nabScrollDownBtn := TSpeedButton.Create(Self);
  with nabScrollDownBtn do begin
    Visible := False;
    Parent := Self;
    OnClick := nabScrollDownBtnClick;
    Glyph.Handle := LoadBaseBitmap('VPDOWNARROW');
    NumGlyphs := 1;
    Left := -20;
    Height := 15;
    Width := 17;
  end;

  {create edit control}
  if not (csDesigning in ComponentState) then begin
    nabEdit := TVpRenameEdit.Create(Self);
    nabEdit.Parent := Self;
    nabEdit.OnExit := nabCommitEdit;
  end;

  Height := 240;
  Width := 120;
  ParentColor := False;

  FAllowRearrange  := True;
  FBackgroundColor := clInactiveCaption;
  FBackgroundImage := TBitmap.Create;
  FBackgroundMethod := bmNormal;
  FBorderStyle := bsSingle;
  FButtonHeight := 20;
  FActiveFolder := -1;
  FActiveItem := -1;
  FSelectedItem := -1;
  FHotFolder := -1;
  FPreviousFolder := -1;
  FPreviousItem := -1;
  FPlaySounds := False;
  FScrollDelta := 2;
  FSoundAlias := 'MenuCommand';

  nabMouseDown := False;
  nabChanging := False;
  nabTopItem := 0;
  nabDragFromItem := -1;
  nabDragFromFolder := -1;
  nabDropY := -1;
  nabTimer := -1;
  nabLastMouseOverItem := -1;
end;
{=====}

destructor TVpCustomNavBar.Destroy;
begin
  Images := nil; {unregister any image list notification}
  nabChanging := True;

  nabEdit.Free;

  FContainers.Free;

  FFolders.Free;
  FFolders := nil;

  FItemFont.Free;
  FItemFont := nil;

  FSelectedItemFont.Free;
  FSelectedItemFont := nil;

  FBackgroundImage.Free;
  FBackgroundImage := nil;

  inherited Destroy;
end;
{=====}

procedure TVpCustomNavBar.BeginUpdate;
begin
  nabChanging := True;
end;
{=====}

procedure TVpCustomNavBar.ItemChanged(FolderIndex, ItemIndex: Integer);
begin
  InvalidateItem(FolderIndex, ItemIndex);
  if not (csDestroying in ComponentState) then
    RecreateWnd;
end;
{=====}

procedure TVpCustomNavBar.FolderChanged(FolderIndex: Integer);
begin
  Invalidate;
  if not (csDestroying in ComponentState) then
    RecreateWnd;
end;
{=====}

procedure TVpCustomNavBar.CMCtl3DChanged(var Msg : TMessage);
begin
  if (csLoading in ComponentState) or not HandleAllocated then
    Exit;

  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;

  inherited;
end;
{=====}

procedure TVpCustomNavBar.CMDesignHitTest(var Msg : TCMDesignHitTest);
begin
  Msg.Result := LongInt(nabOverButton);
end;
{=====}

procedure TVpCustomNavBar.CMFontChanged(var Message: TMessage);
begin
  nabRecalcDisplayNames;
end;
{=====}

procedure TVpCustomNavBar.CMParentColorChanged(var Message: TMessage);
begin
  inherited;

  if ParentColor then
    SetBackgroundColor(Color);
end;
{=====}

procedure TVpCustomNavBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
    Style := LongInt(Style) or BorderStyles[FBorderStyle];

  if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
end;
{=====}

procedure TVpCustomNavBar.CreateWnd;
begin
  if (csDestroying in ComponentState) then exit;
  inherited CreateWnd;

  nabRecalcDisplayNames;
end;
{=====}

procedure TVpCustomNavBar.Loaded;
begin
  inherited Loaded;
  if DrawingStyle = dsEtchedButton then
    BorderStyle := bsNone;
  if FolderCollection.Count > 0 then
    FActiveFolder := 0
  else
    FActiveFolder := -1;
end;
{=====}

procedure TVpCustomNavBar.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FContainers.Count - 1 do
    Proc(TComponent(FContainers[I]));
end;
{=====}

function TVpCustomNavBar.AddContainer(
  Container: TVpFolderContainer): Integer;
begin
  result := FContainers.Add(Container);
end;
{=====}

procedure TVpCustomNavBar.RemoveContainer(Container: TVpFolderContainer);
begin
  FContainers.Remove(Container);
  Container.Free;
end;
{=====}

procedure TVpCustomNavBar.DoArrange;
begin
  if Assigned(FOnArrange) then
    FOnArrange(Self);
end;
{=====}

procedure TVpCustomNavBar.DoFolderChange(Index : Integer;
  var AllowChange: Boolean);
begin
  if Assigned(FOnFolderChange) then
    FOnFolderChange(Self, Index, AllowChange, nabDragFromItem <> -1);
end;
{=====}

procedure TVpCustomNavBar.DoFolderChanged(Index : Integer);
begin
  if Assigned(FOnFolderChanged) then
    FOnFolderChanged(Self, Index);
end;
{=====}

procedure TVpCustomNavBar.DoFolderClick(Button : TMouseButton;
                                       Shift : TShiftState;
                                       Index : Integer);
begin
  if Assigned(FOnFolderClick) then
    FOnFolderClick(Self, Button, Shift, Index);
end;
{=====}

procedure TVpCustomNavBar.DoItemClick(Button : TMouseButton;
                                     Shift : TShiftState;
                                     Index : Integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Button, Shift, Index);
end;
{=====}

procedure TVpCustomNavBar.DoMouseOverItem(X, Y, ItemIndex : Integer);
begin
  if Assigned(FOnMouseOverItem) then
    FOnMouseOverItem(Self,
      Folders[ActiveFolder].Items[GetItemAt(X, Y)]);
end;
{=====}

procedure TVpCustomNavBar.EndUpdate;
begin
  nabChanging := False;
  nabRecalcDisplayNames;
end;
{=====}

function TVpCustomNavBar.GetFolderCount : Integer;
begin
  Result := FFolders.Count;
end;
{=====}

function TVpCustomNavBar.GetFolder(Index : Integer)  : TVpNavFolder;
begin
  Result := TVpNavFolder(FFolders.GetItem(Index));
end;
{=====}

function TVpCustomNavBar.GetFolderAt(X, Y : Integer) : Integer;
var
  Dummy : Integer;
begin
  nabGetHitTest(X, Y, Result, Dummy);
end;
{=====}

function TVpCustomNavBar.GetContainer(Index: Integer):TVpFolderContainer;
begin
  try
    result := FContainers[Index];
  except
    result := nil;
  end;
end;
{=====}

function TVpCustomNavBar.GetItemAt(X, Y : Integer) : Integer;
var
  Dummy : Integer;
begin
  nabGetHitTest(X, Y, Dummy, Result);
end;
{=====}

function TVpCustomNavBar.Container: TVpFolderContainer;
begin
  if Folders[FActiveFolder].FolderType = ftContainer then
    result := FContainers[Folders[FActiveFolder].ContainerIndex]
  else
    result := nil;
end;
{=====}

procedure TVpCustomNavBar.InsertFolder(const ACaption : string;
                                                  AFolderIndex : Integer);
{$IFNDEF VERSION4}
var
  I : Integer;
{$ENDIF}
begin
{$IFNDEF VERSION4}
  FFolders.Add;
  for I := AFolderIndex to FFolders.Count - 2 do begin
    Folders[I].Index := I + 1;
  end;
  Folders[FFolders.Count - 1].Index := AFolderIndex;
{$ELSE}
  FFolders.Insert(AFolderIndex);
{$ENDIF}  
  Folders[AFolderIndex].Caption := ACaption;
  if FolderCount = 1 then begin
    FActiveFolder := 0;
    FActiveItem := -1;
    FSelectedItem := -1;
  end;
  nabRecalcDisplayNames;
end;
{=====}

procedure TVpCustomNavBar.AddFolder(const ACaption : string);
var
  NewFolder: TVpNavFolder;
begin
  NewFolder := TVpNavFolder(FFolders.Add);
  NewFolder.Caption := ACaption;
  if FolderCount = 1 then begin
    FActiveFolder := 0;
    FActiveItem := -1;
    FSelectedItem := -1;
  end;
  nabRecalcDisplayNames;
end;
{=====}

procedure TVpCustomNavBar.RemoveFolder(AFolderIndex : Integer);
var
  Folder: TVpNavFolder;
begin
  Folder := TVpNavFolder(FolderCollection.Items[AFolderIndex]);
  Folder.Free;
  {$IFDEF VERSION5}
  FolderCollection.Delete(AFolderIndex);
  {$ENDIF}
end;
{=====}

procedure TVpCustomNavBar.RenameFolder(AFolderIndex: Integer);
var
  Folder : TVpNavFolder;
begin
  Folder := Folders[AFolderIndex];
  nabEdit.FolderIndex := AFolderIndex;
  nabEdit.ItemIndex := -1;
  nabEdit.Font.Size := Font.Size;
  nabEdit.BorderStyle := bsNone;
  nabEdit.Top := Folder.lfRect.Top+2;
  nabEdit.Left := Folder.lfRect.Left+2;
  nabEdit.Height := HeightOf(Folder.lfRect)-5;
  nabEdit.Width := Folder.lfRect.Right - Folder.lfRect.Left-5;
  nabEdit.Visible := True;
  nabEdit.Text := Folder.Caption;
  nabEdit.SelectAll;
  nabEdit.SetFocus;
end;
{=====}

procedure TVpCustomNavBar.InsertItem(const ACaption : string;
                                    AFolderIndex, AItemIndex,
                                    AIconIndex : Integer);
var
  AFolder : TVpNavFolder;
{$IFNDEF VERSION4}
  I : Integer;
{$ENDIF}
begin
  AFolder := Folders[AFolderIndex];
{$IFNDEF VERSION4}
  AFolder.FItems.Add;
  for I := AFolderIndex to AFolder.FItems.Count - 2 do
    AFolder.Items[I].Index := I + 1;
  AFolder.Items[AFolder.FItems.Count-1].Index := AFolderIndex;
{$ELSE}
  AFolder.FItems.Insert(AItemIndex);
{$ENDIF}
  AFolder.Items[AItemIndex].Caption := ACaption;
  AFolder.Items[AItemIndex].IconIndex := AIconIndex;
  Invalidate;
end;
{=====}

procedure TVpCustomNavBar.AddItem(const ACaption : string;
                                             AFolderIndex,
                                             AIconIndex : Integer);
var
  AFolder : TVpNavFolder;
  AItem: TVpNavBtnItem;
begin
  AFolder := Folders[AFolderIndex];
  AItem := TVpNavBtnItem(AFolder.FItems.Add);
  AItem.Caption := ACaption;
  AItem.IconIndex := AIconIndex;
  Invalidate;
end;
{=====}

procedure TVpCustomNavBar.RemoveItem(AFolderIndex, AItemIndex : Integer);
var
  Folder : TVpNavFolder;
begin
  Folder := TVpNavFolder(FolderCollection.GetItem(AFolderIndex));
  Folder.Items[AItemIndex].Free;
  {$IFDEF VERSION5}
  FolderCollection.Delete(AItemIndex);
  {$ENDIF}
end;
{=====}

procedure TVpCustomNavBar.InvalidateItem(FolderIndex, ItemIndex : Integer);
var
  F : TRect;
  R : TRect;
begin
  R := TVpNavBtnItem(Folders[FolderIndex].Items[ItemIndex]).FIconRect;
  {expand rect}
  Dec(R.Top);
  Dec(R.Left);
  Inc(R.Bottom, 2);
  Inc(R.Right, 2);
  { Might be a hidden folder. }
  if (not ((FolderCount = 1) and (Folders[0].Caption = '')))
     or (csDesigning in ComponentState) then
    F := nabGetFolderArea(FolderIndex)
  else
    F := R;
  R.Top := MaxI(R.Top, F.Top);
  R.Bottom := MinI(R.Bottom, F.Bottom);
  if RectHeight(R) > 0 then
    InvalidateRect(Handle, @R, False);
end;
{=====}

procedure TVpCustomNavBar.RenameItem(AFolderIndex, AItemIndex : Integer);
var
  Item   : TVpNavBtnItem;
begin
  Item := Folders[AFolderIndex].Items[AItemIndex];
  nabEdit.FolderIndex := AFolderIndex;
  nabEdit.ItemIndex := AItemIndex;
  nabEdit.Font.Size := ItemFont.Size;
  nabEdit.Font.Size := ItemFont.Size;
  nabEdit.BorderStyle := bsSingle;
  nabEdit.Top := Item.LabelRect.Top-1;
  nabEdit.Left := 10;
  nabEdit.Height := HeightOf(Item.LabelRect) + 2;
  nabEdit.Width := Width - 24;
  nabEdit.Visible := True;
  nabEdit.Text := Item.Caption;
  nabEdit.SelectAll;
  nabEdit.SetFocus;
end;
{=====}

function GetLargeIconDisplayName(Canvas : TCanvas;
                                  Rect : TRect;
                                  const Name : string) : string;
  {-given a string, and a rectangle, find the string that can be displayed
    using two lines. Add ellipsis to the end of each line if necessary and
    possible}
var
  TestRect : TRect;
  SH, DH : Integer;
  Buf : array[0..255] of Char;
  I : Integer;
  TempName : string;
  Temp2 : string;
begin
  TempName := Trim(Name);
  {get single line height}
  with TestRect do begin
    Left := 0;
    Top := 0;
    Right := 1;
    Bottom := 1;
  end;
  SH := DrawText(Canvas.Handle, 'W W', 3, TestRect,
    DT_SINGLELINE or DT_CALCRECT);

  {get double line height}
  with TestRect do begin
    Left := 0;
    Top := 0;
    Right := 1;
    Bottom := 1;
  end;
  DH := DrawText(Canvas.Handle, 'W W', 3, TestRect,
    DT_WORDBREAK or DT_CALCRECT);

  {see if the text can fit within the existing rect without growing}
  TestRect := Rect;
  StrPLCopy(Buf, TempName, 255);
  DrawText(Canvas.Handle, Buf, Length(TempName), TestRect,
            DT_WORDBREAK or DT_CALCRECT);
  I := Pos(' ', TempName);
  if (RectHeight(TestRect) = SH) or (I < 2) then
    Result := GetDisplayString(Canvas, TempName, 1, RectWidth(Rect))
  else begin
    {the first line only has ellipsis if there's only one word on it and
    that word won't fit}
    Temp2 := GetDisplayString(Canvas, Copy(TempName, 1, I-1), 1,
                              RectWidth(Rect));
    if CompareStr(Temp2, Copy(TempName, 1, I-1)) <> 0 then begin
      Result := GetDisplayString(Canvas, Copy(TempName, 1, I-1), 1,
                                 RectWidth(Rect)) +
                ' ' +
                GetDisplayString(Canvas, Copy(TempName, I+1,
                                 Length(TempName) - I), 1, RectWidth(Rect));
    end else begin
      {2 or more lines, and the first line isn't getting an ellipsis}
      if (RectHeight(TestRect) = DH) and
         (RectWidth(TestRect) <= RectWidth(Rect)) then
        {it will fit}
        Result := TempName
      else begin
        {it won't fit, but the first line wraps OK - 2nd line needs an ellipsis}
        TestRect.Right := Rect.Right + 1;
        while (RectWidth(TestRect) > RectWidth(Rect)) or
              (RectHeight(TestRect) > DH) do begin
          if Length(TempName) > 1 then begin
            TestRect := Rect;
            Delete(TempName, Length(TempName), 1);
            TempName := Trim(TempName);
            StrPLCopy(Buf, TempName + '...', 255);
            DrawText(Canvas.Handle, Buf, Length(TempName) + 3, TestRect,
              DT_WORDBREAK or DT_CALCRECT);
            Result := TempName + '...';
          end else begin
            Result := TempName + '..';
            TestRect := Rect;
            StrPLCopy(Buf, Result, 255);
            DrawText(Canvas.Handle, Buf, Length(Result), TestRect,
                      DT_WORDBREAK or DT_CALCRECT);
            if (RectWidth(TestRect) <= RectWidth(Rect)) and
              (RectHeight(TestRect) > DH) then
                Break;
            Result := TempName + '.';
            TestRect := Rect;
            StrPLCopy(Buf, Result, 255);
            DrawText(Canvas.Handle, Buf, Length(Result), TestRect,
                      DT_WORDBREAK or DT_CALCRECT);
            if (RectWidth(TestRect) <= RectWidth(Rect)) and
              (RectHeight(TestRect) > DH) then
                Break;
            Result := TempName;
          end;
        end;
      end;
    end;
  end;
end;
{=====}

function TVpCustomNavBar.nabButtonRect(Index : Integer) : TRect;
begin
  Result := Folders[Index].lfRect;
end;
{=====}

procedure TVpCustomNavBar.nabCommitEdit(Sender : TObject);
var
  Folder : TVpNavFolder;
  Item   : TVpNavBtnItem;
begin
  if not Assigned(nabEdit) then
    Exit;

  if (nabEdit.FolderIndex > -1) then begin
    if nabEdit.ItemIndex = -1 then begin
      {rename the folder}
      Folder := Folders[nabEdit.FolderIndex];
      Folder.Caption := nabEdit.Text;
    end else begin
      Item := Folders[nabEdit.FolderIndex].Items[nabEdit.ItemIndex];
      Item.Caption := nabEdit.Text;
    end;
  end;
  nabEdit.FolderIndex := -1;
  nabEdit.ItemIndex   := -1;
  nabEdit.Visible     := False;
  Update;
end;
{=====}

function TVpCustomNavBar.nabDropHitTest(X, Y : Integer) : Boolean;
  {given an X, Y, is this a legal spot to drop a folder?}
var
  I           : Integer;
  SpaceTop    : Integer;
  SpaceBottom : Integer;
  OldDrop     : Integer;
  Folder      : TVpNavFolder;
begin
  Result := False;
  {assume that X,Y aren't on a folder or item}
  OldDrop := nabDropY;
  try
    nabDragToFolder := -1;
    nabDragToItem := -1;
    if FolderCount = 0 then
      Exit;

    Folder := Folders[FActiveFolder];
    if Y <= Folder.lfRect.Bottom then
      Exit;

    if FolderCount > FActiveFolder+1 then
      if Y >= Folders[FActiveFolder+1].lfRect.Top then
        Exit;

    if (X < 0) or (X > ClientWidth) then
      Exit;

    {we're somewhere in the active folder}
    if Folder.ItemCount = 0 then begin
      {the active folder is empty}
      nabDropY := Folders[FActiveFolder].lfRect.Bottom + 3;
      nabDragToFolder := FActiveFolder;
      nabDragToItem := 0;
      Result := True;
      Exit;
    end;

    for I := nabTopItem to Folder.ItemCount-1 do begin
      {is there space above this item?}
      if I = nabTopItem then
        SpaceTop := Folder.lfRect.Bottom+1
      else
        SpaceTop := TVpNavBtnItem(Folder.Items[I - 1]).FLabelRect.Bottom + 1;
      SpaceBottom := TVpNavBtnItem(Folder.Items[I]).FIconRect.Top - 1;
      if (Y >= SpaceTop) and (Y <= SpaceBottom) then begin
        if SpaceTop - SpaceBottom < 6 then
          nabDropY := SpaceTop + (SpaceBottom - SpaceTop) div 2
        else
          nabDropY := SpaceTop + 3;
        Result := True;
        nabDragToFolder := FActiveFolder;
        nabDragToItem := I;
        nabExternalDragItem := I;
        Exit;
      end;
    end;

    {check below the last item...}
    SpaceTop :=
      TVpNavBtnItem(Folder.Items[Folder.ItemCount - 1]).FLabelRect.Bottom+1;
    SpaceBottom := nabItemsRect.Bottom - 1;
    if (Y >= SpaceTop) and (Y <= SpaceBottom) then begin
      nabDropY := SpaceTop + 3;
      nabDragToFolder := FActiveFolder;
      nabDragToItem := Folder.ItemCount;
      if nabFolderAccept then
        nabExternalDragItem := nabDragToItem
      else
        nabExternalDragItem := Folder.ItemCount - 1;
      Result := True;
    end;

  finally
    if (nabDropY <> OldDrop) then
      Repaint;
  end;
end;
{=====}

procedure TVpCustomNavBar.nabFolderChange(Sender : TObject);
var
  ParentForm: TCustomForm;
begin
  if not (csDestroying in ComponentState) then begin

    if FolderCount = 0 then
      FActiveFolder := -1
    else begin
      if Folders[FActiveFolder].FolderType = ftContainer then begin
        ParentForm := GetParentForm(Self);
        if ParentForm <> nil then
          if ContainsControl(ParentForm.ActiveControl) then
            ParentForm.ActiveControl := Self;
      end;
      if FActiveFolder = -1 then
        FActiveFolder := 0;
      if FActiveFolder >= FolderCount then
        FActiveFolder := 0;
    end;
    nabTopItem := 0;
    nabRecalcDisplayNames;
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.nabFolderSelected(Sender : TObject; Index : Integer);
begin
  if not (csDestroying in ComponentState) then
    ActiveFolder := Index;
end;
{=====}

procedure TVpCustomNavBar.nabFontChanged(Sender : TObject);
begin
  Perform(CM_FONTCHANGED, 0, 0);
end;
{=====}

procedure TVpCustomNavBar.nabGetEditorCaption(var Caption : string);
begin
  Caption := RSEditingFolders;
end;
{=====}

procedure TVpCustomNavBar.nabGetHitTest(X, Y : Integer;
                                       var FolderIndex : Integer;
                                       var ItemIndex : Integer);
var
  I      : Integer;
  Item   : TVpNavBtnItem;
  Folder : TVpNavFolder;
begin
  FolderIndex := -1;
  ItemIndex := -1;

  if FolderCount > 0 then begin
    {see if we've hit a folder}
    for I := 0 to FolderCount-1 do begin
      Folder := Folders[I];
      if PtInRect(Folder.lfRect, Point(X, Y)) then begin
      nabCursorOverItem := False;
        FolderIndex := I;
        Exit;
      end;
    end;

    {nope, check the active folder to see if we've hit an item}
    Folder := Folders[FActiveFolder];
    for I := nabTopItem to Folder.ItemCount-1 do begin
      Item := Folder.Items[I];
      if PtInRect(Item.FIconRect, Point(X,Y)) or
         (PtInRect(Item.FLabelRect, Point(X,Y)) and
         (Item.Caption <> '')) then begin
        if nabExternalDrag then begin
          nabCursorOverItem := True;
          nabExternalDragItem := I;
        end;
        ItemIndex := I;
        Exit;
      end else
        if nabExternalDrag then
          nabCursorOverItem := False;
    end;
  end;
end;
{=====}

function TVpCustomNavBar.nabGetFolderArea(Index : Integer) : TRect;
var
  I : Integer;
begin
  Result := ClientRect;
  for I := 0 to ActiveFolder do
    Inc(Result.Top, FButtonHeight);
  for I := FolderCount-1 downto ActiveFolder+1 do
    Dec(Result.Bottom, FButtonHeight);
end;
{=====}

procedure TVpCustomNavBar.nabImagesChanged(Sender : TObject);
begin
  Invalidate;
end;
{=====}

procedure TVpCustomNavBar.nabRecalcDisplayNames;
var
  I : Integer;
begin
  if not HandleAllocated then
    exit;
  Canvas.Font := Self.Font;
  {figure out display names for each folder...}
  for I := 0 to FolderCount-1 do
    Folders[I].lfDisplayName := GetDisplayString(Canvas, Folders[I].Caption, 1,
      ClientWidth);
  Invalidate;
end;
{=====}

function TVpCustomNavBar.nabShowScrollDown : Boolean;
var
  Folder : TVpNavFolder;
  Item   : TVpNavBtnItem;
begin
  Result := False;
  if (FolderCount > 0) then begin
    Folder := Folders[FActiveFolder];
    if Folder.ItemCount > 0 then begin
      Item := Folder.Items[Folder.ItemCount-1];
      Result := Item.FLabelRect.Bottom > nabItemsRect.Bottom;
    end;
  end;
end;
{=====}

procedure TVpCustomNavBar.nabScrollDownBtnClick(Sender : TObject);
begin
  if nabShowScrollDown then begin
    Inc(nabTopItem);
    InvalidateRect(Handle, @nabItemsRect, False);
  end;
end;
{=====}

function TVpCustomNavBar.nabShowScrollUp : Boolean;
begin
  Result := nabTopItem > 0;
end;
{=====}

procedure TVpCustomNavBar.nabScrollUpBtnClick(Sender : TObject);
begin
  if nabTopItem > 0 then begin
    Dec(nabTopItem);
    InvalidateRect(Handle, @nabItemsRect, False);
  end;
end;
{=====}

procedure TVpCustomNavBar.nabTimerEvent(Sender : TObject; Handle : Integer;
          Interval : Cardinal; ElapsedTime : LongInt);
var
  Pt : TPoint;
  Form : TCustomForm;
begin
  GetCursorPos(Pt);
  Pt := ScreenToClient(Pt);
  if not PtInRect(ClientRect, Pt) then begin
    if not nabMouseDown then begin
      {we're not doing internal dragging anymore}
      nabMouseDown := False;
      nabDragFromFolder := -1;
      nabDragFromItem := -1;
      if nabDropY <> -1 then begin
        nabDropY := -1;
        Repaint;
      end;
      if FActiveItem <> -1 then begin
        InvalidateItem(FActiveFolder, FActiveItem);
        FActiveItem := -1;
      end;
    end else if FAllowRearrange then begin
      Form := GetParentForm(Self);
      if (Form <> nil) then
        if Form.Active then begin
          SetCursor(Screen.Cursors[crNoDrop]);
          nabDropY := -1;
          Repaint;
        end;
    end;
  end else begin
    if nabDragFromItem <> -1 then begin
      {we're still doing internal dragging - update the cursor}
      if nabDropHitTest(Pt.X, Pt.Y) then
        SetCursor(Screen.Cursors[DragCursor])
      else begin
        SetCursor(Screen.Cursors[crNoDrop]);
        nabDropY := -1;
        Repaint;
      end;
    end;
  end;
end;
{=====}

procedure TVpCustomNavBar.MouseDown(Button : TMouseButton;
                                   Shift  : TShiftState;
                                   X, Y   : Integer);
begin
  {complete any editing}
  nabCommitEdit(nil);

  {get folder/item clicked}
  nabGetHitTest(X, Y, FPreviousFolder, FPreviousItem);

  {was it a click on a folder button?}
  if FPreviousFolder <> -1 then begin
    if Folders[FPreviousFolder].Enabled or
       (csDesigning in ComponentState) then begin
      if (Button = mbLeft) then begin
        nabMouseDown := True;
        Invalidate;
      end;
      Exit;
    end;
  end;

  if FPreviousItem <> -1 then begin
    if Folders[FActiveFolder].Enabled or
       (csDesigning in ComponentState) then begin
      if (Button = mbLeft) then begin
        InvalidateItem(FActiveFolder, FPreviousItem);
        nabMouseDown := True;
      end;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;
{=====}

procedure TVpCustomNavBar.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  ItemIndex   : Integer;
  FolderIndex : Integer;
begin
  nabGetHitTest(X, Y, FolderIndex, ItemIndex);

  {if FActiveItem is valid, and mouse is down, we're starting dragging}
  if nabMouseDown or nabExternalDrag then begin
    if nabScrollDownBtn.Visible then begin
      if Y > nabScrollDownBtn.Top then begin
        Inc(nabTopItem);
        InvalidateRect(Handle, @nabItemsRect, False);
        inherited MouseMove(Shift, X, Y);
        Exit;
      end;
    end;
    if nabScrollUpBtn.Visible then begin
      if Y < (nabScrollUpBtn.Top + nabScrollUpBtn.Height)then begin
        Dec(nabTopItem);
        InvalidateRect(Handle, @nabItemsRect, False);
        inherited MouseMove(Shift, X, Y);
        Exit;
      end;
    end;
    if (FActiveItem <> -1) and (ItemIndex = -1) and FAllowRearrange then begin
      nabDragFromFolder := FActiveFolder;
      nabDragFromItem := FActiveItem;
      if (FolderIndex = -1) then begin
        if nabDropHitTest(X, Y) then
          SetCursor(Screen.Cursors[DragCursor])
        else begin
          SetCursor(Screen.Cursors[crNoDrop]);
          nabDropY := -1;
          Repaint;
        end;
      end;
    end;
    if (FolderIndex <> -1) and FAllowRearrange then begin
      ActiveFolder := FolderIndex;
      nabDropY := -1;
      FActiveItem := -1;
      Repaint;
    end;
  end else begin
    if ItemIndex <> -1 then begin
      if (ItemIndex <> FActiveItem) then begin
        if FActiveItem <> -1 then
          {invalidate the old activeItem}
          InvalidateItem(FActiveFolder, FActiveItem);
        FActiveItem := ItemIndex;
        if FActiveItem <> -1 then begin
          {invalidate the new active item}
          InvalidateItem(FActiveFolder, FActiveItem);
        end;
      end;
    end else if FActiveItem <> -1 then begin
      InvalidateItem(FActiveFolder, FActiveItem);
      FActiveItem := -1;
    end;
    if FolderIndex <> -1 then begin
      if (FolderIndex <> FHotFolder) then begin
        if FHotFolder <> -1 then
          {invalidate the old activeItem}
          Invalidate;
        FHotFolder := FolderIndex;
        if FHotFolder <> -1 then begin
          {invalidate the new active item}
          Invalidate;
        end;
      end;
    end else if FHotFolder <> -1 then begin
      Invalidate;
      FHotFolder := -1;
    end;
  end;

  if ItemIndex <> - 1 then begin
    if nabLastMouseOverItem <> ItemIndex then
      DoMouseOverItem(X, Y, ItemIndex);
    nabLastMouseOverItem := ItemIndex;
  end else
    nabLastMouseOverItem := -1;

  inherited MouseMove(Shift, X, Y);
end;
{=====}

procedure TVpCustomNavBar.MouseUp(Button : TMouseButton; Shift : TShiftState;
          X, Y : Integer);
var
  FolderIndex : Integer;
  ItemIndex   : Integer;
  Folder      : TVpNavFolder;
  Item        : TVpNavBtnItem;
  FromItem    : TVpNavBtnItem;
  SourceName  : string;
begin

  if nabMouseDown then begin
    try
      nabGetHitTest(X, Y, FolderIndex, ItemIndex);

      if (FActiveItem <> -1) and (ItemIndex <> -1) then begin
        FSelectedItem := ItemIndex;
        InvalidateItem(FActiveFolder, ItemIndex);
        if FActiveItem = ItemIndex then
          DoItemClick(Button, Shift, ItemIndex);
      end;

      if nabDragFromItem <> -1 then begin
        if nabDropHitTest(X, Y) then begin
          {get the old item}
          Folder := Folders[nabDragFromFolder];
          FromItem := TVpNavBtnItem(Folder.Items[nabDragFromItem]);
          {create the new item}
          Folder := Folders[nabDragToFolder];


          Item := TVpNavBtnItem(Folder.FItems.Insert(nabDragToItem));
          Item.Assign(FromItem);
          SourceName := FromItem.Name;
          FromItem.Free;
          Item.Name := SourceName;
          nabRecalcDisplayNames;
          DoArrange;
        end;
        nabDragFromFolder := -1;
        nabDragFromItem := -1;
      end;

      if (ItemIndex = -1) then begin
        { Fire the OnFolderClick event. }
        DoFolderClick(Button, Shift, FolderIndex);
        ActiveFolder := FolderIndex;
      end;
    finally
      Invalidate;
      nabMouseDown := False;
    end;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;
{=====}

procedure TVpCustomNavBar.Notification(AComponent : TComponent;
                                            Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then begin
    if AComponent = FImages then
      Images := nil;
  end;
end;
{=====}

procedure TVpCustomNavBar.Paint;
var
  I             : Integer;
  J             : Integer;
  X             : Integer;
  W             : Integer;
  H             : Integer;
  CurPos        : Integer;
  Offset        : Integer;
  BkMode        : Integer;
  LabelWidth    : Integer;
  Flags         : Integer;
  MyRect        : TRect;
  TR            : TRect;
  ContainerRect : TRect;
  FolderType    : TVpFolderType;
  BkColor       : TColor;
  Folder        : TVpNavFolder;
  Item          : TVpNavBtnItem;
  DrawBmp       : TBitmap;
  Text          : string;
  Buf           : array[0..255] of Char;
  DrawFolder    : Boolean;
  BM            : TBitmap;
  RowStart      : Integer;
  ILeft         : Integer;
  IHeight       : Integer;
  IWidth        : integer;

begin
  if nabChanging then
    Exit;

  DrawBmp := TBitMap.Create;
  try
    DrawBmp.Width  := ClientWidth;
    DrawBmp.Height := ClientHeight;

    DrawBmp.Canvas.Font := Self.Font;
    with DrawBmp.Canvas do begin
      Pen.Color := FBackgroundColor;
      Brush.Color := FBackgroundColor;

      MyRect := ClientRect;

      DrawFolder := (FolderCount > 0);

      if DrawFolder then
        TR := nabGetFolderArea(FActiveFolder)
      else
        TR := ClientRect;

      if FBackgroundImage.Empty or (FBackgroundMethod = bmNone) then
        Rectangle(TR.Left, TR.Top, TR.Right, TR.Bottom)

      else begin
        case FBackgroundMethod of
          bmNormal  :
              Draw(TR.Left, TR.Top, FBackgroundImage);

          bmStretch :
              StretchDraw(TR, FBackgroundImage);

          bmTile    :
            begin
              {Tile the background in the default folder}
              RowStart := 0;
              IHeight := FBackgroundImage.Height;
              IWidth  := FBackgroundImage.Width;
              ILeft   := 0;
              while (RowStart < ClientRect.Bottom) do begin
                while (ILeft < ClientRect.Right) do begin
                  Draw(TR.Left + ILeft, RowStart, FBackgroundImage);
                  Inc(ILeft, IWidth);
                end;
                ILeft := 0;
                Inc(RowStart, IHeight)
              end;
            end;
        end;
      end;

      CurPos := 0;
      if FolderCount = 0 then begin
        nabScrollUpBtn.Visible := False;
        nabScrollDownBtn.Visible := False;
        Exit;
      end;

      {draw the folder buttons at the top}
      if DrawFolder then begin
        for I := 0 to FActiveFolder do begin
          MyRect.Top := CurPos;
          MyRect.Bottom := CurPos + FButtonHeight;
          Folders[I].lfRect := MyRect;

          {Draw the top tabs based on the selected style...}
          case FDrawingStyle of

            dsDefButton : begin
              {Draw regular buttons}
              TR := DrawButtonFace(DrawBmp.Canvas, MyRect, 1, bsNew, False,
                (I = FHotFolder) and nabMouseDown, False);
            end;

            dsEtchedButton : begin
              {Draw regular etched (Win98 style) buttons}
              Brush.Color := clBtnFace;
              FillRect(MyRect);
              Pen.Color := clBtnShadow;
              Brush.Style := bsClear;
              Rectangle(MyRect.Left, MyRect.Top, MyRect.Right - 1,
                MyRect.Bottom);
              Pen.Color := clBtnHighlight;
              MoveTo(MyRect.Left + 1, MyRect.Bottom - 2);
              LineTo(MyRect.Left + 1, MyRect.Top + 1);
              LineTo(MyRect.Right - 2, MyRect.Top + 1);
              { Draw border around control. }
              MoveTo(Width - 1, Top);
              LineTo(Width - 1, Height - 1);
              LineTo(0, Height - 1);
              Pen.Color := clWindowFrame;
              MoveTo(Width - 1, MyRect.Bottom);
              LineTo(1, MyRect.Bottom);
              LineTo(1, Height - 1);
              TR := MyRect;
            end;

           dsCoolTab: begin
              {Draw cool (Netscape Sidebar style) tabs}
              TR := DrawNavTab(DrawBmp.Canvas,       {Canvas}
                            MyRect,                      {Client Rect}
                            1,                           {Bevel Width}
                            FBackgroundColor,            {Tab Color}
                            I,                           {Tab Number}
                            true,                        {Cool Tabs?}
                            (I = FHotFolder),            {Is Focused}
                            (I = nabLastMouseOverItem)); {MouseOverItem}
            end;

            dsStandardTab: begin
              {Draw regular old tabs}
              TR := DrawNavTab(DrawBmp.Canvas,       {Canvas}
                            MyRect,                      {Client Rect}
                            1,                           {Bevel Width}
                            FBackgroundColor,            {Tab Color}
                            I,                           {Tab Number}
                            false,                       {Cool Tabs?}
                            (I = FHotFolder),            {Is Focused}
                            (I = nabLastMouseOverItem)); {MouseOverItem}
            end;

          end;
          StrPLCopy(Buf, Folders[I].lfDisplayName, 255);
          Inc(TR.Top);
          Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
          if Folders[I].Enabled then begin
            DrawText(DrawBmp.Canvas.Handle, Buf, StrLen(Buf), TR, Flags);
            if (I = FHotFolder) and not nabMouseDown then begin

              case FDrawingStyle of

                dsDefButton : begin
                  { Regular button style. }
                    InflateRect(TR,1,1);
                    inc(TR.Left);
                    Frame3D(DrawBmp.Canvas, TR, clBtnHighlight, clWindowFrame,
                      1);
                  end;

                dsEtchedButton : begin
                  { Etched style (Outlook98). }
                  Pen.Color := clWindowFrame;
                  MoveTo(TR.Right - 2, TR.Top);
                  LineTo(TR.Right - 2, TR.Bottom - 1);
                  LineTo(0, TR.Bottom - 1);
                  Pen.Color := clBtnShadow;
                  if I = ActiveFolder then
                    Offset := 1
                  else
                    Offset := 2;
                  MoveTo(TR.Right - 3, TR.Top - 2);
                  LineTo(TR.Right - 3, TR.Bottom - Offset);
                  LineTo(1, TR.Bottom - Offset);
                  if I = ActiveFolder then
                    Pixels[1, TR.Bottom - Offset] := clBtnHighlight;
                end;
              end;
            end;
          end else begin
            {use shadow text for inactive folder text}
            DrawBmp.Canvas.Font.Color := clHighlightText;
            SetBkMode(Canvas.Handle, OPAQUE);
            DrawText(DrawBmp.Canvas.Handle, Buf, -1, TR, Flags);
            SetBkMode(DrawBmp.Canvas.Handle, TRANSPARENT);
            DrawBmp.Canvas.Font.Color := clBtnShadow;
            OffsetRect(TR, -2, -1);
            DrawText(DrawBmp.Canvas.Handle, Buf, -1, TR, Flags);
            DrawBmp.Canvas.Font.Color := Self.Font.Color;
          end;
          Inc(CurPos, FButtonHeight);
        end;
      end else begin
        if FDrawingStyle = dsEtchedButton then begin
          { Draw border around control. }
          Pen.Color := clBtnHighlight;
          MoveTo(Width - 1, Top);
          LineTo(Width - 1, Height - 1);
          LineTo(0, Height - 1);
          Pen.Color := clWindowFrame;
          MoveTo(0, Height - 1);
          LineTo(0, 1);
          LineTo(Width - 2, 1);
        end;
        CurPos := 0;
      end;

      BkMode := GetBkMode(Handle);
      BkColor := GetBkColor(Handle);
      SetBkColor(Handle, DWord(FBackgroundColor));
      SetBkMode(Handle, TRANSPARENT);

      { draw the items for the active folder }
      Folder := Folders[FActiveFolder];

      if Folder.FolderType = ftDefault then
        if Folder.ItemCount > 0 then begin
          Inc(CurPos, 8);
          with nabItemsRect do begin
            Top := CurPos;
            Left := 0;
            Right := ClientWidth;
            Bottom := ClientHeight
                    - (FolderCount - FActiveFolder - 1) * FButtonHeight + 1;
          end;

          for J := 0 to Folder.ItemCount-1 do
            TVpNavBtnItem(Folder.Items[J]).FLabelRect.Bottom :=
               nabItemsRect.Bottom + 1;

          for J := nabTopItem to Folder.ItemCount-1 do begin
            if (FSelectedItem = J) then
              DrawBmp.Canvas.Font := FSelectedItemFont
            else
              DrawBmp.Canvas.Font := FItemFont;

            Item := Folder.Items[J];
            { If the caption is empty at designtime then display the item's }
            { name instead                                                  }
            if (csDesigning in ComponentState) and (Item.Caption = '') then
              Text := Item.Name
            else
              Text := Item.Caption;

            if Folder.IconSize = isLarge then begin {large icons}
              { glyph is at the top }
              with Item.FIconRect do begin
                { If an image list is assigned then use the image }
                { size. If no image list is assinged then assume  }
                { a 32 x 32 image size.                           }
                if Assigned(FImages) then begin
                  W := FImages.Width + 2;
                  H := FImages.Height + 2;
                end else begin
                  W := 32;
                  H := 32;
                end;
                Top := CurPos;
                Bottom := CurPos + H;
                Left := (ClientWidth - W) shr 1;
                Right := Left + W;
                if Top > nabItemsRect.Bottom then
                  Break;

                if FShowButtons then begin
                  if FActiveItem = J then begin
                    if nabMouseDown then
                      Pen.Color := clBlack
                    else
                      Pen.Color := clWhite;
                    MoveTo(Left-1, Bottom+1);
                    LineTo(Left-1, Top-1);
                    LineTo(Right+1, Top-1);
                    if nabMouseDown then
                      Pen.Color := clWhite
                    else
                      Pen.Color := clBlack;
                    LineTo(Right+1, Bottom+1);
                    LineTo(Left-1, Bottom+1);
                  end else begin
                    Pen.Color := FBackgroundColor;
                    Brush.Color := FBackgroundColor;
                  end;
                  if Assigned(FImages) and
                     (Item.IconIndex >= 0) and
                     (Item.IconIndex < FImages.Count) then
                    FImages.Draw(DrawBmp.Canvas, Item.FIconRect.Left + 2,
                      Item.FIconRect.Top + 2, Item.IconIndex);
                  {make the icon's bottom blend into the label's top}
                  Item.FIconRect.Bottom := Item.FIconRect.Bottom + 4;
                end;
              end;
              Inc(CurPos, H + 4);

              {now, draw the text}
              with Item.FLabelRect do begin
                Top := CurPos;
                Bottom := CurPos + (FButtonHeight shl 1) - 7;

                Left := 0;
                Right := ClientWidth - 1;
                Item.liDisplayName := GetLargeIconDisplayName(DrawBmp.Canvas,
                  Item.FLabelRect, Text);
                X := DrawBmp.Canvas.TextWidth(Item.liDisplayName);
                Left := (ClientWidth - X) div 2;
                if Left < 5 then
                  Left := 5;
                Right := Left + X;
                if Right > ClientWidth-5 then
                  Right := ClientWidth-5;
                if Top > nabItemsRect.Bottom then
                  Break;
              end;

              StrPLCopy(Buf, Item.liDisplayName, 255);
              DrawText(DrawBmp.Canvas.Handle, Buf, Length(Item.liDisplayName),
                       Item.FLabelRect, DT_CENTER or DT_VCENTER or
                       DT_WORDBREAK or DT_CALCRECT);
              LabelWidth := RectWidth(Item.FLabelRect);
              with Item.FLabelRect do begin
                Left := (ClientWidth - LabelWidth) div 2;
                Right := Left + LabelWidth + 1;
              end;
              BkMode := SetBkMode(DrawBmp.Canvas.Handle, TRANSPARENT);
              Inc(CurPos, DrawText(DrawBmp.Canvas.Handle, Buf,
                        Length(Item.liDisplayName),
                        Item.FLabelRect,
                        DT_CENTER or DT_VCENTER or DT_WORDBREAK));
              SetBkMode(DrawBmp.Canvas.Handle, BkMode);

              Inc(CurPos, FItemSpacing);
            end else begin {small icons}
              {glyph is at the left}
              with Item.FIconRect do begin
                Top := CurPos;
                Offset := (Abs(DrawBmp.Canvas.Font.Height)) div 2;
                if Offset > 8 then
                  Top := Top + Offset - 8;
                Bottom := Top + 16;
                Left := 8;
                Right := Left + 16;
                if Top > nabItemsRect.Bottom then
                  Break;

                if FShowButtons then begin
                  if FActiveItem = J then begin
                    if nabMouseDown then
                      Pen.Color := clBlack
                    else
                      Pen.Color := clWhite;
                    MoveTo(Left-1, Bottom+1);
                    LineTo(Left-1, Top-1);
                    LineTo(Right+1, Top-1);
                    if nabMouseDown then
                      Pen.Color := clWhite
                    else
                      Pen.Color := clBlack;
                    LineTo(Right+1, Bottom+1);
                    LineTo(Left-1, Bottom+1);
                    Brush.Color := FBackgroundColor;
                  end else begin
                    Pen.Color := FBackgroundColor;
                    Brush.Color := FBackgroundColor;
                    Rectangle(Item.FIconRect.Left - 1,
                               Item.FIconRect.Top - 1,
                               Item.FIconRect.Right + 1,
                               Item.FIconRect.Bottom + 1);
                  end;
                  if Assigned(FImages) then begin
                    BM := TBitmap.Create;
                    try
                      BM.Width := FImages.Width;
                      BM.Height := FImages.Height;
                      FImages.Draw(BM.Canvas, 0, 0, Item.IconIndex);
                      DrawBmp.Canvas.BrushCopy(Item.FIconRect, BM,
                        Rect(0, 0, BM.Width, BM.Height), BM.Canvas.Pixels[0,
                          BM.Height-1]);
                    finally
                      BM.Free;
                    end;
                  end;
                end;
                {make the icon's right blend into the label's left}
                Item.FIconRect.Right := Item.FIconRect.Right + 3;
              end;

              {now, draw the text}
              with Item.FLabelRect do begin
                Top := CurPos;
                Bottom := CurPos + (FButtonHeight shl 1) -7;
                Left := Item.FIconRect.Right;
                X := Self.ClientWidth - Left - 7;
                Right := Left + X;
                if Top > nabItemsRect.Bottom then
                  Break;
              end;
              Item.liDisplayName :=
                GetDisplayString(DrawBmp.Canvas, Text, 1,
                  RectWidth(Item.FLabelRect));
              StrPLCopy(Buf, Item.liDisplayName, 255);
              DrawText(DrawBmp.Canvas.Handle, Buf, Length(Item.liDisplayName),
                       Item.FLabelRect, DT_LEFT or DT_VCENTER or DT_CALCRECT);
              LabelWidth := RectWidth(Item.FLabelRect);
              with Item.FLabelRect do
                Right := Left + LabelWidth + 1;
              DrawText(DrawBmp.Canvas.Handle, Buf, Length(Item.liDisplayName),
                       Item.FLabelRect, DT_LEFT or DT_VCENTER);

              Inc(CurPos, FItemSpacing);
            end;
          end;
      end;


      {now, draw the folder buttons at the bottom}
      DrawBmp.Canvas.Font := Self.Font;
      SetBkMode(Handle, BkMode);
      SetBkColor(Handle, BkColor);

      case FDrawingStyle of
        { Regular button style. }
        dsDefButton :
          CurPos := ClientHeight - FButtonHeight;
        { Etched style (Outlook98). }
        dsEtchedButton :
          CurPos := ClientHeight - FButtonHeight - 1;
        { Cool Tab }
        dsCoolTab:
          CurPos := ClientHeight - FButtonHeight;
        { Regular Tab }
        dsStandardTab:
          CurPos := ClientHeight - FButtonHeight;
      end;

      for I := FolderCount-1 downto FActiveFolder+1 do begin
        MyRect.Top := CurPos;
        MyRect.Bottom := CurPos + FButtonHeight;
        Folders[I].lfRect := MyRect;
        case FDrawingStyle of

          dsDefButton : begin
            {Regular Old Buttons}
            TR := DrawButtonFace(DrawBmp.Canvas, MyRect, 1, bsNew, False,
              (I = FHotFolder) and nabMouseDown, False);
          end;

          dsEtchedButton : begin
            {Etched (Outlook98 style) buttons}
            Brush.Color := clBtnFace;
            FillRect(MyRect);
            Pen.Color := clBtnShadow;
            Brush.Style := bsClear;
            Rectangle(MyRect.Left, MyRect.Top, MyRect.Right - 1,
              MyRect.Bottom);
            Pen.Color := clBtnHighlight;
            MoveTo(MyRect.Left + 1, MyRect.Bottom - 2);
            LineTo(MyRect.Left + 1, MyRect.Top + 1);
            LineTo(MyRect.Right - 2, MyRect.Top + 1);
            Pen.Color := clBtnHighlight;
            MoveTo(Width - 1, 0);
            LineTo(Width - 1, Height);
            TR := MyRect;
          end;

          dsCoolTab: begin
            {Draw cool (Netscape Sidebar style) tabs}
            TR := DrawNavTab(DrawBmp.Canvas,       {Canvas}
                          MyRect,                      {Client Rect}
                          1,                           {Bevel Width}
                          FBackgroundColor,            {Tab Color}
                          I,                           {Tab Number}
                          true,                        {Cool Tabs?}
                          (I = FHotFolder),            {Is Focused}
                          (I = nabLastMouseOverItem)); {MouseOverItem}
          end;

          dsStandardTab: begin
            {Draw regular old tabs}
            TR := DrawNavTab(DrawBmp.Canvas,       {Canvas}
                          MyRect,                      {Client Rect}
                          1,                           {Bevel Width}
                          FBackgroundColor,            {Tab Color}
                          I,                           {Tab Number}
                          false,                       {Cool Tabs?}
                          (I = FHotFolder),            {Is Focused}
                          (I = nabLastMouseOverItem)); {MouseOverItem}
          end;

        end;
        Inc(TR.Top);
        StrPLCopy(Buf, Folders[I].lfDisplayName, 255);
        Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
        if Folders[I].Enabled then begin
          DrawText(DrawBmp.Canvas.Handle, Buf, StrLen(Buf), TR, Flags);
          if (I = FHotFolder) and not nabMouseDown then begin
            case FDrawingStyle of

              dsDefButton : begin
                { Regular button style. }
                InflateRect(TR,1,1);
                inc(TR.Left);
                Frame3D(DrawBmp.Canvas, TR, clBtnHighlight, clWindowFrame, 1);
              end;

              dsEtchedButton : begin
                { Etched (Outlook98 style). }
                Pen.Color := clWindowFrame;
                MoveTo(TR.Right - 2, TR.Top);
                LineTo(TR.Right - 2, TR.Bottom - 1);
                LineTo(0, TR.Bottom - 1);
                Pen.Color := clBtnShadow;
                MoveTo(TR.Right - 3, TR.Top - 2);
                LineTo(TR.Right - 3, TR.Bottom - 2);
                LineTo(1, TR.Bottom - 2);
              end;
            end;
          end;
        end else begin
          {use shadow text for inactive folder text}
          DrawBmp.Canvas.Font.Color := clHighlightText;
          SetBkMode(Canvas.Handle, OPAQUE);
          DrawText(DrawBmp.Canvas.Handle, Buf, -1, TR, Flags);
          SetBkMode(DrawBmp.Canvas.Handle, TRANSPARENT);
          DrawBmp.Canvas.Font.Color := clBtnShadow;
          OffsetRect(TR, -2, -1);
          DrawText(DrawBmp.Canvas.Handle, Buf, -1, TR, Flags);
          DrawBmp.Canvas.Font.Color := Self.Font.Color;
        end;
        Dec(CurPos, FButtonHeight);
      end;

      if not (csDesigning in ComponentState) then begin
        {show the top scroll button}
        if nabShowScrollUp then begin
          nabScrollUpBtn.Top := Folders[FActiveFolder].lfRect.Bottom + 5;
          nabScrollUpBtn.Left := ClientWidth - 20;
          nabScrollUpBtn.Visible := True;
        end else
          nabScrollUpBtn.Visible := False;

        {show the bottom scroll button}
        if nabShowScrollDown then begin
          if FActiveFolder = FolderCount-1 then
            {there are no folders beyond the active one}
            nabScrollDownBtn.Top := ClientHeight -20
          else
            nabScrollDownBtn.Top := Folders[FActiveFolder+1].lfRect.Top - 20;
          nabScrollDownBtn.Left := ClientWidth - 20;
          nabScrollDownBtn.Visible := True;
        end else
          nabScrollDownBtn.Visible := False;
      end;
      {if we're dragging, show the drag marker}
      if (nabDragFromItem <> -1) or nabExternalDrag then begin
        if (nabDropY <> -1) then begin
          { Don't draw the drag marker if we're doing external }
          { dragging and the cursor is over an item. }
          if nabExternalDrag then
            if not nabFolderAccept or nabCursorOverItem then
              Exit;
          Pen.Color := clBlack;
          Brush.Color := clBlack;
          MoveTo(5, nabDropY);
          LineTo(ClientWidth - 5, nabDropY);
          DrawBmp.Canvas.Polygon([ Point(3,nabDropY+4),
                           Point(7,nabDropY),
                           Point(3, nabDropY-4)]);
          DrawBmp.Canvas.FloodFill(5, nabDropY, clBlack, fsBorder);
          DrawBmp.Canvas.Polygon([ Point(ClientWidth-3,nabDropY+4),
                           Point(ClientWidth-7,nabDropY),
                           Point(ClientWidth-3,nabDropY-4)]);
          DrawBmp.Canvas.FloodFill(ClientWidth-5, nabDropY, clBlack, fsBorder);
        end;
      end;
    end;
  finally
    Canvas.CopyMode := cmSrcCopy;
    Canvas.CopyRect(ClientRect, DrawBmp.Canvas, ClientRect);
    DrawBmp.Free;
  end;

  {For container style folders...}

  {Hide the containers for all inactive folders}
  for I := 0 to FFolders.Count - 1 do begin
    if I <> FActiveFolder then begin
      if Folders[i].FolderType = ftContainer then
      with Containers[Folders[i].ContainerIndex] do begin
        Width := 0;
        Height := 0;
        Visible := false;
      end;
    end;
  end;

  Folder := Folders[FActiveFolder];
  TR := nabGetFolderArea(FActiveFolder);

  if Folder.FolderType = ftContainer then
  with Containers[Folder.ContainerIndex] do begin
  {Position and show the folder's container}
    Height := TR.Bottom - TR.Top;
    Top := TR.Top;
    Left := TR.Left;
    Width := TR.Right - TR.Left;
    Visible := true;
    BringToFront;

    for I := 0 to ControlCount - 1 do
      Controls[i].Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.SetActiveFolder(Value : Integer);
var
  Y      : Integer;
  YDelta : Integer;
  R      : TRect;
  R2     : TRect;
  Buf    : array[0..1023] of Char;
  AllowChange : Boolean;
begin
  if Value <> FActiveFolder then begin

    if FolderCount = 0 then
      FActiveFolder := -1
    else if (Value > -1) and (Value < FolderCount) then begin

    { Fire DoFolderChange only if not dragging. }
    if nabDragFromItem = -1 then begin
      { Default for AllowChange is True. }
      AllowChange := True;
      { Fire the OnFolderChange event. }
      DoFolderChange(Value, AllowChange);
      { If AllowChange is False then bail out. }
      if not AllowChange then
        Exit;
    end;
      {animated scroll}
      if FActiveFolder > -1 then begin
        {play sound}
        if FPlaySounds and (FSoundAlias > '') then begin
          StrPLCopy(Buf, FSoundAlias, SizeOf(Buf)-1);
          FPlaySounds := PlaySound(@Buf, 0, SND_ASYNC);
        end;

        if Parent <> nil then begin
          {scroll selection}
          Canvas.Brush.Color := FBackgroundColor;
          R := nabGetFolderArea(FActiveFolder);
          R2 := R;
          if Value > FActiveFolder then begin
            {up}
            YDelta := -FScrollDelta;
            Inc(R.Bottom, Abs(Value-FActiveFolder)*FButtonHeight);
            R2.Top := R2.Bottom+Abs(Value-FActiveFolder)*FButtonHeight;
            R2.Bottom := R2.Top;
          end else begin
            {down}
            YDelta := +FScrollDelta;
            Dec(R.Top, Abs(Value-FActiveFolder)*FButtonHeight);
            R2.Bottom := R2.Top-Abs(Value-FActiveFolder)*FButtonHeight;
            R2.Top := R2.Bottom;
          end;
          Y := RectHeight(R)-FScrollDelta;
          while Y > 0 do begin
            ScrollWindow(Handle, 0, YDelta, @R, @R);
            Dec(Y, FScrollDelta);
            {fill scrolled area}
            if YDelta > 0 then
              Inc(R2.Bottom, FScrollDelta)
            else
              Dec(R2.Top, FScrollDelta);
            Canvas.FillRect(R2);
          end;
        end;
      end;

      FActiveFolder := Value;
      nabTopItem := 0;
      FActiveItem := -1;
      FSelectedItem := -1;
      Invalidate;

    end;
    { Fire the OnFolderChanged event. }
    DoFolderChanged(FActiveFolder)
  end;
end;
{=====}

procedure TVpCustomNavBar.SetBackgroundColor(Value : TColor);
begin
  if Value <> FBackgroundColor then begin
    FBackgroundColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.SetBackgroundImage(Value : TBitmap);
begin
  if Assigned(Value) then
    FBackgroundImage.Assign(Value)
  else begin
    FBackgroundImage.Free;
    FBackgroundImage := TBitmap.Create;
  end;
  Invalidate;
end;
{=====}

procedure TVpCustomNavBar.SetBackgroundMethod(Value : TVpBackgroundMethod);
begin
  if Value <> FBackgroundMethod then begin
    FBackgroundMethod := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.SetBorderStyle(const Value : TBorderStyle);
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;
{=====}

procedure TVpCustomNavBar.SetButtonHeight(Value : Integer);
begin
  if Value <> FButtonHeight then begin
    {Minimum ButtonHeight for CoolTabs is 17}
    if FDrawingStyle = dsCoolTab then begin
      if Value < 17 then FButtonHeight := 17
      else FButtonHeight := Value;
    end else
      FButtonHeight := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.SetDrawingStyle(Value: TVpFolderDrawingStyle);
begin
  if Value <> FDrawingStyle then begin
    FDrawingStyle := Value;
    if FDrawingStyle = dsEtchedButton  then
      BorderStyle := bsNone
    else
      BorderStyle := bsSingle;

    {Minimum ButtonHeight for CoolTabs is 17}
    if (FDrawingStyle = dsCoolTab) and (FButtonHeight < 17) then
      FButtonHeight := 17;

    Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  nabRecalcDisplayNames;
end;
{=====}

procedure TVpCustomNavBar.SetImages(Value : TImageList);
begin
  if FImages <> nil then
    FImages.OnChange := nil;
  FImages := Value;
  if FImages <> nil then begin
    Images.OnChange := nabImagesChanged;
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.SetItemFont(Value : TFont);
begin
  if Assigned(Value) then
    FItemFont.Assign(Value);
end;
{=====}

procedure TVpCustomNavBar.SetItemSpacing(Value : Word);
begin
  if (Value > 0) then begin
    FItemSpacing := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.SetSelectedItemFont(Value : TFont);
begin
  if Assigned(Value) then
    FSelectedItemFont.Assign(Value);
end;
{=====}

procedure TVpCustomNavBar.SetScrollDelta(Value: Integer);
begin
  if Value <= 0 then
    FScrollDelta := 1
  else
    FScrollDelta := Value;
end;
{=====}

procedure TVpCustomNavBar.WMEraseBkGnd(var Msg : TWMEraseBkGnd);
begin
  Msg.Result := 1;  {don't erase background}
end;
{=====}

procedure TVpCustomNavBar.WMGetDlgCode(var Msg : TWMGetDlgCode);
begin
  {tell windows we are a static control to avoid receiving the focus}
  Msg.Result := DLGC_STATIC;
end;
{=====}

procedure TVpCustomNavBar.WMNCHitTest(var Msg : TWMNCHitTest);
begin
  inherited;
  nabHitTest.X := Msg.Pos.X;
  nabHitTest.Y := Msg.Pos.Y;
end;
{=====}

procedure TVpCustomNavBar.WMSetCursor(var Msg : TWMSetCursor);
var
  I : Integer;
  R : TRect;
begin
  if csDesigning in ComponentState then begin
    if (Msg.HitTest = HTCLIENT) then begin
      nabOverButton := False;
      nabHitTest := ScreenToClient(nabHitTest);
      {check if mouse is over a button}
      for I := 0 to FolderCount-1 do begin
        R := nabButtonRect(I);
        if PtInRect(R, nabHitTest) then begin
          nabOverButton := True;
          Break;
        end;
      end;
    end;
  end;
  inherited;
end;
{=====}

{ Overridden DragOver method. }
procedure TVpCustomNavBar.DragOver(Source: TObject;
                                  X, Y: Integer;
                                  State: TDragState;
                              var Accept: Boolean);
var
  ItemIndex    : Integer;
  FolderIndex  : Integer;
begin
  { If State is dsDragLeave then the user has dragged }
  { outside us. Invalidate the component to get rid   }
  { of any left-over drawing and exit. }
  if State = dsDragLeave then begin
    nabExternalDrag := False;
    nabFolderAccept := False;
    nabItemAccept := False;
    nabMouseDown := False;
    nabChanging := False;
    nabTopItem := 0;
    nabDragFromItem := -1;
    nabDragFromFolder := -1;
    Invalidate;
    nabAcceptAny := False;
    inherited DragOver(Source, X, Y, State, nabAcceptAny);
    Exit;
  end;

  nabFolderAccept := True;
  nabItemAccept   := True;
  { Call the user's OnDragOver. }
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, Source,
      X, Y, State, nabFolderAccept, nabItemAccept);

  { Might have to scroll the items in the folder. }
  if nabScrollDownBtn.Visible then begin
    if Y > nabScrollDownBtn.Top then begin
      Inc(nabTopItem);
      InvalidateRect(Handle, @nabItemsRect, False);
    end;
  end;
  if nabScrollUpBtn.Visible then begin
    if Y < (nabScrollUpBtn.Top + nabScrollUpBtn.Height)then begin
      Dec(nabTopItem);
      InvalidateRect(Handle, @nabItemsRect, False);
    end;
  end;

  Accept := nabFolderAccept or nabItemAccept;
  if nabFolderAccept or nabItemAccept then begin
    nabGetHitTest(X, Y, FolderIndex, ItemIndex);
    nabDropHitTest(X, Y);
    nabExternalDrag := True;
    { Change folder if necessary. }
    if (FolderIndex <> -1) and (FolderIndex <> FActiveFolder) then
      ActiveFolder := FolderIndex;
    if nabItemAccept then
      FActiveItem := ItemIndex;
    Invalidate;
  end;
end;
{=====}

procedure TVpCustomNavBar.DragDrop(Source: TObject; X, Y : Integer);
begin
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, Source, X, Y, FActiveFolder, nabExternalDragItem);
  nabExternalDrag := False;
  nabFolderAccept := False;
  nabItemAccept := False;
  nabMouseDown := False;
  nabChanging := False;
  nabTopItem := 0;
  nabDragFromFolder := -1;
  Invalidate;
  inherited DragDrop(Source, X, Y);
end;
{=====}

function TVpCustomNavBar.GetChildOwner: TComponent;
begin
  Result := Self;
end;

end.
