{ Release 5 is the work of Kurt Blide <kub@sam.sdu.dk>
  (old address is kub@busieco.ou.dk) }

{ Release 3 & 4 are updates by Jordan Russell

  Release 4 changes include:
  - Added the missing events that were on Delphi's original TDBNavigator,
    including OnClick and BeforeAction.
  - Updated my site address below :)

  Release 3 changes include:
  - Support for Toolbar97 1.65.
  - Problem with repeating buttons fixed.
  - Compiler warnings fixed. }

{Original source code for the DBNavigator (C) by Borland
*********************************************************
DBNavigator97 (Release 5)
*********************************************************
Source modified and enhanced for use with the Toolbar97 components
from Jordan Russell (http://www.digicron.com/jordanr/).

The DBNavigator97 works exactly like the original Delphi 2.0 DB Navigator.
When putting it on a Toolbar97,the buttons will be separated from each
other when running the application,and the toolbar can be resized with
correct "button wrapping" instead of keeping one unseparated block.

I didn't test it with Delphi 1. If there are problems I suppose they have
to do with the database implementation.

Expect for the original DBNavigator source parts by Borland,this source is
(C) by Arsène von Wyss (avonwyss@beaulieu-software.ch)}

unit DBNav97;

interface

uses
  SysUtils,Windows,Messages,Classes,Controls,Forms,ExtCtrls,DB,DBTables,Buttons,DBCtrls,TB97,TB97Tlbr,TB97Ctls;

{ Determine Delphi/C++Builder version }
{$IFNDEF VER90}  { if it's not Delphi 2.0 }
  {$IFNDEF VER93}  { and it's not C++Builder 1.0 }
    {$DEFINE DBNav97Delphi3}  { then it must be Delphi 3 or C++Builder 3 }
    {$IFNDEF VER100}  { if it's not Delphi 3.0 }
      {$ObjExportAll On}  { <- needed for compatibility with run-time packages in C++Builder 3 }
      {$IFNDEF VER110}  { and it's not C++Builder 3.0 }
        {$DEFINE DBNav97Delphi4}  { then it must be at least Delphi 4 or C++Builder 4 }
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

type
  TNavButton97=class;
  TNavDataLink97=class;

{TDBNavigator97}
  TDBNavigator97=class(TCustomPanel)
  private
    FDataLink: TNavDataLink97;
    FVisibleButtons,FEnabledButtons: TButtonSet;
    FDisplayMode: TButtonDisplayMode;
    FHints: TStrings;
    FCaptions: TStrings;
    FWordWrap: Boolean;
    FLayout: TButtonLayout;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: ENavClick;
    FBeforeAction: ENavClick;
    FocusedButton: TNavigateBtn;
    FConfirmDelete,FOpaque: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure InitButtons;
    procedure InitHints;
    procedure InitCaptions;
    procedure SetCaptions(Value: TStrings);
    procedure SetDisplayMode (Value: TButtonDisplayMode);
    procedure SetWordWrap (Value: Boolean);
    procedure InitFont;
    procedure ClickHandler(Sender: TObject);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure SetVisible(Value: TButtonSet);
    procedure SetBtnEnabled(Value: TButtonSet);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetHints(Value: TStrings);
    procedure SetOpaque(Value: Boolean);
    procedure SetLayout (Value: TButtonLayout);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBackground(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    Buttons: array[TNavigateBtn] of TNavButton97;
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc {$IFDEF DBNav97Delphi3}; Root: TComponent {$ENDIF}); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight: Integer); override;
    procedure BtnClick(Index: TNavigateBtn);
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TButtonSet read FVisibleButtons write SetVisible default [nbFirst,nbPrior,nbNext,nbLast,nbInsert,nbDelete,nbEdit,nbPost,nbCancel,nbRefresh];
    property EnabledButtons: TButtonSet read FEnabledButtons write SetBtnEnabled default [nbFirst,nbPrior,nbNext,nbLast,nbInsert,nbDelete,nbEdit,nbPost,nbCancel,nbRefresh];
    property Opaque: Boolean read FOpaque write SetOpaque default True;
    property Align;
    property Enabled;
    property Hints: TStrings read FHints write SetHints;
    property Captions: TStrings read FCaptions write SetCaptions;
    property DisplayMode: TButtonDisplayMode read FDisplayMode write SetDisplayMode default dmBoth;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property Visible;
    property Font;
    property BeforeAction: ENavClick read FBeforeAction write FBeforeAction;
    property OnClick: ENavClick read FOnNavClick write FOnNavClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF DBNav97Delphi4}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    {$IFDEF DBNav97Delphi4}
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;
  end;

{TNavButton97}
  TNavButton97=class(TToolbarButton97)
  private
    FIndex: TNavigateBtn;
  protected
    procedure Paint; override;
  public
    property Index : TNavigateBtn read FIndex write FIndex;
  end;

{TNavDataLink97}
  TNavDataLink97=class(TDataLink)
  private
    FNavigator: TDBNavigator97;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TDBNavigator97);
    destructor Destroy; override;
  end;

procedure Register;

implementation

{$R DBNav97.Res}

uses
  BDE,Clipbrd,DBConsts,Dialogs;

{TDBNavigator97}

const
  BtnTypeName: array[TNavigateBtn] of PChar=('FIRST','PRIOR','NEXT','LAST',
      'INSERT','DELETE','EDIT','POST','CANCEL','REFRESH');

  BtnHintId: array[TNavigateBtn] of {$IFNDEF DBNav97Delphi3}Word{$ELSE}String{$ENDIF}=(SFirstRecord,SPriorRecord,SNextRecord,SLastRecord,
      SInsertRecord,SDeleteRecord,SEditRecord,SPostEdit,SCancelEdit,SRefreshRecord);

procedure TDBNavigator97.SetParent(AParent: TWinControl);
var
  I: TNavigateBtn;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    for I:=High(Buttons) downto Low(Buttons) do
      with Buttons[I] do begin
        Visible:=False;
        Top:=Top;
        Left:=Left;
        Parent:=self.Parent;
        Visible:=True;
      end;
end;

procedure TDBNavigator97.SetBtnEnabled(Value: TButtonSet);
var
  I: TNavigateBtn;
begin
  FEnabledButtons:=Value;
  for I:=Low(Buttons) to High(Buttons) do
    Buttons[I].Enabled:=I in Value;
  Invalidate;
end;

procedure TDBNavigator97.SetOpaque(Value: Boolean);
var
  I: TNavigateBtn;
begin
  if Value<>FOpaque then begin
    FOpaque:=Value;
    if Value then
      ControlStyle := ControlStyle + [csOpaque]
    else
      ControlStyle := ControlStyle - [csOpaque];
    for I:=Low(Buttons) to High(Buttons) do
      Buttons[I].Opaque:=Value;
    Invalidate;
  end;
end;

procedure TDBNavigator97.WMEraseBackground(var Message: TWMEraseBkgnd);
begin
end;

constructor TDBNavigator97.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle:=ControlStyle-[csAcceptsControls,csSetCaption]+[csOpaque];
  FDataLink:=TNavDataLink97.Create(Self);
  FVisibleButtons:=[nbFirst,nbPrior,nbNext,nbLast,nbInsert,nbDelete,nbEdit,nbPost,nbCancel,nbRefresh];
  FEnabledButtons:=FVisibleButtons;
  FHints:=TStringList.Create;
  FCaptions:=TStringList.Create;
  FWordWrap:=True;
  FOpaque:=True;
  FLayout:=blGlyphLeft;
  FDisplayMode:=dmBoth;
  InitButtons;
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  Width:=230;
  Height:=22;
  ButtonWidth:=0;
  FocusedButton:=nbFirst;
  FConfirmDelete:=True;
  if not (csDesigning in ComponentState) then
    Visible:=False;
end;

destructor TDBNavigator97.Destroy;
begin
  FDataLink.Free;
  FHints.Free;
  FCaptions.Free;
  FDataLink:=nil;
  inherited;
end;

procedure TDBNavigator97.InitButtons;
var
  I: TNavigateBtn;
  Btn: TNavButton97;
  X: Integer;
begin
  MinBtnSize:=Point(20,18);
  X:=0;
  for I:=Low(Buttons) to High(Buttons) do begin
    Btn:=TNavButton97.Create(self);
    with Btn do begin
      if (csDesigning in ComponentState) then
        Parent:=Self;
      Index:=I;
      SetBounds(X,0,MinBtnSize.X,MinBtnSize.Y);

      { Changed resource name, and added IFDEF }
      {$IFDEF DBNav97Delphi3}
      Glyph.LoadFromResourceName (HInstance, Format('dbn_%s97', [BtnTypeName[I]]));
      {$ELSE}
      Glyph.Handle:=LoadBitmap(HInstance,PChar(Format('dbn_%s97',[BtnTypeName[I]])));
      {$ENDIF}
      NumGlyphs:=2;
      Enabled:=True;
      Opaque:=FOpaque;
      Layout:=FLayout;
      DisplayMode:=FDisplayMode;
      OnClick:=self.ClickHandler;
      OnMouseDown:=BtnMouseDown;
      Visible:=I in FVisibleButtons;
    end;
    Buttons[I]:=Btn;
    X:=X+MinBtnSize.X;
  end;
  InitHints;
  InitCaptions;
  InitFont;
  Buttons[nbPrior].Repeating := True;
  Buttons[nbNext].Repeating := True;
end;

procedure TDBNavigator97.InitHints;
var
  I: Integer;
  J: TNavigateBtn;
begin
  for J:=Low(Buttons) to High(Buttons) do begin
    Buttons[J].Hint:={$IFNDEF DBNav97Delphi3}LoadStr{$ENDIF}(BtnHintId[J]);
    Buttons[J].ShowHint:=ShowHint;
  end;
  J:=Low(Buttons);
  for I:=0 to (FHints.Count-1) do begin
    if FHints.Strings[I]<>'' then
      with Buttons[J] do begin
        Hint:=FHints.Strings[I];
        ShowHint:=self.ShowHint;
      end;
    if J=High(Buttons) then
      Exit;
    Inc(J);
  end;
end;

procedure TDBNavigator97.SetHints(Value: TStrings);
begin
  FHints.Assign(Value);
  InitHints;
end;

procedure TDBNavigator97.InitCaptions;
var
  I: Integer;
  J: TNavigateBtn;
begin
  for J:=Low(Buttons) to High(Buttons) do begin
    Buttons[J].Caption:={$IFNDEF DBNav97Delphi3}LoadStr{$ENDIF}(BtnHintId[J]);
  end;
  J:=Low(Buttons);
  for I:=0 to (FCaptions.Count-1) do begin
    if FCaptions.Strings[I]<>'' then
      with Buttons[J] do begin
        Caption:=FCaptions.Strings[I];
      end;
    if J=High(Buttons) then
      Exit;
    Inc(J);
  end;
end;

procedure TDBNavigator97.SetWordWrap(Value: Boolean);
var
  J: TNavigateBtn;
begin
     if FWordWrap<>Value then begin
        FWordWrap:=Value;
        for J:=Low(Buttons) to High(Buttons) do begin
                Buttons[J].WordWrap:= FWordWrap;
        end;
     end;
end;

procedure TDBNavigator97.SetCaptions(Value: TStrings);
begin
     FCaptions.Assign(Value);
     InitCaptions;
end;

procedure TDBNavigator97.SetDisplayMode (Value: TButtonDisplayMode);
var
        J: TNavigateBtn;
begin
   if FDisplayMode <> Value then begin
      FDisplayMode := Value;

      for J:=Low(Buttons) to High(Buttons) do begin
          Buttons[J].DisplayMode:=Value;
      end;
    end;
end;

procedure TDBNavigator97.InitFont;
var
        J: TNavigateBtn;
begin
    for J:=Low(Buttons) to High(Buttons) do begin
       Buttons[J].Font.Assign(Font);
       end;
end;

procedure TDBNavigator97.SetLayout (Value: TButtonLayout);
var
        J: TNavigateBtn;

begin
  if FLayout <> Value then begin
     FLayout := Value;

     for J:=Low(Buttons) to High(Buttons) do begin
         Buttons[J].Layout := Value;
     end;
  end;
end;

procedure TDBNavigator97.GetChildren(Proc: TGetChildProc {$IFDEF DBNav97Delphi3}; Root: TComponent {$ENDIF});
begin
end;

procedure TDBNavigator97.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) and Assigned(FDataLink) and     (AComponent=DataSource) then
    DataSource:=nil;
end;

procedure TDBNavigator97.SetVisible(Value: TButtonSet);
var
  I: TNavigateBtn;
  W,H: Integer;
begin
  W:=Width;
  H:=Height;
  FVisibleButtons:=Value;
  for I:=Low(Buttons) to High(Buttons) do
    Buttons[I].Visible:=I in FVisibleButtons;
  SetSize(W,H);
  if (W<>Width) or (H<>Height) then
    inherited SetBounds(Left,Top,W,H);
  Invalidate;
end;

procedure TDBNavigator97.SetSize(var W: Integer; var H: Integer);
var
  Count: Integer;
  MinW: Integer;
  I: TNavigateBtn;
  Space,Temp,Remain: Integer;
  X,XOffset,YOffset: Integer;
begin
  if (csLoading in ComponentState) or (Buttons[nbFirst]=nil) then
    Exit;
  if (csDesigning in ComponentState) then begin
    XOffset:=0;
    YOffset:=0;
  end else begin
    XOffset:=Left;
    YOffset:=Top;
  end;
  Count:=0;
  for I:=Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Inc(Count);
  if Count=0 then
    Inc(Count);
  MinW:=Count*MinBtnSize.X;
  if W<MinW then
    W:=MinW;
  if H<MinBtnSize.Y then
    H:=MinBtnSize.Y;
  ButtonWidth:=W div Count;
  Temp:=Count*ButtonWidth;
  if Align=alNone then
    W:=Temp;
  X:=0;
  Remain:=W-Temp;
  Temp:=Count div 2;
  for I:=Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then begin
      Space:=0;
      if Remain<>0 then begin
        Dec(Temp,Remain);
        if Temp<0 then begin
          Inc(Temp,Count);
          Space:=1;
        end;
      end;
      Buttons[I].SetBounds(XOffset+X,YOffset,ButtonWidth+Space,Height);
      Inc(X,ButtonWidth+Space);
    end else
      Buttons[I].SetBounds (XOffset+Width+1,YOffset,ButtonWidth,Height);
  if not (csDesigning in ComponentState) then
    Visible:=False;
end;

procedure TDBNavigator97.SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
var
  W,H: Integer;
begin
  W:=AWidth;
  H:=AHeight;
  SetSize (W,H);
  inherited SetBounds (ALeft,ATop,W,H);
end;

procedure TDBNavigator97.WMSize(var Message: TWMSize);
var
  W,H: Integer;
begin
  inherited;
  W:=Width;
  H:=Height;
  SetSize(W,H);
  if (W<>Width) or (H<>Height) then
    inherited SetBounds(Left,Top,W,H);
  Message.Result:=0;
end;

procedure TDBNavigator97.ClickHandler(Sender: TObject);
begin
  BtnClick(TNavButton97(Sender).Index);
end;

procedure TDBNavigator97.BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  OldFocus: TNavigateBtn;
begin
  OldFocus:=FocusedButton;
  FocusedButton:=TNavButton97 (Sender).Index;
  if TabStop and (GetFocus<>Handle) and CanFocus then begin
    SetFocus;
    if (GetFocus<>Handle) then
      Exit;
  end else if TabStop and (GetFocus=Handle) and (OldFocus<>FocusedButton) then begin
    Buttons[OldFocus].Invalidate;
    Buttons[FocusedButton].Invalidate;
  end;
end;

procedure TDBNavigator97.BtnClick(Index: TNavigateBtn);
begin
  if (DataSource<>nil) and (DataSource.State<>dsInactive) then
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
      case Index of
      nbPrior: Prior;
      nbNext: Next;
      nbFirst: First;
      nbLast: Last;
      nbInsert: Insert;
      nbEdit: Edit;
      nbCancel: Cancel;
      nbPost: Post;
      nbRefresh: Refresh;
      nbDelete:
        if (not FConfirmDelete) or (MessageDlg({$IFNDEF DBNav97Delphi3}LoadStr{$ENDIF}(SDeleteRecordQuestion),mtConfirmation,mbOKCancel,0)<>idCancel) then
          Delete;
      end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self,Index);
end;

procedure TDBNavigator97.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDBNavigator97.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDBNavigator97.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TNavigateBtn;
  OldFocus: TNavigateBtn;
begin
  OldFocus:=FocusedButton;
  case Key of
  VK_RIGHT: begin
      NewFocus:=FocusedButton;
      repeat
        if NewFocus<High(Buttons) then
          NewFocus:=Succ(NewFocus);
      until (NewFocus=High(Buttons)) or (Buttons[NewFocus].Visible);
      if NewFocus<>FocusedButton then begin
        FocusedButton:=NewFocus;
        Buttons[OldFocus].Invalidate;
        Buttons[FocusedButton].Invalidate;
      end;
    end;
  VK_LEFT: begin
      NewFocus:=FocusedButton;
      repeat
        if NewFocus>Low(Buttons) then
          NewFocus:=Pred(NewFocus);
      until (NewFocus=Low(Buttons)) or (Buttons[NewFocus].Visible);
      if NewFocus<>FocusedButton then begin
        FocusedButton:=NewFocus;
        Buttons[OldFocus].Invalidate;
        Buttons[FocusedButton].Invalidate;
      end;
    end;
  VK_SPACE:
    if Buttons[FocusedButton].Enabled then
      Buttons[FocusedButton].Click;
  end;
end;

procedure TDBNavigator97.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result:=DLGC_WANTARROWS;
end;

procedure TDBNavigator97.DataChanged;
var
  Editing,CanModify,UpEnable,DnEnable: Boolean;
begin
  {Check dataset state}
  UpEnable:=Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable:=Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  CanModify:=Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Editing:=FDataLink.Editing;
  {Change "current record" operations}
  if not Editing then
  begin
    Buttons[nbFirst].Enabled:=UpEnable and (nbFirst in EnabledButtons);
    Buttons[nbPrior].Enabled:=UpEnable and (nbPrior in EnabledButtons);
    Buttons[nbNext].Enabled:=DnEnable and (nbNext in EnabledButtons);
    Buttons[nbLast].Enabled:=DnEnable and (nbLast in EnabledButtons);
    Buttons[nbDelete].Enabled:= (CanModify) and (not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF))
    and (nbDelete in EnabledButtons);
  end else
  begin
    {Force the user to choose Cancel or Post}
    Buttons[nbFirst].Enabled:= False;
    Buttons[nbPrior].Enabled:= False;
    Buttons[nbNext].Enabled:= False;
    Buttons[nbLast].Enabled:= False;
    Buttons[nbDelete].Enabled:= False;
  end;
  {Prevent from inserting,editing once again}
  Buttons[nbInsert].Enabled:= (CanModify) and (not Editing) and (nbInsert in EnabledButtons);
  Buttons[nbEdit].Enabled:= (CanModify) and (not Editing) and (nbEdit in EnabledButtons);
  Buttons[nbRefresh].Enabled:= not (FDataLink.DataSet is TQuery) and (not Editing) and (nbRefresh in EnabledButtons);
  {Cancel and Post odd to Insert, Edit and Refresh}
  Buttons[nbPost].Enabled:= (CanModify) and (Editing) and (nbPost in EnabledButtons);
  Buttons[nbCancel].Enabled:= (CanModify) and (Editing) and (nbCancel in EnabledButtons);
end;

procedure TDBNavigator97.EditingChanged;
var
  Editing,CanModify: Boolean;
begin
  CanModify:=Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Editing:=FDataLink.Editing;
  {Force the user to choose Cancel or Post}
  Buttons[nbFirst].Enabled:= False;
  Buttons[nbPrior].Enabled:= False;
  Buttons[nbNext].Enabled:= False;
  Buttons[nbLast].Enabled:= False;
  Buttons[nbDelete].Enabled:= False;
  {The other buttons}
  Buttons[nbInsert].Enabled:=CanModify and (not Editing) and (nbInsert in EnabledButtons);
  Buttons[nbEdit].Enabled:=CanModify and (not Editing) and (nbEdit in EnabledButtons);
  Buttons[nbPost].Enabled:=CanModify and (Editing) and (nbPost in EnabledButtons);
  Buttons[nbCancel].Enabled:=CanModify and (Editing) and (nbCancel in EnabledButtons);
  Buttons[nbRefresh].Enabled:=not (FDataLink.DataSet is TQuery) and (not Editing) and (nbRefresh in EnabledButtons);
end;

procedure TDBNavigator97.ActiveChanged;
var
  I: TNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
    for I:=Low(Buttons) to High(Buttons) do
      Buttons[I].Enabled:=False
  else begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TDBNavigator97.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TDBNavigator97.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource:=Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if Value<>nil then
    Value.FreeNotification(Self);
end;

function TDBNavigator97.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

procedure TDBNavigator97.Loaded;
var
  W,H: Integer;
begin
  inherited;
  W:=Width;
  H:=Height;
  SetSize (W,H);
  if (W<>Width) or (H<>Height) then
    inherited SetBounds (Left,Top,W,H);
  InitHints;
  InitCaptions;
  InitFont;
  ActiveChanged;
end;

{TNavButton97}

procedure TNavButton97.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if (GetFocus=Parent.Handle) and (FIndex=TDBNavigator97(Owner).FocusedButton) then       begin
    R:=Bounds(0,0,Width,Height);
    InflateRect(R,-3,-3);
    if FState=bsDown then
      OffsetRect(R,1,1);
    DrawFocusRect(Canvas.Handle,R);
  end;
end;

{TNavDataLink97}

constructor TNavDataLink97.Create(ANav: TDBNavigator97);
begin
  inherited Create;
  FNavigator:=ANav;
end;

destructor TNavDataLink97.Destroy;
begin
  FNavigator:=nil;
  inherited Destroy;
end;

procedure TNavDataLink97.EditingChanged;
begin
  if FNavigator<>nil then
    FNavigator.EditingChanged;
end;

procedure TNavDataLink97.DataSetChanged;
begin
  if FNavigator<>nil then
    FNavigator.DataChanged;
end;

procedure TNavDataLink97.ActiveChanged;
begin
  if FNavigator<>nil then
    FNavigator.ActiveChanged;
end;

procedure Register;
begin
  RegisterComponents('Toolbar97',[TDBNavigator97]);
end;

end.
