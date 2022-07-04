
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit MemoCtrls;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, GraphTools, StdCtrls,
  Forms, ExtCtrls, ComCtrls;

type
  TCommentsBox = class;
  TComment = class;

{ TCommentControlGroup }

  TCommentControlGroup = class(TComponent)
  private
    FBevel: TBevel;
    FCaption: TLabel;
    FMemo: TMemo;
    FComment: TComment;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Caption: TLabel read FCaption;
    property Bevel: TBevel read FBevel;
    property Memo: TMemo read FMemo;
  end;

{ TComment }

  TComment = class(TCollectionItem)
  private
    FData: Pointer;
    FUserName: string;
    FTimeStamp: TDateTime;
    FControlGroup: TCommentControlGroup;
    FIdent: Integer;
    procedure LinesChange(Sender: TObject);
    procedure UpdateCaption;
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    procedure SetTimeStamp(const Value: TDateTime);
    procedure SetUserName(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ControlGroup: TCommentControlGroup read FControlGroup;
    property Ident: Integer read FIdent write FIdent;
    property Data: Pointer read FData write FData;
  published
    property UserName: string read FUserName write SetUserName;
    property TimeStamp: TDateTime read FTimeStamp write SetTimeStamp;
    property Lines: TStrings read GetLines write SetLines;
  end;

{ TComments }

  TComments = class(TCollection)
  private
    FControl: TCommentsBox;
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
    function Get(Index: Integer): TComment;
    procedure Put(Index: Integer; Value: TComment);
    property Control: TCommentsBox read FControl;
  public
    constructor Create(Control: TCommentsBox);
    function Add: TComment;
    function Insert(Index: Integer): TComment;
    property Items[Index: Integer]: TComment read Get write Put; default;
  end;

{ TCommentsBox }

  TCommentsBox = class(TScrollingWinControl)
  private
    FBorderStyle: TBorderStyle;
    FComments: TComments;
    FLabel: TLabel;
    FMemo: TMemo;
    FUserName: string;
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Resize; override;
    procedure UpdateControls; virtual;
    property Memo: TMemo read FMemo;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddComment; dynamic;
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Comments: TComments read FComments;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    property Lines: TStrings read GetLines write SetLines;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UserName: string read FUserName write FUserName;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TCommentControlGroup }

constructor TCommentControlGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBevel := TBevel.Create(Self);
  FBevel.Anchors := [akLeft, akTop, akRight];
  FCaption := TLabel.Create(Self);
  FCaption.AutoSize := False;
  FCaption.Anchors := [akLeft, akTop, akRight];
  FCaption.Font.Style := [fsBold];
  FMemo := TMemo.Create(Self);
  FMemo.Color := clBtnFace;
  FMemo.BorderStyle := bsNone;
  FMemo.Anchors := [akLeft, akTop, akRight];
  FMemo.ReadOnly := True;
end;

destructor TCommentControlGroup.Destroy;
begin
  FComment.FControlGroup := nil;
  inherited Destroy;
end;

{ TComment }

constructor TComment.Create(Collection: TCollection);
var
  CommentsBox: TCommentsBox;
begin
  FTimeStamp := Now;
  FUserName := ClassName;
  if TComments(Collection).Control <> nil then
  begin
    CommentsBox := TComments(Collection).Control;
    FControlGroup := TCommentControlGroup.Create(CommentsBox);
    with FControlGroup do
    begin
      FComment := Self;
      Memo.Parent := CommentsBox;
      Memo.OnChange := LinesChange;
      Caption.Parent := CommentsBox;
      Bevel.Parent := CommentsBox;
    end;
    UpdateCaption;
  end;
  inherited Create(Collection);
end;

destructor TComment.Destroy;
begin
  FControlGroup.Free;
  inherited Destroy;
end;

procedure TComment.Assign(Source: TPersistent);
var
  Comment: TComment absolute Source;
begin
  if Source is TComment then
  begin
    UserName := Comment.UserName;
    TimeStamp := Comment.TimeStamp;
    Lines := Comment.Lines;
  end
  else
    inherited Assign(Source);
end;

procedure TComment.LinesChange(Sender: TObject);
begin
  Changed(False);
end;

procedure TComment.UpdateCaption;
begin
  FControlGroup.Caption.Caption := Format('%s %s', [FUserName,
    FormatDateTime('mm/dd hh:mm am/pm', FTimeStamp)]);
end;

function TComment.GetLines: TStrings;
begin
  Result := FControlGroup.Memo.Lines;
end;

procedure TComment.SetLines(const Value: TStrings);
begin
  FControlGroup.Memo.Text := Value.Text;
end;

procedure TComment.SetTimeStamp(const Value: TDateTime);
begin
  FTimeStamp := Value;
  UpdateCaption;
end;

procedure TComment.SetUserName(const Value: string);
begin
  FUserName := Value;
  UpdateCaption;
end;

{ TComments }

constructor TComments.Create(Control: TCommentsBox);
begin
  inherited Create(TComment);
  FControl := Control;
end;

function TComments.Add: TComment;
begin
  Result := TComment(inherited Add);
end;

function TComments.Insert(Index: Integer): TComment;
begin
  Result := TComment(inherited Insert(Index));
end;

procedure TComments.Update(Item: TCollectionItem);
begin
  if (FControl <> nil) and FControl.HandleAllocated then
    FControl.UpdateControls;
end;

function TComments.GetOwner: TPersistent;
begin
  Result := FControl;
end;

function TComments.Get(Index: Integer): TComment;
begin
  Result := TComment(GetItem(Index));
end;

procedure TComments.Put(Index: Integer; Value: TComment);
begin
  SetItem(Index, Value);
end;

{ TCommentsBox }

constructor TCommentsBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  VertScrollBar.Tracking := True;
  FBorderStyle := bsSingle;
  FComments := TComments.Create(Self);
  FLabel := TLabel.Create(Self);
  with FLabel do
  begin
    Parent := Self;
    AutoSize := False;
    Top := 8;
    Left := 10;
    Width := 176;
    Caption := 'Comments:';
    Anchors := [akLeft, akTop, akRight];
  end;
  FMemo := TMemo.Create(Self);
  with FMemo do
  begin
    Parent := Self;
    Top := 25;
    Left := 10;
    Width := 176;
    Anchors := [akLeft, akTop, akRight];
  end;
end;

procedure TCommentsBox.AddComment;
var
  Comment: TComment;
begin
  with Comments do
  begin
    BeginUpdate;
    try
      if Count = 0 then
        Comment := Add
      else
        Comment := Insert(0);
      Comment.Lines := Lines;
      Comment.TimeStamp := Now;
      Comment.UserName := UserName;
      Lines.Clear;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCommentsBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TCommentsBox.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCommentsBox.Resize;
begin
  UpdateControls;
end;

procedure TCommentsBox.UpdateControls;
var
  Rect: TRect;
  DC: HDC;
  PriorFont: HFONT;
  Comment: TComment;
  ControlGroup: TCommentControlGroup;
  S: string;
  I: Integer;
begin
  Rect := FMemo.BoundsRect;
  DC := GetDC(Handle);
  PriorFont := SelectObject(DC, Font.Handle);
  InflateRect(Rect, 10, 10);
  Slide(Rect, drDown, 16);
  for I := 0 to FComments.Count - 1 do
  begin
    Comment := FComments[I];
    ControlGroup := Comment.ControlGroup;
    InflateRect(Rect, -10, -10);
    with ControlGroup.Bevel do
    begin
      Left := Rect.Left;
      Top := Rect.Top - 22;
      Width := WidthOf(Rect);
      Height := 2;
    end;
    with ControlGroup.Caption do
    begin
      Left := Rect.Left;
      Top := Rect.Top - 16;
      Width := WidthOf(Rect);
      Height := 22;
    end;
    Inc(Rect.Left, 12);
    S := ControlGroup.Memo.Text;
    if Trim(S) = '' then S := 'A';
    Rect.Bottom :=  Rect.Top + CalculateMemoHeight(DC, S, WidthOf(Rect)) + 8;
    ControlGroup.Memo.BoundsRect := Rect;
    Dec(Rect.Left, 12);
    InflateRect(Rect, 10, 10);
    Slide(Rect, drDown, 8);
  end;
  SelectObject(DC, PriorFont);
  ReleaseDC(Handle, DC);
end;

function TCommentsBox.GetLines: TStrings;
begin
  Result := FMemo.Lines;
end;

procedure TCommentsBox.SetLines(const Value: TStrings);
begin
  FMemo.Lines := Value;
end;

procedure TCommentsBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

initialization
  RegisterClasses([TComments, TComment]);
end.
