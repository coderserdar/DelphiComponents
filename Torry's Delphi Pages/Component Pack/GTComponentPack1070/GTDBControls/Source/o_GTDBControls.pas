unit o_GTDBControls;

interface
uses
   Classes
  ,DB
  ,DBCtrls
  ,StdCtrls
  ,Controls
  ,Messages
  ,ComCtrls
  ,ExtCtrls
  ,Buttons
  ,Gauges
  ;
type
{------------------------------------------------------------------------------}
  TgtBoundLabelPosition = (
                             blpLeft
                            ,blpRight
                            ,blpTopLeft
                            ,blpTopRight
                            ,blpBottomLeft
                            ,blpBottomRight
                           );
{------------------------------------------------------------------------------}
  TgtBoundLabel = class(TLabel)
  private
    FAssosiate     : TWinControl;
    FLabelPosition : TgtBoundLabelPosition;
    procedure SetAssosiate(const Value: TWinControl);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification(AComponent : TComponent ; Operation : TOperation);override;
  protected
    FOldResizeEvent      : TNotifyEvent;
    FOldAssosiateWndProc : TWndMethod;
    procedure RePositionToAssosiate;
    procedure InternalOnAssosiateResize(Sender : TObject);
    procedure AssosiateWindowProc(var Message : TMessage);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Assosiate     : TWinControl            read FAssosiate     write SetAssosiate;
    property LabelPosition : TgtBoundLabelPosition  read FLabelPosition write SetLabelPosition default blpLeft;
  end;
{------------------------------------------------------------------------------}
  TgtDBEdit = class(TDBEdit)
  private
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property BoundLabel   : TgtBoundLabel         read FBoundLabel;
    property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBMemo = class(TDBMemo)
  private
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property BoundLabel : TgtBoundLabel read FBoundLabel;
    property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBImage = class(TDBImage)
  private
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property BoundLabel : TgtBoundLabel read FBoundLabel;
    property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBListBox = class(TDBListBox)
  private
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property BoundLabel : TgtBoundLabel read FBoundLabel;
    property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBComboBox = class(TDBComboBox)
  private
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property BoundLabel : TgtBoundLabel read FBoundLabel;
    property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBLookUpListBox = class(TDBLookUpListBox)
  private
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property BoundLabel : TgtBoundLabel read FBoundLabel;
    property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBLookUpComboBox = class(TDBLookUpComboBox)
  private
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property BoundLabel : TgtBoundLabel read FBoundLabel;
    property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBRichEdit = class(TDBRichEdit)
  private
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property BoundLabel : TgtBoundLabel read FBoundLabel;
    property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBProgressBar = class(TProgressBar)
  private
    { Private declarations }
    FDataLink : TFieldDataLink;
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
    procedure DataChange(Sender : TObject);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Field : TField read GetField;
  published
    { Published declarations}
   property BoundLabel   : TgtBoundLabel         read FBoundLabel;
   property DataSource   : TDataSource           read GetDataSource  write SetDataSource;
   property DataField    : string                read GetDataField   write SetDataField;
   property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
   property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBTrackBar = class(TTrackBar)
  private
    { Private declarations }
    FDataLink : TFieldDataLink;
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
    procedure DataChange(Sender : TObject);
    procedure Changed;override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Field : TField read GetField;
  published
    { Published declarations}
   property BoundLabel   : TgtBoundLabel         read FBoundLabel;
   property DataSource   : TDataSource           read GetDataSource  write SetDataSource;
   property DataField    : string                read GetDataField   write SetDataField;
   property ReadOnly     : Boolean               read GetReadOnly    write SetReadOnly;
   property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
   property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBUpDown = class(TUpDown)
  private
    { Private declarations }
    FDataLink : TFieldDataLink;
    FBoundLabel: TgtBoundLabel;
    FLabelCaption: string;
    FLabelPosition: TgtBoundLabelPosition;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
    procedure DataChange(Sender : TObject);
    procedure WMHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message CN_VSCROLL;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Field : TField read GetField;
  published
    { Published declarations}
   property BoundLabel   : TgtBoundLabel         read FBoundLabel;
   property DataSource   : TDataSource           read GetDataSource  write SetDataSource;
   property DataField    : string                read GetDataField   write SetDataField;
   property ReadOnly     : Boolean               read GetReadOnly    write SetReadOnly;
   property LabelCaption : string                read FLabelCaption  write SetLabelCaption;
   property LabelPosition: TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
  end;
{------------------------------------------------------------------------------}
  TgtDBShape = class(TShape)
  private
    { Private declarations }
    FDataLink : TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
    procedure DataChange(Sender : TObject);
    procedure DblClick;override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Field : TField read GetField;
  published
    { Published declarations}
   property DataSource   : TDataSource           read GetDataSource  write SetDataSource;
   property DataField    : string                read GetDataField   write SetDataField;
   property ReadOnly     : Boolean               read GetReadOnly    write SetReadOnly;
  end;
{------------------------------------------------------------------------------}
  TgtDBGauge = class(TGauge)
  private
    { Private declarations }
    FDataLink : TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
    { Protected declarations }
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
    procedure DataChange(Sender : TObject);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Field : TField read GetField;
  published
    { Published declarations}
   property DataSource   : TDataSource           read GetDataSource  write SetDataSource;
   property DataField    : string                read GetDataField   write SetDataField;
   property ReadOnly     : Boolean               read GetReadOnly    write SetReadOnly;
  end;
{------------------------------------------------------------------------------}
  TgtDBDateTimePicker = class(TEdit)
  private
    { Private declarations }
    FCalendar        : TMonthCalendar;
    FCalendarBtn     : TSpeedButton;
    FDateFormat      : string;
    FDefaultEditMask : string;
    procedure SetDateFormat(const Value: string);
  private
    FDataLink : TFieldDataLink;
    FBoundLabel: TgtBoundLabel;
    FLabelPosition: TgtBoundLabelPosition;
    FLabelCaption: string;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function  GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TgtBoundLabelPosition);
    procedure ActiveChange(Sender: TObject);
  protected
    { Protected declarations }
    procedure InternalCalendarOnClick    (Sender : TObject);
    procedure InternalCalendarBtnOnClick (Sender : TObject);
    procedure DoEnter;override;
    procedure DoExit;override;
    procedure Change;override;
    procedure SetParent(AParent : TWinControl);override;
    procedure SetName(const NewName: TComponentName); override;
    procedure DataChange(Sender : TObject);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Field : TField read GetField;
  published
    { Published declarations}
    property DefaultEditMask : string                read FDefaultEditMask write FDefaultEditMask;
    property DateFormat      : string                read FDateFormat      write SetDateFormat;
    property DataSource      : TDataSource           read GetDataSource    write SetDataSource;
    property DataField       : string                read GetDataField     write SetDataField;
    property ReadOnly        : Boolean               read GetReadOnly      write SetReadOnly;
    property BoundLabel      : TgtBoundLabel         read FBoundLabel;
    property LabelCaption    : string                read FLabelCaption  write SetLabelCaption;
    property LabelPosition   : TgtBoundLabelPosition read FLabelPosition write SetLabelPosition;
{  published
    property EditMask;}
  end;
{------------------------------------------------------------------------------}


implementation

uses
   SysUtils
  ,Dialogs
  ;

type
  _TWinControl = class(TWinControl);

{ TgtBoundLabel }
{------------------------------------------------------------------------------}
constructor TgtBoundLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := blpLeft;
end;
{------------------------------------------------------------------------------}
destructor TgtBoundLabel.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtBoundLabel.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FAssosiate then
      Assosiate := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtBoundLabel.InternalOnAssosiateResize(Sender: TObject);
begin
  if Assigned(FOldResizeEvent) then
    FOldResizeEvent(Assosiate);
  RePositionToAssosiate;
end;
{------------------------------------------------------------------------------}
procedure TgtBoundLabel.AssosiateWindowProc(var Message: TMessage);
begin
 FOldAssosiateWndProc(Message);
  case Message.Msg of
    WM_MOVE : RePositionToAssosiate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtBoundLabel.RePositionToAssosiate;
begin
 case FLabelPosition of
    blpLeft       :
      begin
        Top  := Assosiate.Top + 3;
        Left := Assosiate.Left - Width - 5;
      end;
    blpRight      :
      begin
        Top  := Assosiate.Top  + 3;
        Left := Assosiate.Left + Assosiate.Width + 5;
      end;
    blpTopLeft    :
      begin
        Top  := Assosiate.Top - Height - 3;
        Left := Assosiate.Left;
      end;
    blpTopRight   :
      begin
        Top  := Assosiate.Top  - Height - 3;
        Left := Assosiate.Left + Assosiate.Width - Width;
      end;
    blpBottomLeft :
      begin
        Top  := Assosiate.Top + Assosiate.Height;
        Left := Assosiate.Left;
      end;
    blpBottomRight:
      begin
        Top  := Assosiate.Top  + Assosiate.Height;
        Left := Assosiate.Left + Assosiate.Width - Width;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtBoundLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  RePositionToAssosiate;
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtBoundLabel.SetAssosiate(const Value: TWinControl);
begin
  if Assigned(FAssosiate) then
  begin
    FAssosiate.RemoveFreeNotification(Self);
    _TWinControl(FAssosiate).OnResize := FOldResizeEvent;
    FAssosiate.WindowProc             := FOldAssosiateWndProc;
  end;

  FAssosiate := Value;

  if Assigned(FAssosiate) then
  begin
    FAssosiate.FreeNotification(Self);
    FOldResizeEvent := _TWinControl(FAssosiate).OnResize;
    _TWinControl(FAssosiate).OnResize := InternalOnAssosiateResize;
    FOldAssosiateWndProc  := FAssosiate.WindowProc;
    FAssosiate.WindowProc := AssosiateWindowProc;
    RePositionToAssosiate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtBoundLabel.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  if FLabelPosition <> Value then
  begin
    FLabelPosition := Value;
    RePositionToAssosiate;
  end;
end;
{------------------------------------------------------------------------------}



{ TgtDBEdit }
{------------------------------------------------------------------------------}
constructor TgtDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel           := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBEdit.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}
procedure TgtDBEdit.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBEdit.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}


{ TgtDBMemo }
{------------------------------------------------------------------------------}
constructor TgtDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel           := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBMemo.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBMemo.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBMemo.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}
procedure TgtDBMemo.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBMemo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}

{ TgtDBImage }

{------------------------------------------------------------------------------}
constructor TgtDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel           := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBImage.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBImage.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBImage.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}
procedure TgtDBImage.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBImage.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}
{ TgtDBListBox }

{------------------------------------------------------------------------------}
constructor TgtDBListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel           := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBListBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBListBox.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBListBox.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}
procedure TgtDBListBox.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBListBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}

{ TgtDBComboBox }

{------------------------------------------------------------------------------}
constructor TgtDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel           := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBComboBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBComboBox.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBComboBox.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}
procedure TgtDBComboBox.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}

{ TgtDBLookUpListBox }

{------------------------------------------------------------------------------}
constructor TgtDBLookUpListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel           := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBLookUpListBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpListBox.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpListBox.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpListBox.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpListBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}

{ TgtDBLookUpComboBox }

{------------------------------------------------------------------------------}
constructor TgtDBLookUpComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel           := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBLookUpComboBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpComboBox.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpComboBox.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpComboBox.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBLookUpComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}

{ TgtDBRichEdit }

{------------------------------------------------------------------------------}
constructor TgtDBRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel           := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBRichEdit.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBRichEdit.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBRichEdit.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}
procedure TgtDBRichEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBRichEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}




{ TgtDBProgressBar }

{------------------------------------------------------------------------------}
constructor TgtDBProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel            := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate  := Self;
  FDataLink              := TFieldDataLink.Create;
  FDataLink.Control      := Self;
  FDataLink.OnDataChange := DataChange;
  Max                    := 100;
  Smooth                 := True;
end;
{------------------------------------------------------------------------------}
destructor TgtDBProgressBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBProgressBar.DataChange(Sender: TObject);
begin
  Enabled := Assigned(FDataLink.Field) and  (not FDataLink.Field.IsNull);
  if Enabled then
    Position := FDataLink.Field.AsInteger;
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtDBProgressBar.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBProgressBar.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}
function TgtDBProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{------------------------------------------------------------------------------}
function TgtDBProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{------------------------------------------------------------------------------}
function TgtDBProgressBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{------------------------------------------------------------------------------}
procedure TgtDBProgressBar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBProgressBar.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBProgressBar.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBProgressBar.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}


{ TgtDBTrackBar }

{------------------------------------------------------------------------------}
constructor TgtDBTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel            := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate  := Self;
  FDataLink              := TFieldDataLink.Create;
  FDataLink.Control      := Self;
  FDataLink.OnDataChange := DataChange;
  Max                    := 100;
end;
{------------------------------------------------------------------------------}
destructor TgtDBTrackBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.DataChange(Sender: TObject);
begin
  Enabled := Assigned(FDataLink.Field) and  (not FDataLink.Field.IsNull);
  if Enabled then
    Position := FDataLink.Field.AsInteger;
end;
{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.Changed;
begin
  if not ReadOnly then
  begin
    inherited Changed;
    case FDataLink.DataSet.State of
      dsBrowse :
        begin
          if Position <> FDataLink.Field.AsInteger then
            Position := FDataLink.Field.AsInteger;
        end;
      dsInsert,dsEdit :
        begin
          FDataLink.Field.AsInteger := Position;
        end;
    end;
  end;
end;
{------------------------------------------------------------------------------}




{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}
function TgtDBTrackBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{------------------------------------------------------------------------------}
function TgtDBTrackBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{------------------------------------------------------------------------------}
function TgtDBTrackBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;
{------------------------------------------------------------------------------}
function TgtDBTrackBar.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;
{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBTrackBar.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}






{ TgtDBUpDown }

{------------------------------------------------------------------------------}
constructor TgtDBUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel            := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate  := Self;
  FDataLink              := TFieldDataLink.Create;
  FDataLink.Control      := Self;
  FDataLink.OnDataChange := DataChange;
  Max                    := 100;
end;
{------------------------------------------------------------------------------}
destructor TgtDBUpDown.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.DataChange(Sender: TObject);
begin
  Enabled := Assigned(FDataLink.Field) and  (not FDataLink.Field.IsNull);
  if Enabled then
    Position := FDataLink.Field.AsInteger;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.WMHScroll(var Message: TWMHScroll);
begin
  if not ReadOnly then
  begin
    inherited;
    case FDataLink.DataSet.State of
      dsBrowse :
        begin
          if Position <> FDataLink.Field.AsInteger then
            Position := FDataLink.Field.AsInteger;
        end;
      dsInsert,dsEdit :
        begin
          FDataLink.Field.AsInteger := Position;
        end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.WMVScroll(var Message: TWMVScroll);
begin
  if not ReadOnly then
  begin
    inherited;
    case FDataLink.DataSet.State of
      dsBrowse :
        begin
          if Position <> FDataLink.Field.AsInteger then
            Position := FDataLink.Field.AsInteger;
        end;
      dsInsert,dsEdit :
        begin
          FDataLink.Field.AsInteger := Position;
        end;
    end;
  end;
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtDBUpDown.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}
function TgtDBUpDown.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{------------------------------------------------------------------------------}
function TgtDBUpDown.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{------------------------------------------------------------------------------}
function TgtDBUpDown.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;
{------------------------------------------------------------------------------}
function TgtDBUpDown.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBUpDown.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}




{ TgtDBShape }

{------------------------------------------------------------------------------}
constructor TgtDBShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink              := TFieldDataLink.Create;
  FDataLink.Control      := Self;
  FDataLink.OnDataChange := DataChange;
end;
{------------------------------------------------------------------------------}
destructor TgtDBShape.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBShape.DataChange(Sender: TObject);
begin
  Enabled := Assigned(FDataLink.Field) and  (not FDataLink.Field.IsNull);
  if Enabled then
    Brush.Color := FDataLink.Field.AsInteger;
end;
{------------------------------------------------------------------------------}
procedure TgtDBShape.DblClick;
var
  ColorDlg : TColorDialog;
begin
  inherited DblClick;
  if not ReadOnly then
  begin
    if FDataLink.DataSet.State in [dsInsert,dsEdit] then
    begin
      ColorDlg := TColorDialog.Create(nil);
      try
        if ColorDlg.Execute then
          FDataLink.Field.AsInteger := ColorDlg.Color;
      finally
        ColorDlg.Free;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtDBShape.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
end;
{------------------------------------------------------------------------------}
procedure TgtDBShape.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
end;
{------------------------------------------------------------------------------}
function TgtDBShape.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{------------------------------------------------------------------------------}
function TgtDBShape.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{------------------------------------------------------------------------------}
function TgtDBShape.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{------------------------------------------------------------------------------}
procedure TgtDBShape.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBShape.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;
{------------------------------------------------------------------------------}
function TgtDBShape.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;
{------------------------------------------------------------------------------}
procedure TgtDBShape.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;
{------------------------------------------------------------------------------}




{ TgtDBDateTimePicker }

{------------------------------------------------------------------------------}
constructor TgtDBDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCalendarBtn             := TSpeedButton.Create(Self);
  FCalendarBtn.Caption     := '...';
  FCalendarBtn.Parent      := Self;
  FCalendarBtn.Align       := alRight;
  FCalendarBtn.OnClick     := InternalCalendarBtnOnClick;
  FDefaultEditMask         := '!99/99/99;1;_';
  FBoundLabel              := TgtBoundLabel.Create(Self);
  FBoundLabel.Assosiate    := Self;
  FDataLink                := TFieldDataLink.Create;
  FDataLink.Control        := Self;
  FDataLink.OnDataChange   := DataChange;
  FDataLink.OnActiveChange := ActiveChange;
  Width                    := Width + 25;
end;
{------------------------------------------------------------------------------}
destructor TgtDBDateTimePicker.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.DataChange(Sender: TObject);
begin
  if FDataLink.Active then
    Self.Text := Field.AsString;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.ActiveChange(Sender : TObject);
begin
  if FDataLink.Active then
    if Assigned(Self.Field) then
      Self.Field.EditMask := Self.DefaultEditMask;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.Change;
begin
  inherited;
  FCalendarBtn.Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.DoEnter;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.DoExit;
begin
  inherited;
  if Assigned(FCalendar) then
    FreeAndNil(FCalendar);
end;
{------------------------------------------------------------------------------}
function TgtDBDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{------------------------------------------------------------------------------}
function TgtDBDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{------------------------------------------------------------------------------}
function TgtDBDateTimePicker.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{------------------------------------------------------------------------------}
function TgtDBDateTimePicker.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.InternalCalendarBtnOnClick(Sender: TObject);
begin
  if Assigned(FCalendar) then
    FreeAndNil(FCalendar);
  FCalendar            := TMonthCalendar.Create(Self);
  FCalendar.Visible    := False;
  FCalendar.OnClick    := InternalCalendarOnClick;
  FCalendar.Left       := Self.Left;
  FCalendar.Top        := Self.Top + Self.Height;
  FCalendar.Parent     := Self.Parent;
  FCalendar.Visible    := True;
  Self.SetFocus;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.InternalCalendarOnClick(Sender: TObject);
begin
  if Assigned(Self.Field) then
    if Self.Field.DataSet.State in [dsInsert,dsEdit] then
      Self.Field.Value := FCalendar.Date;
  FCalendar.Visible := True;
  FCalendar.Parent  := nil;
  Self.SetFocus;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
  if Assigned(Field) then
    if Field is TDateTimeField then
      TDateTimeField(Field).DisplayFormat := FDateFormat;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  FBoundLabel.Caption := FLabelCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.SetLabelPosition(const Value: TgtBoundLabelPosition);
begin
  FLabelPosition := Value;
  FBoundLabel.LabelPosition := FLabelPosition;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.SetName(const NewName: TComponentName);
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    FBoundLabel.Caption := Self.Name;
    FLabelCaption       := FBoundLabel.Caption;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.SetParent(AParent: TWinControl);
begin
  inherited;
  FBoundLabel.Parent := Self.Parent;
end;
{------------------------------------------------------------------------------}
procedure TgtDBDateTimePicker.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;
{------------------------------------------------------------------------------}












{ TgtDBGauge }

{------------------------------------------------------------------------------}
constructor TgtDBGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink              := TFieldDataLink.Create;
  FDataLink.Control      := Self;
  FDataLink.OnDataChange := DataChange;
end;
{------------------------------------------------------------------------------}
destructor TgtDBGauge.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGauge.DataChange(Sender: TObject);
begin
  Enabled := Assigned(FDataLink.Field) and  (not FDataLink.Field.IsNull);
  if Enabled then
    Progress := FDataLink.Field.AsInteger;
end;
{------------------------------------------------------------------------------}




{------------------------------------------------------------------------------}
procedure TgtDBGauge.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGauge.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
end;
{------------------------------------------------------------------------------}
function TgtDBGauge.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{------------------------------------------------------------------------------}
function TgtDBGauge.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{------------------------------------------------------------------------------}
function TgtDBGauge.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGauge.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGauge.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;
{------------------------------------------------------------------------------}
function TgtDBGauge.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGauge.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;
{------------------------------------------------------------------------------}

end.
