unit o_GTControlArray;

interface
uses
   Classes
  ,Controls
  ,Contnrs
  ;
type
{------------------------------------------------------------------------------}
  TgtControlArrayClickEvent = procedure (Sender : TObject ; ControlIndex : Integer) of Object;
{------------------------------------------------------------------------------}
  TgtControlArray = class(TWinControl)
  private
    FMaxControlsOnARow: Integer;
    FControlHeight: Integer;
    FControlWidth: Integer;
    FControlFontSize: Integer;
    FMaxControls: Integer;
    FControlsCaption: string;
    FControlClass: TControlClass;
    FOnControlClick: TgtControlArrayClickEvent;
    function GetControl(Index: Integer): TControl;
    procedure SetMaxControls(const Value: Integer);
    procedure SetControlClass(const Value: TControlClass);
    { Private declarations }
  protected
    { Protected declarations }
    FControlList : TComponentList;
    procedure InternalOnControlClick(Sender : TObject);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property  ControlClass : TControlClass read FControlClass write SetControlClass;
    procedure CreateControls;
    procedure InvalidateControls;
  public
    property ArrayControl [Index : Integer] : TControl read GetControl;
  published
    { Published declarations}
    property ControlWidth       : Integer                       read FControlWidth         write FControlWidth;
    property ControlHeight      : Integer                       read FControlHeight        write FControlHeight;
    property ControlFontSize    : Integer                       read FControlFontSize      write FControlFontSize;
    property MaxControlsOnARow  : Integer                       read FMaxControlsOnARow    write FMaxControlsOnARow;
    property MaxControls        : Integer                       read FMaxControls          write SetMaxControls;
    property ControlsCaption    : string                        read FControlsCaption      write FControlsCaption;
  published
    property OnControlClick     : TgtControlArrayClickEvent     read FOnControlClick       write FOnControlClick;
  published
    property Align;
  end;
{------------------------------------------------------------------------------}

implementation
uses
  SysUtils
  ;
type
  _TControl = class(TControl);
{ TgtControlArray }
{------------------------------------------------------------------------------}
constructor TgtControlArray.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlClass      := nil;
  FControlWidth      := 120;
  FControlHeight     := 120;
  FMaxControlsOnARow := 4;
  FMaxControls       := 16;
  FControlList       := TComponentList.Create(True);
  Self.Width         := 400;
  Self.Height        := 400;
end;
{------------------------------------------------------------------------------}
destructor TgtControlArray.Destroy;
begin
  FControlList.Clear;
  FreeAndNil(FControlList);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtControlArray.CreateControls;
var
  Ctrl : TControl;
  i    : Integer;
begin
  if Assigned(FControlClass) then
  begin
    FControlList.Clear;
    for i:=0 to Pred(FMaxControls) do
    begin
      Ctrl := FControlClass.Create(Self);
      FControlList.Add(Ctrl);

      Ctrl.Parent := Self;

      Ctrl.Height := FControlHeight;
      Ctrl.Width  := FControlWidth;

      Ctrl.Tag    := i;

      _TControl(Ctrl).OnClick := InternalOnControlClick;

     if i = 0 then
      begin
        Ctrl .Left := 0;
        Ctrl .Top  := 0;
      end
      else
      begin
          Ctrl .Left := TControl(FControlList.Items[i-1]).Left + TControl(FControlList.Items[i-1]) .Width;
          if (i mod MaxControlsOnARow) <> 0 then
          begin
            Ctrl.Top  := TControl(FControlList.Items[i-1]).Top;
          end
          else
          begin
            Ctrl.Left := 0;
            Ctrl.Top  := TControl(FControlList.Items[i-1]).Top + TControl(FControlList.Items[i-1]).Height;
          end;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtControlArray.InvalidateControls;
var
  i : Integer;
begin
  for i:=0 to Pred(FControlList.Count) do
    TControl(FControlList[i]).Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TgtControlArray.InternalOnControlClick(Sender: TObject);
begin
  if Assigned(FOnControlClick) then
    FOnControlClick(Sender,TComponent(Sender).Tag);
end;
{------------------------------------------------------------------------------}




{------------------------------------------------------------------------------}
function TgtControlArray.GetControl(Index: Integer): TControl;
begin
  Result := TControl(FControlList[Index]);
end;
{------------------------------------------------------------------------------}
procedure TgtControlArray.SetMaxControls(const Value: Integer);
begin
  FMaxControls := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtControlArray.SetControlClass(const Value: TControlClass);
begin
  FControlClass := Value;
  if Assigned(FControlClass) then
    CreateControls;
end;
{------------------------------------------------------------------------------}

end.
