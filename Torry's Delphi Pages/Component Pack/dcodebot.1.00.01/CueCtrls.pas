unit CueCtrls;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Windows, Messages, GraphTools,
  CtrlKit, TypInfo;

type
  ECueAssociateError = class(Exception);

  TCueState = (csDefault, csRequired, csError);

  TCueControl = class(TGraphicControl)
  private
    FAssociate: TWinControl;
    FState: TCueState;
    procedure SetAssociate(Value: TWinControl);
    procedure SetState(Value: TCueState);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateBounds;
    property Associate: TWinControl read FAssociate write SetAssociate;
    property State: TCueState read FState write SetState;
  end;

procedure AssociateCueControl(Control: TWinControl; State: TCueState);
function FindCueControl(Control: TWinControl): TCueControl;
procedure UpdateCueControls(Owner: TComponent);

implementation

uses
  StrConst;

procedure AssociateCueControl(Control: TWinControl; State: TCueState);
var
  Cue: TCueControl;
begin
  Cue := FindCueControl(Control);
  if Cue = nil then
  begin
    Cue := TCueControl.Create(Control.Owner);
    Cue.Associate := Control;
    Cue.State := State;
  end
  else
    Cue.State := State;
end;

procedure UpdateCueControls(Owner: TComponent);
var
  Cue: TCueControl;
begin
  if Owner <> nil then
  begin
    ForEach(Owner, TCueControl);
    while NextEach(Cue) do
      Cue.UpdateBounds;
  end;
end;

{ CueControl linked list }

type
  PCueLink = ^TCueLink;
  TCueLink = record
    Control: TWinControl;
    Cue: TCueControl;
    Next: PCueLink;
  end;

var
  Links: PCueLink;

function FindCueControl(Control: TWinControl): TCueControl;
var
  Link: PCueLink;
begin
  Result := nil;
  Link := Links;
  while Link <> nil do
    if Link.Control = Control then
    begin
      Result := Link.Cue;
      Break;
    end
    else
      Link := Link.Next;
end;

procedure AddCueControl(Control: TWinControl; Cue: TCueControl);
var
  Root: PCueLink;
  Link: PCueLink;
begin
  if Control = nil then Exit;
  if FindCueControl(Control) <> nil then
    raise ECueAssociateError.Create(SInvalidPropertyValue);
  New(Link);
  Link.Control := Control;
  Link.Cue := Cue;
  Link.Next := nil;
  Root := Links;
  if Root <> nil then
  begin
    while Root.Next <> nil do
      Root := Root.Next;
    Root.Next := Link;
  end
  else
    Links := Link;
end;

procedure RemoveCueControl(Control: TWinControl);
var
  Parent: PCueLink;
  Link: PCueLink;
begin
  Parent := nil;
  Link := Links;
  while Link <> nil do
    if Link.Control = Control then
    begin
      if Parent <> nil then
        Parent.Next := Link.Next
      else
        Links := Link.Next;
      Dispose(Link);
      Break;
    end
    else
    begin
      Parent := Link;
      Link := Link.Next;
    end;
end;

{ TCueControl }

constructor TCueControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Brush.Color := clBtnFace;
end;

destructor TCueControl.Destroy;
begin
  SetAssociate(nil);
  inherited Destroy;
end;

procedure TCueControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAssociate) then
    SetAssociate(nil);
end;

procedure TCueControl.Paint;
var
  Rect: TRect;
  DC: HDC;
begin
  if FAssociate = nil then Exit;
  Rect := ClientRect;
  DC := Canvas.Handle;
  case State of
    csRequired:
      begin
        DrawBox(DC, clBtnShadow, Rect);
        DrawBox(DC, clBtnShadow, Rect);
      end;
    csError:
      begin
        DrawBox(DC, clRed, Rect);
        DrawBox(DC, clRed, Rect);
      end;
  end;
  Canvas.FillRect(Rect);
end;

procedure TCueControl.UpdateBounds;
var
  Rect: TRect;
begin
  if FAssociate <> nil then
  begin
    Rect := FAssociate.BoundsRect;
    InflateRect(Rect, 4, 4);
    BoundsRect := Rect;
  end
  else
    Visible := False;
end;

procedure TCueControl.SetAssociate(Value: TWinControl);
begin
  if Value <> FAssociate then
  begin
    if FAssociate <> nil then
    begin
      FAssociate.RemoveFreeNotification(Self);
      RemoveCueControl(FAssociate);
    end;
    Visible := False;
    FAssociate := Value;
    if FAssociate <> nil then
    begin
      FAssociate.FreeNotification(Self);
      AddCueControl(FAssociate, Self);
      Parent := FAssociate.Parent;
      Canvas.Brush.Color := TForm(Parent).Color;
      UpdateBounds;
      Visible := True;
    end;
  end;
end;

procedure TCueControl.SetState(Value: TCueState);
begin
  if Value <> FState then
  begin
    FState := Value;
    Invalidate;
  end;
end;

end.
