unit Glassbut;

{ v1.0 - released to CIS's delphi forum }

{ Due to some rather strange designer 'features', transparent wincontrols
  cannot cleanly be dragged around.  It works find at runtime but at design
  time it can get rather trashy looking.

  I have tried using NOCOPYBITS but that did not seem to solve the trashy
  render problem.  Recently I came across the Visible/Invisible trick and
  it seems to work in all cases except the designer who ignores visiblity.
  sigh...  I can't seem to win :-)

  If anyone comes up with a better design time method give me a shout. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Menus, StdCtrls;

type
  TGlassButton = class(TCustomControl)
  private
    { Private declarations }
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

  protected
    { Protected declarations }
    procedure Paint; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    { just in case someone wants to draw on us }
    property Canvas;

  published
    { Published declarations }
    property Enabled;
    property Height default 32;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 32;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Lib', [TGlassButton]);
end;

constructor TGlassButton.Create(AOwner: TComponent);
begin
     { call mom }
     inherited Create(AOwner);

     { initialize and create some of our properties }
     Width := 32; Height := 32;
end;

destructor TGlassButton.Destroy;
begin
     { destroy the properties that we created }
     { nothing in this case }

     { call gozer }
     inherited Destroy;
end;

procedure TGlassButton.CreateParams(var Params: TCreateParams);
begin
     { call the create of the params }
     inherited CreateParams(Params);

     { and then add our twist, transparency }
     Params.ExStyle := Params.ExStyle + WS_EX_Transparent;

end;

procedure TGlassButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
     { let mom handle the hard work }
     inherited;

     { lie to everyone, we have not actually erased anything }
     Message.Result := 1;
end;

{ the secret to moving around is cloaking ourself in invisiblity }
procedure TGlassButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
   WasVisible: boolean;
begin
     { make ourself go away if we are visible }
     WasVisible := Visible;
     if WasVisible then Visible := False;

     { let mom handle the hard work }
     inherited SetBounds(ALeft, ATop, AWidth, AHeight);

     { if we were visible make us that way again }
     if WasVisible then Visible := True;
end;

procedure TGlassButton.Paint;
var
   ARect: TRect;
begin
     { we only paint if we are in design mode, so we can see where the
       transparent glass object is. }
     if (csDesigning in ComponentState) then begin
        ARect := GetClientRect;
        with Canvas do begin
             Pen.Color := clBlack;
             MoveTo(ARect.Left,ARect.Top);
             LineTo(ARect.Right-1,ARect.Top);
             LineTo(ARect.Right-1,ARect.Bottom-1);
             LineTo(ARect.Left,ARect.Bottom-1);
             LineTo(ARect.Left,ARect.Top);
             LineTo(ARect.Right-1,ARect.Bottom-1);
             MoveTo(ARect.Right-1,ARect.Top);
             LineTo(ARect.Left,ARect.Bottom-1);
        end;
     end
end;

end.
