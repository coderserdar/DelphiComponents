unit RepeatButton;

{ A button inherited from SpeedButton which provides all the
  standard features of SpeedButton, but adds the following:
  1. a "repeat" function to the OnClick event
  2. OnMouseEnter event
  3. OnMouseLeave event }

{ Author:  Howard Harvey
  Email:   hharvey@dove.net.au
  Date:    23/FEBRUARY/2000
  Version: 1.10
  Release: Freeware if not used in a shareware/commercial product }

interface

uses Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms,
     StdCtrls, ExtCtrls, Mask, Buttons, ComCtrls;

type
  ThhRepeatButton = class(TSpeedButton)
  private
    fRepeatEnable: boolean;
    fRepeatDelay: longint;
    fRepeatInterval: longint;
    fRepeatTimer: TTimer;
    fCount : word ;
    procedure TimerExpired(Sender: TObject);
  protected
    fOnMouseEnter : TNotifyEvent;
    fOnMouseLeave  : TNotifyEvent;
    procedure CMMouseEnter( var message:TMessage);message CM_MOUSEENTER;
    procedure CMMouseLeave( var message:TMessage);message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure SetRepeatDelay( V:longint ) ;
    procedure SetRepeatInterval( V:longint ) ;
    procedure SetRepeatTimer;
    procedure ResetRepeatTimer;
  public
    constructor Create( AOwner:TComponent); override ;
    destructor Destroy; override;
    procedure Click; override;
  published
    property AllowAllUp;
    property GroupIndex;
    property Down;
    property Caption;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property Layout;
    property Margin;
    property NumGlyphs;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Spacing;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Count: word read fCount default 0 ;
    property RepeatEnable: boolean read fRepeatEnable write FRepeatEnable default false;
    property RepeatDelay: longint read fRepeatDelay write SetRepeatDelay default 400;
    property RepeatInterval: longint read fRepeatInterval write SetRepeatInterval default 200;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
  end;

  procedure Register ;

implementation

{ -------------------------------------------------------------------- }

constructor ThhRepeatButton.Create(AOwner:Tcomponent);
begin
  inherited Create(AOwner);
  ParentFont      := True;
  fRepeatEnable   := false;
  fRepeatDelay    := 400;
  fRepeatInterval := 200;
  fRepeatTimer    := nil;
  fCount          := 0 ;
end;

{ -------------------------------------------------------------------- }

destructor ThhRepeatButton.Destroy;
begin
  if Assigned(fRepeatTimer) then fRepeatTimer.Free;
  inherited Destroy;
end;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.SetRepeatTimer;
{ Creates the timer if necessary and initialises its properties }
begin
  if (csDesigning in componentstate) then exit ;
  if not Assigned(fRepeatTimer) then fRepeatTimer := TTimer.Create(Self);
  fRepeatTimer.Interval := fRepeatDelay ;
  fRepeatTimer.OnTimer := TimerExpired ;
  fRepeatTimer.Enabled := true ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.ResetRepeatTimer;
{ Frees timer if it exists }
begin
  if Assigned(fRepeatTimer)
  then begin
    fRepeatTimer.Free ;
    fRepeatTimer := nil ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  fCount := 1 ;
  if fRepeatEnable then SetRepeatTimer ;
end;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if Assigned(fRepeatTimer) then fRepeatTimer.Enabled  := False;
end;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.CMMouseEnter(var message:TMessage);
begin
  inherited;
  if (Assigned(fOnMouseEnter)) then fOnMouseEnter(Self);
end ;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.CMMouseLeave(var message:TMessage);
begin
  inherited;
  if (Assigned(fOnMouseLeave)) then fOnMouseLeave(Self);
end ;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.TimerExpired(Sender: TObject);
{ Reinitialises the timer if valid }
begin
  if Assigned(fRepeatTimer)
  then begin
    if fRepeatEnable
    then fRepeatTimer.Interval := fRepeatInterval
    else fRepeatTimer.Enabled := false ;
  end ;
  if (FState = bsDown) and MouseCapture
  then begin
    try
      Click;
      INC(fCount) ;
    except
      fRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.Click;
begin
  inherited Click;
end;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.SetRepeatDelay( V:longint ) ;
begin
  fRepeatDelay := V ;
  if Assigned(fRepeatTimer) then fRepeatTimer.Interval := V ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhRepeatButton.SetRepeatInterval( V:longint ) ;
begin
  fRepeatInterval := V ;
  if Assigned(fRepeatTimer) then fRepeatTimer.Interval := V ;
end ;

{ ----------------------- Registration ------------------------------- }

procedure Register ;
begin
  RegisterComponents('Howie',[ThhRepeatButton]) ;
end ;

end.

