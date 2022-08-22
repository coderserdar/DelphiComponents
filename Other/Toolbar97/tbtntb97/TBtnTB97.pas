unit TBtnTB97;

interface

{$I TB97Ver.inc}

{ Special Toolbar97 for using with TToolbarButton97. Added some published
  properties:

    property btnAlignment   
    property btnDisplayMode 
    property btnFlat        
    property btnImages      
    property btnLayout      
    property btnWordWrap    
    property btnMargin      
    property btnSpacing     
    property btnWidth       
    property btnHeight      

  for applying buttons group operations and default settings.

  Grischenko Alexander.
  4 October, 1999.
  e-mail: gralex@mailbox.riga.lv
}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Buttons,
  TB97, TB97Tlbr, TB97Ctls {$IFDEF TB97D4}, ImgList {$ENDIF} ;

type
  TChangePropMethod = procedure(Button: TToolbarButton97; const Value) of object;

  TButtonsToolbar97 = class(TToolbar97)
  private
    { Private declarations }
    FAlignment   : TAlignment;
    FSize        : TPoint;
    FDisplayMode : TButtonDisplayMode;
    FFlat        : boolean;
    FImages      : TCustomImageList;
    FLayout      : TButtonLayout;
    FWordWrap    : boolean;
    FMargin      : Integer;
    FSpacing     : Integer;

    FImageChangeLink : TChangeLink;

    procedure setAlignment(const Value: TAlignment);
    procedure setSize(const Value: TPoint);
    procedure setDisplayMode(const Value: TButtonDisplayMode);
    procedure setFlat(const Value: boolean);
    procedure setImages(const Value: TCustomImageList);
    procedure setLayout(const Value: TButtonLayout);
    procedure setWordWrap(const Value: Boolean);
    procedure setMargin(const Value: Integer);
    procedure setSpacing(const Value: Integer);
    procedure setHeight(const Value: Integer);
    procedure setWidth(const Value: Integer);

    procedure propAlignment(Btn: TToolbarButton97; const Value);
    procedure propSize(Btn: TToolbarButton97; const Value);
    procedure propDisplayMode(Btn: TToolbarButton97; const Value);
    procedure propFlat(Btn: TToolbarButton97; const Value);
    procedure propImages(Btn: TToolbarButton97; const Value);
    procedure propLayout(Btn: TToolbarButton97; const Value);
    procedure propWordWrap(Btn: TToolbarButton97; const Value);
    procedure propMargin(Btn: TToolbarButton97; const Value);
    procedure propSpacing(Btn: TToolbarButton97; const Value);

    procedure CMControlListChange (var Message: TCMControlListChange);
       message CM_CONTROLLISTCHANGE;

  protected
    { Protected declarations }
    procedure Notification (AComponent: TComponent; Operation: TOperation); override;
    procedure ChangeProperty(SetPropMethod: TChangePropMethod; const Value);

  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    property btnSize: TPoint read FSize write setSize;

  published
    { Published declarations }

    property btnAlignment   : TAlignment read FAlignment write setAlignment default taCenter;
    property btnDisplayMode : TButtonDisplayMode read FDisplayMode write setDisplayMode;
    property btnFlat        : boolean read FFlat write setFlat default true;
    property btnImages      : TCustomImageList read FImages write setImages;
    property btnLayout      : TButtonLayout read FLayout write setLayout default blGlyphLeft;
    property btnWordWrap    : boolean read FWordWrap write setWordWrap default false;
    property btnMargin      : Integer read FMargin write setMargin default -1;
    property btnSpacing     : Integer read FSpacing write setSpacing default 4;
    property btnWidth       : Integer read FSize.X write setWidth  default 22;
    property btnHeight      : Integer read FSize.Y write setHeight default 22;

  end;

procedure Register;

implementation

Type TB = TButtonsToolbar97;

procedure Register;
begin
  RegisterComponents('Toolbar97', [TButtonsToolbar97]);
end;

constructor TB.Create(aOwner: TComponent);
Begin
   Inherited Create(aOwner);
   FSize.X:=22; FSize.Y:=22;
   FFlat:=True; FMargin := -1;
   FSpacing:=4; FLayout := blGlyphLeft;
   FDisplayMode := dmBoth;
   FAlignment := taCenter;
end;

destructor TB.Destroy;
Begin
   FImageChangeLink.Free;
   Inherited Destroy;
end;

procedure TB.ChangeProperty(SetPropMethod: TChangePropMethod; const Value);
var i:Integer;
    Control: TControl;
begin
  if not Assigned(SetPropMethod) then Exit;

  BeginUpdate;
  try
    for I := 0 to ControlCount-1 do
    begin
      Control := Controls[i];
      if Control is TToolbarButton97 then
          SetPropMethod(TToolbarButton97(Control), Value);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TB.propAlignment(Btn: TToolbarButton97; const Value);
Begin
     Btn.Alignment := TAlignment(Value);
end;

procedure TB.propSize(Btn: TToolbarButton97; const Value);
Begin
     with Btn do
       SetBounds(Left,Top, TPoint(Value).X, TPoint(Value).Y);
end;

procedure TB.propDisplayMode(Btn: TToolbarButton97; const Value);
Begin
     Btn.DisplayMode := TButtonDisplayMode(Value);
end;

procedure TB.propFlat(Btn: TToolbarButton97; const Value);
Begin
     Btn.Flat := Boolean(Value)
end;

procedure TB.propImages(Btn: TToolbarButton97; const Value);
Begin
     Btn.Images := TCustomImageList(Value);
end;

procedure TB.propLayout(Btn: TToolbarButton97; const Value);
Begin
     Btn.Layout := TButtonLayout(Value)
end;

procedure TB.propWordWrap(Btn: TToolbarButton97; const Value);
Begin
     Btn.WordWrap := Boolean(Value);
end;

procedure TB.propMargin(Btn: TToolbarButton97; const Value);
Begin
     Btn.Margin := Integer(Value);
end;

procedure TB.propSpacing(Btn: TToolbarButton97; const Value);
Begin
     Btn.Spacing := Integer(Value);
end;

{ ------ }

procedure TB.setAlignment(const Value: TAlignment);
begin
     FAlignment := Value;
     if not (csLoading in ComponentState) then
        ChangeProperty(propAlignment, Value);
end;

procedure TB.setDisplayMode(const Value: TButtonDisplayMode);
begin
     FDisplayMode := Value;
     if not (csLoading in ComponentState) then
        ChangeProperty(propDisplayMode, Value);
end;

procedure TB.setFlat(const Value: boolean);
begin
     FFlat := Value;
     if not (csLoading in ComponentState) then
       ChangeProperty(propFlat, Value);
end;

procedure TB.setImages(const Value: TCustomImageList);
begin
     if FImages <> Value then begin
       if FImages <> nil then
         FImages.UnRegisterChanges (FImageChangeLink);
       FImages := Value;
       if FImages <> nil then begin
         if FImageChangeLink = nil then
           FImageChangeLink := TChangeLink.Create;
         FImages.RegisterChanges (FImageChangeLink);
         FImages.FreeNotification (Self);
       end
       else begin
         FImageChangeLink.Free;
         FImageChangeLink := nil;
       end;
     end;

     if not (csLoading in ComponentState) then
       ChangeProperty(propImages, Value);
end;

procedure TB.setLayout(const Value: TButtonLayout);
begin
     FLayout := Value;
     if not (csLoading in ComponentState) then
       ChangeProperty(propLayout, Value);
end;

procedure TB.setWordWrap(const Value: Boolean);
begin
     FWordWrap := Value;
     if not (csLoading in ComponentState) then
       ChangeProperty(propWordWrap, Value);
end;

procedure TB.setMargin(const Value: Integer);
begin
     FMargin := Value;
     if not (csLoading in ComponentState) then
       ChangeProperty(propMargin, Value);
end;

procedure TB.setSpacing(const Value: Integer);
begin
     FSpacing := Value;
     if not (csLoading in ComponentState) then
       ChangeProperty(propSpacing,Value);
end;

procedure TB.setHeight(const Value: Integer);
begin
     SetSize(Point(FSize.X, Value));
end;

procedure TB.setWidth(const Value: Integer);
begin
     SetSize(Point(Value, FSize.Y));
end;

procedure TB.setSize(const Value: TPoint);
begin
     FSize := Value;
     if not (csLoading in ComponentState) then
       ChangeProperty(propSize, Value);
end;

{ ----- }

procedure TB.Notification (AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if (AComponent = FImages) then FImages := nil;
  end;
end;

procedure TB.CMControlListChange (var Message: TCMControlListChange);
{ The VCL sends this message is sent whenever a child control is inserted into
  or deleted from the toolbar }
begin
  inherited;
  with Message do begin
    if Inserting then
      if Control is TToolbarButton97 then
      with TToolbarButton97(Control) do
      Begin
         Alignment   := FAlignment;
         DisplayMode := FDisplayMode;
         Flat        := FFlat;
         SetBounds(Left,Top, FSize.X, FSize.Y);
         Images      := FImages;
         Layout      := FLayout;
         WordWrap    := FWordWrap;
         Margin      := FMargin;
         Spacing     := FSpacing;
      end;
  end;
end;


end.
