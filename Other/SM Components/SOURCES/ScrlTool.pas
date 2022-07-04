{ Copyright (C) 1998-2008, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  Этот компонент предоставляет возможность пролистывать
  ToolButton's в ToolBar, если они все не помещаются
  на экране (like Delphi Component Palette)
}
unit ScrlTool;

interface

{$IFDEF VER100}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  extctrls, comctrls,
  {$IFDEF SMForDelphi6} DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF}
  ;

type
  TScrollToolBar = class(TWinControl)
  private
    { Private declarations }
    FScrollerToolBar: TToolBar;
    FLeftBtn, FRightBtn: TImage; {Левая и правая прокрутка}
    FVisibleCount: Integer; {количество кнопок, помещающихся на ToolBar}
    FShiftCount: Integer; {количество кнопок перед "левым кадром" - слева от прокрутки}
    procedure SwitchEnableImages;
    procedure FLeftClick(Sender: TObject);
    procedure FRightClick(Sender: TObject);
    procedure FReVisibleCount(Sender: TObject);
    procedure ShowHideButtons(Sender: TObject);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property ScrollerToolBar: TToolBar read FScrollerToolBar write FScrollerToolBar;
  end;

{ TScrollToolBarEditor }

  TScrollToolBarEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation
{ $R SCRLTOOL.RES}
{$R *.RES}

procedure Register;
begin
  RegisterComponentEditor(TScrollToolBar, TScrollToolBarEditor);
  RegisterComponents('SMComponents', [TScrollToolBar]);
end;

constructor TScrollToolBar.Create(AOwner: TComponent);
//var
//  i: integer;
begin
  if not (AOwner is TWinControl) then
  begin
    MessageDlg('Parent component must to be a inherited from TWinControl', mtError, [mbOk], 0);
    Abort;
  end;

  inherited Create(AOwner);
  Height := 32;
  Parent := AOwner as TWinControl;
  Align := alTop;

  FLeftBtn := TImage.Create(Self);
  FLeftBtn.Enabled := False;
  FLeftBtn.Width := 16;
  FLeftBtn.Parent := Self;
  FLeftBtn.Stretch := True;
  FLeftBtn.Align := alLeft;
  FLeftBtn.OnClick := FLeftClick;

  FRightBtn := TImage.Create(Self);
  FRightBtn.Enabled := False;
  FRightBtn.Width := 16;
  FRightBtn.Parent := Self;
  FRightBtn.Stretch := True;
  FRightBtn.Align := alRight;
  FRightBtn.OnClick := FRightClick;

  FScrollerToolBar := TToolBar.Create(Self);
  FScrollerToolBar.Parent := Self;
  FScrollerToolBar.Align := alClient;
  FScrollerToolBar.ShowCaptions := True;
  FScrollerToolBar.Wrapable := False;
  FScrollerToolBar.OnResize := FReVisibleCount;

{  for i := 1 to 10 do
    with TToolButton.Create(Self) do
    begin
      Caption := 'new_' + inttostr(i);
      Enabled := True;
      Style := tbsButton;
      Visible := True;
      Parent := FScrollerToolBar;
    end;
}
  FShiftCount := 0;
  FReVisibleCount(FScrollerToolBar);
end;

destructor TScrollToolBar.Destroy;
begin
  FScrollerToolBar.Free;

  FLeftBtn.Free;
  FLeftBtn := nil;

  FRightBtn.Free;
  FRightBtn := nil;

  inherited Destroy;
end;

procedure TScrollToolBar.SwitchEnableImages;
begin
  with FLeftBtn do
  begin
    Enabled := (FShiftCount > 0);
    if Enabled then
      Picture.Bitmap.Handle := LoadBitmap(hInstance, 'LEFTBTNENABLED')
    else
      Picture.Bitmap.Handle := LoadBitmap(hInstance, 'LEFTBTNDISABLED')
  end;

  with FRightBtn do
  begin
    Enabled := (FScrollerToolBar.ButtonCount - FShiftCount - FVisibleCount > 0);
    if Enabled then
      Picture.Bitmap.Handle := LoadBitmap(hInstance, 'RIGHTBTNENABLED')
    else
      Picture.Bitmap.Handle := LoadBitmap(hInstance, 'RIGHTBTNDISABLED')
  end;

  ShowHideButtons(FScrollerToolBar);
end;

procedure TScrollToolBar.FLeftClick(Sender: TObject);
begin
  if (FLeftBtn.Enabled = True) and (FShiftCount > 0) then
  begin
    Dec(FShiftCount);
    SwitchEnableImages;
  end;
end;

procedure TScrollToolBar.FRightClick(Sender: TObject);
begin
  if (FRightBtn.Enabled = True) and (FScrollerToolBar.ButtonCount - FShiftCount - FVisibleCount > 0) then
  begin
    Inc(FShiftCount);
    SwitchEnableImages;
  end;
end;

procedure TScrollToolBar.FReVisibleCount(Sender: TObject);
begin
  if (FScrollerToolBar.ButtonCount > 0) then
    FVisibleCount := FScrollerToolBar.Width div FScrollerToolBar.Buttons[0].Width
//    FVisibleCount := FScrollerToolBar.Width div FScrollerToolBar.Buttons[FScrollerToolBar.ButtonCount-1].Width
  else
    FVisibleCount := 0;

  SwitchEnableImages;
end;

procedure TScrollToolBar.ShowHideButtons(Sender: TObject);
var i: Integer;
begin
  for i := 0 to FScrollerToolBar.ButtonCount - 1 do
    FScrollerToolBar.Buttons[i].Visible := (i > FShiftCount - 1) and (i < FShiftCount + FVisibleCount + 1);
end;

{ TScrollToolBarEditor }

procedure TScrollToolBarEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: MessageDlg('Index = ' + IntToStr(Index), mtInformation, [mbOk], 0);
    1: MessageDlg('Index = ' + IntToStr(Index), mtInformation, [mbOk], 0);
    //ShowProxyEditor(Designer, TPageManager(Component));
  end;
end;

function TScrollToolBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'New Button';
    1: Result := 'New Separator';
  end;
end;

function TScrollToolBarEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
