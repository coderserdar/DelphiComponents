///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMSpeedButton;

interface

uses
  SysUtils, Classes, Controls, Buttons;

type
  TMDIButtonStyle = (mbMinimize, mbRestore, mbClose);

  TDMSpeedButton = class(TSpeedButton)
  private
    { Private declarations }
    FButtonStyle: TMDIButtonStyle;
    procedure SetButtonStyle(BS: TMDIButtonStyle);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
  published
    { Published declarations }
    property ButtonStyle: TMDIButtonStyle read FButtonStyle write SetButtonStyle;
  end;

procedure Register;

implementation

uses Windows, {DFCS_xxx} Themes;

procedure Register;
begin
  RegisterComponents('DM2003', [TDMSpeedButton]);
end;

{ TDMSpeedButton }

procedure TDMSpeedButton.Paint;
const
  ButtonStyles: array[TMDIButtonStyle] of word = (DFCS_CAPTIONMIN, 
    DFCS_CAPTIONRESTORE, DFCS_CAPTIONCLOSE);
  MouseStyles: array[Boolean] of word = (0, DFCS_HOT);
  PushStyles: array[Boolean] of word = (0, DFCS_PUSHED);
  FlatStyles: array[Boolean] of word = (0, DFCS_FLAT);
begin
  DrawFrameControl(Canvas.Handle, ClientRect, DFC_CAPTION,
    MouseStyles[MouseInControl] or 
    ButtonStyles[ButtonStyle] or 
    FlatStyles[ThemeServices.ThemesEnabled] or
    PushStyles[FState = bsDown]);                           
end;

procedure TDMSpeedButton.SetButtonStyle(BS: TMDIButtonStyle);
begin
  if BS<>FButtonStyle then
  begin
    FButtonStyle:=BS;
    Invalidate;
  end;
end;

end.
