{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnSkinStyle;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls;

type
  ICnSkinParams = interface(IUnknown)
    ['{C4FDB92B-AE10-49F9-918E-F4F7DC9CD1E4}']
    function GetFaceColor: TColor;
    function GetLightColor: TColor;
    function GetMenuHotColor: TColor;
    function GetShadowColor: TColor;
    function GetActiveCaptionColor: TColor;
    function GetInActiveCaptionColor: TColor;
    function GetCaptionHeight: Integer;
    function GetBorderSize: Integer;
    function GetButtonSize: Integer;
    function GetButtonRight: Integer;
    function GetButtonTop: Integer;
    function GetCaptionFontSize: Integer;
    function GetRgnSize: Integer;
    function GetWindowBmp: TBitmap;
    function GetWindowBtnBmp: TBitmap;
    function GetScrollBarBmp: TBitmap;
    function GetButtonBmp: TBitmap;
    function GetCheckBmp: TBitmap;
    function GetComboBmp: TBitmap;
    function GetRadioBmp: TBitmap;

    property FaceColor: TColor read GetFaceColor;
    {* ���������ɫ }
    property LightColor: TColor read GetLightColor;
    {* ������ɫ }
    property ShadowColor: TColor read GetShadowColor;
    {* ��Ӱ��ɫ }
    property MenuHotColor: TColor read GetMenuHotColor;
    {* �˵�������ɫ }
    property ActiveCaptionColor: TColor read GetActiveCaptionColor;
    {* �����ı�����ɫ }
    property InActiveCaptionColor: TColor read GetInActiveCaptionColor;
    {* �ǻ����ı�����ɫ }
    property CaptionHeight: Integer read GetCaptionHeight;
    {* �������߶� }
    property BorderSize: Integer read GetBorderSize;
    {* ����߿��ϸ }
    property ButtonSize: Integer read GetButtonSize;
    {* �������ⰴť�ĳߴ� }
    property ButtonTop: Integer read GetButtonTop;
    {* ���ⰴť�ඥ�˵ĳߴ� }
    property ButtonRight: Integer read GetButtonRight;
    {* ���ⰴť���ұߵĳߴ� }
    property CaptionFontSize: Integer read GetCaptionFontSize;
    {* ���������ֳߴ� }
    property RgnSize: Integer read GetRgnSize;
    {* ����ߴ� }
    property WindowBmp: TBitmap read GetWindowBmp;
    {* ����������ͼƬ }
    property WindowBtnBmp: TBitmap read GetWindowBtnBmp;  
    {* ����ϵͳ��ťͼƬ }  
    property ScrollBarBmp: TBitmap read GetScrollBarBmp;
    {* ��������ͼƬ }
    property ButtonBmp: TBitmap read GetButtonBmp;
    {* ��ͨ��ťͼƬ }
    property CheckBmp: TBitmap read GetCheckBmp;
    {* ��ѡ��ͼƬ }
    property RadioBmp: TBitmap read GetRadioBmp;
    {* ��ѡťͼƬ }
    property ComboBmp: TBitmap read GetComboBmp;
    {* ������ͼƬ }
  end;

  TCnSkinStyle = class(TComponent, ICnSkinParams)
  private
    FWindowBmp: TBitmap;
    FWindowBtnBmp: TBitmap;
    FScrollBarBmp: TBitmap;
    FButtonBmp: TBitmap;
    FCheckBmp: TBitmap;
    FRadioBmp: TBitmap;
    FComboBmp: TBitmap;
    FShadowColor: TColor;
    FLightColor: TColor;
    FMenuHotColor: TColor;
    FFaceColor: TColor;
    FActiveCaptionColor: TColor;
    FInActiveCaptionColor: TColor;
    FCaptionHeight: Integer;
    FBorderSize: Integer;
    FButtonSize: Integer;
    FButtonRight: Integer;
    FButtonTop: Integer;
    FCaptionFontSize: Integer;
    FRgnSize: Integer;
    function GetFaceColor: TColor;
    function GetLightColor: TColor;
    function GetMenuHotColor: TColor;
    function GetScrollBarBmp: TBitmap;
    function GetShadowColor: TColor;
    function GetActiveCaptionColor: TColor;
    function GetInActiveCaptionColor: TColor;
    procedure SetFaceColor(const Value: TColor);
    procedure SetLightColor(const Value: TColor);
    procedure SetMenuHotColor(const Value: TColor);
    procedure SetScrollBarBmp(const Value: TBitmap);
    procedure SetShadowColor(const Value: TColor);
    procedure SetActiveCaptionColor(const Value: TColor);
    procedure SetInActiveCaptionColor(const Value: TColor);
    procedure SetCaptionHeight(const Value: Integer);
    function GetCaptionHeight: Integer;
    procedure SetBorderSize(const Value: Integer);
    function GetBorderSize: Integer;
    procedure SetButtonSize(const Value: Integer);
    function GetButtonSize: Integer;
    procedure SetButtonRight(const Value: Integer);
    procedure SetButtonTop(const Value: Integer);
    function GetButtonRight: Integer;
    function GetButtonTop: Integer;
    function GetCaptionFontSize: Integer;
    procedure SetCaptionFontSize(const Value: Integer);
    procedure SetRgnSize(const Value: Integer);
    function GetRgnSize: Integer;

    function GetButtonBmp: TBitmap;
    function GetCheckBmp: TBitmap;
    function GetComboBmp: TBitmap;
    function GetRadioBmp: TBitmap;
    procedure SetButtonBmp(const Value: TBitmap);
    procedure SetCheckBmp(const Value: TBitmap);
    procedure SetComboBmp(const Value: TBitmap);
    procedure SetRadioBmp(const Value: TBitmap);
    function GetWindowBmp: TBitmap;
    function GetWindowBtnBmp: TBitmap;
    procedure SetWindowBmp(const Value: TBitmap);
    procedure SetWindowBtnBmp(const Value: TBitmap);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitConsts; virtual;
    procedure InitResources; virtual;
    procedure Clear;
  published
    property FaceColor: TColor read GetFaceColor write SetFaceColor;
    {* ���������ɫ }
    property LightColor: TColor read GetLightColor write SetLightColor;
    {* ������ɫ }
    property ShadowColor: TColor read GetShadowColor write SetShadowColor;
    {* ��Ӱ��ɫ }
    property MenuHotColor: TColor read GetMenuHotColor write SetMenuHotColor;
    {* �˵�������ɫ }
    property ActiveCaptionColor: TColor read GetActiveCaptionColor write SetActiveCaptionColor;
    {* �����ı�����ɫ }
    property InActiveCaptionColor: TColor read GetInActiveCaptionColor write SetInActiveCaptionColor;
    {* �ǻ����ı�����ɫ }
    property CaptionHeight: Integer read GetCaptionHeight write SetCaptionHeight;
    {* �������߶� }
    property BorderSize: Integer read GetBorderSize write SetBorderSize;
    {* ����߿��ϸ }
    property ButtonSize: Integer read GetButtonSize write SetButtonSize;
    {* �������ⰴť�ĳߴ� }
    property ButtonTop: Integer read GetButtonTop write SetButtonTop;
    {* ���ⰴť�ඥ�˵ĳߴ� }
    property ButtonRight: Integer read GetButtonRight write SetButtonRight;
    {* ���ⰴť���ұߵĳߴ� }
    property CaptionFontSize: Integer read GetCaptionFontSize write SetCaptionFontSize;
    {* ���������ֳߴ� }
    property RgnSize: Integer read GetRgnSize write SetRgnSize;
    {* ����ߴ� }
    property WindowBmp: TBitmap read GetWindowBmp write SetWindowBmp;
    {* ����������ͼƬ }
    property WindowBtnBmp: TBitmap read GetWindowBtnBmp write SetWindowBtnBmp;
    {* ����ϵͳ��ťͼƬ }
    property ScrollBarBmp: TBitmap read GetScrollBarBmp write SetScrollBarBmp;
    {* ��������ͼƬ }
    property ButtonBmp: TBitmap read GetButtonBmp write SetButtonBmp;
    {* ��ͨ��ťͼƬ }
    property CheckBmp: TBitmap read GetCheckBmp write SetCheckBmp;
    {* ��ѡ��ͼƬ }
    property RadioBmp: TBitmap read GetRadioBmp write SetRadioBmp;
    {* ��ѡťͼƬ }
    property ComboBmp: TBitmap read GetComboBmp write SetComboBmp;
    {* ������ͼƬ }
  end;

  TCnSkinXPStyle = class(TCnSkinStyle)
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitConsts; override;
    procedure InitResources; override;
  published

  end;

procedure CnReadBmpFromResource(Bmp: TBitmap; const ResName: string);
{* ����Դ������Ҫ��ͼƬ}

implementation

uses
  CnSkinTheme;

procedure CnReadBmpFromResource(Bmp: TBitmap; const ResName: string);
var
  Stream: TResourceStream;
begin
  if (Bmp <> nil) and (ResName <> '') then
  begin
    Stream := TResourceStream.Create(HInstance, ResName, 'BMP');
    try
      Bmp.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

{ TCnSkinStyle }

procedure TCnSkinStyle.Assign(Source: TPersistent);
var
  ASkinStyle: TCnSkinStyle;
begin
  if Source is TCnSkinStyle then
  begin
    ASkinStyle := (Source as TCnSkinStyle);
    FaceColor := ASkinStyle.FaceColor;
    LightColor := ASkinStyle.LightColor;
    ShadowColor := ASkinStyle.ShadowColor;
    MenuHotColor := ASkinStyle.MenuHotColor;
    ActiveCaptionColor := ASkinStyle.ActiveCaptionColor;
    InActiveCaptionColor := ASkinStyle.InActiveCaptionColor;
    CaptionHeight := ASkinStyle.CaptionHeight;
    BorderSize := ASkinStyle.BorderSize;
    ButtonSize := ASkinStyle.ButtonSize;
    ButtonTop := ASkinStyle.ButtonTop;
    ButtonRight := ASkinStyle.ButtonRight;
    CaptionFontSize := ASkinStyle.CaptionFontSize;
    RgnSize  := ASkinStyle.RgnSize;
    WindowBmp.Assign(ASkinStyle.WindowBmp);
    WindowBtnBmp.Assign(ASkinStyle.WindowBtnBmp);
    ScrollBarBmp.Assign(ASkinStyle.ScrollBarBmp);
    ButtonBmp.Assign(ASkinStyle.ButtonBmp);
    CheckBmp.Assign(ASkinStyle.CheckBmp);
    RadioBmp.Assign(ASkinStyle.RadioBmp);
    ComboBmp.Assign(ASkinStyle.ComboBmp);
  end
  else
    inherited;
end;

procedure TCnSkinStyle.Clear;
begin

end;

constructor TCnSkinStyle.Create(AOwner: TComponent);
begin
  inherited;
  FWindowBmp := TBitmap.Create;
  FWindowBtnBmp := TBitmap.Create;
  FScrollBarBmp := TBitmap.Create;
  FButtonBmp := TBitmap.Create;
  FCheckBmp := TBitmap.Create;
  FRadioBmp := TBitmap.Create;
  FComboBmp := TBitmap.Create;
  
  InitConsts;
  InitResources;

  CnSkinThemes.AddSkin(Self);
end;

destructor TCnSkinStyle.Destroy;
begin
  FComboBmp.Free;
  FRadioBmp.Free;
  FCheckBmp.Free;
  FButtonBmp.Free;
  FScrollBarBmp.Free;
  FWindowBtnBmp.Free;
  FWindowBmp.Free;
  inherited;
end;

function TCnSkinStyle.GetActiveCaptionColor: TColor;
begin
  Result := FActiveCaptionColor;
end;

function TCnSkinStyle.GetBorderSize: Integer;
begin
  Result := FBorderSize;
end;

function TCnSkinStyle.GetButtonBmp: TBitmap;
begin
  Result := FButtonBmp;
end;

function TCnSkinStyle.GetButtonRight: Integer;
begin
  Result := FButtonRight;
end;

function TCnSkinStyle.GetButtonSize: Integer;
begin
  Result := FButtonSize;
end;

function TCnSkinStyle.GetButtonTop: Integer;
begin
  Result := FButtonTop;
end;

function TCnSkinStyle.GetCaptionFontSize: Integer;
begin
  Result := FCaptionFontSize;
end;

function TCnSkinStyle.GetCaptionHeight: Integer;
begin
  Result := FCaptionHeight;
end;

function TCnSkinStyle.GetCheckBmp: TBitmap;
begin
  Result := FCheckBmp;
end;

function TCnSkinStyle.GetComboBmp: TBitmap;
begin
  Result := FComboBmp;
end;

function TCnSkinStyle.GetFaceColor: TColor;
begin
  Result := FFaceColor;
end;

function TCnSkinStyle.GetInActiveCaptionColor: TColor;
begin
  Result := FInActiveCaptionColor;
end;

function TCnSkinStyle.GetLightColor: TColor;
begin
  Result := FLightColor;
end;

function TCnSkinStyle.GetMenuHotColor: TColor;
begin
  Result := FMenuHotColor;
end;

function TCnSkinStyle.GetRadioBmp: TBitmap;
begin
  Result := FRadioBmp;
end;

function TCnSkinStyle.GetRgnSize: Integer;
begin
  Result := FRgnSize;
end;

function TCnSkinStyle.GetScrollBarBmp: TBitmap;
begin
  Result := FScrollBarBmp;
end;

function TCnSkinStyle.GetShadowColor: TColor;
begin
  Result := FShadowColor;
end;

function TCnSkinStyle.GetWindowBmp: TBitmap;
begin
  Result := FWindowBmp;
end;

function TCnSkinStyle.GetWindowBtnBmp: TBitmap;
begin
  Result := FWindowBtnBmp;
end;

procedure TCnSkinStyle.InitConsts;
begin
  // ��ʼ������
  FShadowColor := clBtnShadow;
  FLightColor := clBtnHighLight;
  FFaceColor := clBtnFace;
  FMenuHotColor := clMenuHighlight;

  FCaptionHeight := 26;
  FBorderSize := 0;
  FButtonSize := 16;
  FButtonRight := 5;
  FButtonTop := 3;
  FCaptionFontSize := 11;
  FRgnSize := 0;
end;

procedure TCnSkinStyle.InitResources;
begin
  // ����ɶ������
end;

procedure TCnSkinStyle.SetActiveCaptionColor(const Value: TColor);
begin
  if FActiveCaptionColor <> Value then
  begin
    FActiveCaptionColor := Value;
  end;
end;

procedure TCnSkinStyle.SetBorderSize(const Value: Integer);
begin
  FBorderSize := Value;
end;

procedure TCnSkinStyle.SetButtonBmp(const Value: TBitmap);
begin
  FButtonBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetButtonRight(const Value: Integer);
begin
  FButtonRight := Value;
end;

procedure TCnSkinStyle.SetButtonSize(const Value: Integer);
begin
  FButtonSize := Value;
end;

procedure TCnSkinStyle.SetButtonTop(const Value: Integer);
begin
  FButtonTop := Value;
end;

procedure TCnSkinStyle.SetCaptionFontSize(const Value: Integer);
begin
  FCaptionFontSize := Value;
end;

procedure TCnSkinStyle.SetCaptionHeight(const Value: Integer);
begin
  FCaptionHeight := Value;
end;

procedure TCnSkinStyle.SetCheckBmp(const Value: TBitmap);
begin
  FCheckBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetComboBmp(const Value: TBitmap);
begin
  FComboBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetFaceColor(const Value: TColor);
begin
  FFaceColor := Value;
end;

procedure TCnSkinStyle.SetInActiveCaptionColor(const Value: TColor);
begin
  if FInActiveCaptionColor <> Value then
  begin
    FInActiveCaptionColor := Value;
  end;
end;

procedure TCnSkinStyle.SetLightColor(const Value: TColor);
begin
  FLightColor := Value;
end;

procedure TCnSkinStyle.SetMenuHotColor(const Value: TColor);
begin
  FMenuHotColor := Value;
end;

procedure TCnSkinStyle.SetRadioBmp(const Value: TBitmap);
begin
  FRadioBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetRgnSize(const Value: Integer);
begin
  FRgnSize := Value;
end;

procedure TCnSkinStyle.SetScrollBarBmp(const Value: TBitmap);
begin
  FScrollBarBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
end;

procedure TCnSkinStyle.SetWindowBmp(const Value: TBitmap);
begin
  FWindowBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetWindowBtnBmp(const Value: TBitmap);
begin
  FWindowBtnBmp.Assign(Value);
end;

{ TCnSkinXPStyle }

constructor TCnSkinXPStyle.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TCnSkinXPStyle.InitConsts;
begin
  inherited;
  FCaptionHeight := 30;
  FBorderSize := 4;
  FButtonRight := 5;
  FButtonTop := 5;
  FRgnSize := 14;
end;

procedure TCnSkinXPStyle.InitResources;
begin
  inherited;

end;

end.
