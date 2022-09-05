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

unit CnVCLBase;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ������ඨ�嵥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ������������Ļ������
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.08 V1.0
*               ����Ԫ�����°汾��
*           2002.02.01 V0.02Demo
*               ���Դ��롢������֪����
*               �Ż�CopyParentImage���������TCnGraphicControl���п�����ʾ��
*               ����Alpha��������ָ���ؼ�͸���ȣ�����ˢ�£�
*               ���Ӵ���ע��
*           2002.01.11 V0.01Demo
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, CnGraphics,
  CnClasses, CnConsts;

type

//------------------------------------------------------------------------------
// ͼ�οؼ�������
//------------------------------------------------------------------------------

{ TCnGraphicControl }

  TCnGraphicControl = class(TControl)
  {* CnPack������ͼ�οؼ�������}
  private
    FLoaded: Boolean;
    FUpdateCount: Integer;
    FIsChanged: Boolean;
    FFace: TCnBitmap;
    FTransparent: Boolean;
    FAlphaBlend: Boolean;
    FAlphaBlendValue: TCnAlpha;
    FFullPaint: Boolean;
    FPainting: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetAlphaBlendValue(const Value: TCnAlpha);
    procedure DoPaint;
    procedure DoChanged;
    procedure SetFullPaint(const Value: Boolean);
    procedure SetAlphaBlend(const Value: Boolean);
  protected
    function IsUpdating: Boolean;
    {* ��ǰ���¼����Ƿ����0�����ڸ��£���ֻ������}
    procedure WndProc(var Msg: TMessage); override;
    procedure Paint; virtual;
    {* �ؼ����Ʒ��������ؼ���ǰ״̬�ı���Զ����ø÷�������������
     |<BR> �ؼ����ش˷����� Face ���л���}
    procedure Loaded; override;
    {* ������������װ�أ�������ڵ�һ��Paintʱ���ã���������
     |<BR> ����ʱ����� inherited}
    procedure Changed; virtual;
    {* �����ѱ���������޸Ŀؼ����Ժ���ø÷�������֪ͨ�ؼ��ػ棬������
     |<BR> ���ø÷�������Ҫ�ٵ��ÿؼ��� Invalidate �ȷ���
     |<BR> ����ʱ����� inherited}
    procedure OnFaceChange(Sender: TObject); virtual;
    procedure OnChildChange(Sender: TObject); virtual;
    {* �������ѱ��֪ͨ��Ĭ��Ϊ����Changed������������}
    procedure MouseEnter; virtual;
    {* �������ؼ�֪ͨ�������أ�����ʱ����� inherited �Բ����¼�}
    procedure MouseLeave; virtual;
    {* ����Ƴ��ؼ�֪ͨ�������أ�����ʱ����� inherited �Բ����¼�}
    procedure SetTransparent(const Value: Boolean); virtual;
    {* ���ÿؼ�͸�����ԣ������أ�����ʱ����� inherited}
    property Face: TCnBitmap read FFace;
    {* �ؼ����滭������TCnBitmap���ͣ����ڴ��б����˿ؼ���ǰͼ��
     |<BR> ����Ҫ����Ļ�ϻ��ƿؼ�ʱ��ֱ�ӽ���λͼ���Ƶ���Ļ���Ի�ÿ��ٵ���ʾ
     |<BR> �����������������ؼ���Canvas���ԣ��û���ֱ����Face�ϻ�ͼ�������Ҫ
           ����Ļ�����������ÿؼ���Refresh��Repaint����}
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend default False;
    {* �ؼ��İ�͸�����ԣ����Ϊ������ؼ���ʾΪ��͸��Ч������͸����ΪAlphaBlendValue}
    property AlphaBlendValue: TCnAlpha read FAlphaBlendValue write SetAlphaBlendValue
      default csMaxAlpha;
    {* �ؼ��Ĳ�͸�������ԣ���AlphaBlend��Ӱ�죬Ϊ0ʱ��ȫ͸����255ʱ��͸��}
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    {* �ؼ��ı���͸�����ԣ�Ϊ������ؼ���ʾΪ���������״��������͸��}
    property FullPaint: Boolean read FFullPaint write SetFullPaint default True;
    {* ����������Transparent����ʱ���Ƿ�ÿ���ػ�ؼ������ƿؼ�����
    |<BR> ���ؼ�����Ŀؼ��͸��ؼ�����Ϊ��̬ʱ���رո����Կ������ʾ�ٶ�
    |<BR> ���ؼ�����Ŀؼ��͸��ؼ�����Ϊ��̬ʱ��������Ϊ��}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {* �������ؼ��¼�}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {* ����Ƴ��ؼ��¼�}
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    {* �ؼ������ػ��¼�}
  public
    constructor Create(AOwner: TComponent); override;
    {* ������������һ���ؼ�ʵ��}
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    {* ���ÿؼ���λ�úʹ�С��������}
    procedure BeginUpdate;
    {* ��ʼ���¿ؼ������¹����пؼ����ػ棬������
     |<BR> �ڶԳ������Խ����޸�ʱ����ø÷�����ע�������EndUpdate�ɶ�ʹ��}
    procedure EndUpdate;
    {* �������£������ǰ���¼���Ϊ0���Զ�����Change������������
     |<BR> �ڶԳ��������޸ĺ�����ø÷�����ע�������BeginUpdate�ɶ�ʹ��}
    procedure Repaint; override;
    {* ǿ�ȿؼ�����Ļ�ϻ��ƣ�����ֶ��޸��� Face ���ԣ�����ô˷�����������}
    procedure ReDraw; virtual;
    {* ǿ�ȿؼ����»�������������Ļ�������������}
  end;

//------------------------------------------------------------------------------
// ���ڿؼ�������
//------------------------------------------------------------------------------

{ TCnWinControl }

  TCnWinControl = class(TWinControl)
  private
    FLoaded: Boolean;
    FUpdateCount: Integer;
    FIsChanged: Boolean;
    FFace: TCnBitmap;
    FTransparent: Boolean;
    FAlphaBlendValue: TCnAlpha;
    FFullPaint: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAlphaBlend: Boolean;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetAlphaBlendValue(const Value: TCnAlpha);
    procedure DoPaint;
    procedure DoChanged;
    procedure SetFullPaint(const Value: Boolean);
    procedure SetAlphaBlend(const Value: Boolean);
  protected
    function IsUpdating: Boolean;
    procedure WndProc(var Msg: TMessage); override;
    procedure Paint; virtual;
    procedure Loaded; override;
    procedure Changed; virtual;
    procedure OnFaceChange(Sender: TObject); virtual;
    procedure OnChildChange(Sender: TObject); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure SetTransparent(const Value: Boolean); virtual;
    procedure PaintControls(Bmp: TCnBitmap; First: TControl);
    procedure PaintHandler(var message: TWMPaint);
    property Face: TCnBitmap read FFace;
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend default False;
    property AlphaBlendValue: TCnAlpha read FAlphaBlendValue write SetAlphaBlendValue
      default csMaxAlpha;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property FullPaint: Boolean read FFullPaint write SetFullPaint default True;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Repaint; override;
  end;

implementation

type
  TParentControl = class(TWinControl);
  TCnPersistentAccess = class(TCnPersistent);
  TCnFontAccess = class(TCnFont);

// �Ӹ��ؼ�����ͼ��
// �����޸���RxLibrary VCLUtils
procedure CopyParentImage(Control: TControl; Bmp: TCnBitmap);
var
  I, Count, x, y, SaveIndex: Integer;
  r, SelfR, CtlR: TRect;
  Parent: TWinControl;
  // CnParent: TCnWinControl;
begin
  if (Control = nil) or (Control.Parent = nil) then Exit;
  Count := Control.Parent.ControlCount;
  Parent := Control.Parent;
  {if Control.Parent is TCnWinControl then
  begin
    CnParent := TCnWinControl(Control.Parent);
    Bmp.DrawEx(0, 0, CnParent.Face, Control.ClientRect);
  end;}
  with Parent do
    ControlState := ControlState + [csPaintCopy];
  try
    with Control do
    begin
      SelfR := Bounds(Left, Top, Width, Height);
      x := -Left;
      y := -Top;
    end;
    // ���Ƹ��ؼ�ͼ��
    SaveIndex := SaveDC(Bmp.DC);
    try
      SetViewportOrgEx(Bmp.DC, x, y, nil);
      IntersectClipRect(Bmp.DC, 0, 0, Parent.ClientWidth,
        Parent.ClientHeight);
      try
        with TParentControl(Parent) do
        begin
          Perform(WM_ERASEBKGND, Bmp.DC, 0);
          PaintWindow(Bmp.DC);
        end;
      except
        ;
      end;
    finally
      RestoreDC(Bmp.DC, SaveIndex);
    end;
    // ����ͼ�οؼ�ͼ��
    for I := 0 to Count - 1 do // �� Z-Order ˳��
    begin
      if Parent.Controls[I] = Control then // ֻ�����ں���Ŀؼ�
        Break
      else if (Parent.Controls[I] <> nil) then
      begin
        if (Parent.Controls[I] is TCnGraphicControl) then
          with TCnGraphicControl(Parent.Controls[I]) do
          begin               // TCnGraphicControl �ؼ�ֱ�Ӵ� Face �и��ƣ����٣�
            CtlR := Bounds(Left, Top, Width, Height);
            if IntersectRect(r, SelfR, CtlR) and (Visible or (csDesigning in
              ComponentState)) then // ��Χ�ཻ
              Bmp.AlphaDraw(Left - Control.Left, Top - Control.Top, Face, ClientRect,
                AlphaBlendValue);
          end
        else if not (Parent.Controls[I] is TWinControl) then // ������ͼ�οؼ�
          with Parent.Controls[I] do
          begin
            CtlR := Bounds(Left, Top, Width, Height);
            if IntersectRect(r, SelfR, CtlR) and (Visible or (csDesigning in
              ComponentState)) then
            begin
              ControlState := ControlState + [csPaintCopy];
              SaveIndex := SaveDC(Bmp.DC);
              try
                SaveIndex := SaveDC(Bmp.DC);
                SetViewportOrgEx(Bmp.DC, Left + x, Top + y, nil);
                IntersectClipRect(Bmp.DC, 0, 0, Width, Height);
                Perform(WM_PAINT, Bmp.DC, 0); // ǿ�ƿؼ����Ƶ�Ŀ��DC
              finally
                RestoreDC(Bmp.DC, SaveIndex);
                ControlState := ControlState - [csPaintCopy];
              end;
            end;
          end;
      end;
    end;
  finally
    with Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

//------------------------------------------------------------------------------
// ͼ�οؼ�������
//------------------------------------------------------------------------------

{ TCnGraphicControl }

// ��ʼ��
constructor TCnGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque]; // ��ֹˢ��ʱ��������
  FUpdateCount := 0;
  FLoaded := False;
  FFace := TCnBitmap.Create(OnFaceChange);
  FFace.Transparent := False;
  FFace.GdiAllocStyle := gsNormal;
  TCnFontAccess(FFace.Font).Owner := Self;
  TCnPersistentAccess(FFace.Font.Gradient).Owner := FFace.Font;
  FTransparent := False;
  FFullPaint := True;
  FAlphaBlend := False;
  FAlphaBlendValue := csMaxAlpha; // ��͸��
  FPainting := False;
end;

// �ͷ�
destructor TCnGraphicControl.Destroy;
begin
  FFace.Free;
  inherited;
end;

//--------------------------------------------------------//
// �����صķ���                                           //
//--------------------------------------------------------//

// ������������װ�أ�������ڵ�һ��Paintʱ���ã�
// ����ʱ����� inherited
procedure TCnGraphicControl.Loaded;
begin
  inherited;
  FLoaded := True;
  Changed;
end;

// �����ѱ��
procedure TCnGraphicControl.Changed;
begin
  if IsUpdating then
    FIsChanged := True
  else
    DoChanged;
end;

// �ؼ����Ʒ���
// �ؼ��ڸù����ж� Face ���л���
procedure TCnGraphicControl.Paint;
begin

end;

// �������
procedure TCnGraphicControl.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

// ����Ƴ�
procedure TCnGraphicControl.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

// �������ݱ��
procedure TCnGraphicControl.OnFaceChange(Sender: TObject);
begin
  //
end;

// �����Ա��
procedure TCnGraphicControl.OnChildChange(Sender: TObject);
begin
  Changed;
end;

//--------------------------------------------------------//
// �������ܷ���                                           //
//--------------------------------------------------------//

// ��ʼ���£������ڼ䲻ˢ����ʾ��
procedure TCnGraphicControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// ��������
procedure TCnGraphicControl.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TCnGraphicControl.EndUpdate');
  Dec(FUpdateCount);

  if not IsUpdating and FIsChanged then // �����ѱ��
    DoChanged;
end;

// ���ڸ���
function TCnGraphicControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

// �����ѱ����ˢ�¿ؼ�
procedure TCnGraphicControl.DoChanged;
begin
  FIsChanged := False;
  DoPaint;
  Invalidate;
end;

// ˢ�¿ؼ���ʾ
procedure TCnGraphicControl.DoPaint;
begin
  if not FPainting then
  begin
    FPainting := True;
    if FTransparent then
      CopyParentImage(Self, Face) // ���͸���ȸ��Ƹ��ؼ�ͼ��
    else
      Face.Fill(Color);       // ��͸����Color���
    Paint;                    // �������ⷽ��Paint���ƿؼ�
    FPainting := False;
  end;
end;

// �ؼ�ˢ����ʾ����
procedure TCnGraphicControl.Repaint;
begin
  inherited;
  { TODO -o�ܾ��� -cͼ�οؼ������� : �ؼ��ػ淽�� }
end;

// �ؼ����»��ƻ���
procedure TCnGraphicControl.ReDraw;
begin
  Changed;
end;

// �ؼ��ػ���Ϣ
procedure TCnGraphicControl.WMPaint(var Msg: TWMPaint);
var
  Bmp: TCnBitmap;
begin
  if (csDesigning in ComponentState) and not FLoaded then
  begin
    FLoaded := True;          // ����ڵ�һ���ػ���� Loaded ����
    Loaded;
  end;

  if AlphaBlend and (AlphaBlendValue = 0) then Exit; // ��ȫ͸��

  if FTransparent and FFullPaint then
    DoPaint;                  // ͸��������ˢ��ʱÿ���ػ涼����

  if not AlphaBlend or (AlphaBlendValue = csMaxAlpha) then // �ް�͸��Ч��
  begin
    if Msg.DC <> 0 then
    begin
      Face.Lock;
      try
        Face.DrawTo(Msg.DC, 0, 0); // �������Ƶ�Ŀ��DC
      finally
        Face.Unlock;
      end;
    end;
  end
  else
  begin                       // ����͸��Ч��
    Bmp := TCnBitmap.Create;
    try
      Bmp.LoadBlank(Width, Height);
      CopyParentImage(Self, Bmp); // �Ӹ��ؼ�����ͼ��
      Bmp.AlphaDraw(Face, AlphaBlendValue, False); // Alpha���
      Bmp.DrawTo(Msg.DC, 0, 0); // ���Ƶ�Ŀ��DC
    finally
      Bmp.Free;
    end;
  end;
end;

// ��Ϣ�������
procedure TCnGraphicControl.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    CM_COLORCHANGED, CM_TEXTCHANGED, CM_FONTCHANGED: Changed;
    CM_MOUSEENTER: MouseEnter; // �������
    CM_MOUSELEAVE: MouseLeave; // ����Ƴ�
  end;
  inherited;
end;

// ���ÿؼ��߽�
procedure TCnGraphicControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if (Face.Width <> Width) or (Face.Height <> Height) then
  begin
    Face.LoadBlank(Width, Height); // �޸Ļ����ߴ�
    Changed;
  end;
end;

// ����͸��
procedure TCnGraphicControl.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

// ���ò�͸����
procedure TCnGraphicControl.SetAlphaBlendValue(const Value: TCnAlpha);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    Changed;
  end;
end;

// ���ò�͸����֧��
procedure TCnGraphicControl.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    Changed;
  end;
end;

// ����͸��ʱ�����ػ�
procedure TCnGraphicControl.SetFullPaint(const Value: Boolean);
begin
  if FFullPaint <> Value then
  begin
    FFullPaint := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// ���ڿؼ�������
//------------------------------------------------------------------------------

{ TCnWinControl }

// ��ʼ��
constructor TCnWinControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque]; // ��ֹˢ��ʱ��������
  FUpdateCount := 0;
  FLoaded := False;
  FFace := TCnBitmap.Create(OnFaceChange);
  FFace.Transparent := False;
  FFace.GdiAllocStyle := gsNormal;
  FTransparent := False;
  FFullPaint := True;
  FAlphaBlend := False;
  FAlphaBlendValue := csMaxAlpha; // ��͸��
end;

// �ͷ�
destructor TCnWinControl.Destroy;
begin
  FFace.Free;
  inherited;
end;

//--------------------------------------------------------//
// �����صķ���                                           //
//--------------------------------------------------------//

// ������������װ�أ�������ڵ�һ��Paintʱ���ã�
// ����ʱ����� inherited
procedure TCnWinControl.Loaded;
begin
  inherited;
  FLoaded := True;
  Changed;
end;

// �����ѱ��
procedure TCnWinControl.Changed;
begin
  if IsUpdating then
    FIsChanged := True
  else
    DoChanged;
end;

// �ؼ����Ʒ���
// �ؼ��ڸù����ж� Face ���л���
procedure TCnWinControl.Paint;
begin

end;

// �������
procedure TCnWinControl.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

// ����Ƴ�
procedure TCnWinControl.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

// �������ݱ��
procedure TCnWinControl.OnFaceChange(Sender: TObject);
begin
  //
end;

// �����Ա��
procedure TCnWinControl.OnChildChange(Sender: TObject);
begin
  Changed;
end;

//--------------------------------------------------------//
// �������ܷ���                                           //
//--------------------------------------------------------//

// ��ʼ���£������ڼ䲻ˢ����ʾ��
procedure TCnWinControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// ��������
procedure TCnWinControl.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TCnWinControl.EndUpdate');
  Dec(FUpdateCount);

  if not IsUpdating and FIsChanged then // �����ѱ��
    DoChanged;
end;

// ���ڸ���
function TCnWinControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

// �����ѱ����ˢ�¿ؼ�
procedure TCnWinControl.DoChanged;
begin
  FIsChanged := False;
  DoPaint;
  Invalidate;
end;

// ˢ�¿ؼ���ʾ
procedure TCnWinControl.DoPaint;
begin
  if FTransparent then
    CopyParentImage(Self, Face) // ���͸���ȸ��Ƹ��ؼ�ͼ��
  else
    Face.Fill(Color);         // ��͸����Color���
  Paint;                      // �������ⷽ��Paint���ƿؼ�
end;

// �ؼ��ػ淽��
procedure TCnWinControl.Repaint;
begin
  inherited;
  { TODO -o�ܾ��� -cͼ�οؼ������� : �ؼ��ػ淽�� }
end;

// �ؼ��ػ���Ϣ
procedure TCnWinControl.WMPaint(var Msg: TWMPaint);
var
  Bmp: TCnBitmap;
begin
  if (csDesigning in ComponentState) and not FLoaded then
  begin
    FLoaded := True;          // ����ڵ�һ���ػ���� Loaded ����
    Loaded;
  end;

  if AlphaBlend and (AlphaBlendValue = 0) then Exit; // ��ȫ͸��

  if FTransparent and FFullPaint then
    DoPaint;                  // ͸��������ˢ��ʱÿ���ػ涼����

  if not AlphaBlend or (AlphaBlendValue = csMaxAlpha) then // �ް�͸��Ч��
  begin
    if Msg.DC <> 0 then
    begin
      Face.Lock;
      try
        Face.DrawTo(Msg.DC, 0, 0); // �������Ƶ�Ŀ��DC
      finally
        Face.Unlock;
      end;
    end;
  end
  else
  begin                       // ����͸��Ч��
    Bmp := TCnBitmap.Create;
    try
      Bmp.LoadBlank(Width, Height);
      CopyParentImage(Self, Bmp); // �Ӹ��ؼ�����ͼ��
      Bmp.AlphaDraw(Face, AlphaBlendValue, False); // Alpha���
      Bmp.DrawTo(Msg.DC, 0, 0); // ���Ƶ�Ŀ��DC
    finally
      Bmp.Free;
    end;
  end;
end;

// ��Ϣ�������
procedure TCnWinControl.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    CM_COLORCHANGED, CM_TEXTCHANGED, CM_FONTCHANGED: Changed;
    CM_MOUSEENTER: MouseEnter; // �������
    CM_MOUSELEAVE: MouseLeave; // ����Ƴ�
  end;
  inherited;
end;

// ���ÿؼ��߽�
procedure TCnWinControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if (Face.Width <> Width) or (Face.Height <> Height) then
  begin
    Face.LoadBlank(Width, Height); // �޸Ļ�������
    Changed;
  end;
end;

// ����͸��
procedure TCnWinControl.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

// ���ò�͸����
procedure TCnWinControl.SetAlphaBlendValue(const Value: TCnAlpha);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    Changed;
  end;
end;

// ���ò�͸����֧��
procedure TCnWinControl.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    Changed;
  end;
end;

// ����͸��ʱ�����ػ�
procedure TCnWinControl.SetFullPaint(const Value: Boolean);
begin
  if FFullPaint <> Value then
  begin
    FFullPaint := Value;
    Changed;
  end;
end;

procedure TCnWinControl.PaintControls(Bmp: TCnBitmap; First: TControl);
//var
  //I, Count, SaveIndex: Integer;
  //FrameBrush: HBRUSH;
begin
  {if FControls <> nil then
  begin
    I := 0;
    if First <> nil then
    begin
      I := FControls.IndexOf(First);
      if I < 0 then I := 0;
    end;
    Count := FControls.Count;
    while I < Count do
    begin
      with TControl(FControls[I]) do
        if (Visible or (csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle)) and
          RectVisible(DC, Rect(Left, Top, Left + Width, Top + Height)) then
        begin
          if csPaintCopy in Self.ControlState then
            Include(FControlState, csPaintCopy);
          SaveIndex := SaveDC(DC);
          MoveWindowOrg(DC, Left, Top);
          IntersectClipRect(DC, 0, 0, Width, Height);
          Perform(WM_PAINT, DC, 0);
          RestoreDC(DC, SaveIndex);
          Exclude(FControlState, csPaintCopy);
        end;
      Inc(I);
    end;
  end;
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      with TWinControl(FWinControls[I]) do
        if FCtl3D and (csFramed in ControlStyle) and
          (Visible or (csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle)) then
        begin
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
          FrameRect(DC, Rect(Left - 1, Top - 1, Left + Width, Top + Height),
            FrameBrush);
          DeleteObject(FrameBrush);
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
          FrameRect(DC, Rect(Left, Top, Left + Width + 1, Top + Height + 1),
            FrameBrush);
          DeleteObject(FrameBrush);
        end;}
end;

procedure TCnWinControl.PaintHandler(var message: TWMPaint);
begin

end;

end.

