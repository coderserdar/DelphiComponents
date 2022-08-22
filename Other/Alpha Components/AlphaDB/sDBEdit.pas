unit sDBEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Mask, DBCtrls,
  sConst, sDefaults, acSBUtils, acntTypes, sCommonData{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBEdit = class(TDBEdit)
  private
    FCommonData: TsScrollWndData;
    FBoundLabel: TsBoundLabel;
    FDisabledKind: TsDisabledKind;
    FVerticalAlignment: TVerticalAlignment;
    FPadding: TacPadding;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure WMPrintClient(var Message: TWMPaint); message WM_PRINTCLIENT;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    function FontStored: boolean;
  protected
    FAddedPadding: TRect;
    procedure WndProc (var Message: TMessage); override;
    procedure PaddingChanged(Sender: TObject);
  public
    ListSW: TacScrollWnd;
    procedure AfterConstruction; override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData: TsScrollWndData read FCommonData write FCommonData;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property Padding: TacPadding read FPadding write FPadding;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default taAlignTop;
  end;


implementation

uses sStyleSimply, sVCLUtils, sMessages, acntUtils, sGraphUtils;


procedure TsDBEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;


constructor TsDBEdit.Create(AOwner: TComponent);
begin
  FCommonData := TsScrollWndData.Create(Self, True);
  FCommonData.COC := COC_TsDBEdit;
  inherited Create(AOwner);
  FPadding := TacPadding.Create(Self, PaddingChanged);
  FVerticalAlignment := taAlignTop;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
end;


destructor TsDBEdit.Destroy;
begin
  FreeAndNil(ListSW);
  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  FPadding.Free;
  inherited Destroy;
end;


function TsDBEdit.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


procedure TsDBEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  RefreshEditScrolls(SkinData, ListSW);
end;


procedure TsDBEdit.PaddingChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then begin
    FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;
    FCommonData.Invalidate(True);
  end;
end;


procedure TsDBEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsDBEdit.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  if FVerticalAlignment <> Value then begin
    FVerticalAlignment := Value;
    PaddingChanged(Self);
  end;
end;


procedure TsDBEdit.WndProc(var Message: TMessage);
var
  DC, SavedDC: hdc;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end; // AlphaSkins supported

      AC_REMOVESKIN: begin
        CommonWndProc(Message, FCommonData);
        FreeAndNil(ListSW);
        if not (CsDesigning in ComponentState) then
          RecreateWnd;

        Exit;
      end;

      AC_REFRESH: begin
        CommonWndProc(Message, FCommonData);
        if not InAnimationProcess then
          RedrawWindow(Handle, nil, 0, RDWA_REPAINT);

        RefreshEditScrolls(SkinData, ListSW);
        Exit;
      end;

      AC_SETNEWSKIN: begin
        CommonWndProc(Message, FCommonData);
        Exit;
      end;

      AC_ENDPARENTUPDATE:
        if FCommonData.Updating then begin
          FCommonData.FUpdating := False;
          Perform(WM_NCPAINT, 0, 0);
          Exit;
        end;

      AC_GETDEFINDEX: begin
        if FCommonData.SkinManager <> nil then
          Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssEdit] + 1;

        Exit;
      end;
    end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else begin
    if CommonWndProc(Message, FCommonData) then
      Exit;

    inherited;
    case Message.Msg of
      CM_SHOWINGCHANGED:
        RefreshEditScrolls(SkinData, ListSW);

      CM_TEXTCHANGED, CM_VISIBLECHANGED, CM_ENABLEDCHANGED:
        FCommonData.Invalidate;

      WM_SETFONT: begin
        if FVerticalAlignment <> taAlignTop then
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;

        FCommonData.Invalidate;
      end;

      WM_SIZE: begin
        if FVerticalAlignment <> taAlignTop then
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;

        SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;

      WM_PRINT: begin
        DC := TWMPaint(Message).DC;
        SavedDC := SaveDC(DC);
        MoveWindowOrg(DC, 2, 2);
        IntersectClipRect(DC, 0, 0, SkinData.FCacheBmp.Width - 2 * 2, SkinData.FCacheBmp.Height - 2 * 2);
        SendMessage(Handle, WM_PAINT, ACNativeInt(DC), Message.lParam);
        RestoreDC(DC, SavedDC);
      end;
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


procedure TsDBEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    FAddedPadding := PaddingToRect(Padding, SkinData.CommonSkinData.PPI);
    if FVerticalAlignment <> taAlignTop then
      case FVerticalAlignment of
        taVerticalCenter: inc(FAddedPadding.Top, (Height - GetFontHeight(Font.Handle) - FAddedPadding.Top - FAddedPadding.Bottom) div 2);
        taAlignBottom:    inc(FAddedPadding.Top,  Height - GetFontHeight(Font.Handle) - FAddedPadding.Top - FAddedPadding.Bottom);
      end;

    AddPadding(Message.CalcSize_Params.rgrc[0], FAddedPadding);
  end
  else
    FAddedPadding := MkRect;
end;


procedure TsDBEdit.WMPrintClient(var Message: TWMPaint);
begin
  if not SkinData.Skinned then
    inherited;
end;               

end.
