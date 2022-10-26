{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

{ Note:
  - in Delphi 4.0 you must add DCLSTD40 to the requires page of the
    package you install this components into.
  - in Delphi 3.0 you must add DCLSTD30 to the requires page of the
    package you install this components into.
  - in C++Builder 3.0 you must add DCLSTD35 to the requires page of the
    package you install this components into. }

unit RxTooReg;

{$I RX.INC}
{$D-,L-,S-}

interface

procedure Register;

implementation

{$R *.D32}

uses
  Classes, SysUtils, Controls, Graphics, TypInfo, Consts,
  ExtCtrls, rxPictEdit, RxHook, rxPicClip, rxPlacemnt, rxPresrDsn, rxMinMaxEd, rxDualList,
  rxClipView, rxSpeedbar, rxSbEdit, rxDataConv, RXCalc, rxPageMngr, rxPgMngrEd, rxMrgMngr,
  rxStrHlder, RXShell, rxAppEvent, rxVCLUtils, rxTimerLst, rxTimLstEd, rxIcoList, rxIcoLEdit,
  {$IFDEF USE_RX_GIF} RxGIF, rxGIFCtrl, {$ENDIF} RXLConst, RXCtrls,
  {$IFDEF RX_D3} {RxResExp,} {$ENDIF} RxMenus, rxMRUList,
  RxNotify, RxGrdCpt, rxGradEdit, rxHintProp,
  {$IFDEF RX_D6} RTLConsts, DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF}; // Polaris

{ TStringsEditor }

type
  TStringsEditor = class(TDefaultEditor)
  public
{$IFDEF RX_D6}   // Polaris
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
{$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
{$ENDIF}
  end;

{$IFDEF RX_D6}   // Polaris
procedure TStringsEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}
procedure TStringsEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'STRINGS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

{ TComponentFormProperty }

type
  TComponentFormProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure TComponentFormProperty.GetValues(Proc: TGetStrProc);
begin
  inherited GetValues(Proc);
{$IFDEF RX_D6}  // Polaris
  if (Designer.Root is GetTypeData(GetPropType)^.ClassType) and
    (Designer.Root.Name <> '') then Proc(Designer.Root.Name);
{$ELSE}
  if (Designer.Form is GetTypeData(GetPropType)^.ClassType) and
    (Designer.Form.Name <> '') then Proc(Designer.Form.Name);
{$ENDIF}
end;

procedure TComponentFormProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
  Component := Designer.GetComponent(Value);
{$IFDEF RX_D6}  // Polaris
  if ((Component = nil) or not (Component is GetTypeData(GetPropType)^.ClassType))
    and (CompareText(Designer.Root.Name, Value) = 0) then
  begin
    if not (Designer.Root is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.Create(ResStr(SInvalidPropertyValue));
    SetOrdValue(Longint(Designer.Root));
{$ELSE}
  if ((Component = nil) or not (Component is GetTypeData(GetPropType)^.ClassType))
    and (CompareText(Designer.Form.Name, Value) = 0) then
  begin
    if not (Designer.Form is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.Create(ResStr(SInvalidPropertyValue));
    SetOrdValue(Longint(Designer.Form));
{$ENDIF}
  end
  else
    inherited SetValue(Value);
end;

{ Designer registration }

procedure Register;
begin
{ Components }
  RegisterComponents(LoadStr(srRXTools), [TPicClip, TFormStorage,
    TFormPlacement, TRxWindowHook, TAppEvents, TSpeedbar, TRxCalculator,
    TRxTimerList, TPageManager, TMergeManager, TMRUManager, TSecretPanel,
    TStrHolder, TRxTrayIcon, TRxMainMenu, TRxPopupMenu,
    TRxFolderMonitor, TClipboardViewer,
    TRxGradientCaption, TDualListDialog
    {$IFNDEF RX_D4}, TConverter {$ENDIF}]);

{$IFDEF RX_D3}
  RegisterNonActiveX([TPicClip, TFormPlacement, TFormStorage, TRxWindowHook,
    TDualListDialog, TSecretPanel, TSpeedbar, TClipboardViewer,
    TPageManager, TMergeManager, TMRUManager, TAppEvents, TRxTimerList, 
    TRxTrayIcon, TRxFolderMonitor, TRxGradientCaption], axrComponentOnly);
{$ENDIF RX_D3}

{ TPicClip }
  RegisterComponentEditor(TPicClip, TGraphicsEditor);

{ TStrHolder }
  RegisterComponentEditor(TStrHolder, TStringsEditor);

{ TFormPlacement }
  RegisterPropertyEditor(TypeInfo(TWinMinMaxInfo), TFormPlacement,
    'MinMaxInfo', TMinMaxProperty);

{ TFormStorage }
  RegisterComponentEditor(TFormStorage, TFormStorageEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TFormStorage, 'StoredProps',
    TStoredPropsProperty);

{ TRxWindowHook }
  RegisterPropertyEditor(TypeInfo(TWinControl), TRxWindowHook,
    'WinControl', TComponentFormProperty);

{ TSpeedbar }
  RegisterNoIcon([TSpeedItem, TSpeedbarSection]);
  RegisterComponentEditor(TSpeedbar, TSpeedbarCompEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TSpeedItem, 'BtnCaption', THintProperty);

{ TPageManager }
  RegisterNoIcon([TPageProxy]);
  RegisterComponentEditor(TPageManager, TPageManagerEditor);
  RegisterPropertyEditor(TypeInfo(TList), TPageManager, 'PageProxies',
    TProxyListProperty);
  RegisterPropertyEditor(TypeInfo(string), TPageProxy, 'PageName',
    TPageNameProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TPageManager, 'PriorBtn',
    TPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TPageManager, 'NextBtn',
    TPageBtnProperty);

{ TMergeManager }
  RegisterPropertyEditor(TypeInfo(TWinControl), TMergeManager,
    'MergeFrame', TComponentFormProperty);

{ TRxTimerList }
  RegisterNoIcon([TRxTimerEvent]);
  RegisterComponentEditor(TRxTimerList, TTimersCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TList), TRxTimerList, 'Events',
    TTimersItemListProperty);

{ TRxTrayIcon }
  RegisterPropertyEditor(TypeInfo(TIconList), nil, '', TIconListProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxTrayIcon, 'Hint',
    TStringProperty);
{$IFDEF RX_D4}

{ RxMenus }
  RegisterPropertyEditor(TypeInfo(Boolean), TRxMainMenu, 'OwnerDraw', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TRxPopupMenu, 'OwnerDraw', nil);
{$ENDIF}

{$IFDEF USE_RX_GIF}
{ TRxGIFAnimator }
  RegisterComponentEditor(TRxGIFAnimator, TGraphicsEditor);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TPictProperty);
  RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TGraphicPropertyEditor);
  RegisterComponentEditor(TImage, TGraphicsEditor);

{ TRxGradientCaption }
  RegisterComponentEditor(TRxGradientCaption, TGradientCaptionEditor);
{$IFNDEF RX_D3}
  RegisterPropertyEditor(TypeInfo(TRxCaptionList), TRxGradientCaption, '',
    TGradientCaptionsProperty);
{$ENDIF}

{$IFDEF RX_D3}
{ Project Resource Expert }
{  RegisterResourceExpert;}
{$ENDIF}
end;

end.