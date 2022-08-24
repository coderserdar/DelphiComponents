unit TB2Reg;

{
  Toolbar2000
  Copyright (C) 1998-2008 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Reg.pas,v 1.32 2008/09/18 19:08:40 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, SysUtils, Classes, Graphics, Controls, Dialogs, ActnList, ImgList,
  {$IFDEF JR_D6} DesignIntf, DesignEditors, VCLEditors, {$ELSE} DsgnIntf, {$ENDIF}
  TB2Toolbar, TB2ToolWindow, TB2Dock, TB2Item, TB2ExtItems, TB2MRU, TB2MDI,
  TB2DsgnItemEditor;

procedure Register;
procedure TBRegisterClasses(const AClasses: array of TPersistentClass);

implementation

{$IFDEF CLR}
{ Delphi.NET doesn't use DCR files for component icons }
{$R 'Icons\TTBBackground.bmp'}
{$R 'Icons\TTBBackground16.bmp'}
{$R 'Icons\TTBDock.bmp'}
{$R 'Icons\TTBDock16.bmp'}
{$R 'Icons\TTBImageList.bmp'}
{$R 'Icons\TTBImageList16.bmp'}
{$R 'Icons\TTBItemContainer.bmp'}
{$R 'Icons\TTBItemContainer16.bmp'}
{$R 'Icons\TTBMDIHandler.bmp'}
{$R 'Icons\TTBMDIHandler16.bmp'}
{$R 'Icons\TTBMRUList.bmp'}
{$R 'Icons\TTBMRUList16.bmp'}
{$R 'Icons\TTBPopupMenu.bmp'}
{$R 'Icons\TTBPopupMenu16.bmp'}
{$R 'Icons\TTBToolbar.bmp'}
{$R 'Icons\TTBToolbar16.bmp'}
{$R 'Icons\TTBToolWindow.bmp'}
{$R 'Icons\TTBToolWindow16.bmp'}
{$ENDIF}

uses
  {$IFDEF CLR} WinUtils, {$ENDIF}
  ImgEdit;

{$IFDEF JR_D5}

{ TTBImageIndexPropertyEditor }

{ Unfortunately TComponentImageIndexPropertyEditor seems to be gone in
  Delphi 6, so we have to use our own image index property editor class } 

type
  TTBImageIndexPropertyEditor = class(TIntegerProperty
    {$IFDEF JR_D6} , ICustomPropertyListDrawing {$ENDIF})
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;

    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); {$IFNDEF JR_D6} override; {$ENDIF}
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); {$IFNDEF JR_D6} override; {$ENDIF}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); {$IFNDEF JR_D6} override; {$ENDIF}
  end;

function TTBImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TTBImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
begin
  Result := nil;
end;

procedure TTBImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  ImgList: TCustomImageList;
  I: Integer;
begin
  ImgList := GetImageListAt(0);
  if Assigned(ImgList) then
    for I := 0 to ImgList.Count-1 do
      Proc(IntToStr(I));
end;

procedure TTBImageIndexPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ImgList: TCustomImageList;
  X: Integer;
begin
  ImgList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  X := ARect.Left + 2;
  if Assigned(ImgList) then begin
    ImgList.Draw(ACanvas, X, ARect.Top + 2, StrToInt(Value));
    Inc(X, ImgList.Width);
  end;
  ACanvas.TextOut(X + 3, ARect.Top + 1, Value);
end;

procedure TTBImageIndexPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(ImgList) and (ImgList.Height + 4 > AHeight) then
    AHeight := ImgList.Height + 4;
end;

procedure TTBImageIndexPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(ImgList) then
    Inc(AWidth, ImgList.Width);
end;

{ TTBItemImageIndexPropertyEditor }

type
  TTBItemImageIndexPropertyEditor = class(TTBImageIndexPropertyEditor)
  public
    function GetImageListAt(Index: Integer): TCustomImageList; override;
  end;

function TTBItemImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  C: TPersistent;
  Item: TTBCustomItem;
begin
  Result := nil;
  { ? I'm guessing that the Index parameter is a component index (one that
    would be passed to the GetComponent function). }
  C := GetComponent(Index);
  if C is TTBCustomItem then begin
    Item := TTBCustomItem(C);
    repeat
      Result := Item.Images;
      if Assigned(Result) then
        Break;
      Item := Item.Parent;
      if Item = nil then
        Break;
      Result := Item.SubMenuImages;
    until Assigned(Result);
  end;
end;

{$ENDIF}

{ TTBImageListEditor }

type
  TTBImageListEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

procedure TTBImageListEditor.Edit;
var
  ImgList: TTBImageList;
begin
  ImgList := Component as TTBImageList;
  if not ImgList.ImagesBitmap.Empty then begin
    if MessageDlg('The image list''s ImagesBitmap property has ' +
       'a bitmap assigned. Because of this, any changes you make in the ' +
       'Image List Editor will not be preserved when the form is saved.'#13#10#13#10 +
       'Do you want to open the editor anyway?', mtWarning,
       [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;
  EditImageList(ImgList);
end;

procedure TTBImageListEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit;
end;

function TTBImageListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TTBImageListEditor.GetVerb(Index: Integer): String;
begin
  if Index = 0 then
    Result := 'ImageList Editor...'
  else
    Result := '';
end;


procedure TBRegisterClasses(const AClasses: array of TPersistentClass);
{$IFDEF CLR}
var
  I: Integer;
  FoundClass: TPersistentClass;
{$ENDIF}
begin
  {$IFDEF CLR}
  { Hack for Delphi.NET (2006): If you recompile an already-installed package
    the IDE doesn't unload the old package before installing the new one.
    Therefore, we must search for and unregister any existing classes before
    registering new ones, to avoid having two incompatible sets of classes
    registered at the same time.
    Without this, if we rebuild tb2kdsgn_dn10 (which implicitly reloads
    tb2k_dn10) and then attempt to open the Demo project's main form in the
    IDE, we get a "Toolbar item cannot be inserted into container of type
    TTBToolbar" exception inside TTBCustomItem.SetParentComponent, because
    apparently the TTBToolbar class it's trying to use is located in the new
    assembly, while the item class is located in the old assembly.
    Note: It appears that this issue only affects registered classes; there
    is no need for an "UnRegisterComponents" call. }
  for I := High(AClasses) downto Low(AClasses) do begin
    { Unregister all classes with the same name }
    while True do begin
      FoundClass := GetClass(AClasses[I].ClassName);
      if FoundClass = nil then
        Break;
      UnRegisterClass(FoundClass);
    end;
  end;
  {$ENDIF}
  RegisterClasses(AClasses);
end;

procedure Register;
begin
  { Note: On Delphi.NET 2006, it's possible for this procedure to be called
    a second time on the same tb2kdsgn instance. See comments in
    TBRegisterItemClass. }

  RegisterComponents('Toolbar2000', [TTBDock, TTBToolbar, TTBToolWindow,
    TTBPopupMenu, TTBImageList, TTBItemContainer, TTBBackground, TTBMRUList,
    TTBMDIHandler]);
  {$IFDEF JR_D4}
  RegisterActions('', [TTBEditAction], nil);
  {$ENDIF}
  RegisterNoIcon([TTBCustomItem]);
  TBRegisterClasses([TTBItem, TTBGroupItem, TTBSubmenuItem, TTBSeparatorItem,
    TTBEditItem, TTBMRUListItem, TTBControlItem, TTBMDIWindowItem,
    TTBVisibilityToggleItem]);

  RegisterComponentEditor(TTBCustomToolbar, TTBItemsEditor);
  RegisterComponentEditor(TTBItemContainer, TTBItemsEditor);
  RegisterComponentEditor(TTBPopupMenu, TTBItemsEditor);
  RegisterComponentEditor(TTBImageList, TTBImageListEditor);
  RegisterPropertyEditor(TypeInfo(TTBRootItem), nil, '', TTBItemsPropertyEditor);
  {$IFDEF JR_D5}
  RegisterPropertyEditor(TypeInfo(TImageIndex), TTBCustomItem, 'ImageIndex',
    TTBItemImageIndexPropertyEditor);
  {$ENDIF}
  {$IFDEF JR_D6}
  { TShortCut properties show up like Integer properties in Delphi 6
    without this... }
  RegisterPropertyEditor(TypeInfo(TShortCut), TTBCustomItem, '',
    TShortCutProperty);
  {$ENDIF}

  { Link in images for the toolbar buttons }
  {$IFNDEF CLR}
    {$R TB2DsgnItemEditor.res}
  {$ELSE}
    {$R 'Icons\TB2DsgnEditorImages.bmp'}
    {$R 'Icons\TTBEditItem.bmp'}
    {$R 'Icons\TTBGroupItem.bmp'}
    {$R 'Icons\TTBMDIWindowItem.bmp'}
    {$R 'Icons\TTBMRUListItem.bmp'}
  {$ENDIF}
  TBRegisterItemClass(TTBEditItem, 'New &Edit', HInstance);
  TBRegisterItemClass(TTBGroupItem, 'New &Group Item', HInstance);
  TBRegisterItemClass(TTBMRUListItem, 'New &MRU List Item', HInstance);
  TBRegisterItemClass(TTBMDIWindowItem, 'New MDI &Windows List', HInstance);
  TBRegisterItemClass(TTBVisibilityToggleItem, 'New &Visibility-Toggle Item', HInstance);
end;

end.
