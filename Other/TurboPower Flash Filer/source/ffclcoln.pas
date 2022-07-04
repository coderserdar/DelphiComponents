{*********************************************************}
{* FlashFiler: Collection property editor                *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffclcoln;

interface

uses
  DB,
  {$IFNDEF DCC4OrLater}
  DBTables,
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF DCC6OrLater}
  DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  StdCtrls;

type
  TffParamEditor = class(TForm)
    lbItems: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbItemsClick(Sender: TObject);
  private
    { Private declarations }
    FParams : TParams;
      { The collection being edited. }
    FComponent : TComponent;
      { The component with which this editor is associated. }

    {$IFDEF DCC4OrLater}
    FDesigner : IDesigner;
    {$ELSE}
    FDesigner : TDesigner;
    {$ENDIF}

    FPropName : string;
      { The property with which this editor is associated. }

    function GetParams : longInt;
    procedure SetParams(anOrdValue : longInt);

  protected

    procedure FillList; virtual;


    {$IFDEF DCC6OrLater}
    procedure SelectComponentList(SelList : IDesignerSelections);
    {$ELSE}
    {$IFDEF DCC5OrLater}
    procedure SelectComponentList(SelList : TDesignerSelectionList);
    {$ELSE}
    procedure SelectComponentList(SelList : TComponentList);
    {$ENDIF}
    {$ENDIF}
    procedure SelectComponent(Component : TComponent);

  public
    { Public declarations }

    property Collection : longInt read GetParams write SetParams;
    {$IFDEF DCC4OrLater}
    property CompDesigner : IDesigner read FDesigner write FDesigner;
    {$ELSE}
    property CompDesigner : TDesigner read FDesigner write FDesigner;
    {$ENDIF}
    property Component : TComponent read FComponent write FComponent;
    property PropertyName : string read FPropName write FPropName;
  end;

{$IFDEF DCC4OrLater}
  procedure FFShowParamEditor(aDesigner : IDesigner;
                              aComponent : TComponent;
                              aPropertyName : string;
                              aCollection : longInt);
{$ELSE}
  procedure FFShowParamEditor(aDesigner : TDesigner;
                              aComponent : TComponent;
                              aPropertyName : string;
                              aCollection : longInt);
{$ENDIF}

var
  ffParamEditor: TffParamEditor;

implementation

{$R *.DFM}

const
  ffcEditing = 'Editing %s.%s';

var
  FFParamsEditors : TList = nil;
    { The list of active collection editors.  We need to track the active
      collection editors because the user may go back to the Object Inspector
      and click the property again.  In that case, we want to bring up the
      existing collection editor instead of creating a new collection editor. }

{===Utility routines=================================================}
{$IFDEF DCC4OrLater}
procedure FFShowParamEditor(aDesigner : IDesigner;
                            aComponent : TComponent;
                            aPropertyName : string;
                            aCollection : longInt);
{$ELSE}
procedure FFShowParamEditor(aDesigner : TDesigner;
                            aComponent : TComponent;
                            aPropertyName : string;
                            aCollection : longInt);
{$ENDIF}
var
  anEditor : TffParamEditor;
  Index : integer;
begin
  { Are there any existing collection editors? }
  if assigned(FFParamsEditors) then
    { Yes.  See if an editor was already created for this property. }
    for Index := 0 to pred(FFParamsEditors.Count) do begin
      anEditor := TffParamEditor(FFParamsEditors.Items[Index]);
      with anEditor do begin
        if (CompDesigner = aDesigner) and
           (Component = aComponent) and
           (Collection = aCollection) and
           (CompareText(PropertyName, aPropertyName) = 0) then begin
          anEditor.Show;
          anEditor.BringToFront;
          Exit;
        end;
      end;
    end
  else
    FFParamsEditors := TList.Create;

  { If we have reached this point, there is no collection editor for this
    collection.  Create a new collection editor. }
  with TffParamEditor.Create(Application) do
    try
      Collection := aCollection;
      Component := aComponent;
      CompDesigner := aDesigner;
      PropertyName := aPropertyName;
      Show;
    except
      Free;
    end;

end;
{====================================================================}

{===TffParamEditor==============================================}
procedure TffParamEditor.FormCreate(Sender: TObject);
begin
  FParams := nil;
  FComponent := nil;
  FDesigner := nil;
  FPropName := '';
  FFParamsEditors.Add(Self);
end;
{--------}
procedure TffParamEditor.FormDestroy(Sender: TObject);
begin
  if assigned(FComponent) then
    SelectComponent(FComponent);

  if assigned(FFParamsEditors) then
    FFParamsEditors.Remove(Self);
end;
{--------}
procedure TffParamEditor.FormShow(Sender: TObject);
begin
  Caption := format(ffcEditing, [FComponent.Name, FPropName]);
  FillList;
end;
{--------}
procedure TffParamEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if assigned(FComponent) then
    SelectComponent(FComponent);

  Action := caFree;
end;
{--------}
function TffParamEditor.GetParams : longInt;
begin
  Result := longInt(FParams);
end;
{--------}
procedure TffParamEditor.SetParams(anOrdValue : longInt);
begin
  FParams := TParams(anOrdValue);
end;
{--------}
{$IFDEF DCC6OrLater}
procedure TffParamEditor.SelectComponentList(SelList : IDesignerSelections);
{$ELSE}
{$IFDEF DCC5OrLater}
procedure TffParamEditor.SelectComponentList(SelList : TDesignerSelectionList);
{$ELSE}
procedure TffParamEditor.SelectComponentList(SelList : TComponentList);
{$ENDIF}
{$ENDIF}
begin
  if assigned(FDesigner) then
  {$IFDEF DCC6OrLater}
    FDesigner.SetSelections(SelList);
  {$ELSE}
    {$IFDEF DCC4OrLater}
      (FDesigner as IFormDesigner).SetSelections(SelList);
    {$ELSE}
      (FDesigner as TFormDesigner).SetSelections(SelList);
    {$ENDIF}
    SelList.Free;
  {$ENDIF}
end;
{--------}
procedure TffParamEditor.SelectComponent(Component : TComponent);
var
  {$IFDEF DCC6OrLater}
  SelList : IDesignerSelections;
  {$ELSE}
    {$IFDEF DCC5OrLater}
    SelList : TDesignerSelectionList;
    {$ELSE}
    SelList : TComponentList;
    {$ENDIF}
  {$ENDIF}
begin
  {$IFDEF DCC6OrLater}
  SelList := TDesignerSelections.Create;
  {$ELSE}
    {$IFDEF DCC5OrLater}
    SelList := TDesignerSelectionList.Create;
    {$ELSE}
    SelList := TComponentList.Create;
    {$ENDIF}
  {$ENDIF}
  SelList.Add(Component);
  SelectComponentList(SelList);
end;
{--------}
procedure TffParamEditor.FillList;
var
  Index : Integer;
begin

  lbItems.Clear;
  lbItems.ItemIndex := -1;

  for Index := 0 to pred(FParams.Count) do
    lbItems.Items.AddObject(
      IntToStr(Index) + ' - ' +
               {$IFDEF DCC4OrLater}
               TParam(FParams.Items[Index]).DisplayName,
               {$ELSE}
               TParam(FParams.Items[Index]).Name,
               {$ENDIF}
      FParams.Items[Index])

end;
{--------}
procedure TffParamEditor.lbItemsClick(Sender: TObject);
var
  {$IFDEF DCC6OrLater}
  SelList : IDesignerSelections;
  {$ELSE}
    {$IFDEF DCC5OrLater}
    SelList : TDesignerSelectionList;
    {$ELSE}
    SelList : TComponentList;
    {$ENDIF}
  {$ENDIF}
  Index : Integer;
begin
  {$IFDEF DCC6OrLater}
  SelList := TDesignerSelections.Create;
  {$ELSE}
    {$IFDEF DCC5OrLater}
    SelList := TDesignerSelectionList.Create;
    {$ELSE}
    SelList := TComponentList.Create;
    {$ENDIF}
  {$ENDIF}
  for Index := 0 to pred(lbItems.Items.Count) do
    if lbItems.Selected[Index] then
      SelList.Add(TComponent(lbItems.Items.Objects[Index]));

  if SelList.Count > 0 then
    SelectComponentList(SelList)
  else
    SelectComponent(FComponent);

end;
{====================================================================}

initialization

finalization
  FFParamsEditors.Free;
  FFParamsEditors := nil;
end.
