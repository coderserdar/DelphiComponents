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

{$I fsdefine.inc}

Unit fsclcoln;

Interface

Uses
  DB,
  {$IFNDEF DCC4OrLater}
  DBTables,
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, fsexfield,
  {$IFDEF DCC6OrLater}
  DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  StdCtrls;

Type
  TfsParamEditor = Class(TForm)
    lbItems: TListBox;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure lbItemsClick(Sender: TObject);
  Private
    { Private declarations }
    FParams: TfsParams;
    { The collection being edited. }
    FComponent: TComponent;
    { The component with which this editor is associated. }

    {$IFDEF DCC4OrLater}
    FDesigner: IDesigner;
    {$ELSE}
    FDesigner: TDesigner;
    {$ENDIF}

    FPropName: String;
    { The property with which this editor is associated. }

    Function GeTfsParams: Longint;
    Procedure SeTfsParams(anOrdValue: Longint);

  Protected

    Procedure FillList; Virtual;

    {$IFDEF DCC6OrLater}
    Procedure SelectComponentList(SelList: IDesignerSelections);
    {$ELSE}
    {$IFDEF DCC5OrLater}
    Procedure SelectComponentList(SelList: TDesignerSelectionList);
    {$ELSE}
    Procedure SelectComponentList(SelList: TComponentList);
    {$ENDIF}
    {$ENDIF}
    Procedure SelectComponent(Component: TComponent);

  Public
    { Public declarations }

    Property Collection: Longint Read GeTfsParams Write SeTfsParams;
    {$IFDEF DCC4OrLater}
    Property CompDesigner: IDesigner Read FDesigner Write FDesigner;
    {$ELSE}
    Property CompDesigner: TDesigner Read FDesigner Write FDesigner;
    {$ENDIF}
    Property Component: TComponent Read FComponent Write FComponent;
    Property PropertyName: String Read FPropName Write FPropName;
  End;

  {$IFDEF DCC4OrLater}
Procedure FSShowParamEditor(aDesigner: IDesigner;
  aComponent: TComponent;
  aPropertyName: String;
  aCollection: Longint);
{$ELSE}
Procedure FSShowParamEditor(aDesigner: TDesigner;
  aComponent: TComponent;
  aPropertyName: String;
  aCollection: Longint);
{$ENDIF}

Var
  fsParamEditor: TfsParamEditor;

Implementation

{$R *.DFM}

Const
  ffcEditing = 'Editing %s.%s';

Var
  FFParamsEditors: TList = Nil;
  { The list of active collection editors.  We need to track the active
    collection editors because the user may go back to the Object Inspector
    and click the property again.  In that case, we want to bring up the
    existing collection editor instead of creating a new collection editor. }

{===Utility routines=================================================}
  {$IFDEF DCC4OrLater}

Procedure FSShowParamEditor(aDesigner: IDesigner;
  aComponent: TComponent;
  aPropertyName: String;
  aCollection: Longint);
{$ELSE}

Procedure FSShowParamEditor(aDesigner: TDesigner;
  aComponent: TComponent;
  aPropertyName: String;
  aCollection: Longint);
{$ENDIF}
Var
  anEditor: TfsParamEditor;
  Index: Integer;
Begin
  { Are there any existing collection editors? }
  If assigned(FFParamsEditors) Then
    { Yes.  See if an editor was already created for this property. }
    For Index := 0 To pred(FFParamsEditors.Count) Do
      Begin
        anEditor := TfsParamEditor(FFParamsEditors.Items[Index]);
        With anEditor Do
          Begin
            If (CompDesigner = aDesigner) And
              (Component = aComponent) And
              (Collection = aCollection) And
              (CompareText(PropertyName, aPropertyName) = 0) Then
              Begin
                anEditor.Show;
                anEditor.BringToFront;
                Exit;
              End;
          End;
      End
  Else
    FFParamsEditors := TList.Create;

  { If we have reached this point, there is no collection editor for this
    collection.  Create a new collection editor. }
  With TfsParamEditor.Create(Application) Do
    Try
      Collection := aCollection;
      Component := aComponent;
      CompDesigner := aDesigner;
      PropertyName := aPropertyName;
      Show;
    Except
      Free;
    End;

End;
{====================================================================}

{===TfsParamEditor==============================================}

Procedure TfsParamEditor.FormCreate(Sender: TObject);
Begin
  FParams := Nil;
  FComponent := Nil;
  FDesigner := Nil;
  FPropName := '';
  FFParamsEditors.Add(Self);
End;
{--------}

Procedure TfsParamEditor.FormDestroy(Sender: TObject);
Begin
  If assigned(FComponent) Then
    SelectComponent(FComponent);

  If assigned(FFParamsEditors) Then
    FFParamsEditors.Remove(Self);
End;
{--------}

Procedure TfsParamEditor.FormShow(Sender: TObject);
Begin
  Caption := format(ffcEditing, [FComponent.Name, FPropName]);
  FillList;
End;
{--------}

Procedure TfsParamEditor.FormClose(Sender: TObject;
  Var Action: TCloseAction);
Begin
  If assigned(FComponent) Then
    SelectComponent(FComponent);

  Action := caFree;
End;
{--------}

Function TfsParamEditor.GeTfsParams: Longint;
Begin
  Result := Longint(FParams);
End;
{--------}

Procedure TfsParamEditor.SeTfsParams(anOrdValue: Longint);
Begin
  FParams := TfsParams(anOrdValue);
End;
{--------}
{$IFDEF DCC6OrLater}

Procedure TfsParamEditor.SelectComponentList(SelList: IDesignerSelections);
{$ELSE}
{$IFDEF DCC5OrLater}

Procedure TfsParamEditor.SelectComponentList(SelList: TDesignerSelectionList);
{$ELSE}

Procedure TfsParamEditor.SelectComponentList(SelList: TComponentList);
{$ENDIF}
{$ENDIF}
Begin
  If assigned(FDesigner) Then
    {$IFDEF DCC6OrLater}
    FDesigner.SetSelections(SelList);
  {$ELSE}
    {$IFDEF DCC4OrLater}
    (FDesigner As IFormDesigner).SetSelections(SelList);
  {$ELSE}
    (FDesigner As TFormDesigner).SetSelections(SelList);
  {$ENDIF}
  SelList.Free;
  {$ENDIF}
End;
{--------}

Procedure TfsParamEditor.SelectComponent(Component: TComponent);
Var
  {$IFDEF DCC6OrLater}
  SelList: IDesignerSelections;
  {$ELSE}
  {$IFDEF DCC5OrLater}
  SelList: TDesignerSelectionList;
  {$ELSE}
  SelList: TComponentList;
  {$ENDIF}
  {$ENDIF}
Begin
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
End;
{--------}

Procedure TfsParamEditor.FillList;
Var
  Index: Integer;
Begin

  lbItems.Clear;
  lbItems.ItemIndex := -1;

  For Index := 0 To pred(FParams.Count) Do
    lbItems.Items.AddObject(
      IntToStr(Index) + ' - ' +
      {$IFDEF DCC4OrLater}
      TParam(FParams.Items[Index]).DisplayName,
      {$ELSE}
      TParam(FParams.Items[Index]).Name,
      {$ENDIF}
      FParams.Items[Index])

End;
{--------}

Procedure TfsParamEditor.lbItemsClick(Sender: TObject);
Var
  {$IFDEF DCC6OrLater}
  SelList: IDesignerSelections;
  {$ELSE}
  {$IFDEF DCC5OrLater}
  SelList: TDesignerSelectionList;
  {$ELSE}
  SelList: TComponentList;
  {$ENDIF}
  {$ENDIF}
  Index: Integer;
Begin
  {$IFDEF DCC6OrLater}
  SelList := TDesignerSelections.Create;
  {$ELSE}
  {$IFDEF DCC5OrLater}
  SelList := TDesignerSelectionList.Create;
  {$ELSE}
  SelList := TComponentList.Create;
  {$ENDIF}
  {$ENDIF}
  For Index := 0 To pred(lbItems.Items.Count) Do
    If lbItems.Selected[Index] Then
      SelList.Add(TComponent(lbItems.Items.Objects[Index]));

  If SelList.Count > 0 Then
    SelectComponentList(SelList)
  Else
    SelectComponent(FComponent);

End;
{====================================================================}

Initialization

Finalization
  FFParamsEditors.Free;
  FFParamsEditors := Nil;
End.

