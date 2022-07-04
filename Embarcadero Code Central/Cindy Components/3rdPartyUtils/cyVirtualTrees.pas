{   Unit cyDebenuPDFLibrary

    Description:
    Unit with functions to use with VirtualTree components: http://www.soft-gems.net/index.php/controls/virtual-treeview.

    Tips:
      Add [toPopupMode] to TreeOptions.PaintOptions to always paint selection background !

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyVirtualTrees;

interface

uses Classes, SysUtils, Windows, cyStrUtils, Dialogs, VirtualTrees;

type
  TGetNodeTextEvent = procedure (Node: Pointer; var ResultText: String);

procedure VirtualStringTree_ChildrenExpanded(VirtualST: TBaseVirtualTree; ParentNode: PVirtualNode; const Expanded: Boolean);
procedure VirtualStringTree_HandleKey(VirtualST: TVirtualStringTree; var Key: Word; Shift: TShiftState);
procedure VirtualStringTree_Filter(VirtualST: TVirtualStringTree; const Input: string; GetNodeTextEvent: TGetNodeTextEvent; const StrFilterOptions: TStrFilterOptions = [strfoCaseInsensitive, strfoAccentsInsensitive]; const IgnoreFocusedNode: Boolean = false);
function VirtualStringTree_GetNextVisibleFromSameLevel(VirtualST: TVirtualStringTree; FromNode: PVirtualNode): PVirtualNode;
function VirtualStringTree_GetPriorVisibleFromSameLevel(VirtualST: TVirtualStringTree; FromNode: PVirtualNode): PVirtualNode;
procedure VirtualStringTree_DeleteNode(VirtualST: TVirtualStringTree; Node: PVirtualNode; const DeleteParentMinLevelIfNoChildren: Integer = 0);
procedure VirtualStringTree_SetChildsNodeCheckState(VirtualST: TVirtualStringTree; ParentNode: PVirtualNode; const Value: VirtualTrees.TCheckState);
procedure VirtualStringTree_InheritsChildNodesCheckState(VirtualST: TVirtualStringTree; ParentNode: PVirtualNode; const DeactivateOnCheckedEvent: Boolean = true);
implementation

procedure VirtualStringTree_ChildrenExpanded(VirtualST: TBaseVirtualTree; ParentNode: PVirtualNode; const Expanded: Boolean);
var
  i: Integer;
  ChildNode: PVirtualNode;
begin
  if not Assigned(ParentNode) then Exit;

  ChildNode := VirtualST.GetFirstChild(ParentNode);

  for i := 0 to ParentNode.ChildCount-1 do
  begin
    VirtualST.Expanded[ChildNode] := false;    // Will call OnCollapsed event !
    ChildNode := VirtualST.GetNextSibling(ChildNode);
  end;
end;

procedure VirtualStringTree_HandleKey(VirtualST: TVirtualStringTree; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  if Shift = [ssCtrl] then
    if Key = VK_HOME   then    // Ctrl + HOME ...
    begin
      Key := 0;
      VirtualST.FocusedNode := VirtualST.GetFirstVisible;
    end
    else
      if Key = VK_END then   // Ctrl + END ...
      begin
        Key := 0;
        VirtualST.FocusedNode := VirtualST.GetLastVisible;
      end;

  if Key = VK_PRIOR then // PAGE UP ...
  begin
    Key := 0;
    VirtualST.BeginUpdate;

    for i := 1 to VirtualST.ClientHeight div VirtualST.DefaultNodeHeight - 1 do
      if Assigned(VirtualST.GetPreviousVisible(VirtualST.FocusedNode))
      then VirtualST.FocusedNode := VirtualST.GetPreviousVisible(VirtualST.FocusedNode)
      else Break;

    VirtualST.EndUpdate;
  end
  else
    if Key = VK_NEXT then // PAGE DOWN ...
    begin
      Key := 0;
      VirtualST.BeginUpdate;

      for i := 1 to VirtualST.ClientHeight div VirtualST.DefaultNodeHeight - 1 do
        if Assigned(VirtualST.GetNextVisible(VirtualST.FocusedNode))
        then VirtualST.FocusedNode := VirtualST.GetNextVisible(VirtualST.FocusedNode)
        else Break;

      VirtualST.EndUpdate;
    end
    else
      if Key = VK_DOWN then
      begin
        Key := 0;
        if Assigned(VirtualST.FocusedNode)
        then VirtualST.FocusedNode := VirtualST.GetNextVisible(VirtualST.FocusedNode)
        else VirtualST.FocusedNode := VirtualST.GetFirstVisible(Nil);
      end
      else
        if Key = VK_UP then
        begin
          Key := 0;
          if Assigned(VirtualST.FocusedNode)
          then VirtualST.FocusedNode := VirtualST.GetPreviousVisible(VirtualST.FocusedNode)
          else VirtualST.FocusedNode := VirtualST.GetLastVisible(Nil);
        end;

  if Assigned(VirtualST.FocusedNode) then
    VirtualST.Selected[VirtualST.FocusedNode] := true;
end;

procedure VirtualStringTree_Filter(VirtualST: TVirtualStringTree; const Input: string; GetNodeTextEvent: TGetNodeTextEvent; const StrFilterOptions: TStrFilterOptions = [strfoCaseInsensitive, strfoAccentsInsensitive]; const IgnoreFocusedNode: Boolean = false);
var
  ParentNode, CurrentNode: PVirtualNode;
  NodeText: String;
  NodeVisible: Boolean;

      procedure HandleChildsNode(ParentNode: PVirtualNode; var RsltAnyChildVisible: Boolean);
      var
        ParentNodeLevel, CurrentNodeLevel: Integer;
        ChildNode: PVirtualNode;
        ChildVisible: Boolean;
      begin
        RsltAnyChildVisible := false;
        ParentNodeLevel := VirtualST.GetNodeLevel(ParentNode);
        CurrentNode := VirtualST.GetNext(ParentNode);

        while Assigned(CurrentNode) do
        begin
          CurrentNodeLevel := VirtualST.GetNodeLevel(CurrentNode);

          if CurrentNodeLevel > ParentNodeLevel then
          begin
            ChildNode := CurrentNode;
            HandleChildsNode(CurrentNode, ChildVisible);

            if not ChildVisible then
              if Input <> '' then
              begin
                if IgnoreFocusedNode then
                  if VirtualST.FocusedNode = ChildNode then
                    ChildVisible := true;

                if not ChildVisible then
                begin
                  GetNodeTextEvent(VirtualST.GetNodeData(ChildNode), NodeText);
                  ChildVisible := String_MatchInput(NodeText, Input, StrFilterOptions);
                end;
              end
              else
                ChildVisible := true;

            VirtualST.IsVisible[ChildNode] := ChildVisible;

            RsltAnyChildVisible := RsltAnyChildVisible or ChildVisible;
          end
          else
            Break;
        end;
      end;


begin
  VirtualST.BeginUpdate;

  ParentNode := VirtualST.GetFirst;

  while Assigned(ParentNode) do
  begin
    HandleChildsNode(ParentNode, NodeVisible);

    if not NodeVisible then
      if Input <> '' then
      begin
        if IgnoreFocusedNode then
          if VirtualST.FocusedNode = ParentNode then // if VirtualST.Selected[ParentNode] then
            NodeVisible := true;

        if not NodeVisible then
        begin
          GetNodeTextEvent(VirtualST.GetNodeData(ParentNode), NodeText);
          NodeVisible := String_MatchInput(NodeText, Input, StrFilterOptions);
        end;
      end
      else
        NodeVisible := true;

    VirtualST.IsVisible[ParentNode] := NodeVisible;

    ParentNode := CurrentNode;
  end;

  VirtualST.EndUpdate;
end;

function VirtualStringTree_GetNextVisibleFromSameLevel(VirtualST: TVirtualStringTree; FromNode: PVirtualNode): PVirtualNode;
var
  Level: Integer;
begin
  Result := Nil;
  if not Assigned(FromNode) then
    Exit;

  Level := VirtualST.GetNodeLevel(FromNode);
  Result := VirtualST.GetNextVisible(FromNode);

  while Assigned(Result) do
  begin
    if VirtualST.GetNodeLevel(Result) = Level
    then Break
    else Result := VirtualST.GetNextVisible(Result);
  end;
end;

function VirtualStringTree_GetPriorVisibleFromSameLevel(VirtualST: TVirtualStringTree; FromNode: PVirtualNode): PVirtualNode;
var
  Level: Integer;
begin
  Result := Nil;
  if not Assigned(FromNode) then
    Exit;

  Level := VirtualST.GetNodeLevel(FromNode);
  Result := VirtualST.GetPreviousVisible(FromNode);

  while Assigned(Result) do
  begin
    if VirtualST.GetNodeLevel(Result) = Level
    then Break
    else Result := VirtualST.GetNextVisible(Result);
  end;
end;

procedure VirtualStringTree_DeleteNode(VirtualST: TVirtualStringTree; Node: PVirtualNode; const DeleteParentMinLevelIfNoChildren: Integer = 0);
var
  ParentNode: PVirtualNode;
  ParentNodeLevel: Integer;
begin
  if not Assigned(Node) then
    Exit;

  if DeleteParentMinLevelIfNoChildren >= 0 then
  begin
    // ERROR !!! We need to pass by a variable ... don' t know why ... if VirtualST.GetNodeLevel(Node) - 1 >= DeleteParentMinLevelIfNoChildren
    ParentNodeLevel := VirtualST.GetNodeLevel(Node) - 1;

    if ParentNodeLevel >= DeleteParentMinLevelIfNoChildren     // !!! BUG VirtualTree : Node.Parent never unassigned !!!
    then ParentNode := Node.Parent
    else ParentNode := Nil;
  end;

  VirtualST.DeleteNode(Node);

  // Remove parent :
  if DeleteParentMinLevelIfNoChildren >= 0 then
    if Assigned(ParentNode) then
      if ParentNode.ChildCount = 0 then
        VirtualStringTree_DeleteNode(VirtualST, ParentNode, DeleteParentMinLevelIfNoChildren);
end;

// Check/Uncheck childs as parent :
procedure VirtualStringTree_SetChildsNodeCheckState(VirtualST: TVirtualStringTree; ParentNode: PVirtualNode; const Value: VirtualTrees.TCheckState);
var
  n: Integer;
  ChildNode: PVirtualNode;
begin
  for n := 0 to ParentNode.ChildCount - 1 do
  begin
    ChildNode := VirtualST.GetFirstChild(ParentNode);


    while Assigned(ChildNode) do
    begin
      if VirtualST.CheckState[ChildNode] <> Value then
        VirtualST.CheckState[ChildNode] := Value;      // TVirtualStringTree.OnChecked will be called !!!

      ChildNode := VirtualST.GetNextSibling(ChildNode);
    end;
  end;
end;

procedure VirtualStringTree_InheritsChildNodesCheckState(VirtualST: TVirtualStringTree; ParentNode: PVirtualNode; const DeactivateOnCheckedEvent: Boolean = true);
var
  ChildNode: PVirtualNode;
  ValueInitialized: Boolean;
  Value: VirtualTrees.TCheckState;
  SavOnChecked: TVTChangeEvent;
begin
  ValueInitialized := false;
  ChildNode := VirtualST.GetFirstChild(ParentNode);

  while Assigned(ChildNode) do
  begin
    if not ValueInitialized then
    begin
      Value := VirtualST.CheckState[ChildNode];
      ValueInitialized := true;
    end
    else
      if Value in [csCheckedNormal, csCheckedPressed] <> (VirtualST.CheckState[ChildNode] in [csCheckedNormal, csCheckedPressed]) then
      begin
        Value := csUncheckedNormal;    // Always Unckeck if not all childs checked
        Break;
      end;

    ChildNode := VirtualST.GetNextSibling(ChildNode);
  end;

  if ValueInitialized then
  begin
    if DeactivateOnCheckedEvent then
    begin
      SavOnChecked := VirtualST.OnChecked;
      VirtualST.OnChecked := Nil;
    end;

    if VirtualST.CheckState[ParentNode] in [csCheckedNormal, csCheckedPressed] <> (Value in [csCheckedNormal, csCheckedPressed]) then
      VirtualST.CheckState[ParentNode] := Value;

    if DeactivateOnCheckedEvent then
      VirtualST.OnChecked := SavOnChecked;
  end;
end;

end.
