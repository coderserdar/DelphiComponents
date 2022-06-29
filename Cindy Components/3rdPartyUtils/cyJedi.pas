{   Unit cyJedi

    Description:
    Unit with functions to use with Jedi components.

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

unit cyJedi;

interface

uses Classes, SysUtils, Graphics, db, jpeg, ComCtrls, JvJVCLUtils, JvComCtrls;

// ReplaceTMetafileBy allow to replace the TMetafile class detected by TJvDBImage by another GraphicClass because original stream from TDBImage (Bitmaps) are detected
// as TMetafile ...
function GetStreamGraphicClass(aStream: TMemoryStream; const ReplaceTMetafileBy: TGraphicClass = Nil): TGraphicClass;
function GetBlobFieldGraphicClass(aBlobField: TBlobField; const ReplaceTMetafileBy: TGraphicClass = Nil): TGraphicClass;
function AssignBlobFieldToPicture(aBlobField: TBlobField; aPicture: TPicture; const ReplaceTMetafileBy: TGraphicClass = Nil; const ClearIfFieldIsNull: Boolean = true): Boolean;
procedure ConvertToJpeg(aDataset: TDataset; aBlobField: TBlobField; const QualityPercent: Integer = 90);

procedure JvTreeView_SetChildsNodeChecked(aJvTreeView: TJvTreeView; ParentNode: TJvTreeNode; const Value: Boolean);
procedure JvTreeView_InheritsChildNodesChecked(aJvTreeView: TJvTreeView; ParentNode: TTreeNode; const DeactivateOnNodeCheckedChange: Boolean = true);
procedure JvTreeView_InheritsParentNodeEnabled(aJvTreeView: TJvTreeView; ParentNode: TTreeNode);

implementation

function GetStreamGraphicClass(aStream: TMemoryStream; const ReplaceTMetafileBy: TGraphicClass = Nil): TGraphicClass;
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  Result := JvJVCLUtils.GetGraphicClass(Stream);

  if GraphicClass = TMetafile then
    Result := ReplaceTMetafileBy;
end;

function GetBlobFieldGraphicClass(aBlobField: TBlobField; const ReplaceTMetafileBy: TGraphicClass = Nil): TGraphicClass;
var
  Graphic: TGraphic;
  Stream: TMemoryStream;
begin
  Result := Nil;

  if not aBlobField.IsBlob then Exit;
  if aBlobField.IsNull then Exit;

  Stream := TMemoryStream.Create;
  try
    aBlobField.SaveToStream(Stream);
    Result := JvJVCLUtils.GetGraphicClass(Stream);

    if Result = TMetafile then
      Result := ReplaceTMetafileBy;
  finally
    Stream.Free;
  end;
end;

function AssignBlobFieldToPicture(aBlobField: TBlobField; aPicture: TPicture; const ReplaceTMetafileBy: TGraphicClass = Nil; const ClearIfFieldIsNull: Boolean = true): Boolean;
var
  EmptyBmp: TBitmap;
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  Result := false;

  if not aBlobField.IsBlob then Exit;

  if aBlobField.IsNull then
  begin
    // Clear actual graphic :
    if Assigned(aPicture.Graphic) and ClearIfFieldIsNull then
    begin
      EmptyBmp := TBitmap.Create;

      try
        EmptyBmp.Width := 1;
        EmptyBmp.Height := 1;
        aPicture.Graphic.Assign(EmptyBmp);
        aPicture.Bitmap.Modified := true;      // Refresh when image on panel visible by with ManualDock ...
      finally
        EmptyBmp.Free;
      end;
    end;

    Result := true;
    Exit;
  end;

  GraphicClass := GetBlobFieldGraphicClass(aBlobField, ReplaceTMetafileBy);

  Stream := TMemoryStream.Create;
  try
    aBlobField.SaveToStream(Stream);
    GraphicClass := JvJVCLUtils.GetGraphicClass(Stream);

    if GraphicClass = TMetafile then
      GraphicClass := ReplaceTMetafileBy;

    if GraphicClass <> nil then
    begin
      Graphic := GraphicClass.Create;
      try
        Stream.Position := 0;
        Graphic.LoadFromStream(Stream);
        // also works ... aPicture.Graphic := Graphic;
        aPicture.Assign(Graphic);
      finally
        Graphic.Free;
      end;
    end
    else // try the old fashioned way
      aPicture.Assign(aBlobField);

    Result := true;
  finally
    Stream.Free;
  end;
end;

procedure ConvertToJpeg(aDataset: TDataset; aBlobField: TBlobField; const QualityPercent: Integer = 90);
var
  GraphicClass: TGraphicClass;
  aBmp: TBitmap;
  aJpeg: TJPEGImage;
  aStream: TmemoryStream;
begin
  if not aBlobField.IsBlob then Exit;
  if aBlobField.IsNull then Exit;

  aDataset.Edit;

  GraphicClass := GetBlobFieldGraphicClass(aBlobField, Nil);

  // Bug do TJvDBImage que detecta o stream do TDBImage de origem como sendo de tipo TMetafile :
  //if GraphicClass = TMetafile then
  //  GraphicClass := Nil;

  if GraphicClass = Nil then
  begin
    aBmp := TBitmap.Create;

    try
      aBmp.Assign(aBlobField);
      aJpeg := TJPEGImage.Create;

      try
        aJpeg.Assign(aBmp);
        aJpeg.CompressionQuality := QualityPercent;
        aStream := TmemoryStream.Create;

        try
          aJpeg.SaveToStream(aStream);
          aBlobField.LoadFromStream(aStream);
        finally
          aStream.Free;
        end;
      finally
        aJpeg.Free;
      end;
    finally
     aBmp.Free;
    end;
  end;

  if GraphicClass = TBitmap then
  begin
    aBmp := TBitmap.Create;

    try
      aStream := TmemoryStream.Create;

      try
        aBlobField.SaveToStream(aStream);
        aBmp.LoadFromStream(aStream);
      finally
        aStream.Free;
      end;

      aBmp.Assign(aBlobField);
      aJpeg := TJPEGImage.Create;

      try
        aJpeg.Assign(aBmp);
        aJpeg.CompressionQuality := QualityPercent;
        aStream := TmemoryStream.Create;

        try
          aJpeg.SaveToStream(aStream);
          aBlobField.LoadFromStream(aStream);
        finally
          aStream.Free;
        end;
      finally
        aJpeg.Free;
      end;
    finally
     aBmp.Free;
    end;
  end;
end;

// Check/Uncheck childs as parent :
procedure JvTreeView_SetChildsNodeChecked(aJvTreeView: TJvTreeView; ParentNode: TJvTreeNode; const Value: Boolean);
var
  n: Integer;
  Node: TTreeNode;
begin
  for n := 0 to ParentNode.Count - 1 do
  begin
    Node := ParentNode.getFirstChild;

    while Assigned(Node) do
    begin
      if TJvTreeNode(Node).Checked <> Value then
        TJvTreeNode(Node).Checked := Value;      // JvTreeView2NodeCheckedChange will be called !!!

      Node := ParentNode.GetNextChild(Node);
    end;
  end;
end;

procedure JvTreeView_InheritsChildNodesChecked(aJvTreeView: TJvTreeView; ParentNode: TTreeNode; const DeactivateOnNodeCheckedChange: Boolean = true);
var
  Node: TTreeNode;
  Value: smallInt;
  SavCheckedChange: TJvTreeViewNodeCheckedChange;
begin
  Value := 2;
  Node := ParentNode.getFirstChild;


  while Assigned(Node) do
  begin
    if Value = 2 then
      if TJvTreeNode(Node).Checked
      then Value := 1
      else Value := 0
    else
      if (Value = 1) <> TJvTreeNode(Node).Checked then
      begin
        Value := 0;    // Always Unckeck if not all childs checked
        Break;
      end;

    Node := ParentNode.GetNextChild(Node);
  end;

  if Value <> 2 then
  begin
    if DeactivateOnNodeCheckedChange then
    begin
      SavCheckedChange := aJvTreeView.OnNodeCheckedChange;
      aJvTreeView.OnNodeCheckedChange := Nil;
    end;

    if TJvTreeNode(ParentNode).Checked <> (Value = 1) then
      TJvTreeNode(ParentNode).Checked := Value = 1;

    if DeactivateOnNodeCheckedChange then
      aJvTreeView.OnNodeCheckedChange := SavCheckedChange;
  end;
end;

// Disable/enable childs as parent :
procedure JvTreeView_InheritsParentNodeEnabled(aJvTreeView: TJvTreeView; ParentNode: TTreeNode);
var
  n: Integer;
  Node: TTreeNode;
begin
  for n := 0 to ParentNode.Count - 1 do
  begin
    Node := ParentNode.getFirstChild;

    while Assigned(Node) do
    begin
      if Node.Enabled <> ParentNode.Enabled then
        Node.Enabled := ParentNode.Enabled;

      Node := ParentNode.GetNextChild(Node);
    end;
  end;
end;

end.
