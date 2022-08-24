{------------------------------------------------------------------------------
  TagCloudReg.pas

  TagCloud for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Registration of components

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. The complete source code remains property of the author
  and may not be distributed, published, given or sold in any form as such.
  No parts of the source code can be included in any other component
  or application without written authorization of the author.

  Copyright (c) 2008-2014  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ Unit that supports the registration of TagCloud components. }
unit TagCloudReg;

interface

uses
  Classes,
  TagCloud,
  TagIndex,
  TagCloudPrmStyler,
  NavPathLabel,
  {$IF CompilerVersion>=15}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$IFEND}
  ;

type
  { Design-time editing options for tag cloud items, color levels and custom scale. }
  TTagCloudControlEditor = class(TDefaultEditor)
  public
    { Returns number of design-time editing options. }
    function GetVerbCount: Integer; override;
    { Returns a caption of the design-time edit option by index. }
    function GetVerb(Index: Integer): string; override;
    { Executes the selected design-time edit option. }
    procedure ExecuteVerb(Index: Integer); override;
    { Opens the tag cloud items editor. }
    procedure Edit; override;
    { Opens the tag cloud color levels editor. }
    procedure EditColors;
    { Opens the tag cloud custom scale editor. }
    procedure EditScale;
  end;

  { Design-time editing options for TTagIndex component. }
  TTagIndexControlEditor = class(TTagCloudControlEditor)
  public
    { Returns number of design-time editing options. }
    function GetVerbCount: Integer; override;
  end;

  { Basic design-time editing options for tag cloud stylers. }
  TTagCloudStylerEditor = class(TDefaultEditor)
  public
    { Returns number of design-time editing options. }
    function GetVerbCount: Integer; override;
    { Returns a caption of the design-time edit option by index. }
    function GetVerb(Index: Integer): string; override;
    { Executes the selected design-time edit option. }
    procedure ExecuteVerb(Index: Integer); override;
    { Fills the styler with the properties of first available TagCloud component (first attached, or first on the owner's form). }
    procedure EditFillIn;
  end;

  { Design-time editing options for TTagCloudPrmStylerEditor. }
  TTagCloudPrmStylerEditor = class(TTagCloudStylerEditor)
  public
    {$IF CompilerVersion>=15}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
    {$IFEND}
  end;

{ Registrates the components and their design-time editing options. }
procedure Register;

implementation

uses
  SysUtils, ColnEdit;

procedure Register;
const
  pal = 'TagCloud';
begin
  RegisterComponents(pal, [TTagCloud, TTagIndex, TTagCloudPrmStyler, TNavPathLabel]);
  RegisterComponentEditor(TCustomTagCloud, TTagCloudControlEditor);
  RegisterComponentEditor(TTagIndex, TTagIndexControlEditor);
  RegisterComponentEditor(TTagCloudPrmStyler, TTagCloudPrmStylerEditor);
end;

//////////////////////////////////////// TTagCloudControlEditor /////////////////////////////////////////
procedure TTagCloudControlEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:Edit;
    1:EditColors;
    2:EditScale;
  end;
end;

procedure TTagCloudControlEditor.Edit;
begin
  ShowCollectionEditor(Designer, Component, TCustomTagCloud(Component).Items, 'Items');
end;

procedure TTagCloudControlEditor.EditColors;
begin
  ShowCollectionEditor(Designer, Component, TCustomTagCloud(Component).Colors, 'Colors');
end;

procedure TTagCloudControlEditor.EditScale;
begin
  ShowCollectionEditor(Designer, Component, TCustomTagCloud(Component).CustomScale, 'CustomScale');
end;

function TTagCloudControlEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Edit items';
    1: Result := 'Edit level colors';
    2: Result := 'Edit custom scale';
  end;
end;

function TTagCloudControlEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

//////////////////////////////////////// TTagIndexControlEditor /////////////////////////////////////////
function TTagIndexControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//////////////////////////////////////// TTagCloudStylerEditor /////////////////////////////////////////
procedure TTagCloudStylerEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:Edit;
    1:EditFillIn;
  end;
end;

procedure TTagCloudStylerEditor.EditFillIn;
var
  i:Integer;
  F:TCustomTagCloud;
begin
  F:=nil;
  for i:=0 To Component.Owner.ComponentCount-1 do
    if Component.Owner.Components[i] is TCustomTagCloud then
    begin
      F:=TCustomTagCloud(Component.Owner.Components[i]);
      if F.Styler=Component then
        break;
    end;
  if Assigned(F) then
  begin
    TCustomTagCloudStyler(Component).LoadFromTagCloud(F);
    Designer.Modified;
  end;
end;

function TTagCloudStylerEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Edit style';
    1: Result := 'Fill in from the first available TagCloud';
  end;
end;

function TTagCloudStylerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$IF CompilerVersion>=15}
procedure TTagCloudPrmStylerEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if SameText(PropName, 'PARAMS') then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;
{$ELSE}
procedure TTagCloudPrmStylerEditor.EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if SameText(PropName, 'PARAMS') then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$IFEND}

end.

