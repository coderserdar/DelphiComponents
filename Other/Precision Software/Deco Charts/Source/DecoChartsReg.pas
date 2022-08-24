{------------------------------------------------------------------------------
  DecoChartsReg.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Registration of components

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. Except where otherwise noted, the complete source code remains
  property of the author and may not be distributed, published, given or sold
  in any form as such. No parts of the source code can be included in any
  other component or application without written authorization of the author.

  Copyright (c) 2008-2013  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ Change log:

  Version 1.3 (2013-11-18)
  - added: Support for Delphi XE3/XE4/XE5
  - improved: Minor improvements and fixes

  Version 1.2 (2012-03-11)
  - added: TDecoCompareGrid, TDecoProgressBar, TDecoRatingStars components

  Version 1.0 (2011-07-14)
  - The first release
}

{ Unit that performs registration of DecoCharts components. }
unit DecoChartsReg;

interface

uses
  Classes,
  DecoBar,
  DecoProgressGrid,
  DecoCompareGrid,
  DecoProgressBar,
  DecoRatingStars,
  {$IF CompilerVersion >= 15}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$IFEND}
  ;

type
  { Design-time editing options for TDecoBar items. }
  TDecoBarControlEditor = class(TDefaultEditor)
  public
    { Returns number of design-time editing options. }
    function GetVerbCount: Integer; override;
    { Returns a caption of the design-time edit option by index. }
    function GetVerb(Index: Integer): string; override;
    { Executes the selected design-time edit option. }
    procedure ExecuteVerb(Index: Integer); override;
    { Opens the items editor. }
    procedure Edit; override;
  end;

  { Design-time editing options for TDecoProgressGrid items and columns. }
  TDecoProgressGridControlEditor = class(TDefaultEditor)
  public
    { Returns number of design-time editing options. }
    function GetVerbCount: Integer; override;
    { Returns a caption of the design-time edit option by index. }
    function GetVerb(Index: Integer): string; override;
    { Executes the selected design-time edit option. }
    procedure ExecuteVerb(Index: Integer); override;
    { Opens the items editor. }
    procedure Edit; override;
    { Opens the columns editor. }
    procedure EditColumns;
  end;


  { Design-time editing options for TDecoCompareGrid items and criterias. }
  TDecoCompareGridControlEditor = class(TDefaultEditor)
  public
    { Returns number of design-time editing options. }
    function GetVerbCount: Integer; override;
    { Returns a caption of the design-time edit option by index. }
    function GetVerb(Index: Integer): string; override;
    { Executes the selected design-time edit option. }
    procedure ExecuteVerb(Index: Integer); override;
    { Opens the items editor. }
    procedure Edit; override;
    { Opens the criterias editor. }
    procedure EditCriterias;
  end;


{ Registration of components and their design-time editing options. }
procedure Register;

implementation

uses
  SysUtils, ColnEdit;

procedure Register;
const
  pal = 'DecoCharts';
begin
  RegisterComponents(pal, [TDecoBar, TDecoProgressGrid, TDecoCompareGrid, TDecoProgressBar, TDecoRatingStars]);
  RegisterComponentEditor(TCustomDecoBar, TDecoBarControlEditor);
  RegisterComponentEditor(TCustomDecoProgressGrid, TDecoProgressGridControlEditor);
  RegisterComponentEditor(TCustomDecoCompareGrid, TDecoCompareGridControlEditor);
end;

//////////////////////////////////////// TDecoBarControlEditor /////////////////////////////////////////
procedure TDecoBarControlEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:Edit;
  end;
end;

procedure TDecoBarControlEditor.Edit;
begin
  ShowCollectionEditor(Designer, Component, TCustomDecoBar(Component).Items, 'Items');
end;

function TDecoBarControlEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Edit items';
  end;
end;

function TDecoBarControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//////////////////////////////////////// TDecoProgressGridControlEditor /////////////////////////////////////////
procedure TDecoProgressGridControlEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:Edit;
    1:EditColumns;
  end;
end;

procedure TDecoProgressGridControlEditor.Edit;
begin
  ShowCollectionEditor(Designer, Component, TCustomDecoProgressGrid(Component).Items, 'Items');
end;

procedure TDecoProgressGridControlEditor.EditColumns;
begin
  ShowCollectionEditor(Designer, Component, TCustomDecoProgressGrid(Component).Columns, 'Columns');
end;

function TDecoProgressGridControlEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Edit items';
    1: Result := 'Edit columns';
  end;
end;

function TDecoProgressGridControlEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

//////////////////////////////////////// TDecoCompareGridControlEditor /////////////////////////////////////////
procedure TDecoCompareGridControlEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:Edit;
    1:EditCriterias;
  end;
end;

procedure TDecoCompareGridControlEditor.Edit;
begin
  ShowCollectionEditor(Designer, Component, TCustomDecoCompareGrid(Component).Items, 'Items');
end;

procedure TDecoCompareGridControlEditor.EditCriterias;
begin
  ShowCollectionEditor(Designer, Component, TCustomDecoCompareGrid(Component).Criterias, 'Criterias');
end;

function TDecoCompareGridControlEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Edit items';
    1: Result := 'Edit criterias';
  end;
end;

function TDecoCompareGridControlEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.

