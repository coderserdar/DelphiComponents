unit cyRegisterDER;

{$I ..\Core\cyCompilerDefines.inc}

interface

Uses
  Classes,

  {$IFDEF DELPHI6_OR_ABOVE}
  DesignEditors, DesignIntf,
  {$ELSE}
  Dsgnintf,
  {$ENDIF}

  {$IFDEF DELPHIXE2_OR_ABOVE}
  vcl.Menus;
  {$ELSE}
  Menus;
  {$ENDIF}

type
  // Add additional units dropping component :
  TcyDocERSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TcyDefaultEditor = class(TDefaultEditor) // class(TComponentEditor)
  public
    { Public declarations }
    procedure Edit; override;  // On double click the component ...
    procedure ExecuteVerb(Index: Integer); override;  // Called when MenuItem was clicked by developper ...
    function GetVerb(Index: Integer): string; override;  // Inform Delphi the captions to appear in the context menu.
    function GetVerbCount: Integer; override;  // Inform Delphi how many items we want to add to the context menu ...
//    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;  // before appearing the contextMenu, we can Show/hide menuItems but we can't destroy them ...
  end;

  procedure Register;

implementation

uses cyformAbout, cyDsnResource, cyDocER;

procedure TcyDefaultEditor.Edit;
begin
  Inherited Edit;
end;

procedure TcyDefaultEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1   // Last one ...
  then begin
    CindyDesignAboutForm;
    // Tell to the IDE that something changed!
    //  Designer.Modified;
  end
  else
    Inherited ExecuteVerb(Index);
end;

function TcyDefaultEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1   // Last one ...
  then Result := RSAboutName
  else Result := Inherited GetVerb(Index);
end;

function TcyDefaultEditor.GetVerbCount: Integer;
begin
  Result := Inherited GetVerbCount + 1;  // We add one MenuItem ...
end;

{procedure TcyDefaultEditor.PrepareItem(Index: Integer; const AItem: TMenuItem);
begin
  case Index of
    0: AItem.Enabled := true;
  end;

  // Tell to the IDE that something changed!
  Designer.Modified;
end;}

{ TcyDocER }
procedure TcyDocERSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('cyDERUtils');
end;

procedure Register;
begin
  RegisterComponents(RSDERPalette, [TcyDocER]);
  RegisterComponentEditor(TcyDocER, TcyDefaultEditor);
  RegisterSelectionEditor(TcyDocER, TcyDocERSelectionEditor);
end;

end.
