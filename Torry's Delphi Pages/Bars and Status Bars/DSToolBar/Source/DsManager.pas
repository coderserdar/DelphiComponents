unit DsManager;

interface

uses
  Classes, Controls, DsToolBar;

type
  TDsManager = class(TComponent)
  private
    fToolbars: TList;
    fDockSites: TList;
    fFileName: String;
    function getCount: Integer;
  protected
    function getToolbar(Index: Integer): TDSToolBar; virtual;
    function getParentByName(aName: String): TWinControl; virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy(); override;
    procedure addToolBar(aToolBar: TDSToolBar);
    procedure addDockSite(aDockSite: TComponent);
    procedure deleteToolBar(aToolBar: TDSToolBar);
    procedure saveToFile;
    procedure loadFromFile;
    procedure loadControls;
    property Toolbars[Index: Integer]: TDSToolBar read getToolbar; default;
  published
    property fileName: String read fFileName write fFileName;
    property count: Integer read getCount;
  end;

  procedure Register;

implementation

uses
  IniFiles, SysUtils, DsDockSite;

const
  cIniSection = 'DsToolBars';
  cIniLeft = 'Left';
  cIniTop = 'Top';
  cIniWidth = 'Width';
  cIniParent = 'Parent';
  cIniVisible = 'Visible';

procedure Register;
begin
  RegisterComponents('DS Menus', [TDsManager]);
end;

{ TDsManager }

constructor TDsManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fToolbars := TList.Create;
  fDockSites := TList.Create;
end;

destructor TDsManager.Destroy;
begin
  FreeAndNil(fToolbars);
  FreeAndNil(fDockSites);
  inherited Destroy;
end;


procedure TDsManager.addToolBar(aToolBar: TDSToolBar);
begin
  fToolbars.Add(aToolBar);
end;

procedure TDsManager.deleteToolBar(aToolBar: TDSToolBar);
begin
  fToolbars.Remove(aToolBar);
end;

function TDsManager.getToolbar(Index: Integer): TDSToolBar;
begin
  Result := TDSToolBar(fToolbars[Index]);
end;

procedure TDsManager.saveToFile;
var
  fIni: TIniFile;
  i: Integer;
begin
  if (Trim(fileName) = EmptyStr) then
    Exit;
  fIni := TIniFile.Create(fileName);
  try
    for i := 0 to fToolbars.Count - 1 do begin
      fIni.WriteInteger(cIniSection, Toolbars[i].Name+cIniLeft, Toolbars[i].Left);
      fIni.WriteInteger(cIniSection, Toolbars[i].Name+cIniTop, Toolbars[i].Top);
      fIni.WriteInteger(cIniSection, Toolbars[i].Name+cIniWidth, Toolbars[i].Width);
      fIni.WriteString(cIniSection, Toolbars[i].Name+cIniParent, Toolbars[i].Parent.Name);
      fIni.WriteBool(cIniSection, Toolbars[i].Name+cIniVisible, Toolbars[i].Visible);
    end;
  finally
    fIni.Free;
  end;
end;

procedure TDsManager.loadFromFile;
var
  fIni: TIniFile;
  i, j: Integer;
  vStr: String;
  vBands: TList;
  vDockSite: TDsDockSite;
  vBand: TDSToolBar;

  function FindPos: Integer;
  var
    j: Integer;
  begin
    if vBands.Count = 0 then begin
      Result := 0;
      Exit;
    end;

    Result := -1;

    if vDockSite.Align in [alNone, alTop] then begin
      for j := 0 to vBands.Count - 1 do begin
        if (TControl(vBands[j]).Top > vBand.Top) or
           ((TControl(vBands[j]).Top = vBand.Top) and
            (TControl(vBands[j]).Left > vBand.Left)) then
        begin
          Result := j;
          Break;
        end;
      end;
    end
    else
    if vDockSite.Align in [alBottom] then begin
      for j := 0 to vBands.Count - 1 do begin
        if (TControl(vBands[j]).Top < vBand.Top) or
           ((TControl(vBands[j]).Top = vBand.Top) and
            (TControl(vBands[j]).Left > vBand.Left)) then
        begin
          Result := j;
          Break;
        end;
      end;
    end
    else
    if vDockSite.Align in [alLeft] then begin
      for j := 0 to vBands.Count - 1 do begin
        if (TControl(vBands[j]).Left > vBand.Left) or
           ((TControl(vBands[j]).Left = vBand.Left) and
            (TControl(vBands[j]).Top > vBand.Top)) then
        begin
          Result := j;
          Break;
        end
      end;
    end
    else
    if vDockSite.Align in [alRight] then begin
      for j := 0 to vBands.Count - 1 do begin
        if (TControl(vBands[j]).Left < vBand.Left) or
           ((TControl(vBands[j]).Top = vBand.Top) and
            (TControl(vBands[j]).Top > vBand.Top)) then
        begin
          Result := j;
          Break;
        end
      end;
    end;

    if Result = -1 then
      Result := vBands.Count;
  end;

begin
  loadControls;

  if (not FileExists(fileName)) then
    Exit;

  fIni := TIniFile.Create(fileName);
  try
    for i := 0 to fDockSites.Count - 1 do begin
      vDockSite := TDsDockSite(fDockSites[i]);
      vBands := TList.Create;
      for j := 0 to fToolbars.Count - 1 do begin
        vBand := TDSToolBar(fToolbars[j]);
        vStr := fIni.ReadString(cIniSection, vBand.Name+cIniParent, '');
        if (vStr = vDockSite.Name) then begin
          vBand.Parent := nil;
          vBand.Left := fIni.ReadInteger(cIniSection, vBand.Name+cIniLeft, vBand.Left);
          vBand.Top := fIni.ReadInteger(cIniSection, vBand.Name+cIniTop, vBand.Top);
          vBand.Width := fIni.ReadInteger(cIniSection, vBand.Name+cIniWidth, vBand.Width);
          vBand.Visible := fIni.ReadBool(cIniSection, vBand.Name+cIniVisible, vBand.Visible);
          vBands.Insert(FindPos, vBand);
        end;
      end;
      for j := 0 to vBands.Count - 1 do
        TDSToolBar(vBands[j]).Parent := vDockSite;
      vBands.Free;
    end;
  finally
    fIni.Free;
  end;
end;

procedure TDsManager.addDockSite(aDockSite: TComponent);
begin
  fDockSites.Add(aDockSite);
end;

function TDsManager.getParentByName(aName: String): TWinControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fDockSites.Count - 1 do
    if aName = TWinControl(fDockSites[i]).Name then
    begin
      Result := TWinControl(fDockSites[i]);
      Break;
    end;
end;

procedure TDsManager.loadControls;
var
  i: Integer;
begin
  if (Owner = nil) then
    Exit;

  for i := 0 to Owner.ComponentCount - 1 do
  begin
    if (Owner.Components[i] is TDsDockSite) then
    begin
      addDockSite(Owner.Components[i]);
    end
    else
    if (Owner.Components[i] is TDSToolBar) then
    begin
      addToolBar(TDSToolBar(Owner.Components[i]));
    end;
  end;
end;

function TDsManager.getCount: Integer;
begin
  Result := fToolbars.Count;
end;

end.
