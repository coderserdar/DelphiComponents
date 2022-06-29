{   Component(s):
    tcyIniForm

    Description:
    Non-visual component that allow to load and save the position and size of the forms

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

unit cyIniForm;

interface

uses Classes, Windows, Messages, Controls, Forms, SysUtils, Inifiles, Registry, SHFolder, ExtCtrls, Graphics, vcl.cyImage;

type
  TcyTempForm = class(TCustomForm)  // In order to access to TCustomForm.Position property
  end;

  RAttributes = Record
    Top: Integer;
    Left: Integer;
    Width: Integer;
    Height: Integer;
    Visible: Boolean;
    State: Integer;
  end;

  TProcOnReadFile = procedure (Sender: TObject; IniFile: TInifile; FileVersion: String) of object;
  TProcOnReadRegistry = procedure (Sender: TObject; Registry: TRegistry; RegistryVersion: String) of object;
  TProcOnWriteFile = procedure (Sender: TObject; IniFile: TInifile) of object;
  TProcOnWriteRegistry = procedure (Sender: TObject; Registry: TRegistry) of object;
  TAutoCorrectAttribute = (atAutoCorrectPosition, atAutoCorrectSize);
  TAutoCorrectAttributes = Set Of TAutoCorrectAttribute;
  TAttribute = (atPosition, atSize, atVisible, atMinimized, atMaximized);
  TAttributes = Set of TAttribute;
  TMode = (mFile, mRegistry, mBoth);
  TIniDirectory = (idProgramLocation, idCommonAppData, idLocalAppData);
  TRegRoot = (rrKEY_CLASSES_ROOT, rrKEY_CURRENT_USER, rrKEY_LOCAL_MACHINE, rrKEY_USERS, rrKEY_CURRENT_CONFIG);

  TcyIniForm = class(TComponent)
  private
    FIniCustomfile: String;
    FAutoLoad: Boolean;
    FAutoSave: Boolean;
    FAttributes: TAttributes;
    FIniCustomSection: String;
    FRegRoot: TRegRoot;
    FRegCustomKey: String;
    FMode: TMode;
    FOnCustomSaveToFile: TProcOnWriteFile;
    FOnCustomLoadFromFile: TProcOnReadFile;
    FOnCustomSaveToRegistry: TProcOnWriteRegistry;
    FOnCustomLoadFromRegistry: TProcOnReadRegistry;
    FStoreVersion: String;
    FOnNotLoadFromRegistry: TProcOnReadRegistry;
    FOnNotLoadFromFile: TProcOnReadFile;
    fIniDirectory: TIniDirectory;
    fIniSubDirs: String;
    FAutoCorrectAttributes: TAutoCorrectAttributes;
    procedure SetIniCustomfile(AValue: String);
    procedure SetAutoLoad(AValue: Boolean);
    procedure SetAutoSave(AValue: Boolean);
    procedure SetAttributes(const Value: TAttributes);
    function LoadAttributesFromFile(OwnerControl: TWinControl; var RAttr: RAttributes): Boolean;
    function LoadAttributesFromRegistry(OwnerControl: TWinControl; var RAttr: RAttributes): Boolean;
    function SaveAttributesToFile(OwnerControl: TWinControl; RAttr: RAttributes): Boolean;
    function SaveAttributesToRegistry(OwnerControl: TWinControl; RAttr: RAttributes): Boolean;
    procedure DeleteAttributesFromFile(OwnerControl: TComponent);
    procedure DeleteAttributesFromRegistry(OwnerControl: TComponent);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CorrectSize(var RAttr: RAttributes);
    procedure CorrectPosition(var RAttr: RAttributes);
    procedure CenterVertically(var RAttr: RAttributes);
    procedure CenterHorizontally(var RAttr: RAttributes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    // procedure BeforeDestruction; override; Can' t be used because its called after receiving owner free notification ...
    function GetIniDirectory: String;
    function GetIniFileDir: String;
    function GetIniFile: String;
    function GetRootKey: HKey;
    function LoadDefinitions: Boolean;
    function SaveDefinitions: Boolean;
    procedure DeleteDefinitions;
  published
    property AutoCorrectAttributes: TAutoCorrectAttributes read FAutoCorrectAttributes write FAutoCorrectAttributes default [atAutoCorrectPosition, atAutoCorrectSize];
    property Attributes: TAttributes read FAttributes write SetAttributes default [atPosition, atSize];
    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad default True;
    property AutoSave: Boolean read FAutoSave write SetAutoSave default True;
    property IniCustomfile: String read FIniCustomfile write SetIniCustomfile;
    property IniCustomSection: String read FIniCustomSection write FIniCustomSection;
    property IniDirectory: TIniDirectory read fIniDirectory write fIniDirectory default idProgramLocation;
    property IniSubDirs: String read fIniSubDirs write fIniSubDirs;
    property Mode: TMode read FMode write FMode default mFile;
    property RegRoot: TRegRoot read FRegRoot write FRegRoot;
    property RegCustomKey: String read FRegCustomKey write FRegCustomKey;
    property StoreVersion: String read FStoreVersion write FStoreVersion;
    property OnCustomLoadFromFile: TProcOnReadFile read FOnCustomLoadFromFile write FOnCustomLoadFromFile;
    property OnCustomSaveToFile: TProcOnWriteFile read FOnCustomSaveToFile write FOnCustomSaveToFile;
    property OnCustomLoadFromRegistry: TProcOnReadRegistry read FOnCustomLoadFromRegistry write FOnCustomLoadFromRegistry;
    property OnCustomSaveToRegistry: TProcOnWriteRegistry read FOnCustomSaveToRegistry write FOnCustomSaveToRegistry;
    property OnNotLoadFromFile: TProcOnReadFile read FOnNotLoadFromFile write FOnNotLoadFromFile;
    property OnNotLoadFromRegistry: TProcOnReadRegistry read FOnNotLoadFromRegistry write FOnNotLoadFromRegistry;
  end;

const
  cNormal = 0;
  cMaximized = 1;
  cMinimized = 2;


  function IniFile_LoadGraphic(Inifile: TMemIniFile; const aSection, aName: string; StoredGraphicClass: TGraphicClass): TGraphic;
  function IniFile_SaveGraphic(Inifile: TMemIniFile; const aSection, aName: string; aGraphic: TGraphic; const UpdateFile: Boolean = true): Boolean;

  function IniFile_LoadPicture(Inifile: TMemIniFile; const aSection, aName: string; StoredGraphicClass: TGraphicClass; Destination: TPicture): Boolean;
  function IniFile_SavePicture(Inifile: TMemIniFile; const aSection, aName: string; aPicture: TPicture; const UpdateFile: Boolean = true): Boolean;

implementation

function IniFile_LoadGraphic(Inifile: TMemIniFile; const aSection, aName: string; StoredGraphicClass: TGraphicClass): TGraphic;
var
  Stream: TMemoryStream;
begin
  Result := Nil;

  try
    Stream := TMemoryStream.Create;

    if Inifile.ReadBinaryStream(aSection, aName, Stream) <> 0 then
    begin
      Result := vcl.cyImage.GraphicCreate(StoredGraphicClass);
      Result.LoadFromStream(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

function IniFile_SaveGraphic(Inifile: TMemIniFile; const aSection, aName: string; aGraphic: TGraphic; const UpdateFile: Boolean = true): Boolean;
var
  Stream: TMemoryStream;
begin
  Result := false;

  if not Assigned(aGraphic) then
  begin
    Inifile.DeleteKey(aSection, aName);

    if UpdateFile then
      Inifile.UpdateFile;

    Result := true;
    Exit;
  end;

  try
    Stream := TMemoryStream.Create;
    aGraphic.SaveToStream(Stream);

    Stream.Position := 0;
    Inifile.WriteBinaryStream(aSection, aName, Stream);

    if UpdateFile then
      Inifile.UpdateFile;

    Result := true;
  finally
    Stream.Free;
  end;
end;

function IniFile_LoadPicture(Inifile: TMemIniFile; const aSection, aName: string; StoredGraphicClass: TGraphicClass; Destination: TPicture): Boolean;
var
  aGraphic: TGraphic;
begin
  Result := false;
  aGraphic := IniFile_LoadGraphic(Inifile, aSection, aName, StoredGraphicClass);
  if aGraphic <> Nil then
    try
      Destination.Assign(aGraphic);
      Result := true;
    finally
      aGraphic.Free;
    end;
end;

function IniFile_SavePicture(Inifile: TMemIniFile; const aSection, aName: string; aPicture: TPicture; const UpdateFile: Boolean = true): Boolean;
begin
  Result := IniFile_SaveGraphic(Inifile, aSection, aName, aPicture.Graphic, UpdateFile);
end;

procedure TcyIniForm.BeforeDestruction;
begin
  if FAutoSave then
  begin
    FAutoSave := false;     // Avoid to be called twice ...
    SaveDefinitions;
  end;

  inherited;
end;

destructor TcyIniForm.Destroy;
begin
  inherited;
end;

constructor TcyIniForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // 2016-04-27 Do not read it here because when saving it can be different ! FOwnerHandle := TWinControl(AOwner).Handle;

  FAutoCorrectAttributes := [atAutoCorrectPosition, atAutoCorrectSize];
  FAttributes := [atPosition, atSize, atMaximized];
  FAutoLoad := True;
  FAutoSave := True;
  FIniCustomfile := '';
  FIniCustomSection := '';
  fIniDirectory := idProgramLocation;
  fIniSubDirs := '';
  FMode := mFile;
  FRegRoot := rrKEY_CURRENT_USER;
  FRegCustomKey := '';
  FStoreVersion := '';
end;

procedure TcyIniForm.Loaded;             // !!! Not called for Dynamic controls !!!
begin
  Inherited;

  if not (csDesigning in ComponentState) then
  begin
    if Owner <> Nil then
      Owner.FreeNotification(self);  // Be notified of owner destruction

    if FAutoLoad then
    begin
      if atPosition in FAttributes then
        if Owner is TCustomForm then   // Set position property if needed ...
          with TcyTempForm(Owner) do
          begin
            if Position in [poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter, poDefaultPosOnly] then
              Position := poDesigned;
          end;

      LoadDefinitions;
    end;
  end;
end;

procedure TcyIniForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if not (csDesigning in ComponentState) then
    if Operation = opRemove then
      if (AComponent = Owner) or (csFreeNotification in Owner.ComponentState) then    // csFreeNotification before csDestroying !
        if FAutoSave then
        begin
          FAutoSave := false;     // Avoid to be called twice ...
          SaveDefinitions;
        end;

{ This code don't work at 100% because components can be already free.
  if not (csDesigning in ComponentState)
  then
    if AComponent = Owner
    then
      if (FAutoSave) and (Operation = opRemove)
      then SaveAttributes;
 }
  inherited Notification(AComponent, Operation);
end;

procedure TcyIniForm.CorrectSize(var RAttr: RAttributes);
begin
  if RAttr.Height > Screen.Height then
    RAttr.Height := Screen.Height;

  if RAttr.Width > Screen.Width then
    RAttr.Width := Screen.Width;
end;

procedure TcyIniForm.CorrectPosition(var RAttr: RAttributes);
begin
  if RAttr.Top < 0 then RAttr.Top := 0;
  if RAttr.Left < 0 then RAttr.Left := 0;

  if RAttr.Top + RAttr.Height > Screen.Height then
    CenterVertically(RAttr);

  if RAttr.Left + RAttr.Width > Screen.Width then
    CenterHorizontally(RAttr);
end;

procedure TcyIniForm.CenterVertically(var RAttr: RAttributes);
begin
  RAttr.Top  := (Screen.Height div 2) - (RAttr.Height div 2);
end;

procedure TcyIniForm.CenterHorizontally(var RAttr: RAttributes);
begin
  RAttr.Left := (Screen.Width div 2) - (RAttr.Width div 2);
end;

function TcyIniForm.LoadDefinitions: Boolean;
var
  RAttr: RAttributes;
  Client: TRect;
  OwnerControl: TWinControl;
begin
  Result := false;
  if Owner = nil then Exit;

  if Owner is TWinControl then
  begin
    OwnerControl := TWinControl(Owner);

    RAttr.Visible := True; // OwnerControl.Visible;
    RAttr.Top := OwnerControl.Top;
    RAttr.Left := OwnerControl.Left;
    RAttr.Width := OwnerControl.Width;
    RAttr.Height := OwnerControl.Height;
    RAttr.State := cNormal;

    if (not Result) and (FMode <> mRegistry) then
      Result := LoadAttributesFromFile(OwnerControl, RAttr);

    if (not Result) and (FMode <> mFile) then       // Don' t need to load twice !!!
      Result := LoadAttributesFromRegistry(OwnerControl, RAttr);

    if Result then
    begin
      // Apply :
      if atVisible in FAttributes then
        OwnerControl.Visible := RAttr.Visible;

      if atPosition in FAttributes then
      begin
        OwnerControl.Top := RAttr.Top;
        OwnerControl.Left := RAttr.Left;
      end;

      if atSize in FAttributes then
      begin
        // 2016-04-27 Do not set Width/height because it doesn' t work if form is maximized !
        //OwnerControl.Width := RAttr.Width;
        //OwnerControl.Height := RAttr.Height;

        Client := OwnerControl.ClientRect;
        OwnerControl.ClientWidth := RAttr.Width - (OwnerControl.Width - Client.Right);
        OwnerControl.ClientHeight := RAttr.Height - (OwnerControl.Height - Client.Bottom);
      end;

      if OwnerControl is TCustomForm then
      begin
        if (atMaximized in FAttributes) and (RAttr.State = cMaximized) then
          TCustomForm(OwnerControl).WindowState := wsMaximized;

        if (atMinimized in FAttributes) and (RAttr.State = cMinimized) then
          TCustomForm(OwnerControl).WindowState := wsMinimized;
      end;
    end;
  end;
end;

function TcyIniForm.GetIniDirectory: String;

    function GetSpecialFolderPath(folder: integer) : string;
    const
      SHGFP_TYPE_CURRENT = 0;
    var
      path: array [0..MAX_PATH] of char;
    begin
      if SHGetFolderPath(0, folder, 0, SHGFP_TYPE_CURRENT, @path[0]) = 0
      then Result := path
      else Result := '';
    end;

begin
  case fIniDirectory of
    idProgramLocation: Result := ExtractFileDir(ParamStr(0));                // Executable
    idCommonAppData:   Result := GetSpecialFolderPath(CSIDL_COMMON_APPDATA); // All users application data
    idLocalAppData:    Result := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA);  // Current user application data
  end;
end;

function TcyIniForm.GetIniFileDir: String;

    function CheckPathDelimiters(Str: String): String;
    var i: Integer;
    begin
      // Initialization :
      if Copy(Str, 1, 2) = '\\'     // Network path ...
      then Result := '\'
      else Result := '';

      // Remove duplicated path delimiters :
      i := Pos('\\', Str);

      while i > 0 do
      begin
        Delete(Str, i, 1);
        i := Pos('\\', Str);
      end;

      Result := Result + Str;
    end;

begin
  Result := '';
  Result := CheckPathDelimiters(GetIniDirectory + '\' + fIniSubDirs + '\');
end;

function TcyIniForm.GetIniFile: String;
begin
  if FIniCustomfile = ''
  then Result := GetIniFileDir + ExtractFileName( ChangeFileExt(ParamStr(0), '.ini') )
  else Result := GetIniFileDir + FIniCustomfile;
end;

function TcyIniForm.LoadAttributesFromFile(OwnerControl: TWinControl; var RAttr: RAttributes): Boolean;
var
  StrFile, FileVersion, StrSection, Str: String;
  ValidVersion: Boolean;
  i: Integer;
  IniF : TIniFile;
begin
  Result := false;
  ValidVersion := false;
  FileVersion := '';
  StrFile := GetIniFile;

  try
    IniF := TIniFile.Create(StrFile);

    if FIniCustomSection = ''
    then StrSection := OwnerControl.Name
    else StrSection := FIniCustomSection;

    // Read stored file version :
    if IniF.ValueExists(StrSection, 'VERSION') then
    begin
      FileVersion  := IniF.ReadString(StrSection, 'VERSION', '');
      ValidVersion := FStoreVersion = FileVersion;
    end;

    if ValidVersion then
    begin
      if atSize in FAttributes then
      begin
        i := IniF.ReadInteger(StrSection, 'WIDTH', 0);
        if i > 0 then RAttr.Width := i;

        i := IniF.ReadInteger(StrSection, 'HEIGHT', 0);
        if i > 0 then RAttr.Height := i;

        if atAutoCorrectSize in FAutoCorrectAttributes then
          CorrectSize(RAttr);
      end;

      if atPosition in FAttributes then
      begin
        if IniF.ValueExists(StrSection, 'TOP') and IniF.ValueExists(StrSection, 'LEFT') then
        begin
          RAttr.Top := IniF.ReadInteger(StrSection, 'TOP', 0);
          RAttr.Left := IniF.ReadInteger(StrSection, 'LEFT', 0);

          if atAutoCorrectPosition in FAutoCorrectAttributes then
            CorrectPosition(RAttr);
        end
        else begin
          // Center owner control in the screen:
          CenterHorizontally(RAttr);
          CenterVertically(RAttr);
        end;
      end;

      if atVisible in FAttributes then
      begin
        Str := IniF.ReadString(StrSection, 'VISIBLE', '');
        RAttr.Visible := Str <> 'N';
      end;

      if (atMaximized in FAttributes) or (atMinimized in FAttributes) then
        RAttr.State := IniF.ReadInteger(StrSection, 'STATE', cNormal);

      Result := true;
    end
    else begin
      if atPosition in FAttributes then
      begin
        // Center owner control in the screen:
        CenterHorizontally(RAttr);
        CenterVertically(RAttr);
      end;

      if Assigned(FOnNotLoadFromFile) then
        FOnNotLoadFromFile(Self, IniF, FileVersion);
    end;

    if Assigned(FOnCustomLoadFromFile) then
      FOnCustomLoadFromFile(Self, IniF, FileVersion);
  finally
    IniF.free;
  end;
end;

function TcyIniForm.GetRootKey: HKey;
begin
  case FRegRoot of
    rrKEY_CLASSES_ROOT:   Result := HKEY_CLASSES_ROOT;
    rrKEY_CURRENT_USER:   Result := HKEY_CURRENT_USER;
    rrKEY_LOCAL_MACHINE:  Result := HKEY_LOCAL_MACHINE;
    rrKEY_USERS:          Result := HKEY_USERS;
    rrKEY_CURRENT_CONFIG: Result := HKEY_CURRENT_CONFIG;
  end;
end;

function TcyIniForm.LoadAttributesFromRegistry(OwnerControl: TWinControl; var RAttr: RAttributes): Boolean;
var
  ValidVersion: Boolean;
  Registro: TRegistry;
  RegistryVersion, aKey: String;
  i: Integer;
begin
  Result := false;
  ValidVersion := false;
  RegistryVersion := '';
  Registro := TRegistry.Create;

  try
    Registro.RootKey := GetRootKey;

    if FRegCustomKey <> '' then
    begin
      aKey := FRegCustomKey;
      if aKey[1] <> '\' then aKey := '\' + aKey;
    end
    else
      aKey := '\software\' + ExtractFileName(ChangeFileExt(ParamStr(0), '')) + '\' + OwnerControl.Name;

    if Registro.OpenKeyReadOnly(aKey) then
    begin
      // Read stored file version :
      if Registro.ValueExists('VERSION') then
      begin
        RegistryVersion := Registro.ReadString('VERSION');
        ValidVersion := FStoreVersion = RegistryVersion;
      end;

      if ValidVersion then
      begin
        if atSize in FAttributes then
          if Registro.ValueExists('WIDTH') and Registro.ValueExists('HEIGHT') then
          begin
            i := Registro.ReadInteger('WIDTH');
            if i > 0 then RAttr.Width := i;

            i := Registro.ReadInteger('HEIGHT');
            if i > 0 then RAttr.Height := i;

            if atAutoCorrectSize in FAutoCorrectAttributes then
              CorrectSize(RAttr);
          end;

        if atPosition in FAttributes then
        begin
          if Registro.ValueExists('TOP') and Registro.ValueExists('LEFT') then
          begin
            i := Registro.ReadInteger('TOP');
            RAttr.Top := i;

            i := Registro.ReadInteger('LEFT');
            RAttr.Left := i;

            if atAutoCorrectPosition in FAutoCorrectAttributes then
              CorrectPosition(RAttr);
          end
          else begin
            // Center owner control in the screen:
            CenterHorizontally(RAttr);
            CenterVertically(RAttr);
          end;
        end;

        if atVisible in FAttributes then
          if Registro.ValueExists('VISIBLE') then
            RAttr.Visible := Registro.ReadBool('VISIBLE');

        if (atMaximized in FAttributes) or (atMinimized in FAttributes) then
          if Registro.ValueExists('STATE') then
            RAttr.State := Registro.ReadInteger('STATE');

        Result := true;
      end
      else begin
        if atPosition in FAttributes then
        begin
          CenterHorizontally(RAttr);
          CenterVertically(RAttr);
        end;

        if Assigned(FOnNotLoadFromRegistry) then
          FOnNotLoadFromRegistry(Self, Registro, RegistryVersion);
      end;

      if Assigned(FOnCustomLoadFromRegistry) then
        FOnCustomLoadFromRegistry(Self, Registro, RegistryVersion);

      Registro.CloseKey;
    end;
  finally
    Registro.Free;
  end;
end;

function TcyIniForm.SaveDefinitions: Boolean;
var
  OwnerControl: TWinControl;
  FOwnerHandle: Cardinal;
  RAttr: RAttributes;
  WinPlacement: TWindowPlacement;
  RectNormalPos: TRect;

    function IsOwnerMaximized: boolean;
    begin
      Result := false;

      try
        if IsZoomed(FOwnerHandle) then
          Result := true
        else
          if Self.Owner is TCustomForm then
            Result := TCustomForm(Self.Owner).WindowState = wsMaximized;
      except
      end;
    end;

    function IsOwnerMinimized: boolean;
    begin
      Result := false;

      try
        if IsIconic(FOwnerHandle) then
          Result := true
        else
          if Self.Owner is TCustomForm then
            Result := TCustomForm(Self.Owner).WindowState = wsMaximized;
      except
      end;
    end;


begin
  Result := false;

  if Owner = nil then Exit;

  if Owner is TWinControl then
  begin
    OwnerControl := TWinControl(Owner);
    if not OwnerControl.HandleAllocated then Exit;

    FOwnerHandle := OwnerControl.Handle;
    RAttr.Visible := OwnerControl.Visible;
    RAttr.State := cNormal;

    // 2016-04-26 ... if atMinimized in FAttributes then
    if IsOwnerMinimized then
      RAttr.State := cMinimized;

    // 2016-04-26 ... if atMaximized in FAttributes then
    if IsOwnerMaximized then
      RAttr.State := cMaximized;

    RectNormalPos := OwnerControl.BoundsRect;

    if RAttr.State <> cNormal then
    begin
      WinPlacement.length := SizeOf(TWindowPlacement);
      // FillChar(WinPlacement, SizeOf(TWindowPlacement), #0);

      try
        if GetWindowPlacement(FOwnerHandle, @WinPlacement) then  // Not working because form is being destroying !?
        begin
          RectNormalPos := WinPlacement.rcNormalPosition;   // Position independently of form state
        end
        else begin
          // 2016-04-27 When minimized, left have big value in order to hide form ...
          RectNormalPos.Top := 0;
          RectNormalPos.Left := 0;
        end;
      except
        // 2016-04-27 When minimized, left have big value in order to hide form ...
        RectNormalPos.Top := 0;
        RectNormalPos.Left := 0;
      end;

(*          if RAttr.Width > Screen.Width then RAttr.Width := Screen.Width;
      if RAttr.Height > Screen.Height then RAttr.Height := Screen.Height;
      if RAttr.Top >= Screen.Height - 20 then RAttr.Top := 0;
      if RAttr.Left >= Screen.Width - 20 then RAttr.Left := 0;    *)
    end;

    RAttr.Top := RectNormalPos.Top;
    RAttr.Left := RectNormalPos.Left;
    RAttr.Width := RectNormalPos.Right-RectNormalPos.Left;
    RAttr.Height := RectNormalPos.Bottom-RectNormalPos.Top;

    if FMode <> mRegistry then
      Result := Result or SaveAttributesToFile(OwnerControl, RAttr);

    if FMode <> mFile then
      Result := Result or SaveAttributesToRegistry(OwnerControl, RAttr);
  end;
end;

function TcyIniForm.SaveAttributesToFile(OwnerControl: TWinControl; RAttr: RAttributes): Boolean;
var
  StrFile, StrSection: String;
  IniF : TIniFile;
begin
  Result := false;
  StrFile := GetIniFile;

  try
    IniF := TIniFile.Create(StrFile);

    try       // This try avoid write error (read only ini file for exemple) occured when only using try finally statement ...

      // Create directory if not exists :
      if not DirectoryExists(ExtractFileDir(StrFile))
      then ForceDirectories(ExtractFileDir(StrFile));

      if FIniCustomSection = ''
      then StrSection := OwnerControl.Name
      else StrSection := FIniCustomSection;

      IniF.WriteString(StrSection, 'VERSION', FStoreVersion);

      if atPosition in FAttributes then
      begin
        IniF.WriteInteger(StrSection, 'TOP', RAttr.Top);
        IniF.WriteInteger(StrSection, 'LEFT', RAttr.Left);
      end;

      if atSize in FAttributes then
      begin
        IniF.WriteInteger(StrSection, 'WIDTH', RAttr.Width);
        IniF.WriteInteger(StrSection, 'HEIGHT', RAttr.Height);
      end;

      if atVisible in FAttributes then
        if RAttr.Visible
        then IniF.WriteString(StrSection, 'VISIBLE', 'Y')
        else IniF.WriteString(StrSection, 'VISIBLE', 'N');

      if (atMinimized in FAttributes) or (atMaximized in FAttributes) then
        IniF.WriteInteger(StrSection, 'STATE', RAttr.State);

      if Assigned(FOnCustomSaveToFile)
      then FOnCustomSaveToFile(Self, IniF);

      Result := true;
    except

    end;
  finally
    IniF.free;  
  end;
end;

function TcyIniForm.SaveAttributesToRegistry(OwnerControl: TWinControl; RAttr: RAttributes): Boolean;
var
  Registro: TRegistry;
  aKey: String;
begin
  Result := false;
  Registro := TRegistry.Create;

  try
    Registro.RootKey := GetRootKey;

    if FRegCustomKey <> ''
    then begin
      aKey := FRegCustomKey;
      if aKey[1] <> '\' then aKey := '\' + aKey;
    end
    else
      aKey := '\software\' + ExtractFileName(ChangeFileExt(ParamStr(0), '')) + '\' + OwnerControl.Name;

    if Registro.OpenKey(aKey, true)
    then begin
      Registro.WriteString('VERSION', FStoreVersion);

      if atPosition in FAttributes
      then begin
        Registro.WriteInteger('TOP', RAttr.Top);
        Registro.WriteInteger('LEFT', RAttr.Left);
      end;

      if atSize  in FAttributes
      then begin
        Registro.WriteInteger('WIDTH', RAttr.Width);
        Registro.WriteInteger('HEIGHT', RAttr.Height);
      end;

      if atVisible in FAttributes
      then Registro.WriteBool('VISIBLE', RAttr.Visible);

      if (atMinimized in FAttributes) or (atMaximized in FAttributes)
      then Registro.WriteInteger('STATE', RAttr.State);

      if Assigned(FOnCustomSaveToRegistry)
      then FOnCustomSaveToRegistry(Self, Registro);

      Registro.CloseKey;
      Result := true;
    end;
  finally
    Registro.Free;
  end;
end;

procedure TcyIniForm.DeleteDefinitions;
begin
  if Owner = nil then Exit;

  if FMode <> mRegistry
  then DeleteAttributesFromFile(Owner);

  if FMode <> mFile
  then DeleteAttributesFromRegistry(Owner);
end;

procedure TcyIniForm.DeleteAttributesFromFile(OwnerControl: TComponent);
var StrFile, StrSection: String;
begin
  StrFile := GetIniFile;
  if FIniCustomSection = ''
  then StrSection := OwnerControl.Name
  else StrSection := FIniCustomSection;

  if FileExists(StrFile)
  then
    with TIniFile.Create(StrFile) do
      try
        EraseSection(StrSection);
      finally
        free;
      end;
end;

procedure TcyIniForm.DeleteAttributesFromRegistry(OwnerControl: TComponent);
var aKey: String;
begin
  with TRegistry.Create do
    try
      RootKey := GetRootKey;

      if FRegCustomKey <> ''
      then begin
        aKey := FRegCustomKey;
        if aKey[1] <> '\' then aKey := '\' + aKey;
      end
      else
        aKey := '\software\' + ExtractFileName(ChangeFileExt(ParamStr(0), ''));

      DeleteKey(aKey);
    finally
      Free;
    end;
end;

procedure TcyIniForm.SetIniCustomfile(AValue: String);
begin
  if AValue <> FIniCustomfile
  then FIniCustomfile := AValue;
end;

procedure TcyIniForm.SetAttributes(const Value: TAttributes);
begin
  FAttributes := Value;
end;

procedure TcyIniForm.SetAutoLoad(AValue: Boolean);
begin
  if AValue <> FAutoLoad
  then FAutoLoad := AValue;
end;

procedure TcyIniForm.SetAutoSave(AValue: Boolean);
begin
  if AValue <> FAutoSave
  then FAutoSave := AValue;  
end;

end.
