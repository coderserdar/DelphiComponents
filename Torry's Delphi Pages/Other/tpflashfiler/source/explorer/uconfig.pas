{*********************************************************}
{* Persistently Stored Configuration Info                *}
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


unit uconfig;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Grids,
  DB,
  DBGrids,
  INIFiles,
  Classes,
  ffllbase,
  ffllprot,
  uconsts;

const
  {$IFDEF UseRegistryConfig}
  cfgRootKey               = HKEY_LOCAL_MACHINE;
  {$ENDIF}
  cfgKeyOptions            = 'Options';
  cfgWindow                = 'Window';
  cfgWindowState           = 'WindowState';
  cfgSplitter              = 'Splitter';
  cfgKeyRegisteredServers  = 'Registered Servers';
  cfgShowBrowser           = 'Show Browser';
  cfgLiveDatasets          = 'Live Datasets';
  cfgDefaultTimeout        = 'Default Timeout';                     {!!.11}

  cfgProtocol              = 'Protocol';
  cfgProtocolSingleUser    = 'Single User';
  cfgProtocolNetBIOS       = 'NetBIOS';
  cfgProtocolTCPIP         = 'TCP/IP';
  cfgProtocolIPXSPX        = 'IPX/SPX';

  cfgLastServer            = 'LastServer';

  cfgSortAvailIndexFields  = 'Available Index Fields Sorted';

  defWindowState           = wsNormal;
  defcfgShowBrowser        = True;
  defcfgLiveDatasets       = False;
  defcfgSortAvailIndexFields  = True;

type
  TffeConfigOptions = set of (coShowBrowser, coLiveDatasets);

  TffeConfig = class(TPersistent)
  private
  protected {private}
    FLastServer : string;
    FWindow: TRect;
    FWindowState: TWindowState;
    FSortAvailIndexFields : Boolean;
    FSplitterPosition: Integer;
    FOptions: TffeConfigOptions;
//    FProtocol: TffCommsProtocolClass;
    FRegisteredServers: TStrings;
    FINIFilename: TFileName;
    {$IFDEF UseRegistryConfig}
    FRegistryKey: TffShStr;
    {$ENDIF}
    FDefaultTimeout: Integer;                                         {!!.11}
      { default timeout for all operations unless overriden in
        table- or sqlwindows etc }
    FWorkingDirectory: String;                                        {!!.11}
      { the current dir upon startup
        (from the "start in" shortcut setting) }

  protected
//    function GetProtocolName: TffShStr;
    procedure ParseWindowString(aWindow: TffShStr);
    procedure SetLastServer(aValue : string);
//    procedure SetProtocol(aValue: TffCommsProtocolClass);
    procedure SetWindowState(aValue: TWindowState);
    procedure SetDefaultTimeout(const Value: Integer);                {!!.11}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Refresh;
    {- Reload all settings from persistent storage}

    procedure Save;
    {- Save the configuration to persistent storage}

    property LastServer : string
      read FLastServer write SetLastServer;
    {- Last server accessed by user. }

    property Options: TffeConfigOptions
      read FOptions write FOptions;
    {- boolean option settings}

//    property Protocol: TffCommsProtocolClass
//      read FProtocol write SetProtocol;
    {- Communications protocol}

//    property ProtocolName: TffShStr
//      read GetProtocolName;
    {- Returns the label associated with the protocol }

    property RegisteredServers: TStrings
      read FRegisteredServers write FRegisteredServers;
    {- Currently registered server names}

    property SortAvailIndexFields : Boolean
      read FSortAvailIndexFields write FSortAvailIndexFields;
    {- Should the available index fields be in sorted or natural order }

    property SplitterPosition: Integer
      read FSplitterPosition write FSplitterPosition;
    {- Position of the main window's splitter bar}

    property Window: TRect
      read FWindow write FWindow;
    {- Coordinates of the main window}

    property WindowState: TWindowState
      read FWindowState write SetWindowState;
    {- State of the main window}

    property DefaultTimeout: Integer read FDefaultTimeout write SetDefaultTimeout; {!!.11}
    {- Default timeout for all Client components }

    property WorkingDirectory: String read FWorkingDirectory; {!!.11}
    {- Directory to save ini- and logfiles etc }
  end;

  procedure FFEConfigSaveFormPrefs(const Section : string; Form : TForm);
  procedure FFEConfigGetFormPrefs(const Section : string; Form : TForm);
  procedure FFEConfigSaveString(const Section, Ident, Value : string);
  function FFEConfigGetString(const Section, Ident, Default : string) : string;
  procedure FFEConfigSaveInteger(const Section, Ident : string; Value : Integer);
  function FFEConfigGetInteger(const Section, Ident : string; Default : Integer) : Integer;
  procedure FFEConfigSaveBoolean(const Section, Ident : string; Value : Boolean);
  function FFEConfigGetBoolean(const Section, Ident : string; Default : Boolean) : Boolean;
  procedure FFEConfigSaveDBColumnPrefs(const Section : string; Columns : TDBGridColumns);
  procedure FFEConfigGetDBColumnPrefs(const Section : string; Columns : TDBGridColumns);
  procedure FFEConfigSaveColumnPrefs(const Section : string; Grid : TStringGrid);
  procedure FFEConfigGetColumnPrefs(const Section : string; Grid : TStringGrid);

var
  Config : TffeConfig;
  FFEIni : TIniFile;
  IsReadOnly : Boolean;                                               {!!.06}

implementation

uses
  {$IFDEF UseRegistryConfig}
  Registry,
  {$ENDIF}
  ffllexcp,
  ffclbase,
  ffclcfg,
  ffconst;

procedure FFEConfigSaveFormPrefs(const Section : string; Form : TForm);
var
  Placement : TWindowPlacement;
begin
  if IsReadOnly then                                                  {!!.06}
    Exit;                                                             {!!.06}
  {Begin !!.11}
  { rewritten.
    NOTE: the 'width' and 'height' names below
    have been kept to keep leftover unused entries to a minimum.
    'windowstate' isn't suitable for reuse since the values are
    not compatible between WindowState and ShowCmd. }
  Placement.length :=SizeOf(TWindowPlacement);
  if not GetWindowPlacement(Form.Handle, @Placement) then
    Exit;
  with Placement do begin
    FFEIni.WriteInteger(Section, 'Flags', Flags);
    FFEIni.WriteInteger(Section, 'ShowCmd', ShowCmd);
    FFEIni.WriteInteger(Section, 'Left', rcNormalPosition.Left);
    FFEIni.WriteInteger(Section, 'Top', rcNormalPosition.Top);
    FFEIni.WriteInteger(Section, 'Width', rcNormalPosition.Right);
    FFEIni.WriteInteger(Section, 'Height', rcNormalPosition.Bottom);
  End;
  {End !!.11}
end;
{--------}
procedure FFEConfigGetFormPrefs(const Section : string; Form : TForm);
var
  Placement : TWindowPlacement;
begin
  {Begin !!.11}
  { rewritten.
    NOTE: the 'width' and 'height' names below
    have been kept to keep leftover unused entries to a minimum.
    'windowstate' isn't suitable for reuse since the values are
    not compatible between WindowState and ShowCmd. }
  with Placement do begin
    length := SizeOf(TWindowPlacement);
    Flags := FFEIni.ReadInteger(Section, 'Flags', 0);
    ShowCmd := FFEIni.ReadInteger(Section, 'ShowCmd', SW_SHOW);
    rcNormalPosition.Left := FFEIni.ReadInteger(Section, 'Left', Form.Left);
    rcNormalPosition.Top := FFEIni.ReadInteger(Section, 'Top', Form.Top);
    rcNormalPosition.Right := FFEIni.ReadInteger(Section, 'Width', Form.Left+Form.Width);
    rcNormalPosition.Bottom := FFEIni.ReadInteger(Section, 'Height', Form.Top+Form.Height);
    IF rcNormalPosition.Right > rcNormalPosition.Left THEN
      SetWindowPlacement(Form.Handle, @Placement)
  end;
  {End !!.11}
end;
{--------}
procedure FFEConfigSaveString(const Section, Ident, Value : string);
begin
  if IsReadOnly then                                                  {!!.06}
    Exit;                                                             {!!.06}
  FFEIni.WriteString(Section, Ident, Value);
end;
{--------}
function FFEConfigGetString(const Section, Ident, Default : string) : string;
begin
  Result := FFEIni.ReadString(Section, Ident, Default);
end;
{--------}
procedure FFEConfigSaveInteger(const Section, Ident : string; Value : Integer);
begin
  if IsReadOnly then                                                  {!!.06}
    Exit;                                                             {!!.06}
  FFEIni.WriteInteger(Section, Ident, Value);
end;
{--------}
function FFEConfigGetInteger(const Section, Ident : string; Default : Integer) : Integer;
begin
  Result := FFEIni.ReadInteger(Section, Ident, Default);
end;
{--------}
procedure FFEConfigSaveBoolean(const Section, Ident : string; Value : Boolean);
begin
  if IsReadOnly then                                                  {!!.06}
    Exit;                                                             {!!.06}
  FFEIni.WriteBool(Section, Ident, Value);
end;
{--------}
function FFEConfigGetBoolean(const Section, Ident : string; Default : Boolean) : Boolean;
begin
  Result :=   FFEIni.ReadBool(Section, Ident, Default);
end;
{--------}
procedure FFEConfigSaveDBColumnPrefs(const Section : string; Columns : TDBGridColumns);
var
  Idx : Integer;
begin
  if IsReadOnly then                                                  {!!.06}
    Exit;                                                             {!!.06}
  FFEIni.EraseSection(Section);
  for Idx := 0 to Pred(Columns.Count) do
    FFEConfigSaveString(Section, Columns[Idx].FieldName, IntToStr(Columns[Idx].Width));
end;
{--------}
procedure FFEConfigGetDBColumnPrefs(const Section : string; Columns : TDBGridColumns);
var
  Idx : Integer;
  Col : TColumn;
  ColumnInfo : TStringList;
  Dataset : TDataSet;
begin
  if Columns.Grid.FieldCount = 0 then Exit;
  
  Dataset := Columns.Grid.Fields[0].DataSet;
  ColumnInfo := TStringList.Create;
  try
    ColumnInfo.Sorted := False;
    FFEIni.ReadSection(Section, ColumnInfo);
    {Begin !!.10}
    { if there are new columns in the dataset, don't use stored column
      settings, otherwise the new columns end up to the far right. }
    for Idx := 0 to Pred(Dataset.FieldCount) do
      if ColumnInfo.IndexOf(Dataset.Fields[Idx].FieldName)<0 then begin
        Columns.RebuildColumns;
        Exit;
      end;
    {End !!.10}
    Columns.BeginUpdate;
    try
      Columns.Clear;
      for Idx := 0 to Pred(ColumnInfo.Count) do begin
        if (Dataset.FindField(ColumnInfo[Idx]) <> nil) then begin
          Col := Columns.Add;
          Col.FieldName := ColumnInfo[Idx];
          Col.Width := FFEConfigGetInteger(Section, Col.FieldName, Col.Width);
        end;
      end;
      for Idx := 0 to Pred(Dataset.FieldCount) do begin
        if (ColumnInfo.IndexOf(Dataset.Fields[Idx].FieldName) = -1) then begin
          Col := Columns.Add;
          Col.FieldName := Dataset.Fields[Idx].FieldName;
        end;
      end;  
    finally
      Columns.EndUpdate;
    end;
  finally
    ColumnInfo.Free;
  end;
end;
{--------}
procedure FFEConfigSaveColumnPrefs(const Section : string; Grid : TStringGrid);
var
  Idx : Integer;
begin
  if IsReadOnly then                                                  {!!.06}
    Exit;                                                             {!!.06}
  for Idx := 0 to Pred(Grid.ColCount) do
    FFEConfigSaveInteger(Section, IntToStr(Idx), Grid.ColWidths[Idx]);
end;
{--------}
procedure FFEConfigGetColumnPrefs(const Section : string; Grid : TStringGrid);
var
  Idx : Integer;
begin
  for Idx := 0 to Pred(Grid.ColCount) do
    Grid.ColWidths[Idx] := FFEConfigGetInteger(Section, IntToStr(Idx), Grid.ColWidths[Idx]);
end;
{--------}
constructor TffeConfig.Create;
begin
  {Begin !!.11}
  FWorkingDirectory := GetCurrentDir;
  if FWorkingDirectory='' then
    FWorkingDirectory := ExtractFilePath(Application.ExeName);
  if Copy(FWorkingDirectory, Length(FWorkingDirectory), 1)<>'\' then
    FWorkingDirectory := FWorkingDirectory + '\';
  {End !!.11}
  FINIFilename := FWorkingDirectory + ChangeFileExt(ExtractFileName(Application.ExeName), '.INI');
  {$IFDEF UseRegistryConfig}
  FRegistryKey := ffStrResClient[ffccREG_PRODUCT] + ffeRegistrySubKey;
  {$ENDIF}
  FRegisteredServers := TStringList.Create;
  Refresh;
end;
{--------}
destructor TffeConfig.Destroy;
begin
  FRegisteredServers.Free;
end;
{--------}
{function TffeConfig.GetProtocolName: TffShStr;
begin
  Result := FFClientConfigGetProtocolName(FProtocol);
end;}
{--------}
procedure TffeConfig.ParseWindowString(aWindow: TffShStr);
type
  TElement = (teLeft, teTop, teRight, teBottom);
var
  J: TElement;
  Element: TffShStr;
begin
  try
    J := teLeft;
    repeat
      FFShStrSplit(aWindow, ' ', Element, aWindow);
      case J of
        teLeft:   FWindow.Left   := StrToInt(Element);
        teTop:    FWindow.Top    := StrToInt(Element);
        teRight:  FWindow.Right  := StrToInt(Element);
        teBottom: FWindow.Bottom := StrToInt(Element);
      end;
      if J < High(J) then Inc(J);
    until aWindow = '';
  except
  end;
end;
{--------}
procedure TffeConfig.Refresh;
{$IFDEF UseINIConfig}
var
  Window: TffShStr;
{$ENDIF}
begin
  FOptions := [];
//  FProtocol := FFClientConfigReadProtocolClass;
  {$IFDEF UseINIConfig}
  with TINIFile.Create(FINIFilename) do
  try
    Window := ReadString(cfgKeyOptions, cfgWindow, '');
    if Window <> '' then
      ParseWindowString(Window);

    FSplitterPosition := ReadInteger(cfgKeyOptions, cfgSplitter, -1);

    FWindowState := TWindowState(ReadInteger(cfgKeyOptions, cfgWindowState, Ord(defWindowState)));

    if ReadBool(cfgKeyOptions, cfgShowBrowser, defcfgShowBrowser) then
      Include(FOptions, coShowBrowser);

    if ReadBool(cfgKeyOptions, cfgLiveDatasets, defcfgLiveDatasets) then
      Include(FOptions, coLiveDatasets);

    FSortAvailIndexFields := ReadBool(cfgKeyOptions, cfgSortAvailIndexFields, defcfgSortAvailIndexFields);

    ReadSection(cfgKeyRegisteredServers, FRegisteredServers);

    FDefaultTimeout := ReadInteger(cfgKeyOptions, cfgDefaultTimeout, 10000);

  finally
    Free;
  end;
  {$ENDIF}

  {$IFDEF UseRegistryConfig}
  with TRegistry.Create do
  try

    { set defaults }
    if defcfgShowBrowser then
      Include(FOptions, coShowBrowser);
    if defcfgLiveDatasets then
      Include(FOptions, coLiveDatasets);
    FSortAvailIndexFields := defcfgSortAvailIndexFields;
    FWindowState := defWindowState;

    { set and open the main key }
    RootKey := cfgRootKey;
    if KeyExists(FRegistryKey + '\' + cfgKeyOptions) then
    OpenKey(FRegistryKey + '\' + cfgKeyOptions, False);

    { get the window size, position }
    if ValueExists(cfgWindow) then
      ParseWindowString(ReadString(cfgWindow));

    FSplitterPosition := -1;
    if ValueExists(cfgSplitter) then
      FSplitterPosition := ReadInteger(cfgSplitter);

    if ValueExists(cfgWindowState) then
      FWindowState := TWindowState(ReadInteger(cfgWindowState));

    if ValueExists(cfgShowBrowser) then
      if ReadBool(cfgShowBrowser) then
        Include(FOptions, coShowBrowser)
      else
        Exclude(FOptions, coShowBrowser);

    if ValueExists(cfgLiveDatasets) then
      if ReadBool(cfgLiveDatasets) then
        Include(FOptions, coLiveDatasets)
      else
        Exclude(FOptions, coLiveDatasets);

    if ValueExists(cfgSortAvailIndexFields) then
      FSortAvailIndexFields := ReadBool(cfgSortAvailIndexFields);

    if ValueExists(cfgLastServer) then
      FLastServer := ReadString(cfgLastServer);

    {Begin !!.11}
    FDefaultTimeout := 10000;
    if ValueExists(cfgDefaultTimeout) then
      FDefaultTimeout := ReadInteger(cfgDefaultTimeout);
    {End !!.11}

    OpenKey(FRegistryKey + '\' + cfgKeyRegisteredServers, False);
    GetKeyNames(FRegisteredServers);
  finally
    Free;
  end;
  {$ENDIF}
end;
{--------}
procedure TffeConfig.Save;
var
  {$IFDEF UseINIConfig}                                            {BEGIN !!.01}
  I: Integer;
  {$ELSE}
  {$IFDEF UseRegistryConfig}
  I: Integer;
  {$ENDIF}
  {$ENDIF}                                                           {END !!.01}
begin
//  FFClientConfigWriteProtocolClass(FProtocol);
  {$IFDEF UseINIConfig}
  with TINIFile.Create(FINIFilename) do
  try
    try

      { Main window stuff }
      with FWindow do
        WriteString(cfgKeyOptions, cfgWindow, Format('%d %d %d %d', [Left, Top, Right, Bottom]));
      WriteInteger(cfgKeyOptions, cfgWindowState, Ord(FWindowState));
      WriteInteger(cfgKeyOptions, cfgSplitter, FSplitterPosition);

      { Options }
      WriteBool(cfgKeyOptions, cfgShowBrowser, (coShowBrowser in FOptions));
      WriteBool(cfgKeyOptions, cfgLiveDatasets, (coLiveDatasets in FOptions));
      WriteBool(cfgKeyOptions, cfgSortAvailIndexFields, FSortAvailIndexFields);
      WriteInteger(cfgKeyOptions, cfgDefaultTimeout, FDefaultTimeout);    {!!.11}

      { Registered Servers }
      EraseSection(cfgKeyRegisteredServers);
      with FRegisteredServers do
        for I := 0 to Count - 1 do
          WriteString(cfgKeyRegisteredServers, Strings[I], '');
    finally
      Free;
    end;
  except
    on E:Exception do
      ShowMessage('Error writing INI file: '+E.Message);
  end;
  {$ENDIF}
  {$IFDEF UseRegistryConfig}
  if (FRegistryKey <> '') and (FRegistryKey[1] = '\') then begin
    with TRegistry.Create do
    try
      RootKey := cfgRootKey;

      {delete the options key and all that's in it}
      DeleteKey(FRegistryKey + '\' + cfgKeyOptions);

      {create the options key afresh, make it the current key}
      OpenKey(FRegistryKey + '\' + cfgKeyOptions, True);

      {write out all the config info}

      { Window coordinates }
      with FWindow do
        WriteString(cfgWindow, Format('%d %d %d %d', [Left, Top, Right, Bottom]));
      WriteInteger(cfgWindowState, Ord(FWindowState));
      WriteInteger(cfgSplitter, FSplitterPosition);

      { Options }
      WriteBool(cfgShowBrowser, (coShowBrowser in FOptions));
      WriteBool(cfgLiveDatasets, (coLiveDatasets in FOptions));
      WriteBool(cfgSortAvailIndexFields, FSortAvailIndexFields);
      WriteInteger(cfgDefaultTimeout, FDefaultTimeout);    {!!.11}

      { Last server }
      WriteString(cfgLastServer, FLastServer);

      { Registered Servers }
      DeleteKey(FRegistryKey + '\' + cfgKeyRegisteredServers);
      CreateKey(FRegistryKey + '\' + cfgKeyRegisteredServers);
      with FRegisteredServers do
        for I := 0 to Count - 1 do
          CreateKey(FRegistryKey + '\' + cfgKeyRegisteredServers + '\' + Strings[I]);
    finally
      Free;
    end;
  end;
  {$ENDIF}
end;
{--------}
{Begin !!.11}
procedure TffeConfig.SetDefaultTimeout(const Value: Integer);
begin
  FDefaultTimeout := Value;
end;
{End !!.11}
{--------}
procedure TffeConfig.SetLastServer(aValue : string);
begin
  if FLastServer <> aValue then
    FLastServer := aValue;
end;
{--------}
{procedure TffeConfig.SetProtocol(aValue : TffCommsProtocolClass);
begin
  if FProtocol <> aValue then begin
    FProtocol := aValue;
    FFClientConfigWriteProtocolClass(FProtocol);
  end;
end;}
{--------}
procedure TffeConfig.SetWindowState(aValue : TWindowState);
begin
  if aValue = wsMinimized then
    aValue := wsNormal;

  if aValue <> FWindowState then
    FWindowState := aValue;
end;
{--------}
procedure InitUnit;
begin
  Config := TffeConfig.Create;
  if FileExists(Config.FINIFilename) then                             {!!.06}
    IsReadOnly := (FileGetAttr(Config.FINIFilename) and               {!!.06}
                   SysUtils.faReadOnly) <> 0                          {!!.06}
  else                                                                {!!.06}
    IsReadOnly := False;                                              {!!.06}

  FFEIni := TIniFile.Create(Config.FINIFilename);
end;
{--------}
procedure TermUnit;
begin
  Config.Free;
  Config := nil;
  FFEIni.Free;
  FFEIni := nil;
end;
{--------}

initialization
  InitUnit;
finalization
  TermUnit;
end.
