unit MMCFactory;

interface

uses ComObj, Snapins;

type
TMMCObjectFactory = class (TComObjectFactory)
private
  fAboutClassID : TGUID;
  fProvider : string;
public
  constructor Create(ComServer: TComServerObject; ComClass: TComponentDataClass;
    const ClassID: TGUID; SnapinAboutClass : TSnapinAboutClass; const AboutClassID : TGuid; const ClassName, Description, Provider: string);
  procedure UpdateRegistry (Register : boolean); override;
end;

implementation

uses Windows;

procedure CreateMMCKey(const Key, ValueName, Value: string);
var
  Handle: HKey;
  Status, Disposition: DWORD;
  MMCKey : string;
begin
  MMCKey := 'SOFTWARE\Microsoft\MMC';
  if Key [1] <> '\' then
    MMCKey := MMCKey + '\';
  MMCKey := MMCKey + Key;
  Status := RegCreateKeyEx(HKEY_LOCAL_MACHINE, PChar(MMCKey), 0, '',
    REG_OPTION_NON_VOLATILE, KEY_READ or KEY_WRITE, nil, Handle,
    @Disposition);
  if Status = 0 then
  begin
    Status := RegSetValueEx(Handle, PChar(ValueName), 0, REG_SZ,
      PChar(Value), Length(Value) + 1);
    RegCloseKey(Handle);
  end;
  if Status <> 0 then raise EOleError.Create('Unable to create snapin key');
end;

procedure DeleteMMCKey(const Key: string);
var
  MMCKey : string;
begin
  MMCKey := 'SOFTWARE\Microsoft\MMC';
  if Key [1] <> '\' then
    MMCKey := MMCKey + '\';
  MMCKey := MMCKey + Key;
  RegDeleteKey(HKEY_LOCAL_MACHINE, PChar(MMCKey));
end;

constructor TMMCObjectFactory.Create(ComServer: TComServerObject; ComClass: TComponentDataClass;
    const ClassID: TGUID; SnapinAboutClass : TSnapinAboutClass; const AboutClassID : TGuid; const ClassName, Description, Provider: string);
begin
  fProvider := Provider;
  inherited Create (ComServer, ComClass, ClassID, ClassName, Description, ciMultiInstance, tmBoth);
  fAboutClassID := AboutClassID;
  TComObjectFactory.Create (ComServer, SnapinAboutClass, AboutClassID, ClassName + 'About', Description + ' Description', ciMultiInstance, tmBoth);

end;

procedure TMMCObjectFactory.UpdateRegistry (Register : boolean);
var
  snapinKey : string;
begin
  snapinKey := 'Snapins\' + GUIDToString (ClassID);
  if Register then
  begin
    inherited UpdateRegistry (Register);
    CreateMMCKey (snapinKey, '', ClassName);
    CreateMMCKey (snapinKey + '\StandAlone', '', '');
    CreateMMCKey (snapinKey, 'Provider', fProvider);
    CreateMMCKey (snapinKey, 'NameString', ClassName);
    CreateMMCKey (snapinKey, 'About', GuidToString (fAboutClassID));
  end
  else
  begin
    DeleteMMCKey (snapinKey + '\StandAlone');
    DeleteMMCKey (snapinKey);
    inherited UpdateRegistry (Register);
  end
end;

end.

