{
  MetaData Example
    Loading & Editing Wma File Tags

    Available Fields:
    ================

    Encoder, Genre, Author, Title, Album, Composer,
    Url, Track, Year, CopyRight, Comments, Lyrics.

    Read Only Fields: Version, Duration, BitRate, FileSize, VBR.

    (C) 2004 Copyright Philip Hadar - Israel
        Philip@EverX.co.il
        WWW.EverX.co.il

   modifications / Delphi-Header by Chris Tr?ken
   Credits goes to Harold Oudshoorn for fixing that it will work fine under WMA-Codec 10 

   (No MetaData.DLL is needed)
}

// Bug Fix it seams that sometimes the TagHeader will show wrong Durations
// Now the Duration is a simple Calculation (more acurate)

// Bug fix at getting at getting TagsData.Comments  (2009-04-22)

unit WMAReader;

interface

{$INCLUDE Delphi_Ver.inc}

uses
  SysUtils,
  Windows,
  Dialogs;

type

  TTagsData = packed record
   {$IFDEF DELPHI_2007_BELOW}
    Encoder, Version, Genre, Author, Title, Album,
      Composer, Url, Track, Year, CopyRight: string[255];
   {$ELSE}
    Encoder, Version, Genre, Author, Title, Album,
      Composer, Url, Track, Year, CopyRight: string;
   {$ENDIF}
    Comments, Lyrics: WideString;
    BitRate: cardinal;
    Duration, Filesize: Int64;
    VBR: Boolean;
  end;
  PTagsData = ^TTagsData;

  PCardinal = ^Cardinal;

function OpenWMAFile(FileName: string; TagsData: PTagsData):boolean;
function SaveWMAFile(TagsData: PTagsData):boolean;
function DurationToStr(Duration: int64; ShowMs: boolean): string;

implementation
//{$R *.RES}

const
  IID_IWMMetadataEditor: TGUID = '{96406bd9-2b2b-11d3-b36b-00c04f6108ff}';
  IID_IWMHeaderInfo: TGUID = '{96406bda-2b2b-11d3-b36b-00c04f6108ff}';
  IID_IWMHeaderInfo3: TGUID = '{15CC68E3-27CC-4ecd-B222-3F5D02D80BD5}';
  IID_IWMHeaderInfo2: TGUID = '{15cf9781-454e-482e-b393-85fae487a810}';

  LibName = 'WMVCORE.DLL';

var
  fFileName: PWideChar;

type

  WMT_CODEC_INFO_TYPE = LongWord;

  WMT_ATTR_DATATYPE =
    (WMT_TYPE_DWORD,
    WMT_TYPE_STRING,
    WMT_TYPE_BINARY,
    WMT_TYPE_BOOL,
    WMT_TYPE_QWORD,
    WMT_TYPE_WORD,
    WMT_TYPE_GUID);

  TWMTAttrDataType = WMT_ATTR_DATATYPE;

type
  IWMMetadataEditor = interface(IUnknown)
    ['{96406BD9-2B2B-11d3-B36B-00C04F6108FF}']
    function Open(pwszFilename: PWideChar): HRESULT; stdcall;
    function Close: HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
  end;

type
  IWMHeaderInfo = interface(IUnknown)
    ['{96406BDA-2B2B-11d3-B36B-00C04F6108FF}']
    function GetAttributeCount(wStreamNum: Word; out pcAttributes: Word): HRESULT; stdcall;
    function GetAttributeByIndex(wIndex: Word; var pwStreamNum: Word;
      pwszName: PWideChar; var pcchNameLen: Word;
      out pType: TWMTAttrDataType; pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;
    function GetAttributeByName(var pwStreamNum: Word; pszName: PWideChar;
      out pType: TWMTAttrDataType; pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;
    function SetAttribute(wStreamNum: Word; pszName: PWideChar;
      Type_: TWMTAttrDataType; {in} pValue: PBYTE;
      cbLength: Word): HRESULT; stdcall;
    function GetMarkerCount(out pcMarkers: Word): HRESULT; stdcall;
    function GetMarker(wIndex: Word; pwszMarkerName: PWideChar;
      var pcchMarkerNameLen: Word; out pcnsMarkerTime: Int64): HRESULT; stdcall;
    function AddMarker(pwszMarkerName: PWideChar; cnsMarkerTime: Int64): HRESULT; stdcall;
    function RemoveMarker(wIndex: Word): HRESULT; stdcall;
    function GetScriptCount(out pcScripts: Word): HRESULT; stdcall;
    function GetScript(wIndex: Word; pwszType: PWideChar;
      var pcchTypeLen: Word; pwszCommand: PWideChar;
      var pcchCommandLen: Word; out pcnsScriptTime: Int64): HRESULT; stdcall;
    function AddScript(pwszType, pwszCommand: PWideChar;
      cnsScriptTime: Int64): HRESULT; stdcall;
    function RemoveScript(wIndex: Word): HRESULT; stdcall;
  end;

type
  IWMHeaderInfo2 = interface(IWMHeaderInfo)
    ['{15CF9781-454E-482e-B393-85FAE487A810}']
    function GetCodecInfoCount(out pcCodecInfos: LongWord): HRESULT; stdcall;
    function GetCodecInfo(wIndex: LongWord; var pcchName: Word;
      pwszName: PWideChar; var pcchDescription: Word;
      pwszDescription: PWideChar; out pCodecType: WMT_CODEC_INFO_TYPE;
      var pcbCodecInfo: Word; pbCodecInfo: PBYTE): HRESULT; stdcall;
  end;

type
  IWMHeaderInfo3 = interface(IWMHeaderInfo2)
    ['{15CC68E3-27CC-4ecd-B222-3F5D02D80BD5}']
    function GetAttributeCountEx(wStreamNum: Word; out pcAttributes: Word): HRESULT; stdcall;
    function GetAttributeIndices(wStreamNum: Word; pwszName: PWideChar;
      pwLangIndex: PWORD; pwIndices: PWORD; var pwCount: Word): HRESULT; stdcall;
    function GetAttributeByIndexEx(wStreamNum: Word; wIndex: Word; pwszName: PWideChar;
      var pwNameLen: Word; out pType: TWMTAttrDataType; out pwLangIndex: Word;
      pValue: PBYTE; var pdwDataLength: LongWord): HRESULT; stdcall;
    function ModifyAttribute(wStreamNum: Word; wIndex: Word; Type_: TWMTAttrDataType;
      wLangIndex: Word; pValue: PBYTE; dwLength: LongWord): HRESULT; stdcall;
    function AddAttribute(wStreamNum: Word; pszName: PWideChar;
      out pwIndex: Word; Type_: TWMTAttrDataType; wLangIndex: Word;
      pValue: PBYTE; dwLength: LongWord): HRESULT; stdcall;
    function DeleteAttribute(wStreamNum, wIndex: Word): HRESULT; stdcall;
    function AddCodecInfo(pwszName: PWideChar; pwszDescription: PWideChar;
      codecType: WMT_CODEC_INFO_TYPE; cbCodecInfo: Word;
      pbCodecInfo: PBYTE): HRESULT; stdcall;
  end;

var
  _WMCreateEditor: function(out ppEditor: IWMMetadataEditor): HRESULT; stdcall;
  DllHandle: THandle;
  ppEditor: IWMMetadataEditor;
  ppHeaderInfo3: IWMHeaderInfo3;

procedure ResetTagsData(TagsData: PTagsData);
begin

  TagsData.Duration := 0;
  TagsData.BitRate := 0;
  TagsData.FileSize := 0;
  TagsData.Encoder := '';
  TagsData.Version := '';
  TagsData.Genre := '';
  TagsData.Author := '';
  TagsData.Composer := '';
  TagsData.Title := '';
  TagsData.Album := '';
  TagsData.Year := '';
  TagsData.Track := '';
  TagsData.Url := '';
  TagsData.Comments := '';
  TagsData.Lyrics := '';
  TagsData.CopyRight := '';

end;

function GetAttribIndex(Attrib: string): Integer;
var
  AttribName: PWideChar;
  P, Lang: PWord;
  AttribLen: Word;
begin

  Result := -1;

  AttribName := StringToOleStr(Attrib);
  Lang := PWord(0);

  ppHeaderInfo3.GetAttributeIndices($FFFF, AttribName, Lang, nil, AttribLen);

  if AttribLen <> 0 then
  begin
    P := AllocMem(AttribLen);

    try
      ppHeaderInfo3.GetAttributeIndices($FFFF, AttribName, Lang, P, AttribLen);
      Result := PWord(P)^;
    finally
      FreeMem(P);
    end;

  end;

end;

function ShowTags(var FileName: PWideChar; var TagsData: PTagsData): Boolean; stdcall;
var
  AttributeCount: Word;
  //Duration : Int64;
  pType: TWMTAttrDataType;
  pValue: PByte;
  I: Integer;
  HR: HRESULT;
  // Vars for the integer Part
  nVal: integer;

  // Vars for the int64/QWORD Part
  nVal_Int64: int64;
  P_Int64: Pint64;
  // Vars for the DWORD Part
  nVal_Car: Cardinal;
  P_Car: PCardinal;
  //
  wIndex: Word;
  pwszName: PWideChar;
  pwNameLen: Word;
  pwLangIndex: Word;
  pdwDataLength: LongWord;

begin
  // Modified : Result value will be True only if the whole process is done successfully.
  //  by Silhwan Hyun
  Result := False;

  DllHandle := LoadLibrary(LibName);

  if DllHandle = 0 then
  begin
    ShowMessage(Format('Could not Load Library ($%x).', [DllHandle]));
  //  Result := False;
    Exit;
  end
  else
    _WMCreateEditor := GetProcAddress(DllHandle, 'WMCreateEditor');

  ResetTagsData(TagsData);

  if not FileExists(FileName) then
  begin
    raise Exception.Create(Format('The file' + #13#10 + '%S' + #13#10 + 'does''nt exists!',
      [FileName]));
    Exit;
  end;

  try

    HR := _WMCreateEditor(ppEditor);

    if Failed(HR) then
    begin
    //  Result := False;
      raise Exception.Create(Format('Could not create Metadata Editor ($%x).', [HR]));
      Exit;
    end;

    ppEditor.Open(FileName);

    HR := ppEditor.QueryInterface(IID_IWMHeaderInfo3, ppHeaderInfo3);

    if Failed(HR) then
    begin
    //  Result := False;
      raise Exception.Create(Format('Could not QI for IWMHeaderInfo3 ($%x).', [HR]));
      Exit;
    end;

   // Result := True;

    ppHeaderInfo3.GetAttributeCountEx(65535, AttributeCount);

    for I := 0 to AttributeCount - 1 do
    begin
      wIndex := Word(I);
      pwNameLen := 0;
      pType := WMT_TYPE_DWORD;
      pwLangIndex := 0;
      pdwDataLength := 0;
      ppHeaderInfo3.GetAttributeByIndexEx(65535, wIndex, nil, pwNameLen, pType, pwLangIndex, nil,
        pdwDataLength);

      pwszName := AllocMem(pwNameLen * 2);
      PValue := AllocMem(pdwDataLength);

      ppHeaderInfo3.GetAttributeByIndexEx(65535, wIndex, pwszName, pwNameLen, pType, pwLangIndex,
        pValue, pdwDataLength);

      if pType = WMT_TYPE_STRING then
      begin

        if UpperCase(pwszName) = 'WM/ENCODEDBY' then
        begin
        //  TagsData.Encoder := Widestring(PValue);
          TagsData.Encoder := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'WMFSDKVERSION' then
        begin
        //  TagsData.Version := Widestring(PValue);
          TagsData.Version := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'WM/GENRE' then
        begin
        //  TagsData.Genre := Widestring(PValue);
          TagsData.Genre := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'AUTHOR' then
        begin
        //  TagsData.Author := Widestring(PValue);
          TagsData.Author := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'TITLE' then
        begin
        //  TagsData.Title := Widestring(PValue);
          TagsData.Title := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'WM/ALBUMTITLE' then
        begin
        //  TagsData.Album := Widestring(PValue);
          TagsData.Album := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'WM/COMPOSER' then
        begin
        // TagsData.Composer := Widestring(PValue);
          TagsData.Composer := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'WM/URL' then
        begin
        //  TagsData.Url := Widestring(PValue);
          TagsData.Url := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'WM/YEAR' then
        begin
        //  TagsData.Year := Widestring(PValue);
          TagsData.Year := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'WM/TRACKNUMBER' then
        begin
        //  TagsData.Track := Widestring(PValue);
          TagsData.Track := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'DESCRIPTION' then
        begin
         //  TagsData.Comments := PWideChar(PValue);
           TagsData.Comments := Widestring(pWideChar(PValue));
           Continue;
        end;

        if UpperCase(pwszName) = 'WM/LYRICS' then
        begin
        //  TagsData.Lyrics := PWideChar(PValue);
          TagsData.Lyrics := Widestring(pWideChar(PValue));
          Continue;
        end;

        if UpperCase(pwszName) = 'COPYRIGHT' then
        begin
        //  TagsData.CopyRight := Widestring(PValue);
          TagsData.CopyRight := Widestring(pWideChar(PValue));
          Continue;
        end;

        Continue;
      end;

      if pType = WMT_TYPE_BOOL then
      begin
        if UpperCase(pwszName) = 'ISVBR' then
        begin
          nVal := pValue^;
          TagsData.VBR := (nVal <> 0);
        end;

        Continue;
      end;

      if pType = WMT_TYPE_DWORD then
      begin
        P_Car := PCardinal(PValue);
        nVal_Car := P_Car^;

        if UpperCase(pwszName) = 'BITRATE' then
          TagsData.BitRate := nVal_Car;

        Continue;
      end;

      if pType = WMT_TYPE_QWORD then
      begin
        P_Int64 := PInt64(PValue);
        nVal_Int64 := P_Int64^;

        if UpperCase(pwszName) = 'FILESIZE' then
        begin
          TagsData.FileSize := nVal_Int64;
          Continue;
        end;

        if UpperCase(pwszName) = 'DURATION' then
        begin
          TagsData.Duration := abs(nVal_int64 div 10000);
          Continue;
        end;

        Continue;
      end;


      FreeMem(pwszName);
      FreeMem(PValue);
    end;

    ppHeaderInfo3 := nil;
    Result := True;
  finally

    ppEditor.Close;
    ppEditor := nil;

    if DllHandle <> 0 then
    begin
      FreeLibrary(DllHandle);
      _WMCreateEditor := nil;
    end;

  end;

end;

function SaveTags(var FileName: PWideChar; var TagsData: PTagsData): Boolean; stdcall;
var
  nIndex, nLength: Integer;
  AttribName, pValue: PWideChar;
  pwIndex: Word;
  HR: HRESULT;
begin
  // Modified : Result value will be True only if the whole process is done successfully.
  //  by Silhwan Hyun
  Result := False;

  if not FileExists(FileName) then
  begin
    raise Exception.Create(Format('The file' + #13#10 + '%S' + #13#10 + 'does''nt exists!',
      [FileName]));
    Exit;
  end;

  DllHandle := LoadLibrary(LibName);

  if DllHandle = 0 then
  begin
    ShowMessage(Format('Could not Load Library ($%x).', [DllHandle]));
  //  Result := False;
    Exit;
  end
  else
    _WMCreateEditor := GetProcAddress(DllHandle, 'WMCreateEditor');

 // Result := True;

  HR := _WMCreateEditor(ppEditor);

  if Failed(HR) then
  begin
    raise Exception.Create(Format('Could not create Metadata Editor ($%x).', [HR]));
    Exit;
  end;

  HR := ppEditor.QueryInterface(IID_IWMHeaderInfo3, ppHeaderInfo3);

  if Failed(HR) then
  begin
    raise Exception.Create(Format('Could not QI for IWMHeaderInfo3 ($%x).', [HR]));
    Exit;
  end;

  ppEditor.Open(FileName);

  try

    AttribName := 'WM/EncodedBy';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Encoder);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Encoder <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'WM/Genre';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Genre);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Genre <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'Author';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Author);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Author <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'WM/Composer';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Composer);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Composer <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'Title';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Title);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Title <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'WM/AlbumTitle';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Album);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Album <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'WM/Year';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Year);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Year <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'WM/TrackNumber';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Track);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Track <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'WM/URL';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Url);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Url <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'Copyright';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Copyright);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Copyright <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'Description';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Comments);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Comments <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    AttribName := 'WM/Lyrics';
    nIndex := GetAttribIndex(AttribName);
    pValue := StringToOleStr(TagsData.Lyrics);
    nLength := Length(pValue) * 2;
    if nIndex >= 0 then
    begin

      if TagsData.Lyrics <> '' then
        ppHeaderInfo3.ModifyAttribute(0, nIndex, WMT_TYPE_STRING, 0, PByte(pValue), nLength)
      else
        ppHeaderInfo3.DeleteAttribute(0, nIndex);

    end
    else if nlength <> 0 then
      ppHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue),
        nLength);

    ppEditor.Flush;
    Result := True;
  finally

    ppHeaderInfo3 := nil;

    ppEditor.Close;
    ppEditor := nil;

    if DllHandle <> 0 then
    begin
      FreeLibrary(DllHandle);
      _WMCreateEditor := nil;
    end;

  end;

end;

function OpenWMAFile(FileName: string; TagsData: PTagsData):boolean;
begin
  Result := false;
  if not FileExists(FileName) then
    exit;
  fFileName := StringToOleStr(FileName);
  Result := ShowTags(fFileName, TagsData);
  if not Result then
     MessageDlg('Sorry ,Could not load the WMA Tag', mtError,[mbOk], 0);
end;

function SaveWMAFile(TagsData: PTagsData):boolean;
begin
  Result :=  SaveTags(fFileName, TagsData);
  if not Result then
    MessageDlg('Sorry ,Could not save the WMA-Tag', mtError,[mbOk], 0);
end;

function DurationToStr(Duration: int64; ShowMs: boolean): string;
begin
  if ShowMS then
  begin
    if Duration >= 3600000 then
      Result := Format('%d:%2.2d:%2.2d.%3.3d', [Duration div 3600000,
        (Duration mod 3600000) div 60000,
          (Duration mod 60000) div 1000,
          Duration mod 1000])
    else
      Result := Format('%d:%2.2d.%3.3d', [Duration div 60000,
        (Duration mod 60000) div 1000,
          Duration mod 1000]);
  end
  else
  begin
    if Duration >= 3600000 then
      Result := Format('%d:%2.2d:%2.2d', [Duration div 3600000,
        (Duration mod 3600000) div 60000,
          (Duration mod 60000) div 1000])
    else
      Result := Format('%d:%2.2d', [Duration div 60000,
        (Duration mod 60000) div 1000]);
  end;
end;

end.

