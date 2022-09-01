{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_wrapper;

interface
{$I psc_defines.inc}

{$WARNINGS OFF}

uses
  {$IFDEF D6}
  System.Variants,
  {$ENDIF}
  System.UITypes,
  System.Types,
  Winapi.activex,
//  Winapi.registry,
  Winapi.windows,
  System.typinfo,
  VCL.forms,
  Vcl.actnlist,
  Vcl.checklst,
  Vcl.graphics,
  Vcl.imglist,
  Vcl.controls,
  Vcl.dialogs,
  Vcl.dbgrids,
  Vcl.mask,
  Vcl.dbctrls,
  Vcl.buttons,
  Vcl.stdctrls,
  System.classes,
  Vcl.comctrls,
  Vcl.extctrls,
  System.sysutils,

  myla_system,
  myla_interfaces,

  psc_procs,
  psc_const;

{------------------------------}

const
  faOnlyFiles    = faAnyFile and not (faDirectory or System.SysUtils.faVolumeID);
  faDirsAndFiles = faAnyFile and not (System.SysUtils.faVolumeID);

  PSCDefaultOpenMode : integer = fmOpenRead or fmShareDenyWrite;

  PSC_SM_CXHSCROLL = 21;
  PSC_SM_CYHSCROLL = 3;

  PSC_COLOR_HOTLIGHT = 26;
  PSC_COLOR_GRADIENTACTIVECAPTION = 27;
  PSC_COLOR_GRADIENTINACTIVECAPTION = 28;

  PSC_ETO_OPAQUE = 2;
  PSC_ETO_CLIPPED = 4;
  PSC_ETO_GLYPH_INDEX = $10;
  PSC_ETO_RTLREADING = $80;
  PSC_ETO_NUMERICSLOCAL = $400;
  PSC_ETO_NUMERICSLATIN = $800;
  PSC_ETO_IGNORELANGUAGE = $1000;
  PSC_ETO_PDY = $2000;

  PSC_WS_OVERLAPPED = 0;
  PSC_WS_POPUP = Cardinal($80000000);
  PSC_WS_CHILD = $40000000;
  PSC_WS_MINIMIZE = $20000000;
  PSC_WS_VISIBLE = $10000000;
  PSC_WS_DISABLED = $8000000;
  PSC_WS_CLIPSIBLINGS = $4000000;
  PSC_WS_CLIPCHILDREN = $2000000;
  PSC_WS_MAXIMIZE = $1000000;
  PSC_WS_CAPTION = $C00000;      { WS_BORDER or WS_DLGFRAME  }
  PSC_WS_BORDER = $800000;
  PSC_WS_DLGFRAME = $400000;
  PSC_WS_VSCROLL = $200000;
  PSC_WS_HSCROLL = $100000;
  PSC_WS_SYSMENU = $80000;
  PSC_WS_THICKFRAME = $40000;
  PSC_WS_GROUP = $20000;
  PSC_WS_TABSTOP = $10000;

  PSC_WS_MINIMIZEBOX = $20000;
  PSC_WS_MAXIMIZEBOX = $10000;

  PSC_WS_TILED = PSC_WS_OVERLAPPED;
  PSC_WS_ICONIC = PSC_WS_MINIMIZE;
  PSC_WS_SIZEBOX = PSC_WS_THICKFRAME;

  PSC_WS_OVERLAPPEDWINDOW = (PSC_WS_OVERLAPPED or PSC_WS_CAPTION or PSC_WS_SYSMENU or
    PSC_WS_THICKFRAME or PSC_WS_MINIMIZEBOX or PSC_WS_MAXIMIZEBOX);
  PSC_WS_TILEDWINDOW = PSC_WS_OVERLAPPEDWINDOW;
  PSC_WS_POPUPWINDOW = (PSC_WS_POPUP or PSC_WS_BORDER or PSC_WS_SYSMENU);
  PSC_WS_CHILDWINDOW = (PSC_WS_CHILD);

  PSC_WS_EX_DLGMODALFRAME = 1;
  PSC_WS_EX_NOPARENTNOTIFY = 4;
  PSC_WS_EX_TOPMOST = 8;
  PSC_WS_EX_ACCEPTFILES = $10;
  PSC_WS_EX_TRANSPARENT = $20;
  PSC_WS_EX_MDICHILD = $40;
  PSC_WS_EX_TOOLWINDOW = $80;
  PSC_WS_EX_WINDOWEDGE = $100;
  PSC_WS_EX_CLIENTEDGE = $200;
  PSC_WS_EX_CONTEXTHELP = $400;

  PSC_WS_EX_RIGHT = $1000;
  PSC_WS_EX_LEFT = 0;
  PSC_WS_EX_RTLREADING = $2000;
  PSC_WS_EX_LTRREADING = 0;
  PSC_WS_EX_LEFTSCROLLBAR = $4000;
  PSC_WS_EX_RIGHTSCROLLBAR = 0;

  PSC_WS_EX_CONTROLPARENT = $10000;
  PSC_WS_EX_STATICEDGE = $20000;
  PSC_WS_EX_APPWINDOW = $40000;
  PSC_WS_EX_OVERLAPPEDWINDOW = (PSC_WS_EX_WINDOWEDGE or PSC_WS_EX_CLIENTEDGE);
  PSC_WS_EX_PALETTEWINDOW = (PSC_WS_EX_WINDOWEDGE or PSC_WS_EX_TOOLWINDOW or PSC_WS_EX_TOPMOST);

  PSC_WS_EX_LAYERED = $00080000;
  PSC_WS_EX_NOINHERITLAYOUT = $00100000; // Disable inheritence of mirroring by children
  PSC_WS_EX_LAYOUTRTL = $00400000; // Right to left mirroring
  PSC_WS_EX_NOACTIVATE = $08000000;

  BrushStyle_Clear=bsClear;
  BrushStyle_Solid=bsSolid;

  PenStyle_Solid=psSolid;
  PenStyle_Dot=psDot;

  FontStyle_Bold=fsBold;
  FontStyle_Italic=fsItalic;
  FontStyle_Underline=fsUnderline;
  FontStyle_StrikeOut=fsStrikeOut;

  CharCase_Normal=ecNormal;

type
  TPSCFileFoundCallBack=procedure (const APath:String;const ASearchRec: TSearchRec;
    Var AContinue:boolean) of object;

  TPSCImage=TImage;

//  THandle=LongWord;

  TPSCMethod=TMethod;

  TPSCIdentMapEntry=TIdentMapEntry;

  TPSCBevel=class(TBevel)
  end;

  TPSCProgressBar=class(TProgressBar)
  end;

  TPSCColorDialog=class(TColorDialog)
  end;

  TPSCFontDialog=class(TFontDialog)
  end;

  TPSCSaveDialog=class(TSaveDialog)
  end;

  TPSCOpenDialog=class(TOpenDialog)
  end;

  TPSCOwnerDrawState=TOwnerDrawState;

  TPSCEditCharCase=TEditCharCase;
  TPSCImageIndex=TImageIndex;
  TPSCImageList=TImageList;

  TPSCAlignment=TAlignment;

  TPSCBitmap=TBitmap;
  {$NODEFINE TPSCBitmap}
  {$HPPEMIT 'typedef Graphics::TBitmap TPSCBitmap;'}

  TPSCCanvas=Vcl.graphics.TCanvas;
  TPSCControlCanvas=TControlCanvas;
  TPSCFont=TFont;
  TPSCMetaFile=TMetaFile;
  TPSCDBEdit=TDBEdit;
  TPSCDBGrid=TDBGrid;
  TPSCSplitter=TSplitter;
  TPSCTimer=TTimer;
  TPSCMaskEdit=class(TMaskEdit)
  end;
  TPSCAction=TAction;
  TPSCActionList=TActionList;

function PSCAllocPatternBitmap(BkColor, FgColor: TPSCColor): TPSCBitmap;
function PSCColorToRGB(AColor:TPSCColor):LongInt;
procedure PSCUpdateCanvasState(ACanvas:TPSCCanvas);
function PSCGetDecimalSeparator:Char;
procedure PSCSetDecimalSeparator(AValue:Char);
function PSCLongMonthNames(AIndex:Integer):string;
function PSCShortMonthNames(AIndex:Integer):string;
function PSCShortTimeFormat:String;
function PSCLongTimeFormat:String;
function PSCShortDateFormat:String;
function PSCLongDateFormat:String;
function PSCWindowsNTOrHigher : boolean;
function PSCWindows2kOrHigher : boolean;
function PSCWindowsXPOrHigher : boolean;
function PSCTimeSeparator:Char;
function PSCDateSeparator:Char;
Function PSCUnderWindowsNT: boolean;
Function PSCGetSystemMetrics(nIndex: Integer): Integer;
function PSCGetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;
function PSCFindResourceInstance(const ResName:String;ResType:PChar):THandle;

function PSCMinDateTime:TDateTime;
function PSCMaxDateTime:TDateTime;

procedure PSCModulesToList(List:TList);
Procedure PSCAssignAllProps(Source,Dest: TPersistent);
Procedure PSCAssignAllPropsExclude(Source,Dest: TPersistent;
  const ExcludeList:IPSCStrings);
Procedure PSCAssignPropInfo(Source,Dest: TPersistent; SourceInfo,DestInfo:
  PPropInfo);
Procedure PSCSetPropValue(C: TPersistent; Const PropName: String;
  Const Value: Variant);
Procedure PSCSetPropValueByPropInfo(Instance: TPersistent; Propinfo: PPropInfo;
  Const Value: Variant);
Procedure PSCAssignAllPropsExcludeStr(Source,Dest: TPersistent; Const
  ExcludeProps: String);
Procedure PSCAssignProps(Source,Dest: TPersistent; Const PropNames: Array Of
  String);
Function PSCGetPropValue(Instance: TPersistent; Const PropName: String):
  Variant;
Function PSCGetPropValueByPropInfo(Instance: TPersistent; Propinfo: PPropInfo):
  Variant;
Function PSCGetPTypeInfo(PropInfo: PPropInfo): PTypeInfo;
Function PSCGetPTypeData(PropInfo: PPropInfo): PTypeData;
procedure PSCGetEnumTypeValueNames(TypeInfo:PTypeInfo;const AValueNames: IPSCStrings);
Procedure PSCAssignProp(Source,Dest: TPersistent; Const Propname: String);
Function PSCGetNotStoredProps(Instance: TPersistent): String;
Function PSCPropertyExists(C: TPersistent; Const PropName: String): Boolean;
Function PSCAssignStringsProp(Instance: TPersistent; Const PropName: String;
  Value: TStrings): boolean;
Function PSCGetStringsPropAsText(Instance: TPersistent;
  Const PropName: String): String;
Procedure PSCPropNamesToStrings(Instance: TPersistent; const PropNames: IPSCStrings;
  StoredProps,NotStoredProps: boolean);
//Function PSCGetHTMLCompiler(Const ACompilerName: String): String;
//Function PSCGetHLPCompiler(Const ACompilerName: String): String;
//function PSCGetDelphiRegKey(VerType:TPSCDelphiVer;const KeyName:String):String;
//function PSCGetSpecificDelphiDir(vertype : TPSCDelphiVer) : string;
function PSCGetDelphiVerStr(ADelphiVer: TPSCDelphiVer): string;
//function PSCGetDelphiHelpFolder(DelphiVer:TPSCDelphiVer):string;
Function PSCGetDelphiHelpFileName(ADelphiVer:TPSCDelphiVer):String;
//function  PSCGetDelphiHelpCntFile(DelphiVer : TPSCDelphiVer) : string;
procedure PSCRemoveHelpFromCntFile(const HelpFile, CntFile: string);
procedure PSCAddHelpToCntFile(const HelpFile, HelpName, CntFile: string);
Function PSCRunProgram(const CmdLine,WorkDir:String;Var ErrorMessage:String;
  HookOutput:Boolean):Boolean;
Function PSCGetTemporaryPath:String;
//function PSCGetDelphiSourceDirsEx(DelphiVer:TPSCDelphiVer):string;
//function PSCGetDelphiSourceDirsEx2(DelphiVer:TPSCDelphiVer;Opts:TPSCDelphiSourceDirsOpts) : string;
function PSCGenDPUPath(DelphiVer:TPSCDelphiVer):String;
procedure PSCModifyFileAttr(const FileName:String;RemoveAttr,AddAttr:Integer);
function PSCCopyFile(const ASrcFile, ADestFile: string): Boolean;
Procedure PSCAssignPropsFromIniStrings( AInstance : TPersistent;
  const AIniStrings : IPSCStrings;AContainTags : Boolean= False;
  Const AGeneralTagName : String= '');
Procedure PSCSetStringsPropsFromIniStrings( AInstance : TPersistent;
       const AIniStrings, APropNames : IPSCStrings;
       ARemoveSection : Boolean= False);
Function PSCIsPropStrings( APropInfo : PPropInfo ) : Boolean;
Procedure PSCSetObjectProp( Instance : TObject; PropInfo : PPropInfo;
  Value : TObject);
Function PSCGetObjectPropClass( Instance : TObject; PropInfo : PPropInfo ) : TClass;
function PSCGetTemporaryFileName(const Prefix : string) : string;
//function PSCCompileFile(DelphiVer: TPSCDelphiVer;const AFullFileName,
//  OutDir,ExtCMDParams: string;var ResultFile,ErrorMessage:string): Boolean;
function PSCGUIDToString(const ClassID: TGUID): string;
procedure  PSCModifyDefines(const FileName: string; Products: TstringList);
Function PSCSubStr(Const S: String; Const Index: Integer;
  Const Separator: String): String;
Function PSCFolderExists(Const Name: String): Boolean;
function PSCAddBitmapFromResourceEx(ImageList: TPSCImageList;
  const ResName: string;Instance: THandle): Integer;
function PSCInsertBitmapFromResourceEx(ImageList: TPSCImageList;
  const ResName: string;Instance: THandle;const Index: Integer): Integer;
function PSCReplaceBitmapFromResourceEx(ImageList: TPSCImageList;
  const ResName: string;Instance: THandle;Index: Integer): Integer;
function PSCAddBitmapFromResource(ImageList:TPSCImageList;
  const ResName:String): Integer;
function PSCInsertBitmapFromResource(ImageList:TPSCImageList;
  const ResName:String; const Index: Integer): Integer;
function PSCReplaceBitmapFromResource(ImageList:TPSCImageList;
  const ResName:String; const Index: Integer): Integer;
procedure PSCLoadBitmapFromResource(ABitmap:TPSCBitmap;const AResName:String);
Function PSCChangeCharTo(FromChar,ToChar: Char; Const S: String): String;
Procedure PSCItemHtDrawEx(Canvas: TPSCCanvas; ARect: TRect; Const Text: String;
  Var PlainItem: String; Var Width: Integer; CalcWidth: Boolean);
Function PSCCorrectBool(Const V: Variant): Variant;
procedure PSCModifyFilesAttr(const Files:IPSCStrings;RemoveAttr,AddAttr:Integer);
procedure PSCAssignPropsFromStrings(Instance:TPersistent;const Params:IPSCStrings);
//function PSCGetCompilerPathAndParams(DelphiVer:TPSCDelphiVer;var Params:String):String;
//function PSCGetBestDVersionFromSet(DelphiVers:TPSCDelphiVers):TPSCDelphiVer;
//function PSCGetDelphiToolsAPIDir(DelphiVer:TPSCDelphiVer) : string;
function PSCSmartExpandRelativePath(const RelativePath: string): string;
procedure PSCSmartExpandPathInStrings(const Strings:IPSCStrings;const Names:String);
procedure PSCRenameToBak(Const Path:String);
procedure PSCCreateAllDirectories(Dir : string; const CreatedDirs : IPSCStrings);
procedure PSCCreateFoldersEx(const Folders,CreatedFolders:IPSCStrings);
procedure PSCFindFiles(const Path: string; Attr: Integer;SubFolders:boolean;
  const Files:IPSCStrings);
function PSCForEachFile(const Path: string; Attr: Integer;SubFolders:boolean;
            CallBack:TPSCFileFoundCallBack):Integer;
Procedure PSCDrawCheck(Canvas: TCanvas; Const R: TRect; AState: TPSCCheckBoxState;
  Flat: Boolean; BkColor: TColor; AEnabled:Boolean);
procedure PSCSetFileDateTime(const AFileName: String;
  const ADateTime: TDateTime);

function PSCRect(ALeft, ATop, ARight, ABottom: Integer): TRect;overload;
function PSCPoint(AX, AY: Integer): TPoint;overload;
{------------------------------------------------------------------}

implementation

uses
  myla_parser;

{---------------------------------------------------------------}

type
  TFindFilesClass=class
  private
    FStrings:IPSCStrings;
  public
    procedure CallBack(const Path:String;const SearchRec: TSearchRec;
      Var AContinue:boolean);
  end;

{---------------------------------------------------------------}

procedure TFindFilesClass.CallBack(const Path:String;const SearchRec: TSearchRec;
  Var AContinue:boolean);
begin
  FStrings.Add(Path+SearchRec.Name);
end;

{---------------------------------------------------------------}

function PSCForEachFile(const Path: string; Attr: Integer;SubFolders:boolean;
            CallBack:TPSCFileFoundCallBack):Integer;

  function Iterat(const Path:String):Integer;
  Var
    SearchRec:TSearchRec;
    R:Integer;
    MyPath,MyFile:String;
    AContinue:boolean;
    FirstR:Integer;
  begin
    Result:=0;
    MyPath:=PSCAddSlash(ExtractFilePath(Path));
    If not PSCFolderExists(MyPath) then  //  Added in V1.4
      exit;                              //

    MyFile:=ExtractFileName(Path);

//----- search and process files -------------

    FirstR:=FindFirst(Path,Attr,SearchRec);
    R:=FirstR;
    try
      AContinue:=True;
      While (R=0) and AContinue do
       begin
         If (SearchRec.Attr and Attr <> 0) or
            ((SearchRec.Attr = 0) and (Attr and (faAnyFile - faDirectory) <> 0)) then
          begin
            CallBack(MyPath,SearchRec,AContinue);
            inc(Result);
          end;
         R:=FindNext(SearchRec);
       end;
    finally
      If FirstR=0 then
        System.SysUtils.FindClose(SearchRec);
    end;

//----------------- iterate subfolders if needed ----------
    If SubFolders then
    begin
      FirstR:=FindFirst(MyPath+'*.*',faDirectory,SearchRec);
      R:=FirstR;
      try
        While R=0 do
        begin
          If (SearchRec.Name[1]<>'.') and (SearchRec.Attr and faDirectory<>0) then
            Inc(Result,Iterat(MyPath+SearchRec.Name+'\'+MyFile));
          R:=FindNext(SearchRec);
        end;
      finally
        If FirstR=0 then
          System.SysUtils.FindClose(SearchRec);
      end;
    end;
  end;

begin
  If Assigned(CallBack) then
    Result:=Iterat(Path)
  else
    Result:=0;
end;

{---------------------------------------------------------------}

procedure PSCFindFiles(const Path: string; Attr: Integer;SubFolders:boolean;
  const Files:IPSCStrings);
var
  FindFilesObj:TFindFilesClass;
begin
  FindFilesObj:=TFindFilesClass.Create;
  FindFilesObj.FStrings:=Files;
  With Files do
    try
      BeginUpdate;
      Clear;
      PSCForEachFile(Path,Attr,SubFolders,FindFilesObj.CallBack);
    finally
      EndUpdate;
      FindFilesObj.Free;
    end;
end;

{------------------------------------------------------------}

procedure PSCCreateFoldersEx(const Folders,CreatedFolders:IPSCStrings);
var
  i:Integer;
begin
  for i:=0 to Folders.Count-1 do
    PSCCreateAllDirectories(Folders[i],CreatedFolders);
end;

{------------------------------------------------------------}

procedure PSCCreateAllDirectories(Dir : string; const CreatedDirs : IPSCStrings);
var
  ldir : integer;
  fpath : string;
begin
  ldir := Length(Dir);
  if ldir = 0 then
    exit;

  if (Dir[ldir] = '\') then
    Delete(Dir, ldir, 1);

  fpath := ExtractFilePath(Dir);

  if (Length(Dir) < 3) or PSCFolderExists(Dir) or (fPath = Dir) then
    Exit;

  PSCCreateAllDirectories(fPath, CreatedDirs);
  CreateDir(Dir);

  if CreatedDirs <> nil then
    CreatedDirs.Add(Dir);
end;

{------------------------------------------------------------------}

procedure PSCRenameToBak(Const Path:String);
Var
  BakName:String;
begin
  BakName:=PSCFileBakName(Path);
  If FileExists(Path) then
  begin
    If FileExists(BakName) then
    begin
      PSCModifyFileAttr( BakName, System.SysUtils.faReadOnly, 0 );
      DeleteFile( BakName);
    end;
    RenameFile(Path,BakName);
  end;
end;

{-------------------------------------------}

procedure PSCSmartExpandPathInStrings(const Strings:IPSCStrings;const Names:String);
var
  i:Integer;
  Temp:String;
  NamePart,ValuePart:String;
begin
  Temp:=';'+Names+';';
  for i:=0 to Strings.Count-1 do
  begin
    NamePart:=PSCExtractNamePart(Strings[i]);
    ValuePart:=PSCExtractValuePart(Strings[i]);

    If (Names='') or (PSCPosIgnoreCase(NamePart,Temp)>0) then
      Strings[i]:=NamePart+'='+PSCSmartExpandRelativePath(ValuePart);
  end;
end;

{--------------------------------------}

function PSCSmartExpandRelativePath(const RelativePath: string): string;
begin
  Result := PSCExpandRelativePathEx(GetCurrentDir, RelativePath);
  if PSCFolderExists(Result)
   then Exit;

  Result := PSCExpandRelativePath(RelativePath);
end;

{--------------------------------------}
{
const
  SPSCToolsApiDir='Source\Toolsapi';

function PSCGetDelphiToolsAPIDir(DelphiVer:TPSCDelphiVer) : string;
begin
  Result := PSCGetSpecificDelphiDir(DelphiVer);
  if Result <> '' then
    Result := Result + SPSCToolsAPIDir;
end;
}
{-------------------------------}
{
function PSCGetBestDVersionFromSet(DelphiVers:TPSCDelphiVers):TPSCDelphiVer;
var
  i:TPSCDelphiVer;
  CompilerPath,Params:String;
begin
  for i:=High(TPSCDelphiVer) downto Low(TPSCDelphiVer) do
  begin
    Result:=i;
    if i in DelphiVers then
    begin
      CompilerPath:=PSCGetCompilerPathAndParams(i,Params);
      If FileExists(CompilerPath) then
        exit;
    end;
  end;
end;
}
{------------------------------------------------------------------}

procedure PSCAssignPropsFromStrings(Instance:TPersistent;const Params:IPSCStrings);
Var
  PropNames:IPSCStrings;
  i:Integer;
  PValue:String;
begin
  PropNames:=PSCCreateStringList;
  PSCPropNamesToStrings(Instance,PropNames,True,True);
  With PropNames do
    for i:=0 to Count-1 do
    begin
      PValue:=Trim(Params.Values[Trim(Strings[i])]);
      If PValue<>'' then
        PSCSetPropValue(Instance,Strings[i],PValue);
    end;
end;

{------------------------------------------------------------------}

Function PSCCorrectBool(Const V: Variant): Variant;
Begin
{$IFDEF D6}
  If VarIsType(V,VarBoolean) And V Then
    Result:=1
  else
    Result:=V;
{$ELSE}
  If (TVarData(V).VType=VarBoolean) And V Then
    Result:=1
  else
    Result:=V;
{$ENDIF}
End;

{-------------------------------------------------------------------------}

Function PSCChangeCharTo(FromChar,ToChar: Char; Const S: String): String;
Var
  i: Integer;
Begin
  Result := S;
  For i := 1 To Length(Result) Do
    If Result[i] = FromChar Then
      Result[i] := ToChar;
End;

{------------------------------------------------------------------}

procedure PSCLoadBitmapFromResource(ABitmap:TPSCBitmap;const AResName:String);
Var
  H:THandle;
begin
  H:=PSCFindResourceInstance(AResName,RT_BITMAP);
  If H<>0 then
    ABitmap.LoadFromResourceName(H, AResName);
end;

{------------------------------------------------------------------}

function PSCAddBitmapFromResource(ImageList:TPSCImageList;
  const ResName:String):Integer;
Var
  H:THandle;
begin
  H:=PSCFindResourceInstance(ResName,RT_BITMAP);

  If H=0 then
    Result:=-1
  else
    Result:=PSCAddBitmapFromResourceEx(ImageList,ResName,H);
end;

{------------------------------------------------------------------}

function PSCAddBitmapFromResourceEx(ImageList: TPSCImageList;
  const ResName: string;Instance: THandle): Integer;
var
  Bitmap: TPSCBitmap;
begin
  Bitmap := TPSCBitmap.Create;
  try
    Bitmap.LoadFromResourceName(Instance, ResName);
    Result := ImageList.AddMasked(Bitmap, BitMap.TransparentColor)
  finally
    Bitmap.Free;
  end;
end;

{------------------------------------------------------------}

function PSCInsertBitmapFromResource(ImageList:TPSCImageList;
  const ResName:String; const Index: Integer): Integer;
Var
  H:THandle;
begin
  H:=PSCFindResourceInstance(ResName,RT_BITMAP);

  If H=0 then
    Result :=-1
  else
    Result := PSCInsertBitmapFromResourceEx(ImageList,ResName,H,Index);
end;

{------------------------------------------------------------------}

function PSCInsertBitmapFromResourceEx(ImageList: TPSCImageList;
  const ResName: string;Instance: THandle;const Index: Integer) : Integer;
var
  Bitmap: TPSCBitmap;
begin
  Result := -1;  
  Bitmap := TPSCBitmap.Create;
  try
    Bitmap.LoadFromResourceName(Instance, ResName);
    ImageList.InsertMasked(Index, Bitmap, BitMap.TransparentColor);
    Result := Index+1;
  finally
    Bitmap.Free;
  end;
end;

{------------------------------------------------------------}

function PSCReplaceBitmapFromResource(ImageList:TPSCImageList;
  const ResName:String; const Index: Integer): Integer;
Var
  H:THandle;
begin
  H:=PSCFindResourceInstance(ResName,RT_BITMAP);

  If H=0 then
    Result:=-1
  else
    Result := PSCReplaceBitmapFromResourceEx(ImageList,ResName,H,Index);
end;

{------------------------------------------------------------------}

function PSCReplaceBitmapFromResourceEx(ImageList: TPSCImageList;
  const ResName: string;Instance: THandle;Index: Integer) : Integer;
var
  Bitmap: TPSCBitmap;
begin
  Result := -1;
  Bitmap := TPSCBitmap.Create;
  try
    Bitmap.LoadFromResourceName(Instance, ResName);
    ImageList.ReplaceMasked(Index, Bitmap, BitMap.TransparentColor);
    Result := Index;
  finally
    Bitmap.Free;
  end;
end;

{------------------------------------------------------------}

Function PSCFolderExists(Const Name: String): Boolean;
Var
  Code: Integer;
Begin
  Code := FileGetAttr(Name);
  Result := (Code <> -1) And (FILE_ATTRIBUTE_DIRECTORY And Code <> 0);
End;

{------------------------------------------------------------------}

function PSCGUIDToString(const ClassID: TGUID): string;
{$IFNDEF LINUX}
var
  P: PWideChar;
begin
  StringFromCLSID(ClassID, P);
  Result := WideCharToString(P);
  CoTaskMemFree(P);
end;
{$ELSE}
begin
  Result:=GUIDToString(ClassID);
end;
{$ENDIF}

{--------------------------------------}

function PSCGetTemporaryFileName(const Prefix : string) : string;
var
  FileName : array[0..1023] of char;
begin
  GetTempFileName(PChar(PSCGetTemporaryPath),PChar(Prefix), 0, FileName);
  result := FileName;
end;

{------------------------------------------------------------------}

Function PSCIsPropStrings( APropInfo : PPropInfo ) : Boolean;
begin
  Result := ( PSCGetPTypeInfo( APropInfo ).Kind = tkClass ) And
            ( PSCGetPTypeData(APropInfo)^.ClassType.InheritsFrom( TStrings ) );
end;

{------------------------------------------------------------------}

Function PSCGetIniStringsSection( Const ASection : String;
  const AIniStrings, ASectionStrings : IPSCStrings;
  ARemoveSection : Boolean=False) : Boolean;
Var
  ValueStart : Integer;
  Line       : String;

  Procedure IncValueStart;
  begin
    If ARemoveSection Then
      AIniStrings.Delete( ValueStart )
    Else
      Inc( ValueStart );
  end;

begin
  Result := False;
  ValueStart := AIniStrings.IndexOf( '[' + ASection + ']' );
  If ValueStart <> -1 Then
  begin
    If ASectionStrings<>nil then
      ASectionStrings.Clear;
    Result := True;
    IncValueStart;
    While ValueStart < AIniStrings.Count Do
    begin
      Line := Trim( AIniStrings[ ValueStart ] );
      If Line <> '' Then
      begin
        If Line[ 1 ] = '[' Then
          Exit;
        If ASectionStrings<>nil then
          ASectionStrings.Add( Line );
      end;
      IncValueStart;
    end;
  end;
end;

{------------------------------------------------------------------}

Function PSCGetObjectPropClass( Instance : TObject; PropInfo : PPropInfo ) : TClass;
Var
  TypeData: PTypeData;
begin
  TypeData := PSCGetPTypeData(PropInfo);
  If TypeData = Nil Then
    Raise Exception.Create(PSCConsts.ErrUnkProp);
  Result := TypeData^.ClassType;
end;

{------------------------------------------------------------------}

Procedure PSCSetObjectProp( Instance : TObject; PropInfo : PPropInfo;
  Value : TObject);
begin
  If ( Value is PSCGetObjectPropClass( Instance, PropInfo ) ) Or
     ( Value = Nil ) Then
    SetOrdProp( Instance, PropInfo, Integer( Value ) );
end;

{------------------------------------------------------------------}

Procedure PSCSetStringsPropsFromIniStrings( AInstance : TPersistent;
       const AIniStrings, APropNames : IPSCStrings; ARemoveSection : Boolean= False);
Var
  PNames, PValues : IPSCStrings;
  I               : Integer;
  TagName         : String;
  MyValues        : TStringList;
  j               : Integer;
begin
  PNames := PSCCreateStringList;
  PValues := PSCCreateStringList;
  PSCPropNamesToStrings( AInstance, PNames, True, True );
  With PNames Do
    For I := 0 To Count - 1 Do
    If PSCIsPropStrings( PPropInfo( Objects[ I ] ) ) Then
      begin
        TagName := '';
        If Assigned( APropNames ) Then
          TagName := APropNames.Values[ Strings[ I ] ];
        If TagName = '' Then
          TagName := Strings[ I ];
        If PSCGetIniStringsSection( TagName, AIniStrings, PValues, ARemoveSection ) Then
        begin
          MyValues:=TStringList.Create;
          try
            for j:=0 to PValues.Count-1 do
              MyValues.Add(PValues[j]);
            PSCSetObjectProp( AInstance, PPropInfo( Objects[ I ] ), MyValues );
          finally
            MYValues.Free;
          end;
        end;
      end;
end;

{-------------------------------------------------------------}

Procedure PSCAssignPropsFromIniStrings( AInstance : TPersistent;
  const AIniStrings : IPSCStrings;AContainTags : Boolean= False;
  Const AGeneralTagName : String= '');
Var
  GeneralParams : IPSCStrings;

  Procedure AssignSimpleProperties(const SimpleParams: IPSCStrings);
  Var
    PropNames : IPSCStrings;
    I         : Integer;
    PValue    : String;
  begin
    PropNames := PSCCreateStringList;
    PSCPropNamesToStrings( AInstance, PropNames, True, True );
    With PropNames Do
      For I := 0 To Count - 1 Do
      begin
        PValue := Trim( SimpleParams.Values[ Trim( Strings[ I ] ) ] );
        If PValue <> '' Then
          PSCSetPropValue( AInstance, Strings[ I ], PValue );
      end;
  end;

begin
  If AContainTags Then
    begin
      GeneralParams := PSCCreateStringList;
      PSCSetStringsPropsFromIniStrings( AInstance, AIniStrings, Nil, False );
      If PSCGetIniStringsSection( AGeneralTagName, AIniStrings, GeneralParams, False ) Then
        AssignSimpleProperties( GeneralParams );
    end
  Else
    AssignSimpleProperties( AIniStrings );
end;

{--------------------------------------}
{
const
  SPSCdcc32Exe='dcc32.exe';
  SPSCMakeExe='make.exe';
  SPSCBpr2MakExe='bpr2mak.exe';

function PSCCompileFile(DelphiVer: TPSCDelphiVer;const AFullFileName,
  OutDir,ExtCMDParams: string;var ResultFile,ErrorMessage:string): Boolean;
const
  SCMDTemplate = '"%s" %s "%s"'; //don't resource
var
  DelphiPath: string;
  CompilerPath: string;
  Params: string;
  FilePath: string;
  FileName: string;
  FileExt: string;
  CompFileName: string;
  FullFileName:String;
  Bpr2MakPath:String;

  procedure _MoveFile(const FileExt: String);
  var
    Source, Dest: string;
  begin
    Source := ChangeFileExt(FullFileName, FileExt);
    Dest := PSCAddSlash(OutDir) + ChangeFileExt(FileName, FileExt);

    If AnsiCompareText(Source,Dest)=0 then
      exit;
      
    if FileExists(Dest) then
       DeleteFile(Dest);

    if AnsiCompareText(ExtractFileDrive(Source), ExtractFileDrive(Dest)) <> 0 then
    begin
      CopyFile(PChar(Source), PChar(Dest), False);
      DeleteFile(Source);
    end
    else MoveFile(PChar(Source), PChar(Dest));
    ResultFile := Dest;
  end;

  procedure _DeleteFile(const FileExt: string);
  begin
    DeleteFile(ChangeFileExt(FullFileName, FileExt));
  end;

Begin
  FullFileName:=AFullFileName;
  DelphiPath := PSCGetSpecificDelphiDir(DelphiVer);
  CompilerPath := DelphiPath + 'Bin\'; //don't resource
  FilePath := ExtractFilePath(FullFileName);
  FileName := ExtractFileName(FullFileName);
  FileExt  := ExtractFileExt(FullFileName);
  case DelphiVer of
   dv_D2, dv_D3, dv_D4, dv_D5, dv_D6,dv_d7: begin
     CompilerPath := CompilerPath + SPSCdcc32Exe;
     Params := '-q -b'; //don't resource
   end;
   dv_C1, dv_C3, dv_C4, dv_C5,dv_c6: begin
     if (AnsiCompareText(FileExt,SPSCPasExt)=0) or (AnsiCompareText(FileExt,SPSCDprExt)=0) then
     begin
       CompilerPath := CompilerPath + SPSCdcc32Exe;
       Params := '-q -b -JPHN'; //don't resource
     end else
     begin
       If (DelphiVer in [dv_c5,dv_c6]) and ((AnsiCompareText(FileExt,SPSCBPKExt)=0) or
         (AnsiCompareText(FileExt,SPSCBPRExt)=0)) then
       begin
         Bpr2MakPath:='"'+CompilerPath+SPSCBpr2MakExe+'"';
         PSCRunProgram(Bpr2MakPath+' '+'"'+FullFileName+'"',
           ExtractFilePath(FullFileName),ErrorMessage,True);
         FullFileName:=ChangeFileExt(FullFileName,SPSCMakExt);
       end;
       CompilerPath := CompilerPath + SPSCmakeExe;
       Params := '-B -f'; //don't resource
     end;
   end;
  end;

  if ExtCMDParams <> ''
   then Params := Params + ' ' + ExtCMDParams;
  CompFileName := Format(SCMDTemplate, [CompilerPath, Params, FullFileName]);

  Result := PSCRunProgram(CompFileName, FilePath, ErrorMessage, True);
  if not Result
   then Exit;

  if AnsiCompareText(FileExt, SPSCPasExt) = 0
   then _MoveFile(SPSCDCUExt)
   else
  if AnsiCompareText(FileExt, SPSCDPKExt) = 0 then
  begin
    _MoveFile(PSCGetPackageExt(DelphiVer));
    _DeleteFile(SPSCDCUExt);
    _DeleteFile(SPSCDCPExt);
  end else
  if AnsiCompareText(FileExt, SPSCbpkExt) = 0 then
  begin
    _MoveFile(PSCGetPackageExt(DelphiVer));
    _DeleteFile(SPSCtdsExt);
    _DeleteFile(SPSCObjExt);
    If DelphiVer in [dv_C5,dv_c6] then
      _DeleteFile(SPSCMakExt);
  end else
  if (AnsiCompareText(FileExt, SPSCdprExt)=0)
   then _MoveFile(SPSCexeExt);
End;
}
{------------------------------------------------------------------}

function PSCCopyFile(const ASrcFile, ADestFile: string): Boolean;
begin
  Result := CopyFile(PChar(ASrcFile), PChar(ADestFile), False);
end;

{-------------------------------------------------------------------------}

procedure PSCModifyFilesAttr(const Files:IPSCStrings;RemoveAttr,AddAttr:Integer);
var
  i:Integer;
begin
  for i:=0 to Files.Count-1 do
    PSCModifyFileAttr(Files[i],RemoveAttr,AddAttr);
end;

{-------------------------------------------------------------------------}

procedure PSCModifyFileAttr(const FileName:String;RemoveAttr,AddAttr:Integer);
var
  FileAttr:Integer;
begin
  FileAttr:=System.SysUtils.FileGetAttr(FileName);
  FileAttr:=(FileAttr and (not RemoveAttr)) or AddAttr;
  System.SysUtils.FileSetAttr(FileName,FileAttr);
end;

{------------------------------------------------------------------}

function PSCGenDPUPath(DelphiVer:TPSCDelphiVer):String;
begin
  Result:=PSCAddSlash(PSCGetTemporaryPath)+'DPU_'+cPSCDelphiVers[DelphiVer];//don't resource
  PSCCreateAllDirectories(Result,nil);
end;

{------------------------------------------------------------------}
{
//BeginSkipConst
function PSCGetDelphiSourceDirsEx2(DelphiVer:TPSCDelphiVer;Opts:TPSCDelphiSourceDirsOpts) : string;
var
  basedir:string;
  SysFolder:String;
begin
  basedir := PSCGetSpecificDelphiDir(DelphiVer);

  if (doExcludeSysUnderD3 in Opts) and (DelphiVer in [dv_d3,dv_c3]) then
    SysFolder:=''
  else
    SysFolder:=basedir +'Source\Rtl\Sys;';

  if basedir <> '' then
    begin
      result := basedir +'Doc;' +
                basedir +'Source\Vcl;' +
                SysFolder +
                basedir +'Source\Rtl\Win;' +
                basedir +'Source\Toolsapi;' +
                basedir +'Source\Internet;' +
                basedir +'Source\Samples;' +
                basedir +'Source\Rtl\Corba;';

      If DelphiVer in [dv_d6,dv_d7,dv_d5,dv_c5] then
        result := result+
                  basedir +'Source\Decision Cube;' +
                  basedir +'Source\Property Editors;'+
                  basedir +'Source\WebMidas;';

      If DelphiVer in [dv_d7,dv_d6,dv_c6] then
        result := result+
                  basedir +'Source\Rtl\Common;' +
                  basedir +'Source\Rtl\Corba40;' +
                  basedir +'Source\Clx;' +
                  basedir +'Source\Soap;';

      If doAddLibFolder in Opts then
        result := result+
                  basedir +'Lib;' +
                  basedir +'Lib\Obj;' +
                  basedir +'Lib\Delphi2;';
    end
  else
    result :='';
end;
//EndSkipConst
 }
{------------------------------------------------------------------}
{
function PSCGetDelphiSourceDirsEx(DelphiVer:TPSCDelphiVer):string;
begin
  Result:=PSCGetDelphiSourceDirsEx2(DelphiVer,[doAddLibFolder]);
end;
}
{--------------------------------------------------}

Function PSCGetTemporaryPath:String;
Var
  PathLen:Integer;
Begin
  SetLength(Result,MAX_PATH);
  PathLen:=GetTempPath(MAX_PATH,PChar(Result));
  If PathLen>3 Then //skip for drive:\ (c:\)
    SetLength(Result,PathLen-1);
End;

{--------------------------------------------------}

Function PSCRunProgram(const CmdLine,WorkDir:String;Var ErrorMessage:String;
                    HookOutput:Boolean):Boolean;
Var
  ProcessInfo:TProcessInformation;
  StartupInfo:TStartupInfo;
  TempFile:String;
  AExitCode:DWORD;
  ErrorFile:TextFile;
  NextStr:String;
  Handle:THandle;
  SD:TSecurityAttributes;
  ErrorCode:Integer;
  Message:String;
Begin
  //PSCWriteToLog('Run program:'+CmdLine);//don't resource

  FillChar(StartupInfo,SizeOf(StartupInfo),0);
  StartupInfo.cb:=SizeOf(StartupInfo);
  If HookOutput Then
  Begin
    TempFile:=PSCGetTemporaryFileName('tempfile');//don't resource

    FillChar(SD,SizeOf(SD),0);
    SD.nLength:=SizeOf(SD);
    SD.bInheritHandle:=True;
    Handle:=CreateFile(PChar(TempFile),GENERIC_WRITE,0,@SD,OPEN_EXISTING,FILE_ATTRIBUTE_ARCHIVE,0);
    StartupInfo.hStdOutput:=Handle;
    StartupInfo.dwFlags:=STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow:=SW_HIDE;
    StartupInfo.dwFlags:=StartupInfo.dwFlags Or STARTF_USESHOWWINDOW;
  End
  Else
    Handle:=0;
  If CreateProcess(Nil,PChar(CmdLine),Nil,Nil,True,0,
                           Nil,PChar(WorkDir),StartupInfo,ProcessInfo)=False Then
  Begin
    Result:=False;
    ErrorCode:=GetLastError;
    Message:=SysErrorMessage(ErrorCode);
    If Message<>'' Then
      ErrorMessage:=Message+'. ';
    ErrorMessage:=ErrorMessage+'ErrorCode='+IntToStr(ErrorCode);//don't resource
    Exit;
  End;
  WaitForSingleObject(ProcessInfo.hProcess,INFINITE);
  If HookOutput Then
  Begin
    FileClose(Handle);
    AssignFile(ErrorFile,TempFile);
    Reset(ErrorFile);
    ErrorMessage:='';
    While Not Eof(ErrorFile) Do
    Begin
      Readln(ErrorFile,NextStr);
      ErrorMessage:=ErrorMessage+NextStr+#13#10;
    End;
    CloseFile(ErrorFile);
    DeleteFile(TempFile);
  End;
  GetExitCodeProcess(ProcessInfo.hProcess,AExitCode);
  CloseHandle(ProcessInfo.hThread);
  CloseHandle(ProcessInfo.hProcess);
  Result:=AExitCode=0;
  If not Result then
    ErrorMessage:=Format(PSCConsts.ErrProgramFailed,[AExitCode])+#13#10+ErrorMessage;
End;

{--------------------------------------}

const
 SIndex = ':Index ';//don't resource
 SInclude = ':Include';//don't resource

procedure PSCRemoveHelpFromCntFile(const HelpFile, CntFile: string);
var
  i: Integer;
  CntStr: string;
begin
  CntStr := ChangeFileExt(HelpFile, SPSCCntExt);
  with TStringList.Create do
  try
    LoadFromFile(CntFile);
    for i := Count - 1 downto 0 do
      if (PSCPosIgnoreCase(HelpFile, Strings[i]) <> 0) or
        (PSCPosIgnoreCase(CntStr, Strings[i]) <> 0) then
        Delete(i);
    SaveToFile(CntFile);
  finally
    Free;
  end;
end;

{--------------------------------------}

procedure PSCAddHelpToCntFile(const HelpFile, HelpName, CntFile: string);
begin
  PSCRemoveHelpFromCntFile(HelpFile, CntFile); // Remove previous version
  with TStringList.Create do
  try
    LoadFromFile(CntFile);
    Add(SInclude + ' ' + ChangeFileExt(HelpFile, SPSCCntExt));
    Add(SIndex + HelpName + ' =' + HelpFile);
    SaveToFile(CntFile);
  finally
    Free;
  end;
end;

{-------------------------------------------}
{
const
  DelphiHelpFiles : array[TPSCDelphiVer] of string =
//    dv_D2       dv_C1        dv_D3         dv_C3        dv_D4         dv_C4        dv_D5      dv_C5       dv_D6
  ('delphi.cnt', 'bcb.cnt', 'delphi3.cnt', 'bcb3.cnt', 'delphi4.cnt','bcb4.cnt','delphi5.cnt','bcb5.cnt','delphi6.cnt','bcb6.cnt','d7.cnt',
//  dv_D8         dv_D2005          dv_D2006          dv_C2006        dv_D2007          dv_C2007      dv_D2009          dv_C2009      dv_D2010         dv_C2010       dv_DXE        dv_CXE);
  'delphi8.cnt', 'delphi2005.cnt', 'delphi2006.cnt', 'bcb2006.cnt', 'delphi2007.cnt', 'bcb2007.cnt', 'delphi2009.cnt', 'bcb2009.cnt', 'delphi2010.cnt','bcb2010.cnt','delphiXE.cnt','bcbXE.cnt');

function  PSCGetDelphiHelpCntFile(DelphiVer : TPSCDelphiVer) : string;
begin
  result := PSCGetDelphiHelpFolder(DelphiVer) + DelphiHelpFiles[DelphiVer];
end;
}
{------------------------------------------------------------------}

Const
  cDelphiHelpFileNames : Array[ TPSCDelphiVer ] Of String=
//    dv_D2      dv_C1      dv_D3     dv_C3      dv_D4      dv_C4      dv_D5        dv_C5      dv_D6
     ( 'vcl',   'bcbvcl',   'vcl3', 'bcbvcl3', 'del4vcl', 'bcb4vcl', 'del5vcl' , 'bcb5vcl', 'del6vcl', 'bcb6vcl', 'd7vcl',
//  dv_D8     dv_D2005   dv_D2006    dv_C2006      dv_D2007   dv_C2007      dv_D2009   dv_C2009      dv_D2010  dv_C2010     dv_DXE  dv_CXE);
      'vcl8', 'vcl2005', 'vcl2006', 'bcb2006vcl', 'vcl2007', 'bcb2007vcl', 'vcl2009', 'bcb2009vcl', 'vcl2010','bcb2010vcl','vclXE','bcbXEvcl');

Function PSCGetDelphiHelpFileName(ADelphiVer:TPSCDelphiVer):String;
begin
  Result:=cDelphiHelpFileNames[ADelphiVer];
end;

{-------------------------------------------------------------------------}
{
function PSCGetDelphiHelpFolder(DelphiVer:TPSCDelphiVer):string;
begin
  Result:=PSCGetSpecificDelphiDir(DelphiVer) + 'HELP\'; //don't resource
end;
}
{--------------------------------------}

function PSCGetDelphiVerStr(ADelphiVer: TPSCDelphiVer): string;
begin
  Result := cPSCDelphiVers[ADelphiVer];
end;

{------------------------}
{
function PSCGetCompilerPathAndParams(DelphiVer:TPSCDelphiVer;var Params:String):String;
begin
  Result:=PSCGetSpecificDelphiDir(DelphiVer)+'Bin\';
  Case DelphiVer Of
    dv_C1,dv_C3,dv_C4,dv_C5,dv_c6:
      Begin
        Result:=Result+'dcc32.exe';
        Params:='-B -JPHN';
      end;
    else
      Begin
        Result:=Result+'dcc32.exe';
        Params:='-B';
      End;
  end;
end;
}
{-------------------------------------------------------------------------}
{
function PSCGetSpecificDelphiDir(vertype : TPSCDelphiVer) : string;
begin
  Result:=PSCAddSlash(PSCGetDelphiRegKey(VerType,'RootDir'));//don't resource
end;
}
{------------------------------------------------------------------}
{
function PSCGetDelphiRegKey(VerType:TPSCDelphiVer;const KeyName:String):String;
const
//BeginSkipConst
  RegPathPrefix = 'Software\Borland\';
  RegPath : Array[TPSCDelphiVer] of string =
  (
    'Delphi\2.0',
    'C++Builder\1.0',
    'Delphi\3.0',
    'C++Builder\3.0',
    'Delphi\4.0',
    'C++Builder\4.0',
    'Delphi\5.0',
    'C++Builder\5.0',
    'Delphi\6.0',
    'C++Builder\6.0',
    'Delphi\7.0',
    'Delphi\8.0',       //D8
    'Delphi\2005',     //D2005
    'Delphi\2006',     //D2006
    'C++Builder\2006',//C2006
    'Delphi\2007',     //D2007
    'C++Builder\2007',//C2007
    'Delphi\2009',     //D2009
    'C++Builder\2009',//C2009
    'Delphi\2010',     //D2010
    'C++Builder\2010',//C2010
    'Delphi\XE',       //DXE
    'C++Builder\XE'   //CXE
  );
//EndSkipConst  
var
  r      : TRegistry;
  RegKey : string;
begin

  RegKey := RegPathPrefix + RegPath[vertype];
  r:= TRegistry.Create;
  try
    r.RootKey:=HKEY_LOCAL_MACHINE;
    if r.Openkey(RegKey,False) then
      result := r.ReadString(KeyName)
    else
      result  := '';
  finally
    r.Free;
  end;
end;
 }
{------------------------------------------------------------------}

Function PSCGetRegKeyValue(AGlobalKey: HKey; Const AKeyName, AValueName: String): String;
Var
  RegKey: HKEY;
  Rt    : DWORD;
  Size  : DWORD;
begin
  Result:='';
  If RegOpenKeyEx(AGlobalKey, pChar(AKeyName), 0, KEY_READ, RegKey) =ERROR_SUCCESS Then
  begin
    If RegQueryValueEx(RegKey, PChar(AValueName), Nil, @Rt, Nil, @Size) =ERROR_SUCCESS Then
    begin
      SetLength(Result, Size);
      RegQueryValueEx(RegKey, PChar(AValueName), Nil, @Rt, @(Result[1]), @Size);
    end;
    RegCloseKey(RegKey);
  end;
end;

{------------------------------------------------------------------}
{
Const
  SPSCAppPathsRegKey ='Software\Microsoft\Windows\CurrentVersion\App Paths\';
  SPSCHTML_CompilerRegKey =SPSCAppPathsRegKey+'hhw.exe\';
  SPSCHRTF_CompilerRegKey =SPSCAppPathsRegKey+'hcw.exe\';
  sPSCPathRegValueName ='Path';

Function PSCSearchCompiler(Const ACompilerName, ACompilerRegKey, ADefaultCompilerName: String): String;

  Function SearchInDelphiDirs(Var Res: String): Boolean;
  Var
    I   : TPSCDelphiVer;
    Name: String;
  begin
    Result:=True;
    For I:=High(TPSCDelphiVer) DownTo Low(TPSCDelphiVer) Do
    begin
      Name:=PSCRelativeFileSearch(Res, PSCGetSpecificDelphiDir(I) + 'Help\Tools');
      If Name <> '' Then
      begin
        Res:=Name;
        Exit;
      end;
    end;
    Result:=False;
  end;

begin
  Result:=PSCExpandRelativeFile(ACompilerName);
  If FileExists(Result) Then
    Exit;
  Result:=PSCGetRegKeyValue(HKEY_LOCAL_MACHINE, ACompilerRegKey, sPSCPathRegValueName);
  Result:=PSCRelativeFileSearch(ADefaultCompilerName, Result);
  If Result <> '' Then
    Exit;

  Result:=ADefaultCompilerName;
  If SearchInDelphiDirs(Result) Then
    Exit;

  Result:='';
end;
}
{------------------------------------------------------------------}
{
Function PSCGetHTMLCompiler(Const ACompilerName: String): String;
begin
  Result:=PSCSearchCompiler(ACompilerName, SPSCHTML_CompilerRegKey, sPSCHTML_CompilerFileName);
end;
}
{------------------------------------------------------------------}
{
Function PSCGetHLPCompiler(Const ACompilerName: String): String;
begin
  Result:=PSCSearchCompiler(ACompilerName, SPSCHRTF_CompilerRegKey, sPSCRTF_CompilerFileName);
end;
}
{-----------------------------------------------------------}

Function PSCGetStringsPropAsText(Instance: TPersistent;
  Const PropName: String): String;
Var
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  FObject: TObject;
Begin
  Result := '';

  PropInfo := GetPropInfo(Instance.ClassInfo,PropName);
  TypeInfo := PSCGetPTypeInfo(PropInfo);

  If (PropInfo = Nil) Or (TypeInfo = Nil) Or (TypeInfo.Kind <> tkClass) Then
    exit;

  FObject := TObject(GetOrdProp(Instance,PropInfo));

  If Not (FObject Is TStrings) Then
    exit;

  Result := TStrings(FObject).Text;
End;

{----------------------------------------------------------}

Procedure PSCAssignProps(Source,Dest: TPersistent; Const PropNames: Array Of
  String);
Var
  i: integer;
Begin
  If Not PSCAreObjNotEqualNotNil(Source,Dest) Then
    exit;
  For i := Low(PropNames) To High(PropNames) Do
    PSCAssignProp(Source,Dest,PropNames[i]);
End;

{----------------------------------------------------------}

Procedure PSCAssignAllPropsExcludeStr(Source,Dest: TPersistent; Const
  ExcludeProps: String);
Var
  Temp: IPSCStrings;
Begin
  Temp := PSCCreateStringList;
  PSCParseString(ExcludeProps, ',',Temp);
  PSCAssignAllPropsExclude(Source,Dest,Temp);
End;

{-----------------------------------------------------------}

Function PSCAssignStringsProp(Instance: TPersistent; Const PropName: String;
  Value: TStrings): boolean;
Var
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  FObject: TObject;
Begin
  Result := False;

  PropInfo := GetPropInfo(Instance.ClassInfo,PropName);
  TypeInfo := PSCGetPTypeInfo(PropInfo);

  If (PropInfo = Nil) Or (TypeInfo = Nil) Or (TypeInfo.Kind <> tkClass) Then
    exit;

  FObject := TObject(GetOrdProp(Instance,PropInfo));

  If Not (FObject Is TStrings) Then
    exit;

  TStrings(FObject).Assign(Value);

  Result := True;
End;

{------------------------------------------------------------------}

Function PSCPropertyExists(C: TPersistent; Const PropName: String): Boolean;
Begin
  Result := GetPropInfo(C.ClassInfo,PropName) <> Nil;
End;

{------------------------------------------------------------------}

Procedure PSCPropNamesToStrings(Instance: TPersistent; const PropNames: IPSCStrings;
  StoredProps,NotStoredProps: boolean);
Var
  I,Count: Integer;
  PropList: PPropList;
  PropIsStored: boolean;
Begin
  If PropNames = Nil Then
    exit;
  PropNames.Clear;
  If Instance = Nil Then
    exit;
  Count := GetTypeData(Instance.ClassInfo)^.PropCount;
  If Count > 0 Then
    Begin
      GetMem(PropList,Count * SizeOf(Pointer));
      Try
        GetPropInfos(Instance.ClassInfo,PropList);
        For I := 0 To Count - 1 Do
          Begin
            PropIsStored := IsStoredProp(Instance,PropList^[I]);
            If (PropIsStored And StoredProps) Or (Not PropIsStored And
              NotStoredProps) Then
              PropNames.AddObject(PropList^[I].Name,Pointer(PropList^[I]));
          End;
      Finally
        FreeMem(PropList,Count * SizeOf(Pointer));
      End;
    End;
End;

{------------------------------------------------------------------}

Function PSCGetNotStoredProps(Instance: TPersistent): String;
Var
  Temp: IPSCStrings;
Begin
  Temp := PSCCreateStringList;
  PSCPropNamesToStrings(Instance,Temp,False,True);
  Result := Temp.GetCommaText;
End;

{--------------------------------------}

procedure PSCGetEnumTypeValueNames(TypeInfo:PTypeInfo;
  const AValueNames: IPSCStrings);
var
  i: Integer;
begin
  if not Assigned(AValueNames) or (TypeInfo = nil) or
    (TypeInfo^.Kind <> tkEnumeration) then
    Exit;
  AValueNames.Clear;
  for i := GetTypeData(TypeInfo)^.MinValue to GetTypeData(TypeInfo)^.MaxValue do
   AValueNames.Add(GetEnumName(TypeInfo, i));
end;

{----------------------------------------------------------}

Procedure PSCAssignPropInfo(Source,Dest: TPersistent; SourceInfo,DestInfo:
  PPropInfo);
Begin
  If Assigned(sourceinfo) And Assigned(destinfo) and (DestInfo.SetProc <> nil) Then
    Case PSCGetPTypeInfo(sourceinfo).kind Of
      tkInteger,tkChar,tkSet,tkClass,tkEnumeration:
        SetOrdProp(Dest,destinfo,GetOrdProp(Source,sourceinfo));
      tkFloat:
        SetFloatProp(Dest,destinfo,GetFloatProp(Source,sourceinfo));
      tkUString,tkString,tkWChar,tkLString,tkWString:   // 7 maggio 2011 - Unicode
        SetStrProp(Dest,destinfo,GetStrProp(Source,sourceinfo));
      tkVariant:
        SetVariantProp(Dest,destinfo,GetVariantProp(Source,sourceinfo));
      tkMethod:
        SetMethodProp(Dest,DestInfo,GetMethodProp(Source,sourceinfo));
    End;
End;

{----------------------------------------------------------}

Procedure PSCAssignProp(Source,Dest: TPersistent; Const Propname: String);
Begin
  PSCAssignPropInfo(Source,Dest,GetPropInfo(Source.ClassInfo,PropName),
    GetPropInfo(Dest.ClassInfo,PropName));
End;

{----------------------------------------------------------}

Procedure PSCAssignAllPropsExclude(Source,Dest: TPersistent;
  const ExcludeList: IPSCStrings);
Var
  I,Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
Begin
  If Not PSCAreObjNotEqualNotNil(Source,Dest) Then
    exit;
  Count := GetTypeData(Source.ClassInfo)^.PropCount;
  If Count > 0 Then
    Begin
      GetMem(PropList,Count * SizeOf(Pointer));
      Try
        GetPropInfos(Source.ClassInfo,PropList);
        For I := 0 To Count - 1 Do
          Begin
            PropInfo := PropList^[I];
            If (ExcludeList <> Nil) Then
              Begin
                If ExcludeList.IndexOf(PropInfo.Name) = -1 Then
                  PSCAssignProp(Source,Dest,PropInfo.Name);
              End
            Else
              PSCAssignProp(Source,Dest,PropInfo.Name);
          End;
      Finally
        FreeMem(PropList,Count * SizeOf(Pointer));
      End;
    End;
End;

{----------------------------------------------------------}

Procedure PSCAssignAllProps(Source,Dest: TPersistent);
Begin
  PSCAssignAllPropsExclude(Source,Dest,Nil)
End;

{-------------------------------------------------------------}

function PSCMinDateTime:TDateTime;
begin
  Result:=MinDateTime;
end;

{-------------------------------------------------------------}

function PSCMaxDateTime:TDateTime;
begin
  Result:=MaxDateTime;
end;

{------------------------------------------------------------------}
{
  TEnumModuleFunc = function (HInstance: NativeInt; Data: Pointer): Boolean;
  TEnumModuleFuncLW = function (HInstance: THandle; Data: Pointer): Boolean;
}
function PSCModulesToListFunc(HInstance: NativeInt; Data: Pointer): Boolean;
begin
  Result:=True;
  TList(Data).Add(TObject(HInstance));
end;

{------------------------------------------------------------------}

procedure PSCModulesToList(List:TList);
begin
  List.Clear;
  EnumModules(PSCModulesToListFunc,List);
end;

{------------------------------------------------------------------}

procedure PSCResModulesToList(List:TList);
begin
  List.Clear;
  EnumResourceModules(PSCModulesToListFunc,List);
end;

{------------------------------------------------------------------}
function PSCFindResourceInstance(const ResName:String;ResType:PChar):THandle;
{$IFNDEF LINUX}
var
  PackList:TList;
  i:Integer;
begin
  PackList:=TList.Create;
  PSCResModulesToList(PackList);
  try
    for i:=0 to PackList.Count-1 do
    begin
      Result:=Integer(PackList[i]);
      If (Result>0) and (FindResource(Result, PChar(ResName), ResType) <> 0) then
        exit;
    end;
    Result:=0;
  finally
    PackList.Free;
  end;
end;
{$ELSE}
begin
  Result:=HInstance;
end;
{$ENDIF}

{---------------------------------------------------------}

function PSCRect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result:=Rect(ALeft, ATop, ARight, ABottom);
end;

{-------------------------------------------------------------------------}

function PSCPoint(AX, AY: Integer): TPoint;
begin
  Result:=Point(AX,AY);
end;

{-------------------------------------------------------------------------}

Function PSCUnderWindowsNT: boolean;
Begin
  result := Win32Platform = VER_PLATFORM_WIN32_NT;
End;

{--------------------------------}

function PSCTimeSeparator:Char;
begin
  Result:=FormatSettings.TimeSeparator;
end;

{--------------------------------}

function PSCDateSeparator:Char;
begin
  Result:=FormatSettings.DateSeparator;
end;

{--------------------------------}

function PSCWindowsNTOrHigher : boolean;
begin
  result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

{--------------------------------}

function PSCWindows2kOrHigher : boolean;
begin
  result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5);
end;

{--------------------------------}

function PSCWindowsXPOrHigher : boolean;
begin
  result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5)
   and (Win32MinorVersion>=1);
end;

{-------------------------------------------------------------}

function PSCShortTimeFormat:String;
begin
  Result:=FormatSettings.ShortTimeFormat;
end;

{-------------------------------------------------------------}

function PSCLongTimeFormat:String;
begin
  Result:=FormatSettings.LongTimeFormat;
end;

{-------------------------------------------------------------}

function PSCShortDateFormat:String;
begin
  Result:=FormatSettings.ShortDateFormat;
end;

{-------------------------------------------------------------}

function PSCLongDateFormat:String;
begin
  Result:=FormatSettings.LongDateFormat;
end;

{-------------------------------------------------------------}

function PSCLongMonthNames(AIndex:Integer):string;
begin
  Result:=FormatSettings.LongMonthNames[AIndex];
end;

{-------------------------------------------------------------}

function PSCShortMonthNames(AIndex:Integer):string;
begin
  Result:=FormatSettings.ShortMonthNames[AIndex];
end;

{-------------------------------------------------------------}

procedure PSCSetDecimalSeparator(AValue:Char);
begin
  FormatSettings.DecimalSeparator:=AValue;
end;

{-------------------------------------------------------------}

function PSCGetDecimalSeparator:Char;
begin
  Result:=FormatSettings.DecimalSeparator;
end;

{---------------------------------------------------------}

function PSCAllocPatternBitmap(BkColor, FgColor: TPSCColor): TPSCBitmap;
begin
  Result:=AllocPatternBitmap(BkColor, FgColor);
end;

{---------------------------------------------------------}

function PSCColorToRGB(AColor:TPSCColor):LongInt;
begin
  Result:=ColorToRGB(AColor);
end;

{---------------------------------------------------------}

type
  THackCanvas=class(TPSCCanvas)
  end;

{------------------------------------------------------------------}

procedure PSCUpdateCanvasState(ACanvas:TPSCCanvas);
begin
  THackCanvas(ACanvas).RequiredState([csHandleValid, csPenValid,
    csBrushValid]);
end;

{------------------------------------------------------------------}

Function PSCGetSystemMetrics(nIndex: Integer): Integer;
Begin
  Result := Winapi.Windows.GetSystemMetrics(nIndex);
End;

{---------------------------------------------------------}

function PSCGetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;
begin
  Result:=GetEnumName(TypeInfo,Value);
end;

{----------------------------------------------------------}

Function PSCGetPTypeInfo(PropInfo: PPropInfo): PTypeInfo;
Begin
  If PropInfo = Nil Then
    Begin
      Result := Nil;
      exit;
    End;
  Result := PropInfo.PropType^;
End;

{------------------------------------------------------------------}

Function PSCGetPropValueByPropInfo(Instance: TPersistent; Propinfo: PPropInfo):
  Variant;
Var
  BaseType: PTypeInfo;
Begin
  If PropInfo = Nil Then
    Result := UnAssigned
  Else
    Case PSCGetPTypeInfo(propinfo).kind Of
      tkEnumeration:
        Begin
          Result := GetOrdProp(Instance,PropInfo);
          BaseType := PSCGetPTypeData(PropInfo).BaseType^;
          If (BaseType = TypeInfo(Boolean)) Or (BaseType = TypeInfo(ByteBool))
            Or
            (BaseType = TypeInfo(WordBool)) Or (BaseType = TypeInfo(LongBool))
              Then
            Result := VarAsType(Result,varBoolean);
        End;
      tkInteger,tkChar,tkSet,tkClass:
        result := GetOrdProp(Instance,propinfo);
      tkFloat:
        result := GetFloatProp(Instance,propinfo);
      tkUString,tkString,tkWChar,tkLString,tkWString:
        result := GetStrProp(Instance,propinfo);
      tkVariant:
        result := GetVariantProp(Instance,propinfo);
    End;
End;

{------------------------------------------------------------------}

Function PSCGetPTypeData(PropInfo: PPropInfo): PTypeData;
Begin
  Result := GetTypeData(PSCGetPTypeInfo(PropInfo));
End;

{------------------------------------------------------------------}

Procedure PSCSetPropValueByPropInfo(Instance: TPersistent; Propinfo: PPropInfo;
  Const Value: Variant);
Var
  CorrectedValue: Integer;
  Kind: System.TypInfo.TTypeKind;
Begin
  If PropInfo = Nil Then
    exit;

  Kind := PSCGetPTypeInfo(propinfo).Kind;

  Case Kind Of
    tkInteger,tkChar,tkSet,tkClass,tkEnumeration:
      Begin
        If (TVarData(Value).VType = VarString) or (TVarData(Value).VType = varUString) Then
          Begin
            If Kind = tkEnumeration Then
              Begin
                System.TypInfo.SetEnumProp(Instance,PropInfo,String(Value));
                exit;
              End
            Else
              Begin
                If PSCCompareText(SPSCTextTrue,Value) = 0 Then
                  CorrectedValue := 1
                Else
                  If PSCCompareText(SPSCTextFalse,Value) = 0 Then
                    CorrectedValue := 0
                  Else
                    CorrectedValue := PSCStrToInt(Value);
              End;
          End
        Else
          CorrectedValue := PSCCorrectBool(Value);

        SetOrdProp(Instance,propinfo,CorrectedValue);
      End;
    tkFloat:
      SetFloatProp(Instance,propinfo,Value);
    tkUString,tkString,tkWChar,tkLString,tkWString:
      SetStrProp(Instance,propinfo,String(Value));
    tkVariant:
      SetVariantProp(Instance,propinfo,Value);
  End;
End;

{------------------------------------------------------------------}

Function PSCGetPropValue(Instance: TPersistent; Const PropName: String):
  Variant;
Var
  propinfo: PPropInfo;
Begin
  If Instance = Nil Then
    Exit;
  propinfo := GetPropInfo(Instance.ClassInfo,PropName);
  Result := PSCGetPropValueByPropInfo(Instance,propinfo);
End;

{------------------------------------------------------------------}

Procedure PSCSetPropValue(C: TPersistent; Const PropName: String;
  Const Value: Variant);
Var
  propinfo: PPropInfo;
Begin
  If C = Nil Then
    Exit;
  propinfo := GetPropInfo(C.ClassInfo,PropName);
  PSCSetPropValueByPropInfo(C,propinfo,Value);
End;

{--------------------------------------}

const
  SDefine       = '{$DEFINE';
  SStopLine     = '{$ENDIF}';

procedure  PSCModifyDefines(const FileName: string; Products: TstringList);
var
  Prod: TstringList;

  function ProcessDefine(const Str: string; var IsCommented: Boolean;
    var DefineName: string): Boolean;
  var
    Data: PChar;
    SaveData: PChar;

    procedure  SkipSpaces;
    const
      Spaces = [#9, #10, #13, ' '];
    begin
      while Data^ in Spaces do
        Data := Data + 1;
    end;

  begin
    if Str = '' then
    begin
      Result := False;
      Exit;
    end;

    Data := @Str[1];
    SkipSpaces;
    if (Data^ = '/') and ((Data + 1) ^= '/') then
    begin
      Data:=Data+2;
      IsCommented:=True;
      SkipSpaces;
    end else
      IsCommented:=False;

    if StrLIComp(Data, SDefine, Length(SDefine)) = 0 then
    begin
      Data := Data + Length(SDefine);
      SkipSpaces;
      SaveData:=Data;
      while UpCase(Data^) in ['A'..'Z','0'..'9','_','$'] do
        Data := Data + 1;
      SetLength(DefineName, Data - SaveData);
      StrLCopy(PChar(DefineName), SaveData, Data - SaveData);
      Result:=True;
    end else
      Result:=False;
  end;

var
  IsCommented: Boolean;
  DefineName: string;
  Str: string;
  i: Integer;
begin
  For I:=0 To Products.Count-1 Do
    Products[I]:=PSCRemoveSpaces(Products[I]);
  Prod:=TstringList.Create;
  try
    Prod.LoadFromFile(FileName);
    For I:=0 To Prod.Count-1 Do
    begin
      if CompareText(Prod[I], SStopLine)=0 Then
        Break;
      if ProcessDefine(Prod[I],IsCommented,DefineName) Then
      begin
        IsCommented:=Products.IndexOf(DefineName)<0;
        if IsCommented Then
          Str:='//'
        Else
          Str:='';
        Str:=Str+'  '+SDefine+' '+DefineName+'}';
        Prod[I]:=Str;
      end;
    end;
    Prod.SaveToFile(FileName);
  finally
    Prod.Free;
  end;
end;

{------------------------------------------------------------------}

Function PSCSubStr(Const S: String; Const Index: Integer;
  Const Separator: String): String;
Var
  I: Integer;
  pB,pE: PChar;
Begin
  Result := '';
  If (Index < 0) Or
    ((Index = 0) And (Length(S) > 0) And (S[1] = Separator)) Then
    Exit;
  pB := PChar(S);
  For I := 1 To Index Do
    Begin
      pB := StrPos(pB,PChar(Separator));
      If pB = Nil Then
        Exit;
      pB := pB + Length(Separator);
    End;
  pE := StrPos(pB + 1,PChar(Separator));
  If pE = Nil Then
    pE := PChar(S) + Length(S);
  If Not (ANSIStrLIComp(pB,PChar(Separator),Length(Separator)) = 0) Then
    SetString(Result,pB,pE - pB);
End;

{-------------------------------------------}
//BeginSkipConst
Procedure PSCItemHtDrawEx(Canvas: TPSCCanvas; ARect: TRect; Const Text: String;
  Var PlainItem: String; Var Width: Integer; CalcWidth: Boolean);

Var
  i: integer;
  M1: String;
  MyInc:Integer;

  Function Cmp(const M1: String): boolean;
  Begin
    Result := AnsiStrLIComp(PChar(Text) + i,PChar(M1),Length(M1)) = 0;
  End;

  Function Cmp1(const M1: String): boolean;
  Begin
    Result := Cmp(M1);
    If Result Then
      inc(i,Length(M1));
  End;

  Function CmpL(M1: String): boolean;
  Begin
    Result := Cmp(M1 + '>');
  End;

  Function CmpL1(M1: String): boolean;
  Begin
    Result := Cmp1(M1 + '>');
  End;

  Procedure Draw(Const M: String);
  Var
    _DrawRect: TRect;
  Begin
    If M='' then
      exit;
    _DrawRect := ARect;
    _DrawRect.Right := _DrawRect.Left + Canvas.TextWidth(M);
    If Not CalcWidth Then
      Canvas.TextRect(_DrawRect,_DrawRect.Left,_DrawRect.Top+MyInc,M);
    ARect.Left := _DrawRect.Right;
  End;

  Procedure Style(Const Style: TFontStyle; Const Include: boolean);
  Begin
    If Include Then
      Canvas.Font.Style := Canvas.Font.Style + [Style]
    Else
      Canvas.Font.Style := Canvas.Font.Style - [Style];
  End;

  Function ProcessColor:TPSCColor;
  var
    CL: String;
  begin
    Result:=clPSCWindow;
    CL := PSCSubStr(PChar(Text) + i,0, '>');
    Try
      If (Length(CL) > 0) And (CL[1] <> '$') Then
        Result := PSCStringToColor('cl' + CL)
      Else
        Result := PSCStringToColor(CL);
    Except
    End;
    inc(i,Length(CL) + 1);
  end;

  procedure ProcessSkip;
  var
    S:String;
    MyIncValue:Integer;
  begin
    S := PSCSubStr(PChar(Text) + i,0, '>');
    MyIncValue:=PSCStrToInt(S);
    inc(ARect.left,MyIncValue);
    inc(i,Length(S) + 1);
  end;

Var
  oldFontStyles: TFontStyles;
  oldFontColor: TPSCColor;
  oldBrushColor: TPSCColor;

  MyColors:Array[0..100] of TPSCColor;
  MyColorsCount:Integer;
Begin
  PlainItem := '';
  MyColorsCount:=0;
  oldFontStyles := Canvas.Font.Style;
  oldFontColor := Canvas.Font.Color;
  oldBrushColor := Canvas.Brush.Color;

  MyInc:=Canvas.TextHeight(Text);
  MyInc:=(ARect.Bottom-ARect.Top-MyInc) div 2;

  Try
    Width := ARect.Left;
    M1 := '';
    i := 1;
    While i <= Length(Text) Do
      Begin
        If (Text[i] = '<') And (CmpL('b') Or CmpL('/b') Or CmpL('i') Or
          CmpL('/i') Or CmpL('u') Or CmpL('/u') Or
          Cmp('c:') or Cmp('cb:') or Cmp('skip:') or CmpL('/c')) Then
          Begin
            Draw(M1);
            PlainItem := PlainItem + M1;

            If CmpL1('b') Then
              Style(FontStyle_Bold,True)
            Else
            If CmpL1('/b') Then
              Style(FontStyle_Bold,False)
            Else
            If CmpL1('i') Then
              Style(FontStyle_Italic,True)
            Else
            If CmpL1('/i') Then
              Style(FontStyle_Italic,False)
            Else
            If CmpL1('u') Then
              Style(FontStyle_Underline,True)
            Else
            If CmpL1('/u') Then
              Style(FontStyle_Underline,False)
            Else
            If Cmp1('c:') Then
              begin
                MyColors[MyColorsCount]:=Canvas.Font.Color;
                inc(MyColorsCount);
                Canvas.Font.Color := ProcessColor;
              end
            else
            If CmpL1('/c') Then
              begin
                dec(MyColorsCount);
                Canvas.Font.Color := MyColors[MyColorsCount];
              end
            else
            If Cmp1('cb:') Then
              Canvas.Brush.Color := ProcessColor
            else
            If Cmp1('skip:') Then
              ProcessSkip;

            M1 := '';
          End
        Else
          M1 := M1 + Text[i];
        inc(i);
      End;
    Draw(M1);
    PlainItem := PlainItem + M1;
  Finally
    Canvas.Font.Style := oldFontStyles;
    Canvas.Font.Color := oldFontColor;
    Canvas.Brush.Color:= oldBrushColor;
  End;

  Width := ARect.Left - Width;
End;
//EndSkipConst

{------------------------------}

Procedure PSCDrawCheck(Canvas: TCanvas; Const R: TRect; AState: TPSCCheckBoxState;
  Flat: Boolean; BkColor: TColor; AEnabled:Boolean);
Var
  DrawState: Integer;
  DrawRect: TRect;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenColor: TColor;

  FCheckWidth: Integer;
  FCheckHeight: Integer;

  Rgn,SaveRgn: TPSCRegion;
Begin
  SaveRgn := 0;
  FCheckWidth := PSCCheckWidth;
  FCheckHeight := PSCCheckheight;

  DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) Div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckWidth) Div 2;
  DrawRect.Right := DrawRect.Left + FCheckWidth;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;
  Case AState Of
    CheckBox_Checked:
      DrawState := DFCS_BUTTONCHECK Or DFCS_CHECKED;
    CheckBox_Unchecked:
      DrawState := DFCS_BUTTONCHECK;
  Else
    DrawState := DFCS_BUTTON3STATE Or DFCS_CHECKED;
  End;
  With Canvas Do
    Begin
      If Flat Then
        Begin
          SaveRgn := CreateRectRgn(0,0,0,0);
          If GetClipRgn(Handle,SaveRgn) <> 1 Then
            Begin
              DeleteObject(SaveRgn);
              SaveRgn := CreateRectRgn(0,0,Screen.Width,Screen.Height);
            End;
          With DrawRect Do
            Rgn := CreateRectRgn(Left + 2,Top + 2,Right - 2,Bottom - 2);
          SelectClipRgn(Handle,Rgn);
          DeleteObject(Rgn);
        End;

      If not AEnabled then
        DrawState:=DrawState or DFCS_INACTIVE;
      DrawFrameControl(Canvas.Handle,DrawRect,DFC_BUTTON,DrawState);
      If Flat Then
        Begin
          SelectClipRgn(Handle,SaveRgn);
          DeleteObject(SaveRgn);
          OldBrushStyle := Brush.Style;
          OldBrushColor := Brush.Color;
          OldPenColor := Pen.Color;
          Brush.Style := BrushStyle_Clear;
          Pen.Color := clPSCBtnShadow;
          With DrawRect Do
            Begin
              Rectangle(Left + 1,Top + 1,Right - 1,Bottom - 1);
              Inc(Left,2);
              Inc(Top,2);
              Dec(Right,2);
              Dec(Bottom,2);
            End;
          Brush.Style := OldBrushStyle;
          Brush.Color := OldBrushColor;
          Pen.Color := OldPenColor;
        End;
    End;
  PSCFillRectExclude(Canvas,R,DrawRect);
End;

{-----------------------------------------------}

procedure PSCSetFileDateTime(const AFileName: String;
  const ADateTime: TDateTime);
var
  Handle : Integer;
begin
  Handle := FileOpen(AFileName, fmOpenWrite or fmShareDenyNone);
  try
    if Handle > 0 then
    begin
      FileSetDate(Handle, DateTimeToFileDate(ADateTime));
    end;
  finally
    FileClose(Handle)
  end;
end;

{---------------------------------------------------------}

end.
