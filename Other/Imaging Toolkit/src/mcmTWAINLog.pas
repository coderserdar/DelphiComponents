// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  15900: mcmTWAINLog.pas
//
//    Rev 1.13    2014-03-28 17:52:56  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.12    2014-01-15 13:41:58  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.11    2013-12-04 23:16:14  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.10    26-08-2009 22:39:50  mcm    Version: DT 3.9
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.9    08-01-2009 21:09:22  mcm
// Added support for Delphi 2009
//
//    Rev 1.8    28-12-2008 19:29:12  mcm    Version: DT 3.8
// Delphi 2009 support
//
//   Rev 1.7    06-11-2003 09:00:52  mcm    Version: DT3.0
// Minor carrige return correction.

//
//   Rev 1.6    04-11-2003 20:18:32  mcm    Version: DT3.0
// Modified MatchTwainInt to handle 32 bit int's.

//
//   Rev 1.5    06-07-2003 11:02:56  mcm    Version: DT 2.5
// Added compiler conditions.

//
//   Rev 1.4    15-04-2003 10:49:12  mcm    Version: DT 2.3
// Modified Triplet2Str to distingues between pre and post calls to the DSM
// Entry procedure.

//
//   Rev 1.3    06-03-2003 10:42:42  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.2    27-09-2002 15:22:00  mcm
// Debug - Log

//
//   Rev 1.1    11-01-2002 15:14:46  mcm    Version: DT 2.0
// Added compiler directive COMPV3 to use longint for Delphi 3 and C++Builder 3, instead of unsupported longword.

//
//   Rev 1.0    04-12-2001 16:49:10  mcm    Version: DT 2.0

unit mcmTWAINLog;

interface

{$INCLUDE mcmDefines.pas}

uses
     {$IFDEF GE_DXE4}
     WinApi.Windows, System.Classes, System.SysUtils,
     {$ELSE}
     {$IFDEF GE_DXE2}
     WinApi.Windows, System.Classes, System.SysUtils,
     {$ELSE}
     Windows, Classes, SysUtils,
     {$ENDIF}
     {$ENDIF}
     twain;

const
  mcmTWAINCvnFile = 'mcmTWAIN.INI';

type
  TmcmTWAINLog = class(TComponent)
  private
    { Private declarations   }
    AWinDir      : string;
  protected
    { Protected declarations }
    FTrackFile   : array[0..1024] of char;
    FLogFileName : string;
  public
    { public declarations    }
    constructor Create            (AOwner   : TComponent); override;
    destructor  Destroy; override;
    procedure   Clear;

    procedure   DeleteLogFile;
    procedure   Str2File          (Value    : string);
    procedure   Triplet2Str       (Dest     : integer;
                                   DG       : integer;
                                   DAT      : integer;
                                   pData    : pointer;
                                   MSG      : integer;
                                   Return   : integer;
                                   Status   : integer;
                                   Level    : TtwnErrorLevel);
    function    MatchTwainInt     (pName    : string;
                                   ID       : TW_INT32;
                                   MaxCount : TW_INT16) : string;
    function    ItemValue2Str     (DataType : word; pData : pointer) : variant; //string;
    function    ItemType2Str      (DataType : word) : string;
    function    Frame2Str         (Value    : pTW_FRAME) : string;
    function    Identity2Str      (Value    : pTW_IDENTITY; NameOnly : boolean) : string;
    function    Parent2Str        (Value    : TW_MEMREF) : string;
    function    DeviceEvent2Str   (Value    : pTW_DEVICEEVENT) : string;
    function    ImageInfo2Str     (Value    : pTW_IMAGEINFO) : string;
    function    ImageLayout2Str   (Value    : pTW_IMAGELAYOUT) : string;
    function    ImageMemXfer2Str  (Value    : pTW_IMAGEMEMXFER) : string;
    function    PendingXfers2Str  (Value    : pTW_PENDINGXFERS) : string;
    function    SetupMemXfer2Str  (Value    : pTW_SETUPMEMXFER) : string;
    function    SetupFileXfer2Str (Value    : pTW_SETUPFILEXFER) : string;
    function    SetupFileXfer22Str(Value    : pTW_SETUPFILEXFER2) : string;
    function    UserInterface2Str (Value    : pTW_USERINTERFACE) : string;
    function    XferGroup         (Value    : TW_UINT32) : string;

    function    Container2Str     (ConType  : longint) : string;
    function    Msg2Str           (Msg      : integer) : string;
    function    Dat2Str           (DAT      : integer) : string;
    function    Cap2Str           (CAP      : integer) : string;
    function    Attribute2Str     (CapStr   : string;
                                   Cap      : integer;
                                   ItemType : TW_UINT16;
                                   pItem    : Variant) : string;
    function    CapData2Str       (twCap    : pTW_Capability) : string;
  published
    { published declarations }
    property INIFile : string
      read   AWinDir;
    property LogFilename : string
      read   FLogFileName
      write  FLogFileName;
  end;

implementation

uses mcmTWAINFix;

{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}


constructor TmcmTWAINLog.Create(AOwner : TComponent);
var CWinDir : array[0..1024] of char;
begin
  Inherited Create(AOwner);
  CR_LF[1] := char($0D);
  GetWindowsDirectory(CWinDir, SizeOf(CWinDir));
  // AWinDir := IncludeTrailingBackslash(StrPas(CWinDir));
  AWinDir := CWinDir;
  if Not(IsPathDelimiter(AWinDir, Length(AWinDir)))
  then AWinDir := AWinDir + '\';

  AWinDir := AWinDir + mcmTWAINCvnFile;
  StrPCopy(FTrackFile, AWinDir);

  FLogFileName := '.\APPTWN.LOG';
end; // TmcmTWAINLog.Create.                                                


destructor TmcmTWAINLog.Destroy;
begin
  Inherited Destroy;
end; // TmcmTWAINLog.Destroy.                                               


procedure TmcmTWAINLog.Clear;
begin
;
end; // TmcmTWAINLog.Clear.                                                 


procedure TmcmTWAINLog.DeleteLogFile;
begin
  if FileExists(FLogFileName)
  then DeleteFile(FLogFileName);
end; // TmcmTWAINLog.DeleteLogFile.                                         


procedure TmcmTWAINLog.Str2File(Value : string);
var LogFile : Text;
begin
  OutputDebugString(PChar(Value));
  AssignFile(LogFile, FLogFileName);
  FileMode := 2;
  if FileExists(FLogFileName)
  then Append(LogFile)
  else Rewrite(LogFile);
  WriteLn(LogFile, Value);
  CloseFile(LogFile);
end; // TmcmTWAINLog.Str2File.                                              


function TmcmTWAINLog.MatchTwainInt(pName    : string;
                                    ID       : TW_INT32;
                                    MaxCount : TW_INT16) : string;
//------------------------------------------------------------------------------
// MatchTwainInt -
//------------------------------------------------------------------------------
var pFieldMem : pointer;
    pField    : PChar;
    number    : TW_INT32;
    ValStr    : string;
    RetStr    : string;
begin
  // FTrackFile is initialized in unit twa_glue procedure TWInitialize.
  // FTrackFile points to x:\windows_directory\twacker.ini

  RetStr := '';
  GetMem(pFieldMem, 2048 * SizeOf(Char));
  if (pFieldMem <> Nil) and (pName <> '')
  then begin
       pField := pFieldMem;
       if (GetPrivateProfileString(PChar(pName), Nil, '', pField, 2048, FTrackFile) > 0)
       then begin
            while (pField <> Nil) and (lstrlen(pField) > 0)
            do begin
               number := GetPrivateProfileInt(PChar(pName), pField, -1, FTrackFile);
               if (number = Id)
               then begin
                    if (lstrlen(pField) > MaxCount)
                    then pField[MaxCount-1] := #0;
                    RetStr := RetStr + pField;
                    Break;
                end;
               pField := @pField[lstrlen(pField) + 1];
            end;
       end;
       FreeMem(pFieldMem);
       try
         if (Length(RetStr) <= 0)
         then begin
              ValStr := IntToStr(ID);
              RetStr := ValStr;
         end;
       except
        On E:Exception
        do ; //ShowMessage(E.Message);
       end;
  end;
  Result := RetStr;
end; // TmcmTWAINLog.MatchTwainInt.                                         


procedure TmcmTWAINLog.Triplet2Str(Dest     : integer;
                                   DG       : integer;
                                   DAT      : integer;
                                   pData    : pointer;
                                   MSG      : integer;
                                   Return   : integer;
                                   Status   : integer;
                                   Level    : TtwnErrorLevel);
var Section      : string;
    TripStr      : string;
    DGStr        : string;
    DATStr       : string;
    CAPStr       : array[0..255] of char;
    CAPNoStr     : string;
    MSGStr       : string;
    DataStr      : string;
    ReturnStr    : string;
    ConditionStr : string;
begin
  if FileExists(FTrackFile)
  then begin
       if (DG > 0)
       then begin
            TripStr := '';

            // Convert Return and Condition code to string.
            if (Return = TWRC_CUSTOMBASE+99) and (Status = TWRC_CUSTOMBASE+99)
            then begin
                 ReturnStr := '# Pre-calling DSM Entry';
            end
            else begin
                 ReturnStr := MatchTwainInt('ReturnCode', TW_INT32(Return), 1024);
                 if (Length(ReturnStr) = 0)
                 then ReturnStr := 'Return code=Unknown'
                 else ReturnStr := 'Return code=' + ReturnStr;
                 if (Return <> TWRC_SUCCESS)
                 then begin
                      ConditionStr := MatchTwainInt('ConditionCode', TW_INT32(Status), 1024);
                      ConditionStr := 'Condition code=' + ConditionStr;
                      ReturnStr := ReturnStr  + CR_LF + ConditionStr;
                 end;
            end;

            // Ensure that Dest is set-up correctly.
            if (DG = DG_CONTROL)
            then begin
                 case DAT of
                 DAT_IDENTITY,
                 DAT_PARENT    : Dest := 0;
                 DAT_STATUS    : ; // (pDest = Nil) -> DSM, see
                                   // FDSM_Entry(pOrigin, pDest, DG, DAT, MSG, pData);
                                   // in mcmTWAINKernel.
                 else Dest := 1;
                 end;
            end
            else Dest := 1;
            case Dest of
            0 : Section := 'DG/DSM/';
            1 : Section := 'DG/DS/';
            end;

            // Get string DG type.
            DGStr := MatchTwainInt('DG', DG, 1024);

            if (DAT > DAT_NULL)
            then begin
                 // Get DAT Section in Twacker.ini.
                 Section := Section + DGStr;

                 // Convert DAT number to text.
                 DATStr := MatchTwainInt(Section, DAT, 1024);

                 // Convert MSG number to text.
                 Section := Copy(DATStr, 5, Length(DATStr));
                 MsgStr := MatchTwainInt(Section, MSG, 1024);

                 TripStr := TripStr + DGStr;
                 TripStr := TripStr + ' / ';
                 TripStr := TripStr + DATStr;

                 try
                   // Convert data to string.
                   if Assigned(pData)
                   then begin
                        case DAT of
                        DAT_CAPABILITY      : begin
                                                CapNoStr := IntToStr(pTW_Capability(pData)^.Cap);
                                                GetPrivateProfileString('CAP NUMBERS', PChar(CapNoStr), PChar(CapNoStr),
                                                                        CAPStr, SizeOf(CAPStr), FTrackFile);
                                                TripStr := TripStr + ' / ';
                                                TripStr := TripStr + CAPStr;
                                                // Convert capability data to string.
                                                DataStr := CapData2Str(pData);
                                              end;
                        DAT_EVENT           : if (MSG = MSG_PROCESSEVENT)
                                              then DataStr := 'Message=' +
                                                              MatchTwainInt('MSG', pTW_EVENT(pData)^.TWMessage, 1024) + CR_LF;
                        DAT_IDENTITY        : if (Return = TWRC_SUCCESS) and
                                                 ((MSG = MSG_GET) or
                                                  (MSG = MSG_GETFIRST) or
                                                  (MSG = MSG_GETNEXT) or
                                                  (MSG = MSG_OPENDS))
                                              then DataStr := Identity2Str(pTW_IDENTITY(pData), (MSG <> MSG_OPENDS));
                        DAT_PARENT          : DataStr := Parent2Str(TW_MEMREF(pData));
                        DAT_PENDINGXFERS    : DataStr := PendingXfers2Str(pTW_PENDINGXFERS(pData));
                        DAT_SETUPMEMXFER    : DataStr := SetupMemXfer2Str(pTW_SETUPMEMXFER(pData));
                        DAT_SETUPFILEXFER   : DataStr := SetupFileXfer2Str(pTW_SETUPFILEXFER(pData));
                        // DAT_STATUS       : ;
                        DAT_USERINTERFACE   : DataStr := UserInterface2Str(pTW_USERINTERFACE(pData));
                        DAT_XFERGROUP       : DataStr := XferGroup(pTW_UINT32(pData)^);
                        // TWUNK
                        // Additional message required for thunker to request the special
                        // identity information.
                        //DAT_TWUNKIDENTITY   : ;
                        DAT_CUSTOMDSDATA    : ;
                        DAT_DEVICEEVENT     : DataStr := DeviceEvent2Str(PTW_DEVICEEVENT(pData));
                        DAT_FILESYSTEM      : ;
                        // DAT_PASSTHRU     : ;
                        // Data Argument Types for the DG_IMAGE Data Group.
                        DAT_IMAGEINFO       : DataStr := ImageInfo2Str(pTW_IMAGEINFO(pData));
                        DAT_IMAGELAYOUT     : DataStr := ImageLayout2Str(pTW_IMAGELAYOUT(pData));
                        DAT_IMAGEMEMXFER    : DataStr := ImageMemXfer2Str(pTW_IMAGEMEMXFER(pData));
                        DAT_IMAGENATIVEXFER : ;
                        DAT_IMAGEFILEXFER   : ;
                        DAT_CIECOLOR        : ;
                        DAT_GRAYRESPONSE    : ;
                        DAT_RGBRESPONSE     : ;
                        DAT_JPEGCOMPRESSION : ;
                        DAT_PALETTE8        : ;
                        DAT_EXTIMAGEINFO    : ;
                        // Data Argument Types for the DG_AUDIO Data Group.
                        DAT_AUDIOFILEXFER   : ;
                        DAT_AUDIOINFO       : ;
                        DAT_AUDIONATIVEXFER : ;
                        DAT_SETUPFILEXFER2  : DataStr := SetupFileXfer22Str(pTW_SETUPFILEXFER2(pData));
                        end;
                   end;
                 except
                   On E:Exception
                   do DataStr := 'Exception: ' + E.Message;
                 end;
            end;

            TripStr := TripStr + ' / ';
            TripStr := TripStr + MSGStr;

            TripStr := TripStr + CR_LF;
            TripStr := TripStr + ReturnStr;

            if (Length(TripStr) > 0)
            then Str2File(TripStr);
            if (Length(DataStr) > 0)
            then Str2File(DataStr)
            else Str2File('');
       end;
  end;
end; // TmcmTWAINLog.Triplet2Str.                                           


function TmcmTWAINLog.ItemValue2Str(DataType : word; pData : pointer) : variant; // string;
//------------------------------------------------------------------------------
// GetItem -
//------------------------------------------------------------------------------
var twInt8   : TW_INT8;
    twInt16  : TW_INT16;
    twUInt16 : TW_UINT16;
    twInt32  : TW_INT32;
    twUInt32 : TW_UINT32;
    twFix32  : TW_FIX32;
    twFrame  : TW_FRAME;
    Value    : real;
    TmpText  : string;
begin
  case DataType of
  TWTY_INT8    : begin
                   twInt8 := TW_INT8(pData^);
                   Result := twInt8;
                   TmpText := IntToStr(twInt8);
                 end;
  TWTY_UINT8   : begin
                   twInt8 := TW_INT8(pData^);
                   Result := twInt8;
                   TmpText := IntToStr(twInt8);
                 end;
  TWTY_INT16,
  TWTY_HANDLE  : begin
                   twInt16 := TW_INT16(pData^);
                   Result := twInt16;
                   TmpText := IntToStr(twInt16);
                 end;
  TWTY_UINT16,
  TWTY_BOOL    : begin
                   twUInt16 := TW_UINT16(pData^);
                   Result := twUInt16;
                   TmpText := IntToStr(twUInt16);
                 end;
  TWTY_INT32   : begin
                   twInt32 := TW_INT32(pData^);
                   Result := twInt32;
                   TmpText := IntToStr(twInt32);
                 end;
  TWTY_UINT32,
  TWTY_MEMREF  : begin
                   twUInt32 := TW_UINT32(pData^);
                   Result := twUInt32;
                   TmpText := IntToStr(twUInt32);
                 end;
  TWTY_FIX32   : begin // TWTY_FIX32
                   twFix32.Whole := pTW_FIX32(pData)^.Whole;
                   twFix32.Frac  := pTW_FIX32(pData)^.Frac;
                   value         := FIX32ToFloat(twFix32);
                   Result := Value;
                   TmpText := FloatToStr(Value);
                 end;
  TWTY_FRAME   : begin
                   twFrame := TW_FRAME(pData^);
                   TmpText := FloatToStrF(FIX32ToFloat(twFrame.Left), ffFixed, 15, 2);
                   TmpText := TmpText + ' ' + FloatToStrF(FIX32ToFloat(twFrame.Top), ffFixed, 15, 2);
                   TmpText := TmpText + ' ' + FloatToStrF(FIX32ToFloat(twFrame.Right), ffFixed, 15, 2);
                   TmpText := TmpText + ' ' + FloatToStrF(FIX32ToFloat(twFrame.Bottom), ffFixed, 15, 2);
                   Result := TmpText;
                 end;
  TWTY_STR32,
  TWTY_STR64,
  TWTY_STR128,
  TWTY_STR255,
  TWTY_STR1024 : begin
                   {$IFDEF UNICODE}
                     TmpText := Utf8ToAnsi(PAnsiChar(pData)^);
                   {$ELSE}
                     TmpText := PAnsiChar(pData)^;
                   {$ENDIF}
                   Result := TmpText;
                 end;
  TWTY_UNI512  : begin
                   {$IFDEF UNICODE}
                     TmpText := PWideChar(pData)^;
                     Result := TmpText;
                   {$ELSE}
                     TmpText := PChar(pData)^;
                     Result := TmpText;
                   {$ENDIF}
                 end;
  // TWTY_SPECIAL,
  // TWTY_LIST,
  TWTY_ELEMENT8 : begin
                    Result := 0; // What to do ?
                  end;
  else begin
         Result := 0; // What to do ?
       end;
  end;
  // Result := TmpText;
end; // TmcmTWAINLog.ItemValue2Str.                                         


function TmcmTWAINLog.ItemType2Str(DataType : word) : string;
//------------------------------------------------------------------------------
// ItemType2Str
// Converts an ItemType numerical indicator to the string representation.
//------------------------------------------------------------------------------
begin
  case datatype of
  TWTY_INT8     : Result := 'TWTY_INT8';
  TWTY_INT16    : Result := 'TWTY_INT16';
  TWTY_INT32    : Result := 'TWTY_INT32';
  TWTY_UINT8    : Result := 'TWTY_UINT8';
  TWTY_UINT16   : Result := 'TWTY_UINT16';
  TWTY_UINT32   : Result := 'TWTY_UINT32';
  TWTY_BOOL     : Result := 'TWTY_BOOL';
  TWTY_FIX32    : Result := 'TWTY_FIX32';
  TWTY_FRAME    : Result := 'TWTY_FRAME';
  TWTY_STR32    : Result := 'TWTY_STR32';
  TWTY_STR64    : Result := 'TWTY_STR64';
  TWTY_STR128   : Result := 'TWTY_STR128';
  TWTY_STR255   : Result := 'TWTY_STR255';
  TWTY_STR1024  : Result := 'TWTY_STR1024';
  TWTY_UNI512   : Result := 'TWTY_UNI512';
  // TWTY_SPECIAL  : Result := 'TWTY_SPECIAL';
  TWTY_MEMREF   : Result := 'TWTY_MEMREF';
  TWTY_HANDLE   : Result := 'TWTY_HANDLE';
  TWTY_ELEMENT8 : Result := 'TWTY_ELEMENT8';
  // TWTY_LIST     : Result := 'TWTY_LIST';
  else Result := '';
  end;
end; // TmcmTWAINLog.ItemType2Str.                                          


function TmcmTWAINLog.Identity2Str(Value    : pTW_IDENTITY;
                                   NameOnly : boolean) : string;
var ValueStr   : string;
begin
  if NameOnly
  then FmtStr(ValueStr, 'Product name=%0:s%1:s', [Value^.ProductName, CR_LF])
  else begin
       FmtStr(ValueStr, 'Product name=%0:s%1:s', [Value^.ProductName, CR_LF]);
       FmtStr(ValueStr, '%0:sProduct family=%1:s%2:s', [ValueStr, Value^.ProductFamily, CR_LF]);
       FmtStr(ValueStr, '%0:sManufacturer=%1:s%2:s', [ValueStr, Value^.Manufacturer, CR_LF]);
       FmtStr(ValueStr, '%0:sId=%1:x%2:s', [ValueStr, Value^.Id, CR_LF]);
       FmtStr(ValueStr, '%0:sVersion=%1:d.%2:d%3:s', [ValueStr, Value^.Version.MajorNum, Value^.Version.MinorNum, CR_LF]);
       FmtStr(ValueStr, '%0:sLanguage=%1:d%2:s', [ValueStr, Value^.Version.Language, CR_LF]);
       FmtStr(ValueStr, '%0:sCountry=%1:d%2:s', [ValueStr, Value^.Version.Country, CR_LF]);
       FmtStr(ValueStr, '%0:sInformation=%1:s%2:s', [ValueStr, Value^.Version.Info, CR_LF]);
       FmtStr(ValueStr, '%0:sProtocol=%1:d.%2:d%3:s', [ValueStr, Value^.ProtocolMajor, Value^.ProtocolMinor, CR_LF]);
       ValueStr := ValueStr + XferGroup(Value^.SupportedGroups);
  end;
  Result := ValueStr;
end; // TmcmTWAINLog.Identity2Str.                                          


function TmcmTWAINLog.Parent2Str(Value : TW_MEMREF) : string;
var ValueStr : string;
begin
  FmtStr(ValueStr, 'Window handle=%0:x%1:s', [THandle(Value^), CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.Parent2Str.                                            


function TmcmTWAINLog.DeviceEvent2Str(Value : pTW_DEVICEEVENT) : string;
var ValueStr : string;
    A, B, C  : Variant;
begin
  ValueStr := 'Device event=' + MatchTwainInt('DeviceEvents', Value^.Event, 1024) + CR_LF;
  FmtStr(ValueStr, '%0:sDevice name=%1:s%2:s', [ValueStr, Value^.DeviceName, CR_LF]);
  case TTwnDeviceEvent(Value^.Event) of
  TWDE_CHECKAUTOMATICCAPTURE  : begin
                                  A := Value^.AutomaticCapture;
                                  B := Value^.TimeBeforeFirstCapture;
                                  C := Value^.TimeBetweenCaptures;
                                  FmtStr(ValueStr, '%0:sAutomatic capture=%1:d%2:s', [ValueStr, {$IFDEF DCB3}longint(A){$ELSE}longword(A){$ENDIF}, CR_LF]);
                                  FmtStr(ValueStr, '%0:sTime before first capture=%1:d%2:s', [ValueStr, {$IFDEF DCB3}longint(B){$ELSE}longword(B){$ENDIF}, CR_LF]);
                                  FmtStr(ValueStr, '%0:sTime between capture=%1:d%2:s', [ValueStr, {$IFDEF DCB3}longint(C){$ELSE}longword(C){$ENDIF}, CR_LF]);
                                end;
  TWDE_CHECKBATTERY           : begin
                                  A := Value^.BatteryMinutes;
                                  B := Value^.BatteryPercentage;
                                  FmtStr(ValueStr, '%0:sBattery minutes=%1:d%2:s', [ValueStr, longint(A), CR_LF]);
                                  FmtStr(ValueStr, '%0:sBattery percentage=%1:d%2:s', [ValueStr, {$IFDEF DCB3}longint(B){$ELSE}longword(B){$ENDIF}, CR_LF]);
                                end;
  TWDE_CHECKDEVICEONLINE      : ;
  TWDE_CHECKFLASH             : begin
                                  A := Value^.FlashUsed2;
                                  FmtStr(ValueStr, '%0:sFlask used 2=%1:d%2:s', [ValueStr, {$IFDEF DCB3}longint(A){$ELSE}longword(A){$ENDIF}, CR_LF]);
                                end;
  TWDE_CHECKPOWERSUPPLY       : begin
                                  A := Value^.PowerSupply;
                                  FmtStr(ValueStr, '%0:sPower supply=%1:d%2:s', [ValueStr, {$IFDEF DCB3}longint(A){$ELSE}longword(A){$ENDIF}, CR_LF]);
                                end;
  TWDE_CHECKRESOLUTION        : begin
                                  A := FIX32ToFloat(Value^.XResolution);
                                  B := FIX32ToFloat(Value^.YResolution);
                                  FmtStr(ValueStr, '%0:sX resolution=%1:0.4f%2:s', [ValueStr, real(A), CR_LF]);
                                  FmtStr(ValueStr, '%0:sY resolution=%1:0.4f%2:s', [ValueStr, real(B), CR_LF]);
                                end;
  // All other TWDE_xxx events does not provide data (A = B = C = 0)
  end;

  Result := ValueStr;
end; // TmcmTWAINLog.DeviceEvent2Str.                                       


function TmcmTWAINLog.ImageInfo2Str(Value : pTW_IMAGEINFO) : string;
var ValueStr : string;
    i        : integer;
begin
  FmtStr(ValueStr, 'XResolution=%0:0.4f%1:s', [FIX32ToFloat(Value^.XResolution), CR_LF]);
  FmtStr(ValueStr, '%0:sYResolution=%1:0.4f%2:s', [ValueStr, FIX32ToFloat(Value^.YResolution), CR_LF]);
  FmtStr(ValueStr, '%0:sImageWidth=%1:d%2:s', [ValueStr, Value^.ImageWidth, CR_LF]);
  FmtStr(ValueStr, '%0:sImageLength=%1:d%2:s', [ValueStr, Value^.ImageLength, CR_LF]);
  FmtStr(ValueStr, '%0:sSamplesPerPixel=%1:d%2:s', [ValueStr, Value^.SamplesPerPixel, CR_LF]);
  FmtStr(ValueStr, '%0:sBitsPerSample=%1:d', [ValueStr, Value^.BitsPerSample[0]]);
  for i := 1 to 7
  do FmtStr(ValueStr, '%0:s, %1:d', [ValueStr, Value^.BitsPerSample[i]]);
  ValueStr := ValueStr + CR_LF;
  FmtStr(ValueStr, '%0:sBitsPerPixel=%1:d%2:s', [ValueStr, Value^.BitsPerPixel, CR_LF]);
  ValueStr := ValueStr + 'Planar=' + MatchTwainInt('Planar', Value^.Planar, 1024) + CR_LF;
  ValueStr := ValueStr + 'PixelType=' + MatchTwainInt('PixelType', Value^.PixelType, 1024) + CR_LF;
  ValueStr := ValueStr + 'Compression=' + MatchTwainInt('Compression', Value^.Compression, 1024) + CR_LF;
  Result := ValueStr;
end; // TmcmTWAINLog.ImageInfo2Str.                                         


function TmcmTWAINLog.Frame2Str(Value : pTW_FRAME) : string;
var ValueStr : string;
begin
  FmtStr(ValueStr, 'Frame.Left=%0:0.4f%1:s', [FIX32ToFloat(Value.Left), CR_LF]);
  FmtStr(ValueStr, '%0:sFrame.Top=%1:0.4f%2:s', [ValueStr, FIX32ToFloat(Value.Top), CR_LF]);
  FmtStr(ValueStr, '%0:sFrame.Right=%1:0.4f%2:s', [ValueStr, FIX32ToFloat(Value.Right), CR_LF]);
  FmtStr(ValueStr, '%0:sFrame.Bottom=%1:0.4f%2:s', [ValueStr, FIX32ToFloat(Value.Bottom), CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.Frame2Str.                                             


function TmcmTWAINLog.ImageLayout2Str(Value : pTW_IMAGELAYOUT) : string;
var ValueStr : string;
begin
  ValueStr := Frame2Str(@Value^.Frame);
  FmtStr(ValueStr, '%0:sDocumentNumber=%1:d%2:s', [ValueStr, Value^.DocumentNumber, CR_LF]);
  FmtStr(ValueStr, '%0:sPageNumber=%1:d%2:s', [ValueStr, Value^.PageNumber, CR_LF]);
  FmtStr(ValueStr, '%0:sFrameNumber=%1:d%2:s', [ValueStr, Value^.FrameNumber, CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.ImageLayout2Str.                                       


function TmcmTWAINLog.ImageMemXfer2Str(Value : pTW_IMAGEMEMXFER) : string;
var ValueStr : string;
    FlagStr  : string;
begin
  ValueStr := 'Compression=' + MatchTwainInt('Compression', Value^.Compression, 1024) + CR_LF;
  FmtStr(ValueStr, '%0:sBytes per row=%1:d%2:s', [ValueStr, Value^.BytesPerRow, CR_LF]);
  FmtStr(ValueStr, '%0:sColumns=%1:d%2:s', [ValueStr, Value^.Columns, CR_LF]);
  FmtStr(ValueStr, '%0:sRows=%1:d%2:s', [ValueStr, Value^.Rows, CR_LF]);
  FmtStr(ValueStr, '%0:sX offset=%1:d%2:s', [ValueStr, Value^.XOffset, CR_LF]);
  FmtStr(ValueStr, '%0:sY offset=%1:d%2:s', [ValueStr, Value^.YOffset, CR_LF]);
  FmtStr(ValueStr, '%0:sBytes written=%1:d%2:s', [ValueStr, Value^.BytesWritten, CR_LF]);

  FlagStr := '';
  if ((Value^.Memory.Flags and TWMF_APPOWNS) <> 0)
  then FlagStr := FlagStr + 'TWMF_APPOWNS, ';
  if ((Value^.Memory.Flags and TWMF_DSMOWNS) <> 0)
  then FlagStr := FlagStr + 'TWMF_DSMOWNS, ';
  if ((Value^.Memory.Flags and TWMF_DSOWNS) <> 0)
  then FlagStr := FlagStr + 'TWMF_DSOWNS, ';
  if ((Value^.Memory.Flags and TWMF_POINTER) <> 0)
  then FlagStr := FlagStr + 'TWMF_POINTER, ';
  if ((Value^.Memory.Flags and TWMF_HANDLE) <> 0)
  then FlagStr := FlagStr + 'TWMF_HANDLE, ';
  ValueStr := ValueStr + 'Memory flags=' + Copy(FlagStr, 0, Length(FlagStr)-2) + CR_LF;

  FmtStr(ValueStr, '%0:sMemory length=%1:d%2:s', [ValueStr, Value^.Memory.Length, CR_LF]);
  FmtStr(ValueStr, '%0:sMemory pointer=%1:x%2:s', [ValueStr, {$IFDEF DCB3}longint(Value^.Memory.TheMem){$ELSE}longword(Value^.Memory.TheMem){$ENDIF}, CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.ImageMemXfer2Str.                                      


function TmcmTWAINLog.PendingXfers2Str(Value : pTW_PENDINGXFERS) : string;
var ValueStr : string;
begin
  FmtStr(ValueStr, 'Pending transfers=%0:d%1:s', [Value^.Count, CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.PendingXfers2Str.                                      


function TmcmTWAINLog.SetupMemXfer2Str(Value : pTW_SETUPMEMXFER) : string;
var ValueStr : string;
begin
  FmtStr(ValueStr, 'Min Buffer Size=%0:d%1:s', [Value^.MinBufSize, CR_LF]);
  FmtStr(ValueStr, '%0:sMax Buffer Size=%1:d%2:s', [ValueStr, Value^.MaxBufSize, CR_LF]);
  FmtStr(ValueStr, '%0:sPreferred Buffer Size=%1:d%2:s', [ValueStr, Value^.Preferred, CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.SetupMemXfer2Str.                                      


function TmcmTWAINLog.SetupFileXfer2Str(Value : pTW_SETUPFILEXFER) : string;
var ValueStr : string;
begin
  FmtStr(ValueStr, 'File name=%0:s%1:s', [Value^.FileName, CR_LF]);
  FmtStr(ValueStr, '%0:sFormat=%1:d%2:s', [ValueStr, Value^.Format, CR_LF]);
  FmtStr(ValueStr, '%0:sVRefNum=%1:d%2:s', [ValueStr, Value^.VRefNum, CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.SetupFileXfer2Str.                                     


function TmcmTWAINLog.SetupFileXfer22Str(Value : pTW_SETUPFILEXFER2) : string;
var ValueStr : string;
begin
  FmtStr(ValueStr, 'File name=%0:s%1:s', [Value^.FileName, CR_LF]);
  FmtStr(ValueStr, '%0:sFile name type=%1:d%2:s', [ValueStr, Value^.FileNameType, CR_LF]);
  FmtStr(ValueStr, '%0:sFormat=%1:d%2:s', [ValueStr, Value^.Format, CR_LF]);
  FmtStr(ValueStr, '%0:sVRefNum=%1:d%2:s', [ValueStr, Value^.VRefNum, CR_LF]);
  FmtStr(ValueStr, '%0:sparID=%1:d%2:s', [ValueStr, Value^.parID, CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.SetupFileXfer22Str.                                    


function TmcmTWAINLog.UserInterface2Str(Value : pTW_USERINTERFACE) : string;
var ValueStr : string;
begin
  FmtStr(ValueStr, 'Show UI=%0:d%1:s', [Value^.ShowUI, CR_LF]);
  FmtStr(ValueStr, '%0:sModal UI=%1:d%2:s', [ValueStr, Value^.ModalUI, CR_LF]);
  FmtStr(ValueStr, '%0:shParent=%1:x%2:s', [ValueStr, Value^.hParent, CR_LF]);
  Result := ValueStr;
end; // TmcmTWAINLog.UserInterface2Str.                                     


function TmcmTWAINLog.XferGroup(Value : TW_UINT32) : string;
var ValueStr : string;
begin
  ValueStr := '';
  if ((Value and DG_CONTROL) <> 0)
  then ValueStr := ValueStr + 'DG_CONTROL, ';
  if ((Value and DG_IMAGE) <> 0)
  then ValueStr := ValueStr + 'DG_IMAGE, ';
  if ((Value and DG_AUDIO) <> 0)
  then ValueStr := ValueStr + 'DG_AUDIO, ';
  ValueStr := 'Supported Groups=' + Copy(ValueStr, 1, Length(ValueStr) - 2);
  Result := ValueStr + CR_LF;
end; // TmcmTWAINLog.XferGroup.                                             


function TmcmTWAINLog.Container2Str(ConType : longint) : string;
begin
  case ConType of
  TWON_ARRAY       : Result := 'TWON_ARRAY';
  TWON_ENUMERATION : Result := 'TWON_ENUMERATION';
  TWON_ONEVALUE    : Result := 'TWON_ONEVALUE';
  TWON_RANGE       : Result := 'TWON_RANGE';
  TWON_DSMCODEID   : Result := 'TWON_DSMCODEID';
  TWON_DSMID       : Result := 'TWON_DSMID';
  TWON_ICONID      : Result := 'TWON_ICONID';
  TWON_DONTCARE8   : Result := 'TWON_DONTCARE8';
  TWON_DONTCARE16  : Result := 'TWON_DONTCARE16';
  longint(TWON_DONTCARE32)  : Result := 'TWON_DONTCARE32';
  else Result := 'ERROR';
  end;
end; // TmcmTWAINLog.Container2Str.                                         


function TmcmTWAINLog.Msg2Str(Msg : integer) : string;
begin
  // mcm 20.05.2000, expanded to include all TWAIN ver 1.9 messages.
  case Msg of
  MSG_NULL                      : Result := 'MSG_NULL';
  MSG_CUSTOMBASE                : Result := 'MSG_CUSTOMBASE';
  MSG_GET                       : Result := 'MSG_GET';
  MSG_GETCURRENT                : Result := 'MSG_GETCURRENT';
  MSG_GETDEFAULT                : Result := 'MSG_GETDEFAULT';
  MSG_GETFIRST                  : Result := 'MSG_GETFIRST';
  MSG_GETNEXT                   : Result := 'MSG_GETNEXT';
  MSG_SET                       : Result := 'MSG_SET';
  MSG_RESET                     : Result := 'MSG_RESET';
  MSG_QUERYSUPPORT              : Result := 'MSG_QUERYSUPPORT';
  MSG_XFERREADY                 : Result := 'MSG_XFERREADY';
  MSG_CLOSEDSREQ                : Result := 'MSG_CLOSEDSREQ';
  MSG_CLOSEDSOK                 : Result := 'MSG_CLOSEDSOK';
  MSG_DEVICEEVENT               : Result := 'MSG_DEVICEEVENT';
  MSG_CHECKSTATUS               : Result := 'MSG_CHECKSTATUS';
  MSG_OPENDSM                   : Result := 'MSG_OPENDSM';
  MSG_CLOSEDSM                  : Result := 'MSG_CLOSEDSM';
  MSG_OPENDS                    : Result := 'MSG_OPENDS';
  MSG_CLOSEDS                   : Result := 'MSG_CLOSEDS';
  MSG_USERSELECT                : Result := 'MSG_USERSELECT';
  MSG_DISABLEDS                 : Result := 'MSG_DISABLEDS';
  MSG_ENABLEDS                  : Result := 'MSG_ENABLEDS';
  MSG_ENABLEDSUIONLY            : Result := 'MSG_ENABLEDSUIONLY';
  MSG_PROCESSEVENT              : Result := 'MSG_PROCESSEVENT';
  MSG_ENDXFER                   : Result := 'MSG_ENDXFER';
  MSG_STOPFEEDER                : Result := 'MSG_STOPFEEDER';
  MSG_CHANGEDIRECTORY           : Result := 'MSG_CHANGEDIRECTORY';
  MSG_CREATEDIRECTORY           : Result := 'MSG_CREATEDIRECTORY';
  MSG_DELETE                    : Result := 'MSG_DELETE';
  MSG_FORMATMEDIA               : Result := 'MSG_FORMATMEDIA';
  MSG_GETCLOSE                  : Result := 'MSG_GETCLOSE';
  MSG_GETFIRSTFILE              : Result := 'MSG_GETFIRSTFILE';
  MSG_GETINFO                   : Result := 'MSG_GETINFO';
  MSG_GETNEXTFILE               : Result := 'MSG_GETNEXTFILE';
  MSG_RENAME                    : Result := 'MSG_RENAME';
  MSG_COPY                      : Result := 'MSG_COPY';
  MSG_AUTOMATICCAPTUREDIRECTORY : Result := 'MSG_AUTOMATICCAPTUREDIRECTORY';
  MSG_PASSTHRU                  : Result := 'MSG_PASSTHRU';
  else Result := IntToStr(Msg);
  end;
end; // TmcmTWAINLog.Msg2Str.                                               


function TmcmTWAINLog.Dat2Str(DAT : integer) : string;
var DATStr    : string;
    ReturnStr : array[0..255] of char;
begin
  DATStr := IntToStr(DAT);
  GetPrivateProfileString('DAT NUMBERS',
                          PChar(DATStr),
                          'ERROR',
                          ReturnStr,
                          SizeOf(ReturnStr),
                          FTrackFile);
  Result := ReturnStr;
end; // TmcmTWAINLog.Dat2Str.                                               


function TmcmTWAINLog.Cap2Str(CAP : integer) : string;
var CapStr    : String;
    ReturnStr : array[0..255] of Char;
begin
  CapStr := IntToStr(Cap);
  GetPrivateProfileString('CAP NUMBERS',
                          PChar(CapStr),
                          'ERROR',
                          ReturnStr,
                          SizeOf(ReturnStr),
                          FTrackFile);
  Result := ReturnStr;
end; // TmcmTWAINLog.Cap2Str.                                               


function TmcmTWAINLog.Attribute2Str(CapStr   : string;
                                    Cap      : integer;
                                    ItemType : TW_UINT16;
                                    pItem    : Variant) : string;
var ReturnStr : string;
    Buffer    : array[0..8192] of char; // mcm 11.07.1999, expanded to hold all
                                        // capabilities.
    Token     : array[0..255] of char;
    TmpStr    : string;
    x, y      : integer;
    pVal      : TW_UINT16;
begin
  case ItemType of
  TWTY_BOOL  : ReturnStr := Variant(Abs(pItem));
  else ReturnStr := pItem;
  end;

  // Use the CAP section instead of SUPORTEDCAPS and CAP_EXTENDEDCAPS
  // section!
  if (CapStr = 'CAP_SUPPORTEDCAPS') or (CapStr = 'CAP_EXTENDEDCAPS')
  then CapStr := 'CAP';
  GetPrivateProfileSection(PChar(CapStr), Buffer, SizeOf(Buffer), FTrackFile);

  // If the cap is numerical, there will be no section entry for it in the
  // INI file, so return the number.  The only case explicitly checked for is
  // FIX32 since bools and enumerated lists will have been picked up with a
  // text substitution.
  if (StrLen(Buffer) <> 0)
  then begin
       x := 0;
       // This will be skipped if numerical cap value from above.
       while (buffer[x] <> #0)
       do begin
          y := 0;
          FillChar(Token, 255, 0);
          while(buffer[x] <> '=')
          do begin
             token[y] := buffer[x];
             inc(x);
             inc(y);
          end;
          inc(x);

          if (ItemType = TWTY_BOOL)
          then TmpStr := Variant(Abs(pItem))
          else TmpStr := pItem; 
          if (TmpStr = PChar(@buffer[x]))
          then begin
               ReturnStr := Token;
               break;
          end;

          // Patch to allow the CAP_SUPPORTEDCAPS.
          if (Cap = CAP_SUPPORTEDCAPS)
          then begin
               pVal := pItem;
               if (pVal = TW_UINT16(StrToInt(PChar(@buffer[x]))))
               then begin
                    ReturnStr := Token;
                    break;
               end;
          end;

          while(buffer[x] <> #0)
          do inc(x);
          inc(x);
       end;
  end;
  Result := ReturnStr;
end; // TmcmTWAINLog.Attribute2Str.                                         


function TmcmTWAINLog.CapData2Str(twCap : pTW_Capability) : string;
var CapStr       : string;
    pRange       : pTW_RANGE;
    pOneValue    : pTW_ONEVALUE;
    pEnumeration : pTW_ENUMERATION;
    pArray       : pTW_ARRAY;
    pItem        : PChar;
    ItemSize     : integer;
    index        : integer;
    ItemStr      : string;
    ReturnStr    : string;
begin
  ReturnStr := '';
  CapStr := Cap2Str(twCap^.Cap);
  if (twCap^.hContainer <> 0)
  then begin
       case twCap^.ConType of
       TWON_ONEVALUE    : begin
                            pOneValue := pTW_ONEVALUE(DSMMemLock(twCap^.hContainer));
                            if Assigned(pOneValue)
                            then begin
                                 ReturnStr := 'ItemType=' + MatchTwainInt('ItemType', pOneValue^.ItemType, 1024);
                                 ReturnStr := ReturnStr + CR_LF;
                                 ItemStr := Attribute2Str(CapStr,
                                                          twCap^.Cap,
                                                          pOneValue^.ItemType,
                                                          ItemValue2Str(pOneValue^.ItemType, PChar(@pOneValue^.Item)));
                                 ReturnStr := ReturnStr + 'Item=' + ItemStr + CR_LF;
                            end;
                            DSMMemUnlock(twCap^.hContainer);
                          end;
       TWON_ENUMERATION : begin
                            pEnumeration := pTW_ENUMERATION(DSMMemLock(twCap^.hContainer));
                            if Assigned(pEnumeration)
                            then begin
                                 ReturnStr := 'ItemType=' + MatchTwainInt('ItemType', pEnumeration^.ItemType, 1024);
                                 ReturnStr := ReturnStr + CR_LF;

                                 FmtStr(ItemStr, 'NumItems=%d', [pEnumeration^.NumItems]);
                                 ReturnStr := ReturnStr + ItemStr + CR_LF;

                                 FmtStr(ItemStr, 'CurrentIndex=%d', [pEnumeration^.CurrentIndex]);
                                 ReturnStr := ReturnStr + ItemStr + CR_LF;

                                 FmtStr(ItemStr, 'DefaultIndex=%d', [pEnumeration^.DefaultIndex]);
                                 ReturnStr := ReturnStr + ItemStr + CR_LF;

                                 pItem    := @pEnumeration^.ItemList;
                                 ItemSize := TWItemSize[pEnumeration^.ItemType];
                                 for index := 0 to (pEnumeration^.NumItems - 1)
                                 do begin
                                    ItemStr := Attribute2Str(CapStr,
                                                             twCap^.Cap,
                                                             pEnumeration^.ItemType,
                                                             ItemValue2Str(pEnumeration^.ItemType, pItem));
                                    ReturnStr := ReturnStr + 'Item[' + IntToStr(index) + ']=' + ItemStr + CR_LF;
                                    pItem := pItem + ItemSize;
                                 end;
                            end;
                            DSMMemUnlock(twCap^.hContainer);
                          end;
       TWON_RANGE       : begin
                            pRange := pTW_RANGE(DSMMemLock(twCap^.hContainer));
                            if Assigned(pRange)
                            then begin
                                 ReturnStr := 'ItemType=' + MatchTwainInt('ItemType', pRange^.ItemType, 1024);
                                 ReturnStr := ReturnStr + CR_LF;

                                 ItemStr := Attribute2Str(CapStr,
                                                          twCap^.Cap,
                                                          pRange^.ItemType,
                                                          ItemValue2Str(pRange^.ItemType, @pRange^.MinValue));
                                 ReturnStr := ReturnStr + 'MinValue=' + ItemStr + CR_LF;

                                 ItemStr := Attribute2Str(CapStr,
                                                          twCap^.Cap,
                                                          pRange^.ItemType,
                                                          ItemValue2Str(pRange^.ItemType, @pRange^.MaxValue));
                                 ReturnStr := ReturnStr + 'MaxValue=' + ItemStr + CR_LF;

                                 ItemStr := Attribute2Str(CapStr,
                                                          twCap^.Cap,
                                                          pRange^.ItemType,
                                                          ItemValue2Str(pRange^.ItemType, @pRange^.StepSize));
                                 ReturnStr := ReturnStr + 'StepSize=' + ItemStr + CR_LF;

                                 ItemStr := Attribute2Str(CapStr,
                                                          twCap^.Cap,
                                                          pRange^.ItemType,
                                                          ItemValue2Str(pRange^.ItemType, @pRange^.DefaultValue));
                                 ReturnStr := ReturnStr + 'DefaultValue=' + ItemStr + CR_LF;

                                 ItemStr := Attribute2Str(CapStr,
                                                          twCap^.Cap,
                                                          pRange^.ItemType,
                                                          ItemValue2Str(pRange^.ItemType, @pRange^.CurrentValue));
                                 ReturnStr := ReturnStr + 'CurrentValue=' + ItemStr + CR_LF;
                            end;
                            DSMMemUnlock(twCap^.hContainer);
                          end;
       TWON_ARRAY       : begin
                            pArray := DSMMemLock(twCap^.hContainer);
                            if Assigned(pArray)
                            then begin
                                 ReturnStr := 'ItemType=' + MatchTwainInt('ItemType', pArray^.ItemType, 1024);
                                 ReturnStr := ReturnStr + CR_LF;

                                 FmtStr(ItemStr, 'NumItems=%d', [pArray^.NumItems]);
                                 ReturnStr := ReturnStr + ItemStr + CR_LF;

                                 pItem    := @pArray^.ItemList;
                                 ItemSize := TWItemSize[pArray^.ItemType];
                                 for index := 0 to (pArray^.NumItems - 1)
                                 do begin
                                    ItemStr := Attribute2Str(CapStr,
                                                             twCap^.Cap,
                                                             pArray^.ItemType,
                                                             ItemValue2Str(pArray^.ItemType, pItem));
                                    ReturnStr := ReturnStr + 'Item[' + IntToStr(index) + ']=' + ItemStr + CR_LF;
                                    pItem := pItem + ItemSize;
                                 end;
                            end;
                            DSMMemUnlock(twCap^.hContainer);
                          end;
       end;
  end;
  Result := ReturnStr;
end; // TmcmTWAINLog.CapData2Str.                                           


{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}
{$IFDEF COMPV3} {$UNDEF COMPV3} {$ENDIF}


end.
