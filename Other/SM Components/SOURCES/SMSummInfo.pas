{ Copyright (C) 1998-2006, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
          mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  Get Summary Information and Document Summary Information for compound file
}

unit SMSummInfo;

interface

uses Classes, Windows, ActiveX, ComObj, AxCtrls;

type
  TSMSummaryInformation = class(TComponent)
  private
    { Private declarations }
    {summary info}
    FAppName: string;
    FAuthor: string;
    FCharCount: Integer;
    FComments: string;
    FCreated: TDateTime;
    FEditTime: Integer;
    FKeywords: string;
    FLastAuthor: string;
    FLastPrinted: TDateTime;
    FLastSaved: TDateTime;
    FPageCount: Integer;
    FRevNumber: string;
    FSubject: string;
    FTemplate: string;
    FTitle: string;
    FWordCount: Integer;

    {document info}
    FCategory: string;
    FPresentationTarget: string;
    FByteCount: Integer;
    FLineCount: Integer;
    FParagraphCount: Integer;
    FSlideCount: Integer;
    FNoteCount: Integer;
    FHiddenSlideCount: Integer;
    FMMClipCount: Integer;
    FManager: string;
    FCompany: string;
    FCharCount2: Integer;
  protected
    { Protected declarations }
    function FileTimeToElapsedTime(FileTime: TFileTime): Integer;
    function FileTimeToDateTime(FileTime: TFileTime): TDateTime;

    procedure LoadSumInfo(Storage: IStorage; IsDocSummary: Boolean);
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
    procedure ParseProperty(propID: dWord; Value: Pointer; IsDocSummary: Boolean);
    procedure ParseDocProperty(propID: dWord; Value: Pointer);
    procedure Clear;

    procedure LoadFromFile(const AFileName: WideString);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromStorage(Storage: IStorage);
  published
    { Published declarations }
    {summary info}
    property AppName: string read FAppName write FAppName;
    property Author: string read FAuthor write FAuthor;
    property CharCount: Integer read FCharCount write FCharCount;
    property Comments: string read FComments write FComments;
    property Created: TDateTime read FCreated write FCreated;
    property EditTime: Integer read FEditTime write FEditTime;
    property Keywords: string read FKeywords write FKeywords;
    property LastAuthor: string read FLastAuthor write FLastAuthor;
    property LastPrinted: TDateTime read FLastPrinted write FLastPrinted;
    property LastSaved: TDateTime read FLastSaved write FLastSaved;
    property PageCount: Integer read FPageCount write FPageCount;
    property RevNumber: string read FRevNumber write FRevNumber;
    property Subject: string read FSubject write FSubject;
    property Template: string read FTemplate write FTemplate;
    property Title: string read FTitle write FTitle;
    property WordCount: Integer read FWordCount write FWordCount;

    {document info}
    property Category: string read FCategory write FCategory;
    property PresentationTarget: string read FPresentationTarget write FPresentationTarget;
    property ByteCount: Integer read FByteCount write FByteCount;
    property LineCount: Integer read FLineCount write FLineCount;
    property ParagraphCount: Integer read FParagraphCount write FParagraphCount;
    property SlideCount: Integer read FSlideCount write FSlideCount;
    property NoteCount: Integer read FNoteCount write FNoteCount;
    property HiddenSlideCount: Integer read FHiddenSlideCount write FHiddenSlideCount;
    property MMClipCount: Integer read FMMClipCount write FMMClipCount;
    property Manager: string read FManager write FManager;
    property Company: string read FCompany write FCompany;
    property CharCount2: Integer read FCharCount2 write FCharCount2;
  end;

procedure Register;

implementation

uses SysUtils;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMSummaryInformation]);
end;

const
  { Summary Information }
  ID_TITLE        = $00000002;
  ID_SUBJECT      = $00000003;
  ID_AUTHOR       = $00000004;
  ID_KEYWORDS     = $00000005;
  ID_COMMENTS     = $00000006;
  ID_TEMPLATE     = $00000007;
  ID_LASTAUTHOR   = $00000008;
  ID_REVNUMBER    = $00000009;
  ID_EDITTIME     = $0000000A;
  ID_LASTPRINTED  = $0000000B;
  ID_CREATE_DTM   = $0000000C;
  ID_LASTSAVE_DTM = $0000000D;
  ID_PAGECOUNT    = $0000000E;
  ID_WORDCOUNT    = $0000000F;
  ID_CHARCOUNT    = $00000010;
  ID_THUMBAIL     = $00000011;
  ID_APPNAME      = $00000012;
  ID_SECURITY     = $00000013;

  { Document Summary Information}
  PID_CATEGORY     = 2;
  PID_PRESFORMAT   = 3;
  PID_BYTECOUNT    = 4;
  PID_LINECOUNT    = 5;
  PID_PARCOUNT     = 6;
  PID_SLIDECOUNT   = 7;
  PID_NOTECOUNT    = 8;
  PID_HIDDENCOUNT  = 9;
  PID_MMCLIPCOUNT  = 10;
  PID_SCALE        = 11;
  PID_HEADINGPAIR  = 12;
  PID_DOCPARTS     = 13;
  PID_MANAGER      = 14;
  PID_COMPANY      = 15;
  PID_LINKSDIRTY   = 16;
  PID_CHARCOUNT2   = 17;


procedure TSMSummaryInformation.Assign(Source: TPersistent);
var
  si: TSMSummaryInformation;
begin
  if Source is TSMSummaryInformation then
  begin
    si := TSMSummaryInformation(Source);
    AppName := si.AppName;
    Author := si.Author;
    CharCount := si.CharCount;
    Comments := si.Comments;
    Created := si.Created;
    EditTime := si.EditTime;
    Keywords := si.Keywords;
    LastAuthor := si.LastAuthor;
    LastPrinted := si.LastPrinted;
    LastSaved := si.LastSaved;
    PageCount := si.PageCount;
    RevNumber := si.RevNumber;
    Subject := si.Subject;
    Template := si.Template;
    Title := si.Title;
    WordCount := si.WordCount;

    Category := si.Category;
    PresentationTarget := si.PresentationTarget;
    ByteCount := si.ByteCount;
    LineCount := si.LineCount;
    ParagraphCount := si.ParagraphCount;
    SlideCount := si.SlideCount;
    NoteCount := si.NoteCount;
    HiddenSlideCount := si.HiddenSlideCount;
    MMClipCount := si.MMClipCount;
    Manager := si.Manager;
    Company := si.Company;
    CharCount2 := si.CharCount2;
  end
  else
    inherited Assign(Source);
end;

procedure TSMSummaryInformation.Clear;
begin
  AppName := '';
  Author := '';
  CharCount := 0;
  Comments := '';
  Created := 0;
  EditTime := 0;
  Keywords := '';
  LastAuthor := '';
  LastPrinted := 0;
  LastSaved := 0;
  PageCount := 0;
  RevNumber := '';
  Subject := '';
  Template := '';
  Title := '';
  WordCount := 0;

  Category := '';
  PresentationTarget := '';
  ByteCount := 0;
  LineCount := 0;
  ParagraphCount := 0;
  SlideCount := 0;
  NoteCount := 0;
  HiddenSlideCount := 0;
  MMClipCount := 0;
  Manager := '';
  Company := '';
  CharCount2 := 0;
end;

function TSMSummaryInformation.FileTimeToElapsedTime(FileTime: TFileTime): Integer;
var
  SystemTime: TSystemTime;
  LocalFileTime: TFileTime;
begin
  Result := 0;
  if FileTimeToLocalFileTime(FileTime, LocalFileTime) and
     FileTimeToSystemTime(LocalFileTime, SystemTime)
  then
    Result := SystemTime.wMinute;
end;

function TSMSummaryInformation.FileTimeToDateTime(FileTime: TFileTime): TDateTime;
var
  FileDate: Integer;
  LocalFileTime: TFileTime;
begin
  Result := 0;
  if FileTimeToLocalFileTime(FileTime, LocalFileTime) and
     FileTimeToDosDateTime(LocalFileTime,
                           LongRec(FileDate).Hi, LongRec(FileDate).Lo) then
    try
      Result := FileDateToDateTime(FileDate);
    except
      Result := 0;
    end;
end;

procedure TSMSummaryInformation.ParseProperty(propID: dWord; Value: Pointer; IsDocSummary: Boolean);
var
  FileTime: TFileTime;
begin
  if IsDocSummary then
  begin
    ParseDocProperty(propID, Value);
    exit;
  end;

  case propID of
    ID_TITLE: FTitle := PChar(Value);
    ID_SUBJECT: FSubject := PChar(Value);
    ID_AUTHOR: FAuthor := PChar(Value);
    ID_KEYWORDS: FKeywords := PChar(Value);
    ID_COMMENTS: FComments := PChar(Value);
    ID_TEMPLATE: FTemplate := PChar(Value);
    ID_LASTAUTHOR: FLastAuthor := PChar(Value);
    ID_REVNUMBER: FRevNumber := PChar(Value);
    ID_EDITTIME: begin
                   CopyMemory(@FileTime, Value, SizeOf(FileTime));
                   FEditTime := FileTimeToElapsedTime(FileTime);
                 end;
    ID_LASTPRINTED: begin
                      CopyMemory(@FileTime, Value, SizeOf(FileTime));
                      FLastPrinted := FileTimeToDateTime(FileTime);
                    end;
    ID_CREATE_DTM: begin
                     CopyMemory(@FileTime, Value, SizeOf(FileTime));
                     FCreated := FileTimeToDateTime(FileTime);
                   end;
    ID_LASTSAVE_DTM: begin
                        CopyMemory(@FileTime, Value, SizeOf(FileTime));
                        FLastSaved := FileTimeToDateTime(FileTime);
                      end;
    ID_PAGECOUNT: CopyMemory(@FPageCount, Value, SizeOf(FPageCount));
    ID_WORDCOUNT: CopyMemory(@FWordCount, Value, SizeOf(FWordCount));
    ID_CHARCOUNT: CopyMemory(@FCharCount, Value, SizeOf(FCharCount));
    ID_THUMBAIL: ;
    ID_APPNAME: FAppName := PChar(Value);
    ID_SECURITY: ;
  end;
end;

procedure TSMSummaryInformation.ParseDocProperty(propID: dWord; Value: Pointer);
begin
  case propID of
    PID_CATEGORY: FCategory := PChar(Value);
    PID_PRESFORMAT: FPresentationTarget := PChar(Value);
    PID_BYTECOUNT: CopyMemory(@FByteCount, Value, SizeOf(FByteCount));
    PID_LINECOUNT: CopyMemory(@FLineCount, Value, SizeOf(FLineCount));
    PID_PARCOUNT: CopyMemory(@FParagraphCount, Value, SizeOf(FParagraphCount));
    PID_SLIDECOUNT: CopyMemory(@FSlideCount, Value, SizeOf(FSlideCount));
    PID_NOTECOUNT: CopyMemory(@FNoteCount, Value, SizeOf(FNoteCount));
    PID_HIDDENCOUNT: CopyMemory(@FHiddenSlideCount, Value, SizeOf(FHiddenSlideCount));
    PID_MMCLIPCOUNT: CopyMemory(@FMMClipCount, Value, SizeOf(FMMClipCount));

    PID_SCALE: {ScaleCrop: bool};
    PID_HEADINGPAIR: {variant/vector};
    PID_DOCPARTS: {Titles of Parts: variant/vector};
    PID_MANAGER: FManager := PChar(Value);
    PID_COMPANY: FCompany := PChar(Value);

    PID_LINKSDIRTY: {LinksUpTo Date: bool};
    PID_CHARCOUNT2: CopyMemory(@FCharCount2, Value, SizeOf(FCharCount2));
  end;
end;

procedure TSMSummaryInformation.LoadFromFile(const AFileName: WideString);
var
  Root: IStorage;
begin
  if StgIsStorageFile(PWideChar(AFileName)) = S_OK then
  begin
    { open the file }
    OleCheck(StgOpenStorage(PWideChar(AFileName), nil,
             STGM_READ or STGM_SHARE_DENY_NONE or STGM_PRIORITY{STGM_READ or STGM_SHARE_EXCLUSIVE}, nil, 0, Root));

    LoadFromStorage(Root)
  end
  else
    raise Exception.Create('File is not a compound storage');
end;

procedure TSMSummaryInformation.LoadFromStream(AStream: TStream);

  type
    TStreamHeader = record
      case Integer of
        0: ( { New }
          Signature: Integer;
          DrawAspect: Integer;
          DataSize: Integer);
        1: ( { Old }
          PartRect: TSmallRect);
    end;

var
  DataHandle: HGlobal;
  Buffer: Pointer;
  Header: TStreamHeader;
  Root: IStorage;
  FLockBytes: ILockBytes;
begin
  DataHandle := GlobalAlloc(GMEM_MOVEABLE, Header.DataSize);
  if DataHandle = 0 then
    OutOfMemoryError;
  try
    Buffer := GlobalLock(DataHandle);
    try
      AStream.Read(Buffer^, Header.DataSize);
    finally
      GlobalUnlock(DataHandle);
    end;
    OleCheck(CreateILockBytesOnHGlobal(DataHandle, True, FLockBytes));
    DataHandle := 0;
    OleCheck(StgOpenStorageOnILockBytes(FLockBytes, nil, STGM_READWRITE or
      STGM_SHARE_EXCLUSIVE, nil, 0, Root));

    LoadFromStorage(Root)
  except
    if DataHandle <> 0 then
      GlobalFree(DataHandle);

    raise Exception.Create('Stream is not a compound storage');
  end;
end;

procedure TSMSummaryInformation.LoadFromStorage(Storage: IStorage);
begin
  Clear;

  {load summary info}
  LoadSumInfo(Storage, False);

  {load document summary info}
  LoadSumInfo(Storage, True);
end;

procedure TSMSummaryInformation.LoadSumInfo(Storage: IStorage; IsDocSummary: Boolean);
var
  OStream: IStream;
  streamName: array[0..MAX_PATH-1] of WideChar;

  Stream: TOleStream;

  str: string;
  ID, PropertyCount, StartPos, prevPos, offset, TypeID, LenID: dWord;
  i, i4: Integer;
  FileTime: TFileTime;
begin
  if IsDocSummary then
    StringToWideChar(#5'DocumentSummaryInformation', streamName, MAX_PATH)
  else
    StringToWideChar(#5'SummaryInformation', streamName, MAX_PATH);

  if Succeeded(Storage.OpenStream(streamName, nil,
               STGM_READ or STGM_SHARE_EXCLUSIVE, 0, OStream)) then
  begin
    Stream := TOleStream.Create(OStream);
    try
      if Stream.Size > 0 then
      begin
        {Seek to offset of first section}
        Stream.Seek($2C, soFromBeginning);
        Stream.Read(StartPos, SizeOf(StartPos));

        {Seek to the PropertyCount value}
        Stream.Seek(StartPos + 4, soFromBeginning);
        Stream.Read(PropertyCount, SizeOf(PropertyCount));
        for i := 0 to PropertyCount-1 do
        begin
          { property ID }
          Stream.Read(ID, SizeOf(ID));
          { property offset }
          Stream.Read(offset, SizeOf(offset));
          prevPos := Stream.Position;

          Stream.Seek(StartPos+offset, soFromBeginning);
          Stream.Read(TypeID, SizeOf(TypeID));

          case TypeID of
            VT_EMPTY: ;
            VT_NULL: ;
            VT_I2: ; { 2 byte signed int }
            VT_I4: begin { 4 byte signed int }
                     Stream.Read(I4, SizeOf(I4));
                     ParseProperty(ID, @I4, IsDocSummary);
                   end;
            VT_R4: ; { 4 byte real }
            VT_R8: ; { 8 byte real }
            VT_CY: ; { currency }
            VT_DATE: ; { date }
            VT_BSTR: ; { binary string }
            VT_DISPATCH: ; { IDispatch FAR* }
            VT_ERROR: ; { SCODE }
            VT_BOOL: ; { True=-1, False=0 }
            VT_VARIANT: ; { VARIANT FAR* }
            VT_UNKNOWN: ; { IUnknown FAR* }

            VT_I1: ; { signed char }
            VT_UI1: ; { unsigned char }
            VT_UI2: ; { unsigned short }
            VT_UI4: ; { unsigned short }
            VT_I8: ; { signed 64-bit int }
            VT_UI8: ; { unsigned 64-bit int }
            VT_INT: ; { signed machine int }
            VT_UINT: ; { unsigned machine int }
            VT_VOID: ; { C style void }
            VT_HRESULT: ;
            VT_PTR: ; { pointer type }
            VT_SAFEARRAY: ; { (use VT_ARRAY in VARIANT) }
            VT_CARRAY: ; { C style array }
            VT_USERDEFINED: ;{ user defined type }
            VT_LPSTR: begin
                        { null terminated string }
                        Stream.Read(LenID, SizeOf(LenID));
                        SetLength(str, LenID);
                        Stream.Read(PChar(str)^, LenID);
                        ParseProperty(ID, @str[1], IsDocSummary);
                      end;
            VT_LPWSTR: ; { wide null terminated string }
            VT_FILETIME: begin
                           { FILETIME }
                           Stream.Read(FileTime, SizeOf(FileTime));
                           ParseProperty(ID, @FileTime, IsDocSummary);
                         end;
            VT_BLOB:                ; { Length prefixed bytes }
            VT_STREAM:              ; { Name of the stream follows }
            VT_STORAGE:             ; { Name of the storage follows }
            VT_STREAMED_OBJECT:     ; { Stream contains an object }
            VT_STORED_OBJECT:       ; { Storage contains an object }
            VT_BLOB_OBJECT:         ; { Blob contains an object }
            VT_CF: ; { Clipboard format }
            VT_CLSID: ; { A Class ID }
            VT_VECTOR: ; { simple counted array }
            VT_ARRAY: ; { SAFEARRAY* }
            VT_BYREF: ;
            VT_RESERVED: ;
          end;
          {restore position}
          Stream.Position := prevPos;
        end;
      end;
    finally
      Stream.Free;
    end;
  end
end;

end.
