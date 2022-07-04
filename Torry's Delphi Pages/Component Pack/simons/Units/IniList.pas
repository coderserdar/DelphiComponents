unit IniList;

{ TIniList (C)opyright 2004 Version 1.06
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Klasse TIniList ist eine Stringliste, die um die Methoden
  von TIniFile erweitert wurde. Man hat somit eine Möglichkeit,
  Werte verschiedener Formate im Speicher zu halten und diese
  erst bei Bedarf in ein Inifile zu speichern oder aus einem
  Inifile zu laden.

  Die Klasse ist Public Domain, das Urheberrecht liegt aber
  beim Autor. }

interface

{$I SRDefine.inc}

uses Classes;

type
  TIniList = class(TObject)
  private
    FLines     : TStringList;
    FSeparator : char;
    FCaseSens,
    FDateFloat : boolean;
    FFileName,
    FTitle     : string;

    function GetTitle: string;
    procedure SetSeparator(NewValue: char);
    procedure SetTitle(NewValue: string);

  protected
    function AddKey(SectionIdx:integer;const Key, Value: string):integer;
    function AddSection(const Section: string):integer;
    function GetKeyFromLine(const Line:string):string;
    function GetKeyIndex(const Section, Key: string):integer;
    function GetKeyIndexInSection(var SectionIdx:integer;const Key: string):integer;
    function GetSectionIndex(const Section: string):integer;
    function GetValueFromLine(const Line:string):string;
    function ReadValue(const Section, Key: string):string;
    procedure WriteValue(const Section, Key: string; Value: string);

  public
    constructor Create;
    destructor Destroy; override;

    property CaseSensitive: boolean read FCaseSens write FCaseSens;
    procedure Clear;
    property DateTimeAsFloat: boolean read FDateFloat write FDateFloat;
    property IniFilename: string read FFileName write FFileName;
    property Separator: char read FSeparator write SetSeparator;
    property Title: string read GetTitle write SetTitle;

    procedure Assign(Source:TIniList);
    procedure CopyFrom(Source:TIniList; const Section: string);
    {$IFDEF SR_Delphi4_Up}
    function LoadFromFile:boolean; overload;
    function LoadFromFile(const FileName:string):boolean; overload;
    {$ELSE}
    function LoadFromFile:boolean;
    {$ENDIF}
    function LoadFromStream(AStream:TStream):boolean;
    procedure LoadFromStrings(AStrings:TStrings);
    {$IFDEF SR_Delphi4_Up}
    function SaveToFile:boolean; overload;
    function SaveToFile(const FileName:string):boolean; overload;
    {$ELSE}
    function SaveToFile:boolean;
    {$ENDIF}
    function SaveToStream(AStream: TStream):boolean;
    procedure SaveToStrings(AStrings:TStrings);

    procedure DeleteKey(const Section, Key: String);
    procedure EraseSection(const Section: string);
    function KeyCount(const Section: string):integer;
    function KeyExists(const Section,Key: string):boolean;
    function SectionCount:integer;
    function SectionExists(const Section: string):boolean;
    function ValueExists(const Section,Value: string):boolean;

    function ReadBool(const Section, Key: string; Default: Boolean): Boolean;
    function ReadDate(const Section, Key: string; Default: TDateTime): TDateTime;
    function ReadDateTime(const Section, Key: string; Default: TDateTime): TDateTime;
    function ReadFloat(const Section, Key: string; Default: Double): Double;
    function ReadInteger(const Section, Key: string; Default: Longint): Longint;
    function ReadString(const Section, Key, Default: string): string;
    function ReadTime(const Section, Key: string; Default: TDateTime): TDateTime;

    procedure ReadSection(const Section: string; AStrings: TStrings);
    procedure ReadSections(AStrings: TStrings);
    procedure ReadSectionKeys(const Section: string; AStrings: TStrings);
    procedure ReadSectionValues(const Section: string; AStrings: TStrings);

    procedure WriteBool(const Section, Key: string; Value: Boolean);
    procedure WriteDate(const Section, Key: string; Value: TDateTime);
    procedure WriteDateTime(const Section, Key: string; Value: TDateTime);
    procedure WriteFloat(const Section, Key: string; Value: Double);
    procedure WriteInteger(const Section, Key: string; Value: Longint);
    procedure WriteString(const Section, Key, Value: String);
    procedure WriteTime(const Section, Key: string; Value: TDateTime);

    procedure WriteSection(const Section: string; AStrings: TStrings);
  end;

implementation

uses SRUtils, SysUtils, Consts;

constructor TIniList.Create;
begin
  inherited Create;

  FSeparator:='=';
  FCaseSens:=false;
  FDateFloat:=false;
  FFileName:='';
  FTitle:='';

  FLines:=TStringList.Create;
  FLines.Add('; NoTitle');
end;

destructor TIniList.Destroy;
begin
  if assigned(FLines) then
    FLines.Free;

  inherited Destroy;
end; {Destroy}

function TIniList.AddKey(SectionIdx:integer;const Key, Value: string):integer;
var AIndex : integer;
begin
  AIndex:=GetKeyIndexInSection(SectionIdx, Key);
  if AIndex>=0 then begin
    Result:=AIndex;
    FLines[AIndex]:=Key+FSeparator+Value;
  end
  else begin
    Result:=SectionIdx;
    if Result>=FLines.Count then
      FLines.Add(Key+FSeparator+Value)
    else
      FLines.Insert(SectionIdx, Key+FSeparator+Value);
  end;
end; {AddKey}

function TIniList.AddSection(const Section: string):integer;
var AIndex : integer;
begin
  AIndex:=GetSectionIndex(Section);
  if AIndex>=0 then
    Result:=AIndex
  else begin
    FLines.Add('');
    Result:=FLines.Add('['+Section+']');
  end;
end; {AddSection}

procedure TIniList.Assign(Source:TIniList);
var AStream : TMemoryStream;
begin
  AStream:=TMemoryStream.Create;
  try
    Source.SaveToStream(AStream);
    AStream.Position:=0;
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end; {Assign}

procedure TIniList.CopyFrom(Source:TIniList; const Section: string);
var SectList,
    ASection : TStringList;
    SectNr   : integer;
begin
  SectList:=TStringList.Create;
  try
    ASection:=TStringList.Create;
    try
      Source.ReadSections(SectList);
      for SectNr:=0 to SectList.Count-1 do begin
        if (Section='') or ((Section<>'') and (LowerCase(SectList[SectNr])=LowerCase(Section))) then begin
          Source.ReadSection(SectList[SectNr], ASection);
          if ASection.Count>0 then
            WriteSection(SectList[SectNr], ASection);
        end;
      end;
    finally
      ASection.Free;
    end;
  finally
    SectList.Free;
  end;
end; {CopyFrom}

procedure TIniList.Clear;
begin
  FLines.Clear;
end; {Clear}

procedure TIniList.DeleteKey(const Section, Key: String);
var KeyIndex  : integer;
begin
  KeyIndex:=GetKeyIndex(Section, Key);
  if KeyIndex>=0 then begin
    FLines.Delete(KeyIndex);
    if KeyCount(Section)=0 then
      EraseSection(Section);
  end;
end; {DeleteKey}

procedure TIniList.EraseSection(const Section: string);
var SectIndex : integer;
    ALine     : string;
    Abort     : boolean;
begin
  SectIndex:=GetSectionIndex(Section);
  if SectIndex>=0 then begin
    if SectIndex>0 then begin
      ALine:=Trim(FLines[SectIndex-1]);
      if ALine='' then begin
        dec(SectIndex);
        FLines.Delete(SectIndex);
      end;
    end;
    ALine:='';
    Abort:=false;
    FLines.BeginUpdate;
    repeat
      FLines.Delete(SectIndex);
      if SectIndex<FLines.Count then
        ALine:=FLines[SectIndex]
      else
        Abort:=true;
    until Abort or (ALine='') or ((ALine<>'') and (ALine[1]='['));
    FLines.EndUpdate;
  end;
end; {EraseSection}

function TIniList.GetKeyFromLine(const Line:string):string;
var P : integer;
begin
  Result:='';
  P:=Pos(FSeparator, Line);
  if P>1 then
    Result:=copy(Line, 1, P-1);
end; {GetKeyFromLine}

function TIniList.GetKeyIndex(const Section, Key: string):integer;
var AIndex,
    ACount : integer;
    ALine,
    Key1,
    Key2   : string;
begin
  Result:=-1;
  ACount:=FLines.Count;
  AIndex:=GetSectionIndex(Section);
  if (ACount>0) and (AIndex>=0) then begin
    if FCaseSens then
      Key1:=Key
    else
      Key1:=ANSIUpperCase(Key);
    repeat
      inc(AIndex);
      if AIndex<ACount then begin
        ALine:=FLines[AIndex];
        if FCaseSens then
          Key2:=GetKeyFromLine(ALine)
        else
          Key2:=ANSIUpperCase(GetKeyFromLine(ALine));
        if Key1=Key2 then
          Result:=AIndex;
      end;
    until (Result>=0) or (AIndex>=ACount) or (ALine='')
     or ((ALine<>'') and (ALine[1]='['));
  end;
end; {GetKeyIndex}

function TIniList.GetKeyIndexInSection(var SectionIdx:integer;const Key: string):integer;
var AIndex,
    ACount : integer;
    ALine,
    Key1,
    Key2   : string;
begin
  Result:=-1;
  ACount:=FLines.Count;
  if (ACount>0) and (SectionIdx>=0) then begin
    if FCaseSens then
      Key1:=Key
    else
      Key1:=ANSIUpperCase(Key);
    AIndex:=SectionIdx;
    repeat
      inc(AIndex);
      if AIndex<ACount then begin
        ALine:=FLines[AIndex];
        if FCaseSens then
          Key2:=GetKeyFromLine(ALine)
        else
          Key2:=ANSIUpperCase(GetKeyFromLine(ALine));
        if Key1=Key2 then
          Result:=AIndex;
      end;
    until (Result>=0) or (AIndex>=ACount) or (ALine='')
     or ((ALine<>'') and (ALine[1]='['));
    SectionIdx:=AIndex;
  end;
end; {GetKeyIndexInSection}

function TIniList.GetSectionIndex(const Section: string):integer;
var AIndex,
    ACount : integer;
    ASect,
    ALine  : string;
begin
  Result:=-1;
  ACount:=FLines.Count;
  if ACount>0 then begin
    if FCaseSens then
      ASect:=Section
    else
      ASect:=ANSIUpperCase(Section);
    for AIndex:=0 to ACount-1 do begin
      if FCaseSens then
        ALine:=FLines[AIndex]
      else
        ALine:=ANSIUpperCase(FLines[AIndex]);
      if (ALine<>'') and (ALine[1]='[') and
       (copy(ALine, 2, length(ALine)-2)=ASect) then begin
        Result:=AIndex;
        Exit;
      end;
    end;
  end;
end; {GetSectionIndex}

function TIniList.GetTitle:string;
var AText : string;
begin
  Result:='';
  if FLines.Count>0 then begin
    AText:=FLines[0];
    if (AText<>'') and (AText[1]=';') then begin
      Result:=Trim(copy(AText, 2, length(AText)-1));
    end;
  end;
end; {GetTitle}

function TIniList.GetValueFromLine(const Line:string):string;
var P,L : integer;
begin
  Result:='';
  P:=Pos(FSeparator, Line);
  L:=length(Line);
  if (P>0) and (P<L) then
    Result:=copy(Line, P+1, L-P);
end; {GetValueFromLine}

function TIniList.KeyCount(const Section: string):integer;
var SectionIdx,
    KCount,
    AIndex,
    ACount     : integer;
    ALine      : string;
begin
  KCount:=0;
  ACount:=FLines.Count;
  SectionIdx:=GetSectionIndex(Section);
  if (ACount>0) and (SectionIdx>=0) then begin
    AIndex:=SectionIdx;
    repeat
      inc(AIndex);
      if AIndex<ACount then begin
        ALine:=Trim(FLines[AIndex]);
        if (ALine<>'') and (ALine[1]<>'[') and (ALine[1]<>';') then
          inc(KCount);
      end;
    until (AIndex>=ACount) or (ALine='')
     or ((ALine<>'') and (ALine[1]='['));
  end;
  Result:=KCount;
end; {KeyCount}

function TIniList.KeyExists(const Section,Key: string):boolean;
var SectionIdx : integer;
begin
  SectionIdx:=GetSectionIndex(Section);
  if SectionIdx>=0 then
    Result:=GetKeyIndexInSection(SectionIdx, Key)>=0
  else
    Result:=false;
end; {KeyExists}

function TIniList.LoadFromFile:boolean;
begin
  Result:=false;
  if FFileName<>'' then begin
    try
      FLines.LoadFromFile(FFileName);
      Result:=true;
    except
      Raise;
    end;
  end;
end; {LoadFromFile}

{$IFDEF SR_Delphi4_Up}
function TIniList.LoadFromFile(const FileName:string):boolean;
begin
  Result:=false;
  if FileName<>'' then begin
    try
      FLines.LoadFromFile(FileName);
      Result:=true;
    except
      Raise;
    end;
  end;
end; {LoadFromFile}
{$ENDIF}

function TIniList.LoadFromStream(AStream: TStream):boolean;
begin
  Result:=false;
  if assigned(AStream) then begin
    try
      FLines.LoadFromStream(AStream);
      Result:=true;
    except
      Raise;
    end;
  end;
end; {LoadFromStream}

procedure TIniList.LoadFromStrings(AStrings:TStrings);
begin
  FLines.Assign(AStrings);
end; {LoadFromStrings}

function TIniList.ReadBool(const Section, Key: string; Default: Boolean): Boolean;
var AText : string;
begin
  Result:=Default;
  AText:=ReadValue(Section, Key);
  if AText='0' then
    Result:=false;
  if AText='1' then
    Result:=true;
end; {ReadBool}

function TIniList.ReadDate(const Section, Key: string; Default: TDateTime): TDateTime;
var AText : string;
begin
  AText:=ReadValue(Section, Key);
  if FDateFloat then
    Result:=StrToFloatDef(AText, Default)
  else
    Result:=StrToDateDef(AText, Default);
end; {ReadDate}

function TIniList.ReadDateTime(const Section, Key: string; Default: TDateTime): TDateTime;
var AText : string;
begin
  AText:=ReadValue(Section, Key);
  if FDateFloat then
    Result:=StrToFloatDef(AText, Default)
  else
    Result:=StrToDateTimeDef(AText, Default);
end; {ReadDateTime}

function TIniList.ReadFloat(const Section, Key: string; Default: Double): Double;
var AText : string;
begin
  AText:=ReadValue(Section, Key);
  Result:=StrToFloatDef(AText, Default);
end; {ReadFloat}

function TIniList.ReadInteger(const Section, Key: string; Default: Longint): Longint;
var AText : string;
begin
  AText:=ReadValue(Section, Key);
  Result:=StrToIntDef(AText, Default);
end; {ReadInteger}

function TIniList.ReadString(const Section, Key, Default: string): string;
var AText : string;
begin
  AText:=ReadValue(Section, Key);
  if AText='' then
    Result:=Default
  else
    Result:=AText;
end; {ReadString}

function TIniList.ReadTime(const Section, Key: string; Default: TDateTime): TDateTime;
var AText : string;
begin
  AText:=ReadValue(Section, Key);
  if FDateFloat then
    Result:=StrToFloatDef(AText, Default)
  else
    Result:=StrToTimeDef(AText, Default);
end; {ReadTime}

procedure TIniList.ReadSection(const Section: string; AStrings: TStrings);
var SectionIdx,
    AIndex,
    ACount     : integer;
    ALine      : string;
begin
  ACount:=FLines.Count;
  SectionIdx:=GetSectionIndex(Section);
  if (ACount>0) and (SectionIdx>=0) and assigned(AStrings) then begin
    AStrings.BeginUpdate;
    AStrings.Clear;
    AIndex:=SectionIdx;
    repeat
      inc(AIndex);
      if AIndex<ACount then begin
        ALine:=Trim(FLines[AIndex]);
        if (ALine<>'') and (ALine[1]<>'[') and (ALine[1]<>';') then
          AStrings.Add(ALine);
      end;
    until (AIndex>=ACount) or (ALine='')
     or ((ALine<>'') and (ALine[1]='['));
    AStrings.EndUpdate;
  end;
end; {ReadSection}

procedure TIniList.ReadSections(AStrings: TStrings);
var AIndex,
    ACount : integer;
    ALine  : string;
begin
  ACount:=FLines.Count;
  if (ACount>0) and assigned(AStrings) then begin
    AStrings.BeginUpdate;
    AStrings.Clear;
    for AIndex:=0 to ACount-1 do begin
      ALine:=Trim(FLines[AIndex]);
      if (ALine<>'') and (ALine[1]='[') and (ALine[length(ALine)]=']') then
        AStrings.Add(copy(ALine, 2, length(ALine)-2));
    end;
    AStrings.EndUpdate;
  end;
end; {ReadSections}

procedure TIniList.ReadSectionKeys(const Section: string; AStrings: TStrings);
var SectionIdx,
    AIndex,
    ACount     : integer;
    ALine      : string;
begin
  ACount:=FLines.Count;
  SectionIdx:=GetSectionIndex(Section);
  if (ACount>0) and (SectionIdx>=0) and assigned(AStrings) then begin
    AStrings.BeginUpdate;
    AStrings.Clear;
    AIndex:=SectionIdx;
    repeat
      inc(AIndex);
      if AIndex<ACount then begin
        ALine:=Trim(FLines[AIndex]);
        if (ALine<>'') and (ALine[1]<>'[') and (ALine[1]<>';') then
          AStrings.Add(GetKeyFromLine(ALine));
      end;
    until (AIndex>=ACount) or (ALine='')
     or ((ALine<>'') and (ALine[1]='['));
    AStrings.EndUpdate;
  end;
end; {ReadSectionKeys}

procedure TIniList.ReadSectionValues(const Section: string; AStrings: TStrings);
var SectionIdx,
    AIndex,
    ACount     : integer;
    ALine      : string;
begin
  ACount:=FLines.Count;
  SectionIdx:=GetSectionIndex(Section);
  if (ACount>0) and (SectionIdx>=0) and assigned(AStrings) then begin
    AStrings.BeginUpdate;
    AStrings.Clear;
    AIndex:=SectionIdx;
    repeat
      inc(AIndex);
      if AIndex<ACount then begin
        ALine:=Trim(FLines[AIndex]);
        if (ALine<>'') and (ALine[1]<>'[') and (ALine[1]<>';') then
          AStrings.Add(GetValueFromLine(ALine));
      end;
    until (AIndex>=ACount) or (ALine='')
     or ((ALine<>'') and (ALine[1]='['));
    AStrings.EndUpdate;
  end;
end; {ReadSectionValues}

function TIniList.ReadValue(const Section, Key: string):string;
var KeyIndex  : integer;
begin
  Result:='';
  KeyIndex:=GetKeyIndex(Section, Key);
  if KeyIndex>=0 then
    Result:=GetValueFromLine(FLines[KeyIndex]);
end; {ReadValue}

function TIniList.SaveToFile:boolean;
begin
  Result:=false;
  if FFileName<>'' then begin
    try
      FLines.SaveToFile(FFileName);
      Result:=true;
    except
      Raise;
    end;
  end;
end; {SaveToFile}

{$IFDEF SR_Delphi4_Up}
function TIniList.SaveToFile(const FileName:string):boolean;
begin
  Result:=false;
  if FileName<>'' then begin
    try
      FLines.SaveToFile(FileName);
      Result:=true;
    except
      Raise;
    end;
  end;
end; {SaveToFile}
{$ENDIF}

function TIniList.SaveToStream(AStream: TStream):boolean;
begin
  Result:=false;
  if assigned(AStream) then begin
    try
      FLines.SaveToStream(AStream);
      Result:=true;
    except
      Raise;
    end;
  end;
end; {SaveToStream}

procedure TIniList.SaveToStrings(AStrings:TStrings);
begin
  AStrings.Assign(FLines);
end; {SaveToStrings}

function TIniList.SectionCount:integer;
var AIndex,
    ACount,
    SCount : integer;
    ALine  : string;
begin
  SCount:=0;
  ACount:=FLines.Count;
  if ACount>0 then begin
    for AIndex:=0 to ACount-1 do begin
      ALine:=Trim(FLines[AIndex]);
      if (ALine<>'') and (ALine[1]='[') and (ALine[length(ALine)]=']') then
        inc(SCount);
    end;
  end;
  Result:=SCount;
end; {SectionCount}

function TIniList.SectionExists(const Section: string):boolean;
begin
  Result:=GetSectionIndex(Section)>=0;
end; {SectionExists}

procedure TIniList.SetSeparator(NewValue: char);
var ACount,
    i,P      : integer;
    ALine    : string;
begin
  if NewValue<>FSeparator then begin
    ACount:=FLines.Count;
    if ACount>0 then begin
      for i:=0 to ACount-1 do begin
        ALine:=FLines[i];
        if (ALine<>'') and (ALine[1]<>';') and (Pos('[', ALine)=0) then begin
          P:=Pos(FSeparator, ALine);
          if P>0 then begin
            ALine[P]:=NewValue;
            FLines[i]:=ALine;
          end;
        end;
      end;
    end;
    FSeparator:=NewValue;
  end;
end; {SetSeparator}

procedure TIniList.SetTitle(NewValue: string);
var ALine : string;
begin
  FTitle:=NewValue;
  if NewValue='' then
    NewValue:='NoTitle';
  if FLines.Count>0 then begin
    ALine:=FLines[0];
    if (ALine<>'') and (ALine[1]=';') then begin
      ALine:='; '+NewValue;
      FLines[0]:=ALine;
    end
    else begin
      FLines.Insert(0, '');
      FLines.Insert(0, '; '+NewValue);
    end;
  end
  else begin
    FLines.Add('; '+NewValue);
    FLines.Add('');
  end;
end; {SetTitle}

function TIniList.ValueExists(const Section,Value: string):boolean;
var AList : TStringList;
    Buf   : string;
    i     : integer;
begin
  Result:=false;
  AList:=TStringList.Create;
  try
    ReadSectionValues(Section, AList);
    if not FCaseSens then
      Buf:=ANSIUpperCase(Value);
    i:=0;
    while not Result and (i<AList.Count) do begin
      if (FCaseSens and (Value=AList[i])) or
       (not FCaseSens and (Buf=ANSIUpperCase(AList[i]))) then
        Result:=true;
      inc(i);
    end;
  finally
    AList.Free;
  end;
end; {ValueExists}

procedure TIniList.WriteBool(const Section, Key: string; Value: Boolean);
begin
  WriteValue(Section, Key, IntToStr(ord(Value)));
end; {WriteBool}

procedure TIniList.WriteDate(const Section, Key: string; Value: TDateTime);
var AText : string;
begin
  if FDateFloat then
    AText:=FloatToStr(Value)
  else
    AText:=DateToStrDef(Value, '', false);
  if AText<>'' then
    WriteValue(Section, Key, AText);
end; {WriteDate}

procedure TIniList.WriteDateTime(const Section, Key: string; Value: TDateTime);
var AText : string;
begin
  if FDateFloat then
    AText:=FloatToStr(Value)
  else
    AText:=DateTimeToStrDef(Value, '', false);
  if AText<>'' then
    WriteValue(Section, Key, AText);
end; {WriteDateTime}

procedure TIniList.WriteFloat(const Section, Key: string; Value: Double);
begin
  WriteValue(Section, Key, FloatToStr(Value));
end; {WriteFloat}

procedure TIniList.WriteInteger(const Section, Key: string; Value: Longint);
begin
  WriteValue(Section, Key, IntToStr(Value));
end; {WriteInteger}

procedure TIniList.WriteSection(const Section: string; AStrings: TStrings);
var Key,
    Value   : string;
    KeyNr,P : integer;
begin
  for KeyNr:=0 to AStrings.Count-1 do begin
    P:=Pos(FSeparator, AStrings[KeyNr]);
    if P>1 then begin
      Key:=copy(AStrings[KeyNr], 1, P-1);
      Value:=copy(AStrings[KeyNr], P+1, length(AStrings[KeyNr])-P);
      WriteValue(Section, Key, Value);
    end;
  end;
end; {WriteSection}

procedure TIniList.WriteString(const Section, Key, Value: String);
begin
  WriteValue(Section, Key, Value);
end; {WriteString}

procedure TIniList.WriteTime(const Section, Key: string; Value: TDateTime);
var AText : string;
begin
  if FDateFloat then
    AText:=FloatToStr(Value)
  else
    AText:=TimeToStrDef(Value, '');
  if AText<>'' then
    WriteValue(Section, Key, AText);
end; {WriteTime}

procedure TIniList.WriteValue(const Section, Key: string; Value: string);
var SectIdx : integer;
begin
  SectIdx:=AddSection(Section);
  if SectIdx>=0 then
    AddKey(SectIdx, Key, Value);
end; {WriteBool}

end.
