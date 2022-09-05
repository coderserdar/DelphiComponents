{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDNS;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�DNS ��������Ԫ
* ��Ԫ���ߣ�CnPack������ Liu Xiao
* ��    ע��
* ����ƽ̨��PWin7 + Delphi 5
* ���ݲ��ԣ�PWinXP/7 + Delphi 2009 ~
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.03.04 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Contnrs,
  CnClasses, CnConsts, CnNetConsts, CnIP, CnUDP, CnNetDecls;

type
{$IFNDEF TBYTES_DEFINED}
  TBytes = array of Byte;
{$ENDIF}

  ECnDNSException = class(Exception);

  TCnDNSQuestion = class
  {* ����һ�� DNS ���е� Question ���͵ļ�¼}
  private
    FQName: string;
    FQClass: Word;
    FQType: Word;
  public
    procedure DumpToStrings(List: TStrings);
    property QName: string read FQName write FQName;
    property QType: Word read FQType write FQType;
    property QClass: Word read FQClass write FQClass;
  end;

  TCnDNSResourceRecord = class
  {* ����һ�� DNS ���е� Resource Record ���͵ļ�¼}
  private
    FRDLength: Word;
    FTTL: LongWord;
    FRName: string;
    FRType: Word;
    FRClass: Word;
    FIP: LongWord;
    FRDString: string;
  public
    procedure DumpToStrings(List: TStrings);

    property RName: string read FRName write FRName;
    property RType: Word read FRType write FRType;
    property RClass: Word read FRClass write FRClass;
    property TTL: LongWord read FTTL write FTTL;
    property RDLength: Word read FRDLength write FRDLength;

    property IP: LongWord read FIP write FIP;
    property RDString: string read FRDString write FRDString;
  end;

  TCnDNSPacketObject = class
  {* ��ʾһ�� DNS �������ݣ����Գ��ж�� TCnDNSQuestion �� TCnDNSResourceRecord ʵ��}
  private
    FQDCount: Integer;
    FANCount: Integer;
    FNSCount: Integer;
    FARCount: Integer;
    FIsResponse: Boolean;
    FIsQuery: Boolean;
    FQDList: TObjectList;
    FANList: TObjectList;
    FNSList: TObjectList;
    FARList: TObjectList;
    FQR: Integer;
    FId: Word;
    FAA: Boolean;
    FTC: Boolean;
    FRA: Boolean;
    FRD: Boolean;
    FRCode: Integer;
    FOpCode: Integer;
    function GetAN(Index: Integer): TCnDNSResourceRecord;
    function GetAR(Index: Integer): TCnDNSResourceRecord;
    function GetNS(Index: Integer): TCnDNSResourceRecord;
    function GetQD(Index: Integer): TCnDNSQuestion;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddQuestion: TCnDNSQuestion;
    function AddAnswer: TCnDNSResourceRecord;
    function AddNameServer: TCnDNSResourceRecord;
    function AddAdditionalRecord: TCnDNSResourceRecord;

    procedure DumpToStrings(List: TStrings);

    property Id: Word read FId write FId;
    property QR: Integer read FQR write FQR;
    property OpCode: Integer read FOpCode write FOpCode;
    property AA: Boolean read FAA write FAA;
    property TC: Boolean read FTC write FTC;
    property RD: Boolean read FRD write FRD;
    property RA: Boolean read FRA write FRA;
    property RCode: Integer read FRCode write FRCode;

    property QD[Index: Integer]: TCnDNSQuestion read GetQD;
    property AN[Index: Integer]: TCnDNSResourceRecord read GetAN;
    property NS[Index: Integer]: TCnDNSResourceRecord read GetNS;
    property AR[Index: Integer]: TCnDNSResourceRecord read GetAR;

    property IsQuery: Boolean read FIsQuery write FIsQuery;
    property IsResponse: Boolean read FIsResponse write FIsResponse;
    property QDCount: Integer read FQDCount write FQDCount;
    property ANCount: Integer read FANCount write FANCount;
    property NSCount: Integer read FNSCount write FNSCount;
    property ARCount: Integer read FARCount write FARCount;
  end;

  TCnDNSResponseEvent = procedure(Sender: TObject; Response: TCnDNSPacketObject) of object;

  TCnDNS = class(TCnComponent)
  {* DNS �շ������}
  private
    FUDP: TCnUDP;
    FNameServerPort: Integer;
    FNameServerIP: string;
    FOnResponse: TCnDNSResponseEvent;
    procedure SetNameServerIP(const Value: string);
    procedure SetNameServerPort(const Value: Integer);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
    procedure UDPDataReceived(Sender: TComponent; Buffer: Pointer;
      Len: Integer; FromIP: string; Port: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function BuildDNSQueryPacket(const Name: string; RandId: Word;
      QueryType: Word = CN_DNS_TYPE_A; QueryClass: Word = CN_DNS_CLASS_IN): TBytes;
    {* ����һ�򵥵ĵ�һ������ѯ����}

    class function ParseIndexedString(var StrResult: string; Base, StrData: PAnsiChar;
      MaxLen: Integer = 0): Integer;
    {* ����ͨ���������ַ������ȴ������������ݣ���ʾ�� StrData ��ʼɨ�裬��󲻳��� StrData + MaxLen
      Base Ϊ�� DNS ���ݰ���ʼ��ַ����� MaxLen Ϊ 0�����ʾ�� #0 ��β��
      ����ֵΪ����ǰ���ĳ��ȣ����ν������ƴ�� StrResult ����}

    class function BuildNameBuffer(const Name: string; Buf: PAnsiChar): PAnsiChar;
    {* ����������ɷֵ�Ų����ϳ����ֽڵĸ�ʽ��Buf ��ָ�Ļ�����Ӧ�������� Length(Name) + 2 ���ֽڳ��ȣ�
      ���� Buf ���� Name ������ #0 ������֮��ĵ�ַ���������߷���������}

    class function ParseDNSResponsePacket(const Response: PAnsiChar; ResponseByteLen: Integer;
      Packet: TCnDNSPacketObject): Boolean;
    {* ������Ӧ���������ݷ��� Packet ����}

    procedure SendHostQuery(const Name: string; QueryType: Word = CN_DNS_TYPE_A;
      QueryClass: Word = CN_DNS_CLASS_IN; ID: Word = 0);
    {* ����һ�򵥵�������ѯ����ID ���Ϊ 0 ���ڲ�����}

  published
    property NameServerIP: string read FNameServerIP write SetNameServerIP;
    {* DNS ������ IP}
    property NameServerPort: Integer read FNameServerPort write SetNameServerPort;
    {* DNS �������˿ڣ�Ĭ�� 53}
    property OnResponse: TCnDNSResponseEvent read FOnResponse write FOnResponse;
    {* �յ� DNS ��Ӧ��ʱ�������¼�}
  end;

implementation

type
  TCnUDPHack = class(TCnUDP);

{ TCnDNSPacket }

function TCnDNSPacketObject.AddAdditionalRecord: TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord.Create;
  FARList.Add(Result);
end;

function TCnDNSPacketObject.AddAnswer: TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord.Create;
  FANList.Add(Result);
end;

function TCnDNSPacketObject.AddNameServer: TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord.Create;
  FNSList.Add(Result);
end;

function TCnDNSPacketObject.AddQuestion: TCnDNSQuestion;
begin
  Result := TCnDNSQuestion.Create;
  FQDList.Add(Result);
end;

constructor TCnDNSPacketObject.Create;
begin
  inherited;
  FQDList := TObjectList.Create;
  FANList := TObjectList.Create;
  FNSList := TObjectList.Create;
  FARList := TObjectList.Create;
end;

destructor TCnDNSPacketObject.Destroy;
begin
  FARList.Free;
  FNSList.Free;
  FANList.Free;
  FQDList.FRee;
  inherited;
end;

procedure TCnDNSPacketObject.DumpToStrings(List: TStrings);
var
  Q: string;
  I: Integer;
begin
  if IsQuery then
    Q := 'Query'
  else if IsResponse then
    Q := 'Response'
  else
    Q := '';

  List.Add(Format('DNS Packet %s. Id %d, OpCode %d, RCode %d.', [Q, FId, FOpCode, FRCode]));
  List.Add(Format('AA %d, TC %d, RD %d, RA %d, QNCount %d, ANCount %d, NSCount %d, ARCount %d.',
    [Integer(FAA), Integer(FTC), Integer(FRD), Integer(FRA),
    FQDCount, FANCount, FNSCount, FARCount]));

  for I := 0 to FQDCount - 1 do
  begin
    List.Add(Format('Query #%d:', [I + 1]));
    QD[I].DumpToStrings(List);
  end;

  for I := 0 to FANCount - 1 do
  begin
    List.Add(Format('Answer #%d:', [I + 1]));
    AN[I].DumpToStrings(List);
  end;

  for I := 0 to FNSCount - 1 do
  begin
    List.Add(Format('Nameserver #%d:', [I + 1]));
    NS[I].DumpToStrings(List);
  end;

  for I := 0 to FARCount - 1 do
  begin
    List.Add(Format('Additional Resource #%d:', [I + 1]));
    AR[I].DumpToStrings(List);
  end;
end;

function TCnDNSPacketObject.GetAN(Index: Integer): TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord(FANList[Index]);
end;

function TCnDNSPacketObject.GetAR(Index: Integer): TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord(FARList[Index]);
end;

function TCnDNSPacketObject.GetNS(Index: Integer): TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord(FNSList[Index]);
end;

function TCnDNSPacketObject.GetQD(Index: Integer): TCnDNSQuestion;
begin
  Result := TCnDNSQuestion(FQDList[Index]);
end;

{ TCnDNS }

constructor TCnDNS.Create(AOwner: TComponent);
begin
  inherited;
  FNameServerPort := 53;
  FUDP := TCnUDP.Create(Self);
  FUDP.RemotePort := FNameServerPort;
  FUDP.OnDataReceived := UDPDataReceived;

  if not (csDesigning in ComponentState) then
    TCnUDPHack(FUDP).UpdateBinding;
end;

destructor TCnDNS.Destroy;
begin
  FUDP.Free;
  inherited;
end;

procedure TCnDNS.SetNameServerIP(const Value: string);
begin
  FNameServerIP := Value;
  FUDP.RemoteHost := Value;
end;

procedure TCnDNS.SetNameServerPort(const Value: Integer);
begin
  FNameServerPort := Value;
  FUDP.RemotePort := Value;
end;

class function TCnDNS.BuildNameBuffer(const Name: string; Buf: PAnsiChar): PAnsiChar;
var
  P: PChar;
  Q: PAnsiChar;
  Len: Byte;
begin
  Result := Buf;
  if (Length(Name) <= 1) or (Length(Name) > 63) then
    Exit;

  P := @Name[1];
  Q := Buf;
  Inc(Q);

  while P ^ <> #0 do
  begin
    Len := 0;
    while (P^ <> '.') and (P^ <> #0) do
    begin
      Q^ := AnsiChar(P^);
      Inc(Len);
      Inc(P);
      Inc(Q);
    end;
    if P^ = #0 then
    begin
      Q^ := #0;
      Buf^ := AnsiChar(Chr(Len));
      Inc(Buf, Len + 2); // Buf ָ�� #0 �ĺ�һ��λ��
      Result := Buf;
      Exit;
    end
    else // ��������
    begin
      Buf^ := AnsiChar(Chr(Len));  // дǰ��ĳ���
      Inc(Buf, Len + 1); // Buf ָ���¸���
    end;
    Inc(P);
    Inc(Q);
  end;
end;

class function TCnDNS.BuildDNSQueryPacket(const Name: string; RandId: Word; QueryType: Word;
  QueryClass: Word): TBytes;
var
  Head: PCnDNSHeader;
  P: PAnsiChar;
  PW: PWORD;
begin
  Result := nil;
  if (Length(Name) <= 1) or (Length(Name) > 63) then
    Exit;

  SetLength(Result, SizeOf(TCnDNSHeader) + Length(Name) + SizeOf(Byte) + 2 * SizeOf(Word));
  Head := PCnDNSHeader(@Result[0]);
  CnSetDNSHeaderId(Head, RandId);  // ��ѯ ID ��
  CnSetDNSHeaderQR(Head, True);    // �ǲ�ѯ����
  CnSetDNSHeaderRD(Head, True);    // �������ݹ�����
  CnSetDNSHeaderQDCount(Head, 1);  // ��һ����ѯ

  P := PAnsiChar(@Head^.SectionData[0]);
  PW := PWORD(BuildNameBuffer(Name, P));
  if PW = nil then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  PW^ := CnHostToNetworkWord(QueryType);
  Inc(PW);
  PW^ := CnHostToNetworkWord(QueryClass);
end;

class function TCnDNS.ParseIndexedString(var StrResult: string; Base, StrData: PAnsiChar; MaxLen: Integer): Integer;
var
  PB: PByte;
  B: Byte;
  Idx, Len: Integer;
  Str: AnsiString;
  First: Boolean;
begin
  if MaxLen > 0 then // �г������ƣ�һ������ RDLength/RData ����
  begin
    PB := PByte(StrData);
    Result := 0;
    while True do
    begin
      if PB^ = 0 then // ��β���ˣ��˳�
      begin
        Inc(Result);  // #0 �������ַ�������ģ�����Ҫ����ȥ
        Exit;
      end
      else if (PB^ and $C0) = $C0 then // ����λ��Ϊ 1����ʾ�����������ַ���
      begin
        B := PB^;
        Inc(PB);
        Inc(Result);
        if Result >= MaxLen then
          Exit;

        Idx := (Word(B and $3F) shl 8) or Word(PB^);
        ParseIndexedString(StrResult, Base, Base + Idx);
        Inc(PB);
        Inc(Result);         // ָ����һ�� 2 �ֽ�
        if Result >= MaxLen then
          Exit;
      end
      else if (PB^ and $C0) = 0 then // ����λ��Ϊ 0�����ַ�������
      begin
        Len := PB^;              // �õ����ֳ���
        SetLength(Str, Len + 1); // ǰ��ĵ�Ҳ����
        Str[1] := '.';           // д��

        Inc(PB);                 // PB ָ�����ַ�����Len Ϊ����
        Inc(Result);
        if Result >= MaxLen then
          raise ECnDNSException.Create(SCnDNSTooLong);

        CopyMemory(@Str[2], PB, Len); // Str ������Ϊ .xxxxx ����

        Inc(PB, Len);            // PB ָ����һ�����Ȼ�����λ��
        Inc(Result, Len);

        StrResult := StrResult + string(Str);
        SetLength(Str, 0);

        if Result >= MaxLen then
          Exit;
      end
      else
        raise ECnDNSException.CreateFmt(SCnDNSInvalidHeadByteFmt, [PB^, PB - Base]);
    end;
  end
  else // �޳������ƣ����ܾ�һ���������޽������������ַ�����ͷ������������������ #0 ����
  begin
    PB := PByte(StrData);
    Result := 0;
    First := True;

    while True do
    begin
      if PB^ = 0 then // ��β���ˣ��˳�
      begin
        Inc(Result);  // #0 �������ַ�������ģ�����Ҫ����ȥ
        Exit;
      end
      else if (PB^ and $C0) = $C0 then // ����λ��Ϊ 1����ʾ�����������ַ���
      begin
        B := PB^;
        Inc(PB);
        Inc(Result);

        Idx := (Word(B and $3F) shl 8) or Word(PB^);
        ParseIndexedString(StrResult, Base, Base + Idx);
        Inc(PB);
        Inc(Result);         // ָ����һ�� 2 �ֽ�

        if First then // �����һ�������ľ���������˵����һ��
          Exit;
      end
      else if (PB^ and $C0) = 0 then // ����λ��Ϊ 0�����ַ�������
      begin
        First := False;

        Len := PB^;              // �õ����ֳ���
        SetLength(Str, Len + 1); // ǰ��ĵ�Ҳ����
        Str[1] := '.';           // д��

        Inc(PB);                 // PB ָ�����ַ�����Len Ϊ����
        Inc(Result);
        CopyMemory(@Str[2], PB, Len); // Str ������Ϊ .xxxxx ����

        Inc(PB, Len);            // PB ָ����һ�����Ȼ�����λ��
        Inc(Result, Len);

        StrResult := StrResult + string(Str);
        SetLength(Str, 0);
      end
      else
        raise ECnDNSException.CreateFmt(SCnDNSInvalidHeadByteFmt, [PB^, PB - Base]);
    end;
  end;
end;

class function TCnDNS.ParseDNSResponsePacket(const Response: PAnsiChar;
  ResponseByteLen: Integer; Packet: TCnDNSPacketObject): Boolean;
var
  I: Integer;
  Head: PCnDNSHeader;
  Data: PAnsiChar;
  Q: TCnDNSQuestion;
  R: TCnDNSResourceRecord;

  // ����һ�� Question
  function ParseQuestion(QuestionData: PAnsiChar; Question: TCnDNSQuestion): PAnsiChar;
  var
    H: PCnDNSQuestionSectionAfterName;
    S: string;
    Len: Integer;
  begin
    Result := QuestionData;
    if (QuestionData <> nil) and (Question <> nil) then
    begin
      Len := ParseIndexedString(S, Response, QuestionData);
      Question.QName := S;
      Inc(QuestionData, Len);

      H := PCnDNSQuestionSectionAfterName(QuestionData);
      Question.QType := CnNetworkToHostWord(H^.QType);
      Question.QClass := CnNetworkToHostWord(H^.QClass);
      Result := QuestionData + SizeOf(TCnDNSQuestionSectionAfterName);
    end;
  end;

  // ����һ�� Resource Record
  function ParseResourceRecord(ResourceRecordData: PAnsiChar; Resource: TCnDNSResourceRecord): PAnsiChar;
  var
    H: PCnDNSResourceRecordAfterName;
    S: string;
    Len: Integer;
  begin
    Result := ResourceRecordData;
    if (ResourceRecordData <> nil) and (Resource <> nil) then
    begin
      S := '';
      Len := ParseIndexedString(S, Response, ResourceRecordData);
      Resource.RName := S;
      Inc(ResourceRecordData, Len);

      H := PCnDNSResourceRecordAfterName(ResourceRecordData);
      Resource.RType := CnNetworkToHostWord(H^.RType);
      Resource.RClass := CnNetworkToHostWord(H^.RClass);
      Resource.TTL := CnNetworkToHostLongWord(H^.TTL);
      Resource.RDLength := CnNetworkToHostWord(H^.RDLength);
      if Resource.RDLength = SizeOf(LongWord) then
      begin
        // 4 �ֽڵ�Ӧ�������� IP ��ַ
        Resource.IP := CnNetworkToHostLongWord((PDWORD(@H^.RData[0]))^);
      end
      else
      begin
        // �������ȵ����ַ�������
        S := '';
        ParseIndexedString(S, Response, @H^.RData[0], Resource.RDLength);
        Resource.RDString := S;
      end;

      Result := ResourceRecordData + SizeOf(TCnDNSResourceRecordAfterName) + Resource.RDLength - 1;
      // ��һ����Ϊ TCnDNSResourceRecordAfterName ����Ѿ��и� 1 �ֽڵ� RData ����
    end;
  end;

begin
  Result := False;
  if (Response = nil) or (ResponseByteLen < SizeOf(TCnDNSHeader)) or (Packet = nil) then
    Exit;

  Head := PCnDNSHeader(Response);
  Packet.Id := CnGetDNSHeaderId(Head);
  Packet.IsQuery := CnGetDNSHeaderQR(Head) = CN_DNS_HEADER_TYPE_QUERY;
  Packet.IsResponse := CnGetDNSHeaderQR(Head) = CN_DNS_HEADER_TYPE_RESPONSE;
  Packet.OpCode := CnGetDNSHeaderOpCode(Head);
  Packet.AA := CnGetDNSHeaderAA(Head);
  Packet.TC := CnGetDNSHeaderTC(Head);
  Packet.RA := CnGetDNSHeaderRA(Head);
  Packet.RD := CnGetDNSHeaderRD(Head);
  Packet.RCode := CnGetDNSHeaderRCode(Head);

  Packet.QDCount := CnGetDNSHeaderQDCount(Head);
  Packet.ANCount := CnGetDNSHeaderANCount(Head);
  Packet.NSCount := CnGetDNSHeaderNSCount(Head);
  Packet.ARCount := CnGetDNSHeaderARCount(Head);

  // ������ͷ�󲿵Ŀɱ䲿�֣��Ȱ� Question ���� QD���ٰ� Resource Record ����������
  Data := PAnsiChar(@Head^.SectionData[0]);
  I := 1;
  while I <= Packet.QDCount do
  begin
    // ���� QD ��� Question ��
    Q := Packet.AddQuestion;
    Data := ParseQuestion(Data, Q);
    Inc(I);
  end;

  I := 1;
  while I <= Packet.ANCount do
  begin
    // ���� AN ��� Resource Record ��
    R := Packet.AddAnswer;
    Data := ParseResourceRecord(Data, R);
    Inc(I);
  end;

  I := 1;
  while I <= Packet.NSCount do
  begin
    // ���� NS ��� Resource Record ��
    R := Packet.AddNameServer;
    Data := ParseResourceRecord(Data, R);
    Inc(I);
  end;

  I := 1;
  while I <= Packet.ARCount do
  begin
    // ���� AR ��� Resource Record ��
    R := Packet.AddAdditionalRecord;
    Data := ParseResourceRecord(Data, R);
    Inc(I);
  end;
end;

procedure TCnDNS.Loaded;
begin
  inherited;
  TCnUDPHack(FUDP).UpdateBinding;
end;

procedure TCnDNS.SendHostQuery(const Name: string; QueryType, QueryClass,
  ID: Word);
var
  Buf: TBytes;
begin
  if ID = 0 then
  begin
    Randomize;
    ID := Trunc(Random * 65535);
  end;

  Buf := TCnDNS.BuildDNSQueryPacket(Name, ID, QueryType, QueryClass);
  FUDP.SendBuffer(@Buf[0], Length(Buf));
  SetLength(Buf, 0);
end;

procedure TCnDNS.UDPDataReceived(Sender: TComponent; Buffer: Pointer;
  Len: Integer; FromIP: string; Port: Integer);
var
  Packet: TCnDNSPacketObject;
begin
  if Assigned(FOnResponse) then
  begin
    Packet := TCnDNSPacketObject.Create;
    try
      ParseDNSResponsePacket(PAnsiChar(Buffer), Len, Packet);
      FOnResponse(Self, Packet);
    finally
      Packet.Free;
    end;
  end;
end;

procedure TCnDNS.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnDNSName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnDNSComment;
end;

{ TCnDNSResourceRecord }

procedure TCnDNSResourceRecord.DumpToStrings(List: TStrings);
begin
  if FRDString = '' then
    List.Add(Format('RType %d, RClass %d, RName: %s, RDLength %d, TTL %d, IP: %s',
      [FRType, FRClass, FRName, FRDLength, FTTL, TCnIp.IntToIP(FIP)]))
  else
    List.Add(Format('RType %d, RClass %d, RName: %s, RDLength %d, TTL %d, RDString: %s',
      [FRType, FRClass, FRName, FRDLength, FTTL, FRDString]));
end;

{ TCnDNSQuestion }

procedure TCnDNSQuestion.DumpToStrings(List: TStrings);
begin
  List.Add(Format('QType %d, QClass %d, QName: %s', [FQType, FQClass, FQName]));
end;

end.
