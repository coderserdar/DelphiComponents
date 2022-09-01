{+--------------------------------------------------------------------------+
 | Unit:        mwEPTokenList
 | Created:     2.98
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: Enhanced Pascal token list
 | Version:     1.1
 | Status:      FreeWare
 | DISCLAIMER:  This is provided as is, expressly without a warranty of any kind.
 |              You use it at your own risc.
 +--------------------------------------------------------------------------+}

unit mwEPTokenList;

interface

uses
  Windows, SysUtils, ComCtrls, mwPasTokenList;

type
  TmInfoKind = (ikAutomated, ikClass, ikClEmpty, ikClFunction, ikClForward,
    ikClProcedure, ikClReference, ikCompDirect, ikConst, ikConstructor,
    ikDestructor, ikError, ikField, ikFunction, ikNull, ikGUID, ikImplementation,
    ikInterface, ikIntEmpty, ikIntForward, ikObEnd, ikObject, ikPrivate,
    ikProcedure, ikProperty, ikProtected, ikPublic, ikPublished, ikType, ikUnit,
    ikUses, ikUnknown, ikVar);

  TmAdditionalInfo = class(TObject)
    aiUnit: String;
    aiPath: String;
    Ready: Boolean;
    constructor Create;
  end; { TmInfoPas }

  TmLPasInfo = class(TObject)
    ID: TmInfoKind;
    Data: String;
    StartIndex: Integer;
    AI: TmAdditionalInfo;
    constructor Create;
    destructor Destroy; override;
  end; { TmInfoPas }

  TmEPTokenList = class(TPasTokenList)
  private
    FInfo: TmLPasInfo;
    fClassNumber: LongInt;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function GetClassKind: TmInfoKind;
    function GetInterfaceKind: TmInfoKind;
    procedure LoadField;
    procedure LoadNonFieldDeclaration;
    function NextMember: TmInfoKind;
    procedure Reset;
    property Info: TmLPasInfo read FInfo;
  published
  end; { TmEPTokenList }

implementation

constructor TmAdditionalInfo.Create;
begin
  inherited Create;
  aiUnit := '';
  aiPath := '';
  Ready := False;
end;

constructor TmLPasInfo.Create;
begin
  inherited Create;
  ID := ikUnknown;
  StartIndex := -1;
end;

destructor TmLPasInfo.Destroy;
begin
  AI.Free;
  AI := nil;
  inherited Destroy;
end;

destructor TmEPTokenList.Destroy;
begin
  FInfo.Free;
  FInfo := nil;
  inherited Destroy;
end; { Destroy }

constructor TmEPTokenList.Create;
begin
  inherited Create;
  FInfo := TmLPasInfo.Create;
  fClassNumber := 0;
end; { Create }

procedure TmEPTokenList.Reset;
begin
  fClassNumber := 0;
end; { Reset }

function TmEPTokenList.GetClassKind: TmInfoKind;
var
  TempRunIndex: LongInt;
begin
  FInfo.ID := ikClass;
  Result := FInfo.ID;
  if Searcher.ClassList.Count > 0 then
  begin
    Visibility := tkUnKnown;
    FInfo.StartIndex := RunIndex;
    NextID(tkClass);
    TempRunIndex := RunIndex;
    FInfo.Data := GetSubString(TokenPosition[FInfo.StartIndex], TokenPosition[RunIndex + 1]);
    NextNonJunk;
    Case RunID of
      tkEnd:
        begin
          FInfo.ID := ikClEmpty;
          Next;
        end;

      tkAutomated, tkClass, tkPrivate, tkProtected, tkPublic, tkPublished:
        FInfo.ID := ikClass;

      tkSemiColon:
        begin
          FInfo.ID := ikClForward;
          EndCount := 0;
          FInfo.Data := FInfo.Data + ';';
          Next;
        end;

      tkOf:
        begin
          FInfo.ID := ikClReference;
          repeat
            Next;
          until RunID = tkSemiColon;
          FInfo.Data := FInfo.Data + GetSubString(TokenPosition[TempRunIndex + 1], TokenPosition[RunIndex + 1]);
          EndCount := 0;
          Next;
        end;

      tkRoundOpen:
        begin
          FInfo.ID := ikClass;
          FInfo.Data := FInfo.Data + '(';
          NextNonComment;
          while RunID <> tkRoundClose do
          begin
            Case RunID of
              tkCRLF, tkSpace: FInfo.Data := FInfo.Data + ' ';
            else FInfo.Data := FInfo.Data + RunToken;
            end;
            NextNonComment;
          end;
          FInfo.Data := FInfo.Data + ')';
          NextNonJunk;
          if RunID = tkSemiColon then
          begin
            FInfo.ID := ikClEmpty;
            FInfo.Data := FInfo.Data + ';';
            Next;
          end else
            if RunId = tkEnd then
            begin
              FInfo.ID := ikClEmpty;
              FInfo.Data := FInfo.Data + RunToken + ';';
              Next;
            end else
              if RunId in IdentDirect then Visibility := tkPublic;
        end;
    else
      if RunId in IdentDirect then
      begin
        FInfo.ID := ikClass;
        Visibility := tkPublic;
      end;
    end;
    Result := FInfo.ID;
  end;
end; { GetClassKind }

procedure TmEPTokenList.LoadField;
var
  RunningIndex: LongInt;
begin
  FInfo.ID := ikField;
  FInfo.StartIndex := RunIndex;
  RoundCount := 0;
  SquareCount := 0;
  FInfo.Data := RunToken;
  NextNonJunk;
  if RunID = tkComma then
  begin
    RunningIndex := RunIndex;
    while TokenId[RunningIndex] <> tkColon do inc(RunningIndex);
    while (not ((TokenId[RunningIndex] = tkSemiColon) and (RoundCount = 0) and
      (SquareCount = 0))) and (TokenId[RunningIndex] <> tkNull) do
    begin
      Case TokenId[RunningIndex] of
        tkCRLF, tkSpace: FInfo.Data := FInfo.Data + ' ';
        tkBorComment, tkAnsiComment, tkSlashesComment: inc(RunningIndex);
      else FInfo.Data := FInfo.Data + Token[RunningIndex];
      end;
      if (TokenID[RunningIndex] in [tkAutomated, tkClass, tkCompDirect, tkConstructor,
        tkDestructor, tkEnd, tkFunction, tkPrivate, tkProcedure, tkProperty,
          tkProtected, tkPublic, tkPublished]) then
      begin
        FInfo.ID := ikError;
        FInfo.Data := '';
        break;
      end;
      inc(RunningIndex);
    end;
  end else
    while (not ((RunId = tkSemiColon) and (RoundCount = 0) and
      (SquareCount = 0))) and (RunID <> tkNull) do
    begin
      Case RunID of
        tkCRLF, tkSpace: FInfo.Data := FInfo.Data + ' ';
      else FInfo.Data := FInfo.Data + RunToken;
      end;
      if (RunId in [tkAutomated, tkClass, tkCompDirect, tkConstructor,
        tkDestructor, tkEnd, tkFunction, tkPrivate, tkProcedure, tkProperty,
          tkProtected, tkPublic, tkPublished]) then
      begin
        FInfo.ID := ikError;
        FInfo.Data := '';
        break;
      end;
      NextNonComment;
    end;
  FInfo.Data := FInfo.Data + ';';
  Next;
end; { LoadField }

procedure TmEPTokenList.LoadNonFieldDeclaration;
begin
  RoundCount := 0;
  SquareCount := 0;
  while (not ((RunId = tkSemiColon) and (RoundCount = 0) and
    (SquareCount = 0))) and (RunID <> tkNull) do
  begin
    Case RunID of
      tkCRLF, tkSpace: FInfo.Data := FInfo.Data + ' ';
    else FInfo.Data := FInfo.Data + RunToken;
    end;
    NextNonComment;
  end;
  FInfo.Data := FInfo.Data + ';';
  NextNonComment;
  while (not (RunId in [tkAutomated, tkClass, tkCompDirect, tkConstructor,
    tkDestructor, tkEnd, tkFunction, tkPrivate, tkProcedure, tkProperty,
      tkProtected, tkPublic, tkPublished])) and
    (RunID <> tkNull) do
  begin
    Case RunID of
      tkCRLF, tkSpace: FInfo.Data := FInfo.Data + ' ';
    else FInfo.Data := FInfo.Data + RunToken;
    end;
    NextNonComment;
  end;
end; { LoadNonFieldDeclaration }

function TmEPTokenList.NextMember: TmInfoKind;
begin
  if RunID <> tkNull then
  begin
    if IsJunk then NextNonJunk;
    Case RunID of
      tkSquareOpen:
        begin
          FInfo.ID := ikGUID;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken;
          repeat
            Next;
            FInfo.Data := FInfo.Data + RunToken;
          until RunID = tkSquareClose;
          NextNonJunk;
        end;

      tkAutomated:
        begin
          Visibility := tkAutomated;
          FInfo.ID := ikAutomated;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken;
          NextNonJunk;
        end;

      tkClass:
        begin
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken + ' ';
          NextNonJunk;
          Case RunID of
            tkFunction:
              begin
                FInfo.ID := ikClFunction;
                LoadNonFieldDeclaration;
              end;
            tkProcedure:
              begin
                FInfo.ID := ikClProcedure;
                LoadNonFieldDeclaration;
              end;
          end;
        end;

      tkCompDirect:
        begin
          FInfo.ID := ikCompDirect;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken;
          NextNonJunk;
        end;

      tkConstructor:
        begin
          FInfo.ID := ikConstructor;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := '';
          LoadNonFieldDeclaration;
        end;

      tkDestructor:
        begin
          FInfo.ID := ikDestructor;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := '';
          LoadNonFieldDeclaration;
        end;

      tkEnd:
        begin
          Visibility := tkUnKnown;
          FInfo.ID := ikObEnd;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken;
          NextNonJunk;
          FInfo.Data := FInfo.Data + RunToken;
          Next;
        end;

      tkFunction:
        begin
          FInfo.ID := ikFunction;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := '';
          LoadNonFieldDeclaration;
        end;

      tkIdentifier:
        begin
          FInfo.Data := '';
          LoadField;
        end;

      tkPrivate:
        begin
          Visibility := tkPrivate;
          FInfo.ID := ikPrivate;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken;
          NextNonJunk;
        end;

      tkProcedure:
        begin
          FInfo.ID := ikProcedure;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := '';
          LoadNonFieldDeclaration;
        end;

      tkProperty:
        begin
          FInfo.ID := ikProperty;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := '';
          LoadNonFieldDeclaration;
        end;

      tkProtected:
        begin
          Visibility := tkProtected;
          FInfo.ID := ikProtected;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken;
          NextNonJunk;
        end;

      tkPublic:
        begin
          Visibility := tkPublic;
          FInfo.ID := ikPublic;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken;
          NextNonJunk;
        end;

      tkPublished:
        begin
          Visibility := tkPublished;
          FInfo.ID := ikPublished;
          FInfo.StartIndex := RunIndex;
          FInfo.Data := RunToken;
          NextNonJunk;
        end;

    else
      if RunID in IdentDirect then
      begin
        FInfo.Data := '';
        LoadField;
      end
      else
      begin
        FInfo.ID := ikError;
        FInfo.Data := '';
      end;
    end;
  end;
  Result := FInfo.ID;
end; { NextMember }

function TmEPTokenList.GetInterfaceKind: TmInfoKind;
begin
  Result := ikUnKnown;
  if Searcher.InterfaceList.Count > 0 then
  begin
    Visibility := tkUnKnown;
    FInfo.StartIndex := RunIndex;
    NextNonJunk;
    while (RunID <> tkInterFace) and (RunID <> tkDispInterFace) do NextNonJunk;
    FInfo.Data := GetSubString(TokenPosition[FInfo.StartIndex], TokenPosition[RunIndex + 1]);
    NextNonJunk;
    Case RunID of
      tkEnd:
        begin
          FInfo.ID := ikIntEmpty;
          Next;
        end;
      tkSquareOpen: FInfo.ID := ikInterface;
      tkSemiColon:
        begin
          FInfo.ID := ikIntForward;
          EndCount := 0;
          FInfo.Data := FInfo.Data + ';';
          Next;
        end;
      tkRoundOpen:
        begin
          FInfo.ID := ikInterface;
          FInfo.Data := FInfo.Data + '(';
          NextNonComment;
          while RunID <> tkRoundClose do
          begin
            Case RunID of
              tkCRLF, tkSpace: FInfo.Data := FInfo.Data + ' ';
            else FInfo.Data := FInfo.Data + RunToken;
            end;
            NextNonComment;
          end;
          FInfo.Data := FInfo.Data + ')';
          NextNonJunk;
          if RunID = tkSemiColon then
          begin
            FInfo.ID := ikIntEmpty;
            FInfo.Data := FInfo.Data + ';';
            Next;
          end else
            if RunId = tkEnd then
            begin
              FInfo.ID := ikIntEmpty;
              FInfo.Data := FInfo.Data + RunToken + ';';
              Next;
            end;
        end;
    end;
    Result := FInfo.ID;
  end;
end; { GetInterfaceKind }

end.

