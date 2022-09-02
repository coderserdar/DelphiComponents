{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{---
 An example program that shows how to use the NWBIND unit.

 This program lists all objects in the bindery in alphabetic order.
 For each object all the properties are listed: for an item
 property the contents are shown in hex, for a set property the
 individual items in the set are shown.
 ---}

program BindList;

{$B-,S-,R-,V-,X+}

uses
  NWBase,
  NWConn,
  {$IFDEF Windows}
  WinCrt,
  {$ENDIF}
  NWBind;

type
  PString = ^string;

  PObjItem = ^TObjItem;
  TObjItem = record
    NextName : PObjItem;
    NextID   : PObjItem;
    ID   : nwLong;
    OTyp : word;
    Name : PString;
  end;

var
  Server : TnwServer;
  ObjList : PObjItem;
  AccessLevel : byte;

const
  Digits : array [0..15] of char = '0123456789ABCDEF';

function HexB(B : Byte) : string;
  {-Return hex string for byte}
  begin
    HexB[0] := #2;
    HexB[1] := Digits[B shr 4];
    HexB[2] := Digits[B and $F];
  end;

function HexW(W : Word) : string;
  {-Return hex string for word}
  begin
    HexW[0] := #4;
    HexW[1] := Digits[hi(W) shr 4];
    HexW[2] := Digits[hi(W) and $F];
    HexW[3] := Digits[lo(W) shr 4];
    HexW[4] := Digits[lo(W) and $F];
  end;

function HexL(L : nwLong) : string;
  {-Return hex string for nwLong}
  type
    LH = record L, H : word; end;
  begin
    HexL := HexW(LH(L).H)+HexW(LH(L).L);
  end;

function NewStr(var S : string) : PString;
  var
    PS : PString;
  begin
    if nwGetMem(PS, succ(length(S))) then
      PS^ := S;
    NewStr := PS;
  end;

procedure DisposeStr(PS : PString);
  begin
    if (PS <> nil) then
      FreeMem(PS, succ(length(PS^)));
  end;

function CharStr(Count : byte; ch : char) : string;
  var
    S : string;
  begin
    FillChar(S[1], Count, Ch);
    S[0] := char(Count);
    CharStr := S;
  end;

function Pad(S : string; Width : byte) : string;
  begin
    if (length(S) < Width) then
      begin
        FillChar(S[succ(length(S))], Width-length(S), ' ');
        S[0] := char(Width);
      end;
    Pad := S;
  end;

function CreateObjList : PObjItem;
  var
    OL : PObjItem;
    Z  : PObjItem;
    S     : TnwObjectStr;
  begin
    New(OL);
    FillChar(OL^, sizeof(TObjItem), 0);

    New(Z);
    FillChar(Z^, sizeof(TObjItem), 0);
    Z^.NextID := Z;
    Z^.ID := $7FFFFFFF;
    Z^.NextName := Z;
    FillChar(S, sizeof(S), #255);
    S[0] := char(pred(sizeof(S)));
    Z^.Name := NewStr(S);

    OL^.NextID := Z;
    OL^.NextName := Z;

    CreateObjList := OL;
  end;

procedure DestroyObjList(OList : PObjItem);
  var
    Temp, Walker : PObjItem;
  begin
    Walker := OList;
    while (Walker <> Walker^.NextID) do
      begin
        Temp := Walker;
        Walker := Walker^.NextID;
        DisposeStr(Temp^.Name);
        Dispose(Temp);
      end;
    DisposeStr(Walker^.Name);
    Dispose(Walker);
  end;

procedure InsertInList(OList : PObjItem; Obj : PObjItem);
  var
    Temp : PObjItem;
  begin
    Temp := OList;

    while (Obj^.ID > Temp^.NextID^.ID) do
      Temp := Temp^.NextID;
    Obj^.NextID := Temp^.NextID;
    Temp^.NextID := Obj;

    Temp := OList;
    while (Obj^.Name^ > Temp^.NextName^.Name^) do
      Temp := Temp^.NextName;
    Obj^.NextName := Temp^.NextName;
    Temp^.NextName := Obj;
  end;

function FindNameFromList(OList : PObjItem; ID : nwLong) : string;
  var
    Temp : PObjItem;
  begin
    Temp := OList^.NextID;
    while (ID > Temp^.ID) do
      Temp := Temp^.NextID;
    if (ID <> Temp^.ID) then
         FindNameFromList := '***UNKNOWN*** (' + HexL(ID) + ')'
    else FindNameFromList := Temp^.Name^;
  end;

function NextObj(CurObj : PObjItem) : PObjItem;
  begin
    CurObj := CurObj^.NextName;
    if (CurObj = CurObj^.NextName) then
         NextObj := nil
    else NextObj := CurObj;
  end;

function ObjectTypeStr(ObjType : word) : string;
  begin
    case ObjType of
      NWboUser             : ObjectTypeStr := 'User              ';
      NWboGroup            : ObjectTypeStr := 'User group        ';
      NWboPrintQueue       : ObjectTypeStr := 'Print server      ';
      NWboFileServer       : ObjectTypeStr := 'File server       ';
      NWboJobServer        : ObjectTypeStr := 'Job server        ';
      NWboGateway          : ObjectTypeStr := 'Gateway           ';
      NWboPrintServer      : ObjectTypeStr := 'Print queue       ';
      NWboArchiveQueue     : ObjectTypeStr := 'Archive queue     ';
      NWboArchiveServer    : ObjectTypeStr := 'Archive server    ';
      NWboJobQueue         : ObjectTypeStr := 'Job queue         ';
      NWboAdministration   : ObjectTypeStr := 'Administration    ';
      NWboRemoteBridge     : ObjectTypeStr := 'Remote bridge     ';
      NWboAdvertisingPrint : ObjectTypeStr := 'AdvertPrintServer ';
    else
      ObjectTypeStr := 'UNKNOWN ('+HexW(ObjType)+')    ';
    end;{case}
  end;

function ObjFlagStr(ObjIsDyn : boolean) : string;
  begin
    if ObjIsDyn then
         ObjFlagStr := 'Dynamic object'
    else ObjFlagStr := 'Static object ';
  end;

function PropFlagStr(PropIsDyn, PropIsSet : boolean) : string;
  begin
    if PropIsDyn then
      if PropIsSet then
           PropFlagStr := 'Dynamic set  '
      else PropFlagStr := 'Dynamic item '
    else
      if PropIsSet then
           PropFlagStr := 'Static set   '
      else PropFlagStr := 'Static item  ';
  end;

function SecFlagStr(Flags : byte) : string;
  var
    S : string;
  begin
    S := 'Security: write-';
    case ((Flags and $F0) shr 4) of
      0 : S := S + 'anyone';
      1 : S := S + 'logged';
      2 : S := S + 'object';
      3 : S := S + 'supervisor';
      4 : S := S + 'NetWare';
    else
      S := S + 'UNKNOWN'
    end;
    S := S + '; read-';
    case (Flags and $0F) of
      0 : S := S + 'anyone';
      1 : S := S + 'logged';
      2 : S := S + 'object';
      3 : S := S + 'supervisor';
      4 : S := S + 'NetWare';
    else
      S := S + 'UNKNOWN'
    end;
    SecFlagStr := S;
  end;

procedure PrintPropContents(ObjType : word; ObjName : TnwObjectStr;
                            PropName : TnwPropStr);
  var
    i, j, index : integer;
    Status : word;
    SegNum : byte;
    MoreSegs : boolean;
    Margin   : string[24];
    PropValue : TnwPropValue;
    PropIsDyn,
    PropIsSet : boolean;
    FoundOne : boolean;
  begin
    Margin := '            Contents:    ';
    SegNum := 0;
    MoreSegs := true;
    FoundOne := false;

    while MoreSegs do
      begin
        inc(SegNum);
        Status := nwbReadPropertyValue(Server, ObjType, ObjName, PropName,
                                       SegNum, PropValue, PropIsDyn,
                                       PropIsSet, MoreSegs);
        if (Status <> 0) then
          begin
            writeln('!! ReadPropertyValue error: ', HexW(Status));
            MoreSegs := false;
          end
        else if PropIsSet then
          begin
            FoundOne := true;
            for i := 1 to 32 do
              if (PropValue.pvSet[i] <> 0) then
                begin
                  writeln(Margin,
                          FindNameFromList(ObjList, PropValue.pvSet[i]));
                  Margin := CharStr(24, ' ');
                end
          end
        else {property is item}
          begin
            FoundOne := true;
            for i := 1 to 16 do
              begin
                write(Margin); index := (i-1)*8;
                for j := 1 to 8 do
                  begin
                    inc(index);
                    write(HexB(ord(PropValue.pvItem[index])), ' ');
                  end;
                index := (i-1)*8;
                write('[');
                for j := 1 to 8 do
                  begin
                    inc(index);
                    if (PropValue.pvItem[index] >= #32) then
                      write(PropValue.pvItem[index])
                    else
                      write('.');
                  end;
                writeln(']');
                Margin := CharStr(24, ' ');
              end;
          end;
      end;
    if not FoundOne then
      writeln(Margin, '--empty--');
  end;

procedure PrintProperty(ObjType : word; ObjName : TnwObjectStr;
                        PropName : TnwPropStr;
                        PropIsDyn, PropIsSet : boolean;
                        PropSec : byte;
                        HasValue : boolean);
  function CanRead(Security : byte) : boolean;
    begin
      CanRead := (Security and $0F) <= (AccessLevel and $0F);
    end;
  var
    Line : string;
  begin
    writeln(CharStr(10, ' '), PropName);

    writeln(CharStr(10, ' '),
            PropFlagStr(PropIsDyn, PropIsSet),
            SecFlagStr(PropSec));

    if HasValue then
      if CanRead(PropSec) then
        PrintPropContents(ObjType, ObjName, PropName)
      else
        writeln(CharStr(12, ' '), '**Cannot read contents**');
  end;

procedure ScanObjectProperties(ObjType : word; ObjName : TnwObjectStr);
  var
    Status : word;
    PropSeq : nwLong;
    PropName : TnwPropStr;
    PropIsDynamic, PropIsSet : boolean;
    PropSec : byte;
    HasValue, MoreProps : boolean;
  begin
    writeln(CharStr(9, ' '), 'Properties...');
    PropName := '*';
    PropSeq := -1;
    Status := nwbScanProperty(Server, ObjType, ObjName, PropSeq,
                              PropName, PropIsDynamic, PropIsSet,
                              PropSec, HasValue, MoreProps);
    while (Status = 0) do
      begin
        if (PropSeq <> 0) then
          inline($cc);

        PrintProperty(ObjType, ObjName,
                      PropName, PropIsDynamic, PropIsSet, PropSec,
                      HasValue);
        PropName := '*';
        Status := nwbScanProperty(Server, ObjType, ObjName, PropSeq,
                                  PropName, PropIsDynamic, PropIsSet,
                                  PropSec, HasValue, MoreProps);
      end;
    (*writeln('!! ScanProperty finally stopped with: ', HexW(Status));*)
  end;

procedure PrintObject(Obj : PObjItem);
  var
    ObjType : word;
    ObjName : TnwObjectStr;
    ObjID   : nwLong;
    ObjIsDyn: boolean;
    ObjSecurity : byte;
    HasProps: boolean;
    Status  : word;
  begin
    with Obj^ do
      begin
        ObjType := OTyp;
        ObjName := Name^;
        ObjID := -1;
      end;
    Status := nwbScanObject(Server, ObjType, ObjName, ObjID,
                                    ObjIsDyn, ObjSecurity, HasProps);
    if (Status <> 0) then
      begin
        writeln('!! Scan for existing object failed: ', HexW(Status));
        Halt(1);
      end;
    writeln(HexL(ObjID), ' ',
            ObjectTypeStr(ObjType), ' ',
            ObjName);
    writeln('         ',
            ObjFlagStr(ObjIsDyn), ' ',
            SecFlagStr(ObjSecurity));

    if HasProps then
       ScanObjectProperties(ObjType, ObjName);

    writeln(CharStr(24, '-'));
  end;

var
  Status : word;
  TempObj : PObjItem;
  ObjType : word;
  ObjName : TnwObjectStr;
  ObjID   : nwLong;
  ObjIsDyn: boolean;
  ObjSecurity : byte;
  HasProps: boolean;
begin
  if (nwShellType = nsNone) then
    begin
      writeln('!! NetWare not found');
      Halt(1);
    end;

  if (ParamCount = 0) then
    Server := nwDefaultServer
  else
    Server := nwServerFromName(ParamStr(1));
  if (Server = 0) then
    begin
      writeln('!! Server not found');
      Halt(1);
    end;

  Status := nwbGetBinderyAccessLevel(Server, AccessLevel, ObjID);
  if (Status <> 0) then
    begin
      writeln('!! GetBinderyAccessLevel error: ', HexW(Status));
      Halt(1);
    end;

  {Create our internal bindery object list}
  ObjList := CreateObjList;

  {Scan through all objects in the bindery}
  ObjType := NWboWild;
  ObjName := '*';
  ObjID := -1;
  Status := nwbScanObject(Server, ObjType, ObjName, ObjID,
                          ObjIsDyn, ObjSecurity, HasProps);
  while (Status = 0) do
    begin
      {Create new object record and insert in list}
      New(TempObj);
      FillChar(TempObj^, sizeof(TObjItem), 0);
      with TempObj^ do
        begin
          Name := NewStr(ObjName);
          OTyp := ObjType;
          ID := ObjID;
        end;
      InsertInList(ObjList, TempObj);

      { Set up for next scan call }
      Objtype := NWboWild;
      ObjName := '*';
      Status := nwbScanObject(Server, ObjType, ObjName, ObjID,
                              ObjIsDyn, ObjSecurity, HasProps);
    end;
  (*writeln('!! ScanObject finally stopped with: ', HexW(Status));*)

  {Walk through bindery object list in alpha order, printing details}
  TempObj := NextObj(ObjList);
  while (TempObj <> nil) do
    begin
      PrintObject(TempObj);
      TempObj := NextObj(TempObj);
    end;

  DestroyObjList(ObjList);
end.
