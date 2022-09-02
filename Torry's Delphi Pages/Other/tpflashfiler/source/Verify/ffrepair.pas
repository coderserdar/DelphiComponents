{*********************************************************}
{* FlashFiler: Table verification & repair component     *}
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

unit ffrepair;

  { TODO:: Have to handle multi-file tables. }

  { Current limitation:
    Block 0 must have a valid signature, ThisBlock = 0, and NextBlock must
      be equal to ffc_W32NoValue. }

interface

uses
  Classes,
  FFChain,
  FFLLBase,
  FFFileInt,
  FFRepCnst;

const
  ciErrorLimit = 10000;
    { The default error limit for verification. Once this many errors has been
      found, the verification process will stop. The repair process is not
      subject to this limit. }

type
  TffRepairEngine = class;  { forward declaration }
    { Use this component to verify & repair FlashFiler tables.

      This class will look for a registered instance of TffFileInterface
      corresponding to the FF version of the table being verified/repaired.

      Description of use:
        1. Decide what is to be verified and/or repaired. The items that may
           be verified are declared in the TffRepairItem enum.
           By default, all items are verified. To change the items to be
           verified, use the Items property of the TffRepair class.

        2. If verifying, decide how many errors may be encountered before the
           verification process stops. By default, the verification process
           will stop after 100 errors have been encountered. To change this
           value, use the ErrorLimit property. To have verification process
           the entire table regardless of the number of errors, set the
           ErrorLimit property to the value zero.

        3. Decide whether the table is to be verified or verified & repaired.
           Verification tells you whether the table contains any structural or
           content errors. Repair performs a verification and attempts to
           correct the errors. See the Repair Procedures section below to
           determine how errors are corrected.

           If the table is to be verified but not repaired, call the
           Verify method.

           If the table is to be verified & repaired, call the Repair method.

           Even though the Verify method was previously called, the Repair
           method will once again Verify the entire table.

        4. TODO:: verify/repair progress

        5. TODO:: verify/repair error reporting

      REPAIR PROCEDURES

        TODO::

    }

  TffRepairState =
    (rmIdle,                        { Not doing anything }
     rmAcquireInfo,                 { Acquiring information from repair engine }
     rmVerify,                      { Verifying/Checking }
     rmRepair                       { Repairing identified problem }
    );

  TffRepairItem =
    (riNone,
     riFileHeader,                  { Check the file header }
     riBlockScan,                   { Verify block headers }
     riCheckDictionary,             { Check the dictionary }
     riBlockChains,                 { Verify the free & data block chains }
     riDeletedBLOBChain,            { Verify deleted blob segment chain }
     riReindex,                     { Rebuilding an index }
     riPack                         { Packing the table }
    );

  TffRepairItems = set of TffRepairItem;

  TffRepairProgressEvent =
    { Event raised so that parent application may check progress. }
    procedure(Repairer : TffRepairEngine;
              State : TffRepairState;
              Item : TffRepairItem;
        const ActionStr : string;
        const Position, Maximum : Integer) of object;

  TffRepairEngine = class(TObject)
    { Use this component to verify & repair FlashFiler tables. }
  protected
    FAbort : Boolean;
    FChainMgrData,
    FChainMgrFree : TffChainMgr;
    FCompleted : TNotifyEvent;
    FCurrentItem : TffRepairItem;
      { The current item being verified or repaired. }
    FErrorCodes : TList;
    FErrorLimit : Integer;
    FErrors : TStringList;
    FFileInterface: TffFileInterface;
    FFixCodes : TList;
    FFixes : TStringList;
    FHighestAction : TffRepairAction;
      { Based upon the errors reported by the file interface, the most serious
        action that must be taken to repair the table. }
    FInfo : TffGeneralFileInfo;
    FItems : TffRepairItems;
    FOnProgress : TffRepairProgressEvent;
    FOnReportError : TffReportErrorEvent;
    FOnReportFix : TffReportFixEvent;
    FOutputVersion : Longint;
    FState : TffRepairState;
    FUnknownBlocks : TList;
    FUsedBlocksValid : Boolean;

    procedure CheckLastBlock;
    procedure ClearChainMgrs;
    procedure ClearErrors;
    procedure ClearFileInterface;

    procedure DoReportError(Block : ICommonBlock;
                      const ErrCode : Integer;
                            args : array of const);

    procedure DoReportFix(Block : ICommonBlock;
                    const ErrCode : Integer;
                          args : array of const);
                                   
    procedure FixUnknownBlock(const BlockNum : TffWord32;
                                    FileHeaderBlock : IFileHeaderBlock);
    
    function GetDictBlock(const Inx : Longint) : IStreamBlock;
    function GetDictBlockCount : Longint;
    function GetErrorCodes(const Inx : Integer) : Integer;
    function GetErrorCount : Integer;
    function GetErrors(const Inx : Integer) : string;
    function GetFixCodes(const Inx : Integer) : Integer;
    function GetFixCount : Integer;
    function GetFixes(const Inx : Integer) : string;

    procedure GetInfo(var Info : TffGeneralFileInfo);

    procedure HandleRebuildProgress(FileInterface : TffFileInterface;
                                    Position, Maximum : Integer);

    procedure HandleReportError(Block : ICommonBlock;
                          const ErrCode : Integer;
                          const ErrorStr : string);

    procedure HandleReportFix(Block : ICommonBlock;
                        const ErrCode : Integer;
                        const RepairStr : string);

    procedure LinkDataCallback(const Block1Num, Block2Num : TffWord32);
    
    function MapItemToActionStr(const Item : TffRepairItem;
                                const State : TffRepairState) : string;
                                
    procedure MoveDataOrphanCallback(const BlockMoved, PrevBlock : TffWord32);
    procedure PopulateChainMgrs;
    procedure ReportProgress(const Position, Maximum : Integer);

    procedure SetErrorLimit(const Value : Integer);
    procedure SetItems(const Value : TffRepairItems);
    procedure VerifyRepair;
      { This method is called by both the Verify & Repair methods. This
        method centralizes the logic for verifying & repairing the file. }

  public
    procedure Close;
      { Closes the currently open file. }

    function GetBlock(const BlockNumber : Longint) : ICommonBlock;
      { Returns the specified block. }
      
    function GetFileHeaderBlock : IFileHeaderBlock;
      { Returns the file header block for the open file. }

    function GetIndexHeaderBlock : IIndexHeaderBlock;
      { Returns the index header block for the open file. }

    function GetFreeChainDetails : TStringList;
      { Returns a string list containing information about the chain of free
        blocks. }

    function GetDataChainDetails : TStringList;
      { Returns a string list containing information about the chain of data
        blocks. }

    procedure Open(const FileName : string);
      { Open a file. }

    procedure Repair;
      { This method will verify &, if one or more errors are encountered,
        repair the currently open table. }

    procedure Verify;
      { This method will verify the structure & content of the currently open
        table. }

    { Properties }

    property Aborted : Boolean
      read FAbort;
      { Returns True if the previous verify was aborted. }

    property DictBlockCount : Integer
      read GetDictBlockCount;
      { Returns the number of data dictionary blocks in the file. }

    property DictBlocks[const Inx : Longint] : IStreamBlock
      read GetDictBlock;
      { Returns the specified data dictionary block. }

    property ErrorCodes[const Inx : Integer] : Integer
      read GetErrorCodes;
      { Use this property to access the error code associated with each flaw
        found in the file. There is a one-to-one correspondence between the
        elements in this property & the elements in the Errors property. }

    property ErrorCount : Integer
      read GetErrorCount;
      { Use this property to determine the number of errors encountered during
        a verify or repair process. }

    property Errors[const Inx : Integer] : string
      read GetErrors;
      { Use this property to access the descriptive message associated with
        each error. There is a one-to-one correspondence between the
        elements in this property & the elements in the ErrorCodes property. }

    property FixCodes[const Inx : Integer] : Integer
      read GetFixCodes;
      { Use this property to access the error code associated with each fix
        made to the file. There is a one-to-one correspondence between the
        elements in this property & the elements in the Fixes property. }

    property FixCount : Integer
      read GetFixCount;
      { Returns the number of errors fixed by a repair operation. }

    property Fixes[const Inx : Integer] : string
      read GetFixes;
      { Use this property to access the descriptive message associated with
        each fix made to the file. Note there is not a one-to-one correspondence
        between the Fixes and the Errors. There is a one-to-one correspondence
        between the elements in this property & the elements in the FixCodes
        property. }

    property OutputVersion : Longint
      read FOutputVersion write FOutputVersion;
      { The FF version to be assigned to the table when the table is packed.
        Defaults to the current FF version. }

    property State : TffRepairState
      read FState;
      { Returns the current state of the repair engine. }

  published
    constructor Create;
    destructor Destroy; override;

    property ErrorLimit : Integer
      read FErrorLimit write SetErrorLimit default ciErrorLimit;
      { Use this property to have the verification process stop after a certain
        number of errors have been reached. To have the verification process
        analyze the entire table regardless of the number of errors, set this
        property to the value zero. Note that this value is ignored by the
        repair process. The default value is 10. }

    property Items : TffRepairItems
      read FItems write SetItems;
      { Use this property to control the items that are analyzed & repaired.
        By default, all items are analyzed & repaired. }

    property OnComplete : TNotifyEvent
      read FCompleted write FCompleted;
      { This event is raised when a repair run has completed. }

    property OnProgress : TffRepairProgressEvent
      read FOnProgress write FOnProgress;
      { This event is raised as a repair run progresses. }

    property OnReportError : TffReportErrorEvent
      read FOnReportError write FOnReportError;
      { This event is raised when an error is detected in a block. This
        event will be raised during both verification & repair. }

    property OnReportFix : TffReportFixEvent
      read FOnReportFix write FOnReportFix;
      { This event is raised when an error is fixed. It is raised only during
        the repair of a file. }

  end;

implementation

uses
  FFSrBase,
  SysUtils;

const
  csIdle = ' only when the repair engine is idle.';

{===TffRepair========================================================}
constructor TffRepairEngine.Create;
var
  Item : TffRepairItem;
begin
  inherited;
  FErrorLimit := ciErrorLimit;
  FErrorCodes := TList.Create;
  FErrors := TStringList.Create;
  FFixCodes := TList.Create;
  FFixes := TStringList.Create;
  FOutputVersion := FFVersionNumber;
  FUnknownBlocks := TList.Create;
  for Item := Low(TffRepairItem) to High(TffRepairItem) do
    Include(FItems, Item);
end;
{--------}
destructor TffRepairEngine.Destroy;
begin
  ClearChainMgrs;
  FErrorCodes.Free;
  FErrors.Free;
  FFixCodes.Free;
  FFixes.Free;
  FUnknownBlocks.Free;
  ClearFileInterface;
  inherited;
end;
{--------}
procedure TffRepairEngine.ClearChainMgrs;
begin
  FChainMgrData.Free;
  FChainMgrData := nil;
  FChainMgrFree.Free;
  FChainMgrFree := nil;
end;
{--------}
procedure TffRepairEngine.ClearErrors;
begin
  FAbort := False;
  FErrorCodes.Clear;
  FErrors.Clear;
  FFixCodes.Clear;
  FFixes.Clear;
  FHighestAction := raDecide;
  FUnknownBlocks.Clear;
end;
{--------}
procedure TffRepairEngine.ClearFileInterface;
begin
  if FFileInterface <> nil then begin
    FInfo.Free;
    FFileInterface.Close;
    FFileInterface := nil;
  end;
end;
{--------}
procedure TffRepairEngine.Close;
begin
  if FState = rmIdle then
    ClearFileInterface
  else
    raise Exception.Create('The Close method can be called' + csIdle);
end;
{--------}
procedure TffRepairEngine.DoReportError(Block : ICommonBlock;
                                  const ErrCode : Integer;
                                        args : array of const);
begin
  HandleReportError(Block, ErrCode,
                    Format(rcErrStr[ErrCode], args));
end;
{--------}
procedure TffRepairEngine.DoReportFix(Block : ICommonBlock;
                                const ErrCode : Integer;
                                      args : array of const);
begin
  HandleReportFix(Block, ErrCode,
                  Format(rcFixStr[ErrCode], args));
end;
{--------}
procedure TffRepairEngine.FixUnknownBlock(const BlockNum : TffWord32;
                                                FileHeaderBlock : IFileHeaderBlock);
var
  PotentialFirstBlock,
  PotentialLastBlock,
  RefBlock : TffWord32;
  Block : ICommonBlock;
  DataBlock : IDataBlock;
begin
  PotentialFirstBlock := ffc_W32NoValue;
  PotentialLastBlock := ffc_W32NoValue;

  { Make sure this block is not referenced in the data chain. We assume
    that since it is an unknown block then it will not be a member of
    the data chain. }
  if FChainMgrData.Referenced(BlockNum, True, RefBlock) then begin
    DataBlock := FFileinterface.GetBlock(RefBlock) as IDataBlock;
    DataBlock.BeginUpdate;
    try
      if DataBlock.PrevDataBlock = BlockNum then begin
        PotentialFirstBlock := DataBlock.BlockNum;
        DataBlock.PrevDataBlock := ffc_W32NoValue
      end
      else begin
        PotentialLastBlock := DataBlock.BlockNum;
        DataBlock.NextDataBlock := ffc_W32noValue;
      end;  { if..else }
    finally
      DataBlock.EndUpdate;
      DataBlock := nil;
    end;
  end;

  { Is the block referenced in the free block chain? }
  if not FChainMgrFree.Referenced(BlockNum, False, RefBlock) then begin
    { It is not referenced. Get the first free block. }
    RefBlock := FileHeaderBlock.FirstFreeBlock;
    { Does the first free block already point to this block? }
    if RefBlock <> BlockNum then begin
      { No. Have the unknown block point to the block listed as the
        first free block. }
      Block := FFileInterface.GetBlock(BlockNum);
      Block.BeginUpdate;
      try
        Block.NextBlock := FileHeaderBlock.FirstFreeBlock;
      finally
        Block.EndUpdate;
        Block := nil;
      end;
      { Set the first free block to be the unknown block. }
      FileHeaderBlock.BeginUpdate;
      try
        FileHeaderBlock.FirstFreeBlock := BlockNum;
      finally
        FileHeaderBlock.EndUpdate;
      end;
    end
    else begin
      { Yes, it is already pointed to by the file header. Add the
        unknown block to the free block chain manager. }
      Block := FFileInterface.GetBlock(BlockNum);
      FChainMgrFree.AddBlock(BlockNum, Block.NextBlock, ffc_W32NoValue);
      Block := nil;
    end;  { if..else }
  end;

  { Is the block referenced in the file header? }
  if FileHeaderBlock.FirstDataBlock = BlockNum then begin
    { Update the file header with the first data block. }
    if PotentialFirstBlock <> ffc_W32NoValue then begin
      FileHeaderBlock.BeginUpdate;
      try
        FileHeaderBlock.FirstDataBlock := PotentialFirstBlock;
      finally
        FileHeaderBlock.EndUpdate;
      end;
    end
    else begin
      { This will be handled later when the data chain is reviewed. }
    end;  { if..else }
  end;

  if FileHeaderBlock.LastDataBlock = BlockNum then begin
    { Update the file header with the last data block. }
    if PotentialLastBlock <> ffc_W32NoValue then begin
      FileHeaderBlock.BeginUpdate;
      try
        FileHeaderBlock.LastDataBlock := PotentialLastBlock;
      finally
        FileHeaderBlock.EndUpdate;
      end;
    end
    else begin
      { This will be handled later when the data chain is reviewed. }
    end;  { if..else }
  end;

end;
{--------}
function TffRepairEngine.GetDictBlock(const Inx : Longint) : IStreamBlock;
begin
  { TODO:: Verify state of repair engine }
  Result := FFileInterface.DictBlocks[Inx];
end;
{--------}
function TffRepairEngine.GetDictBlockCount : Longint;
begin
  { TODO:: Verify state of repair engine }
  Result := FFileInterface.DictBlockCount;
end;
{--------}
function TffRepairEngine.GetErrorCodes(const Inx : Integer) : Integer;
begin
  { TODO:: Verify state of repair engine }
  Result := Integer(FErrorCodes[Inx]);
end;
{--------}
function TffRepairEngine.GetErrorCount : Integer;
begin
  { TODO:: Verify state of repair engine }
  Result := FErrors.Count;
end;
{--------}
function TffRepairEngine.GetErrors(const Inx : Integer) : string;
begin
  { TODO:: Verify state of repair engine }
  Result := FErrors[Inx];
end;
{--------}
function TffRepairEngine.GetFixCodes(const Inx : Integer) : Integer;
begin
  { TODO:: Verify state of repair engine }
  Result := Integer(FFixCodes[Inx]);
end;
{--------}
function TffRepairEngine.GetFixCount : Integer;
begin
  { TODO:: Verify state of repair engine }
  Result := FFixes.Count;
end;
{--------}
function TffRepairEngine.GetFixes(const Inx : Integer) : string;
begin
  { TODO:: Verify state of repair engine }
 Result := FFixes[Inx];
end;
{--------}
function TffRepairEngine.GetBlock(const BlockNumber : Longint) : ICommonBlock;
begin
  { TODO:: Verify state of repair engine }
  Result := FFileInterface.GetBlock(BlockNumber);
  if Result <> nil then
    Result.OnGetInfo := GetInfo;
end;
{--------}
function TffRepairEngine.GetFileHeaderBlock : IFileHeaderBlock;
begin
  { TODO:: Verify state of repair engine }
  Result := FFileInterface.GetFileHeaderBlock;
  if Result <> nil then
    Result.OnGetInfo := GetInfo;
end;
{--------}
function TffRepairEngine.GetFreeChainDetails : TStringList;
begin
  PopulateChainMgrs;
  Result := FChainMgrFree.Describe;
end;
{--------}
function TffRepairEngine.GetIndexHeaderBlock : IIndexHeaderBlock;
begin
  { TODO:: Verify state of repair engine }
  Result := FFileInterface.GetIndexHeaderBlock;
  if Result <> nil then
    Result.OnGetInfo := GetInfo;
end;
{--------}
procedure TffRepairEngine.GetInfo(var Info : TffGeneralFileInfo);
begin
  Info := FInfo;
end;
{--------}
function TffRepairEngine.GetDataChainDetails : TStringList;
begin
  PopulateChainMgrs;
  Result := FChainMgrData.Describe;
end;
{--------}
procedure TffRepairEngine.HandleRebuildProgress(FileInterface : TffFileInterface;
                                                Position, Maximum : Integer);
begin
  ReportProgress(Position, Maximum);
end;
{--------}
procedure TffRepairEngine.HandleReportError(Block : ICommonBlock;
                                      const ErrCode : Integer;
                                      const ErrorStr : string);
begin
  if Block = nil then
    FErrors.Add(Format('Code %d: %s', [ErrCode, ErrorStr]))
  else
    FErrors.Add(Format('Block %d, code %d: %s',
                       [Block.BlockNum, ErrCode, ErrorStr]));
  FErrorCodes.Add(Pointer(Errcode));
  { Record the most severe action that must be taken to repair this file. }
  if rcAction[ErrCode] > FHighestAction then
    FHighestAction := rcAction[ErrCode];

  { Detect errors that must be handled at this level. }
  if ErrCode = rciInvalidUsedBlocks then
    { Indicate that the used blocks field in the file header is invalid. }
    FUsedBlocksValid := False
  else if ErrCode = rciUnknownBlockType then
    { The block type is not valid. When repairing, it will be switched to a
      free block. However, we must make sure it is not in the chain of used
      data blocks & is not referenced as the first or last data block in the
      file header. }
    FUnknownBlocks.Add(Pointer(Block.BlockNum));

  if Assigned(FOnReportError) then
    FOnReportError(Block, ErrCode, ErrorStr);

  { Have we reached the error limit? }
  if (State = rmVerify) and (FErrors.Count = FErrorLimit) then
    FAbort := True;
end;
{--------}
procedure TffRepairEngine.HandleReportFix(Block : ICommonBlock;
                                    const ErrCode : Integer;
                                    const RepairStr : string);
begin
  if Block = nil then
    FErrors.Add(Format('Code %d: %s', [ErrCode, RepairStr]))
  else
    FFixes.Add(Format('Block %d (%d): %s',
                      [Block.BlockNum, ErrCode, RepairStr]));
  FFixCodes.Add(Pointer(Errcode));
  if ErrCode = rciInvalidUsedBlocks then
    FUsedBlocksValid := True;
  if Assigned(FOnReportFix) then
    FOnReportFix(Block, ErrCode, RepairStr);
end;
{--------}
procedure TffRepairEngine.LinkDataCallback(const Block1Num, Block2Num : TffWord32);
var
  Block1, Block2 : IDataBlock;
begin
  Block1 := FFileInterface.GetBlock(Block1Num) as IDataBlock;
  Block2 := FFileInterface.GetBlock(Block2Num) as IDataBlock;

  Block1.BeginUpdate;
  try
    Block1.NextDataBlock := Block2Num;
  finally
    Block1.EndUpdate;
    Block1 := nil;
  end;

  Block2.BeginUpdate;
  try
    Block2.PrevDataBlock := Block1Num;
  finally
    Block2.EndUpdate;
    Block2 := nil;
  end;
end;
{--------}
function TffRepairEngine.MapItemToActionStr(const Item : TffRepairItem;
                                            const State : TffRepairState): string;
begin
  if State = rmVerify then
    case Item of
      riFileHeader        : Result := 'Verifying file header';
      riBlockScan         : Result := 'Scanning blocks';
      riCheckDictionary   : Result := 'Verifying dictionary';
      riBlockChains       : Result := 'Verifying block chains';
      riDeletedBLOBChain  : Result := 'Verifying deleted BLOB chain';
    end
  else if State = rmRepair then
    case Item of
      riFileHeader        : Result := 'Repairing file header';
      riBlockScan         : Result := 'Repairing blocks';
      riCheckDictionary   : Result := 'Repairing dictionary';
      riBlockChains       : Result := 'Repairing block chains';
      riDeletedBLOBChain  : Result := 'Repairing deleted BLOB chain';
      riReindex           : Result := 'Reindexing';
      riPack              : Result := 'Packing';
    end
end;
{--------}
procedure TffRepairEngine.MoveDataOrphanCallback(const BlockMoved, PrevBlock : TffWord32);
var
  MovedBlock, PreviousDataBlock : IDataBlock;
begin
  MovedBlock := FFileInterface.GetBlock(BlockMoved) as IDataBlock;
  PreviousDataBlock := FFileInterface.GetBlock(PrevBlock) as IDataBlock;
  MovedBlock.BeginUpdate;
  try
    MovedBlock.PrevDataBlock := PrevBlock;
    MovedBlock.NextDataBlock := ffc_W32NoValue;
  finally
    MovedBlock.EndUpdate;
    MovedBlock := nil;
  end;

  PreviousDataBlock.BeginUpdate;
  try
    PreviousDataBlock.NextDataBlock := BlockMoved;
  finally
    PreviousDataBlock.EndUpdate;
    PreviousDataBlock := nil;
  end;
end;
{--------}
procedure TffRepairEngine.Open(const FileName :string);
begin
  if FileExists(FileName) then begin
    ClearFileInterface;
    FFileInterface := TffFileInterface.FindInterface(FileName);
    if FFileInterface = nil then
      raise Exception.Create('Could not find an interface to handle this file.')
    else begin
      FFileInterface.Open(FileName);
      FFileInterface.OnReportError := HandleReportError;
      FFileInterface.OnReportFix := HandleReportFix;
      FInfo := FFileInterface.GetFileInfo;
      ClearChainMgrs;
      FChainMgrData := TffChainMgr.Create;
      FChainMgrFree := TffChainMgr.Create;
    end;
  end
  else
    raise Exception.Create('File ' + FileName + ' does not exist.');
end;
{--------}
procedure TffRepairEngine.PopulateChainMgrs;
var
  FileHeaderBlock : IFileHeaderBlock;
  Block : ICommonBlock;
  DataBlock : IDataBlock;
  Inx,
  MaxBlocks : TffWord32;
begin
  { TODO:: File must be open. }

  { If the chain managers have not been populated then scan through the
    blocks. }
  if not FChainMgrData.Populated then begin
    FileHeaderBlock := GetFileHeaderBlock;
    if FUsedBlocksValid then
      MaxBlocks := FileHeaderBlock.UsedBlocks
    else
      MaxBlocks := FileHeaderBlock.EstimatedUsedBlocks;
    for Inx := 1 to Pred(MaxBlocks) do begin
      Block := FFileInterface.GetBlock(Inx);
      { If this is a data block or free block then add information to the
        appropriate chain manager. }
      if Block.BlockType = btData then begin
        DataBlock := (Block as IDataBlock);
        FChainMgrData.AddBlock(Block.BlockNum,
                               DataBlock.NextDataBlock,
                               DataBlock.PrevDataBlock)
      end
      else if Block.BlockType = btFree then
          FChainMgrFree.AddBlock(Block.BlockNum,
                                 Block.NextBlock,
                                 ffc_W32NoValue);
      Block := nil;
    end;  { for }
    FChainMgrFree.Fixup;
    FChainMgrData.Fixup;
    FChainMgrFree.Populated := True;
    FChainMgrData.Populated := True;
  end;  { if }
end;
{--------}
procedure TffRepairEngine.Repair;
begin
  if FState <> rmIdle then
    raise Exception.Create('The Repair method can be called' + csIdle);
  FState := rmRepair;
  VerifyRepair;
end;
{--------}
procedure TffRepairEngine.ReportProgress(const Position, Maximum : Integer);
var
  ActionStr : string;
begin
  if Assigned(FOnProgress) then begin
    ActionStr := MapItemToActionStr(FCurrentItem, FState);
    FOnProgress(Self, FState, FCurrentItem, ActionStr, Position, Maximum);
  end;
end;
{--------}
procedure TffRepairEngine.SetErrorLimit(const Value : Integer);
begin
 if FState = rmIdle then
   FErrorLimit := Value
 else
   raise Exception.Create('ErrorLimit can be set' + csIdle);
end;
{--------}
procedure TffRepairEngine.SetItems(const Value : TffRepairItems);
begin
 if FState = rmIdle then
   FItems := Value
 else
   raise Exception.Create('RepairItems can be set' + csIdle);
end;
{--------}
procedure TffRepairEngine.Verify;
begin
  if FState <> rmIdle then
    raise Exception.Create('The Verify method can be called' + csIdle);

  FState := rmVerify;
  VerifyRepair;
end;
{--------}
procedure TffrepairEngine.CheckLastBlock;
var
  Block : ICommonBlock;
  DataBlock : IDataBlock;
begin
  { The last block's NextBlock reference should be ffc_W32NoValue. }
  if (FChainMgrData.LastBlockNumber <> ffc_W32NoValue) and
     (FChainMgrData.LastBlockNextBlockNumber <> ffc_W32NoValue) then begin
    { Get the last data block. }
    Block := FFileInterface.GetBlock(FChainMgrData.LastBlockNumber);
    try
      Block.BeginUpdate;
      try
        DataBlock := (Block as IDataBlock);
        DataBlock.NextDataBlock := ffc_W32NoValue;
        DoReportFix(Block, rciInvalidBlockRefNext, [ffc_W32NoValue]);
      finally
        Block.EndUpdate;
      end;
    finally
      Block := nil;
    end;
  end;  { if }
end;
{--------}
procedure TffRepairEngine.VerifyRepair;
var
  FileHeaderBlock : IFileHeaderBlock;
  Block : ICommonBlock;
  DataBlock : IDataBlock;
  Inx,
  BlockNum,
  MaxBlocks : TffWord32;
begin
  FChainMgrData.Clear;
  FChainMgrFree.Clear;
  ClearErrors;
  try
    { Init vars }
    FInfo.Free;
    FInfo := FFileInterface.GetFileInfo;
    FUsedBlocksValid := True;
    FileHeaderBlock := GetFileHeaderBlock;

    { Verify the file header. }
    if riFileHeader in FItems then begin
      FCurrentItem := riFileHeader;
      ReportProgress(25, 100);
      FileHeaderBlock.OnGetInfo := GetInfo;
      if FState = rmVerify then
        FileHeaderBlock.Verify
      else
        FileHeaderBlock.Repair;
      ReportProgress(100, 100);
    end;

    if FAbort then
      Exit;

    { Scan through the blocks. }
    if (riBlockScan in FItems) then begin
      FCurrentItem := riBlockScan;
      if FUsedBlocksValid then
        MaxBlocks := FileHeaderBlock.UsedBlocks
      else
        MaxBlocks := FileHeaderBlock.EstimatedUsedBlocks;
      ReportProgress(0, MaxBlocks);
      for Inx := 1 to Pred(MaxBlocks) do begin
        Block := FFileInterface.GetBlock(Inx);
        try
          { If this is a data block or free block then add information to the
            appropriate chain manager. }
          if Block.BlockType = btData then begin
            DataBlock := Block as IDataBlock;
            try
              FChainMgrData.AddBlock(Block.BlockNum,
                                     DataBlock.NextDataBlock,
                                     DataBlock.PrevDataBlock);
            finally
              DataBlock := nil;
            end;
          end
          else if Block.BlockType = btFree then
              FChainMgrFree.AddBlock(Block.BlockNum,
                                     Block.NextBlock,
                                     ffc_W32NoValue);
          Block.OnGetInfo := GetInfo;
          if FState = rmVerify then
            Block.Verify
          else
            Block.Repair;
        finally
          Block := nil;
        end;
        ReportProgress(Inx, MaxBlocks + TffWord32(FUnknownBlocks.Count));
        if FAbort then
          Exit;
      end;  { for }

      { Check for the case where there is only 1 data block or 1 free block
        in the table. }
      FChainMgrFree.Fixup;
      FChainMgrData.Fixup;
      FChainMgrFree.Populated := True;
      FChainMgrData.Populated := True;

      { Are we repairing and, if so, were any unknown blocks encountered? }
      if FState = rmRepair then begin
        { Yes. Roll through the blocks. By this point we assume they have been
          marked as free blocks. }
        if FUnknownBlocks.Count > 0 then
          { Note: The previous line was added because Inx is TffWord32 and
            Pred(FUnknownBlocks.Count) = -1 which translates to the max value
            of TffWord32 }
          for Inx := Pred(FUnknownBlocks.Count) downto 0 do begin
            BlockNum := TffWord32(FUnknownBlocks[Inx]);
            FixUnknownBlock(BlockNum, FileHeaderBlock);
            FUnknownBlocks.Delete(Inx);
            ReportProgress(MaxBlocks + Inx,
                           MaxBlocks + TffWord32(FUnknownBlocks.Count));
          end;  { for }
      end;  { if }
    end;

    if FAbort then
      Exit;

    if riCheckDictionary in FItems then begin
      FCurrentItem := riCheckDictionary;
      { TODO }
    end;

    if FAbort then
      Exit;

    if FAbort then
      Exit;

    if riBlockChains in FItems then begin
      FCurrentItem := riBlockChains;
      { Verify the data block chain first. }
      { Are the used data blocks split across multiple chains? }
      if not FChainMgrData.HasValidChain then begin
        DoReportError(nil, rciSplitUsedDataBlocks, []);
        if FState = rmRepair then begin
          FChainMgrData.LinkChains(LinkDataCallback);
          DoReportFix(nil, rciSplitUsedDataBlocks, []);
        end;
      end;  { if }
      ReportProgress(20, 100);

      { Are there any orphaned blocks? }
      if FChainMgrData.HasOrphans then begin
        DoReportError(nil, rciOrphanedUsedDataBlocks, []);
        if FState = rmRepair then begin
          { Add each orphan to the end of the data chain. }
          FChainMgrData.MoveOrphansToTail(MoveDataOrphanCallback);
          DoReportFix(nil, rciOrphanedUsedDataBlocks, []);
        end;
      end;  { if }
      ReportProgress(40, 100);

      if FState = rmRepair then begin
        CheckLastBlock;
          { Note: The code for CheckLastBlock was put into its own procedure
            in order to force the block to go out of scope & be freed prior
            to the table being packed. }

        { Verify the LastDataBlock property of the file header block. Get what
          should be the last data block from the chain manager. }
        if (FileHeaderBlock.LastDataBlock <> FChainMgrData.LastBlockNumber) then begin
          FileHeaderBlock.BeginUpdate;
          try
            FileHeaderBlock.LastDataBlock := FChainMgrData.LastBlockNumber;
            DoReportFix(FileHeaderBlock, rciInvalidBlockRefLastData,
                        [FileHeaderBlock.LastDataBlock]);
          finally
            FileHeaderBlock.EndUpdate;
          end;
        end;  { if }
      end;


      { Check the free block chain. }
      { TODO }

      { Verify the FirstDataBlock property of the file header block. }
      { TODO }

      ReportProgress(100, 100);
    end;

    if FAbort then
      Exit;

    if riDeletedBLOBChain in FItems then begin
      FCurrentItem := riDeletedBLOBChain;
      { TODO }
    end;

    if FAbort then
      Exit;

    { Any high-level repairs necessary? }
    if (FState = rmRepair) and (FHighestAction = raPack) then begin
      { Deref the file header block so that it will be fully freed when the file
        is closed for a reindex or pack. }
      FileHeaderBlock := nil;
      FFileInterface.OnRebuildProgress := HandleRebuildProgress;
      FFileInterface.OutputVersion := FOutputVersion;
      FCurrentItem := riPack;
      FFileInterface.Pack;
    end;  { if }
  finally
    FState := rmIdle;
    if Assigned(FCompleted) then
      FCompleted(Self);
  end;
end;
{====================================================================}

end.
