//
// The original Delphi code is : NameSpaceSystemLists.pas released 20.07.2003
// Last version: 0.87 released 26.07.2003
// The initial developer is Cedomir Plavljanic (cedomirp@yahoo.com)
// Copyright (C) 2003-2004 Cedomir Plavljanic
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Classes and fuctions in this unit are copy from ABRA PROJECT.
//
// This unit contains next classes:
// TListPointer, TListClass, TListFoldersData
//
// This unit contains next functions:
// IncPtr(Value:Pointer;AInc:Integer):Pointer;
// IncPtrType(Value:Pointer;AType,AInc:Integer):Pointer;
// FreeObjectAndNil(var AObject);
// FreeMemAndNil(var APointer);
// RecordAlloc(Size:LongWord):Pointer;
// RecordMove(Source,Dest:Pointer);
// RecordFree(Item:Pointer);
// RecordFreeAndNil(var Ptr);
//
// version: 0.87
//
unit NameSpaceSystemLists;

interface

const	elListIndex		= -1;
			elListExpand  = -2;
			elListShrink  = -3;

type  TMethodForEach		=	procedure(AItem:Pointer) of object;
			TMethodFirstThat	=	function(AItem:Pointer):Boolean of object;

			TListPointer	=	class
			private
				FList			:	Pointer;
				FCount		:	Integer;
				FCapacity :	Integer;
				FCanClear	:	Boolean;

				procedure	SetCapacity(const Value:Integer);

			protected
				procedure	Error(Info,Code:Integer);
				procedure	Grow;
				function	LocalAt(Index:Integer):Pointer;
				procedure LocalClear;
				procedure	LocalDelete(Index:Integer);
				procedure LocalPut(Index:Integer;const Value:Pointer);
				procedure	LocalInsert(Index:Integer;AItem:Pointer);
				function	LocalFind(AItem:Pointer):Integer;
				procedure	LocalSwap(First,Second:Integer);
				procedure LocalMove(AFrom,ATo:Integer);

			public
				constructor Create(ACanClear:Boolean=True);
				destructor	Destroy;override;

				function 	Add(AItem:Pointer):Integer;overload;virtual;
				procedure Clear;virtual;
				procedure Exchange(First,Second:Integer);
				function 	IndexOf(AItem:Pointer):Integer;virtual;
				procedure Insert(Index: Integer;AItem:Pointer);virtual;
				procedure	MoveFromTo(AFrom,ATo:Integer);
				procedure	Pack;
				procedure	Remove(Index:Integer);virtual;
				procedure	RemoveWithPack(Index:Integer);virtual;
				procedure	Delete(AItem:Pointer);virtual;
				procedure	DeleteWithPack(AItem:Pointer);virtual;

				procedure	ForEach(Action:Pointer;Start:Integer);overload;
				procedure	ForEach(Action:TMethodForEach;Start:Integer);overload;
				procedure	ForEach(Action:Pointer;Start:Integer;CallBackData:Pointer);overload;

				function	FirstThat(Action:Pointer;Start:Integer):Integer;overload;
				function	FirstThat(Action:TMethodFirstThat;Start:Integer):Integer;overload;
				function	FirstThat(Action:Pointer;Start:Integer;CallBackData:Pointer):Integer;overload;

				property	Capacity:Integer read FCapacity write SetCapacity;
				property	Item[Index:Integer]:Pointer read LocalAt; default;
				property	Count:Integer read FCount;
				property	List:Pointer read FList;
				property	CanClear:Boolean read FCanClear write FCanClear default True;
			end;

			TListClass	=	class(TListPointer)
			protected
				function LocalAt(Index: Integer): TObject;
			public
				procedure Clear;override;
				procedure	Remove(Index:Integer);override;
				procedure	RemoveWithPack(Index:Integer);override;
				procedure	Delete(AItem:Pointer);override;
				procedure	DeleteWithPack(AItem:Pointer);override;

				property	Item[Index:Integer]:TObject read LocalAt; default;
			end;

			TListFoldersData	=	class(TListPointer)
			private
				FFolder: String;
			public
				constructor Create(const AFolder:String);

				property Folder:String read FFolder write FFolder;
			end;

function	IncPtr(Value:Pointer;AInc:Integer):Pointer;
function	IncPtrType(Value:Pointer;AType,AInc:Integer):Pointer;

procedure	FreeObjectAndNil(var AObject);
procedure	FreeMemAndNil(var APointer);

function	RecordAlloc(Size:LongWord):Pointer;
procedure RecordMove(Source,Dest:Pointer);
procedure	RecordFree(Item:Pointer);
procedure RecordFreeAndNil(var Ptr);

implementation

uses SysUtils;

resourcestring
	esListIndex					=	'Index %d is out of range';
	esListExpand				=	'Cann''t expand list on %d size';
	esListShrink				=	'Cann''t shrink list on %d size';

{ TListPointer }

function TListPointer.Add(AItem: Pointer): Integer;

begin
	Result:=FCount;
	LocalInsert(Result,AItem);
end;

procedure TListPointer.Clear;

var i:Integer;

begin
	if CanClear then for i:=0 to Pred(FCount) do if Item[i]<>nil then FreeMem(Item[i]);
	LocalClear;
end;

constructor TListPointer.Create(ACanClear: Boolean);

begin
	inherited Create;
	CanClear:=ACanClear;
end;

procedure TListPointer.Delete(AItem: Pointer);

begin
	LocalPut(IndexOf(AItem),nil);
	if CanClear then FreeMem(AItem);
end;

procedure TListPointer.DeleteWithPack(AItem: Pointer);

begin
	LocalDelete(IndexOf(AItem));
	if CanClear then FreeMem(AItem);
end;

destructor TListPointer.Destroy;

begin
	Clear;
	if FList<>nil then FreeMemAndNil(FList);
	inherited Destroy;
end;

procedure TListPointer.Error(Info, Code: Integer);

begin
	case Code of
		elListIndex		:	raise Exception.Create(Format(esListIndex,[Info]));
		elListExpand	:	raise	Exception.Create(Format(esListExpand,[Info]));
		elListShrink	:	raise	Exception.Create(Format(esListShrink,[Info]));
	end;
end;

procedure TListPointer.Exchange(First, Second: Integer);

begin
	LocalSwap(First,Second);
end;

function TListPointer.FirstThat(Action:Pointer;Start:Integer;CallBackData: Pointer): Integer;

var	SaveCall:Integer;

asm
	mov SaveCall,edx
@StartLoop:
	cmp ecx,[eax].FCount
	jae @End
	mov edx,[eax].FList
	push eax
	push ecx
	push dword ptr [edx+ecx*4]
	push CallBackData
	call dword ptr SaveCall
	or al,al
	pop ecx
	pop eax
	jne @EndFind
	inc ecx
	jmp	@StartLoop
@End:
	mov ecx,-1
@EndFind:
	mov eax,ecx
@Exit:
end;

function TListPointer.FirstThat(Action: Pointer;Start:Integer): Integer;

var	SaveCall:Integer;
		SaveBP:Integer;

asm
	mov SaveCall,edx
	mov edx,[ebp]
	mov SaveBP,edx
@StartLoop:
	cmp ecx,[eax].FCount
	jae @End
	mov edx,[eax].FList
	push eax
	push ecx
	mov eax,[edx+ecx*4]
	push SaveBP
	call dword ptr SaveCall
	pop edx
	or al,al
	pop ecx
	pop eax
	jne @EndFind
	inc ecx
	jmp	@StartLoop
@End:
	mov ecx,-1
@EndFind:
	mov eax,ecx
@Exit:
end;

function TListPointer.FirstThat(Action: TMethodFirstThat;Start:Integer): Integer;

asm
	mov ecx,edx
@StartLoop:
	cmp ecx,[eax].FCount
	jae @End
	mov edx,[eax].FList
	push eax
	push ecx
	mov edx,[edx+ecx*4]
	mov eax,[ebp+$0c]
	call dword ptr [ebp+$08]
	or al,al
	pop ecx
	pop eax
	jne @EndFind
	inc ecx
	jmp	@StartLoop
@End:
	mov ecx,-1
@EndFind:
	mov eax,ecx
@Exit:
end;

procedure TListPointer.ForEach(Action:Pointer;Start:Integer;CallBackData: Pointer);

var	SaveCall:Integer;

asm
	mov SaveCall,edx
@StartLoop:
	cmp ecx,[eax].FCount
	jae @End
	mov edx,[eax].FList
	push eax
	push ecx
	push dword ptr [edx+ecx*4]
	push CallBackData
	call dword ptr SaveCall
	pop ecx
	pop eax
	inc ecx
	jmp	@StartLoop
@End:
end;

procedure TListPointer.ForEach(Action: TMethodForEach;Start:Integer);

asm
	mov ecx,edx
@StartLoop:
	cmp ecx,[eax].FCount
	jae @End
	mov edx,[eax].FList
	push eax
	push ecx
	mov edx,[edx+ecx*4]
	mov eax,[ebp+$0c]
	call dword ptr [ebp+$08]
	pop ecx
	pop eax
	inc ecx
	jmp	@StartLoop
@End:
end;

procedure TListPointer.ForEach(Action: Pointer;Start:Integer);

var	SaveCall:Integer;
		SaveBP:Integer;

asm
	mov SaveCall,edx
	mov edx,[ebp]
	mov SaveBP,edx
@StartLoop:
	cmp ecx,[eax].FCount
	jae @End
	mov edx,[eax].FList
	push eax
	push ecx
	push SaveBP
	mov eax,[edx+ecx*4]
	call dword ptr SaveCall
	pop edx
	pop ecx
	pop eax
	inc ecx
	jmp	@StartLoop
@End:
end;

procedure TListPointer.Grow;

asm
	mov edx,[eax].FCapacity
	cmp edx,64
	jb @1
	shr edx,2
	add edx,[eax].FCapacity
	jmp @End
@1:
	cmp edx,8
	jb @2
	add edx,16
	jmp @End
@2:
	add edx,4
@End:
	call TListPointer.SetCapacity
end;

function TListPointer.IndexOf(AItem: Pointer): Integer;

begin
	Result:=LocalFind(AItem);
end;

procedure TListPointer.Insert(Index: Integer; AItem: Pointer);

begin
	LocalInsert(Index,AItem);
end;

function TListPointer.LocalAt(Index: Integer): Pointer;

asm
	cmp edx,[eax].FCount
	jae @Error
	mov eax,[eax].FList
	mov eax,[eax+edx*4]
	jmp @Exit
@Error:
	mov ecx,elListIndex
	call TListPointer.Error
	xor eax,eax
@Exit:
end;

procedure TListPointer.LocalClear;

asm
	mov ecx,[eax].FCount
	jecxz @Exit
	push edi
	mov edi,[eax].FList
	xor edx,edx
	mov [eax].FCount,edx
	mov eax,edx
	cld
	rep stosd
	pop edi
@Exit:
end;

procedure TListPointer.LocalDelete(Index: Integer);

asm
	push edi
	push esi
	cmp edx,[eax].FCount
	jae @Error
	dec [eax].FCount
	mov ecx,[eax].FCount
	sub ecx,edx
	jecxz @Exit
	mov edi,edx
	shl edi,2
	add edi,[eax].FList
	lea esi,[edi+4]
	cld
	rep movsd
	jmp @Exit
@Error:
	mov ecx,elListIndex
	call TListPointer.Error
@Exit:
	pop esi
	pop edi
end;

function TListPointer.LocalFind(AItem: Pointer): Integer;

asm
	push edi
	mov ecx,[eax].FCount
	jecxz @NoFind
	mov edi,[eax].FList
@StartFind:
	cld
	xchg eax,edx
	repne scasd
	jne @NoFind
	mov eax,[edx].FCount
	sub eax,ecx
	dec eax
	jmp @Exit
@NoFind:
	mov eax,-1
@Exit:
	pop edi
end;

procedure TListPointer.LocalInsert(Index: Integer; AItem: Pointer);

asm
	push edi
	push esi
	cmp edx,[eax].FCount
	ja @ErrIndex
	mov edi,[eax].FCount
	cmp edi,[eax].FCapacity
	jb @Insert
	push edx
	push ecx
	push eax
	call TListPointer.Grow
	pop eax
	pop ecx
	pop edx
@Insert:
	push ecx
	mov ecx,[eax].FCount
	inc	[eax].FCount
	mov edi,ecx
	shl edi,2
	add edi,[eax].FList
	lea esi,[edi-4]
	sub ecx,edx
	je @Put
	std
	rep movsd
@Put:
	cld
	pop eax
	stosd
	jmp @Exit
@ErrIndex:
	mov ecx,elListIndex
	call TListPointer.Error
@Exit:
	pop esi
	pop edi
end;

procedure TListPointer.LocalMove(AFrom, ATo: Integer);

asm
	push esi
	push edi
	push ebx
	cmp edx,[eax].FCount
	jae @Err1
	cmp ecx,[eax].FCount
	jae @Err
	mov edi,edx
	shl edi,2
	add edi,[eax].FList
	mov ebx,[edi]
	lea esi,[edi+4]
	push ecx
	mov ecx,[eax].FCount
	sub ecx,edx
	dec ecx
	jecxz @Next
	cld
	rep movsd
@Next:
	pop ecx
	mov edi,[eax].FCount
	dec edi
	shl edi,2
	add edi,[eax].FList
	lea esi,[edi-4]
	push ecx
	mov edx,[eax].FCount
	sub edx,ecx
	mov ecx,edx
	dec ecx
	jecxz @Exit
	std
	rep movsd
	cld
@Exit:
	pop ecx
	mov eax,[eax].FList
	mov [ecx*4+eax],ebx
	jmp @Exit1
@Err:
	mov edx,ecx
@Err1:
	mov ecx,elListIndex
	call TListPointer.Error
@Exit1:
	pop ebx
	pop edi
	pop esi
end;

procedure TListPointer.LocalPut(Index: Integer; const Value: Pointer);

asm
	cmp edx,[eax].FCount
	jae @Error
	mov eax,[eax].FList
	mov [edx*4+eax],ecx
	jmp @Exit
@Error:
	mov ecx,elListIndex
	call TListPointer.Error
@Exit:
end;

procedure TListPointer.LocalSwap(First, Second: Integer);

asm
	push edi
	cmp edx,[eax].FCount
	jae @Err1
	cmp ecx,[eax].FCount
	jae @Err
@1:
	mov eax,[eax].FList
	mov edi,[ecx*4+eax]
	xchg [edx*4+eax],edi
	mov [ecx*4+eax],edi
	jmp @Exit
@Err:
	mov edx,ecx
@Err1:
	mov ecx,elListIndex
	call TListPointer.Error
@Exit:
	pop edi
end;

procedure TListPointer.MoveFromTo(AFrom, ATo: Integer);

begin
	LocalMove(AFrom,ATo);
end;

procedure TListPointer.Pack;

var i:Integer;

begin
	i:=0;
	while i<Count do if Item[i]=nil then LocalDelete(i) else Inc(i)
end;

procedure TListPointer.Remove(Index: Integer);

var Help:Pointer;

begin
	Help:=Item[Index];
	LocalPut(Index,nil);
	if CanClear then FreeMem(Help);
end;

procedure TListPointer.RemoveWithPack(Index: Integer);

var Help:Pointer;

begin
	Help:=Item[Index];
	LocalDelete(Index);
	if CanClear then FreeMem(Help);
end;

procedure TListPointer.SetCapacity(const Value: Integer);

begin
	if Value=FCapacity then Exit;
	if Value<FCapacity then Error(Value,elListShrink) else begin
		ReAllocMem(FList,Value*SizeOf(Pointer));
		FCapacity:=Value;
	end;
end;

{ TListClass }

procedure TListClass.Clear;

var i:Integer;

begin
	if CanClear then for i:=0 to Pred(FCount) do Item[i].Free;
	LocalClear;
end;

procedure TListClass.Delete(AItem: Pointer);

begin
	LocalPut(IndexOf(AItem),nil);
	if CanClear then TObject(AItem).Free;
end;

procedure TListClass.DeleteWithPack(AItem: Pointer);

begin
	LocalDelete(IndexOf(AItem));
	if CanClear then TObject(AItem).Free;
end;

function TListClass.LocalAt(Index: Integer): TObject;

asm
	cmp edx,[eax].FCount
	jae @Error
	mov eax,[eax].FList
	mov eax,[eax+edx*4]
	jmp @Exit
@Error:
	mov ecx,elListIndex
	call TListPointer.Error
	xor eax,eax
@Exit:
end;

procedure TListClass.Remove(Index: Integer);

var Help:TObject;

begin
	Help:=Item[Index];
	LocalPut(Index,nil);
	if CanClear then Help.Free;
end;

procedure TListClass.RemoveWithPack(Index: Integer);

var Help:TObject;

begin
	Help:=Item[Index];
	LocalDelete(Index);
	if CanClear then Help.Free;
end;

//function

function	IncPtr(Value:Pointer;AInc:Integer):Pointer;

asm
	add eax,AInc
end;

function	IncPtrType(Value:Pointer;AType,AInc:Integer):Pointer;

asm
	xchg eax,ecx
	mul edx
	add eax,ecx
end;

procedure	FreeObjectAndNil(var AObject);

begin
	TObject(AObject).Free;
	TObject(AObject):=nil;
end;

procedure	FreeMemAndNil(var APointer);

begin
	FreeMem(Pointer(APointer));
	Pointer(APointer):=nil;
end;

type	TRecordSize	=	packed record
				Size : Integer;
			end;
			PRecordSize	= ^TRecordSize;

function	Min(First,Second:LongWord):LongWord;

asm
	cmp eax,edx
	jbe @End
	mov eax,edx
@End:
end;

function	RecordAlloc(Size:LongWord):Pointer;

begin
	Result:=AllocMem(Size);
	if Result<>nil then asm
		mov eax,Size
		mov edx,Result
		mov [edx],eax
	end;
end;

procedure	RecordMove(Source,Dest:Pointer);

var ASize:Integer;

begin
	ASize:=Min(PRecordSize(Source).Size,PRecordSize(Dest).Size);
	Move(IncPtr(Source,SizeOf(ASize))^,IncPtr(Dest,SizeOf(ASize))^,ASize-SizeOf(ASize));
end;

procedure	RecordFree(Item:Pointer);

begin
	FreeMem(Item);
end;

procedure RecordFreeAndNil(var Ptr);

begin
	FreeMemAndNil(Ptr);
end;

{ TListClassFoldersData }

constructor TListFoldersData.Create(const AFolder: String);

begin
	inherited Create(True);
	Folder:=AFolder;
end;

end.
