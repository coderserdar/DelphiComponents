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
// $Log:  17583: mcmQsort.pas
//
//    Rev 1.4    01-03-2011 20:39:42  mcm    Version: IMG 3.4
//
//    Rev 1.3    22-10-2009 17:02:22  mcm
// Added QuickSelect for using in Median filter.
//
//    Rev 1.2    27-08-2007 18:53:58  mcm    Version: IMG 3.1
// Added support for Delphi 2007
//
//   Rev 1.1    27-01-2003 13:43:44  mcm

//
//   Rev 1.0    27-05-2002 16:22:26  mcm

//------------------------------------------------------------------------------
{$A+} // Align data Byte = $A-, Word = $A+.
{$F+} // Force FAR call.
{$G+} // Generate 80286 code.
{$N+} // CoProcessor code generation.
{$R-} // Range check.
{$W-} // Protected mode only $W-, Real mode stack frame support $W+.
{$X+} // Extended syntax (When on, function can be used as procedures).
//------------------------------------------------------------------------------

Unit mcmQsort;

interface

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses mcmImageTypeDef;


procedure QSort    (var c; Lo, Hi : longint);

procedure QSortInt (var c; Lo, Hi : longint);

procedure QSortLong(var c; Lo, Hi : longint);

procedure QSortSingle(var c; Lo, Hi : longint);

procedure DxyQSort(a, b : PVectorD; Lo, Hi : longint);

function QuickSelect(Data : PVectorB; DataSize : word) : byte;

implementation

{ $DEFINE MCMASM}

{$DEFINE RECURSIVE_QSORT}

{$IFDEF RECURSIVE_QSORT}

procedure QSort(var c; Lo, Hi : longint);
type ByteArray = array[0..0] of byte;

     procedure Sort(const l, r : longint);
     {$IFDEF MCMASM}
     var i, j    : longint;
         c_ptr   : pointer;
     {$ELSE}
     var i, j, k : longint;
         x, y    : byte;
     {$ENDIF}
    begin
     {$IFDEF MCMASM}

       c_ptr := @ByteArray(c)[0];
       asm
         mov     ebx,l     // i := l;
         mov     edx,r     // j := r;
         mov     edi,DWORD PTR [c_ptr]

         @NextSort:
         mov     ecx,edx
         add     ecx,ebx
         shr     ecx,1   // k := (l + r) shr 1;

         mov     al,byte ptr [edi+ecx] // x := ByteArray(c)[k];

         // Repeat

         // while (ByteArray(c)[i] < x)
         // do inc(i);
         @NextI:
         cmp     al,byte ptr [edi+ebx]
         jbe      @GotI
         inc     ebx
         jmp     @NextI
         @GotI:

         // while (x < ByteArray(c)[j])
         // do dec(j);
         @NextJ:
         cmp     al,byte ptr [edi+edx]
         jae     @GotJ
         dec     edx
         jmp     @NextJ
         @GotJ:

         // if (i <= j)
         // then begin
         cmp     ebx,edx
         jg      @NoExchange

         // exchange ByteArray(c)[i] and ByteArray(c)[j];
         //mov     byte ptr [edi+ebx],dh
         //mov     byte ptr [edi+ecx],dl

         mov     ah,byte ptr [edi+ebx]
         xchg    ah,byte ptr [edi+edx]
         dec     edx  // dec(j);
         mov     byte ptr [edi+ebx],ah
         inc     ebx  // inc(i);

         @NoExchange:

         // until (i > j);
         cmp     ebx,edx
         jle     @NextI

         {
         // if (l < j)
         // then Sort(l, j);
         cmp     ecx,l
         jle     @Sortir
         mov     ebx,l
         jmp     @NextSort

         @Sortir:
         // if (i < r)
         // then Sort(i, r);
         cmp     ebx,r
         jge     @Done
         mov     ecx,r
         jmp     @NextSort
         }
         mov     i,ebx
         mov     j,edx
         @Done:
       end;

       if (l < j)
       then Sort(l, j);
       if (i < r)
       then Sort(i, r);

     {$ELSE}

       i := l;
       j := r;
       k := (l + r) shr 1;
       x := ByteArray(c)[k];
       repeat
         while (ByteArray(c)[i] < x)
         do inc(i);
         while (x < ByteArray(c)[j])
         do dec(j);
         if (i <= j)
         then begin
              y               := ByteArray(c)[i];
              ByteArray(c)[i] := ByteArray(c)[j];
              ByteArray(c)[j] := y;
              inc(i);
              dec(j);
         end;
       until (i > j);
       if (l < j)
       then Sort(l, j);
       if (i < r)
       then Sort(i, r);

     {$ENDIF}
     end; // Sort.

     {
     procedure Sort2(l, r : longint);  // Twice as slow as the above implementation.
     var i, Piv : longint;
         Temp, Cmp : byte;
     begin
       if (l >= r)
       then exit;
       cmp := ByteArray(c)[l];
       piv := l;
       for i := l + 1 to r
       do if (ByteArray(c)[i] < cmp)
          then begin
               inc(piv);
               Temp := ByteArray(c)[Piv];
               ByteArray(c)[Piv] := ByteArray(c)[i];
               ByteArray(c)[i] := Temp;
          end;
       Temp := ByteArray(c)[Piv];
       ByteArray(c)[Piv] := ByteArray(c)[l];
       ByteArray(c)[l] := Temp;
       Sort2(l, Piv - 1);
       Sort2(Piv + 1, r);
     end;
     }

begin // Begin QSort
  Sort(Lo, Hi);
end; // End QSort

{$ELSE}

const MAX_LEVELS = 1024;
var
    Low   : array[0..MAX_LEVELS] of integer;
    High  : array[0..MAX_LEVELS] of integer;

procedure QSort(var c; Lo, Hi : longint); // : boolean;
// Non-recursive implementation of Q-Sort.
type ByteArray = array[0..0] of byte;
var i     : integer;
    l, r  : longint;
    Pivot : integer;
begin
  i := 0;

  Low[0]  := 0;
  High[0] := Hi; // Number of elements;
  while (i >= 0)
  do begin
     l := Low[i];
     r := High[i];// - 1;
     if (l < r)
     then begin
          Pivot := ByteArray(c)[l];
          if (i = MAX_LEVELS - 1)
          then begin
               // Result := False;
               Exit;
          end;
          while (l < r)
          do begin
             while (ByteArray(c)[r] >= Pivot) and (l < r)
             do dec(r);
             if (l < r)
             then begin
                  // ByteArray(c)[L++] = ByteArray(c)[r];
                  ByteArray(c)[l] := ByteArray(c)[r];
                  inc(l);
             end;
             while (ByteArray(c)[l] <= Pivot) and (l < r)
             do inc(l);
             if (l < r)
             then begin
                  // ByteArray(c)[R--] = ByteArray(c)[l];
                  ByteArray(c)[r] := ByteArray(c)[l];
                  dec(r);
             end;
          end;
          ByteArray(c)[l] := Pivot;
          Low[i+1]  := l + 1;
          High[i+1] := High[i];
          // High[i++] := l;
          High[i] := l;
          inc(i);
     end
     else dec(i);
  end;
  // Result := True;
end; // QSort.

{$ENDIF}

procedure QSortInt(var c; Lo, Hi : longint);
type IntArray = array[0..0] of integer;

     procedure Sort(const l, r : longint);
     var i, j, k : longint;
         x, y    : integer;
     begin
       i := l;
       j := r;
       k := (l + r) shr 1;
       x := IntArray(c)[k];
       repeat
         while (IntArray(c)[i] < x)
         do inc(i);
         while (x < IntArray(c)[j])
         do dec(j);
         if (i <= j)
         then begin
              y              := IntArray(c)[i];
              IntArray(c)[i] := IntArray(c)[j];
              IntArray(c)[j] := y;
              inc(i);
              dec(j);
         end;
       until (i > j);
       if (l < j)
       then Sort(l, j);
       if (i < r)
       then Sort(i, r);
     end; // End Sort.

begin // Begin QSortInt
  Sort(Lo, Hi);
end; // End QSortInt.


procedure QSortLong(var c; Lo, Hi : longint);
type LongArray = array[0..0] of longint;

     procedure Sort(const l, r : longint);
     var i, j, k : longint;
         x, y    : longint;
     begin
       i := l;
       j := r;
       k := (l + r) shr 1;
       x := LongArray(c)[k];
       repeat
         while (LongArray(c)[i] < x)
         do inc(i);
         while (x < LongArray(c)[j])
         do dec(j);
         if (i <= j)
         then begin
              y               := LongArray(c)[i];
              LongArray(c)[i] := LongArray(c)[j];
              LongArray(c)[j] := y;
              inc(i);
              dec(j);
         end;
       until (i > j);
       if (l < j)
       then Sort(l, j);
       if (i < r)
       then Sort(i, r);
     end; // End Sort.

begin // Begin QSortLong
  Sort(Lo, Hi);
end; // End QSortLong.


procedure DxyQSort(a, b : PVectorD; Lo, Hi : longint);

     procedure xySort(const l, r : longint);
     var i, j, k : longint;
         x, y    : double;
     begin
       i := l;
       j := r;
       k := (l + r) shr 1;
       x := a^[k];
       repeat
         while (a^[i] < x)
         do inc(i);
         while (x < a^[j])
         do dec(j);
         if (i <= j)
         then begin
              y     := a^[i];
              a^[i] := a^[j];
              a^[j] := y;
              y     := b^[i];
              b^[i] := b^[j];
              b^[j] := y;
              inc(i);
              dec(j);
         end;
       until (i > j);
       if (l < j)
       then xySort(l, j);
       if (i < r)
       then xySort(i, r);
     end; // End xySort.

begin // Begin DxyQSort
  if (Lo < Hi)
  then xySort(Lo, Hi);
end; // End DxyQSort


procedure SwapByte(var A, B : byte);
var C : byte;
begin
  C := A;
  A := B;
  B := C;
end; // SwapByte.


function QuickSelect(Data : PVectorB; DataSize : word) : byte;
type ByteArray = array[0..0] of byte;
var Middle     : word;
    NibbleLow  : word;
    NibbleHigh : word;
	  Low        : word;
	  High       : word;
	  Median     : word;
begin
  // This Quickselect routine is based on the algorithm described in
  // "Numerical recipies in C", Second Edition,
  // Cambridge University Press, 1992, Section 8.5, ISBN 0-521-43108-5

  // 63 % faster than QSort on byte 9, 262144 times.

	Low    := 0;
	High   := DataSize - 1;
	Median := (Low + High) shr 1;
  Result := Median;
	while (True)
	do begin
		 if (High <= Low) // One element in Data
     then begin
          Result := Data[Median];
          Break;
     end;

		 if (High = Low + 1)
		 then begin
		     	// Two elements in Data
			    if (Data[Low] > Data[High])
				  then SwapByte(Data[Low], Data[High]);
			    Result := Data[Median];
          Break;
     end;

		 // Find median of low, middle and high items.
		 Middle := (Low + High) shr 1;
		 if (Data[Middle] > Data[High])
     then SwapByte(Data[Middle], Data[High]);
		 if (Data[Low] > Data[High])
     then SwapByte(Data[Low], Data[High]);
		 if (Data[Middle] > Data[Low])
     then SwapByte(Data[Middle], Data[Low]);

		 // Swap low item (now in position middle) into position (low + 1)
		 SwapByte(Data[Middle], Data[Low+1]);

		 // Nibble from each end towards middle, swapping items when stuck
		 NibbleLow  := Low + 1;
		 NibbleHigh := High;
		 while (True)
		 do begin
        repeat
          inc(NibbleLow)
        until (Data[Low] <= Data[NibbleLow]);

        repeat
          dec(NibbleHigh);
        until (Data[NibbleHigh] <= Data[Low]);

			  if (NibbleHigh < NibbleLow)
			  then Break;

			  SwapByte(Data[NibbleLow], Data[NibbleHigh]);
     end;

		 // Swap middle item (in position low) back into correct position
		 SwapByte(Data[Low], Data[NibbleHigh]);

		 // Re-set active partition
		 if (NibbleHigh <= Median)
     then Low := NibbleLow;
		 if (NibbleHigh >= Median)
     then High := NibbleHigh - 1;
  end;
end; // QuickSelect.


procedure QSortSingle(var c; Lo, Hi : longint);
type SingleArray = array[0..0] of single;

     procedure Sort(const l, r : longint);
     var i, j, k : longint;
         x, y    : single;
     begin
       i := l;
       j := r;
       k := (l + r) shr 1;
       x := SingleArray(c)[k];
       repeat
         while (SingleArray(c)[i] < x)
         do inc(i);
         while (x < SingleArray(c)[j])
         do dec(j);
         if (i <= j)
         then begin
              y              := SingleArray(c)[i];
              SingleArray(c)[i] := SingleArray(c)[j];
              SingleArray(c)[j] := y;
              inc(i);
              dec(j);
         end;
       until (i > j);
       if (l < j)
       then Sort(l, j);
       if (i < r)
       then Sort(i, r);
     end; // End Sort.

begin // Begin QSortInt
  Sort(Lo, Hi);
end; // End QSortInt.



end.

