{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

Unit HeapChk;

{$R-,S-,F+}

Interface
  Procedure InstallHeapErrorFunc;
  Procedure DeInstallHeapErrorFunc;

Implementation

Const
  OldHeapError : Pointer = Nil;

  Function HeapCheck ( ASize : Word ) : Integer; far;
  Begin
    If ( HeapError <> Nil ) Then
      ASM
        PUSH    ASize
        CALL    OldHeapError;
      End;
    HeapCheck := 1;
  End;

  Procedure InstallHeapErrorFunc;
  Begin
    OldHeapError := HeapError;
    HeapError := @HeapCheck;
  End;

  Procedure DeInstallHeapErrorFunc;
  Begin
    If OldHeapError <> Nil Then Begin
      HeapError := OldHeapError;
      OldHeapError := Nil;
    End;
  End;

End.
