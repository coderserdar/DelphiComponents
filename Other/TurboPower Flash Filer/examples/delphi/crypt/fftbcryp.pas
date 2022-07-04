{*********************************************************}
{* FlashFiler: Example table encryption unit with LockBox*}
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
 
(* Development notes:

       This unit replaces the standard FlashFiler encryption unit with
       a user-defined one (the FlashFiler standard FFTBCRYP unit is
       not shipped, either in source code form or in compiled form).
       This will enable you to write your own secure FlashFiler
       Server that'll have encrypted tables that only you can read.

       The FFTBCRYP unit provides four routines (their syntax
       definitions are below in the interface section):

         FFCodeBlock
         FFDecodeBlock
         FFCodeBlockServer
         FFDecodeBlockServer

       The first two encrypt and decrypt a block of table data. Recall
       that a FlashFiler table is a file (or set of files) of many
       blocks, each of which is the same size: 4, 8, 16 or 32KB.
       FFCodeBlock is called just prior to writing one of these blocks
       to disk and so is responsible for encrypting the data.
       FFDecodeBlock is called just after reading a block from disk
       and so is responsible for decrypting the encoded data.

       The second two routines do exactly the same as the first two,
       except that this time the table is one of the server-specific
       tables, and hence must be encrypted in a different fashion. The
       reason for this is to avoid the situation where a user maps an
       alias to the server directory and reads the server-specific
       tables directly: if the server tables are encrypted in a
       different fashion they will be unintelligible and hence only
       usable by the internal code in the server.

       The parameters to the routines are the same in all four cases.
       aBlock is a pointer to the block to be encrypted/decrypted.
       aBlockLen is the length of this block. aRandomizer is a special
       value that is different for different blocks (actually it's the
       block number): this will introduce a 'random' element into the
       encryption/decryption process, ie, each block can be encrypted
       in a slightly different way if you so wish (if you don't, just
       ignore the parameter).

       Tables are encrypted in two situations: firstly when they are
       created with the encryption option set, and secondly if they
       are server-specific tables. (Note that for either of these two
       cases to apply the SecureServer compiler define must be active
       in FFDEFINE.INC, and the server must have been rebuilt.) The
       header record for FlashFiler tables (the first block in each
       table file) is not encrypted: this allows non-secure servers to
       read the header and determine that the table is encrypted. The
       routines you write in this unit will not be called for header
       blocks.

       FlashFiler does not use passwords to encrypt and decrypt table
       data. Consequently, you could try and fashion a 'password' as
       key for your encryption routine from the aRandomizer parameter.
       The example code below uses one technique.

       The encryption routine used in this example unit is the LockBox
       Stream Cipher from TurboPower's LockBox library. LockBox is a
       collection of stream and block cipher algorithms for the Delphi
       programmer; unfortunately, because of US export laws, it is
       only available in the USA. You can replace it with any
       encryption routine you have to hand, a good source for such
       routines (with printed code in C) is Advanced Cryptography by
       Bruce Schneier.

       Simpler alternatives could be XORing a key (a random key?) over
       and over into a block: the longer the key the better. But be
       warned: simpler encryption algorithms are usually more easily
       broken.

       To compile this example FFTBCRYP unit into the FlashFiler
       Server, you must activate the SecureServer compiler define in
       the FFDEFINE.INC file. Open this file, locate the SecureServer
       compiler define and remove the period after the opening brace.
       The compiler define should look like:

         {$DEFINE SecureServer}

       Save and build the entire FFSERVER project. You now have a
       secure server.
*)

{$I ffdefine.inc}

unit fftbcryp;

interface

uses
  SysUtils,
  ffllbase,
  lbcipher; {the LockBox cipher routines}

procedure FFCodeBlock(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
  {-encode a block}
procedure FFCodeBlockServer(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
  {-encode a block for a server table}
procedure FFDecodeBlock(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
  {-decode a block}
procedure FFDecodeBlockServer(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
  {-decode a block for a server table}

implementation

type
  TKey = packed record
    kR : TffWord32;
    kS : array [1..6] of AnsiChar;
  end;

procedure FFCodeBlock(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
var
  Key        : TKey;
  LSCContext : TLSCContext;
begin
  {set up the key}
  Key.kR := aRandomizer;
  Key.kS := 'CLIENT';
  {initialise the LockBox Steam Cipher context}
  InitEncryptLSC(Key, sizeof(Key), LSCContext);
  {using the context, encrypt the block}
  EncryptLSC(LSCContext, aBlock^, aBlockLen);
end;

procedure FFCodeBlockServer(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
var
  Key        : TKey;
  LSCContext : TLSCContext;
begin
  Key.kR := aRandomizer;
  Key.kS := 'SERVER';
  InitEncryptLSC(Key, sizeof(Key), LSCContext);
  EncryptLSC(LSCContext, aBlock^, aBlockLen);
end;

procedure FFDecodeBlock(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
var
  Key        : TKey;
  LSCContext : TLSCContext;
begin
  Key.kR := aRandomizer;
  Key.kS := 'CLIENT';
  InitEncryptLSC(Key, sizeof(Key), LSCContext);
  EncryptLSC(LSCContext, aBlock^, aBlockLen);
end;

procedure FFDecodeBlockServer(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
var
  Key        : TKey;
  LSCContext : TLSCContext;
begin
  Key.kR := aRandomizer;
  Key.kS := 'SERVER';
  InitEncryptLSC(Key, sizeof(Key), LSCContext);
  EncryptLSC(LSCContext, aBlock^, aBlockLen);
end;

end.

