unit Blowunit;
{*****************************************************************************
 UNIT: BlowUnit
 Description:  "Blowfish, a new secret-key block cipher.  It is a
                Feistel network, iterating a simple encryption function 16 times.
                The block size is 64 bits, and the key can be any length up to
                448 bits.  Although there is a complex initialization phase
                required before any encryption can take place, the actual
                encryption of data is very efficient on large microprocessors."
                ...." it is only suitable for applications where the key does
                not change often, like a communications link or an automatic file
                encryptor.  It is significantly faster than DES when implemented
                on 32-bit microprocessors with large data caches, such as the
                Pentium and the PowerPC."

                From BlowFish.Doc, Bruce Schneier Counterpane Systems,
                730 Fair Oaks Ave, Oak Park, IL  60302, schneier@winternet.com

"Blowfish is unpatented, and will remain so in all countries.  The
 algorithm is hereby placed in the public domain, and can be
 freely used by anyone.", Bruce Schneier Counterpane Systems(Algorithm Author)
 (See LEGAL)
 This Object Pascal is largely a port of the reference C Code.
 -----------------------------------------------------------------------------
 Code Author:  Greg Carter, gregc@cryptocard.com
 Organization: CRYPTOCard Corporation, info@cryptocard.com, http://www.cryptocard.com
               R&D Division, Carleton Place, ON, CANADA, K7C 3T2
               1-613-253-3152 Voice, 1-613-253-4685 Fax.
 Date of V.1:  Jan. 3 1996.
 -----------------------------------------------------------------------------}
 {Useage:  Below is typical usage(for File)of the BlowFish Object, Follow these steps:
           1) Declare and Create Variable of type TBlowFish.
           2) Set InputSource Type, either SourceFile, SourceByteArray, or
              SourceString(Pascal style string).
           3) Set Cipher Mode, optionally IVector.
           4) Point to Input Source and set Input Length(If needed)
           5) Point to Output Structure(array, file).
           6) Set Key;
           7) Call BF_EncipherData Method.
           8) Reference the Output. Thats it.
 **** Note **** Steps 2..6 can occure in any order.
 Here is a procedure in Delphi used to encrypt a file:
procedure Tcryptfrm.OpenCiphButtonClick(Sender: TObject);
var
 BlowFish: TBlowFish; (*Step 1*)
begin
BlowFish := TBlowFish.Create;(*Step 1b*)
 try
  If OpenDialog1.Execute then
  begin
   BlowFish.InputType := SourceFile; (*Step 2*)
   BlowFish.CipherMode := ECBMode;   (*Step 3*)
   BlowFish.InputFilePath := OpenDialog1.FileName; (*Step 4*)
   BlowFish.OutputFilePath := ChangeFileExt(OpenDialog1.FileName, '.ccc'); (*Step 5*)
   BlowFish.Key := 'abcdefghijklmnopqrstuvwxyz'; (*Step 6*)
   BlowFish.BF_EncipherData(False);  (*Step 7*)
  end;
 finally
  BlowFish.free;
 end;
end;
-----------------------------------------------------------------------------}
{LEGAL:        The algorithm was placed into the public domain, hence requires
               no license or runtime fees.  However this code is copyright by
               CRYPTOCard.  CRYPTOCard grants anyone who may wish to use, modify
               or redistribute this code privileges to do so, provided the user
               agrees to the following three(3) rules:

               1)Any Applications, (ie exes which make use of this
               Object...), for-profit or non-profit,
               must acknowledge the author of this Object(ie.
               BlowFish Implementation provided by Greg Carter, CRYPTOCard
               Corporation) somewhere in the accompanying Application
               documentation(ie AboutBox, HelpFile, readme...).  NO runtime
               or licensing fees are required!

               2)Any Developer Component(ie Delphi Component, Visual Basic VBX,
               DLL) derived from this software must acknowledge that it is
               derived from "BlowFish Object Pascal Implementation Originated by
               Greg Carter, CRYPTOCard Corporation 1996". Also all efforts should
               be made to point out any changes from the original.
               !!!!!Further, any Developer Components based on this code
               *MAY NOT* be sold for profit.  This Object was placed into the
               public domain, and therefore any derived components should
               also.!!!!!

               3)CRYPTOCard Corporation makes no representations concerning this
               software or the suitability of this software for any particular
               purpose. It is provided "as is" without express or implied
               warranty of any kind. CRYPTOCard accepts no liability from any
               loss or damage as a result of using this software.

CRYPTOCard Corporation is in no way affiliated with Bruce Schneier
(Algorithm Author) or Counterpane Systems.
-----------------------------------------------------------------------------
SECURITY:      BLOWFISH is a relatively new encryption algorithm.  I do not
               think that it has been proven to be either unsecure or secure.

MODE:          ECB(Electronic CodeBook Mode) is used to encipher data.
-----------------------------------------------------------------------------
Why Use this instead of a freely available C DLL?

The goal was to provide a number of Encryption/Hash implementations in Object
Pascal, so that the Pascal Developer has considerably more freedom.  These
Implementations are geared toward the PC(Intel) Microsoft Windows developer,
who will be using Borland's New 32bit developement environment(Delphi32).  The
code generated by this new compiler is considerablely faster then 16bit versions.
And should provide the Developer with faster implementations then those using
C DLLs. Also DLLs are a secruity risk, it is very easy to 'imitate' a DLL.  An
Attacker could replace the C DLL, unknown to your exe.  Now if you call the DLL,
you will mostlikely be passing in the Keys in plaintext.  Now the Attacker has
all the secret keys. Using these routines a Pascal developer can keep all the
Key passing within one exe.
-----------------------------------------------------------------------------
NOTES:       This code is based on the supplied reference C code.
------------------------------------------------------------------------------
Revised:  00/00/00 BY: ******* Reason: ******
------------------------------------------------------------------------------
}
interface
{Declare the compiler defines}
{$I CRYPTDEF.INC}
{------Changeable compiler switches-----------------------------------}
{$A+   Word align variables }
{$F+   Force Far calls }
{$K+   Use smart callbacks
{$N+   Allow coprocessor instructions }
{$P+   Open parameters enabled }
{$S+   Stack checking }
{$T-   @ operator is NOT typed }
{$IFDEF DELPHI}
{$U-   Non Pentium safe FDIV }
{$Z-   No automatic word-sized enumerations}
{$ENDIF}
{---------------------------------------------------------------------}
uses SysUtils, Cryptcon{$IFDEF DELPHI}, Classes{$ENDIF}
     {$IFDEF BP7},objects{$ENDIF};
{,WinTypes, WinProcs,Messages, Graphics,}

type

 twoAword = record
  Xl: aword; {this assumes bytes are read in in MSB, so we have to flip}
  Xr: aword;
 end;

 PtwoAword = ^twoAword;

 Const
  bf_N = 16;
 type

 Pbf_P = ^Tbf_PArray;
 Tbf_PArray = array[0..(bf_N + 1)] of UWORD_32bits;

 Pbf_S = ^Tbf_SArray;
 Tbf_SArray = array[0..3, 0..255] of UWORD_32bits;

{$IFDEF DELPHI}
TBlowFish = class(TCrypto)
 Private
 { Private declarations }
{$ENDIF}
{$IFDEF BP7}
 PTBlowFish = ^TBlowFish; {For BP7 Objects}
 TBlowFish = object(TCrypto)
 Public             {Since BP7 doesn't support Properties, we make these Public}
{$ENDIF}
 Private
  Fpbf_P: Pbf_P;    {Pointer to bf_P array}
  Fpbf_S: Pbf_S;    {Pointer to bf_S array}
  FpXl: Paword;     {Lower 32 bits of Active Encipher Data}
  FpXr: Paword;     {Upper 32 bits of Active Encipher Data}
  Function bf_F(x: Paword): aword;
  Procedure InitArray;
  Procedure ROUND(a, b: Paword; n: BYTE);
  Procedure BF_Initialize;           {Computation of the SubKeys}
  Procedure BF_Encipher;             {Enciphers 64bit block}
  Procedure BF_Decipher;             {Deciphers 64bit block}
{$IFDEF DELPHI}
{$ENDIF}
{$IFDEF BP7}
  Procedure EncipherBLOCK; virtual;{Enciphers Block, calls BF_Encipher}
  Procedure DecipherBLOCK; virtual;{Deciphers Block, calls BF_Decipher}
  Procedure SetKeys;       virtual;{Used to set SubKeys}
{$ENDIF}
{$IFDEF DELPHI}
 protected
    { Protected declarations }
{$ENDIF}
 public
    { Public declarations }
{$IFDEF DELPHI}
  constructor Create(Owner: TComponent);override;
  destructor  Destroy;override;

  Procedure EncipherBLOCK;override; {Enciphers Block, calls BF_Encipher}
  Procedure DecipherBLOCK;override; {Deciphers Block, calls BF_Decipher}
  Procedure SetKeys;override;       {Used to set SubKeys}
{$ENDIF}
{$IFDEF BP7}
  constructor Init;
  destructor  Done;virtual;
{$ENDIF}
end;{TBlowFish}

{$IFDEF DELPHI}
 procedure Register;{register the component to the Delphi toolbar}
{$ENDIF}
Type
  uw = UWORD_32bits;
 Const
 {bf_N = 16;}
 KEYBYTES = 8;
 MAXKEYBYTES = 56;
 BF_MAXKEYLENGTH = 65;
 BF_MINKEYLENGTH = 8;

 bf_P: array[0..(bf_N + 1)] of UWORD_32bits = (
  uw($243f6a88), uw($85a308d3), uw($13198a2e), uw($03707344),
  uw($a4093822), uw($299f31d0), uw($082efa98), uw($ec4e6c89),
  uw($452821e6), uw($38d01377), uw($be5466cf), uw($34e90c6c),
  uw($c0ac29b7), uw($c97c50dd), uw($3f84d5b5), uw($b5470917),
  uw($9216d5d9), uw($8979fb1b));
{Initial Random SubKey boxes, set to Pi}
 bf_S: array[0..3, 0..255] of UWORD_32bits =
(
( uw($d1310ba6), uw($98dfb5ac), uw($2ffd72db), uw($d01adfb7),
  uw($b8e1afed), uw($6a267e96), uw($ba7c9045), uw($f12c7f99),
  uw($24a19947), uw($b3916cf7), uw($0801f2e2), uw($858efc16),
  uw($636920d8), uw($71574e69), uw($a458fea3), uw($f4933d7e),

  uw($0d95748f), uw($728eb658), uw($718bcd58), uw($82154aee),
  uw($7b54a41d), uw($c25a59b5), uw($9c30d539), uw($2af26013),
  uw($c5d1b023), uw($286085f0), uw($ca417918), uw($b8db38ef),
  uw($8e79dcb0), uw($603a180e), uw($6c9e0e8b), uw($b01e8a3e),

  uw($d71577c1), uw($bd314b27), uw($78af2fda), uw($55605c60),
  uw($e65525f3), uw($aa55ab94), uw($57489862), uw($63e81440),
  uw($55ca396a), uw($2aab10b6), uw($b4cc5c34), uw($1141e8ce),
  uw($a15486af), uw($7c72e993), uw($b3ee1411), uw($636fbc2a),

  uw($2ba9c55d), uw($741831f6), uw($ce5c3e16), uw($9b87931e),
  uw($afd6ba33), uw($6c24cf5c), uw($7a325381), uw($28958677),
  uw($3b8f4898), uw($6b4bb9af), uw($c4bfe81b), uw($66282193),
  uw($61d809cc), uw($fb21a991), uw($487cac60), uw($5dec8032),

  uw($ef845d5d), uw($e98575b1), uw($dc262302), uw($eb651b88),
  uw($23893e81), uw($d396acc5), uw($0f6d6ff3), uw($83f44239),
  uw($2e0b4482), uw($a4842004), uw($69c8f04a), uw($9e1f9b5e),
  uw($21c66842), uw($f6e96c9a), uw($670c9c61), uw($abd388f0),

  uw($6a51a0d2), uw($d8542f68), uw($960fa728), uw($ab5133a3),
  uw($6eef0b6c), uw($137a3be4), uw($ba3bf050), uw($7efb2a98),
  uw($a1f1651d), uw($39af0176), uw($66ca593e), uw($82430e88),
  uw($8cee8619), uw($456f9fb4), uw($7d84a5c3), uw($3b8b5ebe),

  uw($e06f75d8), uw($85c12073), uw($401a449f), uw($56c16aa6),
  uw($4ed3aa62), uw($363f7706), uw($1bfedf72), uw($429b023d),
  uw($37d0d724), uw($d00a1248), uw($db0fead3), uw($49f1c09b),
  uw($075372c9), uw($80991b7b), uw($25d479d8), uw($f6e8def7),

  uw($e3fe501a), uw($b6794c3b), uw($976ce0bd), uw($04c006ba),
  uw($c1a94fb6), uw($409f60c4), uw($5e5c9ec2), uw($196a2463),
  uw($68fb6faf), uw($3e6c53b5), uw($1339b2eb), uw($3b52ec6f),
  uw($6dfc511f), uw($9b30952c), uw($cc814544), uw($af5ebd09),

  uw($bee3d004), uw($de334afd), uw($660f2807), uw($192e4bb3),
  uw($c0cba857), uw($45c8740f), uw($d20b5f39), uw($b9d3fbdb),
  uw($5579c0bd), uw($1a60320a), uw($d6a100c6), uw($402c7279),
  uw($679f25fe), uw($fb1fa3cc), uw($8ea5e9f8), uw($db3222f8),

  uw($3c7516df), uw($fd616b15), uw($2f501ec8), uw($ad0552ab),
  uw($323db5fa), uw($fd238760), uw($53317b48), uw($3e00df82),
  uw($9e5c57bb), uw($ca6f8ca0), uw($1a87562e), uw($df1769db),
  uw($d542a8f6), uw($287effc3), uw($ac6732c6), uw($8c4f5573),

  uw($695b27b0), uw($bbca58c8), uw($e1ffa35d), uw($b8f011a0),
  uw($10fa3d98), uw($fd2183b8), uw($4afcb56c), uw($2dd1d35b),
  uw($9a53e479), uw($b6f84565), uw($d28e49bc), uw($4bfb9790),
  uw($e1ddf2da), uw($a4cb7e33), uw($62fb1341), uw($cee4c6e8),

  uw($ef20cada), uw($36774c01), uw($d07e9efe), uw($2bf11fb4),
  uw($95dbda4d), uw($ae909198), uw($eaad8e71), uw($6b93d5a0),
  uw($d08ed1d0), uw($afc725e0), uw($8e3c5b2f), uw($8e7594b7),
  uw($8ff6e2fb), uw($f2122b64), uw($8888b812), uw($900df01c),

  uw($4fad5ea0), uw($688fc31c), uw($d1cff191), uw($b3a8c1ad),
  uw($2f2f2218), uw($be0e1777), uw($ea752dfe), uw($8b021fa1),
  uw($e5a0cc0f), uw($b56f74e8), uw($18acf3d6), uw($ce89e299),
  uw($b4a84fe0), uw($fd13e0b7), uw($7cc43b81), uw($d2ada8d9),

  uw($165fa266), uw($80957705), uw($93cc7314), uw($211a1477),
  uw($e6ad2065), uw($77b5fa86), uw($c75442f5), uw($fb9d35cf),
  uw($ebcdaf0c), uw($7b3e89a0), uw($d6411bd3), uw($ae1e7e49),
  uw($00250e2d), uw($2071b35e), uw($226800bb), uw($57b8e0af),

  uw($2464369b), uw($f009b91e), uw($5563911d), uw($59dfa6aa),
  uw($78c14389), uw($d95a537f), uw($207d5ba2), uw($02e5b9c5),
  uw($83260376), uw($6295cfa9), uw($11c81968), uw($4e734a41),
  uw($b3472dca), uw($7b14a94a), uw($1b510052), uw($9a532915),

  uw($d60f573f), uw($bc9bc6e4), uw($2b60a476), uw($81e67400),
  uw($08ba6fb5), uw($571be91f), uw($f296ec6b), uw($2a0dd915),
  uw($b6636521), uw($e7b9f9b6), uw($ff34052e), uw($c5855664),
  uw($53b02d5d), uw($a99f8fa1), uw($08ba4799), uw($6e85076a)),
  {second 256}
 (uw($4b7a70e9), uw($b5b32944), uw($db75092e), uw($c4192623),
  uw($ad6ea6b0), uw($49a7df7d), uw($9cee60b8), uw($8fedb266),
  uw($ecaa8c71), uw($699a17ff), uw($5664526c), uw($c2b19ee1),
  uw($193602a5), uw($75094c29), uw($a0591340), uw($e4183a3e),

  uw($3f54989a), uw($5b429d65), uw($6b8fe4d6), uw($99f73fd6),
  uw($a1d29c07), uw($efe830f5), uw($4d2d38e6), uw($f0255dc1),
  uw($4cdd2086), uw($8470eb26), uw($6382e9c6), uw($021ecc5e),
  uw($09686b3f), uw($3ebaefc9), uw($3c971814), uw($6b6a70a1),

  uw($687f3584), uw($52a0e286), uw($b79c5305), uw($aa500737),
  uw($3e07841c), uw($7fdeae5c), uw($8e7d44ec), uw($5716f2b8),
  uw($b03ada37), uw($f0500c0d), uw($f01c1f04), uw($0200b3ff),
  uw($ae0cf51a), uw($3cb574b2), uw($25837a58), uw($dc0921bd),

  uw($d19113f9), uw($7ca92ff6), uw($94324773), uw($22f54701),
  uw($3ae5e581), uw($37c2dadc), uw($c8b57634), uw($9af3dda7),
  uw($a9446146), uw($0fd0030e), uw($ecc8c73e), uw($a4751e41),
  uw($e238cd99), uw($3bea0e2f), uw($3280bba1), uw($183eb331),

  uw($4e548b38), uw($4f6db908), uw($6f420d03), uw($f60a04bf),
  uw($2cb81290), uw($24977c79), uw($5679b072), uw($bcaf89af),
  uw($de9a771f), uw($d9930810), uw($b38bae12), uw($dccf3f2e),
  uw($5512721f), uw($2e6b7124), uw($501adde6), uw($9f84cd87),

  uw($7a584718), uw($7408da17), uw($bc9f9abc), uw($e94b7d8c),
  uw($ec7aec3a), uw($db851dfa), uw($63094366), uw($c464c3d2),
  uw($ef1c1847), uw($3215d908), uw($dd433b37), uw($24c2ba16),
  uw($12a14d43), uw($2a65c451), uw($50940002), uw($133ae4dd),

  uw($71dff89e), uw($10314e55), uw($81ac77d6), uw($5f11199b),
  uw($043556f1), uw($d7a3c76b), uw($3c11183b), uw($5924a509),
  uw($f28fe6ed), uw($97f1fbfa), uw($9ebabf2c), uw($1e153c6e),
  uw($86e34570), uw($eae96fb1), uw($860e5e0a), uw($5a3e2ab3),

  uw($771fe71c), uw($4e3d06fa), uw($2965dcb9), uw($99e71d0f),
  uw($803e89d6), uw($5266c825), uw($2e4cc978), uw($9c10b36a),
  uw($c6150eba), uw($94e2ea78), uw($a5fc3c53), uw($1e0a2df4),
  uw($f2f74ea7), uw($361d2b3d), uw($1939260f), uw($19c27960),

  uw($5223a708), uw($f71312b6), uw($ebadfe6e), uw($eac31f66),
  uw($e3bc4595), uw($a67bc883), uw($b17f37d1), uw($018cff28),
  uw($c332ddef), uw($be6c5aa5), uw($65582185), uw($68ab9802),
  uw($eecea50f), uw($db2f953b), uw($2aef7dad), uw($5b6e2f84),

  uw($1521b628), uw($29076170), uw($ecdd4775), uw($619f1510),
  uw($13cca830), uw($eb61bd96), uw($0334fe1e), uw($aa0363cf),
  uw($b5735c90), uw($4c70a239), uw($d59e9e0b), uw($cbaade14),
  uw($eecc86bc), uw($60622ca7), uw($9cab5cab), uw($b2f3846e),

  uw($648b1eaf), uw($19bdf0ca), uw($a02369b9), uw($655abb50),
  uw($40685a32), uw($3c2ab4b3), uw($319ee9d5), uw($c021b8f7),
  uw($9b540b19), uw($875fa099), uw($95f7997e), uw($623d7da8),
  uw($f837889a), uw($97e32d77), uw($11ed935f), uw($16681281),

  uw($0e358829), uw($c7e61fd6), uw($96dedfa1), uw($7858ba99),
  uw($57f584a5), uw($1b227263), uw($9b83c3ff), uw($1ac24696),
  uw($cdb30aeb), uw($532e3054), uw($8fd948e4), uw($6dbc3128),
  uw($58ebf2ef), uw($34c6ffea), uw($fe28ed61), uw($ee7c3c73),

  uw($5d4a14d9), uw($e864b7e3), uw($42105d14), uw($203e13e0),
  uw($45eee2b6), uw($a3aaabea), uw($db6c4f15), uw($facb4fd0),
  uw($c742f442), uw($ef6abbb5), uw($654f3b1d), uw($41cd2105),
  uw($d81e799e), uw($86854dc7), uw($e44b476a), uw($3d816250),

  uw($cf62a1f2), uw($5b8d2646), uw($fc8883a0), uw($c1c7b6a3),
  uw($7f1524c3), uw($69cb7492), uw($47848a0b), uw($5692b285),
  uw($095bbf00), uw($ad19489d), uw($1462b174), uw($23820e00),
  uw($58428d2a), uw($0c55f5ea), uw($1dadf43e), uw($233f7061),

  uw($3372f092), uw($8d937e41), uw($d65fecf1), uw($6c223bdb),
  uw($7cde3759), uw($cbee7460), uw($4085f2a7), uw($ce77326e),
  uw($a6078084), uw($19f8509e), uw($e8efd855), uw($61d99735),
  uw($a969a7aa), uw($c50c06c2), uw($5a04abfc), uw($800bcadc),

  uw($9e447a2e), uw($c3453484), uw($fdd56705), uw($0e1e9ec9),
  uw($db73dbd3), uw($105588cd), uw($675fda79), uw($e3674340),
  uw($c5c43465), uw($713e38d8), uw($3d28f89e), uw($f16dff20),
  uw($153e21e7), uw($8fb03d4a), uw($e6e39f2b), uw($db83adf7)),
  {Thrid 256}
 (uw($e93d5a68), uw($948140f7), uw($f64c261c), uw($94692934),
  uw($411520f7), uw($7602d4f7), uw($bcf46b2e), uw($d4a20068),
  uw($d4082471), uw($3320f46a), uw($43b7d4b7), uw($500061af),
  uw($1e39f62e), uw($97244546), uw($14214f74), uw($bf8b8840),

  uw($4d95fc1d), uw($96b591af), uw($70f4ddd3), uw($66a02f45),
  uw($bfbc09ec), uw($03bd9785), uw($7fac6dd0), uw($31cb8504),
  uw($96eb27b3), uw($55fd3941), uw($da2547e6), uw($abca0a9a),
  uw($28507825), uw($530429f4), uw($0a2c86da), uw($e9b66dfb),

  uw($68dc1462), uw($d7486900), uw($680ec0a4), uw($27a18dee),
  uw($4f3ffea2), uw($e887ad8c), uw($b58ce006), uw($7af4d6b6),
  uw($aace1e7c), uw($d3375fec), uw($ce78a399), uw($406b2a42),
  uw($20fe9e35), uw($d9f385b9), uw($ee39d7ab), uw($3b124e8b),

  uw($1dc9faf7), uw($4b6d1856), uw($26a36631), uw($eae397b2),
  uw($3a6efa74), uw($dd5b4332), uw($6841e7f7), uw($ca7820fb),
  uw($fb0af54e), uw($d8feb397), uw($454056ac), uw($ba489527),
  uw($55533a3a), uw($20838d87), uw($fe6ba9b7), uw($d096954b),

  uw($55a867bc), uw($a1159a58), uw($cca92963), uw($99e1db33),
  uw($a62a4a56), uw($3f3125f9), uw($5ef47e1c), uw($9029317c),
  uw($fdf8e802), uw($04272f70), uw($80bb155c), uw($05282ce3),
  uw($95c11548), uw($e4c66d22), uw($48c1133f), uw($c70f86dc),

  uw($07f9c9ee), uw($41041f0f), uw($404779a4), uw($5d886e17),
  uw($325f51eb), uw($d59bc0d1), uw($f2bcc18f), uw($41113564),
  uw($257b7834), uw($602a9c60), uw($dff8e8a3), uw($1f636c1b),
  uw($0e12b4c2), uw($02e1329e), uw($af664fd1), uw($cad18115),

  uw($6b2395e0), uw($333e92e1), uw($3b240b62), uw($eebeb922),
  uw($85b2a20e), uw($e6ba0d99), uw($de720c8c), uw($2da2f728),
  uw($d0127845), uw($95b794fd), uw($647d0862), uw($e7ccf5f0),
  uw($5449a36f), uw($877d48fa), uw($c39dfd27), uw($f33e8d1e),

  uw($0a476341), uw($992eff74), uw($3a6f6eab), uw($f4f8fd37),
  uw($a812dc60), uw($a1ebddf8), uw($991be14c), uw($db6e6b0d),
  uw($c67b5510), uw($6d672c37), uw($2765d43b), uw($dcd0e804),
  uw($f1290dc7), uw($cc00ffa3), uw($b5390f92), uw($690fed0b),

  uw($667b9ffb), uw($cedb7d9c), uw($a091cf0b), uw($d9155ea3),
  uw($bb132f88), uw($515bad24), uw($7b9479bf), uw($763bd6eb),
  uw($37392eb3), uw($cc115979), uw($8026e297), uw($f42e312d),
  uw($6842ada7), uw($c66a2b3b), uw($12754ccc), uw($782ef11c),

  uw($6a124237), uw($b79251e7), uw($06a1bbe6), uw($4bfb6350),
  uw($1a6b1018), uw($11caedfa), uw($3d25bdd8), uw($e2e1c3c9),
  uw($44421659), uw($0a121386), uw($d90cec6e), uw($d5abea2a),
  uw($64af674e), uw($da86a85f), uw($bebfe988), uw($64e4c3fe),

  uw($9dbc8057), uw($f0f7c086), uw($60787bf8), uw($6003604d),
  uw($d1fd8346), uw($f6381fb0), uw($7745ae04), uw($d736fccc),
  uw($83426b33), uw($f01eab71), uw($b0804187), uw($3c005e5f),
  uw($77a057be), uw($bde8ae24), uw($55464299), uw($bf582e61),

  uw($4e58f48f), uw($f2ddfda2), uw($f474ef38), uw($8789bdc2),
  uw($5366f9c3), uw($c8b38e74), uw($b475f255), uw($46fcd9b9),
  uw($7aeb2661), uw($8b1ddf84), uw($846a0e79), uw($915f95e2),
  uw($466e598e), uw($20b45770), uw($8cd55591), uw($c902de4c),

  uw($b90bace1), uw($bb8205d0), uw($11a86248), uw($7574a99e),
  uw($b77f19b6), uw($e0a9dc09), uw($662d09a1), uw($c4324633),
  uw($e85a1f02), uw($09f0be8c), uw($4a99a025), uw($1d6efe10),
  uw($1ab93d1d), uw($0ba5a4df), uw($a186f20f), uw($2868f169),

  uw($dcb7da83), uw($573906fe), uw($a1e2ce9b), uw($4fcd7f52),
  uw($50115e01), uw($a70683fa), uw($a002b5c4), uw($0de6d027),
  uw($9af88c27), uw($773f8641), uw($c3604c06), uw($61a806b5),
  uw($f0177a28), uw($c0f586e0), uw($006058aa), uw($30dc7d62),

  uw($11e69ed7), uw($2338ea63), uw($53c2dd94), uw($c2c21634),
  uw($bbcbee56), uw($90bcb6de), uw($ebfc7da1), uw($ce591d76),
  uw($6f05e409), uw($4b7c0188), uw($39720a3d), uw($7c927c24),
  uw($86e3725f), uw($724d9db9), uw($1ac15bb4), uw($d39eb8fc),

  uw($ed545578), uw($08fca5b5), uw($d83d7cd3), uw($4dad0fc4),
  uw($1e50ef5e), uw($b161e6f8), uw($a28514d9), uw($6c51133c),
  uw($6fd5c7e7), uw($56e14ec4), uw($362abfce), uw($ddc6c837),
  uw($d79a3234), uw($92638212), uw($670efa8e), uw($406000e0)),
  {Fourth 256}
 (uw($3a39ce37), uw($d3faf5cf), uw($abc27737), uw($5ac52d1b),
  uw($5cb0679e), uw($4fa33742), uw($d3822740), uw($99bc9bbe),
  uw($d5118e9d), uw($bf0f7315), uw($d62d1c7e), uw($c700c47b),
  uw($b78c1b6b), uw($21a19045), uw($b26eb1be), uw($6a366eb4),

  uw($5748ab2f), uw($bc946e79), uw($c6a376d2), uw($6549c2c8),
  uw($530ff8ee), uw($468dde7d), uw($d5730a1d), uw($4cd04dc6),
  uw($2939bbdb), uw($a9ba4650), uw($ac9526e8), uw($be5ee304),
  uw($a1fad5f0), uw($6a2d519a), uw($63ef8ce2), uw($9a86ee22),

  uw($c089c2b8), uw($43242ef6), uw($a51e03aa), uw($9cf2d0a4),
  uw($83c061ba), uw($9be96a4d), uw($8fe51550), uw($ba645bd6),
  uw($2826a2f9), uw($a73a3ae1), uw($4ba99586), uw($ef5562e9),
  uw($c72fefd3), uw($f752f7da), uw($3f046f69), uw($77fa0a59),

  uw($80e4a915), uw($87b08601), uw($9b09e6ad), uw($3b3ee593),
  uw($e990fd5a), uw($9e34d797), uw($2cf0b7d9), uw($022b8b51),
  uw($96d5ac3a), uw($017da67d), uw($d1cf3ed6), uw($7c7d2d28),
  uw($1f9f25cf), uw($adf2b89b), uw($5ad6b472), uw($5a88f54c),

  uw($e029ac71), uw($e019a5e6), uw($47b0acfd), uw($ed93fa9b),
  uw($e8d3c48d), uw($283b57cc), uw($f8d56629), uw($79132e28),
  uw($785f0191), uw($ed756055), uw($f7960e44), uw($e3d35e8c),
  uw($15056dd4), uw($88f46dba), uw($03a16125), uw($0564f0bd),

  uw($c3eb9e15), uw($3c9057a2), uw($97271aec), uw($a93a072a),
  uw($1b3f6d9b), uw($1e6321f5), uw($f59c66fb), uw($26dcf319),
  uw($7533d928), uw($b155fdf5), uw($03563482), uw($8aba3cbb),
  uw($28517711), uw($c20ad9f8), uw($abcc5167), uw($ccad925f),

  uw($4de81751), uw($3830dc8e), uw($379d5862), uw($9320f991),
  uw($ea7a90c2), uw($fb3e7bce), uw($5121ce64), uw($774fbe32),
  uw($a8b6e37e), uw($c3293d46), uw($48de5369), uw($6413e680),
  uw($a2ae0810), uw($dd6db224), uw($69852dfd), uw($09072166),

  uw($b39a460a), uw($6445c0dd), uw($586cdecf), uw($1c20c8ae),
  uw($5bbef7dd), uw($1b588d40), uw($ccd2017f), uw($6bb4e3bb),
  uw($dda26a7e), uw($3a59ff45), uw($3e350a44), uw($bcb4cdd5),
  uw($72eacea8), uw($fa6484bb), uw($8d6612ae), uw($bf3c6f47),

  uw($d29be463), uw($542f5d9e), uw($aec2771b), uw($f64e6370),
  uw($740e0d8d), uw($e75b1357), uw($f8721671), uw($af537d5d),
  uw($4040cb08), uw($4eb4e2cc), uw($34d2466a), uw($0115af84),
  uw($e1b00428), uw($95983a1d), uw($06b89fb4), uw($ce6ea048),

  uw($6f3f3b82), uw($3520ab82), uw($011a1d4b), uw($277227f8),
  uw($611560b1), uw($e7933fdc), uw($bb3a792b), uw($344525bd),
  uw($a08839e1), uw($51ce794b), uw($2f32c9b7), uw($a01fbac9),
  uw($e01cc87e), uw($bcc7d1f6), uw($cf0111c3), uw($a1e8aac7),

  uw($1a908749), uw($d44fbd9a), uw($d0dadecb), uw($d50ada38),
  uw($0339c32a), uw($c6913667), uw($8df9317c), uw($e0b12b4f),
  uw($f79e59b7), uw($43f5bb3a), uw($f2d519ff), uw($27d9459c),
  uw($bf97222c), uw($15e6fc2a), uw($0f91fc71), uw($9b941525),

  uw($fae59361), uw($ceb69ceb), uw($c2a86459), uw($12baa8d1),
  uw($b6c1075e), uw($e3056a0c), uw($10d25065), uw($cb03a442),
  uw($e0ec6e0e), uw($1698db3b), uw($4c98a0be), uw($3278e964),
  uw($9f1f9532), uw($e0d392df), uw($d3a0342b), uw($8971f21e),

  uw($1b0a7441), uw($4ba3348c), uw($c5be7120), uw($c37632d8),
  uw($df359f8d), uw($9b992f2e), uw($e60b6f47), uw($0fe3f11d),
  uw($e54cda54), uw($1edad891), uw($ce6279cf), uw($cd3e7e6f),
  uw($1618b166), uw($fd2c1d05), uw($848fd2c5), uw($f6fb2299),

  uw($f523f357), uw($a6327623), uw($93a83531), uw($56cccd02),
  uw($acf08162), uw($5a75ebb5), uw($6e163697), uw($88d273cc),
  uw($de966292), uw($81b949d0), uw($4c50901b), uw($71c65614),
  uw($e6c6c7bd), uw($327a140a), uw($45e1d006), uw($c3f27b9a),

  uw($c9aa53fd), uw($62a80f00), uw($bb25bfe2), uw($35bdd2f6),
  uw($71126905), uw($b2040222), uw($b6cbcf7c), uw($cd769c2b),
  uw($53113ec0), uw($1640e3d3), uw($38abbd60), uw($2547adf0),
  uw($ba38209c), uw($f746ce76), uw($77afa1c5), uw($20756060),

  uw($85cbfe4e), uw($8ae88dd8), uw($7aaaf9b0), uw($4cf9aa7e),
  uw($1948c25c), uw($02fb8a8c), uw($01c36ae4), uw($d6ebe1f9),
  uw($90d4f869), uw($a65cdea0), uw($3f09252d), uw($c208e69f),
  uw($b74e6132), uw($ce77e25b), uw($578fdfe3), uw($3ac372e6)){,}
);{bf_S}

implementation

{$IFDEF DELPHI}
procedure Register;
  {Registers the Component to the toobar, on the tab named 'Crypto'}
  {Now all a Delphi programmer needs to do is drag n drop to have
   Blowfish encryption}
begin
  RegisterComponents('Crypto', [TBlowfish]);
end;
{$ENDIF}

{$IFDEF DELPHI}
constructor TBlowFish.Create(Owner: TComponent);
{$ENDIF}
{$IFDEF BP7}
constructor TBlowFish.Init;
{$ENDIF}
 begin
  FBLOCKSIZE := SizeOf(aword) * 2;
  New(Fpbf_P);  {Pointer to bf_P array}
  New(Fpbf_S);  {Pointer to bf_S array}
  FIVTemp := nil;
{  GetMem(FIVTemp, FBLOCKSIZE);}
 {$IFDEF DELPHI}
  inherited Create(Owner);
 {$ENDIF}

end;{TBlowFish.Create}

{$IFDEF DELPHI}
destructor TBlowFish.Destroy;
{$ENDIF}
{$IFDEF BP7}
destructor TBlowFish.Done;
{$ENDIF}
 begin
  If Fpbf_P <> nil then Dispose(Fpbf_P);
  If Fpbf_S <> nil then Dispose(Fpbf_S);
 { FreeMem(FIVTemp, FBLOCKSIZE);}
  {$IFDEF DELPHI}
  inherited Destroy;
  {$ENDIF}
end;{TBlowFish.Destroy;}


Procedure TBlowFish.InitArray;
{ PURPOSE:  Since the bf_S and bf_P Arrays are modified during the execution
            of the BlowFish algorithm, it is necessary to copy them to dynamic
            variables. This is only done once per encryption/decryption, or
            when the Key is changed}

var i, j: integer;
begin
  {FIXME: Just use a 'Move' function????? for array initialization}
  For i:= 0 to (bf_N + 1) do begin
   Fpbf_P^[i] := bf_P[i];
  end;

  For i:= 0 to 3 do begin
   for j:= 0 to 255 do begin
    Fpbf_S^[i, j] := bf_S[i, j];
   end;{j while}
  end;{i for}

end;{TBlowFish.InitArray}

Procedure TBlowFish.SetKeys;
var
 kLength: integer;
begin
 kLength := Length(FKey);
 If (kLength < BF_MINKEYLENGTH) or (kLength > BF_MAXKEYLENGTH) then
{FIXME: Should signal an error of some kind, for Delphi use Exceptions}
    FKey := '';
 InitArray;    {Set Sub Key Box arrays back to initial values}
 BF_Initialize;{Generate SBoxs with new Key}
end;{TBlowFish.SetKey}

function TBlowFish.bf_F(x: Paword): aword;
 {FIXME! Put in 'ROUND' to speed up.}
 {FIXED}
 begin
   bf_F.Lword := ((Fpbf_S^[0, x^.w.byte0] + Fpbf_S^[1, x^.w.byte1])
   Xor Fpbf_S^[2, x^.w.byte2]) + Fpbf_S^[3, x^.w.byte3];
end;{TBlowFish.bf_F}

Procedure TBlowFish.ROUND(a, b: Paword; n: BYTE);
var
 bF : aword;
 begin
   bF.Lword := ((Fpbf_S^[0, b^.w.byte0] + Fpbf_S^[1, b^.w.byte1])
   Xor Fpbf_S^[2, b^.w.byte2]) + Fpbf_S^[3, b^.w.byte3];
   a^.Lword := a^.Lword Xor (bF.Lword Xor Fpbf_P^[n]);
end;{TBlowFish.ROUND}

Procedure TBlowFish.BF_Initialize;
 var
  i, j: integer;
  data, datal, datar: UWORD_32bits;
  temp: aword;
  Key: PChar;

 begin
 If Length(FKey) = 0 then exit;{divide by zero, ohh}

 {$IFDEF DELPHI}
   Key := StrAlloc(Length(FKey) + 1);
   try {protect dyanmic memory allocation}
   StrPCopy(Key, FKey);
 {$ENDIF}
 {$IFDEF BP7}
   GetMem(Key,Length(FKEY));
   Move(FKey[1], Key^, Length(FKEY));
 {$ENDIF}

  j := 0;

  For i:= 0 to (bf_N + 1) do begin
    temp.Lword := 0;
    temp.w.byte0 := BYTE(Key[j]);
    temp.w.byte1 := BYTE(Key[(j+1) MOD Length(FKey)]);
    temp.w.byte2 := BYTE(Key[(j+2) MOD Length(FKey)]);
    temp.w.byte3 := BYTE(Key[(j+3) MOD Length(FKey)]);
    data := temp.Lword;
    Fpbf_P^[i] := Fpbf_P^[i] Xor data;
    j := (j + 4) MOD Length(FKey);
  end;{For}

  {zeroize copy of Key for security}
  FillChar(Key^, Length(FKey), #0);

  datal := 0; FpXl := @datal;
  datar := 0; FpXr := @datar;

  i:= 0;
  while (i < (bf_N + 2)) do begin
   BF_Encipher;
   Fpbf_P^[i] := datal;
   Fpbf_P^[i + 1] := datar;
   Inc(i, 2);
  end;

  j:=0;
  For i:= 0 to 3 do begin
   while (j < 256) do begin
    BF_Encipher;
    Fpbf_S^[i, j] := datal;
    Fpbf_S^[i, j + 1] := datar;
    Inc(j, 2);
   end;{j while}
   j:= 0;
  end;{i for}
{$IFDEF DELPHI}
  finally
   StrDispose(Key);
  end;{Finally}
{$ENDIF}
{$IFDEF BP7}
   FreeMem(Key,Length(FKEY));
{$ENDIF}
end;{TBlowFish.BF_Initialize}

Procedure TBlowFish.BF_Encipher;
{We can't flip(intel) the bytes here, because BF_Encipher is used to generate the
 SubKeys, Key generation does not require byte flipping, since it intrepreted
 as an ascii string}

 var
  Xr: aword;
 begin

  FpXl^.Lword := FpXl^.Lword Xor Fpbf_P^[0];
  ROUND (FpXr, FpXl, 1);  ROUND (FpXl, FpXr, 2);
  ROUND (FpXr, FpXl, 3);  ROUND (FpXl, FpXr, 4);
  ROUND (FpXr, FpXl, 5);  ROUND (FpXl, FpXr, 6);
  ROUND (FpXr, FpXl, 7);  ROUND (FpXl, FpXr, 8);
  ROUND (FpXr, FpXl, 9);  ROUND (FpXl, FpXr, 10);
  ROUND (FpXr, FpXl, 11); ROUND (FpXl, FpXr, 12);
  ROUND (FpXr, FpXl, 13); ROUND (FpXl, FpXr, 14);
  ROUND (FpXr, FpXl, 15); ROUND (FpXl, FpXr, 16);
  FpXr^.Lword := FpXr^.Lword Xor Fpbf_P^[17];

  Xr := FpXr^;
  FpXr^ := FpXl^;
  FpXl^ := Xr;
end;{TBlowFish.BF_Encipher}

Procedure TBlowFish.BF_Decipher;
 var
  Xr: aword;
 begin
 {Flip the bytes around}
 {Despite what you might think, this does NOT
  slow the implementation down, it adds about
  200ms per MegaByte of data encrypted.}
 {we either do this, or read in one byte at a
  time and shift left}
{Intelx86}
{$IFDEF ORDER_DCBA}
    Xr := FpXl^;
    FpXl^.w.byte3 := Xr.w.byte0;
    FpXl^.w.byte2 := Xr.w.byte1;
    FpXl^.w.byte1 := Xr.w.byte2;
    FpXl^.w.byte0 := Xr.w.byte3;
    Xr := FpXr^;
    FpXr^.w.byte3 := Xr.w.byte0;
    FpXr^.w.byte2 := Xr.w.byte1;
    FpXr^.w.byte1 := Xr.w.byte2;
    FpXr^.w.byte0 := Xr.w.byte3;
{$ENDIF}

  FpXl^.Lword := FpXl^.Lword Xor Fpbf_P^[17];
  ROUND (FpXr, FpXl, 16);  ROUND (FpXl, FpXr, 15);
  ROUND (FpXr, FpXl, 14);  ROUND (FpXl, FpXr, 13);
  ROUND (FpXr, FpXl, 12);  ROUND (FpXl, FpXr, 11);
  ROUND (FpXr, FpXl, 10);  ROUND (FpXl, FpXr, 9);
  ROUND (FpXr, FpXl, 8);   ROUND (FpXl, FpXr, 7);
  ROUND (FpXr, FpXl, 6);   ROUND (FpXl, FpXr, 5);
  ROUND (FpXr, FpXl, 4);   ROUND (FpXl, FpXr, 3);
  ROUND (FpXr, FpXl, 2);   ROUND (FpXl, FpXr, 1);
  FpXr^.Lword := FpXr^.Lword Xor Fpbf_P^[0];

  Xr := FpXr^;
  FpXr^ := FpXl^;
  FpXl^ := Xr;
{Intelx86}
{$IFDEF ORDER_DCBA}
    Xr := FpXl^;
    FpXl^.w.byte3 := Xr.w.byte0;
    FpXl^.w.byte2 := Xr.w.byte1;
    FpXl^.w.byte1 := Xr.w.byte2;
    FpXl^.w.byte0 := Xr.w.byte3;
    Xr := FpXr^;
    FpXr^.w.byte3 := Xr.w.byte0;
    FpXr^.w.byte2 := Xr.w.byte1;
    FpXr^.w.byte1 := Xr.w.byte2;
    FpXr^.w.byte0 := Xr.w.byte3;
{$ENDIF}
end;{TBlowFish.BF_Decipher}

Procedure TBlowFish.EncipherBLOCK;
{FSmallBuffer holds the clear text, on exit it holds the Cipher text}
 var
  pLong64: PtwoAword;
  leLong64: twoAword;
 begin
  pLong64 := @FSmallBuffer;
  FpXl := @pLong64^.Xl;
  FpXr := @pLong64^.Xr;

{Intelx86}
{$IFDEF ORDER_DCBA}
    leLong64 := pLong64^;
    pLong64^.Xl.w.byte3 := leLong64.Xl.w.byte0;
    pLong64^.Xl.w.byte2 := leLong64.Xl.w.byte1;
    pLong64^.Xl.w.byte1 := leLong64.Xl.w.byte2;
    pLong64^.Xl.w.byte0 := leLong64.Xl.w.byte3;

    pLong64^.Xr.w.byte3 := leLong64.Xr.w.byte0;
    pLong64^.Xr.w.byte2 := leLong64.Xr.w.byte1;
    pLong64^.Xr.w.byte1 := leLong64.Xr.w.byte2;
    pLong64^.Xr.w.byte0 := leLong64.Xr.w.byte3;
{$ENDIF}
    BF_Encipher;
{Flip Bytes Here!!!}
{$IFDEF ORDER_DCBA}
    leLong64 := pLong64^;
    pLong64^.Xl.w.byte3 := leLong64.Xl.w.byte0;
    pLong64^.Xl.w.byte2 := leLong64.Xl.w.byte1;
    pLong64^.Xl.w.byte1 := leLong64.Xl.w.byte2;
    pLong64^.Xl.w.byte0 := leLong64.Xl.w.byte3;

    pLong64^.Xr.w.byte3 := leLong64.Xr.w.byte0;
    pLong64^.Xr.w.byte2 := leLong64.Xr.w.byte1;
    pLong64^.Xr.w.byte1 := leLong64.Xr.w.byte2;
    pLong64^.Xr.w.byte0 := leLong64.Xr.w.byte3;
{$ENDIF}
end;{TBlowFish.EncipheBLOCK}

Procedure TBlowFish.DecipherBLOCK;
var
  pLong64: PtwoAword;
 begin
  pLong64 := @FSmallBuffer;
  FpXl := @pLong64^.Xl;
  FpXr := @pLong64^.Xr;
  BF_Decipher;
end;{TBlowFish.DecipherBLOCK}
end.
