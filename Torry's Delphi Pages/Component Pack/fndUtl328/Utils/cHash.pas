{                                                                              }
{                           Hashing functions v3.06                            }
{                                                                              }
{             This unit is copyright © 2002-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                     Its original file name is cHash.pas                      }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Description:                                                                 }
{                                                                              }
{   Hashes are algorithms for computing (fixed size) 'condensed                }
{   representations' of 'messages'. The 'message' refers to the (usually       }
{   large variable size) data being hashed. The 'condensed representation'     }
{   is refered to as the 'message digest'.                                     }
{                                                                              }
{   Hashing functions are useful when a (unique) 'fingerprint' is              }
{   needed to represent a (large) piece of data.                               }
{                                                                              }
{   Hashes are refered to as 'secure' if it is computationally infeasible to   }
{   find a message that produce a given digest.                                }
{                                                                              }
{   'Keyed' hashes use a key (password) in the hashing process.                }
{                                                                              }
{ Supported hashes:                                                            }
{                                                                              }
{   Algorithm    Digest size (bits)  Type                                      }
{   ------------ ------------------  ------------------------------            }
{   Checksum       32                Checksum                                  }
{   XOR8           8                 Checksum                                  }
{   XOR16          16                Checksum                                  }
{   XOR32          32                Checksum                                  }
{   CRC16          16                Checksum / Error detection                }
{   CRC32          32                Checksum / Error detection                }
{   ELF            32                Checksum                                  }
{   MD5            128               Secure hash                               }
{   SHA1           160               Secure hash                               }
{   HMAC/MD5       128               Secure keyed hash                         }
{   HMAC/SHA1      160               Secure keyed hash                         }
{                                                                              }
{ Other:                                                                       }
{                                                                              }
{   Algorithm    Type                                                          }
{   ------------ --------------------------------------------------            }
{   ISBN         Check-digit for International Standard Book Number            }
{   LUHN         Check-digit for credit card numbers                           }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   02/04/2002  0.01  Initial version from cUtils unit.                        }
{                     Hash: Checksum, XOR, CRC, MD5, SHA1, SNS                 }
{                     Keyed hash: HMAC-MD5, HMAC-SHA1                          }
{   03/04/2002  0.02  Securely clear passwords from memory after use.          }
{   05/04/2002  0.03  Added SNS hashing.                                       }
{   19/04/2002  0.04  Added ISBN checksum.                                     }
{   26/07/2003  0.05  Added ELF hashing.                                       }
{   09/08/2003  3.06  Revise for Fundamentals 3.                               }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cHash;

interface

uses
  { Delphi }
  SysUtils;



{                                                                              }
{ Hash digests                                                                 }
{                                                                              }
type
  PByte = ^Byte;
  PWord = ^Word;
  PLongWord = ^LongWord;
  T128BitDigest = record
    case integer of
      0 : (Int64s : Array [0..1] of Int64);
      1 : (Longs  : Array [0..3] of LongWord);
      2 : (Words  : Array [0..7] of Word);
      3 : (Bytes  : Array [0..15] of Byte);
    end;
  P128BitDigest = ^T128BitDigest;
  T160BitDigest = record
    case integer of
      0 : (Longs : Array [0..4] of LongWord);
      1 : (Words : Array [0..9] of Word);
      2 : (Bytes : Array [0..19] of Byte);
    end;
  P160BitDigest = ^T160BitDigest;
  
const
  MaxHashDigestSize = Sizeof (T160BitDigest);

Procedure DigestToHexBuf (const Digest; const Size : Integer; const Buf);
Function  DigestToHex (const Digest; const Size : Integer) : String;
Function  Digest128Equal (const Digest1, Digest2 : T128BitDigest) : Boolean;
Function  Digest160Equal (const Digest1, Digest2 : T160BitDigest) : Boolean;



{                                                                              }
{ Hash errors                                                                  }
{                                                                              }
const
  hashNoError            = 0;
  hashInternalError      = 1;
  hashInvalidHashType    = 2;
  hashInvalidBuffer      = 3;
  hashInvalidBufferSize  = 4;
  hashInvalidDigest      = 5;
  hashInvalidKey         = 6;
  hashInvalidFileName    = 7;
  hashFileOpenError      = 8;
  hashFileSeekError      = 9;
  hashFileReadError      = 10;
  hashNotKeyedHashType   = 11;
  hashTooManyOpenHandles = 12;
  hashInvalidHandle      = 13;
  hashMAX_ERROR          = 13;

Function  GetHashErrorMessage (const ErrorCode : LongWord) : PChar;

type
  EHashError = class (Exception)
    protected
    FErrorCode : LongWord;

    public
    Constructor Create (const ErrorCode : LongWord; const Msg : String = '');
    Property ErrorCode : LongWord read FErrorCode;
  end;



{                                                                              }
{ Secure memory clear                                                          }
{   Used to clear keys (passwords) from memory                                 }
{                                                                              }
Procedure SecureClear (var Buf; const BufSize : Integer);
Procedure SecureClearStr (var S : String);



{                                                                              }
{ Checksum hashing                                                             }
{                                                                              }
Function  CalcChecksum32 (const Buf; const BufSize : Integer) : LongWord; overload;
Function  CalcChecksum32 (const Buf : String) : LongWord; overload;



{                                                                              }
{ XOR hashing                                                                  }
{                                                                              }
Function  CalcXOR8 (const Buf; const BufSize : Integer) : Byte; overload;
Function  CalcXOR8 (const Buf : String) : Byte; overload;

Function  CalcXOR16 (const Buf; const BufSize : Integer) : Word; overload;
Function  CalcXOR16 (const Buf : String) : Word; overload;

Function  CalcXOR32 (const Buf; const BufSize : Integer) : LongWord; overload;
Function  CalcXOR32 (const Buf : String) : LongWord; overload;



{                                                                              }
{ CRC 16 hashing                                                               }
{                                                                              }
{   The theory behind CCITT V.41 CRCs:                                         }
{                                                                              }
{      1. Select the magnitude of the CRC to be used (typically 16 or 32       }
{         bits) and choose the polynomial to use. In the case of 16 bit        }
{         CRCs, the CCITT polynomial is recommended and is                     }
{                                                                              }
{                       16    12    5                                          }
{               G(x) = x   + x   + x  + 1                                      }
{                                                                              }
{         This polynomial traps 100% of 1 bit, 2 bit, odd numbers of bit       }
{         errors, 100% of <= 16 bit burst errors and over 99% of all           }
{         other errors.                                                        }
{                                                                              }
{      2. The CRC is calculated as                                             }
{                               r                                              }
{               D(x) = (M(x) * 2 )  mod G(x)                                   }
{                                                                              }
{         This may be better described as : Add r bits (0 content) to          }
{         the end of M(x). Divide this by G(x) and the remainder is the        }
{         CRC.                                                                 }
{                                                                              }
{      3. Tag the CRC onto the end of M(x).                                    }
{                                                                              }
{      4. To check it, calculate the CRC of the new message D(x), using        }
{         the same process as in 2. above. The newly calculated CRC            }
{         should be zero.                                                      }
{                                                                              }
{   This effectively means that using CRCs, it is possible to calculate a      }
{   series of bits to tag onto the data which makes the data an exact          }
{   multiple of the polynomial.                                                }
{                                                                              }
Procedure CRC16Init (var CRC16 : Word);
Function  CRC16Byte (const CRC16 : Word; const Octet : Byte) : Word;
Function  CRC16Buf (const CRC16 : Word; const Buf; const BufSize : Integer) : Word;

Function  CalcCRC16 (const Buf; const BufSize : Integer) : Word; overload;
Function  CalcCRC16 (const Buf : String) : Word; overload;



{                                                                              }
{ CRC 32 hashing                                                               }
{                                                                              }
Procedure SetCRC32Poly (const Poly : LongWord);

Procedure CRC32Init (var CRC32 : LongWord);
Function  CRC32Byte (const CRC32 : LongWord; const Octet : Byte) : LongWord;
Function  CRC32Buf (const CRC32 : LongWord; const Buf; const BufSize : Integer) : LongWord;
Function  CRC32BufNoCase (const CRC32 : LongWord; const Buf; const BufSize : Integer) : LongWord;

Function  CalcCRC32 (const Buf; const BufSize : Integer) : LongWord; overload;
Function  CalcCRC32 (const Buf : String) : LongWord; overload;



{                                                                              }
{ ELF hashing                                                                  }
{                                                                              }
Procedure ELFInit (var Digest : LongWord);
Function  ELFBuf (const Digest : LongWord; const Buf; const BufSize : Integer) : LongWord;

Function  CalcELF (const Buf; const BufSize : Integer) : LongWord; overload;
Function  CalcELF (const Buf : String) : LongWord; overload;



{                                                                              }
{ ISBN checksum                                                                }
{                                                                              }
Function  IsValidISBN (const S : String) : Boolean;



{                                                                              }
{ LUHN checksum                                                                }
{                                                                              }
{   The LUHN forumula (also known as mod-10) is used in major credit card      }
{   account numbers for validity checking.                                     }
{                                                                              }
Function  IsValidLUHN (const S : String) : Boolean;



{                                                                              }
{ MD5 hashing                                                                  }
{                                                                              }
{   MD5 is an Internet standard secure hashing function, that was              }
{   developed by Professor Ronald L. Rivest in 1991. Subsequently it has       }
{   been placed in the public domain.                                          }
{   MD5 was developed to be more secure after MD4 was 'broken'.                }
{   Den Boer and Bosselaers estimate that if a custom machine were to be       }
{   built specifically to find collisions for MD5 (costing $10m in 1994) it    }
{   would on average take 24 days to find a collision.                         }
{                                                                              }
Procedure MD5InitDigest (var Digest : T128BitDigest);
Procedure MD5Buf (var Digest : T128BitDigest; const Buf; const BufSize : Integer);
Procedure MD5FinalBuf (var Digest : T128BitDigest; const Buf; const BufSize : Integer;
          const TotalSize : Int64);

Function  CalcMD5 (const Buf; const BufSize : Integer) : T128BitDigest; overload;
Function  CalcMD5 (const Buf : String) : T128BitDigest; overload;

Function  MD5DigestAsString (const Digest : T128BitDigest) : String;
Function  MD5DigestToHex (const Digest : T128BitDigest) : String;



{                                                                              }
{ HMAC-MD5 keyed hashing                                                       }
{                                                                              }
{   HMAC allows secure keyed hashing (hashing with a password).                }
{   HMAC was designed to meet the requirements of the IPSEC working group in   }
{   the IETF, and is now a standard.                                           }
{   HMAC, are proven to be secure as long as the underlying hash function      }
{   has some reasonable cryptographic strengths.                               }
{   See RFC 2104 for details on HMAC.                                          }
{                                                                              }
Procedure HMAC_MD5Init (const Key : Pointer; const KeySize : Integer;
          var Digest : T128BitDigest; var K : String);
Procedure HMAC_MD5Buf (var Digest : T128BitDigest; const Buf; const BufSize : Integer);
Procedure HMAC_MD5FinalBuf (const K : String; var Digest : T128BitDigest;
          const Buf; const BufSize : Integer; const TotalSize : Int64);

Function  CalcHMAC_MD5 (const Key : Pointer; const KeySize : Integer;
          const Buf; const BufSize : Integer) : T128BitDigest; overload;
Function  CalcHMAC_MD5 (const Key : String; const Buf; const BufSize : Integer) : T128BitDigest; overload;
Function  CalcHMAC_MD5 (const Key, Buf : String) : T128BitDigest; overload;



{                                                                              }
{ SHA1 Hashing                                                                 }
{                                                                              }
{   Specification at http://www.itl.nist.gov/fipspubs/fip180-1.htm             }
{   Also see RFC 3174.                                                         }
{   SHA1 was developed by NIST and is specified in the Secure Hash Standard    }
{   (SHS, FIPS 180) and corrects an unpublished flaw the original SHA          }
{   algorithm.                                                                 }
{   SHA1 produces a 160-bit digest and is considered more secure than MD5.     }
{   SHA1 has a similar design to the MD4-family of hash functions.             }
{                                                                              }
Procedure SHA1InitDigest (var Digest : T160BitDigest);
Procedure SHA1Buf (var Digest : T160BitDigest; const Buf; const BufSize : Integer);
Procedure SHA1FinalBuf (var Digest : T160BitDigest; const Buf; const BufSize : Integer;
          const TotalSize : Int64);

Function  CalcSHA1 (const Buf; const BufSize : Integer) : T160BitDigest; overload;
Function  CalcSHA1 (const Buf : String) : T160BitDigest; overload;

Function  SHA1DigestAsString (const Digest : T160BitDigest) : String;
Function  SHA1DigestToHex (const Digest : T160BitDigest) : String;



{                                                                              }
{ HMAC-SHA1 keyed hashing                                                      }
{                                                                              }
Procedure HMAC_SHA1Init (const Key : Pointer; const KeySize : Integer;
          var Digest : T160BitDigest; var K : String);
Procedure HMAC_SHA1Buf (var Digest : T160BitDigest; const Buf; const BufSize : Integer);
Procedure HMAC_SHA1FinalBuf (const K : String; var Digest : T160BitDigest;
          const Buf; const BufSize : Integer; const TotalSize : Int64);

Function  CalcHMAC_SHA1 (const Key : Pointer; const KeySize : Integer;
          const Buf; const BufSize : Integer) : T160BitDigest; overload;
Function  CalcHMAC_SHA1 (const Key : String; const Buf; const BufSize : Integer) : T160BitDigest; overload;
Function  CalcHMAC_SHA1 (const Key, Buf : String) : T160BitDigest; overload;



{                                                                              }
{ Hash class wrappers                                                          }
{                                                                              }
type
  { AHash                                                                      }
  {   Base class for hash classes.                                             }
  AHash = class
    protected
    FDigest    : Pointer;
    FTotalSize : Int64;

    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); virtual; abstract;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); virtual; abstract;
    Procedure ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64); virtual;

    public
    class Function DigestSize : Integer; virtual; abstract;

    Procedure Init (const Digest : Pointer; const Key : Pointer = nil;
              const KeySize : Integer = 0); overload;
    Procedure Init (const Digest : Pointer; const Key : String = ''); overload;

    Procedure HashBuf (const Buf; const BufSize : Integer; const FinalBuf : Boolean);
    Procedure HashFile (const FileName : String; const Offset : Int64 = 0;
              const MaxCount : Int64 = -1);
  end;
  THashClass = class of AHash;

  { TChecksum32Hash                                                            }
  TChecksum32Hash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { TXOR8Hash                                                                  }
  TXOR8Hash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { TXOR16Hash                                                                 }
  TXOR16Hash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { TXOR32Hash                                                                 }
  TXOR32Hash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { TCRC16Hash                                                                 }
  TCRC16Hash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { TCRC32Hash                                                                 }
  TCRC32Hash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { TELFHash                                                                   }
  TELFHash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { TMD5Hash                                                                   }
  TMD5Hash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;
    Procedure ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { THMAC_MD5Hash                                                              }
  THMAC_MD5Hash = class (AHash)
    protected
    FKey : String;

    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;
    Procedure ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64); override;

    public
    class Function DigestSize : Integer; override;

    Destructor Destroy; override;
  end;

  { TSHA1Hash                                                                  }
  TSHA1Hash = class (AHash)
    protected
    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;
    Procedure ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64); override;

    public
    class Function DigestSize : Integer; override;
  end;

  { THMAC_SHA1Hash                                                             }
  THMAC_SHA1Hash = class (AHash)
    protected
    FKey : String;

    Procedure InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer); override;
    Procedure ProcessBuf (const Buf; const BufSize : Integer); override;
    Procedure ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64); override;

    public
    class Function DigestSize : Integer; override;

    Destructor Destroy; override;
  end;



{                                                                              }
{ THashType                                                                    }
{                                                                              }
type
  THashType = (hashChecksum32, hashXOR8, hashXOR16, hashXOR32,
               hashCRC16, hashCRC32,
               hashELF,
               hashMD5, hashSHA1,
               hashHMAC_MD5, hashHMAC_SHA1);



{                                                                              }
{ GetHashClassByType                                                           }
{                                                                              }
Function  GetHashClassByType (const HashType : THashType) : THashClass;
Function  GetDigestSize (const HashType : THashType) : Integer;



{                                                                              }
{ CalculateHash                                                                }
{                                                                              }
Procedure CalculateHash (const HashType : THashType;
          const Buf; const BufSize : Integer; const Digest : Pointer;
          const Key : Pointer = nil; const KeySize : Integer = 0); overload;
Procedure CalculateHash (const HashType : THashType;
          const Buf; const BufSize : Integer;
          const Digest : Pointer; const Key : String = ''); overload;
Procedure CalculateHash (const HashType : THashType;
          const Buf : String; const Digest : Pointer;
          const Key : String = ''); overload;


          
{                                                                              }
{ HashString                                                                   }
{                                                                              }
{   HashString is a fast general purpose ASCII string hashing function.        }
{   It returns a 32 bit value in the range 0 to Slots - 1. If Slots = 0 then   }
{   the full 32 bit value is returned.                                         }
{   If CaseSensitive = False then HashString will return the same hash value   }
{   regardless of the case of the characters in the string.                    }
{                                                                              }
{   The implementation is based on CRC32. It uses up to 48 characters from     }
{   the string (first 16 characters, last 16 characters and 16 characters      }
{   uniformly sampled from the remaining characters) to calculate the hash     }
{   value.                                                                     }
{                                                                              }
Function  HashString (const StrBuf : Pointer; const StrLength : Integer;
          const Slots : LongWord = 0; const CaseSensitive : Boolean = True) : LongWord; overload;
Function  HashString (const S : String; const Slots : LongWord = 0;
          const CaseSensitive : Boolean = True) : LongWord; overload;



implementation



{                                                                              }
{ Hash errors                                                                  }
{                                                                              }
const
  hashErrorMessages : Array [0..hashMAX_ERROR] of String = (
      '',
      'Internal error',
      'Invalid hash type',
      'Invalid buffer',
      'Invalid buffer size',
      'Invalid digest',
      'Invalid key',
      'Invalid file name',
      'File open error',
      'File seek error',
      'File read error',
      'Not a keyed hash type',
      'Too many open handles',
      'Invalid handle');

Function GetHashErrorMessage (const ErrorCode : LongWord) : PChar;
  Begin
    if (ErrorCode = hashNoError) or (ErrorCode > hashMAX_ERROR) then
      Result := nil else
      Result := PChar (hashErrorMessages [ErrorCode]);
  End;



{                                                                              }
{ EHashError                                                                   }
{                                                                              }
Constructor EHashError.Create (const ErrorCode : LongWord; const Msg : String);
  Begin
    FErrorCode := ErrorCode;
    if (Msg = '') and (ErrorCode <= hashMAX_ERROR) then
      inherited Create (hashErrorMessages [ErrorCode]) else
      inherited Create (Msg);
  End;



{                                                                              }
{ Checksum hashing                                                             }
{                                                                              }
{$IFDEF CPU_INTEL386}
Function CalcChecksum32 (const Buf; const BufSize : Integer) : LongWord;
  Asm
      or eax, eax              // eax = Buf
      jz @fin
      or edx, edx              // edx = BufSize
      jbe @finz
      push esi
      mov esi, eax
      add esi, edx
      xor eax, eax
      xor ecx, ecx
    @l1:
      dec esi
      mov cl, [esi]
      add eax, ecx
      dec edx
      jnz @l1
      pop esi
    @fin:
      ret
    @finz:
      xor eax, eax
  End;
{$ELSE}
Function CalcChecksum32 (const Buf; const BufSize : Integer) : LongWord;
var I : Integer;
    P : PByte;
  Begin
    Result := 0;
    P := @Buf;
    For I := 1 to BufSize do
      begin
        Inc (Result, P^);
        Inc (P);
      end;
  End;
{$ENDIF}

Function CalcChecksum32 (const Buf : String) : LongWord;
  Begin
    Result := CalcChecksum32 (Pointer (Buf)^, Length (Buf));
  End;



{                                                                              }
{ XOR hashing                                                                  }
{                                                                              }
{$IFDEF CPU_INTEL386}
Function XOR32Buf (const Buf; const BufSize : Integer) : LongWord;
  Asm
      or eax, eax
      jz @fin
      or edx, edx
      jz @finz

      push esi
      mov esi, eax
      xor eax, eax

      mov ecx, edx
      shr ecx, 2
      jz @rest

    @l1:
      xor eax, [esi]
      add esi, 4
      dec ecx
      jnz @l1

    @rest:
      and edx, 3
      jz @finp
      xor al, [esi]
      dec edx
      jz @finp
      inc esi
      xor ah, [esi]
      dec edx
      jz @finp
      inc esi
      mov dl, [esi]
      shl edx, 16
      xor eax, edx

    @finp:
      pop esi
      ret
    @finz:
      xor eax, eax
    @fin:
      ret
  End;
{$ELSE}
Function XOR32Buf (const Buf; const BufSize : Integer) : LongWord;
var I : Integer;
    L : Byte;
    P : PChar;
  Begin
    Result := 0;
    L := 0;
    P := @Buf;
    For I := 1 to BufSize do
      begin
        Result := Result xor (Byte (P^) shl L);
        Inc (L, 8);
        if L = 32 then
          L := 0;
        Inc (P);
      end;
  End;
{$ENDIF}

Function CalcXOR8 (const Buf; const BufSize : Integer) : Byte;
var L : LongWord;
  Begin
    L := XOR32Buf (Buf, BufSize);
    Result := Byte (L) xor
              Byte (L shr 8) xor
              Byte (L shr 16) xor
              Byte (L shr 24);
  End;

Function CalcXOR8 (const Buf : String) : Byte;
  Begin
    Result := CalcXOR8 (Pointer (Buf)^, Length (Buf));
  End;

Function CalcXOR16 (const Buf; const BufSize : Integer) : Word;
var L : LongWord;
  Begin
    L := XOR32Buf (Buf, BufSize);
    Result := Word (L) xor
              Word (L shr 16);
  End;

Function CalcXOR16 (const Buf : String) : Word;
  Begin
    Result := CalcXOR16 (Pointer (Buf)^, Length (Buf));
  End;

Function CalcXOR32 (const Buf; const BufSize : Integer) : LongWord;
  Begin
    Result := XOR32Buf (Buf, BufSize);
  End;

Function CalcXOR32 (const Buf : String) : LongWord;
  Begin
    Result := XOR32Buf (Pointer (Buf)^, Length (Buf));
  End;



{                                                                              }
{ CRC 16 hashing                                                               }
{                                                                              }
const
  CRC16Table : Array [Byte] of Word = (
    $0000, $1021, $2042, $3063, $4084, $50a5, $60c6, $70e7,
    $8108, $9129, $a14a, $b16b, $c18c, $d1ad, $e1ce, $f1ef,
    $1231, $0210, $3273, $2252, $52b5, $4294, $72f7, $62d6,
    $9339, $8318, $b37b, $a35a, $d3bd, $c39c, $f3ff, $e3de,
    $2462, $3443, $0420, $1401, $64e6, $74c7, $44a4, $5485,
    $a56a, $b54b, $8528, $9509, $e5ee, $f5cf, $c5ac, $d58d,
    $3653, $2672, $1611, $0630, $76d7, $66f6, $5695, $46b4,
    $b75b, $a77a, $9719, $8738, $f7df, $e7fe, $d79d, $c7bc,
    $48c4, $58e5, $6886, $78a7, $0840, $1861, $2802, $3823,
    $c9cc, $d9ed, $e98e, $f9af, $8948, $9969, $a90a, $b92b,
    $5af5, $4ad4, $7ab7, $6a96, $1a71, $0a50, $3a33, $2a12,
    $dbfd, $cbdc, $fbbf, $eb9e, $9b79, $8b58, $bb3b, $ab1a,
    $6ca6, $7c87, $4ce4, $5cc5, $2c22, $3c03, $0c60, $1c41,
    $edae, $fd8f, $cdec, $ddcd, $ad2a, $bd0b, $8d68, $9d49,
    $7e97, $6eb6, $5ed5, $4ef4, $3e13, $2e32, $1e51, $0e70,
    $ff9f, $efbe, $dfdd, $cffc, $bf1b, $af3a, $9f59, $8f78,
    $9188, $81a9, $b1ca, $a1eb, $d10c, $c12d, $f14e, $e16f,
    $1080, $00a1, $30c2, $20e3, $5004, $4025, $7046, $6067,
    $83b9, $9398, $a3fb, $b3da, $c33d, $d31c, $e37f, $f35e,
    $02b1, $1290, $22f3, $32d2, $4235, $5214, $6277, $7256,
    $b5ea, $a5cb, $95a8, $8589, $f56e, $e54f, $d52c, $c50d,
    $34e2, $24c3, $14a0, $0481, $7466, $6447, $5424, $4405,
    $a7db, $b7fa, $8799, $97b8, $e75f, $f77e, $c71d, $d73c,
    $26d3, $36f2, $0691, $16b0, $6657, $7676, $4615, $5634,
    $d94c, $c96d, $f90e, $e92f, $99c8, $89e9, $b98a, $a9ab,
    $5844, $4865, $7806, $6827, $18c0, $08e1, $3882, $28a3,
    $cb7d, $db5c, $eb3f, $fb1e, $8bf9, $9bd8, $abbb, $bb9a,
    $4a75, $5a54, $6a37, $7a16, $0af1, $1ad0, $2ab3, $3a92,
    $fd2e, $ed0f, $dd6c, $cd4d, $bdaa, $ad8b, $9de8, $8dc9,
    $7c26, $6c07, $5c64, $4c45, $3ca2, $2c83, $1ce0, $0cc1,
    $ef1f, $ff3e, $cf5d, $df7c, $af9b, $bfba, $8fd9, $9ff8,
    $6e17, $7e36, $4e55, $5e74, $2e93, $3eb2, $0ed1, $1ef0);

Function CRC16Byte (const CRC16 : Word; const Octet : Byte) : Word;
  Begin
    Result := CRC16Table [Byte (Hi (CRC16) xor Octet)] xor Word (CRC16 shl 8);
  End;

Function CRC16Buf (const CRC16 : Word; const Buf; const BufSize : Integer) : Word;
var I : Integer;
    P : PByte;
  Begin
    Result := CRC16;
    P := @Buf;
    For I := 1 to BufSize do
      begin
        Result := CRC16Byte (Result, P^);
        Inc (P);
      end;
  End;

Procedure CRC16Init (var CRC16 : Word);
  Begin
    CRC16 := $FFFF;
  End;

Function CalcCRC16 (const Buf; const BufSize : Integer) : Word;
  Begin
    CRC16Init (Result);
    Result := CRC16Buf (Result, Buf, BufSize);
  End;

Function CalcCRC16 (const Buf : String) : Word;
  Begin
    Result := CalcCRC16 (Pointer (Buf)^, Length (Buf));
  End;



{                                                                              }
{ CRC 32 hashing                                                               }
{                                                                              }
var
  CRC32TableInit : Boolean = False;
  CRC32Table     : Array [Byte] of LongWord;
  CRC32Poly      : LongWord = $EDB88320;

Procedure InitCRC32Table;
var I, J : Byte;
    R    : LongWord;
  Begin
    For I := $00 to $FF do
      begin
        R := I;
        For J := 8 downto 1 do
          if R and 1 <> 0 then
            R := (R shr 1) xor CRC32Poly else
            R := R shr 1;
        CRC32Table [I] := R;
      end;
    CRC32TableInit := True;
  End;

Procedure SetCRC32Poly (const Poly : LongWord);
  Begin
    CRC32Poly := Poly;
    CRC32TableInit := False;
  End;

Function CalcCRC32Byte (const CRC32 : LongWord; const Octet : Byte) : LongWord;
  Begin
    Result := CRC32Table [Byte (CRC32) xor Octet] xor ((CRC32 shr 8) and $00FFFFFF);
  End;

Function CRC32Byte (const CRC32 : LongWord; const Octet : Byte) : LongWord;
  Begin
    if not CRC32TableInit then
      InitCRC32Table;
    Result := CalcCRC32Byte (CRC32, Octet);
  End;

Function CRC32Buf (const CRC32 : LongWord; const Buf; const BufSize : Integer) : LongWord;
var P : PByte;
    I : Integer;
  Begin
    if not CRC32TableInit then
      InitCRC32Table;
    P := @Buf;
    Result := CRC32;
    For I := 1 to BufSize do
      begin
        Result := CalcCRC32Byte (Result, P^);
        Inc (P);
      end;
  End;

Function CRC32BufNoCase (const CRC32 : LongWord; const Buf; const BufSize : Integer) : LongWord;
var P : PByte;
    I : Integer;
    C : Byte;
  Begin
    if not CRC32TableInit then
      InitCRC32Table;
    P := @Buf;
    Result := CRC32;
    For I := 1 to BufSize do
      begin
        C := P^;
        if Char (C) in ['A'..'Z'] then
          C := C or 32;
        Result := CalcCRC32Byte (Result, C);
        Inc (P);
      end;
  End;

Procedure CRC32Init (var CRC32 : LongWord);
  Begin
    CRC32 := $FFFFFFFF;
  End;

Function CalcCRC32 (const Buf; const BufSize : Integer) : LongWord;
  Begin
    CRC32Init (Result);
    Result := not CRC32Buf (Result, Buf, BufSize);
  End;

Function CalcCRC32 (const Buf : String) : LongWord;
  Begin
    Result := CalcCRC32 (Pointer (Buf)^, Length (Buf));
  End;



{                                                                              }
{ ELF hashing                                                                  }
{                                                                              }
Procedure ELFInit (var Digest : LongWord);
  Begin
    Digest := 0;
  End;

Function ELFBuf (const Digest : LongWord; const Buf; const BufSize : Integer) : LongWord;
var I : Integer;
    P : PByte;
    X : LongWord;
  Begin
    Result := Digest;
    P := @Buf;
    For I := 1 to BufSize do
      begin
        Result := (Result shl 4) + P^;
        Inc (P);
        X := Result and $F0000000;
        if X <> 0 then
          Result := Result xor (X shr 24);
        Result := Result and (not X);
      end;
  End;


Function CalcELF (const Buf; const BufSize : Integer) : LongWord;
  Begin
    Result := ELFBuf (0, Buf, BufSize);
  End;


Function CalcELF (const Buf : String) : LongWord;
  Begin
    Result := CalcELF (Pointer (Buf)^, Length (Buf));
  End;



{                                                                              }
{ ISBN checksum                                                                }
{                                                                              }
Function IsValidISBN (const S : String) : Boolean;
var I, L, M, D, C : Integer;
    P : PChar;
  Begin
    L := Length (S);
    if L < 10 then // too few digits
      begin
        Result := False;
        exit;
      end;
    M := 10;
    C := 0;
    P := Pointer (S);
    For I := 1 to L do
      begin
        if (P^ in ['0'..'9']) or ((M = 1) and (P^ in ['x', 'X'])) then
          begin
            if M = 0 then // too many digits
              begin
                Result := False;
                exit;
              end;
            if P^ in ['x', 'X'] then
              D := 10 else
              D := Ord (P^) - Ord ('0');
            Inc (C, M * D);
            Dec (M);
          end;
        Inc (P);
      end;
    if M > 0 then // too few digits
      begin
        Result := False;
        exit;
      end;
    Result := C mod 11 = 0;
  End;



{                                                                              }
{ LUHN checksum                                                                }
{                                                                              }
Function IsValidLUHN (const S : String) : Boolean;
var P : PChar;
    I, L, M, C, D : Integer;
    R : Boolean;
  Begin
    L := Length (S);
    if L = 0 then
      begin
        Result := False;
        exit;
      end;
    P := Pointer (S);
    Inc (P, L - 1);
    C := 0;
    M := 0;
    R := False;
    For I := 1 to L do
      begin
        if P^ in ['0'..'9'] then
          begin
            D := Ord (P^) - Ord ('0');
            if R then
              begin
                D := D * 2;
                D := (D div 10) + (D mod 10);
              end;
            Inc (C, D);
            Inc (M);
            R := not R;
          end;
        Dec (P);
      end;
    Result := (M >= 1) and (C mod 10 = 0);
  End;



{                                                                              }
{ Digests                                                                      }
{                                                                              }
Procedure DigestToHexBuf (const Digest; const Size : Integer; const Buf);
const s_HexDigitsLower : String [16] = '0123456789abcdef';
var I : Integer;
    P : PChar;
    Q : PByte;
  Begin
    P := @Buf;;
    Assert (Assigned (P), 'Assigned (Buf)');
    Q := @Digest;
    Assert (Assigned (Q), 'Assigned (Digest)');
    For I := 0 to Size - 1 do
      begin
        P^ := s_HexDigitsLower [Q^ shr 4 + 1];
        Inc (P);
        P^ := s_HexDigitsLower [Q^ and 15 + 1];
        Inc (P);
        Inc (Q);
      end;
  End;

Function DigestToHex (const Digest; const Size : Integer) : String;
  Begin
    SetLength (Result, Size * 2);
    DigestToHexBuf (Digest, Size, Pointer (Result)^);
  End;

Function Digest128Equal (const Digest1, Digest2 : T128BitDigest) : Boolean;
var I : Integer;
  Begin
    For I := 0 to 3 do
      if Digest1.Longs[I] <> Digest2.Longs[I] then
        begin
          Result := False;
          exit;
        end;
    Result := True;
  End;

Function Digest160Equal (const Digest1, Digest2 : T160BitDigest) : Boolean;
var I : Integer;
  Begin
    For I := 0 to 4 do
      if Digest1.Longs[I] <> Digest2.Longs[I] then
        begin
          Result := False;
          exit;
        end;
    Result := True;
  End;



{                                                                              }
{ MD5 hashing                                                                  }
{                                                                              }
const
  MD5Table_1 : Array [0..15] of LongWord = (
      $D76AA478, $E8C7B756, $242070DB, $C1BDCEEE,
      $F57C0FAF, $4787C62A, $A8304613, $FD469501,
      $698098D8, $8B44F7AF, $FFFF5BB1, $895CD7BE,
      $6B901122, $FD987193, $A679438E, $49B40821);
  MD5Table_2 : Array [0..15] of LongWord = (
      $F61E2562, $C040B340, $265E5A51, $E9B6C7AA,
      $D62F105D, $02441453, $D8A1E681, $E7D3FBC8,
      $21E1CDE6, $C33707D6, $F4D50D87, $455A14ED,
      $A9E3E905, $FCEFA3F8, $676F02D9, $8D2A4C8A);
  MD5Table_3 : Array [0..15] of LongWord = (
      $FFFA3942, $8771F681, $6D9D6122, $FDE5380C,
      $A4BEEA44, $4BDECFA9, $F6BB4B60, $BEBFBC70,
      $289B7EC6, $EAA127FA, $D4EF3085, $04881D05,
      $D9D4D039, $E6DB99E5, $1FA27CF8, $C4AC5665);
  MD5Table_4 : Array [0..15] of LongWord = (
      $F4292244, $432AFF97, $AB9423A7, $FC93A039,
      $655B59C3, $8F0CCC92, $FFEFF47D, $85845DD1,
      $6FA87E4F, $FE2CE6E0, $A3014314, $4E0811A1,
      $F7537E82, $BD3AF235, $2AD7D2BB, $EB86D391);

{ Calculates a MD5 Digest (16 bytes) given a Buffer (64 bytes)                 }
{$Q-}
Procedure TransformMD5Buffer (var Digest : T128BitDigest; const Buffer);
var A, B, C, D : LongWord;
    P          : PLongWord;
    I          : Integer;
    J          : Byte;
    Buf        : Array [0..15] of LongWord absolute Buffer;
  Begin
    A := Digest.Longs [0];
    B := Digest.Longs [1];
    C := Digest.Longs [2];
    D := Digest.Longs [3];

    P := @MD5Table_1;
    For I := 0 to 3 do
      begin
        J := I * 4;
        Inc (A, Buf [J]     + P^ + (D xor (B and (C xor D))));  A := A shl  7 or A shr 25 + B; Inc (P);
        Inc (D, Buf [J + 1] + P^ + (C xor (A and (B xor C))));  D := D shl 12 or D shr 20 + A; Inc (P);
        Inc (C, Buf [J + 2] + P^ + (B xor (D and (A xor B))));  C := C shl 17 or C shr 15 + D; Inc (P);
        Inc (B, Buf [J + 3] + P^ + (A xor (C and (D xor A))));  B := B shl 22 or B shr 10 + C; Inc (P);
      end;

    P := @MD5Table_2;
    For I := 0 to 3 do
      begin
        J := I * 4;
        Inc (A, Buf [J + 1]           + P^ + (C xor (D and (B xor C))));  A := A shl  5 or A shr 27 + B; Inc (P);
        Inc (D, Buf [(J + 6) mod 16]  + P^ + (B xor (C and (A xor B))));  D := D shl  9 or D shr 23 + A; Inc (P);
        Inc (C, Buf [(J + 11) mod 16] + P^ + (A xor (B and (D xor A))));  C := C shl 14 or C shr 18 + D; Inc (P);
        Inc (B, Buf [J]               + P^ + (D xor (A and (C xor D))));  B := B shl 20 or B shr 12 + C; Inc (P);
      end;

    P := @MD5Table_3;
    For I := 0 to 3 do
      begin
        J := 16 - (I * 4);
        Inc (A, Buf [(J + 5) mod 16]  + P^ + (B xor C xor D));  A := A shl  4 or A shr 28 + B; Inc (P);
        Inc (D, Buf [(J + 8) mod 16]  + P^ + (A xor B xor C));  D := D shl 11 or D shr 21 + A; Inc (P);
        Inc (C, Buf [(J + 11) mod 16] + P^ + (D xor A xor B));  C := C shl 16 or C shr 16 + D; Inc (P);
        Inc (B, Buf [(J + 14) mod 16] + P^ + (C xor D xor A));  B := B shl 23 or B shr  9 + C; Inc (P);
      end;

    P := @MD5Table_4;
    For I := 0 to 3 do
      begin
        J := 16 - (I * 4);
        Inc (A, Buf [J mod 16]        + P^ + (C xor (B or not D)));  A := A shl  6 or A shr 26 + B; Inc (P);
        Inc (D, Buf [(J + 7) mod 16]  + P^ + (B xor (A or not C)));  D := D shl 10 or D shr 22 + A; Inc (P);
        Inc (C, Buf [(J + 14) mod 16] + P^ + (A xor (D or not B)));  C := C shl 15 or C shr 17 + D; Inc (P);
        Inc (B, Buf [(J + 5) mod 16]  + P^ + (D xor (C or not A)));  B := B shl 21 or B shr 11 + C; Inc (P);
      end;

    Inc (Digest.Longs [0], A);
    Inc (Digest.Longs [1], B);
    Inc (Digest.Longs [2], C);
    Inc (Digest.Longs [3], D);
  End;
{$IFDEF DEBUG}{$Q+}{$ENDIF}

Procedure MD5InitDigest (var Digest : T128BitDigest);
  Begin
    Digest.Longs [0] := $67452301;        // fixed initialization key
    Digest.Longs [1] := $EFCDAB89;
    Digest.Longs [2] := $98BADCFE;
    Digest.Longs [3] := $10325476;
  End;

Procedure MD5Buf (var Digest : T128BitDigest; const Buf; const BufSize : Integer);
var P : PByte;
    I, J : Integer;
  Begin
    I := BufSize;
    if I <= 0 then
      exit;
    Assert (I mod 64 = 0, 'BufSize must be multiple of 64 bytes');
    P := @Buf;
    For J := 0 to I div 64 - 1 do
      begin
        TransformMD5Buffer (Digest, P^);
        Inc (P, 64);
      end;
  End;

Procedure ReverseMem (var Buf; const BufSize : Integer);
var I : Integer;
    P : PByte;
    Q : PByte;
    T : Byte;
  Begin
    P := @Buf;
    Q := P;
    Inc (Q, BufSize - 1);
    For I := 1 to BufSize div 2 do
      begin
        T := P^;
        P^ := Q^;
        Q^ := T;
        Inc (P);
        Dec (Q);
      end;
  End;

Procedure StdFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64; var Buf1, Buf2 : String; const SwapEndian : Boolean);
var P, Q : PByte;
    I : Integer;
    L : Int64;
  Begin
    Assert (BufSize < 64, 'Final BufSize must be less than 64 bytes');
    Assert (TotalSize >= BufSize, 'TotalSize >= BufSize');

    P := @Buf;
    SetLength (Buf1, 64);
    Q := Pointer (Buf1);
    if BufSize > 0 then
      begin
        Move (P^, Q^, BufSize);
        Inc (Q, BufSize);
      end;
    Q^ := $80;
    Inc (Q);

    L := Int64 (TotalSize * 8);
    if SwapEndian then
      ReverseMem (L, 8);
    if BufSize + 1 > 64 - Sizeof (Int64) then
      begin
        FillChar (Q^, 64 - BufSize - 1, #0);
        SetLength (Buf2, 64);
        Q := Pointer (Buf2);
        FillChar (Q^, 64 - Sizeof (Int64), #0);
        Inc (Q, 64 - Sizeof (Int64));
        PInt64 (Q)^ := L;
      end else
      begin
        I := 64 - Sizeof (Int64) - BufSize - 1;
        FillChar (Q^, I, #0);
        Inc (Q, I);
        PInt64 (Q)^ := L;
        Buf2 := '';
      end;
  End;

Procedure MD5FinalBuf (var Digest : T128BitDigest; const Buf; const BufSize : Integer; const TotalSize : Int64);
var S1, S2 : String;
  Begin
    StdFinalBuf (Buf, BufSize, TotalSize, S1, S2, False);
    TransformMD5Buffer (Digest, Pointer (S1)^);
    if S2 <> '' then
      TransformMD5Buffer (Digest, Pointer (S2)^);
  End;

Function CalcMD5 (const Buf; const BufSize : Integer) : T128BitDigest;
var I, J : Integer;
    P    : PByte;
  Begin
    MD5InitDigest (Result);
    P := @Buf;
    if BufSize <= 0 then
      I := 0 else
      I := BufSize;
    J := (I div 64) * 64;
    if J > 0 then
      begin
        MD5Buf (Result, P^, J);
        Inc (P, J);
        Dec (I, J);
      end;
    MD5FinalBuf (Result, P^, I, BufSize);
  End;

Function CalcMD5 (const Buf : String) : T128BitDigest;
  Begin
    Result := CalcMD5 (Pointer (Buf)^, Length (Buf));
  End;

Function MD5DigestAsString (const Digest : T128BitDigest) : String;
  Begin
    SetLength (Result, Sizeof (Digest));
    Move (Digest, Pointer (Result)^, Sizeof (Digest));
  End;

Function MD5DigestToHex (const Digest : T128BitDigest) : String;
  Begin
    Result := DigestToHex (Digest, Sizeof (Digest));
  End;



{                                                                              }
{ HMAC-MD5 keyed hashing                                                       }
{                                                                              }
Procedure XORBlock (var Buf : String; const XOR8 : Byte);
var I : Integer;
  Begin
    For I := 1 to Length (Buf) do
      Buf [I] := Char (Byte (Buf [I]) xor XOR8);
  End;

Procedure SecureClear (var Buf; const BufSize : Integer);
  Begin
    if BufSize <= 0 then
      exit;
    // Securely clear memory
    FillChar (Buf, BufSize, #$AA);
    FillChar (Buf, BufSize, #$55);
    FillChar (Buf, BufSize, #0);
  End;

Procedure SecureClearStr (var S : String);
  Begin
    SecureClear (Pointer (S)^, Length (S));
  End;

Procedure HMAC_KeyBlock (const Key; const KeySize : Integer; var Buf : String);
var P : PChar;
  Begin
    Assert (KeySize <= 64, 'KeySize <= 64');
    SetLength (Buf, 64);
    P := Pointer (Buf);
    if KeySize > 0 then
      begin
        Move (Key, P^, KeySize);
        Inc (P, KeySize);
      end;
    FillChar (P^, 64 - KeySize, #0);
  End;

Procedure HMAC_MD5Init (const Key : Pointer; const KeySize : Integer; var Digest : T128BitDigest; var K : String);
var S : String;
    D : T128BitDigest;
  Begin
    MD5InitDigest (Digest);

    if KeySize > 64 then
      begin
        D := CalcMD5 (Key^, KeySize);
        HMAC_KeyBlock (D, Sizeof (D), K);
      end else
      HMAC_KeyBlock (Key^, KeySize, K);

    S := K;
    XORBlock (S, $36);
    TransformMD5Buffer (Digest, Pointer (S)^);
    SecureClearStr (S);
  End;

Procedure HMAC_MD5Buf (var Digest : T128BitDigest; const Buf; const BufSize : Integer);
  Begin
    MD5Buf (Digest, Buf, BufSize);
  End;

Procedure HMAC_MD5FinalBuf (const K : String; var Digest : T128BitDigest; const Buf; const BufSize : Integer; const TotalSize : Int64);
var S : String;
  Begin
    MD5FinalBuf (Digest, Buf, BufSize, TotalSize + 64);
    S := K;
    XORBlock (S, $5C);
    Digest := CalcMD5 (S + MD5DigestAsString (Digest));
    SecureClearStr (S);
  End;

Function CalcHMAC_MD5 (const Key : Pointer; const KeySize : Integer; const Buf; const BufSize : Integer) : T128BitDigest;
var I, J : Integer;
    P    : PByte;
    K    : String;
  Begin
    HMAC_MD5Init (Key, KeySize, Result, K);
    P := @Buf;
    if BufSize <= 0 then
      I := 0 else
      I := BufSize;
    J := (I div 64) * 64;
    if J > 0 then
      begin
        HMAC_MD5Buf (Result, P^, J);
        Inc (P, J);
        Dec (I, J);
      end;
    HMAC_MD5FinalBuf (K, Result, P^, I, BufSize);
    SecureClearStr (K);
  End;

Function CalcHMAC_MD5 (const Key : String; const Buf; const BufSize : Integer) : T128BitDigest;
  Begin
    Result := CalcHMAC_MD5 (Pointer (Key), Length (Key), Buf, BufSize);
  End;

Function CalcHMAC_MD5 (const Key, Buf : String) : T128BitDigest;
  Begin
    Result := CalcHMAC_MD5 (Key, Pointer (Buf)^, Length (Buf));
  End;



{                                                                              }
{ SHA hashing                                                                  }
{                                                                              }
Procedure SHA1InitDigest (var Digest : T160BitDigest);
var P : P128BitDigest;
  Begin
    P := @Digest;
    MD5InitDigest (P^);
    Digest.Longs [4] := $C3D2E1F0;
  End;

Function SwapEndian (const Value : LongWord) : LongWord;
  Asm
        XCHG    AH, AL
        ROL     EAX, 16
        XCHG    AH, AL
  End;

Procedure SwapEndianBuf (var Buf; const Count : Integer);
var P : PLongWord;
    I : Integer;
  Begin
    P := @Buf;
    For I := 1 to Count do
      begin
        P^ := SwapEndian (P^);
        Inc (P);
      end;
  End;

Function RotateLeftBits (const Value : LongWord; const Bits : Byte) : LongWord;
  Asm
     MOV   CL, DL
     ROL   EAX, CL
  End;

{ Calculates a SHA Digest (20 bytes) given a Buffer (64 bytes)                 }
{$Q-}
Procedure TransformSHABuffer (var Digest : T160BitDigest; const Buffer; const SHA1 : Boolean);
var A, B, C, D, E : LongWord;
    W : Array [0..79] of LongWord;
    P, Q : PLongWord;
    I : Integer;
    J : LongWord;
  Begin
    P := @Buffer;
    Q := @W;
    For I := 0 to 15 do
      begin
        Q^ := SwapEndian (P^);
        Inc (P);
        Inc (Q);
      end;
    For I := 0 to 63 do
      begin
        P := Q;
        Dec (P, 16);
        J := P^;
        Inc (P, 2);
        J := J xor P^;
        Inc (P, 6);
        J := J xor P^;
        Inc (P, 5);
        J := J xor P^;
        if SHA1 then
          J := RotateLeftBits (J, 1);
        Q^ := J;
        Inc (Q);
      end;

    A := Digest.Longs [0];
    B := Digest.Longs [1];
    C := Digest.Longs [2];
    D := Digest.Longs [3];
    E := Digest.Longs [4];

    P := @W;
    For I := 0 to 3 do
      begin
        Inc (E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + P^ + $5A827999); B := B shr 2 or B shl 30; Inc (P);
        Inc (D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + P^ + $5A827999); A := A shr 2 or A shl 30; Inc (P);
        Inc (C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + P^ + $5A827999); E := E shr 2 or E shl 30; Inc (P);
        Inc (B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + P^ + $5A827999); D := D shr 2 or D shl 30; Inc (P);
        Inc (A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + P^ + $5A827999); C := C shr 2 or C shl 30; Inc (P);
      end;

    For I := 0 to 3 do
      begin
        Inc (E, (A shl 5 or A shr 27) + (D xor B xor C) + P^ + $6ED9EBA1); B := B shr 2 or B shl 30; Inc (P);
        Inc (D, (E shl 5 or E shr 27) + (C xor A xor B) + P^ + $6ED9EBA1); A := A shr 2 or A shl 30; Inc (P);
        Inc (C, (D shl 5 or D shr 27) + (B xor E xor A) + P^ + $6ED9EBA1); E := E shr 2 or E shl 30; Inc (P);
        Inc (B, (C shl 5 or C shr 27) + (A xor D xor E) + P^ + $6ED9EBA1); D := D shr 2 or D shl 30; Inc (P);
        Inc (A, (B shl 5 or B shr 27) + (E xor C xor D) + P^ + $6ED9EBA1); C := C shr 2 or C shl 30; Inc (P);
      end;

    For I := 0 to 3 do
      begin
        Inc (E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + P^ + $8F1BBCDC); B := B shr 2 or B shl 30; Inc (P);
        Inc (D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + P^ + $8F1BBCDC); A := A shr 2 or A shl 30; Inc (P);
        Inc (C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + P^ + $8F1BBCDC); E := E shr 2 or E shl 30; Inc (P);
        Inc (B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + P^ + $8F1BBCDC); D := D shr 2 or D shl 30; Inc (P);
        Inc (A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + P^ + $8F1BBCDC); C := C shr 2 or C shl 30; Inc (P);
      end;

    For I := 0 to 3 do
      begin
        Inc (E, (A shl 5 or A shr 27) + (D xor B xor C) + P^ + $CA62C1D6); B := B shr 2 or B shl 30; Inc (P);
        Inc (D, (E shl 5 or E shr 27) + (C xor A xor B) + P^ + $CA62C1D6); A := A shr 2 or A shl 30; Inc (P);
        Inc (C, (D shl 5 or D shr 27) + (B xor E xor A) + P^ + $CA62C1D6); E := E shr 2 or E shl 30; Inc (P);
        Inc (B, (C shl 5 or C shr 27) + (A xor D xor E) + P^ + $CA62C1D6); D := D shr 2 or D shl 30; Inc (P);
        Inc (A, (B shl 5 or B shr 27) + (E xor C xor D) + P^ + $CA62C1D6); C := C shr 2 or C shl 30; Inc (P);
      end;

    Inc (Digest.Longs [0], A);
    Inc (Digest.Longs [1], B);
    Inc (Digest.Longs [2], C);
    Inc (Digest.Longs [3], D);
    Inc (Digest.Longs [4], E);
  End;
{$IFDEF DEBUG}{$Q+}{$ENDIF}

Procedure SHA1Buf (var Digest : T160BitDigest; const Buf; const BufSize : Integer);
var P : PByte;
    I, J : Integer;
  Begin
    I := BufSize;
    if I <= 0 then
      exit;
    Assert (I mod 64 = 0, 'BufSize must be multiple of 64 bytes');
    P := @Buf;
    For J := 0 to I div 64 - 1 do
      begin
        TransformSHABuffer (Digest, P^, True);
        Inc (P, 64);
      end;
  End;

Procedure SHA1FinalBuf (var Digest : T160BitDigest; const Buf; const BufSize : Integer; const TotalSize : Int64);
var S1, S2 : String;
  Begin
    StdFinalBuf (Buf, BufSize, TotalSize, S1, S2, True);
    TransformSHABuffer (Digest, Pointer (S1)^, True);
    if S2 <> '' then
      TransformSHABuffer (Digest, Pointer (S2)^, True);
    SwapEndianBuf (Digest, Sizeof (Digest) div Sizeof (LongWord));
  End;

Function CalcSHA1 (const Buf; const BufSize : Integer) : T160BitDigest;
var I, J : Integer;
    P    : PByte;
  Begin
    SHA1InitDigest (Result);
    P := @Buf;
    if BufSize <= 0 then
      I := 0 else
      I := BufSize;
    J := (I div 64) * 64;
    if J > 0 then
      begin
        SHA1Buf (Result, P^, J);
        Inc (P, J);
        Dec (I, J);
      end;
    SHA1FinalBuf (Result, P^, I, BufSize);
  End;

Function CalcSHA1 (const Buf : String) : T160BitDigest;
  Begin
    Result := CalcSHA1 (Pointer (Buf)^, Length (Buf));
  End;

Function SHA1DigestAsString (const Digest : T160BitDigest) : String;
  Begin
    SetLength (Result, Sizeof (Digest));
    Move (Digest, Pointer (Result)^, Sizeof (Digest));
  End;

Function SHA1DigestToHex (const Digest : T160BitDigest) : String;
  Begin
    Result := DigestToHex (Digest, Sizeof (Digest));
  End;



{                                                                              }
{ HMAC-SHA1 keyed hashing                                                      }
{                                                                              }
Procedure HMAC_SHA1Init (const Key : Pointer; const KeySize : Integer; var Digest : T160BitDigest; var K : String);
var D : T160BitDigest;
    S : String;
  Begin
    SHA1InitDigest (Digest);

    if KeySize > 64 then
      begin
        D := CalcSHA1 (Key^, KeySize);
        HMAC_KeyBlock (D, Sizeof (D), K);
      end else
      HMAC_KeyBlock (Key^, KeySize, K);

    S := K;
    XORBlock (S, $36);
    TransformSHABuffer (Digest, Pointer (S)^, True);
    SecureClearStr (S);
  End;

Procedure HMAC_SHA1Buf (var Digest : T160BitDigest; const Buf; const BufSize : Integer);
  Begin
    SHA1Buf (Digest, Buf, BufSize);
  End;

Procedure HMAC_SHA1FinalBuf (const K : String; var Digest : T160BitDigest; const Buf; const BufSize : Integer; const TotalSize : Int64);
var S : String;
  Begin
    SHA1FinalBuf (Digest, Buf, BufSize, TotalSize + 64);
    S := K;
    XORBlock (S, $5C);
    Digest := CalcSHA1 (S + SHA1DigestAsString (Digest));
    SecureClearStr (S);
  End;

Function CalcHMAC_SHA1 (const Key : Pointer; const KeySize : Integer; const Buf; const BufSize : Integer) : T160BitDigest;
var I, J : Integer;
    P    : PByte;
    K    : String;
  Begin
    HMAC_SHA1Init (Key, KeySize, Result, K);
    P := @Buf;
    if BufSize <= 0 then
      I := 0 else
      I := BufSize;
    J := (I div 64) * 64;
    if J > 0 then
      begin
        HMAC_SHA1Buf (Result, P^, J);
        Inc (P, J);
        Dec (I, J);
      end;
    HMAC_SHA1FinalBuf (K, Result, P^, I, BufSize);
    SecureClearStr (K);
  End;

Function CalcHMAC_SHA1 (const Key : String; const Buf; const BufSize : Integer) : T160BitDigest;
  Begin
    Result := CalcHMAC_SHA1 (Pointer (Key), Length (Key), Buf, BufSize);
  End;

Function CalcHMAC_SHA1 (const Key, Buf : String) : T160BitDigest;
  Begin
    Result := CalcHMAC_SHA1 (Key, Pointer (Buf)^, Length (Buf));
  End;



{                                                                              }
{ CalculateHash                                                                }
{                                                                              }
Procedure CalculateHash (const HashType : THashType;
          const Buf; const BufSize : Integer;
          const Digest : Pointer;
          const Key : Pointer; const KeySize : Integer);
  Begin
    if KeySize > 0 then
      Case HashType of
        hashHMAC_MD5  : P128BitDigest (Digest)^ := CalcHMAC_MD5 (Key, KeySize, Buf, BufSize);
        hashHMAC_SHA1 : P160BitDigest (Digest)^ := CalcHMAC_SHA1 (Key, KeySize, Buf, BufSize);
      else
        raise EHashError.Create (hashNotKeyedHashType);
      end
    else
      Case HashType of
        hashChecksum32 : PLongWord (Digest)^     := CalcChecksum32 (Buf, BufSize);
        hashXOR8       : PByte (Digest)^         := CalcXOR8 (Buf, BufSize);
        hashXOR16      : PWord (Digest)^         := CalcXOR16 (Buf, BufSize);
        hashXOR32      : PLongWord (Digest)^     := CalcXOR32 (Buf, BufSize);
        hashCRC16      : PWord (Digest)^         := CalcCRC16(Buf, BufSize);
        hashCRC32      : PLongWord (Digest)^     := CalcCRC32 (Buf, BufSize);
        hashMD5        : P128BitDigest (Digest)^ := CalcMD5 (Buf, BufSize);
        hashSHA1       : P160BitDigest (Digest)^ := CalcSHA1 (Buf, BufSize);
        hashHMAC_MD5   : P128BitDigest (Digest)^ := CalcHMAC_MD5 (nil, 0, Buf, BufSize);
        hashHMAC_SHA1  : P160BitDigest (Digest)^ := CalcHMAC_SHA1 (nil, 0, Buf, BufSize);
      else
        raise EHashError.Create (hashInvalidHashType);
      end;
  End;

Procedure CalculateHash (const HashType : THashType; const Buf; const BufSize : Integer; const Digest : Pointer; const Key : String);
  Begin
    CalculateHash (HashType, Buf, BufSize, Digest, Pointer (Key), Length (Key));
  End;

Procedure CalculateHash (const HashType : THashType; const Buf : String; const Digest : Pointer; const Key : String);
  Begin
    CalculateHash (HashType, Pointer (Buf)^, Length (Buf), Digest, Key);
  End;

  

{                                                                              }
{ AHash                                                                        }
{                                                                              }
Procedure AHash.ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64);
  Begin
    ProcessBuf (Buf, BufSize);
  End;

Procedure AHash.Init (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    Assert (Assigned (Digest), 'Assigned (Digest)');
    FDigest := Digest;
    FTotalSize := 0;
    InitHash (Digest, Key, KeySize);
  End;

Procedure AHash.Init (const Digest : Pointer; const Key : String);
  Begin
    Init (Digest, Pointer (Key), Length (Key));
  End;

Procedure AHash.HashBuf (const Buf; const BufSize : Integer; const FinalBuf : Boolean);
var I : Integer;
    P : PChar;
  Begin
    Inc (FTotalSize, BufSize);

    P := @Buf;
    I := (BufSize div 64) * 64;
    if I > 0 then
      begin
        ProcessBuf (P^, I);
        Inc (P, I);
      end;

    I := BufSize mod 64;
    if FinalBuf then
      ProcessFinalBuf (P^, I, FTotalSize) else
      if I > 0 then
        raise EHashError.Create (hashInvalidBufferSize, 'Buffer must be multiple of 64 bytes');
  End;

Procedure AHash.HashFile (const FileName : String; const Offset : Int64; const MaxCount : Int64);
const ChunkSize = 8192;
var Handle : Integer;
    Buf    : Pointer;
    I, C   : Integer;
    Left   : Int64;
    Fin    : Boolean;
  Begin
    if FileName = '' then
      raise EHashError.Create (hashInvalidFileName);
    Handle := FileOpen (FileName, fmOpenReadWrite or fmShareDenyNone);
    if Handle = -1 then
      raise EHashError.Create (hashFileOpenError);
    if Offset > 0 then
      I := FileSeek (Handle, Offset, 0) else
    if Offset < 0 then
      I := FileSeek (Handle, Offset, 2) else
      I := 0;
    if I = -1 then
      raise EHashError.Create (hashFileSeekError);
    try
      GetMem (Buf, ChunkSize);
      try
        if MaxCount < 0 then
          Left := High (Int64) else
          Left := MaxCount;
        Repeat
          if Left > ChunkSize then
            C := ChunkSize else
            C := Left;
          if C = 0 then
            begin
              I := 0;
              Fin := True;
            end else
            begin
              I := FileRead (Handle, Buf^, C);
              if I = -1 then
                raise EHashError.Create (hashFileReadError);
              Dec (Left, I);
              Fin := (I < C) or (Left <= 0);
            end;
          HashBuf (Buf^, I, Fin);
        Until Fin;
      finally
        FreeMem (Buf, ChunkSize);
      end;
    finally
      FileClose (Handle);
    end;
  End;



{                                                                              }
{ TChecksum32Hash                                                              }
{                                                                              }
Procedure TChecksum32Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    PLongWord (Digest)^ := 0;
  End;

Procedure TChecksum32Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    PLongWord (FDigest)^ := PLongWord (FDigest)^ + CalcChecksum32 (Buf, BufSize);
  End;

class Function TChecksum32Hash.DigestSize : Integer;
  Begin
    Result := 4;
  End;



{                                                                              }
{ TXOR8Hash                                                                    }
{                                                                              }
Procedure TXOR8Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    PByte (Digest)^ := 0;
  End;

Procedure TXOR8Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    PByte (FDigest)^ := PByte (FDigest)^ xor CalcXOR8 (Buf, BufSize);
  End;

class Function TXOR8Hash.DigestSize : Integer;
  Begin
    Result := 1;
  End;



{                                                                              }
{ TXOR16Hash                                                                   }
{                                                                              }
Procedure TXOR16Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    PWord (Digest)^ := 0;
  End;

Procedure TXOR16Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    PWord (FDigest)^ := PWord (FDigest)^ xor CalcXOR16 (Buf, BufSize);
  End;

class Function TXOR16Hash.DigestSize : Integer;
  Begin
    Result := 2;
  End;



{                                                                              }
{ TXOR32Hash                                                                   }
{                                                                              }
Procedure TXOR32Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    PLongWord (Digest)^ := 0;
  End;

Procedure TXOR32Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    PLongWord (FDigest)^ := PLongWord (FDigest)^ xor CalcXOR32 (Buf, BufSize);
  End;

class Function TXOR32Hash.DigestSize : Integer;
  Begin
    Result := 4;
  End;



{                                                                              }
{ TCRC16Hash                                                                   }
{                                                                              }
Procedure TCRC16Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    CRC16Init (PWord (Digest)^);
  End;

Procedure TCRC16Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    PWord (FDigest)^ := CRC16Buf (PWord (FDigest)^, Buf, BufSize);
  End;

class Function TCRC16Hash.DigestSize : Integer;
  Begin
    Result := 2;
  End;



{                                                                              }
{ TCRC32Hash                                                                   }
{                                                                              }
Procedure TCRC32Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    CRC32Init (PLongWord (Digest)^);
  End;

Procedure TCRC32Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    PLongWord (FDigest)^ := CRC32Buf (PLongWord (FDigest)^, Buf, BufSize);
  End;

class Function TCRC32Hash.DigestSize : Integer;
  Begin
    Result := 4;
  End;



{                                                                              }
{ TELFHash                                                                     }
{                                                                              }
Procedure TELFHash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    ELFInit (PLongWord (Digest)^);
  End;

Procedure TELFHash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    PLongWord (FDigest)^ := ELFBuf (PLongWord (FDigest)^, Buf, BufSize);
  End;

class Function TELFHash.DigestSize : Integer;
  Begin
    Result := 4;
  End;



{                                                                              }
{ TMD5Hash                                                                     }
{                                                                              }
Procedure TMD5Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    MD5InitDigest (P128BitDigest (FDigest)^);
  End;

Procedure TMD5Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    MD5Buf (P128BitDigest (FDigest)^, Buf, BufSize);
  End;

Procedure TMD5Hash.ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64);
  Begin
    MD5FinalBuf (P128BitDigest (FDigest)^, Buf, BufSize, TotalSize);
  End;

class Function TMD5Hash.DigestSize : Integer;
  Begin
    Result := 16;
  End;



{                                                                              }
{ THMAC_MD5Hash                                                                }
{                                                                              }
Destructor THMAC_MD5Hash.Destroy;
  Begin
    SecureClearStr (FKey);
    inherited Destroy;
  End;

Procedure THMAC_MD5Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    HMAC_MD5Init (Key, KeySize, P128BitDigest (FDigest)^, FKey);
  End;

Procedure THMAC_MD5Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    HMAC_MD5Buf (P128BitDigest (FDigest)^, Buf, BufSize);
  End;

Procedure THMAC_MD5Hash.ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64);
  Begin
    HMAC_MD5FinalBuf (FKey, P128BitDigest (FDigest)^, Buf, BufSize, TotalSize);
  End;

class Function THMAC_MD5Hash.DigestSize : Integer;
  Begin
    Result := 16;
  End;



{                                                                              }
{ TSHA1Hash                                                                    }
{                                                                              }
Procedure TSHA1Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    SHA1InitDigest (P160BitDigest (FDigest)^);
  End;

Procedure TSHA1Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    SHA1Buf (P160BitDigest (FDigest)^, Buf, BufSize);
  End;

Procedure TSHA1Hash.ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64);
  Begin
    SHA1FinalBuf (P160BitDigest (FDigest)^, Buf, BufSize, TotalSize);
  End;

class Function TSHA1Hash.DigestSize : Integer;
  Begin
    Result := 20;
  End;



{                                                                              }
{ THMAC_SHA1Hash                                                               }
{                                                                              }
Destructor THMAC_SHA1Hash.Destroy;
  Begin
    SecureClearStr (FKey);
    inherited Destroy;
  End;

Procedure THMAC_SHA1Hash.InitHash (const Digest : Pointer; const Key : Pointer; const KeySize : Integer);
  Begin
    HMAC_SHA1Init (Key, KeySize, P160BitDigest (FDigest)^, FKey);
  End;

Procedure THMAC_SHA1Hash.ProcessBuf (const Buf; const BufSize : Integer);
  Begin
    HMAC_SHA1Buf (P160BitDigest (FDigest)^, Buf, BufSize);
  End;

Procedure THMAC_SHA1Hash.ProcessFinalBuf (const Buf; const BufSize : Integer; const TotalSize : Int64);
  Begin
    HMAC_SHA1FinalBuf (FKey, P160BitDigest (FDigest)^, Buf, BufSize, TotalSize);
  End;

class Function THMAC_SHA1Hash.DigestSize : Integer;
  Begin
    Result := 20;
  End;



{                                                                              }
{ HashString                                                                   }
{                                                                              }
Function HashString (const StrBuf : Pointer; const StrLength : Integer; const Slots : LongWord; const CaseSensitive : Boolean) : LongWord;
var P    : PChar;
    I, J : Integer;

  Procedure CRC32StrBuf (const Size : Integer);
    Begin
      if CaseSensitive then
        Result := CRC32Buf (Result, P^, Size) else
        Result := CRC32BufNoCase (Result, P^, Size);
    End;

  Begin
    // Return 0 for an empty string
    Result := 0;
    if (StrLength <= 0) or not Assigned (StrBuf) then
      exit;

    if not CRC32TableInit then
      InitCRC32Table;
    Result := $FFFFFFFF;
    P := StrBuf;

    if StrLength <= 48 then // Hash everything for short strings
      CRC32StrBuf (StrLength) else
      begin
        // Hash first 16 bytes
        CRC32StrBuf (16);

        // Hash last 16 bytes
        Inc (P, StrLength - 16);
        CRC32StrBuf (16);

        // Hash 16 bytes sampled from rest of string
        I := (StrLength - 48) div 16;
        P := StrBuf;
        Inc (P, 16);
        For J := 1 to 16 do
          begin
            CRC32StrBuf (1);
            Inc (P, I + 1);
          end;
      end;

    // Mod into slots
    if (Slots <> 0) and (Slots <> High (LongWord)) then
      Result := Result mod Slots;
  End;

Function HashString (const S : String; const Slots : LongWord; const CaseSensitive : Boolean) : LongWord;
  Begin
    Result := HashString (Pointer (S), Length (S), Slots, CaseSensitive);
  End;



{                                                                              }
{ Hash by THashType                                                            }
{                                                                              }
const
  HashTypeClasses : Array [THashType] of THashClass = (
      TChecksum32Hash, TXOR8Hash, TXOR16Hash, TXOR32Hash,
      TCRC16Hash, TCRC32Hash, TELFHash,
      TMD5Hash, TSHA1Hash,
      THMAC_MD5Hash, THMAC_SHA1Hash);

Function GetHashClassByType (const HashType : THashType) : THashClass;
  Begin
    Result := HashTypeClasses [HashType];
  End;

Function GetDigestSize (const HashType : THashType) : Integer;
  Begin
    Result := GetHashClassByType (HashType).DigestSize;
  End;

  

{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$IFDEF SELFTEST}
Procedure Test_Unit (const U : TUnit);
var S, T : String;
  Begin
    With U do
      begin
        TestID := 'Hash';

        AssertEq (CalcChecksum32 (''), 0, 'CalcChecksum32');
        AssertEq (CalcChecksum32 ('A'), 65, 'CalcChecksum32');
        AssertEq (CalcChecksum32 ('Delphi Fundamentals'), 1880, 'CalcChecksum32');

        AssertEq (CalcXOR8 (''), 0, 'CalcXOR8');
        AssertEq (CalcXOR8 ('A'), 65, 'CalcXOR8');
        AssertEq (CalcXOR8 ('Delphi Fundamentals'), 40, 'CalcXOR8');

        AssertEq (CalcXOR16 (''), 0, 'CalcXOR16');
        AssertEq (CalcXOR16 ('A'), 65, 'CalcXOR16');
        AssertEq (CalcXOR16 ('AB'), $4241, 'CalcXOR16');
        AssertEq (CalcXOR16 ('Delphi Fundamentals'), $4860, 'CalcXOR16');

        AssertEq (CalcXOR32 (''), 0, 'CalcXOR32');
        AssertEq (CalcXOR32 ('A'), 65, 'CalcXOR32');
        AssertEq (CalcXOR32 ('ABCD'), $44434241, 'CalcXOR32');
        AssertEq (CalcXOR32 ('Delphi Fundamentals'), $23356B55, 'CalcXOR32');

        AssertEq (CalcCRC16 (''), $FFFF, 'CalcCRC16');
        AssertEq (CalcCRC16 ('Delphi Fundamentals'), 18831, 'CalcCRC16');

        AssertEq (CalcCRC32 (''), 0, 'CalcCRC32');
        AssertEq (CalcCRC32 ('Delphi Fundamentals'), 3810393938, 'CalcCRC32');

        AssertEq (MD5DigestToHex (CalcMD5 ('')),                    'd41d8cd98f00b204e9800998ecf8427e', 'CalcMD5');
        AssertEq (MD5DigestToHex (CalcMD5 ('Delphi Fundamentals')), 'ea98b65da23d19756d46a36faa481dd8', 'CalcMD5');

        AssertEq (SHA1DigestToHex (CalcSHA1 ('')),                    'da39a3ee5e6b4b0d3255bfef95601890afd80709', 'CalcSHA1');
        AssertEq (SHA1DigestToHex (CalcSHA1 ('Delphi Fundamentals')), '6c412217909d2767d36a6bbeab5e50e14b19b941', 'CalcSHA1');

        AssertEq (MD5DigestToHex (CalcHMAC_MD5 ('', '')),                    '74e6f7298a9c2d168935f58c001bad88', 'CalcHMAC_MD5');
        AssertEq (MD5DigestToHex (CalcHMAC_MD5 ('', 'Delphi Fundamentals')), 'b9da02d5f94bd6eac410708a72b05d9f', 'CalcHMAC_MD5');
        AssertEq (MD5DigestToHex (CalcHMAC_MD5 ('Delphi Fundamentals', '')), 'a09f3300c236156d27f4d031db7e91ce', 'CalcHMAC_MD5');
        AssertEq (MD5DigestToHex (CalcHMAC_MD5 ('Delphi', 'Fundamentals')),  '1c4e8a481c2c781eb43ca58d9324c37d', 'CalcHMAC_MD5');

        AssertEq (SHA1DigestToHex (CalcHMAC_SHA1 ('', '')),                    'fbdb1d1b18aa6c08324b7d64b71fb76370690e1d', 'CalcHMAC_SHA1');
        AssertEq (SHA1DigestToHex (CalcHMAC_SHA1 ('', 'Delphi Fundamentals')), '62f9196071f587cde151d8b99919ed0f6e51bf26', 'CalcHMAC_SHA1');
        AssertEq (SHA1DigestToHex (CalcHMAC_SHA1 ('Delphi Fundamentals', '')), 'e4dbfa59f410ee75c368c1ba6df1a2c701e0cea0', 'CalcHMAC_SHA1');
        AssertEq (SHA1DigestToHex (CalcHMAC_SHA1 ('Delphi', 'Fundamentals')),  'fa96341a0b790f3a6f3248b7053372ede8d41e7c', 'CalcHMAC_SHA1');

        AssertEq (HashString ('Delphi Fundamentals', 0, False),
                  HashString ('dELPHI fundamentalS', 0, False), 'HashString');
        AssertEq (HashString ('Delphi Fundamentals', 0, True), 484573357);

        AssertEq (IsValidISBN ('3880530025'), True, 'ISBN');

        AssertEq (IsValidLUHN ('49927398716'), True, 'ISBN');

        // Test cases from RFC 2202
        AssertEq (MD5DigestToHex (CalcHMAC_MD5 ('Jefe', 'what do ya want for nothing?')), '750c783e6ab0b503eaa86e310a5db738', 'CalcHMAC_MD5');
        SetLength (S, 16); FillChar (Pointer (S)^, 16, #$0B);
        AssertEq (MD5DigestToHex (CalcHMAC_MD5 (S, 'Hi There')), '9294727a3638bb1c13f48ef8158bfc9d', 'CalcHMAC_MD5');
        SetLength (S, 16); FillChar (Pointer (S)^, 16, #$AA);
        SetLength (T, 50); FillChar (Pointer (T)^, 50, #$DD);
        AssertEq (MD5DigestToHex (CalcHMAC_MD5 (S, T)), '56be34521d144c88dbb8c733f0e8b3f6', 'CalcHMAC_MD5');
        SetLength (S, 80); FillChar (Pointer (S)^, 80, #$AA);
        AssertEq (MD5DigestToHex (CalcHMAC_MD5 (S, 'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data')), '6f630fad67cda0ee1fb1f562db3aa53e', 'CalcHMAC_MD5');

        AssertEq (SHA1DigestToHex (CalcHMAC_SHA1 ('Jefe', 'what do ya want for nothing?')), 'effcdf6ae5eb2fa2d27416d5f184df9c259a7c79', 'CalcHMAC_SHA1');
        SetLength (S, 20); FillChar (Pointer (S)^, 20, #$0B);
        AssertEq (SHA1DigestToHex (CalcHMAC_SHA1 (S, 'Hi There')), 'b617318655057264e28bc0b6fb378c8ef146be00', 'CalcHMAC_SHA1');
        SetLength (S, 80); FillChar (Pointer (S)^, 80, #$AA);
        AssertEq (SHA1DigestToHex (CalcHMAC_SHA1 (S, 'Test Using Larger Than Block-Size Key - Hash Key First')), 'aa4ae5e15272d00e95705637ce8a3b55ed402112', 'CalcHMAC_SHA1');
      end;
  End;
{$ENDIF}



end.

