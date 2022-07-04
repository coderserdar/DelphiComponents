{$INCLUDE ..\cDefines.inc}
unit cDictionaries;

{                                                                              }
{                     Data structures: Dictionaries v3.11                      }
{                                                                              }
{             This unit is copyright © 1999-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                 Its original file name is cDictionaries.pas                  }
{                      It was generated 1 Aug 2004 23:30.                      }
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
{   Dictionaries are associative arrays where the key value is a string.       }
{                                                                              }
{   Associative arrays, also referred to as mappings, are unordered            }
{   collections where an arbitrary key can be used to index a value.           }
{                                                                              }
{   This unit implements dictionary classes for each of the following types:   }
{     + Integer                                                                }
{     + Cardinal                                                               }
{     + Int64                                                                  }
{     + Single                                                                 }
{     + Double                                                                 }
{     + Extended                                                               }
{     + Pointer                                                                }
{     + String                                                                 }
{     + TObject                                                                }
{     + IInterface                                                             }
{                                                                              }
{   For example, the class TIntegerDictionary is used where the key is an      }
{   arbitrary string and the value an integer.                                 }
{                                                                              }
{       Ages := TIntegerDictionary.Create;                                     }
{       Ages['John'] := 29;                                                    }
{       Ages['Tori'] := 35;                                                    }
{       if Ages.HasKey['John'] then                                            }
{         Ages.Delete('John');                                                 }
{       Ages.Free;                                                             }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   [ cDataStructs ]                                                           }
{   1999/11/12  0.01  Split cTypes from cDataStruct and cHolder.               }
{   2000/06/16  1.02  Added ADictionary.                                       }
{   2000/06/14  1.03  Converted cDataStructs to template.                      }
{   2000/06/16  1.04  Added dictionaries stored in AArrays.                    }
{   2000/07/07  1.05  Added ATypeDictionary.                                   }
{   2001/01/19  1.06  Added THashedStringDictionary.                           }
{   2001/04/13  1.07  Added TObjectDictionary.                                 }
{   2001/08/20  2.08  Merged cTypes and cDataStructs to allow object           }
{                     interface implementation in base classes.                }
{   2002/01/14  2.09  Replaced AllowDuplicates property with DuplicatesAction  }
{                     property.                                                }
{   [ cDictionaries ]                                                          }
{   2002/05/15  3.10  Created cDictionaries unit from cDataStructs.            }
{                     Refactored for Fundamentals 3.                           }
{   2003/09/11  3.11  Added TInterfaceDictionary.                              }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cUtils,
  cTypes,
  cArrays;

const
  UnitName      = 'cDictionaries';
  UnitVersion   = '3.11';
  UnitDesc      = 'Data structures: Dictionaries';
  UnitCopyright = 'Copyright (c) 1999-2004 David J Butler';



{                                                                              }
{ DICTIONARY BASE CLASSES                                                      }
{   Classes with the 'A' prefix are abstract base classes. They define the     }
{   interface for the type and must never be instanciated.                     }
{   Instead, create one of the implementation classes with a 'T' prefix.       }
{                                                                              }



{                                                                              }
{ ADictionary                                                                  }
{   Base class for a dictionary.                                               }
{                                                                              }
type
  TDictionaryDuplicatesAction = (ddError,    // raises an exception on duplicate keys
                                 ddAccept,   // allow duplicate keys
                                 ddIgnore);  // silently discard duplicates
  ADictionary = class(AType)
  protected
    procedure RaiseDictionaryError(const Msg: String;
              const ErrorClass: ExceptClass = nil);
    procedure RaiseKeyNotFoundError(const Key: String;
              const ErrorClass: ExceptClass = nil);
    procedure RaiseDuplicateKeyError(const Key: String;
              const ErrorClass: ExceptClass = nil);

    function  GetAddOnSet: Boolean; virtual; abstract;
    procedure SetAddOnSet(const AddOnSet: Boolean); virtual; abstract;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; virtual; abstract;
    procedure SetDuplicatesAction(const Value: TDictionaryDuplicatesAction); virtual; abstract;
    function  GetKeysCaseSensitive: Boolean; virtual; abstract;

  public
    { ADictionary                                                              }
    procedure Delete(const Key: String); virtual; abstract;
    function  HasKey(const Key: String): Boolean; virtual; abstract;
    procedure Rename(const Key, NewKey: String); virtual; abstract;

    function  Count: Integer; virtual; abstract;
    function  GetKeyByIndex(const Idx: Integer): String; virtual; abstract;
    procedure DeleteItemByIndex(const Idx: Integer); virtual; abstract;

    property  AddOnSet: Boolean read GetAddOnSet write SetAddOnSet;
    property  DuplicatesAction: TDictionaryDuplicatesAction
              read GetDuplicatesAction write SetDuplicatesAction;
    property  KeysCaseSensitive: Boolean read GetKeysCaseSensitive;
  end;
  EDictionary = class(EType);



{                                                                              }
{ ALongIntDictionary                                                           }
{   A Dictionary with LongInt values and String keys.                          }
{                                                                              }
type
  ALongIntDictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): LongInt; virtual;
    procedure SetItem(const Key: String; const Value: LongInt); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { ALongIntDictionary                                                      }
    property  Item[const Key: String]: LongInt read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: LongInt); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): LongInt; virtual; abstract;
    function  LocateItem(const Key: String; var Value: LongInt): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: LongInt): Integer; virtual; abstract;
  end;
  ELongIntDictionary = class(EDictionary);



{                                                                              }
{ AIntegerDictionary                                                           }
{                                                                              }
type
  AIntegerDictionary = ALongIntDictionary;



{                                                                              }
{ ALongWordDictionary                                                          }
{   A Dictionary with LongWord values and String keys.                         }
{                                                                              }
type
  ALongWordDictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): LongWord; virtual;
    procedure SetItem(const Key: String; const Value: LongWord); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { ALongWordDictionary                                                      }
    property  Item[const Key: String]: LongWord read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: LongWord); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): LongWord; virtual; abstract;
    function  LocateItem(const Key: String; var Value: LongWord): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: LongWord): Integer; virtual; abstract;
  end;
  ELongWordDictionary = class(EDictionary);



{                                                                              }
{ ACardinalArray                                                               }
{                                                                              }
type
  ACardinalDictionary = ALongWordDictionary;



{                                                                              }
{ AInt64Dictionary                                                             }
{   A Dictionary with Int64 values and String keys.                            }
{                                                                              }
type
  AInt64Dictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): Int64; virtual;
    procedure SetItem(const Key: String; const Value: Int64); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { AInt64Dictionary                                                      }
    property  Item[const Key: String]: Int64 read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: Int64); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): Int64; virtual; abstract;
    function  LocateItem(const Key: String; var Value: Int64): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Int64): Integer; virtual; abstract;
  end;
  EInt64Dictionary = class(EDictionary);



{                                                                              }
{ ASingleDictionary                                                            }
{   A Dictionary with Single values and String keys.                           }
{                                                                              }
type
  ASingleDictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): Single; virtual;
    procedure SetItem(const Key: String; const Value: Single); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { ASingleDictionary                                                      }
    property  Item[const Key: String]: Single read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: Single); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): Single; virtual; abstract;
    function  LocateItem(const Key: String; var Value: Single): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Single): Integer; virtual; abstract;
  end;
  ESingleDictionary = class(EDictionary);



{                                                                              }
{ ADoubleDictionary                                                            }
{   A Dictionary with Double values and String keys.                           }
{                                                                              }
type
  ADoubleDictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): Double; virtual;
    procedure SetItem(const Key: String; const Value: Double); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { ADoubleDictionary                                                      }
    property  Item[const Key: String]: Double read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: Double); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): Double; virtual; abstract;
    function  LocateItem(const Key: String; var Value: Double): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Double): Integer; virtual; abstract;
  end;
  EDoubleDictionary = class(EDictionary);



{                                                                              }
{ AExtendedDictionary                                                          }
{   A Dictionary with Extended values and String keys.                         }
{                                                                              }
type
  AExtendedDictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): Extended; virtual;
    procedure SetItem(const Key: String; const Value: Extended); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { AExtendedDictionary                                                      }
    property  Item[const Key: String]: Extended read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: Extended); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): Extended; virtual; abstract;
    function  LocateItem(const Key: String; var Value: Extended): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Extended): Integer; virtual; abstract;
  end;
  EExtendedDictionary = class(EDictionary);



{                                                                              }
{ APointerDictionary                                                           }
{   A Dictionary with Pointer values and String keys.                          }
{                                                                              }
type
  APointerDictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): Pointer; virtual;
    procedure SetItem(const Key: String; const Value: Pointer); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { APointerDictionary                                                      }
    property  Item[const Key: String]: Pointer read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: Pointer); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): Pointer; virtual; abstract;
    function  LocateItem(const Key: String; var Value: Pointer): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Pointer): Integer; virtual; abstract;
  end;
  EPointerDictionary = class(EDictionary);



{                                                                              }
{ AStringDictionary                                                            }
{   A Dictionary with String values and String keys.                           }
{                                                                              }
type
  AStringDictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): String; virtual;
    procedure SetItem(const Key: String; const Value: String); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { AStringDictionary                                                      }
    property  Item[const Key: String]: String read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: String); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): String; virtual; abstract;
    function  LocateItem(const Key: String; var Value: String): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: String): Integer; virtual; abstract;

    function  GetItemLength(const Key: String): Integer; virtual;
    function  GetTotalLength: Int64; virtual;
  end;
  EStringDictionary = class(EDictionary);






{                                                                              }
{ AObjectDictionary                                                            }
{                                                                              }
type
  AObjectDictionary = class(ADictionary)
  protected
    function  GetItem(const Key: String): TObject; virtual;
    procedure SetItem(const Key: String; const Value: TObject); virtual; abstract;
    function  GetIsItemOwner: Boolean; virtual; abstract;
    procedure SetIsItemOwner(const IsItemOwner: Boolean); virtual; abstract;

  public
    { AType                                                                    }
    function  GetAsString: String; override;
    procedure Clear; override;
    procedure Assign(const Source: TObject); overload; override;

    { AObjectDictionary                                                        }
    procedure Add(const Key: String; const Value: TObject); virtual; abstract;
    property  Item[const Key: String]: TObject read GetItem write SetItem; default;

    function  GetItemByIndex(const Idx: Integer): TObject; virtual; abstract;
    function  LocateItem(const Key: String; var Value: TObject): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: TObject): Integer; virtual; abstract;

    property  IsItemOwner: Boolean read GetIsItemOwner write SetIsItemOwner;
    function  ReleaseItem(const Key: String): TObject; virtual; abstract;
    procedure ReleaseItems; virtual; abstract;
    procedure FreeItems; virtual; abstract;
  end;
  EObjectDictionary = class(EDictionary);



{                                                                              }
{ AInterfaceDictionary                                                         }
{   A Dictionary with Interface values and String keys.                        }
{                                                                              }
type
  AInterfaceDictionary = class(ADictionary)
  protected
    function  GetAsString: String; override;

    function  GetItem(const Key: String): IInterface; virtual;
    procedure SetItem(const Key: String; const Value: IInterface); virtual; abstract;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;

    { AInterfaceDictionary                                                      }
    property  Item[const Key: String]: IInterface read GetItem write SetItem; default;
    procedure Add(const Key: String; const Value: IInterface); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): IInterface; virtual; abstract;
    function  LocateItem(const Key: String; var Value: IInterface): Integer; virtual; abstract;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: IInterface): Integer; virtual; abstract;
  end;
  EInterfaceDictionary = class(EDictionary);






{                                                                              }
{ DICTIONARY IMPLEMENTATIONS                                                   }
{                                                                              }



{                                                                              }
{ TLongIntDictionary                                                           }
{   Implements ALongIntDictionary using arrays.                                }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralLongIntDictionary = class(ALongIntDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : ALongIntArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { ALongIntDictionary                                                    }
    procedure SetItem(const Key: String; const Value: LongInt); override;

  public
    { TGeneralLongIntDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: ALongIntArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: ALongIntArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { ALongIntDictionary                                                    }
    procedure Add(const Key: String; const Value: LongInt); override;
    function  GetItemByIndex(const Idx: Integer): LongInt; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: LongInt);
    function  LocateItem(const Key: String; var Value: LongInt): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: LongInt): Integer; override;
  end;

  TLongIntDictionary = class(TGeneralLongIntDictionary)
  protected
    function  GetItem(const Key: String): LongInt; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TLongIntArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: LongInt): Integer; override;
  end;



{                                                                              }
{ TIntegerDictionary                                                           }
{                                                                              }
type
  TGeneralIntegerDictionary = TGeneralLongIntDictionary;
  TIntegerDictionary = TLongIntDictionary;



{                                                                              }
{ TLongWordDictionary                                                          }
{   Implements ALongWordDictionary using arrays.                               }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralLongWordDictionary = class(ALongWordDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : ALongWordArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { ALongWordDictionary                                                    }
    procedure SetItem(const Key: String; const Value: LongWord); override;

  public
    { TGeneralLongWordDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: ALongWordArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: ALongWordArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { ALongWordDictionary                                                    }
    procedure Add(const Key: String; const Value: LongWord); override;
    function  GetItemByIndex(const Idx: Integer): LongWord; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: LongWord);
    function  LocateItem(const Key: String; var Value: LongWord): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: LongWord): Integer; override;
  end;

  TLongWordDictionary = class(TGeneralLongWordDictionary)
  protected
    function  GetItem(const Key: String): LongWord; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TLongWordArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: LongWord): Integer; override;
  end;



{                                                                              }
{ TCardinalDictionary                                                          }
{                                                                              }
type
  TGeneralCardinalDictionary = TGeneralLongWordDictionary;
  TCardinalDictionary = TLongWordDictionary;



{                                                                              }
{ TInt64Dictionary                                                             }
{   Implements AInt64Dictionary using arrays.                                  }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralInt64Dictionary = class(AInt64Dictionary)
  protected
    FKeys             : AStringArray;
    FValues           : AInt64Array;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { AInt64Dictionary                                                    }
    procedure SetItem(const Key: String; const Value: Int64); override;

  public
    { TGeneralInt64Dictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: AInt64Array = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: AInt64Array read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { AInt64Dictionary                                                    }
    procedure Add(const Key: String; const Value: Int64); override;
    function  GetItemByIndex(const Idx: Integer): Int64; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: Int64);
    function  LocateItem(const Key: String; var Value: Int64): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Int64): Integer; override;
  end;

  TInt64Dictionary = class(TGeneralInt64Dictionary)
  protected
    function  GetItem(const Key: String): Int64; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TInt64Array = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: Int64): Integer; override;
  end;



{                                                                              }
{ TSingleDictionary                                                            }
{   Implements ASingleDictionary using arrays.                                 }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralSingleDictionary = class(ASingleDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : ASingleArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { ASingleDictionary                                                    }
    procedure SetItem(const Key: String; const Value: Single); override;

  public
    { TGeneralSingleDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: ASingleArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: ASingleArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { ASingleDictionary                                                    }
    procedure Add(const Key: String; const Value: Single); override;
    function  GetItemByIndex(const Idx: Integer): Single; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: Single);
    function  LocateItem(const Key: String; var Value: Single): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Single): Integer; override;
  end;

  TSingleDictionary = class(TGeneralSingleDictionary)
  protected
    function  GetItem(const Key: String): Single; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TSingleArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: Single): Integer; override;
  end;



{                                                                              }
{ TDoubleDictionary                                                            }
{   Implements ADoubleDictionary using arrays.                                 }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralDoubleDictionary = class(ADoubleDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : ADoubleArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { ADoubleDictionary                                                    }
    procedure SetItem(const Key: String; const Value: Double); override;

  public
    { TGeneralDoubleDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: ADoubleArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: ADoubleArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { ADoubleDictionary                                                    }
    procedure Add(const Key: String; const Value: Double); override;
    function  GetItemByIndex(const Idx: Integer): Double; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: Double);
    function  LocateItem(const Key: String; var Value: Double): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Double): Integer; override;
  end;

  TDoubleDictionary = class(TGeneralDoubleDictionary)
  protected
    function  GetItem(const Key: String): Double; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TDoubleArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: Double): Integer; override;
  end;



{                                                                              }
{ TExtendedDictionary                                                          }
{   Implements AExtendedDictionary using arrays.                               }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralExtendedDictionary = class(AExtendedDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : AExtendedArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { AExtendedDictionary                                                    }
    procedure SetItem(const Key: String; const Value: Extended); override;

  public
    { TGeneralExtendedDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: AExtendedArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: AExtendedArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { AExtendedDictionary                                                    }
    procedure Add(const Key: String; const Value: Extended); override;
    function  GetItemByIndex(const Idx: Integer): Extended; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: Extended);
    function  LocateItem(const Key: String; var Value: Extended): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Extended): Integer; override;
  end;

  TExtendedDictionary = class(TGeneralExtendedDictionary)
  protected
    function  GetItem(const Key: String): Extended; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TExtendedArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: Extended): Integer; override;
  end;



{                                                                              }
{ TPointerDictionary                                                           }
{   Implements APointerDictionary using arrays.                                }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralPointerDictionary = class(APointerDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : APointerArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { APointerDictionary                                                    }
    procedure SetItem(const Key: String; const Value: Pointer); override;

  public
    { TGeneralPointerDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: APointerArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: APointerArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { APointerDictionary                                                    }
    procedure Add(const Key: String; const Value: Pointer); override;
    function  GetItemByIndex(const Idx: Integer): Pointer; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: Pointer);
    function  LocateItem(const Key: String; var Value: Pointer): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: Pointer): Integer; override;
  end;

  TPointerDictionary = class(TGeneralPointerDictionary)
  protected
    function  GetItem(const Key: String): Pointer; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TPointerArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: Pointer): Integer; override;
  end;



{                                                                              }
{ TStringDictionary                                                            }
{   Implements AStringDictionary using arrays.                                 }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralStringDictionary = class(AStringDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : AStringArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { AStringDictionary                                                    }
    procedure SetItem(const Key: String; const Value: String); override;

  public
    { TGeneralStringDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: AStringArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: AStringArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { AStringDictionary                                                    }
    procedure Add(const Key: String; const Value: String); override;
    function  GetItemByIndex(const Idx: Integer): String; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: String);
    function  LocateItem(const Key: String; var Value: String): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: String): Integer; override;
  end;

  TStringDictionary = class(TGeneralStringDictionary)
  protected
    function  GetItem(const Key: String): String; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TStringArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: String): Integer; override;
  end;



{                                                                              }
{ TObjectDictionary                                                            }
{   Implements AObjectDictionary using arrays.                                 }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralObjectDictionary = class(AObjectDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : AObjectArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { AObjectDictionary                                                        }
    function  GetIsItemOwner: Boolean; override;
    procedure SetIsItemOwner(const IsItemOwner: Boolean); override;
    
    procedure SetItem(const Key: String; const Value: TObject); override;

  public
    { TGeneralObjectDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: AObjectArray = nil;
                const IsItemOwner: Boolean = False;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: AObjectArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { AObjectDictionary                                                        }
    procedure Add(const Key: String; const Value: TObject); override;
    function  GetItemByIndex(const Idx: Integer): TObject; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: TObject);
    function  LocateItem(const Key: String; var Value: TObject): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: TObject): Integer; override;

    function  ReleaseItem(const Key: String): TObject; override;
    procedure ReleaseItems; override;
    procedure FreeItems; override;
  end;

  TObjectDictionary = class(TGeneralObjectDictionary)
  protected
    function  GetItem(const Key: String): TObject; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TObjectArray = nil;
                const IsItemOwner: Boolean = False;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: TObject): Integer; override;
  end;



{                                                                              }
{ TInterfaceDictionary                                                         }
{   Implements AInterfaceDictionary using arrays.                              }
{   A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneralInterfaceDictionary = class(AInterfaceDictionary)
  protected
    FKeys             : AStringArray;
    FValues           : AInterfaceArray;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction); override;

    { AInterfaceDictionary                                                    }
    procedure SetItem(const Key: String; const Value: IInterface); override;

  public
    { TGeneralInterfaceDictionary                                               }
    constructor Create;
    constructor CreateEx(const Keys: AStringArray = nil;
                const Values: AInterfaceArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: AStringArray read FKeys;
    property  Values: AInterfaceArray read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: String); override;
    function  HasKey(const Key: String): Boolean; override;
    procedure Rename(const Key: String; const NewKey: String); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { AInterfaceDictionary                                                    }
    procedure Add(const Key: String; const Value: IInterface); override;
    function  GetItemByIndex(const Idx: Integer): IInterface; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: IInterface);
    function  LocateItem(const Key: String; var Value: IInterface): Integer; override;
    function  LocateNext(const Key: String; const Idx: Integer;
              var Value: IInterface): Integer; override;
  end;

  TInterfaceDictionary = class(TGeneralInterfaceDictionary)
  protected
    function  GetItem(const Key: String): IInterface; override;
    function  LocateKey(const Key: String; var LookupIdx: Integer;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(const Keys: TStringArray = nil;
                const Values: TInterfaceArray = nil;
                const KeysCaseSensitive: Boolean = True;
                const AddOnSet: Boolean = True;
                const DuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: String; var Value: IInterface): Integer; override;
  end;



{                                                                              }
{ Dictionary functions                                                         }
{                                                                              }
const
  AverageHashChainSize = 4;

function  DictionaryRehashSize(const Count: Integer): Integer;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Fundamentals }
  cStrings;



{                                                                              }
{ DICTIONARY BASE CLASSES                                                      }
{                                                                              }



{                                                                              }
{ ADictionary                                                                  }
{                                                                              }
procedure ADictionary.RaiseDictionaryError(const Msg: String;
    const ErrorClass: ExceptClass);
var E : ExceptClass;
begin
  if not Assigned(ErrorClass) then
    E := EDictionary
  else
    E := ErrorClass;
  RaiseTypeError(Msg, nil, E);
end;

procedure ADictionary.RaiseKeyNotFoundError(const Key: String;
    const ErrorClass: ExceptClass);
begin
  RaiseDictionaryError('Key not found: ' + Key, ErrorClass);
end;

procedure ADictionary.RaiseDuplicateKeyError(const Key: String;
    const ErrorClass: ExceptClass);
begin
  RaiseDictionaryError('Duplicate key: ' + Key, ErrorClass);
end;



{                                                                              }
{ ALongIntDictionary                                                           }
{                                                                              }
function ALongIntDictionary.GetItem(const Key: String): LongInt;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, ELongIntDictionary);
end;

procedure ALongIntDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is ALongIntDictionary then
    begin
      Clear;
      For I := 0 to ALongIntDictionary(Source).Count - 1 do
        Add(ALongIntDictionary(Source).GetKeyByIndex(I),
             ALongIntDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function ALongIntDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + IntToStr(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;



{                                                                              }
{ ALongWordDictionary                                                          }
{                                                                              }
function ALongWordDictionary.GetItem(const Key: String): LongWord;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, ELongWordDictionary);
end;

procedure ALongWordDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is ALongWordDictionary then
    begin
      Clear;
      For I := 0 to ALongWordDictionary(Source).Count - 1 do
        Add(ALongWordDictionary(Source).GetKeyByIndex(I),
             ALongWordDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function ALongWordDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + IntToStr(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;



{                                                                              }
{ AInt64Dictionary                                                             }
{                                                                              }
function AInt64Dictionary.GetItem(const Key: String): Int64;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, EInt64Dictionary);
end;

procedure AInt64Dictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is AInt64Dictionary then
    begin
      Clear;
      For I := 0 to AInt64Dictionary(Source).Count - 1 do
        Add(AInt64Dictionary(Source).GetKeyByIndex(I),
             AInt64Dictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function AInt64Dictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + IntToStr(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;



{                                                                              }
{ ASingleDictionary                                                            }
{                                                                              }
function ASingleDictionary.GetItem(const Key: String): Single;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, ESingleDictionary);
end;

procedure ASingleDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is ASingleDictionary then
    begin
      Clear;
      For I := 0 to ASingleDictionary(Source).Count - 1 do
        Add(ASingleDictionary(Source).GetKeyByIndex(I),
             ASingleDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function ASingleDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + FloatToStr(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;



{                                                                              }
{ ADoubleDictionary                                                            }
{                                                                              }
function ADoubleDictionary.GetItem(const Key: String): Double;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, EDoubleDictionary);
end;

procedure ADoubleDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is ADoubleDictionary then
    begin
      Clear;
      For I := 0 to ADoubleDictionary(Source).Count - 1 do
        Add(ADoubleDictionary(Source).GetKeyByIndex(I),
             ADoubleDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function ADoubleDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + FloatToStr(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;



{                                                                              }
{ AExtendedDictionary                                                          }
{                                                                              }
function AExtendedDictionary.GetItem(const Key: String): Extended;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, EExtendedDictionary);
end;

procedure AExtendedDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is AExtendedDictionary then
    begin
      Clear;
      For I := 0 to AExtendedDictionary(Source).Count - 1 do
        Add(AExtendedDictionary(Source).GetKeyByIndex(I),
             AExtendedDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function AExtendedDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + FloatToStr(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;



{                                                                              }
{ AStringDictionary                                                            }
{                                                                              }
function AStringDictionary.GetItem(const Key: String): String;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, EStringDictionary);
end;

procedure AStringDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is AStringDictionary then
    begin
      Clear;
      For I := 0 to AStringDictionary(Source).Count - 1 do
        Add(AStringDictionary(Source).GetKeyByIndex(I),
             AStringDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function AStringDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + StrQuote(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;

function AStringDictionary.GetItemLength(const Key: String): Integer;
begin
  Result := Length(GetItem(Key));
end;

function AStringDictionary.GetTotalLength: Int64;
var I : Integer;
begin
  Result := 0;
  For I := 0 to Count - 1 do
    Inc(Result, Length(GetItemByIndex(I)));
end;



{                                                                              }
{ APointerDictionary                                                           }
{                                                                              }
function APointerDictionary.GetItem(const Key: String): Pointer;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, EPointerDictionary);
end;

procedure APointerDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is APointerDictionary then
    begin
      Clear;
      For I := 0 to APointerDictionary(Source).Count - 1 do
        Add(APointerDictionary(Source).GetKeyByIndex(I),
             APointerDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function APointerDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + PointerToStr(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;



{                                                                              }
{ AObjectDictionary                                                            }
{                                                                              }
function AObjectDictionary.GetItem(const Key: String): TObject;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, EObjectDictionary);
end;

function AObjectDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I) + ':' + ObjectClassName(GetItemByIndex(I));
      if I < L then
        Result := Result + ',';
    end;
end;

procedure AObjectDictionary.Clear;
begin
  if IsItemOwner then
    FreeItems else
    ReleaseItems;
end;

procedure AObjectDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is AObjectDictionary then
    begin
      Clear;
      For I := 0 to AObjectDictionary(Source).Count - 1 do
        Add(AObjectDictionary(Source).GetKeyByIndex(I),
             AObjectDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;



{                                                                              }
{ AInterfaceDictionary                                                         }
{                                                                              }
function AInterfaceDictionary.GetItem(const Key: String): IInterface;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key, EInterfaceDictionary);
end;

procedure AInterfaceDictionary.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is AInterfaceDictionary then
    begin
      Clear;
      For I := 0 to AInterfaceDictionary(Source).Count - 1 do
        Add(AInterfaceDictionary(Source).GetKeyByIndex(I),
             AInterfaceDictionary(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function AInterfaceDictionary.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  For I := 0 to L do
    begin
      Result := Result + GetKeyByIndex(I);
      if I < L then
        Result := Result + ',';
    end;
end;



{                                                                              }
{ DICTIONARY IMPLEMENTATIONS                                                   }
{                                                                              }



{ Dictionary helper functions                                                  }
function DictionaryRehashSize(const Count: Integer): Integer;
var L : Integer;
begin
  L := Count div AverageHashChainSize; // Number of "slots"
  if L <= $10 then                     // Rehash in powers of 16
    Result := $10 else
  if L <= $100 then
    Result := $100 else
  if L <= $1000 then
    Result := $1000 else
  if L <= $10000 then
    Result := $10000 else
  if L <= $100000 then
    Result := $100000 else
  if L <= $1000000 then
    Result := $1000000 else
    Result := $10000000;
end;

{                                                                              }
{ TGeneralLongIntDictionary                                                    }
{                                                                              }
constructor TGeneralLongIntDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TLongIntArray.Create;
end;

constructor TGeneralLongIntDictionary.CreateEx(const Keys: AStringArray;
    const Values: ALongIntArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TLongIntArray.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TLongIntDictionary.CreateEx(const Keys: TStringArray;
    const Values: TLongIntArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralLongIntDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralLongIntDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralLongIntDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralLongIntDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralLongIntDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralLongIntDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralLongIntDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, ELongIntDictionary);
  Result := -1;
end;

procedure TGeneralLongIntDictionary.Add(const Key: String; const Value: LongInt);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralLongIntDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralLongIntDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralLongIntDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralLongIntDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralLongIntDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralLongIntDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralLongIntDictionary.LocateItem(const Key: String; var Value: LongInt): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := 0;
end;

function TGeneralLongIntDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: LongInt): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, ELongIntDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, ELongIntDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, ELongIntDictionary);
end;

procedure TGeneralLongIntDictionary.SetItem(const Key: String; const Value: LongInt);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, ELongIntDictionary);
end;

procedure TGeneralLongIntDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', ELongIntDictionary);
end;

function TGeneralLongIntDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralLongIntDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralLongIntDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralLongIntDictionary.GetItemByIndex(const Idx: Integer): LongInt;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralLongIntDictionary.SetItemByIndex(const Idx: Integer; const Value: LongInt);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralLongIntDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TLongIntDictionary                                                           }
{                                                                              }
function TLongIntDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, ELongIntDictionary);
  Result := -1;
end;

function TLongIntDictionary.GetItem(const Key: String): LongInt;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TLongIntArray(FValues).Data[I]
  else
    Result := 0;
end;

function TLongIntDictionary.LocateItem(const Key: String; var Value: LongInt): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TLongIntArray(FValues).Data[Result]
  else
    Value := 0;
end;



{                                                                              }
{ TGeneralLongWordDictionary                                                   }
{                                                                              }
constructor TGeneralLongWordDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TLongWordArray.Create;
end;

constructor TGeneralLongWordDictionary.CreateEx(const Keys: AStringArray;
    const Values: ALongWordArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TLongWordArray.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TLongWordDictionary.CreateEx(const Keys: TStringArray;
    const Values: TLongWordArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralLongWordDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralLongWordDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralLongWordDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralLongWordDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralLongWordDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralLongWordDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralLongWordDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, ELongWordDictionary);
  Result := -1;
end;

procedure TGeneralLongWordDictionary.Add(const Key: String; const Value: LongWord);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralLongWordDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralLongWordDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralLongWordDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralLongWordDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralLongWordDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralLongWordDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralLongWordDictionary.LocateItem(const Key: String; var Value: LongWord): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := 0;
end;

function TGeneralLongWordDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: LongWord): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, ELongWordDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, ELongWordDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, ELongWordDictionary);
end;

procedure TGeneralLongWordDictionary.SetItem(const Key: String; const Value: LongWord);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, ELongWordDictionary);
end;

procedure TGeneralLongWordDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', ELongWordDictionary);
end;

function TGeneralLongWordDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralLongWordDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralLongWordDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralLongWordDictionary.GetItemByIndex(const Idx: Integer): LongWord;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralLongWordDictionary.SetItemByIndex(const Idx: Integer; const Value: LongWord);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralLongWordDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TLongWordDictionary                                                          }
{                                                                              }
function TLongWordDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, ELongWordDictionary);
  Result := -1;
end;

function TLongWordDictionary.GetItem(const Key: String): LongWord;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TLongWordArray(FValues).Data[I]
  else
    Result := 0;
end;

function TLongWordDictionary.LocateItem(const Key: String; var Value: LongWord): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TLongWordArray(FValues).Data[Result]
  else
    Value := 0;
end;



{                                                                              }
{ TGeneralInt64Dictionary                                                      }
{                                                                              }
constructor TGeneralInt64Dictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TInt64Array.Create;
end;

constructor TGeneralInt64Dictionary.CreateEx(const Keys: AStringArray;
    const Values: AInt64Array; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TInt64Array.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TInt64Dictionary.CreateEx(const Keys: TStringArray;
    const Values: TInt64Array; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralInt64Dictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralInt64Dictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralInt64Dictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralInt64Dictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralInt64Dictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralInt64Dictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralInt64Dictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EInt64Dictionary);
  Result := -1;
end;

procedure TGeneralInt64Dictionary.Add(const Key: String; const Value: Int64);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralInt64Dictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralInt64Dictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralInt64Dictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralInt64Dictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralInt64Dictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralInt64Dictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralInt64Dictionary.LocateItem(const Key: String; var Value: Int64): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := 0;
end;

function TGeneralInt64Dictionary.LocateNext(const Key: String; const Idx: Integer; var Value: Int64): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, EInt64Dictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, EInt64Dictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, EInt64Dictionary);
end;

procedure TGeneralInt64Dictionary.SetItem(const Key: String; const Value: Int64);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, EInt64Dictionary);
end;

procedure TGeneralInt64Dictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', EInt64Dictionary);
end;

function TGeneralInt64Dictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralInt64Dictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralInt64Dictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralInt64Dictionary.GetItemByIndex(const Idx: Integer): Int64;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralInt64Dictionary.SetItemByIndex(const Idx: Integer; const Value: Int64);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralInt64Dictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TInt64Dictionary                                                             }
{                                                                              }
function TInt64Dictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EInt64Dictionary);
  Result := -1;
end;

function TInt64Dictionary.GetItem(const Key: String): Int64;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TInt64Array(FValues).Data[I]
  else
    Result := 0;
end;

function TInt64Dictionary.LocateItem(const Key: String; var Value: Int64): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TInt64Array(FValues).Data[Result]
  else
    Value := 0;
end;



{                                                                              }
{ TGeneralSingleDictionary                                                     }
{                                                                              }
constructor TGeneralSingleDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TSingleArray.Create;
end;

constructor TGeneralSingleDictionary.CreateEx(const Keys: AStringArray;
    const Values: ASingleArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TSingleArray.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TSingleDictionary.CreateEx(const Keys: TStringArray;
    const Values: TSingleArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralSingleDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralSingleDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralSingleDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralSingleDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralSingleDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralSingleDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralSingleDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, ESingleDictionary);
  Result := -1;
end;

procedure TGeneralSingleDictionary.Add(const Key: String; const Value: Single);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralSingleDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralSingleDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralSingleDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralSingleDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralSingleDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralSingleDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralSingleDictionary.LocateItem(const Key: String; var Value: Single): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := 0.0;
end;

function TGeneralSingleDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: Single): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, ESingleDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, ESingleDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, ESingleDictionary);
end;

procedure TGeneralSingleDictionary.SetItem(const Key: String; const Value: Single);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, ESingleDictionary);
end;

procedure TGeneralSingleDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', ESingleDictionary);
end;

function TGeneralSingleDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralSingleDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralSingleDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralSingleDictionary.GetItemByIndex(const Idx: Integer): Single;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralSingleDictionary.SetItemByIndex(const Idx: Integer; const Value: Single);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralSingleDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TSingleDictionary                                                            }
{                                                                              }
function TSingleDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, ESingleDictionary);
  Result := -1;
end;

function TSingleDictionary.GetItem(const Key: String): Single;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TSingleArray(FValues).Data[I]
  else
    Result := 0.0;
end;

function TSingleDictionary.LocateItem(const Key: String; var Value: Single): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TSingleArray(FValues).Data[Result]
  else
    Value := 0.0;
end;



{                                                                              }
{ TGeneralDoubleDictionary                                                     }
{                                                                              }
constructor TGeneralDoubleDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TDoubleArray.Create;
end;

constructor TGeneralDoubleDictionary.CreateEx(const Keys: AStringArray;
    const Values: ADoubleArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TDoubleArray.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TDoubleDictionary.CreateEx(const Keys: TStringArray;
    const Values: TDoubleArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralDoubleDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralDoubleDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralDoubleDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralDoubleDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralDoubleDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralDoubleDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralDoubleDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EDoubleDictionary);
  Result := -1;
end;

procedure TGeneralDoubleDictionary.Add(const Key: String; const Value: Double);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralDoubleDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralDoubleDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralDoubleDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralDoubleDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralDoubleDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralDoubleDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralDoubleDictionary.LocateItem(const Key: String; var Value: Double): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := 0.0;
end;

function TGeneralDoubleDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: Double): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, EDoubleDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, EDoubleDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, EDoubleDictionary);
end;

procedure TGeneralDoubleDictionary.SetItem(const Key: String; const Value: Double);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, EDoubleDictionary);
end;

procedure TGeneralDoubleDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', EDoubleDictionary);
end;

function TGeneralDoubleDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralDoubleDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralDoubleDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralDoubleDictionary.GetItemByIndex(const Idx: Integer): Double;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralDoubleDictionary.SetItemByIndex(const Idx: Integer; const Value: Double);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralDoubleDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TDoubleDictionary                                                            }
{                                                                              }
function TDoubleDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EDoubleDictionary);
  Result := -1;
end;

function TDoubleDictionary.GetItem(const Key: String): Double;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TDoubleArray(FValues).Data[I]
  else
    Result := 0.0;
end;

function TDoubleDictionary.LocateItem(const Key: String; var Value: Double): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TDoubleArray(FValues).Data[Result]
  else
    Value := 0.0;
end;



{                                                                              }
{ TGeneralExtendedDictionary                                                   }
{                                                                              }
constructor TGeneralExtendedDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TExtendedArray.Create;
end;

constructor TGeneralExtendedDictionary.CreateEx(const Keys: AStringArray;
    const Values: AExtendedArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TExtendedArray.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TExtendedDictionary.CreateEx(const Keys: TStringArray;
    const Values: TExtendedArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralExtendedDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralExtendedDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralExtendedDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralExtendedDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralExtendedDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralExtendedDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralExtendedDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EExtendedDictionary);
  Result := -1;
end;

procedure TGeneralExtendedDictionary.Add(const Key: String; const Value: Extended);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralExtendedDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralExtendedDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralExtendedDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralExtendedDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralExtendedDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralExtendedDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralExtendedDictionary.LocateItem(const Key: String; var Value: Extended): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := 0.0;
end;

function TGeneralExtendedDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: Extended): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, EExtendedDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, EExtendedDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, EExtendedDictionary);
end;

procedure TGeneralExtendedDictionary.SetItem(const Key: String; const Value: Extended);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, EExtendedDictionary);
end;

procedure TGeneralExtendedDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', EExtendedDictionary);
end;

function TGeneralExtendedDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralExtendedDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralExtendedDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralExtendedDictionary.GetItemByIndex(const Idx: Integer): Extended;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralExtendedDictionary.SetItemByIndex(const Idx: Integer; const Value: Extended);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralExtendedDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TExtendedDictionary                                                          }
{                                                                              }
function TExtendedDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EExtendedDictionary);
  Result := -1;
end;

function TExtendedDictionary.GetItem(const Key: String): Extended;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TExtendedArray(FValues).Data[I]
  else
    Result := 0.0;
end;

function TExtendedDictionary.LocateItem(const Key: String; var Value: Extended): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TExtendedArray(FValues).Data[Result]
  else
    Value := 0.0;
end;



{                                                                              }
{ TGeneralPointerDictionary                                                    }
{                                                                              }
constructor TGeneralPointerDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TPointerArray.Create;
end;

constructor TGeneralPointerDictionary.CreateEx(const Keys: AStringArray;
    const Values: APointerArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TPointerArray.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TPointerDictionary.CreateEx(const Keys: TStringArray;
    const Values: TPointerArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralPointerDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralPointerDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralPointerDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralPointerDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralPointerDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralPointerDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralPointerDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EPointerDictionary);
  Result := -1;
end;

procedure TGeneralPointerDictionary.Add(const Key: String; const Value: Pointer);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralPointerDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralPointerDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralPointerDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralPointerDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralPointerDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralPointerDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralPointerDictionary.LocateItem(const Key: String; var Value: Pointer): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := nil;
end;

function TGeneralPointerDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: Pointer): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, EPointerDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, EPointerDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, EPointerDictionary);
end;

procedure TGeneralPointerDictionary.SetItem(const Key: String; const Value: Pointer);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, EPointerDictionary);
end;

procedure TGeneralPointerDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', EPointerDictionary);
end;

function TGeneralPointerDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralPointerDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralPointerDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralPointerDictionary.GetItemByIndex(const Idx: Integer): Pointer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralPointerDictionary.SetItemByIndex(const Idx: Integer; const Value: Pointer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralPointerDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TPointerDictionary                                                           }
{                                                                              }
function TPointerDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EPointerDictionary);
  Result := -1;
end;

function TPointerDictionary.GetItem(const Key: String): Pointer;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TPointerArray(FValues).Data[I]
  else
    Result := nil;
end;

function TPointerDictionary.LocateItem(const Key: String; var Value: Pointer): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TPointerArray(FValues).Data[Result]
  else
    Value := nil;
end;



{                                                                              }
{ TGeneralStringDictionary                                                     }
{                                                                              }
constructor TGeneralStringDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TStringArray.Create;
end;

constructor TGeneralStringDictionary.CreateEx(const Keys: AStringArray;
    const Values: AStringArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TStringArray.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TStringDictionary.CreateEx(const Keys: TStringArray;
    const Values: TStringArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralStringDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralStringDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralStringDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralStringDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralStringDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralStringDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralStringDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EStringDictionary);
  Result := -1;
end;

procedure TGeneralStringDictionary.Add(const Key: String; const Value: String);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralStringDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralStringDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralStringDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralStringDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralStringDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralStringDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralStringDictionary.LocateItem(const Key: String; var Value: String): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := '';
end;

function TGeneralStringDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: String): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, EStringDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, EStringDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, EStringDictionary);
end;

procedure TGeneralStringDictionary.SetItem(const Key: String; const Value: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, EStringDictionary);
end;

procedure TGeneralStringDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', EStringDictionary);
end;

function TGeneralStringDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralStringDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralStringDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralStringDictionary.GetItemByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralStringDictionary.SetItemByIndex(const Idx: Integer; const Value: String);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralStringDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TStringDictionary                                                            }
{                                                                              }
function TStringDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EStringDictionary);
  Result := -1;
end;

function TStringDictionary.GetItem(const Key: String): String;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TStringArray(FValues).Data[I]
  else
    Result := '';
end;

function TStringDictionary.LocateItem(const Key: String; var Value: String): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TStringArray(FValues).Data[Result]
  else
    Value := '';
end;



{                                                                              }
{ TGeneralObjectDictionary                                                     }
{                                                                              }
constructor TGeneralObjectDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TObjectArray.Create;
end;

constructor TGeneralObjectDictionary.CreateEx(const Keys: AStringArray;
    const Values: AObjectArray; const IsItemOwner: Boolean;
    const KeysCaseSensitive: Boolean; const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values else
    FValues := TObjectArray.Create;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FValues.IsItemOwner := IsItemOwner;
  FCaseSensitive := KeysCaseSensitive;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TObjectDictionary.CreateEx(const Keys: TStringArray;
    const Values: TObjectArray; const IsItemOwner: Boolean;
    const KeysCaseSensitive: Boolean; const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, IsItemOwner, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralObjectDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralObjectDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralObjectDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralObjectDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralObjectDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

function TGeneralObjectDictionary.GetIsItemOwner: Boolean;
begin
  Result := FValues.IsItemOwner;
end;

procedure TGeneralObjectDictionary.SetIsItemOwner(const IsItemOwner: Boolean);
begin
  FValues.IsItemOwner := IsItemOwner;
end;

procedure TGeneralObjectDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralObjectDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EObjectDictionary);
  Result := -1;
end;

procedure TGeneralObjectDictionary.Add(const Key: String; const Value: TObject);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralObjectDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralObjectDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralObjectDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralObjectDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralObjectDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralObjectDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralObjectDictionary.LocateItem(const Key: String; var Value: TObject): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := nil;
end;

function TGeneralObjectDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: TObject): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, EObjectDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, EObjectDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, EObjectDictionary);
end;

procedure TGeneralObjectDictionary.SetItem(const Key: String; const Value: TObject);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, EObjectDictionary);
end;

procedure TGeneralObjectDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', EObjectDictionary);
end;

function TGeneralObjectDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralObjectDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralObjectDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralObjectDictionary.GetItemByIndex(const Idx: Integer): TObject;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralObjectDictionary.SetItemByIndex(const Idx: Integer; const Value: TObject);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

function TGeneralObjectDictionary.ReleaseItem(const Key: String): TObject;
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  Result := FValues.ReleaseItem(I);
end;

procedure TGeneralObjectDictionary.ReleaseItems;
begin
  FKeys.Clear;
  FValues.ReleaseItems;
  FHashSize := 0;
  FLookup := nil;
end;

procedure TGeneralObjectDictionary.FreeItems;
begin
  FKeys.Clear;
  FValues.FreeItems;
  FHashSize := 0;
  FLookup := nil;
end;

procedure TGeneralObjectDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TObjectDictionary                                                            }
{                                                                              }
function TObjectDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EObjectDictionary);
  Result := -1;
end;

function TObjectDictionary.GetItem(const Key: String): TObject;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TObjectArray(FValues).Data[I]
  else
    Result := nil;
end;

function TObjectDictionary.LocateItem(const Key: String; var Value: TObject): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TObjectArray(FValues).Data[Result]
  else
    Value := nil;
end;



{                                                                              }
{ TGeneralInterfaceDictionary                                                  }
{                                                                              }
constructor TGeneralInterfaceDictionary.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := TStringArray.Create;
  FValues := TInterfaceArray.Create;
end;

constructor TGeneralInterfaceDictionary.CreateEx(const Keys: AStringArray;
    const Values: AInterfaceArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(Keys) then
    begin
      FKeys := Keys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := TStringArray.Create;
      L := 0;
    end;
  if Assigned(Values) then
    FValues := Values
  else
    FValues := TInterfaceArray.Create;
  FCaseSensitive := KeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AddOnSet;
  FDuplicatesAction := DuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TInterfaceDictionary.CreateEx(const Keys: TStringArray;
    const Values: TInterfaceArray; const KeysCaseSensitive: Boolean;
    const AddOnSet: Boolean;
    const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(Keys, Values, KeysCaseSensitive, AddOnSet,
      DuplicatesAction);
end;

destructor TGeneralInterfaceDictionary.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneralInterfaceDictionary.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneralInterfaceDictionary.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneralInterfaceDictionary.SetAddOnSet(const AddOnSet: Boolean);
begin
  FAddOnSet := AddOnSet;
end;

function TGeneralInterfaceDictionary.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;

procedure TGeneralInterfaceDictionary.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FKeys[I], 0, FCaseSensitive) and L], I);
end;

function TGeneralInterfaceDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, J, L, K : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      K := Length(Key);
      if FCaseSensitive then
        H := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        H := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      For I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrPEqualStr(Pointer(Key), K, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EInterfaceDictionary);
  Result := -1;
end;

procedure TGeneralInterfaceDictionary.Add(const Key: String; const Value: IInterface);
var H, L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr(Key, 0, FCaseSensitive) and (L - 1));
  I := FKeys.AppendItem(Key);
  Append(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div AverageHashChainSize > L then
    Rehash;
end;

procedure TGeneralInterfaceDictionary.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr(FKeys[Idx], 0, FCaseSensitive) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := PosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);

  For I := 0 to FHashSize - 1 do
    For J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneralInterfaceDictionary.Delete(const Key: String);
var I, H : Integer;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneralInterfaceDictionary.HasKey(const Key: String): Boolean;
var H : Integer;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneralInterfaceDictionary.Rename(const Key, NewKey: String);
var I, J, H : Integer;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := PosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  Remove(FLookup[H], J, 1);
  Append(FLookup[HashStr(NewKey, 0, FCaseSensitive) and (FHashSize - 1)], I);
end;

function TGeneralInterfaceDictionary.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneralInterfaceDictionary.SetDuplicatesAction(const DuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := DuplicatesAction;
end;

function TGeneralInterfaceDictionary.LocateItem(const Key: String; var Value: IInterface): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := nil;
end;

function TGeneralInterfaceDictionary.LocateNext(const Key: String; const Idx: Integer; var Value: IInterface): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key, EInterfaceDictionary);
  H := HashStr(Key, 0, FCaseSensitive) and (L - 1);
  For I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key, EInterfaceDictionary);
          For K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key, EInterfaceDictionary);
end;

procedure TGeneralInterfaceDictionary.SetItem(const Key: String; const Value: IInterface);
var I, H : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key, EInterfaceDictionary);
end;

procedure TGeneralInterfaceDictionary.RaiseIndexError;
begin
  RaiseDictionaryError('Index out of range', EInterfaceDictionary);
end;

function TGeneralInterfaceDictionary.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneralInterfaceDictionary.GetKeyByIndex(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneralInterfaceDictionary.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneralInterfaceDictionary.GetItemByIndex(const Idx: Integer): IInterface;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneralInterfaceDictionary.SetItemByIndex(const Idx: Integer; const Value: IInterface);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;

procedure TGeneralInterfaceDictionary.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{ TInterfaceDictionary                                                         }
{                                                                              }
function TInterfaceDictionary.LocateKey(const Key: String; var LookupIdx: Integer;
    const ErrorIfNotFound: Boolean): Integer;
var H, I, L, K : Integer;
    P : PInteger;
begin
  L := FHashSize;
  if L > 0 then
    begin
      P := Pointer(Key);
      if Assigned(P) then
        begin
          Dec(P);
          K := P^;
        end
      else
        K := 0;
      if FCaseSensitive then
        LongWord(H) := HashStrBuf(Pointer(Key), K, 0) and (L - 1)
      else
        LongWord(H) := HashStrBufNoCase(Pointer(Key), K, 0) and (L - 1);
      LookupIdx := H;
      P := Pointer(FLookup);
      Inc(P, H);
      P := Pointer(P^);
      if Assigned(P) then
        begin
          Dec(P);
          For I := 0 to P^ - 1 do
            begin
              Inc(P);
              Result := P^;
              if StrPEqualStr(Pointer(Key), K, TStringArray(FKeys).Data[Result],
                  FCaseSensitive) then
                exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key, EInterfaceDictionary);
  Result := -1;
end;

function TInterfaceDictionary.GetItem(const Key: String): IInterface;
var H, I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := TInterfaceArray(FValues).Data[I]
  else
    Result := nil;
end;

function TInterfaceDictionary.LocateItem(const Key: String; var Value: IInterface): Integer;
var H : Integer;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := TInterfaceArray(FValues).Data[Result]
  else
    Value := nil;
end;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
var F : TIntegerDictionary;
    G : TStringDictionary;
    I : Integer;
begin
  {$IFNDEF FREEPASCAL}
  F := TIntegerDictionary.Create;
  For I := 0 to 16384 do
    F.Add(IntToStr(I), I);
  Assert(F.Count = 16385, 'Dictionary.Count');
  For I := 0 to 16384 do
    Assert(F.GetKeyByIndex(I) = IntToStr(I), 'Dictionary.GetKeyByIndex');
  Assert(F['0'] = 0, 'Dictionary.GetItem');
  Assert(F['5'] = 5, 'Dictionary.GetItem');
  Assert(F['16384'] = 16384, 'Dictionary.GetItem');
  For I := 0 to 16384 do
    Assert(F.GetItemByIndex(I) = I, 'Dictionary.GetItemByIndex');
  Assert(F.HasKey('5'), 'Dictionary.HasKey');
  Assert(not F.HasKey('X'), 'Dictionary.HasKey');
  F.Rename('5', 'X');
  Assert(not F.HasKey('5'), 'Dictionary.Rename');
  Assert(F.HasKey('X'), 'Dictionary.Rename');
  Assert(F['X'] = 5, 'Dictionary.Rename');
  F.Delete('X');
  Assert(not F.HasKey('X'), 'Dictionary.Delete');
  Assert(F.Count = 16384, 'Dictionary.Delete');
  F.Delete('0');
  Assert(not F.HasKey('0'), 'Dictionary.Delete');
  Assert(F.Count = 16383, 'Dictionary.Delete');
  F.DeleteItemByIndex(0);
  Assert(not F.HasKey('1'), 'Dictionary.DeleteItemByIndex');
  Assert(F.Count = 16382, 'Dictionary.DeleteItemByIndex');
  F.Free;

  G := TStringDictionary.Create;
  For I := 0 to 16384 do
    G.Add(IntToStr(I), IntToStr(I));
  Assert(G.Count = 16385, 'Dictionary.Count');
  For I := 0 to 16384 do
    Assert(G.GetKeyByIndex(I) = IntToStr(I), 'Dictionary.GetKeyByIndex');
  Assert(G['0'] = '0', 'Dictionary.GetItem');
  Assert(G['5'] = '5', 'Dictionary.GetItem');
  Assert(G['16384'] = '16384', 'Dictionary.GetItem');
  For I := 0 to 16384 do
    Assert(G.GetItemByIndex(I) = IntToStr(I), 'Dictionary.GetItemByIndex');
  Assert(G.HasKey('5'), 'Dictionary.HasKey');
  Assert(not G.HasKey('X'), 'Dictionary.HasKey');
  G.Rename('5', 'X');
  Assert(not G.HasKey('5'), 'Dictionary.Rename');
  Assert(G.HasKey('X'), 'Dictionary.Rename');
  Assert(G['X'] = '5', 'Dictionary.Rename');
  G.Delete('X');
  Assert(not G.HasKey('X'), 'Dictionary.Delete');
  Assert(G.Count = 16384, 'Dictionary.Delete');
  G.Delete('0');
  Assert(not G.HasKey('0'), 'Dictionary.Delete');
  Assert(G.Count = 16383, 'Dictionary.Delete');
  G.DeleteItemByIndex(0);
  Assert(not G.HasKey('1'), 'Dictionary.DeleteItemByIndex');
  Assert(G.Count = 16382, 'Dictionary.DeleteItemByIndex');
  G.Free;
  {$ENDIF}
end;



end.

