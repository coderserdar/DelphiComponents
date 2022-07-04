{$INCLUDE ..\cDefines.inc}
unit cArrays;

{                                                                              }
{                        Data structures: Arrays v3.21                         }
{                                                                              }
{             This unit is copyright © 1999-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is cArrays.pas                     }
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
{ Revision history:                                                            }
{   [ cDataStructs ]                                                           }
{   1999/11/12  0.01  Split cTypes from cDataStruct and cHolder.               }
{   1999/11/14  0.02  Added AListType.                                         }
{   2000/02/08  1.03  Initial version. AArray, TArray and TStreamArray.        }
{   2000/06/07  1.04  Base classes (AIntegerArray, ASet).                      }
{   2000/06/08  1.05  Added AObjectArray.                                      }
{   2000/06/03  1.06  Added AArray, AIntegerArray, AExtendedArray,             }
{                     AStringArray and ABitArray (formerly ASet) with some     }
{                     implementations.                                         }
{   2000/06/06  1.07  TFlatBitArray implementation.                            }
{                     Added AInt64Array.                                       }
{   2000/06/08  1.08  Added TObjectArray.                                      }
{   2000/06/14  1.09  Converted cDataStructs to template.                      }
{   2001/07/15  1.10  Changed memory arrays to pre-allocate when growing.      }
{   2001/08/20  2.11  Merged cTypes and cDataStructs to allow object           }
{                     interface implementation in base classes.                }
{   [ cArrays ]                                                                }
{   2002/05/15  3.12  Created cArrays unit from cDataStructs.                  }
{                     Refactored for Fundamentals 3.                           }
{   2002/09/30  3.13  Moved stream array classes to unit cStreamArrays.        }
{   2002/12/17  3.14  Added THashedStringArray.                                }
{   2003/03/08  3.15  Renamed Add methods to Append.                           }
{   2003/05/26  3.16  Added Remove methods to object array.                    }
{   2003/09/11  3.17  Added TInterfaceArray.                                   }
{   2004/01/02  3.18  Bug fixed in TStringArray.SetAsString by Eb.             }
{   2004/01/18  3.19  Added TWideStringArray.                                  }
{   2004/07/24  3.20  Fixed bug in Sort with duplicate values. Thanks to Eb    }
{                     and others for reporting it.                             }
{   2004/08/01  3.21  Added AArray.RemoveDuplicates.                           }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cUtils,
  cTypes;

const
  UnitName      = 'cArrays';
  UnitVersion   = '3.21';
  UnitDesc      = 'Data structures: Arrays';
  UnitCopyright = 'Copyright (c) 1999-2004 David J Butler';



{                                                                              }
{ ARRAY BASE CLASSES                                                           }
{   Classes with the A-prefix are abstract base classes. They define the       }
{   interface for the type and must never be instanciated.                     }
{   Instead, create one of the implementation classes (T-prefix).              }
{                                                                              }



{                                                                              }
{ AArray                                                                       }
{   Base class for an array.                                                   }
{                                                                              }
type
  AArray = class(AType)
  protected
    procedure RaiseIndexError(const Idx: Integer); virtual;

    function  GetCount: Integer; virtual; abstract;
    procedure SetCount(const NewCount: Integer); virtual; abstract;

  public
    { AType                                                                    }
    procedure Clear; override;

    { AArray                                                                   }
    property  Count: Integer read GetCount write SetCount;

    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; virtual; abstract;
    procedure ExchangeItems(const Idx1, Idx2: Integer); virtual; abstract;
    procedure Sort; virtual;
    procedure ReverseOrder; virtual;
    procedure RemoveDuplicates(const IsSortedAscending: Boolean); virtual;

    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; virtual; abstract;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); virtual; abstract;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); virtual; abstract;
    function  AppendArray(const V: AArray): Integer; overload; virtual; abstract;
  end;
  EArray = class(EType);
  ArrayClass = class of AArray;



{                                                                              }
{ ALongIntArray                                                                }
{   Base class for an array of LongInt's.                                      }
{                                                                              }
type
  ALongIntArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): LongInt; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: LongInt); virtual; abstract;

    function  GetRange(const LoIdx, HiIdx: Integer): LongIntArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: LongIntArray); virtual;

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { ALongIntArray interface                                                      }
    property  Item[const Idx: Integer]: LongInt read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: LongIntArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: LongInt); virtual;
    function  AppendItem(const Value: LongInt): Integer; virtual;
    function  AppendArray(const V: LongIntArray): Integer; overload; virtual;
    function  PosNext(const Find: LongInt; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;
  ELongIntArray = class(EArray);



{                                                                              }
{ AIntegerArray                                                                }
{                                                                              }
type
  AIntegerArray = ALongIntArray;
  EIntegerArray = ELongIntArray;



{                                                                              }
{ ALongWordArray                                                               }
{   Base class for an array of LongWord's.                                     }
{                                                                              }
type
  ALongWordArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): LongWord; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: LongWord); virtual; abstract;

    function  GetRange(const LoIdx, HiIdx: Integer): LongWordArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: LongWordArray); virtual;

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { ALongWordArray interface                                                     }
    property  Item[const Idx: Integer]: LongWord read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: LongWordArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: LongWord); virtual;
    function  AppendItem(const Value: LongWord): Integer; virtual;
    function  AppendArray(const V: LongWordArray): Integer; overload; virtual;
    function  PosNext(const Find: LongWord; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;
  ELongWordArray = class(EArray);



{                                                                              }
{ ACardinalArray                                                               }
{                                                                              }
type
  ACardinalArray = ALongWordArray;
  ECardinalArray = ELongWordArray;



{                                                                              }
{ AInt64Array                                                                  }
{   Base class for an array of Int64's.                                        }
{                                                                              }
type
  AInt64Array = class(AArray)
  protected
    function  GetItem(const Idx: Integer): Int64; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: Int64); virtual; abstract;

    function  GetRange(const LoIdx, HiIdx: Integer): Int64Array; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: Int64Array); virtual;

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AInt64Array interface                                                        }
    property  Item[const Idx: Integer]: Int64 read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: Int64Array read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: Int64); virtual;
    function  AppendItem(const Value: Int64): Integer; virtual;
    function  AppendArray(const V: Int64Array): Integer; overload; virtual;
    function  PosNext(const Find: Int64; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;
  EInt64Array = class(EArray);



{                                                                              }
{ ASingleArray                                                                 }
{   Base class for an array of Single's.                                       }
{                                                                              }
type
  ASingleArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): Single; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: Single); virtual; abstract;

    function  GetRange(const LoIdx, HiIdx: Integer): SingleArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: SingleArray); virtual;

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { ASingleArray interface                                                       }
    property  Item[const Idx: Integer]: Single read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: SingleArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: Single); virtual;
    function  AppendItem(const Value: Single): Integer; virtual;
    function  AppendArray(const V: SingleArray): Integer; overload; virtual;
    function  PosNext(const Find: Single; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;
  ESingleArray = class(EArray);



{                                                                              }
{ ADoubleArray                                                                 }
{   Base class for an array of Double's.                                       }
{                                                                              }
type
  ADoubleArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): Double; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: Double); virtual; abstract;

    function  GetRange(const LoIdx, HiIdx: Integer): DoubleArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: DoubleArray); virtual;

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { ADoubleArray interface                                                       }
    property  Item[const Idx: Integer]: Double read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: DoubleArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: Double); virtual;
    function  AppendItem(const Value: Double): Integer; virtual;
    function  AppendArray(const V: DoubleArray): Integer; overload; virtual;
    function  PosNext(const Find: Double; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;
  EDoubleArray = class(EArray);



{                                                                              }
{ AExtendedArray                                                               }
{   Base class for an array of Extended's.                                     }
{                                                                              }
type
  AExtendedArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): Extended; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: Extended); virtual; abstract;

    function  GetRange(const LoIdx, HiIdx: Integer): ExtendedArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: ExtendedArray); virtual;

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AExtendedArray interface                                                     }
    property  Item[const Idx: Integer]: Extended read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: ExtendedArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: Extended); virtual;
    function  AppendItem(const Value: Extended): Integer; virtual;
    function  AppendArray(const V: ExtendedArray): Integer; overload; virtual;
    function  PosNext(const Find: Extended; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;
  EExtendedArray = class(EArray);



{                                                                              }
{ APointerArray                                                                }
{   Base class for an array of Pointer's.                                      }
{                                                                              }
type
  APointerArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): Pointer; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: Pointer); virtual; abstract;

    function  GetRange(const LoIdx, HiIdx: Integer): PointerArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: PointerArray); virtual;

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { APointerArray interface                                                      }
    property  Item[const Idx: Integer]: Pointer read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: PointerArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: Pointer); virtual;
    function  AppendItem(const Value: Pointer): Integer; virtual;
    function  AppendArray(const V: PointerArray): Integer; overload; virtual;
    function  PosNext(const Find: Pointer; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;
  EPointerArray = class(EArray);



{                                                                              }
{ AStringArray                                                                 }
{   Base class for an array of Strings.                                        }
{                                                                              }
type
  EStringArray = class(EArray);
  AStringArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): String; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: String); virtual; abstract;
    function  GetRange(const LoIdx, HiIdx: Integer): StringArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: StringArray); virtual;
    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AStringArray interface                                                   }
    property  Item[const Idx: Integer]: String read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: StringArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: String = ''); virtual;
    function  AppendItem(const Value: String): Integer; virtual;
    function  AppendArray(const V: StringArray): Integer; overload; virtual;
    function  PosNext(const Find: String; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;



{                                                                              }
{ AWideStringArray                                                             }
{   Base class for an array of WideStrings.                                    }
{                                                                              }
type
  EWideStringArray = class(EArray);
  AWideStringArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): WideString; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: WideString); virtual; abstract;
    function  GetRange(const LoIdx, HiIdx: Integer): WideStringArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: WideStringArray); virtual;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AWideStringArray interface                                               }
    property  Item[const Idx: Integer]: WideString read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: WideStringArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: WideString = ''); virtual;
    function  AppendItem(const Value: WideString): Integer; virtual;
    function  AppendArray(const V: WideStringArray): Integer; overload; virtual;
    function  PosNext(const Find: WideString; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;



{                                                                              }
{ AObjectArray                                                                 }
{   Base class for an array of objects.                                        }
{                                                                              }
type
  EObjectArray = class(EArray);
  AObjectArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): TObject; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: TObject); virtual; abstract;
    function  GetRange(const LoIdx, HiIdx: Integer): ObjectArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: ObjectArray); virtual;
    function  GetAsString: String; override;
    function  GetIsItemOwner: Boolean; virtual; abstract;
    procedure SetIsItemOwner(const IsItemOwner: Boolean); virtual; abstract;

  public
    { AType                                                                    }
    procedure Clear; override;
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;
    function  Compare(const V: TObject): TCompareResult; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;

    { AObjectArray interface                                                   }
    property  Item[const Idx: Integer]: TObject read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: ObjectArray read GetRange write SetRange;
    function  AppendItem(const Value: TObject): Integer; virtual;
    function  AppendArray(const V: ObjectArray): Integer; overload; virtual;

    function  PosNext(const Find: TObject; const PrevPos: Integer): Integer; overload;
    function  PosNext(var Item: TObject; const ClassType: TClass; const PrevPos: Integer = -1): Integer; overload;
    function  PosNext(var Item: TObject; const ClassName: String; const PrevPos: Integer = -1): Integer; overload;
    function  Find(const ClassType: TClass; const Count: Integer = 1): TObject; overload;
    function  Find(const ClassName: String; const Count: Integer = 1): TObject; overload;
    function  FindAll(const ClassType: TClass): ObjectArray; overload;
    function  FindAll(const ClassName: String): ObjectArray; overload;
    function  CountItems(const ClassType: TClass): Integer; overload;
    function  CountItems(const ClassName: String): Integer; overload;
    function  DeleteValue(const Value: TObject): Boolean;
    function  DeleteAll(const Value: TObject): Integer;

    property  IsItemOwner: Boolean read GetIsItemOwner write SetIsItemOwner;
    procedure ReleaseItems; virtual; abstract;
    procedure FreeItems; virtual; abstract;
    function  ReleaseItem(const Idx: Integer): TObject; virtual; abstract;
    function  ReleaseValue(const Value: TObject): Boolean;
    function  RemoveItem(const Idx: Integer): TObject;
    function  RemoveValue(const Value: TObject): Boolean;
  end;



{                                                                              }
{ AInterfaceArray                                                              }
{   Base class for an array of Interface's.                                    }
{                                                                              }
type
  AInterfaceArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): IInterface; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: IInterface); virtual; abstract;

    function  GetRange(const LoIdx, HiIdx: Integer): InterfaceArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: InterfaceArray); virtual;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AInterfaceArray interface                                                    }
    property  Item[const Idx: Integer]: IInterface read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: InterfaceArray read GetRange write SetRange;
    procedure Fill(const Idx, Count: Integer; const Value: IInterface); virtual;
    function  AppendItem(const Value: IInterface): Integer; virtual;
    function  AppendArray(const V: InterfaceArray): Integer; overload; virtual;
    function  PosNext(const Find: IInterface; const PrevPos: Integer = -1;
              const IsSortedAscending: Boolean = False): Integer;
  end;
  EInterfaceArray = class(EArray);



{                                                                              }
{ ABitArray                                                                    }
{   Base class for bit array implementations.                                  }
{   Bits are defined as False at initialization.                               }
{   FindRange finds Count consecutive bits that are equal to Value. It         }
{   returns the index of the leftmost bit or -1 if not found.                  }
{                                                                              }
type
  EBitArray = class(EArray);
  ABitArray = class(AArray)
  protected
    function  GetBit(const Idx: Integer): Boolean; virtual; abstract;
    procedure SetBit(const Idx: Integer; const Value: Boolean); virtual; abstract;
    function  GetRangeL(const Idx: Integer): LongWord; virtual;
    procedure SetRangeL(const Idx: Integer; const Value: LongWord); virtual;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;
    function  AppendArray(const V: AArray): Integer; override;
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;

    { ABitArray interface                                                      }
    property  Bit[const Idx: Integer]: Boolean read GetBit write SetBit; default;
    property  RangeL[const Idx: Integer]: LongWord read GetRangeL write SetRangeL;
    function  IsRange(const LoIdx, HiIdx: Integer; const Value: Boolean): Boolean; virtual;
    procedure Fill(const Idx, Count: Integer; const Value: Boolean); virtual;
    function  AppendItem(const Value: Boolean): Integer; virtual;
    procedure Invert; virtual;

    function  Find(const Value: Boolean = False;
              const Start: Integer = 0): Integer; virtual;
    function  FindRange(const Value: Boolean = False;
              const Start: Integer = 0;
              const Count: Integer = 1): Integer; virtual;
  end;



{                                                                              }
{ ARRAY IMPLEMENTATIONS                                                        }
{                                                                              }



{                                                                              }
{ TLongIntArray                                                                }
{   ALongIntArray implemented using a dynamic array.                           }
{                                                                              }
type
  TLongIntArray = class(ALongIntArray)
  protected
    FData     : LongIntArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { ALongIntArray                                                            }
    function  GetItem(const Idx: Integer): LongInt; override;
    procedure SetItem(const Idx: Integer; const Value: LongInt); override;
    function  GetRange(const LoIdx, HiIdx: Integer): LongIntArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: LongIntArray); override;
    procedure SetData(const Data: LongIntArray); virtual;

  public
    constructor Create(const V: LongIntArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { ALongIntArray                                                            }
    procedure Assign(const V: LongIntArray); overload;
    procedure Assign(const V: Array of LongInt); overload;
    function  AppendItem(const Value: LongInt): Integer; override;

    { TLongIntArray                                                            }
    property  Data: LongIntArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TIntegerArray                                                                }
{                                                                              }
type
  TIntegerArray = TLongIntArray;



{                                                                              }
{ TLongWordArray                                                               }
{   ALongWordArray implemented using a dynamic array.                          }
{                                                                              }
type
  TLongWordArray = class(ALongWordArray)
  protected
    FData     : LongWordArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { ALongWordArray                                                            }
    function  GetItem(const Idx: Integer): LongWord; override;
    procedure SetItem(const Idx: Integer; const Value: LongWord); override;
    function  GetRange(const LoIdx, HiIdx: Integer): LongWordArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: LongWordArray); override;
    procedure SetData(const Data: LongWordArray); virtual;

  public
    constructor Create(const V: LongWordArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { ALongWordArray                                                            }
    procedure Assign(const V: LongWordArray); overload;
    procedure Assign(const V: Array of LongWord); overload;
    function  AppendItem(const Value: LongWord): Integer; override;

    { TLongWordArray                                                            }
    property  Data: LongWordArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TCardinalArray                                                               }
{                                                                              }
type
  TCardinalArray = TLongWordArray;



{                                                                              }
{ TInt64Array                                                                  }
{   AInt64Array implemented using a dynamic array.                             }
{                                                                              }
type
  TInt64Array = class(AInt64Array)
  protected
    FData     : Int64Array;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { AInt64Array                                                            }
    function  GetItem(const Idx: Integer): Int64; override;
    procedure SetItem(const Idx: Integer; const Value: Int64); override;
    function  GetRange(const LoIdx, HiIdx: Integer): Int64Array; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: Int64Array); override;
    procedure SetData(const Data: Int64Array); virtual;

  public
    constructor Create(const V: Int64Array = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AInt64Array                                                            }
    procedure Assign(const V: Int64Array); overload;
    procedure Assign(const V: Array of Int64); overload;
    function  AppendItem(const Value: Int64): Integer; override;

    { TInt64Array                                                            }
    property  Data: Int64Array read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TSingleArray                                                                 }
{   ASingleArray implemented using a dynamic array.                            }
{                                                                              }
type
  TSingleArray = class(ASingleArray)
  protected
    FData     : SingleArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { ASingleArray                                                            }
    function  GetItem(const Idx: Integer): Single; override;
    procedure SetItem(const Idx: Integer; const Value: Single); override;
    function  GetRange(const LoIdx, HiIdx: Integer): SingleArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: SingleArray); override;
    procedure SetData(const Data: SingleArray); virtual;

  public
    constructor Create(const V: SingleArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { ASingleArray                                                            }
    procedure Assign(const V: SingleArray); overload;
    procedure Assign(const V: Array of Single); overload;
    function  AppendItem(const Value: Single): Integer; override;

    { TSingleArray                                                            }
    property  Data: SingleArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TDoubleArray                                                                 }
{   ADoubleArray implemented using a dynamic array.                            }
{                                                                              }
type
  TDoubleArray = class(ADoubleArray)
  protected
    FData     : DoubleArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { ADoubleArray                                                            }
    function  GetItem(const Idx: Integer): Double; override;
    procedure SetItem(const Idx: Integer; const Value: Double); override;
    function  GetRange(const LoIdx, HiIdx: Integer): DoubleArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: DoubleArray); override;
    procedure SetData(const Data: DoubleArray); virtual;

  public
    constructor Create(const V: DoubleArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { ADoubleArray                                                            }
    procedure Assign(const V: DoubleArray); overload;
    procedure Assign(const V: Array of Double); overload;
    function  AppendItem(const Value: Double): Integer; override;

    { TDoubleArray                                                            }
    property  Data: DoubleArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TExtendedArray                                                               }
{   AExtendedArray implemented using a dynamic array.                          }
{                                                                              }
type
  TExtendedArray = class(AExtendedArray)
  protected
    FData     : ExtendedArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { AExtendedArray                                                            }
    function  GetItem(const Idx: Integer): Extended; override;
    procedure SetItem(const Idx: Integer; const Value: Extended); override;
    function  GetRange(const LoIdx, HiIdx: Integer): ExtendedArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: ExtendedArray); override;
    procedure SetData(const Data: ExtendedArray); virtual;

  public
    constructor Create(const V: ExtendedArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AExtendedArray                                                            }
    procedure Assign(const V: ExtendedArray); overload;
    procedure Assign(const V: Array of Extended); overload;
    function  AppendItem(const Value: Extended): Integer; override;

    { TExtendedArray                                                            }
    property  Data: ExtendedArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TPointerArray                                                                }
{   APointerArray implemented using a dynamic array.                           }
{                                                                              }
type
  TPointerArray = class(APointerArray)
  protected
    FData     : PointerArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { APointerArray                                                            }
    function  GetItem(const Idx: Integer): Pointer; override;
    procedure SetItem(const Idx: Integer; const Value: Pointer); override;
    function  GetRange(const LoIdx, HiIdx: Integer): PointerArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: PointerArray); override;
    procedure SetData(const Data: PointerArray); virtual;

  public
    constructor Create(const V: PointerArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { APointerArray                                                            }
    procedure Assign(const V: PointerArray); overload;
    procedure Assign(const V: Array of Pointer); overload;
    function  AppendItem(const Value: Pointer): Integer; override;

    { TPointerArray                                                            }
    property  Data: PointerArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TStringArray                                                                 }
{   AStringArray implemented using a dynamic array.                            }
{                                                                              }
type
  TStringArray = class(AStringArray)
  protected
    FData     : StringArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { AStringArray                                                            }
    function  GetItem(const Idx: Integer): String; override;
    procedure SetItem(const Idx: Integer; const Value: String); override;
    function  GetRange(const LoIdx, HiIdx: Integer): StringArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: StringArray); override;
    procedure SetData(const Data: StringArray); virtual;

  public
    constructor Create(const V: StringArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AStringArray                                                            }
    procedure Assign(const V: StringArray); overload;
    procedure Assign(const V: Array of String); overload;
    function  AppendItem(const Value: String): Integer; override;

    { TStringArray                                                            }
    property  Data: StringArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TWideStringArray                                                             }
{   AWideStringArray implemented using a dynamic array.                        }
{                                                                              }
type
  TWideStringArray = class(AWideStringArray)
  protected
    FData     : WideStringArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { AWideStringArray                                                            }
    function  GetItem(const Idx: Integer): WideString; override;
    procedure SetItem(const Idx: Integer; const Value: WideString); override;
    function  GetRange(const LoIdx, HiIdx: Integer): WideStringArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: WideStringArray); override;
    procedure SetData(const Data: WideStringArray); virtual;

  public
    constructor Create(const V: WideStringArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AWideStringArray                                                            }
    procedure Assign(const V: WideStringArray); overload;
    procedure Assign(const V: Array of WideString); overload;
    function  AppendItem(const Value: WideString): Integer; override;

    { TWideStringArray                                                            }
    property  Data: WideStringArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TObjectArray                                                                 }
{   AObjectArray implemented using a dynamic array.                            }
{                                                                              }
type
  TObjectArray = class(AObjectArray)
  protected
    FData        : ObjectArray;
    FCapacity    : Integer;
    FCount       : Integer;
    FIsItemOwner : Boolean;

    procedure Init; override;
    procedure SetData(const Data: ObjectArray); virtual;

    { AArray                                                                   }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { AObjectArray                                                             }
    function  GetItem(const Idx: Integer): TObject; override;
    procedure SetItem(const Idx: Integer; const Value: TObject); override;
    function  GetRange(const LoIdx, HiIdx: Integer): ObjectArray; override;
    function  GetIsItemOwner: Boolean; override;
    procedure SetIsItemOwner(const IsItemOwner: Boolean); override;

  public
    { TObjectArray interface                                                   }
    constructor Create(const V: ObjectArray = nil;
                const IsItemOwner: Boolean = False); reintroduce; overload;
    destructor Destroy; override;

    property  Data: ObjectArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
    property  IsItemOwner: Boolean read FIsItemOwner write FIsItemOwner;
    procedure FreeItems; override;
    procedure ReleaseItems; override;
    function  ReleaseItem(const Idx: Integer): TObject; override;

    { AArray                                                                   }
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AObjectArray                                                             }
    function  AppendItem(const Value: TObject): Integer; override;
  end;



{                                                                              }
{ TInterfaceArray                                                              }
{   AInterfaceArray implemented using a dynamic array.                         }
{                                                                              }
type
  TInterfaceArray = class(AInterfaceArray)
  protected
    FData     : InterfaceArray;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { AInterfaceArray                                                            }
    function  GetItem(const Idx: Integer): IInterface; override;
    procedure SetItem(const Idx: Integer; const Value: IInterface); override;
    function  GetRange(const LoIdx, HiIdx: Integer): InterfaceArray; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: InterfaceArray); override;
    procedure SetData(const Data: InterfaceArray); virtual;

  public
    constructor Create(const V: InterfaceArray = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;

    { AInterfaceArray                                                            }
    procedure Assign(const V: InterfaceArray); overload;
    procedure Assign(const V: Array of IInterface); overload;
    function  AppendItem(const Value: IInterface): Integer; override;

    { TInterfaceArray                                                            }
    property  Data: InterfaceArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;



{                                                                              }
{ TBitArray                                                                    }
{   ABitArray implemented using a dynamic array.                               }
{                                                                              }
type
  TBitArray = class(ABitArray)
  protected
    FData  : LongWordArray;
    FCount : Integer;

    { AArray                                                                   }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { ABitArray                                                                }
    function  GetBit(const Idx: Integer): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
    function  GetRangeL(const Idx: Integer): LongWord; override;
    procedure SetRangeL(const Idx: Integer; const Value: LongWord); override;

  public
    { ABitArray                                                                }
    procedure Fill(const LoIdx, HiIdx: Integer; const Value: Boolean); override;
    function  IsRange(const LoIdx, HiIdx: Integer; const Value: Boolean): Boolean; override;
  end;



{                                                                              }
{ THashedStringArray                                                           }
{   AStringArray that maintains a hash lookup table of array values.           }
{                                                                              }
type
  THashedStringArray = class(TStringArray)
  protected
    FLookup        : Array of IntegerArray;
    FCaseSensitive : Boolean;

    function  LocateItemHashBuf(const ValueStrPtr: PChar;
              const ValueStrLen: Integer;
              var LookupList, LookupIdx: Integer): Boolean;
    function  LocateItemHash(const Value: String;
              var LookupList, LookupIdx: Integer): Boolean;
    procedure Rehash;

    procedure Init; override;
    procedure SetItem(const Idx: Integer; const Value: String); override;
    procedure SetData(const Data: StringArray); override;

  public
    constructor Create(const CaseSensitive: Boolean = True);

    procedure Assign(const Source: TObject); override;
    procedure Clear; override;

    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    procedure Delete(const Idx: Integer; const Count: Integer = 1); override;
    procedure Insert(const Idx: Integer; const Count: Integer = 1); override;
    function  AppendItem(const Value: String): Integer; override;

    function  PosNextBuf(const FindStrPtr: PChar; const FindStrLen: Integer;
              const PrevPos: Integer = -1): Integer;
    function  PosNext(const Find: String; const PrevPos: Integer = -1): Integer;
  end;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Fundamentals }
  cStrings;



{                                                                              }
{                                                                              }
{ TYPE BASE CLASSES                                                            }
{                                                                              }
{                                                                              }



{                                                                              }
{ AArray                                                                       }
{                                                                              }
procedure AArray.RaiseIndexError(const Idx: Integer);
begin
  RaiseTypeError('Array index out of bounds'
      {$IFDEF DEBUG} + ': ' + IntToStr(Idx) + '/' + IntToStr(GetCount){$ENDIF},
      nil, EArray);
end;

procedure AArray.Clear;
begin
  Count := 0;
end;

procedure AArray.Sort;

  procedure QuickSort(L, R: Integer);
  var I, J : Integer;
      M    : Integer;
    begin
      Repeat
        I := L;
        J := R;
        M := (L + R) shr 1;
        Repeat
          While CompareItems(I, M) = crLess do
            Inc(I);
          While CompareItems(J, M) = crGreater do
            Dec(J);
          if I <= J then
            begin
              ExchangeItems(I, J);
              if M = I then
                M := J else
                if M = J then
                  M := I;
              Inc(I);
              Dec(J);
            end;
        Until I > J;
        if L < J then
          QuickSort(L, J);
        L := I;
      Until I >= R;
    end;

var I : Integer;
begin
  I := Count;
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure AArray.ReverseOrder;
var I, L : Integer;
begin
  L := Count;
  For I := 1 to L div 2 do
    ExchangeItems(I - 1, L - I);
end;

procedure AArray.RemoveDuplicates(const IsSortedAscending: Boolean);
var I, C, J, L : Integer;
begin
  L := GetCount;
  if L = 0 then
    exit;
  if IsSortedAscending then
    begin
      J := 0;
      Repeat
        I := J + 1;
        While (I < L) and (CompareItems(I, J) = crEqual) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Delete(J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      While J < L - 1 do
        begin
          I := J + 1;
          While I <= L - 1 do
            if CompareItems(J, I) = crEqual then
              begin
                Delete(I, 1);
                Dec(L);
              end else
              Inc(I);
          Inc(J);
        end;
    end;
end;



{                                                                              }
{ ALongIntArray                                                                }
{                                                                              }
procedure ALongIntArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : LongInt;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function ALongIntArray.AppendItem(const Value: LongInt): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function ALongIntArray.GetRange(const LoIdx, HiIdx: Integer): LongIntArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function ALongIntArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := ALongIntArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  ALongIntArray(Result).Count := C;
  For I := 0 to C - 1 do
    ALongIntArray(Result)[I] := Item[L + I];
end;

procedure ALongIntArray.SetRange(const LoIdx, HiIdx: Integer; const V: LongIntArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure ALongIntArray.Fill(const Idx, Count: Integer; const Value: LongInt);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function ALongIntArray.AppendArray(const V: LongIntArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function ALongIntArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : LongInt;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if I < J then
    Result := crLess else
  if I > J then
    Result := crGreater else
    Result := crEqual;
end;

function ALongIntArray.PosNext(const Find: LongInt;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : LongInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if D > Find then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function ALongIntArray.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := IntToStr(Item[0]);
  For I := 1 to L - 1 do
    Result := Result + ',' + IntToStr(Item[I]);
end;

procedure ALongIntArray.SetAsString(const S: String);
var F, G, L, C : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  While F < C do
    begin
      G := 0;
      While (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      if G = 0 then
        Item[L - 1] := 0
      else
        Item[L - 1] := StrToInt(Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;

procedure ALongIntArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is ALongIntArray then
    begin
      L := ALongIntArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := ALongIntArray(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function ALongIntArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is ALongIntArray then
    begin
      L := ALongIntArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> ALongIntArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function ALongIntArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is ALongIntArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := ALongIntArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, ELongIntArray);
end;

procedure ALongIntArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure ALongIntArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ ALongWordArray                                                               }
{                                                                              }
procedure ALongWordArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : LongWord;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function ALongWordArray.AppendItem(const Value: LongWord): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function ALongWordArray.GetRange(const LoIdx, HiIdx: Integer): LongWordArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function ALongWordArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := ALongWordArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  ALongWordArray(Result).Count := C;
  For I := 0 to C - 1 do
    ALongWordArray(Result)[I] := Item[L + I];
end;

procedure ALongWordArray.SetRange(const LoIdx, HiIdx: Integer; const V: LongWordArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure ALongWordArray.Fill(const Idx, Count: Integer; const Value: LongWord);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function ALongWordArray.AppendArray(const V: LongWordArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function ALongWordArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : LongWord;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if I < J then
    Result := crLess else
  if I > J then
    Result := crGreater else
    Result := crEqual;
end;

function ALongWordArray.PosNext(const Find: LongWord;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : LongWord;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if D > Find then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function ALongWordArray.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := IntToStr(Item[0]);
  For I := 1 to L - 1 do
    Result := Result + ',' + IntToStr(Item[I]);
end;

procedure ALongWordArray.SetAsString(const S: String);
var F, G, L, C : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  While F < C do
    begin
      G := 0;
      While (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      if G = 0 then
        Item[L - 1] := 0
      else
        Item[L - 1] := StrToInt(Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;

procedure ALongWordArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is ALongWordArray then
    begin
      L := ALongWordArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := ALongWordArray(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function ALongWordArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is ALongWordArray then
    begin
      L := ALongWordArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> ALongWordArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function ALongWordArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is ALongWordArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := ALongWordArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, ELongWordArray);
end;

procedure ALongWordArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure ALongWordArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ AInt64Array                                                                  }
{                                                                              }
procedure AInt64Array.ExchangeItems(const Idx1, Idx2: Integer);
var I : Int64;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function AInt64Array.AppendItem(const Value: Int64): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function AInt64Array.GetRange(const LoIdx, HiIdx: Integer): Int64Array;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function AInt64Array.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := AInt64Array(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  AInt64Array(Result).Count := C;
  For I := 0 to C - 1 do
    AInt64Array(Result)[I] := Item[L + I];
end;

procedure AInt64Array.SetRange(const LoIdx, HiIdx: Integer; const V: Int64Array);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure AInt64Array.Fill(const Idx, Count: Integer; const Value: Int64);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function AInt64Array.AppendArray(const V: Int64Array): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function AInt64Array.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : Int64;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if I < J then
    Result := crLess else
  if I > J then
    Result := crGreater else
    Result := crEqual;
end;

function AInt64Array.PosNext(const Find: Int64;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Int64;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if D > Find then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function AInt64Array.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := IntToStr(Item[0]);
  For I := 1 to L - 1 do
    Result := Result + ',' + IntToStr(Item[I]);
end;

procedure AInt64Array.SetAsString(const S: String);
var F, G, L, C : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  While F < C do
    begin
      G := 0;
      While (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      if G = 0 then
        Item[L - 1] := 0
      else
        Item[L - 1] := StrToInt(Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;

procedure AInt64Array.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is AInt64Array then
    begin
      L := AInt64Array(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := AInt64Array(Source).Item[I];
    end else
  if Source is ALongIntArray then
    begin
      L := ALongIntArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := ALongIntArray(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function AInt64Array.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is AInt64Array then
    begin
      L := AInt64Array(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> AInt64Array(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function AInt64Array.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is AInt64Array then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := AInt64Array(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EInt64Array);
end;

procedure AInt64Array.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure AInt64Array.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ ASingleArray                                                                 }
{                                                                              }
procedure ASingleArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Single;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function ASingleArray.AppendItem(const Value: Single): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function ASingleArray.GetRange(const LoIdx, HiIdx: Integer): SingleArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function ASingleArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := ASingleArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  ASingleArray(Result).Count := C;
  For I := 0 to C - 1 do
    ASingleArray(Result)[I] := Item[L + I];
end;

procedure ASingleArray.SetRange(const LoIdx, HiIdx: Integer; const V: SingleArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure ASingleArray.Fill(const Idx, Count: Integer; const Value: Single);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function ASingleArray.AppendArray(const V: SingleArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function ASingleArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : Single;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if I < J then
    Result := crLess else
  if I > J then
    Result := crGreater else
    Result := crEqual;
end;

function ASingleArray.PosNext(const Find: Single;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Single;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if D > Find then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function ASingleArray.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := FloatToStr(Item[0]);
  For I := 1 to L - 1 do
    Result := Result + ',' + FloatToStr(Item[I]);
end;

procedure ASingleArray.SetAsString(const S: String);
var F, G, L, C : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  While F < C do
    begin
      G := 0;
      While (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      if G = 0 then
        Item[L - 1] := 0.0
      else
        Item[L - 1] := StrToFloat(Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;

procedure ASingleArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is ASingleArray then
    begin
      L := ASingleArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := ASingleArray(Source).Item[I];
    end else
  if Source is AInt64Array then
    begin
      L := AInt64Array(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := AInt64Array(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function ASingleArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is ASingleArray then
    begin
      L := ASingleArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> ASingleArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function ASingleArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is ASingleArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := ASingleArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, ESingleArray);
end;

procedure ASingleArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure ASingleArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ ADoubleArray                                                                 }
{                                                                              }
procedure ADoubleArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Double;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function ADoubleArray.AppendItem(const Value: Double): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function ADoubleArray.GetRange(const LoIdx, HiIdx: Integer): DoubleArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function ADoubleArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := ADoubleArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  ADoubleArray(Result).Count := C;
  For I := 0 to C - 1 do
    ADoubleArray(Result)[I] := Item[L + I];
end;

procedure ADoubleArray.SetRange(const LoIdx, HiIdx: Integer; const V: DoubleArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure ADoubleArray.Fill(const Idx, Count: Integer; const Value: Double);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function ADoubleArray.AppendArray(const V: DoubleArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function ADoubleArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : Double;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if I < J then
    Result := crLess else
  if I > J then
    Result := crGreater else
    Result := crEqual;
end;

function ADoubleArray.PosNext(const Find: Double;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Double;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if D > Find then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function ADoubleArray.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := FloatToStr(Item[0]);
  For I := 1 to L - 1 do
    Result := Result + ',' + FloatToStr(Item[I]);
end;

procedure ADoubleArray.SetAsString(const S: String);
var F, G, L, C : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  While F < C do
    begin
      G := 0;
      While (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      if G = 0 then
        Item[L - 1] := 0.0
      else
        Item[L - 1] := StrToFloat(Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;

procedure ADoubleArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is ADoubleArray then
    begin
      L := ADoubleArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := ADoubleArray(Source).Item[I];
    end else
  if Source is AInt64Array then
    begin
      L := AInt64Array(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := AInt64Array(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function ADoubleArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is ADoubleArray then
    begin
      L := ADoubleArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> ADoubleArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function ADoubleArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is ADoubleArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := ADoubleArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EDoubleArray);
end;

procedure ADoubleArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure ADoubleArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ AExtendedArray                                                               }
{                                                                              }
procedure AExtendedArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Extended;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function AExtendedArray.AppendItem(const Value: Extended): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function AExtendedArray.GetRange(const LoIdx, HiIdx: Integer): ExtendedArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function AExtendedArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := AExtendedArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  AExtendedArray(Result).Count := C;
  For I := 0 to C - 1 do
    AExtendedArray(Result)[I] := Item[L + I];
end;

procedure AExtendedArray.SetRange(const LoIdx, HiIdx: Integer; const V: ExtendedArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure AExtendedArray.Fill(const Idx, Count: Integer; const Value: Extended);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function AExtendedArray.AppendArray(const V: ExtendedArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function AExtendedArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : Extended;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if I < J then
    Result := crLess else
  if I > J then
    Result := crGreater else
    Result := crEqual;
end;

function AExtendedArray.PosNext(const Find: Extended;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Extended;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if D > Find then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function AExtendedArray.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := FloatToStr(Item[0]);
  For I := 1 to L - 1 do
    Result := Result + ',' + FloatToStr(Item[I]);
end;

procedure AExtendedArray.SetAsString(const S: String);
var F, G, L, C : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  While F < C do
    begin
      G := 0;
      While (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      if G = 0 then
        Item[L - 1] := 0.0
      else
        Item[L - 1] := StrToFloat(Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;

procedure AExtendedArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is AExtendedArray then
    begin
      L := AExtendedArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := AExtendedArray(Source).Item[I];
    end else
  if Source is AInt64Array then
    begin
      L := AInt64Array(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := AInt64Array(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function AExtendedArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is AExtendedArray then
    begin
      L := AExtendedArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> AExtendedArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function AExtendedArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is AExtendedArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := AExtendedArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EExtendedArray);
end;

procedure AExtendedArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure AExtendedArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ APointerArray                                                                }
{                                                                              }
procedure APointerArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Pointer;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function APointerArray.AppendItem(const Value: Pointer): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function APointerArray.GetRange(const LoIdx, HiIdx: Integer): PointerArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function APointerArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := APointerArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  APointerArray(Result).Count := C;
  For I := 0 to C - 1 do
    APointerArray(Result)[I] := Item[L + I];
end;

procedure APointerArray.SetRange(const LoIdx, HiIdx: Integer; const V: PointerArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure APointerArray.Fill(const Idx, Count: Integer; const Value: Pointer);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function APointerArray.AppendArray(const V: PointerArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function APointerArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : Pointer;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if LongWord(I) < LongWord(J) then
    Result := crLess else
  if LongWord(I) > LongWord(J) then
    Result := crGreater else
    Result := crEqual;
end;

function APointerArray.PosNext(const Find: Pointer;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Pointer;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if LongWord(D) > LongWord(Find) then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function APointerArray.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := PointerToStr(Item[0]);
  For I := 1 to L - 1 do
    Result := Result + ',' + PointerToStr(Item[I]);
end;

procedure APointerArray.SetAsString(const S: String);
var F, G, L, C : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  While F < C do
    begin
      G := 0;
      While (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      if G = 0 then
        Item[L - 1] := nil
      else
        Item[L - 1] := StrToPointer(Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;

procedure APointerArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is APointerArray then
    begin
      L := APointerArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := APointerArray(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function APointerArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is APointerArray then
    begin
      L := APointerArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> APointerArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function APointerArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is APointerArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := APointerArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EPointerArray);
end;

procedure APointerArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure APointerArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ AStringArray                                                                 }
{                                                                              }
procedure AStringArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : String;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function AStringArray.AppendItem(const Value: String): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function AStringArray.GetRange(const LoIdx, HiIdx: Integer): StringArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function AStringArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := AStringArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  AStringArray(Result).Count := C;
  For I := 0 to C - 1 do
    AStringArray(Result)[I] := Item[L + I];
end;

procedure AStringArray.SetRange(const LoIdx, HiIdx: Integer; const V: StringArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure AStringArray.Fill(const Idx, Count: Integer; const Value: String);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function AStringArray.AppendArray(const V: StringArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function AStringArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : String;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if I < J then
    Result := crLess else
  if I > J then
    Result := crGreater else
    Result := crEqual;
end;

function AStringArray.PosNext(const Find: String;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : String;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if D > Find then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function AStringArray.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := StrQuote(Item[0]);
  For I := 1 to L - 1 do
    Result := Result + ',' + StrQuote(Item[I]);
end;

procedure AStringArray.SetAsString(const S: String);
var F, G, L, C : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  While F < C do
    begin
      G := 0;
      While (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      if G = 0 then
        Item[L - 1] := ''
      else
        Item[L - 1] := StrUnquote(Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;

procedure AStringArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is AStringArray then
    begin
      L := AStringArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := AStringArray(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function AStringArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is AStringArray then
    begin
      L := AStringArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> AStringArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function AStringArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is AStringArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := AStringArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EStringArray);
end;

procedure AStringArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure AStringArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ AWideStringArray                                                             }
{                                                                              }
procedure AWideStringArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : WideString;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function AWideStringArray.AppendItem(const Value: WideString): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function AWideStringArray.GetRange(const LoIdx, HiIdx: Integer): WideStringArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function AWideStringArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := AWideStringArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  AWideStringArray(Result).Count := C;
  For I := 0 to C - 1 do
    AWideStringArray(Result)[I] := Item[L + I];
end;

procedure AWideStringArray.SetRange(const LoIdx, HiIdx: Integer; const V: WideStringArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure AWideStringArray.Fill(const Idx, Count: Integer; const Value: WideString);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function AWideStringArray.AppendArray(const V: WideStringArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function AWideStringArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : WideString;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if I < J then
    Result := crLess else
  if I > J then
    Result := crGreater else
    Result := crEqual;
end;

function AWideStringArray.PosNext(const Find: WideString;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : WideString;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if D > Find then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

procedure AWideStringArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is AWideStringArray then
    begin
      L := AWideStringArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := AWideStringArray(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function AWideStringArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is AWideStringArray then
    begin
      L := AWideStringArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> AWideStringArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function AWideStringArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is AWideStringArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := AWideStringArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EWideStringArray);
end;

procedure AWideStringArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure AWideStringArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ AObjectArray                                                                 }
{                                                                              }
procedure AObjectArray.Clear;
begin
  if IsItemOwner then
    FreeItems
  else
    ReleaseItems;
end;

procedure AObjectArray.Assign(const Source: TObject);
var I, L : Integer;
    V    : TObject;
begin
  if Source is AObjectArray then
    begin
      FreeItems;
      IsItemOwner := AObjectArray(Source).IsItemOwner;
      L := AObjectArray(Source).Count;
      Count := L;
      if GetIsItemOwner then
        For I := 0 to L - 1 do
          begin
            V := AObjectArray(Source)[I];
            if V is AArray then
              Item[I] := AArray(V).Duplicate else
              Item[I] := V;
          end
      else
        For I := 0 to L - 1 do
          Item[I] := AObjectArray(Source)[I];
    end else
    inherited Assign(Source);
end;

function AObjectArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
    A, B : TObject;
begin
  if V is AObjectArray then
    begin
      L := AArray(V).Count;
      if Count <> L then
        begin
          Result := False;
          exit;
        end;
      For I := 0 to L - 1 do
        begin
          A := Item[I];
          B := AObjectArray(V)[I];
          Result := A = B;
          if not Result then
            exit;
          end;
      Result := True;
    end else
    Result := inherited IsEqual(V);
end;

function AObjectArray.Compare(const V: TObject): TCompareResult;
var I, C1, C2 : Integer;
    A, B : TObject;
begin
  if V is AObjectArray then
    begin
      C1 := GetCount;
      C2 := AObjectArray(V).GetCount;
      if C1 < C2 then
        Result := crLess else
      if C1 > C2 then
        Result := crGreater else
        begin
          Result := crEqual;
          For I := 0 to GetCount - 1 do
            begin
              A := Item[I];
              B := AObjectArray(V)[I];
              if A <> B then
                begin
                  Result := crUndefined;
                  exit;
                end;
            end;
        end;
    end else
    Result := inherited Compare(V);
end;

function AObjectArray.GetRange(const LoIdx, HiIdx: Integer): ObjectArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L  + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[L + I] := Item[I];
end;

procedure AObjectArray.SetRange(const LoIdx, HiIdx: Integer; const V: ObjectArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L  + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

function AObjectArray.GetAsString: String;
var I, L : Integer;
    V : TObject;
begin
  Result := '';
  L := Count;
  For I := 0 to L - 1 do
    begin
      V := Item[I];
      Result := Result + PointerToStr(V);
      if I < L - 1 then
        Result := Result + ',';
    end;
end;

procedure AObjectArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : TObject;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function AObjectArray.AppendItem(const Value: TObject): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function AObjectArray.AppendArray(const V: ObjectArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

{$WARNINGS OFF}
function AObjectArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  if V is AObjectArray then
    begin
      Result := Count;
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := AObjectArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EObjectArray);
end;
{$WARNINGS ON}

procedure AObjectArray.Delete(const Idx, Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(Idx + I, GetItem(Idx + Count + I));
      SetCount(C - L);
    end;
end;

function AObjectArray.PosNext(const Find: TObject; const PrevPos: Integer): Integer;
var I : Integer;
begin
  For I := MaxI(PrevPos + 1, 0) to Count - 1 do
    if Find = Item[I] then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function AObjectArray.PosNext(var Item: TObject; const ClassType: TClass;
    const PrevPos: Integer): Integer;
var I : Integer;
begin
  For I := MaxI(PrevPos + 1, 0) to Count - 1 do
    begin
      Item := GetItem(I);
      if Item.InheritsFrom(ClassType) then
        begin
          Result := I;
          exit;
        end;
    end;
  Item := nil;
  Result := -1;
end;

function AObjectArray.PosNext(var Item: TObject; const ClassName: String;
    const PrevPos: Integer): Integer;
var I : Integer;
begin
  For I := MaxI(PrevPos + 1, 0) to Count - 1 do
    begin
      Item := GetItem(I);
      if Assigned(Item) and Item.ClassNameIs(ClassName) then
        begin
          Result := I;
          exit;
        end;
    end;
  Item := nil;
  Result := -1;
end;

function AObjectArray.Find(const ClassType: TClass; const Count: Integer): TObject;
var I, J : Integer;
begin
  I := -1;
  For J := 1 to Count do
    begin
      I := PosNext(Result, ClassType, I);
      if I = -1 then
        break;
    end;
  if I = -1 then
    Result := nil;
end;

function AObjectArray.Find(const ClassName: String; const Count: Integer): TObject;
var I, J : Integer;
begin
  I := -1;
  For J := 1 to Count do
    begin
      I := PosNext(Result, ClassName, I);
      if I = -1 then
        break;
    end;
  if I = -1 then
    Result := nil;
end;

function AObjectArray.FindAll(const ClassType: TClass): ObjectArray;
var I : Integer;
    V : TObject;
begin
  SetLength(Result, 0);
  I := PosNext(V, ClassType);
  While I >= 0 do
    begin
      Append(Result, V);
      I := PosNext(V, ClassType, I);
    end;
end;

function AObjectArray.FindAll(const ClassName: String): ObjectArray;
var I : Integer;
    V : TObject;
begin
  SetLength(Result, 0);
  I := PosNext(V, ClassName);
  While I >= 0 do
    begin
      Append(Result, V);
      I := PosNext(V, ClassName, I);
    end;
end;

function AObjectArray.CountItems(const ClassType: TClass): Integer;
var I : Integer;
    V : TObject;
begin
  Result := 0;
  I := PosNext(V, ClassType);
  While I >= 0 do
    begin
      Inc(Result);
      I := PosNext(V, ClassType, I);
    end;
end;

function AObjectArray.CountItems(const ClassName: String): Integer;
var I : Integer;
    V : TObject;
begin
  Result := 0;
  I := PosNext(V, ClassName);
  While I >= 0 do
    begin
      Inc(Result);
      I := PosNext(V, ClassName, I);
    end;
end;

function AObjectArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var A, B : TObject;
begin
  A := Item[Idx1];
  B := Item[Idx2];
  if A = B then
    Result := crEqual else
    Result := crUndefined;
end;

function AObjectArray.DeleteValue(const Value: TObject): Boolean;
var I : Integer;
begin
  I := PosNext(Value, -1);
  Result := I >= 0;
  if Result then
    Delete(I, 1);
end;

function AObjectArray.DeleteAll(const Value: TObject): Integer;
begin
  Result := 0;
  While DeleteValue(Value) do
    Inc(Result);
end;

function AObjectArray.ReleaseValue(const Value: TObject): Boolean;
var I : Integer;
begin
  I := PosNext(Value, -1);
  Result := I >= 0;
  if Result then
    ReleaseItem(I);
end;

function AObjectArray.RemoveItem(const Idx: Integer): TObject;
begin
  Result := ReleaseItem(Idx);
  Delete(Idx, 1);
end;

function AObjectArray.RemoveValue(const Value: TObject): Boolean;
var I : Integer;
begin
  I := PosNext(Value, -1);
  Result := I >= 0;
  if Result then
    RemoveItem(I);
end;



{                                                                              }
{ AInterfaceArray                                                              }
{                                                                              }
procedure AInterfaceArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : IInterface;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function AInterfaceArray.AppendItem(const Value: IInterface): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function AInterfaceArray.GetRange(const LoIdx, HiIdx: Integer): InterfaceArray;
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  For I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function AInterfaceArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := AInterfaceArray(CreateInstance);
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := H - L + 1;
  AInterfaceArray(Result).Count := C;
  For I := 0 to C - 1 do
    AInterfaceArray(Result)[I] := Item[L + I];
end;

procedure AInterfaceArray.SetRange(const LoIdx, HiIdx: Integer; const V: InterfaceArray);
var I, L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(Count - 1, HiIdx);
  C := MinI(Length(V), H - L + 1);
  For I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure AInterfaceArray.Fill(const Idx, Count: Integer; const Value: IInterface);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Item[I] := Value;
end;

function AInterfaceArray.AppendArray(const V: InterfaceArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function AInterfaceArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : IInterface;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if LongWord(I) < LongWord(J) then
    Result := crLess else
  if LongWord(I) > LongWord(J) then
    Result := crGreater else
    Result := crEqual;
end;

function AInterfaceArray.PosNext(const Find: IInterface;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : IInterface;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          Repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                While (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if LongWord(D) > LongWord(Find) then
              H := I - 1 else
              L := I + 1;
          Until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      For I := MaxI(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

procedure AInterfaceArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is AInterfaceArray then
    begin
      L := AInterfaceArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Item[I] := AInterfaceArray(Source).Item[I];
    end else
    inherited Assign(Source);
end;

function AInterfaceArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is AInterfaceArray then
    begin
      L := AInterfaceArray(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      For I := 0 to L - 1 do
        if Item[I] <> AInterfaceArray(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function AInterfaceArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is AInterfaceArray then
    begin
      L := V.Count;
      Count := Result + L;
      For I := 0 to L - 1 do
        Item[Result + I] := AInterfaceArray(V)[I];
    end
  else
    RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EInterfaceArray);
end;

procedure AInterfaceArray.Delete(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  J := MaxI(Idx, 0);
  C := GetCount;
  L := MinI(Count, C - J);
  if L > 0 then
    begin
      For I := J to J + C - 1 do
        SetItem(I, GetItem(I + Count));
      SetCount(C - L);
    end;
end;

procedure AInterfaceArray.Insert(const Idx: Integer; const Count: Integer);
var I, C, J, L : Integer;
begin
  if Count <= 0 then
    exit;
  C := GetCount;
  SetCount(C + Count);
  J := MinI(MaxI(Idx, 0), C);
  L := C - J;
  For I := C - 1 downto C - L do
    SetItem(I + Count, GetItem(I));
end;



{                                                                              }
{ ABitArray                                                                    }
{                                                                              }
function ABitArray.GetRangeL(const Idx: Integer): LongWord;
var I : Integer;
begin
  Result := 0;
  For I := 0 to BitsPerLongWord - 1 do
    if Bit[Idx + I] then
      Result := Result or BitMaskTable[I];
end;

procedure ABitArray.SetRangeL(const Idx: Integer; const Value: LongWord);
var I : Integer;
    C : LongWord;
begin
  C := 1;
  For I := Idx to Idx + BitsPerLongWord - 1 do
    begin
      Bit[I] := Value and C <> 0;
      C := C shl 1;
    end;
end;

procedure ABitArray.Fill(const Idx, Count: Integer; const Value: Boolean);
var I : Integer;
begin
  For I := Idx to Idx + Count - 1 do
    Bit[I] := Value;
end;

function ABitArray.IsRange(const LoIdx, HiIdx: Integer; const Value: Boolean): Boolean;
var I : Integer;
begin
  For I := LoIdx to HiIdx do
    if Bit[I] <> Value then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

procedure ABitArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is ABitArray then
    begin
      L := AArray(Source).Count;
      Count := L;
      For I := 0 to L - 1 do
        Bit[I] := ABitArray(Source)[I];
    end
  else
    inherited Assign(Source);
end;

function ABitArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is ABitArray then
    begin
      L := AArray(V).Count;
      if Count <> L then
        begin
          Result := False;
          exit;
        end;
      For I := 0 to L - 1 do
        if Bit[I] <> ABitArray(V)[I] then
          begin
            Result := False;
            exit;
          end;
      Result := True;
    end
  else
    Result := inherited IsEqual(V);
end;

procedure ABitArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Boolean;
begin
  I := Bit[Idx1];
  Bit[Idx1] := Bit[Idx2];
  Bit[Idx2] := I;
end;

function ABitArray.AppendItem(const Value: Boolean): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Bit[Result] := Value;
end;

function ABitArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
begin
  Result := cUtils.Compare(Bit[Idx1], Bit[Idx2]);
end;

procedure ABitArray.Invert;
var I : Integer;
begin
  For I := 0 to Count - 1 do
    Bit[I] := not Bit[I];
end;

function ABitArray.Find(const Value: Boolean; const Start: Integer): Integer;
var I, C : Integer;
begin
  if Start < 0 then
    I := 0
  else
    I := Start;
  C := Count;
  While I < C do
    if Bit[I] = Value then
      begin
        Result := I;
        exit;
      end
    else
      Inc(I);
  Result := -1;
end;

function ABitArray.FindRange(const Value: Boolean; const Start: Integer;
    const Count: Integer): Integer;
var I, C, F : Integer;
begin
  if Count <= 0 then
    begin
      Result := -1;
      exit;
    end;
  if Start < 0 then
    I := 0
  else
    I := Start;
  C := self.Count;
  F := 0;
  While I + F < C do
    if Bit[I + F] = Value then
      begin
        Inc(F);
        if F = Count then
          begin
            Result := I;
            exit;
          end;
      end
    else
      begin
        Inc(I, F + 1);
        F := 0;
      end;
  Result := -1;
end;

procedure ABitArray.Delete(const Idx: Integer; const Count: Integer);
var I, C : Integer;
begin
  C := GetCount;
  {$IFOPT R+}
  if (Idx < 0) or (Idx + Count > C) then
    RaiseIndexError(Idx);
  {$ENDIF}
  For I := Idx + Count to C - 1 do
    SetBit(I - Count, GetBit(I));
  SetCount(C - Count);
end;

procedure ABitArray.Insert(const Idx: Integer; const Count: Integer);
var I, C : Integer;
begin
  C := GetCount;
  {$IFOPT R+}
  if (Idx < 0) or (Idx > C) then
    RaiseIndexError(Idx);
  {$ENDIF}
  SetCount(C + Count);
  For I := Idx to C - 1 do
    SetBit(I + Count, GetBit(I));
  Fill(Idx, Idx + Count - 1, False);
end;

function ABitArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, C : Integer;
begin
  C := GetCount;
  {$IFOPT R+}
  if (LoIdx < 0) or (LoIdx > HiIdx) or (HiIdx >= C) then
    RaiseIndexError(HiIdx);
  {$ENDIF}
  Result := ABitArray(CreateInstance);
  C := HiIdx - LoIdx + 1;
  Result.Count := C;
  For I := 0 to C - 1 do
    ABitArray(Result)[I] := GetBit(LoIdx + I);
end;

function ABitArray.AppendArray(const V: AArray): Integer;
var I, C : Integer;
begin
  if V is ABitArray then
    begin
      Result := Count;
      C := ABitArray(V).Count;
      if C = 0 then
        exit;
      SetCount(Result + C);
      For I := 0 to C - 1 do
        SetBit(Result + I, ABitArray(V).GetBit(I));
    end
  else
    begin
      RaiseTypeError(ClassName + ' can not append ' + ObjectClassName(V), nil, EBitArray);
      Result := -1;
    end;
end;



{                                                                              }
{ ARRAY IMPLEMENTATIONS                                                        }
{                                                                              }
{   Memory allocation strategy to reduce memory copies:                        }
{     * For first allocation: allocate the exact size.                         }
{     * For change to < 16: allocate 16 entries.                               }
{     * For growing to >= 16: pre-allocate 1/8th of NewCount.                  }
{     * For shrinking blocks: shrink actual allocation when Count is less      }
{          than half of the allocated size.                                    }
{                                                                              }

{                                                                              }
{ TLongIntArray                                                                }
{                                                                              }
function TLongIntArray.GetItem(const Idx: Integer): LongInt;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TLongIntArray.SetItem(const Idx: Integer; const Value: LongInt);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TLongIntArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : LongInt;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TLongIntArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TLongIntArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TLongIntArray.AppendItem(const Value: LongInt): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TLongIntArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TLongIntArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TLongIntArray.GetRange(const LoIdx, HiIdx: Integer): LongIntArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TLongIntArray.SetRange(const LoIdx, HiIdx: Integer; const V: LongIntArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(LongInt));
end;

constructor TLongIntArray.Create(const V: LongIntArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TLongIntArray.SetData(const Data: LongIntArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TLongIntArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TLongIntArray;
  TLongIntArray(Result).FCount := C;
  if C > 0 then
    TLongIntArray(Result).FData := Copy(FData, L, C);
end;

procedure TLongIntArray.Assign(const V: LongIntArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TLongIntArray.Assign(const V: Array of LongInt);
begin
  FData := AsLongIntArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TLongIntArray.Assign(const Source: TObject);
begin
  if Source is TLongIntArray then
    begin
      FCount := TLongIntArray(Source).FCount;
      FData := Copy(TLongIntArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TLongWordArray                                                               }
{                                                                              }
function TLongWordArray.GetItem(const Idx: Integer): LongWord;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TLongWordArray.SetItem(const Idx: Integer; const Value: LongWord);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TLongWordArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : LongWord;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TLongWordArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TLongWordArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TLongWordArray.AppendItem(const Value: LongWord): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TLongWordArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TLongWordArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TLongWordArray.GetRange(const LoIdx, HiIdx: Integer): LongWordArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TLongWordArray.SetRange(const LoIdx, HiIdx: Integer; const V: LongWordArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(LongWord));
end;

constructor TLongWordArray.Create(const V: LongWordArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TLongWordArray.SetData(const Data: LongWordArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TLongWordArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TLongWordArray;
  TLongWordArray(Result).FCount := C;
  if C > 0 then
    TLongWordArray(Result).FData := Copy(FData, L, C);
end;

procedure TLongWordArray.Assign(const V: LongWordArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TLongWordArray.Assign(const V: Array of LongWord);
begin
  FData := AsLongWordArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TLongWordArray.Assign(const Source: TObject);
begin
  if Source is TLongWordArray then
    begin
      FCount := TLongWordArray(Source).FCount;
      FData := Copy(TLongWordArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TInt64Array                                                                  }
{                                                                              }
function TInt64Array.GetItem(const Idx: Integer): Int64;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TInt64Array.SetItem(const Idx: Integer; const Value: Int64);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TInt64Array.ExchangeItems(const Idx1, Idx2: Integer);
var I : Int64;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TInt64Array.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TInt64Array.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TInt64Array.AppendItem(const Value: Int64): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TInt64Array.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TInt64Array.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TInt64Array.GetRange(const LoIdx, HiIdx: Integer): Int64Array;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TInt64Array.SetRange(const LoIdx, HiIdx: Integer; const V: Int64Array);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Int64));
end;

constructor TInt64Array.Create(const V: Int64Array);
begin
  inherited Create;
  SetData(V);
end;

procedure TInt64Array.SetData(const Data: Int64Array);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TInt64Array.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TInt64Array;
  TInt64Array(Result).FCount := C;
  if C > 0 then
    TInt64Array(Result).FData := Copy(FData, L, C);
end;

procedure TInt64Array.Assign(const V: Int64Array);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TInt64Array.Assign(const V: Array of Int64);
begin
  FData := AsInt64Array(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TInt64Array.Assign(const Source: TObject);
begin
  if Source is TInt64Array then
    begin
      FCount := TInt64Array(Source).FCount;
      FData := Copy(TInt64Array(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TSingleArray                                                                 }
{                                                                              }
function TSingleArray.GetItem(const Idx: Integer): Single;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TSingleArray.SetItem(const Idx: Integer; const Value: Single);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TSingleArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Single;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TSingleArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TSingleArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TSingleArray.AppendItem(const Value: Single): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TSingleArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TSingleArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TSingleArray.GetRange(const LoIdx, HiIdx: Integer): SingleArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TSingleArray.SetRange(const LoIdx, HiIdx: Integer; const V: SingleArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Single));
end;

constructor TSingleArray.Create(const V: SingleArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TSingleArray.SetData(const Data: SingleArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TSingleArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TSingleArray;
  TSingleArray(Result).FCount := C;
  if C > 0 then
    TSingleArray(Result).FData := Copy(FData, L, C);
end;

procedure TSingleArray.Assign(const V: SingleArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TSingleArray.Assign(const V: Array of Single);
begin
  FData := AsSingleArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TSingleArray.Assign(const Source: TObject);
begin
  if Source is TSingleArray then
    begin
      FCount := TSingleArray(Source).FCount;
      FData := Copy(TSingleArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TDoubleArray                                                                 }
{                                                                              }
function TDoubleArray.GetItem(const Idx: Integer): Double;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TDoubleArray.SetItem(const Idx: Integer; const Value: Double);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TDoubleArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Double;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TDoubleArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TDoubleArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TDoubleArray.AppendItem(const Value: Double): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TDoubleArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TDoubleArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TDoubleArray.GetRange(const LoIdx, HiIdx: Integer): DoubleArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TDoubleArray.SetRange(const LoIdx, HiIdx: Integer; const V: DoubleArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Double));
end;

constructor TDoubleArray.Create(const V: DoubleArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TDoubleArray.SetData(const Data: DoubleArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TDoubleArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TDoubleArray;
  TDoubleArray(Result).FCount := C;
  if C > 0 then
    TDoubleArray(Result).FData := Copy(FData, L, C);
end;

procedure TDoubleArray.Assign(const V: DoubleArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TDoubleArray.Assign(const V: Array of Double);
begin
  FData := AsDoubleArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TDoubleArray.Assign(const Source: TObject);
begin
  if Source is TDoubleArray then
    begin
      FCount := TDoubleArray(Source).FCount;
      FData := Copy(TDoubleArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TExtendedArray                                                               }
{                                                                              }
function TExtendedArray.GetItem(const Idx: Integer): Extended;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TExtendedArray.SetItem(const Idx: Integer; const Value: Extended);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TExtendedArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Extended;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TExtendedArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TExtendedArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TExtendedArray.AppendItem(const Value: Extended): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TExtendedArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TExtendedArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TExtendedArray.GetRange(const LoIdx, HiIdx: Integer): ExtendedArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TExtendedArray.SetRange(const LoIdx, HiIdx: Integer; const V: ExtendedArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Extended));
end;

constructor TExtendedArray.Create(const V: ExtendedArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TExtendedArray.SetData(const Data: ExtendedArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TExtendedArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TExtendedArray;
  TExtendedArray(Result).FCount := C;
  if C > 0 then
    TExtendedArray(Result).FData := Copy(FData, L, C);
end;

procedure TExtendedArray.Assign(const V: ExtendedArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TExtendedArray.Assign(const V: Array of Extended);
begin
  FData := AsExtendedArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TExtendedArray.Assign(const Source: TObject);
begin
  if Source is TExtendedArray then
    begin
      FCount := TExtendedArray(Source).FCount;
      FData := Copy(TExtendedArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TPointerArray                                                                }
{                                                                              }
function TPointerArray.GetItem(const Idx: Integer): Pointer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TPointerArray.SetItem(const Idx: Integer; const Value: Pointer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TPointerArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : Pointer;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TPointerArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TPointerArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TPointerArray.AppendItem(const Value: Pointer): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TPointerArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TPointerArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TPointerArray.GetRange(const LoIdx, HiIdx: Integer): PointerArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TPointerArray.SetRange(const LoIdx, HiIdx: Integer; const V: PointerArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Pointer));
end;

constructor TPointerArray.Create(const V: PointerArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TPointerArray.SetData(const Data: PointerArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TPointerArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TPointerArray;
  TPointerArray(Result).FCount := C;
  if C > 0 then
    TPointerArray(Result).FData := Copy(FData, L, C);
end;

procedure TPointerArray.Assign(const V: PointerArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TPointerArray.Assign(const V: Array of Pointer);
begin
  FData := AsPointerArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TPointerArray.Assign(const Source: TObject);
begin
  if Source is TPointerArray then
    begin
      FCount := TPointerArray(Source).FCount;
      FData := Copy(TPointerArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TStringArray                                                                 }
{                                                                              }
function TStringArray.GetItem(const Idx: Integer): String;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TStringArray.SetItem(const Idx: Integer; const Value: String);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TStringArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : String;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TStringArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TStringArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLength(FData, N);
      FCapacity := N;
    end;
end;

function TStringArray.AppendItem(const Value: String): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TStringArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TStringArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TStringArray.GetRange(const LoIdx, HiIdx: Integer): StringArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TStringArray.SetRange(const LoIdx, HiIdx: Integer; const V: StringArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(String));
end;

constructor TStringArray.Create(const V: StringArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TStringArray.SetData(const Data: StringArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TStringArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TStringArray;
  TStringArray(Result).FCount := C;
  if C > 0 then
    TStringArray(Result).FData := Copy(FData, L, C);
end;

procedure TStringArray.Assign(const V: StringArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TStringArray.Assign(const V: Array of String);
begin
  FData := AsStringArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TStringArray.Assign(const Source: TObject);
begin
  if Source is TStringArray then
    begin
      FCount := TStringArray(Source).FCount;
      FData := Copy(TStringArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TWideStringArray                                                             }
{                                                                              }
function TWideStringArray.GetItem(const Idx: Integer): WideString;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TWideStringArray.SetItem(const Idx: Integer; const Value: WideString);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TWideStringArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : WideString;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TWideStringArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TWideStringArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLength(FData, N);
      FCapacity := N;
    end;
end;

function TWideStringArray.AppendItem(const Value: WideString): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TWideStringArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TWideStringArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TWideStringArray.GetRange(const LoIdx, HiIdx: Integer): WideStringArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TWideStringArray.SetRange(const LoIdx, HiIdx: Integer; const V: WideStringArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(WideString));
end;

constructor TWideStringArray.Create(const V: WideStringArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TWideStringArray.SetData(const Data: WideStringArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TWideStringArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TWideStringArray;
  TWideStringArray(Result).FCount := C;
  if C > 0 then
    TWideStringArray(Result).FData := Copy(FData, L, C);
end;

procedure TWideStringArray.Assign(const V: WideStringArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TWideStringArray.Assign(const V: Array of WideString);
begin
  FData := AsWideStringArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TWideStringArray.Assign(const Source: TObject);
begin
  if Source is TWideStringArray then
    begin
      FCount := TWideStringArray(Source).FCount;
      FData := Copy(TWideStringArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TObjectArray                                                                 }
{                                                                              }
constructor TObjectArray.Create(const V: ObjectArray; const IsItemOwner: Boolean);
begin
  inherited Create;
  FData := V;
  FIsItemOwner := IsItemOwner;
  FCount := Length(FData);
  FCapacity := FCount;
end;

destructor TObjectArray.Destroy;
begin
  if FIsItemOwner then
    FreeItems;
  inherited Destroy;
end;

procedure TObjectArray.Init;
begin
  inherited Init;
  FIsItemOwner := False;
end;

procedure TObjectArray.FreeItems;
begin
  FreeObjectArray(FData);
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TObjectArray.ReleaseItems;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

function TObjectArray.GetIsItemOwner: Boolean;
begin
  Result := FIsItemOwner;
end;

procedure TObjectArray.SetIsItemOwner(const IsItemOwner: Boolean);
begin
  FIsItemOwner := IsItemOwner;
end;

function TObjectArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TObjectArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if N = FCount then
    exit;
  if (N < FCount) and FIsItemOwner then
    FreeObjectArray(FData, N, FCount - 1);
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TObjectArray.GetItem(const Idx: Integer): TObject;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TObjectArray.SetItem(const Idx: Integer; const Value: TObject);
var P : ^TObject;
    V : TObject;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  P := Pointer(FData);
  Inc(P, Idx);
  if FIsItemOwner then
    begin
      V := P^;
      if V = Value then
        exit;
      V.Free;
    end;
  P^ := Value;
end;

function TObjectArray.AppendItem(const Value: TObject): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

function TObjectArray.ReleaseItem(const Idx: Integer): TObject;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
  if Assigned(Result) and FIsItemOwner then
    FData[Idx] := nil;
end;

function TObjectArray.GetRange(const LoIdx, HiIdx: Integer): ObjectArray;
begin
  Result := Copy(FData, LoIdx, MinI(HiIdx, FCount - 1) - LoIdx + 1);
end;

procedure TObjectArray.SetData(const Data: ObjectArray);
begin
  if FIsItemOwner then
    FreeItems;
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TObjectArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I : Integer;
    V : TObject;
begin
  Result := CreateInstance as TObjectArray;
  For I := LoIdx to MinI(HiIdx, FCount - 1) do
    begin
      V := FData[I];
      if V is AType then
        V := AType(V).Duplicate;
      TObjectArray(Result).AppendItem(V);
    end;
end;

procedure TObjectArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count, FIsItemOwner);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TObjectArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;



{                                                                              }
{ TInterfaceArray                                                              }
{                                                                              }
function TInterfaceArray.GetItem(const Idx: Integer): IInterface;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TInterfaceArray.SetItem(const Idx: Integer; const Value: IInterface);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure TInterfaceArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : IInterface;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function TInterfaceArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TInterfaceArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLength(FData, N);
      FCapacity := N;
    end;
end;

function TInterfaceArray.AppendItem(const Value: IInterface): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure TInterfaceArray.Delete(const Idx: Integer; const Count: Integer = 1);
var N : Integer;
begin
  N := Remove(FData, Idx, Count);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TInterfaceArray.Insert(const Idx: Integer; const Count: Integer = 1);
var I : Integer;
begin
  I := ArrayInsert(FData, Idx, Count);
  if I >= 0 then
    begin
      Inc(FCapacity, Count);
      Inc(FCount, Count);
    end;
end;

function TInterfaceArray.GetRange(const LoIdx, HiIdx: Integer): InterfaceArray;
var L, H : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure TInterfaceArray.SetRange(const LoIdx, HiIdx: Integer; const V: InterfaceArray);
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(MinI(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(IInterface));
end;

constructor TInterfaceArray.Create(const V: InterfaceArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TInterfaceArray.SetData(const Data: InterfaceArray);
begin
  FData := Data;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TInterfaceArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxI(0, LoIdx);
  H := MinI(HiIdx, FCount);
  C := MaxI(0, H - L + 1);
  Result := CreateInstance as TInterfaceArray;
  TInterfaceArray(Result).FCount := C;
  if C > 0 then
    TInterfaceArray(Result).FData := Copy(FData, L, C);
end;

procedure TInterfaceArray.Assign(const V: InterfaceArray);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TInterfaceArray.Assign(const V: Array of IInterface);
begin
  FData := AsInterfaceArray(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TInterfaceArray.Assign(const Source: TObject);
begin
  if Source is TInterfaceArray then
    begin
      FCount := TInterfaceArray(Source).FCount;
      FData := Copy(TInterfaceArray(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;



{                                                                              }
{ TBitArray                                                                    }
{                                                                              }
const
  TrueLongWord  : LongWord = $FFFFFFFF;
  FalseLongWord : LongWord = $00000000;

function TBitArray.GetBit(const Idx: Integer): Boolean;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := cUtils.IsBitSet(FData[Idx shr 5], Idx and 31);
end;

procedure TBitArray.SetBit(const Idx: Integer; const Value: Boolean);
var L : ^LongWord;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  L := @FData[Idx shr 5];
  if Value then
    L^ := cUtils.SetBit(L^, Idx and 31)
  else
    L^ := cUtils.ClearBit(L^, Idx and 31);
end;

function TBitArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TBitArray.SetCount(const NewCount: Integer);
begin
  if NewCount = FCount then
    exit;
  SetLengthAndZero(FData, (NewCount + BitsPerLongWord - 1) div BitsPerLongWord);
  FCount := NewCount;
end;

function TBitArray.GetRangeL(const Idx: Integer): LongWord;
var F : Byte;
  I : Integer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  F := Idx and 31;
  I := Idx shr 5;
  if F = 0 then
    Result := FData[I]
  else
    begin
      Result := FData[I] shr F;
      if I + 1 < Length(FData) then
        Result := Result or (FData[I + 1] shl (BitsPerLongWord - F));
    end;
end;

procedure TBitArray.SetRangeL(const Idx: Integer; const Value: LongWord);
var F : Byte;
    I : Integer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  F := Idx and 31;
  I := Idx shr 5;
  if F = 0 then
    FData[I] := Value
  else
    begin
      FData[I] := (FData[I] and LowBitMask(F))
               or (Value shl F);
      if I + 1 < Length(FData) then
        FData[I + 1] := (FData[I + 1] and HighBitMask(F))
                     or (Value shr (BitsPerLongWord - F));
    end;
end;

function TBitArray.IsRange(const LoIdx, HiIdx: Integer; const Value: Boolean): Boolean;
var B, I   : LongWord;
    IL, IH : Integer;
begin
  {$IFOPT R+}
  if (LoIdx < 0) or (LoIdx > HiIdx) or (HiIdx >= FCount) then
    RaiseIndexError(HiIdx);
  {$ENDIF}
  // Check bits in FData[IL]
  IL := LoIdx shr 5;
  IH := HiIdx shr 5;
  B := HighBitMask(LoIdx and 31);
  I := FData[IL];
  if Value then
    Result := I or B = I else
    Result := I and not B = I;
  if not Result or (IL = IH) then
    exit;
  // Check bits in FData[IH]
  B := LowBitMask(HiIdx and 31);
  I := FData[IH];
  if Value then
    Result := I or B = I else
    Result := I and not B = I;
  if not Result or (IH = IL + 1) then
    exit;
  // Check bits in FStore[IL + 1..IR - 1]
  For I := IL + 1 to IH - 1 do
    if (Value and (FData[I] <> TrueLongWord)) or
       (not Value and (FData[I] <> FalseLongWord)) then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

procedure TBitArray.Fill(const LoIdx, HiIdx: Integer; const Value: Boolean);
var B, I   : LongWord;
    IL, IH : Integer;
begin
  {$IFOPT R+}
  if (LoIdx < 0) or (LoIdx > HiIdx) or (HiIdx >= FCount) then
    RaiseIndexError(HiIdx);
  {$ENDIF}
  IL := LoIdx shr 5;
  IH := HiIdx shr 5;
  // Set bits in FData[IL]
  if IH = IL then
    B := RangeBitMask(LoIdx and 31, HiIdx and 31) else
    B := HighBitMask(LoIdx and 31);
  I := FData[IL];
  if Value then
    FData[IL] := I or B else
    FData[IL] := I and not B;
  if IH = IL then
    exit;
  // Set bits in FData[IH]
  B := LowBitMask(HiIdx and 31);
  I := FData[IH];
  if Value then
    FData[IH] := I or B else
    FData[IH] := I and not B;
  if IH = IL + 1 then
    exit;
  // Set bits in FData[IL + 1..IR - 1]
  For I := IL + 1 to IH - 1 do
    if Value then
      FData[I] := TrueLongWord else
      FData[I] := FalseLongWord;
end;



{                                                                              }
{ Hashed Array helper function                                                 }
{                                                                              }
const
  AverageHashChainSize = 4;

function ArrayRehashSize(const Count: Integer): Integer;
var L : Integer;
begin
  L := Count div AverageHashChainSize; // Number of slots
  if L <= 16 then                      // Rehash in powers of 16
    Result := 16 else
  if L <= 256 then
    Result := 256 else
  if L <= 4096 then
    Result := 4096 else
  if L <= 65536 then
    Result := 65536 else
  if L <= 1048576 then
    Result := 1048576 else
  if L <= 16777216 then
    Result := 16777216 else
    Result := 268435456;
end;



{                                                                              }
{ THashedStringArray                                                           }
{                                                                              }
constructor THashedStringArray.Create(const CaseSensitive: Boolean);
begin
  inherited Create(nil);
  FCaseSensitive := CaseSensitive;
end;

procedure THashedStringArray.Init;
begin
  inherited Init;
  FCaseSensitive := True;
end;

procedure THashedStringArray.Assign(const Source: TObject);
begin
  if Source is THashedStringArray then
    begin
      // Assign array data
      inherited Assign(Source);
      // Assign hash lookup
      FLookup := Copy(THashedStringArray(Source).FLookup);
      FCaseSensitive := THashedStringArray(Source).FCaseSensitive;
    end
  else
    inherited Assign(Source);
end;

procedure THashedStringArray.Clear;
begin
  inherited Clear;
  FLookup := nil;
end;

function THashedStringArray.LocateItemHashBuf(const ValueStrPtr: PChar;
    const ValueStrLen: Integer; var LookupList, LookupIdx: Integer): Boolean;
var I: Integer;
begin
  // Hash value
  if FCaseSensitive then
    LookupList := HashStrBuf(ValueStrPtr, ValueStrLen, Length(FLookup))
  else
    LookupList := HashStrBufNoCase(ValueStrPtr, ValueStrLen, Length(FLookup));
  // Locate value in hash lookup
  For I := 0 to Length(FLookup[LookupList]) - 1 do
    if StrPEqualStr(ValueStrPtr, ValueStrLen, FData[FLookup[LookupList][I]], FCaseSensitive) then
      begin
        LookupIdx := I;
        Result := True;
        exit;
      end;
  // Not found
  LookupIdx := -1;
  Result := False;
end;

function THashedStringArray.LocateItemHash(const Value: String;
         var LookupList, LookupIdx: Integer): Boolean;
begin
  Result := LocateItemHashBuf(Pointer(Value), Length(Value),
      LookupList, LookupIdx);
end;

procedure THashedStringArray.Rehash;
var I, C, L : Integer;
begin
  C := FCount;
  L := ArrayRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  For I := 0 to C - 1 do
    Append(FLookup[HashStr(FData[I], L, FCaseSensitive)], I);
end;

procedure THashedStringArray.ExchangeItems(const Idx1, Idx2: Integer);
var L1, L2, I1, I2: Integer;
begin
  // Swap lookup
  if LocateItemHash(FData[Idx1], L1, I1) and
     LocateItemHash(FData[Idx2], L2, I2) then
    Swap(FLookup[L1][I1], FLookup[L2][I2]);
  // Swap array items
  inherited ExchangeItems(Idx1, Idx2);
end;

procedure THashedStringArray.Delete(const Idx: Integer; const Count: Integer);
var I, L, V : Integer;
begin
  // Delete lookup
  For I := MaxI(0, Idx) to MinI(FCount, Idx + Count - 1) do
    if LocateItemHash(FData[I], L, V) then
      Remove(FLookup[L], V, 1);
  // Delete array
  inherited Delete(Idx, Count);
end;

procedure THashedStringArray.Insert(const Idx: Integer; const Count: Integer);
var I, J : Integer;
begin
  // Insert array
  inherited Insert(Idx, Count);
  // Add lookup
  J := MaxI(Idx, 0);
  For I := J to J + Count - 1 do
    Append(FLookup[0], I);
end;

procedure THashedStringArray.SetData(const Data: StringArray);
begin
  inherited SetData(Data);
  Rehash;
end;

procedure THashedStringArray.SetItem(const Idx: Integer; const Value: String);
var S    : String;
    I, J : Integer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  // Remove old hash
  S := FData[Idx];
  if LocateItemHash(S, I, J) then
    Remove(FLookup[I], J, 1);
  // Set array value
  FData[Idx] := Value;
  // Add new hash
  Append(FLookup[HashStr(Value, Length(FLookup), FCaseSensitive)], Idx);
end;

function THashedStringArray.AppendItem(const Value: String): Integer;
var L : Integer;
begin
  // add to array
  Result := Count;
  Count := Result + 1;
  FData[Result] := Value;
  // add lookup
  L := Length(FLookup);
  Append(FLookup[HashStr(Value, L, FCaseSensitive)], Result);
  if (Result + 1) div AverageHashChainSize > L then
    Rehash;
end;

function THashedStringArray.PosNextBuf(const FindStrPtr: PChar;
    const FindStrLen: Integer; const PrevPos: Integer): Integer;
var I, J, F, L, P : Integer;
begin
  // locate first
  if not LocateItemHashBuf(FindStrPtr, FindStrLen, I, J) then
    begin
      Result := -1;
      exit;
    end;
  if PrevPos < 0 then
    begin
      Result := FLookup[I][J];
      exit;
    end;
  // locate previous
  L := Length(FLookup[I]);
  P := -1;
  For F := J to L - 1 do
    if FLookup[I][F] = PrevPos then
      begin
        P := F;
        break;
      end;
  if P = -1 then
    begin
      Result := 1;
      exit;
    end;
  // locate next
  For F := P + 1 to L - 1 do
    begin
      Result := FLookup[I][F];
      if StrPEqualStr(FindStrPtr, FindStrLen, FData[Result], FCaseSensitive) then
        // found
        exit;
    end;
  // not found
  Result := 1;
end;

function THashedStringArray.PosNext(const Find: String; const PrevPos: Integer): Integer;
begin
  Result := PosNextBuf(Pointer(Find), Length(Find), PrevPos);
end;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
var I : Integer;
    F : TIntegerArray;
begin
  // TIntegerArray
  F := TIntegerArray.Create;
  For I := 0 to 16384 do
    Assert(F.AppendItem(I) = I, 'Array.AppendItem');
  Assert(F.Count = 16385, 'Array.Count');
  For I := 0 to 16384 do
    Assert(F[I] = I,      'Array.GetItem');
  For I := 0 to 16384 do
    F[I] := I + 1;
  For I := 0 to 16384 do
    Assert(F[I] = I + 1,  'Array.SetItem');
  F.Delete(0, 1);
  Assert(F.Count = 16384, 'Array.Delete');
  For I := 0 to 16383 do
    Assert(F[I] = I + 2,  'Array.Delete');
  F.Insert(0, 2);
  F[0] := 0;
  F[1] := 1;
  For I := 0 to 16384 do
    Assert(F[I] = I,      'Array.Insert');

  F.Count := 4;
  Assert(F.Count = 4,     'Array.SetCount');
  F[0] := 9;
  F[1] := -2;
  F[2] := 3;
  F[3] := 4;
  F.Sort;
  Assert(F[0] = -2,       'Array.Sort');
  Assert(F[1] = 3,        'Array.Sort');
  Assert(F[2] = 4,        'Array.Sort');
  Assert(F[3] = 9,        'Array.Sort');

  F.Count := 7;
  F[0] := 3;
  F[1] := 5;
  F[2] := 5;
  F[3] := 2;
  F[4] := 5;
  F[5] := 5;
  F[6] := 1;
  F.Sort;
  Assert(F[0] = 1,        'Array.Sort');
  Assert(F[1] = 2,        'Array.Sort');
  Assert(F[2] = 3,        'Array.Sort');
  Assert(F[3] = 5,        'Array.Sort');
  Assert(F[4] = 5,        'Array.Sort');
  Assert(F[5] = 5,        'Array.Sort');
  Assert(F[6] = 5,        'Array.Sort');

  F.Count := 7;
  F[0] := 1;
  F[1] := 5;
  F[2] := 5;
  F[3] := 1;
  F[4] := 5;
  F[5] := 2;
  F[6] := 1;
  F.RemoveDuplicates(False);
  Assert(F.Count = 3,     'Array.RemoveDuplicates');
  Assert(F[0] = 1,        'Array.RemoveDuplicates');
  Assert(F[1] = 5,        'Array.RemoveDuplicates');
  Assert(F[2] = 2,        'Array.RemoveDuplicates');

  F.Count := 7;
  F[0] := 1;
  F[1] := 1;
  F[2] := 1;
  F[3] := 2;
  F[4] := 5;
  F[5] := 5;
  F[6] := 5;
  F.RemoveDuplicates(True);
  Assert(F.Count = 3,     'Array.RemoveDuplicates');
  Assert(F[0] = 1,        'Array.RemoveDuplicates');
  Assert(F[1] = 2,        'Array.RemoveDuplicates');
  Assert(F[2] = 5,        'Array.RemoveDuplicates');

  F.Clear;
  Assert(F.Count = 0,     'Array.Clear');
  F.Free;
end;



end.

