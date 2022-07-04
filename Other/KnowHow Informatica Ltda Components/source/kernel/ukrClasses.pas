{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukrClasses;

{$I s:\v100\include\iKLIB100.inc}

{$BOOLEVAL OFF}

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Menus, Forms, Controls, Grids,
	TypInfo, uksyTypes, uksyUtils, uksyClasses, ukrConsts;

type

	EKRClasses = class( EKKernel );

{
--------------------------------------------------------------------------------
------------------ Generic Linkable Components Architecture --------------------
--------------------------------------------------------------------------------
}

	TLinkEvent = type Byte;
	TKCustomLink = class;
	TKCustomLinkable = class;
	TKCustomLinkableClass = class of TKCustomLinkable;

	TKLinkNotification = procedure( Link: TKCustomLink;
		LinkEvent: TLinkEvent; Data: LongInt ) of object;

{ TKCustomLink }

	TKCustomLink = class( TPersistent )
	private
		FOwner: TKCustomLinkable;
		FLinkNotification: TKLinkNotification;

		procedure SetLinkAble( Value: TKCustomLinkable );

	protected
		procedure DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt ); virtual;

		property Owner: TKCustomLinkable
						 read FOwner write SetLinkAble;
		property OnLinkEvent: TKLinkNotification
						 read FLinkNotification write FLinkNotification;

	public
	  destructor Destroy; override;
		constructor Create( AOwner: TKCustomLinkable ); virtual;
		constructor CreateLinked( AOwner: TKCustomLinkable;
		  AOwnerClass: TKCustomLinkableClass ); virtual;

	end;

	TKCustomLinkClass = class of TKCustomLink;

{ TKCustomLinkable }

	TKControlMethod = procedure( AControl: TControl ) of object;
	TKComponentMethod = procedure( AComponent: TComponent ) of object;

	TKCustomLinkable = class( TComponent )
	private
		FLinkList: TList;

		procedure AddLink( ALink: TKCustomLink );
		procedure RemoveLink( ALink: TKCustomLink );
		function GetInstanceCount( Index: TComponentClass ): Integer;

	protected
		procedure NotifyLinks( LinkEvent: TLinkEvent; Data: LongInt ); virtual;

	public
		destructor Destroy; override;

		procedure ForEachControlDo( AMethod: TKControlMethod );
		procedure ForEachComponentDo( AMethod: TKComponentMethod );
		procedure ForEachControlClassDo( AClass: TControlClass; AMethod: TKControlMethod );
		procedure ForEachComponentClassDo( AClass: TComponentClass; AMethod: TKComponentMethod );

		procedure GetInstanceList( AList: TList; AClass: TComponentClass ); dynamic;

		property InstanceCount[Index: TComponentClass]: Integer
						 read GetInstanceCount;

	end;
  
{
--------------------------------------------------------------------------------
-------------------------------- List Objects ----------------------------------
--------------------------------------------------------------------------------
}

{ TKStrings }

	EKStrings = class( EKRClasses );

	TKStrings = class( TStringList )
	private
		FName: string;
		FFreeObjects: Boolean;

		procedure SetValue( const Name, Value: string );
		function GetValuesByIndex( Index: Integer ): string;
		procedure SetValuesByIndex( Index: Integer; const Value: string );
		function GetNames( Index: Integer ): string;
		procedure SetNames( Index: Integer; const Value: string );

	protected
		procedure DestroyObject( Index: Integer ); dynamic;
    procedure QuickSort( l, r: Integer; SortType: TKStringType ); dynamic;
		function BinarySearch( SearchType: TKStringType; const Value: string ): Integer; dynamic;
		function BinarySearchNearest( SearchType: TKStringType; Higher: Boolean;
			const Value: string ): Integer; dynamic;

	public
		destructor Destroy; override;
		constructor Create; virtual;
		constructor CreateAutoFree( FreeObjects: Boolean ); virtual;
		constructor CreateFromStrings( sl: TStrings ); virtual;

		procedure AdjustForValues;
		procedure Assign( Source: TPersistent ); override;
		function Contains( const Source: string ): Boolean;
		function ContainsObject( Source: TObject ): Boolean;
    procedure SortEx( SortType: TKStringType ); dynamic;
		procedure Clear; override;
		function AddLine: Integer; virtual;
		procedure Intersect( List: TStrings );
		procedure IntersectObjects( List: TStrings );
		procedure Delete( Index: Integer ); override;
		function Remove( const Source: string ): Integer; virtual;
		function RemoveObject( Source: TObject ): Integer; virtual;
		function GetNameIndex( const Value: string ): Integer; virtual;
		function GetValueIndex( const Value: string ): Integer; virtual;
		function GetStringIndex( const Value: string ): Integer; virtual;
		function GetNearestNameIndex( IsHigher: Boolean; const Value: string ): Integer; virtual;
		function GetNearestValueIndex( IsHigher: Boolean; const Value: string ): Integer; virtual;
		function GetNearestStringIndex( IsHigher: Boolean; const Value: string ): Integer; virtual;

		function AddFmt( const S: string; const Args: array of const ): Integer; virtual;
		function IndexOfValues( const Value: string ): Integer; virtual;

		class procedure Error( const Message: string; Data: Integer ); virtual;

		property FreeObjects: Boolean
						 read FFreeObjects;
		property Name: string
						 read FName write FName;
		property Names[Index: Integer]: string
						 read GetNames write SetNames;				 
		property ValuesByIndex[Index: Integer]: string
						 read GetValuesByIndex write SetValuesByIndex;
		property Values
						 write SetValue;

	end;

	TKStringsClass = class of TKStrings;

{ TKPCharStream }

	EKPCharStream = class( EKRClasses );

{ TKCustomPCharStream }

	TKCustomPCharStream = class( TObject )
	private
		FStream: TStream;
		FOwnStream: Boolean;

		procedure Normalize;
		procedure ForceIndex( Index: Cardinal );
		procedure SetItemIndex( Index: Cardinal );
		procedure ForcePosition( APosition: Cardinal );

		function GetPCharCount: Cardinal;
		function GetPChars( Index: Cardinal ): PChar;
		function GetPCharCopy( Index: Cardinal ): PChar;
		procedure SetPChars( Index: Cardinal; APChar: PChar );

	protected
		property ItemIndex: Cardinal
						 write SetItemIndex;

		property PCharCount: Cardinal
						 read GetPCharCount;
		property PChars[Index: Cardinal]: PChar
						 read GetPChars write SetPChars; default;
		property PCharCopy[Index: Cardinal]: PChar
						 read GetPCharCopy;
		property Stream: TStream
						 read FStream;

	public
		destructor Destroy; override;
		constructor Create( AStream: TStream ); virtual;
		constructor CreateFromFile( const AFileName: string ); virtual;

		function ReadPChar( var APChar: PChar ): Cardinal;
		function ReadStrings( Strings: TStrings ): Cardinal;
		function ReadString( var AString: string ): Cardinal;

		function WritePChar( APChar: PChar ): Cardinal;
		function WriteStrings( Strings: TStrings ): Cardinal;
		function WriteString( const AString: string ): Cardinal;

		procedure LoadFromStringsFile( const FileName: string );

		function Insert( APChar: PChar ): Boolean;
		function InsertAt( APChar: PChar; APosition: Cardinal ): Boolean;

		function Delete: Boolean;
		function DeleteAt( APosition: Cardinal ): Boolean;

		function AsPChar: PChar;
		function PCharAt( APosition: Cardinal ): PChar;

		function AsPCharCopy: PChar;
		function PCharCopyAt( APosition: Cardinal ): PChar;

		function AsString: string;
		function StringAt( APosition: Cardinal ): string;

		procedure SetStrings( Strings: TStrings );
		procedure SetStringsAt( Strings: TStrings; APosition: Cardinal );

	end;

{ TKPCharStream }

	TKPCharStream = class( TKCustomPCharStream )
	public
		property PCharCount;
		property PChars;
		property PCharCopy;
		property Stream;

	end;

{ TKMemoryStream }

	TKMemoryDataStream = class( TKCustomMemoryStream )
	public
		function Write( const Buffer; Count: LongInt ): LongInt; override;

	end;

{ TKStringStream }

	EKStringStream = class( EKCustomStringStream );

	TKStringStream = class( TKCustomStringStream )
	public
		constructor CreateFromStrings( sl: TStrings ); virtual;
		constructor CreateFromStream( AStream: TStream; SourcePos, Count: Cardinal ); virtual;

		procedure SkipSpaces;
		function GetChar: Char;
		function GetLine: string;
		function GetFloat: Extended;
		function GetInteger: LongInt;
		procedure PutBack( const Token: string );
		procedure SaveToFile( const FileName: string );
		function GetToken( const Delimiters: string ): string;
		function Write( const Buffer; Count: LongInt ): LongInt; override;
		function GetNextShortString( const Delimiters: ShortString ): ShortString;
		procedure CopyBuffer( const SourcePos, DestPos: Integer; Count: Integer );
		procedure CopyBufferEx( const SourcePos, DestPos: Integer; Count: Integer );

		procedure ReadBufferErr( var Buffer; Count: Integer; const Message: string );
		procedure WriteBufferErr( const Buffer; Count: Integer; const Message: string );
		procedure ReadBufferErrFmt( var Buffer; Count: Integer; const Message: string;
			const Args: array of const );
		procedure WriteBufferErrFmt( const Buffer; Count: Integer; const Message: string;
			const Args: array of const );
		procedure CopyFromBuffer( Source: TStream; Count: LongInt );

	end;

{ Stream Routines }

procedure CopyBuffer( Stream: TStream; const SourcePos, DestPos: Integer; Count: Integer );
procedure CopyBufferEx( Stream: TStream; const SourcePos, DestPos: Integer; Count: Integer );

type

{ TKDataStructure }

  EKDataStructure = class( EKRClasses );

	TKDataStructure = class;
	TKDataStructureClass = class of TKDataStructure;

	TKDataStructure = class( TObject )
	private
		FList: TList;

		function GetCount: Integer;

	protected
		function GetList: TList;
    function IndexOf( Item: Pointer ): Integer;
    
    procedure AssignError( Source: TKDataStructure ); dynamic;
    procedure AssignTo( Dest: TKDataStructure ); virtual;
    procedure Append( Sender: TKDataStructure ); virtual; abstract;
    
		property List: TList
						 read GetList;

	public
    destructor Destroy; override;
		constructor Create; virtual;
    procedure Assign( Source: TKDataStructure ); virtual;

		procedure Clear; virtual;

		property Count: Integer
						 read GetCount;

	end;

{ TKFifo }

	EKFifo = class( EKDataStructure );

	TKFifo = class( TKDataStructure )
  protected
    procedure AssignTo( Dest: TKDataStructure ); override;

	public
    procedure Clear; override;
		function Push( Item: Pointer ): Integer;
		function Pop: Pointer;
    procedure Append( Sender: TKDataStructure ); override;
    
	end;

	TKFifoClass = class of TKFifo;

{ TKStack }

	EKStack = class( EKDataStructure );

	TKStack = class( TKDataStructure )
  protected
    procedure AssignTo( Dest: TKDataStructure ); override;

	public
		procedure Clear; override;
		function Push( Item: Pointer ): Integer;
		function Pop: Pointer;
    procedure Append( Sender: TKDataStructure ); override;

	end;

	TKStackClass = class of TKStack;

{ TKIntegerFifo }

	TKIntegerFifo = class( TKFifo )
	public
    function IndexOf( Item: Integer ): Integer; virtual;
		function Push( Item: Integer ): Integer; virtual;
		function Pop: Integer; virtual;

	end;

{ TKExtendedFifo }

	TKExtendedFifo = class( TKFifo )
	public
		function Push( Item: Extended ): Integer; virtual;
		function Pop: Extended; virtual;

	end;

{ TKStringFifo }

	TKStringFifo = class( TKFifo )
	public
		function Push( const Item: string ): Integer; virtual;
		function Pop: string; virtual;

	end;

{ TKIntegerStack }

	TKIntegerStack = class( TKStack )
	public
    function IndexOf( Item: Integer ): Integer; virtual;
		function Push( Item: Integer ): Integer; virtual;
		function Pop: Integer; virtual;

	end;

{ TKExtendedStack }

	TKExtendedStack = class( TKStack )
	public
		function Push( Item: Extended ): Integer; virtual;
		function Pop: Extended; virtual;

	end;

{ TKStringStack }

	TKStringStack = class( TKStack )
	public
		function Push( const Item: string ): Integer; virtual;
		function Pop: string; virtual;

	end;

{ TK32BitsList }

	TK32BitsList = class( TObject )
	private
		FBitsList: LongInt;

		procedure SetBits( Index: TKBitEnum; const Value: Boolean );
		function GetBits( Index: TKBitEnum ): Boolean;
		procedure SetAsString( const Value: string );
		function GetAsString: string;
		function GetBitSet: Byte;
		function ValidRange( const Index: Integer ): Boolean;

	public
		constructor Create;
		constructor CreateFilled( const B: array of Boolean );

		procedure AssignBits( ABits: TBits );

		procedure FlipAllBits;
		procedure ClearAllBits;
		procedure ClearBit( BitNo: TKBitEnum );
		procedure SetBit( BitNo: TKBitEnum );
		procedure FlipBit( BitNo: TKBitEnum );
		procedure ReverseBits;
		procedure Boolean2BitList( const B: array of Boolean );

		property Bits[Index: TKBitEnum]: Boolean
						 read GetBits write SetBits;
		property BitsSetCount: Byte
						 read GetBitSet;
		property BitsList: LongInt
						 read FBitsList;
		property AsString: string
						 read GetAsString write SetAsString;

	end;

{ TKBits }

	EKBits = class( EKRClasses );

	TKBits = class( TBits )
	private
		function GetAsString: string;
		procedure SetAsString( const Value: string );

	public
		constructor CreateFromString( const Value: string ); virtual;
    constructor CreateDefault( ASize: Integer; ABits: Boolean ); virtual;

		procedure Clear; virtual;
		procedure Assign( Source: TBits ); virtual;
		function InvOpenBit: Integer; virtual;

		property AsString: string
						 read GetAsString write SetAsString;

	end;

{ TKBitsWrapper }

	TKBitsWrapper = class( TPersistent )
	private
		FBits: TKBits;
		FOwner: TPersistent;

		procedure ReadBits( Reader: TReader );
		procedure WriteBits( Writer: TWriter );

	protected
		procedure DefineProperties( Filer: TFiler ); override;
		function GetOwnerInstance: TPersistent;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TPersistent ); virtual;

		property Bits: TKBits
						 read FBits;
		property Owner: TPersistent
						 read GetOwnerInstance;

	end;

{ TKBytes }

	EKBytes = class( EKRClasses );

	TKBytes = class( TObject )
	private
		FCount: LongInt;
		FBytes: PByteArray;

		procedure SetCount( sz: LongInt );
		procedure CopyBytes( pt: Pointer; sz: LongInt );
		function  GetLong( Index: LongInt ): LongInt;
		procedure SetLong( Index: LongInt; Value: LongInt );
		function  GetWord( Index: LongInt ): Word;
		procedure SetWord( Index: LongInt; Value: Word );
		function  GetByte( Index: LongInt ): Byte;
		procedure SetByte( Index: LongInt; Value: Byte );
		function  GetBits( BitPos: LongInt ): Boolean;
		procedure SetBits( BitPos: LongInt; Value: Boolean );
		function  GetDibits( DibitPos: LongInt ): Byte;
		procedure SetDibits( DibitPos: LongInt; Value: Byte );
		function  GetNibble( NibblePos: LongInt ): Byte;
		procedure SetNibble( NibblePos: LongInt; Value: Byte );

	protected
		property BytesList: PByteArray
						 read FBytes;

	public
		destructor  Destroy; override;
		constructor Create( Size: LongInt ); virtual;
		constructor CreateCopy( pt: Pointer; sz: LongInt );  virtual;

		procedure Clear;

		procedure AssignBits( ABits: TBits );

		function AsString: string;
		function AssignBytes( pt: Pointer; sz: LongInt ): Boolean;
		function SetBit( BitPos: LongInt; Value: Boolean ): Boolean;
		function GetBit( BitPos: LongInt; var Bit: Boolean ): Boolean;
		function SetDibit( DibitPos: LongInt; Value: Byte ): Boolean;
		function GetDibit( DibitPos: LongInt; var Dibit: Byte ): Boolean;

		property Count: LongInt
						 read FCount write SetCount;
		property Longs[Index: LongInt]: LongInt
						 read GetLong write SetLong;
		property Words[Index: LongInt]: Word
						 read GetWord write SetWord;
		property Bytes[Index: LongInt]: Byte
						 read GetByte write SetByte; default;
		property Bits[Index: LongInt]: Boolean
						 read GetBits write SetBits;
		property DiBits[Index: LongInt]: Byte
						 read GetDiBits write SetDiBits;
		property Nibbles[Index: LongInt]: Byte
						 read GetNibble write SetNibble;

	end;

{
--------------------------------------------------------------------------------
------------------------------- Charset Objects --------------------------------
--------------------------------------------------------------------------------
}

	EKCharSet = class( EKRClasses );

{ TKPersistentCharSet }

	TKPersistentCharSet = class( TPersistent )
	private
		FOwner: TComponent;
		FCharSet: TKCharSet;

		function GetCharSet: TKCharSet;
		procedure SetCharSet( const Value: TKCharSet );
		procedure SetCharSetSetString( const Value: string );
		function GetCharSetAsString: string;
		function GetCharSetAsTaggedString: string;
		function GetCharSetAsSetString: string;
		procedure SetCharSetAsString( const Value: string );
		procedure WriteData( Writer: TWriter );
		procedure ReadData( Reader: TReader );

	protected
		procedure DefineProperties( Filer: TFiler ); override;

	public
		constructor Create( AOwner: TComponent ); virtual;

		procedure Assign( Source: TPersistent ); override;

		property Owner: TComponent
						 read FOwner;
		property CharSet: TKCharSet
						 read GetCharSet write SetCharSet;
		property AsString: string
						 read GetCharSetAsString write SetCharSetAsString;
		property AsTaggedString: string
						 read GetCharSetAsTaggedString;
		property AsSetString: string
						 read GetCharSetAsSetString write SetCharSetSetString;

	end;

{
--------------------------------------------------------------------------------
---------------------------- TStringsArray Objects -----------------------------
--------------------------------------------------------------------------------
}

	TKLocation = record
		Row: Integer;
		Col: Integer;
	end;

	TEditorState = ( esGetting, esSetting, esPreparing, esAddRow, esAddCol, esDelRow,
		esDelCol, esDestroying, esCleaning, esHelp, esNone );

	EKStringsArray = class( EKRClasses );

{ TKCustomStringsArray }

	TKCustomStringsArray = class( TPersistent )
	private
		FList: TList;
		FBits: TBits;
		FName: string;
		FFreeObjects: Boolean;
		FAcceptNoOwnedNilAdd: Boolean;
		FColsAsRows: Boolean;
		FDuplicates: TDuplicates;
		FUpdateCount: Integer;
		FOnChanged: TNotifyEvent;
		FOnChanging: TNotifyEvent;

		procedure SetUpdateState( Updating: Boolean );
		function GetCount: Integer;
		function GetStrCount( ARow: Integer ): Integer;
		function GetHighColCount: Integer;
		function Get( ARow, ACol: Integer ): string;
		function GetObjects( ARow, ACol: Integer ): TObject;
		function GetValues( ARow: Integer; const ANames: string ): string;
		function GetNames( ARow, ACol: Integer ): string;
		function GetText( ARow: Integer ): string;
		function GetCommaText( ARow: Integer ): string;
		function GetValuesByIndex( ARow, ACol: Integer ): string;
		procedure Put( ARow, ACol: Integer; Value: string );
		procedure PutObjects( ARow, ACol: Integer; Value: TObject );
		procedure SetText( ARow: Integer; const Value: string );
		procedure SetCommaText( ARow: Integer; const Value: string );
		procedure SetDuplicates( const Value: TDuplicates );
		procedure SetValues( ARow: Integer; const ANames, Value: string );
		procedure SetValuesByIndex( ARow, ACol: Integer; const Value: string );
		procedure ClearRow( I: Integer );
		procedure ReadStrings( Reader: TReader );
		procedure WriteStrings( Writer: TWriter );

	protected
		function GetRows( ARow: Integer ): TKStrings;
		function GetCols( ACol: Integer ): TKStrings;
		procedure SetRows( ARow: Integer; Strings: TKStrings );
		procedure SetCols( ACol: Integer; Strings: TKStrings );
		procedure DefineProperties( Filer: TFiler ); override;
		function GetStringsClass: TKStringsClass; virtual; abstract;
		procedure CheckOrCreate( ARow: Integer ); dynamic;
		function GetRowAsString( ARow: Integer ): string; virtual;
		procedure SetRowAsString( ARow: Integer; const Value: string ); virtual;

		procedure Changed; dynamic;
		procedure Changing; dynamic;

		procedure TestRow( ARow: Integer );
		procedure TestCol( ARow, ACol: Integer );

		procedure FreeObject( AObject: Pointer; Location: TKLocation ); virtual;

		procedure ReadStringsArray( Reader: TReader; const ARow: Integer ); virtual;
		procedure WriteStringsArray( Writer: TWriter; const ARow: Integer ); virtual;

		procedure LoadFromStream( Stream: TStream ); virtual;
		procedure SaveToStream( Stream: TStream ); virtual;

	public
		destructor Destroy; override;
		constructor Create( FreeObjects: Boolean ); virtual;
		
		procedure Assign( Source: TPersistent ); override;

		procedure LoadFromFile( const FileName: string );
		procedure SaveToFile( const FileName: string );

		procedure GetHeader( Sender: TObject; Header: TStrings ); virtual;
		procedure GetGridOptions( Sender: TObject; var GridOptions: TGridOptions ); virtual;
		procedure EditorState( Sender: TObject; MultiString: TKCustomStringsArray;
		 State: TEditorState; var Handled: Boolean ); virtual;

		procedure Clear; virtual;

		function Equals( MS: TKCustomStringsArray ): Boolean;

		procedure BeginUpdate;
		procedure EndUpdate;
		procedure Delete( ARow: Integer ); virtual;
		procedure DeleteStr( ARow, ACol: Integer ); virtual;
		procedure Exchange( ARow1, ARow2: Integer ); virtual;
		procedure ExchangeColStr( ARow, ACol1, ACol2: Integer ); virtual;
		procedure ExchangeRowStr( ACol, ARow1, ARow2: Integer ); virtual;
		function ExchangeStr( const Item1, Item2: string ): Boolean; virtual;
		function Find( const S: string; var ARow, ACol: Integer ): Boolean; virtual;
		procedure SortRow( ARow: Integer ); virtual;
		class procedure Error( const Message: string; Data: Integer ); virtual;

		function AddStr( const S: string; const ARow: Integer ): Integer; virtual;
		function AddStrObject( const S: string; AObject: TObject;
			const ARow: Integer ): Integer; virtual;
		function IndexOf( const S: string ): TKLocation; virtual; 

		function Add( S: TKStrings; const AName: string ): Integer;

		function GetNameIndex( ARow: Integer; const Value: string ): Integer; virtual;
		function GetValueIndex( ARow: Integer; const Value: string ): Integer; virtual;
		function GetNearestNameIndex( ARow: Integer; IsHigher: Boolean;
			const Value: string ): Integer; virtual;
		function GetNearestValueIndex( ARow: Integer; IsHigher: Boolean;
			const Value: string ): Integer; virtual;

		function IndexOfPropName( const S: string ): Integer; virtual;

		property ColCount[ARow: Integer]: Integer
						 read GetStrCount;
		property Cols[ACol: Integer]: TKStrings
						 read GetCols write SetCols;
		property HighColCount: Integer
						 read GetHighColCount;
		property RowAsString[ARow: Integer]: string
						 read GetRowAsString write SetRowAsString;
		property RowCount: Integer
						 read GetCount;
		property Rows[ARow: Integer]: TKStrings
						 read GetRows write SetRows;

		property Strings[ARow, ACol: Integer]: string
						 read Get write Put; default;
		property Objects[ARow, ACol: Integer]: TObject
						 read GetObjects write PutObjects;
		property Names[ARow, ACol: Integer]: string
						 read GetNames;
		property Values[ARow: Integer; const Names: string]: string
						 read GetValues write SetValues;
		property ValuesByIndex[ARow, ACol: Integer]: string
						 read GetValuesByIndex write SetValuesByIndex;
		property Text[ARow: Integer]: string
						 read GetText write SetText;
		property CommaText[ARow: Integer]: string
						 read GetCommaText write SetCommaText;
		property Duplicates: TDuplicates
						 read FDuplicates write SetDuplicates	default dupIgnore;
		property FreeObjects: Boolean
						 read FFreeObjects;

		property Name: string
						 read FName write FName;
		property ColsAsRows: Boolean
						 read FColsAsRows write FColsAsRows default false;

		property AcceptNoOwnedNilAdd: Boolean
						 read FAcceptNoOwnedNilAdd write FAcceptNoOwnedNilAdd default false;

		property OnChanged: TNotifyEvent
						 read FOnChanged write FOnChanged;
		property OnChanging: TNotifyEvent
						 read FOnChanging write FOnChanging;

	end;

	TKCustomStringsArrayClass = class of TKCustomStringsArray;

{ TKStringsArray }

	TKStringsArray = class( TKCustomStringsArray )
	protected
		function GetStringsClass: TKStringsClass; override;

	public
		property RowCount;
		property ColCount;
		property HighColCount;
		property Rows;
		property Cols;
		property Strings;
		property Objects;
		property Names;
		property Values;
		property Text;
		property CommaText;
		property FreeObjects;
		property OnChanged;
		property OnChanging;
		property Duplicates;
		property Name;
		property ColsAsRows;
		property AcceptNoOwnedNilAdd;

	end;

{
--------------------------------------------------------------------------------
------------------------------- Pallete Object ---------------------------------
--------------------------------------------------------------------------------
}

const
	pcExplicit   = PC_EXPLICIT;
	pcNoCollapse = PC_NOCOLLAPSE;
	pcReserved   = PC_RESERVED;

type

	EKPalette = class( EKRClasses );

	PPEArray = ^TPEArray;
	TPEArray = array[0..255] of PPaletteEntry;

	TKPaletteUse = ( puNoStatic, puStatic, puError );

{ TKPaletteEntries } 

	TKPaletteEntries = class( TPersistent )
	private
		FEntries: TList;
    FChanged: Boolean;
    FLogPalette: PLogPalette; 
		FLogPaletteSize: Integer;
		FPaletteEntriesSize: Integer;
		FPaletteEntries: PPaletteEntry;

		function GetCount: Integer;
		function GetPalEntry( Index: Integer ): TPaletteEntry;
		procedure SetPalEntry( Index: Integer; Value: TPaletteEntry );

	public
		destructor Destroy; override;
		constructor Create; virtual;

		procedure Pack;
		procedure Clear;
		procedure Delete( Index: Integer );
		function Add( Value: Pointer ): Integer;

		procedure DisposeLogPalette;
		procedure DisposePaletteEntries;
		function AllocLogPalette: Pointer;
		function AllocPaletteEntries: Pointer;

		property PalEntry[Index: Integer]: TPaletteEntry
						 read GetPalEntry write SetPalEntry; default;
		property Count: Integer
						 read GetCount;

	end;

{ TKPalette }

	TKPalette = class( TPersistent )
	private
		FHandle: HPALETTE;
		FCanvas: TCanvas;
		FOldHandle: HPALETTE;
		FForceBackGround: Boolean;
		FEntries: TKPaletteEntries;

		function GetCount: Integer;
		function GetSysPaletteUse: TKPaletteUse;
		procedure SetSysPaletteUse( Value: TKPaletteUse );

	protected
		procedure FreeHandle; virtual;

	public
		destructor Destroy; override;
		constructor Create( ACanvas: TCanvas ); virtual;

		function ValidCanvas: Boolean;
		function ValidPalette: Boolean;

		function CreatePalette: Boolean;
		function RealizePalette: Boolean;
		function CreateHalftonePalette: Boolean;
		function CreateHalftonePaletteEx( ACanvas: TCanvas ): Boolean;
		function SelectPalette( ACanvas: TCanvas; AsBackGround: Boolean ): HPALETTE;
		function AnimatePalette( Index, Count: Integer; kpe: TKPaletteEntries ): Boolean;

		procedure DeletePalEntry( Index: Integer );
		function AddPalEntry( Value: TPaletteEntry ): Integer;
		function GetSystemPalEntries( Index, Count: Integer; kpe: TKPaletteEntries ): Boolean;

		function SetPalEntries( Index: Integer; kpe: TKPaletteEntries ): Boolean;
		function GetPalEntries( Index, Count: Integer; kpe: TKPaletteEntries ): Boolean;
		function SetPalEntry( pal: TKPalette; Index: Integer; pe: TPaletteEntry ): Boolean;
		function GetPalEntry( pal: TKPalette; Index: Integer; var pe: TPaletteEntry ): Boolean;

		function NearestColor( Color: TColor ): TColor;
		function NearestPaletteIndex( Color: TColor ): Integer;

		function GetEnhMetafilePalEntries( mf: TMetafile; kpe: TKPaletteEntries ): Boolean;

		property Canvas: TCanvas
						 read FCanvas write FCanvas;
		property Count: Integer
						 read GetCount;
		property ForceBackGround: Boolean
						 read FForceBackGround write FForceBackGround default false;
		property Handle: HPALETTE
						 read FHandle;
		property OldHandle: HPALETTE
						 read FOldHandle;
		property PaletteEntries: TKPaletteEntries
						 read FEntries write FEntries;
		property SysPaletteUse: TKPaletteUse
						 read GetSysPaletteUse write SetSysPaletteUse;

	end;

{
--------------------------------------------------------------------------------
-------------------------------- TKEnvironment ---------------------------------
--------------------------------------------------------------------------------
}

type

	EKEnvironment = class( EKRClasses );

{ TKEnvironment }

	TKEnvironment = class( TObject )
	private
		FParams: TStrings;

		function GetParamCount: Word;
		function GetValues( const Name: string ): string;
		procedure SetValues( const Name, Value: string );

	public
		constructor Create;
		destructor Destroy; override;

		procedure UpdateEnvironment;
		procedure LoadEnvironment( ss: TStrings );

		property ParamCount: Word
						 read GetParamCount;
		property Params: TStrings
						 read FParams;
		property Values[const Name: string]: string
						 read GetValues write SetValues; default;

	end;

{ TKServerEnvironment }

	TKServerEnvironment = class( TKEnvironment )
	private
		function GetGatewayInterface: string;
		function GetServerAdmin: string;
		function GetServerName: string;
		function GetServerSoftware: string;
		function GetAuthType: string;
		function GetContentLength: string;
		function GetContentType: string;
		function GetHTTPRequestMethod: string;
		function GetPath: string;
		function GetPathInfo: string;
		function GetPathTranslated: string;
		function GetQueryString: string;
		function GetRemoteAddr: string;
		function GetRemoteHost: string;
		function GetRemoteIdent: string;
		function GetRemoteUser: string;
		function GetScriptFileName: string;
		function GetScriptName: string;
		function GetServerPort: string;

{added on 16.07.1999}

		function GetURL: string;
		function GetHTTPReferer: string;
		function GetHTTPUserAgent: string;
		function GetHTTPAcceptLanguage: string;
		function GetLocalAddr: string;

	public
{ Server Info }
		property GatewayInterface: string
						 read GetGatewayInterface;
		property ServerAdmin: string
						 read GetServerAdmin;
		property ServerName: string
						 read GetServerName;
		property ServerSoftware: string
						 read GetServerSoftware;
						 
{ Other HTTP environment Info }
		property AuthType: string
						 read GetAuthType;
		property ContentLength: string
						 read GetContentLength;
		property ContentType: string
						 read GetContentType;
		property HTTPRequestMethod: string
						 read GetHTTPRequestMethod;
		property Path: string
						 read GetPath;
		property PathInfo: string
						 read GetPathInfo;
		property PathTranslated: string
						 read GetPathTranslated;
		property QueryString: string
						 read GetQueryString;
		property RemoteAddr: string
						 read GetRemoteAddr;
		property RemoteHost: string
						 read GetRemoteHost;
		property RemoteIdent: string
						 read GetRemoteIdent;
		property RemoteUser: string
						 read GetRemoteUser;
		property ScriptFileName: string
						 read GetScriptFileName;
		property ScriptName: string
						 read GetScriptName;
		property ServerPort: string
						 read GetServerPort;

{added on 16.07.1999}

		property URL: string
						 read GetURL;
		property HTTPReferer: string
						 read GetHTTPReferer;
		property HTTPUserAgent: string
						 read GetHTTPUserAgent;
		property HTTPAcceptLanguage: string
						 read GetHTTPAcceptLanguage;
		property LocalAddr: string
						 read GetLocalAddr;

	end;

{
--------------------------------------------------------------------------------
-------------------------------- Hook Control ----------------------------------
--------------------------------------------------------------------------------
}

	EKHook = class( EKRClasses );

	PKHookInfo = ^TKHookInfo;
	TKHookInfo = record
		Handle: THandle;
		Process: Boolean;
		IsHookBefore: Boolean;
	end;

	TKHookMethod = procedure( var Message: TMessage; var HookInfo: TKHookInfo ) of object;

	PHookData = ^THookData;
	THookData = record
		Handle: THandle;
		HookBeforeProc: TKHookMethod;
		HookAfterProc: TKHookMethod;
	end;

function HookWindow( HookData: THookData ): Boolean;
function UnHookWindow( HookData: THookData ): Boolean;
procedure CallWndProc( AHandle: THandle; var Message: TMessage );

function MakeHookData( AHandle: THandle; HookBefore, HookAfter: TKHookMethod ): THookData;

{
--------------------------------------------------------------------------------
------------------------------ PropList Support --------------------------------
--------------------------------------------------------------------------------
}

type

	EKPropList = class( EKRClasses );

	PKPropInfo = ^TKPropInfo;
	TKPropInfo = record
		PropName: ShortString;
		PropTypeKind: TTypeKind;
		PropValue: Pointer;
	end;

{ TKPropList }

	TKPropList = class( TList )
	private
		FStrings: TStrings;

		function GetPropNamesList: TStrings;
		function GetPropNames( Index: Integer ): ShortString;

	protected
		procedure DestroyPropInfo( Index: Integer ); dynamic;
		function InternalAdd( const PropName: ShortString; PropTypeKind: TTypeKind;
			const PropValue ): PKPropInfo; dynamic;
{
	Subscribed Static Protected Methods as Virtual. These methods provide
	access methods for the Items property. Get returns the PropValue
	pointer instead of the PKPropInfo pointer. Put raises an exception
	because (in this class) the Items property is read-only. We	could
	subscribe the property, but it will result in a similar situation.
}
		function Get( Index: Integer ): Pointer; virtual;
		procedure Put( Index: Integer; Item: Pointer ); virtual;

	public
		constructor Create; virtual;
		destructor Destroy; override;

		function AddSet( const PropName: ShortString; const SetValue ): Integer; virtual;
		function AddClass( const PropName: ShortString; ClassValue: TObject ): Integer; virtual;
		function AddOrdinal( const PropName: ShortString; OrdValue: LongInt ): Integer; virtual;
		function AddString( const PropName: ShortString; const StrValue: string ): Integer; virtual;
		function AddFloat( const PropName: ShortString; FltValue: Extended ): Integer; virtual;
		function AddVariant( const PropName: ShortString; VarValue: Variant ): Integer; virtual;
		function AddMethod( const PropName: ShortString; MthValue: TMethod ): Integer; virtual;

{Subscribed Public Methods as Virtual}
		function Add( Item: PKPropInfo ): Integer; virtual;
		procedure Clear; {$IFDEF DELPHI4}override{$ELSE}dynamic{$ENDIF};
		procedure Delete( Index: Integer ); virtual;

		property PropNamesList: TStrings
						 read GetPropNamesList;
		property PropNames[Index: Integer]: ShortString
						 read GetPropNames;

	end;

function SetPubPropsEx( Obj: TObject; PropValueList: TKPropList;
	ProcessChildren: Boolean ): Boolean;

{##FNS##}
{
--------------------------------------------------------------------------------
---------------------------- Caption Icon Support ------------------------------
--------------------------------------------------------------------------------
}

type

	EKCaption = class( EKRClasses );
	EKSystemIcon = class( EKCaption );

{ TKCustomSystemIcon }

	TKCustomSystemIcon = class( TPersistent )
	private
		FRect: TRect;
		FICon: TIcon;
		FOwner: TKWindowInfo;
		FSystemMenu: TPopupMenu;

	protected
		function CaptionRect: TRect;

		property Owner: TKWindowInfo
						 read FOwner;
		property Rect: TRect
						 read FRect;
		property Icon: TIcon
						 read FIcon write FIcon;
		property SystemMenu: TPopupMenu
						 read FSystemMenu write FSystemMenu;

	public
		constructor Create( AOwner: TKWindowInfo );

		procedure Assign( Source: TPersistent ); override;
		procedure SysMenuPopup; virtual;
		procedure Draw( dc: HDC; var rt: TRect ); virtual;
		function ButtonAtPoint( pt: TPoint ): Boolean; dynamic;

	end;

{ TKSystemIcon }

	TKSystemIcon = class( TKCustomSystemIcon )
	public
		property Owner: TKWindowInfo
						 read FOwner;
		property Rect: TRect
						 read FRect;

	published
		property Icon: TIcon
						 read FIcon write FIcon;
		property SystemMenu: TPopupMenu
						 read FSystemMenu write FSystemMenu;

	end;

{
--------------------------------------------------------------------------------
---------------------------- Caption Button Support ----------------------------
--------------------------------------------------------------------------------
}

	EKCaptionButton = class( EKCaption );
	EKCaptionButtons = class( EKCaption );

	TKCaptionButton = class;

	TKCaptionButtonKind = ( cbkClose, cbkMaximize, cbkMinimize, cbkHelp, cbkRollup,
		cbkCustom );
	TKCaptionButtonKinds = set of TKCaptionButtonKind;

	TKDrawCaptionButtonEvent = procedure( Sender: TKCaptionButton; dc: HDC; rt: TRect )
	  of object;

{ TCaptionButton }

	TKCaptionButton = class( TCollectionItem )
	private
		FRect: TRect;
		FPushed: Boolean;
		FEnabled: Boolean;
		FVisible: Boolean;
		FDrawPushed: Boolean;
		FOnClick: TNotifyEvent;
		FOnChange: TNotifyEvent;
		FKind: TKCaptionButtonKind;
		FOnCustomButtonDraw: TKDrawCaptionButtonEvent;

		FInternalState: Byte;
		FWindowInfo: TKWindowInfo;

		function GetBorderStyle: TFormBorderStyle;
		function GetWindowState: TWindowState;

		procedure SetPushed( Value: Boolean );
		procedure SetEnabled( Value: Boolean );
		procedure SetVisible( Value: Boolean );
		procedure SetDrawPushed( Value: Boolean );
		procedure SetKind( Value: TKCaptionButtonKind );

	protected
		procedure DoClick; virtual;
		procedure Changed( AllItems: Boolean ); virtual;
		function Draw( Handle: HWnd; dc: HDC; var rt: TRect ): Boolean; virtual;

		property BorderStyle: TFormBorderStyle
						 read GetBorderStyle;
		property WindowInfo: TKWindowInfo
						 read FWindowInfo;
		property WindowState: TWindowState
						 read GetWindowState;

	public
		constructor Create( Collection: TCollection ); override;

		procedure Click; dynamic;
		procedure Assign( Source: TPersistent ); override;

		property DrawPushed: Boolean
						 read FDrawPushed write SetDrawPushed default false;
		property InternalState: Byte
						 read FInternalState write FInternalState default KCBS_DEFAULT;
		property Pushed: Boolean
						 read FPushed write SetPushed default false;
		property Rect: TRect
						 read FRect;

		property OnClick: TNotifyEvent
						 read FOnClick write FOnClick;
		property OnCustomButtonDraw: TKDrawCaptionButtonEvent
						 read FOnCustomButtonDraw write FOnCustomButtonDraw;

	published
		property Enabled: Boolean
						 read FEnabled write SetEnabled default true;
		property Kind: TKCaptionButtonKind
						 read FKind write SetKind default cbkClose;
		property Visible: Boolean
						 read FVisible write SetVisible default true;

		property OnChange: TNotifyEvent
						 read FOnChange write FOnChange;

	end;

{ TKCaptionButtons }

	TKCaptionButtons = class( TCollection )
	private
		FOwner: TKWindowInfo;

		function GetItemPushed: TKCaptionButton;
		function GetItem( Index: Integer ): TKCaptionButton;
		procedure SetItem( Index: Integer; Value: TKCaptionButton );
		function GetButton( Index: TKCaptionButtonKind ): TKCaptionButton;

	protected
		function CaptionRect: TRect;
		function FindButton( pt: TPoint ): TKCaptionButton;

	public
		constructor Create( AOwner: TKWindowInfo ); virtual;

		function LeftMostButton: Integer;
		function AnyButtonPushed: Boolean;
		function IsPushed( pt: TPoint ): Boolean;
		procedure Draw( dc: HDC; var rt: TRect ); virtual;
		function KindExists( bk: TKCaptionButtonKind ): Boolean;
		function Add( Kind: TKCaptionButtonKind ): TKCaptionButton;
		function ButtonAtPoint( pt: TPoint; var cbn: TKCaptionButton ): Boolean;

		property Owner: TKWindowInfo
						 read FOwner;
		property Buttons[Index: TKCaptionButtonKind]: TKCaptionButton
						 read GetButton; default;
		property Items[Index: Integer]: TKCaptionButton
						 read GetItem write SetItem;
		property ItemPushed: TKCaptionButton
						 read GetItemPushed;

	end;

{
--------------------------------------------------------------------------------
-------------------------------- Form Painting ---------------------------------
--------------------------------------------------------------------------------
}

	TKMouseButton = ( kmbNone, kmbLeft, kmbRight );

{ This class was meant for runtime support only!!! }

{ TKCustomFormPainter }
	TKCustomFormPainter = class( TPersistent )
	private
		FHandle: HWnd;
		FCaption: string;

		FActiveFont: TFont;
		FInactiveFont: TFont;
		FBackGround: TKGradient;
		FActiveCaption: TKGradient;
		FInactiveCaption: TKGradient;

		FSystemIcon: TKSystemIcon;
		FButtons: TKCaptionButtons;
		FWindowManager: TKWindowManager;

		FOldFormWndProc: TFarProc;
		FNewFormWndProc: TFarProc;
		FOldClientWndProc: TFarProc;
		FNewClientWndProc: TFarProc;

		FPaintActive: Boolean;
		FUsePaintActive: Boolean;

		FMouseCaptured: Boolean;
		FHelpButtonDown: Boolean;
		FMouseButtonDown: TKMouseButton;

		FOldHeight: Integer;
		FMaximizing: Boolean;
		FOldSizeable: Boolean;

    FOnCloseQuery: TCloseQueryEvent;

		function GetCaptionFont: TFont;
		function GetCaptionGradient: TKGradient;

		function GetCaption: string;
		procedure SetCaption( const Value: string );

	protected
		procedure ControlDispatch( var Message: TMessage );

		procedure NCPaint( var Msg: TMessage ); virtual;

		procedure DoRollupAction( ra: LongInt );
		procedure DoSystemAction( ws: TWindowState );

		procedure DoClose; dynamic;
		procedure DoHelpLeftButton; virtual;
		procedure DoHelpRightButton; virtual;
		procedure DoSystemMenuClick; virtual;
		procedure DoSystemMenuDblClick; virtual;
		procedure DoRightButtonClick( pt: TPoint ); virtual;
		procedure DoHelpButtonPush( cbn: TKCaptionButton ); virtual;
		procedure DoCaptionButtonClick( cbn: TKCaptionButton ); virtual;

		function GetClientHandle: HWnd; dynamic;
		procedure FormWndProc( var Message: TMessage ); dynamic;
		procedure FormWndProcException( E: Exception ); virtual;
		procedure FormClientWndProc( var Message: TMessage ); dynamic;

		function ClientToScreen( pt: TPoint ): TPoint;
		function ScreenToClient( pt: TPoint ): TPoint;

		procedure Invalidate; dynamic;
		procedure NCInvalidate; dynamic;
		procedure PaintCaption; dynamic;
		function PaintCaptionDC( dc: HDC ): TRect; dynamic;

		property MouseCaptured: Boolean
						 read FMouseCaptured write FMouseCaptured;
		property MouseButtonDown: TKMouseButton
						 read FMouseButtonDown write FMouseButtonDown;

		property ActiveCaption: TKGradient
						 read FActiveCaption write FActiveCaption;
		property ActiveFont: TFont
						 read FActiveFont write FActiveFont;
		property BackGround: TKGradient
						 read FBackGround write FBackGround;
		property Buttons: TKCaptionButtons
						 read FButtons;
		property Caption: string
						 read GetCaption write SetCaption;
		property CaptionFont: TFont
						 read GetCaptionFont;
		property CaptionGradient: TKGradient
						 read GetCaptionGradient;
		property Handle: HWnd
						 read FHandle;
		property InactiveCaption: TKGradient
						 read FInactiveCaption write FInactiveCaption;
		property InactiveFont: TFont
						 read FInactiveFont write FInactiveFont;
		property SystemIcon: TKSystemIcon
						 read FSystemIcon;
		property WindowManager: TKWindowManager
						 read FWindowManager;

		property OnCloseQuery: TCloseQueryEvent
						 read FOnCloseQuery write FOnCloseQuery;

	public
		destructor Destroy; override;
		constructor Create( AHandle: HWnd ); virtual;

	end;

  TKCustomFormPainterClass = class of TKCustomFormPainter;

	TKFormPainter = class( TKCustomFormPainter )
	public
		property Handle;
		property MouseCaptured;
		property MouseButtonDown;

		property ActiveCaption;
		property ActiveFont;
		property BackGround;
		property Buttons;
		property Caption;
		property CaptionFont;
		property CaptionGradient;
		property InactiveCaption;
		property InactiveFont;
		property SystemIcon;
		property WindowManager;

		property OnCloseQuery;

	end;

	TKFormPainterClass = class of TKFormPainter;

{
--------------------------------------------------------------------------------
------------------------- Generic Dialog Architecture --------------------------
--------------------------------------------------------------------------------
}

{	TKKernelBaseDialog }

	TKKernelBaseDialog = class( TKCustomLinkable )
	public
		constructor Create( AOwner: TComponent ); override;

	end;

	TKKernelBaseDialogClass = class of TKKernelBaseDialog;

{##FNS##}

{
--------------------------------------------------------------------------------
------------------------- Generic Parser Architecture --------------------------
--------------------------------------------------------------------------------
}

	EKParser = class( EKRClasses );
	EKParserClass = class of EKParser;

	TKTokenType = type char;

	TKCustomParser = class;

	TKParserCharSets = array[LOW_CHARSET_IDX..HIGH_CHARSET_IDX] of TKCharSet;

	TKRelOp = ( roNone, roEqual, roLessThan, roGreaterThan, roLessEqual, roGreaterEqual,
		roDifferent );

{ TKParsingFrame }

  TKParsingFrame = class( TObject )
  private
    FBuffer: PChar;
    FBufPtr: Cardinal;
    FBufEnd: Cardinal;
    FSourcePtr: Cardinal;
    FSourceEnd: Cardinal;
    FStringPtr: Cardinal;
    FStreamPos: LongInt;
    FSaveChar: Char;
    FOrigin: LongInt;
    FRelOp: TKRelOp;
		FToken: TKTokenType;
    FSourceLine: Integer;
    FBufferSize: Integer;
    FParser: TKCustomParser;

  protected
    property Parser: TKCustomParser
             read FParser;
             
  public
    destructor Destroy; override;
    constructor Create( AParser: TKCustomParser ); virtual;
    procedure RestoreFrame; virtual;

	end;

  TKParsingFrameClass = class of TKParsingFrame;

{ TKParsingStack }

	TKParsingStack = class( TObject )
	private
		FList: TList;
		FDestroy: Boolean;
		FParser: TKCustomParser;

    function GetCount: Integer;

	protected
		property Parser: TKCustomParser
						 read FParser;

	public
		destructor Destroy; override;
		constructor Create( AParser: TKCustomParser );

		function Push: Integer;
		function Pop: Integer;
    
    property Count: Integer
             read GetCount;

	end;

	{ TKCustomParser }

	TKCustomParser = class( TObject )
	private
		FBuffer: PChar;
		FBufPtr: PChar;
		FBufEnd: PChar;
		FSaveChar: Char;
		FStream: TStream;
		FOrigin: Longint;
		FRelOp: TKRelOp;
		FTokenPtr: PChar;
		FSourcePtr: PChar;
		FSourceEnd: PChar;
		FStringPtr: PChar;
		FToken: TKTokenType;
		FSourceLine: Integer;
		FStack: TKParsingStack;
		FErrorClass: EKParserClass;
		FCharSets: TKParserCharSets;

	protected
		procedure DoReadBuffer; virtual;

		function CheckSymbolToken( P: PChar ): Boolean; virtual;
		function CheckStringToken( P: PChar ): Boolean; virtual;
		function CheckNumberToken( P: PChar ): Boolean; virtual;
		function CheckHexaToken( P: PChar ): Boolean; virtual;
		function CheckRelOpToken( P: PChar ): Boolean; virtual;
		function CheckSpecialToken( P: PChar ): Boolean; virtual;
		function CheckCommentToken( P: PChar ): Boolean; virtual;
		function CheckBlankToken( P: PChar ): Boolean; virtual;

		function GetSymbolCharSet: TKCharSet; virtual;
		function GetStringCharSet: TKCharSet; virtual;
		function GetHexaCharSet: TKCharSet; virtual;
		function GetIntegerCharSet: TKCharSet; virtual;
		function GetFloatCharSet: TKCharSet; virtual;
		function GetCommentCharSet: TKCharSet; virtual;
		function GetRelOpCharSet: TKCharSet; virtual;
		function GetSpecialCharSet: TKCharSet; virtual;
		function GetBlankCharSet: TKCharSet; virtual;

		procedure ProcessBlank; dynamic;
		function ProcessSymbol( var P: PChar ): TKTokenType; virtual;
		function ProcessString( var P: PChar ): TKTokenType; virtual;
		function ProcessHexa( var P: PChar ): TKTokenType; virtual;
		function ProcessNumber( var P: PChar ): TKTokenType; virtual;
		function ProcessComment( var P: PChar ): TKTokenType; virtual;
		function ProcessRelOpToken( var P: PChar ): TKTokenType; virtual;
		function ProcessSpecialToken( var P: PChar ): TKTokenType; virtual;
		function ProcessGarbage( var P: PChar ): TKTokenType; virtual;

		function InternalNextToken( var P: PChar ): TKTokenType; virtual;

		class function GetDefaulBufferSize: Cardinal; dynamic;
		function GetDefaultErrorClass: EKParserClass; virtual;
    function GetParsingFrameClass: TKParsingFrameClass; virtual;

    procedure IncSourceLine;
    procedure SetToken( AToken: TKTokenType );
    
		property Buffer: PChar
						 read FBuffer;
		property Stream: TStream
						 read FStream;
		property CharSets: TKParserCharSets
						 read FCharSets write FCharSets;
		property SourcePtr: PChar
						 read FSourcePtr write FSourcePtr;
		property TokenPtr: PChar
						 read FTokenPtr write FTokenPtr;
		property StringPtr: PChar
						 read FStringPtr write FStringPtr;
		property RelOp: TKRelOp
						 read FRelOp write FRelOp;

	public
		destructor Destroy; override;
		constructor Create( AStream: TStream; StartScanning: Boolean ); 

		procedure Error( const Ident: string ); dynamic;
		procedure ErrorFmt( const Ident: string; const Args: array of const ); dynamic;

		function CheckToken( AToken: TKTokenType ): Boolean; virtual;
		function CheckAnyToken( const Tokens: array of TKTokenType ): Boolean; virtual;
		function CheckString( const AString: string ): Boolean; virtual;
		function CheckSymbol( const Symbol: string ): Boolean; virtual;
		function CheckRelOp( ARelOp: TKRelOp ): Boolean; virtual;
		function CheckNumber( const Number: string ): Boolean; virtual;

		procedure ForceToken( AToken: TKTokenType ); virtual;
		procedure ForceString( const AString: string ); virtual;
		procedure ForceSymbol( const Symbol: string ); virtual;
		procedure ForceRelOp( ARelOp: TKRelOp ); virtual;
		procedure ForceNumber( const Number: string ); virtual;

		function NextToken: TKTokenType; virtual;

{ Actually these methods just fill the structure, does not implement buffer rearragement }
		procedure SaveToFrame( var ppf: TKParsingFrame ); virtual;
		procedure RestoreFromFrame( ppf: TKParsingFrame ); virtual;

		function SourcePos: Longint;
		function TokenFloat: Extended;
		function TokenInt: Longint;
		function TokenHexa: Longint;
		function TokenString: string;

		property SourceLine: Integer
						 read FSourceLine;
		property Token: TKTokenType
						 read FToken;
		property Stack: TKParsingStack
						 read FStack write FStack;

	end;

	TKCustomParserClass = class of TKCustomParser;

{ TKCodeParsingFrame }

  TKCodeParsingFrame = class( TKParsingFrame )
  private
		FLine: Integer;
		FPosition: Integer;
    FTargetPos: Integer;
		FKeyWordsText: string;
		FOutPutString: string;
  public
    constructor Create( AParser: TKCustomParser ); override;
    procedure RestoreFrame; override;

  end;

{ TKCustomCodeParser }

	EKCodeParser = class( EKParser );

	TKCustomCodeParser = class;

	TKCodeEvent = procedure( Sender: TKCustomCodeParser; var Source: string ) of object;
	TKBeforeCodeEvent = procedure( Sender: TKCustomCodeParser; var Source: string; const Token: string ) of object;

	TKCustomCodeParser = class( TKCustomParser )
	private
		FTarget: TStream;
		FLine: Integer;
		FPosition: Integer;
		FKeyWords: TStrings;
    FFileName: string;  
		FOutPutString: string;
		FOnBuildFileHeader: TKCodeEvent;
		FOnBuildFileFooter: TKCodeEvent;
		FBeforeString: TKBeforeCodeEvent;
		FAfterString: TKCodeEvent;
		FBeforeKeyword: TKBeforeCodeEvent;
		FAfterKeyword: TKCodeEvent;
		FBeforeSymbol: TKBeforeCodeEvent;
		FAfterSymbol: TKCodeEvent;
		FBeforeComment: TKBeforeCodeEvent;
		FAfterComment: TKCodeEvent;
		FBeforeInteger: TKBeforeCodeEvent;
		FAfterInteger: TKCodeEvent;
		FBeforeHexa: TKBeforeCodeEvent;
		FAfterHexa: TKCodeEvent;
		FBeforeFloat: TKBeforeCodeEvent;
		FAfterFloat: TKCodeEvent;

		FOnCheckString: TKCodeEvent;
		FOnCheckComment: TKCodeEvent;
		FOnUnknownToken: TKCodeEvent;

		FOnEndOfStream: TNotifyEvent;
		FOnReadBuffer: TNotifyEvent;

	protected
		procedure DoReadBuffer; override;

		procedure DoBuildHeader; virtual;
		procedure DoBuildFooter; virtual;
		procedure DoEndOfStream; virtual;
		procedure DoBeforeString( const Str: string ); virtual;
		procedure DoAfterString; virtual;
		procedure DoBeforeKeyword( const KeyWord: string ); virtual;
		procedure DoAfterKeyword; virtual;
		procedure DoBeforeSymbol( const Symbol: string ); virtual;
		procedure DoAfterSymbol; virtual;
		procedure DoBeforeComment( const Comment: string ); virtual;
		procedure DoAfterComment; virtual;
		procedure DoBeforeNumber( AType: TKTokenType; const Number: string ); virtual;
		procedure DoAfterNumber( AType: TKTokenType ); virtual;
		procedure DoCheckString( var s: string ); virtual;
		procedure DoCheckComment( var s: string ); virtual;
		procedure DoUnknownToken; virtual;

		function ValidateSpecialToken( Ch: Char ): string; virtual;
		function MakeStringLegal( const s: string ): string; virtual;
		function MakeCommentLegal( const s: string ): string; virtual;

		function GetTargetFileName: string; virtual;
		function GetDefaultKeyWords: string; virtual;
		class function GetAutoScanning: Boolean; virtual;

		function GetDefaultErrorClass: EKParserClass; override;
    function GetParsingFrameClass: TKParsingFrameClass; override;

		procedure ProcessConversion( const s: string ); virtual;

		property OutPutString: string
						 read FOutPutString write FOutPutString;
		property Line: Integer
						 read FLine write FLine;
		property Position: Integer
						 read FPosition write FPosition;
		property KeyWords: TStrings
						 read FKeyWords;
		property FileName: string
						 read FFileName;
		property Target: TStream
						 read FTarget;

		property OnBuildFileHeader: TKCodeEvent
						 read FOnBuildFileHeader write FOnBuildFileHeader;
		property OnBuildFileFooter: TKCodeEvent
						 read FOnBuildFileFooter write FOnBuildFileFooter;
		property BeforeString: TKBeforeCodeEvent
						 read FBeforeString write FBeforeString;
		property AfterString: TKCodeEvent
						 read FAfterString write FAfterString;
		property BeforeKeyword: TKBeforeCodeEvent
						 read FBeforeKeyword write FBeforeKeyword;
		property AfterKeyword: TKCodeEvent
						 read FAfterKeyword write FAfterKeyword;
		property BeforeSymbol: TKBeforeCodeEvent
						 read FBeforeSymbol write FBeforeSymbol;
		property AfterSymbol: TKCodeEvent
						 read FAfterSymbol write FAfterSymbol;
		property BeforeComment: TKBeforeCodeEvent
						 read FBeforeComment write FBeforeComment;
		property AfterComment: TKCodeEvent
						 read FAfterComment write FAfterComment;
		property BeforeInteger: TKBeforeCodeEvent
						 read FBeforeInteger write FBeforeInteger;
		property AfterInteger: TKCodeEvent
						 read FAfterInteger write FAfterInteger;
		property BeforeHexa: TKBeforeCodeEvent
						 read FBeforeHexa write FBeforeHexa;
		property AfterHexa: TKCodeEvent
						 read FAfterHexa write FAfterHexa;
		property BeforeFloat: TKBeforeCodeEvent
						 read FBeforeFloat write FBeforeFloat;
		property AfterFloat: TKCodeEvent
						 read FAfterFloat write FAfterFloat;
		property OnCheckString: TKCodeEvent
						 read FOnCheckString write FOnCheckString;
		property OnCheckComment: TKCodeEvent
						 read FOnCheckComment write FOnCheckComment;
		property OnUnknownToken: TKCodeEvent
						 read FOnUnknownToken write FOnUnknownToken;
		property OnEndOfStream: TNotifyEvent
						 read FOnEndOfStream write FOnEndOfStream;
		property OnReadBuffer: TNotifyEvent
						 read FOnReadBuffer write FOnReadBuffer;

	public
		destructor Destroy; override;
    constructor CreateFromFile( const AFileName: string ); virtual;
		constructor Create( ASource, ATarget: TStream; const AKeyWords: string );
		constructor CreateKeyWords( ASource, ATarget: TStream; AKeyWords: TStrings );

		procedure Convert; virtual;

	end;

	TKCustomCodeParserClass = class of TKCustomCodeParser;

const

	tpEOF     = TKTokenType( 0 ); { Null Terminator }
	tpSymbol  = TKTokenType( 1 ); { Source identifier tokens (i.e. user words - variable names) }
	tpString  = TKTokenType( 2 ); { Any tokens between StrToken (i.e. '????') }
	tpInteger = TKTokenType( 3 ); { Integer numbers }
	tpHexa	  = TKTokenType( 4 ); { Hexa numbers (i.e. numbers started with hex token - $ or 0x) }
	tpFloat   = TKTokenType( 5 ); { Float numbers }
	tpComment = TKTokenType( 6 ); { Any kind of Comments }

	tpRelOp   = TKTokenType( 8 ); { mathmatical relational operators- pascal parser }
	tpSpecial = TKTokenType( 9 ); { any special (user defined) token }

{
--------------------------------------------------------------------------------
-------------------------- Generic Lexer Architecture --------------------------
--------------------------------------------------------------------------------
}

type

	EKLexer = class( EKRClasses );
	EKLexerClass = class of EKLexer;

	{ TKCustomLexer }

	TKCustomLexer = class( TObject )
	private
		FLexing: Boolean;
		FSource: TStream;
		FTarget: TStream;
		FOnLexer: TNotifyEvent;
    FOnReset: TNotifyEvent;
		FParser: TKCustomParser;
		FErrorClass: EKLexerClass;
		FValidTokenTypes: TKCharSet;

	protected
		function GetParser: TKCustomParser;
		function GetDefaultErrorClass: EKLexerClass; virtual;

		procedure Goal; virtual; abstract;
		procedure CreateParser; virtual; abstract;

		procedure MatchToken( AToken: TKTokenType ); virtual;
		procedure MatchAnyToken( const Tokens: array of TKTokenType ); virtual;
		procedure MatchSymbol( const Symbol: string ); virtual;
		procedure MatchNumber( AType: TKTokenType ); virtual;
		procedure MatchString( const AString: string ); virtual;

		function DetectEOF: Boolean; virtual;
		function GetValidTokenTypes: TKCharSet; virtual;

		procedure DoLexer; dynamic;
		procedure DoReset; dynamic;

		property Parser: TKCustomParser
						 read GetParser write FParser;
		property Source: TStream
						 read FSource write FSource;
		property Target: TStream
						 read FTarget write FTarget;
		property ValidTokenTypes: TKCharSet
						 read FValidTokenTypes;

	public
		destructor Destroy; override;
		constructor Create( ASource, ATarget: TStream ); virtual;

		procedure Lexer; virtual;
		procedure Reset; virtual;

		procedure Error( const Ident: string ); dynamic;
		procedure ErrorFmt( const Ident: string; const Args: array of const ); dynamic;

		property Lexing: Boolean
						 read FLexing;
		property OnLexer: TNotifyEvent
						 read FOnLexer write FOnLexer;
		property OnReset: TNotifyEvent
						 read FOnReset write FOnReset;

	end;

{
--------------------------------------------------------------------------------
-------------------------------- List of Variants ------------------------------
--------------------------------------------------------------------------------
}

	{ TKVariantList }

	TKVariantList = class( TObject )
	private
		FItems: TStringList;

		function GetItemsByID( Index: Integer ): OleVariant;
		procedure SetItemsByID( Index: Integer; Value: OleVariant );
		function GetItems( Index: OleVariant ): OleVariant;
		procedure SetItems( Index: OleVariant; Value: OleVariant );
		function GetVarCount: LongInt;

	protected
		function VarStr( Value: OleVariant ): string; dynamic;
		function AllowedVariantIndexType( Value: OleVariant ): Boolean; dynamic;

	public
		destructor Destroy; override;
		constructor Create;

		procedure Clear; virtual;
		function Dump: OleVariant; virtual;

		property ItemsByID[Index: Integer]: OleVariant
						 read GetItemsByID write SetItemsByID; 
		property Items[Index: OleVariant]: OleVariant
						 read GetItems write SetItems; default;
		property VarCount: LongInt
						 read GetVarCount;

	end;

(*
	 não devia estar aqui... essa implementação tira a flexibilidade do
	 programador em definir o gerenciamento de sessão com o advento de
	 novas tecnologias/novas formas de iteração com o ASP framework; joga
	 essa implementação para o pacote COM!!!

{ TKSessionInfo	}

	TKSessionInfo = class( TKVariantList )
	private
		FID: LongInt;
		FSeqNo: LongInt;
		FServerEnv: TKServerEnvironment;

		function GetServerVars( Index: OleVariant ): OleVariant;
		function GetTickCount: LongInt;

	public
		destructor Destroy; override;
		constructor Create;

		function NewSeqNo: LongInt; virtual;
		function DumpServerEnv: OleVariant; virtual;

		property ID: LongInt
						 read FID;
		property SeqNo: LongInt
						 read FSeqNo;
		property ServerVars[Index: OleVariant]: OleVariant
						 read GetServerVars;
		property TickCount: LongInt
						 read GetTickCount;

	end;
*)

implementation

uses
	Consts, uksyConsts, uksyPackReg, ukrResStr, ukrUtils;

{
--------------------------------------------------------------------------------
------------------ Generic Linkable Components Architecture --------------------
--------------------------------------------------------------------------------
}

{
	« This architecture was closely based on Delphi's dataset and
		datasource link architecture. »

	TYPE DESCRIPTION
	----------------

	.TLinkEvent:
		 Allows maximum flexibility while dealing with events.
		 Create your own events and deal with them in the proper
		 situation. No enumeration (please), otherwise the architecture
		 would NOT be flexible as we've made it.

	.TCustomLink:
		 This class is the base for the link mechanism; it will
		 be responsible for processing all custom events for the
		 TCustomLinkable class, once it has been notified about
		 these events. This classe's main concern is synchronization.
		 To use this class, derive it and override the DoLinkEvent
		 method, or use it as-is, giving it a proper TCustomLinkable
		 owner.

	.TCustomLinkable:
		Base component class for the link mechanism. This is where
		action takes place. Classes that want to allow linking and
		notification must derive from this one. Just call notify
		link with a custom (user defined) link event and an optional
		Data. This method will notify every link (linked to this
		component) of each link event.

}

{-------------------------------- TKCustomLink ---------------------------------}

constructor TKCustomLink.Create( AOwner: TKCustomLinkable );
begin
	inherited Create;
	if CheckObject( AOwner ) then
		SetLinkable( AOwner );
end;

constructor TKCustomLink.CreateLinked( AOwner: TKCustomLinkable;
	AOwnerClass: TKCustomLinkableClass );
begin
	ForceObjectClass( AOwner, AOwnerClass );
	Create( AOwner );
end;

destructor TKCustomLink.Destroy;
begin
	if CheckObject( FOwner ) then
		FOwner.RemoveLink( Self ); { Hard Couple! }
	inherited Destroy;
end;

procedure TKCustomLink.DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt );
begin
	if Assigned( FLinkNotification ) then
		FLinkNotification( Self, LinkEvent, Data );
end;

procedure TKCustomLink.SetLinkAble( Value: TKCustomLinkable );
begin
	if ( FOwner <> Value ) then
	begin
		if CheckObject( FOwner ) then
			FOwner.RemoveLink( Self ); { Hard Couple! }
		if CheckObject( Value ) then
			Value.AddLink( Self );     { Hard Couple! }
	end;
end;

{------------------------------ TKCustomLinkable -------------------------------}

destructor TKCustomLinkable.Destroy;
begin
	FreeClean( FLinkList );
	inherited Destroy;
end;

function TKCustomLinkable.GetInstanceCount( Index: TComponentClass ): Integer;
var
	i: Integer;
begin
	if CheckClass( Index ) then
		Result := 0
	else
	begin
		Result := -1;
		Exit;
	end;
	for i := 0 to ComponentCount - 1 do
		Inc( Result, Ord( CheckObjectClass( Components[i], Index ) ) );
end;

procedure TKCustomLinkable.ForEachControlDo( AMethod: TKControlMethod );
var
	i: Integer;
begin
	if ( not CheckObjectClass( Self, TWinControl ) ) then
		Exit;
	ForceReference( AMethod );
	for i := 0 to TWinControl( Self ).ControlCount - 1 do
		AMethod( TWinControl( Self ).Controls[i] );
end;

procedure TKCustomLinkable.ForEachComponentDo( AMethod: TKComponentMethod );
var
	i: Integer;
begin
	ForceReference( AMethod );
	for i := 0 to ComponentCount - 1 do
		AMethod( Components[i] );
end;

procedure TKCustomLinkable.ForEachControlClassDo( AClass: TControlClass; AMethod: TKControlMethod );
var
	i: Integer;
begin
	if ( not CheckObjectClass( Self, TWinControl ) ) then
		Exit;
	ForceReference( AMethod );
	if ( not CheckClass( AClass ) ) then
		AClass := TControl;
	if ( AClass = TControl ) then
		ForEachControlDo( AMethod )
	else
		for i := 0 to TWinControl( Self ).ControlCount - 1 do
			if CheckObjectClass( TWinControl( Self ).Controls[i], AClass ) then
				AMethod( TWinControl( Self ).Controls[i] );
end;

procedure TKCustomLinkable.ForEachComponentClassDo( AClass: TComponentClass; AMethod: TKComponentMethod );
var
	i: Integer;
begin
	ForceReference( AMethod );
	if ( not CheckClass( AClass ) ) then
		AClass := TComponent;
	if ( AClass = TComponent ) then
		ForEachComponentDo( AMethod )
	else
		for i := 0 to ComponentCount - 1 do
			if CheckObjectClass( Components[i], AClass ) then
				AMethod( Components[i] );
end;

procedure TKCustomLinkable.GetInstanceList( AList: TList; AClass: TComponentClass );
var
	i: Integer;
begin
	ForceObject( AList );
	ForceClass( AClass );
	AList.Clear;
	for i := 0 to ComponentCount - 1 do
		if CheckObjectClass( Components[i], AClass ) then
			AList.Add( Components[i] );
end;

procedure TKCustomLinkable.AddLink( ALink: TKCustomLink );
begin
	if ( not CheckObject( FLinkList ) ) then
		FLinkList := TList.Create;
	FLinkList.Add( ALink );
	ALink.FOwner := Self; { Hard Couple! }
end;

procedure TKCustomLinkable.RemoveLink( ALink: TKCustomLink );
begin
	ALink.FOwner := nil; { Hard Couple! }
	if CheckObject( FLinkList ) then
		FLinkList.Remove( ALink );
end;

procedure TKCustomLinkable.NotifyLinks( LinkEvent: TLinkEvent; Data: LongInt );
var
	i: Integer;
begin
	if CheckObject( FLinkList ) then
		for i := 0 to FLinkList.Count - 1 do
			TKCustomLink( FLinkList.Items[i] ).DoLinkEvent( LinkEvent, Data );
end;

{
--------------------------------------------------------------------------------
-------------------------------- List Objects ----------------------------------
--------------------------------------------------------------------------------
}

{--------------------------------- TKStrings -----------------------------------}

constructor TKStrings.Create;
begin
	inherited Create;
	FName := Copy( ClassName, 2, Length( ClassName ) );
end;

constructor TKStrings.CreateAutoFree( FreeObjects: Boolean );
begin
	Create;
	FFreeObjects := FreeObjects;
end;

constructor TKStrings.CreateFromStrings( sl: TStrings );
begin
	ForceObject( sl );
	Create;
	if CheckStrings( sl ) then
		Assign( sl );
end;

destructor TKStrings.Destroy;
begin
	if FreeObjects then
		Clear;
	inherited Destroy;
end;

class procedure TKStrings.Error( const Message: string; Data: Integer );

	function ReturnAddr: Pointer;
	asm
		MOV     EAX,[EBP+4]
	end;

begin
	RaiseExceptionAtFmt( EKStrings, Message, ReturnAddr, [Data] );
end;

function TKStrings.AddFmt( const S: string; const Args: array of const ): Integer;
begin
  Result := Add( Format( S, Args ) ); 
end;

function TKStrings.IndexOfValues( const Value: string ): Integer;
var
	iPos: Integer;
	s: string;
begin
	for Result := 0 to Count - 1 do
	begin
		s := Strings[Result];
		iPos := AnsiPos( CH_EQUAL_TOKEN, s );
		if ( iPos <> 0 ) and ( AnsiCompareText( Copy( s, iPos + 1, MaxInt ), Value ) = 0 ) then
		  Exit;
  end;
  Result := -1;
end;

procedure TKStrings.DestroyObject( Index: Integer );
begin
	Objects[Index].Free;
	Objects[Index] := nil;
end;

procedure TKStrings.Clear;
var
	i: Integer;
begin
	if FreeObjects then
		for i := Count - 1 downto 0 do
			DestroyObject( i );
	inherited Clear;
end;

procedure TKStrings.Delete( Index: Integer );
begin
	if FreeObjects then
    DestroyObject( Index );
  inherited Delete( Index )
end;

function TKStrings.AddLine: Integer;
begin
  Result := Add( CH_CRLF );
end;

function TKStrings.Remove( const Source: string ): Integer;
begin
	Result := IndexOf( Source );
	if ( Result <> -1 ) then
		Delete( Result );
end;

function TKStrings.RemoveObject( Source: TObject ): Integer;
begin
	Result := IndexOfObject( Source );
	if ( Result <> -1 ) then
		Delete( Result );
end;

procedure TKStrings.Assign( Source: TPersistent );
begin
	if ( not CheckObject( Source ) ) then
		Clear
	else
		inherited Assign( Source );
end;

function TKStrings.BinarySearchNearest( SearchType: TKStringType; Higher: Boolean;
 const Value: string ): Integer;
var
	k,
	ll,
	hl: Integer;
begin
	Result := BinarySearch( SearchType, Value );
	if ( Result <> -1 ) or ( Count = 0 ) then
		Exit;
	ll := 0;
	hl := ( Count - 1 );
	k := ( ( hl + ll ) div 2 );
	case SearchType of
		stStrings:
		begin
{ it only works when Strings are sorted!!! }
			while ( k <> hl ) and ( k <> ll ) do
				if ( Strings[k] > Value ) then
				begin
					hl := k;
					k  := ( ( k + ll ) div 2 );
				end
				else
				begin
					ll := k;
					k := ( ( k + hl ) div 2 );
				end;
			if ( Higher ) then
			begin
				if ( Strings[k] > Value ) then
					Result := k
				else
					Result := k + 1;
			end
			else
			begin
				if ( Strings[k] < Value ) then
					Result := k
				else
					Result := k - 1;
			end;
		end;

		stNames:
		begin
{ it only works when Names are sorted!!! }
			while ( k <> hl ) and ( k <> ll ) do
				if ( Names[k] > Value ) then
				begin
					hl := k;
					k  := ( ( k + ll ) div 2 );
				end
				else
				begin
					ll := k;
					k := ( ( k + hl ) div 2 );
				end;
			if ( Higher ) then
			begin
				if ( Names[k] > Value ) then
					Result := k
				else
					Result := k + 1;
			end
			else
			begin
				if ( Names[k] < Value ) then
					Result := k
				else
					Result := k - 1;
			end;
		end;

		stValues:
		begin
{ it only works when Values are sorted!!! }
			while ( k <> hl ) and ( k <> ll ) do
				if ( ValuesByIndex[k] > Value ) then
				begin
					hl := k;
					k  := ( ( k + ll ) div 2 );
				end
				else
				begin
					ll := k;
					k := ( ( k + hl ) div 2 );
				end;
			if ( Higher ) then
			begin
				if ( ValuesByIndex[k] > Value ) then
					Result := k
				else
					Result := k + 1;
			end
			else
			begin
				if ( ValuesByIndex[k] < Value ) then
					Result := k
				else
					Result := k - 1;
			end;
		end;
	end;
	if ( Result > ( Count - 1 ) ) then
		Result := ( Count - 1 )
	else if ( Result < 0 ) then
		Result := 0;
end;

function TKStrings.BinarySearch( SearchType: TKStringType; const Value: string ): Integer;
var
	k,
	ll,
	hl: Integer;
begin
	Result := -1;
	if ( Count = 0 ) then
		Exit;
	ll := 0;
	hl := ( Count - 1 );
	k := ( ( hl + ll ) div 2 );
	case SearchType of
		stStrings:
		begin
{ it only works when Strings are sorted!!! }
			while ( Strings[k] <> Value ) and ( k <> hl ) and ( k <> ll )  do
				if ( Strings[k] > Value ) then
				begin
					hl := k;
					k  := ( ( k + ll ) div 2 );
				end
				else
				begin
					ll := k;
					k := ( ( k + hl ) div 2 );
				end;
			if ( Strings[k] = Value ) then
			begin
				while ( k > ll ) and CheckStrEqual( Strings[k - 1], Strings[k] ) do
					Dec( k );
				Result := k;
			end;
		end;

		stNames:
		begin
{ it only works when Names are sorted!!! }
			while ( Names[k] <> Value ) and ( k <> hl ) and ( k <> ll )  do
				if ( Names[k] > Value ) then
				begin
					hl := k;
					k  := ( ( k + ll ) div 2 );
				end
				else
				begin
					ll := k;
					k := ( ( k + hl ) div 2 );
				end;
			if ( Names[k] = Value ) then
			begin
				while ( k > ll ) and CheckStrEqual( Names[k - 1], Names[k] ) do
					Dec( k );
				Result := k;
			end;
		end;

		stValues:
		begin
{ it only works when Values are sorted!!! }
			while ( ValuesByIndex[k] <> Value ) and ( k <> hl ) and ( k <> ll )  do
				if ( ValuesByIndex[k] > Value ) then
				begin
					hl := k;
					k  := ( ( k + ll ) div 2 );
				end
				else
				begin
					ll := k;
					k := ( ( k + hl ) div 2 );
				end;
			if ( ValuesByIndex[k] = Value ) then
			begin
				while ( k > ll ) and CheckStrEqual( ValuesByIndex[k - 1], ValuesByIndex[k] ) do
					Dec( k );
				Result := k;
			end;
		end;
	end;
end;

procedure TKStrings.QuickSort( l, r: Integer; SortType: TKStringType );
var
	i,
	j: Integer;
	s,
	si,
	sj: string;
begin
	repeat
		i := l;
		j := r;
		case SortType of
			stStrings:
			begin
				s := Strings[( l + r ) shr 1];
				si := Strings[i];
				sj := Strings[j];
			end;
			stNames  :
			begin
				s := Names[( l + r ) shr 1];
				si := Names[i];
				sj := Names[j];
			end;
			stValues :
			begin
				s := ValuesByIndex[( l + r ) shr 1];
				si := ValuesByIndex[i];
				sj := ValuesByIndex[j];
			end;
		end;
		repeat
			while ( AnsiCompareText( si, s ) < 0 ) do
			begin
				case SortType of
					stStrings: si := Strings[i];
					stNames  : si := Names[i];
					stValues : si := ValuesByIndex[i];
				end;
				Inc( i );
			end;
			while ( AnsiCompareText( sj, s ) > 0 ) do
			begin
				case SortType of
					stStrings: sj := Strings[j];
					stNames  : sj := Names[j];
					stValues : sj := ValuesByIndex[j];
				end;
				Dec( j );
			end;	
			if ( i <= j ) then
			begin
				Exchange( i, j );
				Inc( i );
				Dec( j );
			end;
		until ( i > j );
		if ( l < j ) then
		  QuickSort( l, j, SortType );
		l := i;
	until ( i >= r );
end;

procedure TKStrings.SortEx( SortType: TKStringType );
begin
	case SortType of
		stStrings: Sort;
		stNames, stValues :
		begin
			Changing;
			QuickSort( 0, Count - 1, SortType );
			Changed;
		end;
	end;
end;

function TKStrings.GetStringIndex( const Value: string ): Integer;
begin
	Result := BinarySearch( stStrings, Value );
end;

function TKStrings.GetNameIndex( const Value: string ): Integer;
begin
	Result := BinarySearch( stNames, Value );
end;

function TKStrings.GetValueIndex( const Value: string ): Integer;
begin
	Result := BinarySearch( stValues, Value );
end;

function TKStrings.GetNearestStringIndex( IsHigher: Boolean; const Value: string ): Integer;
begin
	Result := BinarySearchNearest( stStrings, IsHigher, Value );
end;

function TKStrings.GetNearestNameIndex( IsHigher: Boolean; const Value: string ): Integer;
begin
	Result := BinarySearchNearest( stNames, IsHigher, Value );
end;

function TKStrings.GetNearestValueIndex( IsHigher: Boolean; const Value: string ): Integer;
begin
	Result := BinarySearchNearest( stValues, IsHigher, Value );
end;

procedure TKStrings.SetValue( const Name, Value: string );
var
	i: Integer;
begin
	i := IndexOfName( Name );
{ if the requested Name doesn't exists append the requested value }	
	if ( i < 0 ) then
		i := Add( '' );
	Strings[i] := ( Name + CH_EQUAL_TOKEN + Value )
end;

function TKStrings.GetValuesByIndex( Index: Integer ): string;
begin
	Result := Values[Names[Index]];
end;

procedure TKStrings.SetValuesByIndex( Index: Integer; const Value: string );
begin
	Values[Names[Index]] := Value;
end;

function TKStrings.GetNames( Index: Integer ): string;
begin
	Result := inherited Names[Index];
end;

procedure TKStrings.SetNames( Index: Integer; const Value: string );
var
  sValue: string;
begin
  ForceTrimStr( Value );
	sValue := ValuesByIndex[Index];
	Strings[Index] := ( Value + CH_EQUAL + sValue );
end;

procedure TKStrings.AdjustForValues;
var
	i : Integer;
begin
	if ( Count = 0 ) then
		Exit;
	for i := 0 to Count - 1 do
		if Pos( CH_EQUAL_TOKEN, Strings[i] ) = 0 then
			Strings[i] := Strings[i] + CH_EQUAL_TOKEN;
end;

function TKStrings.Contains( const Source: string ): Boolean;
begin
	Result := ( IndexOf( Source ) <> -1 );
end;

function TKStrings.ContainsObject( Source: TObject ): Boolean;
begin
	Result := ( IndexOfObject( Source ) <> -1 );
end;

procedure TKStrings.Intersect( List: TStrings );
var
	i: Integer;
begin
	for i := Count - 1 downto 0 do
		if ( CheckObjectClass( List, TKStrings ) and
			 ( not TKStrings( List ).Contains( Strings[i] ) ) ) or
			 ( not ( List.IndexOf( Strings[i] ) <> -1 ) ) then
			Delete( i );
end;

procedure TKStrings.IntersectObjects( List: TStrings );
var
	i: Integer;
begin
	for i := Count - 1 downto 0 do
		if ( CheckObjectClass( List, TKStrings ) and
			 ( not TKStrings( List ).ContainsObject( Objects[i] ) ) ) or
			 ( not ( List.IndexOfObject( Objects[i] ) <> -1 ) ) then
			Delete( i );
end;

{---------------------------------- TKPCharStream ------------------------------}

constructor TKCustomPCharStream.Create( AStream: TStream );
begin
	inherited Create;
	FStream := AStream;
	FOwnStream := ( not CheckObject( FStream ) );
	if FOwnStream then
		FStream := TMemoryStream.Create;
end;

constructor TKCustomPCharStream.CreateFromFile( const AFileName: string );
begin
	ForceFile( AFileName );
	Create( TFileStream.Create( AFileName, fmOpenRead or fmShareDenyNone ) );
end;

destructor TKCustomPCharStream.Destroy;
begin
	if FOwnStream then
		FStream.Free;
	inherited Destroy;	  
end;

procedure TKCustomPCharStream.ForcePosition( APosition: Cardinal );
begin
{ Here we must TypeCast MaxLen to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Size
	also returns Integer ranged values... }
	if ( Integer( APosition ) > Stream.Size ) then
		RaiseExceptionFmt( EKPCharStream, sErrPSInvPos, [APosition] );
end;

procedure TKCustomPCharStream.ForceIndex( Index: Cardinal );
begin
	if ( Index > PCharCount ) then
		RaiseExceptionFmt( EKPCharStream, sErrPSInvIdx, [Index] );
end;

procedure TKCustomPCharStream.Normalize;
var
	ch: Char;
	bNull: Boolean;
begin
	ch := CH_NULL;
	Stream.Position := 0;
	bNull := false;
{ remove NULLs at the beginning of stream }
	while ( Stream.Size > Stream.Position ) and
				( Stream.Read( ch, SizeOf( Char ) ) = 1 ) and ( ch = CH_NULL ) do
		bNull := true;
	if ( ( ch <> CH_NULL ) and bNull ) then
		CopyBufferEx( Stream, Stream.Position, 0, Stream.Size - Stream.Position );
{ insert a NULL at the end of stream (if necessary) }
	Stream.Position := Stream.Size;
	if ( Stream.Size > 0 ) then
	begin
		Stream.Position := ( Stream.Position - 1 );
		if ( Stream.Read( ch, SizeOf( Char ) ) = 1 ) and ( ch <> CH_NULL ) then
		begin
			ch := CH_NULL;
			Stream.WriteBuffer( ch, 1 );
		end;
	end;
end;

procedure TKCustomPCharStream.SetItemIndex( Index: Cardinal );
var
	iIndex: Cardinal;
begin
	ForceIndex( Index );
	iIndex := 0;
	Stream.Position := 0;
	while ( Stream.Size > Stream.Position ) and ( iIndex < Index ) do
	begin
		AsPChar;
		Inc( iIndex );
	end;
	if ( iIndex <> Index ) then
		RaiseExceptionFmt( EKPCharStream, sErrPSNotFound, [Index] );
end;

function TKCustomPCharStream.GetPCharCount: Cardinal;
var
	iPos: Cardinal;
begin
	Result := 0;
	iPos := Stream.Position;
	try
		Stream.Position := 0;
		while ( Stream.Size > Stream.Position ) do
			Inc( Result, Ord( CheckPointer( AsPChar ) ) );
	finally
		Stream.Position := iPos;
	end;
end;

function TKCustomPCharStream.GetPChars( Index: Cardinal ): PChar;
var
	iPos: Cardinal;
begin
	ForceIndex( Index );
	iPos := Stream.Position;
	try
		ItemIndex := Index;
		Result := AsPChar;
	finally
		Stream.Position := iPos;
	end;
end;

function TKCustomPCharStream.GetPCharCopy( Index: Cardinal ): PChar;
var
	iPos: Cardinal;
begin
	ForceIndex( Index );
	iPos := Stream.Position;
	try
		ItemIndex := Index;
		Result := AsPCharCopy;
	finally
		Stream.Position := iPos;
	end;
end;

procedure TKCustomPCharStream.SetPChars( Index: Cardinal; APChar: PChar );
begin
	if ( Index > PCharCount ) then
	begin
		if CheckPointer( APChar ) then
			Stream.Position := Stream.Size
		else
			Exit;
	end
	else
	begin
		ItemIndex := Index;
		Delete;
		if ( not CheckPointer( APChar ) ) then
			Exit;
	end;
	Insert( APChar );
end;

function TKCustomPCharStream.ReadPChar( var APChar: PChar ): Cardinal;
begin
	APChar := AsPCharCopy;
	if CheckPointer( APChar ) then
		Result := StrLen( APChar ) + 1
	else
		Result := 0;
end;

function TKCustomPCharStream.ReadStrings( Strings: TStrings ): Cardinal;
var
	s: string;
begin
	ForceStrings( Strings );
	Result := 0;
	while ( Stream.Size > Stream.Position ) do
	begin
		Result := Result + ReadString( s );
		Strings.Add( s );
	end;
end;

function TKCustomPCharStream.ReadString( var AString: string ): Cardinal;
begin
	AString := StrPas( AsPChar );
	if CheckStr( AString ) then
		Result := Length( AString ) + 1
	else
		Result := 0;
end;

function TKCustomPCharStream.WritePChar( APChar: PChar ): Cardinal;
begin
	ForceTrimPChar( APChar );
	Result := Stream.Write( Pointer( APChar )^, StrLen( APChar ) + 1 );
end;

function TKCustomPCharStream.WriteString( const AString: string ): Cardinal;
begin
	if ( not CheckStr( AString ) ) then
		Result := 0
	else
		Result := WritePChar( PChar( AString ) );
end;

function TKCustomPCharStream.WriteStrings( Strings: TStrings ): Cardinal;
var
	i: Integer;
begin
	ForceStrings( Strings );
	Result := 0;
	for i := 0 to Strings.Count - 1 do
		Result := Result + WriteString( Strings[i] );
end;

procedure TKCustomPCharStream.LoadFromStringsFile( const FileName: string );
var
	ss: TStringList;
begin
	ss := TStringList.Create;
	try
		ss.LoadFromFile( FileName );
		Stream.Size := 0;
		WriteStrings( ss );
		Stream.Position := 0;
	finally
		ss.Free;
	end;
end;

function TKCustomPCharStream.Insert( APChar: PChar ): Boolean;
var
	iPos,
	iLen: Cardinal;
begin
	ForceTrimPChar( APChar );
	iLen := StrLen( APChar ) + 1;
	if ( Stream.Size > Stream.Position ) then
	begin
		iPos := Stream.Position;
		try
			CopyBuffer( Stream, iPos, iPos + iLen, iLen );
		finally
			Stream.Position := iPos;
		end;
	end;
	Result := ( WritePChar( APChar ) = iLen );
end;

function TKCustomPCharStream.InsertAt( APChar: PChar; APosition: Cardinal ): Boolean;
begin
	ForceTrimPChar( APChar );
	ForcePosition( APosition );
	Stream.Position := APosition;
	Result := Insert( APChar );
end;

function TKCustomPCharStream.Delete: Boolean;
{ Here we must TypeCast MaxLen to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	also returns Integer ranged values... }
var
	iPos,
	iLen: Integer;
begin
	if ( Stream.Size > Stream.Position ) then
	begin
		iPos := Stream.Position;
		try
			iLen := Integer( StrLen( AsPChar ) ) + 1;
			CopyBufferEx( Stream, iPos + iLen, iPos, Stream.Size - ( iPos + iLen ) );
			Normalize;
		finally
			Stream.Position := iPos;
		end;
	end;
	Result := true;
end;

function TKCustomPCharStream.DeleteAt( APosition: Cardinal ): Boolean;
begin
	ForcePosition( APosition );
	Stream.Position := APosition;
	Result := Delete;
end;

function TKCustomPCharStream.AsPChar: PChar;
begin
	Result := PCharAt( Stream.Position );
{ Here we must TypeCast MaxLen to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	also returns Integer ranged values... }
	if CheckPointer( Result ) then
		Stream.Position := Stream.Position + Integer( StrLen( Result ) ) + 1;
end;

function TKCustomPCharStream.PCharAt( APosition: Cardinal ): PChar;
begin
	ForcePosition( APosition );
{ Here we must TypeCast MaxLen to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	also returns Integer ranged values... }
	if CheckObjectClass( Stream, TStringStream ) then
	begin
		Stream.Position := Stream.Position + Integer( APosition );
		Result := PChar( TStringStream( Stream ).DataString[APosition + 1] );
	end
	else if CheckObjectClass( Stream, TMemoryStream ) then
	begin
		Stream.Position := Stream.Position + Integer( APosition );
		Result := PChar( IncPtr( TMemoryStream( Stream ).Memory, APosition ) );
	end
	else
		Result := PChar( NextString( Stream, CH_NULL, Stream.Size - Stream.Position + 1 ) );
end;

function TKCustomPCharStream.AsPCharCopy: PChar;
begin
	Result := PCharCopyAt( Stream.Position );
{ Here we must TypeCast MaxLen to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	also returns Integer ranged values... }
	if CheckPointer( Result ) then
		Stream.Position := Stream.Position + Integer( StrLen( Result ) ) + 1;
end;

function TKCustomPCharStream.PCharCopyAt( APosition: Cardinal ): PChar;
var
	pc: PChar;
begin
	ForcePosition( APosition );
	Result := nil;
	pc := PCharAt( APosition );
	if CheckPointer( pc ) then
		try
			Result := StrNew( pc );
		except
			StrDispose( Result );
			raise;
		end;
end;

function TKCustomPCharStream.AsString: string;
begin
	Result := string( AsPChar );
end;

function TKCustomPCharStream.StringAt( APosition: Cardinal ): string;
begin
	ForcePosition( APosition );
	Result := string( PCharAt( APosition ) );
end;

procedure TKCustomPCharStream.SetStrings( Strings: TStrings );
begin
	ForceObject( Strings );
	Strings.BeginUpdate;
	try
		Strings.Clear;
		while ( Stream.Size > Stream.Position ) do
			Strings.Add( AsString );
	finally
		Strings.EndUpdate;
	end;
end;

procedure TKCustomPCharStream.SetStringsAt( Strings: TStrings; APosition: Cardinal );
begin
	ForceObject( Strings );
	ForcePosition( APosition );
	Stream.Position := APosition;
	SetStrings( Strings );
end;

{------------------------------- TKMemoryDataStream ----------------------------}

function TKMemoryDataStream.Write( const Buffer; Count: LongInt ): LongInt;
var
	p: Pointer;
begin
	if ( Position + Count > Size ) then
		Count := ( Size - Position );
	p := IncPtr( Memory, Position );
	Move( Buffer, p^, Count );
	Position := ( Position + Count );
	Result := Count;
end;

{--------------------------------- TKStringStream ------------------------------}

constructor TKStringStream.CreateFromStrings( sl: TStrings );
begin
	ForceObject( sl );
	if CheckStrings( sl ) then
		inherited Create( sl.Text )
	else
		inherited Create( '' );
	Position := 0;	
end;

constructor TKStringStream.CreateFromStream( AStream: TStream; SourcePos,
	Count: Cardinal );
begin
	ForceObject( AStream );
	inherited Create( '' );
{ Here we must TypeCast MaxLen to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Size
	also returns Integer ranged values... }
	if ( Integer( SourcePos ) < AStream.Size ) then
	begin
		AStream.Position := Integer( SourcePos );
		CopyFrom( AStream, Count );
	end;
	Position := 0;
end;

function TKStringStream.Write( const Buffer; Count: Longint ): Longint;
begin
	Result := Count;
{ Reimplemented here to avoid DataString truncation }
	if ( ( Position + Result ) > Size ) then
		SetSize( Position + Result );
	Move( Buffer, PChar( @DataString[Position + 1] )^, Result );
	Position := Position + Result;
end;

procedure TKStringStream.SaveToFile( const FileName: string );
var
	fs: TFileStream;
begin
	fs := TFileStream.Create( FileName, fmCreate );
	try
		ForceStreamCopy( Self, fs );
	finally
		fs.Free;
	end;
end;

procedure TKStringStream.PutBack( const Token: string );
begin
  Seek( -Length( Token ), soFromCurrent );
end;

function TKStringStream.GetToken( const Delimiters: string ): string;
var
	Ch: Char;
begin
	Result := '';
	while ( Read( Ch, 1 ) = 1 ) do
	begin
		if ( Length( Delimiters ) = 0 ) and ( Ch < CH_SPACE ) then
		begin
			PutBack( ch );
			Break;
		end
		else if ( Length( Delimiters ) > 0 ) and ( Pos( Ch, Delimiters ) > 0 ) then
		begin
			Putback( Ch );
			Break;
		end;
		AppendStr( Result, Ch );
	end;
end;

function TKStringStream.GetLine: string;
var
	Ch: Char;
begin
	Result := '';
{ Read characters until reaching at end-of-line character. }
	while ( Read( Ch, 1 ) = 1 ) and not ( Ch in [CH_LF, CH_CR] ) do
		AppendStr( Result, Ch );
{ If the end-of-line is CR, look for a subsequent LF. }
	if Ch = CH_CR then
	begin
		if ( Read( Ch, 1 ) = 1 ) and ( Ch <> CH_LF ) then
			Putback( Ch );
	end;
end;

procedure TKStringStream.SkipSpaces;
var
	C: Char;
begin
	while ( Read( C, 1 ) = 1 ) do
		if ( C > CH_SPACE ) then
		begin
{ Stop with the first nonspace character. }
			Putback( C );
			Break;
		end;
end;

function TKStringStream.GetChar: Char;
begin
	inherited ReadBuffer( Result, 1 );
end;

function TKStringStream.GetInteger: LongInt;
begin
	SkipSpaces;
	Result := StrToInt( GetToken( '' ) );
end;

function TKStringStream.GetFloat: Extended;
begin
	SkipSpaces;
	Result := StrToFloat( GetToken( '' ) );
end;

function TKStringStream.GetNextShortString( const Delimiters: ShortString ): ShortString;
begin
	Result := NextShortString( Self, Delimiters );
end;

procedure TKStringStream.CopyBuffer( const SourcePos, DestPos: Integer; Count: Integer );
begin
	ukrClasses.CopyBuffer( Self, SourcePos, DestPos, Count );
end;

procedure TKStringStream.CopyBufferEx( const SourcePos, DestPos: Integer; Count: Integer );
begin
	ukrClasses.CopyBufferEx( Self, SourcePos, DestPos, Count );
end;

procedure TKStringStream.ReadBufferErr( var Buffer; Count: Integer;
	const Message: string );
begin
	if ( Count <> 0 ) and ( Read( Buffer, Count ) <> Count ) then
		RaiseException( EKStringStream, Message );
end;

procedure TKStringStream.ReadBufferErrFmt( var Buffer; Count: Integer; const Message: string;
	const Args: array of const );
begin
	ReadBufferErr( Buffer, Count, SysUtils.Format( Message, Args ) );
end;

procedure TKStringStream.WriteBufferErr( const Buffer; Count: Integer;
	const Message: string );
begin
	if ( Count <> 0 ) and ( Write( Buffer, Count ) <> Count ) then
		RaiseException( EKStringStream, Message );
end;

procedure TKStringStream.WriteBufferErrFmt( const Buffer; Count: Integer; const Message: string;
	const Args: array of const );
begin
	WriteBufferErr( Buffer, Count, SysUtils.Format( Message, Args ) );
end;

procedure TKStringStream.CopyFromBuffer( Source: TStream; Count: LongInt );
var
	iRead : Integer;
begin
	iRead := CopyFrom( Source, Count );
	if ( Count = 0 ) then
		Count := Source.Size;
	if ( iRead <> Count ) then
		RaiseExceptionFmt( EKStringStream, sErrSSCopyFromBuffer, [Count] );
end;

{ Stream Routines }

procedure MoveStreamData( Stream: TStream; const SourcePos, DestPos: Integer; Count: Integer );
var
	p: Pointer;
	ss: TKStringStream;
begin
	if CheckObjectClass( Stream, TStringStream ) then
		System.Move( PChar( @TStringStream( Stream ).DataString[SourcePos + 1] )^,
								 PChar( @TStringStream( Stream ).DataString[DestPos + 1] )^, Count )
	else if CheckObjectClass( Stream, TMemoryStream ) then
	begin
		p := TMemoryStream( Stream ).Memory;
		System.Move( IncPtr( p, SourcePos )^, IncPtr( p, DestPos )^, Count );
	end
	else
	begin
		ss := TKStringStream.CreateFromStream( Stream, SourcePos, Count );
		try
			Stream.Position := DestPos;
			Stream.CopyFrom( ss, Count );
		finally
			ss.Free;
		end;
	end;
end;

procedure CopyBuffer( Stream: TStream; const SourcePos, DestPos: Integer; Count: Integer );
var
	bShrink: Boolean;
begin
  ForceStream( Stream );
	if not ( ValueBetween( SourcePos, 0, Stream.Size, true ) or
					 ValueBetween( DestPos, 0, Stream.Size, true )  or
					 ValueBetWeen( Count, 0, Stream.Size, true ) ) then
		RaiseException( EKStringStream, sErrInvMoveParams );
	if ( SourcePos = DestPos ) then
		Exit
	else if ( SourcePos < DestPos ) then
	begin
{ Moving towards the end of the stream }
		if ( ( DestPos + Count ) > Stream.Size ) then
			Stream.Size := ( DestPos + Count );
		MoveStreamData( Stream, SourcePos, DestPos, Count );
		Stream.Position := DestPos;
	end
	else
	begin
{ Moving towards the beginning of the stream }
		bShrink := ( SourcePos + Count > Stream.Size );
		if bShrink then
			Count := ( Stream.Size - SourcePos );
		MoveStreamData( Stream, SourcePos, DestPos, Count );
		if bShrink then
			Stream.Size := ( DestPos + Count );
		Stream.Position := DestPos;
	end;
end;

procedure CopyBufferEx( Stream: TStream; const SourcePos, DestPos: Integer; Count: Integer );
begin
	CopyBuffer( Stream, SourcePos, DestPos, Count );
	if ( SourcePos > DestPos ) and ( SourcePos + Count = Stream.Size ) then
	begin
		Stream.Size := ( DestPos + Count );
		Stream.Position := DestPos;
	end;
end;

{ TKDataStructure }

constructor TKDataStructure.Create;
begin
	inherited Create;
	FList := TList.Create;
end;

destructor TKDataStructure.Destroy;
begin
	Clear;
	FreeClean( FList );
	inherited Destroy;
end;

procedure TKDataStructure.Clear;
begin
	List.Clear;
end;

procedure TKDataStructure.AssignError( Source: TKDataStructure );
var
  SourceName: string;
begin
  if CheckObject( Source ) then
    SourceName := Source.ClassName
  else
    SourceName := 'nil';
  RaiseExceptionFmt( EKDataStructure, sErrDSAssignError, [SourceName, ClassName] );
end;

procedure TKDataStructure.AssignTo( Dest: TKDataStructure );
begin
  Dest.AssignError( Self );
end;

procedure TKDataStructure.Assign( Source: TKDataStructure );
begin
  if CheckObject( Source ) then
    Source.AssignTo( Self )
  else
    AssignError( nil );
end;

function TKDataStructure.IndexOf( Item: Pointer ): Integer;
begin
  Result := FList.IndexOf( Item );
end;

function TKDataStructure.GetList: TList;
begin
	Result := FList;
end;

function TKDataStructure.GetCount: Integer;
begin
	Result := List.Count;
end;

{ TKFifo }

procedure TKFifo.Append( Sender: TKDataStructure );
var
  i: Integer;
begin
  ForceObjectClass( Sender, TKFifo );
  for i := 0 to Count - 1 do
    TKFifo( Sender ).Push( List[i] );
end;

procedure TKFifo.AssignTo( Dest: TKDataStructure );
begin
  if CheckObjectClass( Dest, TKFifo ) then
  begin
    Dest.Clear;
    Dest.Append( Self );
  end
  else
    inherited AssignTo( Dest );
end;

procedure TKFifo.Clear;
begin
	while ( Count > 0 ) do
		Pop;
	inherited Clear;
end;

function TKFifo.Push( Item: Pointer ): Integer;
begin
	Result := List.Add( Item );
end;

function TKFifo.Pop: Pointer;
begin
	Result := List.First;
	List.Delete( 0 );
end;

{ TKIntegerFifo }

function TKIntegerFifo.IndexOf( Item: Integer ): Integer;
begin
  Result := inherited IndexOf( Pointer( Item ) );
end;

function TKIntegerFifo.Push( Item: Integer ): Integer;
begin
	Result := ( inherited Push( Pointer( Item ) ) );
end;

function TKIntegerFifo.Pop: Integer;
begin
	Result := Integer( inherited Pop );
end;

{ TKExtendedFifo }

function TKExtendedFifo.Push( Item: Extended ): Integer;
var
	pe: PExtended;
begin
	pe := New( PExtended );
	try
		pe^ := Item;
		Result := inherited Push( pe );
	except
		Dispose( pe );
		raise;
	end;
end;

function TKExtendedFifo.Pop: Extended;
var
	pe: PExtended;
begin
	pe := inherited Pop;
	try
		Result := pe^;
	finally
		Dispose( pe );
	end;
end;

{ TKStringFifo }

function TKStringFifo.Push( const Item: string ): Integer;
begin
	Result := ( inherited Push( StrNew( PChar( Item ) ) ) );
end;

function TKStringFifo.Pop: string;
var
  p: PChar;
begin
  p := inherited Pop;
  SetString( Result, p, StrLen( p ) );
  StrDispose( p );
end;

{ TKStack }

procedure TKStack.Append( Sender: TKDataStructure );
var
  i: Integer;
begin
  ForceObjectClass( Sender, TKStack );
  for i := 0 to Count - 1 do
    TKStack( Sender ).Push( List[i] );
end;

procedure TKStack.AssignTo( Dest: TKDataStructure );
begin
  if CheckObjectClass( Dest, TKStack ) then
  begin
    Dest.Clear;
    Dest.Append( Self );
  end
  else
    inherited AssignTo( Dest );
end;

procedure TKStack.Clear;
begin
	while ( Count > 0 ) do
		Pop;
	inherited Clear;
end;

function TKStack.Push( Item: Pointer ): Integer;
begin
	Result := List.Add( Item );
end;

function TKStack.Pop: Pointer;
begin
	Result := List.Last;
	List.Delete( Count - 1 );
end;

{ TKIntegerStack }

function TKIntegerStack.IndexOf( Item: Integer ): Integer;
begin
  Result := inherited IndexOf( Pointer( Item ) );
end;

function TKIntegerStack.Push( Item: Integer ): Integer;
begin
	Result := ( inherited Push( Pointer( Item ) ) );
end;

function TKIntegerStack.Pop: Integer;
begin
	Result := Integer( inherited Pop );
end;

{ TKExtendedStack }

function TKExtendedStack.Push( Item: Extended ): Integer;
var
	pe: PExtended;
begin
	pe := New( PExtended );
	try
		pe^ := Item;
		Result := inherited Push( pe );
	except
		Dispose( pe );
		raise;
	end;
end;

function TKExtendedStack.Pop: Extended;
var
	pe: PExtended;
begin
	pe := inherited Pop;
	try
		Result := pe^;
	finally
		Dispose( pe );
	end;
end;

{ TKStringStack }

function TKStringStack.Push( const Item: string ): Integer;
begin
	Result := ( inherited Push( StrNew( PChar( Item ) ) ) );
end;

function TKStringStack.Pop: string;
var
  p: PChar;
begin
  p := inherited Pop;
  SetString( Result, p, StrLen( p ) );
  StrDispose( p );
end;

{---------------------------------- TK32BitsList -------------------------------}

{ TK32BitsList }

constructor TK32BitsList.Create;
begin
	inherited Create;
	ClearAllBits;
end;

constructor TK32BitsList.CreateFilled( const B: array of Boolean );
begin
	Create;
	Boolean2BitList( B );
end;

procedure TK32BitsList.AssignBits( ABits: TBits );
var
  i: Byte;
begin
	ForceObject( ABits );
	ClearAllBits;
	for i := 0 to Min( ABits.Size - 1, High( TKBitEnum ) ) do
    SetBits( TKBitEnum( i ), ABits.Bits[i] );
end;

function TK32BitsList.ValidRange( const Index: Integer ): Boolean;
begin
	Result := ( Index <= High( TKBitEnum ) ) and ( Index >= Low( TKBitEnum ) );
end;

procedure TK32BitsList.Boolean2BitList( const B: array of Boolean );
var
	i: Byte;
begin
	for i := Low( B ) to High( B ) do
		if ValidRange( i ) then
			SetBits( i, B[i] );
end;

procedure TK32BitsList.ClearBit( BitNo: TKBitEnum );
begin
	if ValidRange( BitNo ) then
		SetBits( BitNo, false );
end;

procedure TK32BitsList.SetBit( BitNo: TKBitEnum );
begin
	if ValidRange( BitNo ) then
		SetBits( BitNo, true );
end;

procedure TK32BitsList.ClearAllBits;
begin
	FBitsList := $00000000;
end;

procedure TK32BitsList.FlipAllBits;
begin
	FBitsList := FBitsList xor LongInt( $FFFFFFFF );
end;

procedure TK32BitsList.FlipBit( BitNo: TKBitEnum );
begin
	FBitsList := FBitsList xor ( 1 shl BitNo );
end;

procedure TK32BitsList.SetBits( Index: TKBitEnum; const Value: Boolean );
begin
	if ( not Value ) then
		FBitsList := FBitsList and ( not ( 1 shl Index ) )
	else
		FBitsList := FBitsList or ( 1 shl Index );
end;

function TK32BitsList.GetBits( Index: TKBitEnum ): Boolean;
begin
	Result := ( ( FBitsList and ( 1 shl Index ) ) <> 0 );
end;

procedure TK32BitsList.SetAsString( const Value: string );
var
	i: Integer;
begin
	for i := 1 to Length( Value ) do
		if ValidRange( i ) then
			SetBits( i, Boolean( Value[i] ) )
		else
			Exit;
end;

function TK32BitsList.GetAsString: string;
var
	i: TKBitEnum;
begin
	SetString( Result, nil, ( High( TKBitEnum ) + 1 ) );
	for i := Low( TKBitEnum ) to High( TKBitEnum ) do
		if GetBits( i ) then
			Result [I + 1] := '1'
		else
			Result [I + 1] := '0';
end;

function TK32BitsList.GetBitSet: Byte;
var
	i: TKBitEnum;
begin
	Result := 0;
	for i := Low( TKBitEnum ) to High( TKBitEnum ) do
		if GetBits( i ) then
			Inc( Result );
end;

procedure TK32BitsList.ReverseBits;
begin
	{ not yet implemented }
end;

{---------------------------------- TKBits ------------------------------------}

constructor TKBits.CreateFromString( const Value: string );
begin
	Create;
	SetAsString( Value );
end;

constructor TKBits.CreateDefault( ASize: Integer; ABits: Boolean );
var
  i: Integer;
begin
  Create;
  Size := ASize;
  if ABits then
    for i := 0 to Size - 1 do
      Bits[i] := ABits;
end;

procedure TKBits.Assign( Source: TBits );
var
	i: Integer;
begin
	if ( not CheckObject( Source ) ) then
		Clear
	else 
	begin
		Size := Source.Size;
		for i := 0 to Size - 1 do
			Bits[i] := Source.Bits[i];
	end;
end;

function TKBits.InvOpenBit: Integer;
begin
	Result := ( Size - 1 );
	if ( Result > 0 ) then
		for Result := 0 to Size - 1 do
			if Bits[Result] then
		   	Exit;
	Result := Size;
end;

function TKBits.GetAsString: string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Size - 1 do
		Result := Result + BOOL_VALUE[Bits[i]];
end;

procedure TKBits.SetAsString( const Value: string );
var
	i: Integer;
begin
	Clear;
	if CheckStr( Value ) then
		ForceStrCharSet( Value, [BOOL_VALUE[False], BOOL_VALUE[True]] )
	else
		Exit;
	Size := Length( Value );
	for i := 1 to Length( Value ) do
		Bits[i-1] := ( Value[i] = BOOL_VALUE[True] );
end;

procedure TKBits.Clear;
begin
	Size := 0;
end;

{------------------------------- TKBitsWrapper ---------------------------------}

constructor TKBitsWrapper.Create( AOwner: TPersistent );
begin
	inherited Create;
	FOwner := AOwner;
	FBits := TKBits.Create;
end;

destructor TKBitsWrapper.Destroy;
begin
	FBits.Free;
	inherited Destroy;
end;

function TKBitsWrapper.GetOwnerInstance: TPersistent;
begin
  Result := FOwner;
end;

procedure TKBitsWrapper.ReadBits( Reader: TReader );
begin
	Bits.AsString := Reader.ReadString;
end;

procedure TKBitsWrapper.WriteBits( Writer: TWriter );
begin
	Writer.WriteString( Bits.AsString );
end;

procedure TKBitsWrapper.DefineProperties( Filer: TFiler );

	function DoWrite: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := True;
			if CheckObjectClass( Filer.Ancestor, TKBitsWrapper ) then
				Result := CheckStrEqual( Bits.AsString,
					TKBitsWrapper( Filer.Ancestor ).Bits.AsString );
		end
		else
			Result := CheckStr( Bits.AsString );
	end;

begin
	Filer.DefineProperty( 'Bits', ReadBits, WriteBits, DoWrite );
end;

{---------------------------------- TKBytes ------------------------------------}

constructor TKBytes.Create( Size: LongInt );
begin
	inherited Create;
	if ( Size > 0 ) then
		SetCount( Size );
end;

constructor TKBytes.CreateCopy( pt: Pointer; sz: LongInt );
begin
	inherited Create;
	AssignBytes( pt, sz );
end;

destructor TKBytes.Destroy;
begin
	SetCount( 0 );
	inherited Destroy;
end;

procedure TKBytes.AssignBits( ABits: TBits );
var
	i: Integer;
begin
	ForceObject( ABits );
	SetCount( ABits.Size );
	for i := 0 to ABits.Size - 1 do
    SetBits( i, ABits.Bits[i] );
end;

procedure TKBytes.SetCount( sz: LongInt );
begin
	if ( sz = FCount ) then
	  Exit;
	ReallocMem( FBytes, sz );
	if ( sz > FCount ) then
		ZeroMemory( @FBytes^[Count], sz - FCount );
	FCount := sz;
end;

procedure TKBytes.Clear;
begin
	ZeroMemory( FBytes, FCount - 1 );
end;

function  TKBytes.AssignBytes( pt: Pointer; sz: LongInt ): Boolean;
begin
	Result := CheckObject( pt ) and ( sz > 0 );
	if Result then
		CopyBytes( pt, sz );
end;

procedure TKBytes.CopyBytes( pt: Pointer; sz: LongInt );
begin
	SetCount( sz );
	Move( PByteArray( pt )^, FBytes^[0], FCount-1 );
end;

function TKBytes.GetByte( Index: LongInt ): Byte;
begin
	if ( Index < 0 ) or ( Index >= Count ) then
		RaiseException( EKBytes, sErrBytesOutRange );
	Result := FBytes^[Index];
end;

procedure TKBytes.SetByte( Index: LongInt; Value: Byte );
begin
	if ( Index < 0 ) or ( Index >= Count ) then
		RaiseException( EKBytes, sErrBytesOutRange );
	FBytes^[Index] := Value;
end;

function TKBytes.GetBits( BitPos: LongInt ): Boolean;
begin
	if ( not GetBit( BitPos, Result ) ) then
		RaiseException( EKBytes, sErrBytesCannotGBit );
end;

procedure TKBytes.SetBits( BitPos: LongInt; Value: Boolean );
begin
	if ( not SetBit( BitPos, Value ) ) then
		RaiseException( EKBytes, sErrBytesCannotSBit );
end;

function TKBytes.GetDibits( DibitPos: LongInt ): Byte;
begin
	if ( not GetDibit( DibitPos, Result ) ) then
		RaiseException( EKBytes, sErrBytesCannotGDiBit );
end;

procedure TKBytes.SetDibits( DibitPos: LongInt; Value: Byte );
begin
	if ( not SetDibit( DibitPos, Value ) ) then
		RaiseException( EKBytes, sErrBytesCannotSDiBit );
end;

function  TKBytes.GetWord( Index: LongInt ): Word;
begin
	if ( Index < 0 ) or ( Index + 1 >= FCount ) then
		RaiseException( EKBytes, sErrBytesOutRange );
	Result := ( Word( FBytes^[Index + 1] ) shl BITS_PER_BYTE ) + FBytes^[Index];
end;

procedure TKBytes.SetWord( Index: LongInt; Value: Word );
begin
	if ( Index < 0 ) or ( Index + 1 >= FCount ) then
		RaiseException( EKBytes, sErrBytesOutRange );
	FBytes^[Index] := Byte( Value );
	FBytes^[Index + 1] := Byte( Value shr BITS_PER_BYTE );
end;

function  TKBytes.GetLong( Index: LongInt ): LongInt;
var
	Word1,
	Word2: Word;
begin
	if ( Index < 0 ) or ( Index + 3 >= FCount ) then
		RaiseException( EKBytes, sErrBytesOutRange );
	Word1 := GetWord( Index );
	Word2 := GetWord( Index + 2 );
	Result := ( LongInt( Word1 ) shl ( 2 * BITS_PER_BYTE ) ) + Word2;
end;

procedure TKBytes.SetLong( Index: LongInt; Value: LongInt );
var
	Word1,
	Word2: Word;
begin
	if ( Index < 0 ) or ( Index + 1 >= FCount ) then
		RaiseException( EKBytes, sErrBytesOutRange );
	Word1 := Word( Value );
	Word2 := Word( Value shr ( 2 * BITS_PER_BYTE ) );
	SetWord( Index, Word2 );
	SetWord( Index + 2, Word1 );
end;

function TKBytes.GetNibble( NibblePos: LongInt ): Byte;
var
	bByte,
	bNibbleIndex: Byte;
	lByteIndex: LongInt;
begin
	lByteIndex := NibblePos div 2;
	bNibbleIndex := ( ( BITS_PER_BYTE div 2 ) * ( NibblePos mod 2 ) );
	if ( lByteIndex >= FCount ) then
		RaiseException( EKBytes, sErrBytesOutRange );
	bByte := FBytes^[lByteIndex];
	Result := Byte( ( bByte shr bNibbleIndex ) and
		( ( 2 * BITS_PER_BYTE ) - 1 ) );
end;

procedure TKBytes.SetNibble( NibblePos: LongInt; Value: Byte );
var
	bMask,
	bByte,
	bNibbleIndex: Byte;
	lByteIndex: LongInt;
begin
	Value := ( Value and ( ( 2 * BITS_PER_BYTE ) - 1 ) );
	lByteIndex := ( NibblePos div 2 );
	bNibbleIndex := ( ( BITS_PER_BYTE div 2 ) * ( NibblePos mod 2 ) );
	if ( lByteIndex >= FCount ) then
		RaiseException( EKBytes, sErrBytesOutRange );
	bMask := Byte( not ( ( ( 2 * BITS_PER_BYTE ) - 1 ) shl bNibbleIndex ) );
	bByte := Byte( FBytes^[lByteIndex] and bMask );
	bByte := Byte( bByte or ( Value shl bNibbleIndex ) );
	FBytes^[lByteIndex] := bByte;
end;

function TKBytes.GetBit( BitPos: LongInt; var Bit: Boolean ): Boolean;
var
	bByte,
	bBitIndex: Byte;
	lByteIndex: LongInt;
begin
	bBitIndex := ( BitPos mod BITS_PER_BYTE );
	lByteIndex := ( BitPos div BITS_PER_BYTE );
	Result := ( lByteIndex < FCount );
	if Result then
	begin
		bByte := FBytes^[lByteIndex];
		Bit := Boolean( ( bByte shr bBitIndex ) and $01 );
	end;
end;

function TKBytes.SetBit( BitPos: LongInt; Value: Boolean ): Boolean;
var
	bMask,
	bByte,
	bBitIndex: Byte;
	lByteIndex: LongInt;
begin
	bBitIndex := ( BitPos mod BITS_PER_BYTE );
	lByteIndex := ( BitPos div BITS_PER_BYTE );
	Result := ( lByteIndex < FCount );
	if Result then
	begin
		bMask := Byte( not ( $01 shl bBitIndex ) );
		bByte := Byte( FBytes^[lByteIndex] and bMask );
		bByte := Byte( bByte or ( Ord( Value ) shl bBitIndex ) );
		FBytes^[lByteIndex] := bByte;
	end;
end;

function TKBytes.GetDibit( DibitPos: LongInt; var Dibit: Byte ): Boolean;
var
	bByte,
	bDibitIndex: Byte;
	lByteIndex: LongInt;
begin
	lByteIndex := ( DibitPos div ( BITS_PER_BYTE div 2 ) );
	bDibitIndex := ( 2 * ( DibitPos mod ( BITS_PER_BYTE div 2 ) ) );
	Result := ( lByteIndex < FCount );
	if Result then
	begin
		bByte := FBytes^[lByteIndex];
		Dibit := Byte( ( bByte shr bDibitIndex ) and $03 );
	end;
end;

function TKBytes.SetDibit( DibitPos: LongInt; Value: Byte ): Boolean;
var
	bMask,
	bByte,
	bDibitIndex: Byte;
	lByteIndex: LongInt;
begin
	lByteIndex := ( DibitPos div ( BITS_PER_BYTE div 2 ) );
	bDibitIndex := ( 2 * ( DibitPos mod ( BITS_PER_BYTE div 2 ) ) );
	Result := ( lByteIndex < FCount ) and ( Value < ( BITS_PER_BYTE div 2 ) );
	if Result then
	begin
		bMask := Byte( not ( $03 shl bDibitIndex ) );
		bByte := Byte( FBytes^[lByteIndex] and bMask );
		bByte := Byte( bByte or ( Value shl bDibitIndex ) );
		FBytes^[lByteIndex] := bByte;
	end;
end;

function TKBytes.AsString: string;
var
	iCounter: LongInt;
begin
	Result := '';
	if ( not CheckPointer( FBytes ) ) or ( FCount = 0 ) then
	  Exit;
	for iCounter := 0 to FCount - 1 do
		Result := Result + Chr( FBytes^[iCounter] );
end;

{
--------------------------------------------------------------------------------
------------------------------- Charset Objects --------------------------------
--------------------------------------------------------------------------------
}

constructor TKPersistentCharSet.Create( AOwner: TComponent );
begin
	inherited Create;
	FOwner := AOwner;
end;

procedure TKPersistentCharSet.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKPersistentCharSet ) then
		CharSet := TKPersistentCharSet( Source ).CharSet
	else
		inherited Assign( Source );
end;

function TKPersistentCharSet.GetCharSet: TKCharSet;
begin
	Result := FCharSet;
end;

procedure TKPersistentCharSet.SetCharSet( const Value: TKCharSet );
begin
	FCharSet := Value;
end;

function TKPersistentCharSet.GetCharSetAsString: string;
begin
	Result := CharSetToStr( CharSet );
end;

function TKPersistentCharSet.GetCharSetAsSetString: string;
begin
	Result := CharSetToSetStr( CharSet );
end;

function TKPersistentCharSet.GetCharSetAsTaggedString: string;
var
	i: Char;
	iSet: TKCharSet;
begin
	iSet := CharSet;
	Result := '';
	for i:= Low( Char ) to High( Char ) do
		if ( i in iSet ) then
		begin
			if ( i <> ListSeparator ) then
				Result := ( Result + i + ListSeparator )
			else
				Result := ( Result + '''' + i + '''' + ListSeparator );
		end;
	if CheckStr( Result ) then
  	Delete( Result, Length( Result ), 1 );
end;

procedure TKPersistentCharSet.SetCharSetAsString( const Value: string );
var
	iSet: TKCharSet;
begin
	iSet := StrToCharSet( Value );
	if ( iSet <> CharSet ) then
		CharSet := iSet;
end;

procedure TKPersistentCharSet.SetCharSetSetString( const Value: string );
var
	s: string;
begin
  ForceTrimStr( Value );
	s := Value;
	s := StringReplace( s, ', ', '', krfAll );
	if ( s[1] = '[' ) then
		Delete( s, 1, 1 );
	if ( CheckStrContains( ']', s ) ) then
		Delete( s, Pos( ']', s ), 1 );
	s := Trim( s );
	SetCharSetAsString( s );
end;

procedure TKPersistentCharSet.WriteData( Writer: TWriter );
begin
	Writer.WriteListBegin;
	Writer.WriteString( AsString );
	Writer.WriteListEnd;
end;

procedure TKPersistentCharSet.ReadData( Reader: TReader );
begin
	Reader.ReadListBegin;
	AsString := Reader.ReadString;
	Reader.ReadListEnd;
end;

procedure TKPersistentCharSet.DefineProperties( Filer: TFiler );

	function DoWrite: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := true;
			if CheckObjectClass( Filer.Ancestor, TKPersistentCharSet ) then
				Result := ( CharSet <> TKPersistentCharSet( Filer.Ancestor ).CharSet );
		end
		else
		  Result := ( CharSet <> [] );
	end;
begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( 'CharSet', ReadData, WriteData, DoWrite );
end;

{
--------------------------------------------------------------------------------
---------------------------- TStringsArray Objects -----------------------------
--------------------------------------------------------------------------------
}

{ Streaming Protocol Tokens }

const
	EOLIN = CH_CRLF;// End-Of-Line for each col in line
	EOROW = CH_TAB; // End-Of-Row  for DefineProperties
	EOFMS = #7;     // End-Of-File-MultiString
	BOFMS = #5;     // Begin-Of-File-MultiString

constructor TKCustomStringsArray.Create( FreeObjects: Boolean );
begin
	inherited Create;
	FList  := TList.Create;
	FBits  := TBits.Create;
	FFreeObjects := FreeObjects;
	FColsAsRows := false;
	FDuplicates := dupIgnore;
end;

destructor TKCustomStringsArray.Destroy;
begin
	Clear;
	FreeClean( FList );
	FreeClean( FBits );
	inherited Destroy;
end;

{ Assignment Support }

procedure TKCustomStringsArray.Assign( Source: TPersistent );
var
	i: Integer;
begin
	if CheckObjectClass( Source, TKCustomStringsArray ) then
		with ( Source as TKCustomStringsArray ) do
			for i := 0 to RowCount - 1 do
			begin
				Self.CheckOrCreate( i );
				( TKStrings( Self.FList.Items[i] ) as GetStringsClass ).Assign(
					( TKStrings( FList.Items[I] ) as GetStringsClass ) );
			end
	else
		inherited Assign( Source );
end;

{ Tests and Checker's }

procedure TKCustomStringsArray.CheckOrCreate( ARow: Integer );
begin
	if ( ARow > ( FList.Count - 1 ) ) then
		Add( nil, '' )
	else if ( ARow < 0 ) then
		Error( sErrStringsArrayIndex, ARow );
end;

class procedure TKCustomStringsArray.Error( const Message: string; Data: Integer );
begin
	TList.Error( Message, Data );
end;

procedure TKCustomStringsArray.TestRow( ARow: Integer );
begin
	if ( ARow < 0 ) or ( ARow >= FList.Count ) then
		Error( sErrStringsArrayIndex, ARow );
end;

procedure TKCustomStringsArray.TestCol( ARow, ACol: Integer );
begin
	if ( ACol < 0 ) or ( ACol >= Rows[ARow].Count ) then
		Error( sErrStringsArrayIndex, ARow + ACol );
end;

procedure TKCustomStringsArray.Changed;
begin
	if Assigned( FOnChanged ) then
		FOnChanged( Self );
end;

procedure TKCustomStringsArray.Changing;
begin
	if Assigned( FOnChanging ) then
		FOnChanging( Self );
end;

procedure TKCustomStringsArray.BeginUpdate;
var
	i: Integer;
begin
	if ( FUpdateCount = 0 ) then
	begin
		SetUpdateState( true );
		for i:= 0 to FList.Count - 1 do
			TKStrings( FList.Items[i] ).BeginUpdate;
	end;
	Inc( FUpdateCount );
end;

procedure TKCustomStringsArray.GetHeader( Sender: TObject; Header: TStrings );
begin
	Header.Add( 'Name' );
	Header.Add( 'Value' );
end;

procedure TKCustomStringsArray.GetGridOptions( Sender: TObject;
	var GridOptions: TGridOptions );
begin
	{Put custom code here!}
end;

procedure TKCustomStringsArray.EditorState( Sender: TObject;
	MultiString: TKCustomStringsArray; State: TEditorState;
	var Handled: Boolean );
begin
	{Put custom code here!}
end;

procedure TKCustomStringsArray.EndUpdate;
var
	i: Integer;
begin
	Dec( FUpdateCount );
	if ( FUpdateCount = 0 ) then
	begin
		SetUpdateState( false );
		for i:= 0 to FList.Count - 1 do
			TKStrings( Flist.Items[i] ).EndUpdate;
	end;
end;

procedure TKCustomStringsArray.SetUpdateState( Updating: Boolean );
begin
	if Updating then
		Changing
	else
		Changed;
end;

{ Streamable support }

function TKCustomStringsArray.Equals( MS: TKCustomStringsArray ): Boolean;
var
	i: Integer;
begin
	Result := false;
	if ( RowCount <> MS.RowCount ) then
	  Exit;
	for i := 0 to RowCount - 1 do
		if ( not TKStrings( FList.Items[I] ).Equals( TKStrings( MS.FList.Items[I] ) ) ) then
			Exit;
	Result := true;
end;

function TKCustomStringsArray.GetRowAsString( ARow: Integer ): string;
begin
{
	Fills the result string with
	Rows[ARow]Col0 CRLF Row[ARow]Col1 CRLF ... CRLF Rows[ARow]ColN
}
	Result := Rows[ARow].Text;
end;

procedure TKCustomStringsArray.SetRowAsString( ARow: Integer;
	const Value: string );
begin
	ForceTrimStr( Value );
{
	The value MUST BE in the following format

	RowXCol0 CRLF RowXCol1 CRLF ... CRLF RowXColN
}
	Rows[ARow].Text := Value;
end;

procedure TKCustomStringsArray.ReadStrings( Reader: TReader );
var
	i: Integer;
	sRowsAffected: TBits;
begin
	Reader.ReadListBegin;
	sRowsAffected := TBits.Create;
	try
		i := 0;
		sRowsAffected.Size := SETSIZE_INTEGER;
		while ( not Reader.EndOfList ) do
		begin
			CheckOrCreate( i );
			TKStrings( FList.Items[i] ).BeginUpdate;
			sRowsAffected.Bits[i] := true;
			ReadStringsArray( Reader, i );
			Inc( i );
			if ( i > sRowsAffected.Size ) then
				sRowsAffected.Size := ( sRowsAffected.Size * 2 );
		end;
	finally
		for i := 0 to sRowsAffected.Size - 1 do
			if ( sRowsAffected.Bits[I] ) then
				TKStrings( FList.Items[I] ).EndUpdate;
		sRowsAffected.Free;
	end;
	Reader.ReadListEnd;
end;

procedure TKCustomStringsArray.ReadStringsArray( Reader: TReader; const ARow: Integer );
begin
	SetRowAsString( ARow, Reader.ReadString );
end;

procedure TKCustomStringsArray.WriteStrings( Writer: TWriter );
var
	i: Integer;
begin
	Writer.WriteListBegin;
	for i := 0 to FList.Count - 1 do
	begin
		WriteStringsArray( Writer, i );
		{Writer.WriteChar( EOROW );}
	end;
	Writer.WriteListEnd;
end;

procedure TKCustomStringsArray.WriteStringsArray( Writer: TWriter; const ARow: Integer );
begin
	Writer.WriteString( GetRowAsString( ARow ) );
end;

procedure TKCustomStringsArray.DefineProperties( Filer: TFiler );

	function DoWrite: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := true;
			if CheckObjectClass( Filer.Ancestor, TKCustomStringsArray ) then
				Result := not Equals( TKCustomStringsArray( Filer.Ancestor ) );
		end
		else
			Result := ( RowCount > 0 ); { No default values! }
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( sDefaultStringsArrayPropertyName, ReadStrings,
		WriteStrings, DoWrite );
end;

{ File Managment Support - Streamable Support Part3 }

procedure TKCustomStringsArray.SaveToStream( Stream: TStream );
var
	i: Integer;
	ch: Char;
begin
	{
		File Streaming Protocol

( BOFMS )      ( EOLIN )                             ( EOROW )
	#5 Row0Col0 CRLF Row0Col1 CRLF ... CRLF Row0ColN #9
		 Row1Col0 CRLF Row1Col1 CRLF ... CRLFf Row1ColN #9
		 .             .             .        .
		 .             .             .        .
		 .             .             .        .         ( EOFMS )
		 RowNCol0 CRLF RowNCol1 CRLF ... CRLF RowNColN #9 #7
	}
	ch := BOFMS;
	Stream.WriteBuffer( ch, SizeOf( Char ) );
	for i:= 0 to FList.Count - 1 do
	begin
		TKStrings( FList.Items[i] ).SaveToStream( Stream );
		ch := EOROW;
		Stream.WriteBuffer( ch, SizeOf( Char ) );
	end;
	ch := EOFMS;
	Stream.WriteBuffer( ch, SizeOf( Char ) );
end;

procedure TKCustomStringsArray.LoadFromStream( Stream: TStream );
var
	i,
	iSize,
	iRead: Integer;
	ch: Char;
	s: string;
	bSure: Boolean;
begin
	ch := CH_NULL;
	i := 0;
{ Read Header }
	Stream.ReadBuffer( ch, SizeOf( Char ) );
	if ( ch <> BOFMS ) then
		Error( sErrInvStringsArrayStream, i );
	iSize := ( Stream.Size-Stream.Position-SizeOf( EOFMS ) );
	if ( iSize <= 0 ) then
		Exit;
	SetString( s, nil, iSize );
{ Read Body } 	
	Stream.ReadBuffer( Pointer( s )^, iSize );
{ Read Footer }
	Stream.ReadBuffer( ch, SizeOf( Char ) );

{ If there is any error, the reasons can be:
	a) There is no EOROW
	b) There is no EOFMS
}
	if ( ch <> EOFMS ) then
		Error( sErrInvStringsArrayStream, -1 );
	if CheckList( FList ) then
	begin
		BeginUpdate;
		bSure := true;
	end
	else
		bSure := false;
	try
		while CheckStr( s ) do
		begin
			CheckOrCreate( i );
			if ( FList.Count - 1 = i ) then
				TKStrings( FList.Items[i] ).BeginUpdate;
			try
				TKStrings( FList.Items[i] ).Text := Copy( s, 1, Pos( EOROW, s ) - 1 );
			finally
				if ( not bSure ) and ( FList.Count - 1 = i ) then
					TKStrings( FList.Items[i] ).EndUpdate;
			end;
			Inc( i );
			iRead := Pos( EOROW, s ) + 1;
			if ( iRead = 1 ) then
				Error( sErrInvStringsArrayStream, i );
			s := Copy( s, iRead, Length( s ) );
		end;
	finally
		if bSure then
			EndUpdate;
	end;
end;

procedure TKCustomStringsArray.SaveToFile( const FileName: string );
var
  FileStream: TFileStream;
begin
	FileStream := TFileStream.Create( FileName, fmCreate or fmShareDenyWrite );
	try
		SaveToStream( FileStream );
	finally
		FileStream.Free;
	end;
end;

procedure TKCustomStringsArray.LoadFromFile( const FileName: string );
var
  FileStream: TFileStream;
begin
	FileStream := TFileStream.Create( FileName, fmOpenRead );
	try
		LoadFromStream( FileStream );
	finally
		FileStream.Free;
	end;
end;

{ Cleaning Support }

procedure TKCustomStringsArray.ClearRow( I: Integer );
var
	j: Integer;
	p: TKLocation;
begin
	with TKStrings( FList.Items[i] ) do
	begin
		for j:= 0 to Count - 1 do
		begin
			if Assigned( Objects[j] ) then
			begin
				P.Row := i;
				P.Col := j;
				FreeObject( Objects[j], p );
			end;
			Objects[j] := nil;
		end;
		Clear;
		Free;
	end;
end;

procedure TKCustomStringsArray.Clear;
var
	i: Integer;
begin
	for i:= 0 to FList.Count - 1 do
	begin
{
	If is to FreeObjects or ( If not to FreeObjects, check the bits to know
	if is FreeObject create with nil. In these cases, call the clean process
	for the line.
}
		if ( FreeObjects ) or ( FBits.Bits[i] ) then
			ClearRow( i );
		FList.Items[i] := nil;
	end;
	FList.Pack;
end;

procedure TKCustomStringsArray.FreeObject( AObject: Pointer; Location: TKLocation );
begin
{
	Default Cleaning mechanism for FreeObjects StringsArray!
	Who derives can implements a default mechanism for cleaning...
}
	try
		if Assigned( TObject( AObject ) ) then
			TObject( AObject ).Free;
	except
		on EInvalidCast do ;
		on EInvalidPointer do ; { Dangerous... }
		else
		  raise;
	end;
end;

{ Utility Routines }

function TKCustomStringsArray.Add( S: TKStrings; const AName: string ): Integer;
type
	TCreation =
		( cNilNotFreeObjects, cNilFreeObjects, cNotFreeObjects, cFreeObjects );
const
	Pattern: array[Boolean, Boolean] of TCreation =
	(
		( cNilNotFreeObjects, cNilFreeObjects ),
		( cNotFreeObjects, cFreeObjects )
	);
var
	R: TKStrings;
begin
	Result := 0;
	FBits.Size := FList.Count + 1;
	case Pattern[( S <> nil ), FreeObjects] of
{
	( S = nil ) and ( FreeObjects = false )
	Creates a new entry and mark the bits for late cleaning!
}
		cNilNotFreeObjects:
		begin
			Result := FList.Add( GetStringsClass.Create );
			{ If isn't FreeObjects and nil! so, we need the management for
				automatic clear if it doesn't allows freeing for the strings
				array.
				If accepts, it's caller responsability to free the strings...
			}
			FBits.Bits[Result] := ( not FAcceptNoOwnedNilAdd );
			if CheckStr( AName ) then
				TKStrings( FList.Items[Result] ).Name := AName;
			Exit;
		end;

{
	( S = nil ) and ( FreeObjects = True ):
	just add a nill empty entry
}
		cNilFreeObjects  : Result := FList.Add( GetStringsClass.Create );

{
	( S <> nil ) and ( FreeObjects = False ):
	Adds a new Reference ( any change into Rows[i] will affect S )
}
		cNotFreeObjects  : Result := FList.Add( S );

{
	( S <> nil ) and ( FreeObjects = True ):
	Adds a new entry with entries for all items ( collumns ) of S
}
		cFreeObjects     :
		begin
			R := GetStringsClass.Create;
			R.Assign( S );
			Result := FList.Add( R );
		end;
	end;
{
	For all cases ( with out cNilNotFreeObjects ), doesn't need the
	cleaning management support
}
	FBits.Bits[Result] := false;
	if CheckStr( AName ) then
		TKStrings( FList.Items[Result] ).Name := AName;
end;

function TKCustomStringsArray.AddStr( const S: string; const ARow: Integer ): Integer;
begin
	Result := Rows[ARow].Add( S );
end;

function TKCustomStringsArray.AddStrObject( const S: string; AObject: TObject;
	const ARow: Integer ): Integer;
begin
	Result := Rows[ARow].AddObject( S, AObject );
end;

procedure TKCustomStringsArray.Delete( ARow: Integer );
begin
	Rows[ARow].Free;
	FList.Delete( ARow );
end;

procedure TKCustomStringsArray.DeleteStr( ARow, ACol: Integer );
begin
{ Doesn't necessary for better performance...

	TestCol( ARow, ACol );
}
	Rows[ARow].Delete( ACol );
end;

procedure TKCustomStringsArray.Exchange( ARow1, ARow2: Integer );
begin
{ Doesn't necessary for better performance...

	TestRow( ARow1 );
	TestRow( ARow2 );
}
	FList.Exchange( ARow1, ARow2 );
end;

function TKCustomStringsArray.ExchangeStr( const Item1, Item2: string ): Boolean;
var
	ARow1,
	ACol1,
	ARow2,
	ACol2: Integer;
	AuxStr: string;
	AuxObj: TObject;
begin
	if CheckStrEqual( Item1, Item2 ) then
	begin
		Result := true;
		Exit;
	end;
	Result := Find( Item1, ARow1, ACol1 ) and  Find( Item2, ARow2, ACol2 );
	if Result then
	begin
		if ( ARow1 = ARow2 ) then
			ExchangeColStr( ARow1, ACol1, ACol2 )
		else if ( ACol1 = ACol2 ) then
			ExchangeRowStr( ACol1, ARow1, ARow2 )
		else
		begin
			AuxStr := Strings[ARow1, ACol1];
			AuxObj := Objects[ARow1, ACol1];
			Strings[ARow1, ACol1] := Strings[ARow2, ACol2];
			Objects[ARow1, ACol1] := Objects[ARow2, ACol2];
			Strings[ARow2, ACol2] := AuxStr;
			Objects[ARow2, ACol2] := AuxObj;
			SortRow( ARow1 );
			SortRow( ARow2 );
		end;
	end;
end;

procedure TKCustomStringsArray.ExchangeColStr( ARow, ACol1, ACol2: Integer );
begin
{
	TestCol( ARow, ACol1 );
	TestCol( ARow, ACol2 );
}
	Rows[ARow].Exchange( ACol1, ACol2 );
end;

procedure TKCustomStringsArray.ExchangeRowStr( ACol, ARow1, ARow2: Integer );
var Aux: string;
begin
{
	TestCol( ARow1, ACol );
	TestCol( ARow2, ACol );
}
	Aux := Rows[ARow1].Strings[ACol];
	Rows[ARow1].Strings[ACol] := Rows[ARow2].Strings[ACol];
	Rows[ARow2].Strings[ACol] := Aux;
end;

procedure TKCustomStringsArray.SortRow( ARow: Integer );
begin
{	TestRow( ARow ); }
	if CheckObjectClass( Rows[ARow], GetStringsClass ) and
		( ( Rows[ARow] as GetStringsClass ).Sorted ) then
		( Rows[ARow] as GetStringsClass ).Sort;
end;

function TKCustomStringsArray.Find( const S: string; var ARow, ACol: Integer ): Boolean;
var
	i: Integer;
begin
	Result := false;
	try
		for i:= 0 to FList.Count - 1 do
		begin
			Result := ( TKStrings( FList.Items[I] ) as GetStringsClass ).Find( S, ACol );
			if Result then
			begin
				ARow := i;
				Exit;
			end;
		end;
	except
		on EInvalidCast do
		begin
			ARow := -1;
			ACol := -1;
			Result := false;
		end
		else
		  raise;
	end;
end;

function TKCustomStringsArray.IndexOf( const S: string ): TKLocation;
var
	i: Integer;
begin
	Result.Col := -1;
	for i:= 0 to FList.Count - 1 do
	begin
		Result.Row := i;
		Result.Col := TKStrings( FList.Items[i] ).IndexOf( S );
		if ( Result.Col <> -1 ) then
			Exit;
	end;
	Result.Row := -1;
end;

function TKCustomStringsArray.GetNameIndex( ARow: Integer; const Value: string ): Integer;
begin
	Result := Rows[ARow].GetNameIndex( Value );
end;

function TKCustomStringsArray.GetValueIndex( ARow: Integer; const Value: string ): Integer;
begin
	Result := Rows[ARow].GetValueIndex( Value );
end;

function TKCustomStringsArray.GetNearestNameIndex( ARow: Integer; IsHigher: Boolean;
	const Value: string ): Integer;
begin
	Result := Rows[ARow].GetNearestNameIndex( IsHigher, Value );
end;

function TKCustomStringsArray.GetNearestValueIndex( ARow: Integer; IsHigher: Boolean;
	const Value: string ): Integer;
begin
	Result := Rows[ARow].GetNearestValueIndex( IsHigher, Value );
end;

function TKCustomStringsArray.IndexOfPropName( const S: string ): Integer;
var
	i: Integer;
begin
	Result := -1;
	for i:= 0 to FList.Count - 1 do
		if CheckStrEqual( TKStrings( FList.Items[I] ).Name, S ) then
		begin
			Result := i;
			Exit;
		end;
end;

{ Get's/Set's }

function TKCustomStringsArray.GetCount: Integer;
begin
	Result := FList.Count;
end;

function TKCustomStringsArray.GetStrCount( ARow: Integer ): Integer;
begin
	Result := Rows[ARow].Count;
end;

function TKCustomStringsArray.GetHighColCount: Integer;
var
	i: Integer;
begin
	Result := -1;
	for i:= 0 to FList.Count - 1 do
		if ( Result < ColCount[i] ) then
			Result := ColCount[i];
end;

function TKCustomStringsArray.Get( ARow, ACol: Integer ): string;
begin
	{ TestCol( ARow, ACol ); }
	Result := Rows[ARow].Strings[ACol];
end;

function TKCustomStringsArray.GetObjects( ARow, ACol: Integer ): TObject;
begin
	{ TestCol( ARow, ACol ); }
	Result := Rows[ARow].Objects[ACol];
end;

function TKCustomStringsArray.GetValues( ARow: Integer; const ANames: string ): string;
begin
	Result := Rows[ARow].Values[ANames];
end;

function TKCustomStringsArray.GetValuesByIndex( ARow, ACol: Integer ): string;
begin
	Result := Rows[ARow].ValuesByIndex[ACol];
end;

function TKCustomStringsArray.GetNames( ARow, ACol: Integer ): string;
begin
	{ TestCol( ARow, ACol ); }
	Result := Rows[ARow].Names[ACol];
end;

function TKCustomStringsArray.GetText( ARow: Integer ): string;
begin
	Result := Rows[ARow].Text;
end;

function TKCustomStringsArray.GetCommaText( ARow: Integer ): string;
begin
	Result := Rows[ARow].CommaText;
end;

function TKCustomStringsArray.GetRows( ARow: Integer ): TKStrings;
begin
{ Leave Only this to prevent Access Violation }
	TestRow( ARow );
	Result := TKStrings( FList.Items[ARow] );
end;

function TKCustomStringsArray.GetCols( ACol: Integer ): TKStrings;
var
  ARow: Integer;
begin
	Result := GetStringsClass.Create;
	try
		for ARow:= 0 to FList.Count - 1 do
		begin
			{ TestCol( ARow, ACol ); }
			Result.Add( TKStrings( FList.Items[ARow] ).Strings[ACol] );
		end;
	except
		Result.Free;
		raise;
	end;
end;

procedure TKCustomStringsArray.SetDuplicates( const Value: TDuplicates );
var
	i: Integer;
begin
	if ( Value <> FDuplicates ) then
	begin
		FDuplicates := Value;
		for i:= 0 to FList.Count - 1 do
			if CheckObjectClass( TKStrings( FList.Items[i] ), GetStringsClass ) then
				( TKStrings( FList.Items[i] ) as GetStringsClass ).Duplicates := Value;
	end;
end;

procedure TKCustomStringsArray.Put( ARow, ACol: Integer; Value: string );
begin
	{ TestCol( ARow, ACol ); }
	Rows[ARow].Strings[ACol] := Value;
end;

procedure TKCustomStringsArray.PutObjects( ARow, ACol: Integer; Value: TObject );
begin
	{ TestCol( ARow, ACol ); }
	Rows[ARow].Objects[ACol] := Value;
end;

procedure TKCustomStringsArray.SetText( ARow: Integer; const Value: string );
begin
	Rows[ARow].Text := Value;
end;

procedure TKCustomStringsArray.SetCommaText( ARow: Integer; const Value: string );
begin
	Rows[ARow].CommaText := Value;
end;

procedure TKCustomStringsArray.SetRows( ARow: Integer; Strings: TKStrings );
var
  S: TKStrings;
begin
	{ Leave Only this to prevent Access Violation }
	TestRow( ARow );
	S := FList.Items[ARow];
	S.Assign( Strings );
	{ TKStrings( FList.Items[ARow] ).Assign( Strings ); }
end;

procedure TKCustomStringsArray.SetCols( ACol: Integer; Strings: TKStrings );
var
	ARow: Integer;
begin
	if ( FList.Count <> Strings.Count ) then
		Error( sErrStringsArrayIndex, ACol );
	for ARow:= 0 to FList.Count - 1 do
	begin
		{TestCol( ARow, ACol );}
		TKStrings( FList.Items[ARow] ).Strings[ACol] := Strings.Strings[ARow];
	end;
end;

procedure TKCustomStringsArray.SetValues( ARow: Integer; const ANames, Value: string );
begin
	Rows[ARow].Values[ANames] := Value;
end;

procedure TKCustomStringsArray.SetValuesByIndex( ARow, ACol: Integer; const Value: string );
begin
	Rows[ARow].ValuesByIndex[ACol] := Value;
end;

{----------------------------- TKStringsArray------------------------------------}

function TKStringsArray.GetStringsClass: TKStringsClass;
begin
	Result := TKStrings;
end;

{
const
	BufSize = 32;

	function GetBuffer( BufSize: Byte ): PChar;
	begin
		Result := StrAlloc( BufSize+1 );
		try
			if ( Stream.Position+BufSize ) > Stream.Size then
				BufSize := Stream.Size-Stream.Position;
			Stream.ReadBuffer( Result^, BufSize );
		except
			StrDispose( Result );
			raise;
		end;
	end;

const
	CharSet = [#0..#255]-[BOFMS, EOFMS, EOROW, #13, #10];

var J, Size, Lido: LongInt;
		X: Char;
		P: PChar;
		S: string;
		bSure: Boolean;
begin
	X := #0;
	S := '';
	J := 0;
	Stream.ReadBuffer( X, SizeOf( Char ) ); // Cabeçalho
	if ( X <> BOFMS ) then
		Error( sInvalidStringsArrayStreamHeader, J );

	if FList.Count > 0 then
	begin
		BeginUpdate;
		bSure := true;
	end
	else
		bSure := false;

	while ( Stream.Size <> Stream.Position ) do
	begin
		P := GetBuffer( BufSize );
		SetString( S, nil, BufSize );
		try
			while ( P^ <> #0 ) do
			begin
				while ( P^ in CharSet ) do
				begin
					if P^ in [#13, #10] then Break;
					AppendStr( S, P^ );
					Inc( P );
				end;
				if ( P^ = #13 ) then
				begin
					Inc( P );
					Check( P^, #10 );
					Inc( P );
				end;
				if ( P^ = #9 ) then
				begin
					Inc( P );
					if ( P^ <> #7 ) then
					begin
						Inc( P );
						if ( P^ in CharSet ) then
						begin
							Dec( P )
							Continue;
						end
						else
					end;
					else
					begin
						Inc( P );
						Check( P^, #0 );
					end;
				end;


			end;
		finally
			StrDispose( P );
		end;

		if ( Stream.Position + SizeOf( EOFMS ) ) = Stream.Size then
		begin
			Stream.ReadBuffer( X, SizeOf( Char ) );
			// Caso o erro seja por falta do tamanho correto, ou é por falta
			// de #9 ( EOROW ) ou é por falta do #7 ( EOFMS )
			if ( X <> EOFMS ) then
				Error( sInvalidStringsArrayStreamFooter, 0 );
		end;
	end;

	Size := Stream.Size-Stream.Position-SizeOf( EOFMS );

	SetString( S, nil, Size );
	Stream.ReadBuffer( Pointer( S )^, Size );

	try
		while ( Length( S ) <> 0 ) do
		begin
			CheckOrCreate( J );
			if ( FList.Count-1 = J ) then
				Rows[J].BeginUpdate;
			try
				Rows[J].Text := Copy( S, 1, Pos( EOROW, S )-1 );
			finally
				if ( not bSure ) and ( FList.Count-1 = J ) then
					Rows[J].EndUpdate;
			end;
			Inc( J );
			Lido := Pos( EOROW, S )+1;
			// Se não achar EOROW em alguma posição ( Pos( EOROW, S ) = 0 )
			if Lido = 1 then
				Error( sInvalidStringsArrayStream, J );
			S := Copy( S, Lido, Length( S ) );
		end;
	finally
		if bSure then
			EndUpdate;
	end;
end;
}

{
--------------------------------------------------------------------------------
------------------------------- Pallete Object ---------------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

const

	PAL_ENTRY_SIZE = SizeOf( TPaletteEntry );

	PALETTE_USE: array[TKPaletteUse] of Integer =
	(
		SYSPAL_NOSTATIC,
		SYSPAL_STATIC,
		SYSPAL_ERROR
	);

function GetSystemPaletteEntries( DC: HDC; StartIndex, NumEntries: UINT;
	PaletteEntries: Pointer ): UINT; stdcall; external gdi32 name
	'GetSystemPaletteEntries';

procedure DisposePalEntry( ppe: Pointer );
begin
	if CheckPointer( ppe ) then
		FreeMem( PPaletteEntry( ppe ), PAL_ENTRY_SIZE );
end;

function NewPalEntry: PPaletteEntry;
begin
	GetMem( Result, PAL_ENTRY_SIZE );
	ZeroMemory( Result, PAL_ENTRY_SIZE );
end;

{---------------------------- Public Implementation ----------------------------}

{------------------------------- TKPaletteEntries ------------------------------}

constructor TKPaletteEntries.Create;
begin
	inherited Create;
	FChanged := false;
	FEntries := TList.Create;
	FLogPalette := nil;
	FLogPaletteSize := 0;
	FPaletteEntries := nil;
	FPaletteEntriesSize := 0;
end;

destructor TKPaletteEntries.Destroy;
begin
	DisposeLogPalette;
	DisposePaletteEntries;
	FEntries.Clear;
	FEntries.Free;
	FEntries := nil;
	inherited Destroy;
end;

function TKPaletteEntries.GetCount: Integer;
{ packing is important because it will avoid nil PPaletteEntry pointers to be
	passed on to API calls }
begin
	Pack;
	Result := FEntries.Count;
end;

function TKPaletteEntries.GetPalEntry( Index: Integer ): TPaletteEntry;
begin
	Result := PPaletteEntry( FEntries[Index] )^;
end;

procedure TKPaletteEntries.SetPalEntry( Index: Integer; Value: TPaletteEntry );
var
	ppe: PPaletteEntry;
begin
	ppe := NewPalEntry;
	ppe^ := Value;
	with FEntries do
	begin
		DisposePalEntry( FEntries[Index] );
		FEntries[Index] := ppe;
	end;
end;

procedure TKPaletteEntries.Pack;
begin
	FEntries.Pack;
end;

procedure TKPaletteEntries.Clear;
var
	i: Integer;
begin
	for i := FEntries.Count - 1 downto 0 do
	begin
		DisposePalEntry( FEntries[i] );
		FEntries[i] := nil;
	end;
	Pack;
end;

procedure TKPaletteEntries.Delete( Index: Integer );
begin
  DisposePalEntry( FEntries[Index] );
	FEntries.Delete( Index );
end;

function TKPaletteEntries.Add( Value: Pointer ): Integer;
begin
	Result := FEntries.Add( Value );
end;

procedure TKPaletteEntries.DisposeLogPalette;
begin
	if ( not CheckPointer( FLogPalette ) ) or ( FLogPaletteSize = 0 ) then
		Exit;
	FreeMem( FLogPalette, FLogPaletteSize );
	FLogPalette := nil;
	FLogPaletteSize := 0;
end;

procedure TKPaletteEntries.DisposePaletteEntries;
begin
	if ( not CheckPointer( FPaletteEntries ) ) or ( FPaletteEntriesSize = 0 ) then
		Exit;
	FreeMem( FPaletteEntries, FPaletteEntriesSize );
	FPaletteEntries := nil;
	FPaletteEntriesSize := 0;
end;

function TKPaletteEntries.AllocLogPalette: Pointer;
begin
	DisposeLogPalette;
	FLogPaletteSize := ( SizeOf( TLogPalette ) + Count * PAL_ENTRY_SIZE );
	GetMem( FLogPalette, FLogPaletteSize );
	ZeroMemory( FLogPalette, FLogPaletteSize );
	Result := FLogPalette;
end;

function TKPaletteEntries.AllocPaletteEntries: Pointer;
begin
	DisposePaletteEntries;
	FPaletteEntriesSize := ( Count * PAL_ENTRY_SIZE );
	GetMem( FPaletteEntries, FPaletteEntriesSize );
	ZeroMemory( FPaletteEntries, FPaletteEntriesSize );
	Result := FPaletteEntries;
end;

{---------------------------------- TKPalette ----------------------------------}

constructor TKPalette.Create( ACanvas: TCanvas );
begin
	inherited Create;
	FCanvas := ACanvas;
	FForceBackGround := false;
	FHandle := 0;
	FOldHandle := 0;
	FEntries := TKPaletteEntries.Create;
end;

destructor TKPalette.Destroy;
begin
	FEntries.Free;
	inherited Destroy;
end;

procedure TKPalette.FreeHandle;
begin
	if ValidPalette then
	begin
		DeleteObject( FHandle );
		FHandle := 0;
	end;
end;

function TKPalette.ValidCanvas: Boolean;
begin
	Result := CheckObject( FCanvas );
end;

function TKPalette.ValidPalette: Boolean;
begin
	Result := CheckHandle( Handle );
end;

function TKPalette.GetCount: Integer;
begin
	Result := FEntries.Count;
end;

function TKPalette.CreatePalette: Boolean;
var
	i: Integer;
	plp: PLogPalette;
begin
	plp := FEntries.AllocLogPalette;
	try
		with plp^ do
		begin
			palVersion := $300;
			palNumEntries := FEntries.Count;
{ The programmer should take care of system palette entries }
			for i := 0 to FEntries.Count - 1 do
				palPalEntry[i] := PPaletteEntry( FEntries[i] )^;
		end;
{ If a palette exists, it is deleted before a new one is created }
		FreeHandle;
		FHandle := Windows.CreatePalette( plp^ );
	finally
		FEntries.DisposeLogPalette;
	end;
	Result := ( FHandle <> 0 );
end;

function TKPalette.RealizePalette: Boolean;
{ Selects the current palette to the current canvas, and realizes it }
begin
	Result := ( ValidPalette and ValidCanvas );
	if ( not Result ) then
		Exit;
	FOldHandle := Windows.SelectPalette( FCanvas.Handle, FHandle, FForceBackGround );
	Windows.RealizePalette( FCanvas.Handle );
	Result := true;
end;

function TKPalette.CreateHalftonePalette: Boolean;
begin
	Result := false;
	if ( not ValidCanvas ) then
		Exit;
{ If a palette exists, it is deleted before a new one is created }
	FreeHandle;
	FHandle := Windows.CreateHalftonePalette( FCanvas.Handle );
end;

function TKPalette.CreateHalftonePaletteEx( ACanvas: TCanvas ): Boolean;
begin
	Result := false;
	if ( not ValidCanvas ) then
		Exit;
{ If a palette exists, it is deleted before a new one is created }
	FreeHandle;
	FHandle := Windows.CreateHalftonePalette( ACanvas.Handle );
end;

function TKPalette.SelectPalette( ACanvas: TCanvas; AsBackGround: Boolean ): HPALETTE;
{ Selects the current palette to ACanvas }
begin
	ForceObject( ACanvas );
	ForceHandle( Handle );
	Result := Windows.SelectPalette( ACanvas.Handle, FHandle, AsBackGround );
end;

function TKPalette.AnimatePalette( Index, Count: Integer; kpe: TKPaletteEntries ): Boolean;
var
	ppe: PPaletteEntry;
begin
	Result := false;
	if ( not ValidPalette ) or ( not CheckObject( kpe ) ) or ( Count <= 0 ) then
		Exit;
	ppe := kpe.AllocPaletteEntries;
	try
		Windows.AnimatePalette( FHandle, Index, Count, ppe );
	finally
		kpe.DisposePaletteEntries;
	end;
end;

procedure TKPalette.DeletePalEntry( Index: Integer );
begin
	FEntries.Delete( Index );
end;

function TKPalette.AddPalEntry( Value: TPaletteEntry ): Integer;
var
	ppe: PPaletteEntry;
begin
	ppe := NewPalEntry;
	ppe^ := Value;
	Result := FEntries.Add( ppe );
end;

function TKPalette.GetSystemPalEntries( Index, Count: Integer; kpe: TKPaletteEntries ): Boolean;
var
	ppea: PPEArray;
	i,
	iCount: Integer;
	ppe: PPaletteEntry;
begin
	Result := false;
  if ( not ValidCanvas ) or ( not CheckObject( kpe ) ) or ( Count <= 0 ) then
		Exit;
{ no older entries should be kept from kpe }
	kpe.Clear;
{ alloc enough memory to hold count palette entries }
	GetMem( ppea, Count * PAL_ENTRY_SIZE );
	try
		ZeroMemory( ppea, Count * PAL_ENTRY_SIZE );
{ how many palette entries were returned? }
		iCount := ukrClasses.GetSystemPaletteEntries( FCanvas.Handle, Index, Count, ppea );
		for i := 0 to iCount - 1 do
		begin
{ for each entry returned, alloc a new palette entry and add it to kpe }
			ppe := NewPalEntry;
			ppe^ := ppea^[i]^;
			kpe.Add( ppe );
		end;
	finally
		FreeMem( ppea, Count * PAL_ENTRY_SIZE );
	end;
end;

function TKPalette.SetPalEntry( pal: TKPalette; Index: Integer; pe: TPaletteEntry ): Boolean;
var
	ppe: PPaletteEntry;
begin
	ForceObject( pal );
	Result := false;
	if ( not CheckHandle( pal.Handle ) ) then
		Exit;
	ppe := NewPalEntry;
	try
		ppe^ := pe;
		SetPaletteEntries( pal.Handle, Index, 1, ppe );
	finally
		DisposePalEntry( ppe );
	end;
end;

function TKPalette.SetPalEntries( Index: Integer; kpe: TKPaletteEntries ): Boolean;
var
	ppe: PPaletteEntry;
begin
	Result := false;
	if ( not ValidPalette ) or ( not CheckObject( kpe ) ) or ( kpe.Count <= 0 ) then
		Exit;
	ppe := kpe.AllocPaletteEntries;
	try
		SetPaletteEntries( FHandle, Index, kpe.Count, ppe );
	finally
		kpe.DisposePaletteEntries;
	end;
end;

function TKPalette.GetPalEntry( pal: TKPalette; Index: Integer; var pe: TPaletteEntry ): Boolean;
{ get the Index-th palette entry from pal and return it in pe }
var
	ppe: PPaletteEntry;
begin
  ForceObject( pal );
	Result := false;
	if ( not pal.ValidCanvas ) then
		Exit;
	ppe := NewPalEntry;
	try
		GetPaletteEntries( FCanvas.Handle, Index, 1, ppe );
		pe := ppe^;
	finally
		DisposePalEntry( ppe );
	end;
end;

function TKPalette.GetPalEntries( Index, Count: Integer; kpe: TKPaletteEntries ): Boolean;
var
	ppea: PPEArray;
	i,
	iCount: Integer;
	ppe: PPaletteEntry;
begin
	Result := false;
	if ( not ValidPalette ) or ( not CheckObject( kpe ) ) or ( Count <= 0 ) then
		Exit;
	kpe.Clear;
{ alloc enough memory to hold count palette entries }
	GetMem( ppea, Count * PAL_ENTRY_SIZE );
	try
		ZeroMemory( ppea, Count * PAL_ENTRY_SIZE );
{ how many palette entries were returned? }
		iCount := GetPaletteEntries( FHandle, Index, Count, ppea );
		for i := 0 to iCount - 1 do
		begin
{ for each entry returned, alloc a new palette entry and add it to kpe }
			ppe := NewPalEntry;
			ppe^ := ppea^[i]^;
			kpe.Add( ppe );
		end;
	finally
		FreeMem( ppea, Count * PAL_ENTRY_SIZE );
	end;
end;

function TKPalette.GetSysPaletteUse: TKPaletteUse;
var
	i: TKPaletteUse;
	iPaletteUse: Integer;
begin
	ForceObject( FCanvas );
	Result := puError;
	iPaletteUse := GetSystemPaletteUse( FCanvas.Handle );
	for i := Low( TKPaletteUse ) to High( TKPaletteUse ) do
		if ( PALETTE_USE[i] = iPaletteUse ) then
		begin
			Result := i;
			Exit;
		end;
end;

procedure TKPalette.SetSysPaletteUse( Value: TKPaletteUse );
begin
	ForceObject( FCanvas );
	SetSystemPaletteUse( FCanvas.Handle, PALETTE_USE[Value] );
end;

function TKPalette.NearestColor( Color: TColor ): TColor;
begin
	Result := -1;
	if ValidPalette then
		Result := GetNearestColor( FHandle, Color );
end;

function TKPalette.NearestPaletteIndex( Color: TColor ): Integer;
begin
	Result := -1;
	if ValidPalette then
		Result := GetNearestPaletteIndex( FHandle, Color );
end;

function TKPalette.GetEnhMetafilePalEntries( mf: TMetafile; kpe: TKPaletteEntries ): Boolean;
var
	ppea: PPEArray;
	i,
  iSize, 
	iCount: Integer;
	ppe: PPaletteEntry;
begin
	Result := false;
	if ( not ValidPalette ) or ( not CheckObject( kpe ) ) or ( Count <= 0 ) then
		Exit;
{ how many entries do we need to allocate? }
	iSize := GetEnhMetafilePaletteEntries( mf.Handle, 0, nil );
	if ( iSize <= 0 ) then
		Exit;
{ no older entries should be kept from kpe }
	kpe.Clear;
{ alloc enough memory to hold count palette entries }
	GetMem( ppea, Count * PAL_ENTRY_SIZE );
	try
		ZeroMemory( ppea, Count * PAL_ENTRY_SIZE );
{ how many palette entries were returned? }
		iCount := GetEnhMetafilePaletteEntries( mf.Handle, iSize, ppea );
		for i := 0 to iCount - 1 do
		begin
{ for each entry returned, alloc a new palette entry and add it to kpe }
			ppe := NewPalEntry;
			ppe^ := ppea^[i]^;
			kpe.Add( ppe );
		end;
	finally
		FreeMem( ppea, Count * PAL_ENTRY_SIZE );
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------------- TKEnvironment ---------------------------------
--------------------------------------------------------------------------------
}

{------------------------------- TKEnvironment ---------------------------------}

constructor TKEnvironment.Create;
begin
	inherited Create;
	FParams := TStringList.Create;
	LoadEnvironment( FParams );
end;

destructor TKEnvironment.Destroy;
begin
	FParams.Free;
	inherited Destroy;
end;

procedure TKEnvironment.UpdateEnvironment;
begin
	LoadEnvironment( FParams );
end;

procedure TKEnvironment.LoadEnvironment( ss: TStrings );
var
	pc: PChar;
begin
	ForceObject( ss );
	pc := GetEnvironmentStrings;
	try
		ss.Clear;
		while ( pc^ <> CH_NULL ) do
		begin
			ss.Add( StrPas( pc ) );
			Inc( pc, StrLen( pc ) + 1 );
		end;
	finally
		FreeEnvironmentStrings( pc );
	end;
end;

function TKEnvironment.GetParamCount: Word;
begin
	Result := 0;
	if Assigned( FParams ) then
		Result := FParams.Count;
end;

function TKEnvironment.GetValues( const Name: string ): string;
begin
	Result := '';
	if Assigned( FParams ) then
		Result := FParams.Values[Name];
end;

procedure TKEnvironment.SetValues( const Name, Value: string );
begin
	SetEnvironmentVariable( PChar( Name ), PChar( Value ) );
	LoadEnvironment( FParams );
end;

{--------------------------- TKServerEnvironment -------------------------------}

function TKServerEnvironment.GetGatewayInterface: string;
begin
	Result := Values['GATEWAY_INTERFACE'];
end;

function TKServerEnvironment.GetServerAdmin: string;
begin
	Result := Values['SERVER_ADMIN'];
end;

function TKServerEnvironment.GetServerName: string;
begin
	Result := Values['SERVER_NAME'];
end;

function TKServerEnvironment.GetServerSoftware: string;
begin
	Result := Values['SERVER_SOFTWARE'];
end;

function TKServerEnvironment.GetAuthType: string;
begin
	Result := Values['AUTH_TYPE'];
end;

function TKServerEnvironment.GetContentLength: string;
begin
	Result := Values['CONTENT_LENGTH'];
end;

function TKServerEnvironment.GetContentType: string;
begin
	Result := Values['CONTENT_TYPE'];
end;

function TKServerEnvironment.GetHTTPRequestMethod: string;
begin
	Result := Values['HTTP_REQUEST_METHOD'];
end;

function TKServerEnvironment.GetPath: string;
begin
	Result := Values['PATH'];
end;

function TKServerEnvironment.GetPathInfo: string;
begin
	Result := Values['PATH_INFO'];
end;

function TKServerEnvironment.GetPathTranslated: string;
begin
	Result := Values['PATH_TRANSLATED'];
end;

function TKServerEnvironment.GetQueryString: string;
begin
	Result := Values['QUERY_STRING'];
end;

function TKServerEnvironment.GetRemoteAddr: string;
begin
	Result := Values['REMOTE_ADDR'];
end;

function TKServerEnvironment.GetRemoteHost: string;
begin
	Result := Values['REMOTE_HOST'];
end;

function TKServerEnvironment.GetRemoteIdent: string;
begin
	Result := Values['REMOTE_IDENT'];
end;

function TKServerEnvironment.GetRemoteUser: string;
begin
	Result := Values['REMOTE_USER'];
end;

function TKServerEnvironment.GetScriptFileName: string;
begin
	Result := Values['SCRIPT_FILENAME'];
end;

function TKServerEnvironment.GetScriptName: string;
begin
	Result := Values['SCRIPT_NAME'];
end;

function TKServerEnvironment.GetServerPort: string;
begin
	Result := Values['SERVER_PORT'];
end;

{added on 16.07.1999}

function TKServerEnvironment.GetURL: string;
begin
	Result := Values['URL'];
end;

function TKServerEnvironment.GetHTTPReferer: string;
begin
	Result := Values['HTTP_REFERER'];
end;

function TKServerEnvironment.GetHTTPUserAgent: string;
begin
	Result := Values['HTTP_USER_AGENT'];
end;

function TKServerEnvironment.GetHTTPAcceptLanguage: string;
begin
	Result := Values['HTTP_ACCEPT_LANGUAGE'];
end;

function TKServerEnvironment.GetLocalAddr: string;
begin
	Result := Values['LOCAL_ADDR'];
end;

{
--------------------------------------------------------------------------------
-------------------------------- Hook Control ----------------------------------
--------------------------------------------------------------------------------
}

{
	The THookControl class is responsible for managing hooks to a window
	procedure ( defined by a handle ). It will keep a list of hooks to
	the window procedure, and call every every hook whenever the window
	procedure	gets called.

	How it works:

		. Whenever a user wants to hook to a window procedure, he will
			call the HookWindow procedure passing the handle of the window
			he wants to hook and a pointer to a TKHookMethod method.

		. HookWindow will look for a THookControl object controlling
			this window; if it finds, the TKHookMethod method will be added
			to the list of callback methods of this THookControl object;
			if it doesn't find, a new THookControl object will be created
			to control the window procedure associated to the handle.

		. UnHookWindow will undo whatever HookWindow has done. Just
			pass the handle and a pointer to a TKHookMethod method.

		. To execute an old window procedure, use CallWndProc passing
			the handle of the window and the message to process.

	This class is not published. It is for internal use only.
	THookControl objects are automatically created and destroyed as
	necessary whenever HookWindow and UnHookWindow procedures are
	called.

}

{-------------------------- Internal Implementation ----------------------------}

type

	THookControl = class( TObject )

	private
		FHandle: THandle;
		FProcListAfter: TList;
		FProcListBefore: TList;
		FOldWindowProc: TFNWndProc;
		FHandleDestroyed: Boolean;
		
		procedure CaptureWndProc;
		procedure ReleaseWndProc;
		procedure NewWindowProc( var Message: TMessage );

	public
		destructor Destroy; override;
		constructor Create( AHandle: THandle );

		procedure CallWindowProc( var Message: TMessage );

		property Handle: THandle
						 read FHandle write FHandle;
		property ProcListBefore: TList
						 read FProcListBefore write FProcListBefore;
		property ProcListAfter: TList
						 read FProcListAfter write FProcListAfter;

	end;

var

	HookControls: TList = nil;

constructor THookControl.Create( AHandle: THandle );
{
	. AHandle defines the window procedure to hook to;
	. FProcList is the list of hooks to be called from within
		the replacement window procedure;
	. CaptureWndProc will capture the window procedure and replace it
		with a new one from where the hook methods get called;
}
begin
	inherited Create;
	FHandle := AHandle;
	FProcListAfter := TList.Create;
	FProcListBefore := TList.Create;
	CaptureWndProc;
	HookControls.Add( Self );
	FHandleDestroyed := false;
end;

destructor THookControl.Destroy;
{
	. Unhook any methods that were not excluded from FProcList;
	. Free FProcList;
	. Replace the window procedure with the captured one;
	. Delete reference of the current THookControl from HookControls;
}
var
	i: Integer;
	kHook: ^TKHookMethod;
begin
	for i := FProcListBefore.Count - 1  downto 0 do
	begin
		kHook := FProcListBefore[i];
		UnHookWindow( MakeHookData( Handle, kHook^, nil ) );
	end;
	for i := FProcListAfter.Count - 1  downto 0 do
	begin
		kHook := FProcListAfter[i];
		UnHookWindow( MakeHookData( Handle, nil, kHook^ ) );
	end;
	FreeClean( FProcListBefore );
	FreeClean( FProcListAfter );
	if ( not FHandleDestroyed ) then
		ReleaseWndProc;
	if ( HookControls.IndexOf( Self ) > -1 ) then
		HookControls.Delete( HookControls.IndexOf( Self ) );
	inherited Destroy;
end;

procedure THookControl.CaptureWndProc;
begin
	FOldWindowProc := TFNWndProc( SetWindowLong( FHandle, GWL_WNDPROC,
		LongInt( MakeObjectInstance( NewWindowProc ) ) ) );
end;

procedure THookControl.ReleaseWndProc;
var
	p: Pointer;
begin
	if CheckHandle( FHandle ) then
	begin
		p := Pointer( SetWindowLong( FHandle, GWL_WNDPROC,
			LongInt( FOldWindowProc ) ) );
		ForcePointer( p );
		FreeObjectInstance( p );
	end;
end;

procedure THookControl.CallWindowProc( var Message: TMessage );
begin
	with Message do
		Result := Windows.CallWindowProc( FOldWindowProc, FHandle,
			Msg, wParam, lParam );
end;

procedure THookControl.NewWindowProc( var Message: TMessage );

	function MakeHookInfo( Process, IsBefore: Boolean ): TKHookInfo;
	begin
		ZeroMemory( @Result, SizeOf( TKHookInfo ) );
		Result.Handle := Handle;
		Result.Process := Process;
		Result.IsHookBefore := IsBefore;
	end;
	
var
	i: Integer;
	hi: TKHookInfo;
	kHook: ^TKHookMethod;
	bProcess, bDefault: Boolean;
begin
{ if the handle has been destroyed and the procedure gets called, let only
	the original window procedure do its task. }
	if FHandleDestroyed then
	begin
		CallWindowProc( Message );
		Exit;
	end;
	bDefault := true;
	bProcess := true;
{ process before hooks }
	for i := 0 to FProcListBefore.Count - 1 do
	begin
		kHook := FProcListBefore[i];
		hi := MakeHookInfo( bProcess, true );
		kHook^( Message, hi );
		bProcess := hi.Process;
		bDefault := ( bDefault and bProcess );
	end;
{ default processing??? }
	if bDefault then
		CallWindowProc( Message );
{ Test a WM_DESTROY message }
	if ( Message.Msg = WM_DESTROY ) then
	begin
		ReleaseWndProc;
		FHandleDestroyed := true;
		Exit;
	end;
{ After hooks should NOT proceed with processing after WM_DESTROY; they'll be
  called only when CallWindowProc is called }
	if bDefault then
		for i := 0 to FProcListAfter.Count - 1 do
		begin
			kHook := FProcListAfter[i];
			hi := MakeHookInfo( false, false );
			kHook^( Message, hi );


		end;
end;

{--------------------------- Public Implementation -----------------------------}

function MakeHookData( AHandle: THandle; HookBefore, HookAfter: TKHookMethod ): THookData;
begin
	ForceHandle( AHandle );
	ZeroMemory( @Result, SizeOf( THookData ) );
	if ( not CheckReference( @HookBefore ) ) then
	begin
		ForceReference( @HookAfter );
		Result.HookBeforeProc := nil;
		Result.HookAfterProc := HookAfter;
	end
	else
	begin
		Result.HookBeforeProc := HookBefore;
		Result.HookAfterProc := HookAfter;
	end;
	Result.Handle := AHandle;
end;

function HookWindow( HookData: THookData ): Boolean;
var
	i: Integer;
	hc: THookControl;
	kHook: ^TKHookMethod;
begin
	with HookData do
	begin
		ForceHandle( Handle );
		if ( not CheckReference( @HookBeforeProc ) ) then
			ForceReference( @HookAfterProc );
	end;
{ Handle is OK, and there is at least on method to hook }
	hc := nil;
	Result := false;
	if ( not CheckObject( HookControls ) ) then
		HookControls := TList.Create;
	for i := 0 to HookControls.Count - 1 do
		if ( THookControl( HookControls[i] ).Handle = HookData.Handle ) then
		begin
			hc := THookControl( HookControls[i] );
			Break;
		end;
	if ( not CheckObject( hc ) ) then
		hc := THookControl.Create( HookData.Handle );
	with HookData do
	begin
		if CheckReference( @HookBeforeProc ) then
		begin
			for i := 0 to hc.ProcListBefore.Count - 1 do
			begin
				kHook := hc.ProcListBefore[i];
				if ( TMethod( kHook^ ).Code = TMethod( HookBeforeProc ).Code ) and
					 ( TMethod( kHook^ ).Data = TMethod( HookBeforeProc ).Data ) then
				begin
					Result := true;
					Break;
				end;
			end;
			if ( not Result ) then
			begin
				New( kHook );
				kHook^ := HookBeforeProc;
				hc.ProcListBefore.Add( kHook );
				Result := true;
			end;
		end;
		if CheckReference( @HookAfterProc ) then
		begin
      Result := false;
			for i := 0 to hc.ProcListAfter.Count - 1 do
			begin
				kHook := hc.ProcListAfter[i];
				if ( TMethod( kHook^ ).Code = TMethod( HookAfterProc ).Code ) and
					 ( TMethod( kHook^ ).Data = TMethod( HookAfterProc ).Data ) then
				begin
					Result := true;
					Break;
				end;
			end;
			if ( not Result ) then
			begin
				New( kHook );
				kHook^ := HookAfterProc;
				hc.ProcListAfter.Add( kHook );
				Result := true;
			end;
		end;
	end;
end;

function UnHookWindow( HookData: THookData ): Boolean;
var
	i: Integer;
	hc: THookControl;
	kHook: ^TKHookMethod;
begin
	if ( not CheckObject( HookControls ) ) then
	begin
		Result := false;
		Exit;
	end;
	with HookData do
	begin
		ForceHandle( Handle );
		if ( not CheckReference( @HookBeforeProc ) ) then
			ForceReference( @HookAfterProc );
	end;
{ Handle is OK, and there is at least on method to unhook }
	hc := nil;
	Result := false;
	for i := 0 to HookControls.Count - 1 do
		if ( THookControl( HookControls[i] ).Handle = HookData.Handle ) then
		begin
			hc := THookControl( HookControls[i] );
			Break;
		end;
	if ( not CheckObject( hc ) ) then
	begin
		Result := false;
		Exit;
	end;
	with HookData do
	begin
		if CheckReference( @HookBeforeProc ) then
			for i := 0 to hc.ProcListBefore.Count - 1 do
			begin
				kHook := hc.ProcListBefore[i];
				if ( TMethod( kHook^ ).Code = TMethod( HookData.HookBeforeProc ).Code ) and
					 ( TMethod( kHook^ ).Data = TMethod( HookData.HookBeforeProc ).Data ) then
				begin
					Dispose( kHook );
					hc.ProcListBefore.Delete( i );
					Result := true;
					Break;
				end;
			end;
		if CheckReference( @HookAfterProc ) then
		begin
			Result := false;
			for i := 0 to hc.ProcListAfter.Count - 1 do
			begin
				kHook := hc.ProcListAfter[i];
				if ( TMethod( kHook^ ).Code = TMethod( HookData.HookAfterProc ).Code ) and
					 ( TMethod( kHook^ ).Data = TMethod( HookData.HookAfterProc ).Data ) then
				begin
					Dispose( kHook );
					hc.ProcListAfter.Delete( i );
					Result := true;
					Break;
				end;
			end;
		end;
	end;
end;

procedure CallWndProc( AHandle: THandle; var Message: TMessage );
var
	i: Integer;
begin
	ForceHandle( AHandle );
	ForceObject( HookControls );
	for i := 0 to HookControls.Count - 1 do
		if ( THookControl( HookControls[i] ).Handle = AHandle ) then
			with THookControl( HookControls[i] ) do
			begin
				CallWindowProc( Message );
				Exit;
			end;
end;

procedure DestroyHookControls;
var
	i: Integer;
begin
	if CheckObject( HookControls ) then
	begin
		for i := HookControls.Count - 1 downto 0 do
			THookControl( HookControls[i] ).Free;
		FreeClean( HookControls );
	end;
end;

{
--------------------------------------------------------------------------------
------------------------------ PropList Support --------------------------------
--------------------------------------------------------------------------------
}

constructor TKPropList.Create;
{ make the constructor virtual from now on to allow polymorphism }
begin
	inherited Create;
end;

destructor TKPropList.Destroy;
begin
	FStrings.Free;
	inherited Destroy;
end;

function TKPropList.GetPropNamesList: TStrings;
var
	i: Integer;
begin
	if ( not CheckObject( FStrings ) ) then
		FStrings := TStringList.Create
	else
		FStrings.Clear;
	for i := 0 to Count - 1 do
		FStrings.Add( PropNames[i] );
	Result := FStrings;
end;

function TKPropList.GetPropNames( Index: Integer ): ShortString;
begin
	Result := PKPropInfo( Items[Index] )^.PropName;
end;

function TKPropList.Add( Item: PKPropInfo ): Integer;
begin
	Result := inherited Add( Item );
end;

function TKPropList.Get( Index: Integer ): Pointer;
begin
	Result := PKPropInfo( inherited Items[Index] )^.PropValue;
end;

procedure TKPropList.Put( Index: Integer; Item: Pointer );
begin
	RaiseException( EKPropList, sErrPLReadOnlyItems );
end;

function TKPropList.InternalAdd( const PropName: ShortString; PropTypeKind: TTypeKind;
	const PropValue ): PKPropInfo;
begin
	Result := New( PKPropInfo );
	try
		ZeroMemory( Result, SizeOf( TKPropInfo ) );
		Result.PropName := PropName;
		Result.PropTypeKind := PropTypeKind;
		Result.PropValue := @PropValue;
	except
		Dispose( Result );
		raise;
  end;
end;

function TKPropList.AddSet( const PropName: ShortString; const SetValue ): Integer;
begin
  Result := Add( InternalAdd( PropName, tkSet, SetValue ) );
end;

function TKPropList.AddClass( const PropName: ShortString; ClassValue: TObject ): Integer;
begin
  Result := Add( InternalAdd( PropName, tkClass, LongInt( ClassValue ) ) );
end;

function TKPropList.AddOrdinal( const PropName: ShortString; OrdValue: LongInt ): Integer;
begin
  Result := Add( InternalAdd( PropName, tkInteger, OrdValue ) );
end;

function TKPropList.AddString( const PropName: ShortString; const StrValue: string ): Integer;
begin
  Result := Add( InternalAdd( PropName, tkLString, Pointer( StrValue )^ ) );
end;

function TKPropList.AddFloat( const PropName: ShortString; FltValue: Extended ): Integer;
begin
  Result := Add( InternalAdd( PropName, tkFloat, FltValue ) );
end;

function TKPropList.AddVariant( const PropName: ShortString; VarValue: Variant ): Integer;
begin
  Result := Add( InternalAdd( PropName, tkVariant, PVariant( @VarValue )^ ) );
end;

function TKPropList.AddMethod( const PropName: ShortString; MthValue: TMethod ): Integer;
begin
  Result := Add( InternalAdd( PropName, tkMethod, PMethod( @MthValue )^ ) );
end;

procedure TKPropList.DestroyPropInfo( Index: Integer );
begin
  if ( Index <  0 ) or ( Index >= Count ) then
		Error( SListIndexError, Index );
  Dispose( PKPropInfo( Items[Index] ) );
  Items[Index] := nil;
end;

procedure TKPropList.Clear;
var
	i: Integer;
begin
	for i := Count - 1 downto 0 do
		DestroyPropInfo( i );
	inherited Clear;
end;

procedure TKPropList.Delete( Index: Integer );
begin
	DestroyPropInfo( Index );
	inherited Delete( Index );
end;

function SetPubPropsEx( Obj: TObject; PropValueList: TKPropList;
	ProcessChildren: Boolean ): Boolean;
var
	i: Integer;
begin
	ForceObjects( [Obj, PropValueList] );
	Result := true;
	for i := 0 to PropValueList.Count - 1 do
		Result := Result and SetPubProp( Obj, PropValueList.PropNames[i],
			PropValueList.Items[i]^, ProcessChildren );
end;

{
--------------------------------------------------------------------------------
----------------------------- Caption Icon Support -----------------------------
--------------------------------------------------------------------------------
}

constructor TKCustomSystemIcon.Create( AOwner: TKWindowInfo );
begin
	ForceObject( AOwner );
	FOwner := AOwner;
	inherited Create;
	SetRectEmpty( FRect );
end;

procedure TKCustomSystemIcon.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKCustomSystemIcon ) then
	begin
		FIcon := TKCustomSystemIcon( Source ).Icon;
		FSystemMenu := TKCustomSystemIcon( Source ).SystemMenu;
	end
	else if CheckObjectClass( Source, TPopupMenu ) then
		FSystemMenu := TPopupMenu( Source )
	else if CheckObjectClass( Source, TIcon ) then
		FIcon := TIcon( Source )
	else
		inherited Assign( Source );
end;

procedure TKCustomSystemIcon.SysMenuPopup;
var
	rt: TRect;
	pt: TPoint;
begin
	rt := Owner.ClientRect;
	pt.Y := rt.Top - 1;
	pt.X := rt.Left;
	if ( Owner.WindowState = wsMinimized ) then
	begin
{ no support for owner drawn menus yet!!! }
		pt.Y := -FSystemMenu.Items.Count *
			GetSystemMetrics( SM_CYMENUSIZE ) + 1;
		ClientToScreen( Owner.Handle, pt );
	end;
	if CheckObject( FSystemMenu ) then
		FSystemMenu.Popup( pt.X, pt.Y );
end;

procedure TKCustomSystemIcon.Draw( dc: HDC; var rt: TRect );
var
	smico: HICON;
	iSize: Integer;
begin
	if ( not CheckObject( FIcon ) ) then
		Exit;
	iSize := GetSystemMetrics( SM_CXSMICON );
	smico := CopyImage( FIcon.Handle, IMAGE_ICON, iSize, iSize, LR_COPYFROMRESOURCE );
	try
		with rt do
			DrawIconEx( dc, Left + 1, Top + 1, smico, 0, 0, 0, 0, DI_NORMAL );
		with FRect do
		begin
			Left := rt.Left;
			Right := Left + iSize;
			Top := rt.Top;
			Bottom := Top + iSize;
		end;
	finally
		DestroyIcon( smico );
	end;
	Inc( rt.Left, iSize + 1 );
end;

function TKCustomSystemIcon.CaptionRect: TRect;
begin
	Result := Owner.CaptionRect;
end;

function TKCustomSystemIcon.ButtonAtPoint( pt: TPoint ): Boolean;
var
	rt: TRect;
begin
	if ( not CheckObject( Icon ) ) then
	begin
		Result := false;
		Exit;
	end;
	rt := CaptionRect;
	pt.Y := pt.Y + 1;
	if ( Owner.WindowState <> wsMinimized ) then
		pt.Y := pt.Y + rt.Bottom - rt.Top;
	Result := PtInRect( Rect, pt );
end;

{
--------------------------------------------------------------------------------
------------------------------- TKCaptionButton --------------------------------
--------------------------------------------------------------------------------
}

constructor TKCaptionButton.Create( Collection: TCollection );
begin
	ForceObjectClass( Collection, TKCaptionButtons );
	inherited Create( Collection );
	FOnChange := nil;
	FEnabled := true;
	FVisible := true;
	FPushed := false;
	FKind := cbkClose;
	FDrawPushed := false;
	SetRectEmpty( FRect );
	FInternalState := KCBS_DEFAULT;
	FWindowInfo := TKCaptionButtons( Collection ).Owner;
end;

procedure TKCaptionButton.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKCaptionButton ) then
	begin
		FKind := TKCaptionButton( Source ).FKind;
		FEnabled := TKCaptionButton( Source ).FEnabled;
		FVisible := TKCaptionButton( Source ).FVisible;
	end
	else
		inherited Assign( Source );
	Changed( false );
end;

procedure TKCaptionButton.SetEnabled( Value: Boolean );
begin
	if ( Value <> FEnabled ) then
	begin
		FEnabled := Value;
		Changed( false );
	end;
end;

procedure TKCaptionButton.SetVisible( Value: Boolean );
begin
	if ( Value <> FVisible ) then
	begin
		FVisible := Value;
		Changed( false );
	end;
end;

function TKCaptionButton.GetBorderStyle: TFormBorderStyle;
begin
	Result := FWindowInfo.BorderStyle;
end;

function TKCaptionButton.GetWindowState: TWindowState;
begin
	Result := FWindowInfo.WindowState;
end;

procedure TKCaptionButton.SetPushed( Value: Boolean );
var
	i: Integer;
begin
	if ( Value <> FPushed ) then
	begin
		FPushed := Value;
		if FPushed then
			with Collection do
			begin
				for i := 0 to Count - 1 do
					if ( Self <> Items[i] ) then
						( Items[i] as TKCaptionButton ).Pushed := false;
				DrawPushed := true;
			end;
		Changed( false );
	end;
end;

procedure TKCaptionButton.SetDrawPushed( Value: Boolean );
var
	i: Integer;
begin
	if ( Value <> FDrawPushed ) then
	begin
		FDrawPushed := Value;
		if FDrawPushed then
			with Collection do
				for i := 0 to Count - 1 do
					if ( Self <> Items[i] ) then
						( Items[i] as TKCaptionButton ).DrawPushed := false;
		Changed( false );
	end;
end;

procedure TKCaptionButton.SetKind( Value: TKCaptionButtonKind );
begin
	if ( Value <> FKind ) then
	begin
		FKind := Value;
		Changed( false );
	end;
end;

procedure TKCaptionButton.Click;
begin
	DoClick;
end;

procedure TKCaptionButton.DoClick;
begin
	if Assigned( FOnClick ) then
		FOnClick( Self );
end;

procedure TKCaptionButton.Changed( AllItems: Boolean );
begin
	inherited Changed( AllItems );
	if Assigned( FOnChange ) then
		FOnChange( Self );
end;

function TKCaptionButton.Draw( Handle: HWnd; dc: HDC;
	var rt: TRect ): Boolean;
const
	DFCS_KIND: array[TKCaptionButtonKind] of UINT =
		( DFCS_CAPTIONCLOSE, DFCS_CAPTIONMAX, DFCS_CAPTIONMIN,
			DFCS_CAPTIONHELP, DFCS_SCROLLUP, DFCS_BUTTONPUSH );
	DFCS_ENABLED_STATE: array[Boolean] of UINT =
		( DFCS_INACTIVE, 0 );
	DFCS_PUSH_STATE: array[Boolean] of UINT =
		( 0, DFCS_PUSHED );
var
	Flags: UINT;
	srcRect: TRect;
	iWidth: Integer;
	bs: TFormBorderStyle;
begin
	Result := ( not FVisible );
	if Result then
	begin
		Result := not ( not ( Result ) );
		Exit;
	end;
{ retrieve the BorderStyle of the current window }
	bs := BorderStyle;
{ A ToolWindow can only have a close button... }
	if ( bs in [bsToolWindow, bsSizeToolWin] ) and ( Kind <> cbkClose ) then
	begin
		FVisible := false;
		Exit;
	end;
	srcRect := rt;
	if ( bs in [bsToolWindow, bsSizeToolWin] ) then
{ Has to be the close button }
	begin
		with srcRect do
			Left := Right - WindowInfo.ButtonWidth + 2;
		Flags := DFCS_CAPTIONCLOSE or DFCS_PUSH_STATE[DrawPushed] or
			DFCS_ENABLED_STATE[Enabled];
		Move( srcRect, FRect, SizeOf( TRect ) );
		DrawFrameControl( dc, srcRect, DFC_CAPTION, Flags );
		rt.Right := srcRect.Left - 5;
		Result := true;
	end
	else
{ Can be any of the button kinds }
	begin
{ Get the kind of button being painted }
		Flags := DFCS_KIND[Kind];
		if ( Kind = cbkMaximize ) and ( WindowState = wsMaximized ) then
		begin
			Flags := DFCS_CAPTIONRESTORE;
			FInternalState := KCBS_RESTORE;
		end
		else if ( Kind = cbkMinimize ) and ( WindowState = wsMinimized ) then
		begin
			Flags := DFCS_CAPTIONRESTORE;
			FInternalState := KCBS_RESTORE;
		end
		else if ( Kind = cbkRollup ) and ( FInternalState = KCBS_INDICATEDN ) then
			Flags := DFCS_SCROLLDOWN
		else
			FInternalState := KCBS_DEFAULT;
{ Add the adequate flags for enabling and pushing the button }
		Flags := Flags or DFCS_PUSH_STATE[DrawPushed] or DFCS_ENABLED_STATE[Enabled];
		iWidth := WindowInfo.ButtonWidth - 2;
		if ( Odd( iWidth ) xor Odd( rt.Bottom - rt.Top ) ) then
			Inc( iWidth );
		srcRect.Left := srcRect.Right - iWidth - 2;
		if ( Kind = cbkClose ) then
{ Close button }
		begin
			Move( srcRect, FRect, SizeOf( TRect ) );
			DrawFrameControl( dc, srcRect, DFC_CAPTION, Flags );
			OffsetRect( srcRect, -iWidth - 4, 0 );
			Dec( rt.Right, iWidth + 4 );
			Result := true;
		end
		else if ( Kind = cbkMaximize ) then
{ Maximize/Restore button }
		begin
			Move( srcRect, FRect, SizeOf( TRect ) );
			DrawFrameControl( dc, srcRect, DFC_CAPTION, Flags );
			OffsetRect( srcRect, -iWidth - 2, 0 );
			Dec( rt.Right, iWidth + 2 );
			Result := true;
		end
		else if ( Kind = cbkMinimize ) then
{ Minimize button }
		begin
			Move( srcRect, FRect, SizeOf( TRect ) );
			DrawFrameControl( dc, srcRect, DFC_CAPTION, Flags );
			OffsetRect( srcRect, -iWidth - 2, 0 );
			Dec( rt.Right, iWidth + 2 );
			Result := true;
		end
		else if ( Kind = cbkHelp ) then
{ Help button }
		begin
			Move( srcRect, FRect, SizeOf( TRect ) );
			DrawFrameControl( dc, srcRect, DFC_CAPTION, Flags );
			OffsetRect( srcRect, -iWidth - 2, 0 );
			Dec( rt.Right, iWidth + 2 );
			Result := true;
		end
		else if ( Kind = cbkRollup ) then
		begin
			Move( srcRect, FRect, SizeOf( TRect ) );
			DrawFrameControl( dc, srcRect, DFC_SCROLL, Flags );
			OffsetRect( srcRect, -iWidth - 2, 0 );
			Dec( rt.Right, iWidth + 2 );
			Result := true;
		end
		else if ( Kind = cbkCustom ) then
		begin
			Move( srcRect, FRect, SizeOf( TRect ) );
			DrawFrameControl( dc, srcRect, DFC_BUTTON, Flags );
			if Assigned( FOnCustomButtonDraw ) then
				FOnCustomButtonDraw( Self, dc, srcRect );
			OffsetRect( srcRect, -iWidth - 2, 0 );
			Dec( rt.Right, iWidth + 2 );
			Result := true;
		end;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------------- TKCaptionButtons -------------------------------
--------------------------------------------------------------------------------
}

constructor TKCaptionButtons.Create( AOwner: TKWindowInfo );
begin
	ForceObject( AOwner );
	FOwner := AOwner;
	inherited Create( TKCaptionButton );
end;

procedure TKCaptionButtons.SetItem( Index: Integer; Value: TKCaptionButton );
begin
	inherited SetItem( Index, Value );
end;

function TKCaptionButtons.GetItemPushed: TKCaptionButton;
var
	i: Integer;
begin
	Result := nil;
	for i :=  0 to Count - 1 do
		if Items[i].Pushed then
		begin
			Result := Items[i];
			Exit;
		end;
end;

function TKCaptionButtons.GetItem( Index: Integer ): TKCaptionButton;
begin
	Result := TKCaptionButton( inherited GetItem( Index ) );
end;

function TKCaptionButtons.Add( Kind: TKCaptionButtonKind ): TKCaptionButton;
begin
	if KindExists( Kind ) then
		RaiseExceptionFmt( EKCaptionButtons, sErrKindAlreadyExists, [EnumName( Cardinal( Kind ),
			TypeInfo( TKCaptionButtonKind ) )] );
	Result := TKCaptionButton( inherited Add );
	Result.Kind := Kind;
end;

function TKCaptionButtons.GetButton( Index: TKCaptionButtonKind ): TKCaptionButton;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Count - 1 do
		if ( Items[i].Kind = Index ) then
		begin
			Result := Items[i];
			Exit;
		end;
end;

procedure TKCaptionButtons.Draw( dc: HDC; var rt: TRect );

	function PaintButton( AKind: TKCaptionButtonKind; var src: TRect ): Boolean;
	var
		cbn: TKCaptionButton;
	begin
		Result := false;
		cbn := Buttons[AKind];
		if CheckObject( cbn ) then
			Result := cbn.Draw( Owner.Handle, dc, src );
	end;

var
	srcRect: TRect;
begin
	if ( not CheckWinNT ) and ( dc <= 0 ) then
		Exit;

	srcRect := rt;
	InflateRect( srcRect, -2, -2 );

{ Dispatch the drawing procedure to the appropriate buttons }
{ always paint the close button- if there is one }
	PaintButton( cbkClose, srcRect );
{$BOOLEVAL ON}
{ always paint the minimize/maximize buttons- if they exist }
	if not ( PaintButton( cbkMaximize, srcRect ) or PaintButton( cbkMinimize, srcRect ) ) then
{ the help and rollup buttons should only be painted if there are
	neither maximize nor minimize buttons }
	begin
		if not PaintButton( cbkHelp, srcRect ) then
		begin
			if KindExists( cbkHelp ) then
				Buttons[cbkHelp].Visible := false;
{ the rollup button should be painted if the help button wasn't }
			if ( not PaintButton( cbkRollup, srcRect ) ) then
				if KindExists( cbkRollup ) then
					Buttons[cbkRollup].Visible := false;
		end
{ the rollup button should not be painted when there is a help button }
		else if KindExists( cbkRollup ) then
			Buttons[cbkRollup].Visible := false;
{$BOOLEVAL OFF}
{ always paint the custom button- if there is one }
		PaintButton( cbkCustom, srcRect );
	end
	else
	begin
		if KindExists( cbkHelp ) then
			Buttons[cbkHelp].Visible := false;
		if KindExists( cbkRollup ) then
			Buttons[cbkRollup].Visible := false;
	end;
	Dec( rt.Right, 3 );
end;

function TKCaptionButtons.IsPushed( pt: TPoint ): Boolean;
var
	cb: TKCaptionButton;
begin
	Result := ButtonAtPoint( pt, cb ) and ( cb.Pushed );
end;

function TKCaptionButtons.LeftMostButton: Integer;
var
	i: Integer;
begin
	Result := Owner.CaptionRect.Right;
	for i := 0 to Count - 1 do
		if TKCaptionButton( Items[i] ).Visible then
			with TKCaptionButton( Items[i] ) do
				if ( Rect.Left < Result ) then
					Result := Rect.Left;
end;

function TKCaptionButtons.AnyButtonPushed: Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := 0 to Count - 1 do
		if ( Items[i].Visible and Items[i].Pushed ) then
		begin
			Result := true;
			Exit;
		end;
end;

function TKCaptionButtons.CaptionRect: TRect;
begin
	Result := Owner.CaptionRect;
end;

function TKCaptionButtons.FindButton( pt: TPoint ): TKCaptionButton;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Count - 1 do
		if Items[i].Visible then
			if PtInRect( Items[i].Rect, pt ) then
			begin
				Result := Items[i];
				Exit;
			end;
end;

function TKCaptionButtons.ButtonAtPoint( pt: TPoint; var cbn: TKCaptionButton ): Boolean;
var
	rt: TRect;
begin
	rt := CaptionRect;
	pt.Y := pt.Y + 1;
	if ( Owner.WindowState <> wsMinimized ) then
		pt.Y := pt.Y + rt.Bottom - rt.Top;
	cbn := FindButton( pt );
	Result := CheckObject( cbn );
end;

function TKCaptionButtons.KindExists( bk: TKCaptionButtonKind ): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := 0 to Count - 1 do
		if ( TKCaptionButton( Items[i] ).Kind = bk ) then
		begin
			Result := true;
			Exit;
		end;
end;

{
-------------------------------------------------------------------------
-------------------------- CustomFormPainter ----------------------------
-------------------------------------------------------------------------
}

type

{$HINTS OFF}
	TControlSHack = class( TComponent )
	private
		FParent: TWinControl;
		FWindowProc: TWndMethod;
		FLeft: Integer;
		FTop: Integer;
		FWidth: Integer;
		FHeight: Integer;

	end;
{$HINTS ON}

constructor TKCustomFormPainter.Create( AHandle: HWnd );
{ This control should not be created in design time. It is up to the
	programmer to avoid this }
begin
	ForceAnyPackagesRunning( ClassName, [perDialogs, perDBDialogs, perForms] );

{ Create the object before the inherited create just to make sure
	we've recieved a valid handle }
	FWindowManager := TKWindowManager.Create( AHandle );
	inherited Create;

{ Object management }
	FHandle := AHandle;
	FPaintActive := true;
	FCaption := GetCaption;
	FUsePaintActive := false;
	FMouseCaptured := false;
	FHelpButtonDown := false;
	FMouseButtonDown := kmbNone;
	FButtons := TKCaptionButtons.Create( FWindowManager );
	FSystemIcon := TKSystemIcon.Create( FWindowManager );

{ Visual support }
	FActiveFont := TFont.Create;
	with ActiveFont do
	begin
		Assign( FWindowManager.SystemCaptionFont );
		Color := clWhite;
	end;
	FInactiveFont := TFont.Create;
	with InactiveFont do
	begin
		Assign( FWindowManager.SystemCaptionFont );
		Color := clYellow;
	end;
	FBackground := TKGradient.Create( nil, nil );
	with FBackground do
	begin
		Steps := 64;
		BeginColor := clYellow;
		EndColor := clRed;
		GradientStyle := gsHorizontal;
	end;
	FActiveCaption := TKGradient.Create( nil, nil );
	with FActiveCaption do
	begin
		Steps := 64;
		BeginColor := clNavy;
		EndColor := $FF9090;
		GradientStyle := gsDoubleVertical;
	end;
	FInactiveCaption := TKGradient.Create( nil, nil );
	with FInactiveCaption do
	begin
		Steps := 64;
		BeginColor := clGray;
		EndColor := clSilver;
		GradientStyle := gsDoubleHorizontal;
	end;
	FMaximizing := false;

{ If the handle passed is owner of a MDIClient window, it means that
	the handle represents an MDI frame, and we've got to give it a special
	treatment }
	if ( FWindowManager.FormStyle = fsMDIForm ) then
	begin
		FNewClientWndProc := MakeObjectInstance( FormClientWndProc );
		FOldClientWndProc := Pointer( GetWindowLong( GetClientHandle, GWL_WNDPROC ) );
		SetWindowLong( GetClientHandle, GWL_WNDPROC, LongInt( FNewClientWndProc ) );
	end
	else
	begin
		FNewClientWndProc := nil;
		FOldClientWndProc := nil;
	end;

	FNewFormWndProc := MakeObjectInstance( FormWndProc );
	FOldFormWndProc := Pointer( GetWindowLong( Handle, GWL_WNDPROC ) );
	SetWindowLong( Handle, GWL_WNDPROC, LongInt( FNewFormWndProc ) );
end;

destructor TKCustomFormPainter.Destroy;
begin
	if ( FWindowManager.FormStyle = fsMDIForm ) then
	begin
		SetWindowLong( GetClientHandle, GWL_WNDPROC, LongInt( FOldClientWndProc ) );
		FreeObjectInstance( FNewClientWndProc );
	end;
	SetWindowLong( Handle, GWL_WNDPROC, LongInt( FOldFormWndProc ) );
	FreeObjectInstance( FNewFormWndProc );

	FreeClean( FButtons );
	FreeClean( FSystemIcon );
	FreeClean( FWindowManager );
	FreeClean( FActiveFont );
	FreeClean( FInactiveFont );
	FreeClean( FBackGround );
	FreeClean( FActiveCaption );
	FreeClean( FInactiveCaption );

	inherited Destroy;
end;

procedure TKCustomFormPainter.DoRollupAction( ra: LongInt );
begin
	if ( FWindowManager.WindowState <> wsNormal ) then
		Exit;
{ Rollup/Rolldown available only when the window is in normal state }
	with FWindowManager do
		if ( ra = KCBS_INDICATEDN ) then
		begin
			Sizeable := FOldSizeable;
			Height := FOldHeight;
		end
		else
		begin
			FOldSizeable := Sizeable;
			FOldHeight := Height;
			Height := CaptionHeight + FrameHeight;
			if ( BorderStyle <> bsSizeable ) then
				Height := Height + 3;
			Sizeable := false;
		end;
end;

procedure TKCustomFormPainter.DoSystemAction( ws: TWindowState );
const
	SHOW_ACTION: array[TWindowState] of Integer =
	( SW_SHOWNORMAL, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED );
begin
	with FWindowManager do
		if not ( Sizeable and Moveable ) then
			Exit;
	if ( ws = wsMaximized ) then
		FMaximizing := true;
	ShowWindow( FWindowManager.Handle, SHOW_ACTION[ws] );
	if ( ws <> wsMinimized ) and ( FBackGround.GradientStyle <> gsNone ) then
		Invalidate;
end;

procedure TKCustomFormPainter.DoHelpLeftButton;
var
	cbn: TKCaptionButton;
begin
	FHelpButtonDown := false;
	cbn := FButtons[cbkHelp];
	if CheckObject( cbn ) then
	begin
{ dispatch the left button helper }
		cbn.Pushed := false;
		cbn.DrawPushed := false;
	end;
	NCInvalidate;
end;

procedure TKCustomFormPainter.DoHelpRightButton;
var
	cbn: TKCaptionButton;
begin
	FHelpButtonDown := false;
	cbn := FButtons[cbkHelp];
	if CheckObject( cbn ) then
	begin
{ dispatch the right button helper }
		cbn.Pushed := false;
		cbn.DrawPushed := false;
	end;
	NCInvalidate;
end;

procedure TKCustomFormPainter.DoSystemMenuClick;
begin
	FSystemIcon.SysMenuPopup;
end;

procedure TKCustomFormPainter.DoSystemMenuDblClick;
begin
	PostMessage( FWindowManager.Handle, WM_CLOSE, 0, 0 );
end;

procedure TKCustomFormPainter.DoRightButtonClick( pt: TPoint );
begin
	Windows.ClientToScreen( FWindowManager.Handle, pt );
	with FSystemIcon do
		if CheckObject( SystemMenu ) then
			SystemMenu.Popup( pt.X, pt.Y );
end;

procedure TKCustomFormPainter.DoHelpButtonPush( cbn: TKCaptionButton );
begin
	FHelpButtonDown := true;
	if CheckObject( cbn ) then
		PostMessage( FWindowManager.Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0 );
end;

procedure TKCustomFormPainter.DoClose;
var
  bCanClose: Boolean;
begin
  bCanClose := true;
	if Assigned( FOnCloseQuery ) then
		FOnCloseQuery( Self, bCanClose );
	if bCanClose then
		PostMessage( FWindowManager.Handle, WM_CLOSE, 0, 0 );
end;

procedure TKCustomFormPainter.DoCaptionButtonClick( cbn: TKCaptionButton );
begin
	if ( cbn.Kind = cbkClose ) then
		DoClose
	else if ( cbn.Kind = cbkRollup ) then
	begin
		DoRollupAction( cbn.InternalState );
		if ( cbn.InternalState = KCBS_INDICATEDN ) then
			cbn.InternalState := KCBS_DEFAULT
		else
			cbn.InternalState := KCBS_INDICATEDN;
	end
	else if ( cbn.Kind = cbkMaximize ) then
	begin
		if ( cbn.InternalState = KCBS_RESTORE ) then
			DoSystemAction( wsNormal )
		else
			DoSystemAction( wsMaximized );
	end
	else if ( cbn.Kind = cbkMinimize ) then
	begin
		if ( cbn.InternalState = KCBS_RESTORE ) then
			DoSystemAction( wsNormal )
		else
			DoSystemAction( wsMinimized );
	end;
end;

function TKCustomFormPainter.GetClientHandle: HWnd;
begin
	Result := FWindowManager.ClientHandle;
end;

function TKCustomFormPainter.ClientToScreen( pt: TPoint ): TPoint;
begin
	Result := pt;
	Windows.ClientToScreen( Handle, Result );
end;

function TKCustomFormPainter.ScreenToClient( pt: TPoint ): TPoint;
begin
	Result := pt;
	Windows.ScreenToClient( Handle, Result );
end;

procedure TKCustomFormPainter.Invalidate;
begin
	InvalidateRect( Handle, nil, true );
end;

procedure TKCustomFormPainter.NCInvalidate;
begin
	SetWindowPos( Handle, 0, 0, 0, 0, 0, SWP_DRAWFRAME or SWP_NOACTIVATE or
		SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER );
end;

procedure TKCustomFormPainter.PaintCaption;
var
	dc: HDC;
begin
	dc := GetWindowDC( Handle );
	try
		PaintCaptionDC( dc );
	finally
		ReleaseDC( Handle, dc );
	end;
end;

function TKCustomFormPainter.PaintCaptionDC( dc: HDC ): TRect;

	procedure PaintCaptionText( dc: HDC; r: TRect );
	var
		hf: HFont;
		ocr: TColorRef;
		i,
		iMode,
		iFlags: Integer;
		sText: string;
	begin
		sText := Caption;
		Inc( r.Left, 2 );
		ocr := SetTextColor( dc, ColorToRGB( CaptionFont.Color ) );
		try
			iMode := SetBkMode( dc, TRANSPARENT );
			try
				hf := SelectObject( dc, CaptionFont.Handle );
				try
					iFlags := ( DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS );
					i := FButtons.LeftMostButton;
					if ValueBetween( i, r.Left, r.Right, True ) then
						r.Right := i;
					DrawTextEx( dc, PChar( FCaption ), -1, r, iFlags, nil );
				finally
					SelectObject( dc, hf );
				end;
			finally
				SetBkMode( dc, iMode );
			end;
		finally
			SetTextColor( dc, ocr );
		end;
	end;

	procedure PaintSolidCaptionRect( dc: HDC; r: TRect );
	var
		b,
		ob: HBRUSH;
	begin
		ob := 0;
		if FPaintActive then
			b := CreateSolidBrush( GetSysColor( COLOR_ACTIVECAPTION ) )
		else
			b := CreateSolidBrush( GetSysColor( COLOR_INACTIVECAPTION ) );
		try
			ob := SelectObject( dc, b );
			with r do
				PatBlt( dc, Left, Top, Right - Left, Bottom - Top, PATCOPY );
		finally
			if ( ob <> 0 ) then
				SelectObject( dc, ob );
			DeleteObject( b );
		end;
	end;

var
	rt: TRect;
	bmp,
	obmp: HBITMAP;
	bmpDC: HDC;
	w, h: Integer;
begin
	if ( not FUsePaintActive ) then
		FPaintActive := FWindowManager.Active;
	obmp := 0;
	rt := FWindowManager.CaptionRect;
	Result := rt;
	OffsetRect( rt, -rt.Left, -rt.Top );
	w := rt.Right - rt.Left;
	h := rt.Bottom - rt.Top;
	bmpDC := CreateCompatibleDC( dc );
	try
		bmp := CreateCompatibleBitmap( dc, w, h );
		try
			obmp := SelectObject( bmpDC, bmp );
			if ( CaptionGradient.GradientStyle <> gsNone ) then
				PaintGradientDC( bmpDC, @rt, CaptionGradient )
			else
				PaintSolidCaptionRect( bmpDC, rt );
			Inc( rt.Left, 1 );
			FButtons.Draw( bmpDC,	rt );
			FSystemIcon.Draw( bmpDC, rt );
			PaintCaptionText( bmpDC, rt );
			with Result do
				BitBlt( dc, Left, Top, w, h, bmpDC, 0, 0, SRCCOPY );
		finally
			if ( obmp <> 0 ) then
				SelectObject( bmpDC, obmp );
			DeleteObject( bmp );
		end;
	finally
		DeleteDC( bmpDC );
	end;
end;

function TKCustomFormPainter.GetCaptionFont: TFont;
begin
	if FPaintActive then
		Result := ActiveFont
	else
		Result := InactiveFont;
end;

procedure TKCustomFormPainter.NCPaint( var Msg: TMessage );
{ An application sends the WM_NCPAINT message to a window when its frame must be painted. }
var
	dc: HDC;
	rgn: HRGN;
	r, wr: TRect;
	bDelete: Boolean;
begin
	if FWindowManager.Visible then
	begin
		rgn := Msg.WParam;
		dc := GetWindowDC( Handle );
		GetWindowRect( Handle, wr );
		bDelete := ( SelectClipRgn( dc, rgn ) = ERROR );
		try
			if bDelete then
			begin
				rgn := CreateRectRgn( wr.Left, wr.Top, wr.Right, wr.Bottom );
				SelectClipRgn( dc, rgn );
			end;
			OffsetClipRgn( dc, -wr.Left, -wr.Top );
			r := PaintCaptionDC( dc );
			ExcludeClipRect( dc, r.Left, r.Top, r.Right, r.Bottom );
			OffsetClipRgn( dc, wr.Left, wr.Top );
			GetClipRgn( dc, rgn );
			with Msg do
				Result := DefWindowProc( Handle, Msg, rgn, LParam );
		finally
			if bDelete then
				DeleteObject( rgn );
			ReleaseDC( Handle, dc );
		end;
	end;
end;

function TKCustomFormPainter.GetCaptionGradient: TKGradient;
begin
	if FPaintActive then
		Result := ActiveCaption
	else
		Result := InactiveCaption;
end;

function TKCustomFormPainter.GetCaption: String;
var
	pcText: array[0..255] of char;
begin
	SetString( Result, pcText, GetWindowText( Handle, pcText, 255 ) );
end;

{ Guarantee VCL Windows compatibility }
procedure TKCustomFormPainter.ControlDispatch( var Message: TMessage );
var
	wc: TWinControl;
begin
	wc := FindControl( Handle );
	if CheckObject( wc ) then
		wc.Dispatch( Message )
end;

procedure TKCustomFormPainter.SetCaption( const Value: String );
begin
	if ( CompareText( FCaption, Value ) = 0 ) then
		SetWindowText( Handle, PChar( Value ) );
end;

procedure TKCustomFormPainter.FormClientWndProc( var Message: TMessage );

	procedure OldWndProc;
	begin
		with Message, FWindowManager do
			Result := CallWindowProc( FOldClientWndProc, ClientHandle, Msg,
				wParam, lParam );
	end;

	procedure DefaultProc;
	begin
		with Message, FWindowManager do
			Result := DefWindowProc( ClientHandle, Msg, WParam, LParam );
	end;

begin
	case Message.Msg of
		WM_ERASEBKGND:
{ => Need to repaint the client area; }
		begin
			PaintGradient( FBackGround );
			with FBackGround do
				BitBlt( TWMEraseBkGnd( Message ).DC, 0, 0, GradientBmp.Width,
					GradientBmp.Height, GradientBmp.Canvas.Handle, 0, 0, SRCCOPY );
{ 16.04.99 :: tell everyone we've processed the message }
			Message.Result := 1;
		end; { WM_ERASEBKGND }
		WM_VSCROLL,
		WM_HSCROLL:
		begin
			OldWndProc;
			InvalidateRect( GetClientHandle, nil, true );
{ 16.04.99 :: tell everyone we've processed the message }
			Message.Result := 0;
		end; { WM_VSCROLL, WM_HSCROLL }
	end;
	OldWndProc;
end;

procedure TKCustomFormPainter.FormWndProc( var Message: TMessage );
const
	WM_SYNCPAINT = $88;
	HITTEST_SET = [HTLEFT, HTRIGHT, HTBOTTOM, HTBOTTOMRIGHT,
								 HTBOTTOMLEFT, HTTOP, HTTOPRIGHT, HTTOPLEFT];

	procedure OldWndProc;
	begin
		with Message do
			Result := CallWindowProc( FOldFormWndProc, Handle, Msg,
				WParam, LParam );
	end;

	procedure DefaultProc;
	begin
		with Message do
			Result := DefWindowProc( Handle, Msg, WParam, LParam );
	end;

var
	rt: TRect;
	pt: TPoint;
	cb: TKCaptionButton;
	bDrawPushed,
	bPaintCaption: Boolean;
begin
	try
		if ( not FWindowManager.Visible ) then
		begin
			OldWndProc;
			Exit;
		end;

		if ( not FWindowManager.Enabled ) and
			 ( not ( Message.Msg in [WM_NCPAINT, WM_SETTEXT, WM_GETTEXT, WM_GETTEXTLENGTH,
				WM_ERASEBKGND, WM_NCACTIVATE] ) ) then
		begin
			OldWndProc;
			Exit;
		end;

		bPaintCaption := false;

		case Message.Msg of

			WM_SYNCPAINT:
				if CheckWinNT then
					PaintCaption; { WM_SYNCPAINT }

			WM_NCPAINT:
				NCPaint( Message ); { WM_NCPAINT }

			WM_SETTEXT:
			begin
				with TWMSetText( Message ) do
					FCaption := Text;
				PaintCaption;
			end; { WM_SETTEXT }

			WM_GETTEXT:
				with TWMGetText( Message ) do
				begin
					StrLCopy( Text, PChar( FCaption ), TextMax - 1 );
					Result := StrLen( Text ) + 1;
				end; { WM_GETTEXT }

			WM_GETTEXTLENGTH:
				with TWMGetTextLength( Message ) do
					Result := Length( FCaption ); { WM_GETTEXTLENGTH }

			WM_EXITSIZEMOVE:
	{
		In Controls.pas:
		 .this message is not processed;
		In Forms.pas:
		 .this message is not processed;
	}
			begin
	{ Make sure that after a move or resize the contents of the window are painted again }
				if ( FBackground.GradientStyle <> gsNone ) then
					Invalidate;
				PaintCaption;
	{ 16.04.99 :: tell everyone we've processed the message }
				Message.Result := 0;
			end; { WM_EXITSIZEMOVE }

			WM_WINDOWPOSCHANGED:
	{
		In Controls.pas:
		 .no WndProc or Default processing;
		 .the message handler paints part of the nonclient area and
				updates the following PRIVATE properties: FLeft, FTop, FRight,
				and FBottom ( this message is sent from the SetBounds method );
		In Forms.pas:
		 .this message is not processed;
	}
			begin
	{ Is this the handle of a Delphi Control? If it is, we'll have to
		update the coordinates of the form, without the implicit call to
		SetBounds. How ? With a private property hack!!! }
				ControlDispatch( Message );
				if FMaximizing then
					FMaximizing := false
				else
					PaintCaption;
			end; { WM_WINDOWPOSCHANGED }

			WM_NCACTIVATE:
	{
		In Controls.pas:
		 .this message is not processed
		In Forms.pas:
		 .no Default processing and no message handler;
		 .WndProc processes this message in a very special case:
				«	HandleAllocated and ( FBorderStyle = bsDialog ) and Ctl3D and
					Assigned( Ctl3DDlgFramePaint ) »
				the WndProc calls Ctl3DDlgFramePaint, which is a call to a
				CTL3D32.DLL; for all remaining situations, the default processing
				occurs; since we're painting everything, we don't need to call
				the OldWndProc;
		Since DefWndProc only paints the nonclient area, we don't need to
		call it either, since we're painting the nonclient area on our own.
	}
			begin
				FPaintActive := TWMNCActivate( Message ).Active;
	{ set the synchronization flag on; for now, we'll always use this flag,
		since the FWindow.Active property is sometimes incorrect. }
				FUsePaintActive := true;
				PaintCaption;
	{ 16.04.99 :: tell everyone we've processed the message }
				Message.Result := Ord( not FPaintActive );
			end; { WM_NCACTIVATE }

			WM_ERASEBKGND:
	{ => Need to repaint the client area; }
			begin
				with FWindowManager, ClientRect, FBackground do
					if ( GradientStyle <> gsNone ) then
					begin
						rt.TopLeft := ScreenToClient( TopLeft );
						rt.BottomRight := ScreenToClient( BottomRight );
						PaintGradientDC( TWMEraseBkGnd( Message ).DC, @rt, FBackGround );
	{ 16.04.99 :: tell everyone we've processed the message }
						Message.Result := 1;
					end
					else
						OldWndProc;
			end; { WM_ERASEBKGND }

			WM_NCHITTEST:
	{ => Need to check for a hit over one of our special areas: in the
			 case of a hit over one of these areas, we'll let Windows think
			 we've hit nothing: we'll later process the event; }
			begin
				with TWMNCHitTest( Message ) do
				begin
					cb := nil;
					pt := ScreenToClient( SmallPointToPoint( Pos ) );
	{ In our special areas, Windows will never know what's going on }
					if FButtons.ButtonAtPoint( pt, cb ) then
					begin
						DefaultProc;
						if ( Result = HTCAPTION ) and ( not cb.Enabled ) then
							Result := HTNOWHERE;
					end
					else if ( not FWindowManager.Sizeable ) then
					begin
						DefaultProc;
						if ( Result in HITTEST_SET ) then
							Result := HTNOWHERE;
					end
					else if ( not FWindowManager.Moveable ) then
					begin
						DefaultProc;
						if ( Result = HTCAPTION ) then
							Result := HTNOWHERE;
					end
					else
						OldWndProc;
				end;
			end; { WM_NCHITTEST }

			WM_MOUSEMOVE:
	{ => Need to synchronize the buttons' push drawstate; }
			begin
				if FButtons.AnyButtonPushed then
					with TWMMouseMove( Message ) do
					begin
						cb := nil;
						pt := SmallPointToPoint( Pos );
						if ( not FMouseCaptured ) then
							pt := ScreenToClient( pt );
						with FButtons, ItemPushed do
						begin
							bDrawPushed := ( Kind = cbkHelp ) and
														 ( ( ( FHelpButtonDown ) and
																 ( FMouseButtonDown = kmbNone ) ) or
															 ( ( IsPushed( pt ) ) and
																 ( not FHelpButtonDown ) and
																 ( FMouseButtonDown = kmbLeft ) ) );
							bDrawPushed := bDrawPushed or
														( Kind <> cbkHelp ) and
														( IsPushed( pt ) ) and
														( FMouseButtonDown = kmbLeft );
							if ( bDrawPushed <> DrawPushed ) then
							begin
								DrawPushed := bDrawPushed;
								NCInvalidate;
							end;
						end;
					end
				else
					OldWndProc;
			end; { WM_MOUSEMOVE }

			WM_RBUTTONDOWN:
			begin
				if FHelpButtonDown and ( not FMouseCaptured ) then
				begin
					DoHelpRightButton;
					PaintCaption;
	{ 16.04.99 :: tell everyone we've processed the message }
					Message.Result := 0;
				end
				else
					OldWndProc;
			end; { WM_RBUTTONDOWN }

			WM_NCRBUTTONDOWN:
	{ => Need to check for a mouse down outside our special elements; }
			begin
				if FHelpButtonDown then
				begin
					FHelpButtonDown := false;
					FButtons[cbkHelp].Pushed := false;
					FButtons[cbkHelp].DrawPushed := false;
					bPaintCaption := true;
				end;
				with TWMNCRButtonDown( Message ) do
				begin
					cb := nil;
					pt := ScreenToClient( Point( XCursor, YCursor ) );
	{ Make sure the mouse was not clicked over one of the caption buttons }
					if ( not FButtons.ButtonAtPoint( pt, cb ) ) then
					begin
	{ Capture the mouse }
						SetCapture( Handle );
						FMouseCaptured := true;
						FMouseButtonDown := kmbRight;
						FHelpButtonDown := false;
						PaintCaption;
	{ 16.04.99 :: tell everyone we've processed the message }
						Message.Result := 0;
					end
					else
					begin
						if bPaintCaption then
							PaintCaption;
						OldWndProc;
					end;
				end;
			end; { WM_NCRBUTTONDOWN }

			WM_RBUTTONUP:
	{ => Need to check for a click of one of our special elements; }
			begin
				if FMouseCaptured then
				begin
					with TWMRButtonUp( Message ) do
					begin
						cb := nil;
						pt := SmallPointToPoint( Pos );
		{ Make sure the mouse was not clicked over one of the caption buttons }
						if ( not FButtons.ButtonAtPoint( pt, cb ) ) then
						begin
							DoRightButtonClick( pt );
							PaintCaption;
						end;
					end;
	{ Since the mouse was captured, release it }
					ReleaseCapture;
					FMouseCaptured := false;
					FMouseButtonDown := kmbNone;
	{ 16.04.99 :: tell everyone we've processed the message }
					Message.Result := 0;
				end
				else
					OldWndProc;
			end; { WM_RBUTTONUP }

			WM_LBUTTONDOWN:
			begin
				if FHelpButtonDown and ( not FMouseCaptured ) then
				begin
					DoHelpLeftButton;
					PaintCaption;
	{ 16.04.99 :: tell everyone we've processed the message }
					Message.Result := 0;
				end
				else
					OldWndProc;
			end; { WM_LBUTTONDOWN }

			WM_NCLBUTTONDBLCLK:
			begin
				if FHelpButtonDown then
				begin
					FHelpButtonDown := false;
					FButtons[cbkHelp].Pushed := false;
					FButtons[cbkHelp].DrawPushed := false;
				end;
				with TWMNCLButtonDblClk( Message ) do
				begin
					cb := nil;
					pt := ScreenToClient( Point( XCursor, YCursor ) );
	{ System menu double click }
					if FSystemIcon.ButtonAtPoint( pt ) then
					begin
	{ no buttons will remain pushed }
						if FButtons.AnyButtonPushed then
							FButtons.ItemPushed.Pushed := false;
	{ no mouse button will remain down }
						FMouseButtonDown := kmbNone;
	{ Perform action }
						DoSystemMenuDblClick;
	{ 16.04.99 :: tell everyone we've processed the message }
						Message.Result := 0;
					end
	{ Caption double click }
					else if ( not FButtons.ButtonAtPoint( pt, cb ) ) then
					begin
	{ no buttons will remain pushed }
						if FButtons.AnyButtonPushed then
							FButtons.ItemPushed.Pushed := false;
	{ no mouse button will remain down }
						FMouseButtonDown := kmbNone;
	{ Perform action }
						with FWindowManager do
							if ( WindowState = wsNormal ) and
								 ( FButtons.KindExists( cbkMaximize ) ) and
								 ( FButtons[cbkMaximize].Visible ) and
								 ( FButtons[cbkMaximize].Enabled ) then
								DoSystemAction( wsMaximized )
							else if ( WindowState = wsMinimized ) and
								 ( FButtons.KindExists( cbkMinimize ) ) and
								 ( FButtons[cbkMinimize].Visible ) and
								 ( FButtons[cbkMinimize].Enabled ) then
								DoSystemAction( wsNormal )
							else if ( WindowState = wsMaximized ) and
								 ( FButtons.KindExists( cbkMaximize ) ) and
								 ( FButtons[cbkMaximize].Visible ) and
								 ( FButtons[cbkMaximize].Enabled ) then
								DoSystemAction( wsNormal );
	{ 16.04.99 :: tell everyone we've processed the message }
						Message.Result := 0;
					end
					else
						OldWndProc;
				end;
			end; { WM_NCLBUTTONDBLCLK }

			WM_NCLBUTTONDOWN:
	{ => Need to check for a mouse down on one of our special elements; }
			begin
				if FHelpButtonDown then
				begin
					FHelpButtonDown := false;
					FButtons[cbkHelp].Pushed := false;
					FButtons[cbkHelp].DrawPushed := false;
					bPaintCaption := true;
				end;
				with TWMNCLButtonDown( Message ) do
				begin
					cb := nil;
					pt := ScreenToClient( Point( XCursor, YCursor ) );
	{ Check for a mouse down over any of our special elements }
					if FButtons.ButtonAtPoint( pt, cb ) and cb.Enabled then
					begin
	{ Capture the mouse }
						SetCapture( Handle );
						FMouseCaptured := true;
						FMouseButtonDown := kmbLeft;
						cb.Pushed := true;
						NCInvalidate;
	{ 16.04.99 :: tell everyone we've processed the message }
						Message.Result := 0;
					end
					else if FSystemIcon.ButtonAtPoint( pt ) then
					begin
	{ Capture the mouse }
						SetCapture( Handle );
						FMouseCaptured := true;
						FMouseButtonDown := kmbLeft;
						FHelpButtonDown := false;
						DoSystemMenuClick;
						PaintCaption;
	{ 16.04.99 :: tell everyone we've processed the message }
						Message.Result := 0;
					end
					else
					begin
						if bPaintCaption then
							PaintCaption;
						if ( FWindowManager.WindowState <> wsMaximized ) then
							OldWndProc;
					end;
				end;
			end; { WM_NCLBUTTONDOWN }

			WM_LBUTTONUP:
	{ => Need to check for a click of one of our special elements; }
			begin
				if FMouseCaptured then
				begin
					with TWMLButtonUp( Message ) do
					begin
						cb := nil;
						pt := SmallPointToPoint( Pos );
	{ If the mouse is over a previously pushed button }
						if FButtons.AnyButtonPushed then
						begin
							with FButtons, ItemPushed do
								if IsPushed( pt ) then
									if ( ItemPushed.Kind = cbkHelp ) then
										DoHelpButtonPush( ItemPushed )
									else
									begin
										DrawPushed := false;
	{ Help buttons do not click }
										if ( ItemPushed.Kind <> cbkHelp ) then
											DoCaptionButtonClick( ItemPushed );
										Pushed := false;
									end;
							PaintCaption;
						end;
	{ Since the mouse was captured, release it }
						ReleaseCapture;
						FMouseCaptured := false;
						FMouseButtonDown := kmbNone;
						PaintCaption;
	{ 16.04.99 :: tell everyone we've processed the message }
						Message.Result := 0;
					end;
				end
				else
					OldWndProc;
			end; { WM_LBUTTONDOWN }

			else
				OldWndProc; { all other messages }

		end; { case }
	{
		Whoever implements a dispatcher will get a chance to modify what
		has been done in the WndProc;
	}
		Dispatch( Message );
	except
		on E: Exception do
			FormWndProcException( E );
	end;
end;

procedure TKCustomFormPainter.FormWndProcException( E: Exception );
begin
	raise E;
end;

{
--------------------------------------------------------------------------------
------------------------- Generic Dialog Architecture --------------------------
--------------------------------------------------------------------------------
}

{	TKKernelBaseDialog }

constructor TKKernelBaseDialog.Create( AOwner: TComponent );
begin
{ Class put here to allow Speed Controls instatiation into Dialogs Package without
	the presence of the Standard Package }
	ForceAnyPackagesRunning( ClassName, [perDialogs, perDBDialogs] );
	inherited Create( AOwner );
end;

{
--------------------------------------------------------------------------------
------------------------- Generic Parser Architecture --------------------------
--------------------------------------------------------------------------------
}

{ TKParsingFrame }

{
  To save the frame we only need to save the buffer pointer and all other information
  are relative to it, so get it's offset! Other information like relop, token, streampos,
  etc, are also saved to. The user MAY call DisposeFrame to correctly clear the pointer
  (call dispose at the save Buffer) or do it by himself.
  The use MUST NOT reuse the same ppf pointer because it will not correctly free the
  buffer pointer. You can use it IF AND ONLY IF, the DisposeFrame was previously called
  The data member is for derived classes and are ignored here
}

constructor TKParsingFrame.Create( AParser: TKCustomParser );
begin
  ForceObject( AParser );
  FBufferSize := AParser.GetDefaulBufferSize;
  GetMem( FBuffer, FBufferSize );
  Move( AParser.FBuffer^, FBuffer^, FBufferSize );
  FBufPtr := ( AParser.FBufPtr - AParser.FBuffer );
  FBufEnd := ( AParser.FBufEnd - AParser.FBuffer );
  FSourcePtr := ( AParser.FSourcePtr - AParser.FBuffer );
  FSourceEnd := ( AParser.FSourceEnd - AParser.FBuffer );
  FStringPtr := ( AParser.FStringPtr - AParser.FBuffer );
  FStreamPos := AParser.Stream.Position;
  FSaveChar := AParser.FSaveChar;
  FOrigin := AParser.FOrigin;
  FRelOp := AParser.FRelOp;
  FToken := AParser.FToken;
  FSourceLine := AParser.FSourceLine;
  FParser := AParser;
end;

destructor TKParsingFrame.Destroy;
begin
  FreeMem( FBuffer, FBufferSize );
  FParser := nil;
  inherited Destroy;
end;

procedure TKParsingFrame.RestoreFrame;
begin
  Move( FBuffer^, FParser.FBuffer^, FBufferSize );
  FParser.FBufPtr := FParser.FBuffer;
  Inc( FParser.FBufPtr, FBufPtr );
  FParser.FBufEnd := FParser.FBuffer;
  Inc( FParser.FBufEnd, FBufEnd );
  FParser.FSourcePtr := FParser.FBuffer;
  Inc( FParser.FSourcePtr, FSourcePtr );
  FParser.FSourceEnd := FParser.FBuffer;
  Inc( FParser.FSourceEnd, FSourceEnd );
  FParser.FStringPtr := FParser.FBuffer;
  Inc( FParser.FStringPtr, FStringPtr );
  FParser.Stream.Position := FStreamPos;
  FParser.FSaveChar := FSaveChar;
  FParser.FOrigin := FOrigin;
  FParser.FRelOp := FRelOp;
  FParser.FToken := FToken;
  FParser.FSourceLine := FSourceLine;
{ Self.Free; }
end;

{ TKParsingStack }

constructor TKParsingStack.Create( AParser: TKCustomParser );
begin
	ForceObject( AParser );
	inherited Create;
	FList := TList.Create;
	FParser := AParser;
	FDestroy := False;
end;

destructor TKParsingStack.Destroy;
begin
	FDestroy := True;
	while CheckList( FList ) do
		Pop;
	FList.Free;
	inherited Destroy;
end;

function TKParsingStack.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TKParsingStack.Push: Integer;
var
  pf: TKParsingFrame;
begin
  pf := FParser.GetParsingFrameClass.Create( FParser );
	try
    Result := FList.Add( pf );
	except
		pf.Free;
		raise;
	end;
end;

function TKParsingStack.Pop: Integer;
begin
	Result := ( FList.Count - 1 );
	if ( Result > -1 ) then
	begin
		if ( not FDestroy ) then
    begin
  		TKParsingFrame( FList[Result] ).RestoreFrame;
      TKParsingFrame( FList[Result] ).Free;
    end;
		FList.Delete( Result );
	end;	
end;

{ TKCustomParser }

constructor TKCustomParser.Create( AStream: TStream; StartScanning: Boolean );
begin
	ForceStream( AStream );
	inherited Create;
	FStream := AStream;
	FRelOp := roNone;
  FToken := tpEOF;
	GetMem( FBuffer, GetDefaulBufferSize );
	FBuffer[0] := CH_NULL;
	FBufPtr := FBuffer;
	FBufEnd := ( FBuffer + GetDefaulBufferSize );
	FSourcePtr := FBuffer;
	FSourceEnd := FBuffer;
	FTokenPtr := FBuffer;
	FSourceLine := 1;
	FErrorClass := GetDefaultErrorClass;
	FStack := TKParsingStack.Create( Self );
	ForceClass( FErrorClass );
	FCharSets[SYMBOL_CHARSET_IDX] := GetSymbolCharSet;
	FCharSets[STRING_CHARSET_IDX] := GetStringCharSet;
	FCharSets[HEXA_CHARSET_IDX] := GetHexaCharSet;
	FCharSets[INTEGER_CHARSET_IDX] := GetIntegerCharSet;
	FCharSets[FLOAT_CHARSET_IDX] := GetFloatCharSet;
	FCharSets[RELOP_CHARSET_IDX] := GetRelOpCharSet;
	FCharSets[SPECIAL_CHARSET_IDX] := GetSpecialCharSet;
	FCharSets[COMMENT_CHARSET_IDX] := GetCommentCharSet;
	FCharSets[BLANK_CHARSET_IDX] := GetBlankCharSet;
	if StartScanning then
		NextToken;
end;

destructor TKCustomParser.Destroy;
begin
	if CheckPointer( FBuffer ) then
	begin
		FStream.Seek( Longint( FTokenPtr ) - Longint( FBufPtr ), soFromCurrent ); //? Why
		FreeMem( FBuffer, GetDefaulBufferSize );
	end;
	FStack.Free;
	inherited Destroy;
end;

class function TKCustomParser.GetDefaulBufferSize: Cardinal;
begin
	Result := DEFAULT_PARSER_BUFFER_SIZE;
end;

function TKCustomParser.GetDefaultErrorClass: EKParserClass;
begin
  Result := EKParser;
end;

function TKCustomParser.GetParsingFrameClass: TKParsingFrameClass;
begin
  Result := TKParsingFrame;
end;

function TKCustomParser.CheckToken( AToken: TKTokenType ): Boolean;
begin
	Result := ( Token = AToken );
end;

function TKCustomParser.CheckAnyToken( const Tokens: array of TKTokenType ): Boolean;
var
  i: Integer;
begin
	Result := False;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := ( Result or CheckToken( Tokens[i] ) );
end;

procedure TKCustomParser.ForceToken( AToken: TKTokenType );
begin
	if ( not CheckToken( AToken ) ) then
		case AToken of
			tpSymbol:
				Error( sErrIdentExpected );
			tpString:
				Error( sErrStrExpected );
			tpInteger, tpFloat, tpHexa:
				Error( sErrNumExpected );
			tpComment:
				Error( sErrComtExpected );
		else
			ErrorFmt( sErrCharExpected, [AToken] );
		end;
end;

function TKCustomParser.CheckString( const AString: string ): Boolean;
begin
	Result := ( ( Token = tpString ) and CheckStrEqual( AString, TokenString ) );
end;

procedure TKCustomParser.ForceString( const AString: string );
begin
	if ( not CheckString( AString ) ) then
		ErrorFmt( sErrStringExpected, [AString] );
end;

function TKCustomParser.CheckSymbol( const Symbol: string ): Boolean;
begin
	Result := ( ( Token = tpSymbol ) and CheckStrEqual( Symbol, TokenString ) );
end;

procedure TKCustomParser.ForceSymbol( const Symbol: string );
begin
	if ( not CheckSymbol( Symbol ) ) then
		ErrorFmt( sErrSymbolExpected, [Symbol] );
end;

function TKCustomParser.CheckNumber( const Number: string ): Boolean;
begin
	Result := ( ( ( Token = tpInteger ) and ( StrToInt( Number ) = TokenInt ) ) or
		( ( Token = tpFloat ) and ( StrToFloat( Number ) = TokenFloat ) ) or
		( ( Token = tpHexa ) and ( HexToInt( Number ) = HexToInt( TokenString ) ) ) );
end;

procedure TKCustomParser.ForceNumber( const Number: string );
begin
	if ( not CheckNumber( Number ) ) then
		ErrorFmt( sErrNumberExpected, [Number] );
end;

function TKCustomParser.CheckRelOp( ARelOp: TKRelOp ): Boolean;
begin
	Result := ( CheckToken( tpRelOp ) and ( RelOp = ARelOp ) );
end;

procedure TKCustomParser.ForceRelOp( ARelOp: TKRelOp );
begin
	if ( not CheckRelOp( ARelOp ) ) then
		Error( sErrInvRelOp );
end;

procedure TKCustomParser.Error( const Ident: string );
begin
	RaiseExceptionFmt( FErrorClass, sErrParsing, [Ident, FSourceLine] );
end;

procedure TKCustomParser.ErrorFmt( const Ident: string; const Args: array of const );
begin
	Error( Format( Ident, Args ) );
end;

function TKCustomParser.CheckSymbolToken( P: PChar ): Boolean;
begin
	Result := ( P^ in FCharSets[SYMBOL_CHARSET_IDX] );
end;

function TKCustomParser.CheckStringToken( P: PChar ): Boolean;
begin
	Result := ( P^ in FCharSets[STRING_CHARSET_IDX] );
end;

function TKCustomParser.CheckNumberToken( P: PChar ): Boolean;
begin
	Result := ( P^ in ( FCharSets[INTEGER_CHARSET_IDX] + FCharSets[FLOAT_CHARSET_IDX] ) );
end;

function TKCustomParser.CheckHexaToken( P: PChar ): Boolean;
begin
	Result := False; { Custom Parser does not process Hexa Tokens }
end;

function TKCustomParser.CheckRelOpToken( P: PChar ): Boolean;
begin
	Result := False; { Custom Parser does not process Relational Operators }
end;

function TKCustomParser.CheckSpecialToken( P: PChar ): Boolean;
begin
	Result := False; { Custom Parser does not process Special tokens }
end;

function TKCustomParser.CheckCommentToken( P: PChar ): Boolean;
begin
	Result := False; { Custom Parser do not process Comment Tokens }
end;

function TKCustomParser.CheckBlankToken( P: PChar ): Boolean;
begin
	Result := ( P^ in FCharSets[BLANK_CHARSET_IDX] );
end;

function TKCustomParser.GetSymbolCharSet: TKCharSet;
begin
	Result := DEFAULT_PARSER_SYMBOL_CHARSET; { 'A'..'Z', 'a'..'z', '_' }
end;

function TKCustomParser.GetStringCharSet: TKCharSet;
begin
	Result := [];
end;

function TKCustomParser.GetHexaCharSet: TKCharSet;
begin
	Result := DEFAULT_PARSER_HEXA_CHARSET; { '0'..'9', 'a'..'f', 'A'..'F' }
end;

function TKCustomParser.GetIntegerCharSet: TKCharSet;
begin
	Result := [];
end;

function TKCustomParser.GetFloatCharSet: TKCharSet;
begin
	Result := [];
end;

function TKCustomParser.GetCommentCharSet: TKCharSet;
begin
	Result := [];
end;

function TKCustomParser.GetRelOpCharSet: TKCharSet;
begin
	Result := [];
end;

function TKCustomParser.GetSpecialCharSet: TKCharSet;
begin
	Result := [];
end;

function TKCustomParser.GetBlankCharSet: TKCharSet;
begin
	Result := [];
end;

function TKCustomParser.ProcessSymbol( var P: PChar ): TKTokenType;
begin
	Inc( P );
	while ( P^ in CharSets[SYMBOL_CHARSET_IDX] ) do
		Inc( P );
	Result := tpSymbol;
end;

function TKCustomParser.ProcessString( var P: PChar ): TKTokenType;
begin
	FStringPtr := P;
	Result := tpString;
end;

function TKCustomParser.ProcessHexa( var P: PChar ): TKTokenType;
begin
	Inc( P );
	while ( P^ in FCharSets[HEXA_CHARSET_IDX] ) do
		Inc( P );
	Result := tpHexa;
end;

function TKCustomParser.ProcessNumber( var P: PChar ): TKTokenType;
begin
	while ( P^ in CharSets[INTEGER_CHARSET_IDX] ) do
		Inc( P );
	Result := tpInteger;
	while ( P^ in CharSets[FLOAT_CHARSET_IDX] ) do
	begin
		Inc( P );
		Result := tpFloat;
	end;
end;

function TKCustomParser.ProcessComment( var P: PChar ): TKTokenType;
begin
	Result := tpComment;
end;

function TKCustomParser.ProcessRelOpToken( var P: PChar ): TKTokenType;
begin
	Result := tpRelOp;
end;

function TKCustomParser.ProcessSpecialToken( var P: PChar ): TKTokenType;
begin
	Result := tpSpecial;
end;

function TKCustomParser.ProcessGarbage( var P: PChar ): TKTokenType;
begin
	Result := P^;
	if ( Result <> toEOF ) then
		Inc( P );
end;

function TKCustomParser.InternalNextToken( var P: PChar ): TKTokenType;
begin	  
	if CheckSymbolToken( P ) then       { Parse identifiers }
		Result := ProcessSymbol( P )
	else if CheckStringToken( P ) then  { Parsing string literals - with def. proc. }
		Result := ProcessString( P )
	else if CheckHexaToken( P ) then    { Check For different start hexa tokens }
		Result := ProcessHexa( P )
	else if CheckNumberToken( P ) then  { Integer and Float are processed together }
		Result := ProcessNumber( P )
	else if CheckCommentToken( P ) then { Process comments }
		Result := ProcessComment( P )
	else if CheckRelOpToken( P ) then   { Mathematical relational operators }
		Result := ProcessRelOpToken( P )
	else if CheckSpecialToken( P ) then { Special (user defined) tokens }
	  Result := ProcessSpecialToken( P )
	else
		Result := ProcessGarbage( P );    { Any other tokens }
end;

function TKCustomParser.NextToken: TKTokenType;
var
	P: PChar;
begin
	ProcessBlank;
	P := FSourcePtr;
	FTokenPtr := P;
	Result := InternalNextToken( P );
	FSourcePtr := P;
	FToken := Result;
end;

procedure TKCustomParser.DoReadBuffer;
var
	iCount: Integer;
begin
	Inc( FOrigin, FSourcePtr - FBuffer );
	FSourceEnd[0] := FSaveChar;
	iCount := ( FBufPtr - FSourcePtr );
	if ( iCount <> 0 ) then
		Move( FSourcePtr[0], FBuffer[0], iCount );
	FBufPtr := ( FBuffer + iCount );
	Inc( FBufPtr, FStream.Read( FBufPtr[0], ( FBufEnd - FBufPtr ) ) );
	FSourcePtr := FBuffer;
	FSourceEnd := FBufPtr;
	if ( FSourceEnd = FBufEnd ) then
	begin
		FSourceEnd := LineStart( FBuffer, FSourceEnd - 1 );
		if ( FSourceEnd = FBuffer ) then
			Error( sErrLineLong );
	end;
	FSaveChar := FSourceEnd[0];
	FSourceEnd[0] := CH_NULL;
end;

procedure TKCustomParser.SaveToFrame( var ppf: TKParsingFrame );
begin
  ppf := GetParsingFrameClass.Create( Self );
end;

procedure TKCustomParser.RestoreFromFrame( ppf: TKParsingFrame );
begin
  ForceObject( ppf );
  ppf.RestoreFrame;
end;

procedure TKCustomParser.ProcessBlank;
begin
	while True do
	begin
		case FSourcePtr^ of
			CH_NULL:
			begin
				DoReadBuffer;
				if ( FSourcePtr^ = CH_NULL ) then
					Exit;
				Continue;
			end;
			CH_LF: IncSourceLine;
		else
			if ( not CheckBlankToken( FSourcePtr ) ) then
				Exit;
		end;
		Inc( FSourcePtr );
	end;
end;

procedure TKCustomParser.IncSourceLine;
begin
  Inc( FSourceLine );
end;

procedure TKCustomParser.SetToken( AToken: TKTokenType );
begin
  FToken := AToken;
end;

function TKCustomParser.SourcePos: Longint;
begin
	Result := ( FOrigin + ( FTokenPtr - FBuffer ) );
end;

function TKCustomParser.TokenFloat: Extended;
begin
	Result := StrToFloat( TokenString );
end;

function TKCustomParser.TokenInt: Longint;
begin
	Result := StrToInt( TokenString );
end;

function TKCustomParser.TokenHexa: Longint;
begin
	Result := HexToInt( TokenString );
end;

function TKCustomParser.TokenString: string;
var
	l: Integer;
begin
	if ( Token = tpString ) then
		l := ( FStringPtr - FTokenPtr )
	else
		l := ( FSourcePtr - FTokenPtr );
	SetString( Result, FTokenPtr, l );
end;

{ TKCodeParsingFrame }

constructor TKCodeParsingFrame.Create( AParser: TKCustomParser );
begin
  ForceObjectClass( AParser, TKCustomCodeParser );
  inherited Create( AParser );
  with ( AParser as TKCustomCodeParser ) do
  begin
  	Self.FLine := FLine;
    Self.FPosition := FPosition;
    Self.FTargetPos := 0;
    if CheckObject( FTarget ) then
      Self.FTargetPos := FTarget.Position;
	  Self.FKeyWordsText := FKeyWords.Text;
	  Self.FOutPutString := FOutPutString;
  end;
end;

procedure TKCodeParsingFrame.RestoreFrame;
begin
  inherited RestoreFrame;
  with ( Parser as TKCustomCodeParser ) do
  begin
    FLine := Self.FLine;
    FPosition := Self.FPosition;
    if CheckObject( FTarget ) then
      FTarget.Position := Self.FTargetPos;
    FKeyWords.Text := Self.FKeyWordsText;
    FOutPutString := Self.FOutPutString;
  end;
end;

{ TKCustomCodeParser }

constructor TKCustomCodeParser.Create( ASource, ATarget: TStream; const AKeyWords: string );
var
	s: string;
begin
	s := GetTargetFileName;
	inherited Create( ASource, GetAutoScanning );
	if ( not CheckStr( FFileName ) ) then
	begin
		ForceObject( ATarget );
		FTarget := ATarget;
	end
	else if CheckStr( s ) then
		FTarget := TFileStream.Create( s, ( fmCreate or fmShareExclusive ) );
	SetString( FOutPutString, nil, GetDefaulBufferSize );
	FKeyWords := TStringList.Create;
	if CheckStr( AKeyWords ) then
		FKeyWords.Text := AKeyWords
	else
		FKeyWords.Text := GetDefaultKeyWords;
	FLine := -1;
	FPosition := -1;
end;

constructor TKCustomCodeParser.CreateKeyWords( ASource, ATarget: TStream; AKeyWords: TStrings );
begin
	ForceObject( AKeyWords );
	Create( ASource, ATarget, AKeyWords.Text );
end;

constructor TKCustomCodeParser.CreateFromFile( const AFileName: string );
begin
	ForceFile( AFileName );
	FFileName := AFileName;
	Create( TFileStream.Create( FFileName, fmOpenRead or fmShareDenyWrite ), nil, '' );
end;

destructor TKCustomCodeParser.Destroy;
begin
	FKeyWords.Free;
	inherited Destroy;
  if CheckStr( FFileName ) then
	begin
		Stream.Free;
		FTarget.Free;
	end;
end;

function TKCustomCodeParser.GetDefaultErrorClass: EKParserClass;
begin
  Result := EKCodeParser;
end;

function TKCustomCodeParser.GetParsingFrameClass: TKParsingFrameClass;
begin
  Result := TKCodeParsingFrame;
end;

class function TKCustomCodeParser.GetAutoScanning: Boolean;
begin
	Result := False;
end;

function TKCustomCodeParser.GetTargetFileName: string;
begin
  if CheckFile( FFileName ) then
  	Result := GetUniqueFileName( FFileName )
  else
    Result := FFileName;  
end;

function TKCustomCodeParser.GetDefaultKeyWords: string;
begin
	Result := '';
end;

function TKCustomCodeParser.ValidateSpecialToken( Ch: Char ): string;
begin
	Result := Ch; { do nothing };
end;

function TKCustomCodeParser.MakeStringLegal( const s: string ): string;
begin
	Result := s;
	DoCheckString( Result );
end;

function TKCustomCodeParser.MakeCommentLegal( const s: string ): string;
var
	i: Integer;
begin
	Result := '';
{ Check special token for each comment char }
	for i := 1 to Length( s ) do
		Result := Result + ValidateSpecialToken( s[i] );
	DoCheckComment( Result );
end;

procedure TKCustomCodeParser.DoBuildHeader;
begin
	if Assigned( FOnBuildFileHeader ) then
		FOnBuildFileHeader( Self, FOutPutString );
end;

procedure TKCustomCodeParser.DoBuildFooter;
begin
	if Assigned( FOnBuildFileFooter ) then
		FOnBuildFileFooter( Self, FOutPutString );
end;

procedure TKCustomCodeParser.DoEndOfStream;
begin
	if Assigned( FOnEndOfStream ) then
		FOnEndOfStream( Self );
end;

procedure TKCustomCodeParser.DoReadBuffer;
begin
	inherited DoReadBuffer;
	if Assigned( FOnReadBuffer ) then
		FOnReadBuffer( Self );
end;

procedure TKCustomCodeParser.DoBeforeString( const Str: string );
begin
	if Assigned( FBeforeString ) then
		FBeforeString( Self, FOutPutString, Str );
end;

procedure TKCustomCodeParser.DoAfterString;
begin
	if Assigned( FAfterString ) then
		FAfterString( Self, FOutPutString );
end;

procedure TKCustomCodeParser.DoBeforeKeyword( const KeyWord: string );
begin
	if Assigned( FBeforeKeyword ) then
		FBeforeKeyword( Self, FOutPutString, KeyWord );
end;

procedure TKCustomCodeParser.DoAfterKeyword;
begin
	if Assigned( FAfterKeyword ) then
		FAfterKeyword( Self, FOutPutString );
end;

procedure TKCustomCodeParser.DoBeforeSymbol( const Symbol: string );
begin
	if Assigned( FBeforeSymbol ) then
		FBeforeSymbol( Self, FOutPutString, Symbol );
end;

procedure TKCustomCodeParser.DoAfterSymbol;
begin
	if Assigned( FAfterSymbol ) then
		FAfterSymbol( Self, FOutPutString );
end;

procedure TKCustomCodeParser.DoBeforeComment( const Comment: string );
begin
	if Assigned( FBeforeComment ) then
		FBeforeComment( Self, FOutPutString, Comment );
end;

procedure TKCustomCodeParser.DoAfterComment;
begin
	if Assigned( FAfterComment ) then
		FAfterComment( Self, FOutPutString );
end;

procedure TKCustomCodeParser.DoBeforeNumber( AType: TKTokenType; const Number: string );
begin
	case AType of
		tpInteger:
			if Assigned( FBeforeInteger ) then
				FBeforeInteger( Self, FOutPutString, Number );
		tpHexa:
			if Assigned( FBeforeHexa ) then
				FBeforeHexa( Self, FOutPutString, Number );
		tpFloat:
			if Assigned( FBeforeFloat ) then
				FBeforeFloat( Self, FOutPutString, Number );
	else
		ErrorFmt( sErrInvPascalNumberToken, [AType] );
	end;
end;

procedure TKCustomCodeParser.DoAfterNumber( AType: TKTokenType );
begin
	case AType of
		tpInteger:
			if Assigned( FAfterInteger ) then
				FAfterInteger( Self, FOutPutString );
		tpHexa:
			if Assigned( FAfterHexa ) then
				FAfterHexa( Self, FOutPutString );
		tpFloat:
			if Assigned( FAfterFloat ) then
				FAfterFloat( Self, FOutPutString );
	else
		ErrorFmt( sErrInvPascalNumberToken, [AType] );
	end;
end;

procedure TKCustomCodeParser.DoCheckString( var s: string );
begin
	if Assigned( FOnCheckString ) then
		FOnCheckString( Self, s );
end;

procedure TKCustomCodeParser.DoCheckComment( var s: string );
begin
	if Assigned( FOnCheckComment ) then
		FOnCheckComment( Self, s );
end;

procedure TKCustomCodeParser.DoUnknownToken;
begin
	if Assigned( FOnUnknownToken ) then
		FOnUnknownToken( Self, FOutPutString );
end;

procedure TKCustomCodeParser.ProcessConversion( const s: string );
begin
	case Token of
		tpSymbol:
		begin
			if ( FKeyWords.IndexOf( s ) = -1 ) then
			begin
				DoBeforeSymbol( s );
				FOutPutString := FOutPutString + s;
				DoAfterSymbol;
			end
			else
			begin
				DoBeforeKeyword( s );
				FOutPutString := FOutPutString + s;
				DoAfterKeyword;
			end;
		end;
		tpString:
		begin
			DoBeforeString( s );
  		FOutPutString := FOutPutString + MakeStringLegal( s );
	  	Position := Position + 2; { Plus 2: string open and close tokens }
			DoAfterString;
		end;
		tpInteger, tpHexa, tpFloat:
		begin
			DoBeforeNumber( Token, s );
			FOutPutString := FOutPutString + s;
			DoAfterNumber( Token );
		end;
		tpComment:
		begin
			DoBeforeComment( s );
			FOutPutString := FOutPutString + MakeCommentLegal( s );
			DoAfterComment;
		end;
		else
		begin
			FOutPutString := FOutPutString + ValidateSpecialToken( Token );
			DoUnknownToken;
		end;
	end;

end;

procedure TKCustomCodeParser.Convert;
var
	s: string;
begin
	DoBuildHeader;
	Line := 1;
	Position := 0;
	while ( Token <> toEOF ) do
	begin
		while ( SourceLine > Line ) do { If source changed (events), add new line }
		begin
			FOutPutString := FOutPutString + CH_CRLF;
			Line := Line + 1;
			Position := Position + Length( CH_CRLF );
		end;
		while ( SourcePos > Position ) do { Add proper format }
		begin
			FOutPutString := FOutPutString + CH_SPACE;
			Position := Position + 1;
		end;
		s := TokenString;
		ProcessConversion( s );
		Position := Position + Length( s );
		NextToken;
	end;
	DoEndOfStream;
	DoBuildFooter;
	FTarget.WriteBuffer( Pointer( FOutPutString )^, Length( FOutPutString ) );
end;

{
--------------------------------------------------------------------------------
-------------------------- Generic Lexer Architecture --------------------------
--------------------------------------------------------------------------------
}

{ TKCustomLexer }

constructor TKCustomLexer.Create( ASource, ATarget: TStream );
begin
	FErrorClass := GetDefaultErrorClass;
	ForceClass( FErrorClass );
	inherited Create;
	FLexing := False;
	if CheckObject( ASource ) then
		FSource := ASource;
	if CheckObject( ATarget ) then
		FTarget := ATarget;
	FValidTokenTypes := GetValidTokenTypes;
end;

destructor TKCustomLexer.Destroy;
begin
	FreeClean( FParser ); 
	inherited Destroy;
end;

procedure TKCustomLexer.DoLexer;
begin
	if Assigned( FOnLexer ) then
		FOnLexer( Self );
end;

procedure TKCustomLexer.DoReset;
begin
	if Assigned( FOnReset ) then
		FOnReset( Self );
end;

function TKCustomLexer.GetDefaultErrorClass: EKLexerClass;
begin
	Result := EKLexer;
end;

function TKCustomLexer.GetParser: TKCustomParser;
begin
	Result := FParser;
end;

function TKCustomLexer.GetValidTokenTypes: TKCharSet;
begin
	Result := ( CHARSET_FULL - [tpEOF, tpComment] );
end;

procedure TKCustomLexer.Error( const Ident: string );
begin
	uksyUtils.RaiseException( FErrorClass, Ident );
end;

procedure TKCustomLexer.ErrorFmt( const Ident: string; const Args: array of const );
begin
	Error( Format( Ident, Args ) );
end;

function TKCustomLexer.DetectEOF: Boolean;
begin
	Result := Parser.CheckToken( tpEOF );
end;

procedure TKCustomLexer.MatchToken( AToken: TKTokenType );
begin
	Parser.ForceToken( AToken );
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in FValidTokenTypes ) or DetectEOF );
end;

procedure TKCustomLexer.MatchAnyToken( const Tokens: array of TKTokenType );
begin
	if ( not Parser.CheckAnyToken( Tokens ) ) then
		Parser.ForceToken( Tokens[0] );
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in FValidTokenTypes ) or DetectEOF );
end;

procedure TKCustomLexer.MatchSymbol( const Symbol: string );
begin
	Parser.ForceToken( tpSymbol );
	Parser.ForceSymbol( Symbol );
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in FValidTokenTypes ) or DetectEOF );
end;

procedure TKCustomLexer.MatchNumber( AType: TKTokenType );
begin
	Parser.ForceToken( AType );
	case AType of
		tpInteger, tpHexa:
			ForceStr( IntToStr( Parser.TokenInt ) );
		tpFloat:
			ForceStr( FloatToStr( Parser.TokenFloat ) );
	end;
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in FValidTokenTypes ) or DetectEOF );
end;

procedure TKCustomLexer.MatchString( const AString: string );
begin
	Parser.ForceToken( tpString );
	Parser.ForceString( AString );
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in FValidTokenTypes ) or DetectEOF );
end;

procedure TKCustomLexer.Reset;
begin
	FreeClean( FParser );
	DoReset;
	CreateParser;
end;

procedure TKCustomLexer.Lexer;
begin
	FLexing := True;
	try
		Reset;
		Parser.NextToken;
		Goal;
		DoLexer;
	finally
	  FLexing := False;
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------------- List of Variants ------------------------------
--------------------------------------------------------------------------------
}

{ TKVariantList }

constructor TKVariantList.Create;
begin
	inherited Create;
	FItems := TStringList.Create;
end;

destructor TKVariantList.Destroy;
begin
	Clear;
	FItems.Free;
	inherited Destroy;
end;

function TKVariantList.AllowedVariantIndexType( Value: OleVariant ): Boolean;
const
	MAX_ALLOWED_TYPES = 10;
	ALLOWED: array[1..MAX_ALLOWED_TYPES] of LongInt =
		( varSmallint, varInteger, varSingle, varDouble, varCurrency,
			varDate, varOleStr, varBoolean, varByte, varString );
var
	i: Byte;
	vt: Integer;
begin
	Result := false;
	vt := VarType( Value );
	for i := 1 to MAX_ALLOWED_TYPES do
		if ( vt = ALLOWED[i] ) then
		begin
			Result := true;
			Break;
		end;
end;

function TKVariantList.VarStr( Value: OleVariant ): string;
begin
	if VarIsEmpty( Value ) then
		Result := '(EMPTY)'
	else if VarIsNull( Value ) then
		Result := '(NULL)'
	else if AllowedVariantIndexType( Value ) then
		Result := Value
	else
		Result := '(NO STRING REPRESENTATION)';
end;

function TKVariantList.GetItemsByID( Index: Integer ): OleVariant;
begin
	if ( ( Index > -1 ) and ( Index < VarCount ) ) then
		Result := POleVariant( FItems.Objects[Index] )^
	else
		Result := UNASSIGNED;
end;

procedure TKVariantList.SetItemsByID( Index: Integer; Value: OleVariant );
begin
	if ( ( Index <= -1 ) or ( Index >= VarCount ) ) then
		Exit;
	if VarIsEmpty( Value ) then
	begin
		POleVariant( FItems.Objects[Index] )^ := UNASSIGNED;
		Dispose( POleVariant( FItems.Objects[Index] ) );
		FItems.Delete( Index );
	end
	else
		POleVariant( FItems.Objects[Index] )^ := Value;
end;

function TKVariantList.GetItems( Index: OleVariant ): OleVariant;
var
	s: string;
	i: Integer;
begin
	if ( not AllowedVariantIndexType( Index ) ) then
	begin
		Result := UNASSIGNED;
		Exit;
	end;
	s := Index;
	i := FItems.IndexOf( s );
	if ( i <> -1 ) then
		Result := POleVariant( FItems.Objects[i] )^
	else
		Result := UNASSIGNED;
end;

procedure TKVariantList.SetItems( Index: OleVariant; Value: OleVariant );
var
	s: string;
	i: Integer;
	pv: POleVariant;
begin
	if ( not AllowedVariantIndexType( Index ) ) then
		Exit;
	s := Index;
	if ( not CheckTrimStr( s ) ) then
		Exit;
	i := FItems.IndexOf( s );
	if ( i <> -1 ) then
	begin
		POleVariant( FItems.Objects[i] )^ := UNASSIGNED;
		if VarIsEmpty( Value ) then
		begin
			Dispose( POleVariant( FItems.Objects[i] ) );
			FItems.Delete( i );
		end
		else
			POleVariant( FItems.Objects[i] )^ := Value;
	end
	else if ( not VarIsEmpty( Value ) ) then
	begin
		pv := New( POleVariant );
		try
			pv^ := Value;
			FItems.AddObject( s, TObject( pv ) );
		except
			Dispose( pv );
			raise;
		end;
	end;
end;

function TKVariantList.GetVarCount: LongInt;
begin
	Result := FItems.Count;
end;

procedure TKVariantList.Clear;
begin
	while ( VarCount > 0 ) do
	begin
		POleVariant( FItems.Objects[0] )^ := UNASSIGNED;
		Dispose( POleVariant( FItems.Objects[0] ) );
		FItems.Delete( 0 );
	end;
end;

function TKVariantList.Dump: OleVariant;
var
	s: string;
	i: Integer;
begin
	s := '';
	if ( VarCount = 0 ) then
		Exit;
	for i := 0 to VarCount - 1 do
	begin
		s := s + Format( '%s=%s', [FItems[i], VarStr( Items[FItems[i]] )] );
		if ( i < ( VarCount - 1 ) ) then
			s := s + CH_SEMICOLON;
	end;
	Result := WideString( s );
end;

(*

{ TKSessionInfo }

constructor TKSessionInfo.Create;
begin
	inherited Create;
	Randomize;
	FSeqNo := -1;
	FID := Random( GetTickCount );
	FServerEnv := TKServerEnvironment.Create;
end;

destructor TKSessionInfo.Destroy;
begin
	FServerEnv.Free;
	inherited Destroy;
end;

function TKSessionInfo.GetTickCount: LongInt;
begin
	Result := Windows.GetTickCount;
end;

function TKSessionInfo.GetServerVars( Index: OleVariant ): OleVariant;
begin
	Result := FServerEnv[Index];
end;

function TKSessionInfo.NewSeqNo: LongInt;
begin
	FSeqNo := TickCount;
	Result := FSeqNo;
end;

function TKSessionInfo.DumpServerEnv: OleVariant;
var
	s: string;
	i: Integer;
begin
	s := '';
	with FServerEnv do
		for i := 0 to ParamCount - 1 do
		begin
			s := s + Format( '%s=%s', [Params.Names[i], Params.Values[Params.Names[i]]] );
			if ( i < ( ParamCount - 1 ) ) then
				s := s + CH_COMMA + CH_SPACE;
		end;
	Result := WideString( s );
end;
*)

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	{ IsMultiThread := true; !!! }
end;

procedure Done;
begin
	DestroyHookControls;
end;

initialization
	Init;

finalization
	Done; 

end.
