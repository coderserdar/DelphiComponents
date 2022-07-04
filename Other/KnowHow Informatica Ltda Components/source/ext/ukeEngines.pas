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

unit ukeEngines;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, SysUtils, Classes, uksyUtils, ukrClasses, ukrEngines, ukrLanguage;

type

	TKExcept = class;
	
{
---------------------------------------------------------------------------------
-------------------------- Except Log File Architecture -------------------------
---------------------------------------------------------------------------------
}

	EKExceptLogEngine = class( EKLogEngine );

{ TKExceptLogFile }

	TKExceptLogErrorEvent = procedure( Sender: TKExcept; E: Exception;
		var LogErrorAction: TKLogErrorAction ) of object;

	TKExceptLogFile = class( TKCustomAuditoryLog )
	private
		FMBInfo: TMemoryBasicInformation;
		FOnExceptLogError: TKExceptLogErrorEvent;

		function GetExceptAddr: string;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DoOnLogFileLogError( Data: Pointer;
			var LogErrorAction: TKLogErrorAction ); override;

	protected
		procedure DefineFixedFields; override;
		procedure DefineCustomFields; override;

		procedure DoOnExceptLogError( AException: Exception;
			var LogErrorAction: TKLogErrorAction ); dynamic;

		function GetLogOwner: TKExcept; virtual;	

		property OnExceptLogError: TKExceptLogErrorEvent
						 read FOnExceptLogError write FOnExceptLogError;

	public
		constructor Create( AOwner: TComponent; const AFileName: TFileName ); override;

		property Owner: TKExcept
						 read GetLogOwner;

	end;

{
---------------------------------------------------------------------------------
------------------------- Except Log Engine Architecture ------------------------
---------------------------------------------------------------------------------
}

{ TKExceptStrings }

	TKExceptStrings = class( TKStrings )
	private
		FLanguage: TLanguage;

		procedure SetLanguage( const Value: TLanguage );
		
	public
		procedure Assign( Source: TPersistent ); override;

		property Language: TLanguage
						 read FLanguage write SetLanguage;
						 
	end;

{ TKExceptLogEngine }

	TKExceptLogEngine = class( TKCustomAuditoryLogEngine )
	private
		iSecRow : Integer;
		iModAddrRow: Integer;
		iModNameRow: Integer;

		iCurrentConvExptAddr: Integer;
		iCurrentRoutine: Integer;
		iCurrentModule: Integer;

		iCurrentRoutineAddrRow: Integer;
		iCurrentLineRow: Integer;

		sCurrentConvExptAddr: ShortString;
		sCurrentRoutine: ShortString;
		sCurrentModule: ShortString;

		FMapLoaded: Boolean;
		FHasMapInfo: Boolean;
		FMapFileName: TFileName;
		FStrArray: TKStringsArray;

		function GetMapFileSections( Index: Integer ): ShortString;

		function GetExceptRoutine( Index: Integer ): ShortString;
		function GetExceptModule( Index: Integer ): ShortString;
		function GetExceptLine( Index: Integer ): ShortString;

		function GetExceptModuleByAddr( const ConvExceptAddr: ShortString ): ShortString;
		function GetExceptRoutineByAddr( const ConvExceptAddr: ShortString ): ShortString;
		function GetExceptLineByAddr( const ConvExceptAddr: ShortString ): ShortString;

		{Put as string to use this method as the callback funtion for the virtual field.
		 Otherwise, the result can be ShortString because a PASCAL_ID don't has more than
		 63 char, so we don't need a string!}
		function GetExceptRoutineCallBack( Index: Integer): string;
		function GetExceptModuleCallBack( Index: Integer): string;
		function GetExceptLineCallBack( Index: Integer ): string;

	protected
		procedure DefineVirtualFields; override;

    function GetAuditoryLogClass: TKCustomAuditoryLogClass; override;

		function CheckCompatibility( const AMapFileName, AkMapFileName: TFileName ): Boolean;
		procedure ClearkMapInfo;
		procedure CreateMapInfo( const AMapFileName: TFileName ); dynamic;
		procedure LoadkMapInfo( Stream: TStream ); dynamic;

		function ExceptAddrRoutineIndex( const ConvExceptAddr: ShortString ): Integer;
		function ExceptAddrModuleIndex( const ConvExceptAddr: ShortString ): Integer;
		function ExceptAddrLineIndex( const ConvExceptAddr: ShortString ): Integer;
				
		property MapFileSections[Index: Integer]: ShortString
						 read GetMapFileSections;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

		procedure LoadFromFile( const AFileName: TFileName ); override;
		procedure GenerateMapDetailFiles;

		property HeaderSize;
		property HighFieldLengh;
		property FieldCount;
		property FileLoaded;
		property FixedFieldCount;
		property LogRecords;
		property LogFields;
		property LogFieldsSize;
		property LogRecordSize;
		property RecordCount;
		property VirtualFieldCount;

		property MapLoaded: Boolean
						 read FMapLoaded;

		property ExceptRoutineByAddr[const ConvExceptAddr: ShortString]: ShortString
						 read GetExceptRoutineByAddr;
		property ExceptModuleByAddr[const ConvExceptAddr: ShortString]: ShortString
						 read GetExceptModuleByAddr;
		property ExceptLineByAddr[const ConvExceptAddr: ShortString]: ShortString
						 read GetExceptLineByAddr;

	published
		property AutoClearLog;
		property AutoClearLogType;
		property AutoLoad default True;
		property DaysToKeep;
		property FileName;
		property MaxByteSize;

		property HasMapInfo: Boolean
						 read FHasMapInfo write FHasMapInfo default True;
		property MapFileName: TFileName
						 read FMapFileName write FMapFileName;

	end;

{
---------------------------------------------------------------------------------
------------------------ Except Log Auditor Architecture ------------------------
---------------------------------------------------------------------------------
}

	EKExcept = class( EKExt );
	
	TKExceptLink = class;

{ TKExceptLink }	

	TKExceptLinkEvent = procedure ( Sender: TKExcept; Link: TKExceptLink;
		E: Exception ) of object;

	TKExceptLink = class( TKCustomLink )
	private
		FBeforeTranslate: TKExceptLinkEvent;
		FAfterTranslate: TKExceptLinkEvent;
		FBeforeException: TKExceptLinkEvent;
		FAfterException: TKExceptLinkEvent;
		FBeforeDisplay: TKExceptLinkEvent;
		FAfterDisplay: TKExceptLinkEvent;
		FBeforeLogExcept: TKExceptLinkEvent;
		FAfterLogExcept: TKExceptLinkEvent;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt ); override;

	public
		property BeforeTranslate: TKExceptLinkEvent
						 read FBeforeTranslate write FBeforeTranslate;
		property AfterTranslate: TKExceptLinkEvent
						 read FAfterTranslate write FAfterTranslate;
		property BeforeException: TKExceptLinkEvent
						 read FBeforeException write FBeforeException;
		property AfterException: TKExceptLinkEvent
						 read FAfterException write FAfterException;
		property BeforeDisplay: TKExceptLinkEvent
						 read FBeforeDisplay write FBeforeDisplay;
		property AfterDisplay: TKExceptLinkEvent
						 read FAfterDisplay write FAfterDisplay;
		property BeforeLogExcept: TKExceptLinkEvent
						 read FBeforeLogExcept write FBeforeLogExcept;
		property AfterLogExcept: TKExceptLinkEvent
						 read FAfterLogExcept write FAfterLogExcept;

		property Owner;

	end;

{ TKExceptStringsArray }

	TKExceptStringsArray = class( TKCustomStringsArray )
	private
		FKExcept: TKExcept;

		function GetLangRow( Index: TLanguage ): TKExceptStrings;
		procedure SetLanguages( ARow: Integer; const Value: TLanguage );
		function GetLanguages( ARow: Integer ): TLanguage;

		function GetRows( ARow: Integer ): TKExceptStrings;
		function GetCols( ACol: Integer ): TKExceptStrings;
		procedure SetRows( ARow: Integer; Strings: TKExceptStrings );
		procedure SetCols( ACol: Integer; Strings: TKExceptStrings );

		procedure ReadLanguages( Reader: TReader );
		procedure WriteLanguages( Writer: TWriter );

	protected
		procedure DefineProperties( Filer: TFiler ); override;
		function GetStringsClass: TKStringsClass; override;

		property OwnerExcept: TKExcept
						 read FKExcept write FKExcept;

	public
		constructor Create( IsOwned: Boolean ); override;

		function Add( S: TKExceptStrings; const AName: string ): Integer; virtual;

		procedure GetHeader( Sender: TObject; Header: TStrings ); override;
		procedure EditorState( Sender: TObject; StringsArray: TKCustomStringsArray;
			State: TEditorState; var Handled: Boolean ); override;

		property RowCount;
		property ColCount;
		property HighColCount;
		property Name;

		property Strings;
		property Objects;
		property Names;
		property Values;
		property Text;
		property CommaText;

		property Rows[ARow: Integer]: TKExceptStrings
						 read GetRows write SetRows;
		property Cols[ACol: Integer]: TKExceptStrings read GetCols write SetCols;
		property RowsByLanguage[ARow: TLanguage] : TKExceptStrings
						 read GetLangRow;
		property Languages[ARow: Integer]: TLanguage
						 read GetLanguages write SetLanguages;
		property ColsAsRows default True;

	end;

{ TKExcept }

	TKOnExceptionEvent =	procedure( Sender: TKExcept; E: Exception;
		Known: Boolean;	var Display, Feedback: Boolean ) of object;

	TKOnFeedBackEvent = procedure( Sender: TKExcept; E: Exception;
		const ExceptOriginal: string; var Comment: string ) of object;

	TKOnTranslateEvent = procedure ( Sender: TKExcept; E: Exception;
		var Translated: Boolean ) of object;

	TKExceptRequestFileNameEvent = procedure( Sender: TKExcept; E: Exception;
		var FileName: string;	const FileDate: TDateTime; const FileSize: LongInt ) of object;

	TKExcept = class( TKCustomAuditable )
	private
		FDialogMessage: string;
		FMsgList: TKExceptStringsArray;
		FOnException: TKOnExceptionEvent;
		FOnBeforeTranslate: TKOnTranslateEvent;
		FOnTranslate: TKOnTranslateEvent;
		FOnFeedBack: TKOnFeedBackEvent;
    FKExceptRequestFileName: TKExceptRequestFileNameEvent;
		FHasUserComment: Boolean;
		FTranslate: Boolean;
		FLanguage: TLanguage;

		function GetExceptLogError: TKExceptLogErrorEvent;
		procedure SetExceptLogError( Value: TKExceptLogErrorEvent );

		procedure SetMessages( const Value: TKExceptStringsArray );
		procedure SetLanguage( const Value: TLanguage );
		function GetExceptLogFile: TKExceptLogFile;

	protected
		procedure DoRequestFileName( Data: Pointer ); override;

		procedure ProcessExceptions( Sender: TObject; E: Exception ); virtual;
		procedure DoBeforeTranslate( E: Exception; var MsgTranslated: Boolean ); dynamic;
		procedure DoTranslate( E: Exception; var MsgTranslated: Boolean ); dynamic;
		procedure DoFeedBack( E: Exception; const ExceptOriginal: string;
			var Comment: string ); dynamic;
		function  TranslateExceptionMessage( E: Exception ): Boolean; virtual;
		procedure LogExcept( E: Exception; const ExceptOriginal: string;
			FeedBack: Boolean ); dynamic;
		procedure LoadDefaultMessages; virtual;

		function GetAuditoryLogClass: TKCustomAuditoryLogClass; override;

		property DialogMessage: string
						 read FDialogMessage;
		property ExceptLog: TKExceptLogFile
						 read GetExceptLogFile;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Handle;
		property FileName;
		property AppUserName;

	published

		property Enabled;
		property Language: TLanguage
						 read FLanguage write SetLanguage default K_LANG_PORTUGUESE_BRAZIL;
		property HasUserComment: Boolean
						 read FHasUserComment write FHasUserComment default False;
		property Messages: TKExceptStringsArray
						 read FMsgList write SetMessages;
		property Translate: Boolean
						 read FTranslate write FTranslate default true;

		property BeforeTranslate: TKOnTranslateEvent
						 read FOnBeforeTranslate write FOnBeforeTranslate;
		property OnException: TKOnExceptionEvent
						 read FOnException write FOnException;
		property OnFeedBack: TKOnFeedBackEvent
						 read FOnFeedBack write FOnFeedBack;
		property OnRequestFileName: TKExceptRequestFileNameEvent
						 read FKExceptRequestFileName write FKExceptRequestFileName;
		property OnLogError: TKExceptLogErrorEvent
						 read GetExceptLogError write SetExceptLogError;
		property OnTranslate: TKOnTranslateEvent
						 read FOnTranslate write FOnTranslate;

	end;

implementation

{$R ..\Lib\sreExcept.res}

{.$R u:\delphi\klib100\source\ext100\lib\sreExcept.res

 We try to use this form to D4 complaint but this generates a incomatible
 directory list for other delphis.... sorry...
}

uses
	Consts, Forms, uksyConsts, ukrConsts, ukrResStr, ukeConsts, ukeResStr, ukeUtils;

{
---------------------------------------------------------------------------------
-------------------------- Except Log File Architecture -------------------------
---------------------------------------------------------------------------------
}

const
	EX_ERRORCLASS_FIELD  = 'Exception.Class';
	EX_ORIGINALMSG_FIELD = 'Exception.Original';
	EX_TRANSMSG_FIELD    = 'Exception.Translated';
	EX_USERCOMMENT_FIELD = 'Exception.UserComment';

	EX_ERRORCLASS_FIELD_INDEX  = 0;
	EX_ORIGINALMSG_FIELD_INDEX = 1;
	EX_TRANSMSG_FIELD_INDEX    = 2;
	EX_USERCOMMENT_FIELD_INDEX = 3;

	EX_EXPTMODULE_VIRTUAL = 'Exception.Module';
	EX_EXPTROUTINE_VIRTUAL = 'Exception.Routine';
	EX_EXPTLINE_VIRTUAL = 'Exception.Line';

	EX_EXPTATT_SIZE = 8;

	EX_EXPTMODULE_VIRTUAL_SIZE = SizeOf( ShortString ) - 1;
	EX_EXPTROUTINE_VIRTUAL_SIZE = ( MAXSIZE_PASCALID * 2 ) + 1;

{ TKExceptLogFile }

constructor TKExceptLogFile.Create( AOwner: TComponent; const AFileName: TFileName );
begin
	inherited Create( AOwner, AFileName );
	DefFileExt := EXPT_LOGEXT;
end;

function TKExceptLogFile.GetLogOwner: TKExcept;
begin
	Result := TKExcept( inherited GetLogOwner );
end;

procedure TKExceptLogFile.DoOnLogFileLogError( Data: Pointer;
	var LogErrorAction: TKLogErrorAction );
begin
 	DoOnExceptLogError( Exception( Data ), LogErrorAction );  
end;

procedure TKExceptLogFile.DoOnExceptLogError( AException: Exception;
	var LogErrorAction: TKLogErrorAction );
begin
	if Assigned( FOnExceptLogError ) and ( not Designing( Owner ) ) then
		FOnExceptLogError( Owner, AException, LogErrorAction );
end;

function TKExceptLogfile.GetExceptAddr: string;
{ See System.TextStart }
const
	SymBolHeaderOfs: Integer = {$IFDEF DELPHI4} $1000 {$ELSE}
		{$IFDEF DELPHI3} $1000 {$ENDIF} { for delphi 2 and 1 - Error! } {$ENDIF};
var
	iConvAddr: Integer;
begin
	if CheckPointer( ExceptAddr ) then
	begin
{ Memory Basic Information that we need to converted the logical value of except Address
	to a physical (nearest) value in the map file }
		ZeroMemory( @FMBInfo, SizeOf( TMemoryBasicInformation ) );
{ MUST BE HERE DON'T PUT IN THE CONSTRUCTOR }
		VirtualQuery( ExceptAddr, FMBInfo, SizeOf( TMemoryBasicInformation ) );
{
	This is the formula to take the efective error address in the map file.
	AllocationBase is the $IMAGEBASE pre compiler directive. $1000 is the
	header of any procedure or function in a object pascal compiler definition.
	In D3 and D4, this value is $1000 and in D2, this value is $1200 (TextStart).
}
		iConvAddr := Integer( ExceptAddr ) - Integer( FMBInfo.AllocationBase ) -
			SymbolHeaderOfs;
		Result := IntToHex( iConvAddr, EX_EXPTATT_SIZE );
	end
	else
		Result := sErrExInvExceptAddr;
end;

procedure TKExceptLogFile.DefineFixedFields;
begin
	inherited DefineFixedFields;
	DefineFixedField( EX_FIXEDFLDNAME_EXPTADDR, EX_EXPTATT_SIZE, GetExceptAddr );
end;

procedure TKExceptLogFile.DefineCustomFields;
begin
	inherited DefineCustomFields;
	DefineCustomField( EX_ERRORCLASS_FIELD, MAXSIZE_PASCALID );
	DefineCustomField( EX_ORIGINALMSG_FIELD, MAXSIZE_MSG );
	DefineCustomField( EX_TRANSMSG_FIELD, MAXSIZE_MSG );
	DefineCustomField( EX_USERCOMMENT_FIELD, MAXSIZE_MSG );
end;

{
---------------------------------------------------------------------------------
------------------------- Except Log Engine Architecture ------------------------
---------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

const
	TEXTFILE_EXT = '.txt';
	KLOG_DATETIME_FORMAT = 'dd/mm/yyyy hh:nn:ss';

procedure CreateKnowHowMapFileEx( const MapFileName, KMapFileName: TFileName;
	ReturnStream: TStream );

var
	kss : TKStringStream;
	ks: TKStrings;
	sIn: string;

{ Utility routines }
	function GetAddr_AND_Name_Pair( const S: string ): string;
	begin
		{ Given the string, copy only the address and name parts. Skip spaces segment }
		Result := Copy( S, 7, 8 ) + CH_EQUAL_TOKEN +	Trim( Copy( S, 15, Length( S ) ) );
	end;

	function GetLine_AND_Addr_Pair( var S: string ): string;
	begin
		{ Given the string, copy only the first line number and address part and consume the
			passed string.}
		Result := Trim( Copy( S, 1, 6 ) + CH_EQUAL_TOKEN + Copy( S, 13, 8 ) );
		S := Copy( S, 21, Length( S ) );
	end;

	function GetModuleName( const Source: string ): string;
	const
		MODULE_START_POS = 18;
	var
		s: string;
		i: Integer;
	begin
		s := '';
		{ Given the string, copy only the module name and path!
													18!
		 Ex: Line numbers for TypInfo(TypInfo.pas) segment .text }
		i := MODULE_START_POS;
		while ( Source[i] <> CH_SPACE ) do
		begin
			s := s + Source[i];
			Inc( i );
		end;
		Result := s;
	end;

	{Functional routines}
	procedure SkipLinesUntilPublicValues;
	var
		iPos: Integer;
	begin
		{Consume the file until the start point for start reading}
		iPos := Pos( PUBLICS_BYVALUES_SIGNATURE, kss.DataString );
		if ( iPos > 0 ) then
		begin
			kss.Position := iPos + Length( PUBLICS_BYVALUES_SIGNATURE ) + 1;
			kss.SkipSpaces;
		end
		else
			kss.Position := kss.Size;
	end;

	procedure ReadAddressNamePairs;
	var
		iFirstAddr,
		iLastAddr : Integer;

	begin
		iFirstAddr := 0;
		sIn := kss.GetLine;
{ Read every method Address and Names only (skip PUBLIC_VARIABLES_ADDRESSES_SEGMENT) }
		while ( CheckTrimStr( sIn ) and
						CheckStrContains( METHODS_ADDRESSES_SEGMENT, sIn ) ) do
		begin
{
	Insert the Address and the method name for each pair method/address and
	maintain two flags, iFirstAddr and iLastAddr for the control of the module name!

	Add a line until find the finalization section. iLastAddr has the index of the
	lasted added string until finalization (add it too).
}
			iLastAddr := ks.Add( GetAddr_AND_Name_Pair( sIn ) );

{
	When "arrives" at finalizartion, the next line is the module name, so,
	Add this string as the last iFirstAddr position into the list. After that
	adjust iFirstAddr to iLastAddr for the next module when it going to be read.
}
			if ( Pos( 'Finalization', sIn ) = 22 ) then
			begin
{
	if the founded line is Finalization, get next line (suposed to be the module
	name) and insert it at a correct position
}
				sIn := kss.GetLine;
				if ( CheckTrimStr( sIn ) and
						CheckStrContains( METHODS_ADDRESSES_SEGMENT, sIn ) ) then
				begin
{ Insert as a new section in the format: ["ModAddr"="ModName"] }
					ks.Insert( iFirstAddr, '['+GetAddr_AND_Name_Pair( sIn )+']' );
					Inc( iLastAddr, 2 );
{ Adjust FirstAddr }
					iFirstAddr := iLastAddr;
				end
				else
{ if the next line readed mismach the while condition, break! } 
					Break;
			end;
			sIn := kss.GetLine;
{ When finish, we has a list of addresses and names separeted by modules names }
		end;
	end;

	procedure MakeAditionalModuleAddressSection;
	var
		i,
		j,
		iFirst,
		iLast: Integer;

	begin
{ Just to maintain compatibility and to gain better future access! }
{ Get the count before the loop because we'll go to add new entries at the end of list! }
		j := ks.Count;
{ Add the new Section! } 
		iFirst := ks.Add( MODULES_ADDR_SECTION );
		iLast := iFirst;
		for i := 0 to j - 1 do
			if ( ks.Strings[i][1] = '[' ) then
				iLast := ks.Add( Copy( ks.Strings[i], 2, Length( ks.Strings[i] )-2 ) );
		j := 0;
		if ( iLast > iFirst ) then
{ Adjust it to be at the begining of the strings! }
			for i := iFirst to iLast do
			begin
				ks.Move( i , j );
				Inc( j );
			end;
	end;

	procedure SkipAddressVarPairs;
	begin
{ sIn was initialized in ReadAddressNamePairs last statement }
{ Consume the file until the start point for reading line numbers }
		sIn := kss.GetLine;
		while ( CheckTrimStr( sIn ) and
						CheckStrContains( PUBLICS_VARIABLES_ADDRESSES_SEGMENT, sIn ) ) do
			sIn := kss.GetLine;
		if ( not CheckTrimStr( sIn ) ) then
			kss.SkipSpaces
		else
			kss.Position := kss.Position - ( Length( sIn ) + 2 );
	end;

	procedure ReadLineAddressPairs;
	begin
{ Get the first Lines numbers for... }
		sIn := kss.GetLine;
		while ( CheckTrimStr( sIn ) and
						CheckStrContains( LINE_NUM_SIGNATURE, sIn ) ) do
		begin
			kss.Position := kss.Position + 2; { Skip CRLF pair. }

{
	Get only the important information from the Lines numbers for... line
	In this case, here we have a new section for line numbers
}
			ks.Add( '[' + GetModuleName( sIn ) + ']' );
			sIn := kss.GetLine;
{ Get each line that contain the line/addr pairs until the end }
			while CheckTrimStr( sIn ) do
			begin
{
  While we have line number/addresse pair in this line, add it to the list at
	the current modules lines section. The function consume sIn and return a
	correct and formatted pair
}
				while CheckStr( sIn ) do
					ks.Add( GetLine_AND_Addr_Pair( sIn ) );
				sIn := kss.GetLine;
			end;

{ After the end of line number/addr pairs Skip Spaces }
			kss.SkipSpaces;
{ Get next Lines numbers for... }
			sIn := kss.GetLine;
		end;
	end;

	procedure MakeAditionalMapCompatibilitySection;
	begin
		ks.Insert( KSA_INDEX_MAPCOMPATIBILITY, MAPCOMPATIBILY_SECTION );
{
	Log in this section the DateTime creation of the .Map file and its size (at the
	begining of the file for better and faster access)
}
		ks.Insert( KSA_INDEX_MAPCOMPATIBILITY+1, FormatDateTime( KLOG_DATETIME_FORMAT,
			FileDateToDateTime( FileAge( MapFileName ) ) ) + CH_EQUAL_TOKEN + IntToStr(kss.Size) );
	end;

	procedure SaveKnowMapFile;
	var
		kMap: TFileName;
	begin
		if ( not CheckStrContains( KNOWHOW_MAPFILE_EXT, KMapFileName ) ) then
			kMap := ChangeFileExt( KMapFileName, KNOWHOW_MAPFILE_EXT )
		else
			kMap := KMapFileName;
    ForceDeleteFile( kMap );
		ks.SaveToFile( kMap );
	end;

var
	fs : TFileStream;
begin
	ForceFile( MapFileName );
	fs := TFileStream.Create( MapFileName, ( fmShareExclusive or fmOpenRead ) );
	try
		kss := TKStringStream.Create( '' );
		try
			ForceStreamCopy( fs, kss );
			kss.Position := 0;
			ks := TKStrings.Create;
			try
{ Separete the methods for better undertanding and subsequent profilling }

				SkipLinesUntilPublicValues;						{Go to Address  Publics by Values}
				SkipAddressVarPairs;									{Skip the first or more 0002: values}
				ReadAddressNamePairs;									{Read all Address Section}
				MakeAditionalModuleAddressSection;		{Search the list and make a fake section}
				SkipAddressVarPairs;									{Skip the rest of more 0002: values}
				ReadLineAddressPairs;									{Read all Lines sections for all modules}
				MakeAditionalMapCompatibilitySection; {Add information for the map file size}

				SaveKnowMapFile;											{Save the StringList into a resultant KnowHowMapFile}
				if CheckObject( ReturnStream ) then
				begin
					sIn := ks.Text;
					if CheckStr( sIn ) then
  					ReturnStream.WriteBuffer( Pointer( sIn )^, Length( sIn ) );
				end;
			finally
				ks.Free;
			end;
		finally
			kss.Free;
		end;
	finally
		fs.Free;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKExceptStrings }

procedure TKExceptStrings.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKExceptStrings ) then
		Language := ( Source as TKExceptStrings ).Language
	else
		inherited Assign( Source );
end;

procedure TKExceptStrings.SetLanguage(const Value: TLanguage);
begin
	if ( Value <> Language ) then
	begin
		ForceLanguageValue( Value );
		FLanguage := Value;
	end;
end;

{ TKExceptLogEngine }

constructor TKExceptLogEngine.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FHasMapInfo := True;
	FMapLoaded := False;
	DefFileExt := EXPT_LOGEXT;
	AutoLoad := True;

	iSecRow := -1;
	iModAddrRow := -1;
	iModNameRow := -1;
	iCurrentConvExptAddr := -1;
	iCurrentRoutine := -1;
	iCurrentModule := -1;
	iCurrentRoutineAddrRow := -1;
	iCurrentLineRow := -1;

	sCurrentConvExptAddr := '';
	sCurrentRoutine := '';
	sCurrentModule := '';

end;

destructor TKExceptLogEngine.Destroy;
begin
	FMapLoaded := False;
	if CheckObject( FStrArray ) then
	begin
		ClearKMapInfo;
		FStrArray.Free;
	end;
	FStrArray := nil;
	inherited Destroy;
end;

function TKExceptLogEngine.GetMapFileSections( Index: Integer ): ShortString;
begin
	if ( FMapLoaded and ( Index <> -1 ) ) then
		with FStrArray do
			Result := Rows[iSecRow].Strings[Index]
	else
		Result := KSA_VALUE_NOTFOUND;
end;

function TKExceptLogEngine.GetExceptModule( Index: Integer ): ShortString;
begin
	if ( Index <> -1 )then
		with FStrArray do
			Result := Rows[iModNameRow].Names[Index]
	else
		Result := KSA_VALUE_NOTFOUND;
	sCurrentModule := Result;
end;

function TKExceptLogEngine.GetExceptRoutine( Index: Integer ): ShortString;
begin
	if ( Index <> -1 )then
		with FStrArray do
			Result := Rows[iCurrentRoutineAddrRow].ValuesByIndex[Index]
	else
		Result := KSA_VALUE_NOTFOUND;
	sCurrentRoutine := Result;
end;

function TKExceptLogEngine.GetExceptLine( Index: Integer ): ShortString;
begin
	if ( Index <> -1 ) then
		with FStrArray do
			Result := Rows[iCurrentLineRow].Names[Index]
	else
		Result := KSA_VALUE_NOTFOUND;
end;

function TKExceptLogEngine.ExceptAddrModuleIndex( const ConvExceptAddr: ShortString ): Integer;
begin
	if FMapLoaded then
		with FStrArray do
		begin
			Result := Rows[iModAddrRow].GetNearestNameIndex( True, ConvExceptAddr );
			sCurrentModule := Rows[iModAddrRow].ValuesByIndex[Result];
		end
	else
	begin
		Result := -1;
		sCurrentModule := KSA_VALUE_NOTFOUND;
	end;
	iCurrentRoutineAddrRow := -1;
	iCurrentLineRow := -1;
	iCurrentModule := Result;
{ sCurrentModule := sCurrentRoutine }
end;

function TKExceptLogEngine.ExceptAddrRoutineIndex( const ConvExceptAddr: ShortString ): Integer;
begin
	if ( iCurrentModule = -1 ) then
	begin
		Result := -1;
		Exit;
	end;
	with FStrArray do
	begin
{
	Given a Converted Except Address, get the lower nearest value into the kmp file that
	is the routine address, so, use this result in the ExceptRoutine property to get the
	correct routine name given a ConvExceptAddr!
}
		iCurrentRoutineAddrRow := IndexOfPropName( Format( KSA_NAME_ADDRESSES, [sCurrentModule] ) );
		Result := Rows[iCurrentRoutineAddrRow].GetNearestNameIndex( False, ConvExceptAddr );
		iCurrentRoutine := Result;
	end;
end;

function TKExceptLogEngine.ExceptAddrLineIndex( const ConvExceptAddr: ShortString ): Integer;
begin
	Result := -1;
	if ( iCurrentModule = -1 ) then
		Exit;
	with FStrArray do
	begin
{ Get the Routine Module Name index of this converted Except Address }
		iCurrentLineRow := IndexOfPropName( Format( KSA_NAME_UNITNAME, [sCurrentModule] ) );
		if ( iCurrentLineRow <> -1 ) then
			Result := Rows[iCurrentLineRow].GetValueIndex( ConvExceptAddr );
	end;
end;

function TKExceptLogEngine.GetExceptModuleByAddr( const ConvExceptAddr: ShortString ): ShortString;
begin
	Result := GetExceptModule( ExceptAddrModuleIndex( ConvExceptAddr ) );
end;

function TKExceptLogEngine.GetExceptRoutineByAddr( const ConvExceptAddr: ShortString ): ShortString;
begin
	Result := GetExceptRoutine( ExceptAddrRoutineIndex( ConvExceptAddr ) );
end;

function TKExceptLogEngine.GetExceptLineByAddr( const ConvExceptAddr: ShortString ): ShortString;
begin
	Result := GetExceptLine( ExceptAddrLineIndex( ConvExceptAddr ) );
end;

function TKExceptLogEngine.GetExceptModuleCallBack( Index: Integer ): string;
{
 var
	iModIndex: Integer;
}
begin
{
	The passed Index is the RecordNumber in the LogFile. So, get the LogRecord ExceptAddr
	field for this RecordNumber. After that, you can call the correct propety that returns
	the result string via the Logged Converted except address.
}
	iCurrentConvExptAddr := Index;
	sCurrentConvExptAddr := LogRecords[EX_FIXEDFLDNAME_EXPTADDR, Index];
	{iModIndex :=} ExceptAddrModuleIndex( sCurrentConvExptAddr );
	Result := sCurrentModule;{GetExceptModule(iModIndex)}
end;

function TKExceptLogEngine.GetExceptRoutineCallBack( Index: Integer ): string;
begin
{
	The passed Index is the RecordNumber in the LogFile. So, get the LogRecord ExceptAddr
	field for this RecordNumber. After that, you can call the correct propety that returns
	the result string via the Logged Converted except address.
}
	if ( Index <> iCurrentConvExptAddr ) then
		sCurrentConvExptAddr := LogRecords[EX_FIXEDFLDNAME_EXPTADDR, Index];
	Result := GetExceptRoutineByAddr( sCurrentConvExptAddr );
end;

function TKExceptLogEngine.GetExceptLineCallBack( Index: Integer ): string;
begin
{
	The passed Index is the RecordNumber in the LogFile. So, get the LogRecord ExceptAddr
	field for this RecordNumber. After that, you can call the correct propety that returns
	the result string via the Logged Converted except address.
}
	if ( Index <> iCurrentConvExptAddr ) then
		sCurrentConvExptAddr := LogRecords[EX_FIXEDFLDNAME_EXPTADDR, Index];
	Result := GetExceptLineByAddr( sCurrentConvExptAddr );
end;

function TKExceptLogEngine.GetAuditoryLogClass: TKCustomAuditoryLogClass;
begin
	Result := TKExceptLogFile;
end;

procedure TKExceptLogEngine.DefineVirtualFields;
begin
	inherited DefineVirtualFields;
	DefineVirtualField( EX_EXPTMODULE_VIRTUAL, EX_EXPTMODULE_VIRTUAL_SIZE, GetExceptModuleCallBack); 	{Full path module name}
	DefineVirtualField( EX_EXPTROUTINE_VIRTUAL, EX_EXPTROUTINE_VIRTUAL_SIZE, GetExceptRoutineCallBack); {Class.Method !}
	DefineVirtualField(EX_EXPTLINE_VIRTUAL, MAX_LINENUMBER_LENGTH, GetExceptLineCallBack); {99999 max line number length from the map file format, but we have '<not found>' that is 11 len.}
end;

function TKExceptLogEngine.CheckCompatibility( const AMapFileName,
	AkMapFileName: TFileName ): Boolean;
var
	tMapFile,
	tKMapFile: TDateTime;
begin
{
	Test the time stamp of the files, with a Delta of one minute to know if the
	.kmp are compatible with the .map In the future use the Map Compatibility
	Section and test the size and/or more things to ensure compatibility.
}
	tMapFile  := FileDateToDateTime( FileAge( AMapFileName ) );
	tKMapFile := FileDateToDateTime( FileAge( AkMapFileName ) );
	IncMinute( tMapFile, 1 );
	Result := ( tMapFile <= tKMapFile );
end;

procedure TKExceptLogEngine.CreateMapInfo( const AMapFileName: TFileName );
var
	kMap : TFileName;
	bCreate: Boolean;
	ss: TStringStream;
	fs: TFileStream;
begin
	kMap := ChangeFileExt( AMapFileName, KNOWHOW_MAPFILE_EXT );
{
	Create the KnowHowMapFile only if it doesn't exists or if exists but it doesn't
	 compatible with the current map file.
}
	bCreate := not ( FileExists( kMap ) and CheckCompatibility( AMapFileName, kMap ) );
	ss := TStringStream.Create( '' );
	try
		if bCreate then
			CreateKnowHowMapFileEx( AMapFileName, kMap, ss );
		if CheckFile( kMap ) then
		begin
{ if not create, fill the stream with the already present and valid KnowHowMapFile }
			if ( not ( bCreate or CheckStream( ss ) ) ) then
			begin
				fs := TFileStream.Create( kMap, ( fmOpenRead or fmShareExclusive ) );
				try
					ForceStreamCopy( fs, ss );
				finally
					fs.Free;
				end;
			end;
			ss.Position := 0;
			LoadkMapInfo( ss );
		end
		else
			RaiseExceptionFmt( EKExceptLogEngine, sErrExInvFilekMapCreation, [kMap] );
	finally
		ss.Free;
	end;
end;

procedure TKExceptLogEngine.ClearkMapInfo;
var
	i: Integer;
	ks: TKStrings;
begin
	if CheckObject( FStrArray ) then
	begin
		for i := 0 to FStrArray.RowCount - 1 do
		begin
			ks := FStrArray.Rows[i];
			ks.Free;
		end;
		FStrArray.Clear;
		FMapLoaded := False;
	end;
end;

procedure TKExceptLogEngine.LoadkMapInfo( Stream: TStream );

	function IsModuleAddrSection( const SecName: string ): Boolean;
	begin
{ ModAddrSec => [000707FC=uTest]  LineAddrSec => [pExceptTest(U:\Delphi\Tests\Except\pExceptTest.dpr)] }
		Result := ( SecName[1] in CHARSET_DIGIT );
	end;

var
	i,
	iLineRows,
	iAddrRows: Integer;
	sSecName,
	sUnitName,
	sFullUnitName: string;
	ksSec,
	ksModAddr,
	ksModName: TKStrings;
begin
	if ( not CheckObject( FStrArray ) )then
	begin
		FStrArray := TKStringsArray.Create( False );
{ Given the responsability of cleaning nil add's for the class } 
		FStrArray.AcceptNoOwnedNilAdd := True;
	end
	else
		ClearkMapInfo;
	ksSec := nil;
	ksModAddr := nil;
	ksModName := nil;
	try
		ksSec := TKStrings.Create; {use this way to minimize the GetRows in StringsArray}
		ksSec.Name := KSA_NAME_SECTIONS;
		ksModAddr := TKStrings.Create;
		ksModAddr.Name := KSA_NAME_MODADDRESSES;
		ksModName := TKStrings.Create;
		ksModName.Name := KSA_NAME_MODNAMES;
		GetSectionsFromStream( Stream, ksSec );
		Stream.Position := 0;
		GetSectionFromStream( Stream, ksSec[KSA_INDEX_MODADDRESSES], ksModAddr );
		iSecRow := FStrArray.Add( ksSec, '' );
		iModAddrRow := FStrArray.Add( ksModAddr, '' );
		iModNameRow := FStrArray.Add( ksModName, '' );
{
	For each module section, get the module name and create the list of line numbers
	The first and second Sections should not be streamed because it's
	[Map Compatibility] and [Module Addresses] "respectivetly" (so from 2..Count-1).
}
		for i := ( KSA_INDEX_MODADDRESSES+1 ) to ksSec.Count - 1 do
		begin
			sSecName  := ksSec.Strings[i];
			if IsModuleAddrSection( sSecName ) then
			begin
				sUnitName := Copy( sSecName, Pos( CH_EQUAL_TOKEN, sSecName ) + 1,
					Length( sSecName ) - Pos( CH_EQUAL_TOKEN, sSecName ) );
{
  Add another strings array line with the addresses and method names with the current
	section and name this line as Addresses_ModuleName
}
				iAddrRows := FStrArray.Add( nil, Format( 	KSA_NAME_ADDRESSES, [sUnitName] ) );
				GetSectionFromStream( Stream, sSecName , FStrArray.Rows[iAddrRows] );
			end
			else
			begin
				sUnitName := Copy( sSecName, 1, Pos( '(', sSecName ) - 1 );
				sFullUnitName := Copy( sSecName, Pos( '(', sSecName ) + 1,
					Pos( ')', sSecName )-Pos( '(', sSecName ) - 1 );
{ From the section name, get only the unit name }
				ksModName.Add( sUnitName + CH_EQUAL_TOKEN + sFullUnitName );
{
	Add another strings array line with the line numbers and respective addresses with
	the current section and name this line as ModName_UnitName
}
				iLineRows := FStrArray.Add( nil, Format( KSA_NAME_UNITNAME, [sUnitName] ) );
				GetSectionFromStream( Stream, sSecName , FStrArray.Rows[iLineRows] );
			end;
		end;
		FMapLoaded := True; { FileLoaded; to get this point, the FileLoaded is True! }
	except
		ksSec.Free;
		ksModAddr.Free;
		ksModName.Free;
	end;
end;

procedure TKExceptLogEngine.LoadFromFile( const AFileName: TFileName );
begin
	inherited LoadFromFile( AFileName );
	if ( HasMapInfo and FileLoaded and CheckFile( MapFileName ) ) then
		CreateMapInfo( MapFileName );
end;

procedure TKExceptLogEngine.GenerateMapDetailFiles;
var
	i: Integer;
begin
	for i := 0 to FStrArray.RowCount - 1 do
		with FStrArray.Rows[i] do
			SaveToFile( Name + TEXTFILE_EXT );
end;

{
---------------------------------------------------------------------------------
------------------------ Except Log Auditor Architecture ------------------------
---------------------------------------------------------------------------------
}

{ Used in Design-Time Only }

var
	FLangList: TList = nil;

{ TKExceptLink }

const

{ LinkEvents numbers for TKExcept! }

	leBeforeTranslate  = $00;
	leAfterTranslate   = $01;
	leBeforeException  = $02;
	leAfterException   = $03;
	leBeforeDisplay    = $04;
	leAfterDisplay     = $05;
	leBeforeLogExcept  = $06;
	leAfterLogExcept   = $07;

procedure TKExceptLink.DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt );
var
  E: Exception;
begin
	E := Exception ( Data );
	case LinkEvent of
		leBeforeTranslate:
			if Assigned( FBeforeTranslate ) then
				FBeforeTranslate( ( Owner as TKExcept ), Self, E );
		leAfterTranslate :
			if Assigned( FAfterTranslate ) then
				FAfterTranslate( ( Owner as TKExcept ), Self, E );
		leBeforeException:
			if Assigned( FBeforeException ) then
				FBeforeException( ( Owner as TKExcept ), Self, E );
		leAfterException :
			if Assigned( FAfterException ) then
				FAfterException( ( Owner as TKExcept ), Self, E );
		leBeforeDisplay  :
			if Assigned( FBeforeDisplay ) then
				FBeforeDisplay( ( Owner as TKExcept ), Self, E );
		leAfterDisplay   :
			if Assigned( FAfterDisplay ) then
				FAfterDisplay( ( Owner as TKExcept ), Self, E );
		leBeforeLogExcept:
			if Assigned( FBeforeLogExcept ) then
				FBeforeLogExcept( ( Owner as TKExcept ), Self, E );
		leAfterLogExcept :
			if Assigned( FAfterLogExcept ) then
				FAfterLogExcept( ( Owner as TKExcept ), Self, E );
		else
			inherited DoLinkEvent( LinkEvent, Data );
	end;
end;

{ TKExceptStringsArray }

{
The structure of the strings is:

Row's => LanguageName.
Col's => OriginalMsg=TranslatedMsg.
}

constructor TKExceptStringsArray.Create( IsOwned: Boolean );
begin
	inherited Create( IsOwned );
	ColsAsRows := True;
end;

function TKExceptStringsArray.GetStringsClass: TKStringsClass;
begin
	Result := TKExceptStrings;
end;

function TKExceptStringsArray.GetRows( ARow: Integer ): TKExceptStrings;
begin
	Result := ( inherited GetRows( ARow ) as TKExceptStrings );
end;

function TKExceptStringsArray.GetCols( ACol: Integer ): TKExceptStrings;
{
	Tomar cuidado pois ao chamar Cols[ACol] TEM QUE ser dentro de um Try..Finally,
	pois ele cria um TStrings novo para cada coluna requisitada que deve ser
	eliminado por quem chamou
}
begin
	Result := ( inherited GetCols( ACol ) as TKExceptStrings );
end;

procedure TKExceptStringsArray.SetRows( ARow: Integer; Strings: TKExceptStrings );
begin
	inherited SetRows( ARow, Strings );
end;

procedure TKExceptStringsArray.SetCols( ACol: Integer; Strings: TKExceptStrings );
begin
	inherited SetCols( ACol, Strings );
end;

function TKExceptStringsArray.GetLangRow( Index: TLanguage ): TKExceptStrings;
{ Return a row for the ARow Language. If one doesn't exist, return nil. }
var
	i: Integer;
begin
{
	Get the first Row that has the Language property = Index. if not found anyone
	row, return nil.
}
	for i := 0 to RowCount - 1 do
		if ( ( Rows[i] as TKExceptStrings ).Language = Index ) then
		begin
			Result := ( Rows[i] as TKExceptStrings );
			Exit;
		end;
	Result := nil;	
end;

procedure TKExceptStringsArray.SetLanguages( ARow: Integer;
	const Value: TLanguage );
begin
	( Rows[ARow] as TKExceptStrings ).Language := Value;
end;

function TKExceptStringsArray.GetLanguages( ARow: Integer ): TLanguage;
begin
	Result := ( Rows[ARow] as TKExceptStrings ).Language;
end;

procedure TKExceptStringsArray.ReadLanguages( Reader: TReader );
var
	i : Integer;
begin
	Reader.ReadListBegin;
	i := 0;
	while ( not Reader.EndOfList ) do
	begin
		CheckOrCreate( i );
		Rows[i].Language := StringToLanguage( Reader.ReadIdent );
		Inc( i );
	end;
	Reader.ReadListEnd;
end;

procedure TKExceptStringsArray.WriteLanguages( Writer: TWriter );
var
	i: Integer;
begin
	Writer.WriteListBegin;
	for i := 0 to RowCount - 1 do
		Writer.WriteIdent( LanguageToString( Rows[i].Language ) );
	Writer.WriteListEnd;
end;

procedure TKExceptStringsArray.DefineProperties( Filer: TFiler );

	function DoWrite: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := True;
			if CheckObjectClass( Filer.Ancestor, TKExceptStringsArray ) then
				Result := ( not Equals( TKExceptStringsArray( Filer.Ancestor ) ) );
		end
		else
			Result := ( RowCount > 0 ); 
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( 'Languages', ReadLanguages, WriteLanguages, DoWrite );
end;

procedure TKExceptStringsArray.GetHeader( Sender: TObject; Header: TStrings );
var
	i : Integer;
begin
{ The header is the composition of any language name = Stirngs[I, 0]! }
	for i := 0 to RowCount - 1 do
		Header.Add( LanguageToString( ( Rows[i] as TKExceptStrings ).Language ) );
end;

function TKExceptStringsArray.Add( S: TKExceptStrings; const AName: string ): Integer;
begin
	Result := ( inherited Add( S, AName ) );
end;

procedure TKExceptStringsArray.EditorState( Sender: TObject;
	StringsArray: TKCustomStringsArray; State: TEditorState; var Handled: Boolean );
var
	i : Integer;
	pLang: PLanguage;
begin
	case State of
{
	This State was posted when the form is created. After creation it calls the
	preparegrid method that post this method.
}
		esPreparing: FLangList := TList.Create;

{ The editor will clear the rows, so, save the list of languages. }
		esGetting:
		begin
			if CheckObjectClass( StringsArray, TKExceptStringsArray ) then
				with ( StringsArray as TKExceptStringsArray ) do
					for i := 0 to RowCount - 1 do
						if CheckObject( FLangList ) and ( FLangList.Count > i ) then
						begin
							pLang := PLanguage( FLangList.Items[i] );
							Rows[i].Language := pLang^;
							Dispose( pLang );
							FLangList.Items[i] := nil;
						end
						else
							RaiseException( EKExcept, sErrExInvMultiStrExceptEditor );
			if CheckObject( FLangList ) then
				FLangList.Pack;
		end;

{
	When the editor setting the language, it takes informations about the StringsArray
	and put it on the RuleGrid. So save the languages here.
}
		esSetting:
		begin
			if CheckObjectClass( StringsArray, TKExceptStringsArray ) then
				with ( StringsArray as TKExceptStringsArray ) do
					for i := 0 to RowCount - 1 do
{ Adiciona as linguas a lista }
						if CheckObject( FLangList ) then
						begin
							pLang := New( PLanguage );
							pLang^ := Rows[I].Language;
							FLangList.Add( pLang );
						end
						else
							RaiseException( EKExcept, sErrExInvMultiStrExceptEditor );
		end;

{ When the form go out of scope, before the Free call, it post this state }
		esDestroying:
		begin
      if ( not CheckObject( FLangList ) ) then
				RaiseException( EKExcept, sErrExInvMultiStrExceptEditor );
			while CheckList( FLangList ) do
			begin
				pLang := FLangList.Items[FLangList.Count - 1];
				Dispose( pLang );
				FLangList.Delete( FLangList.Count - 1 );
			end;
			FreeClean( FLangList );
		end;
	end;
{ Leave the editor to make the other changes! ( is the default value ). }
	Handled := True;
end;

{ TKExcept }

constructor TKExcept.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTranslate := true;
	FHasUserComment := False;
{ Language of the KExcept Object }
	FLanguage := K_LANG_PORTUGUESE_BRAZIL;
	FDialogMessage := sExceptMessage;
	FMsgList := TKExceptStringsArray.Create( True );
	FMsgList.OwnerExcept := Self;
{ Set TKExceptLogFile.FileName }
	LoadDefaultMessages;
	if ( not Designing( Self ) ) then
		Application.OnException := ProcessExceptions;
end;

destructor TKExcept.Destroy;
begin
	if CheckObject( FMsgList ) then
	begin
		FMsgList.OwnerExcept := nil;
		FreeClean( FMsgList );
	end;
	inherited Destroy;
end;

procedure TKExcept.SetLanguage( const Value: TLanguage );
begin
	if ( Value <> FLanguage ) then
	begin
		ForceLanguageValue( Value );
		FLanguage := Value;
	end;
end;

procedure TKExcept.SetMessages( const Value: TKExceptStringsArray );
begin
	FMsgList.Assign( Value );
end;

function TKExcept.GetExceptLogFile: TKExceptLogFile;
begin
	Result := TKExceptLogFile( inherited GetAuditoryLog );
end;

function TKExcept.GetAuditoryLogClass: TKCustomAuditoryLogClass;
begin
	Result := TKExceptLogFile;
end;

function TKExcept.GetExceptLogError: TKExceptLogErrorEvent;
begin
	Result := ExceptLog.OnExceptLogError;
end;

procedure TKExcept.SetExceptLogError( Value: TKExceptLogErrorEvent );
begin
  ExceptLog.OnExceptLogError := Value;
end;

procedure TKExcept.DoRequestFileName( Data: Pointer );
var
	sFileName: string;
	dFileDate: TDateTime;
	iFileSize: LongInt;
begin
	inherited DoRequestFileName( Data );
	if Assigned( FKExceptRequestFileName ) then
	begin
		sFileName := FileName;
		if CheckFile( sFileName ) then
		begin
			dFileDate := GetFileDateCreated( sFileName );
			iFileSize := GetFileSize( sFileName, nil );
		end
		else
		begin
			dFileDate := Now;
			iFileSize := 0;
		end;
		FKExceptRequestFileName( Self, Exception( Data ), sFileName, dFileDate, iFileSize );
		if CheckTrimStr( sFileName ) then
			FileName := sFileName;
	end;
end;

procedure TKExcept.ProcessExceptions( Sender: TObject; E: Exception );
var
	bKnown,
	bDisplay,
	bFeedback: Boolean;
	sOriginal: string;
begin
	if ( not Enabled ) then
	begin
{ If not Enabled, proceed with a default exception handling mechanism }
		Application.ShowException( E );
		Exit;
	end;
	sOriginal := E.Message;
	bDisplay := true;
	bKnown := CheckObjectClass( E, EKnowHow );
	bFeedback := false;
{
	Antes era com "or", mas agora o mecanismo de Translate mudou!
	Se for EKnowHow, Translate é true desde o início, ficando false apenas se
	não tiver Message registrada ou ainda no evento MsgTranslated retornar false
	( que é o Default )!
}
	if Translate then
	begin
		NotifyLinks( leBeforeTranslate, LongInt( E ) );
		DoBeforeTranslate( E, bKnown );
		if ( not bKnown ) then
			bKnown := ( TranslateExceptionMessage( E ) or	CheckObjectClass( E, EKnowHow ) );
		NotifyLinks( leAfterTranslate, LongInt( E ) );
	end;
	if CheckStr( E.Message ) and ( not ( AnsiLastChar( E.Message )^ in ['.', '!', '?'] ) ) then
		E.Message := E.Message + '.';
	if Assigned( FOnException ) then
	begin
		bFeedback := ( HasUserComment and ( not bKnown ) );
		NotifyLinks( leBeforeException, LongInt( E ) );
		FOnException( Self, E, bKnown, bDisplay, bFeedback );
		NotifyLinks( leAfterException, LongInt( E ) );
	end;
	if bDisplay then
	begin
		NotifyLinks( leBeforeDisplay, LongInt( E ) );
		if CheckObjectClass( E, EKLIB ) then
			ShowDialog( Application.Title, DialogMessage + E.Message, nil, dsOk, boSoftBug )
		else
			ShowDialog( Application.Title, DialogMessage + E.Message, nil, dsOk, boHardBug );
		NotifyLinks( leAfterDisplay, LongInt( E ) );
	end;
	NotifyLinks( leBeforeLogExcept, LongInt( E ) );
{ We cannot log an exception raised within the context of the ExceptLogFile class! }
	if ( not CheckObjectClass( E, EKLogAPI ) ) then
		LogExcept( E, sOriginal, bFeedback );
{ for EKLogFile exceptions use the leAfterLogExcept for write the log entry manualy ? }
	NotifyLinks( leAfterLogExcept, LongInt( E ) );
end;

procedure TKExcept.DoFeedBack( E: Exception; const ExceptOriginal: string;
  var Comment: string );
begin
	if Assigned( FOnFeedBack ) then
		FOnFeedBack( Self, E, ExceptOriginal, Comment );
end;

procedure TKExcept.LogExcept( E: Exception; const ExceptOriginal: string;
	FeedBack: Boolean );
var
	sComment: string;
	sl: TStrings;
begin
	sComment := '';
{ Is the exception supposed to be commented ? }
	if Feedback then
	begin
		DoFeedBack( E, ExceptOriginal, sComment );
		if CheckTrimStr( sComment ) then
			sComment := StringReplace( sComment, #13#10, #13#10#9, krfAll );
	end;
	sl := ExceptLog.CreateEmptyLogRecord;
	try
{
	Here sl coming with the format: CustomFieldNameI=.
	In the Except LogFile, we have four default fields ( see DefineCustomFields ).
	For this reason, i put the number directly!

	0 = 'Exception.Class', 63;
	1 = 'Exception.Original', MAX_MSG_SIZE;
	2 = 'Exception.Translated', MAX_MSG_SIZE;
	3 = 'UserComment', MAX_MSG_SIZE*3;

	The string will be trunced if it's larger than the indicated size above and
	will be right padded with blankspaces until it fits in the correct size.
}
		sl.Values[sl.Names[EX_ERRORCLASS_FIELD_INDEX]] := E.ClassName;
		sl.Values[sl.Names[EX_ORIGINALMSG_FIELD_INDEX]] := ExceptOriginal;
		sl.Values[sl.Names[EX_TRANSMSG_FIELD_INDEX]] := E.Message;
		sl.Values[sl.Names[EX_USERCOMMENT_FIELD_INDEX]] := GetFirstString( [sComment,
		  EXCEPTLOG_NOUSERCOMMENT] );
		ExceptLog.LogData( sl, E );
	finally
		sl.Free;
	end;
end;

function TKExcept.TranslateExceptionMessage( E: Exception ): Boolean;
var
	s: string;
	i: Integer;
	skl: TKExceptStrings;
	bMsgTranslated: Boolean;
begin
{ Se for EKnowHow já é conhecida! Porém, ainda podemos traduzir!
	Qualquer mensagem do Delphi (ou nossa) que seja gerada com uma
	string em aspas simples (plick) é automaticamente parametrizável,
	bastando que se ofereça, na versão traduzida, um parâmetro %s para
	formatação; o texto em plicks é extraído da mensagem original e
	substituído no %s da mensagem traduzida. }
	bMsgTranslated := false;
	Result := CheckObjectClass( E, EKnowHow );
	skl := FMsgList.RowsByLanguage[Language];
	if CheckObject( skl ) then
		for i := 0 to skl.Count - 1 do
			if ( ( skl.IndexOfName( E.Message ) <> -1 ) or
					 CheckStrContains( skl.Names[i], E.Message ) ) then
			begin
				s := E.Message;
				bMsgTranslated := True;
				E.Message := skl.Values[skl.Names[i]];
				if ( CheckStrContains( '%s', E.Message ) and { do not resource }
						 ( CountTokens( s, CH_PLICK ) = 2 ) ) then
				begin
					s := Copy( s, Pos( CH_PLICK, s ) + 1, Length( s ) );
					s := Copy( s, 1, Pos( CH_PLICK, s ) - 1 );
					E.Message := Format( E.Message, [CH_PLICK + s + CH_PLICK] );
				end;
				Break;
			end;
	if ( not bMsgTranslated ) then
		DoTranslate( E, bMsgTranslated );
	Result := ( Result and bMsgTranslated );
end;

procedure TKExcept.DoBeforeTranslate( E: Exception; var MsgTranslated: Boolean );
begin
	if Assigned( FOnBeforeTranslate ) then
		FOnBeforeTranslate( Self, E, MsgTranslated );
end;

procedure TKExcept.DoTranslate( E: Exception; var MsgTranslated: Boolean );
begin
	if Assigned( FOnTranslate ) then
		FOnTranslate( Self, E, MsgTranslated );
end;

procedure TKExcept.LoadDefaultMessages;
var
	i,
	j: Integer;
begin
	i := FMsgList.Add( nil, '' );
	FMsgList.Rows[i].Language := K_LANG_PORTUGUESE_BRAZIL;
	for j := 0 to KEX_DEF_MESSAGE_COUNT - 1 do
		FMsgList.Rows[i].Add( LoadStr( EXCEPT_RES_DELERRMSG + j + 1 ) + CH_EQUAL_TOKEN +
			LoadStr( EXCEPT_RES_KERRMSG + j + 1 ) );
{
  Put here the default message to load into the StringsArray
	 Normally messages in the same language of the kexcept object.
}
end;

end.
