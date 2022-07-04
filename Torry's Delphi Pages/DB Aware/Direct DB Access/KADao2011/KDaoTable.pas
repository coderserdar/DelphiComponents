unit KDaoTable;
{$B-}
//******************************************************************************
//                         Delphi Dao Project
//                 Copyright (c) 2000-2001 by Kiril Antonov
//******************************************************************************
{$DEFINE USEPARAMS}           //  Active only in Delphi 5
{$I KADaoCommonDirectives.pas}
//******************************* CHANGES ************************************** 
// 28.05.2000 - Fixed a minor bug which raises exception when
//              getting GetQueryDefSQLText
// 28.05.2000 - Added FieldChanged TList - each item corresponds to a field                        
//              in the record
//              If  Boolean(FieldChanged[X]) is true then when posting data this
//              field is updated
//              This prevents from writing bak entire record to the database -
//              only changed fields are posted.
// 28.05.2000 - Added new property editor for SortedBy property which allows an                         
//              easy method to define sort order of a Table/Query
//              A new property SortedByText gives low level access to the
//              SortedBy property
// 28.05.2000 - Added new property editor for QueryDefParameters property which                        
//              allows an easy method to enter parameters to QUERYDEF dao object
//              A new property QueryDefParametersText gives low level access to
//              the QueryDefParameters property
// 30.05.2000 - Fixed a bug in GetRecNo which gives some troubles with DBGrids
//
// 31.05.2000 - Changed InternalSetToRecord method to speedup positioning inside
//              a table
//
// 01.06.2000 - Created Master/Detail Relationship support with property editor
//              similar to Delphi
//              A few new properties are anounced:
//                - MasterSource : TDataSource; - DataSource of the Master Table
//                - MasterFields : TStrings;    - A StringList with
//                  relationships in the form: "DetailField -> MasterField"
// 04.06.2000 - Handled default values for fields
//
// 07.06.2000 - Handled empty fields (WITH NOT REQUITRED VALUE)
//              when posting new records to database
//
// 08.06.2000 - Added support for Dynamycally setting DAO Version
//
// 11.06.2000 - Filter is working properly now
//
// 11.06.2000 - Added FULL support for Master/Detail (Table and Query)
//
// 11.06.2000 - Changed many options for Bookmarks
//              to support Dynamic DAO (OleVariant)
//
// 11.06.2000 - Added support for Locate
//
// 11.06.2000 - Added support for Lookup
//
// 12.06.2000 - InternalGotoBookmark rewrited completely
//
// 12.06.2000 - Locate supports TLocateOptions now
//
// 12.06.2000 - Added four new methods:
//              CreateField
//              CreateIndex
//              DeleteField
//              DeleteIndex
//
// 14.06.2000 - Property Editors for Tables, QueryDefs and Indexes use
//              RefreshDefinitions to reflect changes made outside Delphi
//
// 18.06.2000 - Added another method of Locate whish is very fast but works only
//              if Table supports bookmarks
//
// 18.06.2000 - Added GetIndexNames method for Compatibility with TTable
//
// 18.06.2000 - Added FOUR New methods for fast search in a table:
//                Find_First
//                Find_Last
//                Find_Next
//                Find_Prior
//              These new methods are similar to TTable but can search on
//              NON-INDEXED fields
//              Call them as calling Locate method
// 19.06.2000 - Two new method where added  Find_Nearest and Find_NearestEx
//              Call Find_NearestEx as calling a Locate method
//              For Find_Nearest you must first call SetKeyFields method with a
//              semicolon separated Field names and then Call Find_Nearest
//              See the new demos on KADao site for full explanation
// 19.06.2000 - Added new method Seek_Nearest
//              Seek_Nearest works as Dao Seek method so you must set the index
//              in which you want to search
//              See the new demos on KADao site for full explanation
//
// 19.06.2000 - Fixed a minor bug with empty tables
//
// 19.06.2000 - Added support for OnFilterRecord event
//              GetRecordCount And GetRecNo now works as standard specifications
//              require
//
// 27.06.2000 - Added CompareBookmarks method - now multiselect in DBGrid
//              works fine
//
// 28.06.2000 - Added GetFieldNames - it receives as a parameter TStringList
//              and fills them with Names of the fields
//              Each Name has a corresponding TObject wich is an integer
//              describing a Field original DAO type (Not BDE type)
//
// 28.06.2000 - Added QueryDefODBCMaxRecords (works only on ODBC data sources)
//              to limit number of returned records
//              Setting to 0 means NO LIMIT
//
// 28.06.2000 - Added QueryDefType property - it returns a QueryDef Type
//              as a string. Original DAO value is stored in QueryDefTypeInt
//
// 28.06.2000 - Added RecordsAffected variable
//              When using  ExecSQL it teturns then number of affected records
//              and also sets RecordsAffected to the same value.
//
// 28.06.2000 - Added Requery method which is useful for refreshing dynaset
//              tables
//
// 28.06.2000 - Added Seek_NearestEx method
//              An additional parameter is SeekType (String) which can be one of
//              the following: '<', '<=', '=', '>=', '>'
//
// 28.06.2000 - Added SetRecNo internal dataset method (still in beta testing)
//
// 28.06.2000 - Added two new variables BlobOffset and BlobNumBytes
//              Whend one of this variables is different then zero
//              reading from a blob field starts from BlobOffset position
//              and the return information is BlobNumBytes in size
//              When BlobNumBytes is > of entire blob size a smaller amount
//              of bytes is returned (realized using DAO GetChunk method)
//              Warninng! This is blob wide i.e. all blobs are affected
//              So you must set them to Zero each time when the another blob
//              which must be read at all is readed from the record
//
// 28.06.2000 - Added two new read only properties TableDateCreated and
//              TableLastUpdated - works only on standart tables and QueryDefs
//
// 28.06.2000 - Added a new meton AppendToBlob - uses DAO AppendChunk method
//
// 29.06.2000 - Added a new variable QueryDefReturnParams of type OleVariant
//              It contains a results from a QueryDefRecordset
//              If result is only one QueryDefReturnParams is a single variant
//              otherwise QueryDefReturnParams is VarArray
//
// 29.06.2000 - AT LAST FIXED PROBLEM WITH EMPTY RECORDESTS - GREAT VICTORY!!!!
//
// 29.06.2000 - Removed SetRecNo internal dataset method (not yet understand)
//
//
// 03.07.2000 - Added new property UseRecordCount
//              Since DBGrid uses RecordCount Very extensivelly which can
//              slowdown database performance you can turn it off by setting
//              UseRecordCount
//
// 04.07.2000 - AT LAST FIXED PROBLEM WITH RETRIEVING ACTUAL TYPE OF
//              dbDate FIELD TYPE - NOW YOU CAN USE dbTime AND dbTimeStamp !!!
//
// 05.07.2000 - Fixed a very rediculous bug with RETRIEVING ACTUAL TYPE OF
//              dbDate FIELD. Now a ftDateTime is the default type
//              But if you set Format property in MS Acess a dbDate and dbTime
//              also is used!
// 05.07.2000 - Added additional code to DateTimeToBuffer and TimeToBuffer
//              routines to support both method of retrieving Date/Time info
//
// 05.07.2000 - Added support for Forward Only Tables - Works Good but
//              DBGrid violates forward only restrictions so use with care
//              A more complicated changes will be made in future to avoid these
//
// 17.07.2000 - Fixed a bug which does not free allocated resources in Append
//              With many thanks to Andrew Baylis for reporting the problem
//
// 19.07.2000 - Added new property UseBrackets - True by default
//              It places Field names in squire brackets "[ ]" when using
//              Locate, Lookup, and Master/Detail
//              Since squire brackets are MS Access specific turn this property
//              to FALSE when using other databases than MDB
//
// 19.07.2000 - Added support for working with part of all fields
//              I.E Field Designer is supported now
//              Not copletely tested but working
//
// 21.07.2000 - Added few new Exec Functions
//                - ExecuteSQL - Executes SQL stored in SQL Property
//                - ExecuteQueryDefSQL - Executes SQL stored selected
//                  by QueryDefName QueryDef
//
// 21.07.2000 - Added Property LockEdits for Locking Recods at runtime
//              Immediatly after you call Edit metod locking is activa
//
// 24.07.2000 - Added new method for Locating data
//              If table type is dbOpenTable and IndexName <> ''
//              then locate tryes to use selected index when searching
//              otherwise a standard search is executed
//
// 30.07.2000 - Added new property SQLExecutionType for use when executing SQL
//              by default it is DaoApi.dbFailOnError but you may use andother
//              constants like DaoApi.dbSQLPassThrough
//              (With many thanks to to Baldemaier Florian for this)
//
// 31.07.2000 - Fixed ALL problems with ACCESS Date and Time conversion
//              All Borland types i.e ftDate, ftDateTime and ftTime can be used
//              now. The magic number is 693594. Do you know why? I know!
//
// 31.07.2000 - Added new method for Find_First, Find_Next Etc..
//              if somebody encounter problems please report ...
//
// 04.08.20000 - A specific change made to Locate
//               her must be written a some special notes to use locate with
//               an index
//               Microsoft has made strange things with its Seek method
//               So to work with indexes you must create index containing ALL
//               fields you willlocate on and no EXTRA fields in this index
//               Otherwise Locate will use non index based method
//               And Microsoft's limitation is 13 fileds maximum (0..12)
//               Have a nice locating! :-)
//               P.S If somebody encounter problems please report ...
//
// 14.08.2000  - The TKBlobStream.Truncate Procedure was rewritten becouse
//               it does not clear Blob fields proprly - lets say it was doing
//               NOTHING. Now it works fine
//               With many thanks to Andrew Baylis for reporting the problem
//
//
// 14.08.2000  - Changed SetFieldData so Field.Clear to work
//               With many thanks to Andrew Baylis for reporting the problem
//
// 15.08.2000  - Added some features to speedup adding new records
//               Now a new system var F_UpdatableFields of type tlist
//               presents all records that can be updated
//               if Boolean(F_UpdatableFields.Items[xxx]) then field can
//               be changed
//               Also a Resync[] in Internal Post is blocked
//               P.S If somebody encounter problems please report ...
// 18.02.2000  - Added a fix to SortedbyDialogEditor to use brackets
//               With many thanks to Baldemaier Florian for reporting problem
//
// 22.08.2000  - Fixed a bug with setting LockEdits property on tables which
//               does not support Locking
//               With many thanks to Dave Zangger for reporting problem
//
// 28.08.2000  - Fixed a bug with generating SQL for Lookup, Locate etc
//               a ftSmallInt and ftWord was missing. Now included
//               Thanls to Analisis y Estudios Financieros for reporting problem
//
// 29.08.2000  - Added some code for QueryDefTimeOut and ODBCTimeOut
//
// 31.08.2000  - Added IsEmpy Checking for Locate, Lookup and Find methods
//               With many thanks to Jiri Kanda for reporting problem
//
// 07.09.2000 - GetRecNo now retuns a 1 based value not a zero bazed
//              Most of TDatasets do so - also this helps on dbGrids
//              Thanls to Jiri Kanda again
//
// 08.09.2000 - Fixed a bug in F_Set_Filtered method thanks to Oliver Häger
//
// 21.09.2000 - Fixed a strange DAO bug in QueryDefs when concatenating
//              dbText fields. Dao returns ZERO for the result field length.
//              Now this situation is handled - result size is 255!
//              Thanls to Tom Peiffer for reporting problem
//
// 21.09.2000 - Added GotoCurrent Method - same as TTable.GotoCurrent
//
// 01.10.2000 - Fixed a VERY BIG bug with RecordLocking.
//              My apologese to everybody that report problems with
//              record locking. But Borland nas NO Documentation about
//              internal TDataset routines. Now all is OK
//
// 01.10.2000 - Fixed a bug with ExecuteQueryDefSQL - it does not handle
//              QueryDefParameters but now they are supported
//              Thanls to Jiri Kanda for reporting the problem
//
// 01.10.2000 - Fixed a bug with Requery - it does not handle
//              QueryDefParameters but now they are supported
//
// 01.10.2000 - Added support for ftAutoInc
//
// 01.10.2000 - Added two new Functions
//                  - GetSourceTableName
//                  - GetSourceFieldName
//              They are very usual to find source TableName and FieldName when
//              using a result from join query and want to find which is the
//              origin of the field in join table
//
// 01.10.2000 - Added support for BookmarkValid Function
//              Note that after calling BookmarkValid current record is cahnged
//              to those pointed by passed TBookmark to BookmarkValid
//
// 01.10.2000 - Added New Function PercentPosition to get info from DAO method
//              PercentPosition. See DAO help for details
//
// 01.10.2000 - Added New Function GetRows(NumRows:Integer):OleVariant
//              This Function returns Two dimaensional variant array
//              with NumRows number of records and all fields.
//              This is a interface to DAO Method GetRows - see DAO help
//              Function positions current record at the next unread record.
//
// 02.10.2000  - Added Support for Parametrized queryes (stored in SQL property)
//               Unfortenatelly this does not work with Delphi 3.0
//               Also it is not tested with Delphi 4.0
//               If you encounter problems during compilation please UNDEFINE
//               USEPARAMS at the begining of this file.
//               Thanks to Andrew Baylis for all this.
//               Any help how to implement this on Delphi 3.0 will be
//               greatly appreciated.
//
// 02.10.2000  - Dramatically Increased speed of the following methods
//                Find_First
//                Find_Last
//                Find_Next
//                Find_Prior
//
// 02.10.2000  - Added changes for Bookmark (previously TSafeArray, now
//               OleVariant (it is Interesting that in fact bookmarks are
//               OleStrings;
//
// 02.10.2000  - Speed of Bookmark operations is Dramatically Increased
//
// 10.10.2000  - Fixed a bug in ExecSQL,ExecutSQL,ExecuteQueryDefSQL
//               Thanls to D. Gene Bland for reporting the problem
//
// 11.10.2000  - Fixed a bug in BuildXXXSQL routines
//               They now support ftAutoInc Field
//               Thanls to Paul Weaver for reporting the problem
//
// 13.10.2000  - Fixed another bug in BuildXXXSQL routines
//               Thanls to Manfred Zieglmeier for reporting the problem
//
// 13.10.2000  - Fixed a bug with OnPostError Event
//               Now OnPostError is supported
//               Thanls to Henry Martin for reporting the problem
//
// 17.10.2000  - Added eight new routines for some compatibility with TTable
//               See explanation in KADao Help docs.
//
//               Procedure FindNearest
//               Function  FindKey
//               Property  IndexFieldCount
//               Property  IndexFields
//               Procedure SetFindData
//               Procedure SetKey
//               Function  GotoKey
//               Procedure LockTable
//               Procedure UnlockTable
//               Property  IndexFieldNames
//
//******************************************************************************
//
// 25.10.2000  - Found a bug in Rollback method-table rasies 'No current record'
//               after rollback - now fixed thanks to Sergey
//
// 26.10.2000  - Twice increased the speed of reading and writing records
//               Before reconstruction KADao adds 1000 records for about 7 sec
//               Now for 3.3 seconds
//
// 30.10.2000   - Found a bug in default values processing - now fixed thanks to
//                Eric BACHMANN
//
// 31.10.2000   - Removed FieldChanged TList - now information about changed
//                fields is stored in RecordData TStringList as objects
//
// 01.11.2000   - Dramaticaly is increased speed of Master/Detail relations
//                /EXPERIMENTAL/
//
// 02.11.2000   - Added Function PromptQueryDefParameters - it brings
//                same dialog as QueryDefParameters editor in design time.
//                Thanks to Jorge Dantas
//
//******************************************************************************
//
// 06.11.2000 - Removed BlobOffset and BlobNumBytes variables for safety reasons
//              Removed method AppendToBlob for safety reasons
//
// 06.11.2000 - Found a VERY BIG bug in KADaoBlob handling
//              Bug affects only BINARY BLOBS and NOT Memos
//              It is reccomended before upgrade to this version of KADao
//              to save all your binary blobs (created with KADAO) to files
//              The proble is that Delphi coverts String to WideChar before
//              sending data to DAO so in MDB files blobs have size twice
//              bigger then normal. Thanks to Albert Molina for reporting.
//
// 12.11.2000 - Fixed a bug in DataEvent Procedure
//              Now fixed - thanks to Gianluca D'Angelo
//
// 12.11.2000 - Fixed a bug in InternalGotoBookmark and SetBookmarkString
//              Bug is present when trying to delete multiple records
//
// 14.11.2000 - Added some code to speedup opening readonly tables and queryes
//              Thanks to Simone.
//
// 14.11.2000 - Added handling of Required in InternalInitFieldDefs
//
// 14.11.2000 - Added AGAIN SetRecNo internal dataset method
//              Now works as expected - you can use KADaoTable1.RecNo:=10 and
//              cursor will position at RecordNO 10 (counting is NOT ZERO based)
//
// 15.11.2000 - Preprocessor defintion USESLOWRECORDCOUNT is removed
//              Now KADao ALWAYS handle possible RecordCount bugs in DAO
//
//******************************************************************************
//
// 22.11.2000 - Removed a Bug wich does not allow using Databases in other Forms
//              or DataModules - Thanks to Josimar Serhid.
//
// 22.11.2000 - Added some code to speedup opening readonly tables and queryes
//              in InternalInitFieldDefs. Thanks to Simone.
//s
// 27.11.2000 - Added some code to enhance ftBoolean fields
//
// 27.11.2000 - Added new property WarnOnBadDatabase - True by default
//              When KADaoTable finds a corrupted database (bad RecordCount)
//              and WarnOnBadDatabase is True then an exeption is raised to
//              inform that database needs COMPACT and REPAIR
//******************************************************************************
//
// 04.12.2000 - Restored positioning method in Find_XXX methods
//              This is the slow method but is not based on Bookmark calculation
//
// 05.12.2000 - Removed rediculous bug in BooleanToBuffer -
//              thanks to Sergey Polevikov
//
// 05.12.2000 - Fixed a bug in Master/Detail fast opening recordset system
//              Now works fine. Thanks to Ingmar Bode for reporting the problem
//
// 05.12.2000 - Fixed a bug in Locate/Find_XXX/Seek_XXX routines which occurs on
//              special conditions. Also removed handling of DataEvent internal.
//              Thanks to Sergey Polevikov for reporting the problem
//
// 05.12.2000  - All Error messages are moved to resourcestring so you can
//               localize your KADAO.
//               Errors between 1000 and 1999 are rezerved for KADaoDatabase
//               Errors between 2000 and 2999 are rezerved for KADaoTable
//                                                                                                
// 07.12.2000  - Master/Detail Routines are COMPLETELY rewritten
//               Also if Detail is a parametrized Query all query parameters
//               that have Names equal with Detail fields will get data from
//               Master. Thanks to Dusko Vuksanovic - he was right!  
//
// 07.12.2000  - InternalGotoBookmark and BookmarkValid are changed reflecting
//               new information about this internal dataset routines.
//               Why Borland does not publisg tech info about this!?
//
// 07.12.2000  - Change made to CompareBookmarks method
//               Some custom DataGrids like InfoPower TwwDBGrid sends
//               PIntegers instead of BookmarkStrings
//
// 08.12.2000  - Fixed a bug in GetRecordCount - it retunts 1 instead of 0
//               whel last record is deleted - thanks to Mark Hamilton.
//
// 08.12.2000  - Added new property MasterAutoActivate - True by default
//               When this property is True if a Detail dataset is set to active
//               and the corresponding Master dataset is not active then
//               Detail dataset activates the Master. 
//******************************************************************************
//
// 11.12.2000  - Added minor change to BufferToDate routine
//
// 17.12.2000  - Requery now supports Master/Detail Relations
//
// 18.12.2000  - Added SaveToStream, SaveToFile,
//               LoadFromStream and LoadFromFile methods.
//               The Stream and File formats are compatible with kbmMemTable
//               created by Kim Bo Madsen - Scandinavia - kbm@optical.dk,
//               which is the best MemoryTable i have seen.
//               Only Data fields are stored. Blobs are stored too.
//               Use LoadFromBinaryFile and LoadFromBinaryStream methodts
//               of kbmMemTable to Load Datasets saved from KADaoTable.
//               Using this two methods you can move your data to other
//               Database platforms away from your office.
//
// 18.12.2000  - Added support for TField.DisplayText wich is equivalent
//               to Caption Property in Access
//
// 20.12.2000  - Added support for TField.OldValue.
//               TField.CurValue and TField.NewValue always return the
//               NEW value of the field.
//
// 20.12.2000  - Added FULL SUPPORT  for the following Methods:
//                 - SetKey
//                 - EditKey
//                 - CancelKey
//                 - GotoKey
//                 - GotoNearest
//               They work now as TTable methods.
//               The old SetKey Method is renamed to SetKeyParam.
//               See explanation of the methods in  the help file.
//
// 22.12.2000 - Fixed a bug in Seek_NearestEx - many thanks to Mark Hamilton
//
// 26.12.2000 - Added support for TField.OnValidate Event
//

// 26.12.2000 - Added FULL SUPPORT  for the following Methods:
//                - SetRange
//                - SetRangeStart
//                - SetRangeEnd
//                - EditRangeStart
//                - EditRangeEnd
//                - ApplyRange
//                - CancelRange
//               They work exactly as TTable methods.
//               See explanation of the methods in  the help file.
//
// 26.12.2000 - Added new propery UseGetRecNo - True by Default
//              Set to False on BIG Datasets wit Applyed Ranges
//              or Filtered Datasets based on OnFilterRecord event
//              This will speedup Table IO at 300%
//
// 26.12.2000 - Fixed a bug in Filtering (when Filtered is false but
//              OnFilterRecord is Assigned the Filtering is done which is not OK
//              Now works as expected
//
// 02.01.2001 - Added SUPERSPEED record positioning for recordsets that support
//              Bookmarks
//
// 02.01.2001 - Fixed a bug in InternalSetDisplayLabels - conflict with Table
//              Editor;
//
// 03.01.2001 - Added new property ProcessMessages - True by default
//              It is used to control processing of windows messages wnen
//              Saving And Loading data to/from File/Stream
//
// 03.01.2001 - Implemented COM cashing which speeds DRAMATICALY KADao I/O
//              Now KADao Adds 1000 records to empty table for 2 Seconds!
//
// 03.01.2001 - Changed the way on which Rollback works
//              Now after Rollback Table's Current record is the first record.
//
// 03.01.2001 - Added new Event OnExportProgress(Current,Total:Integer);
//              The event is triggered each time a new records is SAVED to
//              File or Stream. Current is zero based position
//              Total is nuber of records in the table -1
//
// 03.01.2001 - Added Support for Default values for String, Memos, Date/Time
//              fields. Note that function based defaults are NOT Supported
//              since they are not DAO based!
//
// 03.01.2001 - Added Handling of situation when user edits a record
//              already deleted by another user.
//
//******************************************************************************
//
// 03.01.2001 - Added support for Default values on Master/Detail relationship
//              Thanks to Jiri Kanda for reporting the problem
//
//
// 07.01.2001 - Removed ULTRAFAST positioning based on Bookmarks
//              it gives ERRORS in too many cases (WHY Microsoft WHY?)
//              Added WORKAROUND CODE to support viewing of BLOB fields
//              in enchanced DBGrids like InfoPower's wwDBGrid
//              This code is workaround becouse viewing of blobs
//              moves DAO cursor on records other than editing record and
//              this cancells editing internally. This results
//              "Update or CancelUpdate without AddNew or Edit" ERROR
//              to be raised when Post/Cancel is called
//              Thanks to Andrew Baylis and Jiri Kanda for reporting the problem
//
//
// 07.01.2001 - Added changes for speedup InternalSetDisplayLabels
//              This is the most then can be do for this routine
//              Sorry but DAO is really too slow on Queryes when
//              retrieving such properties
//
//******************************************************************************
//
// 08.01.2001 - Added propertiy UseCaptions - False by Default
//              Quering some field properties is extremely slow with MS Dao
//              This property controls DisplayLabels of Fileds which is equal
//              to MS Access Caption property
//              When set to True DisplayLabels are retrieved from the
//              Caption property orhervise DisplayLabels are set to Field names
//
// 08.01.2001 - Added property UseDaoProperties -True by Default
//              Quering some field properties is extremely slow with MS Dao
//              This property controls some Fileds properties
//              which can make easy adding new records
//              When set to False, Default Values are not shown when adding new records
//              Also Required property is not set on the fields that are required
//              Also you can modify fields that cannot be modified
//              (this will raise exception on Post)
//              Setting this property to False will increase speed
//              of opening Queries about 10000% but You must do coding carefully
//
// 10.01.2000 - Found A bug in SetBookmarkStr - it appears when deleting couple
//              of records trough multiselection in DBGrid
//              Thanls to Alfredo Milani-Comparetti for bugreport
//
// 12.01.2000 - All KADao Routines for positioning are REWRITED due to
//              special considerations with Indexes.
//
// 13.01.2000 - Fixed a small bug in InternalSetDisplayLabels.
//              Thanks to Jiri Kanda for bugfix.
//
// 14.01.2000 - Added Enchancemet which FANTASTICALLY SPEEDSUP adding records to
//              table. Now Append and Insert work at 500% faster.
//              The only need is to set the NEW property BatchMode to True
//              before adding recodrs and to False after that.
//
// 14.01.2000 - Now Default values are suported in Filtered and Sorted Tables
//
// 15.01.2000 - EmptyTable now is 500% faster.
//
// 15.01.2000 - GotoKey now Support StandardTable too
//
// 16.01.2000 - Fixed a bug in Bookmark Handling - with many thanks to
//              Mark Hamilton.
//
// 16.01.2000 - A little much more code added for handling
//              default fields in blobs
//
// 22.01.2000 - Added minor changes in LoadFromSream for compatibility with
//              KBMMemTable - new Event OnImportProgress(Current:Integer);
//              The event is triggered each time a new records is Loaded from
//              File or Stream - by Mark Hamilton.
//
// 23.01.2000 - Fixed a bug assosiated with Lookup and Calc fields
//              Now everything works properly
//
// 23.01.2000 - WarnOnBadDatabase is now False by default
//
// 23.01.2000 - Added new property CacheMemos - True by default
//              Set to False if you dont need displaying memos in dbGrids
//
// 24.01.2000 - KADAO Search Engine modifyed
//              Now Locate, Find and SeekNearestEx methods are much more faster
//
// 24.01.2000 - Removed IndexChacking in Locate! Now programmers are reposible
//              for setting correct Index when callinc Locate on StandardTables
//              CheckFieldsInIndex is NOT called which speeds up operations.
//
// 24.01.2000 - Added a special workaraound for MS Acces Formulas
//              IN DATE FIELDS ONLY - With Many Thanks to Richard Blanchard
//******************************************************************************
//
// 28.01.2001 - Fixed a small bug in OnPostError event handling - thanks to
//              Jiri Kanda
//
// 30.01.2001 - Added new property CacheBlobs - False by default
//              Set to False if you dont need displaying blobs in dbGrids
//
// 30.01.2001 - Default value for F_CacheMemos is now FALSE!
//
// 30.01.2001 - Changed Blob Stream Handling routine to support
//              BlobViewing dbGrids! Someday i will write why this violates
//              everything created by Borland to speedup tables!
//
// 31.01.2001 - Fixed a small bug in SetBookmarkData
//              Borland passes BookmarkString instead of Bookmark;
//
// 31.01.2001 - Added some code in GetRecNo to support new positioning engine
//
// 01.02.2001 - Bookmarks Revisited. Now all bookmark functions use
//              Integer/PInteger values. This also fixes some bugs which
//              appear on custom dbGrids.
//
// 05.02.2001 - ProcessMessages is now set to FALSE by default.
//              The reason is when using multithread functions based on atoms
//              it will add some asynchronous troubles.
//              Of course you can use them without any throuble ia all other
//              projects.
//
// 05.02.2001 - Fixed a small bug in GetRows function. Thanks to Milan Cyprich!
//
// 08.02.2001 - Added minor changes to PercentPosition and RollbackRefresh
//              Now empty tables are also supported.
//
// 08.02.2001 - Fixed a bug in KADao Search Engine
//              Bug is based on bad approximating calculation
//              Now all is OK. Thanks to Jacques Verleijen
//
// 19.02.2001 - Fixed a very interesting bug in KADaoTable.
//              When state is dsEdit and a grid attached to table is resized
//              a haos records are displayed. Now fixed.
//              Thanks to Jiri Kanda for bug report.
//
// 19.02.2001 - Fixed a bug in processing parametrized queryes
//              Thanks to Shmuel Rosen for bug report.
//
// 19.02.2001 - Added a new routine FindKeyExact according to
//              sujjestions of Joseph Glosz.
//              FindKey now uses Seek('=') and FindKeyEx uses Seek('>=')
//
// 23.02.2001 - Fixed a bug in sorting - a new method called Sort is created
//              see help for details. Thanks to Johannes Hardmeier
//
// 23.02.2001 - Fixed a bug in Master/Detail relations. Bug appears in very
//              specilal conditions. Thanks to Paul Weaver.
//
//******************************************************************************
//
// 28.02.2001 - Fixed a bug in the IndexName property. When table is not active
//              IndexFieldCount contans invalid value. Now OK.
//
// 01.03.2001 - Added support for DefaultExpression field property
//              Note that if you set DefaultExpression it has big priority
//              then the DefaultValue property of the MS Access Field.
//              Thanks to Marcelo Ceschin for reporting the problem.
//
// 01.03.2001 - Fixed a bug which generates live poiners after execution
//
// 09.03.2001 - Speed of detail table which is NOT based on SQL queery is
//              increased dramaticlly.
//
// 20.03.2001 - Now setting IndexFieldNames to empty string clears current
//              Index i.e IndexName is also empty string
//
// 20.03.2001 - Fixed a bug with reading blob data when CacheBlobs is true.
//              Now works properly.
//
// 22.03.2001 - Fixed a bug in GetQueryDefReturnParams routine
//              Bug appears when return parameter is only one.
//              Now works properly.
//              Also return format is not Name=Value but just Value
//              This change is needed becouse result data will be in
//              native format. In previous code return data is always in
//              String format which may cause problems when getting Date and
//              Time data.
//
// 25.03.2001 - Added support for ReadOnly fields;
//
// 27.03.2001 - Added two new Functions which vcan retrive QueryDef from
//              which recordset is open.
//              - Function  CopyQueryDef : OleVariant;
//                returns QueryDef Object as OleVariant;
//              - Function  CopyQueryDefText : String;
//                returns QueryDef SQL text as String;
//
// 27.03.2001 - Added support for OnDeleteError - thanks to Flemming Brandt
//              Clausen for reporting the problem;
//
// 27.03.2001 - Added support for OnEditError
//
// 29.03.2001 - Fixed a bug in GetRecNo with Non-Bookmarkable tables
//              No more comments...
//
// 03.04.2001 - Added tree new methods
//                - AccessExportToTXT(FileName:String; IncludeBlobs, DeleteOld:Boolean);
//                - AccessExportToHTML(FileName:String; IncludeBlobs, DeleteOld:Boolean);
//                - AccessExportToExcel(FileName, SheetName :String; ExcelVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
//                - AccessExportToParadox(FileName :String; ParadoxVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
//                - AccessExportToDbase(FileName :String; DBaseVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
//                See help for more details
//
// 09.04.2001 - Now exceptions generated with default field value functions
//              are supressed. Thanks to Mark Elissen.
//
// 18.04.2001 - Added support for external Encrypting of Strings and Blobs
//              See help for more details
//
// 18.04.2001 - Added support for more flexibility using FieldsEditor
//
// 04.05.2001 - Fixed a bug in Internalrefresh method. Thanks to Andy Chan for
//              reporting the problem.
//
// 16.05.2001 - Fixed a bug in SetFieldData method. Thanks to
//              Serg Gribanov for reporting the problem.
//
// 17.05.2001 - Added support for IndexDefs like TTable
//
// 21.05.2001 - Fixed a bug with Parameters that contain part of the name of
//              another parameter - thanks to Stephane Poudret for reporting.
//
//******************************************************************************
// 29.05.2001 - Added new method CreateTable for compatibility with TTable.
//              See help for details.
//
// 29.05.2001 - Added new method AppendTable adding Fields and indexes to
//              existing table. See help for details.
//
// 04.06.2001 - Added support for BlockReadSize/dsBlockRead
//              by request from Jorg Schaefer.
//
// 04.06.2001 - Added enchancement for work with ForwardOnly Tables
//
// 04.06.2001 - Fixed a bug with Detail dataset which uses QueryDef as source.
//              (Close twice)
//
// 04.06.2001 - Fixed a bug with Detail dataset which uses SQL with Parameters
//              as source (wrong field count).
//
// 05.06.2001 - Fixed a bug with Mater/Detail relations (Bad Deactivation)
//              Thanks to Jorg Schaefer for reporting.
//
// 12.06.2001 - Fixed a bug in Notification routine of TKADaoTable
//              Thanks to Ingmar Bode for reporting the problem.
//
// 17.06.2001 - Changed the way on which BookmarkValid works
//              Now after calling BookmarkValid recordset stays at
//              previous position - i.e as Borland reccomends.
//              Also if table is ForwardOnly - i.e. bookmarks are not supported
//              returned BookmarkSize is 0
//
// 04.06.2001 - Added anodher enchancement for work with ForwardOnly Tables
//
// 25.06.2001 - Found another bug in Master/Detail Relations - Thanks to
//              Martin Rohleder for reporting the problem.
//
// 25.07.2001 - Added enchanced support for Lookup and Lookup/Calc Fields
//
//******************************************************************************
//
// 29.08.2001 - Fixed a bug in GotoNearest and GotoKey with a Date/Time values
//              Thanks to Martin Hart for reporting the problem.
//
//******************************************************************************
//
// 08.09.2001 - Found a bug in Bookmark manegement system
//              Bug appears only on Non-Microsoft databases as Paradox
//
// 17.09.2001 - Fixed a small bug in FieldDef system
//              Now when change TableName the correct fields are displayed
//              in the editor.
//
// 17.09.2001 - Fixed a small bug in DefaultExpression field property
//              Thanks to Marcelo Ceschin for reporting the problem.
//
// 20.9.2001 -  Fixed a bug in SetFieldData routine
//              Thanks to Herman Klijnsma for reporting the problem.
//
// 22.09.2001 - Fixed a bug in internal BlobToString routine
//              Bug appears on empty blob fields
//              Thanks to Len Richter for reporting the problem.
//
// 25.09.2001 - Added a minor change to GetCurrentRecord routine
//
// 04.10.2001 - Removed a locale dependance of F_ComposeSQL routine
//              Now Date, Time and DateTime parameters will be interpreted
//              correctly. Tanks to Walter AJ van Rensburg
//              for reporting the problem.
//
// 09.10.2001 - Added a specilal code in Locate and Lookup for handling
//              batch append/insert of records. This significantly speeds
//              processing of tables with lookup fields
//              Thanks to Vlado Neychev for reporting the problem.
//
// 13.10.2001 - Added a new property CacheLookups - False by default
//              When set to True all Lookup fields data will be cached
//              i.e. each Field.LookupCache property will be set to True.
//
// 13.10.2001 - Added new method RefreshLookups
//              When called all Lookup fields with property LookupCache = True
//              will be updateted.
//
// 27.10.2001 - Found a bug in IndexName property. When IndexName is set to an
//              empty string the Index is not Removed - now fixed;
//
// 27.10.2001 - Added new property ExportMethod which can be one of
//              the following: VisibleFields, AllFields
//              If ExportMethod is VisibleFields then only fields selected
//              in the table editor and visibe will be exported
//              Otherwise all fields from the table will be exported
//              Default value is: VisibleFields
//
//******************************************************************************
//
// 28.11.2001 - Another bug with bookmarks was found and removed!
//
// 02.01.2002 - Fixed two bugs - thansk to brian_asap for reporting them!
//              1. UseGetRecNo now can be turned to false for the following
//                 seek based functions:
//                   Locate
//                   Find
//                   Seek_NearestEx
//              2. LockEdits property now work as expected
//
// 02.01.2002 - A much more enchanced method of getting RecNo is developed
//              You will see the improvement.
//
// 02.01.2002 - Added new property RefreshSorted - false by default
//              If RefreshSorted is set to true then each time when new record
//              is posted to a sorted table entire table is refreshed.
//              This can slowdown operations on big tables.
//
// 17.01.2002 - Added support for AM and PM in date default values
//              Thanks to Niall R Scott for code submission
//
// 26.01.2002 - Added support for AutoIcrement Fields with Random Values
//
// 26.01.2002 - An addition to BookmarValid rountine
//              Thanks to Fabian Becker for code submission
//
// 29.01.2002 - LockEdit property is now ReadOnly
//              There is a conflict between two properties
//              LockEdits an LockType
//              If LockType is set to Pessimistic and  LockEdits is set to False
//                   then Locking is Optimistic
//              If LockType is set to Optimistic and  LockEdits is set to True
//                   then Locking is Pessimistic
//              A new method - SetLockEdits is added.
//              which can be called in runtime only for Lock switching
//              WITH MANY THANKS TO Brian O'Hara FOR THE TITANIC CODE
//              HE WROTE TO TEST THE LOCKING!!!!!!!!!!!!!
//
// 31.01.2002   Added two new  utility routines
//              Function IsFieldUniqueIndex(Table : TKaDaoTable; FieldName : String ) : Boolean;
//              Function GetUniqueIndexFields(Table : TKaDaoTable) : String;
//              Thanks to Jörg Schäfer for providing the code
//
// 31.01.2002   Fixed a BUG in all BUILDXXX routines
//              Bug appears when a passed value is an empty string
//              Thanks to Johan Korten for reporting the problem.
//
// 01.02.2002   Fixed a bug on IsNull - it does not work correctly on BLOB/MEMO
//              fields - now is OK.
//
// 04.02.2002   Many changes on Edit/Post for better support of
//              multyuser Environment
//
// 12.02.2002   Fixed a bug in Locate method - Thanks to Brian O'Hara
//              for reporting the problem.
//              Also a new enchanced FindGoodIndex is added so
//              the optimal index is used when using Locate
//              To leave KADAO to search for the optimal index set
//              IndexName property to an empty string.
//
// 22.02.2002   Fixed another bug in Locate method - it raises an error when
//              Locate is used with a NON StandardTable
//
// 22.02.2002   Filtered and Filter properties now use standart dataset methods
//              SetFiltered and SetFilterText
//
// 25.02.2002   Added new export method
//              AccessExportToFoxPro(FileName:String; FoxProVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
//              WARNING - works only with Dao 3.5 and NOT with Dao 3.6
//
// 27.02.2002   Added support for GUID fields - thanks to Slavik for
//              reporting the problem
//
// 01.03.2002   Fixed a Memory leak in Find method
//              Thanks to Flemming Brandt Clausen for finding the bug.
//
// 04.03.2002   Fixed a Memory leak in BookmarkValid method
//              Thanks to Flemming Brandt Clausen for finding the bug.
//
// 08.03.2002   Fixed a bug in F_ComposeSQL routine
//              Bug appears in string parameters containing double quotes
//              or currency and float parameters containing commas
//              Thanks to Formentz for reporting the bug
//
// 12.03.2002   RollbackRefresh routine is modifyed for better
//              Transaction support
//******************************************************************************
//
// 02.04.2002   Found a bug in parametrized queries.
//              Bug appears when DecimalSeparator is different then point
//              Thanks to Sergey Zaytseff for reporting the problem.
//
//
// 04.04.2002   Added new property QueryDefSQLModify - Boolean (Default False)
//              When set to true any modifications maded to QueryDefSQLText
//              are written back to the database.
//
//
//******************************************************************************
//
// 15.08.2002  Changed the way in which KADao Handles Queries
//             Before KADao escapes ALL reserved characters
//             However DAO treats queries in different way
//             =     - No escaping is needed
//             LIKE  - Escaping is required
//             Now KAdao escapes only the double quotes
//             If you want to use reserved characters in you queries use
//             EscapeReservedChars function
//             Example:
//             KADaoTable1.Params[0].AsString := EscapeReservedChars('***NEW***');
//
// 15.08.2002  Found a memory leak in KADao compiled on Delphi and CB ver. 3-5
//             Delphi6 and CBuilder6 are not affected.
//
//******************************************************************************
// 19.02.2003  Fixed a Bug in SetFieldData when state is SetKey
//             Thanks to Norbert Meier
//
//******************************************************************************

interface
uses
DAOApi,
{$IFDEF DAO35}
DAO35Api,
{$ENDIF}
{$IFDEF DAO36}
DAO36Api,
{$ENDIF}
{$IFDEF DAO120}
DAO120Api,
{$ENDIF}
Windows, SysUtils, Classes, Db, DBCommon, KDaoDataBase, ActiveX, Forms
{$IFDEF D6UP}, Variants, WideStrUtils{$ENDIF};

//******************************************************* DatabaseError Messages
{$I ErrLangTB.pas}
//******************************************************************************


const
        MYBOOKMARKSIZE   = 4;
        GUID_ID          = 47554944;
        GUID_VALID_CHARS = ['{','}','-','0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','a','b','c','d','e','f'];


Type
TKADaoTable = class;

TBlobData = String;

TDaoInfo=record
        RecordNo        : Integer;
        RecordData      : TStringList;
        BookmarkFlag    : TBookmarkFlag;
        BookmarkData    : Integer;
End;
PDaoInfo=^TDaoInfo;

TLockType = (ltReadLock, ltWriteLock);
TKeyType  = (KeyValue,RangeStart,RangeEnd);

TLoadMode = (lmAppend, lmEmptyAppend);

TOO    = (
          dbDenyWrite,
          dbDenyRead,
          dbReadOnly,
          dbAppendOnly,
          dbInconsistent,
          dbConsistent,
          dbSQLPassThrough,
          dbFailOnError,
          dbForwardOnly,
          dbSeeChanges,
          dbRunAsync,
          dbExecDirect
          );
TOOSet = Set of TOO;

TExportMethod        = (VisibleFields,AllFields);
TExportProgressEvent = procedure(Current,Total:Integer) of object;
TImportProgressEvent = procedure(Current:Integer) of object;

{$IFDEF D4UP}
TKADaoIndexDefs = Class(TIndexDefs)
  Private
    FDataset : TKADaoTable;
    Procedure UpdateIndexes;
    Procedure CreateIndex(IndexDef : TIndexDef);
    Function  DeleteIndex(const Name : string):Boolean;
  Public
    Constructor Create(DataSet: TDataSet);
End;
{$ELSE}
TKADaoIndexDefs = Class(TIndexDefs)
  Private
    F_Dataset : TKADaoTable;
  Public
    Constructor Create(DataSet: TDataSet);
    Procedure Add(const Name, Fields: string;  Options: TIndexOptions);
    Function DeleteIndex(const Name : string):Boolean;
End;
{$ENDIF}


TKADaoTable = class(TDataSet)
private
        F_RecNo           : Integer;
        F_RecPos          : Integer;
        F_LastRecord      : Integer;
        F_RefreshRC       : Boolean;
        F_OldRC           : Integer;
        F_PostMade        : Boolean;
        F_InPost          : Boolean;
        F_BatchMode       : Boolean;


        F_OldValue        : TRecordBuffer;
        F_ActiveKeyBuffer : TRecordBuffer;
        F_KeyBuffer       : TRecordBuffer;
        F_RangeStartBuffer: TRecordBuffer;
        F_RangeEndBuffer  : TRecordBuffer;
        F_FilterBuffer    : TRecordBuffer;

        F_BookmarkRN      : TList;
        F_BookmarkID      : TList;
        F_Bookmarkable    : Boolean;


        F_BufferSize      : Integer;
        F_StartMyInfo     : Integer;
        F_StartCalc       : Integer;
        F_MDisabled       : Boolean;
        F_KeyFields       : TStringList;
        F_UpdatableFields : TList;

        {$IFDEF USEPARAMS}
          {$IFNDEF VER100}
            {$IFNDEF VER110}
        F_ParamCheck      : Boolean;
        F_Params          : TParams;
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}

        Procedure       F_OnGetMemoText(Sender: TField; var Text: String; DisplayText: Boolean);
        Procedure       F_OnGetGUIDText(Sender: TField; var Text: String; DisplayText: Boolean);
        Procedure       F_OnSetGUIDText(Sender: TField; const Text: string);
        Function        GetActiveRecordBuffer:  TRecordBuffer;
        Function        FilterRecord(Buffer: TRecordBuffer): Boolean;
protected
        F_Database               : TKADaoDatabase;
        F_Active                 : Boolean;
        F_ReadOnly               : Boolean;
        F_ProcessMessages        : Boolean;

        {$IFDEF DYNADAO} //****************************************************
        F_DaoTable               : OleVariant;
        F_DetailRecordset        : OleVariant;
        {$ELSE}
        F_DaoTable               : Recordset;
        F_DetailRecordset        : Recordset;
        {$ENDIF}


        F_SQL                    : TStrings;
        F_SortedBy               : TStrings;
        F_RefreshSorted          : Boolean;
        F_FieldNames             : TStrings;
        F_SortFieldNames         : TStrings;
        F_FieldTypeNames         : TStrings;
        F_DefaultValues          : TStrings;
        F_DisplayLabels          : TStrings;

        F_QD_ParamNames          : TStringList;
        F_QD_ParamDaoTypes       : TStringList;
        F_QD_ParamBDETypes       : TStringList;
        F_QueryDefMaxRecords     : Integer;
        F_QueryDefType           : String;
        F_QueryDefCanModify      : Boolean;

        F_MasterLink             : TMasterDataLink;
        F_MasterFields           : TStrings;
        F_UseBrackets            : Boolean;
        F_MasterAutoActivate     : Boolean;
        F_DatabaseAutoActivate   : Boolean;
        F_UseRecordCountCache    : Boolean;
        F_UseGetRecNo            : Boolean;
        F_UseDisplayLabels       : Boolean;
        F_UseDaoProperties       : Boolean;
        F_AutoFindIndex          : Boolean;

        F_IndexDefs              : TKADaoIndexDefs;

        F_RangeFiltered          : Boolean;
        F_Filtered               : Boolean;
        F_Filter                 : String;
        F_OnFilterRecord         : TFilterRecordEvent;
        F_OnExportProgress       : TExportProgressEvent;
        F_OnImportProgress       : TImportProgressEvent;

        F_TableName              : String;
        F_QueryDefName           : String;
        F_QueryDefParameters     : TStrings;
        F_QueryDefSQLText        : TStrings;
        F_IndexName              : String;
        F_IndexFieldCount        : Integer;
        F_TableType              : Integer;
        F_LockType               : Integer;
        F_OpenOptions            : TOOSet;
        F_RecordSize             : Integer;

        F_FindKeyFields          : String;
        F_FindKeyValues          : Variant;
        F_FindOptions            : TLocateOptions;

        F_ExportMethod           : TExportMethod;

        F_KeyKeyFields           : String;
        F_KeyKeyValues           : Variant;

        F_DateCreated            : String;
        F_LastUpdated            : String;
        F_OLE_ON                 : Boolean;
        F_ComponentVersion       : String;
        F_WarnOnBadDatabase      : Boolean;
        F_CacheMemos             : Boolean;
        F_CacheBlobs             : Boolean;
        F_CacheLookups           : Boolean;
        F_ShowGUID               : Boolean;

        F_Encrypter              : TComponent;
        F_EncodedString          : Pointer;
        F_DecodedString          : Pointer;
        F_HasEncoder             : Boolean;

        F_Translation            : Boolean;

        Letters                  : String;
        DaoFields                : OleVariant;
        DaoOpenOptions           : Integer;
        DaoSortString            : String;
        InInternalOpen           : Boolean;

        Procedure                Loaded; override;
        Procedure                Notification(AComponent: TComponent; Operation: TOperation);Override;

        Procedure       F_Set_ComponentVersion(Value: String);
        Function        F_Get_Database:TKADaoDatabase;
        Procedure       F_Set_Database(Value:TKADaoDatabase);
        Function        F_Get_TableName:String;
        Procedure       F_Set_TableName(Value:String);
        Function        F_Get_DateCreated:String;
        Function        F_Get_LastUpdated:String;

        Function        F_Get_IndexName:String;
        Procedure       F_Set_IndexName(Value:String);
        Function        F_Get_IndexFieldNames:String;
        Procedure       F_Set_IndexFieldNames(Value:String);
        Function        F_Get_IndexFieldCount:Integer;
        Procedure       F_Set_IndexFieldCount(Value:Integer);

        Procedure       F_Set_TableType(Value:Integer);
        Procedure       F_Set_LockType(Value:Integer);
        Procedure       F_Set_OpenOptions(Value:TOOSet);
        Procedure       F_Set_ReadOnly(Value:Boolean);
        Function        F_Get_LockEdits:Boolean;
        Procedure       F_Set_LockEdits(Value:Boolean);
        Procedure       F_Set_Sort(Value:TStrings);

        Procedure       F_Set_SQL(Value:TStrings);
        Procedure       F_Set_QueryDefName(Value:String);
        Procedure       F_Set_QueryDefParameters(Value:TStrings);
        Procedure       F_Set_QueryDefSQLText(Value:TStrings);
        Function        F_Get_QueryDefType:String;

        Function        F_Get_MasterSource: TDataSource;
        Procedure       F_Set_MasterSource(Value: TDataSource);
        Procedure       F_ProcessMasterFields(Value:TStrings);
        Procedure       F_Set_MasterFields(Value:TStrings);

        Procedure       F_Set_Master(Value:TStrings);
        Procedure       F_Set_Detail(Value:TStrings);
        Function        WWStringReplace(Src,Pattern,Repl:String):String;
        Function        ChangeQuotes(S:String):String;
        Function        ChangeOnlyQuotes(S:String):String;
        Function        ChangeCommas(S:String):String;
        Function        F_ComposeSQL(SQL:TStrings):String;
        Function        F_RecalculateRecNo(TempRS:OleVariant;BK:Integer):Integer;

        //*********************************************************** 22.02.2002
        Function        ComposeFilter:String;
        Procedure       SetFiltered(Value:Boolean);Override;
        Procedure       SetFilterText(Const Value:String);Override;
        //*********************************************************** 22.02.2002

        Procedure       F_Set_OnFilterRecord(Value: TFilterRecordEvent);

        Function        F_Get_IndexField(Index: Integer): TField;
        Procedure       F_Set_IndexField(Index: Integer; Value: TField);

        Procedure       F_SetBatchMode(Value:Boolean);
        Procedure       F_Set_CacheMemos(Value:Boolean);
        Procedure       F_Set_CacheBlobs(Value:Boolean);
        Procedure       F_Set_CacheLookups(Value:Boolean);
        Procedure       F_Set_ShowGUID(Value:Boolean);

        Procedure       F_Set_Encrypter(Value:TComponent);

        //**********************************************************************
        {$IFDEF USEPARAMS}
          {$IFNDEF VER100}
            {$IFNDEF VER110}
        Procedure SetParamsList(Value: TParams);
        Procedure UpdateParamsList(Sender: TObject);
        Procedure WriteParamData(Writer: TWriter);
        Function  GetParamsCount: Word;
        Procedure DefineProperties(Filer: TFiler); override;
        Procedure ReadParamData(Reader: TReader);
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
        //**********************************************************************
        Procedure       MasterDatasetChanged;
        Procedure       UpdateFromMaster;
        Procedure       RefreshQueryParams;
        Procedure       MasterChanged(Sender: TObject);
        Procedure       MasterDisabled(Sender: TObject);
        Procedure       DoOnNewRecord; override;
        //**********************************************************************
        Procedure       ClearKey;
        Procedure       ClearRange(Var Buffer:TRecordBuffer);
        Function        FilterRange(Buffer:TRecordBuffer): Boolean;
        Function        CompareRecordsRange(B1,B2: TRecordBuffer; CT : Integer) : Integer;
        Function        CompareFieldsRange(B1,B2 : String; FieldType: TFieldType):Integer;
        //**********************************************************************
        Function        InternalCalcRecordSize:Integer;
        Function        IntegerToBuffer(Buffer: Pointer; S: String): Boolean;
        Function        FloatToBuffer(Buffer: Pointer; S: String): Boolean;
        Function        BooleanToBuffer(Buffer: Pointer; S: String): Boolean;

        Function        DateToBuffer(Buffer: Pointer; S: String): Boolean;
        Function        TimeToBuffer(Buffer: Pointer; S: String): Boolean;
        Function        DateTimeToBuffer(Buffer: Pointer; S: String): Boolean;

        Function        BufferToDate(Buffer: Pointer): String;
        Function        BufferToTime(Buffer: Pointer): String;
        Function        BufferToDateTime(Buffer: Pointer): String;

        Function        GUIDToBuffer(Buffer: Pointer; S: String): Boolean;
        Function        BufferToGUID(Buffer:Pointer):String;

        Function        StringToBlob(Field:TBlobField; Data:AnsiString):OleVariant;
        Function        BlobToString(Field:TBlobField; Data:OleVariant; DataSize:Integer):AnsiString;

        Function        ProcessDTDefault(S:String):String;
        Procedure       OpenDaoRecordset;
        Procedure       ReOpenDaoRecordset;
        Procedure       GetQueryDefReturnParams(QueryDefName:String);
        Procedure       CloseDaoRecordset;

        Procedure       InternalOpen; override;
        Procedure       InternalClose; override;
        Function        IsCursorOpen: Boolean; override;
        Function        GetCanModify: Boolean; override;
        Function        GetRecordSize: Word;override;
        Function        AllocRecordBuffer: TRecordBuffer; override;
        Procedure       FreeRecordBuffer(var Buffer: TRecordBuffer); override;
        Function        InternalFillRecordData(RS: OleVariant; MainTable : Boolean; Buffer:TRecordBuffer):Boolean;
        Function        GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
        Procedure       InternalInitIndexDefs;
        Procedure       UpdateIndexDefs; override;
        Procedure       InternalInitFieldDefs; override;
        Procedure       InternalSetDisplayLabels;
        Procedure       InternalInitRecord(Buffer: TRecordBuffer); override;
        Procedure       SetFieldData(Field: TField; Buffer: Pointer);override;
        Procedure       ClearCalcFields(Buffer: TRecordBuffer);override;



        //*********************************************** Navigation and Editing
        Procedure       InternalFirst;override;
        Procedure       InternalLast;override;
        Procedure       InternalMoveToBookmark(Bookmark: Pointer);
        Procedure       InternalSetToRecord(Buffer: TRecordBuffer); override;
        Procedure       InternalEdit; override;
        Procedure       InternalCancel; override;
        Procedure       InternalPost; override;
        Procedure       InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
        Procedure       InternalDelete; override;
        Procedure       InternalRefresh; override;
        Procedure       DaoInternalRefresh;
        //***********************************************
        Function        GetDaoBookMark(RS:Variant):Integer;
        Function        GetDaoLastModifiedBookMark(RS:Variant):Integer;

        Procedure       InternalClearBookmarks;
        Procedure       InternalGotoBookmark(Bookmark: Pointer); override;

        Function        GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
        Procedure       SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;

        Function        GetBookmarkStr: TBookmarkStr; override;
        Procedure       SetBookmarkStr(const Value: TBookmarkStr); override;

        Procedure       GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
        Procedure       SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;

        Procedure       InternalHandleException; override;

        Function        GetRecordCount  : Integer; override;
        Function        GetRecNo        : Integer; override;
        Procedure       SetRecNo        (Value: Integer); override;


        //************************************************* TTable Compatibility
        Function        FindRecord(Restart, GoForward: Boolean): Boolean; override;
        //************************************************* TTable Compatibility

        Procedure       StringToList(Items: String; List: TStringList);
        Procedure       VariantToList(Items: Variant; List: TStringList);
        Procedure       AssignVarValue(Var V :Variant;const Value: TVarRec);

        Function        BuildKeySQL(KN,KV:TStringList):String;
        Function        BuildLocateSQL(KN,KV:TStringList;Options: TLocateOptions):String;
        Function        BuildDetailSQL  : String;

        Function        Find(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions;FindType:Integer): Boolean;
        Function        InsertSQLString(MDString: String): String;
        Function        UnquoteString(S:String):String;
        Function        GetTickCountEx:Cardinal;


  public
        //*********************************** Public By Property Editors request
        F_Detail                         : TStrings;
        F_Master                         : TStrings;
        F_MDFieldNames                   : TStrings;
        //**********************************************************************
        MainDatabaseShutdown             : Boolean;
        QueryDefTypeInt                  : Integer;
        QueryDefReturnParams             : OleVariant;
        RecordsAffected                  : Integer;

        {$IFDEF DYNADAO}
        CoreRecordset                    : OleVariant;
        {$ELSE}
        CoreRecordset                    : Recordset;
        {$ENDIF}
        SQLExecutionType                 : Integer;
        Constructor                        Create(AOwner: TComponent); override;
        Destructor                         Destroy; override;

        Property                           BatchMode : Boolean Read F_BatchMode Write F_SetBatchMode;

        Procedure                          Post; override;
        Procedure                          Resync(Mode: TResyncMode);override;
        Procedure                          RefreshData;
        Procedure                          RefreshDataEx;
        Procedure                          RollbackRefresh;

        Function                           Reposition : Boolean;

        Function                           FindGoodIndex(KeyFields:String):String;
        Function                           GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
        Function                           CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

        Procedure                          SetKeyFields(const KeyFields: string);
        Function                           GetFieldIndexName(FiledName:String):String;
        Function                           CheckFieldsInIndex(KF:TStringList):Boolean;
        Function                           Find_First(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):Boolean;
        Function                           Find_Last(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):Boolean;
        Function                           Find_Next(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):Boolean;
        Function                           Find_Prior(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):Boolean;
        Function                           Find_Nearest(const KeyValues: array of const):Boolean;
        Function                           Find_NearestEx(const KeyFields: string; const KeyValues: Variant):Boolean;
        Function                           Seek_Nearest(const KeyValues: array of const):Boolean;
        Function                           Seek_NearestEx(const KeyValues: array of const; SeekType:String):Boolean;

        //*******************************  For TTable Compatibility
        Procedure                          FindNearest(const KeyValues: array of const);
        Property                           IndexFieldNames : String Read F_Get_IndexFieldNames Write F_Set_IndexFieldNames;
        Property                           IndexFields[Index: Integer]: TField read F_Get_IndexField write F_Set_IndexField;
        Procedure                          SetFindData(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions);
        Procedure                          LockTable(LockType: TLockType);
        Procedure                          UnlockTable(LockType: TLockType);
        Procedure                          SetLockEdits(LockEdits : Boolean);
        Function                           GetCurrentRecord(Buffer: TRecordBuffer): Boolean; override;

        {$IFDEF D4UP}
        Function                           Translate(Src, Dest: PAnsiChar; ToOem: Boolean):Integer; override;
        {$ELSE}
        Procedure                          Translate(Src, Dest: PAnsiChar; ToOem: Boolean); override;
        {$ENDIF}


        //*******************************  Key Routines
        Procedure                          SetKey;
        Procedure                          EditKey;
        Procedure                          CancelKey;
        Procedure                          SetKeyParam(const KeyFields: Array of String;const KeyValues: array of const);
        Function                           GotoKey: Boolean;
        Procedure                          GotoNearest;
        Function                           FindKey(const KeyValues: array of const):Boolean;
        Function                           FindKeyEx(const KeyValues: array of const):Boolean;
        //*******************************  Key Routines

        //*******************************  Range Routines
        Procedure                          SetRange(const StartValues, EndValues:array of const);
        Procedure                          SetRangeStart;
        Procedure                          SetRangeEnd;
        Procedure                          EditRangeStart;
        Procedure                          EditRangeEnd;
        Procedure                          ApplyRange;
        Procedure                          CancelRange;
        //*******************************  Range Routines

        //*******************************  For TTable Compatibility
        Function                           Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
        Function                           Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
        Procedure                          RefreshLookups;

        Function                           CreateField(FieldName:String;FieldType:Integer;FiledSize:Integer):Boolean;
        Function                           CreateIndex(FieldName:String;IndexType:Integer):Boolean;
        Function                           DeleteField(FieldName:String):Boolean;
        Function                           DeleteIndex(FieldName:String):Boolean;
        Function                           EmptyTable:Boolean;
        Procedure                          CreateTable;
        Procedure                          AppendTable;

        Function                           CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
        Function                           BookmarkValid(Bookmark: TBookmark): Boolean; override;
        Function                           GetRows(NumRows:Integer):OleVariant;
        Function                           GetRawFieldData(FieldName : String):OleVariant;
        Function                           SetRawFieldData(FieldName : String; Value : OleVariant):Boolean;
        Function                           CopyQueryDef : OleVariant;
        Function                           CopyQueryDefText : String;
        Procedure                          AccessExportToTXT(FileName:String; IncludeBlobs, DeleteOld:Boolean);
        Procedure                          AccessExportToHTML(FileName:String; IncludeBlobs,DeleteOld:Boolean);
        Procedure                          AccessExportToExcel(FileName, SheetName :String; ExcelVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
        Procedure                          AccessExportToParadox(FileName:String; ParadoxVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
        Procedure                          AccessExportToDBase(FileName:String; DBaseVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
        Procedure                          AccessExportToFoxPro(FileName:String; FoxProVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
        Procedure                          AccessExportToMDB(FileName, NewTableName:String; IncludeBlobs, DeleteOld:Boolean);

        Function                           IsFieldUniqueIndex(Table : TKaDaoTable; FieldName : String ) : Boolean;
        Function                           GetUniqueIndexFields(Table : TKaDaoTable) : String;

        Function                           GetGUIDAsString(GUID : String):String;
        Function                           GetStringAsGUID(GUID : String):TGUID;
        Function                           PutGUIDInString(GUID : String):String;

        Function                           EscapeReservedChars(S:String):String;

        Property  Bookmarkable           : Boolean         Read F_Bookmarkable;
        Property  MasterLink             : TMasterDataLink Read F_MasterLink;
        Property  FieldNames             : TStrings        Read F_FieldNames;
        Property  SortFieldNames         : TStrings        Read F_SortFieldNames;
        Property  LinkableFields         : TStrings        Read F_MDFieldNames;

        {$IFNDEF D4UP}
        Property  IndexDefs              : TKADaoIndexDefs Read F_IndexDefs Write F_IndexDefs;
        {$ENDIF}

        {$IFDEF USEPARAMS}
          {$IFNDEF VER100}
            {$IFNDEF VER110}
        Property ParamCount              : Word read GetParamsCount;
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
        Function                           ExecSQL(SQL:TStrings):Integer;
        Function                           ExecSQLString(SQL:String):Integer;
        Function                           ExecuteSQL:Integer;
        Function                           ExecuteQueryDefSQL:Integer;

        Function                           Requery : Boolean;
        Procedure                          GotoCurrent(Table: TKADaoTable);

        Procedure                          GetIndexNames(List: TStrings);
        {$IFDEF D7UP}
        Procedure                          GetFieldNames(List: TStrings); Override;
        {$ELSE}
        Procedure                          GetFieldNames(List: TStrings);
        {$ENDIF}
        Function                           PercentPosition:Single;
        Function                           GetSourceFieldName(FieldName:String):String;
        Function                           GetSourceTableName(FieldName:String):String;
        Function                           GetLastDaoError:TDaoErrRec;
        Function                           PropertyExists(PropObject:OleVariant;PropertyName:String):Boolean;

        Procedure                          GetQueryDefParameters(F_QD_ParamNames,F_QD_ParamDaoTypes, F_QD_ParamBDETypes:TStringList);
        Function                           PromptQueryDefParameters:Boolean;

        Procedure                          Sort;

        //**************************************************** Storage Functions
        Function  StoreField(X:Integer): Boolean;
        Procedure SaveToStream(Stream: TStream);
        Procedure SaveToFile(const FileName: String);

        Procedure LoadFromStream(Stream: TStream; Mode : TLoadMode);
        Procedure LoadFromFile(const FileName: String; Mode : TLoadMode);
        //**********************************************************************
  published
        Property AutoFindIndex           : Boolean Read F_AutoFindIndex Write F_AutoFindIndex;
        Property ComponentVersion        : String  Read F_ComponentVersion Write F_Set_ComponentVersion;
        Property CacheBlobs              : Boolean Read F_CacheBlobs Write F_Set_CacheBlobs;
        Property CacheMemos              : Boolean Read F_CacheMemos Write F_Set_CacheMemos;
        Property CacheLookups            : Boolean Read F_CacheLookups Write F_Set_CacheLookups;
        Property Database                : TKADaoDatabase Read F_Get_Database Write F_Set_Database;
        Property Encrypter               : TComponent Read F_Encrypter Write F_Set_Encrypter;
        Property ExportMethod            : TExportMethod Read F_ExportMethod Write F_ExportMethod;
        Property RefreshSorted           : Boolean Read F_RefreshSorted Write F_RefreshSorted;
        Property TableName               : String Read F_Get_TableName Write F_Set_TableName;
        Property SortedBy                : TStrings Read F_SortedBy Write F_Set_Sort;
        Property SortedByText            : TStrings Read F_SortedBy Write F_Set_Sort;
        Property QueryDefName            : String Read F_QueryDefName Write F_Set_QueryDefName;
        Property QueryDefParameters      : TStrings Read F_QueryDefParameters Write F_Set_QueryDefParameters;
        Property QueryDefParametersText  : TStrings Read F_QueryDefParameters Write F_Set_QueryDefParameters;
        Property QueryDefSQLText         : TStrings Read F_QueryDefSQLText Write F_Set_QueryDefSQLText;
        Property QueryDefSQLModify       : Boolean  Read F_QueryDefCanModify Write F_QueryDefCanModify;
        Property QueryDefODBCMaxRecords  : Integer Read F_QueryDefMaxRecords Write F_QueryDefMaxRecords;
        Property QueryDefType            : String Read F_Get_QueryDefType Write F_QueryDefType;
        Property SQL                     : TStrings Read F_SQL Write F_Set_SQL;
        Property ShowGUID                : Boolean Read F_ShowGUID Write F_Set_ShowGUID;
        {$IFDEF USEPARAMS}
          {$IFNDEF VER100}
           {$IFNDEF VER110}
        Property Params                  : TParams read F_Params Write SetParamsList Stored False;
           {$ENDIF}
         {$ENDIF}
        {$ENDIF}
        Property TableType               : Integer Read F_TableType Write F_Set_TableType;
        Property TableDateCreated        : String Read F_Get_DateCreated Write F_DateCreated;
        Property TableLastUpdated        : String Read F_Get_LastUpdated Write F_LastUpdated;
        Property Translation             : Boolean Read F_Translation Write F_Translation;
        Property LockType                : Integer Read F_LockType Write F_Set_LockType;
        Property OpenOptions             : TOOSet Read  F_OpenOptions Write F_Set_OpenOptions;
        {$IFDEF D4UP}
        Property FieldDefs;
        Property IndexDefs               : TKADaoIndexDefs  Read F_IndexDefs Write F_IndexDefs;
        {$ENDIF}
        Property IndexFieldCount         : Integer Read F_Get_IndexFieldCount Write F_Set_IndexFieldCount;
        Property IndexName               : String Read F_Get_IndexName Write F_Set_IndexName;
        Property ReadOnly                : Boolean Read F_ReadOnly Write F_Set_ReadOnly;
        Property LockEdits               : Boolean Read F_Get_LockEdits Write F_Set_LockEdits;
        Property MasterSource            : TDataSource Read F_Get_MasterSource Write F_Set_MasterSource;
        Property MasterFields            : TStrings Read F_MasterFields Write F_Set_MasterFields;
        Property MasterAutoActivate      : Boolean Read F_MasterAutoActivate Write F_MasterAutoActivate;
        Property DatabaseAutoActivate    : Boolean Read F_DatabaseAutoActivate  Write F_DatabaseAutoActivate;
        Property UseBrackets             : Boolean Read F_UseBrackets Write F_UseBrackets;
        Property UseCaptions             : Boolean Read F_UseDisplayLabels Write F_UseDisplayLabels;
        Property UseDaoProperties        : Boolean Read F_UseDaoProperties Write F_UseDaoProperties;
        Property UseGetRecNo             : Boolean Read F_UseGetRecNo Write F_UseGetRecNo;
        Property UseRecordCount          : Boolean Read F_UseRecordCountCache Write F_UseRecordCountCache;
        Property WarnOnBadDatabase       : Boolean Read F_WarnOnBadDatabase Write F_WarnOnBadDatabase;
        Property Filtered                : Boolean Read F_Filtered Write SetFiltered;
        Property Filter                  : String  Read F_Filter Write SetFilterText;
        Property OnExportProgress        : TExportProgressEvent Read F_OnExportProgress Write F_OnExportProgress;
        Property OnImportProgress        : TImportProgressEvent Read F_OnImportProgress Write F_OnImportProgress;
        Property OnFilterRecord          : TFilterRecordEvent read F_OnFilterRecord write F_Set_OnFilterRecord;
        {$IFDEF USEPARAMS}
          {$IFNDEF VER100}
            {$IFNDEF VER110}
        Property ParamCheck              : Boolean Read F_ParamCheck Write F_ParamCheck;
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
        Property ProcessMessages         : Boolean Read F_ProcessMessages Write F_ProcessMessages;
        Property BeforeOpen;
        Property AfterOpen;
        Property BeforeClose;
        Property AfterClose;
        Property BeforeInsert;
        Property AfterInsert;
        Property BeforeEdit;
        Property AfterEdit;
        Property BeforePost;
        Property AfterPost;
        Property BeforeCancel;
        Property AfterCancel;
        Property BeforeDelete;
        Property AfterDelete;
        Property BeforeScroll;
        Property AfterScroll;
        Property OnCalcFields;
        Property OnDeleteError;
        Property OnEditError;
        Property OnNewRecord;
        Property OnPostError;
        Property AutoCalcFields;
        Property Active;
End;


// Handle Memo fields
  TKBlobStream = class(TStream)
  private
    F_Field      : TBlobField;
    F_DataSet    : TKADaoTable;
    F_Buffer     : TRecordBuffer;
    F_Mode       : TBlobStreamMode;
    F_Opened     : Boolean;
    F_Modified   : Boolean;
    F_Position   : Longint;
    F_BlobData   : AnsiString;
    F_BlobSize   : Integer;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    Function Read(var Buffer; Count: Longint): Longint; override;
    Function Write(const Buffer; Count: Longint): Longint; override;
    Function Seek(Offset: Longint; Origin: Word): Longint; override;
    Procedure Truncate;
  End;



Procedure Register;

implementation
Uses ComObj, DaoUtils, Dialogs, TypInfo, QueryDefDialogUnit;

Const
  CRLF=#13+#10;
  {$IFNDEF D4UP}
  FieldTypeNames: Array[TFieldType] of String = (
    'Unknown', 'String', 'SmallInt', 'Integer', 'Word', 'Boolean', 'Float',
    'Currency', 'BCD', 'Date', 'Time', 'DateTime', 'Bytes', 'VarBytes',
    'AutoInc', 'Blob', 'Memo', 'Graphic', 'FmtMemo', 'ParadoxOle',
    'dBaseOle', 'TypedBinary', 'Cursor');
  {$ENDIF}

{$IFDEF D4UP}
Constructor TKADaoIndexDefs.Create(DataSet: TDataSet);
Begin
  Inherited Create(Dataset);
  FDataSet := Dataset As TKADaoTable;
End;

//******************************************************************************
// Warning!  This Routine temporary closes the KAdaoTable
//******************************************************************************
Procedure TKADaoIndexDefs.CreateIndex(IndexDef : TIndexDef);
Var
  FieldsList     : TStringList;
  DescFieldsList : TStringList;
  NewTable       : OleVariant;
  NewField       : OleVariant;
  NewIndex       : OleVariant;
  X              : Integer;
  Reopen         : Boolean;
Begin
 if FDataSet.F_TableName='' Then Exit;
 if FDataset.F_Database.TableNames.IndexOf(FDataSet.F_TableName)=-1 Then Exit;
 FieldsList     := TStringList.Create;
 DescFieldsList := TStringList.Create;
 Try
   Reopen := False;
   Try
     FDataSet.StringToList(IndexDef.Fields,FieldsList);
     FDataSet.StringToList(IndexDef.DescFields,DescFieldsList);
     For X := 0 To FieldsList.Count-1 do
         Begin
            If FDataset.FieldNames.IndexOf(FieldsList.Strings[X]) = -1 Then Exit;
         End;
    For X := 0 To DescFieldsList.Count-1 do
         Begin
            If FDataset.FieldNames.IndexOf(DescFieldsList.Strings[X]) = -1 Then Exit;
         End;
    if FDataSet.Active Then
       Begin
          Reopen := True;
          FDataSet.Close;
       End;
    FDataSet.F_Database.RefreshDefinitions;
    NewTable  := FDataSet.F_Database.CoreDatabase.TableDefs.Item[FDataSet.F_TableName];
    NewIndex  := NewTable.CreateIndex(IndexDef.Name);
    if (ixPrimary in IndexDef.Options) Then NewIndex.Primary  := True;
    if (ixUnique  in IndexDef.Options) Then NewIndex.Unique  := True;
    For X := 0 To FieldsList.Count-1 do
        Begin
          NewField       := NewTable.CreateField(FieldsList.Strings[X]);
          NewIndex.Fields.Append(NewField);
        End;
    For X := 0 To DescFieldsList.Count-1 do
        Begin
          NewField            := NewTable.CreateField(FieldsList.Strings[X]);
          NewField.Attributes := NewField.Attributes OR dbDescending;
          NewIndex.Fields.Append(NewField);
        End;
    NewTable.Indexes.Append(NewIndex);
    FDataSet.F_Database.RefreshDefinitions;
   Finally
     if Reopen Then FDataSet.Open;
   End;
 Finally
   FieldsList.Free;
   DescFieldsList.Free;
 End;
End;

Procedure TKADaoIndexDefs.UpdateIndexes;
Var
  SL      : TStringList;
  X       : Integer;
Begin
  SL := TStringList.Create;
  Try
    FDataSet.GetIndexNames(SL);
    For X := 0 To Count-1 do
        Begin
          if SL.IndexOf(Items[X].Name) = -1 Then
             Begin
               CreateIndex(Self.Items[X]);
             End;
        End;
    For X := 0 To SL.Count-1 do
        Begin
          if Self.IndexOf(SL.Strings[X])=-1 Then
             Begin
               DeleteIndex(SL.Strings[X]);
             End;
        End;
  Finally
    SL.Free;
    FDataset.InternalInitIndexDefs;
  End;
End;

//******************************************************************************
// Warning!  This Routine temporary closes the KAdaoTable
//******************************************************************************
Function TKADaoIndexDefs.DeleteIndex(const Name : string):Boolean;
Var
  Reopen : Boolean;
Begin
  Result := False;
  if FDataSet.F_TableName='' Then Exit;
  Reopen := False;
  Try
    if FDataSet.Active Then
       Begin
         Reopen := True;
         FDataSet.Close;
       End;
    FDataSet.F_Database.DeleteIndexByName(FDataSet.F_TableName,Name);
    Result := True;
  Finally
    if Reopen Then FDataSet.Open;
  End;
End;
//******************************************************************************
{$ELSE}
//******************************************************************************
Constructor TKADaoIndexDefs.Create(DataSet: TDataSet);
Begin
  Inherited Create(Dataset);
  F_DataSet := Dataset As TKADaoTable;
End;

//******************************************************************************
// Warning!  This Routine temporary closes the KAdaoTable
//******************************************************************************
Procedure TKADaoIndexDefs.Add(const Name, Fields: string;  Options: TIndexOptions);
Var
  FieldsList : TStringList;
  NewTable   : OleVariant;
  NewField   : OleVariant;
  NewIndex   : OleVariant;
  X          : Integer;
  Reopen     : Boolean;
Begin
  if F_DataSet.F_TableName='' Then Exit;
  Inherited Add(Name, Fields, Options);
  if NOT F_DataSet.PropertyExists(OleVariant(F_DataSet.F_Database.CoreDatabase.TableDefs),F_DataSet.F_TableName) Then Exit;
  If F_DataSet.PropertyExists(OleVariant(F_DataSet.F_Database.CoreDatabase.TableDefs.Item[F_DataSet.F_TableName].Indexes),Name) Then
     Begin
      //*************************************************** Index already exists
     End
  Else
     Begin
       //**************************************** Here DAO Index must be created
       FieldsList:=TStringList.Create;
       Reopen := False;
       Try
         F_DataSet.StringToList(Fields,FieldsList);
         For X := 0 To FieldsList.Count-1 do
             Begin
               if NOT F_DataSet.PropertyExists(OleVariant(F_DataSet.F_Database.CoreDatabase.TableDefs.Item[F_DataSet.F_TableName].Fields),FieldsList.Strings[X]) Then
                  Begin
                   FieldsList.Clear;
                   System.Break;
                  End;
             End;
         if FieldsList.Count > 0 Then
            Begin
              if F_DataSet.Active Then
                 Begin
                  Reopen := True;
                  F_DataSet.Close;
                 End;
              F_DataSet.F_Database.RefreshDefinitions;
              NewTable  := F_DataSet.F_Database.CoreDatabase.TableDefs.Item[F_DataSet.F_TableName];
              NewIndex  := NewTable.CreateIndex(Name);
              if (ixPrimary in Options) Then NewIndex.Primary  := True;
              if (ixUnique  in Options) Then NewIndex.Unique  := True;
              For X := 0 To FieldsList.Count-1 do
                  Begin
                    NewField := NewTable.CreateField(FieldsList.Strings[X]);
                    if (ixDescending in Options) Then NewField.Attributes := NewField.Attributes OR dbDescending;
                    NewIndex.Fields.AppEnd(NewField);
                  End;
              NewTable.Indexes.AppEnd(NewIndex);
              F_DataSet.F_Database.RefreshDefinitions;
            End;
       Finally
         if Reopen Then F_DataSet.Open;
         FieldsList.Free;
       End;
     End;
End;

//******************************************************************************
// Warning!  This Routine temporary closes the KAdaoTable
//******************************************************************************
Function TKADaoIndexDefs.DeleteIndex(const Name : string):Boolean;
Var
  Index  : Integer;
  Reopen : Boolean;
Begin
  Result := False;
  if F_DataSet.F_TableName='' Then Exit;
  Index := inherited IndexOf(Name);
  if Index = -1 Then Exit;
  Inherited Items[Index].Free;
  Reopen := False;
  If F_DataSet.PropertyExists(OleVariant(F_DataSet.F_Database.CoreDatabase.TableDefs.Item[F_DataSet.F_TableName].Indexes),Name) Then
     Begin
      //*********************************************** Here we delete the index
      Try
        if F_DataSet.Active Then
            Begin
              Reopen := True;
              F_DataSet.Close;
            End;
        F_DataSet.F_Database.DeleteIndexByName(F_DataSet.F_TableName,Name);
      Except
        if Reopen Then F_DataSet.Open;
        Exit;
      End;
     End;
   if Reopen Then F_DataSet.Open;
   Result := True;
End;
//******************************************************************************
{$ENDIF}

constructor TKADaoTable.Create(AOwner: TComponent);
Var
  OLE_INIT : Integer;
  X        : Integer;
Begin
  inherited Create(AOwner);
  Randomize;
  MainDatabaseShutdown   := False;
  F_ComponentVersion     := '2011.1';
  F_TableName            := '';
  F_TableType            := dbOpenDynaset;
  F_LockType             := dbOptimistic;
  F_Encrypter            := Nil;
  F_HasEncoder           := False;
  F_OpenOptions          := [];
  F_ReadOnly             := False;
  F_ProcessMessages      := False;
  F_RefreshSorted        := False;
  F_Translation          := False;
  F_QueryDefName         := '';
  F_QueryDefSQLText      := TStringList.Create;
  F_QueryDefSQLText.Clear;
  F_QueryDefMaxRecords   := 0;
  F_QueryDefType         := '';
  QueryDefTypeInt        := 0;
  F_QueryDefCanModify    := False;
  RecordsAffected        := 0;
  F_SQL                  := TStringList.Create;
  F_SQL.Clear;
  F_SortedBy             := TStringList.Create;
  F_SortedBy.Clear;
  F_FieldNames           := TStringList.Create;
  F_FieldNames.Clear;
  F_SortFieldNames       := TStringList.Create;
  F_SortFieldNames.Clear;
  F_FieldTypeNames       := TStringList.Create;
  F_FieldTypeNames.Clear;
  F_DefaultValues        := TStringList.Create;
  F_DefaultValues.Clear;
  F_MDFieldNames         := TStringList.Create;
  F_DisplayLabels        := TStringList.Create;
  F_MDFieldNames.Clear;
  F_DateCreated          := '';
  F_LastUpdated          := '';
  //****************************************************************************
  F_QD_ParamNames        := TStringList.Create;
  F_QD_ParamNames.Clear;
  F_QD_ParamDaoTypes     := TStringList.Create;
  F_QD_ParamDaoTypes.Clear;
  F_QD_ParamBDETypes     := TStringList.Create;
  F_QD_ParamBDETypes.Clear;

  F_QueryDefParameters   := TStringList.Create;
  F_QueryDefParameters.Clear;
  SQLExecutionType       :=DaoApi.dbFailOnError;
  //****************************************************************************
  F_FindKeyFields        := '';
  F_FindKeyValues        := Null;
  F_FindOptions          := [];
  F_ExportMethod         := VisibleFields;

  F_KeyKeyFields         := '';
  F_KeyKeyValues         := Null;
  //****************************************************************************
  F_MDisabled                   := False;
  F_MasterFields                := TStringList.Create;
  F_MasterFields.Clear;
  F_MasterLink                  := TMasterDataLink.Create(Self);
  F_MasterLink.OnMasterChange   := MasterChanged;
  F_MasterLink.OnMasterDisable  := MasterDisabled;
  F_Detail               := TStringList.Create;
  F_Detail.Clear;
  F_Master               := TStringList.Create;
  F_Master.Clear;
  //****************************************************************************

  F_KeyFields            := TStringList.Create;
  F_KeyFields.Clear;
  F_UpdatableFields      := TList.Create;
  F_UpdatableFields.Clear;

  F_BookmarkRN           := TList.Create;
  F_BookmarkRN.Clear;
  F_BookmarkID           := TList.Create;
  F_BookmarkID.Clear;
  F_Bookmarkable         := False;
  F_PostMade             := False;
  F_InPost               := False;
  F_BatchMode            := False;

  F_UseBrackets          := True;
  F_MasterAutoActivate   := True;
  F_DatabaseAutoActivate := False;
  F_UseRecordCountCache  := True;
  F_UseGetRecNo          := True;
  F_UseDisplayLabels     := False;
  F_UseDaoProperties     := True;
  F_AutoFindIndex        := True;

  F_Filtered             := False;
  F_RangeFiltered        := False;
  //************************************************************
  F_Database             := Nil;
  F_OldValue             := Nil;
  F_WarnOnBadDatabase    := False;
  F_CacheMemos           := False;
  F_CacheBlobs           := False;
  F_CacheLookups         := False;
  F_ShowGUID             := False; 
  //************************************************************
  {$IFDEF USEPARAMS}
   {$IFNDEF VER100}
    {$IFNDEF VER110}
  TStringList(F_SQL).OnChange := UpdateParamsList;
  F_ParamCheck                := True;
  F_Params                    := TParams.Create(Self);
    {$ENDIF}
   {$ENDIF}
  {$ENDIF}
  //************************************************************
  {$IFDEF DYNADAO}
   F_DetailRecordset   := Unassigned;
  {$ELSE}
   F_DetailRecordset   := NIL;
  {$ENDIF}

  F_OnFilterRecord   := Nil;
  F_OnExportProgress := Nil;
  F_OnImportProgress := Nil;

  F_OLE_ON:=False;
  OLE_INIT:= CoInitialize(NIL);
  if (OLE_INIT = S_OK) or (OLE_INIT = S_FALSE) then F_OLE_ON:= True
  else DatabaseError(E2001);
  //**************************************************************** Com Cashing
  DaoFields:=VarArrayCreate([0,1],VarVariant);
  //****************************************************************************
  F_IndexDefs := TKADaoIndexDefs.Create(Self);
  //****************************************************************************
  DaoOpenOptions := 0;
  DaoSortString  := '';
  Letters        := '_';
  For X := 32 to 255 do
      Begin
        if IsCharAlphaNumeric(CHR(X)) Then Letters:=Letters+CHR(X);
      End;
  InInternalOpen := False;
End;

destructor TKADaoTable.Destroy;
Begin
  if F_Active Then
      Begin
        Close;
        F_Active:=False;
      End;
  //**************************************************************** Com Cashing
  VarArrayRedim(DaoFields,0);
  DaoFields := NULL;
  //****************************************************************************
  F_SQL.Free;
  F_SortedBy.Free;
  F_FieldNames.Free;
  F_SortFieldNames.Free;
  F_FieldTypeNames.Free;
  F_DefaultValues.Free;
  F_MDFieldNames.Free;
  F_DisplayLabels.Free;
  F_QueryDefParameters.Free;
  F_QueryDefSQLText.Free;
  F_QD_ParamNames.Free;
  F_QD_ParamDaoTypes.Free;
  F_QD_ParamBDETypes.Free;
  F_MasterLink.Free;
  F_MasterFields.Free;
  F_Detail.Free;
  F_Master.Free;
  F_KeyFields.Free;
  F_UpdatableFields.Free;

  F_BookmarkRN.Free;
  F_BookmarkID.Free;

  //****************************************************************************
  F_IndexDefs.Free;
  //****************************************************************************

  {$IFDEF USEPARAMS}
   {$IFNDEF VER100}
    {$IFNDEF VER110}
  F_Params.Free;
    {$ENDIF}
   {$ENDIF}
  {$ENDIF}
  {$IFDEF DYNADAO}
  VarClear(F_DaoTable);
  {$ELSE}
  F_DaoTable  := Nil;
  {$ENDIF}
  if F_OLE_ON then CoUninitialize;
  inherited Destroy; 
End;

Function TKADaoTable.GetTickCountEx:Cardinal;
Begin
  Result := GetTickCount+Random(10000);
End;

Procedure TKADaoTable.F_Set_ComponentVersion(Value: String);
Begin
 //*************************** ReadOnly
End;

Function TKADaoTable.ExecSQL(SQL:TStrings):Integer;
Begin
 Result:=0;
 RecordsAffected:=Result;
 if Assigned(F_Database) And (F_Database.Connected) Then
    Begin
      F_Database.CoreDatabase.Execute(F_ComposeSQL(SQL),SQLExecutionType);
      Result:=F_Database.CoreDatabase.RecordsAffected;
      RecordsAffected:=Result;
    End
 Else
    DatabaseError(E2002);
End;

Function TKADaoTable.ExecSQLString(SQL:String):Integer;
Var
 SQ : TStringList;
Begin
 Result:=0;
 SQ := TStringList.Create;
 Try
  RecordsAffected:=Result;
  SQ.Text:=SQL;
  if Assigned(F_Database) And (F_Database.Connected) Then
    Begin
      F_Database.CoreDatabase.Execute(F_ComposeSQL(SQ),SQLExecutionType);
      Result:=F_Database.CoreDatabase.RecordsAffected;
      RecordsAffected:=Result;
    End
  Else
    DatabaseError(E2002);
 Finally
  SQ.Free;
 End;
End;

Function TKADaoTable.F_RecalculateRecNo(TempRS:OleVariant;BK:Integer):Integer;
Var
  FPP : Single;
  CR  : Integer;
  RC  : Integer;
Begin
  //******************************************************************* 2.1.2001
  Result := -1;
  if Not F_UseGetRecNo Then Exit;
  //****************************************************************************
  FPP := TempRS.PercentPosition;
  //*************************************************** Decrease for calc errors
  FPP := FPP-2;
  if FPP < 0 Then FPP:=0;
  //****************************************************************************
  RC  := RecordCount;
  CR  := Round((FPP*(RC))/100);
  TempRS.MoveFirst;
  TempRS.Move(CR);
  //****************************************************************************
  While (NOT TempRS.EOF) And (GetDaoBookmark(TempRS) <> BK) do
    Begin
      TempRS.MoveNext;
      Inc(CR);
    End;
  //*************************************************************** Safety check
  if TempRS.EOF Then
     Begin
       CR  := 0;
       TempRS.MoveFirst;
       While GetDaoBookmark(TempRS) <> BK do
         Begin
          TempRS.MoveNext;
          Inc(CR);
         End;
     End;
  if TempRS.BOF Then
     Begin
       CR := RC;
       TempRS.MoveLast;
       While GetDaoBookmark(TempRS) <> BK do
         Begin
          TempRS.MovePrevious;
          Dec(CR);
         End;
     End;
  //****************************************************************************
  Result := CR;
End;

Function TKADaoTable.ExecuteSQL:Integer;
Begin
 Result:=0;
 RecordsAffected:=Result;
 if Assigned(F_Database) And (F_Database.Connected) Then
    Begin
      F_Database.CoreDatabase.Execute(F_ComposeSQL(SQL),SQLExecutionType);
      Result:=F_Database.CoreDatabase.RecordsAffected;
      RecordsAffected:=Result;
    End
 Else
    DatabaseError(E2003);
End;

Function TKADaoTable.ExecuteQueryDefSQL:Integer;
Var
 X         : Integer;
 TabN      : String;
 NRP       : Integer;
 Dir       : Integer;
Begin                                                        
 Result:=0;
 RecordsAffected:=Result;
 if Assigned(F_Database) And (F_Database.Connected) And (F_QueryDefName <> '') Then
    Begin
      TabN:=F_QueryDefName;
      NRP:=0;
      For X:=0 To Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Count-1 do
          Begin
            Dir := F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Direction;
            if (Dir=dbParamInput) Or (Dir=dbParamInputOutput) Then
                Begin
                 Try
                  if F_QueryDefParameters.Strings[NRP]='NULL' Then
                     Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Item[X].Value:=NULL
                  Else
                      Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Item[X].Value:=F_QueryDefParameters.Strings[NRP];
                  Inc(NRP);
                 Except
                  DatabaseError(E2004);
                 End;
                End;
          End;
      if F_Database.QueryTimeout <> 60 Then
      F_Database.CoreDatabase.QueryDefs.Item[F_QueryDefName].ODBCTimeout:=F_Database.QueryTimeout;
      F_Database.CoreDatabase.QueryDefs.Item[F_QueryDefName].Execute(SQLExecutionType);
      Result:=F_Database.CoreDatabase.RecordsAffected;
      RecordsAffected:=Result;
      GetQueryDefReturnParams(F_QueryDefName);
    End
 Else
    DatabaseError(E2005);
End;

Function  TKADaoTable.Requery : Boolean;
Var
 X         : Integer;
 TabN      : String;
 NRP       : Integer;
 Dir       : Integer;
Begin
  Result:=False;
  If Not F_Active Then Exit;
  if Not F_DaoTable.Restartable Then Exit;
  if (F_QueryDefName <> '') Then
      Begin
        TabN:=F_QueryDefName;
        NRP:=0;
        For X:=0 To Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Count-1 do
          Begin
            Dir := F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Direction;
            if (Dir=dbParamInput) Or (Dir=dbParamInputOutput) Then
                Begin
                 Try
                  if F_QueryDefParameters.Strings[NRP]='NULL' Then
                     Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Item[X].Value:=NULL
                  Else
                     Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Item[X].Value:=F_QueryDefParameters.Strings[NRP];
                  Inc(NRP);
                 Except
                  DatabaseError(E2006);
                 End;
                End;
          End;
      End;
  if (MasterSource <> NIL) And (Not F_MDisabled) then
     Begin
       MasterDatasetChanged;
     End
  Else
     Begin
      CheckBrowseMode;
      InternalClearBookmarks;
      ClearBuffers;
      OleVariant(F_DaoTable).Requery;
      F_RefreshRC := True;
      ActivateBuffers;
      First;
     End;
  Result:=True;
End;

Procedure TKADaoTable.GotoCurrent(Table: TKADaoTable);
Begin
  CheckBrowseMode;
  Table.CheckBrowseMode;
  if (AnsiCompareText(F_Database.Database, Table.Database.Database) <> 0)
  or (AnsiCompareText(TableName, Table.TableName) <> 0)
  or (AnsiCompareText(IndexName, Table.IndexName) <> 0) then
     DatabaseError(E2007);
  Table.UpdateCursorPos;
  CheckBrowseMode;
  First;
  MoveBy(Table.RecNo-1);
  Resync([rmExact, rmCenter]);
End;

Procedure TKADaoTable.GetIndexNames(List: TStrings);
Var
 Count,X : Integer;
Begin
  List.Clear;
  Try
    if Assigned(F_Database) And (F_Database.Connected) Then
     Begin
      F_Database.RefreshDefinitions;
      Count :=F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes.Count;
      For X := 0 to  Count-1 do
          Begin
            List.Add(F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes.Item[X].Name);
          End;
     End;
  Except
  End;
End;

Procedure TKADaoTable.GetFieldNames(List: TStrings);
Var
 X      : Integer;
 IsOpen : Boolean;
Begin
  List.Clear;
  IsOpen := Active;
  Try
    Try
      if Not IsOpen Then Open;
      For X := 0 To FieldCount-1 do
          Begin
            List.Add(Self.Fields[X].FieldName)
          End;
    Finally
      if (Not IsOpen) And (Active) Then Close;
    End;
  Except
  End;
End;

Function TKADaoTable.PercentPosition:Single;
Begin
 Result := -1;
 if NOT F_Active  Then Exit;
 if F_DaoTable.BOF Then Exit;
 if F_DaoTable.EOF Then Exit;
 Try
    Result := F_DaoTable.PercentPosition;
 Except
 End;
End;

Function  TKADaoTable.GetSourceFieldName(FieldName:String):String;
Begin
 Result :='';
 if Not F_Active Then Exit;
 Try
   Result := F_DaoTable.Fields.Item[FieldName].SourceField;
 Except
 End;
End;

Function  TKADaoTable.GetSourceTableName(FieldName:String):String;
Begin
 Result :='';
 if Not F_Active Then Exit;
 Try
   Result := F_DaoTable.Fields.Item[FieldName].SourceTable;
 Except
 End;
End;

Function  TKADaoTable.GetLastDaoError:TDaoErrRec;
Begin
  if Assigned(F_Database) And (F_Database.Connected) Then
     Result := F_Database.GetLastDaoError;
End;

Function TKADaoTable.PropertyExists(PropObject:OleVariant;PropertyName:String):Boolean;
Var
  X : Integer;
Begin
  Result := False;
  For X := 0 to PropObject.Count-1 do
      Begin
        if AnsiCompareText(PropObject.Item[X].Name,PropertyName)=0 Then
           Begin
             Result := True;
             Exit;
           End;
      End;
End;

Procedure TKADaoTable.GetQueryDefParameters(F_QD_ParamNames,F_QD_ParamDaoTypes, F_QD_ParamBDETypes:TStringList);
Var
  X       : Integer;
  Dir     : Integer;
  NP      : Integer;
  Typ     : Integer;
Begin
  if NOT Assigned(F_Database) Then Exit;
  if NOT (F_Database.Connected) Then Exit;
  if F_QueryDefName='' Then Exit;
  if Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters.Count=0 Then
     Begin
      if (csDesigning in ComponentState) Then DatabaseError(E2008);
      Exit;
     End;
  F_QD_ParamNames.Clear;
  F_QD_ParamDaoTypes.Clear;
  F_QD_ParamBDETypes.Clear;
  Try
     NP:=0;
     For X := 0 To Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters.Count-1 do
        Begin
          Dir:= Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Direction;
          if (Dir=dbParamInput) Or (Dir=dbParamInputOutput) Then
             Begin
              Inc(NP);
              {$IFDEF DYNADAO}
              Typ :=Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Type;
              {$ELSE}
              Typ :=Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Type_;
              {$ENDIF}
              if (Typ=dbDate) Then Typ:=dbTimeStamp;
              F_QD_ParamNames.AddObject(Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Name,TObject(Typ));
              F_QD_ParamDaoTypes.AddObject(GetDaoFieldTypeNames(Typ),TObject(Typ));
              F_QD_ParamBDETypes.AddObject(GetBDEFieldTypeNames(DaoToBDE(Typ)),TObject(DaoToBDE(Typ)));
             End;
        End;
     if NP=0 Then
        Begin
           if (csDesigning in ComponentState) Then DatabaseError(E2009);
           Exit;
        End;
  Except
      DatabaseError(E2010);
      Exit;
  End;
End;

Function TKADaoTable.PromptQueryDefParameters:Boolean;
Begin
  Result := False;
  GetQueryDefParameters(F_QD_ParamNames ,F_QD_ParamDaoTypes, F_QD_ParamBDETypes);
  if F_QD_ParamNames.Count=0 Then Exit;
  Application.CreateForm(TQueryDefDialog,QueryDefDialog);
  Result := QueryDefDialog.Execute(F_QD_ParamNames,F_QD_ParamDaoTypes,F_QD_ParamBDETypes,F_QueryDefParameters);
  QueryDefDialog.Free;
End;

Function TKADaoTable.StoreField(X:Integer): Boolean;
Begin
   Case Fields[X].FieldKind of
        fkData       : Result := True;
        fkCalculated : Result := False;
        fkLookup     : Result := False;
   Else                Result := False;
   End;
   if NOT Fields[X].Visible Then Result := False;
End;


Procedure TKADaoTable.SaveToStream(Stream: TStream);
Var
   X          : Integer;
   Book       : TBookmark;
   Writer     : TWriter;
   Current    : Integer;
   Total      : Integer;
Begin
  if IsEmpty Then Exit;
  Book         := GetBookmark;
  Try
   DisableControls;
   Writer := TWriter.Create(Stream, 16384);
   Writer.WriteSignature;
   Try
   //*************************************************** Write Structure
   Writer.WriteListBegin;
   For X:=0 to FieldCount-1 do
       Begin
        If F_ProcessMessages Then Application.ProcessMessages;
        if StoreField(X) then
           Begin
            Writer.WriteString(Fields[X].FieldName);
            Writer.WriteString(FieldTypeNames[Fields[X].DataType]);
            Writer.WriteInteger(Fields[X].Size);
            Writer.WriteString(Fields[X].DisplayName);
            Writer.WriteString(Fields[X].EditMask);
            Writer.WriteInteger(Fields[X].DisplayWidth);
            Writer.WriteBoolean(Fields[X].Required);
            Writer.WriteBoolean(Fields[X].ReadOnly);
           End;
       end;
   Writer.WriteListEnd;

   //******************************************************** Write Data
   Total  := RecordCount-1;
   Current:=0;
   Writer.WriteListBegin;
   First;
   While Not EOF do
     Begin
      if Assigned(F_OnExportProgress) Then F_OnExportProgress(Current,Total);
      For X:=0 to FieldCount-1 do
        Begin
          If F_ProcessMessages Then Application.ProcessMessages;
          if StoreField(X) Then
             Begin
               Case Fields[X].DataType of
                    ftBoolean   : Writer.WriteBoolean(Fields[X].AsBoolean);
                    ftSmallInt  ,
                    ftInteger   ,
                    ftWord      ,
                    ftAutoInc   : Writer.WriteInteger(Fields[X].AsInteger);
                    ftFloat     : Writer.WriteFloat(Fields[X].AsFloat);
                    ftBCD       ,
                    ftCurrency  : Writer.WriteFloat(Fields[X].AsCurrency);
                    ftDate      ,
                    ftTime      ,
                    ftDateTime  : Writer.WriteFloat(Fields[X].AsFloat);
               Else
                    Writer.WriteString(Fields[X].AsString);
               End;
             End;
        End;
      Inc(Current);
      Next;
      F_Database.Idle;
     End;
   Writer.WriteListEnd;
   Finally
     Writer.FlushBuffer;
     Writer.Free;
   End;
  Finally
    GotoBookmark(Book);
    EnableControls;
    FreeBookmark(Book);
  End;
End;

Procedure TKADaoTable.SaveToFile(const FileName: String);
Var
 Stream: TStream;
Begin
 Stream := TFileStream.Create(FileName, fmCreate);
 Try
  SaveToStream(Stream);
 Finally
  if Stream.Size=0 Then
     Begin
       Stream.Free;
       DeleteFile(FileName);
     End
  Else
     Begin
       Stream.Free;
     End;
 End;
End;

Procedure TKADaoTable.LoadFromStream(Stream: TStream; Mode : TLoadMode);
Var
  Reader       : TReader;
  FieldName    : String;
  DataTypeName : String;
  DisplayName  : String;
  EditMask     : String;
  DisplayWidth : Integer;
  Required     : Boolean;
  ReadOnly     : Boolean;
  I            : Integer;
  X            : Integer;
  Field        : TField;
  FNames       : TStringList;
  Book         : TBookmark;
  OK           : Boolean;
  Current      : LongInt;
  KbmFileVers  : Integer;
Begin
  if Not Active Then DatabaseError(E2058);
  if Mode = lmEmptyAppend Then EmptyTable;
  Book   := GetBookmark;
  Reader := TReader.Create(Stream, 16384);
  FNames := TStringList.Create;
  Try
   DisableControls;
   Reader.ReadSignature;
   if (Reader.NextValue = vaList) Then
       KbmFileVers := 100 Else
       KbmFileVers := Reader.ReadInteger;
   //************************************************************ Read Structure
   Reader.ReadListBegin;
     While (Not Reader.EndOfList) Do
      Begin
       If F_ProcessMessages Then Application.ProcessMessages;
       FieldName    := Reader.ReadString;
       DataTypeName := Reader.ReadString;
                       Reader.ReadInteger;
       DisplayName  := Reader.ReadString;
       EditMask     := Reader.ReadString;
       DisplayWidth := Reader.ReadInteger;
       Required     := Reader.ReadBoolean;
       ReadOnly     := Reader.ReadBoolean;
       if (KbmFileVers >= 250) Then Reader.ReadString;
       FNames.Add(FieldName);
       I := FieldDefs.IndexOf(FieldName);
       if I > -1 Then
          Begin
            Field:=FindField(FieldName);
            if Field <> Nil Then
               Begin
                Field.DisplayLabel := DisplayName;
                Field.EditMask     := EditMask;
                Field.DisplayWidth := DisplayWidth;
                Field.Required     := Required;
                Field.ReadOnly     := ReadOnly;
              End
            Else
              DatabaseError(E2059);
          End
       Else
         DatabaseError(E2059);
     End;
   Reader.ReadListEnd;
   //***************************************************************** Read Data
   Last;
   Reader.ReadListBegin;
   Try
     F_Database.StartTransaction;
   Except
   End;
   Current := 0;
   While (NOT Reader.EndOfList) do
    Begin
     if Assigned(F_OnImportProgress) Then F_OnImportProgress(Current);
     OK := False;
     For X :=0 to FNames.Count-1 do
         Begin
          If F_ProcessMessages Then Application.ProcessMessages;
          Field := FindField(FNames.Strings[X]);
          if (Field <> Nil) Then
             Begin
               if NOT OK Then
                  Begin
                    OK := True;
                    Insert;
                  End;
               Case Field.DataType of
                    ftBoolean  : if Not Field.ReadOnly Then Field.AsBoolean  := Reader.ReadBoolean Else Reader.ReadBoolean;
                    ftSmallInt ,
                    ftInteger  ,
                    ftWord     ,
                    ftAutoInc  : if Not Field.ReadOnly Then Field.AsInteger  := Reader.ReadInteger Else Reader.ReadInteger;
                    ftFloat    : if Not Field.ReadOnly Then Field.AsFloat    := Reader.ReadFloat   Else Reader.ReadFloat;
                    ftBCD,
                    ftCurrency : if Not Field.ReadOnly Then Field.AsCurrency := Reader.ReadFloat   Else Reader.ReadFloat;
                    ftDate     ,
                    ftTime     ,
                    ftDateTime : if Not Field.ReadOnly Then Field.AsFloat    := Reader.ReadFloat   Else Reader.ReadFloat;
               Else
                    Begin
                      if Not Field.ReadOnly Then            Field.AsString   := Reader.ReadString  Else Reader.ReadString;
                    End;
               End;
             End;
         End;
     if OK Then Post;
     F_Database.Idle;
     Inc(Current);
    End;
   Try
     F_Database.Commit;
   Except
   End;
   Reader.ReadListEnd;
  Finally
   Reader.Free;
   FNames.Free;
   if Mode = lmAppend Then GotoBookmark(Book);
   EnableControls;
   FreeBookmark(Book);
  End;
End;


Procedure TKADaoTable.LoadFromFile(const FileName: String; Mode : TLoadMode);
Var
 Stream: TStream;
Begin
 if Not Active Then DatabaseError(E2058);
 Stream := TFileStream.Create(FileName, fmOpenRead);
 Try
  LoadFromStream(Stream, Mode);
 Finally
  Stream.Free;
 End;
End;

Function TKADaoTable.F_Get_Database:TKADaoDatabase;
Begin
 Result:=F_Database;
End;

Procedure TKADaoTable.F_Set_Database(Value:TKADaoDatabase);
Begin
 if Active Then DatabaseError(E2011);
 {$IFDEF VER130}
 if Assigned(F_Database) Then F_Database.RemoveFreeNotification(Self);
 {$ENDIF}
 F_Database := Value;
 if Assigned(F_Database) Then F_Database.FreeNotification(Self);
End;

Function TKADaoTable.F_Get_DateCreated:String;
Begin
 Result := '';
 if F_Active Then
    Begin
     Try
      if TableType=dbOpenTable Then Result:=F_DaoTable.DateCreated
      Else
      if F_QueryDefName <> '' then Result:=Database.CoreDatabase.QueryDefs.Item[F_QueryDefName].DateCreated;
     Except
     End;
    End;
End;

Function TKADaoTable.F_Get_LastUpdated:String;
Begin
 Result := '';
 if F_Active Then
    Begin
     Try
      if TableType=dbOpenTable Then Result:=F_DaoTable.LastUpdated
      Else
      if F_QueryDefName <> '' then Result:=Database.CoreDatabase.QueryDefs.Item[F_QueryDefName].LastUpdated;
     Except
     End;
    End;
End;

Function TKADaoTable.F_Get_TableName:String;
Begin
 Result:= F_TableName;
End;

Procedure TKADaoTable.F_Set_TableName(Value:String);
Begin
  if Active Then DatabaseError(E2012);
  F_TableName:=Value;
  if Value <> '' Then
     Begin
      F_IndexName:='';
      F_SQL.Clear;
      F_QueryDefName:='';
      F_QueryDefSQLText.Clear;
      F_QueryDefParameters.Clear;
      F_SortedBy.Clear;
      F_DisplayLabels.Clear;
      FieldDefs.Clear;
      IndexDefs.Clear;
     End;
End;

Procedure TKADaoTable.F_Set_SQL(Value:TStrings);
Begin
 F_SQL.SetText(Value.GetText);
 if Length(Value.GetText) > 0 Then
    Begin
     F_QueryDefParameters.Clear;
     F_QueryDefSQLText.Clear;
     F_QueryDefName:='';
     F_IndexName:='';;
     F_TableName:='';
     F_DisplayLabels.Clear;
     FieldDefs.Clear;
     IndexDefs.Clear;
    End;
End;

Procedure TKADaoTable.F_Set_QueryDefName(Value:String);
Begin
  if F_Active Then DatabaseError(E2065);
  Try
    if Assigned(F_Database) And (F_Database.Connected) Then
       Begin
         F_QueryDefSQLText.Clear;
         if Value <> '' Then F_QueryDefSQLText.SetText(PChar(F_Database.GetQueryDefSQLText(Value)));
       End;
  Except
  End;
  F_QueryDefName:=Value;
  F_QueryDefType:=F_Get_QueryDefType;
  if Value <> '' Then
     Begin
      F_IndexName:='';
      F_TableName:='';
      F_SQL.Clear;
      F_QueryDefParameters.Clear;
      F_Master.Clear;
      F_Detail.Clear;
      F_MasterFields.Clear;
      F_DisplayLabels.Clear;
      FieldDefs.Clear;
      IndexDefs.Clear;
      if (F_TableType=dbOpenTable)
      Or (F_TableType=dbOpenDynamic) Then F_TableType:=dbOpenDynaset;
     End;
End;



Function TKADaoTable.F_Get_IndexName:String;
Begin
 Result:= F_IndexName;
End;

Procedure TKADaoTable.F_Set_IndexName(Value:String);
Begin
  if Active Then
     Begin
       if (TableType=dbOpenTable) Then
          Begin
             F_SortedBy.Clear;
             F_DaoTable.Index  := Value;
             CheckBrowseMode;
             ClearBuffers;
             F_RefreshRC := True;
             ActivateBuffers;
             First;
          End
       Else if Value <> '' Then DatabaseError(E2013);
     End
   Else
     Begin
       if (TableType=dbOpenTable) And (Value <> '') Then
          Begin
            F_SortedBy.Clear;
          End
       Else
          Begin
            if Value <> '' Then DatabaseError(E2013);
          End;
     End;
  F_IndexName:=Value;
End;

Function  TKADaoTable.F_Get_IndexFieldNames:String;
Var
  X     : Integer;
  Count : Integer;
Begin
  Result := '';
  if F_IndexName='' Then Exit;
  Try
    Count := F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes.Item[F_IndexName].Fields.Count-1;
    For X := 0 To Count do
      Begin
        if X = Count Then
           Result := Result + F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes.Item[F_IndexName].Fields.Item[X].Name
        Else
           Result := Result + F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes.Item[F_IndexName].Fields.Item[X].Name+';';
      End;
  Except
  End;
End;

Function  TKADaoTable.F_Get_IndexFieldCount:Integer;
Begin
 F_IndexFieldCount := 0;
 if  (TableType=dbOpenTable)
 And (Assigned(F_Database))
 And (F_Database.Connected)
 And (F_TableName <> '')
 And (F_IndexName <> '') Then
     Begin
      Try
       F_IndexFieldCount := F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes.Item[F_IndexName].Fields.Count;
      Except
       F_IndexFieldCount := 0;
      End;
     End;
 Result := F_IndexFieldCount;
End;

Procedure TKADaoTable.F_Set_IndexFieldCount(Value:Integer);
Begin
 //******************************************************************* Read Only
End;

Function  TKADaoTable.FindGoodIndex(KeyFields:String):String;
Var
  KFL    :  TStringList;
  IFL    :  TStringList;
  X,Y    :  Integer;
  BR     :  Integer;
  Exact  :  Boolean;
  Value  :  String;
Begin
  Result := '';
  //****************************************************************************
  if IndexDefs.Count=0 Then Exit;
  if TableType <> dbOpenTable then Exit;
  if KeyFields='' Then Exit;
  Value := KeyFields;
  if Value[1]='!' Then
     Begin
      Exact:=True;
      System.Delete(Value,1,1);
     End
  Else
     Begin
       Exact:=False;
     End;
  if Value = '' Then Exit;
  //****************************************************************************

  KFL := TStringList.Create;
  IFL := TStringList.Create;
  Try
    StringToList(Value,KFL);
    For X := 0 To IndexDefs.Count-1 Do
      Begin
        StringToList(IndexDefs.Items[X].Fields,IFL);
        if Exact Then
           Begin
             if IFL.Count = KFL.Count Then
                Begin
                  BR:=0;
                  For Y := 0 to KFL.Count-1 do
                    Begin
                      if NOT (IFL.IndexOf(KFL.Strings[Y]) < 0) Then Inc(BR);
                    End;
                  if BR=KFL.Count Then
                     Begin
                      Result := IndexDefs.Items[X].Name;
                      Exit;
                     End;
                End;
           End
        Else
           Begin
             if IFL.Count >= KFL.Count Then
                Begin
                   BR:=0;
                   For Y := 0 to KFL.Count-1 do
                    Begin
                      if NOT (IFL.IndexOf(KFL.Strings[Y]) < 0) Then Inc(BR);
                    End;
                   if BR=KFL.Count Then
                      Begin
                       Result:=IndexDefs.Items[X].Name;
                       if KFL.IndexOf(IFL.Strings[0]) <> -1 Then Exit;
                      End;
                 End;
           End;
      End;
  Finally
    KFL.Free;
    IFL.Free;
  End;
End;

Procedure TKADaoTable.F_Set_IndexFieldNames(Value:String);
Var
  S : String;
Begin
  if TableType <> dbOpenTable then Exit;
  if Value='' Then
     Begin
       F_Set_IndexName(Value);
       Exit;
     End;
  S:=FindGoodIndex(Value);
  if S <> '' Then F_Set_IndexName(S);
End;

Function TKADaoTable.F_Get_IndexField(Index: Integer): TField;
Var
 FieldName:String;
Begin
 Result := Nil;
 if NOT Active Then DatabaseError(E2014);
 if F_IndexName='' Then Exit;
 Try
  FieldName:=F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes.Item[F_IndexName].Fields.Item[Index].Name;
 Except
  Exit;
 End;
 Result := FindField(FieldName);
End;

Procedure TKADaoTable.F_Set_IndexField(Index: Integer; Value: TField);
Begin
 //******************************************************************* Read Only
End;

Procedure TKADaoTable.F_SetBatchMode(Value:Boolean);
Begin
 F_BatchMode := Value;
 if Value Then DisableControls Else EnableControls; 
 if Not Value Then
    Begin
      Resync([]); //************************************** 3.1.2002
      if F_UseGetRecNo Then GetRecNo;  
    End;
End;

Procedure TKADaoTable.F_Set_TableType(Value:Integer);
Begin
  if Active Then DatabaseError(E2015);
  F_TableType:=Value;
  if F_TableType=dbOpenTable Then
     Begin
       F_SortedBy.Clear;
     End
  Else
     Begin
       F_IndexName:='';
       IndexDefs.Clear;
     End;
  if F_TableType=dbOpenForwardOnly Then F_SortedBy.Clear;
End;

Procedure TKADaoTable.F_Set_LockType(Value:Integer);
Begin
  if Active Then DatabaseError(E2016);
  F_LockType:=Value;
End;

Procedure TKADaoTable.F_Set_OpenOptions(Value:TOOSet);
Begin
  F_OpenOptions:=Value;
  if F_Active Then
     Begin
       CheckBrowseMode;
       ClearBuffers;
       CloseDaoRecordset;
       OpenDaoRecordset;
       ActivateBuffers;
       First;
     End;
End;

Procedure TKADaoTable.LockTable(LockType: TLockType);
Var
  OO:TOOSet;
Begin
  if LockType = ltReadLock  Then OO := F_OpenOptions+[dbDenyRead];
  if LockType = ltWriteLock Then OO := F_OpenOptions+[dbDenyWrite];
  if Active Then F_Set_OpenOptions(OO);
End;

Procedure TKADaoTable.UnlockTable(LockType: TLockType);
Var
  OO:TOOSet;
Begin
  if LockType = ltReadLock  Then OO := F_OpenOptions-[dbDenyRead];
  if LockType = ltWriteLock Then OO := F_OpenOptions-[dbDenyWrite];
  if Active Then F_Set_OpenOptions(OO);
End;

Procedure TKADaoTable.F_Set_ReadOnly(Value:Boolean);
Begin
  if Assigned(F_Database) And (F_Database.Connected) and (F_Database.ReadOnly) And (NOT Value) Then
     Begin
       Value := True;
     End;
  F_ReadOnly:=Value;
End;

Procedure TKADaoTable.SetLockEdits(LockEdits : Boolean);
Begin
  if  (Active)
  And (F_Database.DatabaseType <> 'ODBC')
  And (NOT F_Database.ReadOnly)                                 
  And (NOT F_ReadOnly)
  And ((F_TableType = dbOpenTable) Or (F_TableType = dbOpenDynaset)) Then
      Begin
        F_DaoTable.LockEdits := LockEdits;
      End;
End;

Procedure TKADaoTable.F_Set_LockEdits(Value:Boolean);
Begin
  //****************************************************************** Read Only
End;

Function TKADaoTable.F_Get_LockEdits:Boolean;
Begin
  Result := False;
  if  (Active)
  And (F_Database.DatabaseType <> 'ODBC')
  And (NOT F_Database.ReadOnly)
  And (NOT F_ReadOnly)
  And ((F_TableType = dbOpenTable) Or (F_TableType = dbOpenDynaset)) Then
      Begin
        Result := F_DaoTable.LockEdits;
      End;
End;

Procedure TKADaoTable.F_Set_Sort(Value:TStrings);
Begin
 F_SortedBy.SetText(Value.GetText);
 F_IndexName:='';
 if F_Active Then
    Begin
     CheckBrowseMode;
     ClearBuffers;
     CloseDaoRecordset;
     OpenDaoRecordset;
     ActivateBuffers;
     First;
    End;
End;

Procedure TKADaoTable.Sort;
Begin
 if F_Active Then
    Begin
     CheckBrowseMode;
     ClearBuffers;
     CloseDaoRecordset;
     OpenDaoRecordset;
     ActivateBuffers;
     First;
    End;
End;

Procedure TKADaoTable.F_Set_QueryDefParameters(Value:TStrings);
Begin
 F_QueryDefParameters.SetText(Value.GetText);
End;

Procedure TKADaoTable.F_Set_QueryDefSQLText(Value:TStrings);
Begin
 if  (Assigned(F_Database))
 And (F_Database.Connected)
 And (Not F_Active)
 And (F_QueryDefName <> '')
 And (F_QueryDefCanModify)
 And (Not (csLoading in ComponentState)) Then
     Begin
      if F_Database.QueryDefNames.IndexOf(F_QueryDefName) <> -1 Then
         F_Database.ModifyQueryDef(F_QueryDefName,F_ComposeSQL(Value))
      Else
         F_Database.CreateQueryDef(F_QueryDefName,F_ComposeSQL(Value));
      F_Database.RefreshDefinitions;
      QueryDefName := F_QueryDefName;
     End;
End;

Function TKADaoTable.F_Get_QueryDefType:String;
Var
 QDType : Integer;
Begin
 Result:='';
 QueryDefTypeInt:=0;
 Try
  if Assigned(F_Database) And (F_Database.Connected) And (F_QueryDefName <> '') Then
    Begin
      {$IFDEF DYNADAO}
      QDType:=Database.CoreDatabase.QueryDefs.Item[F_QueryDefName].Type;
      {$ELSE}
      QDType:=Database.CoreDatabase.QueryDefs.Item[F_QueryDefName].Type_;
      {$ENDIF}
      if QDType=dbQSelect         Then Begin Result := 'dbQSelect'        ; QueryDefTypeInt := dbQSelect         ; End;
      if QDType=dbQProcedure      Then Begin Result := 'dbQProcedure'     ; QueryDefTypeInt := dbQProcedure      ; End;
      if QDType=dbQAction         Then Begin Result := 'dbQAction'        ; QueryDefTypeInt := dbQAction         ; End;
      if QDType=dbQCrosstab       Then Begin Result := 'dbQCrosstab'      ; QueryDefTypeInt := dbQCrosstab       ; End;
      if QDType=dbQDelete         Then Begin Result := 'dbQDelete'        ; QueryDefTypeInt := dbQDelete         ; End;
      if QDType=dbQUpdate         Then Begin Result := 'dbQUpdate'        ; QueryDefTypeInt := dbQUpdate         ; End;
      if QDType=dbQAppend         Then Begin Result := 'dbQAppend'        ; QueryDefTypeInt := dbQAppend         ; End;
      if QDType=dbQMakeTable      Then Begin Result := 'dbQMakeTable'     ; QueryDefTypeInt := dbQMakeTable      ; End;
      if QDType=dbQDDL            Then Begin Result := 'dbQDDL'           ; QueryDefTypeInt := dbQDDL            ; End;
      if QDType=dbQSQLPassThrough Then Begin Result := 'dbQSQLPassThrough'; QueryDefTypeInt := dbQSQLPassThrough ; End;
      if QDType=dbQSetOperation   Then Begin Result := 'dbQSetOperation'  ; QueryDefTypeInt := dbQSetOperation   ; End;
      if QDType=dbQSPTBulk        Then Begin Result := 'dbQSPTBulk'       ; QueryDefTypeInt := dbQSPTBulk        ; End;
      if QDType=dbQCompound       Then Begin Result := 'dbQCompound'      ; QueryDefTypeInt := dbQCompound       ; End;
    End;
 Except
 End;
End;

Function TKADaoTable.WWStringReplace(Src,Pattern,Repl:String):String;
Var
  S  : String;
  Pat: String;
  L  : Integer;
  P  : Integer;
  PR : Integer;
Begin
  Result := Src;
  L := Length(Result);
  if L=0 Then Exit;
  Result := '';
  S   := ' '+AnsiLowerCase(Src)+' ';
  Pat := AnsiLowerCase(Pattern);
  L   := Length(Pat);
  Repeat
    P := AnsiPos(Pat,S);
    if P > 0 Then
       Begin
        PR := P-1;
        if  (Pos(S[P-1],Letters) = 0)
        And (Pos(S[P+L],Letters) = 0) Then
          Begin
            Result := Result+System.Copy(Src,1,PR-1);
            Result := Result+Repl;
          End
        Else
          Begin
           Result := Result+System.Copy(Src,1,PR+L-1);
          End;
        System.Delete(S,1,P+L-1);
        System.Delete(Src,1,PR+L-1);
        S:=' '+S;
       End;
  Until P =0;
  Result := Result+Src;
End;

Function TKADaoTable.ChangeOnlyQuotes(S:String):String;
Var
 X, L : Integer;
Begin
 Result := '';
 L      := Length(S);
 if L   =  0 Then Exit;
 For X := 1 To L do
     Begin
       Result := Result+S[X];
       if S[X]='"'   Then Result := Result+'"';
     End;
End;

Function TKADaoTable.ChangeQuotes(S:String):String;
Var
 X, L : Integer;
Begin
 Result := '';
 L      := Length(S);
 if L   =  0 Then Exit;
 For X := 1 To L do
     Begin
       if (S[X]='*')
       Or (S[X]='?')
       Or (S[X]='#')
       Or (S[X]='[')
       Or (S[X]=']')
       Or (S[X]='!')
       Or (S[X]='-') Then Result := Result+'[';
       Result := Result+S[X];
       if (S[X]='*')
       Or (S[X]='?')
       Or (S[X]='#')
       Or (S[X]='[')
       Or (S[X]=']')
       Or (S[X]='!')
       Or (S[X]='-') Then Result := Result+']';
     End;
End;

Function TKADaoTable.ChangeCommas(S:String):String;
Var
 X, L : Integer;
Begin
 Result := '';
 L      := Length(S);
 if L   =  0 Then Exit;
 For X := 1 To L do
     Begin
       if S[X]=DecimalSeparator Then
          Result := Result+'.'
       Else
       if S[X]<> ThousandSeparator Then Result := Result+S[X];
     End;
End;

Function TKADaoTable.EscapeReservedChars(S:String):String;
Begin
  Result := ChangeQuotes(S);
End;

Function TKADaoTable.F_ComposeSQL(SQL:TStrings):String;
Var
 X       : Integer;
{$IFDEF USEPARAMS}
 {$IFNDEF VER100}
  {$IFNDEF VER110}
 S, Sep  : String;
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

Begin
 Result := SQL.Text;
 {$IFDEF USEPARAMS}
  {$IFNDEF VER100}
   {$IFNDEF VER110}
 if F_ParamCheck then
    Begin
      For X := 0 to F_Params.Count - 1 do
        Begin
            if F_Params[X].IsNull Then
               Begin
                 S := ' IS NULL';
               End
            Else
               Begin
                 Case F_Params[X].DataType of
                      ftDateTime   ,
                      ftDate       ,
                      ftTime       : Sep := '#';
                      ftUnknown    : Sep := '';
                      ftBlob       ,
                      ftMemo       ,
                      ftString     ,
                      ftWideMemo   ,
                      ftWideString : Sep := '"';
                 Else
                      Sep := '';
                 end;
                 //******************************************************* 04.10.2001
                 Case F_Params[X].DataType of
                      ftBytes      : Begin
                                       S:= Sep + '{guid '+GetGUIDAsString(F_Params[X].AsString)+'}'   + Sep;
                                     End;
                      ftDate       : Begin
                                       S:= Sep + FormatDateTime('mm"/"dd"/"yyyy', F_Params[X].AsDate) + Sep;
                                     End;
                      ftTime       : Begin
                                       S:= Sep + FormatDateTime('hh":"nn":"ss', F_Params[X].AsTime)   + Sep;
                                     End;
                      ftDateTime   : Begin
                                       S:= Sep + FormatDateTime('mm"/"dd"/"yyyy hh":"nn":"ss', F_Params[X].AsDateTime) + Sep;
                                     End;
                      ftBlob       ,
                      ftMemo       ,
                      ftString     ,
                      ftWideMemo   ,
                      ftWideString : Begin
                                       S := Sep + ChangeOnlyQuotes(F_Params[X].AsString) + Sep;
                                     End;
                      ftBCD        ,
                      ftCurrency,
   		      ftFloat                : Begin
                                       S := Sep + ChangeCommas(F_Params[X].AsString) + Sep;
                                     End;
                      Else
                         S := Sep + F_Params[X].AsString + Sep;
                 End;
                //******************************************************************
               End;
            Result := WWStringReplace(Result, ':' + F_Params[X].Name, S);
        end;
    End;
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
End;

Function TKADaoTable.ComposeFilter:String;
Begin
   if F_Filtered Then Result := F_Filter Else Result := '';
   if (MasterSource <> NIL) And (Not(F_MDisabled)) And (MasterSource.Enabled)  then
       Begin
         F_ProcessMasterFields(F_MasterFields);
         if (F_Master.Count > 0) Then
             Begin
               Result := BuildDetailSQL;
               Result := InsertSQLString(Result);
              End;
       End;
End;

Procedure TKADaoTable.SetFiltered(Value:Boolean);
var
  Old_Filtered : Boolean;
Begin
  Inherited SetFiltered(F_Filtered);
  Old_Filtered := F_Filtered;
  Try
    F_Filtered:=Value;
    if F_Filtered=Old_Filtered Then Exit;
    if F_Active Then
       Begin
         CheckBrowseMode;
         ClearBuffers;
         CloseDaoRecordset;
         OpenDaoRecordset;
         ActivateBuffers;
         if Not IsEmpty Then First;
       End;
  Except
    F_Filtered := Old_Filtered;
    Raise;
  End;
End;

Procedure TKADaoTable.SetFilterText(Const Value:String);
Begin
  Inherited SetFilterText(F_Filter);
  F_Filter:=Value;
  if (F_Active) And (F_Filtered) Then
     Begin
       CheckBrowseMode;
       ClearBuffers;
       CloseDaoRecordset;
       OpenDaoRecordset;
       ActivateBuffers;
       if Not IsEmpty Then First;
     End;
End;

Procedure TKADaoTable.F_Set_CacheMemos(Value:Boolean);
Begin
  F_CacheMemos:=Value;
  if (csLoading in ComponentState) Then Exit;
  if (F_Active) Then
     Begin
       Close;
       Open;
       First;
     End;
End;

Procedure TKADaoTable.F_Set_CacheBlobs(Value:Boolean);
Begin
  F_CacheBlobs:=Value;
  if (csLoading in ComponentState) Then Exit;
  if (F_Active) Then
     Begin
       Close;
       Open;
       First;
     End;
End;

Procedure TKADaoTable.F_Set_ShowGUID(Value:Boolean);
Begin
  F_ShowGUID:=Value;
  if (csLoading in ComponentState) Then Exit;
  if (F_Active) Then
     Begin
       Close;
       Open;
       First;
     End;
End;

Procedure TKADaoTable.F_Set_CacheLookups(Value:Boolean);
Begin
  F_CacheLookups:=Value;
  if (csLoading in ComponentState) Then Exit;
  if (F_Active) Then
     Begin
       Close;
       Open;
       First;
     End;
End;

Procedure TKADaoTable.F_Set_Encrypter(Value:TComponent);
Begin
  F_Encrypter := Value;
  if (csLoading in ComponentState) Then Exit;
  if (F_Active) Then
     Begin
       Close;
       Open;
       First;
     End;
End;


Procedure TKADaoTable.F_Set_OnFilterRecord(Value: TFilterRecordEvent);
Begin
  F_OnFilterRecord:=Value;
  if (F_Active) And (F_Filtered) Then
     Begin
       CheckBrowseMode;
       ClearBuffers;
       CloseDaoRecordset;
       OpenDaoRecordset;
       ActivateBuffers;
       First;
     End;
  Inherited OnFilterRecord:=Value;
End;

//******************************************************************************
Function TKADaoTable.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
Begin
  Result:=TKBlobStream.Create(TBlobField(Field),Mode);
End;

Function TKADaoTable.InternalCalcRecordSize:Integer;
Begin
 F_RecordSize:=0;
 Result:=F_RecordSize;
End;


Procedure TKADaoTable.GetQueryDefReturnParams(QueryDefName:String);
Var
  X, Dir, NRP : Integer;
Begin
  if (NOT Assigned(F_Database)) OR (NOT F_Database.Connected) Then Exit;
  if NOT VarIsNull(QueryDefReturnParams) Then QueryDefReturnParams:=NULL;
  NRP:=0;
 Try
  For X:=0 To F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters.Count-1 do
      Begin
        Dir := F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Direction;
        if (Dir=dbParamOutput) Or (Dir=dbParamInputOutput) Or (Dir=dbParamReturnValue) Then Inc(NRP);
      End;
  if NRP=0 Then Exit;
  if NRP=1 Then
      Begin
       For X:=0 To F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters.Count-1 do
           Begin
            Dir := F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Direction;
            if (Dir=dbParamOutput) Or (Dir=dbParamInputOutput) Or (Dir=dbParamReturnValue) Then
               Begin
                 QueryDefReturnParams:=F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Value;
                 Exit;
               End;
           End;
      End
  Else
      Begin
        QueryDefReturnParams:=VarArrayCreate([0, NRP],varVariant);
        For X:=0 To F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters.Count-1 do
           Begin
             Dir := F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Direction;
             if (Dir=dbParamOutput) Or (Dir=dbParamInputOutput) Or (Dir=dbParamReturnValue) Then
                QueryDefReturnParams[X]:=F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Value;
           End;
     End;
 Except
 End;
End;

Function TKADaoTable.ProcessDTDefault(S:String):String;
Var
 P         : Integer;
 M,D,Y     : Integer;
 Ho,Mi,
 Se,Ms     : Integer;
 DT        : TDateTime;
 DTS       : TTimeStamp;
Begin
 Result := '';
 if S = '' Then Exit;
 Try
 Ho := 0;
 Mi := 0;
 Se := 0;
 Ms := 0;
 P := Pos('/',S);
 //********************************** Mesec
 M :=0;
 if P > 0 Then
    Begin
     M := StrToInt(Copy(S,1,P-1));
     System.Delete(S,1,P);
    End;
  //********************************** Den
  D := 0;
  P := Pos('/',S);
  if P > 0 Then
     Begin
      D := StrToInt(Copy(S,1,P-1));
      System.Delete(S,1,P);
     End;
  //********************************** Godina
  P := Pos(' ',S);
  if P=0 Then
     Begin
       Y := StrToInt(S);
       DT:=EncodeDate(Y,M,D);
       DTS:=DateTimeToTimeStamp(DT);
       S:=IntToStr(DTS.Date)+' '+IntToStr(DTS.Time);
     End
  Else
     Begin
       Y := StrToInt(Copy(S,1,P-1));
       System.Delete(S,1,P);

       //********************************** Chasove
       Ho :=0;
       P := Pos(':',S);
       if P > 0 Then
          Begin
           Ho := StrToInt(Copy(S,1,P-1));
           System.Delete(S,1,P);
           //************************************ 17.01.2002
           P := Pos(' AM', S);
           if P > 0 Then System.Delete(S, P, 3);
           P := Pos(' PM', S);
           if P > 0 Then
              Begin
               Ho := Ho + 12;
               System.Delete(S, P, 3);
              End;
           //************************************ 17.01.2002   
          End;
       //********************************** Minuti
       Mi := 0;
       P := Pos(':',S);
       if P > 0 Then
          Begin
           Mi := StrToInt(Copy(S,1,P-1));
           System.Delete(S,1,P);
          End;
       //********************************** Secundi
       Se :=0;
       if S <> '' Then Se := StrToInt(S);
       //********************************** MiliSecundi
       Ms := 0;
     End;

  //********************************** Encode All
  DT:=EncodeTime(Ho,Mi,Se,Ms);
  DTS:=DateTimeToTimeStamp(DT);
  S:=IntToStr(DTS.Time);
  DT:=EncodeDate(Y,M,D);
  DTS:=DateTimeToTimeStamp(DT);
  //********************************** Compose Result
  S:=IntToStr(DTS.Date)+' '+S;
  Except
   S:='';
  End;
  Result := S;
End;                           

Procedure TKADaoTable.OpenDaoRecordset;
Var
 X         : Integer;
 L         : Integer;
 S         : String;
 FldType   : Integer;
 FldAttr   : Integer;
 FldCount  : Integer;
 TabType   : Integer;
 LoType    : Integer;
 Options   : Integer;
 {$IFDEF DYNADAO}
 TempRS    : OleVariant;
 {$ELSE}
 TempRS    : Recordset;
 {$ENDIF}
 TabN      : String;
 TempSort  : String;
 NRP, Dir  : Integer;
Begin
        if Not Assigned(F_Database) Then
           Begin
             DatabaseError(E2018);
           End;
        if (TableName='') And
           (SQL.Count=0)  And
           (QueryDefName='')
        Then DatabaseError(E2019);

        if Not F_DatabaseAutoActivate Then
           Begin
             if F_Database.Connected=False Then DatabaseError(E2020);
           End
        Else
           Begin
             if F_Database.Connected=False Then F_Database.Connected:=True;
           End;

        if (F_TableType=dbOpenDynamic) And (F_Database.DatabaseType <> 'ODBC') Then DatabaseError(E2021);
        TabType:=F_TableType;
        LoType:=F_LockType;

        if (F_Database.ReadOnly) And (NOT F_ReadOnly) Then F_ReadOnly:=True;
        if F_TableType=dbOpenForwardOnly Then F_ReadOnly:=True;
        if F_TableType=dbOpenSnapshot Then F_ReadOnly:=True;

        Options:=0;
        if dbDenyWrite      in F_OpenOptions Then Options:=Options + DAOApi.dbDenyWrite;
        if dbDenyRead       in F_OpenOptions Then Options:=Options + DAOApi.dbDenyRead;
        if dbReadOnly       in F_OpenOptions Then Options:=Options + DAOApi.dbReadOnly;
        if dbAppendOnly     in F_OpenOptions Then Options:=Options + DAOApi.dbAppendOnly;
        if dbInconsistent   in F_OpenOptions Then Options:=Options + DAOApi.dbInconsistent;
        if dbConsistent     in F_OpenOptions Then Options:=Options + DAOApi.dbConsistent;
        if dbSQLPassThrough in F_OpenOptions Then Options:=Options + DAOApi.dbSQLPassThrough;
        if dbFailOnError    in F_OpenOptions Then Options:=Options + DAOApi.dbFailOnError;
        if dbForwardOnly    in F_OpenOptions Then Options:=Options + DAOApi.dbOpenForwardOnly;
        if dbSeeChanges     in F_OpenOptions Then Options:=Options + DAOApi.dbSeeChanges;
        if dbRunAsync       in F_OpenOptions Then Options:=Options + DAOApi.dbRunAsync;
        if dbExecDirect     in F_OpenOptions Then Options:=Options + DAOApi.dbExecDirect;

        DaoOpenOptions := Options;

        {$IFDEF DYNADAO}
           if (NOT VarIsEmpty(F_DetailRecordset)) And (NOT VarIsNull(F_DetailRecordset)) Then F_DetailRecordset.Close;
           VarClear(F_DetailRecordset);
        {$ELSE}
           if F_DetailRecordset <> NIL Then F_DetailRecordset.Close;
           F_DetailRecordset:=NIL;
        {$ENDIF}

        RecordsAffected:=0;
        TabN:=TableName;
        if F_SQL.Count > 0 Then
           Begin
             if (MasterSource <> NIL) Then RefreshQueryParams;
             TabN:=F_ComposeSQL(F_SQL);
           End;
        if F_QueryDefName <> '' Then                                                           
           Begin
             TabN:=F_QueryDefName;
             NRP:=0;
               For X:=0 To Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Count-1 do
                 Begin
                   Dir := F_Database.CoreDatabase.QueryDefs.Item[QueryDefName].Parameters[X].Direction;
                   if (Dir=dbParamInput) Or (Dir=dbParamInputOutput) Then
                       Begin
                        Try
                         if F_QueryDefParameters.Strings[NRP]='NULL' Then
                            Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Item[X].Value:=NULL
                         Else
                            Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Item[X].Value:=F_QueryDefParameters.Strings[NRP];
                         Inc(NRP);
                        Except
                         DatabaseError(E2022);
                        End;
                       End;
                 End;
           End;
        if (F_QueryDefName <> '') And (Database.CoreDatabase.QueryDefs.Item[TabN].Parameters.Count > 0) Then
            Begin
                F_QueryDefSQLText.Clear;
                if Assigned(F_Database) And (F_Database.Connected) Then
                   F_QueryDefSQLText.SetText(PChar(F_Database.GetQueryDefSQLText(TabN)));
                Database.CoreDatabase.QueryDefs.Item[TabN].MaxRecords:=F_QueryDefMaxRecords;
                if F_Database.QueryTimeout <> 60 Then
                F_Database.CoreDatabase.QueryDefs.Item[TabN].ODBCTimeout:=F_Database.QueryTimeout;
                F_DaoTable:=Database.CoreDatabase.QueryDefs.Item[TabN].OpenRecordset(TabType,Options,LoType);
                GetQueryDefReturnParams(TabN);
            End
        Else
            Begin
               F_QueryDefSQLText.Clear;
               if (F_QueryDefName <> '') Then
                   Begin
                     F_QueryDefSQLText.SetText(PChar(F_Database.GetQueryDefSQLText(TabN)));
                     Database.CoreDatabase.QueryDefs.Item[TabN].MaxRecords:=F_QueryDefMaxRecords;
                     if F_Database.QueryTimeout <> 60 Then
                     F_Database.CoreDatabase.QueryDefs.Item[TabN].ODBCTimeout:=F_Database.QueryTimeout;
                     F_DaoTable:=Database.CoreDatabase.QueryDefs.Item[TabN].OpenRecordset(TabType,Options,LoType);
                   End
               Else
                   Begin
                     F_DaoTable:=Database.CoreDatabase.OpenRecordset(TabN,TabType,Options,LoType);
                   End;
            End;
            
        F_Database.Idle;
        if NOT F_DaoTable.Updatable Then F_ReadOnly:=True;
        F_Bookmarkable := F_DaoTable.Bookmarkable;
        InternalClearBookmarks;
        F_Database.RefreshDefinitions;
        RecordsAffected:=F_Database.CoreDatabase.RecordsAffected;

        //******************************************************** Setting Index
        if F_IndexName <> '' Then
           Begin
             Try
               F_DaoTable.Index:=F_IndexName;
             Except
               //******** May raise exception when table is empty
             End;
           End;
        //**********************************************************************
        FldCount := F_DaoTable.Fields.Count;
        //******************************************* Default Values
        F_UpdatableFields.Clear;
        F_DefaultValues.Clear;
        //********************************* Fast Open without quering properties
        For X :=0 To FldCount-1 do
         Begin
           F_DefaultValues.Add('');
           F_UpdatableFields.Add(Pointer(True));
         End;
        //**********************************************************************
        if (NOT F_ReadOnly) And (F_UseDaoProperties) Then
         Begin
          F_UpdatableFields.Clear;
          F_DefaultValues.Clear;
          For X :=0 To FldCount-1 do
            Begin
             {$IFDEF DYNADAO}
             FldType := F_DaoTable.Fields.Item[X].Type;
             {$ELSE}
             FldType := F_DaoTable.Fields.Item[X].Type_;
             {$ENDIF}
             FldAttr := F_DaoTable.Fields.Item[X].Attributes;
             F_UpdatableFields.Add(Pointer(False));
             if (FldAttr And dbUpdatableField) > 0 Then
                Begin
                 if (FldAttr And dbAutoIncrField) = 0 Then
                    Begin
                      if (FldAttr And dbSystemField) = 0 Then
                          Begin
                           F_UpdatableFields.Items[X]:=Pointer(True);
                          End;
                    End;
                End;
             Try
                //**************************************************************
                S:='';
                if F_Database.EngineType=dbUseJet Then
                   S:=F_DaoTable.Fields.Item[X].DefaultValue;
                //**************************************************************
                if (FldType=dbText) or (FldType=dbMemo) Then
                   Begin
                     L := Length(S);
                     if (L > 1) And (S[1]='"') And (S[L]='"') Then
                        Begin
                          System.Delete(S,L,1);
                          System.Delete(S,1,1);
                        End;
                   End;
                if (FldType=dbDate) Then
                   Begin
                     L := Length(S);
                     if (L > 1) And (S[1]='#') And (S[L]='#') Then
                        Begin
                          System.Delete(S,L,1);
                          System.Delete(S,1,1);
                          S:=ProcessDTDefault(S);
                        End
                      Else
                        S := '';
                   End;
                F_DefaultValues.Add(S);
                if AnsiCompareText(F_DefaultValues.Strings[X],'Null')=0 Then F_DefaultValues.Strings[X] := '';
              Except
                F_DefaultValues.Add('');
              End;
            End;
         End;
        if F_SortedBy.Count > 0 Then
           Begin
             TempSort:='';
             For X:=0 To F_SortedBy.Count-1 Do
                Begin
                  TempSort:=TempSort+F_SortedBy.Strings[X];
                  if X < F_SortedBy.Count-1 Then TempSort:=TempSort+',';
                  TempSort:=TempSort+' ';
                End;
             DaoSortString:=TempSort;
             F_DaoTable.Sort:=TempSort;
             TempRS:=F_DaoTable;
             F_DaoTable:=TempRS.OpenRecordset(TabType,Options);
             TempRS.Close;
             {$IFDEF DYNADAO}
             VarClear(TempRS);
             {$ELSE}
             TempRS:=Nil;
             {$ENDIF}
           End;
        if F_Filtered Then
           Begin
             if Filter<>'' Then
                Begin
                 F_DaoTable.Filter:=Filter;
                 TempRS:=F_DaoTable;
                 F_DaoTable:=TempRS.OpenRecordset(TabType,Options);
                 TempRS.Close;
                 {$IFDEF DYNADAO}
                 VarClear(TempRS);
                 {$ELSE}
                 TempRS:=Nil;
                 {$ENDIF}
                End;
           End;
        if (MasterSource <> NIL) And (Assigned(F_MasterLink.DataSet)) Then
           Begin
             F_MDisabled := Not (F_MasterLink.Active);
             if (NOT F_MasterLink.DataSet.Active) And (F_MasterAutoActivate) Then
                Begin
                  Try
                    F_MasterLink.DataSet.Active := True;
                  Finally
                    F_MDisabled := Not (F_MasterLink.Active);
                  End;
                End;                                                            
           End
        Else
           Begin
             F_MDisabled := True;
           End;
        if (MasterSource <> NIL) And (Not(F_MDisabled)) And (MasterSource.Enabled) then
            Begin
              F_ProcessMasterFields(F_MasterFields);
              if (F_Master.Count > 0) Then
                  Begin
                   TabN:=BuildDetailSQL;
                   TabN:=InsertSQLString(TabN);
                   F_DaoTable.Filter:=TabN;
                   if (F_TableName <> '') Then
                      Begin
                        F_DaoTable.Close;
                        if DaoSortString <> '' Then
                           F_DaoTable:=F_Database.CoreDatabase.OpenRecordset('Select * From ['+F_TableName+'] Where '+TabN+' Order By '+DaoSortString+';',TabType,Options,F_LockType)
                        Else
                           F_DaoTable:=F_Database.CoreDatabase.OpenRecordset('Select * From ['+F_TableName+'] Where '+TabN+';',TabType,Options,F_LockType);
                      End
                   Else
                      Begin
                        F_DetailRecordset := F_DaoTable;
                        F_DaoTable        := F_DetailRecordset.OpenRecordset(TabType,Options);
                      End;
                  End;
            End;
        //********************************************************** COM Cashing
        VarArrayRedim(DaoFields,Integer(FldCount-1));
        For X := 0 To FldCount-1 do
            Begin
              DaoFields[X] := OleVariant(F_DaoTable.Fields[X]);
            End;
        //********************************************************** COM Cashing
        CoreRecordset := F_DaoTable;
        F_RefreshRC := True;
        F_OldRC:=-1;
        F_RecNo:=-1;
        F_LastRecord:=-1;
        F_Database.Idle; //****************************************** 12.03.2002
End;

Procedure TKADaoTable.ReOpenDaoRecordset;
Var
  TabN     : String;
  X        : Integer;
Begin
  InternalClearBookmarks;
  TabN:=BuildDetailSQL;
  if (F_TableName <> '')  Then
     Begin
       TabN:=InsertSQLString(TabN);
       if DaoSortString <> '' Then
          F_DaoTable:=F_Database.CoreDatabase.OpenRecordset('Select * From ['+F_TableName+'] Where '+TabN+' Order By '+DaoSortString+';',F_TableType,DaoOpenOptions,F_LockType)
       Else
          F_DaoTable:=F_Database.CoreDatabase.OpenRecordset('Select * From ['+F_TableName+'] Where '+TabN+';',F_TableType,DaoOpenOptions,F_LockType);
     End
  Else
     Begin
       OleVariant(F_DetailRecordset).Requery;
       F_DetailRecordset.Filter:=TabN;
       F_DaoTable:=F_DetailRecordset.OpenRecordset(EmptyParam,dbSeeChanges)
     End;
  CoreRecordset := F_DaoTable;
  //**************************************************************** COM Cashing
  VarArrayRedim(DaoFields,Integer(F_DaoTable.Fields.Count-1));
  For X := 0 To F_DaoTable.Fields.Count-1 do
      Begin
        DaoFields[X] := OleVariant(F_DaoTable.Fields[X]);
      End;
  //****************************************************************************
  F_RefreshRC := True;
  F_OldRC:=-1;
  F_RecNo:=-1;
  F_LastRecord:=-1;
  GetRecordCount;
  F_Database.Idle; //************************************************ 12.03.2002
End;

Procedure TKADaoTable.Loaded;
begin
  try
    inherited Loaded;
  except
    Application.HandleException(Self)
  end;
end;

Procedure TKADaoTable.Notification(AComponent: TComponent; Operation: TOperation);
Begin
 If (Operation = opRemove) And (AComponent = F_Database) Then F_Database := Nil;
 If (Operation = opRemove) And (AComponent = F_Encrypter) Then
     Begin
      if F_Active Then Close;
      F_HasEncoder := False;
      F_Encrypter  := Nil;
     End;
 Inherited Notification(AComponent, Operation);
End;

Function TKADaoTable.UnquoteString(S:String):String;
Var
 L: Integer;
Begin
 Result := S;
 L:=Length(Result);
 if L=0 Then Exit;
 if Result[1]='''' Then System.Delete(Result,1,1);
 L:=Length(Result);
 if L=0 Then Exit;
 if Result[L]='''' Then System.Delete(Result,L,1);
End;

Procedure TKADaoTable.InternalOpen;
Var
   X       : Integer;
   TempMD  : Boolean;
   FF      : TField;
   Prop    : Pointer;
Begin
        OpenDaoRecordset;
        if Self.Name='' Then Self.Name:='KADaoTable_'+IntToStr(Abs(GetTickCountEx));

        InInternalOpen:=True;
        InternalInitFieldDefs;
        InInternalOpen:=False;
        if DefaultFields then CreateFields;
        if F_CacheLookups Then
           Begin
            For X := 0 To FieldCount-1 do
                Begin
                  if Fields[X].FieldKind=fkLookup Then Fields[X].LookupCache:=True;
                End;
           End;
        For X := 0 To FieldCount-1 do
            Begin
              if Fields[X].DataType=ftIDispatch Then Fields[X].ReadOnly := True;
            End;
        BindFields(True);
        if F_UseDisplayLabels Then InternalSetDisplayLabels;
        if F_Bookmarkable Then BookmarkSize := MYBOOKMARKSIZE Else BookmarkSize := 0;
        F_StartMyInfo:=InternalCalcRecordSize;
        F_StartCalc:=F_StartMyInfo+SizeOf(TDaoInfo);
        F_BufferSize:=F_RecordSize+Sizeof(TDaoInfo)+CalcFieldsSize;
        //****************************************************************
        TempMD:=F_MDisabled;
        F_MDisabled:= True;
        F_FieldNames.Clear;
        F_SortFieldNames.Clear;
        F_MDFieldNames.Clear;
        For X:=0 to FieldDefs.Count-1 do
            Begin
             FF  :=FindField(FieldDefs.Items[X].Name);
             if (FF <> Nil) Then
                Begin
                  if Boolean(F_UpdatableFields.Items[X])=False Then
                     Begin
                       FF.ReadOnly:=True;
                     End;
                  if FF.DefaultExpression <> '' Then
                     Begin
                       F_DefaultValues.Strings[X]:=UnQuoteString(FF.DefaultExpression);
                     End;
                 If (NOT FF.IsBlob) Then
                    Begin
                      F_SortFieldNames.Add(FieldDefs.Items[X].Name);
                    End;
                 F_FieldNames.Add(FieldDefs.Items[X].Name);
                 F_FieldTypeNames.Add(GetBDEFieldTypeNames(FieldDefs.Items[X].DataType));
                 if (NOT (FF.DataType=ftBlob)) Then
                    Begin
                     F_MDFieldNames.Add(FieldDefs.Items[X].Name);
                    End;
                 if (FF.DataType=ftDateTime) Then
                     if (DefaultFields) And (FF.DisplayWidth < 20) Then FF.DisplayWidth:=20;
                 if (FF.DataType=ftMemo) or (FF.DataType=ftWideMemo) Then
                    Begin
                      if F_CacheMemos Then
                         Begin
                           if (DefaultFields) And  (FF.DisplayWidth < 30) Then FF.DisplayWidth:=30;
                           FF.OnGetText:=F_OnGetMemoText;
                         End;
                    End;
                  {$IFDEF D6UP}
                   if (FF.DataType=ftFloat) Then TFloatField(FF).Precision := 7;
                  {$ENDIF}
                  if (FF.DataType=ftBytes) And (FieldDefs.Items[X].Precision=GUID_ID) Then
                     Begin
                      FF.ValidChars := GUID_VALID_CHARS;
                      if F_ShowGUID Then
                         Begin
                          if (DefaultFields) And (FF.DisplayWidth < 38) Then  FF.DisplayWidth := 38;
                          FF.OnGetText    := F_OnGetGUIDText;
                          FF.OnSetText    := F_OnSetGUIDText;
                         End;
                     End;
                End;
            End;
        F_MDisabled:=TempMD;

        F_OldValue          := Nil;
        F_KeyBuffer         := AllocRecordBuffer;
        F_RangeStartBuffer  := AllocRecordBuffer;
        F_RangeEndBuffer    := AllocRecordBuffer;

        F_HasEncoder        := False;
        if Assigned(F_Encrypter) Then
           Begin
             Prop := GetPropInfo(F_Encrypter.ClassInfo, 'EncodedString');
             if Prop <> Nil Then
                Begin
                  F_EncodedString:=Prop;
                  Prop := GetPropInfo(F_Encrypter.ClassInfo, 'DecodedString');
                  If Prop <> Nil Then
                     Begin
                      F_DecodedString := Prop;
                      F_HasEncoder := True;
                     End;
                End;
           End;
        F_Active:=True;
        Try
          if F_TableType=dbOpenTable Then GetRecordCount; //*********** 5.1.2002
        Except
        End;
        InternalFirst;
        //****************************************************************
        F_Database.ActiveTableNames.AddObject(Self.Name,Self);
        if (F_Database.TrackTransactions) And (F_Database.GetTransactionCount > 0) Then
           Begin
             F_Database.AddRNToTransaction(Self.Name,1)
           End;
        F_Database.Idle; //****************************************** 12.03.2002
End;

Procedure TKADaoTable.CloseDaoRecordset;
Var
 X : Integer;
Begin                    
 //****************************************** Com Cashing
 For X :=0 To F_DaoTable.Fields.Count-1 do
     Begin
       DaoFields[X]:=NULL;
     End;          
 //******************************************
 Try                                          
   F_DaoTable.Close;
   {$IFDEF DYNADAO}
   VarClear(F_DaoTable);
   {$ELSE}
   F_DaoTable := Nil;
   {$ENDIF}
 Except
 End;  
End;

Procedure TKADaoTable.InternalClose;
Var
  I : Integer;
Begin
        if Not F_Active Then Exit;
        Try
         if State=dsEdit Then OleVariant(F_DaoTable).CancelUpdate;
        Except
        End;
        //************************************************** Changed 16.11.2000
        F_Active:=False;
        //************************************************** Changed 16.11.2000
        DaoOpenOptions := 0;
        DaoSortString  := '';
        BindFields(False);
        if DefaultFields then DestroyFields;
        CloseDaoRecordset;
        {$IFDEF DYNADAO}
        if (NOT VarIsEmpty(F_DetailRecordset)) And (NOT VarIsNull(F_DetailRecordset)) Then F_DetailRecordset.Close;
        VarClear(F_DetailRecordset);
        {$ELSE}
        if F_DetailRecordset <> Nil Then F_DetailRecordset.Close;
        F_DetailRecordset := NIL;
        {$ENDIF}
        if Assigned(F_Database) And (Not MainDatabaseShutdown) Then
           Begin
              I := F_Database.ActiveTableNames.IndexOfObject(Self);
              if I <> -1 Then  F_Database.ActiveTableNames.Delete(I);
           End
        Else
           MainDatabaseShutdown  := False;
        if F_OldValue <> Nil then FreeRecordBuffer(F_OldValue);
        FreeRecordBuffer(F_KeyBuffer);
        FreeRecordBuffer(F_RangeStartBuffer);
        FreeRecordBuffer(F_RangeEndBuffer);
End;

//*********************************************************** BOOKMARK Functions
Procedure TKADaoTable.InternalClearBookmarks;
Begin
  F_BookmarkRN.Clear;
  F_BookmarkID.Clear;
End;

Procedure TKADaoTable.InternalGotoBookmark(Bookmark: Pointer);
Var
  I       : Integer;
  X       : Integer;
  BK      : OleVariant;
  P       : PChar;
  PB      : PChar;
  Invalid : Boolean;
Begin
  Invalid := False;
  if NOT F_Active Then DatabaseError(E2023);
  if Bookmark=Nil Then DatabaseError(E2024);
  //***************************************************** 30.04.2001
  if IsEmpty Then Exit;
  //***************************************************** 30.04.2001
  X:=PInteger(Bookmark)^;
  if (F_Bookmarkable) And (X <> 0) Then
     Begin
       Try
        I:= F_BookmarkID.IndexOf(Pointer(X));
        if I = -1 Then
           Begin
             Invalid := True;
             DatabaseError(E2024);
           End
        Else
           Begin
             BK  := VarArrayCreate([0, 3],varByte);
             P   := PChar(Bookmark);
             PB  := VarArrayLock(BK);
             For X := 0 to 3 do PB[X] := P[X];
             VarArrayUnLock(BK);
             OleVariant(F_DaoTable).Bookmark:=VarAsType(BK, varArray or VarByte);
             F_RecNo:=Integer(F_BookmarkRN.Items[I]);
             VarClear(BK);
           End;
       Except
        if Invalid Then Raise;
        if GetLastDaoError.ErrNo=3167 Then
           Begin
             DaoInternalRefresh;
             InternalFirst;
             Raise;
           End;
       End; 
     End
  Else
     Begin
       DatabaseError(E2025);
     End;
End;

Function TKADaoTable.BookmarkValid(Bookmark: TBookmark): Boolean;
Var
  TmpBookmark : TBookmark;
  BK          : Integer;
Begin
  Result := False;
  //***************************************************** 30.04.2001
  if IsEmpty Then Exit;
  //***************************************************** 30.04.2001
  If (F_Active) And (F_Bookmarkable) And (Assigned(Bookmark)) then
  Begin
   //**************************************************** 26.01.2002
   BK := PInteger(Bookmark)^;
   if (BK <> 0) Then
   Begin
      if F_BookmarkID.IndexOf(Pointer(BK)) = -1 Then Exit;
   End;
   //**************************************************** 26.01.2002
   TmpBookmark:=GetBookmark;
   Try
    InternalGotoBookmark(Bookmark);
    CursorPosChanged;
    Result := True;
   Except
    if Assigned(TmpBookmark) Then
       Begin
        InternalGotoBookmark(TmpBookmark);
        CursorPosChanged;
       End;
   End;
   FreeBookmark(TmpBookMark);
  End;
End;



Function TKADaoTable.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
Begin
  Result:=PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkFlag;
End;

Procedure TKADaoTable.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
Begin
  PDaoInfo(Buffer + F_StartMyInfo)^.BookmarkFlag := Value;
  if (F_BatchMode) And (ControlsDisabled) Then
     Begin
       if (Value=bfEOF) or (Value=bfInserted) Then F_InPost := True;
     End;
End;

Function TKADaoTable.GetBookmarkStr: TBookmarkStr;
Var
 Buffer : TRecordBuffer;
 RN     : Integer;
 I      : Integer;
 BKS    : Integer;
 PIN    : PInteger;
Begin
  BKS :=  0;
  PIN := @BKS;
  SetString(Result,PChar(PIN),BookmarkSize);
  Try
    if F_Bookmarkable Then
       Begin
         Buffer:=GetActiveRecordBuffer;
         if (Buffer <> Nil) Then
             Begin
               PIN:=@PDaoInfo(Buffer + F_StartMyInfo)^.BookmarkData;
               SetString(Result,PChar(PIN),BookmarkSize);
               RN     := PDaoInfo(Buffer + F_StartMyInfo)^.RecordNo;
               I      := F_BookmarkRN.IndexOf(Pointer(RN));
               if I=-1 Then
                  Begin
                   F_BookmarkRN.Add(Pointer(RN));
                   F_BookmarkID.Add(Pointer(PDaoInfo(Buffer + F_StartMyInfo)^.BookmarkData));
                  End
               Else
                  Begin
                   F_BookmarkID.Items[I]:=Pointer(PDaoInfo(Buffer + F_StartMyInfo)^.BookmarkData);
                  End;
             End;
       End;
  Except
    BKS :=  0;
    PIN := @BKS;
    SetString(Result,PChar(PIN),BookmarkSize);
  End;
End;
                                     
Procedure TKADaoTable.SetBookmarkStr(const Value: TBookmarkStr);
Var
 PBI : PInteger;
Begin
 //***************************************************** 30.04.2001
 if IsEmpty Then Exit;
 //***************************************************** 30.04.2001
 if (F_Bookmarkable) And (Value <> '') Then
     Begin
      PBI:=PInteger(PChar(Value));
      InternalGotoBookmark(PBI);
      //***************************************************** 14.09.2002
      Resync([rmExact,rmCenter]);  //Resync([]);
      //***************************************************** 14.09.2002
     End;
End;

Procedure TKADaoTable.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
Var
  I  : Integer;
  RN : Integer;
Begin
  if (F_Bookmarkable) And (Buffer <> Nil) And (Data <> Nil) Then
    Begin
     PInteger(Data)^ := PDaoInfo(Buffer + F_StartMyInfo)^.BookmarkData;
     RN              := PDaoInfo(Buffer + F_StartMyInfo)^.RecordNo;
     I               := F_BookmarkRN.IndexOf(Pointer(RN));
     if I=-1 Then
        Begin
          F_BookmarkRN.Add(Pointer(RN));
          F_BookmarkID.Add(Pointer(PDaoInfo(Buffer + F_StartMyInfo)^.BookmarkData));
        End
      Else
        Begin
          F_BookmarkID.Items[I]:=Pointer(PDaoInfo(Buffer + F_StartMyInfo)^.BookmarkData);
        End;
    End
  Else
    Begin
     if Data <> Nil Then PInteger(Data)^ := 0;
    End;
End;

Procedure TKADaoTable.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
Begin
  if (Buffer <> Nil) And (Data <> Nil) Then
      Begin
        PDaoInfo(Buffer + F_StartMyInfo)^.BookmarkData := PInteger(Data)^;
      End;
End;

Function TKADaoTable.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
Const
  ResultCodes     : array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
Begin
  Result := ResultCodes[Bookmark1 = nil, Bookmark2 = nil];
  If Result = 2 then
     Begin
       Result := 0;
       if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ Then Result := -1;
       if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ Then Result :=  1;
     End;
End;
//*********************************************************** BOOKMARK Functions

Function TKADaoTable.GetRawFieldData(FieldName : String):OleVariant;
Var
  Buffer : TRecordBuffer;
  FF     : TField;
Begin
  Result := NULL;
  if (F_Active) And (F_Bookmarkable) Then
     Begin
      FF := FindField(FieldName);
      if FF=Nil Then Exit;
      Buffer:=GetActiveRecordBuffer;
      if (Buffer = Nil) Then Exit;
      InternalSetToRecord(Buffer);
      Result := F_DaoTable.Fields.Item[FieldName].Value;
     End;
End;

Function TKADaoTable.SetRawFieldData(FieldName : String; Value : OleVariant):Boolean;
Var
  Buffer : TRecordBuffer;
  FF     : TField;
Begin
  Result := False;
  if (F_Active) And (F_Bookmarkable) Then
     Begin
      FF := FindField(FieldName);
      if FF=Nil Then Exit;
      Buffer:=GetActiveRecordBuffer;
      if (Buffer = Nil) Then Exit;
      InternalSetToRecord(Buffer);
      Try
        F_DaoTable.Edit;
      Except
        if F_DaoTable.EditMode <> DaoApi.dbEditInProgress Then OleVariant(F_DaoTable).Move(0);
        DaoInternalRefresh;
      End;
      F_DaoTable.Fields.Item[FieldName].Value:=Value;
      OleVariant(F_DaoTable).Update;
      Result := True;
     End;
End;


Function TKADaoTable.GetRows(NumRows:Integer):OleVariant;
Var
  Buffer : TRecordBuffer;
Begin
  Result:=NULL;
  if (F_Active) And (F_Bookmarkable) Then
     Begin
      if (F_Filtered) And (Assigned(F_OnFilterRecord)) Then DatabaseError(E2063);
      Buffer:=GetActiveRecordBuffer;
      if (Buffer = Nil) Then Exit;
      InternalSetToRecord(Buffer);
      Result:=F_DaoTable.GetRows(NumRows);
      if F_RecNo < RecordCount Then Inc(F_RecNo,NumRows);
      Try
        Resync([]);
      Except
        InternalFirst;
        Resync([]);
        Raise;
      End;
     End;
End;

Function  TKADaoTable.CopyQueryDef : OleVariant;
Begin
  Result := OleVariant(F_DaoTable).CopyQueryDef;
End;

Function  TKADaoTable.CopyQueryDefText : String;
Var
  QD : OleVariant;
Begin
  QD := OleVariant(F_DaoTable).CopyQueryDef;
  Result := QD.SQL;
End;

Procedure TKADaoTable.AccessExportToTXT(FileName:String; IncludeBlobs, DeleteOld:Boolean);
Var
 SQL : TStringList;
 FN  : String;
 FP  : String;
 L   : Integer;
 SS  : String;
 X   : Integer;
 FC  : Integer;
 TN  : String;
Begin
 If Not F_Active Then DatabaseError('Table must be open in order to export data!');
 TN := '';
 //*****************************************************************************
 Try
 if (DeleteOld) And FileExists(FileName) Then DeleteFile(FileName);
 if F_TableName='' Then
    Begin
      if F_QueryDefName='' Then
         Begin
           TN := 'Query'+IntToStr(Integer(Abs(GetTickCountEx)));
           Try
             F_Database.CoreDatabase.BeginTrans;
           Except
           End;
           F_Database.CreateQueryDef(TN,F_ComposeSQL(F_SQL));
         End
      Else
         Begin
          TN := F_QueryDefName;
         End;
    End
 Else
    Begin
      TN := F_TableName;
    End;
 FN := ExtractFileName(FileName);
 FP := ExtractFilePath(FileName);
 if FP='' Then FP:='.';
 L  := Length(FN);
 If L=0 Then DatabaseError('FileName is empty!');
 For X :=1 To L do If FN[X]='.' Then FN[X]:='#';
 SS:='';
 FC := FieldCount-1;
 For X := 0 To FC do
     Begin
      if StoreField(X) Then
       Begin
        if IncludeBlobs Then
          Begin
            if F_UseBrackets Then
               Begin
                 SS := SS+' '+BracketField(Fields[X].FieldName);
                 if X < FC Then SS := SS+',';
               End
            Else
               Begin
                 if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
               End;
          End
        Else
          Begin
           if Fields[X].DataType<>ftBlob Then
              Begin
               if F_UseBrackets Then
                  Begin
                    SS := SS+' '+BracketField(Fields[X].FieldName);
                    if X < FC Then SS := SS+',';
                  End
               Else
                  Begin
                    if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
                  End;
              End;
          End;
       End;
     End; 
 if SS[Length(SS)]=',' Then System.Delete(SS,Length(SS),1);
 if F_ExportMethod = AllFields Then SS :='*';
 SQL := TStringList.Create;
 Try
   SQL.Add(Format('Select %s INTO [%s] IN "%s"[Text;] FROM [%s]',[SS,FN,FP,TN]));
   if (F_Filtered) And (F_Filter <> '') Then SQL.Add(' WHERE '+F_Filter);
   SQL.Add(';');
   ExecSQL(SQL);
 Finally
   SQL.Free;
 End;
 Finally
   if (F_TableName='') And (F_QueryDefName='') And (TN <> '') Then
       Begin
         F_Database.DeleteQueryDef(TN);
         Try
           F_Database.CoreDatabase.Rollback;
         Except
         End;
       End;
 End;
 //*****************************************************************************
End;

Procedure TKADaoTable.AccessExportToHTML(FileName:String; IncludeBlobs, DeleteOld:Boolean);
Var
 SQL : TStringList;
 FN  : String;
 FP  : String;
 SS  : String;
 X   : Integer;
 FC  : Integer;
 TN  : String;
Begin
 If Not F_Active Then DatabaseError('Table must be open in order to export data!');
 TN := '';
 //*****************************************************************************
 Try
 if (DeleteOld) And FileExists(FileName) Then DeleteFile(FileName);
 if F_TableName='' Then
    Begin
      if F_QueryDefName='' Then
         Begin
           TN := 'Query'+IntToStr(Integer(Abs(GetTickCountEx)));
           Try
             F_Database.CoreDatabase.BeginTrans;
           Except
           End;
           F_Database.CreateQueryDef(TN,F_ComposeSQL(F_SQL));
         End
      Else
         Begin
          TN := F_QueryDefName;
         End;
    End
 Else
    Begin
      TN := F_TableName;
    End;
 FN := ExtractFileName(FileName);
 FP := ExtractFilePath(FileName);
 if FP='' Then FP:='.';
 SS:='';
 FC := FieldCount-1;
 For X := 0 To FC do
     Begin
      if StoreField(X) Then
       Begin
        if IncludeBlobs Then
          Begin
            if F_UseBrackets Then
               Begin
                 SS := SS+' '+BracketField(Fields[X].FieldName);
                 if X < FC Then SS := SS+',';
               End
            Else
               Begin
                 if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
               End;
          End
        Else
          Begin
           if Fields[X].DataType<>ftBlob Then
              Begin
               if F_UseBrackets Then
                  Begin
                    SS := SS+' '+BracketField(Fields[X].FieldName);
                    if X < FC Then SS := SS+',';
                  End
               Else
                  Begin
                    if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
                  End;
              End;
          End;
       End;
     End;
 if SS[Length(SS)]=',' Then System.Delete(SS,Length(SS),1);
 if F_ExportMethod = AllFields Then SS :='*';
 SQL := TStringList.Create;
 Try
   SQL.Add(Format('Select %s INTO [%s] IN "%s"[HTML Export;] FROM [%s]',[SS,FN,FP,TN]));
   if (F_Filtered) And (F_Filter <> '') Then SQL.Add(' WHERE '+F_Filter);
   SQL.Add(';');
   ExecSQL(SQL);
 Finally
   SQL.Free;
 End;
 Finally
   if (F_TableName='') And (F_QueryDefName='') And (TN <> '') Then
       Begin
         F_Database.DeleteQueryDef(TN);
         Try
           F_Database.CoreDatabase.Rollback;
         Except
         End;
       End;
 End;
 //*****************************************************************************
End;

//*****************************************************************************
// Please use ExcelVersion=9 to export to Excel 12.0 Xml file format
//*****************************************************************************
Procedure TKADaoTable.AccessExportToExcel(FileName, SheetName :String; ExcelVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
Var
 SQL : TStringList;
 EV  : String;
 SS  : String;
 X   : Integer;
 FC  : Integer;
 TN  : String;
Begin
 If Not F_Active Then DatabaseError('Table must be open in order to export data!');
 TN :='';
 //*****************************************************************************
 Try
 if (DeleteOld) And FileExists(FileName) Then DeleteFile(FileName);
 if F_TableName='' Then
    Begin
      if F_QueryDefName='' Then
         Begin
           TN := 'Query'+IntToStr(Integer(Abs(GetTickCountEx)));
           Try
             F_Database.CoreDatabase.BeginTrans;
           Except
           End;
           F_Database.CreateQueryDef(TN,F_ComposeSQL(F_SQL));
         End
      Else
         Begin
          TN := F_QueryDefName;
         End;
    End
 Else
    Begin
      TN := F_TableName;
    End;
 EV := 'Excel 8.0;';
 Case ExcelVersion of
      3  : EV := 'Excel 3.0;';
      4  : EV := 'Excel 4.0;';
      5  : EV := 'Excel 5.0;';
      8  : EV := 'Excel 8.0;';
      9  : EV := 'Excel 12.0 Xml;';
      12 : EV := 'Excel 12.0;';
 End;
 SS:='';
 FC := FieldCount-1;
 For X := 0 To FC do
     Begin
      if StoreField(X) Then
       Begin
        if IncludeBlobs Then
          Begin
            if F_UseBrackets Then
               Begin
                 SS := SS+' '+BracketField(Fields[X].FieldName);
                 if X < FC Then SS := SS+',';
               End
            Else
               Begin
                 if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
               End;
          End
        Else
          Begin
           if Fields[X].DataType<>ftBlob Then
              Begin
               if F_UseBrackets Then
                  Begin
                    SS := SS+' '+BracketField(Fields[X].FieldName);
                    if X < FC Then SS := SS+',';
                  End
               Else
                  Begin
                    if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
                  End;
              End;
          End;
       End;
     End;
 if SS[Length(SS)]=',' Then System.Delete(SS,Length(SS),1);
 if F_ExportMethod = AllFields Then SS :='*';
 SQL := TStringList.Create;
 Try
   SQL.Add(Format('Select %s INTO [%s] IN "%s"[%s] FROM [%s]',[SS,SheetName,FileName,EV,TN]));
   if (F_Filtered) And (F_Filter <> '') Then SQL.Add(' WHERE '+F_Filter);
   SQL.Add(';');
   ExecSQL(SQL);
 Finally
   SQL.Free;
 End;
 Finally
   if (F_TableName='') And (F_QueryDefName='') And (TN <> '') Then
       Begin
         F_Database.DeleteQueryDef(TN);
         Try
           F_Database.CoreDatabase.Rollback;
         Except
         End;
       End;
 End;
 //*****************************************************************************
End;

Procedure TKADaoTable.AccessExportToParadox(FileName:String; ParadoxVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
Var
 SQL          : TStringList;
 FN           : String;
 FP           : String;
 SS           : String;
 X            : Integer;
 FC           : Integer;
 PV           : String;
 P            : PChar;
 TN           : String;
 IncludeMemos : Boolean;
Begin
 If Not F_Active Then DatabaseError('Table must be open in order to export data!');
 TN := '';
 //*****************************************************************************
 Try
 if F_TableName='' Then
    Begin
      if F_QueryDefName='' Then
         Begin
           TN := 'Query'+IntToStr(Integer(Abs(GetTickCountEx)));
           Try
             F_Database.CoreDatabase.BeginTrans;
           Except
           End;
           F_Database.CreateQueryDef(TN,F_ComposeSQL(F_SQL));
         End
      Else
         Begin
          TN := F_QueryDefName;
         End;
    End
 Else
    Begin
      TN := F_TableName;
    End;
 FN := ExtractFileName(FileName);
 FP := ExtractFilePath(FileName);
 if (DeleteOld) Then
    Begin
     P:=StrRScan(PChar(FN),'.');
     if P <> Nil Then P[0] := #0;
     FN := StrPas(PChar(FN));
     DeleteFile(FP+FN+'.db');
     DeleteFile(FP+FN+'.mb');
     DeleteFile(FP+FN+'.px');
     DeleteFile(FP+FN+'.val');
    End;
 FN := ExtractFileName(FileName);
 if FP='' Then FP:='.';
 SS           := '';
 FC           := FieldCount-1;
 //****************************************************************** 28.02.2003
 IncludeMemos := True;
 if  (F_Database.Version='3.5')
 And (ParadoxVersion=3) Then IncludeMemos := False;
 //****************************************************************** 28.02.2003
 For X := 0 To FC do
     Begin
      if StoreField(X) Then
       Begin
        if IncludeBlobs Then
          Begin
            if F_UseBrackets Then
               Begin
                 SS := SS+' '+BracketField(Fields[X].FieldName);
                 if X < FC Then SS := SS+',';
               End
            Else
               Begin
                 if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
               End;
          End
        Else
          Begin
           //******************************************************** 28.02.2003
           if (Fields[X].DataType=ftMemo) or (Fields[X].DataType=ftWideMemo) Then
              Begin
                if IncludeMemos Then
                   Begin
                     if F_UseBrackets Then
                        Begin
                         SS := SS+' '+BracketField(Fields[X].FieldName);
                         if X < FC Then SS := SS+',';
                        End
                     Else
                         Begin
                           if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
                         End;
                   End;
              End
           Else
           //******************************************************** 28.02.2003
           if Fields[X].DataType<>ftBlob Then
              Begin
               if F_UseBrackets Then
                  Begin
                    SS := SS+' '+BracketField(Fields[X].FieldName);
                    if X < FC Then SS := SS+',';
                  End
               Else
                  Begin
                    if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
                  End;
              End;
          End;
       End;
     End;
 Case ParadoxVersion of
      3 : PV := 'Paradox 3.X;';
      4 : PV := 'Paradox 4.X;';
      5 : PV := 'Paradox 5.X;';
      7 : PV := 'Paradox 7.X;';
 End;
 if SS[Length(SS)]=',' Then System.Delete(SS,Length(SS),1);
 if F_ExportMethod = AllFields Then SS :='*';
 SQL := TStringList.Create;
 Try
   SQL.Add(Format('Select %s INTO [%s] IN "%s"[%s] FROM [%s]',[SS,FN,FP,PV,TN]));
   if (F_Filtered) And (F_Filter <> '') Then SQL.Add(' WHERE '+F_Filter);
   SQL.Add(';');
   ExecSQL(SQL);
 Finally
   SQL.Free;
 End;
 Finally
   if (F_TableName='') And (F_QueryDefName='') And (TN <> '') Then
       Begin
         F_Database.DeleteQueryDef(TN);
         Try
           F_Database.CoreDatabase.Rollback;
         Except
         End;
       End;
 End;
 //*****************************************************************************
End;

Procedure TKADaoTable.AccessExportToDBase(FileName:String; DBaseVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
Var
 SQL : TStringList;
 FN  : String;
 FP  : String;
 SS  : String;
 X   : Integer;
 FC  : Integer;
 DV  : String;
 P   : PChar;
 TN  : String;
Begin
 If Not F_Active Then DatabaseError('Table must be open in order to export data!');
 TN := '';
 //*****************************************************************************
 Try
 if F_TableName='' Then
    Begin
      if F_QueryDefName='' Then
         Begin
           TN := 'Query'+IntToStr(Integer(Abs(GetTickCountEx)));
           Try
             F_Database.CoreDatabase.BeginTrans;
           Except
           End;
           F_Database.CreateQueryDef(TN,F_ComposeSQL(F_SQL));
         End
      Else
         Begin
          TN := F_QueryDefName;
         End;
    End
 Else
    Begin
      TN := F_TableName;
    End;
 FN := ExtractFileName(FileName);
 FP := ExtractFilePath(FileName);
 if (DeleteOld) Then
    Begin
     P:=StrRScan(PChar(FN),'.');
     if P <> Nil Then P[0] := #0;
     FN := StrPas(PChar(FN));
     DeleteFile(FP+FN+'.dbf');
     DeleteFile(FP+FN+'.dbt');
     DeleteFile(FP+FN+'.ndx');
     DeleteFile(FP+FN+'.ntx');
     DeleteFile(FP+FN+'.mdx');
    End;
 FN := ExtractFileName(FileName);
 if FP='' Then FP:='.';
 SS:='';
 FC := FieldCount-1;
 For X := 0 To FC do
     Begin
      if StoreField(X) Then
       Begin
        if IncludeBlobs Then
          Begin
            if F_UseBrackets Then
               Begin
                 SS := SS+' '+BracketField(Fields[X].FieldName);
                 if X < FC Then SS := SS+',';
               End
            Else
               Begin
                 if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
               End;
          End
        Else
          Begin
           if Fields[X].DataType<>ftBlob Then
              Begin
               if F_UseBrackets Then
                  Begin
                    SS := SS+' '+BracketField(Fields[X].FieldName);
                    if X < FC Then SS := SS+',';
                  End
               Else
                  Begin
                    if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
                  End;
              End;
          End;
       End;
     End;
 Case DBaseVersion of
      3 : DV := 'dBase III;';
      4 : DV := 'dBase IV;';
      5 : DV := 'dBase 5.0;';
 End;
 if SS[Length(SS)]=',' Then System.Delete(SS,Length(SS),1);
 if F_ExportMethod = AllFields Then SS :='*';
 SQL := TStringList.Create;
 Try
   SQL.Add(Format('Select %s INTO [%s] IN "%s"[%s] FROM [%s]',[SS,FN,FP,DV,TN]));
   if (F_Filtered) And (F_Filter <> '') Then SQL.Add(' WHERE '+F_Filter);
   SQL.Add(';');
   ExecSQL(SQL);
 Finally
   SQL.Free;
 End;
 Finally
   if (F_TableName='') And (F_QueryDefName='') And (TN <> '') Then
       Begin
         F_Database.DeleteQueryDef(TN);
         Try
           F_Database.CoreDatabase.Rollback;
         Except
         End;
       End;
 End;
 //*****************************************************************************
End;


Procedure TKADaoTable.AccessExportToFoxPro(FileName:String; FoxProVersion:Integer; IncludeBlobs, DeleteOld:Boolean);
Var
 SQL : TStringList;
 FN  : String;
 FP  : String;
 SS  : String;
 X   : Integer;
 FC  : Integer;
 DV  : String;
 P   : PChar;
 TN  : String;
Begin
 If Not F_Active Then DatabaseError('Table must be open in order to export data!');
 TN := '';
 //*****************************************************************************
 Try
 if F_TableName='' Then
    Begin
      if F_QueryDefName='' Then
         Begin
           TN := 'Query'+IntToStr(Integer(Abs(GetTickCountEx)));
           Try
             F_Database.CoreDatabase.BeginTrans;
           Except
           End;
           F_Database.CreateQueryDef(TN,F_ComposeSQL(F_SQL));
         End
      Else
         Begin
          TN := F_QueryDefName;
         End;
    End
 Else
    Begin
      TN := F_TableName;
    End;
 FN := ExtractFileName(FileName);
 FP := ExtractFilePath(FileName);
 if (DeleteOld) Then
    Begin
     P:=StrRScan(PChar(FN),'.');
     if P <> Nil Then P[0] := #0;
     FN := StrPas(PChar(FN));
     DeleteFile(FP+FN+'.dbf');
     DeleteFile(FP+FN+'.fpt');
     DeleteFile(FP+FN+'.cdx');
    End;
 FN := ExtractFileName(FileName);
 if FP='' Then FP:='.';
 SS:='';
 FC := FieldCount-1;
 For X := 0 To FC do
     Begin
      if StoreField(X) Then
       Begin
        if IncludeBlobs Then
          Begin
            if F_UseBrackets Then
               Begin
                 SS := SS+' '+BracketField(Fields[X].FieldName);
                 if X < FC Then SS := SS+',';
               End
            Else
               Begin
                 if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
               End;
          End
        Else
          Begin
           if Fields[X].DataType<>ftBlob Then
              Begin
               if F_UseBrackets Then
                  Begin
                    SS := SS+' '+BracketField(Fields[X].FieldName);
                    if X < FC Then SS := SS+',';
                  End
               Else
                  Begin
                    if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
                  End;
              End;
          End;
       End;
     End;
 Case FoxProVersion of
      20 : DV := 'FoxPro 2.0;';
      25 : DV := 'FoxPro 2.5;';
      26 : DV := 'FoxPro 2.6;';
      30 : DV := 'FoxPro 3.0;';
 End;
 if SS[Length(SS)]=',' Then System.Delete(SS,Length(SS),1);
 if F_ExportMethod = AllFields Then SS :='*';
 SQL := TStringList.Create;
 Try
   SQL.Add(Format('Select %s INTO [%s] IN "%s"[%s] FROM [%s]',[SS,FN,FP,DV,TN]));
   if (F_Filtered) And (F_Filter <> '') Then SQL.Add(' WHERE '+F_Filter);
   SQL.Add(';');
   ExecSQL(SQL);
 Finally
   SQL.Free;
 End;
 Finally
   if (F_TableName='') And (F_QueryDefName='') And (TN <> '') Then
       Begin
         F_Database.DeleteQueryDef(TN);
         Try
           F_Database.CoreDatabase.Rollback;
         Except
         End;
       End;
 End;
 //*****************************************************************************
End;

Procedure TKADaoTable.AccessExportToMDB(FileName,NewTableName:String; IncludeBlobs, DeleteOld:Boolean);
Var
 SQL : TStringList;
 SS  : String;
 X   : Integer;
 FC  : Integer;
 TN  : String;
Begin
 If Not F_Active Then DatabaseError('Table must be open in order to export data!');
 TN := '';
 //*****************************************************************************
 Try
 if (DeleteOld) Then
     Begin
       SQL := TStringList.Create;
       Try
        SQL.Add('DROP TABLE ['+FileName+'].['+NewTableName+'];');
        ExecSQL(SQL);
       Except
       End;
       SQL.Free;
     End;
 if F_TableName='' Then
    Begin
      if F_QueryDefName='' Then
         Begin
           TN := 'Query'+IntToStr(Integer(Abs(GetTickCountEx)));
           F_Database.CreateQueryDef(TN,F_ComposeSQL(F_SQL));
         End
      Else
         Begin
          TN := F_QueryDefName;
         End;
    End
 Else
    Begin
      TN := F_TableName;
    End;
 SS:='';
 FC := FieldCount-1;
 For X := 0 To FC do
     Begin
      if StoreField(X) Then
       Begin
        if IncludeBlobs Then
          Begin
            if F_UseBrackets Then
               Begin
                 SS := SS+' '+BracketField(Fields[X].FieldName);
                 if X < FC Then SS := SS+',';
               End
            Else
               Begin
                 if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
               End;
          End
        Else
          Begin
           if Fields[X].DataType<>ftBlob Then
              Begin
               if F_UseBrackets Then
                  Begin
                    SS := SS+' '+BracketField(Fields[X].FieldName);
                    if X < FC Then SS := SS+',';
                  End
               Else
                  Begin
                    if X < FC Then SS := SS+' '+Fields[X].FieldName+',' Else SS := SS+' '+Fields[X].FieldName;
                  End;
              End;
          End;
       End;
     End;
 if SS[Length(SS)]=',' Then System.Delete(SS,Length(SS),1);
 if F_ExportMethod = AllFields Then SS :='*';
 SQL := TStringList.Create;
 Try
   SQL.Add(Format('Select %s INTO [%s].[%s] FROM [%s]',[SS,FileName,NewTableName,TN]));
   if (F_Filtered) And (F_Filter <> '') Then SQL.Add(' WHERE '+F_Filter);
   SQL.Add(';');
   ExecSQL(SQL);
 Finally
   SQL.Free;
 End;
 Finally
   if (F_TableName='') And (F_QueryDefName='') And (TN <> '') Then F_Database.DeleteQueryDef(TN);
 End;
 //*****************************************************************************
End;

{$IFDEF D4UP}
Procedure TKADaoTable.InternalInitIndexDefs;
Var
  ID         : OleVariant;
  IndexCount : Integer;
  FieldCount : Integer;
  X          : Integer;
  Y          : Integer;
  IName      : String;
  IFields    : String;
  IPrimary   : Boolean;
  IUnique    : Boolean;
  IDesc      : Boolean;
  IDescFields: String;
  Options    : TIndexOptions;
Begin
 F_IndexDefs.Clear;
 if F_TableName='' Then Exit;
 if F_TableType <> DaoApi.dbOpenTable Then Exit;
 if F_Database.EngineType=DaoApi.dbUseODBC Then Exit;
 ID := F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes;
 //****************************************************************** 13.02.2002
 Try
   ID.Refresh;
 Except
 End;
 //****************************************************************** 13.02.2002
 IndexCount := ID.Count;
 For X := 0 To IndexCount-1 do
     Begin
       IName       := '';
       IPrimary    := False;
       IUnique     := False;
       Try
        IName       := ID.Item[X].Name;
        IPrimary    := ID.Item[X].Primary;
        IUnique     := ID.Item[X].Unique;
       Except
       End;
       IDesc       := True;
       IFields     := '';
       IDescFields := '';
       FieldCount  := ID.Item[X].Fields.Count;
       For Y := 0 To FieldCount-1 Do
           Begin
             if (ID.Item[X].Fields.Item[Y].Attributes and DaoApi.dbDescending) = 0 Then
                Begin
                 IFields := IFields+ID.Item[X].Fields.Item[Y].Name+';';
                 IDesc   := False;
                End
             Else
                Begin
                  IDescFields := IDescFields+ID.Item[X].Fields.Item[Y].Name+';';
                End;
           End;
       Options  := [];
       if IPrimary Then Options := Options+[ixPrimary];
       if IUnique  Then Options := Options+[ixUnique];
       if IDesc    Then Options := Options+[ixDescending];
       if Length(IDescFields) > 0 Then System.Delete(IDescFields,Length(IDescFields),1);
       if Length(IFields)     > 0 Then System.Delete(IFields,Length(IFields),1);
       F_IndexDefs.Add(IName,IFields,Options);
       F_IndexDefs.Items[F_IndexDefs.Count-1].DescFields := IDescFields;
     End;
End;
{$ELSE}
Procedure TKADaoTable.InternalInitIndexDefs;
Var
  {$IFDEF DYNADAO}
  ID         : OleVariant;
  {$ELSE}
  ID         : Indexes;
  {$ENDIF}
  IndexCount : Integer;
  FieldCount : Integer;
  X          : Integer;
  Y          : Integer;
  IName      : String;
  IFields    : String;
  IPrimary   : Boolean;
  IUnique    : Boolean;
  IDesc      : Boolean;
  Options    : TIndexOptions;
Begin
 F_IndexDefs.Clear;
 if F_TableName='' Then Exit;
 if F_TableType <> DaoApi.dbOpenTable Then Exit;
 if F_Database.EngineType=dbUseODBC Then Exit;
 ID := F_Database.CoreDatabase.TableDefs.Item[F_TableName].Indexes;
 //****************************************************************** 13.02.2002
 Try
   ID.Refresh;
 Except
 End;
 //****************************************************************** 13.02.2002
 IndexCount := ID.Count;
 For X := 0 To IndexCount-1 do
     Begin
       IName       := '';
       IPrimary    := False;
       IUnique     := False;
       Try
        IName       := ID.Item[X].Name;
        IPrimary    := ID.Item[X].Primary;
        IUnique     := ID.Item[X].Unique;
       Except
       End;
       IDesc       := True;
       IFields     := '';
       FieldCount := ID.Item[X].Fields.Count;
       For Y := 0 To FieldCount-1 Do
           Begin
             if Y < FieldCount-1 Then
                IFields:=IFields+ID.Item[X].Fields.Item[Y].Name+';'
             Else
                IFields:=IFields+ID.Item[X].Fields.Item[Y].Name;
             if (ID.Item[X].Fields.Item[Y].Attributes and dbDescending) = 0 Then IDesc := False;
           End;
       Options  := [];
       if IPrimary Then Options:=Options+[ixPrimary];
       if IUnique  Then Options:=Options+[ixUnique];
       if IDesc    Then Options:=Options+[ixDescending];
       F_IndexDefs.Add(IName,IFields,Options);
     End;
End;
{$ENDIF}

Procedure TKADaoTable.UpdateIndexDefs;
Begin
 {$IFDEF D4UP}
   F_IndexDefs.UpdateIndexes;
 {$ELSE}
   InternalInitIndexDefs;
 {$ENDIF}
End;

Procedure TKADaoTable.InternalInitFieldDefs;
Var
  X        : Integer;
  Sz       : Integer;
  Typ      : Integer;
  ResTyp   : TFieldType;
  Nam      : String;
  F_Format : String;
Begin
        FieldDefs.Clear;
        F_DisplayLabels.Clear;
        if Not InInternalOpen Then
           Begin
            if Not F_Active Then OpenDaoRecordset;
           End;
        with FieldDefs do
        Begin
          For X:=0 To F_DaoTable.Fields.Count-1 do
              Begin
                Typ  := DaoFields[X].Type;
                Nam := DaoFields[X].Name;
                Sz:=DaoSizeToBDESize(Typ,DaoFields[X].Size);
                if (Typ=dbDate) And (PropertyExists(OleVariant(DaoFields[X].Properties),'Format')) Then
                   Begin
                     F_Format:=DaoFields[X].Properties.Item['Format'].Value;
                     if AnsiCompareText(F_Format,'Long Time')=0    Then Typ:=dbTime
                        Else
                        if AnsiCompareText(F_Format,'Medium Time')=0  Then Typ:=dbTime
                           Else
                           if AnsiCompareText(F_Format,'Short Time')=0   Then Typ:=dbTime
                              Else
                                if AnsiCompareText(F_Format,'General Date')=0   Then Typ:=dbTimeStamp;
                   End
                Else
                   if (Typ=dbDate) Then Typ:=dbTimeStamp;
                   if (Typ=dbText) And (Sz=0) Then Sz:=255;
                   if (Typ=dbLong) And ((DaoFields[X].Attributes And dbAutoIncrField) > 0) Then Typ := dbAutoIncInteger;
                   //************************************************ 26.01.2002
                   if F_Database.EngineType=dbUseJet Then
                      Begin
                        if (Typ=dbAutoIncInteger) Then
                           Begin
                             F_DefaultValues.Strings[X] := '';
                           End;
                        if ((Typ=dbFloat) or (Typ=dbLong) or (Typ=dbInteger) or (Typ=dbCurrency) or (Typ=dbSingle) or (Typ=dbDouble) or (Typ=dbBigInt) or (Typ=dbNumeric) or (Typ=dbDecimal))
                        And (Pos('genuniqueid',AnsiLowercase(DaoFields[X].DefaultValue))>0) Then
                            Begin
                              F_DefaultValues.Strings[X] := '';
                            End;
                        if (Typ=dbGUID) And (Pos('genguid',AnsiLowercase(DaoFields[X].DefaultValue))>0) Then
                            Begin
                              F_DefaultValues.Strings[X] := '';
                            End;
                      End;
                   //************************************************ 26.01.2002
                   //***********************************************************
                   ResTyp := DaoToBDE(Typ);
                   if F_ReadOnly Then
                      Add(Nam,ResTyp,Sz,False)
                   Else
                      if (F_UseDaoProperties) Then
                          Add(Nam,ResTyp,Sz,DaoFields[X].Required)
                      Else
                          Add(Nam,ResTyp,Sz,False);
                   //***********************************************************
                   if ResTyp=ftBlob Then F_DefaultValues.Strings[X] := '';
                   //***********************************************************
                if (F_UseDisplayLabels) And (PropertyExists(OleVariant(DaoFields[X].Properties),'Caption')) Then
                   F_DisplayLabels.Add(DaoFields[X].Properties['Caption'])
                Else
                   F_DisplayLabels.Add(Nam);
                //**************************************************************
                // Tricky way to check out GUID
                //**************************************************************
                if (Typ=dbGUID) Then Items[Count-1].Precision := GUID_ID;
                //**************************************************************
              End;
        End;
        InternalInitIndexDefs;
        if Not InInternalOpen Then
           Begin
            if Not F_Active Then CloseDaoRecordset;
           End;
End;

Procedure TKADaoTable.InternalSetDisplayLabels;
Var
  X  : Integer;
  FF : TField;
Begin
 For X:=0 To FieldDefs.Count-1 do
  Begin
   FF := FindField(FieldDefs.Items[X].Name);
   if FF <> Nil Then FF.DisplayLabel:=F_DisplayLabels.Strings[X];
  End;
End;

Function TKADaoTable.GetActiveRecordBuffer:  TRecordBuffer;
Begin
        case State of
             {$IFDEF D4UP}
             dsBlockRead   ,
             {$ENDIF}
             dsBrowse      : if IsEmpty Then
                                Result := Nil
                             Else
                                Result := ActiveBuffer;
             dsCalcFields  : Result    := CalcBuffer;
             dsFilter      : Result    := F_FilterBuffer;
             dsEdit        ,
             dsInsert      ,
             dsNewValue    ,
             dsCurValue    : Result    := ActiveBuffer;
             dsOldValue:     if F_OldValue=Nil then
                              Result   :=ActiveBuffer
                           Else
                              Result   := F_OldValue;
             dsSetKey      :  Result   := F_ActiveKeyBuffer;
        Else Result:=Nil;
        End;
End;


Procedure TKADaoTable.InternalHandleException;
Begin
     Application.HandleException(Self);
End;

Procedure TKADaoTable.ClearCalcFields(Buffer: TRecordBuffer);
Begin
    FillChar(Buffer[F_StartCalc],CalcFieldsSize,0);
End;

Procedure TKADaoTable.F_OnGetMemoText(Sender: TField; var Text: String; DisplayText: Boolean);
Var
 P      : Integer;
 Buffer : TRecordBuffer;
 DInfo  : TDaoInfo;
Begin
   if F_CacheMemos Then
      Begin
       Buffer := GetActiveRecordBuffer;
       if Buffer=Nil Then Exit;
       DInfo := PDaoInfo(Buffer+F_StartMyInfo)^;
       P := Pos(#13,DInfo.RecordData.Strings[Sender.FieldNo-1]);
       if P > 0 Then
          Text := Copy(DInfo.RecordData.Strings[Sender.FieldNo-1],1,P-1)
       Else
          Text := DInfo.RecordData.Strings[Sender.FieldNo-1];
      End;
End;


Procedure TKADaoTable.F_OnGetGUIDText(Sender: TField; var Text: String; DisplayText: Boolean);
Var
 P      : Integer;
 Buffer : TRecordBuffer;
 DInfo  : TDaoInfo;
Begin
   if F_ShowGUID Then
      Begin
       Buffer := GetActiveRecordBuffer;
       if Buffer=Nil Then Exit;
       DInfo := PDaoInfo(Buffer+F_StartMyInfo)^;
       Text  := DInfo.RecordData.Strings[Sender.FieldNo-1];
       P := Pos('{guid ',Text);
       if P = 1 Then
          Begin
            System.Delete(Text,1,6);
            P := Pos('}}',Text);
            if P = Length(Text)-1 Then System.Delete(Text,P,1);
          End;
      End;
End;

Procedure TKADaoTable.F_OnSetGUIDText(Sender: TField; const Text: string);
Var
 SGUID : String;
Begin
  if F_ShowGUID Then
     Begin
       if Length(Text) = 38 Then
          Begin
            SGUID := PutGUIDInString(Text);
            Sender.AsString := SGUID;
          End;
     End;
End;

Function TKADaoTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  SourceBuffer : TRecordBuffer;
  Value        : TStringList;
  FieldNumber  : Integer;
  TempString   : AnsiString;
  TempWide     : String;
  TempVar      : OleVariant;
  BKD          : Integer;
Begin
        Result:=False;
        SourceBuffer:=GetActiveRecordBuffer;
        if (not F_Active) or (SourceBuffer=nil) then
           Begin
              Exit;
           End;
        if (Field.FieldKind=fkCalculated) or (Field.FieldKind=fkLookup) then
          Begin
                Inc(SourceBuffer,F_StartCalc+Field.Offset);
                if (SourceBuffer[0]=0) or (Buffer=nil) then
                  Begin
                    if SourceBuffer[0] <> 0 Then Result := True;
                    Exit;
                  End
                Else
                  Begin
                    Move(SourceBuffer[1], Buffer^, Field.DataSize);
                    Result:=True;
                  End;
          end
        else
          Begin
           Try
             Value := PDaoInfo(PChar(SourceBuffer)+F_StartMyInfo)^.RecordData;
           Except
             Exit;
           End;
           FieldNumber:=Field.FieldNo-1;
           if (Value=Nil) Then Exit;
           if (Buffer = Nil)  Then
            Begin
              Result:=(Value.Strings[FieldNumber] <> '');
            End
           Else
            Begin
              Case Field.DataType of
                ftBytes     : Begin
                               //***********************************************
                               If Field.ValidChars = GUID_VALID_CHARS Then
                                  Begin
                                    //******************************* GUID
                                    Result := GUIDToBuffer(Buffer,Value.Strings[FieldNumber])
                                  End
                               Else
                                  Begin
                                    //******************************* BYTE ARRAY
                                    TempString:=Value.Strings[FieldNumber];
                                    TempString:=TempString+#0;
                                    CopyMemory(PChar(Buffer),PChar(TempString),Length(TempString));
                                    Result := Value.Strings[FieldNumber] <> '';
                                  End;
                               //***********************************************
                              End;
                ftInteger   : Result := IntegerToBuffer(Buffer,Value.Strings[FieldNumber]);
                ftAutoInc   : Result := IntegerToBuffer(Buffer,Value.Strings[FieldNumber]);
                ftSmallint  : Result := IntegerToBuffer(Buffer,Value.Strings[FieldNumber]);
                ftCurrency  : Result := FloatToBuffer(Buffer,Value.Strings[FieldNumber]);
                ftFloat     : Result := FloatToBuffer(Buffer,Value.Strings[FieldNumber]);
                ftDate      : Result := DateToBuffer(Buffer,Value.Strings[FieldNumber]);
                ftString    : Begin
                                TempString := Value.Strings[FieldNumber];
                                TempString := TempString+#0;
                                CopyMemory(PChar(Buffer),PChar(TempString),Length(TempString));
                                Result     := Value.Strings[FieldNumber] <> '';
                              End;
                ftWideString: Begin
                                TempWide := Value.Strings[FieldNumber];
                                WStrCopy(Buffer, PWideChar(TempWide));
                                Result     := Value.Strings[FieldNumber] <> '';
                              End;
                ftTime      : Result := TimeToBuffer(Buffer,Value.Strings[FieldNumber]);
                ftDateTime  : Result := DateTimeToBuffer(Buffer,Value.Strings[FieldNumber]);
                ftBoolean   : Result := BooleanToBuffer(Buffer,Value.Strings[FieldNumber]);
                //*************************************************** 07.04.2007
                ftIDispatch : Begin
                                TempVar := Unassigned;
                                {
                                if  (State <> dsInsert)
                                and (NOT F_DaoTable.BOF)
                                and (NOT F_DaoTable.EOF) Then
                                   Begin
                                     BKD     := GetDaoBookmark(F_DaoTable);
                                     TempVar := GetRawFieldData(Field.FieldName);
                                     InternalMoveToBookmark(@BKD);
                                   End;
                                }
                                IDispatch(Buffer^) := TempVar;
                                Result             := Value.Strings[FieldNumber] <> '';
                              End;
                //************************************************************** 
              End;
            End;
          End;
End;

Procedure TKADaoTable.SetFieldData(Field: TField; Buffer: Pointer);
var
        DestinationBuffer: TRecordBuffer;
        Tmp              : String;
        BTmp             : WordBool;
        BBTmp            : Boolean;
Begin
        DestinationBuffer:=GetActiveRecordBuffer;
        if DestinationBuffer=Nil Then Exit;
        if (Field.FieldKind=fkCalculated) or (Field.FieldKind=fkLookup) then
           Begin
                Inc(DestinationBuffer,F_StartCalc+Field.Offset);
                Boolean(DestinationBuffer[0]):=(Buffer<>nil);
                if Boolean(DestinationBuffer[0]) then CopyMemory(@DestinationBuffer[1],Buffer,Field.DataSize);
           End
        Else
          Begin
            //************************************** Field Checking And Validation
            if Field.FieldKind in [fkData, fkInternalCalc] then Field.Validate(Buffer);
            if (Field.ReadOnly) And (State <> dsSetKey) Then Exit; // 19.02.2003
            //********************************************************************
            Tmp:='';
            if Buffer <> Nil Then
               Begin
                 Case Field.DataType of
                  ftBytes      : Begin
                                   //*******************************************
                                   If Field.ValidChars = GUID_VALID_CHARS Then
                                      Begin
                                        //*************************** GUID
                                        Tmp := BufferToGUID(Buffer);
                                      End
                                   Else
                                      Begin
                                        //*************************** BYTE ARRAY
                                        SetString(Tmp,PChar(Buffer),Field.Size);
                                      End;
                                   //*******************************************
                                 End;
                  ftString     : Tmp := PAnsiChar(Buffer);
                  ftWideString : Tmp := PWideChar(Buffer);
                  ftSmallint   : Tmp := IntToStr(Integer(Buffer^));
                  ftWord       : Tmp := IntToStr(Integer(Buffer^));
                  ftInteger    : Tmp := IntToStr(Integer(Buffer^));
                  ftAutoInc    : Tmp := IntToStr(Integer(Buffer^));

                  ftBCD        : Tmp := FloatToStr(Double(Buffer^));
                  ftCurrency   : Tmp := FloatToStr(Double(Buffer^));
                  ftFloat      : Tmp := FloatToStr(Double(Buffer^));

                  ftDate       : Tmp := BufferToDate(Buffer);
                  ftTime       : Tmp := BufferToTime(Buffer);
                  ftDateTime   : Tmp := BufferToDateTime(Buffer);
                  ftBoolean    : Begin
                                   BTmp := WordBool(Buffer^);
                                   BBTmp:=Boolean(BTmp);
                                   Case BBTmp Of
                                     True   : Tmp:= 'True';
                                     False  : Tmp:= 'False';
                                   End;
                                 End;
                 End;
               End;
            PDaoInfo(DestinationBuffer+F_StartMyInfo)^.RecordData.Strings[Field.FieldNo-1]:=Tmp;
            PDaoInfo(DestinationBuffer+F_StartMyInfo)^.RecordData.Objects[Field.FieldNo-1]:=TObject(True);
         End;
        if not (State in [{$IFDEF D4UP}dsInternalCalc, {$ENDIF} dsCalcFields, dsFilter, dsNewValue]) then DataEvent(deFieldChange, Longint(Field));
End;

Procedure TKADaoTable.InternalFirst;
Begin
  F_RecNo:=-1;
  F_RecPos:=-1;
  if (F_DaoTable.BOF) And (F_DaoTable.EOF) Then Exit;
  if F_TableType = dbOpenForwardOnly Then Exit;
  Try
   F_DaoTable.MoveFirst;
   F_DaoTable.MovePrevious;
  Except
  End;
End;

Procedure TKADaoTable.InternalLast;
Var
 TmpRS        : OleVariant;
 DoRaise      : Boolean;
 OldR         : Integer;
Begin
     if (F_DaoTable.BOF) And (F_DaoTable.EOF) Then Begin F_RecNo:=-1; Exit; End;
     DoRaise := False;
     Try
      if F_TableType = dbOpenForwardOnly Then
        Begin
         if NOT F_DaoTable.EOF Then
           Begin
            While NOT F_DaoTable.EOF Do
              Begin
                F_DaoTable.MoveNext;
                Inc(F_RecPos);
                F_RecNo:=F_RecPos;
              End;
             Dec(F_RecPos);
           End;
          F_RecNo:=F_RecPos;
        End
      Else
        Begin
         OleVariant(F_DaoTable).MoveLast;
         F_DaoTable.MoveNext;
         OldR    := F_RecNo;
         F_RecNo := F_DaoTable.RecordCount;
         if F_TableType = dbOpenTable Then
           Begin
            if (F_RecNo > F_LastRecord) Or (OldR > F_RecNo) Then
                Begin
                 TmpRS:=OleVariant(F_DaoTable).OpenRecordset(dbOpenSnapShot);
                 TmpRS.MoveLast;
                 F_RecNo:=TmpRS.RecordCount;
                 TmpRS.Close;
                 F_LastRecord:=F_RecNo;
                 if (F_RecNo <> F_DaoTable.RecordCount) And (F_WarnOnBadDatabase) Then
                    Begin
                      DoRaise := True;
                      DatabaseError(Format(E2026,[F_Database.Database]));
                    End;
                End;
           End;
        End;
     Except
       if DoRaise Then Raise;
     End;
End;

Procedure TKADaoTable.InternalMoveToBookmark(Bookmark: Pointer);
Var
  X       : Integer;
  BK      : OleVariant;
  P       : PChar;
  PB      : PChar;
Begin
   BK  := VarArrayCreate([0, 3],varByte);
   P   := PChar(Bookmark);
   PB  := VarArrayLock(BK);
   For X := 0 to 3 do PB[X] := P[X];
   VarArrayUnLock(BK);
   Try
     OleVariant(F_DaoTable).Bookmark:=VarAsType(BK, varArray or VarByte);
   Except
     if GetLastDaoError.ErrNo=3167 Then First;
     VarClear(BK);
     Raise;
   End;
   VarClear(BK);
End;

Procedure TKADaoTable.InternalSetToRecord(Buffer: TRecordBuffer);
Var
  RN     : Integer;
  Delta  : Integer;
  Err    : String;
Begin
  if (F_DaoTable.BOF) And (F_DaoTable.EOF) Then Exit;
  if Buffer=Nil Then Exit;
  IF PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkFlag in [bfCurrent, bfInserted] Then
     Begin
       RN:=F_RecNo;
       F_RecNo:=PDaoInfo(Buffer+F_StartMyInfo)^.RecordNo;
       if F_TableType = dbOpenForwardOnly Then Exit;
       if State = dsSetKey Then Exit;
       if F_Bookmarkable Then
          Begin
            Try
             if (State <> dsEdit) And (State <> dsInsert) Then
             InternalMoveToBookmark(@PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkData);
            Except
              //****************************************** HANDLE DELETED RECORD
              Err := GetLastDaoError.Description;
              Try
                InternalFirst;
                Resync([rmCenter]);
                DatabaseError(Err);
              Finally
              End;
              //****************************************** HANDLE DELETED RECORD
            End;
          End
       Else
          Begin
           Delta:=F_RecNo-RN;
           if Delta=0 Then Exit;
           Try
             If ((F_Filtered) And (Assigned(F_OnFilterRecord))) Or (F_RangeFiltered) Then
                 Begin
                  F_DaoTable.MoveFirst;
                  OleVariant(F_DaoTable).Move(F_RecNo);
                 End
             Else
                 Begin
                   OleVariant(F_DaoTable).Move(Delta);
                 End;
           Except
             F_DaoTable.MoveFirst;
             OleVariant(F_DaoTable).Move(F_RecNo);
           End;
          End;
    End;
End;

Procedure TKADaoTable.InternalEdit;
Label
   Again;
Var
  PS       : TRecordBuffer;
  PT       : TRecordBuffer;
  JumpAgain: Boolean;
  Action   : TDataAction;
  LDE      : TDaoErrRec;
Begin
     if F_OldValue <> Nil then FreeRecordBuffer(F_OldValue);
     F_OldValue:=AllocRecordBuffer;
     PT := F_OldValue+F_StartMyInfo;
     PS := GetActiveRecordBuffer;
     if PS <> Nil Then
        Begin
         PS := PS+F_StartMyInfo;
         PDaoInfo(PT)^.BookmarkData := PDaoInfo(PS)^.BookmarkData;
         PDaoInfo(PT)^.BookmarkFlag := PDaoInfo(PS)^.BookmarkFlag;
         PDaoInfo(PT)^.RecordNo := PDaoInfo(PS)^.RecordNo;
         PDaoInfo(PT)^.RecordData.Assign(PDaoInfo(PS)^.RecordData);
        End;
Again:
     JumpAgain:= False;
     Try
       if F_DaoTable.EditMode <> DaoApi.dbEditInProgress Then F_DaoTable.Edit;
     Except
       On E:Exception do
          Begin
           LDE:=GetLastDaoError;
           //******************************************************** 04.02.2002
           if F_DaoTable.EditMode <> DaoApi.dbEditInProgress Then
              Begin
               OleVariant(F_DaoTable).Move(0);
               Resync([]);
              End;
           //******************************************************** 04.02.2002
           if Assigned(OnEditError) Then
              Begin
                E.HelpContext := LDE.HelpContext;
                E.Message     := LDE.Description;
                OnEditError(Self,EDatabaseError(E),Action);
                if Action=daRetry  Then JumpAgain:=True;
                if Action=daAbort  Then Exit;
                if Action=daFail   Then Raise;
              End
           Else
              Begin
                Raise;
              End;
          End;
     End;
     if JumpAgain Then Goto Again;
     inherited InternalEdit;
     //*************************************************************************
     if PS <> Nil Then InternalFillRecordData(OleVariant(F_DaoTable), True, PS);
     //*************************************************************************
End;

Procedure TKADaoTable.InternalCancel;
Begin
     Try
       If (F_DaoTable.EditMode = DaoApi.dbEditInProgress) Then
          OleVariant(F_DaoTable).CancelUpdate;
     Except
     End;
     if F_OldValue <> Nil Then FreeRecordBuffer(F_OldValue);
     F_Database.Idle;   //******************************************* 27.01.2002
     inherited InternalCancel;
End;

Procedure TKADaoTable.InternalPost;
Label Again;
Var
 Buffer    : TRecordBuffer;
 X         : Integer;
 RData     : TStringList;
 S         : String;
 DTSV      : OleVariant;
 FF        : TField;
 Action    : TDataAction;
 JumpAgain : Boolean;
 LDE       : TDaoErrRec;
Begin
 F_PostMade := False;
 CheckActive;
 //*************************************************************** Special Check
 If (State=dsEdit) And (F_DaoTable.EditMode <> DaoApi.DbEditInProgress) Then
     Begin
       Try
        F_DaoTable.Edit;
       Except
        if F_DaoTable.EditMode <> DaoApi.dbEditInProgress Then OleVariant(F_DaoTable).Move(0);
        DaoInternalRefresh;
        Raise;
       End;
     End;
 //*****************************************************************************
 Again:
 JumpAgain := False;
 if State = dsEdit then //************************************* EDITING A RECORD
  Begin
    Buffer:=GetActiveRecordBuffer;
    RData:=PDaoInfo(Buffer+F_StartMyInfo)^.RecordData;
    For X:=0 to RData.Count-1 do
        Begin
        FF:=FindField(FieldDefs.Items[X].Name);
        if (Boolean(RData.Objects[X])) And (FF <> Nil) Then
         Begin
          S:=RData.Strings[X];
          if Boolean(F_UpdatableFields.Items[X]) Then
             Begin
               if S='' Then
                  Begin
                    if (FF.DataType=ftString) or (FF.DataType=ftWideString) or (FF.DataType=ftMemo) or (FF.DataType=ftWideMemo) Then
                       Begin
                         if DaoFields[X].AllowZeroLength Then
                            Begin
                              DaoFields[X].Value := '';
                            End
                         Else
                            Begin
                              DaoFields[X].Value := NULL;
                            End;
                       End
                    Else
                       Begin
                         DaoFields[X].Value := NULL;
                       End;
                  End
               Else
                  Begin
                    //*********************************************** Byte Array
                    if  (FF.DataType=ftBytes)
                    And (FF.ValidChars <> GUID_VALID_CHARS) Then
                        Begin
                          DTSV := StringToBlob(TBlobField(FF), S);
                          DaoFields[X].Value:=DTSV;
                          VarClear(DTSV);
                          DTSV := NULL;
                        End
                    Else
                    //*********************************************** Date/Time
                    if (FF.DataType=ftDate) or
                       (FF.DataType=ftTime) or
                       (FF.DataType=ftDateTime) Then
                       Begin
                         DTSV:=ComposeDateTimeVariant(S);
                         if DTSV <> NULL Then DaoFields[X].Value:=VarAsType(DTSV,VarDate);
                         VarClear(DTSV);
                         DTSV:=NULL;
                       End
                    Else
                       Begin
                         if (FF.IsBlob) Then
                            Begin
                              if F_HasEncoder Then
                                  Begin
                                   //*******************************************
                                   // Perform Encoding here
                                   //*******************************************
                                   SetStrProp(F_Encrypter, F_DecodedString,S);
                                   S:=GetStrProp(F_Encrypter, F_EncodedString);
                                  End;
                              DTSV := StringToBlob(TBlobField(FF), S);
                              DaoFields[X].Value:=DTSV;
                              VarClear(DTSV);
                              DTSV:=NULL;
                            End
                         Else
                            Begin
                              if (F_HasEncoder) And ((FF.DataType=ftString) or (FF.DataType=ftWideString)) Then
                                 Begin
                                   //*******************************************
                                   // Perform Encoding here
                                   //*******************************************
                                   SetStrProp(F_Encrypter, F_DecodedString,S);
                                   S:=GetStrProp(F_Encrypter, F_EncodedString);
                                 End;
                              DaoFields[X].Value:=S;
                            End;
                       End;
                  End;
             End;
         End;
        End;
    Try
      OleVariant(F_DaoTable).Update;
    Except
      On E:Exception do
           Begin
            If Assigned(OnPostError) Then
               Begin
                   LDE := GetLastDaoError;
                   E.HelpContext := LDE.HelpContext;
                   E.Message     := LDE.Description;
                   OnPostError(Self,EDatabaseError(E),Action);
                   if Action = daRetry Then
                      Begin
                        JumpAgain := True;
                        If (F_DaoTable.EditMode <> DaoApi.dbEditInProgress) Then OleVariant(F_DaoTable).Move(0);
                      End
                   Else
                   if Action = daAbort Then
                      Begin
                        If  (F_DaoTable.LockEdits=False)
                        And (F_DaoTable.EditMode = DaoApi.dbEditInProgress) Then OleVariant(F_DaoTable).CancelUpdate;
                        If (F_DaoTable.EditMode <> DaoApi.dbEditInProgress) Then OleVariant(F_DaoTable).Move(0);
                        Exit;
                      End
                   Else
                   if Action = daFail  Then
                      Begin
                        If  (F_DaoTable.LockEdits=False)
                        And (F_DaoTable.EditMode = DaoApi.dbEditInProgress) Then OleVariant(F_DaoTable).CancelUpdate;
                        If (F_DaoTable.EditMode <> DaoApi.dbEditInProgress) Then OleVariant(F_DaoTable).Move(0);
                        Raise;
                      End;
               End
            Else
               Begin
                 If  (F_DaoTable.LockEdits=False)
                 And (F_DaoTable.EditMode = DaoApi.dbEditInProgress) Then OleVariant(F_DaoTable).CancelUpdate;
                 If (F_DaoTable.EditMode <> DaoApi.dbEditInProgress) Then OleVariant(F_DaoTable).Move(0);
                 Raise;
               End;
           End;
    End;
    if JumpAgain Then Goto Again;                         
  End
 Else
  Begin //************************************************** ADDING A NEW RECORD
    Buffer:=GetActiveRecordBuffer;
    RData:=PDaoInfo(Buffer+F_StartMyInfo)^.RecordData;
    F_DaoTable.AddNew;
    For X:=0 to RData.Count-1 do
        Begin
         FF:=FindField(FieldDefs.Items[X].Name);
         if (Boolean(RData.Objects[X])) And (FF <> Nil) Then
         Begin
          S:=RData.Strings[X];
          if Boolean(F_UpdatableFields.Items[X]) Then
             Begin
               if S='' Then
                  Begin
                    if (FF.DataType=ftString) or (FF.DataType=ftWideString) or (FF.DataType=ftMemo) or (FF.DataType=ftWideMemo) Then
                       Begin
                         if DaoFields[X].AllowZeroLength Then
                            Begin
                              DaoFields[X].Value := '';
                            End
                         Else
                            Begin
                              DaoFields[X].Value := NULL;
                            End;
                       End
                    Else
                       Begin
                         DaoFields[X].Value := NULL;
                       End;
                  End
               Else
                  Begin
                    //*********************************************** Byte Array
                    if  (FF.DataType=ftBytes)
                    And (FF.ValidChars <> GUID_VALID_CHARS) Then
                        Begin
                          DTSV := StringToBlob(TBlobField(FF), S);
                          DaoFields[X].Value:=DTSV;
                          VarClear(DTSV);
                          DTSV := NULL;
                        End
                    Else
                    //*********************************************** Date/Time
                    if (FF.DataType=ftDate) or
                       (FF.DataType=ftTime) or
                       (FF.DataType=ftDateTime) Then
                        Begin
                          DTSV:=ComposeDateTimeVariant(S);
                          if DTSV <> NULL Then DaoFields[X].Value:=VarAsType(DTSV,VarDate);
                          VarClear(DTSV);
                          DTSV:=NULL;
                        End
                    Else
                        Begin
                          if (FF.IsBlob) Then
                             Begin
                               if F_HasEncoder Then
                                  Begin
                                   //*******************************************
                                   // Perform Encoding here
                                   //*******************************************
                                   SetStrProp(F_Encrypter, F_DecodedString,S);
                                   S:=GetStrProp(F_Encrypter, F_EncodedString);
                                  End;
                               DTSV := StringToBlob(TBlobField(FF), S);
                               DaoFields[X].Value:=DTSV;
                               VarClear(DTSV);
                               DTSV:=NULL;
                             End
                          Else
                             Begin
                               if (F_HasEncoder) And ((FF.DataType=ftString) or (FF.DataType=ftWideString)) Then
                                  Begin
                                   //*******************************************
                                   // Perform Encoding here
                                   //*******************************************
                                   SetStrProp(F_Encrypter, F_DecodedString,S);
                                   S:=GetStrProp(F_Encrypter, F_EncodedString);
                                  End;
                               DaoFields[X].Value:=S;
                             End;
                         End;
                  End;
             End;
          End;
        End;
      Try
        OleVariant(F_DaoTable).Update;
      Except
        On E:Exception do
           Begin
            If Assigned(OnPostError) Then
               Begin
                   LDE := GetLastDaoError;
                   E.HelpContext := LDE.HelpContext;
                   E.Message     := LDE.Description;
                   OnPostError(Self,EDatabaseError(E),Action);
                   if Action = daRetry Then
                      Begin
                        JumpAgain := True;
                      End
                   Else
                   if Action = daAbort Then
                      Begin
                        If F_DaoTable.EditMode = DaoApi.dbEditAdd Then OleVariant(F_DaoTable).CancelUpdate;
                        Exit;
                      End
                   Else
                   if Action = daFail Then
                      Begin
                        Raise;
                      End;
               End
            Else
               Begin
                 If F_DaoTable.EditMode = DaoApi.dbEditAdd Then OleVariant(F_DaoTable).CancelUpdate;
                 Raise;
               End;
           End;
      End;
      if JumpAgain Then Goto Again;
      Try
       //************************************************* CHANGED AT 06.01.2001
       F_RefreshRC := True;
       Inc(F_LastRecord);
       Inc(F_RecNo);
       If Not F_Bookmarkable Then
          Begin
           InternalLast;
           PDaoInfo(Buffer+F_StartMyInfo)^.RecordNo:=F_RecNo-1;
          End;
       //***********************************************************************
      Except
      End;
  End;
  if State <> dsEdit then InternalLast; //10.10.2004 BETA TEST TCHANGE
  If F_Bookmarkable Then
     Begin
       if F_Database.EngineType=dbUseJet Then
          Begin
            PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkData:=GetDaoLastModifiedBookMark(F_DaoTable);
            InternalMoveToBookmark(@PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkData);
          End
       Else
          Begin
            If State=dsEdit Then
               PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkData:=GetDaoLastModifiedBookMark(F_DaoTable)
            Else
               PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkData:=GetDaoBookmark(F_DaoTable);
            InternalMoveToBookmark(@PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkData);
          End;
     End;
  if F_OldValue <> Nil Then FreeRecordBuffer(F_OldValue);
  F_Database.Idle;   //********************************************** 27.01.2002
  F_PostMade := True;
End;

Procedure TKADaoTable.Post;
Begin
  F_InPost   := True;
  Try
    Inherited Post;
  Finally
    F_InPost := False;
  End;
  If (F_SortedBy.Count > 0) And (F_RefreshSorted) Then
     Begin
       CheckBrowseMode;
       InternalClearBookmarks;
       ClearBuffers;
       OleVariant(F_DaoTable).Requery;
       F_RefreshRC := True;
       ActivateBuffers;
       First;
     End;
End;

Procedure TKADaoTable.Resync(Mode: TResyncMode);
Begin
  Inherited Resync(Mode);
End;

Function TKADaoTable.Reposition : Boolean;
Var
  Buffer : TRecordBuffer;
Begin
   Result := False;
   Buffer := GetActiveRecordBuffer;
   if (Buffer = Nil) Then Exit;
   InternalSetToRecord(Buffer);
   Result := True;
End;

Procedure TKADaoTable.InternalAddRecord(Buffer: Pointer; Append: Boolean);
Begin
    if Append Then
       Begin
         InternalLast;
         SetBookmarkFlag(Buffer, bfEOF);
       End;
    InternalPost;
End;

Procedure TKADaoTable.InternalDelete;
Label Again;
Var
  Buffer   : TRecordBuffer;
  X        : Integer;
  I        : Integer;
  RN       : Integer;
  RR       : Integer;
  Action   : TDataAction;
  LDE      : TDaoErrRec;
  JumpAgain: Boolean;
Begin
  Buffer := GetActiveRecordBuffer;
  if Buffer=Nil Then Exit;
  RN     := PDaoInfo(Buffer + F_StartMyInfo)^.RecordNo;
  I      := F_BookmarkRN.IndexOf(Pointer(RN));
  if I > -1 Then
     Begin
       F_BookmarkRN.Delete(I);
       F_BookmarkID.Delete(I);
     End;
  For X:=0 to F_BookmarkRN.Count-1 do
      Begin
       RR := Integer(F_BookmarkRN.Items[X]);
       if RR > RN Then
          Begin
            Dec(RR);
            F_BookmarkRN.Items[X]:=Pointer(RR);
          End;
      End;
Again:
  JumpAgain:=False;
  Try
    F_DaoTable.Delete;
  Except
    On E:Exception do
     Begin
       LDE:=GetLastDaoError;
       if Assigned(OnDeleteError) Then
          Begin
           E.HelpContext := LDE.HelpContext;
           E.Message     := LDE.Description;
           OnDeleteError(Self,EDatabaseError(E),Action);
           if Action = daRetry Then
              Begin
               JumpAgain := True;
              End
           Else
           if Action = daAbort Then
              Begin
               F_RefreshRC := True;
               DaoInternalRefresh;
               F_RefreshRC := True;
               Exit;
              End
           Else
           if Action = daFail  Then
              Begin
                if LDE.ErrNo=3167 Then
                   Begin
                    F_RefreshRC := True;
                    DaoInternalRefresh;
                   End;
                Raise;
              End;
          End
       Else
          Begin
           if LDE.ErrNo=3167 Then
              Begin
               F_RefreshRC := True;
               DaoInternalRefresh;
               Exit;
              End;
          End;
       if Not JumpAgain Then Raise; //******************************* 15.01.2002
     End;
  End;
  if JumpAgain Then Goto Again;
  F_Database.Idle;   //********************************************** 27.01.2002
  F_RefreshRC := True;
  IF (F_DaoTable.EOF) then
     Begin
       OleVariant(F_DaoTable).MoveLast;
     End
  Else
     Begin
       F_DaoTable.MoveNext;
     End;
End;


Procedure TKADaoTable.RollbackRefresh;
Begin
 F_RefreshRC := True;
 ClearBuffers;
 ActivateBuffers;
End;

Procedure TKADaoTable.DaoInternalRefresh;
Var
  TempRecNo:Integer;
Begin
    Try
     F_RefreshRC := True;
     Resync([rmExact, rmCenter]);
    Except
     TempRecNo:=F_RecNo;
     CheckBrowseMode;
     ClearBuffers;
     CloseDaoRecordset;
     OpenDaoRecordset;
     ActivateBuffers;
     First;
     if TempRecNo < RecordCount Then MoveBy(TempRecNo) Else Last;
    End;
End;

Procedure TKADaoTable.InternalRefresh;
Var
  TempRecNo : Integer;
Begin
    Try
     F_RefreshRC := True;
     if NOT Self.ControlsDisabled Then Resync([rmExact, rmCenter]);
    Except
     TempRecNo:=F_RecNo;
     CheckBrowseMode;
     ClearBuffers;
     CloseDaoRecordset;
     OpenDaoRecordset;
     ActivateBuffers;
     First;
     if TempRecNo < RecordCount Then MoveBy(TempRecNo) Else Last;
    End;
End;

Procedure TKADaoTable.RefreshDataEx;
Var
  TempRecNo : Integer;
Begin
  Try
    CheckBrowseMode;
  Except
  End;
  TempRecNo := F_RecNo;
  CheckBrowseMode;
  ClearBuffers;
  CloseDaoRecordset;
  OpenDaoRecordset;
  ActivateBuffers;
  First;
  if TempRecNo < RecordCount Then MoveBy(TempRecNo) Else Last;
End;

Procedure TKADaoTable.RefreshData;
Var
 BK     : TBookmark;
 FD     : OleVariant;
 Exists : Boolean;
Begin
  //************************************************************* 28.03.2002
  Try
    CheckBrowseMode;
  Except
  End;
  //*************************************************************
  if F_DaoTable.Restartable Then
     Begin
      InternalClearBookmarks;
      if F_Bookmarkable Then BK := Bookmark;
      ClearBuffers;
      Exists := True;
      //************************** Check if record is not deleted *** 28.03.2002
      Try
       If Not ISEmpty Then FD := F_DaoTable.Fields[0].Value;
      Except
       Exists := False;
      End;
      //************************** Check if table is empty ********** 20.08.2002
      if Exists Then Exists := NOT ((F_DaoTable.EOF) And (F_DaoTable.BOF));
      //********************************************************************
      VarClear(FD);
      OleVariant(F_DaoTable).Requery;
      F_RefreshRC := True;
      ActivateBuffers;
      If Not ISEmpty Then First;
      //********************************** If Record exists then we reposition
      if (F_Bookmarkable) And (Exists) Then
         Begin
           Bookmark := BK;
           Resync([rmExact, rmCenter]);
         End;
      //************************************************************* 28.03.2002
     End
  Else
     Begin
       //************************************************************ 28.03.2002
       Close;
       Open;
       If Not ISEmpty Then First;
       //*******************************************************************
     End;
End;

Function TKADaoTable.IsCursorOpen: Boolean;
Begin
  Result:=F_Active;
End;

Function TKADaoTable.GetCanModify: Boolean;
Begin
 Result := (F_Active) And (NOT F_ReadOnly);
End;

Function TKADaoTable.GetRecordSize: Word;
Begin
  Result:=F_BufferSize;
End;                                                       

Function TKADaoTable.AllocRecordBuffer: TRecordBuffer;
Var
  X:Integer;
Begin
        GetMem(Result,F_BufferSize);
        FillChar(Result^,F_BufferSize,0);
        PDaoInfo(Result+F_StartMyInfo)^.RecordData:=TStringList.Create;
        For X:=0 To FieldDefs.Count-1 do
          Begin
            PDaoInfo(Result+F_StartMyInfo)^.RecordData.AddObject('',TObject(False));
          End;
End;

Procedure TKADaoTable.FreeRecordBuffer(var Buffer: TRecordBuffer);
Begin
        if Buffer=Nil Then Exit;
        PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Free;
        PDaoInfo(Buffer+F_StartMyInfo)^.RecordData:=Nil;
        FreeMem(Buffer,F_BufferSize);
        Buffer:=Nil;
End;

Procedure TKADaoTable.InternalInitRecord(Buffer: TRecordBuffer);
Var
  X          : Integer;
  PT         : TRecordBuffer;
  PS         : TRecordBuffer;
  FF         : TField;
Begin
     //*************************************************************************
     if F_OldValue <> Nil Then FreeRecordBuffer(F_OldValue);
     F_OldValue:=AllocRecordBuffer;
     PT := F_OldValue+F_StartMyInfo;
     PS := GetActiveRecordBuffer;
     if PS <> Nil Then
        Begin
         PS := PS+F_StartMyInfo;
         PDaoInfo(PT)^.BookmarkData := PDaoInfo(PS)^.BookmarkData;
         PDaoInfo(PT)^.BookmarkFlag := PDaoInfo(PS)^.BookmarkFlag;
         PDaoInfo(PT)^.RecordNo := PDaoInfo(PS)^.RecordNo;
         PDaoInfo(PT)^.RecordData.Assign(PDaoInfo(PS)^.RecordData);                         
        End;
     //*************************************************************************
     For X:=0 To FieldDefs.Count-1 do
          Begin
            PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Objects[X]:=TObject(False);
            FF := FindField(FieldDefs.Items[X].Name);
            if (FF <> Nil) And  (FF.DefaultExpression <> '') Then
               Begin
                 F_DefaultValues.Strings[X]:=UnQuoteString(FF.DefaultExpression);
                 PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Objects[X]:=TObject(True);
               End;
            PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X]:=F_DefaultValues.Strings[X];
          End;
     PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkFlag := bfInserted;
     PDaoInfo(Buffer+F_StartMyInfo)^.BookmarkData := 0;
     PDaoInfo(Buffer+F_StartMyInfo)^.RecordNo     := -1;
End;

Function TKADaoTable.GetCurrentRecord(Buffer: TRecordBuffer): Boolean;
Var
  AB : TRecordBuffer;
begin
  Result := F_Active;
  if Result Then Result := Not IsEmpty;
  AB := GetActiveRecordBuffer;
  if AB = Nil Then Result := False;
  if Result then Move(AB^, Buffer^, F_BufferSize);
end;

{$IFDEF D4UP}
Function  TKADaoTable.Translate(Src, Dest: PAnsiChar; ToOem: Boolean):Integer;
{$ELSE}
Procedure TKADaoTable.Translate(Src, Dest: PAnsiChar; ToOem: Boolean);
{$ENDIF}
Begin
  {$IFDEF D4UP}
  Result := Inherited Translate(Src, Dest, ToOem);
  {$ELSE}
  Inherited Translate(Src, Dest, ToOem);
  {$ENDIF}
  if F_Translation Then
     Begin
      {$IFDEF D4UP}
      Result := 0;
      {$ENDIF}
      if (Src <> nil) and (Src <> Dest) then
         Begin
           if ToOem Then CharToOemA(Src, Dest) Else OemToCharA(Src,Dest);
           {$IFDEF D4UP}
           Result := StrLen(Dest);
           {$ENDIF}
         End;
     End;
End;

Function TKADaoTable.InternalFillRecordData(RS: OleVariant; MainTable : Boolean; Buffer:TRecordBuffer):Boolean;
Var
 X             : Integer;
 RD            : OleVariant;
 DTS           : TTimeStamp;
 FF            : TField;
 SZ            : Integer;
 IDS           : String;
 ReadData      : Boolean;
 GetDataSize   : Boolean;
Begin
 Result := True;
 F_Database.Idle;   //*********************************************** 27.01.2002
 With PDaoInfo(Buffer+F_StartMyInfo)^ do
   Begin
    if F_Bookmarkable Then BookmarkData:=GetDaoBookmark(RS) Else  BookmarkData:=0;
    RecordNo     := F_RecNo;
    BookmarkFlag := bfCurrent;
    For X:=0 To FieldDefs.Count-1 do
        Begin
         FF := FindField(FieldDefs.Items[X].Name);
         if FF <> Nil Then
            Begin
              ReadData     := True;
              GetDataSize  := True;
              //****************************************************************
              if (FF.DataType=ftIDispatch) Then
                 Begin
                   GetDataSize  := False;
                 End;
              //****************************************************************
              if (FF.IsBlob) Then
                 Begin
                   ReadData := False;
                   if  (FF.DataType = ftMemo)     And (F_CacheMemos) Then ReadData := True;
                   if  (FF.DataType = ftWideMemo) And (F_CacheMemos) Then ReadData := True;
                   if  (FF.DataType = ftBlob)     And (F_CacheBlobs) Then ReadData := True;
                 End;
              //****************************************************************
              if ReadData Then
                 Begin
                  Try
                   if MainTable Then RD:=DaoFields[X].Value Else RD:=RS.Fields.Item[X].Value
                  Except
                   RD:=NULL;
                   //********** Edit Conflict with other user.
                   if GetLastDaoError.ErrNo=3167 Then
                      Begin
                        Result:=False;
                        Exit;
                      End;
                    //****************************************
                  End;
                 End
              Else
                 Begin
                   RD:='';
                 End;
              //****************************************************************
              if VarType(RD) = varNull then
                 Begin
                  RD := ''
                 End
              Else
                 Begin
                  //********************************************* Array Handling
                  if  (NOT (FF.IsBlob))
                  And (VarISArray(RD)) Then
                     Begin
                       RD := BlobToString(TBlobField(FF),RD,(VarArrayHighBound(RD,1)-VarArrayLowBound(RD,1))+1);
                     End;
                  //********************************************* Date/Time Handling
                  if (FF.DataType=ftDateTime)
                  Or (FF.DataType=ftDate)
                  Or (FF.DataType=ftTime) Then
                     Begin
                       DTS:=DateTimeToTimeStamp(VarAsType(RD,varDate));
                       RD:=IntToStr(DTS.Date)+' '+IntToStr(DTS.Time);
                     End;
                  //********************************************* Boolean Handling
                  if (FF.DataType=ftBoolean) Then
                     Begin
                       if RD Then RD := 'True' Else RD := 'False';
                     End;
                 End;
              //****************************************************************
              if ReadData Then
                 Begin
                   if (FF.DataType = ftBlob) Then
                      Begin
                        SZ := 0;
                        if GetDataSize Then
                           Begin
                             If MainTable Then SZ := DaoFields[X].FieldSize Else SZ := RS.Fields.Item[X].FieldSize;
                           End;
                         RecordData.Strings[X]:=BlobToString((TBlobField(FF)),RD,SZ);
                      End
                   Else
                   if (FF.DataType=ftIDispatch) Then
                      Begin
                        SetLength(IDS,SizeOf(RD));
                        Move(TVarData(RD).RawData[0],IDS[1], SizeOf(RD));
                        RecordData.Strings[X] := IDS;
                      End
                   Else
                      Begin
                        RecordData.Strings[X] := RD;
                      End;
                 End
              Else
                 Begin
                  //************************************************* 01.02.2002
                  SZ := 0;
                  if GetDataSize Then
                     Begin
                       If MainTable Then SZ := DaoFields[X].FieldSize Else SZ := RS.Fields.Item[X].FieldSize;
                     End;
                  if SZ=0 Then
                     RecordData.Strings[X]:=''
                  Else
                     RecordData.Strings[X]:=IntToStr(SZ);
                  //************************************************* 01.02.2002   
                 End;
              //****************************************************************
              if (F_HasEncoder) And (ReadData) Then
                 Begin
                  //*******************************************
                  // Perform Decoding here
                  //*******************************************
                  if  (FF.DataType=ftString)
                  OR  (FF.DataType=ftWideString)
                  OR  (FF.IsBlob) Then
                    Begin
                     SetStrProp(F_Encrypter, F_EncodedString,RecordData.Strings[X]);
                     RecordData.Strings[X]:=GetStrProp(F_Encrypter, F_DecodedString);
                    End;
                 End;
              //*************************************************************
              RecordData.Objects[X]:=TObject(False);
              VarClear(RD);
              RD:=NULL;
            End;
        End;
   End; {WITH}
End;

Function TKADaoTable.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
 Acceptable : Boolean;
Begin
   Result:=grOK;
   Acceptable:=False;
   //********************************************************* SKIP UNUSUAL READ
   if (ControlsDisabled) And
      (F_InPost)         And
      (F_BatchMode)      And
      (GetMode <> gmCurrent) Then
      Begin
         if NOT (F_Filtered And Assigned(F_OnFilterRecord)) Then
            Begin
             Result:=grEOF;
             Exit;
            End;
      End;
   //***************************************************************************
   if State=dsInsert Then
      Begin
        //*********************************************************** 25.01.2002
        if NOT ((F_DaoTable.BOF) AND (F_DaoTable.EOF)) Then CheckBrowseMode;
        Result := grError;
        Exit;
        //*********************************************************** 25.01.2002
      End;
   if State=dsEdit Then
      Begin
        //*********************************************************** 25.01.2002
        if F_DaoTable.EditMode = DaoApi.dbEditInProgress Then CheckBrowseMode;
        Result := grError;
        Exit;
        //*********************************************************** 25.01.2002
      End;
   //***************************************************************************
   Repeat
    Case GetMode of
       gmNext:
        Begin
          if (F_TableType = dbOpenForwardOnly) And (F_RecNo=-1) Then
             Begin
               //******************* Do not call MoveNext at first record
             End
          Else
             Begin
               if Not F_DaoTable.EOF Then F_DaoTable.MoveNext;
             End;
          if F_DaoTable.EOF Then Result := grEOF;
          if Result=grOK Then
             Begin
               Inc(F_RecNo);
               Inc(F_RecPos);
             End;
        End;
      gmPrior:
        Begin
          if F_TableType = dbOpenForwardOnly Then
             Begin
               Result   := grBOF;
             End
          Else
             Begin
               if Not F_DaoTable.BOF Then F_DaoTable.MovePrevious;
               if F_DaoTable.BOF Then Result := grBOF;
             End;
          if Result=grOK Then
             Begin
               Dec(F_RecNo);
               Dec(F_RecPos);
             End;
        End;
      gmCurrent:
        Begin
          if F_DaoTable.BOF Then Result := grBOF;
          if F_DaoTable.EOF Then Result := grEOF;
        End;
    End;{CASE}
    //**************************************************************************
    if Result=grEOF Then
       Begin
         F_LastRecord := F_RecNo+1; /// +1 **************************** 5.1.2002
       End
    Else
       Begin
         if F_LastRecord < F_RecNo Then F_LastRecord := F_RecNo;        
       End;
    //**************************************************************************
    if Result=grOk then
       Begin
        if Not InternalFillRecordData(OleVariant(F_DaoTable), True, Buffer) Then
           Begin
             Result:=grError;
             Exit;
           End;
        Acceptable := FilterRecord(Buffer);
        if (GetMode=gmCurrent) And (Not Acceptable) Then Result:=grError;
       End;
   Until (Result <> grOk) or (Acceptable);
End;

Function TKADaoTable.FilterRecord(Buffer: TRecordBuffer): Boolean;
var
  SaveState: TDatasetState;
Begin
 Result:=True;
 SaveState:=SetTempState(dsFilter);
 ClearCalcFields(Buffer);
 GetCalcFields(Buffer);
 if F_RangeFiltered Then Result:=FilterRange(Buffer);
 if (F_Filtered) And (Result) And (Assigned(F_OnFilterRecord)) Then
    Begin
      F_FilterBuffer:=Buffer;
      OnFilterRecord(Self,Result);
    End;
 RestoreState(SaveState);
End;

Function TKADaoTable.GetRecordCount: Integer;
var
  SaveState    : TDataSetState;
  SavePosition : Integer;
  TempBuffer   : TRecordBuffer;
  TmpRS        : OleVariant;
  DoRaise      : Boolean;
Begin
 Result:=-1;
 if F_TableType=dbOpenForwardOnly Then Exit;
 if F_UseRecordCountCache Then
    Begin
     if NOT F_RefreshRC Then
        Begin
         Result := F_OldRC;
         F_LastRecord:=Result;
         Exit;
        End;
    End;

 DoRaise     := False;
 F_RefreshRC := False;
 if (F_DaoTable.BOF) And (F_DaoTable.EOF) Then
    Begin
      Result:=0;
      F_OldRC:=Result;
      F_LastRecord:=Result;
      F_RecNo := -1;
      Exit;
    End;
 If ((F_Filtered) And (Assigned(F_OnFilterRecord))) Or (F_RangeFiltered) Then
     Begin
       Result:=0;
       SaveState:=SetTempState(dsBrowse);
       SavePosition:=F_RecNo;
       Try
         TempBuffer:=AllocRecordBuffer;
         InternalFirst;
         While GetRecord(TempBuffer,gmNext,True)=grOk do Inc(Result);
       Finally
         RestoreState(SaveState);
         F_RecNo:=SavePosition;
         FreeRecordBuffer(TempBuffer);
       End;                                                  
     End
 Else
     Begin
      if F_TableType=dbOpenTable Then
         Begin
           Try
            Result:=F_DaoTable.RecordCount;
            if (Result > F_LastRecord) Then 
               Begin
                TmpRS:=OleVariant(F_DaoTable).OpenRecordset(dbOpenSnapShot);
                TmpRS.MoveLast;
                Result:=TmpRS.RecordCount;
                TmpRS.Close;
                VarClear(TmpRS);
                if (Result <> F_DaoTable.RecordCount) And (F_WarnOnBadDatabase) Then
                   Begin
                     DoRaise := True;
                     DatabaseError(Format(E2026,[F_Database.Database]));
                   End;
               End;
           Except
             if DoRaise Then Raise;
           End;
         End
      Else
         Begin
          Try
           F_DaoTable.MoveFirst;
           OleVariant(F_DaoTable).MoveLast;
           Result:=F_DaoTable.RecordCount;
           Except
           End;
          End; 
       if F_Bookmarkable Then                                                  
         Begin
           TempBuffer := GetActiveRecordBuffer;
           if TempBuffer <> Nil Then
              Begin
                if PDaoInfo(TempBuffer+F_StartMyInfo)^.BookmarkData <> 0 Then
                   InternalMoveToBookmark(@PDaoInfo(TempBuffer+F_StartMyInfo)^.BookmarkData);
              End;                                
         End
       Else
         Begin
           F_DaoTable.MoveFirst;
           if F_RecNo=-1 Then
            Begin
             F_DaoTable.MovePrevious;
            End
           Else
            Begin
             if (F_RecNo < Result) Then OleVariant(F_DaoTable).Move(F_RecNo);
            End;
         End;
     End;                                     
     F_OldRC:=Result;
     F_LastRecord:=Result;
End;

Function  TKADaoTable.GetRecNo: Integer;
var
  SaveState     : TDataSetState;
  SavePosition  : Integer;
  TempBuffer    : TRecordBuffer;
  BK            : Integer;
Begin
  UpdateCursorPos;
  //******************************************************************* 1.1.2002
  TempBuffer:=GetActiveRecordBuffer;
  if TempBuffer <> Nil Then InternalSetToRecord(TempBuffer);
  //****************************************************************************

  if NOT F_UseGetRecNo Then
     Begin
       Result := -1;
       Exit;
     End;

  if F_RecNo<-1 Then F_PostMade:=True;

  if (F_TableType = dbOpenForwardOnly) Then
     Begin
       Result := F_RecNo+1;
       Exit;
     End;

  if F_DaoTable.BOF Then
     Begin
       Result := -1;
       Exit;
     End;

  If ((F_Filtered) And (Assigned(F_OnFilterRecord)))  Or (F_RangeFiltered) Then
    Begin
     Result := -1;
     SaveState:=SetTempState(dsBrowse);
     TempBuffer:=GetActiveRecordBuffer;
     if TempBuffer <> Nil Then
        Begin
          SavePosition:=PDaoInfo(TempBuffer+F_StartMyInfo)^.BookmarkData;
          Try
           TempBuffer:=AllocRecordBuffer;
           InternalFirst;
           Result := 0;
           While (GetRecord(TempBuffer,gmNext,True)=grOk) And
                 (PDaoInfo(TempBuffer+F_StartMyInfo)^.BookmarkData <> SavePosition)
                 do Inc(Result);
          Finally
           if (PDaoInfo(TempBuffer+F_StartMyInfo)^.BookmarkData <> SavePosition) Then
              Begin
               InternalSetToRecord(GetActiveRecordBuffer);
              End;
           FreeRecordBuffer(TempBuffer);
          End;
        End;
     RestoreState(SaveState);
     if Result=-1 Then Exit;
    End
 Else
    Begin  
      if F_PostMade Then
         Begin
          TempBuffer:=GetActiveRecordBuffer;
          if TempBuffer <> Nil Then
             Begin
              F_RecNo:=-1;
              if (F_TableType=dbOpenDynaset)
              OR (F_TableType=dbOpenSnapshot)
              OR (F_TableType=dbOpenDynamic) Then
                 Begin
                   F_RecNo := F_DaoTable.AbsolutePosition;
                 End
              Else
                 Begin
                   //***********************************************************
                   // If we are at the end of the table then we can easy calc
                   // the RecNo
                   Try
                    F_DaoTable.MoveNext;
                    if F_DaoTable.EOF Then F_RecNo := F_DaoTable.RecordCount-1;
                   Except
                   End;
                   F_DaoTable.MovePrevious;
                   //***********************************************************
                   // if Previous test does not work then
                   if F_RecNo = -1 Then
                      Begin
                        if F_Bookmarkable Then
                           Begin
                             BK:=GetDaoLastModifiedBookMark(F_DaoTable);
                             F_RecNo:=F_RecalculateRecNo(OleVariant(F_DaoTable),BK);
                           End
                        Else
                           Begin
                             //**************************************** TOO Slow
                             While Not F_DaoTable.BOF Do
                               Begin
                                Inc(F_RecNo);
                                F_DaoTable.MovePrevious;
                               End;
                           End;
                      End;
                   //***********************************************************
                   PDaoInfo(TempBuffer+F_StartMyInfo)^.RecordNo:=F_RecNo;
                   if F_Bookmarkable Then
                      Begin
                       InternalMoveToBookmark(@PDaoInfo(TempBuffer+F_StartMyInfo)^.BookmarkData);
                      End
                   Else
                      Begin
                       F_DaoTable.MoveFirst;
                       OleVariant(F_DaoTable).Move(F_RecNo);
                      End;
                 End;
              F_PostMade:=False;
             End;
         End
      Else
         Begin
           //********************************************************** 2.1.2002
           if (F_TableType=dbOpenDynaset)
           OR (F_TableType=dbOpenSnapshot)
           OR (F_TableType=dbOpenDynamic) Then
              Begin
                 F_RecNo := F_DaoTable.AbsolutePosition;
              End;
           //*******************************************************************   
         End;
      Result:=F_RecNo;
    End;
 Inc(Result);
End;


Procedure TKADaoTable.SetRecNo(Value: Integer);
Var
 SaveState      : TDataSetState;
 SavePosition   : Integer;
 TempBuffer     : TRecordBuffer;
Begin
  CheckBrowseMode;
  CursorPosChanged;
  DoBeforeScroll;
  If ((F_Filtered) And (Assigned(F_OnFilterRecord))) Or (F_RangeFiltered) Then
     Begin
       SaveState:=SetTempState(dsBrowse);
       SavePosition:=F_RecNo;
       try
         TempBuffer:=AllocRecordBuffer;
         InternalFirst;
         Repeat
           Begin
             if GetRecord(TempBuffer,gmNext,True)=grOk Then
               Begin
                Dec(Value);
               End
             Else
               Begin
                 F_RecNo  := SavePosition;
                 Break;
               End;
           End;
         Until Value=0;
       Finally
         RestoreState(SaveState);
         FreeRecordBuffer(TempBuffer);
       End;
     End
  Else
     Begin
      F_RecNo := (Value-1);
      F_DaoTable.MoveFirst;
      OleVariant(F_DaoTable).Move(F_RecNo);
     End;
  Resync([rmExact,rmCenter]);
  DoAfterScroll;
end;

Procedure TKADaoTable.StringToList(Items: String; List: TStringList);
var
  X: Integer;
Begin
  For X:= 1 To Length(Items) Do If Items[X] = ';' Then Items[X]:= #13;
  List.Clear;
  List.Text:=Items;
  For X:= 0 To List.Count - 1 Do List[X]:= Trim(List[X]);
End;

Procedure TKADaoTable.VariantToList(Items: Variant; List: TStringList);
Var
   X    : Integer;
   V    : Variant;
   Count: Integer;
Begin
   List.Clear;
   if VarIsArray(Items) Then
      Begin
        Count:=(VarArrayHighBound(Items, 1) - VarArrayLowBound(Items, 1))+1;
        For X:=0 to Count-1 do
            Begin
             V:=Items[VarArrayLowBound(Items, 1) + X];
             if VarIsNull(V) Then
                List.Add('NULL')                         
             Else
                List.Add(VarAsType(V,VarString));
            End;
      End
   Else
      Begin
         V:=Items;
         if VarIsNull(V) Then
            List.Add('NULL')
         Else
            List.Add(VarAsType(V,VarString));
      End;
End;

Procedure TKADaoTable.AssignVarValue(Var V :Variant; const Value: TVarRec);
Begin
  with Value do
    case VType of
      vtInteger:
        V := VInteger;
      vtBoolean:
        V := VBoolean;
      vtChar:
        V := VChar;
      vtExtended:
        V := VExtended^;
      vtString:
        V := VString^;
      vtPointer:
        if VPointer <> nil then DatabaseError(E2027);
      vtPChar:
        if VPChar <> nil then DatabaseError(E2027);
      vtObject:
         DatabaseError('Invalid object');
      vtAnsiString:
        V := string(VAnsiString);
      vtCurrency:
        V := VCurrency^;
      vtVariant: 
        if not VarIsEmpty(VVariant^) then V := VVariant^;
    else
      DatabaseError(E2027);
    End;
End;

Function  TKADaoTable.BuildKeySQL(KN,KV:TStringList):String;
Var
 X  : Integer;
 S  : String;
 FT : TField;
Begin
S:='';
Result:='';
if KN.Count > 0 Then
     Begin
      For X:=0 To KN.Count-1 do
         Begin
          S:=S+'(';
          if F_UseBrackets Then
             S:=S+BracketField(KN.Strings[X])
          Else
             S:=S+KN.Strings[X];
          S:=S+' ';
          FT :=FieldByName(KN.Strings[X]);
          if KV.Strings[X]='NULL' Then S:= S + 'IS NULL'
          Else
          Case FT.DataType of
             ftBytes    :  Begin
                             if KV.Strings[X] = '' Then
                                Begin
                                  S := S + ' IS NULL';
                                End
                             Else
                                Begin
                                  if FT.ValidChars = GUID_VALID_CHARS Then
                                     S := S + ' = {guid '+KV.Strings[X]+'}'
                                  Else
                                     S := S + ' = "' + KV.Strings[X] + '"';
                                End;
                           End;
             ftString      ,
             ftWideString  ,
             ftWideMemo    ,
             ftBlob        ,
             ftMemo        : S := S + ' = "' + ChangeOnlyQuotes(KV.Strings[X]) + '"';
             ftBoolean     ,
             ftBCD         ,
             ftCurrency    ,
             ftFloat       ,
             ftSmallint    ,
             ftWord        ,
             ftAutoInc     ,
             ftInteger     : Begin
                              if KV.Strings[X]='' Then
                                 S := S + ' IS NULL'
                              Else
                                 S := S + ' = ' + ChangeCommas(KV.Strings[X]);
                             End;
             ftDate        : Begin
                               if KV.Strings[X]='' Then
                                 Begin
                                   S := S + ' IS NULL';
                                 End
                              Else
                                 Begin
                                   KV.Strings[X]:=RemoveNonDigitChars(KV.Strings[X]);
                                   S:= S + ' = #' + FormatDateTime('mm"/"dd"/"yyyy', StrToDateTime(KV.Strings[X])) + '#';
                                 End;
                             End;
             ftTime        : Begin
                               if KV.Strings[X]='' Then
                                 Begin
                                   S := S + ' IS NULL';
                                 End
                              Else
                                 Begin
                                   KV.Strings[X]:=RemoveNonDigitChars(KV.Strings[X]);
                                   S:= S + ' = #' + FormatDateTime('hh":"nn":"ss', StrToDateTime(KV.Strings[X])) + '#';
                                 End;
                             End;
             ftDateTime    : Begin
                               if KV.Strings[X]='' Then
                                 Begin
                                   S := S + ' IS NULL';
                                 End
                              Else
                                 Begin
                                   KV.Strings[X]:=RemoveNonDigitChars(KV.Strings[X]);
                                   S:= S + ' = #' + FormatDateTime('mm"/"dd"/"yyyy hh":"nn":"ss', StrToDateTime(KV.Strings[X])) + '#';
                                 End;
                             End;

             Else
             DatabaseError(E2028)
          End;
          S:=S+')';
          if (X < KN.Count-1) Then S:=S+' AND ';
         End;
     End;
 Result := S;
End;

Function  TKADaoTable.BuildLocateSQL(KN,KV:TStringList;Options: TLocateOptions):String;
Var
 X  : Integer;
 S  : String;
 FT : TField;
Begin
S:='';
Result:='';
if KN.Count > 0 Then
     Begin
      For X:=0 To KN.Count-1 do
         Begin
          S:=S+'(';
          FT :=FieldByName(KN.Strings[X]);
          if F_UseBrackets Then
             S:=S+BracketField(KN.Strings[X])
          Else
             S:=S+KN.Strings[X];
          S:=S+' ';
          if KV.Strings[X]='NULL' Then S:= S + 'IS NULL'
          Else
          Case FT.DataType of
             ftBytes    :  Begin
                             if KV.Strings[X] = '' Then
                                Begin
                                  S := S + ' IS NULL';
                                End
                             Else
                                Begin
                                  if FT.ValidChars = GUID_VALID_CHARS Then
                                     S := S + ' = {guid '+KV.Strings[X]+'}'
                                  Else
                                     S := S + ' = "' + KV.Strings[X] + '"';
                                End;
                           End;
             ftString     ,
             ftWideString ,
             ftWideMemo   ,
             ftBlob       ,
             ftMemo     :  Begin
                             if loCaseInsensitive in Options Then KV.Strings[X]:=AnsiLowerCase(KV.Strings[X]);
                             If loPartialKey in Options Then
                                Begin
                                  if loCaseInsensitive in Options Then
                                     Begin
                                       S:= S + ' LIKE LCASE("' + ChangeOnlyQuotes(ChangeQuotes(KV.Strings[X])) + '*")';
                                     End
                                  Else
                                     Begin
                                       S:= S + ' LIKE "' + ChangeOnlyQuotes(ChangeQuotes(KV.Strings[X])) + '*"';
                                     End;
                                End
                             Else
                                Begin
                                  if loCaseInsensitive in Options Then
                                     Begin
                                       S:= S + ' = LCASE("' + ChangeOnlyQuotes(KV.Strings[X]) + '")';
                                     End
                                   Else
                                     Begin
                                       S:= S + ' = "' + ChangeOnlyQuotes(KV.Strings[X]) + '"';
                                     End;
                                End;
                           End;
             ftBoolean  ,
             ftCurrency ,
             ftFloat    ,
             ftBCD      ,
             ftSmallint ,
             ftWord     ,
             ftAutoInc  ,
             ftInteger : Begin
                          if KV.Strings[X]='' Then
                             S := S + ' IS NULL'
                          Else
                             S := S + ' = ' + ChangeCommas(KV.Strings[X]);
                         End;
             ftDate    : Begin
                           if KV.Strings[X]='' Then
                              Begin
                                 S := S + ' IS NULL'
                              End
                           Else
                              Begin
                                KV.Strings[X]:=RemoveNonDigitChars(KV.Strings[X]);
                                S:= S + ' = #' + FormatDateTime('mm"/"dd"/"yyyy', StrToDateTime(KV.Strings[X])) + '#';
                              End;
                         End;
             ftTime    : Begin
                            if KV.Strings[X]='' Then
                              Begin
                                 S := S + ' IS NULL'
                              End
                           Else
                              Begin
                               KV.Strings[X]:=RemoveNonDigitChars(KV.Strings[X]);
                               S:= S + ' = #' + FormatDateTime('hh":"nn":"ss', StrToDateTime(KV.Strings[X])) + '#';
                              End;
                         End;
             ftDateTime: Begin
                            if KV.Strings[X]='' Then
                              Begin
                                 S := S + ' IS NULL'
                              End
                           Else
                              Begin
                               KV.Strings[X]:=RemoveNonDigitChars(KV.Strings[X]);
                               S:= S + ' = #' + FormatDateTime('mm"/"dd"/"yyyy hh":"nn":"ss', StrToDateTime(KV.Strings[X])) + '#';
                              End;
                         End;
             Else
             DatabaseError(E2029)
          End;
          S:=S+')';
          if (X < KN.Count-1) Then S:=S+' AND ';
         End;
     End;
 Result := S;
End;

Function  TKADaoTable.BuildDetailSQL:String;
Var
 X  : Integer;
 S  : String;
 FT : TField;
Begin
S:='';
Result:='';
if F_Master.Count <> F_Detail.Count Then
     Begin
       DatabaseError(E2030);
     End;
if F_Master.Count > 0 Then
     Begin
      For X:=0 To F_Master.Count-1 do
         Begin
          S:=S+'(';
          if F_UseBrackets Then
             S:=S+BracketField(F_Detail.Strings[X])
          Else
             S:=S+F_Detail.Strings[X];
          S:=S+' ';
          FT :=F_MasterLink.Dataset.FieldByName(F_Master.Strings[X]);
          if FT.IsNull then S:= S + 'IS NULL'
          Else
          Case FT.DataType of
             ftBytes    :  Begin
                             if FT.AsString = '' Then
                                Begin
                                  S := S + ' IS NULL';
                                End
                             Else
                                Begin
                                  if FT.ValidChars = GUID_VALID_CHARS Then
                                     S := S + ' = {guid '+GetGUIDAsString(FT.AsString)+'}'
                                  Else
                                     S := S + ' = "' + FT.AsString + '"';
                                End;
                           End;
             ftString     ,
             ftWideString ,
             ftWideMemo   ,
             ftBlob       ,
             ftMemo       : S := S + ' = "' + ChangeOnlyQuotes(FT.AsString) + '"';
             ftCurrency   ,
             ftFloat      ,
             ftBCD        ,
             ftSmallint   ,
             ftWord       ,
             ftAutoInc    ,
             ftInteger    : Begin
                              if FT.AsString='' Then
                                 S := S + ' IS NULL'
                              Else
                                 S := S + ' = ' + ChangeCommas(FT.AsString);
                            End;
             ftBoolean    : Begin
                              if FT.AsString='' Then
                                 S := S + ' IS NULL'
                              Else
                                 If FT.AsBoolean then S:= S + ' = True' Else S:= S + ' = False';
                            End;
             ftDate       : Begin
                              if FT.AsString='' Then
                                 S := S + ' IS NULL'
                              Else
                                 S := S + ' = #' + FormatDateTime('mm"/"dd"/"yyyy', FT.AsDateTime) + '#';
                            End;
             ftTime       : Begin
                              if FT.AsString='' Then
                                 S := S + ' IS NULL'
                              Else
                                 S := S + ' = #' + FormatDateTime('hh":"nn":"ss', FT.AsDateTime) + '#';
                            End;
             ftDateTime   : Begin
                              if FT.AsString='' Then
                                 S := S + ' IS NULL'
                              Else
                                 S := S + ' = #' + FormatDateTime('mm"/"dd"/"yyyy hh":"nn":"ss', FT.AsDateTime) + '#';
                            End;
             Else
                DatabaseError(E2031)
          End;
          S:=S+')';
          if (X < F_Master.Count-1) Then S:=S+' AND ';
         End;
     End;
 Result := S;
End;

//***************************************************************************************
Function TKADaoTable.GetDaoLastModifiedBookMark(RS:Variant):Integer;
Var
 TempBK : Pointer;
Begin
 Result:=0;
 if (RS.BOF) And (RS.EOF) Then Exit;
 if F_Bookmarkable Then
    Begin
      TempBK:=TVarData(RS.LastModified).VPointer;
      if Assigned (PSafeArray(TempBK)) Then
         Begin
           Result := PInteger(PSafeArray(TempBK)^.pvData)^;
         End
      Else
         Begin
           Result := GetDaoBookMark(RS);
         End;
    End
 Else
    Begin
      Result := 0;
    End;
End;


Function  TKADaoTable.GetDaoBookMark(RS:Variant):Integer;
Var
 TempBK : Pointer;
Begin
 Result:=0;
 if (RS.BOF) Or (RS.EOF) Then Exit;
 Try
  if F_Bookmarkable Then
    Begin
      TempBK:=TVarData(RS.Bookmark).VPointer;
      Result:=PInteger(PSafeArray(TempBK)^.pvData)^;
    End
  Else
    Begin
      Result := 0;
    End;
  Except
    InternalFirst;
  End;
End;


Function TKADaoTable.GetFieldIndexName(FiledName:String):String;
Var
  X,Y : Integer;
Begin
 if Assigned(F_Database) And (F_Database.Connected) Then
 Begin
 Try
  For X :=0 To F_Database.CoreDatabase.TableDefs[F_TableName].Indexes.Count-1 do
      Begin
        For Y := 0 To F_Database.CoreDatabase.TableDefs[F_TableName].Indexes.Item[X].Fields.Count-1 do
            Begin
              if AnsiCompareText(FiledName,F_Database.CoreDatabase.TableDefs[F_TableName].Indexes.Item[X].Fields.Item[Y].Name)=0 Then
                 Begin
                  Result :=F_Database.CoreDatabase.TableDefs[F_TableName].Indexes.Item[X].Name;
                  Exit;
                 End;
            End;
      End;
  Except
  End;
  End;
  Result := '';
End;

Function TKADaoTable.CheckFieldsInIndex(KF:TStringList):Boolean;
Var
  X,Y  : Integer;
  OK   : Boolean;
Begin
  Result := False;
  if F_IndexName='' Then Exit;
  if (NOT Assigned(F_Database))  Or (NOT F_Database.Connected) Then Exit;
  For X :=0 To KF.Count-1 do
      Begin
        OK :=False;
        For Y:=0 To F_Database.CoreDatabase.TableDefs[F_TableName].Indexes.Item[F_IndexName].Fields.Count-1 do
            Begin
             if AnsiCompareText(KF.Strings[X],F_Database.CoreDatabase.TableDefs[F_TableName].Indexes.Item[F_IndexName].Fields.Item[Y].Name)=0 Then OK :=True;
            End;
        if Not OK Then Exit;
      End;
  Result := True;
End;

//******************************************************************* 31.01.2002
Function TKADaoTable.GetUniqueIndexFields(Table : TKaDaoTable) : String;
Var
  X : integer;
Begin
  Result := '';
  Table.IndexDefs.Update;
  For X := 0 to Table.IndexDefs.Count - 1 do
      Begin
       if ixUnique in Table.IndexDefs.Items[X].Options then
          Begin
           Result := Table.IndexDefs.Items[X].Fields;
           System.Break;
          End;
      End;
End;

Function TKADaoTable.IsFieldUniqueIndex(Table : TKaDaoTable; FieldName : String ) : Boolean;
Var
  X : Integer;
Begin
  Result := False;
  Table.IndexDefs.Update;
  for X := 0 to Table.IndexDefs.Count -1 do
      Begin
  	if  (Table.IndexDefs.Items[X].Fields = FieldName)
        And (ixUnique in Table.IndexDefs.Items[X].Options ) Then
            Begin
             Result := true;
             System.Break;
            End;
      End;
end;
//******************************************************************* 31.01.2002



Function  TKADaoTable.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
Var
 KF       : TStringList;
 KV       : TStringList;
 X        : Integer;
 CR       : Integer;
 RI       : Integer;
 FN       : Integer;
 Find     : Boolean;
 S1,S2    : String;
 L        : Integer;
 Filter   : String;
 KVV      : Array[0..12] of OleVariant;
 IdxC     : Integer;
 IdxCT    : Integer;
 IndexOK  : Boolean;
 CompText : String;
 //*************************************
 BK       : Integer;
 TempRS   : OleVariant;
 APOK     : Boolean;
 IdxName  : String;
 //*************************************
Begin
 Result:=False;
 if IsEmpty Then Exit;
 if F_BatchMode  Then Exit;
 If ((F_Filtered) And (Assigned(F_OnFilterRecord))) Or (F_RangeFiltered) Then Exit;
 if (NOT Assigned(F_Database))  Or (NOT F_Database.Connected) Then Exit;
 KF :=  TStringList.Create;
 KV :=  TStringList.Create;
 Try
  StringToList(KeyFields,KF);
  VariantToList(KeyValues,KV);
  If (KF.Count <> KV.Count)  Then DatabaseError(E2032);
  //****************************************************************************
  APOK := False;
  if     (F_TableType=dbOpenDynaset)
      OR (F_TableType=dbOpenSnapshot)
      OR (F_TableType=dbOpenDynamic) Then APOK:=True;
  //****************************************************************************
  InternalSetToRecord(GetActiveRecordBuffer);
  CR:=F_RecNo;
  if F_Bookmarkable Then
     Begin
       //************************************************************ 13.02.2002
       if F_AutoFindIndex Then
          Begin
            IdxName   := FindGoodIndex('!'+KeyFields);
            if IdxName = '' Then IdxName := FindGoodIndex(KeyFields);
            if IdxName = '' Then IdxName := F_IndexName;
          End
       Else
          Begin
            IdxName := F_IndexName;
            if IdxName='' Then
               Begin
                IdxName   := FindGoodIndex('!'+KeyFields);
                if IdxName = '' Then IdxName := FindGoodIndex(KeyFields);
               End;
          End;
       //************************************************************ 13.02.2002
       IndexOK := (IdxName<>'');
       if (TableType=dbOpenTable) And (IndexOK) Then
          Begin
            //******************************************************************
            For X := 0 to 12 do KVV[X]:=NULL;
            IdxC  := F_Database.CoreDatabase.TableDefs[F_TableName].Indexes[IdxName].Fields.Count;
            IdxCT := 0;
            For X:=0 to IdxC-1 do
                Begin
                  L:=KF.IndexOf(F_Database.CoreDatabase.TableDefs[F_TableName].Indexes[IdxName].Fields.Item[X].Name);
                  if L <> -1 Then
                     Begin
                       KVV[X]:=KV.Strings[L];
                       IdxCT:=X+1;
                     End;
                End;
            //******************************************************************
            if IdxCT > 0 Then IdxC:=IdxCT;
            CompText := '=';
            if KF.Count <> F_Database.CoreDatabase.TableDefs[F_TableName].Indexes.Item[IdxName].Fields.Count Then CompText := '>=';
            if loPartialKey in Options then CompText := '>=';

            TempRS:=F_DaoTable.Clone;
            TempRS.Index:=IdxName;
            TempRS.MoveFirst;
            if IdxC=1 Then OleVariant(TempRS).Seek(CompText,KVV[0])
            Else
            if IdxC=2 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1])
            Else
            if IdxC=3 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2])
            Else
            if IdxC=4 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3])
            Else
            if IdxC=5 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4])
            Else
            if IdxC=6 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4],KVV[5])
            Else
            if IdxC=7 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4],KVV[5],KVV[6])
            Else
            if IdxC=8 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4],KVV[5],KVV[6],KVV[7])
            Else
            if IdxC=9 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4],KVV[5],KVV[6],KVV[7],KVV[8])
            Else
            if IdxC=10 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4],KVV[5],KVV[6],KVV[7],KVV[8],KVV[9])
            Else
            if IdxC=11 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4],KVV[5],KVV[6],KVV[7],KVV[8],KVV[9],KVV[10])
            Else
            if IdxC=12 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4],KVV[5],KVV[6],KVV[7],KVV[8],KVV[9],KVV[10],KVV[11])
            Else
            if IdxC=13 Then OleVariant(TempRS).Seek(CompText,KVV[0],KVV[1],KVV[2],KVV[3],KVV[4],KVV[5],KVV[6],KVV[7],KVV[8],KVV[9],KVV[10],KVV[11],KVV[12]);
            For X:=0 to 12 do Begin VarClear(KVV[X]); KVV[X]:=NULL; End;
          End
       Else
          Begin
            if (TableType=dbOpenTable) Then DatabaseError(E2062);
            //******************************************************************
            Filter := BuildLocateSQL(KF,KV,Options);
            TempRS := F_DaoTable.Clone;
            TempRS.MoveFirst;
            //OleVariant(TempRS).Move(CR); //09.04.2005 this is wrong assumption
            TempRS.FindFirst(Filter);
            //******************************************************************
          End;
       Find:=NOT TempRS.NoMatch;
       if (Find) Then
           Begin
             Result:= True;
             BK:=GetDaoBookMark(TempRS);
             CheckBrowseMode;
             CursorPosChanged;
             DoBeforeScroll;
             if APOK Then
                Begin
                  CR := TempRS.AbsolutePosition;
                End
             Else
                Begin
                   CR:=F_RecalculateRecNo(TempRS,BK);
                End;
             InternalMoveToBookmark(@BK);
             F_RecNo:=CR;
             //ClearBuffers;
             Resync([]);
             DoAfterScroll;
           End;
       TempRS.Close;
       VarClear(TempRS);
     End
  Else
     Begin
       CheckBrowseMode;
       CursorPosChanged;
       DoBeforeScroll;
       F_DaoTable.MoveFirst;
       Find:=False;
       RI:=0;
       While Not (F_DaoTable.EOF) Do
             Begin
              Find:=True;
              For X:=0 to KF.Count-1 do
                  Begin
                   FN:=Integer(KF.Objects[X])-1;
                   S1:=KV[X];
                   S2:=VarAsType(F_DaoTable.Fields.Item[FN].Value,VarString);
                   if loCaseInsensitive in Options Then
                       Begin
                        S1:=AnsiLowerCase(S1);
                        S2:=AnsiLowerCase(S2);
                       End;
                   if loPartialKey in Options Then
                       Begin
                        L:=Length(S1);
                        if S1[L]='*' Then System.Delete(S1,L,1);
                        if S1[1]='*' Then System.Delete(S1,1,1);
                        if Pos(S1,S2) = 0 Then Find:=False;
                       End
                   Else
                       Begin
                        if S1 <> S2 Then Find:=False;
                       End;
                   if NOT Find Then Break;
                  End;
              If Find Then
                 Begin
                  DoBeforeScroll;
                  F_RecNo:=RI;
                  Result:= True;
                  //ClearBuffers;
                  Resync([]);
                  DoAfterScroll;
                  Break;
                 End
              Else
                 Begin
                  Inc(RI);
                  F_DaoTable.MoveNext;
                 End;
             End;
       if Not(Find) Then
          Begin
            F_DaoTable.MoveFirst;
            OleVariant(F_DaoTable).Move(CR);
            //ClearBuffers;
            Resync([rmExact, rmCenter]);
          End;
       DoAfterScroll;
     End;
 Finally
  KV.Free;
  KF.Free;
 End; 
End;


Function  TKADaoTable.Find(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions;FindType:Integer): Boolean;
Var
 KF       : TStringList;
 KV       : TStringList;
 X        : Integer;
 CR       : Integer;
 Filter   : String;
 //*************************************
 BK       : Integer;
 TempRS   : OleVariant;
 APOK     : Boolean;
 //*************************************
Begin
 Result:=False;
 if IsEmpty Then Exit;
 If ((F_Filtered) And (Assigned(F_OnFilterRecord))) Or (F_RangeFiltered) Then Exit;
 KF :=  TStringList.Create;
 KV :=  TStringList.Create;
 Try
  StringToList(KeyFields,KF);
  VariantToList(KeyValues,KV);
  If (KF.Count <> KV.Count)  Then DatabaseError(E2032);
  For X:=0 To KF.Count-1 do KF.Objects[X]:=Pointer(FieldByName(KF[X]).FieldNo);
  //*****************************************************************************
  APOK := False;
  if     (F_TableType=dbOpenDynaset)
     OR (F_TableType=dbOpenSnapshot)
     OR (F_TableType=dbOpenDynamic) Then APOK:=True;
  //*****************************************************************************
  InternalSetToRecord(GetActiveRecordBuffer);
  CR:=F_RecNo;
  if F_Bookmarkable Then
    Begin
      Filter:=BuildLocateSQL(KF,KV,Options);
      TempRS:=F_DaoTable.Clone;
      TempRS.MoveFirst;
      OleVariant(TempRS).Move(CR);
      Case FindType of
           1 : TempRS.FindFirst(Filter);
           2 : TempRS.FindLast(Filter);
           3 : TempRS.FindNext(Filter);
           4 : TempRS.FindPrevious(Filter);
      End;
      if (Not TempRS.NoMatch) Then
          Begin
            Result:= True;
            BK:=GetDaoBookMark(TempRS);
            CheckBrowseMode;
            CursorPosChanged;
            DoBeforeScroll;
            if APOK Then
               Begin
                 CR := TempRS.AbsolutePosition;
               End
            Else
               Begin
                  CR:=F_RecalculateRecNo(TempRS,BK);
               End;
            InternalMoveToBookmark(@BK);
            F_RecNo:=CR;
            //ClearBuffers;
            Resync([rmExact, rmCenter]);
            DoAfterScroll;
          End;
      TempRS.Close;
      VarClear(TempRS);
    End
  Else
    Result:=False;
 Finally
  KV.Free;
  KF.Free;
 End;
End;

Procedure TKADaoTable.SetKeyFields(const KeyFields: string);
Begin
 StringToList(KeyFields,F_KeyFields);
End;

Function TKADaoTable.Find_First(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):Boolean;
Begin
  Result:=Find(KeyFields,KeyValues,Options,1);
End;

Function TKADaoTable.Find_Last(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):Boolean;
Begin
  Result:=Find(KeyFields,KeyValues,Options,2);
End;

Function TKADaoTable.Find_Next(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):Boolean;
Begin
  Result:=Find(KeyFields,KeyValues,Options,3);
End;

Function TKADaoTable.Find_Prior(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):Boolean;
Begin
  Result:=Find(KeyFields,KeyValues,Options,4);
End;

Procedure TKADaoTable.SetFindData(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions);
Begin
 F_FindKeyFields:=KeyFields;
 F_FindKeyValues:=KeyValues;
 F_FindOptions:=Options;
End;

Function TKADaoTable.FindRecord(Restart, GoForward: Boolean): Boolean;
Begin
   Result:=False;
   if F_FindKeyFields='' Then Exit;
   if VarIsNull(F_FindKeyValues) Then Exit;
   if (Restart) And (GoForward)         Then Result:=Find_First(F_FindKeyFields,F_FindKeyValues,F_FindOptions);
   if (Restart) And (NOT GoForward)     Then Result:=Find_Last(F_FindKeyFields,F_FindKeyValues,F_FindOptions);
   if (NOT Restart) And (GoForward)     Then Result:=Find_Next(F_FindKeyFields,F_FindKeyValues,F_FindOptions);
   if (NOT Restart) And (NOT GoForward) Then Result:=Find_Prior(F_FindKeyFields,F_FindKeyValues,F_FindOptions);
End;

//*************************************************************** Range Routines
Function TKADaoTable.CompareFieldsRange(B1, B2: String; FieldType: TFieldType):Integer;
Var
  BOOL1, BOOL2 : WordBool;
  DOUB1, DOUB2 : Double;
  SMAL1, SMAL2 : SmallInt;
  WORD1, WORD2 : Word;
  INTE1, INTE2 : Integer;
Begin
 Result := 0;
 Case FieldType of
      ftWideString,
      ftWideMemo :       Begin
                           Result := WideCompareText(B1, B2);
                         End;
      ftString,
      ftMemo     :      Begin
                           Result := AnsiCompareText(B1, B2);
                         End;
      ftBoolean  :       Begin
                           if AnsiLowerCase(B1) = 'true' Then BOOL1 := True Else BOOL1 := False;
                           if AnsiLowerCase(B2) = 'true' Then BOOL2 := True Else BOOL2 := False;
                           if BOOL1 > BOOL2 Then Result:=1
                           Else
                           if BOOL1 < BOOL2 Then Result:=-1;
                         End;
      ftCurrency,
      ftFloat    :       Begin
                           Try
                            DOUB1 := StrToFloat(B1);
                            DOUB2 := StrToFloat(B2);
                            if DOUB1 > DOUB2 Then Result:=1
                            Else
                            if DOUB1 < DOUB2 Then Result:=-1;
                           Except
                           End;
                         End;

      ftSmallInt :       Begin
                           Try
                            SMAL1 := SmallInt(StrToInt(B1));
                            SMAL2 := SmallInt(StrToInt(B2));
                            Result:=SMAL1-SMAL2;
                           Except
                           End;
                         End;

      ftWord     :       Begin
                           Try
                            WORD1 := Word(StrToInt(B1));
                            WORD2 := Word(StrToInt(B2));
                            Result:=WORD1-WORD2;
                           Except
                           End;
                         End;
      ftAutoInc,
      ftInteger  :       Begin
                           Try
                            INTE1 := LongInt(StrToInt(B1));
                            INTE2 := LongInt(StrToInt(B2));
                            Result:=INTE1-INTE2;
                           Except
                           End;
                         End;
      ftDate     :       Begin
                           Result := AnsiCompareText(B1, B2);
                         End;
      ftTime     :       Begin
                           Result := AnsiCompareText(B1, B2);
                         End;
      ftDateTime :       Begin
                           Result := AnsiCompareText(B1, B2);
                         End;
 End;
End;

Function TKADaoTable.CompareRecordsRange(B1,B2 : TRecordBuffer; CT : Integer) : Integer;
Var
 X       : Integer;
 F1,F2   : String;
Begin
 Result := 0;
 If (B1=Nil) Or (B2=nil) then Exit;
 For X := 0  to FieldCount-1 do
     Begin
       F1 := PDaoInfo(B1+F_StartMyInfo)^.RecordData.Strings[X];
       F2 := PDaoInfo(B2+F_StartMyInfo)^.RecordData.Strings[X];
       if (F1 <> '') And (F2 <> '') Then
          Begin
            Result := CompareFieldsRange(F1,F2,Fields[X].DataType);
          End
       Else
          Begin
            //*************** SET OUTSIDE RANGE IF THERE ARE NO VALUE TO COMPARE
            if F2 <> '' Then
               Begin
                 if (CT=1) Then Result:=-1
                 Else
                 if (CT=2) Then Result:=1;
               End;
          End;
       if (Result < 0) And (CT=1) Then Break;
       if (Result > 0) And (CT=2) Then Break;
     End;
End;

Function TKADaoTable.FilterRange(Buffer:TRecordBuffer): Boolean;
Var
 R1,R2 : Integer;
Begin
 R1 := CompareRecordsRange(Buffer,F_RangeStartBuffer,1);
 R2 := CompareRecordsRange(Buffer,F_RangeEndBuffer,2);
 Result := (R1 >=0) And (R2 <=0);
End;

Procedure TKADaoTable.ClearRange(Var Buffer:TRecordBuffer);
Var
  X : Integer;
Begin
  PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Clear;
  For X := 0 To FieldDefs.Count-1 do
      Begin
       PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.AddObject('',TObject(False));
      End;
  SetState(dsBrowse);
  //******************************************* 29.08.2001
  DataEvent(deDataSetChange, 0);
  //******************************************* 29.08.2001
End;

Procedure TKADaoTable.ApplyRange;
Var
 B1 : String;
 B2 : String;
Begin
 B1 := StrPas(PDaoInfo(F_RangeStartBuffer+F_StartMyInfo)^.RecordData.GetText);
 B2 := StrPas(PDaoInfo(F_RangeEndBuffer+F_StartMyInfo)^.RecordData.GetText);
 B1 :=Trim(B1);
 B2 :=Trim(B2);
 F_RangeFiltered := (B1 <> '') And (B2 <> '');
 F_RefreshRC     := True;
 SetState(dsBrowse);
 //******************************************* 29.08.2001
 DataEvent(deDataSetChange, 0);
 //******************************************* 29.08.2001
 First;
End;

Procedure TKADaoTable.CancelRange;
Begin
  F_RangeFiltered   := False;
  F_ActiveKeyBuffer := Nil;
  F_RefreshRC       := True;
  First;
  Resync([rmExact]);
End;

Procedure TKADaoTable.SetRange(const StartValues, EndValues : Array of Const);
var
   Maks  : Integer;
   Mini  : Integer;
   X     : Integer;
Begin
     CheckBrowseMode;
     //***************************************************** Setting Start Range
     SetRangeStart;
     Mini := High(StartValues);
     Maks := PDaoInfo(F_RangeStartBuffer+F_StartMyInfo)^.RecordData.Count;
     if Maks > Mini Then Maks := Mini;
     //For X := 0 to Maks do Fields[X].AssignValue(StartValues[X]);      ???????
     For X := 0 to Maks do IndexFields[X].AssignValue(StartValues[X]);
     //******************************************************* Setting End Range
     SetRangeEnd;
     Mini := High(StartValues);
     Maks := PDaoInfo(F_RangeEndBuffer+F_StartMyInfo)^.RecordData.Count;
     if Maks > Mini Then Maks := Mini;
     //For X := 0 to Maks do Fields[X].AssignValue(EndValues[X]);        ???????
     For X := 0 to Maks do IndexFields[X].AssignValue(EndValues[X]);
     //****************************************************** Applying the Range
     ApplyRange;
End;

Procedure TKADaoTable.SetRangeStart;
Begin
  ClearRange(F_RangeStartBuffer);
  F_ActiveKeyBuffer := F_RangeStartBuffer;
  SetState(dsSetKey);
  DataEvent(deDataSetChange, 0);
End;

Procedure TKADaoTable.SetRangeEnd;
Begin
  ClearRange(F_RangeEndBuffer);
  F_ActiveKeyBuffer := F_RangeEndBuffer;
  SetState(dsSetKey);
  DataEvent(deDataSetChange, 0);
End;

Procedure TKADaoTable.EditRangeStart;
Begin
 F_ActiveKeyBuffer := F_RangeStartBuffer;
 SetState(dsSetKey);
 DataEvent(deDataSetChange, 0);
End;

Procedure TKADaoTable.EditRangeEnd;
Begin
  F_ActiveKeyBuffer := F_RangeEndBuffer;
  SetState(dsSetKey);
  DataEvent(deDataSetChange, 0);
End;
//***************************************************************** Key Routines

Procedure TKADaoTable.SetKeyParam(const KeyFields: Array of String;const KeyValues: array of const);
Var
  X : Integer;
Begin
  F_KeyKeyFields:='';
  F_KeyKeyValues:=Null;
  For X:=0 to High(KeyFields) do
      Begin
        if X < High(KeyValues) Then
           F_KeyKeyFields := F_KeyKeyFields+KeyFields[X]+';'
        Else
           F_KeyKeyFields := F_KeyKeyFields+KeyFields[X];
       End;
  if High(KeyValues)=0 then
    Begin
      AssignVarValue(F_KeyKeyValues,KeyValues[0]);
    End
  Else
     Begin
       F_KeyKeyValues:=VarArrayCreate([0,High(KeyValues)],varVariant);
       For X:=0 to High(KeyFields) do AssignVarValue(F_KeyKeyValues,KeyValues[X]);
     End;
End;

Procedure TKADaoTable.CancelKey;
Var
  Buffer : TRecordBuffer;
  X      : Integer;
begin
     Buffer := F_KeyBuffer;
     PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Clear;
     For X := 0 To FieldDefs.Count-1 do
          Begin
            PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.AddObject('',TObject(False));
          End;
     F_ActiveKeyBuffer := Nil;
     F_KeyKeyFields    := '';
     VarClear(F_KeyKeyValues);
     F_KeyKeyValues    := Null;
     SetState(dsBrowse);
     //******************************************* 29.08.2001
     DataEvent(deDataSetChange, 0);
     //******************************************* 29.08.2001
     Resync([rmExact]);
end;

Procedure TKADaoTable.ClearKey;
Var
  Buffer : TRecordBuffer;
  X      : Integer;
begin
     Buffer := F_KeyBuffer;
     PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Clear;
     For X := 0 To FieldDefs.Count-1 do
          Begin
            PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.AddObject('',TObject(False));
          End;
     SetState(dsBrowse);
     //******************************************* 29.08.2001
     DataEvent(deDataSetChange, 0);
     //******************************************* 29.08.2001
end;

Procedure TKADaoTable.SetKey;
begin
     ClearKey;
     F_ActiveKeyBuffer := F_KeyBuffer;
     SetState(dsSetKey);
     DataEvent(deDataSetChange, 0);
end;

Procedure TKADaoTable.EditKey;
begin
     F_ActiveKeyBuffer := F_KeyBuffer;
     SetState(dsSetKey);
     DataEvent(deDataSetChange, 0);
end;

Function  TKADaoTable.GotoKey: Boolean;
Var
  Buffer    : TRecordBuffer;
  X         : Integer;
  Count     : Integer;
  NumFields : Integer;
  NF        : Integer;
  FF        : TField;
Begin
  Result := False;
  if State=dsSetKey Then
     Begin
      Buffer := GetActiveRecordBuffer;
      if Buffer=Nil Then Exit;
      F_KeyKeyFields:='';
      F_KeyKeyValues:=Null;
      Count := PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Count-1;
      NumFields := 0;
      For X := 0 To Count Do
          Begin
            if PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X] <> '' Then
               Begin
                 F_KeyKeyFields := F_KeyKeyFields+FieldDefs[X].Name+';';
                 Inc(NumFields);
               End;
          End;
       if NumFields > 1 Then F_KeyKeyValues:=VarArrayCreate([0,NumFields-1],varVariant);
       NF:=0;
       For X := 0 To Count Do
          Begin
            if PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X] <> '' Then
               Begin
                 if NumFields > 1 Then F_KeyKeyValues[NF]:=PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X]
                 Else F_KeyKeyValues :=PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X];
                 //************************************************** 29.08.2001
                 FF:=FindField(FieldDefs[X].Name);
                 if (FF <> Nil) And ((FF.DataType=ftDateTime) or (FF.DataType=ftDate) or (FF.DataType=ftTime)) Then
                    Begin
                     if NumFields > 1 Then
                        F_KeyKeyValues[NF]:=ComposeDateTimeVariant(PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X])
                     Else
                        F_KeyKeyValues:=ComposeDateTimeVariant(PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X]);
                    End;
                 //************************************************** 29.08.2001
                 Inc(NF);
               End;
          End;
      SetState(dsBrowse);
      DataEvent(deDataSetChange, 0);
      if (TableType=dbOpenDynaset) or (TableType=dbOpenSnapshot) Then
         Begin
            Result := Find(F_KeyKeyFields,F_KeyKeyValues,[],3);
            if Not Result Then
               Result := Find(F_KeyKeyFields,F_KeyKeyValues,[],1);
         End;
      if (TableType=dbOpenTable) And (F_IndexName <> '') Then
          Begin
            Result := Locate(F_KeyKeyFields,F_KeyKeyValues,[]);
          End;
      if (Not Result) And (Not ISEmpty) Then Resync([]);
     End
  Else
     Begin
       if F_KeyKeyFields = '' Then Exit;
       if (TableType=dbOpenDynaset) or (TableType=dbOpenSnapshot) Then
         Begin
          Result := Find(F_KeyKeyFields,F_KeyKeyValues,[],3);
          if Not Result Then
             Result := Find(F_KeyKeyFields,F_KeyKeyValues,[],1);
         End;
       if (TableType=dbOpenTable) And (F_IndexName <> '') Then
          Begin
            Result := Locate(F_KeyKeyFields,F_KeyKeyValues,[]);
          End;
     End;
  VarClear(F_KeyKeyValues);
  F_KeyKeyValues:=Null;
End;

Procedure  TKADaoTable.GotoNearest;
Var
  Buffer    : TRecordBuffer;
  X         : Integer;
  Count     : Integer;
  NumFields : Integer;
  NF        : Integer;
  FF        : TField;
Begin
  if State=dsSetKey Then
     Begin
      Buffer := GetActiveRecordBuffer;
      if Buffer=Nil Then Exit;
      F_KeyKeyFields:='';
      F_KeyKeyValues:=Null;
      Count := PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Count-1;
      NumFields := 0;
      For X := 0 To Count Do
          Begin
            if PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X] <> '' Then
               Begin
                 if X < Count Then
                    F_KeyKeyFields := F_KeyKeyFields+FieldDefs[X].Name+';'
                 Else
                    F_KeyKeyFields := F_KeyKeyFields+FieldDefs[X].Name;
                 Inc(NumFields);
               End;
          End;
       if NumFields > 1 Then F_KeyKeyValues:=VarArrayCreate([0,NumFields-1],varVariant);
       NF:=0;
       For X := 0 To Count Do
          Begin
            if PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X] <> '' Then
               Begin
                 if NumFields > 1 Then F_KeyKeyValues[NF]:=PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X]
                 Else F_KeyKeyValues :=PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X];
                 //************************************************** 29.08.2001
                 FF:=FindField(FieldDefs[X].Name);
                 if (FF <> Nil) And ((FF.DataType=ftDateTime) or (FF.DataType=ftDate) or (FF.DataType=ftTime)) Then
                    Begin
                     if NumFields > 1 Then
                        F_KeyKeyValues[NF]:=ComposeDateTimeVariant(PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X])
                     Else
                        F_KeyKeyValues:=ComposeDateTimeVariant(PDaoInfo(Buffer+F_StartMyInfo)^.RecordData.Strings[X]);
                    End;
                 //************************************************** 29.08.2001
                 Inc(NF);
               End;
          End;
      SetState(dsBrowse);
      DataEvent(deDataSetChange, 0);
      Find_NearestEx(F_KeyKeyFields,F_KeyKeyValues);
     End
  Else
     Begin
       if F_KeyKeyFields = '' Then Exit;
       Find_NearestEx(F_KeyKeyFields,F_KeyKeyValues);
     End;
  VarClear(F_KeyKeyValues);
  F_KeyKeyValues:=Null;   
End;

Function TKADaoTable.FindKey(const KeyValues: array of const):Boolean;
Begin
 Result:=Seek_NearestEx(KeyValues,'=');
End;

Function TKADaoTable.FindKeyEx(const KeyValues: array of const):Boolean;
Begin
 Result:=Seek_NearestEx(KeyValues,'>=');
End;

//******************************************************************************

Function TKADaoTable.Find_NearestEx(const KeyFields: string; const KeyValues: Variant):Boolean;
Var
  Options:TLocateOptions;
Begin
  Options:=[loCaseInsensitive,loPartialKey];
  Result:=Find(KeyFields,KeyValues,Options,1);
  if Not Result Then Result:=Find(KeyFields,KeyValues,Options,3);
End;

Function TKADaoTable.Find_Nearest(const KeyValues: array of const):Boolean;
Var
  KF         : String;
  KV         : Variant;
  KT         : Variant;
  X          : Integer;
Begin
  KF:='';
  For X:=0 to High(KeyValues) do
      Begin
        if X < High(KeyValues) Then KF := KF+F_KeyFields.Strings[X]+';' Else  KF := KF+F_KeyFields.Strings[X];
       End;
  if High(KeyValues)=0 then
    Begin
      AssignVarValue(KV,KeyValues[0]);
    End
  Else
     Begin
       KV:=VarArrayCreate([0,High(KeyValues)],varVariant);
       For X:=0 to High(KeyValues) do
           Begin
            AssignVarValue(KT,KeyValues[X]);
            KV[X]:=KT;
           End;
     End;
  Result:=Find_NearestEx(KF,KV);
  VarClear(KV);
  KV:=NULL;
End;


Function TKADaoTable.Seek_NearestEx(const KeyValues: array of const; SeekType:String):Boolean;
Var
 KV     : Variant;
 KT     : Variant;
 X      : Integer;
 CR     : Integer;
 NumVals: Integer;
 //*************************************
 BK       : Integer;
 TempRS   : OleVariant;
 //*************************************
Begin
 Result:=False;
 if F_IndexName='' Then Exit;
 if IsEmpty Then Exit;
 If ((F_Filtered) And (Assigned(F_OnFilterRecord))) Or (F_RangeFiltered) Then Exit;
 if High(KeyValues)=0 then
    Begin
      NumVals:=1;
      AssignVarValue(KV,KeyValues[0]);
    End
  Else
     Begin
       KV:=VarArrayCreate([0,High(KeyValues)],varVariant);
       NumVals:=High(KeyValues)+1;
       For X:=0 to High(KeyValues) do
           Begin
            AssignVarValue(KT,KeyValues[X]);
            KV[X]:=KT;
           End;
     End;
     InternalSetToRecord(GetActiveRecordBuffer);
     CR:=F_RecNo;
     TempRS:=F_DaoTable.Clone;
     TempRS.Index:=F_IndexName;
     TempRS.MoveFirst;
     TempRS.Move(CR);
     if NumVals=1 Then TempRS.Seek(SeekType,KV)
     Else
     if NumVals=2 Then TempRS.Seek(SeekType,KV[0],KV[1])
     Else
     if NumVals=3 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2])
     Else
     if NumVals=4 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3])
     Else
     if NumVals=5 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4])
     Else
     if NumVals=6 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4],KV[5])
     Else
     if NumVals=7 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4],KV[5],KV[6])
     Else
     if NumVals=8 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4],KV[5],KV[6],KV[7])
     Else
     if NumVals=9 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4],KV[5],KV[6],KV[7],KV[8])
     Else
     if NumVals=10 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4],KV[5],KV[6],KV[7],KV[8],KV[9])
     Else
     if NumVals=11 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4],KV[5],KV[6],KV[7],KV[8],KV[9],KV[10])
     Else
     if NumVals=12 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4],KV[5],KV[6],KV[7],KV[8],KV[9],KV[10],KV[11])
     Else
     if NumVals=13 Then TempRS.Seek(SeekType,KV[0],KV[1],KV[2],KV[3],KV[4],KV[5],KV[6],KV[7],KV[8],KV[9],KV[10],KV[11],KV[12])
     Else
        DatabaseError(E2033);
     VarClear(KV);
     KV := NULL;
     if (Not TempRS.NoMatch) Then
          Begin
            Result:= True;
            BK:=GetDaoBookMark(TempRS);
            CheckBrowseMode;
            CursorPosChanged;
            DoBeforeScroll;
            CR:=F_RecalculateRecNo(TempRS,BK);
            InternalMoveToBookmark(@BK);
            F_RecNo:=CR;
            //ClearBuffers;
            Resync([rmExact, rmCenter]);
            DoAfterScroll;
          End;
     TempRS.Close;
     VarClear(TempRS);
End;

Function TKADaoTable.Seek_Nearest(const KeyValues: array of const):Boolean;
Begin
 Result:=Seek_NearestEx(KeyValues,'>=');
End;

Procedure TKADaoTable.FindNearest(const KeyValues: array of const);
Begin
 if Seek_NearestEx(KeyValues,'>=')=False Then
    Begin
    End;
End;


Function  TKADaoTable.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
Var
 KF     : TStringList;
 KV     : TStringList;
 RF     : TStringList;
 {$IFDEF DYNADAO}
 RS     : OleVariant;
 TempRS : OleVariant;
 {$ELSE}
 RS     : Recordset;
 TempRS : Recordset;
 {$ENDIF}
 FT      : String;
 X       : Integer;
 FN      : Integer;
 FF      : TField;
 HasLKF  : Boolean;
Begin
 Result := False;
 if IsEmpty Then Exit;
 if F_BatchMode  Then Exit;
 KF := TStringList.Create;
 KV := TStringList.Create;
 RF := TStringList.Create;
 Try
  StringToList(KeyFields,KF);
  VariantToList(KeyValues,KV);
  StringToList(ResultFields,RF);
  if (KF.Count <> KV.Count)  Then DatabaseError(E2032);
  if (RF.Count=0) Or (ResultFields='') Then DatabaseError(E2034);
  HasLKF := False;
  For X:=0 To RF.Count-1 do
     Begin
       FF := FindField(RF.Strings[X]);
       if FF <> Nil Then
          Begin
           FN := FF.FieldNo;
           RF.Objects[X]:=Pointer(FN);
           if (FF.FieldKind<>fkData) Then HasLKF := True;
          End
       Else
          Begin
            DatabaseError(E2070+RF.Strings[X]);
          End;
     End;
  RS:=F_DaoTable;
  FT:=F_DaoTable.Filter;
  RS.Filter:=BuildKeySQL(KF,KV);
  F_Database.Idle;
  TempRS:=RS.OpenRecordset(dbOpenSnapshot,dbReadOnly);
  If Not(TempRS.EOF and TempRS.BOF) then
    Begin
       TempRS.MoveFirst;
       if HasLKF Then
           Begin
             //*********************************** We have Calc or Lookup fields
             InternalFillRecordData(TempRS,False, TempBuffer);
             SetTempState(dsCalcFields);
             Try
              CalculateFields(TempBuffer);
              if RF.Count=1 Then
               Begin
                 Result := FieldValues[ResultFields];
               End
             Else
               Begin
                 Result := VarArrayCreate([0,RF.Count - 1], varVariant);
                 For X := 0 To RF.Count-1 do
                    Begin
                     Result[X] := FieldValues[RF.Strings[X]];
                    End;
               End;
             Finally
              RestoreState(dsBrowse);
             End;
           End
       Else
           Begin
            //************************************* Only DAO Fields so go faster
            if RF.Count=1 Then
               Begin
                 FN     := Integer(RF.Objects[0])-1;
                 Result := TempRS.Fields.Item[FN].Value;
               End
            Else
               Begin
                 Result := VarArrayCreate([0,RF.Count - 1], varVariant);
                 For X  :=0 To RF.Count-1 do
                     Begin
                      FN       := Integer(RF.Objects[X])-1;
                      Result[X]:= TempRS.Fields.Item[FN].Value
                    End;
               End;
           End;
    End;
  TempRS.Close;
  {$IFDEF DYNADAO}
  VarClear(TempRS);
  {$ELSE}
  TempRS:=Nil;
  {$ENDIF}
  RS.Filter:=FT;
 Finally
  RF.Free;
  KV.Free;
  KF.Free;
 End; 
End;

Procedure TKADaoTable.RefreshLookups;
Var
  X : Integer;
Begin
  if NOT F_Active then Exit;
  For X := 0 to FieldCount-1 do
     Begin
       if (Fields[X].FieldKind=fkLookup) And (Fields[X].LookupCache) Then
          Fields[X].RefreshLookupList;
     End;
End;

{*************************************************************** DAO FIELD TYPES
  dbBoolean = 1;
  dbByte = 2;
  dbInteger = 3;
  dbLong = 4;
  dbCurrency = 5;
  dbSingle = 6;
  dbDouble = 7;
  dbDate = 8;
  dbBinary = 9;
  dbText = 10;
  dbLongBinary = 11;
  dbMemo = 12;
  dbGUID = 15;
  dbBigInt = 16;
  dbVarBinary = 17;
  dbChar = 18;
  dbNumeric = 19;
  dbDecimal = 20;
  dbFloat = 21;
  dbTime = 22;
  dbTimeStamp = 23;
//******************************************************************************
}

Function TKADaoTable.CreateField(FieldName:String;FieldType:Integer;FiledSize:Integer):Boolean;
Var
  FN,FT,FS,FI,FR:Variant;
Begin
  Result:=False;
  if F_TableName='' Then
     Begin
       DatabaseError(E2035);
       Exit;
     End;
  if Not Assigned(F_Database) Then
         Begin
           DatabaseError(E2036);
           Exit;
         End;
   if Not (F_Database.Connected) Then
         Begin
           DatabaseError(E2037);
           Exit;
         End;
  if F_Active Then
     Begin
      DatabaseError(E2038);
      Exit;
     End;
  FN:=VarArrayCreate([0, 0], varOleStr);
  FT:=VarArrayCreate([0, 0], varInteger);
  FS:=VarArrayCreate([0, 0], varInteger);
  FI:=VarArrayCreate([0, 0], varInteger);
  FR:=VarArrayCreate([0, 0], varInteger);
  FN[0]:=FieldName;
  FT[0]:=FieldType;
  FS[0]:=DaoSizeToBDESize(FieldType,FiledSize);
  FI[0]:=0;
  FR[0]:=0;
  Try
    Result:=F_Database.AddFieldsToTable(F_TableName,FN,FT,FS,FI,FR);
  Except
    Exit;
  End;
  VarClear(FN);FN:=NULL;
  VarClear(FT);FT:=NULL;
  VarClear(FS);FS:=NULL;
  VarClear(FI);FI:=NULL;
  VarClear(FR);FR:=NULL;
End;

Function TKADaoTable.CreateIndex(FieldName:String;IndexType:Integer):Boolean;
Begin
  Result:=False;
  if F_TableName='' Then
     Begin
       DatabaseError(E2039);
       Exit;
     End;
  if Not Assigned(F_Database) Then
     Begin
       DatabaseError(E2040);
       Exit;
     End;
   if Not (F_Database.Connected) Then
         Begin
           DatabaseError(E2041);
           Exit;
         End;
  if F_Active Then
     Begin
       DatabaseError(E2042);
       Exit;
     End;
  Result:=F_Database.CreateIndex(F_TableName,FieldName,IndexType);
End;

Function TKADaoTable.DeleteField(FieldName:String):Boolean;
Begin
  Result:=False;
  if F_TableName='' Then
     Begin
       DatabaseError(E2043);
       Exit;
     End;
  if Not Assigned(F_Database) Then
     Begin
       DatabaseError(E2044);
       Exit;
     End;
  if Not (F_Database.Connected) Then
     Begin
       DatabaseError(E2045);
       Exit;
     End;
  if F_Active Then
     Begin
       DatabaseError(E2046);
       Exit;
     End;
  Try
    F_Database.DeleteField(F_TableName,FieldName);
  Except
    Exit;
  End;
  Result:=True;
End;

Function TKADaoTable.DeleteIndex(FieldName:String):Boolean;
Begin
  Result:=False;
  if F_TableName='' Then
     Begin
       DatabaseError(E2047);
       Exit;
     End;
  if Not Assigned(F_Database) Then
     Begin
       DatabaseError(E2048);
       Exit;
     End;
  if F_Active Then
     Begin
       DatabaseError(E2049);
       Exit;
     End;
  if Not (F_Database.Connected) Then
     Begin
       DatabaseError(E2050);
       Exit;
     End;

  Try
    F_Database.DeleteIndexByFieldName(F_TableName,FieldName);
  Except
    Exit;
  End;
  Result:=True;
End;

Function TKADaoTable.EmptyTable:Boolean;
Begin
 Result := True;
 if IsEmpty Then Exit;
 if F_ReadOnly Then DatabaseError(E2064);
 BatchMode := True;
 Try
   First;
   While NOT EOF do
     Begin
       F_InPost := True;
       Delete;
       F_InPost := False;
     End;
  CursorPosChanged;
  Resync([]);
 Finally
   BatchMode:=False;
   F_InPost := False;
   Result := Not IsEmpty;
 End;
End;

//******************************************************************************
Procedure TKADaoTable.CreateTable;
Var
 TM : TKadaoTableManager;
Begin
 if F_Active Then DatabaseError(E2066);
 if F_TableName='' Then DatabaseError(E2067);
 TM := TKADaoTableManager.Create(F_Database);
 Try
  TM.TableName:=F_TableName;
  TM.FieldDefs.Assign(Self.FieldDefs);
  TM.IndexDefs.Assign(Self.IndexDefs);
  TM.CreateTable;
 Finally
  TM.Free;
 End;
End;

Procedure TKADaoTable.AppendTable;
Var
 TM : TKadaoTableManager;
Begin
 if F_Active Then DatabaseError(E2068);
 if F_TableName='' Then DatabaseError(E2069);
 TM := TKADaoTableManager.Create(F_Database);
 Try
  TM.TableName:=F_TableName;
  TM.FieldDefs.Assign(Self.FieldDefs);
  TM.IndexDefs.Assign(Self.IndexDefs);
  TM.AppendTable;
 Finally
  TM.Free;
 End;
End; 
//******************************************************************************

Function TKADaoTable.InsertSQLString(MDString: String): String;
Begin
  Result:='';
  if F_Filtered Then Result:= Filter;
  if MDString <> '' then
    Begin
      if Result <> '' Then
         Result := '('+MDString+') AND ('+Result+')'
      Else
         Result := MDString;
    End;
End;

//******************************************************************************
//*                  Master/Detail Handling
//******************************************************************************
Function  TKADaoTable.F_Get_MasterSource : TDataSource;
Begin
 Result:= F_MasterLink.DataSource;
End;

Procedure TKADaoTable.F_Set_MasterSource(Value: TDataSource);
Begin
 if IsLinkedTo(Value) then DatabaseError(E2057);
 if (Value=Nil) Then MasterFields.Clear;
 F_MasterLink.DataSource:= Value;
 if (Active) Then
     Begin
       CheckBrowseMode;
       ClearBuffers;
       CloseDaoRecordset;
       OpenDaoRecordset;
       ActivateBuffers;
       First;
     End;
End;

Procedure TKADaoTable.F_ProcessMasterFields(Value:TStrings);
Var
  X                       : Integer;
  I                       : Integer;
  S                       : String;
  MasterField,DetailField : String;
  FieldNames              : String;
Begin
  F_Detail.Clear;
  F_Master.Clear;
  if (Value.Count=1) And (Pos(';',Value.Strings[0]) > 0) Then
     Begin
       S := Value.Strings[0];
       Repeat
        I := Pos(';',S);
        if I > 0 Then
           Begin
            DetailField:=Copy(S,1,I-1);
            System.Delete(S,1,I);
           End
        Else
           Begin
            DetailField:=S;
           End;
        if Length(DetailField) > 0 Then
           Begin
            MasterField:=DetailField;
            F_Detail.Add(DetailField);
            F_Master.Add(MasterField);
           End;
       Until I = 0;
     End
  Else
  For X:=0 to Value.Count-1 do
      Begin
        S := Value.Strings[X];
        I := Pos(' -> ',S);
        if I > 0 Then
        Begin
         DetailField:=Copy(S,1,I-1);
         System.Delete(S,1,I+Length(' -> ')-1);
         MasterField:=S;
         F_Detail.Add(DetailField);
         F_Master.Add(MasterField);
        End;
      End;
  FieldNames:='';
  For X := 0 To F_Detail.Count-1 do
      Begin
        if X < F_Detail.Count-1 Then
           FieldNames:=FieldNames+F_Master.Strings[X]+';'
        Else
           FieldNames:=FieldNames+F_Master.Strings[X];
      End;
  F_MasterLink.FieldNames:=FieldNames;
End;

Procedure TKADaoTable.F_Set_MasterFields(Value:TStrings);
Begin
 F_MasterFields.SetText(Value.GetText);
 if (Active) Then
     Begin
       CheckBrowseMode;
       ClearBuffers;
       CloseDaoRecordset;
       OpenDaoRecordset;
       ActivateBuffers;
       First;
     End;
End;

Procedure TKADaoTable.F_Set_Master(Value:TStrings);
Begin
 F_Master.SetText(Value.GetText);
End;

Procedure TKADaoTable.F_Set_Detail(Value:TStrings);
Begin
 F_Detail.SetText(Value.GetText);
End;


Procedure TKADaoTable.MasterDatasetChanged;
Begin
  if csDestroying in ComponentState then EXIT;
  F_MDisabled := Not (F_MasterLink.Active);
  if  (MasterSource <> NIL)
  And (Not MasterSource.DataSet.ControlsDisabled)
  And (Not ControlsDisabled)
  And (Not F_MDisabled) then
  Begin
  //***************************************************************** 28.01.2002
  if NOT MasterSource.Enabled    Then Exit;
  if MasterSource.State = dsEdit Then Exit;
  //***************************************************************** 28.01.2002
  if F_Master.Count > 0 Then
     Begin
      //*************************************************
      CheckBrowseMode;
      ClearBuffers;
      //*************************************************
      CloseDaoRecordset;
      if F_SQL.Count > 0 Then
         Begin
           OpenDaoRecordset
         End
       Else
         Begin
           Try
             ReOpenDaoRecordset;
           Except
             OpenDaoRecordset;
           End;
          End;
      ActivateBuffers;
      //*************************************************
      First;
      //*************************************************
     End;
  End;
End;


Procedure TKADaoTable.RefreshQueryParams;
var
  DataSet   : TDataSet;
  {$IFDEF USEPARAMS}{$IFNDEF VER100}{$IFNDEF VER110}
  X         : Integer;
  TempParam : TParam;
  {$ENDIF}{$ENDIF}{$ENDIF}
Begin
  Try
    if F_MasterLink.DataSource <> nil then
       Begin
        DataSet := F_MasterLink.DataSource.DataSet;
        if (DataSet <> Nil)
            And (DataSet.Active)
            And (DataSet.State <> dsSetKey) Then
              Begin
                {$IFDEF USEPARAMS}{$IFNDEF VER100}{$IFNDEF VER110}
                If ((F_ParamCheck) And (F_Params.Count > 0)) Then
                    Begin
                      For X := 0 to F_MasterLink.DataSource.DataSet.Fields.Count - 1 do
                        Begin
                          TempParam := F_Params.FindParam(F_MasterLink.DataSource.DataSet.Fields[X].FieldName);
                          if TempParam <> Nil Then
                             Begin
                               if TempParam.DataType=ftUnknown Then TempParam.DataType:=F_MasterLink.DataSource.DataSet.Fields[X].DataType;
                               TempParam.Assign(F_MasterLink.DataSource.DataSet.Fields[X]);
                             End;
                        End;
                    End;
                {$ENDIF}{$ENDIF}{$ENDIF}
              End;
       End;
  Finally
  End;
End;


Procedure TKADaoTable.UpdateFromMaster;
Var
  X         : Integer;
  TempField : TField;
Begin
  For X := 0 to F_MasterLink.Fields.Count - 1 do
      Begin
       TempField := FieldByName(F_Detail.Strings[X]);
       TempField.Assign(TField(F_MasterLink.Fields[X]));
      End;
End;

Procedure TKADaoTable.DoOnNewRecord;
begin
  If (F_MasterLink.Active) And (F_MasterLink.Fields.Count>0) Then
     Begin
      UpdateFromMaster;
     End;
  inherited DoOnNewRecord;
end;

Procedure TKADaoTable.MasterChanged(Sender: TObject);
Begin
 if not Active then Exit;
 CheckBrowseMode;
 If (F_MasterLink.Active) And (F_MasterLink.Fields.Count>0)  Then
     Begin                                                              
      if (F_SQL.Count > 0) Then RefreshQueryParams;
      MasterDatasetChanged;
     End;
End;

Procedure TKADaoTable.MasterDisabled(Sender: TObject);
Begin
 CheckBrowseMode;
 F_MDisabled := Not (F_MasterLink.Active);
End;
//******************************************************************************
//*                         Blob Stream Handling
//******************************************************************************
Function TKADaoTable.BlobToString(Field:TBlobField; Data:OleVariant; DataSize:Integer):AnsiString;
Var
   P      : PAnsiChar;
   S      : WideString;
Begin
  //***************************************************************** 22.09.2001
  Result := '';
  if VarIsNull(Data)  Then Exit;
  if VarIsEmpty(Data) Then Exit;
  if DataSize=0       Then Exit;
  //****************************************************************************
  if (Field.BlobType=ftMemo) Then
     Begin
       Result:=Data;
     End
  Else
  if (Field.BlobType=ftWideMemo) Then
     Begin
       SetLength(Result,DataSize);
       S := Data;
       Move(S[1],Result[1],DataSize);
     End
  Else
     Begin
       P:=VarArrayLock(Data);
       SetString(Result,P,DataSize);
       VarArrayUnlock(Data);
     End;
End;

Function TKADaoTable.StringToBlob(Field:TBlobField; Data:AnsiString):OleVariant;
Var
   DataSize : Integer;
   P        : PAnsiChar;
   pData    : PAnsiChar;
   S        : WideString;
Begin
   if (Field.DataType=ftMemo) Then
      Begin
        Result := Data;
      End
   Else
   if (Field.DataType=ftWideMemo) Then
      Begin
        SetLength(S,Length(Data) DIV 2);
        Move(Data[1],S[1],Length(Data));
        Result := S;
      End
   Else
      Begin
        DataSize := Length(Data);
        Result := VarArrayCreate([0,DataSize-1],VarByte);
        P := VarArrayLock(Result);
        pData := PAnsiChar(Data);
        Move(pData[0],P[0],DataSize);
        VarArrayUnlock(Result);
     End;
End;

Constructor TKBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
Var
   RD     : OleVariant;
   RS     : Integer;
   DInfo  : TDaoInfo;
   TempBK : Integer;
Begin
     F_BlobData := '';
     F_BlobSize := 0;
     Size       := F_BlobSize;
     F_Position := 0;
     F_Mode     := Mode;
     F_Field    := Field;
     F_Opened   := True;
     F_DataSet  := F_Field.DataSet as TKADaoTable;
     F_Buffer   := F_DataSet.GetActiveRecordBuffer;
     //************************************************** Table is empty so exit
     if F_Buffer = Nil Then Exit;
     //*************************************************************************
     if Mode = bmWrite then
        Begin
          if F_DataSet.ReadOnly Then DatabaseError(E2056);
          Truncate;
        End
     Else
     if Not F_Field.Modified Then
        Begin
           DInfo := PDaoInfo(F_Buffer+F_DataSet.F_StartMyInfo)^;
           //*******************************************************************
           //              CACHED MEMOS HANDLING
           //*******************************************************************
           if  ((Field.DataType = ftMemo) or (Field.DataType = ftWideMemo))
           And (F_Dataset.F_CacheMemos)  Then
               Begin
                 F_BlobData:=DInfo.RecordData.Strings[F_Field.FieldNo-1];
                 if (DInfo.RecordNo=-1) And (F_BlobData='') Then
                    F_BlobData := F_DataSet.F_DefaultValues.Strings[F_Field.FieldNo-1];
                 F_BlobSize:=Length(F_BlobData);
                 Size := F_BlobSize;
                 Exit;
               End;
           //*******************************************************************
           //              CACHED BLOBS HANDLING
           //*******************************************************************
           if  (Field.DataType = ftBlob)
           And (F_Dataset.F_CacheBlobs)  Then
               Begin
                 F_BlobData:=DInfo.RecordData.Strings[F_Field.FieldNo-1];
                 if (DInfo.RecordNo=-1) And (F_BlobData='') Then
                    F_BlobData := F_DataSet.F_DefaultValues.Strings[F_Field.FieldNo-1];
                 F_BlobSize:=Length(F_BlobData);
                 Size := F_BlobSize;
                 Exit;
               End;
            //*******************************************************************
           //            Save Current Position and go to the desired row
           //*******************************************************************
           if (F_DataSet.State = dsBrowse) Then
               Begin
                  if DInfo.BookmarkData > 0 Then
                     Begin
                       TempBK := F_DataSet.GetDaoBookmark(F_DataSet.F_DaoTable);
                       F_DataSet.InternalMoveToBookmark(@DInfo.BookmarkData);
                     End;
               End;
           //*******************************************************************
           //   UNIQUE CODE TO SUPPORT BOTH VIEW OF BLOBS IN GRIDS
           //   AND DEFAULT VALUES FOR BLOBS
           //*******************************************************************
           if (F_DataSet.State = dsInsert) And (DInfo.RecordNo=-1) Then
              Begin
                F_BlobData := '';
                F_BlobData := DInfo.RecordData.Strings[F_Field.FieldNo-1];
                if (F_BlobData='') And
                   (F_DataSet.F_DefaultValues.Strings[F_Field.FieldNo-1] <> '') Then
                    Begin
                      F_BlobData := F_DataSet.F_DefaultValues.Strings[F_Field.FieldNo-1];
                    End;
              End
           Else
              Begin
                Try
                  //************************************************* 01.02.2002
                  RD := F_DataSet.F_DaoTable.Fields.Item[F_Field.FieldNo-1].Value;
                  RS := F_DataSet.F_DaoTable.Fields.Item[F_Field.FieldNo-1].FieldSize;
                  if VarType(RD) = varNull Then
                     F_BlobData := ''
                  Else
                     F_BlobData := F_DataSet.BlobToString(F_Field,RD,RS);
                  if F_DataSet.F_HasEncoder Then
                     Begin
                      //*******************************************
                      // Perform Decoding here
                      //*******************************************
                       SetStrProp(F_DataSet.F_Encrypter, F_DataSet.F_EncodedString,F_BlobData);
                       F_BlobData:=GetStrProp(F_DataSet.F_Encrypter, F_DataSet.F_DecodedString);
                     End;
                  //************************************************* 01.02.2002
                Except
                  F_BlobData:='';
                  if F_DataSet.F_TableType <> dbOpenForwardOnly Then F_DataSet.DaoInternalRefresh;
                End;
              End;
           F_BlobSize:=Length(F_BlobData);
           Size := F_BlobSize;
           //******************************************************** Reposition
           if TempBK > 0 Then
              Begin
               F_DataSet.InternalMoveToBookmark(@TempBK);
              End;
           //*******************************************************************
        End;
End;                                  


Destructor TKBlobStream.Destroy;
Begin
 if F_Modified then
   try                                                                                           
     F_DataSet.DataEvent(deFieldChange, Longint(F_Field));
     F_BlobData := '';
     F_Buffer   := Nil;
     F_Opened   := False;
   Except
     Application.HandleException(Self);
   End;
End;


Function TKBlobStream.Read(var Buffer; Count: Longint): Longint;
Begin
  Result := 0;
  if F_Opened then
  Begin
    if Count > Size - F_Position then
       Result := Size - F_Position
    Else
       Result := Count;
    if Result > 0 then
       Begin
         Move(PAnsiChar(F_BlobData)[F_Position], Buffer, Result);
         Inc(F_Position, Result);
       End;
     End;
End;


Function TKBlobStream.Write(const Buffer; Count: Longint): Longint;
var
   pTemp  : Pointer;
   sTemp  : AnsiString;
   RData  : TStringList;
Begin
 Result := 0;
 if F_Opened then
    Begin
     try
       SetLength(sTemp,Count);
       pTemp:=PAnsiChar(sTemp);
       CopyMemory(pTemp, @Buffer, Count);
       F_BlobData  := Copy(F_BlobData,1,F_Position)+sTemp;
       F_BlobSize  := Length(F_BlobData);
       Size := F_BlobSize;
       RData:=PDaoInfo(F_Buffer+F_DataSet.F_StartMyInfo)^.RecordData;
       RData.Strings[F_Field.FieldNo-1]:=F_BlobData;
       RData.Objects[F_Field.FieldNo-1]:=TObject(True);
       F_Modified := True;
     Finally
     End;
     Inc(F_Position, Count);
     Result := Count;
     F_Modified := True;
   End;
End;


Function TKBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
Begin
 Case Origin of
      0: F_Position := Offset;
      1: Inc(F_Position, Offset);
      2: F_Position := F_BlobSize + Offset;
 End;
 Result := F_Position;
End;


Procedure TKBlobStream.Truncate;
Var
   RData  : TStringList;
Begin
 if F_Opened then
    Begin
     RData:=PDaoInfo(F_Buffer+F_DataSet.F_StartMyInfo)^.RecordData;
     SetLength(F_BlobData,F_Position);
     F_BlobSize  := Length(F_BlobData);
     Size := F_BlobSize;
     RData.Strings[F_Field.FieldNo-1]:=F_BlobData;
     RData.Objects[F_Field.FieldNo-1]:=TObject(True);
     F_Modified := True;
   End;
End;

//***********************************************************************************
Function TKADaoTable.IntegerToBuffer(Buffer: Pointer; S: String): Boolean;
Begin
     Result:=False;
     if Buffer=Nil Then Exit;
     Result := (S <> '');
     if S = '' then S := '0';
     Try
       Integer(Buffer^) := StrToInt(S);
     Except
       Try
         Integer(Buffer^) := Round(StrToFloat(S));
       Except
       End;  
     End;
End;


Function TKADaoTable.FloatToBuffer(Buffer: Pointer; S: String): Boolean;
Begin
     Result:=False;
     if Buffer=Nil Then Exit;
     Result := (S <> '');
     if S = '' then S := '0';
     Try
       Double(Buffer^) := StrToFloat(S);
     Except
     End;
End;

Function TKADaoTable.BooleanToBuffer(Buffer: Pointer; S: String): Boolean;
Begin
     Result:=False;
     if Buffer=Nil Then Exit;
     Result := (S <> '');
     //************************************************************** 25.01.2002
     if Result Then
        Begin
         if S[1]='=' Then System.Delete(S,1,1);
         if AnsiLowerCase(S)='false' Then S := '0'
         Else
         if AnsiLowerCase(S)='true'  Then S := '1'
         Else
         if AnsiLowerCase(S)='no'    Then S := '0'
         Else
         if AnsiLowerCase(S)='yes'   Then S := '1'
         Else
         if AnsiLowerCase(S)='on'    Then S := '1'
         Else
         if AnsiLowerCase(S)='off'   Then S := '0'
         Else
         if S = ''    Then S := '0'
         Else
         if S = '-1'  Then S := '1';
         Try
          WordBool(Buffer^) := WordBool(StrToInt(S));
         Except
         End;
        End;
End;

//************************************************************************** OK
Function TKADaoTable.DateToBuffer(Buffer: Pointer; S: String): Boolean;
var
   Ttmp : TTimeStamp;
   Dtmp : ^TDateTimeRec;
   P    : Integer;
Begin
 Result:=False;
 if Buffer=Nil Then Exit;
 P := Pos(' ',S);
 if P=0 Then Exit;
 Try
  Ttmp.Date:=StrToInt(Copy(S,1,P-1));
  System.Delete(S,1,P);
  Ttmp.Time:=StrToInt(S);
  Dtmp := Buffer;
  Dtmp^.Date := Ttmp.Date;
  Result:=True;
 Except
 End;
End;


//************************************************************************** OK
Function TKADaoTable.TimeToBuffer(Buffer: Pointer; S: String): Boolean;
var
   Ttmp : TTimeStamp;
   Dtmp : ^TTimeStamp;
   P    : Integer;
Begin
 Result:=False;
 if Buffer=Nil Then Exit;
 P := Pos(' ',S);
 if P=0 Then Exit;
 Try
  Ttmp.Date:=StrToInt(Copy(S,1,P-1));
  System.Delete(S,1,P);
  Ttmp.Time:=StrToInt(S);
  Dtmp:=Buffer;
  Dtmp^.Time:=Ttmp.Time;
  Result:=True;
 Except
 End;
End;

//************************************************************************** OK
Function TKADaoTable.DateTimeToBuffer(Buffer: Pointer; S: String): Boolean;
var
   Ttmp : TTimeStamp;
   Dtmp : ^TDateTimeRec;
   P    : Integer;
Begin
 Result:=False;
 if Buffer=Nil Then Exit;
 P := Pos(' ',S);
 if P=0 Then Exit;
 Try
  Ttmp.Date:=StrToInt(Copy(S,1,P-1));
  System.Delete(S,1,P);
  Ttmp.Time:=StrToInt(S);
  Dtmp := Buffer;
  Dtmp^.DateTime := TimeStampToMSecs(Ttmp);
  Result:=True;
 Except
 End;
End;

//************************************************************************** OK
Function TKADaoTable.GUIDToBuffer(Buffer: Pointer; S: String): Boolean;
Var
  BGUID : TGUID;
  PGUID : Pointer;
  SGUID : String;
  P     : Integer;
Begin
  Result:=False;
  if Buffer=Nil Then Exit;
  Result := (S <> '');
  if S = '' Then Exit;
  PGUID  := @BGUID;
  SGUID  := S;
  P := Pos('{guid ',SGUID);
  if P = 1 Then
     Begin
       System.Delete(SGUID,1,6);
       P := Pos('}}',SGUID);
       if P = Length(SGUID)-1 Then System.Delete(SGUID,P,1);
     End;
  BGUID := StringToGUID(AnsiUpperCase(SGUID));
  Move(PGUID^,Buffer^,SizeOf(TGUID));
End;

//************************************************************************** OK
Function TKADaoTable.BufferToGUID(Buffer:Pointer):String;
Var
 S  : String;
Begin
 Result := '';
 S   := AnsiUpperCase(GUIDToString(TGUID(Buffer^)));
 if S = AnsiUpperCase(GUIDToString(GUID_NULL)) Then Exit;
 Result := '{guid '+S+'}';
End;

//************************************************************************** OK
Function  TKADaoTable.GetGUIDAsString(GUID : String):String;
Var
  BGUID : TGUID;
  PGUID : Pointer;
  SGUID : String;
Begin
  Result  := '';
  if Length(GUID) <> SizeOF(TGUID) Then Exit;
  PGUID  := @BGUID;
  SGUID  := GUID;
  Move(SGUID[1],PGUID^,SizeOf(TGUID));
  Result := GUIDToString(BGUID);
  if AnsiUpperCase(Result)=AnsiUpperCase(GUIDToString(GUID_NULL)) Then Result:='';
End;

//************************************************************************** OK
Function  TKADaoTable.GetStringAsGUID(GUID : String) : TGUID;
Begin
 Result := StringToGUID(GUID);
End;

//************************************************************************** OK
Function  TKADaoTable.PutGUIDInString(GUID : String):String;
Var
 BGUID : TGUID;
 PGUID : Pointer;
Begin
 PGUID  := @BGUID;
 BGUID  := StringToGUID(GUID);
 SetString(Result,PChar(PGUID),SizeOf(TGUID)) ;
End;

//************************************************************************** OK
Function TKADaoTable.BufferToDate(Buffer: Pointer): String;
var
   Dtmp : ^TDateTimeRec;
Begin
     Result := '';
     Dtmp   := Buffer;
     if Dtmp=Nil Then Exit;
     Try
       Result := IntToStr(Dtmp.Date)+' '+IntToStr(0);
     Except
      Result := '';
     End;
End;

//************************************************************************** OK
Function TKADaoTable.BufferToDateTime(Buffer: Pointer): String;
var
   TTmp : TTimeStamp;
   Dtmp : ^TDateTimeRec;
Begin
     Result := '';
     Dtmp   := Buffer;
     if Dtmp=Nil Then Exit;
     Ttmp   := MsecsToTimeStamp(Dtmp.DateTime);
     Try
       Result := IntToStr(Ttmp.Date)+' '+IntToStr(Ttmp.Time);
     Except
       Result := '';
     End;
End;

//************************************************************************** OK
Function TKADaoTable.BufferToTime(Buffer: Pointer): String;
var
   Dtmp : ^TTimeStamp;
Begin
     Result := '';
     Dtmp   := Buffer;
     if Dtmp=Nil Then Exit;
     //******************************* SHAME MICROSOFT!!!
     Try
       Result := IntToStr(693594)+' '+IntToStr(Dtmp.Time);
     Except
       Result := '';
     End;
End;


//***************************************************************** TPARAMETERS
{$IFDEF USEPARAMS}
 {$IFNDEF VER100}
  {$IFNDEF VER110}
Procedure TKADaoTable.SetParamsList(Value: TParams);
begin
    F_Params.AssignValues(Value);
end;

Procedure TKADaoTable.UpdateParamsList(Sender: TObject);
var
    List: TParams;
begin
    if not (csReading in ComponentState) then
        if ParamCheck or (csDesigning in ComponentState) then
        begin
            List := TParams.Create(Self);
            try
                List.ParseSQL(SQL.Text, True);
                List.AssignValues(F_Params);
                F_Params.Clear;
                F_Params.Assign(List);
            finally
                List.Free;
            end;
        end;
end;

Function TKADaoTable.GetParamsCount: Word;
begin
    Result := F_Params.Count;
end;

Procedure TKADaoTable.DefineProperties(Filer: TFiler);

    Function WriteData: Boolean;
    begin
        if Filer.Ancestor <> nil then
            Result := not F_Params.IsEqual(TKADaoTable(Filer.Ancestor).F_Params)
        else
            Result := F_Params.Count > 0;
    end;

begin
    inherited DefineProperties(Filer);
    Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

Procedure TKADaoTable.ReadParamData(Reader: TReader);
begin
    Reader.ReadValue;
    Reader.ReadCollection(F_Params);
end;

Procedure TKADaoTable.WriteParamData(Writer: TWriter);
begin
    Writer.WriteCollection(Params);
end;
   {$ENDIF}
 {$ENDIF}
{$ENDIF}
//************************************************************** TPARAMETERS END

Procedure Register;
Begin
    RegisterComponents('KA Dao', [TKADaoTable]);
End;
end.


