{
  Delphi Test program for Open Odbc DbExpress Driver.
  Version 3.205 2008-11-27

  Copyright (c) 2001-2006 Edward Benson

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.
}

{.$DEFINE _DEBUG_}
{.$UNDEF _DEBUG_}

(*

Change History

Beta, 28 Oct 2001
-------------------------
- First public release

Version 1.03, 06 Dec 2001
-------------------------
- License changed from LGPL to GPL, and header updated
- Added context menu to TreeView to generate Sql statements


Version 1.05, 11 September 2002. Vadim V.Lopushansky:
-------------------------------
  - some bug fixing. For detaoils seek to block:
     //  {+}
     //    ... new or modificied code
     //  {+.}
  - blob exception handling
  - execute BDE extended query when <:> changed to {::}
  - autoconfigure apply update "Where" if exist primary key

Version 2.00, 2003-10-24 Vadim V.Lopushansky:
-------------------------------

  - manu interface changes
  - added transaction access buttons
  - blob exception handling
  - blob scrolling
  - blob hex editor
  - added history connection string (file: "SavedODBCCon.sav")
  - added sql manual history buffer (file: "SavedODBCQuery.sav")
  - added Open ODBC connection option extension box
  - Added show columns when ODBC not realized procedures for this option

Version 2.1, 2004-06-07 Vadim V.Lopushansky:
-------------------------------
  - Some bugs fixed
  - Added support InternalCloneConnection Connection Option
  - Added support CursorPreserved (Automaticaly disable PackedRecords).
  - Added support VendorLib connection option
  - Added support of embedding SqlExpr fixes (SqlExprFif.pas).
  - Added generation index metadata scripts.
  - Added Quotes for Table Names, Columns ...

  todo:
  - Added support text (base64/compressed) blob. See functions:
      DecodeStringToBinary(), DecodeBinaryStringToBinary(), BinaryIsStringBinary().

Version 2.2, 2005-11-05 Vadim V.Lopushansky:
-------------------------------
  - converted to delphi 2005
  - added supports new driver options

*)

// DbxStaticLink
// Linking dbxoodbc driver statically.
{$define DbxStaticLink}

// _HexEditor_
// [http://www.torry.net/vcl/edits/diffedits/msthexedit.zip]
// [http://www.mirkes.de/en/delphi/vcls/hexedit.php]
// limitation: need create temporary file for binary data
{$define _HexEditor_}

// _SynEdit_
// [http://synedit.sourceforge.net]
// TODO: contained bug: not saved text to dataset
{$define _SynEdit_}

// _GraphicEx_
// Allow view many image formats.
// [http://www.delphi-gems.com/Graphics.php]
// Need change file "GraphicConfiguration.inc": need
//       enable: $define UseLZW for support GIF image format.
{$define _GraphicEx_}

// _kbm_compress_
// added convertion Binary field to "Compressed + Base64" field.
//   It enables to keep blob the data in chars fields (SQLite).
{.$define _kbm_compress_}

{$define _XPMan_} { AV information:

  D7, D9 - AV when closing app and connection is On.

  After move code from method TFormOdbcExplor.FormDestroy to
  method TFormOdbcExplor.FormClose - all work is fine !!!

  It bug is depend from use unit XPMan !!! After remove it unit not need
  to moving code ...
}

{$UNDEF _XPMENU_}
{.$DEFINE _XPMENU_}

{.$UNDEF  _CHECK_LEAKS_}
{.$DEFINE  _CHECK_LEAKS_} // nedd copy FastMM4Options.dbg.inc into FastMM4Options.inc

// _DBX_FIXES_
// Delphi Fixes for "procedure TCustomSQLDataSet.SetSchemaOption" in module "SqlExpr.pas";
// Access for limits of the buffer (damage of a code).

{$define _DBX_FIXES_} // if use SqlExprFix.pas
//################<---------<<------------o
//                                        ^
//                                        |
//                                       / \
//################################################################################
//##############################      Nota Bene     ##############################
//################################################################################

unit frmOdbcExplor;

{$INCLUDE DbxOpenOdbc.inc}

{$IFDEF _D6UP_}
  {$WARN SYMBOL_PLATFORM OFF} // Suppress .Net warnings
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

{$IFDEF _D7UP_}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ELSE}
  {$undef _XPMan_}
{$ENDIF}

{$IFDEF _D9UP_}
  {$undef _DBX_FIXES_}
{$ENDIF}

{+}
{$B-,W+,J+}
{$D+,O-,L+}
{+.}

interface

uses
  DbxOpenOdbcInterface,
  DbxOpenOdbcCallback,
  {$IFDEF DbxStaticLink}
  DbxOpenOdbcStatic,
  //OdbcApi,
  //RegExpr,
  //DbxObjectParser,
  //DbxOpenOdbc,
  {$ENDIF}
  {$ifdef _DBX_FIXES_}
  SqlExprFix,
  {$endif}
  Variants, VarUtils, ComObj,
  {$ifdef _HexEditor_}
  MPHexEditor, MPHexEditorEx,
  {$endif ifdef _HexEditor_}
  {$ifdef _SynEdit_}
  SynHighlighterSQL, SynDBEdit, SynEdit,
  {$endif ifdef _SynEdit_}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, Grids, DBGrids,
  DBClient, Provider, ExtCtrls, DBCtrls, ComCtrls, Menus, FMTBcd, SqlExpr,
  Tabs, ExtDlgs,

  DBCommon,
  {$IF CompilerVersion < 18.50}
  DBXpress,
  {$ELSE}
  DBXCommon,
  {$IFEND}

  {$IF CompilerVersion > 17.00}
  WideStrings,
  {$IFEND}

  {$IF CompilerVersion >= 19.00}
  AnsiStrings,
  {$IFEND}

  {$IFDEF _XPMan_}
  XPMan,
  {$ENDIF}
  {$ifdef _kbm_compress_}
  kbmCompress,
  {$endif}
  {$IFDEF _XPMENU_} XPMenu, {$ENDIF}
  ImgList, Buttons, EncdDecd, ActnList;

const
 c_version = '2008.11.27.00';

{$IFNDEF _D9UP_}
  DBXERR_NONE = DBXpress.SQL_SUCCESS;
  DBX_MAXSTATICERRORS = DBXpress.MaxReservedStaticErrors;
{$ENDIF}

  c_skip_memo = 1;
  c_skip_blob = 2;
  c_skip_hexeditor = 4;

  WM_USER_INIT_TREE_VIEW = WM_USER + 100;

type

  {$IFDEF _D10UP_}
   ISQLConnection = ISQLConnection25;
   ISQLCommand = ISQLCommand25;
   ISQLCursor = ISQLCursor25;
   ISQLMetaData = ISQLMetaData25;

   TISQLConnection25H = class(TISQLConnection)
   private
     I: ISQLConnection25;
   public
     property SQLConnection: ISQLConnection25 read I;
   end;

   {
   TISQLMetaData25H = class(TISQLMetaData)
   protected
     I : ISQLMetaData25;
   public
     property SQLMetaData: ISQLMetaData25 read I;
   end;
   }

    XString =  WideString;
    XPChar = PWideChar;
  {$ELSE}
    XString =  String;
    XPChar = PAnsiChar;

    TISQLMetaData = ISQLMetaData;
    TISQLCursor = ISQLCursor;
  {$ENDIF}


  {$ifdef _HexEditor_}
  THexEditor = TMPHexEditorEx;
  {$endif ifdef _HexEditor_}

  {$ifdef _SynEdit_}
  TSQLDBMemo = TDBSynEdit;
  {$else}
  TSQLDBMemo = TDBMemo;
  {$endif}

  TClientDataSet = class( DBClient.TClientDataSet )
  private
    fBriefCaseTableName: String;
    fBriefCaseFileName: String;
    fUpdatesErrorCount: Integer;
    fUpdatesErrorRecNo: Integer;
  public
    procedure CopyRecord;
    procedure ReadRecordToVariant(var vDest: Variant;
      bPackBlobsToString: Boolean = False;
      bCompressed: Boolean = False;
      bEncodeBase64: Boolean = False);
    procedure WriteVarintToRecord(const vSource: Variant);
    procedure MarkCurrentRecordsAsNew(
      bPackBlobsToString: Boolean = False;
      bCompressed: Boolean = False;
      bEncodeBase64: Boolean = False);
    procedure MarkAllRecordsAsNew(
      bPackBlobsToString: Boolean = False;
      bCompressed: Boolean = False;
      bEncodeBase64: Boolean = False);
  end;

  TTimer = class(ExtCtrls.TTimer)
  private
    fStartTime: TDateTime;
  end;

  TFormOdbcExplor = class(TForm)
    SQLConnection1: TSQLConnection;
    DataSetProvider1: TDataSetProvider;
    CDS: TClientDataSet;
    DataSource1: TDataSource;
    SQLDataSet1: TSQLDataSet;
    pmImage: TPopupMenu;
    PopMenuImageCut: TMenuItem;
    PopMenuImageCopy: TMenuItem;
    PopMenuImagePaste: TMenuItem;
    pClient: TPanel;
    spTree: TSplitter;
    pData: TPanel;
    pTop: TPanel;
    spCon: TSplitter;
    PopupMenuTree: TPopupMenu;
    mnuGenSqlCreateTable: TMenuItem;
    mnuGenSqlInsert: TMenuItem;
    mnuGenSqlSelect: TMenuItem;
    mnuGenerateSqlSelectEach: TMenuItem;
    mnuGenerateSqlUpdate: TMenuItem;
    N1: TMenuItem;
    miImageClear: TMenuItem;
    pConnOptEx: TPanel;
    Notebook1: TNotebook;
    lbMemoData: TLabel;
    DBMemo1: TDBMemo;
    TabSet1: TTabSet;
    CDSQuery: TClientDataSet;
    sCDSQuery: TDataSource;
    CDSQueryid: TSQLTimeStampField;
    CDSQueryqry: TMemoField;
    pbOpen: TBitBtn;
    pbExecute: TBitBtn;
    gbLogin: TGroupBox;
    DSN: TLabel;
    UID: TLabel;
    PWD: TLabel;
    dfUID: TEdit;
    dfPWD: TEdit;
    pbConnect: TBitBtn;
    dfDSN: TComboBox;
    chkConnected: TCheckBox;
    dbQryLastAccessed: TDBText;
    lbQryLastAccessedCap: TLabel;
    lbQryCntCap: TLabel;
    lbQryCnt: TLabel;
    pBinary: TPanel;
    lbBinaryData: TLabel;
    cmMemo: TComboBox;
    cmBinary: TComboBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Save1: TMenuItem;
    SaveData1: TMenuItem;
    N2: TMenuItem;
    SaveOptions1: TMenuItem;
    Open1: TMenuItem;
    OpenData1: TMenuItem;
    N3: TMenuItem;
    OpenSavedQueries1: TMenuItem;
    N4: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Options1: TMenuItem;
    Help1: TMenuItem;
    Glosary1: TMenuItem;
    N5: TMenuItem;
    About1: TMenuItem;
    pImage: TPanel;
    lbImageData: TLabel;
    cmImage: TComboBox;
    sbImage: TScrollBox;
    DBImage1: TDBImage;
    pmMemo: TPopupMenu;
    pmMemoFont: TMenuItem;
    pnMemoFontPlus: TMenuItem;
    pnMemoFontMinus: TMenuItem;
    pmImageRecompess: TMenuItem;
    N6: TMenuItem;
    pmImageSave: TMenuItem;
    pmImageLoad: TMenuItem;
    OPD: TOpenPictureDialog;
    SPD: TSavePictureDialog;
    lbStatementsPerConnection: TLabel;
    cbAutoClone: TCheckBox;
    SB: TStatusBar;
    ImageList: TImageList;
    lbConnectedTime: TLabel;
    Timer1: TTimer;
    N7: TMenuItem;
    pmnuShowData: TMenuItem;
    pmnuGenerateSqlDrop: TMenuItem;
    N8: TMenuItem;
    pmnuRefreshAll: TMenuItem;
    pConOpt: TPanel;
    cbDefConOpt: TCheckBox;
    pcData: TPageControl;
    tsData: TTabSheet;
    tsUpdateOpt: TTabSheet;
    Panel3: TPanel;
    lbTableName: TLabel;
    dbNavData: TDBNavigator;
    btnBeginTransaction: TBitBtn;
    btnCommit: TBitBtn;
    btnRollack: TBitBtn;
    DBGrid1: TDBGrid;
    GroupBox4: TGroupBox;
    rbUpdateWhereAll: TRadioButton;
    rbUpdateWhereKey: TRadioButton;
    rbUpdateWhereChanged: TRadioButton;
    pbApplyUpdates: TBitBtn;
    pcUpdSQL: TPageControl;
    tsUpdSQL_Insert: TTabSheet;
    tsUpdSQL_Modify: TTabSheet;
    tsUpdSQL_Delete: TTabSheet;
    mSQLInsert: TMemo;
    mSQLModify: TMemo;
    mSQLDelete: TMemo;
    pTree: TPanel;
    TreeView1: TTreeView;
    btTreeAllign: TBitBtn;
    btConAllign: TBitBtn;
    cbCursorPreserved: TCheckBox;
    LSVendors: TLabel;
    VendorLib: TComboBox;
    pUO: TPanel;
    cbUpdIgnoreError: TCheckBox;
    cbUpdateCustSQL: TCheckBox;
    pbMarkAllRecordsAsNew: TBitBtn;
    pbLogChanges: TBitBtn;
    gbTextBlob: TGroupBox;
    cbCompressBlob: TCheckBox;
    cbPackBlobsToString: TCheckBox;
    cbBase64: TCheckBox;
    pbMarkCurrentRecordsAsNew: TBitBtn;
    pmMemoWordWrap: TMenuItem;
    ScrollBox1: TScrollBox;
    gbCustConOpt: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lbBlockRowsCount: TLabel;
    lbFetchRowsInfo: TLabel;
    cbTrimChar: TCheckBox;
    cbMapInt64ToBcd: TCheckBox;
    cbIgnoreUnknownFieldType: TCheckBox;
    cbEmptyStrParam: TCheckBox;
    edBlobChunkSize: TEdit;
    edConPacketSize: TEdit;
    cbMixedFetch: TCheckBox;
    edBlockRowsCount: TEdit;
    cbEnableBCD: TCheckBox;
    cbMaxBCD: TCheckBox;
    cbBlobNotTermChar: TCheckBox;
    cbInternalCloneConnection: TCheckBox;
    cbAutoInc: TCheckBox;
    cbParamDateByOdbcLevel: TCheckBox;
    Label_drv_bugs: TLabel;
    cbReadOnlyField: TCheckBox;
    cbUnicodeChars: TCheckBox;
    cbBCD2Exp: TCheckBox;
    Shape1: TShape;
    Shape2: TShape;
    sh_coICloneCon: TShape;
    sh_drv_bugs: TShape;
    sh_coNumericSeparator: TShape;
    sh_coMixedFetch: TShape;
    sh_coTrimChar: TShape;
    Shape8: TShape;
    sh_coMapInt64ToBCD: TShape;
    sh_coIgnoreUnkFldType: TShape;
    sh_coEnableBCD: TShape;
    sh_coBCD2Exp: TShape;
    sh_coBlobChunkSize: TShape;
    sh_coNetPacketSize: TShape;
    sh_FetchRowCount: TShape;
    Shape16: TShape;
    sh_coNullStrParam: TShape;
    sh_coReadOnly: TShape;
    edDecimalSeparator: TEdit;
    Label7: TLabel;
    pDataNav: TPanel;
    dbNavQuery: TDBNavigator;
    co_coTrimChar: TLabel;
    co_coMapInt64ToBCD: TLabel;
    co_coIgnoreUnkFldType: TLabel;
    co_coEnableBCD: TLabel;
    co_coBCD2Exp: TLabel;
    co_coBlobChunkSize: TLabel;
    co_coNetPacketSize: TLabel;
    co_FetchRowCount: TLabel;
    co_coMixedFetch: TLabel;
    co_coICloneCon: TLabel;
    co_coNumericSeparator: TLabel;
    co_coAutoInc: TLabel;
    co_coFldReadOnly: TLabel;
    co_coParDateByLev3: TLabel;
    co_coMaxBCD: TLabel;
    co_coBlobNotTermChar: TLabel;
    co_coEmptyStrParam: TLabel;
    sh_coSupportsMetadata: TShape;
    sh_coLockMode: TShape;
    sh_coSupportsSchemaFilter: TShape;
    sh_coMapSmallBcdToNative: TShape;
    sh_coMapCharAsBDE: TShape;
    sh_coSupportsCatalog: TShape;
    sh_coCatalogPrefix: TShape;
    sh_coConTimeout: TShape;
    sh_coEmptyStrParam: TShape;
    sh_coEnableUnicode: TShape;
    sh_coSafeMode: TShape;
    sh_template: TShape;
    sh_drv_custom: TShape;
    Label3: TLabel;
    co_coNullStrParam: TLabel;
    cbNullStrParam: TCheckBox;
    lbMixedFetchInfo: TLabel;
    lbMixedFetchInfoNB: TLabel;
    co_coSafeMode: TLabel;
    cbSafeMode: TCheckBox;
    co_coMapSmallBcdToNative: TLabel;
    cbMapSmallBcdToNative: TCheckBox;
    co_coMapCharAsBDE: TLabel;
    cbMapCharAsBDE: TCheckBox;
    co_coEnableUnicode: TLabel;
    cbEnableUnicode: TCheckBox;
    co_coLockMode: TLabel;
    co_coReadOnly: TLabel;
    cbReadOnly: TCheckBox;
    edLockMode: TEdit;
    co_coConTimeout: TLabel;
    edConTimeout: TEdit;
    Label4: TLabel;
    co_coSupportsSchemaFilter: TLabel;
    cbSupportsSchemaFilter: TCheckBox;
    pMemoCli: TPanel;
    pQueryOpt: TPanel;
    lbScript: TLabel;
    Label5: TLabel;
    cbGetMetadata: TCheckBox;
    edPacketRecords: TEdit;
    cbPacketRecords: TCheckBox;
    edScriptLD: TEdit;
    cbRemoveScriptComments: TCheckBox;
    pbDetach: TBitBtn;
    pSQL: TPanel;
    btBlobSave: TBitBtn;
    SD: TSaveDialog;
    btBlobLoad: TBitBtn;
    OD: TOpenDialog;
    btBlobClear: TBitBtn;
    btMemoSaveToFile: TBitBtn;
    btMemoLoadFromFile: TBitBtn;
    btMemoClear: TBitBtn;
    btImageSaveToFile: TBitBtn;
    btImageLoadFromFile: TBitBtn;
    btImageClear: TBitBtn;
    ac_Connection: TActionList;
    ac_Query: TActionList;
    ac_Memo: TActionList;
    ac_Image: TActionList;
    ac_Binary: TActionList;
    ac_Connection_Connect: TAction;
    ac_Connection_Disconnect: TAction;
    ac_Connection_ReConnect: TAction;
    ac_Main: TActionList;
    ac_Main_Connection: TAction;
    ac_Main_Query: TAction;
    ac_Main_Memo: TAction;
    ac_Main_Image: TAction;
    ac_Main_Binary: TAction;
    ac_Query_Open: TAction;
    ac_Query_Execute: TAction;
    ac_Query_Close: TAction;
    ac_Main_Query_Data: TAction;
    ac_Main_Query_UpdateOptions: TAction;
    ac_Main_ApplyUpdates: TAction;
    ac_Main_LogChanges: TAction;
    ac_Main_BeginTransaction: TAction;
    ac_Main_Commit: TAction;
    ac_Main_Rollback: TAction;
    co_coBlobFragmntns: TLabel;
    cbBlobFragmntns: TCheckBox;
    sh_coBlobNotTermChar: TShape;
    Label6: TLabel;
    co_coQueryTimeout: TLabel;
    edQueryTimeout: TEdit;
    Label8: TLabel;
    mem_sql_monitor: TMemo;
    p_sql_monitor_btns: TPanel;
    btn_sql_monitor_clear: TButton;
    cb_sql_monitor_active: TCheckBox;
    procedure cbBlobNotTermCharClick(Sender: TObject);
    procedure cbNullStrParamClick(Sender: TObject);
    procedure dfDSNSelect(Sender: TObject);
    procedure edDecimalSeparatorChange(Sender: TObject);
    procedure cbBCD2ExpClick(Sender: TObject);
    procedure pbConnectClick(Sender: TObject);
    procedure SQLConnection1BeforeConnect(Sender: TObject);
    procedure SQLConnection1AfterConnect(Sender: TObject);
    procedure SQLConnection1AfterDisconnect(Sender: TObject);
    procedure pbOpenClick(Sender: TObject);
    procedure pbApplyUpdatesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbExecuteClick(Sender: TObject);
    procedure PopMenuImageCutClick(Sender: TObject);
    procedure PopMenuImageCopyClick(Sender: TObject);
    procedure PopMenuImagePasteClick(Sender: TObject);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure memoSql1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure memoSql1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeView1DblClick(Sender: TObject);
    procedure mnuGenSqlCreateTableClick(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CDSAfterOpen(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure btnBeginTransactionClick(Sender: TObject);
    procedure btnCommitClick(Sender: TObject);
    procedure btnRollackClick(Sender: TObject);
    procedure miImageClearClick(Sender: TObject);
    procedure CDSAfterClose(DataSet: TDataSet);
    procedure CDSAfterScroll(DataSet: TDataSet);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PopupMenuTreePopup(Sender: TObject);
    procedure dbNavQueryBeforeAction(Sender: TObject;
      Button: TNavigateBtn);
    procedure CDSQueryBeforePost(DataSet: TDataSet);
    procedure pmImagePopup(Sender: TObject);
    procedure sCDSQueryDataChange(Sender: TObject; Field: TField);
    procedure CDSBeforeClose(DataSet: TDataSet);
    procedure CDSBeforeCancel(DataSet: TDataSet);
    procedure CDSBeforePost(DataSet: TDataSet);
    procedure CDSAfterRefresh(DataSet: TDataSet);
    procedure dbNavDataBeforeAction(Sender: TObject; Button: TNavigateBtn);
    procedure pBinaryResize(Sender: TObject);
    procedure cmMemoChange(Sender: TObject);
    procedure cmImageChange(Sender: TObject);
    procedure cmBinaryChange(Sender: TObject);
    procedure CDSAfterCancel(DataSet: TDataSet);
    procedure cbTrimCharClick(Sender: TObject);
    procedure cbMapInt64ToBcdClick(Sender: TObject);
    procedure cbIgnoreUnknownFieldTypeClick(Sender: TObject);
    procedure cbEmptyStrParamClick(Sender: TObject);
    procedure edBlobChunkSizeChange(Sender: TObject);
    procedure edConPacketSizeChange(Sender: TObject);
    procedure edBlockRowsCountChange(Sender: TObject);
    procedure cbMixedFetchClick(Sender: TObject);
    procedure pnMemoFontPlusClick(Sender: TObject);
    procedure pnMemoFontMinusClick(Sender: TObject);
    procedure SQLConnection1BeforeDisconnect(Sender: TObject);
    procedure CDSAfterEdit(DataSet: TDataSet);
    procedure pmImageSaveClick(Sender: TObject);
    procedure pmImageRecompessAs(Sender: TObject);
    procedure pmImageLoadClick(Sender: TObject);
    procedure edPacketRecordsChange(Sender: TObject);
    procedure cbPacketRecordsClick(Sender: TObject);
    procedure cbAutoCloneClick(Sender: TObject);
    procedure SaveData1Click(Sender: TObject);
    procedure OpenData1Click(Sender: TObject);
    procedure DataSetProvider1UpdateError(Sender: TObject;
      DataSet: TCustomClientDataSet; E: EUpdateError;
      UpdateKind: TUpdateKind; var Response: TResolverResponse);
    procedure SaveOptions1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure cbGetMetadataClick(Sender: TObject);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure pmnuRefreshAllClick(Sender: TObject);
    procedure cbEnableBCDClick(Sender: TObject);
    procedure cbMaxBCDClick(Sender: TObject);
    procedure cbDefConOptClick(Sender: TObject);
    procedure DataSetProvider1BeforeUpdateRecord(Sender: TObject;
      SourceDS: TDataSet; DeltaDS: TCustomClientDataSet;
      UpdateKind: TUpdateKind; var Applied: Boolean);
    procedure btTreeAllignClick(Sender: TObject);
    procedure btConAllignClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure cbInternalCloneConnectionClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbMarkAllRecordsAsNewClick(Sender: TObject);
    procedure pbLogChangesClick(Sender: TObject);
    procedure pbMarkCurrentRecordsAsNewClick(Sender: TObject);
    procedure pmMemoWordWrapClick(Sender: TObject);
    procedure pbDetachClick(Sender: TObject);
    procedure VendorLibChange(Sender: TObject);
    procedure dfDSNCloseUp(Sender: TObject);
    procedure cbAutoIncClick(Sender: TObject);
    procedure cbParamDateByOdbcLevelClick(Sender: TObject);
    procedure cbReadOnlyFieldClick(Sender: TObject);
    procedure DBGrid1DrawDataCell(Sender: TObject; const Rect: TRect;
      Field: TField; State: TGridDrawState);
    procedure cbUnicodeCharsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbSafeModeClick(Sender: TObject);
    procedure cbMapSmallBcdToNativeClick(Sender: TObject);
    procedure cbMapCharAsBDEClick(Sender: TObject);
    procedure cbEnableUnicodeClick(Sender: TObject);
    procedure cbReadOnlyClick(Sender: TObject);
    procedure edLockModeChange(Sender: TObject);
    procedure edConTimeoutChange(Sender: TObject);
    procedure cbSupportsSchemaFilterClick(Sender: TObject);
    procedure btBlobSaveClick(Sender: TObject);
    procedure btBlobLoadClick(Sender: TObject);
    procedure btBlobClearClick(Sender: TObject);
    procedure btMemoSaveToFileClick(Sender: TObject);
    procedure btMemoLoadFromFileClick(Sender: TObject);
    procedure btMemoClearClick(Sender: TObject);
    procedure ac_Connection_ConnectExecute(Sender: TObject);
    procedure ac_Connection_DisconnectExecute(Sender: TObject);
    procedure ac_Connection_ReConnectExecute(Sender: TObject);
    procedure ac_Main_SwitchSheetExecute(Sender: TObject);
    procedure ac_Main_Query_DataExecute(Sender: TObject);
    procedure ac_Main_Query_UpdateOptionsExecute(Sender: TObject);
    procedure ac_Main_ApplyUpdatesExecute(Sender: TObject);
    procedure ac_Main_LogChangesExecute(Sender: TObject);
    procedure ac_Main_BeginTransactionExecute(Sender: TObject);
    procedure ac_Main_CommitExecute(Sender: TObject);
    procedure ac_Main_RollbackExecute(Sender: TObject);
    procedure ac_Query_OpenExecute(Sender: TObject);
    procedure ac_Query_ExecuteExecute(Sender: TObject);
    procedure ac_Query_CloseExecute(Sender: TObject);
    procedure cbBlobFragmntnsClick(Sender: TObject);
    procedure edQueryTimeoutChange(Sender: TObject);
    procedure btn_sql_monitor_clearClick(Sender: TObject);
    procedure cb_sql_monitor_activeClick(Sender: TObject);
  private
    fSqlConOdbcIntf: ISqlConnectionOdbc;
    { Private declarations }
    procedure InsertMemoText(sText: string);
    procedure PopulateTableNode(aNode: TTreeNode;
      SearchTableType: LongWord; ImageIndex: Integer);
    procedure PopulateColumnNode(TableNode: TTreeNode);
    procedure PopulateIndexNode(IndexNode: TTreeNode);
    procedure PopulateStoredProcedureNode(Node: TTreeNode; ImageIndex: Integer);
    procedure WMInitTreeView(var Message:TMessage); message WM_USER_INIT_TREE_VIEW;
    procedure DoInitTreeView();
    procedure InitTreeView();
    procedure PopulateStoredProcedureParamNode(Node: TTreeNode);
    procedure CreateMemoSql();
    procedure CheckSateMemoSql;

   {$IFDEF _D10UP_}
    procedure DataSetProvider1GetTableName_D10_(Sender: TObject; DataSet: TDataSet;
      var TableName: WideString);
   {$ELSE}
    procedure DataSetProvider1GetTableName_D67_(Sender: TObject;
      DataSet: TDataSet; var TableName: String);
   {$ENDIF}

  {$IFDEF _XPMENU_}
  protected
    fXPMenu: XPMenu.TXPMenu;
  {$ENDIF}
  public
    { Public declarations }
    TD: TTransactionDesc;
    fMemoField: String;
    function LinkMemoToField(const FieldName: String): Boolean;
    function LinkImageToField(const FieldName: String): Boolean;
    procedure LinkEditors(bActivate: Boolean; dwSkipEditor: DWORD = 0);
    procedure DbImageSetBounds;
    procedure FillLinkCombos(bActivate: Boolean);
    procedure LoadImage;
  {$ifdef _HexEditor_}
  private
    { Private declarations }
    HexEditor: THexEditor;
    fBinaryField: String;
    fSheetActions: array[0..5] of TActionList;
    procedure LoadBinary;
    procedure HexEditOnStateChanged(Sender: TObject);
    procedure UpdateHexEditField;
    function LinkBinaryToField(const FieldName: String): Boolean;
  {$endif ifdef _HexEditor_}
  private
    TickCountInfo: Cardinal;
    memoSql: TSQLDBMemo;
    {$ifdef _SynEdit_}
    SynSQLSyn: TSynSQLSyn;
    {$endif ifdef _SynEdit_}
    fDetachedSQLList: TList;
    fQuoteChar: string;
    fSupportsSqlPrimaryKeys: Boolean;
    function Quote(const sName: string; bTable: Boolean = False): string;
    procedure SaveConfigurations;
    procedure SetConnectionOptionsBeforeConnect;
    procedure SetConnectionOptionsAfterConnect;
  //
  // SQLMonitor
  //
  private
    SQLMonitor: TSQLMonitor;
    procedure SQLMonitorTrace(Sender: TObject; TraceInfo: TDBXTraceInfo; var LogTrace: Boolean);
  end;

resourcestring
  cCaptionDef = 'DbExpress Open ODBC Explorer:';

var
  FormOdbcExplor: TFormOdbcExplor;

const
  cImageIndexDefault = 0;

  cImageIndexAllTables = 1;
  cImageIndexTables = 2;
  cImageIndexViews = 3;
  cImageIndexSystemTables = 4;
  cImageIndexSynonums = 5;
  cImageIndexStoredProcs = 6;

  cImageIndexAllTable = 7;
  cImageIndexTable = 8;
  cImageIndexView = 9;
  cImageIndexSystemTable = 10;
  cImageIndexSynonum = 11;
  cImageIndexStoredProc = 12;

  cImageIndexField = 13;
  cImageIndexIndexes = 14;
  cImageIndexIndexBase = 15;
  cImageIndexIndexA  = cImageIndexIndexBase + 0; // =15
  cImageIndexIndexD  = cImageIndexIndexA    + 1; // =16
  cImageIndexIndexAU = cImageIndexIndexBase + 2; // =17
  cImageIndexIndexDU = cImageIndexIndexAU   + 1; // =18
  cImageIndexIndexPK = cImageIndexIndexBase + 4; // = 19

  cImageIndexIndexColA = 20;
  cImageIndexIndexColD = 21;
  cImageIndexIndexCol  = 22;

  // compressed blob header format:
  cBlobsAsStringID = '{1F775377-C0EB-4DB0-B4F6-1F6B4050AC8F}';
  cBlobsAsStringIdTemplate = cBlobsAsStringID + 'bcTTXXXXXX'#13#10;
  //                                             || | |
  //                                             || | Reserved (4 bytes)
  //                                             || Field Data Type (2 bytes)
  //                                             |Compressed Flag (1 byte)
  //                                             |
  //                                             Base64 (required for text blob)
  //
  cBAS_BASE_INDEXES = Length(cBlobsAsStringID) + 1;
  cBAS_BASE64_INDEX = cBAS_BASE_INDEXES;
  cBAS_COMPRESSED_INDEX = cBAS_BASE_INDEXES + 2;
  cBAS_DATATYPE_INDEX = cBAS_COMPRESSED_INDEX + 1;
  cBAS_DATATYPE_LENGTH = 2;

implementation

uses
  SqlConst, JPEG,
  {$ifdef _GraphicEx_}
  JPG, GraphicEx,
  {$endif}
  StrUtils;

const
  cSaveConnectionFile: String = 'SavedODBCCon.sav';
  cSaveQueriesFile: String = 'SavedODBCQuery.sav';
resourcestring
  cTickCountInfo = 'Tick Count: %d';

type
  TWinControlPro = class(TWinControl);

{$R *.dfm}

procedure FreeXSQLCursor(var Obj: TISQLCursor);
begin
  {$IFDEF _D10UP_}
    FreeAndNil(TObject(Obj));
  {$ELSE}
    Obj := nil;
  {$ENDIF}
end;

function DataSetToEditMode(DS:TDataSet; bAllowAppend: Boolean = False): Boolean;
begin
  Result := False;
  if not DS.Active then
    exit;
  if DS.IsEmpty then
  begin
    if bAllowAppend then
    begin
      DS.Append;
      Result := True;
    end;
  end
  else
  begin
    if not ( DS.State in [dsInsert, dsEdit] ) then
      DS.Edit;
    Result := True;
  end;
end;

{$ifdef _GraphicEx_}
type
  TSaveToStream = procedure (Stream: TStream) of object;
{$endif ifdef _GraphicEx_}

function BinaryIsStringBinary( Input: TStream; out pFieldType: TFieldType;
 bCompressed: Boolean = False; bEncodeBase64: Boolean = False): Boolean;  //???
var
  sInput: string;
begin
  pFieldType := ftUnknown;
  Result := False;
  if Input.Size < Length(cBlobsAsStringIdTemplate) then
    exit;

  Input.Position := 0;
  SetLength(sInput, Length(cBlobsAsStringIdTemplate));
  Input.Read(PAnsiString(sInput)^, Length(cBlobsAsStringIdTemplate));
  Input.Position := 0;

  // check header
  if cBlobsAsStringID <> Copy(sInput, 1, Length(cBlobsAsStringID)) then
    exit;

    // base64
  if not (sInput[cBAS_BASE64_INDEX] in ['B', 'N']) then
    exit;

    // compressed
  if not (sInput[cBAS_BASE64_INDEX] in ['N', 'L'{, 'Z'}]) then
    exit;

  // read header info

    // field data type
  pFieldType := TFieldType( StrToIntDef(Copy(sInput, cBAS_DATATYPE_INDEX, cBAS_DATATYPE_LENGTH),
    Integer(ftUnknown) ) );

  if not ( pFieldType in [ ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftTypedBinary, ftOraBlob, ftOraClob ] )
  then
  begin
    pFieldType := ftUnknown;
    exit;
  end;
  Result := True;
end;

procedure DecodeStringToBinary(const sInput: string; Output: TMemoryStream; out pFieldType: TFieldType);
var
  ms, msDeCompressed:TMemoryStream;
  bCompressed: Boolean;
  cCompressed: Char;
  bEncodeBase64: Boolean;
begin
  pFieldType := ftUnknown;
  if Length(sInput) < Length(cBlobsAsStringIdTemplate) then
    exit;

  // check header
  if cBlobsAsStringID <> Copy(sInput, 1, Length(cBlobsAsStringID)) then
    exit;
  // read header info

    // base64
  bEncodeBase64 := sInput[cBAS_BASE64_INDEX] = 'B';;

    // compressed
  cCompressed := sInput[cBAS_COMPRESSED_INDEX];
  bCompressed := cCompressed in [ 'L'{, 'Z'}]; // L- LZH, Z - ZIP

    // field data type
  pFieldType := TFieldType( StrToIntDef(Copy(sInput, cBAS_DATATYPE_INDEX, cBAS_DATATYPE_LENGTH),
    Integer(ftUnknown) ) );

  if not ( pFieldType in [ ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftTypedBinary, ftOraBlob, ftOraClob ] )
  then
  begin
    pFieldType := ftUnknown;
    exit;
  end;

  msDeCompressed := nil;
  ms := TMemoryStream.Create;
  try
    ms.Size := Length(sInput) - Length(cBlobsAsStringIdTemplate);
    if ms.Size = 0 then
      exit;
    ms.Write(sInput[Length(cBlobsAsStringIdTemplate)+1], ms.Size);
    ms.Position := 0;

    if bCompressed then
    begin
      {$ifdef _kbm_compress_} // LZHDecompressLoad

      if bEncodeBase64 then
      begin
        msDeCompressed := TMemoryStream.Create;
        EncdDecd.DecodeStream(ms, msDeCompressed);
      end
      else
      begin
        msDeCompressed := ms;
        ms := nil;
      end;

      // decompress stream
      //msDeCompressed.SaveToFile('cd02.bin');
      msDeCompressed.Position := 0;
      if cCompressed = 'L' then
        // LZH:
        kbmCompress.LZHDecompressLoad(msDeCompressed, Output)
      else
        // ZIP:
        //kbmCompress.ZIPDecompressLoad(msDeCompressed, Output);
        exit;

      {$else}
      exit;
      {$endif}
    end
    else
    begin
      if bEncodeBase64 then
        EncdDecd.DecodeStream(ms, Output)
      else
        Output.LoadFromStream(ms);
    end;

  finally
    ms.Free;
    msDeCompressed.Free;
  end;

end;

procedure DecodeBinaryStringToBinary(Input, Output: TMemoryStream; out pFieldType: TFieldType);
var
  sInput: string;
begin
  pFieldType := ftUnknown;
  if Input.Size = 0 then
    exit;
  SetLength(sInput, Input.Size);
  Input.Position := 0;
  Input.Read( sInput[1], Input.Size);
  Output.Size := 0;
  DecodeStringToBinary(sInput, Output, pFieldType);
end;

procedure EncodeBinaryToString(Input: TStream; out sOutput: string; AFieldType: TFieldType;
  bCompressed: Boolean = False; bEncodeBase64: Boolean = False);
var
  ms, msCompressed:TMemoryStream;
  s: string;
  //cCompressed: Char;
begin
  sOutput := '';
  if Input.Size = 0 then
    exit;
  if not ( AFieldType in [ ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftTypedBinary, ftOraBlob, ftOraClob ] )
  then
    exit;
  if (Integer(AFieldType) >= 100) or (Integer(AFieldType) < 0) then
    exit;

  msCompressed := nil;
  ms := TMemoryStream.Create;
  try
    Input.Position := 0;
    //cCompressed := 'N'; // no compress
    {$ifdef _kbm_compress_}
    if bCompressed and (Input.Size >= 1024) then
    begin
      cCompressed := 'L'; // L- LZH, Z - ZIP
      msCompressed := TMemoryStream.Create;
      // compress stream
      if cCompressed = 'L' then
        // LZH:
        kbmCompress.LZHCompressSave(Input, msCompressed)
      else
        // ZIP:
        //kbmCompress.ZIPCompressSave(Input, msCompressed)
        exit;
      if Input.Size - msCompressed.Size > 1024 then
      begin
        //msCompressed.SaveToFile('cd01.bin');
        msCompressed.Position := 0;
        if bEncodeBase64 then
          EncdDecd.EncodeStream(msCompressed, ms)
        else
        begin
          FreeAndNil(ms);
          ms := msCompressed;
          msCompressed := nil;
        end;
      end
      else // ignore compress
      if bEncodeBase64 then
        EncdDecd.EncodeStream(Input, ms);
    end
    else
    {$endif}
      if bEncodeBase64 then
        EncdDecd.EncodeStream(Input, ms);

    // make header:
    sOutput := cBlobsAsStringIdTemplate;
    if bEncodeBase64 then
      sOutput[cBAS_BASE64_INDEX] := 'B'
    else
      sOutput[cBAS_BASE64_INDEX] := 'N';

    // compressed
    {$ifdef _kbm_compress_}
    if bCompressed then
      sOutput[cBAS_COMPRESSED_INDEX] := cCompressed
    else
    {$endif}
      sOutput[cBAS_COMPRESSED_INDEX] := 'N';

    // field data type
    s := IntToStr(Integer(AFieldType));
    if Length(s) < 2 then
      s := '0'+s;

    sOutput[cBAS_DATATYPE_INDEX] := s[1];
    sOutput[cBAS_DATATYPE_INDEX+1] := s[2];

    SetLength(sOutput, Length(sOutput) + ms.Size);
    ms.Position := 0;
    ms.Read(sOutput[Length(cBlobsAsStringIdTemplate)+1], ms.Size);

  finally
    ms.Free;
    msCompressed.Free;
  end;
end;

{Parse options in string:}
function GetOptionValue(var ConnectString: string; OptionName: string;
  HideOption: Boolean = False; TrimResult: Boolean = True; bOneChar: Boolean = False;
  const HideTemplate: string = #0): string;
var
  pLeft, pRight: Integer;
  sLeft, sVal: string;
  sUprConnectString: string;
  cLeft, lRight, cLeftVal, cTmp, cRight: PAnsiChar;
  bIsValue: Boolean;
begin
  Result := #0; // when result is #0 then value not found, but if result='' then value is empty
  if (OptionName = '') or (Length(ConnectString) <= Length(OptionName)) then
    exit;
  OptionName := UpperCase(OptionName);
  sUprConnectString := UpperCase(ConnectString);
  // seek to option name
  cLeft := StrPos( PAnsiChar(sUprConnectString), PAnsiChar(OptionName) );
  if cLeft=nil then
    exit;
  lRight := PAnsiChar(@sUprConnectString[Length(sUprConnectString)]);// pointed to last symbol
  cTmp := #0;
  bIsValue := False;
  repeat
    begin
      // check right limitation
      cLeftVal := cLeft;
      inc(cLeftVal, Length(OptionName));// seek to last symbol in OptionName
      if DWORD(cLeftVal) > DWORD(lRight) then
        break;
      // seek to symbol '='
      while cLeftVal^<>#0 do
      begin
        if cLeftVal^='=' then
        begin
          inc(cLeftVal,2);
          bIsValue := True;
          break;
        end
        else
        if cLeftVal ^ in [#9, #10, #13, #32] then
          inc(cLeftVal)
        else
          break;
      end;
      if bIsValue then
      begin
        // search left delimiter ';' or start pos sUprConnectString:
        cTmp := cLeft-1;
        while cTmp >= PAnsiChar(sUprConnectString) do
        begin
          if cTmp^=';' then
            break
          else
          if cTmp^ in [#9, #10, #13, #32] then
            dec(cTmp)
          else
          begin
            bIsValue := False;
            break;
          end;
        end;
        if bIsValue then
          break;
      end;
      // seek to next same option name
      cLeft := StrPos( cLeftVal, PAnsiChar(OptionName) );
    end //of: repeat:
  until (cLeft=nil);
  if bIsValue then
  begin
    pLeft := DWORD(cLeft)-DWORD(PAnsiChar(sUprConnectString));
    sLeft := Copy(ConnectString, 1, pLeft - 1);
    pLeft := DWORD(cLeftVal)-DWORD(PAnsiChar(sUprConnectString));
    cRight := StrPos( cLeftVal, '=' );
    if cRight = nil then
    begin
      pRight := Length(sUprConnectString) + 1;
      cTmp := lRight;
    end
    else
    begin
      dec(cRight);
      while (cRight > cTmp) and (cRight^<>';') do
        dec(cRight);
      cTmp := cRight;
      pRight := DWORD(cRight) - DWORD(PAnsiChar(sUprConnectString)) + 1;
    end;
    sVal := Copy(ConnectString, pLeft, pRight - pLeft{ + 1});
    if HideOption then
    begin
      if HideTemplate = #0 then
      begin
        // remove options value and name
        if cTmp^=';' then
          inc(pRight);
        pLeft := Length(ConnectString) - pRight + 1;
        if (pLeft>0)and(sLeft<>'')and(sLeft[Length(sLeft)]<>';') then
          sLeft := sLeft + ';';
        ConnectString := sLeft + Copy(ConnectString, pRight, pLeft);
      end
      else // hide only value
      begin
        // replace value to template
        ConnectString :=
          Copy(ConnectString, 1, pLeft-1) +
          HideTemplate +
          Copy(ConnectString, pRight, Length(ConnectString) - pRight + 1);
      end;
    end;
    if TrimResult then
      Result := Trim(sVal)
    else
      Result := sVal;
    if bOneChar then
      Result := (Result+' ')[1];
  end;
end;

{  DBCommon.NextSQLToken }

procedure RemoveComments(var sScript: string; const sDelimiter: string );
var
  DotStart: Boolean;
  p: PChar;
  Token: string;
  CurSection, pCurSection: TSQLToken;
  Result: TSQLToken;

  function NextTokenIs(Value: string; var Str: string): Boolean;
  var
    Tmp: PChar;
    S: string;
  begin
    Tmp := p;
    NextSQLToken(Tmp, S, CurSection);
    Result := AnsiCompareText(Value, S) = 0;
    if Result then
    begin
      Str := Str + ' ' + S;
      p := Tmp;
    end;
  end;

  function GetSQLToken(var Str: string): TSQLToken;
  var
    l: PChar;
    s: string;
  begin
    if Length(Str) = 0 then
      Result := stEnd else
    if (Str = '*') and (CurSection = stSelect) then
      Result := stAllFields else
    if DotStart then
      Result := stFieldName else
    if (AnsiCompareText('DISTINCT', Str) = 0) and (CurSection = stSelect) then
      Result := stDistinct else
    if (AnsiCompareText('ASC', Str) = 0) or (AnsiCompareText('ASCENDING', Str) = 0)then
      Result := stAscending else
    if (AnsiCompareText('DESC', Str) = 0) or (AnsiCompareText('DESCENDING', Str) = 0)then
      Result := stDescending else
    if AnsiCompareText('SELECT', Str) = 0 then
      Result := stSelect else
    if AnsiCompareText('AND', Str) = 0 then
      Result := stAnd else
    if AnsiCompareText('OR', Str) = 0 then
      Result := stOr else
    if AnsiCompareText('LIKE', Str) = 0 then
      Result := stLike else
    if (AnsiCompareText('IS', Str) = 0) then
    begin
      if NextTokenIs('NULL', Str) then
        Result := stIsNull else
      begin
        l := p;
        s := Str;
        if NextTokenIs('NOT', Str) and NextTokenIs('NULL', Str) then
          Result := stIsNotNull else
        begin
          p := l;
          Str := s;
          Result := stValue;
        end;
      end;
    end else
    if AnsiCompareText('FROM', Str) = 0 then
      Result := stFrom else
    if AnsiCompareText('WHERE', Str) = 0 then
      Result := stWhere else
    if (AnsiCompareText('GROUP', Str) = 0) and NextTokenIs('BY', Str) then
      Result := stGroupBy else
    if AnsiCompareText('HAVING', Str) = 0 then
      Result := stHaving else
    if AnsiCompareText('UNION', Str) = 0 then
      Result := stUnion else
    if AnsiCompareText('PLAN', Str) = 0 then
      Result := stPlan else
    if (AnsiCompareText('FOR', Str) = 0) and NextTokenIs('UPDATE', Str) then
      Result := stForUpdate else
    if (AnsiCompareText('ORDER', Str) = 0) and NextTokenIs('BY', Str)  then
      Result := stOrderBy else
    if AnsiCompareText('NULL', Str) = 0 then
      Result := stValue else
    if CurSection = stFrom then
      Result := stTableName else
      Result := stFieldName;
  end;

var
  TokenStart: PChar;

  procedure StartToken;
  begin
    if not Assigned(TokenStart) then
      TokenStart := p;
  end;

  procedure JamComment;
  begin
    if Assigned(TokenStart) and Assigned(p) then
    begin
      FillChar(TokenStart^, dword(p) - dword(TokenStart), #1);
    end;
  end;

var
  Literal: Char;
  Mark: PChar;
  bExit: Boolean;
begin
  p := PChar(sScript);
  pCurSection := stUnknown;
  Result := stUnknown;

  repeat

  CurSection := stUnknown;
  //Result := stUnknown;
  Token := '';

  TokenStart := nil;
  DotStart := False;
  bExit := False;
  while True do
  begin
    case p^ of
      '"','''','`':
      begin
        StartToken;
        Literal := p^;
        Mark := p;
        repeat Inc(p) until (p^ in [Literal,#0]);
        if p^ = #0 then
        begin
          p := Mark;
          Inc(p);
        end else
        begin
          Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Mark := PChar(Token);
          Token := AnsiExtractQuotedStr(Mark, Literal);
          if DotStart then
            Result := stFieldName else
          if p^ = '.' then
            Result := stTableName else
            Result := stValue;
          //Exit;
          //bExit := True; break;
          break;
        end;
      end;
      '/':
      begin
        StartToken;
        Inc(p);
        if (sDelimiter <> '//') and ((sDelimiter <> '/*'))then
        begin
          if p^ in ['/','*'] then
          begin
            if p^ = '*' then
            begin
              repeat Inc(p) until (p = #0) or ((p^ = '*') and (p[1] = '/'));
            end else
              while not (p^ in [#0, #10, #13]) do Inc(p);
            SetString(Token, TokenStart, p - TokenStart);
            Result := stComment;
            //Exit;
            JamComment();
            //bExit := True; break;
            break;
          end;
        end;
      end;
      {+} // ADDED NEW:
      '-':
      begin
        StartToken;
        Inc(p);
        if sDelimiter <> '--' then
        begin
          if (p^ ='-') and (Length(Token) = 0) then
          begin
            while not (p^ in [#0, #10, #13]) do Inc(p);
            SetString(Token, TokenStart, p - TokenStart);
            Result := stComment;
            JamComment();
            //Exit;
            //bExit := True; break;
            break;
          end;
        end;
      end;
      {+.}
      ' ', #10, #13, ',', '(':
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := GetSQLToken(Token);
          //Exit;
          //bExit := True; break;
          break;
        end else
          while (p^ in [' ', #10, #13, ',', '(']) do Inc(p);
      end;
      '.':
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := stTableName;
          //Exit;
          //bExit := True; break;
          break;
        end else
        begin
          DotStart := True;
          Inc(p);
        end;
      end;
      '=','<','>':
      begin
        if not Assigned(TokenStart) then
        begin
          TokenStart := p;
          while p^ in ['=','<','>'] do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stPredicate;
          //Exit;
          //bExit := True; break;
          break;
        end;
        Inc(p);
      end;
      '0'..'9':
      begin
        if not Assigned(TokenStart) then
        begin
          TokenStart := p;
          while p^ in ['0'..'9','.'] do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stNumber;
          //Exit;
          //bExit := True; break;
          break;
        end else
          Inc(p);
      end;
      #0:
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := GetSQLToken(Token);
          //Exit;
          //bExit := True; break;
          break;
        end else
        begin
          Result := stEnd;
          Token := '';
          //Exit;
          //bExit := True; break;
          break;
        end;
      end;
    else
      StartToken;
      Inc(p);
    end;//of: case p^
    if bExit then
      break;
  end;//of: while True


    if Result in SQLSections then
      pCurSection := Result;
    CurSection := pCurSection;
  until Result in [stEnd, stFrom];

  sScript := StringReplace( sScript, #1, '', [rfReplaceAll, rfIgnoreCase]);
end;

{ TClientDataSet }

procedure TClientDataSet.ReadRecordToVariant(var vDest: Variant;
  bPackBlobsToString: Boolean = False; bCompressed: Boolean = False; bEncodeBase64: Boolean = False);
var
  i: Integer;
  BlobStream: TStream;
  ms: TMemoryStream;
  s: string;
  AFieldType: TFieldType;
begin
  vDest := Null;
  if (not Active) or (IsEmpty) then
    exit;

  vDest := VarArrayCreate([0, FieldCount - 1], varVariant);
  ms := nil;
  try

  for i := 0 to FieldCount - 1 do
    if (not Fields[i].ReadOnly) then
    begin
      if bPackBlobsToString and
        (Fields[i].DataType in [ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
        ftTypedBinary, ftOraBlob, ftOraClob] ) then
      begin
        BlobStream := CreateBlobStream(TBlobField(Fields[i]), bmRead);
        try
          if (bCompressed or bEncodeBase64) and // if need binary data handle
            (not BinaryIsStringBinary(BlobStream, AFieldType)) then
          begin
            EncodeBinaryToString(BlobStream, s, Fields[i].DataType, bCompressed, bEncodeBase64);
            vDest[i] := s;
          end
          else
            vDest[i] := Fields[i].Value;
        finally
          BlobStream.Free;
        end;
      end
      else
        vDest[i] := Fields[i].Value;
    end;

  finally
    ms.Free;
  end;

end;

procedure TClientDataSet.WriteVarintToRecord(const vSource: Variant);
var
  i: Integer;
  OAV: TAutoRefreshFlag;
begin
  if (not Active) or (IsEmpty) or VarIsNull(vSource) then
    exit;

  DataSetToEditMode(Self);

  for i := 0 to FieldCount - 1 do
    if (not Fields[i].ReadOnly) then
    begin
      if Fields[i].AutoGenerateValue <> arNone then
      begin
        OAV := Fields[i].AutoGenerateValue;
        Fields[i].AutoGenerateValue := arNone;
      end
      else
        OAV := arNone;

      Fields[i].Value := vSource[i];

      if OAV <> arNone then
        Fields[i].AutoGenerateValue := OAV;
    end;

end;

procedure TClientDataSet.CopyRecord;
var
  V: Variant;
begin
  if (not Active) or (IsEmpty) then
    exit;

  ReadRecordToVariant(V, False);

  try
    DisableControls;
    Next;
    if not EOF then
      Insert
    else
      Append;
  finally
    EnableControls;
  end;

  WriteVarintToRecord(V);

end;

procedure TClientDataSet.MarkCurrentRecordsAsNew(bPackBlobsToString: Boolean = False;
  bCompressed: Boolean = False; bEncodeBase64: Boolean = False);
var
  vRec: Variant;
  OIFN, OIN: string;
begin
   if (not Active) or IsEmpty then
     exit;

   DisableControls;
   try

   MergeChangeLog;
   OIFN := IndexFieldNames;
   OIN := IndexName;
   IndexFieldNames := '';;
   IndexName := '';;
   LogChanges := True;
   vRec := Null;

   ReadRecordToVariant(vRec, bPackBlobsToString, bCompressed, bEncodeBase64);
   LogChanges := False;
   Delete();
   LogChanges := True;
   Append;
   WriteVarintToRecord(vRec);
   Post;

   if OIFN<> '' then
     IndexFieldNames := OIFN;
   if OIN <> '' then
     IndexName := OIN;

   finally
     LogChanges := True;
     EnableControls;
   end;
end;

procedure TClientDataSet.MarkAllRecordsAsNew(bPackBlobsToString: Boolean = False;
  bCompressed: Boolean = False; bEncodeBase64: Boolean = False);
var
  vRec: Variant;
  i, iRC: Integer;
  OIFN, OIN: string;
begin
   if (not Active) or IsEmpty then
     exit;

   DisableControls;
   try

   MergeChangeLog;
   OIFN := IndexFieldNames;
   OIN := IndexName;
   IndexFieldNames := '';;
   IndexName := '';;
   LogChanges := True;
   iRC := RecordCount;
   vRec := Null;


   for i := 0 to iRC-1 do
   begin
     First;
       ReadRecordToVariant(vRec, bPackBlobsToString, bCompressed, bEncodeBase64);
     LogChanges := False;
     Delete();
     LogChanges := True;
     Append;
       WriteVarintToRecord(vRec);
     Post;
   end;

   if OIFN<> '' then
     IndexFieldNames := OIFN;
   if OIN <> '' then
     IndexName := OIN;

   finally
     LogChanges := True;
     EnableControls;
   end;
end;

procedure TFormOdbcExplor.FormCreate(Sender: TObject);
var
  AllowChange: Boolean;
  sFileName: String;
  i,j: integer;
  s: String absolute sFileName;
  SL: TStringList;
  {$ifdef _GraphicEx_}
  mi: TMenuItem;
  TargetClass: TGraphicClass;

  Base: TSaveToStream;
  Impl: TSaveToStream;
  ClassTGraphic: TClass;

  {$endif ifdef _GraphicEx_}
const
   MaxHistoryLines = 300;
begin
  SQLMonitor := TSQLMonitor.Create(Self);
  SQLMonitor.OnTrace := SQLMonitorTrace;
  SQLMonitor.SQLConnection := SQLConnection1;
  SQLMonitor.Active := cb_sql_monitor_active.Checked;

  //{$IFDEF DbxStaticLink}
  //ShowMessage('DbxOpenOdbcStatic: ' + IntToStr(Integer(DbxOpenOdbcStatic.IsStatic())));
  //{$ENDIF}
  {$IFDEF _D10UP_}
    DataSetProvider1.OnGetTableName := DataSetProvider1GetTableName_D10_;
  {$ELSE}
    DataSetProvider1.OnGetTableName := DataSetProvider1GetTableName_D67_;
  {$ENDIF}

  fSheetActions[0] := ac_Connection;
  fSheetActions[1] := ac_Query;
  fSheetActions[2] := ac_Memo;
  fSheetActions[3] := ac_Image;
  fSheetActions[4] := ac_Binary;
  fSheetActions[5] := nil; // ac_SQLMonitor

  SQLConnection1.Params.Clear;
  //edDecimalSeparator.Text := DecimalSeparator;
  gbCustConOpt.Height := sh_template.Top + 2;
  ScrollBox1.VertScrollBar.Position := 0;

  edLockMode.Hint :=
    '-1: Suspends the process until the lock is released.'#13#10+
    ' 0: Ends the operation immediately and returns an error code.'#13#10+
    '>0: Suspends the process until the lock is released, or until the end of'
      + ' the specified number of seconds.'#13#10+
    '    Default: cLockModeDefault = 17.';

  Caption := {$IFDEF _DEBUG_}'DEBUG: '+{$ENDIF}cCaptionDef;
  lbTableName.Caption := '';
  cbDefConOptClick(nil);
  FillChar(TD, Sizeof(TD), 0);
  TD.TransactionID := 1;

  TabSet1.TabIndex := 0;
  Notebook1.PageIndex := TabSet1.TabIndex;
  pcData.ActivePageIndex := 0;
  TabSet1.Visible := False;
  TabSet1Change(TabSet1, 0, AllowChange);

  CDSQuery.CreateDataSet;
  CDSQuery.LogChanges := False;

  CreateMemoSql();

  {$ifdef _HexEditor_}
  HexEditor := THexEditor.Create(Self);
  HexEditor.Align := alClient;
  HexEditor.DoubleBuffered := True;
  HexEditor.Parent := pBinary;
  HexEditor.ReadOnlyView := False;
  HexEditor.ReadOnlyFile := False;
  HexEditor.NoSizeChange := False;
  HexEditor.AllowInsertMode := True;
  HexEditor.CreateBackup := False;
  pBinary.Caption := '';
  cbUnicodeChars.Visible := True;
  btBlobSave.Visible := True;
  btBlobLoad.Visible := True;
  btBlobClear.Visible := True;
  {$endif ifdef _HexEditor_}

  SL := TStringList.Create;
  {$ifdef _GraphicEx_}
  FileFormatList.GetExtensionList(SL);
  for i:=0 to SL.Count-1 do
  begin // Create Recompres/Convert Menus
    TargetClass := FileFormatList.GraphicFromExtension(Sl[i]);

    if TargetClass=nil then
      continue;

    if TargetClass.InheritsFrom(TIcon) then
      continue;

    if TargetClass.InheritsFrom(TMetafile) then
      continue;

    mi := TMenuItem.Create(pmImage);
    mi.OnClick := pmImageRecompessAs;

    // ------ check begin:
    ClassTGraphic := TargetClass;
    if TargetClass.InheritsFrom(TGraphicExGraphic) then
    begin
      Impl := TGraphicExGraphic(@ClassTGraphic).SaveToStream;
      while (ClassTGraphic <> nil) and (ClassTGraphic <> TGraphicExGraphic) do
        ClassTGraphic := ClassTGraphic.ClassParent;
      if ClassTGraphic <> nil then
        Base := TGraphicExGraphic(@ClassTGraphic).SaveToStream;
    end
    else
    begin
      Impl := TGraphic(@ClassTGraphic).SaveToStream;
      while (ClassTGraphic <> nil) and (ClassTGraphic <> TGraphic) do
        ClassTGraphic := ClassTGraphic.ClassParent;
      if ClassTGraphic <> nil then
        Base := TGraphic(@ClassTGraphic).SaveToStream;
    end;
    mi.Enabled := not (
       (ClassTGraphic = nil) or (TMethod(Impl).Code = TMethod(Base).Code)
    );
    //--------check end.

    mi.Caption := 'to "'+SL[i]+'" - ' + FileFormatList.GetDescription(TargetClass);
    mi.Hint := SL[i];
    pmImageRecompess.Add(mi);
  end;

  {$endif ifdef _GraphicEx_}
  pmImageRecompess.Enabled := pmImageRecompess.Count>0;

  pImage.DoubleBuffered := True;
  sbImage.DoubleBuffered := True;
  dbImage1.DoubleBuffered := True;

  LinkEditors(False);

  SL.Clear;
  try
    sFileName := ExtractFilePath(ParamStr(0))+cSaveConnectionFile;
    if FileExists(sFileName) then
      try
        SL.LoadFromFile(sFileName);
        // count is limited to MaxHistoryLines
        if SL.Count > MaxHistoryLines then
        begin
          s := '';
          for i:=0 to MaxHistoryLines-1 do
          begin
            s:=s+SL[i]+#13#10;
          end;
          SL.Text := s;
        end;
        i := SL.IndexOf('?');
        if i>=0 then
          SL.Delete(i);
      except
        SL.Clear;
      end;
    // drop empty lines
    for i:=SL.Count-1 downto 0 do
      if Trim(SL[i])='' then
        SL.Delete(i);
    // drop duplicates
    i:=1;
    while i<SL.Count do
    begin
      s := Sl[i-1];
      for j := SL.Count-1 downto i do
      begin
        if SL[j]=s then
          Sl.Delete(j);
      end;
      inc(i);
    end;
    // add query connection string to first line
    SL.Insert(0, '?');
    dfDSN.Items.Text := SL.Text;
  finally
    SL.Free;
  end;

  if dfDSN.Items.Count>1 then
  begin
    dfDSN.ItemIndex := 1;  // Last connected string
    dfDSNSelect(dfDSN);
  end
  else
    dfDSN.ItemIndex := 0; // '?'

  {$IFDEF _XPMENU_}
  begin
    fXPMenu := XPMenu.TXPMenu.Create(Self);
    fXPMenu.Active := False;
    fXPMenu.xpContainers := [xccForm, xccFrame, xccPanel, {xccScrollBox,} xccTabSheet, xccPageScroller];
    fXPMenu.xpControls := [xcMainMenu, xcPopupMenu, xcListBox, xcEdit, xcMaskEdit, xcMemo, xcRichEdit,
      xcMiscEdit, xcCheckBox, xcButton, xcBitBtn, xcSpeedButton, xcUpDown, xcPanel, xcTreeView, xcListView];
  end;
  {$ENDIF}

  sFileName := ExtractFilePath(ParamStr(0))+cSaveQueriesFile;
  if FileExists(sFileName) then
  try
    CDSQuery.LoadFromFile(sFileName);
  except
    CDSQuery.Close;
    CDSQuery.CreateDataSet;
  end;
  CDSQuery.LogChanges := False;

  if not CDSQuery.isEmpty then
  begin
    CDSQuery.Last; // Last executed Query
    //CDSQuery.Edit
  end
  else
  begin
    CDSQuery.Append;
    CDSQuery.Fields[1].AsString := 'SELECT * FROM `country`';//'SELECT * FROM "dbo"."customer"';
    CDSQuery.Post;
  end;

  {$ifdef _kbm_compress_}
    //cbCompressAfterPack.Checked := True;
    cbCompressBlob.Enabled := True;
    gbTextBlob.Enabled := True;
    cbCompressBlob.Enabled := True;
    cbBase64.Enabled := True;
    cbPackBlobsToString.Enabled := True;
  {$else}
    cbCompressBlob.Checked := False;
    cbCompressBlob.Enabled := False;
    gbTextBlob.Enabled := False;
    cbCompressBlob.Enabled := False;
    cbBase64.Enabled := False;
    cbPackBlobsToString.Enabled := False;
  {$endif}

  fDetachedSQLList := TList.Create;
  {
  pbMarkAllRecordsAsNew.Visible := True;
  pbMarkCurrentRecordsAsNew.Visible := True;
  gbTextBlob.Visible := True;
  cbPackBlobsToString.Visible := True;
  {}
end;

procedure TFormOdbcExplor.CreateMemoSql();
begin
  if memoSql <> nil then
    FreeAndNil(memoSql);
  memoSql := TSQLDBMemo.Create(Self);
  memoSQL.DataSource := sCDSQuery;
  memoSQL.DataField := 'qry';
  memoSql.ScrollBars := ssBoth;
  {$ifdef _SynEdit_}
  memoSql.Options := memoSql.Options
    + [eoGroupUndo, eoShowScrollHint, eoTabsToSpaces, eoTrimTrailingSpaces];
  memoSql.BorderStyle := bsNone;
  memoSql.Gutter.ShowLineNumbers := True;
  memoSql.Gutter.Visible := True;
  memoSql.RightEdge := 120;
  memoSql.TabWidth := 4;
  memoSql.WantReturns := True;
  memoSql.WantTabs := True;

  //memoSql.Color := clSilver;
  //memoSql.ActiveLineColor := $00F9E6E6;
  //memoSql.RightEdgeColor := $00F9E6E6;

  if SynSQLSyn <> nil then
    FreeAndNil(SynSQLSyn);
  SynSQLSyn := TSynSQLSyn.Create(Self);
  SynSQLSyn.SQLDialect := sqlStandard;
  memoSQL.Highlighter := SynSQLSyn;
  memoSql.Font.Size := 12;
  memoSql.Gutter.Font.Size := memoSql.Font.Size;
  {$endif ifdef _SynEdit_}
  memoSql.PopupMenu := pmMemo;
  memoSQL.Enabled := True;
  memoSQL.Visible := True;
  memoSQL.Align := alClient;
  memoSQL.Parent := pSQL;
  //memoSQL.Align := alNone;
  //memoSQL.Anchors := [akLeft,akTop,akRight,akBottom];
end;

procedure TFormOdbcExplor.FormDestroy(Sender: TObject);
//var
//  i: integer;
begin
  // BUG: AV in D7, D9 when use XPMan and SqlConnection1.Connected is "true".
  {
  if SqlConnection1.Connected then
  begin
    SqlConnection1.Close;
    //Application.ProcessMessages();
  end
  else
    SaveConfigurations;
  for i:=fDetachedSQLList.Count-1 downto 0 do
  begin
    TObject(fDetachedSQLList[i]).Free;
    fDetachedSQLList[i] := nil;
  end;
  fDetachedSQLList.Free;
  {}
end;

procedure HaltEx(uExitCode: Integer = 0);
begin
  TerminateProcess(OpenProcess(PROCESS_TERMINATE, False, GetCurrentProcessID()), uExitCode);
end;

procedure TFormOdbcExplor.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: integer;
begin
  if SqlConnection1.Connected then
  begin
    SqlConnection1.Close;
    //Application.ProcessMessages();
    {$ifdef _SynEdit_}
    FreeAndNil(memoSql);
    {$endif ifdef _SynEdit_}
  end
  else
    SaveConfigurations;

  {.$IFNDEF _D10UP_}
  {$IFNDEF  _CHECK_LEAKS_}
  HaltEx(0);
  {$ENDIF}
  {.$ENDIF}

  for i:=fDetachedSQLList.Count-1 downto 0 do
  begin
    TObject(fDetachedSQLList[i]).Free;
    fDetachedSQLList[i] := nil;
  end;
  fDetachedSQLList.Free;
end;

procedure TFormOdbcExplor.SaveConfigurations;
begin
  // Save Configuration
  try
    dfDSN.Items.SaveToFile(ExtractFilePath(ParamStr(0))+cSaveConnectionFile);
    if CDSQuery.Active then
    begin
       if ( CDSQuery.State in [dsInsert,dsEdit] ) then
        CDSQuery.Post;
      if (not CDSQuery.IsEmpty) then
        CDSQuery.SaveToFile(ExtractFilePath(ParamStr(0))+cSaveQueriesFile);
    end;
  except
  end
end;

procedure TFormOdbcExplor.SetConnectionOptionsBeforeConnect;
begin
  if SQLConnection1.Connected then
    exit;
  if not cbDefConOpt.Checked then
  begin
    //if cbInternalCloneConnection.Checked then
       cbInternalCloneConnectionClick(nil);
  end
  else
  begin
    cbInternalCloneConnectionClick(nil);
  end;
end;

procedure TFormOdbcExplor.SetConnectionOptionsAfterConnect;
begin
  if not SQLConnection1.Connected then
    exit;
  if not cbDefConOpt.Checked then
  begin
    // Custom Connection Options
    // **************************
    //if not cbSafeMode.Checked then
      cbSafeModeClick(nil); // default is True in driver
    //if cbTrimChar.Checked then
      cbTrimCharClick(nil);// default is False in driver
    //if not cbMapInt64ToBcd.Checked then
      cbMapInt64ToBcdClick(nil);// default is True in driver
    cbIgnoreUnknownFieldTypeClick(nil);
    //if cbEmptyStrParam.Checked then
      cbEmptyStrParamClick(nil); // default is False in driver
    edBlobChunkSizeChange(nil);
    edConPacketSizeChange(nil);
    //if cbMixedFetch.Checked then
      cbMixedFetchClick(nil);
    //if cbAutoInc.Checked then
      cbAutoIncClick(nil);
    edBlockRowsCountChange(nil);
    //if not cbEnableBCD.Checked then
      cbEnableBCDClick(nil); // default is True in driver
    //if not cbBCD2Exp.Checked then
      cbBCD2ExpClick(nil); // default is True in driver
    if edDecimalSeparator.Text <> '' then
      edDecimalSeparatorChange(nil);
    //if cbMaxBCD.Checked then
      cbMaxBCDClick(nil); // default is False in driver
    //if cbParamDateByOdbcLevel2.Checked then
      cbParamDateByOdbcLevelClick(nil);
    //if cbReadOnlyField.Checked then
      cbReadOnlyFieldClick(nil);
    // if cbEmptyStrParam.Checked tyhen
      cbEmptyStrParamClick(nil);
    //if cbNullStrParam.Checked then
      cbNullStrParamClick(nil);
    //if cbMapSmallBcdToNative.Checked then
      cbMapSmallBcdToNativeClick(nil);
    //if cbMapCharAsBDE.Checked then
      cbMapCharAsBDEClick(nil);
    //if cbEnableUnicode.Checked then
      cbEnableUnicodeClick(nil);
    if edLockMode.Text <> '' then
      edLockModeChange(nil);
    //if cbReadOnly.Checked then
      cbReadOnlyClick(nil);
    if edConTimeout.Text <> '' then
      edConTimeoutChange(nil);
    //if cbSupportsSchemaFilter.Checked then
      cbSupportsSchemaFilterClick(nil);
    //if cbBlobFragmntns.Checked then
      cbBlobFragmntnsClick(nil);
    //if cbBlobNotTermChar.Checked then
      cbBlobNotTermCharClick(nil);
    if edQueryTimeout.Text <> '' then
      edQueryTimeoutChange(nil);
  end
  else
  begin
    // Default Connection Options
    // **************************
    cbSafeModeClick(nil);
    cbTrimCharClick(nil);
    cbMapInt64ToBcdClick(nil);
    cbIgnoreUnknownFieldTypeClick(nil);
    cbEmptyStrParamClick(nil);
    edBlobChunkSizeChange(nil);
    edConPacketSizeChange(nil);
    cbMixedFetchClick(nil);
    cbAutoIncClick(nil);
    edBlockRowsCountChange(nil);
    cbEnableBCDClick(nil);
    cbBCD2ExpClick(nil);
    edDecimalSeparatorChange(nil);
    cbMaxBCDClick(nil);
    cbParamDateByOdbcLevelClick(nil);
    cbReadOnlyFieldClick(nil);
    cbEmptyStrParamClick(nil);
    cbNullStrParamClick(nil);
    cbMapSmallBcdToNativeClick(nil);
    cbMapCharAsBDEClick(nil);
    cbEnableUnicodeClick(nil);
    edLockModeChange(nil);
    cbReadOnlyClick(nil);
    edConTimeoutChange(nil);
    cbSupportsSchemaFilterClick(nil);
    cbBlobFragmntnsClick(nil);
    cbBlobNotTermCharClick(nil);
    edQueryTimeoutChange(nil);
  end;
end;

function ExtractVendorLibName(sLib: string): string;
var
  iTmp, iLev: integer;
begin
  iTmp := Pos(':', sLib);
  if (iTmp > 0) and (iTmp < Length(sLib)) then
  begin
    iLev := StrToIntDef(Copy(sLib, 1, iTmp-1), -1);
    if iLev > 0 then
      sLib := Copy(sLib, iTmp+1, Length(sLib)-iTmp)
    else
      iLev := -1;
  end
  else
    iLev := -1;
  Result := ExtractFileName(sLib);
  if iLev > 0 then
    Result := IntTostr(iLev) + ':' + Result;
end;

procedure TFormOdbcExplor.pbConnectClick(Sender: TObject);
var
  sConString, sVendorLib, sVendorDesc: String;
  Status: SQLResult;
  Len : Smallint;
  vCursor: TCursor;
  {begin: exception stack demo}
  //  O: TObject;
  {end.}
begin
  //Canvas.Lock;
  //try

  {begin: exception stack demo}
  {
    O := Self;
   if O <> nil then
     O := nil;
   ShowMessage( O.ClassName() );
  //}
  {end.}
  vCursor := Screen.Cursor;
  Screen.Cursor := crSQLWait;
  try
  try
    TickCountInfo := GetTickCount;

    //SQLConnection1.Connected := not (SQLConnection1.Connected);
    if SQLConnection1.Connected then
    begin
      SQLConnection1.Connected := False;
      TreeView1.FullCollapse;
      TreeView1.Items.Clear;
    end
    else
    try
      //pTop.Visible := False;
      if cbDefConOpt.Checked then
        SQLConnection1.Params.Clear;
      try
        SQLConnection1.Connected := True;
      finally
        // remove cached connection options cConnectionOptionsNames[coInternalCloneConnection]:
        sConString := dfDSN.Text;
        sVendorLib := GetOptionValue(sConString, cConnectionOptionsNames[coInternalCloneConnection],
          {HideOption=}True, {TrimResult=}True, {bOneChar=}False,
          {HideTemplate=}#0);
        if sVendorLib <> #0 then
          dfDSN.Text := sConString;
      end;
    except
      on e:Exception do
      begin
        if pos(DbxOpenOdbcInterface.rsNotSpecifiedDNSName, E.Message) > 0 then
          exit
        else
          raise;
      end;
    end;

//    exit; //???

    TickCountInfo := GetTickCount - TickCountInfo;
    SB.Panels[0].Text := Format(cTickCountInfo, [TickCountInfo]);

    if SQLConnection1.Connected then
    begin
      repeat

        pbConnect.Caption := 'Disc&onnect';
        // goto query page
        TabSet1.Visible := True;
        TabSet1.TabIndex := 1;
        // save last connection string to second position (after '?')
        sConString := Trim(dfDSN.Text);
        if sConString <> '' then
          sConString := GetOptionValue(sConString, 'DSN');
        if (dfDSN.Text='?') or (sConString='') or (sConString='?') then
        begin
          Len := 0;
          Status := SQLConnection1.SQLConnection.getOption(TSQLConnectionOption(xeConnConnectionName),
            nil, 0, Len);
          if (Status <> 0) or (Len <= 0) then
            break;
          SetLength(sConString, Len);
          Status := SQLConnection1.SQLConnection.getOption(TSQLConnectionOption(xeConnConnectionName),
            PChar(sConString), Len, Len);
          if Len > 0 then
            SetLength(sConString, Len);
          if (Status <> 0) or (Len <= 0) then
            break;
        end
        else
        begin
          sConString := dfDSN.Text;
        end;

        sVendorLib := GetOptionValue(sConString, 'VENDORLIB',
          {HideOption=}False, {TrimResult=}True, {bOneChar=}False,
          {HideTemplate=}#0);
        if sVendorLib = #0 then
        begin
          sVendorDesc := VendorLib.Text;
          sVendorLib := GetOptionValue(sVendorDesc, 'VENDORLIB',
            {HideOption=}False, {TrimResult=}True, {bOneChar=}False,
            {HideTemplate=}#0);
          if (sVendorLib <> #0) and (CompareText(sVendorLib, 'odbc32.dll') <> 0) then
          begin
            if Pos('=', sConString) > 1 then
              sConString := 'VendorLib='+sVendorLib + ';' + sConString
            else
              sConString := 'VendorLib='+sVendorLib + ';DSN=' + sConString
          end;
        end;
        if sVendorLib <> #0 then
        begin
          sVendorLib := ExtractVendorLibName(sVendorLib);
          sVendorDesc := ';VENDORLIB='+UpperCase(sVendorLib)+';';
          for Len := 0 to VendorLib.Items.Count-1 do
          begin
            //if Pos(sVendorDesc, UpperCase(VendorLib.Items[Len])) > 0 then
            if Pos(sVendorDesc, ';'+UpperCase(Trim(VendorLib.Items[Len])+';')) > 0 then
            begin
              sVendorDesc := #0;
              VendorLib.ItemIndex := Len;
              Break;
            end;
          end;
          if sVendorDesc <> #0 then
          begin
            sVendorDesc := 'VendorName=?' + sVendorLib +';VendorLib='+sVendorLib;
            VendorLib.Items.Add(sVendorDesc);
            VendorLib.ItemIndex := VendorLib.Items.Count-1;
          end;
        end
        else
        begin
          VendorLib.ItemIndex := 0;
        end;

        Len := dfDSN.Items.IndexOf(sConString);
        if (Len>1)or(Len<0) then
        begin
          if Len>0 then
            dfDSN.Items.Delete(Len);
          dfDSN.Items.Insert(1, sConString);
        end;
        dfDSN.Text := sConString;

      until True; // break
    end
    else
      pbConnect.Caption := 'C&onnect';


  except
    on e: Exception do
    begin
      if not SQLConnection1.Connected then
        SQLConnection1AfterDisconnect(SQLConnection1)
      else
        SQLConnection1AfterConnect(SQLConnection1);
      //raise;
      //Application.ProcessMessages;
      //TWinControlPro(Self).RecreateWnd();
      Application.HandleException(e);
    end;
  end;
  finally
    Screen.Cursor := vCursor;
  end;

  //finally
  //  Canvas.UnLock;
  //  pTop.Visible := True;
  //end;
end;

procedure TFormOdbcExplor.SQLConnection1BeforeConnect(Sender: TObject);
var
  sConString, sVendorLib, sVendorDesc: string;
begin
  if (not cbDefConOpt.Checked) then
    SetConnectionOptionsBeforeConnect;

  SQLConnection1.AutoClone := True;
  SQLConnection1.DriverName := 'OpenOdbc';
  SQLConnection1.GetDriverFunc := 'getSQLDriverODBC';
  SQLConnection1.LibraryName := ExtractFileDir(Application.ExeName) + '\dbxoodbc.dll';
  if not FileExists(SQLConnection1.LibraryName) then
    SQLConnection1.LibraryName := 'dbxoodbc.dll';
  sConString := dfDSN.Text;
  if Trim(sConString) = '' then
  begin
    sConString := '?';
    dfDSN.Text := '?';
  end;
  sVendorLib := GetOptionValue(sConString, 'VENDORLIB',
    {HideOption=}True, {TrimResult=}True, {bOneChar=}False,
    {HideTemplate=}#0);
  if sVendorLib = #0 then
  begin
    sVendorDesc := VendorLib.Text;
    sVendorLib := GetOptionValue(sVendorDesc, 'VENDORLIB',
      {HideOption=}False, {TrimResult=}True, {bOneChar=}False,
      {HideTemplate=}#0);
  end;
  if sVendorLib <> #0 then
    SQLConnection1.VendorLib := sVendorLib
  else
    SQLConnection1.VendorLib := 'odbc32.dll';


// *** CONNECTION STRING ***

  // NEW UNIVERSAL dbxoodbc connection method:
  SQLConnection1.Params.Values[CUSTOM_INFO] := cConnectionOptionsNames[coConnectionString] + '=' + sConString;
  SQLConnection1.Params.Values[DATABASENAME_KEY] := '?'; // '?' == ignored value when CONNECTION_STRING is defined.

  // OLD CODE: not work when bFixedSqlExpr = False because CONNECTION_STRING is limited to 255 chars.
  (*
  {.$IFDEF _D9UP_}
  //  SQLConnection1.Params.Values[DATABASENAME_KEY] := '?'; // '?' == ignored value when CONNECTION_STRING is defined.
  //  // Only for Unicode driver:
  //  SQLConnection1.Params.Values[CONNECTION_STRING] := sConString;
  {.$ELSE}
    {$ifndef _DBX_FIXES_}
    if Length(sConString)>255 then                   1
    {$else}
    if not bFixedSqlExpr then
    {$endif}
    SetLength(sConString, 255);
    SQLConnection1.Params.Values[DATABASENAME_KEY] := sConString;
  {.$ENDIF}
  //*)

// *** AUTHORIZATION ***

  SQLConnection1.Params.Values['User_Name'] := dfUID.Text;
  SQLConnection1.Params.Values['Password'] := dfPWD.Text;

// --
  //SQLConnection1.Params.Values[DECIMALSEPARATOR] := '.'; // *** debug ***

end;

procedure TFormOdbcExplor.VendorLibChange(Sender: TObject);
var
  S, sConString, sVendorLib, sVendorLibC: string;
begin
  if SQLConnection1.Connected or (VendorLib.Tag=1) then
    exit;

  S := VendorLib.Text;
  sVendorLibC := GetOptionValue(S, 'VENDORLIB',
    {HideOption=}False, {TrimResult=}True, {bOneChar=}False,
    {HideTemplate=}#0);

  sConString :=dfDSN.Text;
  //replace if exist:
  sVendorLib := GetOptionValue(sConString, 'VENDORLIB',
    {HideOption=}True, {TrimResult=}True, {bOneChar=}False,
    {HideTemplate=}sVendorLibC);

  if sVendorLib <> #0 then
    dfDSN.Text := sConString;
end;

type
  TSQLConnectionPro = class(TSQLConnection);

procedure TFormOdbcExplor.SQLConnection1AfterConnect(Sender: TObject);
var
  STransIsolationKey: string;
  iLen : Smallint;
  Status: SQLResult;
const
  cBool: array[Boolean] of string = ('False', 'True');
begin

  ac_Main.State := asNormal;

  if (not cbDefConOpt.Checked) then
    SetConnectionOptionsAfterConnect
  else
  begin
    STransIsolationKey := dfDSN.Text;
    if GetOptionValue(STransIsolationKey, cConnectionOptionsNames[coTrimChar],
    {HideOption=}False, {TrimResult=}False, {bOneChar=}False,
    {HideTemplate=}'') = #0 then
      SQLConnection1.SQLConnection.SetOption(TSQLConnectionOption(xeConnTrimChar), Integer(True) );
  end;

  pbConnect.Caption := 'Disc&onnect';

  chkConnected.Checked := SQLConnection1.Connected;
  pbApplyUpdates.Enabled := CDS.Active and (CDS.ChangeCount>0);
  btnBeginTransaction.Enabled := True;
  btnCommit.Enabled := True;
  btnRollack.Enabled := True;
  cbInternalCloneConnection.Enabled := False;
  cbPacketRecords.Enabled := True;
  edPacketRecords.Enabled := True;
  cbPacketRecords.Checked := True;
  cbCursorPreserved.Checked := True;
  VendorLib.Enabled := False;
  fQuoteChar := TSQLConnectionPro(SQLConnection1).QuoteChar;

  cbAutoClone.Checked := True;
  cbAutoClone.Enabled := SQLConnection1.MaxStmtsPerConn > 0;

  lbStatementsPerConnection.Caption := 'Statements Per Connection : ['+
    IntToStr(SQLConnection1.MaxStmtsPerConn)+']';

  Timer1.fStartTime := Now;
  Timer1Timer(nil);
  Timer1.Enabled := True;

  InitTreeView;
//  exit; //???

  with SQLConnection1 do begin
    STransIsolationKey := Format(TRANSISOLATION_KEY, [DriverName]);
    if Params.Values[STransIsolationKey] <> '' then
    begin
      if LowerCase(Params.Values[STransIsolationKey]) = SRepeatRead then
        TD.IsolationLevel := xilRepeatableRead
      else if LowerCase(Params.Values[STransIsolationKey]) = SDirtyRead then
        TD.IsolationLevel := xilDirtyRead
      else
        TD.IsolationLevel := xilReadCommitted;
    end
    else
      TD.IsolationLevel := xilReadCommitted;
  end;

  fSupportsSqlPrimaryKeys := True;

  fSqlConOdbcIntf := nil;
  Status := SQLConnection1.SQLConnection.getOption(TSQLConnectionOption(xeConnNativeHandle), Pointer(@fSqlConOdbcIntf), SizeOf(Pointer), iLen);
  if (Status <> 0) or (fSqlConOdbcIntf = nil){or (iLen <> SizeOf(Pointer))} then
  begin
    ShowMessage('You use old version of "dbxoodbc.dll"');
    // OLD "dbxoodbc.dll":
    fSqlConOdbcIntf := nil;
    {$IFDEF _D10UP_}
    TISQLConnection25H(SQLConnection1.SqlConnection).SqlConnection.QueryInterface(ISqlConnectionOdbc, fSqlConOdbcIntf);
    {$ELSE}
    SQLConnection1.SqlConnection.QueryInterface(ISqlConnectionOdbc, fSqlConOdbcIntf);
    {$ENDIF}
  end;

  if fSqlConOdbcIntf=nil then
    ShowMessage('??? "dbxoodbc.dll"');

  if fSqlConOdbcIntf<>nil then
  begin
    Caption := {$IFDEF _DEBUG_}'DEBUG: '+{$ENDIF}cCaptionDef + ' "' +fSqlConOdbcIntf.GetDbmsName + '", version: '+
    Format('%d.%d.%d.%d. DirectOdbc=%s. OdcLevel=%d. TransactSup=%s', [
      fSqlConOdbcIntf.GetDbmsVersionMajor,
      fSqlConOdbcIntf.GetDbmsVersionMinor,
      fSqlConOdbcIntf.GetDbmsVersionRelease,
      fSqlConOdbcIntf.GetDbmsVersionBuild,
      cBool[not fSqlConOdbcIntf.GetIsSystemODBCManager()],
      fSqlConOdbcIntf.GetOdbcDriverLevel(),
      cBool[SQLConnection1.TransactionsSupported]
      ]);

    {$ifdef _SynEdit_}
    case fSqlConOdbcIntf.GetDbmsType of
      //eDbmsTypeGupta: SynSQLSyn.SQLDialect := sqlIngres;
      eDbmsTypeMsSqlServer: SynSQLSyn.SQLDialect := sqlMSSQL2K;
      //eDbmsTypeIbmDb2: ;
      eDbmsTypeMySql, eDbmsTypeMySqlMax: SynSQLSyn.SQLDialect := sqlMySQL;
      eDbmsTypeOracle: SynSQLSyn.SQLDialect := sqlOracle;
      eDbmsTypeInterbase: SynSQLSyn.SQLDialect := sqlInterbase6;
      //eDbmsTypeInformix:
      eDbmsTypeSybase: SynSQLSyn.SQLDialect := sqlSybase;
      //eDbmsTypeSapDb:
      else
        SynSQLSyn.SQLDialect := sqlStandard;
    end;
    {$endif ifdef _SynEdit_}

    if fSqlConOdbcIntf.GetDbmsType in [ eDbmsTypeUnspecified, eDbmsTypeExcel, eDbmsTypeText,
      eDbmsTypeSQLite, eDbmsTypeInterSystemCache ]
    then
      cbPackBlobsToString.Checked := True
    else
      cbPackBlobsToString.Checked := False;

    // + Added support for 'Open Firebird, Interbase6 ODBC Driver': http://www.xtgsystems.com/
    //  But only read only mode: for this driver function SQLRowCount always returns -1.
    //  This driver returns bad columns Size(fColSize) for BCD field.

    if (fSqlConOdbcIntf.GetOdbcDriverType = eOdbcDriverTypeInterbase) and
       ( StrLComp(PChar(UpperCase(fSqlConOdbcIntf.GetOdbcDriverName)),
         'IB6XTG', 6 ) = 0 )
    then
      cbUpdIgnoreError.Checked := True;

    if not fSqlConOdbcIntf.GetCursorPreserved then
    begin
      //cbPacketRecords.Checked := False;
      //cbPacketRecords.Enabled := False;
      //edPacketRecords.Enabled := False;
      cbCursorPreserved.Checked := False;
    end;

    fSupportsSqlPrimaryKeys := fSqlConOdbcIntf.GetSupportsSqlPrimaryKeys();

    lbStatementsPerConnection.Caption := 'Odbc Statements Per Connection : ['+
      IntToStr(fSqlConOdbcIntf.GetStatementsPerConnection() )+']';

    // Decimal Separator:
    edDecimalSeparator.OnChange := nil;
    edDecimalSeparator.Text := fSqlConOdbcIntf.GetDecimalSeparator;
    edDecimalSeparator.OnChange := edDecimalSeparatorChange;

  end
  else
  begin
    // Decimal Separator:
    iLen := 2;
    SetLength(STransIsolationKey, iLen);
    Status := SQLConnection1.SQLConnection.getOption(TSQLConnectionOption(xeConnDecimalSeparator),
      PChar(STransIsolationKey), iLen, iLen);
    if (Status = 0) and (iLen <> 0) then
    begin
      edDecimalSeparator.OnChange := nil;
      edDecimalSeparator.Text := STransIsolationKey[1];
      edDecimalSeparator.OnChange := edDecimalSeparatorChange;
    end;
  end;//of: if fSqlConOdbcIntf<>nil

end;

procedure TFormOdbcExplor.SQLConnection1AfterDisconnect(Sender: TObject);
var
  AllowChange: Boolean;
begin
  //Application.ProcessMessages();
  Caption := {$IFDEF _DEBUG_}'DEBUG: '+{$ENDIF}cCaptionDef;
  pbConnect.Caption := 'C&onnect';
//  exit; //???
  lbStatementsPerConnection.Caption := 'Statements Per Connection : [?]';
  chkConnected.Checked := SQLConnection1.Connected;
  pbApplyUpdates.Enabled := False;
  btnBeginTransaction.Enabled := False;
  btnCommit.Enabled := False;
  btnRollack.Enabled := False;
  Timer1.Enabled := False;
  cbInternalCloneConnection.Enabled := not cbDefConOpt.Checked;
  lbConnectedTime.Caption := 'Connected Time: 00:00:00';
  VendorLib.Enabled := True;
  cbAutoClone.Checked := True;
  cbAutoClone.Enabled := False;
  {$ifdef _SynEdit_}
  //SynSQLSyn.SQLDialect := sqlStandard;
  {$endif ifdef _SynEdit_}
  SaveConfigurations;
  TreeView1.FullCollapse;
  TreeView1.Items.Clear;

  if Notebook1.PageIndex = 0 then
  begin
    TabSet1Change(TabSet1, 0, AllowChange);
  end;
  ac_Main.State := asSuspended;
  TabSet1.Visible := False;
end;

procedure TFormOdbcExplor.InsertMemoText(sText: string);
var
  NewSelStart: integer;
begin
  CheckSateMemoSql;
  CDSQuery.Fields[0].AsDateTime := Now;
  NewSelStart := memoSql.SelStart+ Length(sText);
  memoSql.Text :=
   Copy(memoSql.Text, 1, memoSql.SelStart) +
   sText +
   Copy(memoSql.Text,
     memoSql.SelStart + memoSql.SelLength + 1,
     Length(memoSql.Text) - memoSql.SelStart - memoSql.SelLength);
   memoSql.SelStart := NewSelStart;
end;

procedure TFormOdbcExplor.LinkEditors(bActivate: Boolean; dwSkipEditor: DWORD = 0 );
var
  i: integer;
  aField: TField;
  MaxLen, SaveIdx: Integer;
begin
  if not bActivate then
  begin
    LinkMemoToField('');
    LinkImageToField('');
    {$ifdef _HexEditor_}
    LinkBinaryToField('');
    {$endif ifdef _HexEditor_}
  end;
//1) TDBMemo

  if (dwSkipEditor and c_skip_memo) = 0 then
  begin
    // link memo to TDBMemo:
    if (dbMemo1.DataField='') then
    for i := 0 to CDS.Fields.Count - 1 do
    begin
      aField := CDS.Fields[i];
      if (aField.DataType in [ftMemo, ftFmtMemo, ftOraClob]) and
         LinkMemoToField(aField.FieldName)
      then
         break;
    end;
  end;

    // link string to TDBMemo:
      // search string field witch max length
    MaxLen := -1;
    SaveIdx := -1;
    for i := 0 to CDS.Fields.Count - 1 do
    begin
      aField := CDS.Fields[i];
      if (aField.DataType in [ftString, ftWideString] ) then
      begin
        if aField.Size > MaxLen then
        begin
          MaxLen := aField.Size;
          SaveIdx := i;
        end;
      end;
    end;

  if (dwSkipEditor and c_skip_memo) = 0 then
  begin
    if (SaveIdx>=0) and (dbMemo1.DataField='') then
      LinkMemoToField(CDS.Fields[SaveIdx].FieldName);
  end;

  {$ifdef _HexEditor_}
  if (dwSkipEditor and c_skip_hexeditor) = 0 then
  begin
      // search next string field witch max length
    if (SaveIdx>=0) and (dbMemo1.DataField<>'') then
    begin
      MaxLen := -1;
      SaveIdx := -1;
      for i := 0 to CDS.Fields.Count - 1 do
      begin
        aField := CDS.Fields[i];
        if (aField.DataType in [ftString, ftWideString] ) and
           (dbMemo1.DataField<>aField.FieldName)
        then
        begin
          if aField.Size > MaxLen then
          begin
            MaxLen := aField.Size;
            SaveIdx := i;
          end;
        end;
      end;
    end;
  end;
  {$endif ifdef _HexEditor_}

//2) TDBImage

  if (dwSkipEditor and c_skip_blob) = 0 then
  begin
    // link image to TDBImage:
    if (dbImage1.DataField='') then
    for i := 0 to CDS.Fields.Count - 1 do
    begin
      aField := CDS.Fields[i];
      if (aField.DataType = ftGraphic ) then
      begin
        if LinkImageToField(aField.FieldName) then
          break;
      end;
    end;
    if (dbImage1.DataField='') then
    for i := 0 to CDS.Fields.Count - 1 do
    begin
      aField := CDS.Fields[i];
      if (aField.DataType in [ftBlob, ftOraBlob] ) then
      begin
        if LinkImageToField(aField.FieldName) then
          break;
      end;
    end;
    if (dbImage1.DataField='') then
    for i := 0 to CDS.Fields.Count - 1 do
    begin
      aField := CDS.Fields[i];
      if (aField.DataType in [ftBytes, ftVarBytes, ftTypedBinary] ) then
      begin
        if LinkImageToField(aField.FieldName) then
          break;
      end;
    end;
  end;

  {$ifdef _HexEditor_}

//3) THexEditor

  if (dwSkipEditor and c_skip_hexeditor) = 0 then
  begin
    // link binary field
    if fBinaryField = '' then
    begin
      for i := 0 to CDS.Fields.Count - 1 do
      begin
        aField := CDS.Fields[i];
        if (aField.DataType in [ftBlob, ftGraphic, ftOraBlob] ) and
           (dbImage1.DataField<>aField.FieldName) and
           LinkBinaryToField(aField.FieldName)
        then
          break;
      end;
    end;
    // link binary field
    if fBinaryField = '' then
    begin
      for i := 0 to CDS.Fields.Count - 1 do
      begin
        aField := CDS.Fields[i];
        if (aField.DataType in [ftBytes, ftVarBytes, ftTypedBinary] ) and
           LinkBinaryToField(aField.FieldName)
        then
          break;
      end;
    end;
    // link memo field
    if fBinaryField = '' then
    begin
      for i := 0 to CDS.Fields.Count - 1 do
      begin
        aField := CDS.Fields[i];
        if (aField.DataType in [ftMemo, ftFmtMemo, ftOraClob] ) and
           (dbMemo1.DataField<>aField.FieldName) and
           LinkBinaryToField(aField.FieldName)
        then
          break;
      end;
    end;
    // link string field witch having max length
    if fBinaryField = '' then
    begin
      if (SaveIdx>=0) then
        LinkBinaryToField(CDS.Fields[SaveIdx].FieldName);
    end;
  end;
  {$endif ifdef _HexEditor_}

  FillLinkCombos(True);
end;

procedure TFormOdbcExplor.FillLinkCombos(bActivate: Boolean);
var
  i: integer;
  aField: TField;
begin
  cmMemo.Clear;
  cmImage.Clear;
  cmBinary.Clear;
  if not bActivate then
    exit;
  cmMemo.AddItem('', nil);
  cmImage.AddItem('', nil);
  cmBinary.AddItem('', nil);
  for i := 0 to CDS.Fields.Count - 1 do
  begin
    aField := CDS.Fields[i];
    // memo:
    if (aField.DataType in [ftMemo, ftFmtMemo, ftOraClob, ftString, ftWideString]) then
      cmMemo.AddItem(aField.FieldName, nil);
    // blob:
    if (aField.DataType in [ftBytes, ftVarBytes, ftBlob, ftGraphic, ftTypedBinary, ftOraBlob] ) then
      cmImage.AddItem(aField.FieldName, nil);
    if //cbPackBlobsToString.Checked and
      (aField.DataType in [ftMemo, ftFmtMemo, ftOraClob, ftString, ftWideString])
    then
      cmImage.AddItem(aField.FieldName, nil);
    // binary
    if (aField.DataType in [
         ftMemo, ftFmtMemo, ftOraClob, ftString, ftWideString,
         ftBlob, ftGraphic, ftOraBlob,
         ftBytes, ftVarBytes, ftTypedBinary
        ])
    then
      cmBinary.AddItem(aField.FieldName, nil);
  end;
  cmMemo.ItemIndex := cmMemo.Items.IndexOf(dbMemo1.DataField);
  cmImage.ItemIndex := cmImage.Items.IndexOf(dbImage1.DataField);
  {$ifdef _HexEditor_}
  cmBinary.ItemIndex := cmBinary.Items.IndexOf(fBinaryField);
  {$endif ifdef _HexEditor_}
end;

procedure TFormOdbcExplor.pbOpenClick(Sender: TObject);
begin
  if pbOpen.Caption= 'Close' then
  begin
    TickCountInfo := GetTickCount;
    CDS.Close;
    TickCountInfo := GetTickCount - TickCountInfo;
    SB.Panels[0].Text := Format(cTickCountInfo, [TickCountInfo]);
    exit;
  end;
  CDS.Close;
  dbMemo1.DataField := '';
  dbImage1.DataField := '';
  SQLDataset1.Close;
  SQLDataset1.CommandText := '';
  SQLDataset1.ParamCheck := Sender<>nil;
  SQLDataset1.CommandText := memoSQL.Text;

  {$IFDEF _D7UP_}
  SQLDataset1.GetMetadata := cbGetMetadata.Checked;
  {$ELSE}
  SQLDataset1.NoMetadata := not cbGetMetadata.Checked;
  {$ENDIF}

  TickCountInfo := GetTickCount;
  CDS.Open;
  TickCountInfo := GetTickCount - TickCountInfo;
  SB.Panels[0].Text := Format(cTickCountInfo, [TickCountInfo]);

  if not ( CDSQuery.State in [dsInsert, dsEdit] ) then
  begin
    CDSQuery.Edit;
    CDSQuery.Fields[0].AsDateTime := Now;
    CDSQuery.Post
  end
  else
    CDSQuery.Fields[0].AsDateTime := Now;
end;

procedure TFormOdbcExplor.pbApplyUpdatesClick(Sender: TObject);
begin
  if rbUpdateWhereKey.Checked then
    DataSetProvider1.UpdateMode := upWhereKeyOnly
  else if rbUpdateWhereAll.Checked then
    DataSetProvider1.UpdateMode := upWhereAll
  else if rbUpdateWhereChanged.Checked then
    DataSetProvider1.UpdateMode := upWhereChanged;

  CDS.fUpdatesErrorCount := 0;
  CDS.fUpdatesErrorRecNo := 0;
  TickCountInfo := GetTickCount;
  CDS.ApplyUpdates(0);
  TickCountInfo := GetTickCount - TickCountInfo;
  SB.Panels[0].Text := Format(cTickCountInfo, [TickCountInfo]);
end;

{$IFNDEF _D7UP_}

function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

{$ENDIF}

procedure TFormOdbcExplor.pbExecuteClick(Sender: TObject);
var
  sScript: string;
  sQuery: string;
  sDelim: string;
  iPosPrev: integer;
  procedure ExtractNextQuery;
  var
    iPosNew: Integer;
  begin
    iPosNew := PosEx(sDelim, sScript, iPosPrev);
    if iPosNew > 0 then
    begin
      sQuery := Trim(Copy(sScript, iPosPrev, iPosNew - iPosPrev));
      iPosPrev := iPosNew + Length(sDelim);
    end
    else
    begin
      iPosPrev := Length(sScript) + 1;
      sQuery := '';
    end;
  end;

begin
//  CreateMemoSql();
//  exit;

  TickCountInfo := GetTickCount;

  //OLD:
  //SqlConnection1.ExecuteDirect(memoSQL.Text);
  //OLD.

  //NEW:
  sDelim := edScriptLD.Text;
  if sDelim <> '' then
  begin
    sDelim := #13#10 + edScriptLD.Text + #13#10;
    sScript := #13#10 + memoSQL.Text + sDelim;
    iPosPrev := 1;
    repeat
      ExtractNextQuery();
      if (sQuery <> '') and cbRemoveScriptComments.Checked then
        RemoveComments(sQuery, ''{edScriptLD.Text} );
      if sQuery <> '' then
        SqlConnection1.ExecuteDirect(sQuery);
    until iPosPrev > Length(sScript);
  end
  else
  begin
    sQuery := memoSQL.Text;
    if (sQuery <> '') and cbRemoveScriptComments.Checked then
      RemoveComments(sQuery, ''{edScriptLD.Text} );
    if sQuery <> '' then
      SqlConnection1.ExecuteDirect(sQuery);
  end;
  //NEW.

  TickCountInfo := GetTickCount - TickCountInfo;
  SB.Panels[0].Text := Format(cTickCountInfo, [TickCountInfo]);

  if not ( CDSQuery.State in [dsInsert,dsEdit] ) then
  begin
    CDSQuery.Edit;
    CDSQuery.Fields[0].AsDateTime := Now;
    CDSQuery.Post
  end
  else
    CDSQuery.Fields[0].AsDateTime := Now;
end;

function AssignNodeToImageIndex(Node: TTreeNode; ImageIndex: Integer): TTreeNode;
begin
  Result := Node;
  if Node<>nil then
  begin
    Node.ImageIndex := ImageIndex;
    Node.SelectedIndex := ImageIndex;
  end;
end;

procedure TFormOdbcExplor.WMInitTreeView(var Message:TMessage);
begin
  DoInitTreeView();
end;

procedure  TFormOdbcExplor.DoInitTreeView();
var
  NewNode: TTreeNode;
//  TreeViewNew: TTreeView;
begin
//  exit; //???
  {
  TreeViewNew := TreeView1;
  RemoveComponent(TreeView1);
  FreeAndNil(TreeViewNew);

  // clone TreeView1
  TreeViewNew := TTreeView.Create(Self);
  TreeViewNew.Parent := pTree;

  TreeViewNew.Align := alClient;
  TreeViewNew.BevelOuter := bvRaised;
  TreeViewNew.DragMode := dmAutomatic;
  TreeViewNew.Font.Charset := DEFAULT_CHARSET;
  TreeViewNew.Font.Color := clWindowText;
  TreeViewNew.Font.Height := -11;
  TreeViewNew.Font.Name := 'MS Sans Serif';
  TreeViewNew.Font.Style := [];
  TreeViewNew.HideSelection := False;
  TreeViewNew.HotTrack := True;
  TreeViewNew.Images := ImageList;
  TreeViewNew.Indent := 19;
  TreeViewNew.MultiSelectStyle := [msControlSelect, msShiftSelect, msSiblingOnly];
  TreeViewNew.PopupMenu := PopupMenuTree;
  TreeViewNew.ReadOnly := True;
  TreeViewNew.RightClickSelect := True;
  TreeViewNew.OnDblClick := TreeView1DblClick;
  TreeViewNew.OnExpanding := TreeView1Expanding;
  TreeViewNew.OnKeyDown := TreeView1KeyDown;
  TreeViewNew.OnMouseDown := TreeView1MouseDown;

  TreeView1 := TreeViewNew;
  {}

  //TreeView1.Items.Clear;
  //TreeView1.Items.BeginUpdate;
  //Canvas.Lock;
  try
    TreeView1.Visible := False;
    TreeView1.FullCollapse;
    TreeView1.Items.Clear;
    TWinControlPro(TreeView1).RecreateWnd();

    // todo: bug: For mssql we receive a mistake after repeated connection
    NewNode := AssignNodeToImageIndex(TreeView1.Items.Add(nil, 'All Tables / Views'), cImageIndexAllTables);
    TreeView1.Items.AddChild(NewNode, '');
    NewNode := AssignNodeToImageIndex(TreeView1.Items.Add(nil, 'Tables'), cImageIndexTables);
    TreeView1.Items.AddChild(NewNode, '');
    NewNode := AssignNodeToImageIndex(TreeView1.Items.Add(nil, 'Views'), cImageIndexViews);
    TreeView1.Items.AddChild(NewNode, '');
    NewNode := AssignNodeToImageIndex(TreeView1.Items.Add(nil, 'System Tables'), cImageIndexSystemTables);
    NewNode.Expanded := false;
    TreeView1.Items.AddChild(NewNode, '');
    NewNode := AssignNodeToImageIndex(TreeView1.Items.Add(nil, 'Synonymns'), cImageIndexSynonums);
    TreeView1.Items.AddChild(NewNode, '');
    NewNode := AssignNodeToImageIndex(TreeView1.Items.Add(nil, 'Stored Procedures'), cImageIndexStoredProcs);
    TreeView1.Items.AddChild(NewNode, '');
  finally
    TreeView1.Visible := True;
    //Canvas.UnLock;
    Invalidate();
    //TreeView1.Items.EndUpdate;
  end;
end;

procedure  TFormOdbcExplor.InitTreeView();
begin
  //Perform(WM_USER_INIT_TREE_VIEW, 0, 0);
  DoInitTreeView();
end;

procedure TFormOdbcExplor.PopMenuImageCutClick(Sender: TObject);
begin
  DbImage1.CutToClipboard;
end;

procedure TFormOdbcExplor.PopMenuImageCopyClick(Sender: TObject);
begin
  DbImage1.CopyToClipboard;
end;

procedure TFormOdbcExplor.PopMenuImagePasteClick(Sender: TObject);
begin
  DbImage1.PasteFromClipboard;
end;

procedure TFormOdbcExplor.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  AllowExpansion := False;
  if not SQLConnection1.Connected then
    SQLConnection1.Open;
  if not SQLConnection1.Connected then
    exit;

  if Node.Deleting then
    exit;
  try

  if Node.Level = 0 then
  begin
    case Node.Index of
      0: PopulateTableNode(Node, 0, cImageIndexAllTable);
      1: PopulateTableNode(Node, eSQLTable, cImageIndexTable);
      2: PopulateTableNode(Node, eSQLView, cImageIndexView);
      3: PopulateTableNode(Node, eSQLSystemTable, cImageIndexSystemTable);
      4: PopulateTableNode(Node, eSQLSynonym, cImageIndexSynonum);
      5: PopulateStoredProcedureNode(Node, cImageIndexStoredProc);
      end;
  end
  else if Node.Level = 1 then
  begin
    if Node.Parent.Index = 5 then
      PopulateStoredProcedureParamNode(Node)
  else
      PopulateColumnNode(Node)
  end
  else if Node.Level = 2 then
  begin
    if node.Text = '[indexes]' then
      PopulateIndexNode(Node)
  end;
  AllowExpansion := True;

  except
    AllowExpansion := False;
    //Node.Collapse(False);
  end;
end;

procedure TFormOdbcExplor.PopulateTableNode(aNode: TTreeNode; SearchTableType: LongWord; ImageIndex: Integer);
var
  MetaData: TISQLMetaData;
  Cursor: TISQLCursor;
  aResult: SQLResult;
  Len: LongWord;
  aSchemaName: String;
  aTableName: string;
  sTableName: string;
  IsBlank: LongBool;
  aTableNode: TTreeNode;
  vCursor: TCursor;
begin
  vCursor := Screen.Cursor;
  TreeView1.Items.BeginUpdate;
  Cursor := nil;
  try
    Screen.Cursor := crSQLWait;

    aNode.DeleteChildren;

    MetaData := SQLConnection1.Metadata;

    aResult := MetaData.GetTables(nil, Cardinal(SearchTableType), Cursor);

    if (aResult <> DBXERR_NONE) then exit;

    Cursor.getColumnLength(3,Len);
    SetLength(aSchemaName,Len+1);

    Cursor.getColumnLength(4,Len);
    SetLength(aTableName,Len+1);

    aResult := Cursor.next;
    while (aResult = DBXERR_NONE) do
    begin
      aSchemaName[1] := #0;
      Cursor.getString(3, PAnsiChar(aSchemaName), IsBlank);

      aTableName[1] := #0;
      Cursor.getString(4, PAnsiChar(aTableName), IsBlank);

      sTableName := StrPas(PAnsiChar(aTableName));
      if aSchemaName[1] <> #0 then
        sTableName := StrPas(PAnsiChar(aSchemaName)) + '.' + sTableName;

      aTableNode := AssignNodeToImageIndex(
        TTreeView(aNode.TreeView).Items.AddChild(aNode, sTableName), ImageIndex);
      TTreeView(aNode.TreeView).Items.AddChild(aTableNode, '');
      aResult := Cursor.next;
    end;

  finally
    FreeXSQLCursor(Cursor);
    TreeView1.Items.EndUpdate;
    Screen.Cursor := vCursor;
  end;
end;

procedure TFormOdbcExplor.PopulateColumnNode(TableNode: TTreeNode);
var
  aResult: SQLResult;
  MetaData: TISqlMetaData;
  Cursor: TISQLCursor;
  Len: LongWord;
  aDataType: smallint;
  aColumnName, aColumnType: string;
  aDataPrecision: integer;
  aDataScale: smallint;
  aDataNullable: smallint;
  IsBlank: LongBool;
  aColumnNode: TTreeNode;
  aString: string;
  qry: TSQLQuery;
  i: Integer;
  pColumns: Word;
  vCursor: TCursor;
  sTableNodeText: XString;
const
{
  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, // 5..11
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, // 19..24
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
    ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval); // 38..41
}
  {cFieldTypeNemes: array[TFieldType] of String = (
    'ftUnknown', 'ftString', 'ftSmallint', 'ftInteger', 'ftWord',
    'ftBoolean', 'ftFloat', 'ftCurrency', 'ftBCD', 'ftDate', 'ftTime', 'ftDateTime',
    'ftBytes', 'ftVarBytes', 'ftAutoInc', 'ftBlob', 'ftMemo', 'ftGraphic', 'ftFmtMemo',
    'ftParadoxOle', 'ftDBaseOle', 'ftTypedBinary', 'ftCursor', 'ftFixedChar', 'ftWideString',
    'ftLargeint', 'ftADT', 'ftArray', 'ftReference', 'ftDataSet', 'ftOraBlob', 'ftOraClob',
    'ftVariant', 'ftInterface', 'ftIDispatch', 'ftGuid', 'ftTimeStamp', 'ftFMTBcd'
    // Delphi 2006
    ,'ftFixedWideChar', 'ftWideMemo', 'ftOraTimeStamp', 'ftOraInterval'
  );{}
  cDBFieldTypeNemes: array[TFieldType] of String = (
    //ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
    'Unknown', 'VARCHAR', 'SMALLINT', 'INTEGER', 'WORD',
    //ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, // 5..11
    'BOOLEAN', 'FLOAT', 'MONEY', 'DECIMAL', 'DATE', 'TIME', 'DATETIME',
    //ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
    'BYTES', 'VARBYTES', 'AUTOINC', 'BLOB', 'TEXT', 'IMAGE', 'TEXT'{CHAR},
    //ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, // 19..24
    'OLE', 'OLE', 'BLOB', 'CURSOR', 'CHAR', 'NVARCHAR',
    //ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
    'DECIMAL(65)'{INT64}, 'ADT', 'ARRAY', 'REFERENCE', 'TABLE', 'BLOB', 'CLOB',
    //ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
    'VARIANT', 'INTERFACE', 'DISPATCH', 'GUID', 'TIMESTAMP', 'DECIMAL'
    {$IFDEF _D10UP_}
    //ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval); // 38..41
    ,'NCHAR', 'NTEXT', 'TIMESTAMP', 'INTERVAL'
    {$ENDIF}
  );

begin

  MetaData := SQLConnection1.MetaData;

  vCursor := Screen.Cursor;
  TreeView1.Items.BeginUpdate;
  Cursor := nil;
  try
    Screen.Cursor := crSQLWait;
    TableNode.DeleteChildren;
    try

      pColumns := 0;
      Cursor := nil;
      sTableNodeText := TableNode.Text;
      aResult := MetaData.GetColumns(XPChar(sTableNodeText), nil, 0, Cursor);

      if (aResult = DBXERR_NONE) and (Cursor<>nil )then
        aResult := Cursor.getColumnCount(pColumns);

      if (aResult <> DBXERR_NONE) or (pColumns <= 0) then
      begin
        (*
          when odbc unsupported or contain bugs in get metadata informations
           - Open Firebird, Interbase6 ODBC Driver: http://www.xtgsystems.com/
           - IBPhoenix ODBC Driver for Firebird: http://www.ibphoenix.com/
        *)
        qry := TSQLQuery.Create(nil);
        try
          qry.SQLConnection := SQLConnection1;
          qry.SQL.Text := 'SELECT * FROM ' + Quote(TableNode.Text, True) + ' WHERE 1=0';
          try
            qry.Open;
            for i:=0 to qry.FieldDefs.Count-1 do
            with qry.FieldDefs[i] do
            begin
              if DataType <> ftString then
                aString := {qry.FieldDefs[i].}Name + ' ' + cDBFieldTypeNemes[DataType];
              case DataType of
                ftString:
                  begin
                    if TStringField(qry.Fields[i]).FixedChar then
                      aString := {qry.FieldDefs[i].}Name + ' CHAR'
                    else
                      aString := {qry.FieldDefs[i].}Name + ' VARCHAR';
                    aString := aString + '(' + IntToStr(Size) + ')';
                  end;
                ftBCD, ftFmtBcd:
                  begin
                    aString := aString + '(' + IntToStr(Precision) + ', ' + IntToStr(Size) + ')';
                  end;
                ftBytes, ftVarBytes:
                  begin
                    aString := aString + '(' + IntToStr(Size) + ')';
                  end;
              end;
              if Required then
                aString := aString + ' NOT NULL'
              else
                aString := aString + ' NULL';
              aColumnNode := AssignNodeToImageIndex(
                TTreeView(TableNode.TreeView).Items.AddChild(TableNode, aString), cImageIndexField);
              aColumnNode.Data := pointer(Length(Name));
            end;
          except
            exit;
          end;
        finally
          qry.Free;
        end;
        exit;
      end;
      Cursor.getColumnLength(5,Len);
      SetLength(aColumnName,Len+1);
      Cursor.getColumnLength(9,Len);
      SetLength(aColumnType,Len+1);
      aResult := Cursor.next;
      while (aResult = DBXERR_NONE) do
      begin
        aColumnName[1] := #0;
        Cursor.getString(5, PAnsiChar(aColumnName), IsBlank);
        if IsBlank or (aColumnName[1]=#0) then
        begin
          aResult := Cursor.next;
          continue;
        end;
        Cursor.getShort(8, @aDataType, IsBlank);
        aColumnType[1] := #0;
        Cursor.getString(9, PAnsiChar(aColumnType), IsBlank);
        aString := StrPas(PAnsiChar(aColumnName)) + ' ' + StrPas(PAnsiChar(aColumnType));
        case aDataType of
          fldZSTRING:
            begin
              Cursor.getLong(11, @aDataPrecision, IsBlank);
              aString := aString + '(' + IntToStr(aDataPrecision) + ')';
            end;
          fldBCD:
            begin
              Cursor.getLong(11, @aDataPrecision, IsBlank);
              Cursor.getShort(12, @aDataScale, IsBlank);
              aString := aString + '(' + IntToStr(aDataPrecision) + ', ' + IntToStr(aDataScale) + ')';
            end;
          fldBYTES, fldVARBYTES:
            begin
              Cursor.getLong(11, @aDataPrecision, IsBlank);
              aString := aString + '(' + IntToStr(aDataPrecision) + ')';
            end;
        end;
        Cursor.getShort(14, @aDataNullable, IsBlank);
        if aDataNullable = 0 then
          aString := aString + ' NOT NULL'
        else
          aString := aString + ' NULL';
        aColumnNode := AssignNodeToImageIndex(
          TTreeView(TableNode.TreeView).Items.AddChild(TableNode, aString), cImageIndexField);
        aColumnNode.Data := Pointer(StrLen(PAnsiChar(aColumnName)));
        aResult := Cursor.next;
      end;
    finally
      aColumnNode := AssignNodeToImageIndex(
        TTreeView(TableNode.TreeView).Items.AddChild(TableNode, '[indexes]'), cImageIndexIndexes);
      TTreeView(TableNode.TreeView).Items.AddChild(aColumnNode, '');

    end;

  finally
    FreeXSQLCursor(Cursor);
    TreeView1.Items.EndUpdate;
    Screen.Cursor := vCursor;
  end;
end;

{$hints off}

  {
  1.  RECNO           fldINT32
        A record number that uniquely identifies each record.
  2.  CATALOG_NAME    fldZSTRING
        The name of the catalog (database) that contains the index.
  3.  SCHEMA_NAME     fldZSTRING
        The name of the schema that identifies the owner of the index.
  4.  TABLE_NAME      fldZSTRING
        The name of the table for which the index is defined.
  5.  INDEX_NAME      fldZSTRING
        The name of the index.
  6.  PKEY_NAME       fldZSTRING  // getString
        The name of the primary key.
  7.  COLUMN_NAME     fldZSTRING
        The name of the column (field) in the index.
  8.  COLUMN_POSITION fldINT16   // getShort
        The position of this field in the index.
  9.  INDEX_TYPE      fldINT16 // getShort
        An eSQLIndexType value (C++) or index type constant (Object Pascal) that
        indicates any special properties of the index.
  10. SORT_ORDER      fldZSTRING
        Indicates whether the index sorts on this field in
        ascending (a) or descending (d) order.
  11. FILTER          fldZSTRING
        A string that gives a filter condition limiting indexed records.
}
//fIndexType := eSQLPrimaryKey + eSQLUnique
//              aMetaIndexColumn.fSortOrder := 'A';
{
IdxType := DataSet.FieldByName(IDX_TYPE_FIELD).Value;
if (IdxType and eSQLPrimaryKey) = eSQLPrimaryKey then
  Options := Options + [ixPrimary];
if (IdxType and eSQLUnique) = eSQLUnique then
  Options := Options + [ixUnique];
SortOrder := DataSet.FieldByName(IDX_SORT_FIELD).Value;
{}
procedure TFormOdbcExplor.PopulateIndexNode(IndexNode: TTreeNode);
var
  aResult: SQLResult;
  MetaData: TISqlMetaData;
  Cursor: TISQLCursor;
  Len: LongWord;
  aIndexName, aIndexColumnName: string;
  aIndexNode: TTreeNode;
  aIndexColumnNode: TTreeNode;
  IsBlank: LongBool;
  aIndexType: Smallint;// eSQLPrimaryKey, eSQLUnique
  aSortOrder: array[0..1]of char;
  ImageIndex: Integer;
  vCursor: TCursor;
  sNodeText: XString;
begin
  MetaData := SQLConnection1.MetaData;

  vCursor := Screen.Cursor;
  TreeView1.Items.BeginUpdate;
  Cursor := nil;
  try
    Screen.Cursor := crSQLWait;
    IndexNode.DeleteChildren;
    sNodeText := IndexNode.Parent.Text;
    aResult := MetaData.getIndices(XPChar(sNodeText), 0, Cursor);
    if (aResult <> DBXERR_NONE) then
      exit;
    Cursor.getColumnLength(5, Len);
    SetLength(aIndexName,Len+1);
    Cursor.getColumnLength(7, Len);
    SetLength(aIndexColumnName,Len+1);
    aResult := Cursor.next;
    aIndexNode := IndexNode;
    while (aResult = DBXERR_NONE) do
    begin
      Cursor.getString(5, PAnsiChar(aIndexName), IsBlank);
      Cursor.getString(7, PAnsiChar(aIndexColumnName), IsBlank);
      aSortOrder[0] := 'A';
        Cursor.getString(10, @aSortOrder, IsBlank);
      if (aIndexName <> aIndexNode.Text) then
      begin
        // Read Index Type and calculate Image Index
        aIndexType := 0;
        Cursor.getShort(9, @aIndexType, IsBlank);
        if (aIndexType and eSQLPrimaryKey) = eSQLPrimaryKey then
          ImageIndex := cImageIndexIndexPK
        else
        begin
          if (aIndexType and eSQLUnique) = eSQLUnique then
            ImageIndex := cImageIndexIndexAU
          else
            ImageIndex := cImageIndexIndexA;
          if aSortOrder[0]<>'A' then
            inc(ImageIndex);
        end;
        // add index node
        aIndexNode := AssignNodeToImageIndex(
          TTreeView(IndexNode.TreeView).Items.AddChild(IndexNode, StrPas(PAnsiChar(aIndexName))), ImageIndex);
      end;
      // add column to index
      case aSortOrder[0] of
        'A':
          ImageIndex := cImageIndexIndexColA;
        'D':
          ImageIndex := cImageIndexIndexColD;
        else
          ImageIndex := cImageIndexIndexCol; // bug in odbc driver
      end;
      aIndexColumnNode := AssignNodeToImageIndex(
        TTreeView(IndexNode.TreeView).Items.AddChild(aIndexNode, StrPas(PAnsiChar(aIndexColumnName))), ImageIndex);
      aResult := Cursor.next;
    end;
  finally
    FreeXSQLCursor(Cursor);
    TreeView1.Items.EndUpdate;
    Screen.Cursor := vCursor;
  end;
end;
{$hints on}

procedure TFormOdbcExplor.PopulateStoredProcedureNode(Node: TTreeNode; ImageIndex: Integer);
var
  aResult: SQLResult;
  MetaData: TISqlMetaData;
  Cursor: TISQLCursor;
  Len: LongWord;
  aStoredProcName, sStoredProcName: string;
  aStoredProcNode: TTreeNode;
  IsBlank: LongBool;
  vCursor: TCursor;
begin

  MetaData := SQLConnection1.MetaData;

  Cursor := nil;
  vCursor := Screen.Cursor;
  TreeView1.Items.BeginUpdate;
  try
    Screen.Cursor := crSQLWait;
    Node.DeleteChildren;

    aResult := MetaData.getProcedures(nil, 0, Cursor);
    if (aResult <> DBXERR_NONE) then exit;
    Cursor.getColumnLength(4,Len);
    SetLength(aStoredProcName,Len+1);
    aResult := Cursor.next;
    while (aResult = DBXERR_NONE) do
    begin
      Cursor.getString(4, PAnsiChar(aStoredProcName), IsBlank);
      sStoredProcName := StrPas(PAnsiChar(aStoredProcName));
      aStoredProcNode := AssignNodeToImageIndex(
        TTreeView(Node.TreeView).Items.AddChild(Node, sStoredProcName), ImageIndex);
      TTreeView(Node.TreeView).Items.AddChild(aStoredProcNode, sStoredProcName);
      aResult := Cursor.next;
    end;
  finally
    FreeXSQLCursor(Cursor);
    TreeView1.Items.EndUpdate;
    Screen.Cursor := vCursor;
  end;
end;

{$hints off}
procedure TFormOdbcExplor.PopulateStoredProcedureParamNode(Node: TTreeNode);
var
  aResult: SQLResult;
  MetaData: TISqlMetaData;
  Cursor: TISQLCursor;
  Len: LongWord;
  aParamName: string;
  aParamNode: TTreeNode;
  IsBlank: LongBool;
  vCursor: TCursor;
  sNodeText: XString;
begin
  MetaData := SQLConnection1.MetaData;

  vCursor := Screen.Cursor;
  TreeView1.Items.BeginUpdate;
  try
    Screen.Cursor := crSQLWait;
    Node.DeleteChildren;

    sNodeText := Node.Text;
    aResult := MetaData.getProcedureParams(XPChar(sNodeText), nil, Cursor);
    if (aResult <> DBXERR_NONE) then exit;
    Cursor.getColumnLength(5,Len);
    SetLength(aParamName,Len+1);
    aResult := Cursor.next;
    while (aResult = DBXERR_NONE) do
    begin
      aParamName[1] := #0;
      Cursor.getString(5, PAnsiChar(aParamName), IsBlank);
      aParamNode := TTreeView(Node.TreeView).Items.AddChild(Node, StrPas(PAnsiChar(aParamName)));
      aResult := Cursor.next;
    end;
  finally
    TreeView1.Items.EndUpdate;
    Screen.Cursor := vCursor;
  end;
end;
{$hints on}

procedure TFormOdbcExplor.memoSql1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source = TObject(TreeView1) then
    Accept := true
  else
  begin
    Accept := false;
    exit;
  end;
end;

procedure TFormOdbcExplor.TreeView1DblClick(Sender: TObject);
var
  TableName: string;
  ColumnName: string;
begin
  try
    if (TreeView1.Selected = nil) or (TreeView1.Selected.Level = 0)
      or (TreeView1.Selected.ImageIndex = cImageIndexIndexes)
    then
      exit;
    TreeView1.Items.BeginUpdate;
    try
      if TreeView1.Selected.Level = 1 then
      begin

        if (TreeView1.Selected.ImageIndex in [
           cImageIndexAllTable, cImageIndexTable, cImageIndexView, cImageIndexSystemTable,
           cImageIndexSynonum ]) then
        begin
          TableName := TreeView1.Selected.Text;
          if (TabSet1.TabIndex<>1) or  (not (CDSQuery.State in [dsInsert, dsEdit])) then
          begin
            CDS.Close;
            SQLDataSet1.Close;
            SQLDataSet1.CommandText := '';
            SQLDataSet1.CommandText := 'SELECT * FROM '+Quote(TableName, True);
            TickCountInfo := GetTickCount;
            CDS.Open;
            TickCountInfo := GetTickCount - TickCountInfo;
            lbTableName.Caption := TableName;
            SB.Panels[0].Text := Format(cTickCountInfo, [TickCountInfo]);
            TreeView1.Selected.Collapse(False);
            exit;
          end
          else
          begin
            if (memoSql.Text = '') then
            begin
              //CheckSateMemoSql;
              CDSQuery.Fields[0].AsDateTime := Now;
              memoSql.Text := 'SELECT * FROM ' + Quote(TableName, True);
              memoSql.SelStart := Pos('*', memoSql.Text) - 1;
              memoSql.SelLength := 1;
            end
            else
              InsertMemoText(TableName + ' ');
          end;
        end
        else
        begin
          if (TabSet1.TabIndex =1) and  (CDSQuery.State in [dsInsert, dsEdit]) then
          begin
            ColumnName := TreeView1.Selected.Text;
            if TreeView1.Selected.Data <> nil then
              ColumnName := Quote(Copy(ColumnName, 1, Integer(TreeView1.Selected.Data)));
            InsertMemoText(ColumnName + ' ');
          end
          else
            exit;
        end;
      end
      else if TreeView1.Selected.Level >= 2 then
      begin
        if (TabSet1.TabIndex =1) and  (CDSQuery.State in [dsInsert, dsEdit]) then
        begin
          ColumnName := TreeView1.Selected.Text;
          if TreeView1.Selected.Data <> nil then
            ColumnName := Quote(Copy(ColumnName, 1, Integer(TreeView1.Selected.Data)));
          InsertMemoText(ColumnName + ' ');
        end
        else
          exit;
      end
      else
        exit;

      if {(TabSet1.TabIndex <> 1) and }(CDSQuery.State in [dsInsert, dsEdit]) then
      begin
        TabSet1.TabIndex := 1;
        memoSql.SetFocus;
      end;

    finally
      TreeView1.Items.EndUpdate;
    end;
  except
    on e:exception do
    begin
      //OutputDebugString(PChar(e.Message));
      raise;
    end;
  end;
end;

procedure TFormOdbcExplor.memoSql1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  TreeView1DblClick(Sender);
end;

function TFormOdbcExplor.Quote(const sName: string; bTable: Boolean = False): string;
var
  i: integer;
  P: PAnsiChar;
  sBuff: string;
  Status: SQLResult;
  Len : smallint;
begin
  if bTable then
  begin
    P := PAnsiChar(sName);
    Status := TSQLConnectionPro(SQLConnection1).Connection.setOption(TSQLConnectionOption(xeConnQualifiedName), LongInt(P));
    if Status = 0 then
    begin
      Len := 0;
      SetLength(sBuff, 1024);
      FillChar(sBuff[1], Length(sBuff), #0);
      Status := TSQLConnectionPro(SQLConnection1).Connection.getOption(TSQLConnectionOption(xeConnQuotedObjectName),
        @sBuff[1], Length(sBuff), Len);
      if Status = 0 then
      begin
        Result := StrPas(PAnsiChar(sBuff));
        exit;
      end;
    end;
  end;

  if (fQuoteChar<>'') then
  begin
    for i := 1 to Length(sName) do
    begin
      if not (LowerCase(sName[i])[1] in ['a'..'z','_','0'..'9','.', '$', '#', '@', '!','~']) then
      begin
        Result := fQuoteChar + sName + fQuoteChar;
        exit;
      end;
    end;
  end;
  Result := sName;
end;

procedure AddString2Strings(const Str: string; SLDest: TStrings);
var
  SLSrc: TstringList;
  i: Integer;
begin
  SLSrc := TStringList.Create;
  try
    SLSrc.Text := Str;
    for i := 0 to  SLSrc.Count - 1 do
    begin
      SLDest.Add(SLSrc[i]);
    end;
  finally
    SLSrc.Free;
  end;
end;

procedure TFormOdbcExplor.mnuGenSqlCreateTableClick(Sender: TObject);
var
  TableNode: TTreeNode;
  ColumnNode: TTreeNode;
  TableName: string;
  sAll, s, sField: string;
  GenType: integer;
  i, j: Integer;
  AllowExpansion: Boolean;
begin
  if TreeView1.Selected = nil then
    exit;

  TreeView1.Items.BeginUpdate;
  try

    TableNode := nil;

    if Sender = TObject(mnuGenSqlSelect) then
      GenType := 0
    else if Sender = TObject(mnuGenerateSqlSelectEach) then
      GenType := 1
    else if Sender = TObject(mnuGenSqlCreateTable) then
      GenType := 2
    else if Sender = TObject(mnuGenSqlInsert) then
      GenType := 3
    else if Sender = TObject(mnuGenerateSqlUpdate) then
      GenType := 4
    else if Sender = TObject(pmnuGenerateSqlDrop) then
      GenType := 5
    else if Sender = TObject(pmnuShowData) then
      GenType := 6
    else
      exit;

    if TreeView1.Selected.Level = 1 then
    begin
      TableNode := TreeView1.Selected;

      if not (TableNode.Parent.ImageIndex in [cImageIndexAllTables, cImageIndexTables, cImageIndexViews,
        cImageIndexSystemTables, cImageIndexSynonums ]) then
      exit;
      //if not (TableNode.Count=0)and(

      //TableNode.Expand(false);
      AllowExpansion := False;
      TreeView1Expanding(TreeView1, TableNode, AllowExpansion);

    end
    else if TreeView1.Selected.Level = 2 then
    begin
      TableNode := TreeView1.Selected.Parent;
      if not (TableNode.Parent.ImageIndex in [cImageIndexAllTables, cImageIndexTables, cImageIndexViews,
        cImageIndexSystemTables, cImageIndexSynonums]) then
      exit;
    end;

    if TableNode = nil then
      exit;

    TableName := TableNode.Text;

    case GenType of
      0:
        begin
          CheckSateMemoSql;
          CDSQuery.Fields[0].AsDateTime := Now;
          memoSql.Lines.Add ( 'SELECT * FROM ' + Quote(TableName, True));
        end;
      1:
        begin
          CheckSateMemoSql;
          CDSQuery.Fields[0].AsDateTime := Now;
          ColumnNode := TableNode.getFirstChild;
          i := 0;
          sAll := '';
          while (ColumnNode <> nil) and (ColumnNode.Text <> '[indexes]') do
          begin
            s :=  ColumnNode.Text;
            if ColumnNode.Data <> nil then
              s := Quote( Copy(s, 1, Integer(ColumnNode.Data)) );
            if i<>0 then
              s :=  '      ,' + s
            else
              s :=  '       ' + s;
            inc(i);
            sAll := sAll + #13#10 + s;
            ColumnNode := ColumnNode.GetNextSibling;
          end;
          if i>0 then
          begin
            //??? todo: bug: #13#10 in memoSql.Lines.Add
            //memoSql.Lines.Add('SELECT' + sAll + #13#10'FROM ' + Quote(TableName, True));
            AddString2Strings('SELECT' + sAll + #13#10'FROM ' + Quote(TableName, True), memoSql.Lines);
          end
          else
            memoSql.Lines.Add ( 'SELECT * FROM ' + Quote(TableName, True));
        end;
      2:
        begin
          CheckSateMemoSql;
          CDSQuery.Fields[0].AsDateTime := Now;
          if memoSql.Lines.Count > 0 then
            //memoSql.Lines.Add( #13#10 + edScriptLD.Text);
            AddString2Strings(#13#10 + edScriptLD.Text, memoSql.Lines);
          memoSql.Lines.Add ( 'CREATE TABLE ' + Quote(TableName, True) );
          memoSql.Lines.Add ( '(' );
          ColumnNode := TableNode.getFirstChild;
          sAll := '';
          while (ColumnNode <> nil) and (ColumnNode.Text <> '[indexes]') do
          begin
            s := ColumnNode.Text;
            s := #09 + Quote( Copy(s, 1, Integer(ColumnNode.Data)) ) +
              Copy(s, Integer(ColumnNode.Data)+1, Length(s)-Integer(ColumnNode.Data)) +
              ', ';
            sAll := sAll + s + #13#10;
            ColumnNode := ColumnNode.GetNextSibling;
          end;
          if sAll <> '' then
            SetLength(sAll, Length(sAll)-4);
          //memoSql.Lines.Add ( sAll );
          AddString2Strings( sAll, memoSql.Lines);
          memoSql.Lines.Add ( ')' );
          // create indeces ...
          //
          // create unique index ANIMALS_PK on ANIMALS(NAME)
          //ALTER TABLE BIOLIFE
          //  ADD CONSTRAINT PRIMARY KEY (SPECNO)
          //    CONSTRAINT BIOLIFPK;
          //
          if (TableNode.Count > 0) and ( TableNode.Item[TableNode.Count-1].Text = '[indexes]' ) then
          begin
            TableNode := TableNode.Item[TableNode.Count-1];
            if (TableNode.Count = 0) or ( (TableNode.Count = 1) and (TableNode[0].ImageIndex = 0) ) then
              PopulateIndexNode(TableNode);
            if (TableNode.Count > 0) and (TableNode[0].ImageIndex > 0) then
            begin
              memoSql.Lines.Add ('');
              sAll := '';
              for i := 0 to TableNode.Count -1 do
              begin
                if TableNode.Item[i].Count = 0 then
                  continue;
                sAll := sAll + #13#10 + edScriptLD.Text + #13#10;
                s := '';
                sField := '';
                for j:= 0 to TableNode[i].Count -1 do
                begin
                  ColumnNode := TableNode[i][j];
                  sField := Quote(ColumnNode.Text);
                  case ColumnNode.ImageIndex of
                    cImageIndexIndexColA:
                      s := s + sField + ' asc, '; // = default. Can be skipped.
                    cImageIndexIndexColD:
                      s := s + sField + ' desc, ';
                    cImageIndexIndexCol:
                      s := s + sField + ', ';
                  end;
                end;
                if Length(s) > 2 then
                  SetLength(s, Length(s)-2);
                ColumnNode := TableNode.Item[i];
                case ColumnNode.ImageIndex of
                  cImageIndexIndexA,
                  cImageIndexIndexD:
                    begin
                      sAll := sAll + format('create index %s on %s(%s)',
                        [ColumnNode.Text, TableName, s]);
                    end;
                  cImageIndexIndexAU,
                  cImageIndexIndexDU:
                    begin
                      sAll := sAll + format('create unique index %s on %s(%s)',
                        [ColumnNode.Text, TableName, s]);
                    end;
                  cImageIndexIndexPK:
                    //if fSupportsSqlPrimaryKeys then
                    begin
                      sAll := sAll +
                        format(
                          'alter table %s add constraint primary key (%s) constraint %s',
                          [TableName, sField, ColumnNode.Text]) +

                        #13#10 +

                        format('//create unique index %s on %s(%s)',
                          [ColumnNode.Text, TableName, s]);
                    end;
                  else
                    break;

                end;//of: case
              end;//of: for i
              //memoSql.Lines.Add (sAll);
              AddString2Strings( sAll, memoSql.Lines);
              memoSql.Lines.Add ('');
            end;
          end;
        end;
      3:
        begin
          CheckSateMemoSql;
          CDSQuery.Fields[0].AsDateTime := Now;
          memoSql.Lines.Add ( 'INSERT INTO ' + Quote(TableName, True));
          memoSql.Lines.Add ( '(' );
          ColumnNode := TableNode.getFirstChild;
          while (ColumnNode <> nil) and (ColumnNode.Text <> '[indexes]') do
          begin
            s :=  ColumnNode.Text;
            if ColumnNode.Data <> nil then
              s := Quote(Copy(s, 1, Integer(ColumnNode.Data)));
            s :=  #09 + s + ' ,';
            memoSql.Lines.Add ( s );
            ColumnNode := ColumnNode.GetNextSibling;
          end;
          memoSql.Lines[memoSql.Lines.Count-1] := Copy(s, 1, Length(s) - 1);
          memoSql.Lines.Add ( ')' );
          memoSql.Lines.Add ( 'VALUES' );
          memoSql.Lines.Add ( '(' );
          ColumnNode := TableNode.getFirstChild;
          while (ColumnNode <> nil) and (ColumnNode.Text <> '[indexes]') do
          begin
            s :=  ColumnNode.Text;
            if ColumnNode.Data <> nil then
              s := Quote(Copy(s, 1, Integer(ColumnNode.Data)));
            s :=  #09 + '[valueof-' + s + '] ,';
            memoSql.Lines.Add ( s );
            ColumnNode := ColumnNode.GetNextSibling;
          end;
          memoSql.Lines.Add ( ')' );
        end;
      4:
        begin
          CheckSateMemoSql;
          CDSQuery.Fields[0].AsDateTime := Now;
          memoSql.Lines.Add ( 'UPDATE ' + Quote(TableName, True));
          ColumnNode := TableNode.getFirstChild;
          while (ColumnNode <> nil) and (ColumnNode.Text <> '[indexes]') do
          begin
            s :=  ColumnNode.Text;
            if ColumnNode.Data <> nil then
              s := Quote(Copy(s, 1, Integer(ColumnNode.Data)));
            s :=  'SET ' + s + ' = [valueof-' + s + '] ,';
            memoSql.Lines.Add ( s );
            ColumnNode := ColumnNode.GetNextSibling;
          end;
          memoSql.Lines[memoSql.Lines.Count-1] := Copy(s, 1, Length(s) - 1);
        end;
      5:
        begin
          CheckSateMemoSql;
          CDSQuery.Fields[0].AsDateTime := Now;
          if memoSql.Lines.Count > 0 then
            //memoSql.Lines.Add ( #13#10 + edScriptLD.Text);
            AddString2Strings( #13#10 + edScriptLD.Text, memoSql.Lines);
          memoSql.Lines.Add ( 'DROP TABLE ' + Quote(TableName, True));
        end;
      6:
        begin
          CDS.Close;
          SQLDataSet1.Close;
          SQLDataSet1.CommandText := 'SELECT * FROM ' + Quote(TableName, True);
          TickCountInfo := GetTickCount;
          CDS.Open;
          TickCountInfo := GetTickCount - TickCountInfo;
          lbTableName.Caption := TableName;
          SB.Panels[0].Text := Format(cTickCountInfo, [TickCountInfo]);
          TreeView1.Selected.Collapse(False);
          exit;
        end;
    end;//of: case GenType
    TabSet1.TabIndex := 1;
    memoSql.SetFocus;
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TFormOdbcExplor.TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DownNode: TTreeNode;
begin
  if (Button = mbRight) then
  begin
    DownNode := TreeView1.GetNodeAt(X, Y);
    if (DownNode<>nil) and (not DownNode.Selected) then
      DownNode.Selected := true;
  end;
end;

{+}
procedure TFormOdbcExplor.CDSAfterOpen(DataSet: TDataSet);
var i:Integer;
begin
   pbOpen.Caption := 'Close';

   pbApplyUpdates.Enabled := SQLConnection1.Connected and (CDS.ChangeCount >0);
   cbUpdateCustSQL.Checked := False;
   pbMarkAllRecordsAsNew.Enabled := True;
   pbLogChanges.Enabled := True;
   if CDS.LogChanges then
     pbLogChanges.Caption := 'LogChanges (True)'
   else
     pbLogChanges.Caption := 'LogChanges (False)';

   // Iniit FieldWidth
   for i:=0 to DataSet.FieldCount-1 do
   begin
     if (DataSet.Fields[i].DataType = ftString) then
     begin
       if (DataSet.Fields[i].DisplayWidth > 50) then
         DataSet.Fields[i].DisplayWidth := 50;
     end
     (*
     else
     if (DataSet.Fields[i].DataType in [ftBCD, ftFmtBcd]) then
     begin
       DataSet.Fields[i].DisplayWidth := TBcdField(DataSet.Fields[i]).Size + 2;
     end
     else
     if (DataSet.Fields[i].DataType in [ftBCD, ftFmtBcd]) then
     begin
       DataSet.Fields[i].DisplayWidth := TFmtBcdField(DataSet.Fields[i]).Size + 2;
     end;
     //*)
   end;

   // Autodefined Update method
   if rbUpdateWhereKey.Checked then
     rbUpdateWhereAll.Checked := True;

   if (CDS.FieldCount>1) then
   begin
     for i:=0 to CDS.IndexFieldCount-1 do
     begin
       if (pfInKey in CDS.IndexFields[0].ProviderFlags) then
       begin
         rbUpdateWhereKey.Checked := True;
         Break;
       end;
     end;

     if not rbUpdateWhereKey.Checked then
       for i:=0 to CDS.FieldCount-1 do
         if pfInKey in CDS.Fields[i].ProviderFlags then
         begin
           rbUpdateWhereKey.Checked := True;
           Break;
         end;
   end;

   // Disable unavailable method or enable ...
   if not rbUpdateWhereKey.Checked then
   begin
     if ActiveControl = rbUpdateWhereKey then
       ActiveControl := pbApplyUpdates;
   end;
   rbUpdateWhereKey.Enabled := rbUpdateWhereKey.Checked;

   // PK- INDEX_TYPE=6
   // UNIQUE - INDEX_TYPE=2

   LinkEditors(True);
end;

{+.}

procedure TFormOdbcExplor.btnBeginTransactionClick(Sender: TObject);
begin
  SQLConnection1.StartTransaction(TD);
end;

procedure TFormOdbcExplor.btnCommitClick(Sender: TObject);
begin
  SQLConnection1.Commit(TD);
end;

procedure TFormOdbcExplor.btnRollackClick(Sender: TObject);
begin
  SQLConnection1.Rollback(TD);
end;

procedure TFormOdbcExplor.miImageClearClick(Sender: TObject);
begin
  if dbImage1.Field <> nil then
  begin
    DataSetToEditMode(CDS, False);
    dbImage1.Field.Clear;
  end;
end;

procedure TFormOdbcExplor.CDSAfterClose(DataSet: TDataSet);
begin
  lbTableName.Caption := '';
  pbOpen.Caption := 'Open';
  LinkEditors(False);
  FillLinkCombos(False);
  SQLDataset1.Close;
  SQLDataset1.CommandText := '';
  CDS.fBriefCaseTableName := '';
  CDS.fBriefCaseFileName := '';
  pbApplyUpdates.Enabled := False;
  pbLogChanges.Enabled := False;
  pbMarkAllRecordsAsNew.Enabled := False;
end;

// VARIANT TO STREAM

//(*
{$IFOPT R+}
  {$DEFINE RANGECHECKS_ON}
{$ELSE}
  {$DEFINE RANGECHECKS_OFF}
{$ENDIF}
{$IFDEF RANGECHECKS_ON} {$R-} {$ENDIF}
procedure VariantBytesToStream(const V:Variant; ms: TMemoryStream);
var
  P: PChar;
  ArraySize: Int64;
begin
  if (VarType(V) = varArray or varByte) then
  begin
    ArraySize := VarArrayHighBound(V, 1);
    ms.Clear;
    if ArraySize <= 0 then
      exit;
    inc(ArraySize);
    P := VarArrayLock(V);
    try
      ms.Write(P^, ArraySize);
      ms.Position := 0;
    finally
      VarArrayUnLock(V);
    end;
  end;
end;

function StreamToVariantBytes(ms: TMemoryStream): Variant;
var
  P: PChar;
  ArraySize: Int64;
begin
  Result := Null;
  if (ms = nil) then
    exit;
  ms.Position := 0;
  ArraySize := ms.Size;
  if (ArraySize = 0) then
    exit;
  Result := VarArrayCreate([0, ArraySize - 1], varByte);
  P := VarArrayLock(Result);
  try
    move(ms.Memory^, P^, ArraySize);
  finally
    VarArrayUnLock(Result);
  end;
end;

{$IFDEF RANGECHECKS_ON} {$R+} {$ENDIF}
//*)

{$ifdef _HexEditor_}
procedure TFormOdbcExplor.LoadBinary;
var
  F:TField;
  ms: TMemoryStream;
//  V:Variant;
begin
  F := CDS.FindField(fBinaryField);
  if F<>nil then
  begin
    HexEditor.BeginUpdate;
    try
      HexEditor.OnChange := nil;
      if F.IsNull then
        HexEditor.AsText := ''
      else
      case F.DataType of
        ftMemo,ftFmtMemo,ftGraphic,ftBlob,ftOraBlob,ftOraClob:
          begin
            ms := TMemoryStream.Create;
            (F as TBlobField).SaveToStream(ms);
            HexEditor.LoadFromStream(ms);
            ms.Free;
          end;
        ftString, ftFixedChar:
        begin
          HexEditor.AsText := F.AsString;
        end;
        ftBytes, ftTypedBinary:
        begin
          HexEditor.AsText := F.AsString;
        end;
        ftVarBytes:
        begin
          HexEditor.AsText := F.AsString;
          {
          //(F As TBinaryField).
          V := F.AsVariant;
          ms := TMemoryStream.Create;
           //WriteVariant(ms, V);
           VariantBytesToStream(V, ms);
           HexEditor.LoadFromStream(ms);
          ms.Free;
          V := VarEmpty;
          {}
        end;
      end;
      HexEditor.ResetSelection(False);
      if HexEditor.DataSize>0 then
      begin
        HexEditor.SelStart := 0;
        HexEditor.SelEnd := 0;
      end;
      HexEditor.Modified := False;
      HexEditor.OnChange := HexEditOnStateChanged;
    finally
      HexEditor.EndUpdate;
    end;
  end;
end;

procedure TFormOdbcExplor.HexEditOnStateChanged(Sender: TObject);
begin
  if HexEditor.Modified then
    DataSetToEditMode(CDS);
end;

// VARIANT TO STREAM

{$endif ifdef _HexEditor_}

procedure TFormOdbcExplor.DbImageSetBounds;
var
  NewBounds: TRect;
  CR: TRect;
begin
  CR := sbImage.ClientRect;

  NewBounds.Top := CR.Top;
  NewBounds.Left := CR.Left;

  if dbImage1.Picture.Width>90 then
    NewBounds.Right := dbImage1.Picture.Width
  else
    NewBounds.Right := CR.Right-CR.Left;

  if dbImage1.Picture.Height>90 then
    NewBounds.Bottom := dbImage1.Picture.Height
  else
    NewBounds.Bottom := CR.Bottom - CR.Top;

  with dbImage1.BoundsRect do
  begin
    if (NewBounds.Left <> Left) or (NewBounds.Right <> Right) or
       (NewBounds.Top <> Top) or (NewBounds.Bottom <> Bottom)
    then
    with NewBounds do
      dbImage1.SetBounds(Left, Top, Right, Bottom);
  end;
end;

function getGraphicFromStream(sm: TMemoryStream; out Graphic: TGraphic): Boolean;
type
  TBufferByte = array[0..64] of Byte;
  PBufferByte = ^TBufferByte;
  {$IFDEF _DEBUG_}
  TBufferChar = array[0..64] of AnsiChar;
  PBufferChar = ^TBufferChar;
  TBuffer = record
    case Byte of
      0: (Bytes: TBufferByte);
      1: (Chars: TBufferChar)
  end;
  PBuffer = ^TBuffer;
  {$ENDIF IFDEF _DEBUG_}
var
  Buff: PBufferByte;
  {$IFDEF _DEBUG_}
  BuffDebug: PBuffer absolute Buff;
  {$ENDIF IFDEF _DEBUG_}
  GraphicClass: TGraphicClass;
  {$IFDEF _GraphicEx_}
  GraphicClassEx: TGraphicExGraphicClass absolute GraphicClass;
  {$ENDIF IFDEF _GraphicEx_}
const
  stmOffsets: array[0..1]of byte = (0, 8);
begin
  Result := False;
  Graphic := nil;
  sm.Position := 0;
  {$IFDEF _GraphicEx_}
    GraphicClassEx := FileFormatList.GraphicFromContent(sm);
    if GraphicClassEx <> nil then
    begin
      Graphic := GraphicClassEx.Create;
      try
        sm.Position := 0;
        Graphic.LoadFromStream(sm);
        Result := True;
        exit;
      except
        Graphic.Free;
        Graphic := nil;
      end;
    end
    else
      sm.Position := 0;
  {$ENDIF IFDEF _GraphicEx_}

  // Binary autodetect:
    GraphicClass := nil;
    Buff := PBufferByte(sm.Memory);

    // JPEG:
    if
      ( // JFIF
        (AnsiChar(Buff[6]) = 'J') and (AnsiChar(Buff[7]) = 'F') and
        (AnsiChar(Buff[8]) = 'I') and (AnsiChar(Buff[9]) = 'F')
      ) or
      ( // Exif
        (AnsiChar(Buff[6]) = 'E') and (AnsiChar(Buff[7]) = 'x') and
        (AnsiChar(Buff[8]) = 'i') and (AnsiChar(Buff[9]) = 'f')
      )
    then
      GraphicClass := TJPEGImage
    else
    // BMP:
    if
      (AnsiChar(Buff[0]) = 'B') and (AnsiChar(Buff[1]) = 'M')
      // and (AnsiChar(Buff[2]) in ['6', 'B', 'z', ':', #$F])
    then
      GraphicClass := TBitmap
    else
    // BMP:
    if
      (AnsiChar(Buff[0+8]) = 'B') and (AnsiChar(Buff[1+8]) = 'M')
      // and (AnsiChar(Buff[2+8]) in ['6', 'B', 'z', ':', #$F])
    then
    begin
      GraphicClass := TBitmap;
      sm.Position := 8;
    end
    else
    // ICO:
    if
      (Buff[0] = 0) and (Buff[1] = 0) and
      (Buff[2] = 1) and (Buff[3] = 0) and
      (Buff[4] = 1) and (Buff[5] = 0)
    then
      GraphicClass := TIcon
    else
    {
    // x-ICON:
    if
      (Buff[0] = 32) and (Buff[1] = 32) and
      (Buff[2] = 01) and (Buff[3] = 32) and
      (Buff[4] = 01) and (Buff[5] = 32)
    then
      GraphicClass := TIcon // not supported
    else
    }
    if sm.Size > 60 then
    begin
      // WMF:
      if
        (AnsiChar(Buff[55]) = 'W') and (AnsiChar(Buff[56]) = 'M') and
        (AnsiChar(Buff[57]) = 'F')
      then
      GraphicClass := TMetafile;
    end;

  if GraphicClass = nil then
    exit;

  Graphic := GraphicClass.Create;

  try
    Graphic.LoadFromStream(sm);
    Result := True;
  except
    Graphic.Free;
    Graphic := nil;
  end;

end;

procedure TFormOdbcExplor.LoadImage;
var
  ms :TMemoryStream;
  stm: TStream;
  Graphic: TGraphic;
  cds_f: TField;
  AFieldType: TFieldType;
  s: string;
begin
  if dbImage1.Visible then
  try
    ms := TMemoryStream.Create;
    try
      cds_f := cds.FieldByName(dbImage1.DataField);
      case cds_f.DataType of
        ftBytes, ftBlob, ftGraphic, ftTypedBinary, ftOraBlob:
          if cds_f is TBlobField then
          begin
            (cds_f as TBlobField).SaveToStream(ms);
            if BinaryIsStringBinary( ms, AFieldType) then
            begin
              if AFieldType in [ftBytes, ftVarBytes, ftBlob, ftGraphic, ftOraBlob] then
                DecodeBinaryStringToBinary(ms, ms, AFieldType)
              else
                Abort;
            end
          end;
        ftString, ftWideString:
          begin
            if cds_f.Size > Length(cBlobsAsStringIdTemplate) then
            begin
              DecodeStringToBinary(cds_f.AsString, ms, AFieldType);
              //todo: unpack image from string
            end;
          end;
        ftVarBytes:
          begin
            if cds_f.Size > 0 then
            begin
              s := cds_f.AsString;
              if Length(s) > 0 then
                ms.WriteBuffer(s[1], Length(s));
              //DecodeStringToBinary(cds_f.AsString, ms, AFieldType);
              //todo: unpack image from string
            end;
          end;
        ftMemo, ftFmtMemo, ftOraClob:
          begin
            if TBlobField(cds_f).BlobSize > Length(cBlobsAsStringIdTemplate) then
            begin
              //DecodeStringToBinary(cds_f.AsString, ms, AFieldType);
              //todo: unpack image from string
              stm := cds.CreateBlobStream(cds_f, bmRead);
              try
                stm.Position := 0;
                ms.LoadFromStream(stm);
              finally
                stm.Free;
              end;
            end;
          end
          else
            exit;
      end;//of case DataType
      if ms.Size > 0 then
      begin
        if getGraphicFromStream(ms, Graphic) then
        begin
          if Graphic <> nil then
            DBImage1.Picture.Graphic := Graphic
          else
            try
              dbImage1.LoadPicture;
            except
            end;
        end
        else
          //Abort;
          dbImage1.LoadPicture;
      end
      else
      begin
        DBImage1.Picture.Graphic := nil;
      end;

      DbImageSetBounds;
    finally
      ms.Free;
    end;
  except
    // unknown image format or non image blob
    LinkImageToField('');
    {$ifdef _HexEditor_}
    if (fBinaryField='') then
    begin
      LinkEditors(True, c_skip_memo or c_skip_blob);
      if (TabSet1.TabIndex=3)and(fBinaryField<>'') then
        TabSet1.TabIndex := 4;
    end;
    {$endif ifdef _HexEditor_}
  end;
end;

procedure TFormOdbcExplor.CDSAfterScroll(DataSet: TDataSet);
begin
  LoadImage;
  {$ifdef _HexEditor_}
  if HexEditor.Visible then
    LoadBinary;
  {$endif ifdef _HexEditor_}
end;

procedure TFormOdbcExplor.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  vSheetOldIdx: Integer;
begin
//  if Notebook1.PageIndex = NewTab then
//    Exit;
  AllowChange := (NewTab = 0) or SQLConnection1.Connected;
  if not AllowChange then
  begin
    exit;
  end;
  vSheetOldIdx := Notebook1.PageIndex;
  Notebook1.PageIndex := NewTab;
  if not (SQLConnection1.Connected and (pClient.Visible)) then
  begin
    if (Notebook1.PageIndex = 0) and (not SQLConnection1.Connected) then
    begin
      if spCon.Visible then
        pTop.Tag := pTop.Height;
      pClient.Visible := False;
      spCon.Visible := False;
      pTop.Align := alClient;
    end
    else
    begin
      pTop.Align := alTop;
      if pTop.Tag > 0 then
        pTop.Height := pTop.Tag
      else
        pTop.Height := 200;
      spCon.Visible := True;
      pClient.Visible := True;
    end;
  end;
  if Assigned(fSheetActions[vSheetOldIdx]) then
    fSheetActions[vSheetOldIdx].State := asSuspended;
  if Assigned(fSheetActions[NewTab]) then
    fSheetActions[NewTab].State := asNormal;
end;

procedure TFormOdbcExplor.PopupMenuTreePopup(Sender: TObject);
var
 bVisible: Boolean;
begin

  bVisible := SQLConnection1.Connected and (TreeView1.Selected <> nil);
  if not bVisible then
    Abort;
end;

procedure TFormOdbcExplor.dbNavQueryBeforeAction(Sender: TObject;
  Button: TNavigateBtn);
begin
  if Button = nbCancel then
  begin
    CDSQuery.Cancel;
    if CDSQuery.RecordCount=0 then
      CDSQuery.Append;
    Abort;
  end
  else
  if (Button = nbDelete) and (CDSQuery.RecordCount=1)then
  begin
    if (not CDSQuery.Fields[1].IsNull) then
    begin
      CDSQuery.Cancel;
      CDSQuery.Edit;
      CDSQuery.Fields[1].Clear;
    end;
    Abort;
  end;
  if (Button = nbPost) then
  begin
    CDSQuery.Fields[0].AsDateTime := Now;
  end;
end;

procedure TFormOdbcExplor.CDSQueryBeforePost(DataSet: TDataSet);
begin
  if CDSQuery.Fields[0].isNull then
    CDSQuery.Fields[0].AsDateTime := Now;
end;

procedure TFormOdbcExplor.CheckSateMemoSql;
begin
  DataSetToEditMode(CDSQuery, True);
end;

procedure TFormOdbcExplor.pmImagePopup(Sender: TObject);
begin
 if dbImage1.DataField='' then
   Abort;
end;

procedure TFormOdbcExplor.sCDSQueryDataChange(Sender: TObject; Field: TField);
begin
  if CDSQuery.Active then
    lbQryCnt.Caption := IntToStr(CDSQuery.RecNo)+'/'+IntToStr(CDSQuery.RecordCount)
  else
    lbQryCnt.Caption := '0';
end;

{$ifdef _HexEditor_}
procedure TFormOdbcExplor.UpdateHexEditField;

var
  F:TField;
  ms: TMemoryStream;
//  V:Variant;
//  P : PChar;
begin
  if not ( HexEditor.Modified and (CDS.State in [dsInsert,dsEdit]) ) then
    exit;
  F := CDS.FindField(fBinaryField);
  if (F<>nil){ and HexEditor.Modified} then
  begin
    //DataSetToEditMode(CDS);
    case F.DataType of
      ftMemo,ftFmtMemo,ftGraphic,ftBlob,ftOraBlob,ftOraClob:
        begin
          if HexEditor.DataSize=0 then
            F.Clear
          else
          begin
            ms := TMemoryStream.Create;
            HexEditor.SaveToStream(ms);
            (F as TBlobField).LoadFromStream(ms);
            ms.Free;
          end;
        end;
      ftString, ftFixedChar:
        F.AsString := HexEditor.AsText;
      ftBytes, ftTypedBinary:
        begin
          if HexEditor.DataSize=0 then
            F.Clear
          else
          begin
            F.AsString := HexEditor.AsText;
          end;
        end;{}
      ftVarBytes:
        begin
          if HexEditor.DataSize=0 then
            F.Clear
          else
          begin
            F.AsString := HexEditor.AsText;
            {ms := TMemoryStream.Create;
            HexEditor.SaveToStream(ms);
            V :=VarArrayCreate([0,HexEditor.DataSize-1], varByte );
            P := VarArrayLock(V);
            try
              //p2 := P;
              //if F.DataType = ftVarBytes then
              //  inc( dword(p2), SizeOf(Word) );
              move(ms.Memory^, P^, HexEditor.DataSize);
            finally
              VarArrayUnLock(V);
            end;
            ms.Free;
            F.AsVariant := V;{}
          end;
        end;
    end;//of: case
    HexEditor.Modified := False;
  end;
end;
{$endif ifdef _HexEditor_}

procedure TFormOdbcExplor.CDSBeforeClose(DataSet: TDataSet);
begin
  {$ifdef _HexEditor_}
  UpdateHexEditField;
  {$endif ifdef _HexEditor_}
end;

procedure TFormOdbcExplor.CDSBeforeCancel(DataSet: TDataSet);
begin
  {$ifdef _HexEditor_}
  if HexEditor.Visible then
  begin
    //UpdateHexEditField;
    HexEditor.OnChange := nil;
    HexEditor.AsText := '';
    HexEditor.Modified := False;
    HexEditor.OnChange := HexEditOnStateChanged;
  end;
  {$endif ifdef _HexEditor_}
end;

procedure TFormOdbcExplor.CDSBeforePost(DataSet: TDataSet);
begin
  {$ifdef _HexEditor_}
  UpdateHexEditField;
  {$endif ifdef _HexEditor_}
end;

procedure TFormOdbcExplor.CDSAfterRefresh(DataSet: TDataSet);
begin
  {$ifdef _HexEditor_}
    LoadBinary;
  {$endif ifdef _HexEditor_}
end;

procedure TFormOdbcExplor.dbNavDataBeforeAction(Sender: TObject;
  Button: TNavigateBtn);
begin
  {$ifdef _HexEditor_}
   if not (Button in [nbDelete, nbEdit, nbCancel, nbRefresh ]) then
     UpdateHexEditField;
  {$endif ifdef _HexEditor_}

  if (Button =nbPost) and dbImage1.Visible
    and (cds.FieldByName(dbImage1.DataField) is TBlobField)
    and (cds.FieldByName(dbImage1.DataField) as TBlobField).Modified then
  begin
    cds.Post;
    LoadImage;
    Abort;
  end;

end;

function TFormOdbcExplor.LinkImageToField(const FieldName: String): Boolean;
var
  F: TField;
begin
  Result := False;
  F := CDS.FindField(FieldName);
  if F<>nil then
  begin
    try
      dbImage1.DataField := F.FieldName;
      Result := True;
    except
      dbImage1.DataField := '';
    end;
  end
  else
  begin
    dbImage1.DataField := '';
    cmImage.ItemIndex := 0;
  end;
  dbImage1.Visible := Result;
  with lbImageData, lbImageData.Font do
  if Result then
  begin
    Style := Style - [fsStrikeOut];
  end
  else
  begin
    Style := Style + [fsStrikeOut];
  end;
end;

function TFormOdbcExplor.LinkMemoToField(const FieldName: String): Boolean;
var
  F: TField;
begin
  Result := False;
  F := CDS.FindField(FieldName);
  if F <> nil then
  begin
    try
      cbUnicodeChars.Checked := False;
      dbMemo1.DataField := F.FieldName;
      fMemoField := FieldName;
      Result := True;
    except
      dbMemo1.DataField := '';
    end;
  end
  else
  begin
    dbMemo1.DataField := '';
    fMemoField := '';
  end;
  dbMemo1.Visible := Result;
  with lbMemoData, lbMemoData.Font do
  if Result then
  begin
    Style := Style - [fsStrikeOut];
  end
  else
  begin
    Style := Style + [fsStrikeOut];
  end;
end;

{$ifdef _HexEditor_}
function TFormOdbcExplor.LinkBinaryToField(const FieldName: String): Boolean;
var
  F: TField;
begin
  Result := False;
  F := CDS.FindField(FieldName);
  if F <> nil then
  begin
    fBinaryField := F.FieldName;
    Result := True;
  end
  else
  begin
    fBinaryField := '';
  end;
  HexEditor.Visible := Result;
  with lbBinaryData, lbBinaryData.Font do
  if Result then
  begin
    Style := Style - [fsStrikeOut];
  end
  else
  begin
    Style := Style + [fsStrikeOut];
  end;
  HexEditor.OnChange := nil;
  HexEditor.AsText := '';
  HexEditor.Modified := False;
  HexEditor.OnChange := HexEditOnStateChanged;
end;
{$endif ifdef _HexEditor_}

procedure TFormOdbcExplor.pBinaryResize(Sender: TObject);
begin
  if dbImage1.Visible then
    DbImageSetBounds;
end;

procedure TFormOdbcExplor.cmMemoChange(Sender: TObject);
begin
  LinkMemoToField(cmMemo.Text);
end;

procedure TFormOdbcExplor.cmImageChange(Sender: TObject);
begin
  LinkImageToField(cmImage.Text);
  LoadImage;
end;

procedure TFormOdbcExplor.cmBinaryChange(Sender: TObject);
begin
{$ifdef _HexEditor_}
  LinkBinaryToField(cmBinary.Text);
  LoadBinary;
{$endif ifdef _HexEditor_}
end;

procedure TFormOdbcExplor.CDSAfterCancel(DataSet: TDataSet);
begin
  LoadImage;
{$ifdef _HexEditor_}
  if HexEditor.Visible then
  begin
    LoadBinary;
  end;
{$endif ifdef _HexEditor_}
end;

procedure TFormOdbcExplor.cbTrimCharClick(Sender: TObject);
var
  bValue: Boolean;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      bValue := True
    else
      bValue := cbTrimChar.Checked;
    SQLConnection1.SQLConnection.SetOption(TSQLConnectionOption(xeConnTrimChar), Integer(bValue) );
  end;
end;

const
  cBool2CharInt: array[Boolean] of Char = ('0', '1');

procedure TFormOdbcExplor.cbEnableBCDClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbEnableBCD.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coEnableBCD]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbBCD2ExpClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbBCD2Exp.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coBCD2Exp]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbMaxBCDClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbMaxBCD.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coMaxBCD]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbMapInt64ToBcdClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbMapInt64ToBcd.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coMapInt64ToBcd]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbIgnoreUnknownFieldTypeClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbIgnoreUnknownFieldType.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coIgnoreUnknownFieldType]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbEmptyStrParamClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[not cbEmptyStrParam.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coEmptyStrParam]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbBlobFragmntnsClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbBlobFragmntns.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coBlobFragmentation]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbBlobNotTermCharClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbMixedFetch.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coBlobNotTerminationChar]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbMixedFetchClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbMixedFetch.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coMixedFetch]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbAutoIncClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbAutoInc.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coSupportsAutoInc]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.edBlobChunkSizeChange(Sender: TObject);
var
  NewChankSize: Integer;
begin
  if SQLConnection1.Connected and (Trim(edBlobChunkSize.Text)<>'') then
  begin
    if cbDefConOpt.Checked then
      NewChankSize := 40960
    else
      NewChankSize := StrToIntDef(edBlobChunkSize.Text, -1);
    if (NewChankSize >= 40960)or(NewChankSize <= 1024000) then
    begin
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coBlobChunkSize] + '=' + IntToStr(NewChankSize))) );
    end;
  end;
end;

procedure TFormOdbcExplor.edConPacketSizeChange(Sender: TObject);
var
  NewNetPacketSize: Integer;
begin
  if SQLConnection1.Connected and (Trim(edConPacketSize.Text)<>'') then
  begin
    if cbDefConOpt.Checked then
      NewNetPacketSize := 4096
    else
      NewNetPacketSize := StrToIntDef(edConPacketSize.Text, -1);
    if (NewNetPacketSize >= 4096){or(NewNetPacketSize <= 102400)} then
    begin
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coNetwrkPacketSize] + '=' + IntToStr(NewNetPacketSize))) );
    end;
  end;
end;

procedure TFormOdbcExplor.edDecimalSeparatorChange(Sender: TObject);
var
  NewDecimalSeparator: Char;
begin
  if SQLConnection1.Connected and (Trim(edDecimalSeparator.Text)<>'') then
  begin
    if cbDefConOpt.Checked then
      NewDecimalSeparator := cOptCharDefault
    else
      NewDecimalSeparator := edDecimalSeparator.Text[1];
    {
    // variant 1:
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coNumericSeparator]+'='+NewDecimalSeparator)) );
    {}
    // OR ALTERNATIVE:
    // variant 2:
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnDecimalSeparator), Integer(Ord(NewDecimalSeparator))
    );
    {}
  end;
end;

procedure TFormOdbcExplor.edBlockRowsCountChange(Sender: TObject);
var
  NewFetchBlockRowsCount: Integer;
begin
  if {SQLConnection1.Connected and} (Trim(edBlockRowsCount.Text)<>'') then
  begin
    if cbDefConOpt.Checked then
      NewFetchBlockRowsCount := 20
    else
      NewFetchBlockRowsCount := StrToIntDef(edBlockRowsCount.Text, -1);
    if NewFetchBlockRowsCount = 0 then
      NewFetchBlockRowsCount := 1;
    if (NewFetchBlockRowsCount <= 0) then
      NewFetchBlockRowsCount := 1;
    SQLConnection1.Params.Values[ROWSETSIZE_KEY] := IntToStr(NewFetchBlockRowsCount)
  end;
end;

procedure TFormOdbcExplor.pnMemoFontPlusClick(Sender: TObject);
begin
  memoSql.Font.Size := memoSql.Font.Size +1;
  {$ifdef _SynEdit_}
  memoSql.Gutter.Font.Size := memoSql.Font.Size;
  {$endif ifdef _SynEdit_}
end;

procedure TFormOdbcExplor.pnMemoFontMinusClick(Sender: TObject);
begin
  memoSql.Font.Size := memoSql.Font.Size -1;
  {$ifdef _SynEdit_}
  memoSql.Gutter.Font.Size := memoSql.Font.Size;
  {$endif ifdef _SynEdit_}
end;

procedure TFormOdbcExplor.SQLConnection1BeforeDisconnect(Sender: TObject);
{
var
  i: Integer;
  qry: TSQLDataset;
{}
begin
  SQLConnection1.AutoClone := True;
  cbAutoClone.Checked := True;
  //exit;
  {
  fSqlConOdbcIntf := nil;
  qry := SQLDataset1;
  for i:=fDetachedSQLList.Count-1 downto 0 do
  begin
    if fDetachedSQLList[i] = qry then
      continue;
    TObject(fDetachedSQLList[i]).Free;
    fDetachedSQLList[i] := nil;
  end;
  {}
end;

procedure TFormOdbcExplor.CDSAfterEdit(DataSet: TDataSet);
begin
  LoadImage;
end;

procedure TFormOdbcExplor.pmImageRecompessAs(Sender: TObject);
{$ifdef _GraphicEx_}
var

  Target: TGraphic;
  TargetClass: TGraphicClass;
  sm: TMemoryStream;

  { // moved to FormCreate
  Base: TSaveToStream;
  Impl: TSaveToStream;
  ClassTGraphic: TClass;
  {}

  procedure RaiseException;
  begin
    raise Exception.Create('Not Implemented method: "'+
      TargetClass.ClassName+'.'+'SaveToStream(...)"');
  end;

{$endif ifdef _GraphicEx_}
begin
{$ifdef _GraphicEx_}
  TargetClass := FileFormatList.GraphicFromExtension((Sender As TMenuItem).Hint);
  if TargetClass=nil then
    exit;
  {  // moved to FormCreate
  // ------ check begin:
  ClassTGraphic := TargetClass;
  if TargetClass.InheritsFrom(TGraphicExGraphic) then
  begin
    Impl := TGraphicExGraphic(@ClassTGraphic).SaveToStream;
    while (ClassTGraphic <> nil) and (ClassTGraphic <> TGraphicExGraphic) do
      ClassTGraphic := ClassTGraphic.ClassParent;
    if ClassTGraphic <> nil then
      Base := TGraphicExGraphic(@ClassTGraphic).SaveToStream;
  end
  else
  begin
    Impl := TGraphic(@ClassTGraphic).SaveToStream;
    while (ClassTGraphic <> nil) and (ClassTGraphic <> TGraphic) do
      ClassTGraphic := ClassTGraphic.ClassParent;
    if ClassTGraphic <> nil then
      Base := TGraphic(@ClassTGraphic).SaveToStream;
  end;
  if (ClassTGraphic = nil) or (TMethod(Impl).Code = TMethod(Base).Code) then
    RaiseException;
  //--------check end.
  {}

  Target := TargetClass.Create;
  sm := nil;
  try

    Target.Assign(dbImage1.Picture.Graphic);

    if DBImage1.Field <> nil then
    begin
      sm := TMemoryStream.Create;
      if Target is TGraphicExGraphic then
        TGraphicExGraphic(Target).SaveToStream(sm)
      else
        Target.SaveToStream(sm);
      if sm.Size>0 then
      begin
        sm.Position := 0;
        DataSetToEditMode(CDS, False);
        (DBImage1.DataSource.DataSet.FieldByName(DBImage1.DataField) as TBlobField).LoadFromStream(sm);
        LoadImage;
      end
      else
        RaiseException
    end;
  finally
    Target.Free;
    sm.Free;
  end;
{$endif ifdef _GraphicEx_}
end;

procedure TFormOdbcExplor.pmImageLoadClick(Sender: TObject);
var
  FN: String;
  sm: TMemoryStream;
  Graphic: TGraphic;
  vField: TField;
begin
  {$ifdef _GraphicEx_}
  OPD.Filter := FileFormatList.GetGraphicFilter([], fstBoth, [foCompact, foIncludeAll, foIncludeExtension], nil);
  {$else ifdef _GraphicEx_}
  OPD.Filter := 'All (*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf)|*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf';
  {$endif ifdef _GraphicEx_}
  if OPD.Execute then
    FN := OPD.FileName
  else
    exit;

  Graphic := nil;
  sm := TMemoryStream.Create;
  try

    sm.LoadFromFile(FN);
    if sm.Size > 0 then
    begin
      if getGraphicFromStream(sm, Graphic) then
      begin

        Graphic.LoadFromFile(FN);
        DBImage1.Picture.Graphic := Graphic;

        if DBImage1.Field <> nil then
        begin
          DataSetToEditMode(CDS, False);

          vField := DBImage1.DataSource.DataSet.FieldByName(DBImage1.DataField);

          case vField.DataType of
            ftBytes, ftBlob, ftGraphic, ftTypedBinary, ftOraBlob, ftMemo, ftFmtMemo, ftOraClob:
              (vField as TBlobField).LoadFromFile(FN);
              //DBImage1.Picture.Graphic := Graphic;
            else
            begin
              sm.Position := 0;
              SetLength(FN, sm.Size);
              sm.ReadBuffer(FN[1], sm.Size);
              vField.AsString := FN;
            end;
          end;
        end

      end
      else
      begin
        ShowMessage('WARNING: Unsupported image format');
      end;
    end
    else
    begin
      if DBImage1.Field <> nil then
      begin
        DataSetToEditMode(CDS, False);
        DBImage1.Field.Clear;
      end;
      DBImage1.Picture.Graphic := nil;
    end;

    DbImageSetBounds;

  finally
    sm.Free;
    Graphic.Free;
  end;

end;

procedure TFormOdbcExplor.pmImageSaveClick(Sender: TObject);
var
  {$ifdef _GraphicEx_}
  GraphicClass: TGraphicExGraphicClass;
  s: String;
  i, iL, iR : integer;
  bIsEx: Boolean;
  {$endif ifdef _GraphicEx_}
  ms: TMemoryStream;
begin
  if dbImage1.Picture.Graphic = nil then
    exit;

  ms := TMemoryStream.Create;
  try
    SPD.FileName := '';
    {$ifdef _GraphicEx_}
    bIsEx := dbImage1.Picture.Graphic is TGraphicExGraphic;
    if bIsEx then
      //TGraphicExGraphic(dbImage1.Picture.Graphic).SaveToStream(ms)
      (cds.FieldByName(dbImage1.DataField) as TBlobField).SaveToStream(ms)
    else
    {$endif ifdef _GraphicEx_}
    if dbImage1.Picture.Graphic is TJPEGImage then
      TJPEGImage(dbImage1.Picture.Graphic).SaveToStream(ms)
    else
      dbImage1.Picture.Graphic.SaveToStream(ms);
    ms.Position := 0;
    {$ifdef _GraphicEx_}
    GraphicClass := FileFormatList.GraphicFromContent(ms);
    //if GraphicClass = nil then
    if not bIsEx then
    {$endif ifdef _GraphicEx_}
    begin
      if dbImage1.Picture.Graphic is TJPEGImage then
        SPD.Filter := 'jpeg'
      else
      if dbImage1.Picture.Graphic is TBitmap then
        SPD.Filter := 'bmp'
      else
      if dbImage1.Picture.Graphic is TIcon then
        SPD.Filter := 'ico'
      else
      if dbImage1.Picture.Graphic is TMetafile then
        SPD.Filter := 'wmf'
      else
        SPD.Filter := '';

      SPD.DefaultExt := SPD.Filter;
      if SPD.Filter <> '' then
        SPD.Filter := SPD.Filter+' (*.'+SPD.Filter+')|*.'+SPD.Filter+'|All Files(*.*)|*.*'
      else
        SPD.Filter := 'All Files(*.*)|*.*';
    end
    {$ifdef _GraphicEx_}
    else
    begin

      SPD.Filter := FileFormatList.GetGraphicFilter([], fstBoth,
        [foCompact, {foIncludeAll,} foIncludeExtension], GraphicClass);
      if SPD.Filter <>'' then
      begin
        s := SPD.Filter;
        iL := Pos('*.', s);
        if iL > 0 then
        begin
          iR := PosEx('|', s, iL+1);
          if iR > 0 then
          begin
            i := PosEx('*.', s, iR+1);
            if i > 0 then
              iL := i;
          end;

          inc(iL, 2);
          iR := Length(s);
          for i:=iL to iR do
            if s[i] in [ ')', '|' ] then
            begin
              iR := i - 1;
              break;
            end;
          SPD.DefaultExt := Copy(s, iL, iR-iL+1);
        end
        else
        begin
          SPD.Filter := '*.*';
          SPD.DefaultExt := '';
        end;
      end
      else
      begin
        SPD.Filter := '*.*';
        SPD.DefaultExt := '';
      end;
    end;
    {$endif ifdef _GraphicEx_}
  finally
    ms.Free;
  end;

  if SPD.Execute then
  begin
    {$ifdef _GraphicEx_}
    if bIsEx then
      (cds.FieldByName(dbImage1.DataField) as TBlobField).SaveToFile(SPD.FileName)
    else
    {$endif ifdef _GraphicEx_}
      dbImage1.Picture.SaveToFile(SPD.FileName);
  end;
end;

procedure TFormOdbcExplor.edPacketRecordsChange(Sender: TObject);
var
  vPacketRecords: Integer;
begin

  if cbPacketRecords.Checked then
  begin
    vPacketRecords := StrToIntDef(edPacketRecords.Text, CDS.PacketRecords);
    if vPacketRecords < DBClient.AllRecords{-1} then
      vPacketRecords := DBClient.AllRecords;
  end
  else
    vPacketRecords := DBClient.AllRecords{-1};

  if vPacketRecords <> CDS.PacketRecords then
  begin
    CDS.PacketRecords := vPacketRecords;
  end;
end;

procedure TFormOdbcExplor.cbPacketRecordsClick(Sender: TObject);
begin
  //edPacketRecords.Enabled := cbPacketRecords.Checked;
  edPacketRecordsChange(Sender);
end;

procedure TFormOdbcExplor.cbAutoCloneClick(Sender: TObject);
begin
  SQLConnection1.AutoClone := cbAutoClone.Checked;
end;

procedure TFormOdbcExplor.SaveData1Click(Sender: TObject);
begin
  if not CDS.Active then
    exit;
  if CDS.fBriefCaseFileName<>'' then
    SPD.FileName := CDS.fBriefCaseFileName
  else
  if lbTableName.Caption <> '' then
    SPD.FileName := UpperCase(lbTableName.Caption)+'.CDS'
  else
    SPD.FileName := 'DATA.CDS';
  SPD.DefaultExt := 'cds';
  SPD.Filter := 'Data Files(*.cds;*.dat)|*.CDS;*.DAT|All Files (*.*)|*.*';
  if SPD.Execute then
  begin
    CDS.SaveToFile(SPD.FileName, dfXML);//(dfBinary, dfXML, dfXMLUTF8)
  end;
end;

{$IFDEF _D10UP_}

procedure TFormOdbcExplor.DataSetProvider1GetTableName_D10_(Sender: TObject;
  DataSet: TDataSet; var TableName: WideString);
var
  sTableName: string;
begin
  if TableName='' then
  begin
    if CDS.fBriefCaseTableName='' then
    begin
      if InputQuery('Update settings', 'enter updating table name', sTableName) then
      begin
        CDS.fBriefCaseTableName := sTableName;
        lbTableName.Caption := sTableName;
      end;
    end;
    TableName := CDS.fBriefCaseTableName;
  end;
end;

{$ELSE}

procedure TFormOdbcExplor.DataSetProvider1GetTableName_D67_(Sender: TObject;
  DataSet: TDataSet; var TableName: String);
begin
  if TableName='' then
  begin
    if CDS.fBriefCaseTableName='' then
    begin
      if InputQuery('Update settings', 'enter updating table name', TableName) then
      begin
        CDS.fBriefCaseTableName := TableName;
        lbTableName.Caption := TableName;
      end;
    end;
    TableName := CDS.fBriefCaseTableName;
  end;
end;

{$ENDIF}

procedure TFormOdbcExplor.OpenData1Click(Sender: TObject);
var
  tCDS: TClientDataSet;
  MapFields: array of string;
  IgFields: array of Boolean;
  i, L, iC: integer;
  s: string;
  OAV: TAutoRefreshFlag;
  OReadOnly: Boolean;
  Field: TField;
begin
  OPD.FileName := '';
  OPD.DefaultExt := 'cds';
  OPD.Filter := 'Data Files(*.cds;*.dat)|*.cds;*dat|All Files (*.*)|*.*';
  if not OPD.Execute then
    exit;

  if CDS.Active then
  begin
    // todo: prompt: to remap cds to opened query
    if (MessageBox(Handle,
      'Remap selected CDS fields to opened query ?',
      'ODBC Explor: Question',
      MB_ICONQUESTION or MB_YESNO or MB_SETFOREGROUND or MB_TOPMOST or
      MB_APPLMODAL or MB_DEFBUTTON2) <> idOK) then
    begin
      CDS.Close;
    end;
  end;

  if CDS.Active then
  begin
    tCDS := TClientDataSet.Create(Self);
    try
      tCDS.LoadFromFile(OPD.FileName);

      // read fields mapping:

      L := tCDS.FieldCount;
      SetLength(MapFields, L);
      SetLength(IgFields, L);
      iC := 0;
      for i := 0 to L-1 do
      begin
        MapFields[i] := tCDS.Fields[i].FieldName;
        IgFields[i] := True;
        if CDS.FindField(MapFields[i]) = nil then
        begin
          s := #0;
          while InputQuery('Enter destination field', 'Map Field "'+MapFields[i]+'" to Field :', s) do
          begin
            if CDS.FindField(s) = nil then
              continue;
            IgFields[i] := False;
            MapFields[i] := s;
            inc(iC);
            break;
          end
        end
        else
        begin
          IgFields[i] := False;
          inc(iC);
        end;
      end;

      if iC=0 then
        exit;

      tCDS.First;
      while not tCDS.Eof do
      begin
        CDS.Append;
          for i := 0 to L-1 do
            if not IgFields[i] then
            begin
              Field := CDS.FieldByName(MapFields[i]);

              if Field.AutoGenerateValue <> arNone then
              begin
                OAV := Field.AutoGenerateValue;
                Field.AutoGenerateValue := arNone;
              end
              else
                OAV := arNone;

              OReadOnly := Field.ReadOnly;
              if OReadOnly then
                Field.ReadOnly := False;

              //if Field.DataType = ftAutoInc then
              //  Field.DataType := ftInteger;
              if Field.DataType = ftAutoInc then
                Field.Required := True;

              Field.AsVariant := tCDS.Fields[i].AsVariant;

              if OAV <> arNone then
                Field.AutoGenerateValue := OAV;
              //if OReadOnly then
              //  Field.ReadOnly := OReadOnly;

            end;

        CDS.Post;

        tCDS.Next;
      end;
    finally
      tCDS.Free;
    end;
  end
  else
  begin
    if CDS.Active then
      CDS.Close;
    CDS.LoadFromFile(OPD.FileName);
    CDS.fBriefCaseFileName := OPD.FileName;
  end;
end;

procedure TFormOdbcExplor.DataSetProvider1UpdateError(Sender: TObject;
  DataSet: TCustomClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind;
  var Response: TResolverResponse);
begin
  if cbUpdIgnoreError.Checked and
  (
    (E.Message = 'Record not found or changed by another user' )
    or
    (E.Message = 'Invalid response')
  )
  then
  begin
    if CDS.fUpdatesErrorRecNo = 0 then
    begin
      Response := rrMerge;
      CDS.fUpdatesErrorRecNo := DataSet.RecNo;//Delta.RecNo;
    end
    else
    begin
      CDS.fUpdatesErrorRecNo := 0;
      Response := rrIgnore;
    end;
  end
  else
  begin
    inc(CDS.fUpdatesErrorCount);
    if CDS.fUpdatesErrorCount=1 then
    try // Show only first error
      raise Exception.Create(e.Message);
    except
      on NewE: Exception do
        Application.HandleException(NewE);
    end;
  end;
end;

procedure TFormOdbcExplor.SaveOptions1Click(Sender: TObject);
begin
  SaveConfigurations;
end;

procedure TFormOdbcExplor.Timer1Timer(Sender: TObject);
begin
  lbConnectedTime.Caption := 'Connected Time: '+TimeToStr(Timer1.fStartTime-Now);
end;

procedure TFormOdbcExplor.cbGetMetadataClick(Sender: TObject);
begin
  {$IFDEF _D7UP_}
  SQLDataset1.GetMetadata := cbGetMetadata.Checked;
  {$ELSE}
  SQLDataset1.NoMetadata := not cbGetMetadata.Checked;
  {$ENDIF}
end;

procedure TFormOdbcExplor.TreeView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_F5 then
 begin
   Key := 0;
   pmnuRefreshAllClick(Sender);
 end
 else
 if Key = VK_SPACE then
 begin
   Key := 0;
   mnuGenSqlCreateTableClick(pmnuShowData);
 end;
end;

procedure TFormOdbcExplor.About1Click(Sender: TObject);
begin
  ShowMessage(
 'Delphi Test program for Open Odbc DbExpress Driver.'#13+
  'Version ' + c_version + #13+
  #13+
  'Copyright (c) 2001-2006 Edward Benson'#13+
  #13+
  'This program is free software; you can redistribute it and/or modify'#13+
  'it under the terms of the GNU General Public License as published by'#13+
  'the Free Software Foundation; either version 2 of the License, or'#13+
  '(at your option) any later version.'#13+
  #13+
  'This program is distributed in the hope that it will be useful,'#13+
  'but WITHOUT ANY WARRANTY; without even the implied warranty of'#13+
  'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'#13+
  'See the GNU General Public License for more details.'
  );
end;

procedure TFormOdbcExplor.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormOdbcExplor.pmnuRefreshAllClick(Sender: TObject);
begin
   TreeView1.FullCollapse;
   InitTreeView;
end;

procedure TFormOdbcExplor.cbDefConOptClick(Sender: TObject);
var
  i: integer;
begin
  if not cbDefConOpt.Checked then
  begin
    edDecimalSeparator.OnChange := nil;
    edDecimalSeparator.Text := '';
    edDecimalSeparator.OnChange := edDecimalSeparatorChange;
  end;
  gbCustConOpt.Enabled := not cbDefConOpt.Checked;
  for i:=0 to gbCustConOpt.ControlCount-1 do
  begin
    if gbCustConOpt.Controls[i].Tag <> 0 then
    begin
      if not gbCustConOpt.Enabled then
      begin
        gbCustConOpt.Controls[i].Tag := 1 + Byte(gbCustConOpt.Controls[i].Enabled);
        gbCustConOpt.Controls[i].Enabled := gbCustConOpt.Enabled;
      end
      else
      begin
        //gbCustConOpt.Controls[i].Tag := 1 + Byte(gbCustConOpt.Enabled);
        gbCustConOpt.Controls[i].Enabled := (gbCustConOpt.Controls[i].Tag - 1) <> 0;
      end;
    end
    else
      gbCustConOpt.Controls[i].Enabled := gbCustConOpt.Enabled;
  end;
  SetConnectionOptionsAfterConnect;
end;

procedure RecordToQueryParams(qry: TSQLQuery; DeltaDS: TCustomClientDataSet);
begin
  // TODO: ???
  raise Exception.Create('Not Implemented it');
end;

procedure TFormOdbcExplor.DataSetProvider1BeforeUpdateRecord(
  Sender: TObject; SourceDS: TDataSet; DeltaDS: TCustomClientDataSet;
  UpdateKind: TUpdateKind; var Applied: Boolean);
var
  qry: TSQLQuery;
begin
  if not cbUpdateCustSQL.Checked then
    exit;
  qry := TSQLQuery.Create(nil);
  try
    qry.SQLConnection := SQLConnection1;
    case UpdateKind of
      ukModify:
        begin
          qry.SQL.Text := mSQLModify.Lines.Text;
        end;
      ukInsert:
        begin
          qry.SQL.Text := mSQLInsert.Lines.Text;
        end;
      ukDelete:
        begin
          qry.SQL.Text := mSQLDelete.Lines.Text;
        end;
    end;

    if Length(Trim(qry.SQL.Text))=0 then
      Applied := False // TCustomResolver.InternalUpdateRecord applyed Cstom Update method
      // but is not right
    else
    begin
      RecordToQueryParams(qry, DeltaDS);
      qry.ExecSQL({Direct=}True);
      if not cbUpdIgnoreError.Checked then
        Applied := qry.RowsAffected>0
      else
        Applied := True;
    end;
  finally
    qry.Free;
  end;
end;

procedure TFormOdbcExplor.btTreeAllignClick(Sender: TObject);
begin
  if pTree.Width<=spTree.MinSize{pTree.Constraints.MinWidth} then
    pTree.Width := 240
  else
    pTree.Width := pTree.Constraints.MinWidth;
end;

procedure TFormOdbcExplor.btConAllignClick(Sender: TObject);
begin
  if pTop.Height<=spCon.MinSize then
  begin
    pTop.Height := 257;
    pTop.Repaint;
  end
  else
    pTop.Height := pTop.Constraints.MinHeight;
end;

procedure TFormOdbcExplor.DataSource1DataChange(Sender: TObject;
  Field: TField);
begin
  pbApplyUpdates.Enabled := SQLConnection1.Connected and (CDS.ChangeCount >0);
end;

procedure TFormOdbcExplor.cbInternalCloneConnectionClick(Sender: TObject);
var
  nValue: Char;
  sConString, sOLDValue: string;
begin
  //???: !!! only before set connection !!!
  if (not SQLConnection1.Connected) and cbInternalCloneConnection.Enabled then
  begin
    sConString := Trim(dfDSN.Text);
    sOLDValue := GetOptionValue(sConString, cConnectionOptionsNames[coInternalCloneConnection],
      {HideOption=}True, {TrimResult=}True, {bOneChar=}False,
      {HideTemplate=}#0);
    if not cbDefConOpt.Checked then
    begin
      nValue := cBool2CharInt[cbInternalCloneConnection.Checked];
      if (sConString = '')  or ( sConString = '?') then
        sConString := 'DSN=?';
      dfDSN.Text := cConnectionOptionsNames[coInternalCloneConnection]+'=' + nValue + ';' + sConString;
    end
    else
    if sOLDValue <> #0 then
      dfDSN.Text := sConString;
  end;
end;

procedure TFormOdbcExplor.FormResize(Sender: TObject);
begin
  if Visible and (not (csDestroying in ComponentState)) then
  begin
    //1
    dfDSN.Width := gbLogin.Width  - dfDSN.Left - 10;
    dfUID.Width := dfDSN.Width;
    //2
    pQueryOpt.Left := Notebook1.ClientWidth - pQueryOpt.Width;
    pSQL.Width := Notebook1.ClientWidth - pQueryOpt.Width -1;
  end;
end;

procedure TFormOdbcExplor.FormShow(Sender: TObject);
begin
  OnShow := nil;
  FormResize(Sender);
  {$IFDEF _XPMENU_}
  fXPMenu.Active := True;
  Invalidate;
  {$ENDIF}
end;

procedure TFormOdbcExplor.pbMarkCurrentRecordsAsNewClick(Sender: TObject);
begin
  try
    CDS.MarkCurrentRecordsAsNew(cbPackBlobsToString.Checked, cbCompressBlob.Checked);
  finally
    if CDS.LogChanges then
      pbLogChanges.Caption := 'LogChanges (True)'
    else
      pbLogChanges.Caption := 'LogChanges (False)';
  end;
end;

procedure TFormOdbcExplor.pbMarkAllRecordsAsNewClick(Sender: TObject);
begin
  try
    CDS.MarkAllRecordsAsNew(cbPackBlobsToString.Checked, cbCompressBlob.Checked);
  finally
    if CDS.LogChanges then
      pbLogChanges.Caption := 'LogChanges (True)'
    else
      pbLogChanges.Caption := 'LogChanges (False)';
  end;
end;

procedure TFormOdbcExplor.pbLogChangesClick(Sender: TObject);
begin
  CDS.LogChanges := not CDS.LogChanges;
  if CDS.LogChanges then
    pbLogChanges.Caption := 'LogChanges (True)'
  else
    pbLogChanges.Caption := 'LogChanges (False)';
end;

procedure TFormOdbcExplor.pmMemoWordWrapClick(Sender: TObject);
begin
  memoSql.WordWrap := not memoSql.WordWrap;
end;

procedure TFormOdbcExplor.pbDetachClick(Sender: TObject);
var
  qry: TSQLDataset;
begin
  //debug: Detach Cursor
  fDetachedSQLList.Add(SQLDataset1);
  qry := SQLDataset1;
  Self.RemoveComponent(qry);
  SQLDataset1 := nil;
  SQLDataset1 := TSQLDataset.Create(Self);
  SQLDataset1.SQLConnection := SQLConnection1;
  DataSetProvider1.DataSet := SQLDataset1;
  SQLDataset1.CommandText := qry.CommandText;
  SQLDataset1.CommandType := qry.CommandType;
end;

procedure TFormOdbcExplor.dfDSNCloseUp(Sender: TObject);
begin
  {empty}
end;
(*
var
  sConString, sVendorLib: string;
begin
  if SQLConnection1.Connected or (VendorLib.Tag=1) then
    exit;

  VendorLib.Tag:=1;
  try
    sConString := dfDSN.Text;
    sVendorLib := GetOptionValue(sConString, 'VENDORLIB',
      {HideOption=}False, {TrimResult=}True, {bOneChar=}False,
      {HideTemplate=}#0);
    if sVendorLib = #0 then
      VendorLib.ItemIndex := 0
    else
    begin
      VendorLib.ItemIndex := -1;
    end;
  finally
    VendorLib.Tag:=0;
  end;
end;
*)

procedure TFormOdbcExplor.dfDSNSelect(Sender: TObject);
var
  sConString, sVendorLib: string;
  i: integer;
begin
  if SQLConnection1.Connected or (VendorLib.Tag=1) then
    exit;

  VendorLib.Tag:=1;
  try
    sConString := dfDSN.Text;
    sVendorLib := GetOptionValue(sConString, 'VENDORLIB',
      {HideOption=}False, {TrimResult=}True, {bOneChar=}False,
      {HideTemplate=}#0);
    if sVendorLib = #0 then
      VendorLib.ItemIndex := 0
    else
    begin
      VendorLib.ItemIndex := -1;
      sVendorLib := ';VENDORLIB='+UpperCase(sVendorLib)+';';
      for i := 0 to VendorLib.Items.Count-1 do
      begin
        if Pos(sVendorLib, ';'+UpperCase(Trim(VendorLib.Items[i])+';')) > 0 then
        begin
          VendorLib.ItemIndex := i;
          Break;
        end;
      end;
    end;
  finally
    VendorLib.Tag:=0;
  end;
end;

procedure TFormOdbcExplor.cbParamDateByOdbcLevelClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbParamDateByOdbcLevel.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coParamDateByOdbcLevel3]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbReadOnlyFieldClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbReadOnlyField.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coFldReadOnly]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbUnicodeCharsClick(Sender: TObject);
begin
  {$ifdef _HexEditor_}
  HexEditor.UnicodeChars := cbUnicodeChars.Checked;
  {$endif}
end;

procedure TFormOdbcExplor.DBGrid1DrawDataCell(Sender: TObject;
  const Rect: TRect; Field: TField; State: TGridDrawState);
var
  bUnicodeString: Boolean;
begin
  if Field is TWideStringField then
    bUnicodeString := True
  else
    bUnicodeString := False;
  if bUnicodeString then
  begin

  end;
end;

procedure TFormOdbcExplor.cbNullStrParamClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[not cbNullStrParam.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coNullStrParam]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbSafeModeClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbSafeMode.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coSafeMode]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbMapSmallBcdToNativeClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbMapSmallBcdToNative.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coMapSmallBcdToNative]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbMapCharAsBDEClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbMapCharAsBDE.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coMapCharAsBDE]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbEnableUnicodeClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbEnableUnicode.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coEnableUnicode]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.cbReadOnlyClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbReadOnly.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coReadOnly]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.edLockModeChange(Sender: TObject);
var
  vLockMode: Integer;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      vLockMode := 17
    else
      vLockMode := StrToIntDef(edLockMode.Text, -2);
    if vLockMode < -1 then
      Exit;
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coLockMode] + '=' + IntToStr(vLockMode))) );
  end;
end;

procedure TFormOdbcExplor.edConTimeoutChange(Sender: TObject);
var
  vConTimeout: Integer;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      vConTimeout := -1
    else
      vConTimeout := StrToIntDef(edConTimeout.Text, -1);
    if vConTimeout < 0 then
      vConTimeout := -1;
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coConTimeout] + '=' + IntToStr(vConTimeout))) );
  end;
end;

procedure TFormOdbcExplor.edQueryTimeoutChange(Sender: TObject);
var
  vNetTimeout: Integer;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      vNetTimeout := -1
    else
      vNetTimeout := StrToIntDef(edQueryTimeout.Text, -1);
    if vNetTimeout < 0 then
      vNetTimeout := -1;
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coNetTimeout] + '=' + IntToStr(vNetTimeout))) );
  end;
end;

procedure TFormOdbcExplor.cbSupportsSchemaFilterClick(Sender: TObject);
var
  nValue: Char;
begin
  if SQLConnection1.Connected then
  begin
    if cbDefConOpt.Checked then
      nValue := cOptCharDefault
    else
      nValue := cBool2CharInt[cbSupportsSchemaFilter.Checked];
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo),
      Integer(PChar(cConnectionOptionsNames[coSupportsSchemaFilter]+'='+nValue)) );
  end;
end;

procedure TFormOdbcExplor.btBlobSaveClick(Sender: TObject);
{$ifdef _HexEditor_}
var
  F: TField;
  ms: TMemoryStream;
{$endif}
begin
{$ifdef _HexEditor_}
  if (fBinaryField = '') or (not SD.Execute) then
    Exit;
  F := CDS.FindField(fBinaryField);
  if F <> nil then
  begin
      ms := TMemoryStream.Create;
      try
        if F is TBlobField then
          TBlobField(F).SaveToStream(ms)
        else
          VariantBytesToStream(F.AsVariant, ms);
        ms.Position := 0;
        ms.SaveToFile(SD.FileName);
      finally
        ms.Free;
      end;
  end;
{$endif}
end;

procedure TFormOdbcExplor.btBlobLoadClick(Sender: TObject);
{$ifdef _HexEditor_}
var
  F: TField;
  ms: TMemoryStream;
{$endif}
begin
{$ifdef _HexEditor_}
  if (fBinaryField = '') or (not OD.Execute) then
    Exit;
  F := CDS.FindField(fBinaryField);
  if F <> nil then
  begin
    if not DataSetToEditMode(F.DataSet, {AllowAppend=}True) then
      Exit;
    if F is TBlobField then
      TBlobField(F).LoadFromFile(OD.FileName)
    else
    begin
      ms := TMemoryStream.Create;
      try
        ms.LoadFromFile(OD.FileName);
        ms.Position := 0;
        F.AsVariant := StreamToVariantBytes(ms);
      finally
        ms.Free;
      end;
    end;

    LoadBinary();
  end;
{$endif}
end;

procedure TFormOdbcExplor.btBlobClearClick(Sender: TObject);
{$ifdef _HexEditor_}
var
  F: TField;
{$endif}
begin
{$ifdef _HexEditor_}
  if (fBinaryField = '')then
    Exit;
  F := CDS.FindField(fBinaryField);
  if F <> nil then
  begin
    if DataSetToEditMode(F.DataSet) then
    begin
      F.Clear();
      LoadBinary();
    end;
  end;
{$endif}
end;

procedure TFormOdbcExplor.btMemoSaveToFileClick(Sender: TObject);
var
  F: TField;
  ms: TMemoryStream;
begin
  if (fMemoField = '') or (not SD.Execute) then
    Exit;
  F := CDS.FindField(fMemoField);
  if F <> nil then
  begin
      ms := TMemoryStream.Create;
      try
        if F is TBlobField then
          TBlobField(F).SaveToStream(ms)
        else
          VariantBytesToStream(F.AsVariant, ms);
        ms.Position := 0;
        ms.SaveToFile(SD.FileName);
      finally
        ms.Free;
      end;
  end;
end;

procedure TFormOdbcExplor.btMemoLoadFromFileClick(Sender: TObject);
var
  F: TField;
  ms: TMemoryStream;
begin
  if (fMemoField = '') or (not OD.Execute) then
    Exit;
  F := CDS.FindField(fMemoField);
  if F <> nil then
  begin
    if not DataSetToEditMode(F.DataSet, {AllowAppend=}True) then
      Exit;
    if F is TBlobField then
      TBlobField(F).LoadFromFile(OD.FileName)
    else
    begin
      ms := TMemoryStream.Create;
      try
        ms.LoadFromFile(OD.FileName);
        ms.Position := 0;
        F.AsVariant := StreamToVariantBytes(ms);
      finally
        ms.Free;
      end;
    end;
  end;
end;

procedure TFormOdbcExplor.btMemoClearClick(Sender: TObject);
var
  F: TField;
begin
  if (fMemoField = '')then
    Exit;
  F := CDS.FindField(fMemoField);
  if F <> nil then
  begin
    if DataSetToEditMode(F.DataSet) then
      F.Clear();
  end;
end;

procedure TFormOdbcExplor.ac_Connection_ConnectExecute(Sender: TObject);
begin
  if not SQLConnection1.Connected then
    pbConnectClick(nil);
end;

procedure TFormOdbcExplor.ac_Connection_DisconnectExecute(Sender: TObject);
begin
  if SQLConnection1.Connected then
    pbConnectClick(nil);
end;

procedure TFormOdbcExplor.ac_Connection_ReConnectExecute(Sender: TObject);
begin
  pbConnectClick(nil);
  if not SQLConnection1.Connected then
    pbConnectClick(nil);
end;

procedure TFormOdbcExplor.ac_Main_SwitchSheetExecute(Sender: TObject);
var
  vNewTab: Integer;
  AllowChange: Boolean;
begin
  vNewTab := TComponent(Sender).Tag;
  TabSet1.TabIndex := vNewTab;
  if Notebook1.PageIndex <> vNewTab then
    TabSet1Change(TabSet1, vNewTab, AllowChange);
end;

procedure TFormOdbcExplor.ac_Main_Query_DataExecute(Sender: TObject);
begin
  if pcData.Enabled then
    pcData.ActivePageIndex := 0;
end;

procedure TFormOdbcExplor.ac_Main_Query_UpdateOptionsExecute(Sender: TObject);
begin
  if pcData.Enabled then
    pcData.ActivePageIndex := 1;
end;

procedure TFormOdbcExplor.ac_Main_ApplyUpdatesExecute(Sender: TObject);
begin
  if pbApplyUpdates.Enabled then
    pbApplyUpdatesClick(nil);
end;

procedure TFormOdbcExplor.ac_Main_LogChangesExecute(Sender: TObject);
begin
  if pbLogChanges.Enabled then
    pbLogChangesClick(nil);
end;

procedure TFormOdbcExplor.ac_Main_BeginTransactionExecute(Sender: TObject);
begin
  if btnBeginTransaction.Enabled then
    btnBeginTransactionClick(nil);
end;

procedure TFormOdbcExplor.ac_Main_CommitExecute(Sender: TObject);
begin
  if btnCommit.Enabled then
    btnCommitClick(nil);
end;

procedure TFormOdbcExplor.ac_Main_RollbackExecute(Sender: TObject);
begin
  if btnRollack.Enabled then
    btnRollackClick(nil);
end;

procedure TFormOdbcExplor.ac_Query_OpenExecute(Sender: TObject);
begin
  if not pbOpen.Enabled then
    Exit;
  pbOpenClick(nil);
  if not CDSQuery.Active then
    pbOpenClick(nil);
end;

procedure TFormOdbcExplor.ac_Query_ExecuteExecute(Sender: TObject);
begin
  if not pbExecute.Enabled then
    Exit;
  if CDSQuery.Active then
    pbOpenClick(nil);
  pbExecuteClick(nil);
end;

procedure TFormOdbcExplor.ac_Query_CloseExecute(Sender: TObject);
begin
  if (not pbOpen.Enabled) or (not CDSQuery.Active) then
    Exit;
  pbOpenClick(nil);
end;

procedure TFormOdbcExplor.btn_sql_monitor_clearClick(Sender: TObject);
begin
  mem_sql_monitor.Lines.Clear;
end;

procedure TFormOdbcExplor.cb_sql_monitor_activeClick(Sender: TObject);
begin
  if SQLMonitor = nil then
    Exit;
  if SQLMonitor.Active <> cb_sql_monitor_active.Checked then
    SQLMonitor.Active := cb_sql_monitor_active.Checked;
end;

//
// SQLMonitor bugs:
//
//   Delphi 2007: not tracced connect/disconnect events (adapter cleared callback info)
//

procedure TFormOdbcExplor.SQLMonitorTrace(Sender: TObject; TraceInfo: TDBXTraceInfo; var LogTrace: Boolean);
var
  iTraceFlag: Integer;
  sTraceFlag, sTraceMessage: string;
begin
  if LogTrace then
  begin
    // compact log
    if mem_sql_monitor.Lines.Count > 1500 then
    begin
      with mem_sql_monitor.Lines do
      begin
        BeginUpdate;
        while Count >1000 do
          Delete(0);
        EndUpdate;
      end;
    end;
    // append log new information
    DbxOpenOdbcCallback.DecodeTraceInfo(TraceInfo, iTraceFlag, sTraceFlag, sTraceMessage);

    mem_sql_monitor.Lines.Add('---- ---- ---- ---- ---- ---- ---- ----');
    mem_sql_monitor.Lines.Add(Format('Monitor Active: %d; TraceFlag: ( %3d: %s ); Message: %s', [
      Integer(SqlMonitor.Active), iTraceFlag, sTraceFlag,
      StringReplace(sTraceMessage, #13#10, ' ', [rfReplaceAll])]));
  end;
end;

end.
