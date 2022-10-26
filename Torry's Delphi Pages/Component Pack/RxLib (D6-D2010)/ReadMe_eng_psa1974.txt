RxLib Component library Delphi VCL Extensions (RX) Library, 
was developed by Fedor Kozhevnikov, Sergey Korolev and Igor 
Pavlyuk. This is free open source product that was very popular
among Delphi developers in ex-USSR and world.

Project development was stopped. Library was included in 
JEDI Visual Component Library. 
Homepage: http://homepages.borland.com/jedi/jvcl/

My own adaptation for 2009-2010. Highlights.
=====================================================================

I DO NOT GUARANTEE that EVERYTHING WAS CONVERTED!!! But...

I checked correctness of use of string types (string, Char, PCHar) in
every place where i found those types. Especially in the case of using 
them as buffers. 
These unites where changed especially hard:

1. Unit RxRichEd.pas:
  - completely reworked methods of internal class TRichEditStrings 
    that work with files/streams LoadFromFile, LoadFromStream, 
    SaveToFile, SaveToStream accordingly to new features of CG2009 
    (to support overridden methods with parameter Encoding: TEncoding)
  - Property TRxCustomRichEdit.StreamMode - for CG2009 flag smUnicode  
    was excluded from tje set of available:   
    TRichStreamMode = (smSelection, smPlainRtf, 
                       smNoObjects{$IFNDEF RX_D12}, smUnicode{$ENDIF});
    TRichStreamModes = set of TRichStreamMode;
    Herewith, appropriate mode was enabled by default for the class 
    TRichEditStrings.

2. Unit rxDbutils.pas:
  - replaced types:
    TBookmark replaced Pointer;
    TBookmarkStr replaced TBookmark;
    PChar replaced TRecordBuffer (where it had sense). 
    Syntax:
    {$IFDEF RX_D12}
      TBookmarkType = TBookmark;
      TBookmarkPointerType = Pointer;
      TBuffer = TRecordBuffer;
    {$ELSE}
      TBookmarkType = TBookmarkStr;
      TBookmarkPointerType = TBookmark;
      TBuffer = PChar;
    {$ENDIF}
    These types where replaced by TBookmarkType, TBuffer, 
	TBookmarkPointerType in sources to keep compatibility with 
	previous Delphi versions.

3. Unit RxMemDS.pas:
  - replaced types (similar to 2):
    {$IFDEF RX_D12}
      TBlobDataArray = array of TBlobData;
      TBlobDataArrayType = TBlobDataArray;
      TBlobDataType = TBlobData;
    {$ELSE}
      TMemBlobData = AnsiString;
      TMemBlobArray = array[0..0] of TMemBlobData;
      TBlobDataArrayType = ^TMemBlobArray;
      TBlobDataType = TMemBlobData;
      PMemBlobArray = ^TMemBlobArray;
    {$ENDIF}   
    
4. Unit rxCheckItm.pas:
  - fixed error in property editor Items of component CheckListBox.

5. Other changes:
	rxAppUtils.pas
	  - function StrToIniStr(const Str: string): string;
		SizeOf(Buffer) changed to Length(Buffer)
	  - function IniStrToStr(const Str: string): string;
		SizeOf(Buffer) changed to Length(Buffer)
	  - procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);
		SizeOf(CSection) changed to Length(CSection)
		SizeOf(CIdent) changed to Length(CIdent)
		SizeOf(CFileName) changed to Length(CFileName)
	rxCtrls.pas
	  - procedure TRxButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
		  TextBounds: TRect; State: TRxButtonState; Flags: Word);
		SizeOf(CString) changed to Length(CString)
	  - procedure TRxButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
		  var Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
		  PopupMark: Boolean; var GlyphPos: TPoint; var TextBounds: TRect; Flags: Word;
		  Images: TImageList; ImageIndex: Integer);
		SizeOf(CString) changed to Length(CString)
	rxCurrEdit.pas
	  - function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
		SizeOf(Buffer) changed to Length(Buffer)
	rxDateUtil.pas
	  - function FormatLongDate(Value: TDateTime): string;
		SizeOf(Buffer) changed to Length(Buffer)
	rxExcptdlg.pas
	  - procedure TRxErrorDialog.ErrorInfo(var LogicalAddress: Pointer;
		  var ModuleName: string);
		SizeOf(Temp) changed to Length(Temp)
		SizeOf(ModName) changed to Length(ModName)
	rxMenus.pas
	  - procedure RefreshMenuItem(MenuItem: TMenuItem; OwnerDraw: Boolean);
		SizeOf(CCaption) changed to Length(CCaption)
	RxRichEd.pas
	  - function TRichEditStrings.Get(Index: Integer): string;
		SizeOf(Text) changed to Length(Text)
	  - function TRxCustomRichEdit.InsertObjectDialog: Boolean;
		SizeOf(NameBuffer) changed to Length(NameBuffer)
		- procedure TRichEditStrings.Put(Index: Integer; const S: string);
        longint changed to LPARAM
	  - procedure TRichEditStrings.Insert(Index: Integer; const S: string);
		var Fmt: PChar changed to Fmt: string		
	RxShell.pas
	  - function IconExtract(const FileName: string; Id: Integer): TIcon;
		SizeOf(S) changed to Length(S)
	RxToolEdit.pas
	  - procedure TFileDirEdit.WMDropFiles(var Msg: TWMDropFiles);
		SizeOf(AFileName) changed to Length(AFileName)
	rxVCLUtils.pas
	  - function GetEnvVar(const VarName: string): string;
		SizeOf(S) changed to Length(S)
	Rxverinf.pas
	  - function AppFileName: string;
		SizeOf(FileName) changed to Length(FileName)
	RxColors.pas    
	  - function RxIdentToColor(const Ident: string; var Color: Longint): Boolean;
		SizeOf(Text) changed to Length(Text)
	rxDBRichEd.pas
	  - procedure TRxDBRichEdit.KeyPress(var Key: Char);
		Key changed to AnsiChar(BytesOf(Key)[0])
	RxLookup.pas    
	  - procedure TRxLookupControl.ProcessSearchKey(Key: Char);
		Key changed to AnsiChar(BytesOf(Key)[0])
	  - procedure TRxDBLookupCombo.KeyPress(var Key: Char);
		Key changed to AnsiChar(BytesOf(Key)[0])
	rxdbfilter.pas
	  - function TFilterExpr.PutConstStr(const Value: string): Integer;
		SizeOf(Buffer) changed to Length(Buffer)
	  - function TFilterExpr.PutFieldNode(Field: TField): Integer;
		SizeOf(Buffer) changed to Length(Buffer)
	  - procedure TExprParser.NextToken;
		SizeOf(StrBuf) changed to Length(StrBuf)
  
Result: 
- Packages compiled without errors, hints and warnings. 
- Demo projects Rxdemo, Riched2, Gifanm32 compiled and work fine.
  Other demos have been outdated too much, so I did not fix them...
- My working projects work fine.

===============================================================
Adaptation: psa1974 
Feedback:
http://forum.ru-board.com/
http://www.dumpz.ru/
