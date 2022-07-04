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

unit ukedfDCC32Opt;

interface

{##NI##}

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, Buttons, ComCtrls, ukeClasses {$IFDEF DELPHI4},
	ImgList{$ENDIF};

const

	CM_DCC32CLOSE = WM_USER + 1;

type

	TPageIndex = ( piItem, piCompiler, piLinker, piDir );
	TPageIndexes = set of TPageIndex;

	TGroupIndex = (
		gbiGenOpt, gbiGrpInfo, rgiDCCVer, rgiErrorAction,       { CompileItem Page }
		gbiCodeGen, gbiSyntaxOpt, gbiRunTimeErrors, gbiAddInfo, { Compiler Page    }
		gbiDebug, gbiMessages,
		rgiMapFile, gbiEXEDLLOpt, rgiLinkerOutPut, gbiMemSize,  { Linker Page      }
		gbiRunTimePack,
		gbiDirectories, gbiSerachPaths, gbiConditionals,        { Dir/Cond Page    }
		gbiUnitAliases );
	TGroupIndexes = set of TGroupIndex;

	TfrmDCC32ItemOpt = class( TForm )
    pnButtons: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
		HelpButton: TButton;
    pnPgCtrl: TPanel;
		pgOpts: TPageControl;
    CompilerPage: TTabSheet;
		gbMessages: TGroupBox;
		cbShowHints: TCheckBox;
		cbShowWarnings: TCheckBox;
		gbDebug: TGroupBox;
		cbDebugInfo: TCheckBox;
		cbLocalSymbols: TCheckBox;
		cbSymbolInfo: TCheckBox;
		cbAssertions: TCheckBox;
		gbSyntaxOpt: TGroupBox;
		cbVarStrCheck: TCheckBox;
		cbFullBoolEval: TCheckBox;
		cbExtSyntax: TCheckBox;
		cbTypedPointers: TCheckBox;
		cbOpenParameters: TCheckBox;
		cbHugeStrings: TCheckBox;
		cbAssignConst: TCheckBox;
		gbRunTimeErrors: TGroupBox;
		cbRangeCheck: TCheckBox;
		cbInOutCheck: TCheckBox;
		cbOverflowCheck: TCheckBox;
		gbCodeGen: TGroupBox;
		cbOptimization: TCheckBox;
		cbRecordAlign: TCheckBox;
		cbStackFrames: TCheckBox;
		cbPentiumSafe: TCheckBox;
		LinkerPage: TTabSheet;
		gbMemSize: TGroupBox;
		lbMinStack: TLabel;
		lbImgBase: TLabel;
		lbMaxStack: TLabel;
		edMinStackSize: TEdit;
		edImageBase: TEdit;
		edMaxStackSize: TEdit;
		gbEXEDLLOpt: TGroupBox;
		cbExtDebug: TCheckBox;
		cbConsoleApp: TCheckBox;
		DirCondPage: TTabSheet;
		gbConditionals: TGroupBox;
		gbDirectories: TGroupBox;
		lbEXEOutDir: TLabel;
		lbDCUOutDir: TLabel;
		gbUnitAliases: TGroupBox;
		CompileItemPage: TTabSheet;
		gbGenOpt: TGroupBox;
		lbCompFileName: TLabel;
		edCompFileName: TEdit;
		cbImplicitBuild: TCheckBox;
		cbQuiet: TCheckBox;
		cbImportedData: TCheckBox;
		cbTypeInfo: TCheckBox;
		gbAddInfo: TGroupBox;
		cbEnabled: TCheckBox;
		cbHasStatistics: TCheckBox;
		cbBuildAll: TCheckBox;
		rgDCCVer: TRadioGroup;
		btCompFileName: TButton;
		rgErrorAction: TRadioGroup;
		gbGrpInfo: TGroupBox;
    cbGrpIdx: TComboBox;
		lbGrpIdx: TLabel;
		lvGrpMembers: TListView;
		Label9: TLabel;
		gbRunTimePack: TGroupBox;
		edEditPackages: TEdit;
		btModifyRuntimePack: TButton;
    cbBuildWithRuntimePackages: TCheckBox;
		edEXEOutDir: TEdit;
		btEXEOutDir: TButton;
		edDCUOutDir: TEdit;
		btDCUOutDir: TButton;
		lbInitDir: TLabel;
		edInitDir: TEdit;
		btInitDir: TButton;
		edConditionals: TEdit;
		edUnitAliases: TEdit;
		lbTargetExt: TLabel;
		edTargetExt: TEdit;
		gbSerachPaths: TGroupBox;
		lbSource: TLabel;
		edSource: TEdit;
		btSource: TButton;
		lbInc: TLabel;
		edRes: TEdit;
		btRes: TButton;
		lbRes: TLabel;
		edInc: TEdit;
		btInc: TButton;
		lbObj: TLabel;
		edObj: TEdit;
		btObj: TButton;
		rgMapFile: TRadioGroup;
		rgLinkerOutPut: TRadioGroup;
		cbDefault: TCheckBox;
		lbCompileItemName: TLabel;
		edCompileItemName: TEdit;
    btCmdLine: TButton;
    imlbGroupMembers: TImageList;
    btParseDOF: TButton;
    cbRunTimePackage: TCheckBox;
    cbDesignTimePackage: TCheckBox;
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure btCompFileNameClick(Sender: TObject);
		procedure cbGrpIdxChange(Sender: TObject);
		procedure edMinStackSizeExit(Sender: TObject);
		procedure edMaxStackSizeExit(Sender: TObject);
		procedure edImageBaseExit(Sender: TObject);
		procedure btModifyRuntimePackClick(Sender: TObject);
		procedure cbBuildWithRuntimePackagesClick(Sender: TObject);
		procedure lvGrpMembersEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
		procedure rgDCCVerExit(Sender: TObject);
    procedure rgErrorActionClick(Sender: TObject);
		procedure cbSwitchesOptionsClick(Sender: TObject);
    procedure cbEnabledClick(Sender: TObject);
    procedure cbHasStatisticsClick(Sender: TObject);
    procedure rgMapFileClick(Sender: TObject);
    procedure rgLinkerOutPutClick(Sender: TObject);
		procedure edCompileItemNameExit(Sender: TObject);
    procedure gbMemSizeExit(Sender: TObject);
    procedure edTargetExtExit(Sender: TObject);
    procedure btDirsClick(Sender: TObject);
    procedure edCheckDirExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
		procedure HelpButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure btCmdLineClick(Sender: TObject);
    procedure edCompileItemNameChange(Sender: TObject);
		procedure btParseDOFClick(Sender: TObject);
    procedure cbRunDesignTimePackageClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
	private
    FCanceled: Boolean;
		FAppending: Boolean;
		FPrematureCancel: Boolean;
		FCompileItem: TKCompileItem;

		procedure CMDCC32Close( var Message: TMessage );
							message CM_DCC32CLOSE;
							
		function GetDCC32: TKDCC32;
		function GetSwitchCheckBox( Index: TKDCC32Switch ): TCheckBox;
		function GetOptionCheckBox( Index: TKDCC32Option ): TCheckBox;
		procedure SetCompileItem( Value: TKCompileItem );
		procedure RefreshFormValues( Pages: TPageIndexes; GroupIndexes: TGroupIndexes );
		procedure SetFormValuesToItem( Pages: TPageIndexes; GroupIndexes: TGroupIndexes );
		procedure SetCompileItemFlag( Value: LongInt; Checked: Boolean );
		procedure FillGroupMembers( GroupIndex: Cardinal );
		procedure SelectFirstPage;
		procedure SaveDefaultItem;
		procedure LoadDefaultItem;

	public
		property CompileItem: TKCompileItem
						 read FCompileItem;
		property DCC32: TKDCC32
						 read GetDCC32;
		property Appending: Boolean
						 read FAppending;
		property Canceled: Boolean
						 read FCanceled;

	end;

{##NI##}

function EditDCC32ItemOptions( CompileItem: TKCompileItem; Appending: Boolean ): Boolean;

implementation

uses
	uksyTypes, uksyConsts, uksyUtils, ukeUtils, ukedConsts;

{$R *.DFM}

const
	ALL_PAGES	= [piItem..piDir];
	ALL_GROUP = [gbiGenOpt..gbiUnitAliases];
	DCC32OPT_OFFSET = Integer( High( TKDCC32Switch ) ) + 1;

  COMPILE_ITEM_IMAGEINDEX_PROJECT = 0;
	COMPILE_ITEM_IMAGEINDEX_PACKAGE = 1;
	COMPILE_ITEM_IMAGEINDEX_UNIT    = 2;

	COMPILE_ITEM_IMAGEINDEX: array[TKCompileFileNameType] of Byte =
		( COMPILE_ITEM_IMAGEINDEX_PROJECT, COMPILE_ITEM_IMAGEINDEX_PACKAGE,
			COMPILE_ITEM_IMAGEINDEX_UNIT, COMPILE_ITEM_IMAGEINDEX_PROJECT );

	
type
	EKCompileOptions = class( EKExt );

{ Utility Functions }

function EditDCC32ItemOptions( CompileItem: TKCompileItem; Appending: Boolean ): Boolean;
var
	frmDCC32ItemOpt: TfrmDCC32ItemOpt;
begin
	ForceObject( CompileItem );
	frmDCC32ItemOpt := TfrmDCC32ItemOpt.Create( nil );
	try
		frmDCC32ItemOpt.FCompileItem := CompileItem.Owner.Add;
		try
			frmDCC32ItemOpt.FPrematureCancel := False;
			frmDCC32ItemOpt.FCanceled := False;
			frmDCC32ItemOpt.FAppending := Appending;
			frmDCC32ItemOpt.SetCompileItem( CompileItem );
			frmDCC32ItemOpt.Caption := Format( COMPILE_ITEM_FORM_CAPTION, [CompileItem.Name] );
			Result := ( frmDCC32ItemOpt.ShowModal = mrOk );
			if Result then
				CompileItem.Assign( frmDCC32ItemOpt.CompileItem );
		finally
			frmDCC32ItemOpt.FCompileItem.Free;
		end;
	finally
		frmDCC32ItemOpt.Free;
	end;
end;

function IsSwitch( Value: LongInt ): Boolean;
begin
	Result := ( Value <= TurnBitOn( 0, TKBitEnum( DCC32OPT_OFFSET - 1 ) ) ); //$00080000 );
end;

function MakeSwitch( Value: TKDCC32Switch ): LongInt;
begin
	Result := TurnBitOn( 0, TKBitEnum( Value ) );
end;

function MakeOption( Value: TKDCC32Option ): LongInt;
begin
	Result := TurnBitOn( 0, TKBitEnum( Value ) + TKBitEnum( DCC32OPT_OFFSET ) );
end;

{$WARNINGS OFF}
function ExtractSwitch( Value: LongInt ): TKDCC32Switch;
var
	i: TKBitEnum;
begin
	if ( not IsSwitch( Value ) ) then
		RaiseException( EKCompileOptions, sErrInvSwitch );
	for i := Low( TKBitEnum ) to High( TKBitEnum ) do
		if IsBitOn( Value, i ) then
			Result := TKDCC32Switch( i );
end;

function ExtractOption( Value: LongInt ): TKDCC32Option;
var
	i: TKBitEnum;
begin
	if IsSwitch( Value ) then
		RaiseException( EKCompileOptions, sErrInvOption );
	for i := DCC32OPT_OFFSET to High( TKBitEnum ) do
		if IsBitOn( Value, i ) then
			Result := TKDCC32Option( i - DCC32OPT_OFFSET );
end;
{$WARNINGS ON}

(*
function ExtractSet( Value: LongInt ): LongInt;
begin
	if IsSwitch( Value ) then
		Result := Value
	else
{ The first 20 bits are used for TKDCC32Switch, the other 7 bits are used for Options
	so, to extract the last seven bits shiht-right 20 bits ($14) }
		Result := ( Value shr DCC32OPT_OFFSET );
end;
*)
{ Form Methods }

function TfrmDCC32ItemOpt.GetSwitchCheckBox( Index: TKDCC32Switch ): TCheckBox;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to ComponentCount - 1 do
		if CheckObjectClass( Components[i], TCheckBox ) and
			 ( Components[i].Tag >= 0 ) and
			 ( Components[i].Tag = MakeSwitch( Index ) ) then
		begin
			Result := ( Components[i] as TCheckBox );
			Break;
		end;
	ForceObject( Result );
end;

function TfrmDCC32ItemOpt.GetOptionCheckBox( Index: TKDCC32Option ): TCheckBox;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to ComponentCount - 1 do
		if CheckObjectClass( Components[i], TCheckBox ) and
       ( Components[i].Tag >= 0 ) and
			 ( Components[i].Tag = MakeOption( Index ) ) then
		begin
			Result := ( Components[i] as TCheckBox );
			Break;
		end;
	ForceObject( Result );
end;

procedure TfrmDCC32ItemOpt.SetFormValuesToItem( Pages: TPageIndexes;
	GroupIndexes: TGroupIndexes );
var
	sOpts: TKDCC32Options;
	sSwitches: TKDCC32Switches;
	i: TGroupIndex;
	j: TKDCC32Switch;
	k: TKDCC32Option;
	ss: TStrings;
begin
	ss := TStringList.Create;
	try
		for i := Low( TGroupIndex ) to High( TGroupIndex ) do
			if ( i in GroupIndexes ) then
				case i of
					gbiGenOpt 		 : FCompileItem.CompileFileName := edCompFileName.Text;
					gbiGrpInfo 	   : FCompileItem.GroupIndex := cbGrpIdx.ItemIndex;
					rgiDCCVer      : FCompileItem.DCC32Version := TKDCC32Version( rgDCCVer.ItemIndex );
					rgiErrorAction : FCompileItem.ErrorAction := TKDCC32CompileErrorAction( rgErrorAction.ItemIndex );
					rgiLinkerOutPut: FCompileItem.DCC32CompiledFilesType := TKDCC32CompiledFilesType ( rgLinkerOutPut.ItemIndex );
					rgiMapFile		 : FCompileItem.DCC32MapFile := TKDCC32MapFile( rgMapFile.ItemIndex );

					gbiRunTimePack :
					begin
						ss.Text := edEditPackages.Text;
						FCompileItem.RunTimePackages := ss;
						ss.Clear;
					end;

					gbiCodeGen,
					gbiSyntaxOpt,
					gbiRunTimeErrors,
					gbiDebug:
					begin
            GroupIndexes := GroupIndexes - [gbiCodeGen, gbiSyntaxOpt,
							gbiRunTimeErrors, gbiDebug];
						sSwitches := [];
						for j := Low( TKDCC32Switch ) to High( TKDCC32Switch ) do
							if GetSwitchCheckBox( j ).Checked then
								Include( sSwitches, j );
						FCompileItem.DCC32Switches := sSwitches;
					end;

					gbiAddInfo,
					gbiMessages,
					gbiEXEDLLOpt:
					begin
 					  GroupIndexes := GroupIndexes - [gbiAddInfo, gbiMessages, gbiEXEDLLOpt];
						sOpts := [];
						for k := Low( TKDCC32Option ) to High( TKDCC32Option ) do
							if GetOptionCheckBox( k ).Checked then
								Include( sOpts, k );
            with FCompileItem do
						begin
							DCC32Options := sOpts;
							Enabled := cbEnabled.Checked;
							HasStatistics := cbHasStatistics.Checked;
							PackageEnvironments := [];
							if cbRunTimePackage.Checked then
								PackageEnvironments := PackageEnvironments + [peRunTime];
							if cbDesignTimePackage.Checked then
								PackageEnvironments := PackageEnvironments + [peDesignTime];
						end;
					end;

					gbiMemSize:
					begin
						FCompileItem.MinStackSize := HexToInt( edMinStackSize.Text );
						FCompileItem.MaxStackSize := HexToInt( edMaxStackSize.Text );
						FCompileItem.ImageBaseAddr := HexToInt( edImageBase.Text );
					end;

					gbiDirectories:
					begin
						FCompileItem.InitialDirectory := edInitDir.Text;
						FCompileItem.EXEOutPutDir := edEXEOutDir.Text;
						FCompileItem.DCUOutPutDir := edDCUOutDir.Text;
						FCompileItem.TargetExtension := edTargetExt.Text;
					end;

					gbiSerachPaths:
					begin
						ss.Text := edSource.Text;
						FCompileItem.UnitPaths := ss;
						ss.Clear;
						ss.Text := edRes.Text;
						FCompileItem.ResourcePaths := ss;
						ss.Clear;
						ss.Text := edInc.Text;
						FCompileItem.IncludePaths := ss;
						ss.Clear;
						ss.Text := edObj.Text;
						FCompileItem.ObjectPaths := ss;
						ss.Clear;
					end;

					gbiConditionals:
					begin
						ss.Text := edConditionals.Text;
						FCompileItem.SymbolDefines := ss;
						ss.Clear;
					end;

					gbiUnitAliases:
					begin
						ss.Text := edUnitAliases.Text;
						FCompileItem.UnitAliases := ss;
						ss.Clear;
					end;

				end;
	finally
	  ss.Free;
	end;
end;

procedure TfrmDCC32ItemOpt.RefreshFormValues( Pages: TPageIndexes;
	GroupIndexes: TGroupIndexes );
var
	i: TGroupIndex;
	j: TKDCC32Switch;
	k: TKDCC32Option;
	cb: TCheckBox;
begin
	for i := Low( TGroupIndex ) to High( TGroupIndex ) do
		if ( i in GroupIndexes ) then
			case i of
				gbiGenOpt:
				begin
					edCompFileName.Text := FCompileItem.CompileFileName;
					edCompileItemName.Text := FCompileItem.Name;
				end;

				gbiGrpInfo	   :
				begin
					cbGrpIdx.ItemIndex := FCompileItem.GroupIndex;
					FillGroupMembers( FCompileItem.GroupIndex );
				end;
				
				rgiDCCVer      : rgDCCVer.ItemIndex := Integer( FCompileItem.DCC32Version );
				rgiErrorAction : rgErrorAction.ItemIndex := Integer( FCompileItem.ErrorAction );
				rgiLinkerOutPut: rgLinkerOutPut.ItemIndex := Integer( FCompileItem.DCC32CompiledFilesType );
				rgiMapFile     : rgMapFile.ItemIndex := Integer( FCompileItem.DCC32MapFile );

				gbiRunTimePack :
				begin
					cbBuildWithRuntimePackages.Checked := CheckStrings( FCompileItem.RunTimePackages );
					cbBuildWithRuntimePackagesClick( cbBuildWithRuntimePackages );
					if cbBuildWithRuntimePackages.Checked then
						edEditPackages.Text := StringReplace(
							FCompileItem.RunTimePackages.Text, CH_CRLF, CH_LIST_TOKEN, krfAll );
				end;

				gbiCodeGen,
				gbiSyntaxOpt,
				gbiRunTimeErrors,
				gbiDebug:
				begin
					for j := Low( TKDCC32Switch ) to High( TKDCC32Switch ) do
					begin
						cb := GetSwitchCheckBox( j );
						cb.OnClick := nil;
						try
							cb.Checked := ( j in FCompileItem.DCC32Switches );
						finally
							cb.OnClick := cbSwitchesOptionsClick;
						end;
					end;
					GroupIndexes := GroupIndexes - [gbiCodeGen, gbiSyntaxOpt,
						gbiRunTimeErrors, gbiDebug];
				end;

				gbiAddInfo,
				gbiMessages,
				gbiEXEDLLOpt:
				begin
					for k := Low( TKDCC32Option ) to High( TKDCC32Option ) do
					begin
						cb := GetOptionCheckBox( k );
						cb.OnClick := nil;
						try
							cb.Checked := ( k in FCompileItem.DCC32Options );
						finally
							cb.OnClick := cbSwitchesOptionsClick;
						end;
					end;
					with FCompileItem do
					begin
						cbEnabled.Checked := Enabled;
						cbHasStatistics.Checked := HasStatistics;
						cbRunTimePackage.Checked := ( peRunTime in PackageEnvironments );
						cbDesignTimePackage.Checked := ( peDesignTime in PackageEnvironments );
					end;
					GroupIndexes := GroupIndexes - [gbiAddInfo, gbiMessages, gbiEXEDLLOpt];
				end;

				gbiMemSize:
				begin
					edMinStackSize.Text := IntToHex( FCompileItem.MinStackSize, BITS_PER_BYTE );
					edMaxStackSize.Text := IntToHex( FCompileItem.MaxStackSize, BITS_PER_BYTE );
					edImageBase.Text := IntToHex( FCompileItem.ImageBaseAddr, BITS_PER_BYTE );
				end;

				gbiDirectories:
				begin
					edInitDir.Text := FCompileItem.InitialDirectory;
					edEXEOutDir.Text := FCompileItem.EXEOutPutDir;
					edDCUOutDir.Text := FCompileItem.DCUOutPutDir;
					edTargetExt.Text := FCompileItem.TargetExtension;
				end;

				gbiSerachPaths:
				begin
					edSource.Text := StringReplace( FCompileItem.UnitPaths.Text, CH_CRLF,
						CH_LIST_TOKEN, krfAll );
					edRes.Text := StringReplace( FCompileItem.ResourcePaths.Text, CH_CRLF,
						CH_LIST_TOKEN, krfAll );
					edInc.Text := StringReplace( FCompileItem.IncludePaths.Text, CH_CRLF,
						CH_LIST_TOKEN, krfAll );
					edObj.Text := StringReplace( FCompileItem.ObjectPaths.Text, CH_CRLF,
						CH_LIST_TOKEN, krfAll );
				end;

				gbiConditionals:
					edConditionals.Text := StringReplace( FCompileItem.SymbolDefines.Text, CH_CRLF,
						CH_LIST_TOKEN, krfAll );

				gbiUnitAliases:
					edUnitAliases.Text := StringReplace( FCompileItem.UnitAliases.Text, CH_CRLF,
						CH_LIST_TOKEN, krfAll );

			end;
end;

function TfrmDCC32ItemOpt.GetDCC32: TKDCC32;
begin
  Result := FCompileItem.Owner.DCC32Comp;
end;

procedure TfrmDCC32ItemOpt.SetCompileItem( Value: TKCompileItem );
begin
	if Appending then
		LoadDefaultItem
	else
	{ After Load the Default item, Load the selected Item }
  	FCompileItem.Assign( Value );
	RefreshFormValues( ALL_PAGES, ALL_GROUP );
	btParseDOF.Enabled := ( DCC32.CompileItems.GetCompileFileNameType( FCompileItem ) = cfntProject );
	cbDefault.Enabled := CheckTrimStr( FCompileItem.Name ) and CheckFile( FCompileItem.CompileFileName );
  OkButton.Enabled := CheckTrimStrs( [FCompileItem.Name, FCompileItem.CompileFileName] );	
end;

procedure TfrmDCC32ItemOpt.SetCompileItemFlag( Value: LongInt; Checked: Boolean );
begin
	if IsSwitch( Value ) then
	begin
		if Checked then
			FCompileItem.DCC32Switches := FCompileItem.DCC32Switches + [ExtractSwitch( Value )]
		else
			FCompileItem.DCC32Switches := FCompileItem.DCC32Switches - [ExtractSwitch( Value )];
		RefreshFormValues( [piCompiler], [gbiCodeGen, gbiSyntaxOpt, gbiRunTimeErrors,
			gbiDebug] );
	end
	else
	begin
		if Checked then
			FCompileItem.DCC32Options := FCompileItem.DCC32Options + [ExtractOption( Value )]
		else
			FCompileItem.DCC32Options := FCompileItem.DCC32Options - [ExtractOption( Value )];
		RefreshFormValues( [piCompiler, piLinker], [gbiAddInfo, gbiMessages, gbiEXEDLLOpt] );
	end;
end;

procedure TfrmDCC32ItemOpt.FillGroupMembers( GroupIndex: Cardinal );
var
	i: Integer;
	cfnt: TKCompileFileNameType;
begin
	lvGrpMembers.Items.BeginUpdate;
	try
		lvGrpMembers.Items.Clear;
		FCompileItem.GroupIndex := GroupIndex;
		for i := 0 to DCC32.CompileItems.Count - 1 do
			if ( ( DCC32.CompileItems.Items[i] <> FCompileItem ) and
				 ( DCC32.CompileItems.Items[i].GroupIndex = GroupIndex ) ) then
				with lvGrpMembers.Items.Add do
				begin
					Caption := DCC32.CompileItems.Items[i].Name;
					cfnt := DCC32.CompileItems.GetCompileFileNameType( DCC32.CompileItems.Items[i] );
					ImageIndex := COMPILE_ITEM_IMAGEINDEX[cfnt];
					StateIndex := COMPILE_ITEM_IMAGEINDEX[cfnt];
					SubItems.Add( DCC32.CompileItems.Items[i].CompileFileName );
				end;
	finally
		lvGrpMembers.Items.EndUpdate;
	end;
end;

procedure TfrmDCC32ItemOpt.SelectFirstPage;
begin
	pgOpts.ActivePage := CompileItemPage;
	edCompileItemName.SetFocus;
end;

procedure TfrmDCC32ItemOpt.SaveDefaultItem;
begin
	try
		FCompileItem.SaveToFile( CurrentDelphiRootDir + '\Bin\' + COMPILE_ITEM_DEF_NAME );
	except
		on E: Exception do
			ShowErrorFmt( sErrInvSaveDefItem, [FCompileItem.Name, E.Message] );
	end;
end;

procedure TfrmDCC32ItemOpt.LoadDefaultItem;
var
	sDefItem: string;
begin
	sDefItem := CurrentDelphiRootDir + '\Bin\' + COMPILE_ITEM_DEF_NAME;
	if CheckFile( sDefItem ) then
		try
			FCompileItem.LoadFromFile( sDefItem );
			FCompileItem.Name := '';
			FCompileItem.CompileFileName := '';
		except
			on E: Exception do
			begin
				DeleteFile( sDefItem );
				ShowErrorFmt( sErrInvLoadDefItem, [E.Message, sDefItem] );
			end;
		end;
end;

procedure TfrmDCC32ItemOpt.CMDCC32Close( var Message: TMessage );
begin
	inherited;
	ModalResult := Message.WParam;
end;

{-------------- Form Events -------------- }

{ Main Events }

procedure TfrmDCC32ItemOpt.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	if ( not Canceled ) then
		SetFormValuesToItem( ALL_PAGES, ALL_GROUP );
	cbDefault.Checked := CheckTrimStr( FCompileItem.Name ) and CheckFile( FCompileItem.CompileFileName );	
	if ( cbDefault.Checked ) and ( not Canceled ) then
		SaveDefaultItem;
end;

procedure TfrmDCC32ItemOpt.FormShow(Sender: TObject);
begin
	SelectFirstPage;
	edCompileItemName.SetFocus;
	if Appending then
		btCompFileNameClick( nil );
	Application.ProcessMessages;
end;

{ Page Control Events }

{ CompileItem Page }

procedure TfrmDCC32ItemOpt.btCompFileNameClick(Sender: TObject);
begin
	FCompileItem.CompileFileName := SelectLoadFiles( sCompFNFilter, sCompFNLoad,
		ApplicationPath, DELPHI_PROJECT_EXT, [ofHideReadOnly, ofPathMustExist,
		ofFileMustExist], nil );
	if ( Appending and not CheckTrimStr( FCompileItem.CompileFileName ) ) then
	begin
		FCanceled := True;
		FPrematureCancel := True;
		Exit;
	end;
	RefreshFormValues( [piItem, piDir], [gbiGenOpt, gbiDirectories] );
	btParseDOF.Enabled := ( DCC32.CompileItems.GetCompileFileNameType( FCompileItem ) = cfntProject );
	cbDefault.Enabled := CheckTrimStr( edCompileItemName.Text ) and CheckFile( edCompFileName.Text );
	OkButton.Enabled := CheckTrimStrs( [FCompileItem.Name, FCompileItem.CompileFileName] );  
end;

procedure TfrmDCC32ItemOpt.edCompileItemNameExit(Sender: TObject);
begin
	try
		FCompileItem.Name := edCompileItemName.Text;
	except
		edCompileItemName.SelectAll;
		edCompileItemName.SetFocus;
		raise;
	end;
	OkButton.Enabled := CheckTrimStrs( [FCompileItem.Name, FCompileItem.CompileFileName] );
end;

procedure TfrmDCC32ItemOpt.cbGrpIdxChange(Sender: TObject);
begin
	FillGroupMembers( cbGrpIdx.ItemIndex );
end;

procedure TfrmDCC32ItemOpt.lvGrpMembersEditing(Sender: TObject; Item: TListItem;
	var AllowEdit: Boolean);
begin
	AllowEdit := False;
end;

procedure TfrmDCC32ItemOpt.rgDCCVerExit(Sender: TObject);
begin
	FCompileItem.DCC32Version := TKDCC32Version( rgDCCVer.ItemIndex );
end;

procedure TfrmDCC32ItemOpt.rgErrorActionClick(Sender: TObject);
begin
	FCompileItem.ErrorAction := TKDCC32CompileErrorAction( rgErrorAction.ItemIndex );
end;

{ Compiler Page }

procedure TfrmDCC32ItemOpt.cbSwitchesOptionsClick(Sender: TObject);
begin
	with ( Sender as TCheckBox ) do
	  SetCompileItemFlag( Tag, Checked );
end;

procedure TfrmDCC32ItemOpt.cbEnabledClick(Sender: TObject);
begin
	FCompileItem.Enabled := cbEnabled.Checked;
end;

procedure TfrmDCC32ItemOpt.cbHasStatisticsClick(Sender: TObject);
begin
	FCompileItem.HasStatistics := cbHasStatistics.Checked;
end;

{ Linker Page }

procedure TfrmDCC32ItemOpt.rgMapFileClick(Sender: TObject);
begin
	FCompileItem.DCC32MapFile := TKDCC32MapFile( rgMapFile.ItemIndex );
end;

procedure TfrmDCC32ItemOpt.rgLinkerOutPutClick(Sender: TObject);
begin
	FCompileItem.DCC32CompiledFilesType := TKDCC32CompiledFilesType( rgLinkerOutPut.ItemIndex );
end;

procedure TfrmDCC32ItemOpt.edMinStackSizeExit(Sender: TObject);
begin
	FCompileItem.MinStackSize := HexToInt( edMinStackSize.Text );
end;

procedure TfrmDCC32ItemOpt.edMaxStackSizeExit(Sender: TObject);
begin
	FCompileItem.MaxStackSize := HexToInt( edMaxStackSize.Text );
end;

procedure TfrmDCC32ItemOpt.edImageBaseExit(Sender: TObject);
begin
	FCompileItem.ImageBaseAddr := HexToInt( edImageBase.Text );
end;

procedure TfrmDCC32ItemOpt.gbMemSizeExit(Sender: TObject);
begin
	RefreshFormValues( [piLinker], [gbiMemSize] );
end;

procedure TfrmDCC32ItemOpt.btModifyRuntimePackClick(Sender: TObject);
var
	sFileName: string;
begin
	sFileName := SelectLoadFiles( sRTPFilter, sRTPTitle, ApplicationPath,
		DELPHI_PACKAGE_EXT, [ofHideReadOnly, ofPathMustExist, ofFileMustExist], nil );
	if CheckTrimStr( sFileName ) then
		edEditPackages.Text := edEditPackages.Text + CH_LIST_TOKEN + sFileName;
end;

procedure TfrmDCC32ItemOpt.cbRunDesignTimePackageClick(Sender: TObject);
const
	CB_DTP = -3;
	CB_RTP = -2;
	PACKAGE_ENVIRON_TYPES: array[Boolean] of TKPackageEnvironment =
	  ( peRunTime, peDesignTime );
begin
	with ( Sender as TCheckBox ) do
		if Checked then
			FCompileItem.PackageEnvironments := FCompileItem.PackageEnvironments +
				[PACKAGE_ENVIRON_TYPES[( Tag = CB_DTP )]]
		else
			FCompileItem.PackageEnvironments := FCompileItem.PackageEnvironments -
				[PACKAGE_ENVIRON_TYPES[( Tag = CB_DTP )]];
end;

procedure TfrmDCC32ItemOpt.cbBuildWithRuntimePackagesClick(Sender: TObject);
begin
	edEditPackages.Enabled := cbBuildWithRuntimePackages.Checked;
	btModifyRuntimePack.Enabled := cbBuildWithRuntimePackages.Checked;
end;

{ Dir/Cond Page }

procedure TfrmDCC32ItemOpt.edTargetExtExit(Sender: TObject);
begin
	if CheckTrimStr( edTargetExt.Text ) and ( edTargetExt.Text[1] <> CH_DOTMARK ) then
		edTargetExt.Text := CH_DOTMARK + edTargetExt.Text;
	FCompileItem.TargetExtension := edTargetExt.Text;
end;

procedure TfrmDCC32ItemOpt.btDirsClick(Sender: TObject);
var
	sDir: string;
begin
	if ShellBrowseFolder( sBrowseDir, '', sDir ) then
	begin
		if ( AnsiLastChar( sDir ) = '\' ) then
			Delete( sDir, Length( sDir ), 1 );
		case ( Sender as TButton ).Tag of
			EDT_INITDIR: edInitDir.Text := sDir;
			EDT_EXEDIR : edEXEOutDir.Text := sDir;
			EDT_DCUDIR : edDCUOutDir.Text := sDir;
			EDT_UNITDIR: edSource.Text := edSource.Text + CH_LIST_TOKEN + sDir;
			EDT_RESDIR : edRes.Text := edRes.Text + CH_LIST_TOKEN + sDir;
			EDT_INCDIR : edInc.Text := edInc.Text + CH_LIST_TOKEN + sDir;
			EDT_OBJDIR : edObj.Text := edObj.Text + CH_LIST_TOKEN + sDir;
		end;
	end;
end;

procedure TfrmDCC32ItemOpt.edCheckDirExit(Sender: TObject);
begin
	try
		ForcePath( ( Sender as TEdit ).Text );
	except
		( Sender as TEdit ).SelectAll;
		( Sender as TEdit ).SetFocus;
		raise;
	end;
end;

{ Panel Button Events }

procedure TfrmDCC32ItemOpt.HelpButtonClick(Sender: TObject);
begin
	NotYetImplemented;
end;

procedure TfrmDCC32ItemOpt.CancelButtonClick(Sender: TObject);
begin
	FCanceled := ConfirmFmt( sDiscardChanges, [FCompileItem.Name] );
	if FCanceled then
		ModalResult := mrCancel;
end;

procedure TfrmDCC32ItemOpt.btCmdLineClick(Sender: TObject);
begin
  Inform( FCompileItem.CompilerCommandLine );
end;

procedure TfrmDCC32ItemOpt.edCompileItemNameChange(Sender: TObject);
begin
	Caption := Format( COMPILE_ITEM_FORM_CAPTION, [edCompileItemName.Text] );
	OkButton.Enabled := CheckTrimStrs( [FCompileItem.Name, FCompileItem.CompileFileName] );
end;

procedure TfrmDCC32ItemOpt.btParseDOFClick(Sender: TObject);
begin
	{ Parse the .dof file for project compile items! }
	if ( not ( FAppending or ConfirmFmt( sDCC32DOFParseReplace, [FCompileItem.Name] ) ) ) then
		Exit;
	Screen.Cursor := crHourGlass;
	try
		ParseDOFFileToCompileItem( FCompileItem );
		RefreshFormValues( ALL_PAGES, ALL_GROUP );
		FAppending := False;
	finally
		Screen.Cursor := crDefault;
	end;
	InformFmt( sDCC32DofParseSucessful, [FCompileItem.Name] );
	if edCompileItemName.CanFocus then
		edCompileItemName.SetFocus;
end;

procedure TfrmDCC32ItemOpt.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
	CanClose := FPrematureCancel or ( ModalResult in [mrCancel, mrOk] );
	if ( FCanceled or ( ModalResult = mrOk ) ) then
		Exit;
	CanClose := ConfirmFmt( sDiscardChanges, [FCompileItem.Name] );
	FCanceled := CanClose;
end;

procedure TfrmDCC32ItemOpt.FormActivate(Sender: TObject);
begin
	if FPrematureCancel then
		PostMessage( Handle, CM_DCC32CLOSE, mrCancel, 0 );
end;

end.
