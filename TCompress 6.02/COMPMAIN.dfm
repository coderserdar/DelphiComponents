�
 TFORM1 0�%  TPF0TForm1Form1LeftITop� WidthHeightCaptionTCompress DemonstrationColorclGreen
Font.ColorclWindowTextFont.Height�	Font.NameSystem
Font.Style 	Icon.Data
�             �     (       @         �                        �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ���                                                                                                   �����������    Owwwwwwww�    x�wwwwww�    ��������    p      �    ��������    p      �    wwwwwwww�    ���������             �    wwwwwwwwww�    p  p pw�    p        w�    p    �   w�    p   ��   w�    www	���www�    www  � www�    www  � www�    www  � www�    www  � www�    www    www�    ����������p                                                                  ���������������������  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ������������PositionpoScreenCenterScaledOnClick	FormClickOnCreate
FormCreate	OnDestroyFormDestroyPixelsPerInch`
TextHeight TPanelPanel2Left Top WidthHeightAlignalClient
BevelOuterbvNoneColorclGreenTabOrder  TShapeShape1LeftTopBWidth�Height  TDBTextDBText1Left� TopGWidth� Height	AlignmenttaCenter	DataFieldCommon_Name
DataSourceDataSource1
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFont  TImageImage1Left� Top� Width� HeightiDragModedmAutomaticStretch	
OnDragDropCDBImage1DragDrop
OnDragOverCDBImage1DragOver  TMemoMemo1Left� Top� Width� Height CursorcrArrowTabStop	AlignmenttaCenterBorderStylebsNoneColor	clBtnFaceDragModedmAutomatic
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style Lines.StringsDrag images here to displaythem in a simple TImage 
ParentFontReadOnly	TabOrder 
OnDragDropCDBImage1DragDrop
OnDragOverCDBImage1DragOver  TMemoMemo2LeftJTop_WidthbHeight`	AlignmenttaCenter
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style Lines.Strings'BLOB Database compression demonstrationcurrently disabled... 1Set the DBDEMOS alias to point to the location of2the BIOLIFE.DB table (usually \DELPHI\DEMOS\DATA). 
ParentFontTabOrderVisible  TDBNavigatorDBNavigator1Left� Top�WidthXHeight
DataSourceDataSource1VisibleButtonsnbFirstnbPriornbNextnbLast TabOrder  TRadioGroupCMethodLeft� Top WidthfHeightYHint%Select the desired compression methodCaptionCompressionColor	clBtnFace
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 	ItemIndexItems.StringsNoneRLELZH1LZH5 ParentColor
ParentFontParentShowHintShowHint	TabOrderOnClickCMethodClick  	TGroupBox	GroupBox1LeftTop Width� HeightHint$Drag files to and from the file listCaption
Disk FilesColor	clBtnFace
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style ParentColor
ParentFontParentShowHintShowHint	TabOrderOnClickGroupBox1Click
OnDragDrop
DLDragDrop
OnDragOver
DLDragOver TFileListBoxFLLeftToppWidth� HeightqDragModedmAutomatic
ItemHeightMultiSelect	
ShowGlyphs	TabOrder OnClickFLClick
OnDragDrop
DLDragDrop
OnDragOver
DLDragOver  TDirectoryListBoxDLLeftTop+Width� HeightCFileListFL
ItemHeightTabOrder
OnDragDrop
DLDragDrop
OnDragOver
DLDragOver  TDriveComboBoxDCBLeftTopWidth� HeightDirListDLTabOrder
OnDragDrop
DLDragDrop
OnDragOver
DLDragOver  TMemoMemo3LeftTop� Width� Height!CursorcrArrowTabStop	AlignmenttaCenterBorderStylebsNoneColor	clBtnFace
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style Lines.Strings Drag these files to the archive,images or memo, & vice versa 
ParentFontReadOnly	TabOrder   	TGroupBoxArchiveGroupLeft\Top Width� HeightHint*Drag the entire archive too if you wish...CaptionArchived FilesColor	clBtnFaceDragModedmAutomatic
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style ParentColor
ParentFontParentShowHintShowHint	TabOrder
OnDragDroparchivefileDragDrop
OnDragOverarchivefileDragOver TLabelArchiveLabelLeft9TopWidth.HeightCaptionArchive:DragModedmAutomatic
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel2Left6TopFWidth7HeightCaption	Contains:
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TEditarchivefileLeftTop(Width� HeightHintEdit archive name as requiredParentShowHintShowHint	TabOrder TextC:\COMPDEMO.ARCOnChangearchivefileChange
OnDragDroparchivefileDragDrop
OnDragOverarchivefileDragOver  TListBoxListBox1LeftTopXWidth� Height� Hint/Click filename for file compression informationDragModedmAutomatic
ItemHeightMultiSelect	ParentShowHintShowHint	TabOrderOnClickListBox1Click
OnDragDroparchivefileDragDrop
OnDragOverarchivefileDragOver  TMemoMemo4LeftTop� Width� Height!CursorcrArrowTabStop	AlignmenttaCenterBorderStylebsNoneColor	clBtnFace
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style Lines.Strings"Drag these files to the file list,trashcan, images or memo 
ParentFontReadOnly	TabOrder   TDBEditFishnameLeft8TopFWidthyHeightTabStopAutoSizeBorderStylebsNoneColor	clBtnFaceCtl3D	DataFieldCommon_Name
DataSourceDataSource1
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 	MaxLength ParentCtl3D
ParentFontTabOrderVisible  TMemoMemo5Left	Top�Width� HeightCursorcrArrowTabStop	AlignmenttaCenterBorderStylebsNoneColor	clBtnFace
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style Lines.Strings%Drag this to the archive or file list 
ParentFontReadOnly	TabOrder  TMemoMemo6Left8Top�Width� HeightCursorcrArrowTabStop	AlignmenttaCenterBorderStylebsNoneColor	clBtnFace
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style Lines.Strings'Drag to the archive, file list or image 
ParentFontReadOnly	TabOrder  TButtonButton1Left�TopWidth2HeightCaptionAboutTabOrder	OnClickButton1Click  TPanelPanel1Left� TopUWidthfHeighty
BevelInnerbvRaised
BevelOuter	bvLoweredTabOrder
OnClickPanel1Click TBevelBevel1Left%TopSWidthHeightParentShowHintShowHint  TLabelTimeLeftTop:WidthIHeight	AlignmenttaCenterAutoSizeColor	clBtnFace
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style ParentColor
ParentFont  TLabel
PercentageLeftTopWidthYHeight	AlignmenttaCenterAutoSizeColor	clBtnFace
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style ParentColor
ParentFont  TLabel	TimeLabelLeftTop+WidthCHeight	AlignmenttaCenterAutoSizeCaptionTime
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TLabelLabel7LeftTopWidthYHeightCaption% Compressed
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFont  TImageTrashcanLeft&TopTWidthHeightHintDrag files here to delete themDragModedmAutomaticParentShowHintPicture.Data
�  TBitmapv  BMv      v   (                                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ���         ��� ���         ��� ��� ������� ��� ��� ������� ��� ��� � w w � ��� ��� � w w � ��� ��� � w � � ��� ��� � w � � ��� ��� � w w � ��� ��� � w w � ��� ��� � w � � ��� ��� � w � � ��� ��� � w w � ��� ��� � w w � ��� � � � w � � � � � � � w � � � � ��  � w w �  �� ��  � w w �  �� ��� � w � � ��� ��� � w � � ��� ��� � � � � ��� ��� � � � � ��� ��           �� ��           �� �� �www����� �� �� �www����� �� ��           �� ��           �� ����� ��� ����� ����� ��� ����� �����     ����� �����     ����� ShowHint	Stretch	
OnDragDropTrashcanDragDrop
OnDragOverTrashcanDragOver   TButtonButton2LeftTTopWidth2HeightCaptionHelpTabOrderOnClickButton2Click  	TCDBImage	CDBImage1Left TopbWidth� HeightqCompressionMethodcoNoneCompressSource	Compress1	DataFieldGraphic
DataSourceDataSource1DragModedmAutomaticTabOrder
OnDragDropCDBImage1DragDrop
OnDragOverCDBImage1DragOver  TCDBMemoCDBMemo1LeftTopbWidth� HeightqCompressionMethodcoNoneCompressSource	Compress1	DataFieldNotes
DataSourceDataSource1DragModedmAutomaticTabOrder
OnDragDropCDBMemo1DragDrop
OnDragOverCDBMemo1DragOver  TButtonButton3Left�TopWidth2HeightHintRegistration informationCaptionReg'nTabOrderOnClickButton3Click   TTableTable1	AfterPostTable1AfterPostDatabaseNameDBDEMOS	TableName
BIOLIFE.DBLeft3Top TFloatFieldTable1SpeciesNo	FieldName
Species No  TStringFieldTable1Category	FieldNameCategorySize  TStringFieldTable1Common_Name	FieldNameCommon_NameSize  TStringFieldTable1SpeciesName	FieldNameSpecies NameSize(  TFloatFieldTable1Lengthcm	FieldNameLength (cm)  TFloatFieldTable1Length_In	FieldName	Length_In  TCGraphicFieldCDBImage1Graphic	FieldNameGraphicBlobType	ftGraphicCompressSource	Compress1CompressionMethodcoNone  TCMemoFieldCDBMemo1Notes	FieldNameNotesBlobTypeftMemoSize2CompressSource	Compress1CompressionMethodcoNone   TDataSourceDataSource1DataSetTable1LeftSTop  	TCompress	Compress1	RegNumber OnCheckFileCompress1CheckFileOnShowProgressCompress1ShowProgressMakeDirectoriesExceptionOnFileErrorKey CheckSpaceBeforeExpandLeftTop   