unit ShellTestUnit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ShellAPI, ShlObj, StdCtrls, NameSpaceLibrary, NameSpaceUnit, ComCtrls,
	NameSpaceTree, Menus, NameSpaceComboEdit, ExtCtrls, NameSpaceList,
	ActiveX, AxCtrls, ComObj, ImgList;

type
	TForm1 = class(TForm)
		Panel: TPanel;
		DM: TRadioGroup;
		VS: TRadioGroup;
		PopUp: TPopupMenu;
		MatchSpecification1: TMenuItem;
		Image: TImageList;
		PopUpT: TPopupMenu;
		MenuItem1: TMenuItem;
		MenuItem2: TMenuItem;
		MenuItem3: TMenuItem;
		TT: TRadioGroup;
		FT: TRadioGroup;
		NST: TNameSpaceTree;
		Splitter1: TSplitter;
		NSL: TNameSpaceList;
		ST: TStaticText;
		LoadStream: TButton;
		SaveStream: TButton;
		PopUpB: TPopupMenu;
		View1: TMenuItem;
		MTiles: TMenuItem;
		MIcons: TMenuItem;
		MList: TMenuItem;
		MDetails: TMenuItem;
		Paste1: TMenuItem;
		Button1: TButton;
		NewFolder2: TMenuItem;
		MatchSpecification2: TMenuItem;
		N1: TMenuItem;
		NSCE: TNameSpaceComboEdit;
		DSize: TRadioGroup;		
		procedure DeleteFolder1Click(Sender: TObject);
		procedure NewFolder1Click(Sender: TObject);
		procedure MatchSpecification1Click(Sender: TObject);
		procedure VSClick(Sender: TObject);
		procedure DMClick(Sender: TObject);
		procedure MenuItem1Click(Sender: TObject);
		procedure MenuItem3Click(Sender: TObject);
		procedure TTClick(Sender: TObject);
		procedure FTClick(Sender: TObject);
    procedure SaveStreamClick(Sender: TObject);
    procedure LoadStreamClick(Sender: TObject);
    procedure MTilesClick(Sender: TObject);
    procedure NSLUpdateBackGroundMenu(Sender: TObject;
      var BackGroundMenu: TPopupMenu);
    procedure Paste1Click(Sender: TObject);
		procedure Button1Click(Sender: TObject);
    procedure NewFolder2Click(Sender: TObject);
    procedure MatchSpecification2Click(Sender: TObject);
    procedure DSizeClick(Sender: TObject);
	private
		{ Private declarations }
	protected
	public
		{ Public declarations }
	end;

var
	Form1: TForm1;

implementation

uses
	Clipbrd;

{$R *.DFM}
procedure TForm1.DeleteFolder1Click(Sender: TObject);

begin
	NSL.DeleteSelect;
end;

procedure TForm1.NewFolder1Click(Sender: TObject);

begin
	NSL.MakeFolder;
end;

procedure TForm1.MatchSpecification1Click(Sender: TObject);

var
	Spec:String;

begin
	Spec:=NSL.MatchSpec;
	if InputQuery('Input File Match Specification','Match Specification',Spec) then NSL.MatchSpec:=Spec;
end;

procedure TForm1.VSClick(Sender: TObject);

begin
	NSL.ViewStyle:=TViewStyle(VS.ItemIndex);
	case NSL.ViewStyle of
		vsIcon			:	MTiles.Checked:=True;
		vsSmallIcon :	MIcons.Checked:=True;
		vsList			:	MList.Checked:=True;
		vsReport		:	MDetails.Checked:=True;
	end;	
end;

procedure TForm1.DMClick(Sender: TObject);

begin
	NSL.TypeDisplayMode:=TTypeDisplayMode(DM.ItemIndex);
	NST.TypeDisplayMode:=TTypeDisplayMode(DM.ItemIndex);
end;

procedure TForm1.MenuItem1Click(Sender: TObject);

begin
	NST.DeleteSelect;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);

begin
	NST.MakeChildForSelect;
end;

procedure TForm1.TTClick(Sender: TObject);

begin
	NSL.TypeThousand:=TTypeThousand(TT.ItemIndex);
end;

procedure TForm1.FTClick(Sender: TObject);

begin
	NSL.TypeFormatSize:=TTypeFormatSize(FT.ItemIndex);
end;

procedure TForm1.SaveStreamClick(Sender: TObject);

var
	Stream:TFileStream;

begin
	Stream:=TFileStream.Create('NameSpaceList.dat',fmCreate);
	NSL.SaveListFoldersData(Stream);
	Stream.Free;
end;

procedure TForm1.LoadStreamClick(Sender: TObject);

var
	Stream:TFileStream;

begin
	Stream:=TFileStream.Create('NameSpaceList.dat',fmOpenRead);
	NSL.LoadListFoldersData(Stream);
	Stream.Free;
end;

procedure TForm1.MTilesClick(Sender: TObject);

begin
	if Sender=MDetails then begin
		VS.ItemIndex:=3;
		VSClick(Self);
		Exit;
	end;
	if Sender=MList then begin
		VS.ItemIndex:=2;
		VSClick(Self);
		Exit;
	end;
	if Sender=MIcons then begin
		VS.ItemIndex:=1;
		VSClick(Self);
		Exit;
	end;
	if Sender=MTiles then begin
		VS.ItemIndex:=0;
		VSClick(Self);
		Exit;
	end;
end;

procedure TForm1.NSLUpdateBackGroundMenu(Sender: TObject;
	var BackGroundMenu: TPopupMenu);
begin
	Paste1.Enabled:=Clipboard.HasFormat(CF_HDROP);
end;

procedure TForm1.Paste1Click(Sender: TObject);

begin
	NSL.ExecuteCommand(scPaste);
end;

procedure TForm1.Button1Click(Sender: TObject);

begin
	NSL.SetCurrentToParent;
end;

procedure TForm1.NewFolder2Click(Sender: TObject);

begin
	NSL.MakeFolder;
end;

procedure TForm1.MatchSpecification2Click(Sender: TObject);

var
	Spec:String;

begin
	Spec:=NSL.MatchSpec;
	if InputQuery('Input File Match Specification','Match Specification',Spec) then NSL.MatchSpec:=Spec;
end;

procedure TForm1.DSizeClick(Sender: TObject);
begin
	NSL.NumberOfDecimalInSize:=TTypeNumberOfDecimal(DSize.ItemIndex);
end;

end.
 