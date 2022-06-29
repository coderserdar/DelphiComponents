unit MLRActionsUnit;

interface

uses
	ActnList, StdActns, Classes;

type
	TEditSelectAll = class(TEditAction)
  public
  	constructor Create(AOwner :TComponent); override;
  	procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TFilePrint = class(TEditAction)
  protected
  	FPrintCaption	:string;
  public
  	constructor Create(AOwner :TComponent); override;
  	procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
  	property PrintCaption	:string read FPrintCaption write FPrintCaption;
  end;

	procedure Register;

implementation

uses
	Printers, ComCtrls, Menus, Windows;

procedure Register;
begin
	RegisterActions('Edit', [TEditSelectAll], nil);
  RegisterActions('File', [TFilePrint], nil);
end;

{ TEditSelectAll }

constructor TEditSelectAll.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  Caption		:= 'Select &All';
  ShortCut  := Menus.TextToShortCut('Ctrl+A');
end;

procedure TEditSelectAll.ExecuteTarget(Target: TObject);
begin
	GetControl(Target).SelectAll;
end;

procedure TEditSelectAll.UpdateTarget(Target: TObject);
begin
	Enabled := GetControl(Target).GetTextLen > 0;
end;

{ TFilePrint }

constructor TFilePrint.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  Caption		:= '&Print';
  ShortCut  := Menus.TextToShortCut('Ctrl+P');
end;

procedure TFilePrint.ExecuteTarget(Target: TObject);
var F	:TextFile;
begin
	if (Target is TCustomRichEdit) then
  	TCustomRichEdit(Target).Print(FPrintCaption)
  else begin
  	AssignPrn(F);
    Rewrite(F);
    try
    	Writeln(F, GetControl(Target).Text);
    finally
    	CloseFile(F);
    end;
  end;
end;

procedure TFilePrint.UpdateTarget(Target: TObject);
begin
	Enabled := GetControl(Target).GetTextLen > 0;
end;

end.
