unit scutmain;

{
DELPHI RAS COMPONENT - Testing Common Shell Subroutines
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright 2002, Magenta Systems Ltd
Last updated: 19 Aug 2010

16th May 2002 - Release 4.80 - Baseline
19 Aug 2010 - Removed cast warning for Delphi 2009 and later,

}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, magsubs2, Spin ;

type
  TForm1 = class(TForm)
    ListFolders: TListBox;
    Label1: TLabel;
    ListItems: TListBox;
    doClose: TButton;
    Label2: TLabel;
    Label3: TLabel;
    doAddScut: TButton;
    Label4: TLabel;
    FolderId: TSpinEdit;
    doFolderId: TButton;
    procedure ListFoldersClick(Sender: TObject);
    procedure doCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doAddScutClick(Sender: TObject);
    procedure ListItemsDblClick(Sender: TObject);
    procedure doFolderIdClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FolderList: array [0..2] of integer = (CSIDL_CONNECTIONS, CSIDL_PRINTERS, CSIDL_CONTROLS) ;

implementation

{$R *.dfm}

procedure TForm1.doCloseClick(Sender: TObject);
begin
    Close ;
end;

procedure TForm1.ListFoldersClick(Sender: TObject);
var
    err: string ;
begin
    ListItems.Items.Clear ;
    Label2.Caption := '' ;
    Label3.Caption := '' ;
    if NOT SCutSpecLink (FolderList [ListFolders.ItemIndex], ListItems.Items, '', err) then
    begin
        Label2.Caption := 'Can Not Find Specified Folder, Err=' + err ;
        exit ;
    end ;
    Label2.Caption := 'Double Click Item for Shortcut' ;
end;

procedure TForm1.ListItemsDblClick(Sender: TObject);
var
    err: string ;
begin
    Label3.Caption := '' ;
    if ListItems.ItemIndex < 0 then exit ;
    if NOT SCutSpecLink (FolderList [ListFolders.ItemIndex], ListItems.Items,
                                    ListItems.Items [ListItems.ItemIndex], err) then
    begin
        Label3.Caption := 'Can Not Add Shortcut, Err=' + err ;
        exit ;
    end ;
    Label3.Caption := 'Added Shortcut Successfully' ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Label2.Caption := '' ;
    Label3.Caption := '' ;
end;

procedure TForm1.doAddScutClick(Sender: TObject);
var
    err: string ;
begin
    if NOT SCutAddLink ('Test Short Cuts.lnk', ParamStr (0), '-test',
                                        'Testing Short Cuts', Nil, '', 0, Err) then
    begin
        Label3.Caption := 'Can Not Add Shortcut, Err=' + err ;
        exit ;
    end ;
    Label3.Caption := 'Added Shortcut Successfully' ;
end;


procedure TForm1.doFolderIdClick(Sender: TObject);
var
    err: string ;
begin
    ListItems.Items.Clear ;
    Label2.Caption := '' ;
    Label3.Caption := '' ;
    if NOT SCutSpecLink (FolderId.Value, ListItems.Items, '', err) then
    begin
        Label2.Caption := 'Can Not Find Specified Folder, Err=' + err ;
        exit ;
    end ;
    Label2.Caption := 'Double Click Item for Shortcut' ;
end;

end.
