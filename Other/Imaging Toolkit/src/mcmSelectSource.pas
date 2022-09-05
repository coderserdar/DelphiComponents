// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  15884: mcmSelectSource.pas 
//
//   Rev 1.1    06-03-2003 11:03:58  mcm    Version: DT 2.2
// Still not complete. Missing possibility to set default source.

//
//   Rev 1.0    04-12-2001 16:49:06  mcm    Version: DT 2.0

unit mcmSelectSource;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     StdCtrls, ComCtrls, ImgList,
     TWAIN, Menus;

type
  TFormSelectSource = class(TForm)
    btnSelect  : TButton;
    btnCancel  : TButton;
    ImageList  : TImageList;
    ListView   : TListView;
    PopupMenu: TPopupMenu;
    List1: TMenuItem;
    Icon1: TMenuItem;
    Report1: TMenuItem;
    SmallIcon1: TMenuItem;
    procedure FormCreate    (Sender : TObject);
    procedure FormShow      (Sender : TObject);
    procedure btnSelectClick(Sender : TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Icon1Click(Sender: TObject);
    procedure List1Click(Sender: TObject);
    procedure Report1Click(Sender: TObject);
    procedure SmallIcon1Click(Sender: TObject);
  private
    { Private declarations }
    WasDSMOpen : boolean;
    SourceStr  : string;
    DSIdentity : TW_IDENTITY;
    FillList   : boolean;
    function InsertDSIcon : integer;
  public
    { Public declarations }
  end;

var FormSelectSource : TFormSelectSource;

implementation

{$R *.DFM}

uses mcmTWAIN, mcmTWAINIntf, mcmTWAINKernel;

type
  TTypeCastTWAINKernel = class(TmcmTWAINKernel);
  TTypeCastTWAINIntf   = class(TmcmTWAINIntf);

procedure TFormSelectSource.FormCreate(Sender : TObject);
begin
  FillList := True;
  WasDSMOpen := False;
  ListView.Items.Clear;

  ImageList.Height := 32;
  ImageList.Width  := 32;
  ImageList.ImageType := itImage;

  Left := 100;
  Top  := 140;
end; // TFormSelectSource.FormCreate.


procedure TFormSelectSource.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  if Not(WasDSMOpen)
  then begin
       if TmcmTWAIN(Owner).IsDSMOpen
       then TmcmTWAIN(Owner).CloseSourceMgr;
  end;
  Action := caHide;
end; // TFormSelectSource.FormClose.


procedure TFormSelectSource.FormShow(Sender : TObject);
var i          : integer;
    SourceName : string;
    OldCursor  : TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  if FillList
  then begin
       FillList := False;
       ListView.SortType := stNone; // Disable list sorting.

       WasDSMOpen := TmcmTWAIN(Owner).IsDSMOpen;
       if TmcmTWAIN(Owner).OpenSourceMgr
       then begin
            // Save info on current data source.
            DSIdentity := TTypeCastTWAINKernel(Owner).FSourceID;

            // Get the default data source.
            TTypeCastTWAINIntf(Owner).GetDefaultSource(SourceName);

            // Get first data source.
            if (TTypeCastTWAINIntf(Owner).GetFirstSource(SourceStr) = TWRC_SUCCESS)
            then begin
                 // Add source name and icon to list
                 ListView.Items.Add;
                 i := ListView.Items.Count - 1;
                 if (SourceStr = SourceName)
                 then ListView.Selected := ListView.Items[i];

                 ListView.Items.Item[i].Caption := SourceStr;
                 ListView.Items.Item[i].ImageIndex := InsertDSIcon;

                 // Get next data source. Iterate until end of data sources.
                 while (TTypeCastTWAINIntf(Owner).GetNextSource(SourceStr) = TWRC_SUCCESS) and
                       (Length(SourceStr) > 0)
                 do begin
                    // Add source name and icon to list
                    ListView.Items.Add;
                    i := ListView.Items.Count - 1;
                    if (SourceStr = SourceName)
                    then ListView.Selected := ListView.Items[i];

                    ListView.Items.Item[i].Caption := SourceStr;
                    ListView.Items.Item[i].ImageIndex := InsertDSIcon;
                 end;
            end;
            // Restore info on current data source.
            TTypeCastTWAINKernel(Owner).FSourceID := DSIdentity;
            ListView.SortType := stText; // Enable list sorting on source text.
            ListView.ShowColumnHeaders := False;
       end;
  end;
  // Ensure that horizontal scrollbar doesn't show.
  ListView.Columns.Items[0].Width := -2;
  Screen.Cursor := OldCursor;
end; // TFormSelectSource.FormShow.


function TFormSelectSource.InsertDSIcon : integer;
var Path  : string;
    Icon  : TIcon;
    Name  : string;
begin
  // Get file path to data source (FSourceID).
  TTypeCastTWAINKernel(Owner).GetDSPath(Path);

  // Create and get icon TWON_ICONID "962" from data source file.
  Icon := TIcon.Create;
  Icon.Handle := TTypeCastTWAINKernel(Owner).GetDSIcon(Path);
  if (Icon.Handle <> 0)
  then begin // Icon loaded sucessfully.
       Icon.Transparent := True;
       ImageList.AddIcon(Icon);
       Result := ImageList.Count - 1;
  end
  else begin // Icon did not exist.
       // Use alternative icon (Scanner, Camera or Image).
       Name := StrPas(TTypeCastTWAINKernel(Owner).FSourceID.ProductName);
       if (Pos('sample source', LowerCase(Name)) <> 0)
       then Result := 5
       else if (Pos('web', LowerCase(Name)) <> 0)
            then Result := 0
            else if (Pos('scan', LowerCase(Name)) <> 0)
                 then Result := 2
                 else Result := 1;
  end;
end; // TFormSelectSource.InsertDSIcon.


procedure TFormSelectSource.btnSelectClick(Sender : TObject);
var SourceName : string;
begin
  // Set selected data source as default data source.
  if Assigned(ListView.Selected)
  then begin
       SourceName := ListView.Selected.Caption;
       if TmcmTWAIN(Owner).IsDSMOpen
       then TTypeCastTWAINIntf(Owner).SetDefaultSource(SourceName);
       TTypeCastTWAINKernel(Owner).SetDSPath('C:\Windows\TWAIN_32\twdsrc32.ds');
  end;
  ModalResult := mrOK;
  Close;
end; // TFormSelectSource.btnSelectClick.

procedure TFormSelectSource.Icon1Click(Sender: TObject);
begin
  ListView.ViewStyle := vsIcon;
  ListView.IconOptions.Arrangement := iaLeft;
  ListView.IconOptions.WrapText := True;
end;

procedure TFormSelectSource.List1Click(Sender: TObject);
begin
  ListView.ViewStyle := vsList;
end;

procedure TFormSelectSource.Report1Click(Sender: TObject);
begin
  ListView.ViewStyle := vsReport;
end;

procedure TFormSelectSource.SmallIcon1Click(Sender: TObject);
begin
  ListView.ViewStyle := vsSmallIcon;
  ListView.IconOptions.WrapText := True;
end;

end.
