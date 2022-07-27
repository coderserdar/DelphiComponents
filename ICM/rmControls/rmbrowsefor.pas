{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmBrowseFor
Purpose  : Dialog Wrapper for the Win32 BrowseForFolder API Call.  
Date     : 03-15-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmBrowseFor;

interface

{$I CompilerDefines.INC}

uses
   Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,Dialogs,
   ShlObj;

type
   TbfType = (bftFolders,bftPrinters,bftComputers,bftFileorFolders);
   TbfRoot = (bfrDesktop,bfrMyComputer,bfrNetworkNeighborhood,bfrPrinters);

   TrmBrowseForFolder = class(TComponent)
   private
      fFolder: string;
      fBrowseInfo: TBrowseInfo;
      fRoot: TBFRoot;
      fType: TBFType;
      fTitle: string;
   public
      function Execute: Boolean;
      constructor create(AOwner: TComponent); override;
      property Folder: string read fFolder write fFolder;
   Published
      property BrowseForType: TBFType read fType write fType default bftFolders;
      property RootNode: TBFRoot read fRoot write fRoot default bfrDeskTop;
      property Title: string read fTitle write fTitle;
   end;

implementation

function BrowseCallback(Wnd: HWND; uMsg: UINT; lParam,lpData: LPARAM): Integer stdcall;
begin
   Result := 0;
   if uMsg = BFFM_Initialized then
   begin
      with TrmBrowseForFolder(lpData) do
      begin
         if Length(Folder) > 0 then
            SendMessage(Wnd,BFFM_SetSelection,1,Longint(PChar(Folder)));
      end;
   end;
end;

{ TrmBrowseForFolder }

constructor TrmBrowseForFolder.create(AOwner: TComponent);
begin
   inherited;
   fRoot := bfrDesktop;
   fType := bftFolders;
end;

function TrmBrowseForFolder.Execute: Boolean;
var
   Buffer: array[0..MAX_PATH] of char;
   ItemIdList: PItemIDList;

begin
   Result := False;

   with fBrowseInfo do
   begin
      hwndOwner := Application.Handle;
      pszDisplayName := Buffer;
      lpszTitle := PChar(FTitle);
      lpfn := BrowseCallback;
      lParam := longint(self);

      case fRoot of
         bfrDesktop: pidlRoot := nil;
         bfrMyComputer: SHGetSpecialFolderLocation(application.handle,CSIDL_DRIVES,pidlRoot);
         bfrNetworkNeighborhood: SHGetSpecialFolderLocation(application.handle,CSIDL_NETWORK,pidlRoot);
         bfrPrinters: SHGetSpecialFolderLocation(application.handle,CSIDL_PRINTERS,pidlRoot);
      end;

      case fType of
         bftFolders: ulFlags := BIF_RETURNONLYFSDIRS;
         bftPrinters: ulFlags := BIF_BROWSEFORPRINTER;
         bftComputers: ulFlags := BIF_BROWSEFORCOMPUTER;
         bftFileorFolders: ulFlags := BIF_BROWSEINCLUDEFILES;
      end;
   end;

   ItemIdList := ShBrowseForFolder(FBrowseInfo);

   if ItemIDList = nil then
      Exit;

   Result := SHGetPathFromIDList(ItemIDList,Buffer);
   fFolder := Buffer;
end;

end.

