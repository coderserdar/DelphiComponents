
1. Unpack downloaded zip file into a folder.

2. In Your Delphi IDE go to menu item 

     Component \ Install Packages...

   press 'Add' button, go to the folder
   where You've unpacked DBGridTreeView files
   and select DBGridTreeView_Dxxxx.bpl, 
   press 'Open' button on 'Add Design Package' 
   dialog window and finally press 'OK' button
   on the installed packages dialog window.
or   
   in Delphi Turbo Explorer open dclusr package
   and add AdvDBGrid_Reg files (dcu + dcr)
   then rebuild dclusr package.

3. Go to  Tools \ Environment Options \ Library
   and check if folder where You've unpacked files
   is included in Library path. If not then add it.


2013-04-28 v1.12 Release
-----------------------
new version 1.12 of TTreeDataset 


2013-04-05 v1.11 Release
-----------------------
new version 1.11 of TTreeDataset 
and changes DEMO application


2013-03-30 v1.1 Release
-----------------------
new version 1.1 of TTreeDataset 
and changes DEMO application


2013-03-22 v1.0 Release
-----------------------
TDBGrid descendants for browsing tree data structures.
if assigned TDataset is not recognized as TTreeDataset
then TDBGridTreeView acts exactly as standard TDBGrid.

published properties
   OnPopupListDropDown: TSelectedColumnEvent;
   OnPopupListCloseUp: TSelectedColumnEvent;

   TTreeViewParams: TTreeViewParams;
     ButtonStyle: TDBGTVButtonStyle read FButtonStyle write SetButtonStyle
                  default bsRectangle;
     LineStyle: TDBGTVLineStyle read FLineStyle write SetLineStyle
                default lsDotted;
     TreeColumnAlign: TBGTVTreeColumnAlign read FTreeColumnAlign
                      write SetTreeColumnAlign default tcaAlignToTree;