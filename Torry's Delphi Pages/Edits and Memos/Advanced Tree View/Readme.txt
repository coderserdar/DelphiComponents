
1. Unpack downloaded zip file into a folder.

2. In Your Delphi IDE go to menu item 

     Component \ Install Packages...

   press 'Add' button, go to the folder
   where You've unpacked AdvEdits files
   and select AdvTreeViews_Dxxxx.bpl, press 'Open'
   button on 'Add Design Package' dialog 
   window and finally press 'OK' button
   on the installed packages dialog window
or
   in Delphi Turbo Explorer open dclusr package
   and add AdvTreeViews_Reg files (dcu + dcr)
   then rebuild dclusr package.

3. Go to  Tools \ Environment Options \ Library
   and check if folder where You've unpacked files
   is included in Library path. If not then add it.


by default nonDB controls are installed on 'Win32 Additional' palette
and DB controls on 'Data Controls' palette, 
but You can change it in AdvTreeViews_Reg.pas

2012-07-19 v1.20 Release
------------------------
in TAdvDBTreeView
  - new property MultiParentAllowed default False; 
    set to True if Tree structure is separated into two phisical tables 
    NodesDefinition:  PRIMARY KEY ( ID ) , UNIQUE  ( Name )
    TreeStructure:    PRIMARY KEY ( Parent_ID, Node_ID ),
                      FOREIGN KEY  ( Node_ID )  REFERENCES  NodesDefinition  ( ID ),
                      FOREIGN KEY  ( Parent_ID )  REFERENCES  NodesDefinition  ( ID ),
    and then You can attach same Node to varies Parents


2012-05-07 v1.10 Release
------------------------
- in TAdvDBTreeView
  there was no call of MasterCahnged for detail datasources on changing record in the TreeView
  
  some optimization
  
  

2012-04-17 v1.00 Release
------------------------
  TAdvTreeView
  - new properies
    DragDropEnabled: Boolean default True;  if set to True then full drag & drop is support     
    ConfirmDelete: Boolean default True;    if set to True then dialog box appear on deleting node
    events
    OnDeletingNode: TTVChangingEvent;
    OnNodeDeleted: TNotifyEvent;
    
  TAdvDBTreeView
  - all new properties of TAdvTreeView and followng properties
  public 
    KeyValue: Variant; property for reading ID value of selected item and fast positioning on desired item
  published
    DataLinkParams: TDBTreeViewDataLinkParams;
      - published properties of TDBTreeViewDataLinkParams
                  DataSource: TDataSource;
                  DisplayField: string;
                  KeyField: string;
                  ParentField: string;
                  RootAsNode: Boolean;
                  RootValue: Variant;
                  TreeBuildMethod: TTreeBuildMethod default tbmProcessRawDataset; (tbmDatasetFilter, tbmParentFieldSort, tbmProcessRawDataset) 
                  !!! IMPORTANT  !!! for the table that takes advantage of FieldIndex and implements fast algorithms on sorted fields searching
                                     (not record by record, like most datasets descendants do) IT IS STRONGLY RECOMMENDED to set underlaying 
                                     dataset into PARENT_ID order and use tbmParentFieldSort method
                  EditingOptions: TAdvDBltvEditOpt; (eoAutoCheckKeyValue,
                                                     eoInsertEnabled, eoEditEnabled, eoDeleteEnabled)
  events
    OnPostInsertedNode: TTVChangedEvent;
    OnDragDropForeignObject: TTVDragDropForeignDBObjectEvent;