1. Introduction

Dynamic DBTreeView controls are based on Virtual TreeView control. They are using clone dataset for internal data manipulation when dynamically building tree nodes and a binary search list for detecting existing nodes. They reflect all changes of underlining dataset (record insertion, deletion, field value change) and support drag and drop operation and node caption editing. There are three controls: TDTADOTree, TDTClientTree, TDTTableTree for TCustomADODataSet, TCustomClientDataSet and TTable accordingly. If your dataset supports filtering and you can create its clone dataset, you can derive your own treeview by overriding CreateCloneDataSet method of the TDTCustomDBTreeView control.

2. Installation.

To install Dynamic DBTreeView controls, Virtual TreeView control must be installed. You can download Virtual TreeView self-installing program at www.soft-gems.net  (in this case skip paragraphs 2.2., 2.3.). Bellow X in dpk files denote Delphi version (5, 6, 7 – Delphi 5, 6, 7,
9 – Delphi 2005).

2.1. Unzip DTTreeView.zip in a directory of your choice.  

2.2. To install Virtual TreeView for Delphi 5, 6 you need to install ThemeManager. To install it Add the directory ...\ThemeManager\Source to your Delphi search path. Open in Delphi ...\ThemeManager\Delphi\thememanagerdXd.dpk and press Compile and Install buttons. 

2.3. Virtual TreeView installation.
Add the directory ...\VirtualTreeView\Source to your Delphi search path. Open in Delphi ...\VirtualTreeView\Delphi\virtualtreesdXd.dpk and press Compile and Install buttons. 

2.4. Dynamic DBTreeView installation.
Open in Delphi ...\DynamicDBTreeView\Delphi\dttreeviewdXd.dpk and press Compile and Install buttons. Add the directory ...\DynamicDBTreeView\Source to your Delphi search path.  

3. Working with Dynamic DBTreeView.

Place Dynamic DBTreeView control on the form. Set its DataSource property. Take care that DataSource Dataset is of appropriate class. Besides, your dataset must have key field, parent field and list field. Key field value must uniquely identify dataset record. Parent field must contain the key value of the parent record. The list field value will be used for drawing node text. Set properties of DBTreeFields object. Set its KeyFieldName property to the name of the key field, ParentFieldName property to the name of the parent field and ListFieldName property to the name of the list field. Set ParentOfRootValue property to the parent field value of the records that don't have parent. To specify more than one field, separate each field name with a semicolon. To allow drag and drop operation set DragMode property to dmAutomatic. To edit node caption you must set TreeOptions.MiscOptions.toEditable to True.
Dynamic DBTreeView automatically detects if the node (record) has children by going through dataset, but if dataset is big or if node has many children this operation may become very time consuming. To avoid it HaschildrenFieldName property of DBTreeFields object has been introduced. To use it, your dataset must have Boolean field whose value must be True if record has children. In this case you must yourself update it when inserting or deleting records. (See Demo2).

To set up  DBTreeView  images assign first a image list  to its Images property. To define images of individual nodes use TDTDBTreeImages object. 
HasChildrenImageIndex  property specifies which image is displayed when a node has children and is in its normal state and is not currently selected
HasChildrenSelectedIndex  property specifies which image is displayed when a node has children and is currently selected
NoChildrenImageIndex property specifies which image is displayed when a node do not have children and is in its normal state and is not currently selected
NoChildrenSelectedIndex property specifies which image is displayed when a node do not have children and is currently selected.
You can override this behavior by writing OnGetImageIndex  event handler.

To navigate in TreeView you can use the following methods: GoTop, Next, EndOfTree, GoUp, GoDown.

To save nodes individual properties in the list items use Param property of TDTNodeItem and OnDBTreeCreateNode  event handler. You can use them in OnPaintText or GetImageIndexEx handlers. See Demo3.

To use multiple columns add columns to DTDBTreeView Header.Columns object. Set their properties and set FieldName property to field name that this column must represent. In this case ListFieldName property of DBTreeFields is not necessary.

4. License.

Dynamic DBTreeView controls are subject to the Mozilla Public License Version 1.1 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at www.mozilla.org/MPL. 

Alternatively, you may redistribute this library, use and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version. You may obtain a copy of the LGPL at www.gnu.org/copyleft. 

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the specific language governing rights and limitations under the License. 

The Initial Developers of the Original Code are Andrew Yushev, Alex Troitsky, Borus software.

If you have any comments or suggestions, send them to info@table-report.com.

Another Borus software product - TableReport - fast, powerful, user-friendly reporting tool for Delphi, C++Builder. For more information visit our website - http://www.table-report.com .

Andrew Yushev 
Borus software