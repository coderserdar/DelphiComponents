
Toolbar97
Version 1.78
Copyright © 1998-2004 by Jordan Russell
All rights reserved.

web site:  	http://www.jrsoftware.org/
Contents
Overview
License
Installation
Global Functions and Variables
TDock97 Reference
TToolbar97 Reference
TToolWindow97 Reference
TToolbarButton97 Reference
TToolbarSep97 Reference
TEdit97 Reference
Known Problems and Conflicts
Tips
Frequently Asked Questions (on the web)
Revision History
Overview
Toolbar97 is a free, open source dockable toolbar component set for Delphi 2.0-7.0 and C++Builder 1.0-6.0 that features the Office 97 look and behavior. Some of its features include:

Toolbars that can be dragged and docked to any side of a form, or be left floating. Multiple toolbars can lined up side-by-side or in rows.
All source code for customization if necessary.
A TToolbarButton97 control that looks and works just like the buttons in Office 97.
A TEdit97 control for creating Office 97-style edit controls on toolbars.
Ability to save and load toolbar positions from the registry.
Full compatibility with Windows 95, 98, 2000, NT 4.0, and 3.51, without requiring the new COMCTL32.DLL, unlike Delphi's TToolBar and TCoolBar components.
100% native VCL code -- no bulky OCX's.
Note that the successor to Toolbar97, Toolbar2000, is now available.

License
Toolbar97 is licensed under the terms of the zlib license.

Toolbar97
Copyright (C) 1998-2004 by Jordan Russell
http://www.jrsoftware.org/

This software is provided 'as-is', without any express or implied
warranty.  In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
Installation
IMPORTANT: When unzipping Toolbar97, make sure your unzipper is configured to recreate the directory structure (in WinZip, check Use Folder Names).

Delphi 2.0/C++Builder 1.0 installation:

Select Component | Install... on the menu bar.
A dialog entitled Install Components will appear. Click the Add... button.
On the Add Module dialog, click the Browse button, then locate TB97Reg.pas.
Delphi 2.0/C++Builder 1.0 upgrade:

Select Component | Rebuild Library on the menu bar.
Delphi 3.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path field if you have not already done so. The Library Path field should then look similar to this:

C:\Delphi3\Lib;C:\Delphi3\Bin;C:\Delphi3\Imports;c:\tb97\source

Click OK.
Select File | Open... on the menu bar. Set Files of type to Delphi package source, locate and select the TB97_d3 package source file in your Toolbar97 Source directory, and click Open.
A package editor window will appear. Click Compile, then click Install.
Delphi 4.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path if you have not already done so. The Library Path field should then look similar to this:

$(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;c:\tb97\source

Click OK.
Select File | Open... on the menu bar. Set Files of type to Delphi package source, locate and select the TB97_d4 package source file in your Toolbar97 Source directory, and click Open.
A package editor window will appear. Click Compile, then click Install.
Delphi 5.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path if you have not already done so. The Library Path field should then look similar to this:

$(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;c:\tb97\source

Click OK.
Select File | Open... on the menu bar. Set Files of type to Delphi package source, locate and select the TB97_d5 package source file in your Toolbar97 Source directory, and click Open.
A package editor window will appear. Click Compile, then click Install.
Delphi 6.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path if you have not already done so. The Library Path field should then look similar to this:

$(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;c:\tb97\source

Click OK.
Select File | Open... on the menu bar. Set Files of type to Delphi package source, locate and select the TB97_d6 package source file in your Toolbar97 Source directory, and click Open.
A package editor window will appear. Click Compile, then click Install.
Delphi 7.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path if you have not already done so. The Library Path field should then look similar to this:

$(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;c:\tb97\source

Click OK.
Select File | Open... on the menu bar. Set Files of type to Delphi package source, locate and select the TB97_d6 package source file in your Toolbar97 Source directory, and click Open.
A package editor window will appear. Click Compile, then click Install.
C++Builder 3.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path field if you have not already done so. The Library Path field should then look similar to this:

$(BCB)\LIB;$(BCB)\LIB\OBJ;c:\tb97\source

Click OK.
Select File | Open Project... on the menu bar. Locate and select the TB97_cb3 package source file in your Toolbar97 Source directory, and click Open.
Select Project | Make TB97 on the menu bar.
Select File | Close All on the menu bar.
Select Component | Install Packages... on the menu bar. Click the Add... button, locate the file TB97_cb3.bpl in your Toolbar97 Source directory, and click Open.
Click OK.
C++Builder 4.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path field if you have not already done so. The Library Path field should then look similar to this:

$(BCB)\LIB;$(BCB)\LIB\OBJ;c:\tb97\source

Click OK.
Select File | Open Project... on the menu bar. Locate and select the TB97_cb4 package source file in your Toolbar97 Source directory, and click Open.
A package editor window will appear. Click Compile, then click Install.
C++Builder 5.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path field if you have not already done so. The Library Path field should then look similar to this:

$(BCB)\Lib;$(BCB)\Bin;$(BCB)\Imports;$(BCB)\Projects\Bpl;c:\tb97\source

Click OK.
Select File | Open Project... on the menu bar. Locate and select the TB97_cb5 package source file in your Toolbar97 Source directory, and click Open.
A package editor window will appear. Click Compile, then click Install.
C++Builder 6.0 installation / upgrade:

Select Tools | Environment Options... on the menu bar. Go to Library tab and add the full path of your Toolbar97 Source directory to the Library Path field if you have not already done so. The Library Path field should then look similar to this:

$(BCB)\Lib;$(BCB)\Bin;$(BCB)\Imports;$(BCB)\Projects\Bpl;c:\tb97\source

Click OK.
Select File | Open Project... on the menu bar. Locate and select the TB97_cb6 package source file in your Toolbar97 Source directory, and click Open.
A package editor window will appear. Click Compile, then click Install.
Once you have Toolbar97 installed, you might want to take a look at the Demo project to see a demonstration of Toolbar97's capabilities.

The Toolbar97 package includes six components: TDock97, TToolbar97, TToolWindow97, TToolbarButton97, TToolbarSep97, and TEdit97. Please read the following sections for important details on each.

Global Functions and Variables
TB97.pas
Functions:

procedure IniLoadToolbarPositions (const Form: TCustomForm; const Filename, SectionNamePrefix: String);
Loads the positions of all toolbars owned by Form to the .INI file specified by Filename. This is provided for backwards compatibility - 32-bit applications should use the registry instead. This should be called when your application starts (usually in the OnCreate handler of your form). If they were not previously saved in the .INI file, IniLoadToolbarPositions has no effect. Each toolbar's data is loaded from a section whose name is the Name property of the toolbar prefixed by SectionNamePrefix.

Example:
procedure TForm1.FormCreate(Sender: TObject);
begin
  IniLoadToolbarPositions (Self, 'test.ini', '');
end;
procedure IniSaveToolbarPositions (const Form: TCustomForm; const Filename, SectionNamePrefix: String);
Saves the positions of all toolbars owned by Form to the .INI file specified by Filename. This is provided for backwards compatibility - 32-bit applications should use the registry instead. This should be called when your application exits (usually in the OnDestroy handler of your form). Each toolbar's data is saved to a section whose name is the Name property of the toolbar prefixed by SectionNamePrefix. Example:
procedure TForm1.FormDestroy(Sender: TObject);
begin
  IniSaveToolbarPositions (Self, 'test.ini', '');
end;
procedure RegLoadToolbarPositions (const Form: TCustomForm; const BaseRegistryKey: String);
Loads the positions of all toolbars owned by Form from the registry. This should be called when your application starts (usually in the OnCreate handler of your form). If they were not previously saved in the registry, RegLoadToolbarPositions has no effect. BaseRegistryKey is the name of the HKEY_CURRENT_USER subkey it loads the data from. RegLoadToolbarPositions will append the Name of the toolbars onto this. For example, if BaseRegistryKey is Software\My Company\My Program\Toolbars and the Name of a toolbar is MyToolbar, it loads the data from the Software\My Company\My Program\Toolbars\MyToolbar key.

Example:
procedure TForm1.FormCreate(Sender: TObject);
begin
  RegLoadToolbarPositions (Self, 'Software\My Company\My Program\Toolbars');
end;
procedure RegLoadToolbarPositionsEx (const Form: TCustomForm; const RootKey: HKEY; const BaseRegistryKey: String);
Same as RegLoadToolbarPositions, but adds a RootKey parameter for specifying the root key for BaseRegistryKey.

procedure RegSaveToolbarPositions (const Form: TCustomForm; const BaseRegistryKey: String);
Saves the positions of all toolbars owned by Form to the registry. This should be called when your application exits (usually in the OnDestroy handler of your form). BaseRegistryKey is the name of the HKEY_CURRENT_USER subkey it saves the data to. RegSaveToolbarPositions will append the Name of the toolbars onto this. For example, if BaseRegistryKey is Software\My Company\My Program\Toolbars and the Name of a toolbar is MyToolbar, it saves the data from the Software\My Company\My Program\Toolbars\MyToolbar key.

Example:
procedure TForm1.FormDestroy(Sender: TObject);
begin
  RegSaveToolbarPositions (Self, 'Software\My Company\My Program\Toolbars');
end;
procedure RegSaveToolbarPositionsEx (const Form: TCustomForm; const RootKey: HKEY; const BaseRegistryKey: String);
Same as RegSaveToolbarPositions, but adds a RootKey parameter for specifying the root key for BaseRegistryKey.
TB97Ctls.pas
Variables:

var ButtonsStayDown: Boolean = True;
When True (the default), TToolbarButton97 controls remain down during the execution of their OnClick handlers, similar to Office 97. When False, they pop up immediately like TSpeedButton.
TDock97 Reference
Description:

Create TDock97 controls at locations you want a TToolbar97 to be able to dock at. These automatically resize as toolbars are docked onto them. Be sure to set the Position property to designate which side of the form the dock is to be located.

Key Properties:

property AllowDrag: Boolean default True;
When True, toolbars on the dock can be dragged. But when it is False, there are several noteworthy differences: child toolbars are not draggable, the positions of child toolbars are neither loaded nor saved, and toolbars from other docks with AllowDrag set to True cannot be docked to it. Remember you are permitted to create two docks with the same Position, so you could create one dock with AllowDrag set to False and another dock with AllowDrag set to True.
property Background: TBitmap;
A background bitmap, which is optional. It is tiled across the length of the dock, and any docked toolbars used it also.
property BackgroundOnToolbars: Boolean default True;
When True, the Background bitmap "shines through" onto docked toolbars.
property BackgroundTransparent: Boolean default False;
When True, the color of the bottom-left pixel of the Background bitmap is considered transparent, and is replaced by value of the Color property.
property BoundLines: TDockBoundLines;
TDockBoundLines = set of (blTop, blBottom, blLeft, blRight);
Use this to add extra lines to the sides of the dock. For docks Positioned at the top of the form, it looks best if you set this to [blTop].
property FixAlign: Boolean default False;
If at run-time you notice a dock not appearing in the location it should, enabling this should correct the problem. This problem only occurs when you have a TDock97 and another control with the same Align setting (i.e. a dock and a list view both set to alLeft). When True, this adds an extra pixel to the width or height so that the VCL is able to align it correctly.
property LimitToOneRow: Boolean default False;
Set this to True if you want to prevent the user from having more than one row of docked toolbars. I generally don't recommend you enable this (since Office 97 doesn't do this) unless absolutely necessary. If you have fixed-size form that would look wrong with too many rows of toolbars, you should instead respond to the OnResize event of TDock97 to make your form resize itself.
property Position: TDockPosition;
TDockPosition = (dpTop, dpBottom, dpLeft, dpRight);
Determines where the dock is located on the form.
property ToolbarCount: Integer;
The number of visible toolbars currently docked.
property Toolbars[Index: Integer]: TCustomToolWindow97;
Zero-based array of all the visible toolbars that are currently docked.
Events:

property OnInsertRemoveBar: TInsertRemoveEvent;
TInsertRemoveEvent = procedure(Sender: TObject; Inserting: Boolean; Bar: TToolbar97) of object;
Occurs after a toolbar is docked (Inserting = True) or undocked (Inserting = False).
property OnRequestDock: TRequestDockEvent;
TRequestDockEvent = procedure(Sender: TObject; Bar: TCustomToolWindow97; var Accept: Boolean) of object;
Occurs whenever a toolbar is moved over the dock as it is being dragged. By setting Accept to False you can prevent a particular toolbar from being docked.
property OnResize: TNotifyEvent;
Occurs whenever the dock is resized.
Key Methods:

procedure BeginUpdate;
Disables toolbar arrangement. Call this when moving multiple toolbars on a dock to reduce flicker. Once the changes are complete, call EndUpdate.
procedure EndUpdate;
Re-enables toolbar arrangement after a call to BeginUpdate.
TToolbar97 Reference
Description:

This is the toolbar control itself. You can insert TToolbarButton97 controls or any other controls on this. All controls in a TToolbar97 are automatically lined up. To create separators, use the TToolbarSep97 component.

At run-time, any new controls created on a TToolbar97 are initially positioned at the end of the toolbar. To change positions of individual controls at run-time, assign to the OrderIndex property.

Remarks:

When the CloseButton property is True (the default), the toolbar can hide itself if the user clicks the close button on a floating toolbar. Because of this, you should always include an item on a menu that toggles the Visible property so the user can get it back if it is closed. See the demo application source code for an example of this.

Toolbars have to recreate themselves whenever they change from a docked to a floating state and vice versa. Because of this, you may notice that when some types of controls are placed on a toolbar (like TComboBoxes), they don't properly handle this situation (due to bugs) and lose some of their values when docking or undocking occurs. If you notice this happening, you need to respond to the OnRecreating and OnRecreated events. Put code in the OnRecreating event that saves the state of the controls in the toolbar, and put code in the OnRecreated event that restores the state. For example, if you had a TComboBox dropdown list, you could make it save its ItemIndex value into a temporary variable in OnRecreating. OnRecreated could then restore the ItemIndex value from the temporary variable.

Key Properties:

property ActivateParent: Boolean default True;
Determines whether the parent form is activated when a floating toolbar is clicked.
property BorderStyle: TBorderStyle default bsSingle;
When set to bsSingle, the toolbar has a 3-D border.
property Caption;
What appears in the title bar of a floating toolbar.
property CloseButton: Boolean default True;
When True, a close button appears in the title bar when the toolbar is floating.
property CloseButtonWhenDocked: Boolean default False;
When True, a close button is displayed when the toolbar is docked.
property DefaultDock: TDock97;
(Note: The LastDock property is meant to supersede this property.)
The default dock location. This is used when the user double-clicks a floating toolbar. If neither this property nor LastDock are set, nothing will happen.
property DockableTo: TDockableTo default [dpTop, dpBottom, dpLeft, dpRight];
TDockableTo = set of (dpTop, dpBottom, dpLeft, dpRight);
Specifies which positions the toolbar may be docked at.
property DockedTo: TDock97;
The TDock97 control that the toolbar is currently docked to. You can assign to this value any TDock97 control that belongs to the same form as the toolbar. To undock a toolbar at design time, delete the value from this property. To undock a toolbar at run time, assign nil to this property.
property DockMode: TToolWindowDockMode;
TToolWindowDockMode = (dmCanFloat, dmCannotFloat, dmCannotFloatOrChangeDocks);
Determines where the user can drag the toolbar. If this is dmCanFloat, the default, the toolbar can float or dock to any dock matching the criteria set by DockableTo. If this is dmCannotFloat, the toolbar can dock to the same docks but it cannot float. If the user moves the mouse outside a dock, the "Unavailable" mouse cursor is displayed. Also, the toolbar does not respond to double-clicks. If this is dmCannotFloatOrChangeDocks, the user cannot drag the toolbar anywhere outside its current dock.
property DockPos: Integer;
This is only valid if the toolbar is currently docked (DockedTo <> nil). This is its current horizontal (or vertical, if docked to a left or right dock) position in pixels.
property DockRow: Integer;
This is only valid if the toolbar is currently docked (DockedTo <> nil). This is the row the toolbar is currently docked at.
property DragHandleStyle: TDragHandleStyle default dhDouble;
TDragHandleStyle = (dhDouble, dhNone, dhSingle);
Determines the type of drag handle displayed on the left (or top, when docked vertically) of the toolbar.
property FloatingMode: TToolWindowFloatingMode;
TToolWindowFloatingMode = (fmOnTopOfParentForm, fmOnTopOfAllForms);
By default, this is set to fmOnTopOfParentForm, meaning when floating the toolbar only stays on top of its parent form. If this is set to fmOnTopOfAllForms, the toolbar will stay above all other forms in the project (except those that are also set to stay on top).
property FullSize: Boolean default False;
When True, the toolbar always fills the entire width (or height, if vertically docked) of the dock, much like Office 97's menu bar.
property HideWhenInactive: Boolean default True;
When True, the toolbar is hidden whenever the application is deactivated (a characteristic of Office 97's toolbars).
property LastDock: TDock97;
The TDock97 control that the toolbar was last docked to, or if the toolbar is currently docked, this is equal to the DockedTo property. Double-clicking a floating toolbar will restore it back to the dock specified by this property, and at the same position it previously was docked at. This property overrides and is meant to be a replacement for DefaultDock. If you want to disable the use of this property, set UseLastDock to False.
property OrderedControls[Index: Integer]: TControl;
Run-time only. OrderedControls works just like the standard Controls property (see Delphi/C++Builder help), except the controls it returns are in the order they appear on the toolbar.
property OrderIndex[Control: TControl]: Integer;
Run-time only. The OrderIndex property holds the position of each control on the toolbar. Use this property to change the position of a control on a toolbar at run-time. (Note that Toolbar97's loading/saving functions do not save this property.) The first control has an OrderIndex value of zero. Much like the way Delphi's TabOrder property works, assigning to one control's OrderIndex automatically shifts other controls' OrderIndexes back or forward. Following are some examples:
// Move a button to the left of the toolbar
Toolbar971.OrderIndex[ToolbarButton971] := 0;

// Move another button to the right of the toolbar
Toolbar971.OrderIndex[ToolbarButton972] := Maxint;
property ShowCaption: Boolean default True;
When True, the toolbar displays a caption bar and a close button (if CloseButton is True) when floating.
property UseLastDock: Boolean default True;
When True, the toolbar saves the last dock it was docked to in the property LastDock, and internally preserves the position it was docked at. See the description of LastDock for more information.
Events:

property OnClose: TNotifyEvent;
Occurs after the toolbar is hidden in response to the user clicking the toolbar's Close button.
property OnCloseQuery: TCloseQueryEvent;
TCloseQueryEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
Same in function as a form's OnCloseQuery event. Setting CanClose to False will cancel the requested close operation.
property OnDockChanged: TNotifyEvent;
Occurs after the toolbar has changed between docks, or from a docked to floating state or vice versa.
property OnDockChanging: TNotifyEvent;
Occurs immediately before the toolbar changes between docks, or from a docked to floating state or vice versa.
property OnDockChangingEx: TDockChangingExEvent;
TDockChangingExEvent = procedure(Sender: TObject; DockingTo: TDock97) of object;
Same as the OnDockChanging event, but has an extra parameter DockingTo that specifies where the toolbar is about to dock to.
property OnDockChangingHidden: TDockChangingExEvent;
TDockChangingExEvent = procedure(Sender: TObject; DockingTo: TDock97) of object;
Similar to the OnDockChangingEx event, but this event is called after the toolbar has already been hidden from the screen in preparation to be moved to another dock. This can be useful if, for example, you want to make changes in the ordering of the toolbar's controls whenever it moves between docks, but don't want any visible flickering during this time.
property OnRecreated: TNotifyEvent;
Occurs immediately after the toolbar recreates itself. This usually happens when it changes between a docked and non-docked state. See the Remarks above for more information.
property OnRecreating: TNotifyEvent;
Occurs immediately before the toolbar recreates itself. This usually happens when it changes between a docked and non-docked state. See the Remarks above for more information.
property OnResize: TNotifyEvent;
Occurs after the toolbar's size changes. Note that this event is fired after any size change, not only when the user resizes a floating toolbar.
property OnVisibleChanged: TNotifyEvent;
Occurs after the toolbar's visibility changes. This event will occur if the application manually changes the visibility of the toolbar (e.g., by toggling the Visible property), or if the user closes the toolbar using its Close button.
Key Methods:

procedure AddDockForm (const Form: TCustomForm);
Adds a form to the list of forms that the toolbar can be docked to besides the current parent's parent form. Keep in mind that moving a toolbar to another form does not change its Owner property; therefore it will still be destroyed when its owner component is destroyed. To change a toolbar's owner, use TComponent's RemoveComponent and InsertComponent methods.
procedure BeginMoving (const InitX, InitY: Integer);
TToolWindowSizeHandle = (twshLeft, twshRight, twshTop, twshTopLeft, twshTopRight, twshBottom, twshBottomLeft, twshBottomRight);
Forces the toolbar to enter "move" mode. This is called internally whenever the user clicks the drag handle of a docked toolbar or the caption bar of a floating toolbar, and will only work properly if it is called while the left mouse button is still down. The InitX and InitY parameters specify the client coordinates where the mouse button went down at, which determine where the dragging rectangle appears initially. In most cases (0, 0) should be fine.
procedure BeginSizing (const ASizeHandle: TToolWindowSizeHandle);
TToolWindowSizeHandle = (twshLeft, twshRight, twshTop, twshTopLeft, twshTopRight, twshBottom, twshBottomLeft, twshBottomRight);
Forces the toolbar to enter "size" mode. This is called internally whenever the user clicks one of the resizing handles on the border of a floating toolbar, and will only work properly if it is called while the left mouse button is still down. The ASizeHandle parameter specifies which resizing handle was clicked.
procedure BeginUpdate;
Disables arrangement of controls on the toolbar. Call this when moving multiple controls on the toolbar to reduce flicker. Once the changes are complete, call EndUpdate.
procedure EndUpdate;
Re-enables control arrangement after a call to BeginUpdate.
procedure RemoveDockForm (const Form: TCustomForm);
Removes a form from the list of forms AddDockForm adds to.
procedure SetSlaveControl (const ATopBottom, ALeftRight: TControl);
Call this when the form is created to designate a top/bottom docked and left/right docked version of a control on the toolbar. At design time, create both versions side-by-side (with no separator in between). Alternatively, you can specify nil for one of the parameters to have no left/right or top/bottom docked version.
See the demo application source code for an example of how to use this.
TToolWindow97 Reference
Description:

This component is very similar to TToolbar97, but has two key differences: contained controls are not arranged automatically, and floating tool windows may be resized freely.

See the help for the TToolbar97 component for explanations of the properties and events not listed here.

Key Properties:

procedure MinClientHeight: Integer default 32;
The minimum height, in client pixels, that the tool window can be resized to.
procedure MinClientWidth: Integer default 32;
The minimum width, in client pixels, that the tool window can be resized to.
procedure Resizable: Boolean default True;
When True, the user may resize the tool window when floating.
TToolbarButton97 Reference
Description:

The TToolbarButton97 component is similar to Delphi 3's TSpeedButton component, but it works more like Office 97 and adds some new features. You aren't required to use this for the buttons on toolbar, but it's recommended that you do so.

If you want your application to look just like Office 97, you should leave the width of buttons at 23 pixels and the height at 22 pixels (when viewed in small fonts). Glyphs should be 16x16 pixels (or 17x17 for some disabled glyphs).

See the help for Delphi's TSpeedButton component for explanations of the properties, methods, and events not listed here.

Key Properties:

property Action;
Delphi 4 only. Specifies an action to associate with the button. The Images property of TToolbarButton97 is not automatically synchronized with the action list's Images property.
property Alignment: TAlignment default taCenter;
TAlignment = (taLeftJustify, taRightJustify, taCenter);
Specifies the alignment to use when drawing multiple lines of text when WordWrap is set to True. This has no effect on buttons with only a single line of text.
property CallDormant: Boolean default True;
Run-time only. When True, TToolbarButton97 calls Dormant on the Glyph TBitmap, which greatly reduces GDI resource consumption. The only downside to having CallDormant set to True is that if the bitmap assigned to Glyph was a DDB, it will be converted to a DIB.
property DisplayMode: TButtonDisplayMode default dmBoth;
TButtonDisplayMode = (dmBoth, dmGlyphOnly, dmTextOnly);
Determines whether the glyph, caption, or both are drawn on the button. You could adjust this if you wanted to, for example, hide glyphs without deleting them.
property DropdownAlways: Boolean default False;
When True, the button always functions in "dropdown mode," regardless of whether the DropdownMenu property has a menu assigned. This can be useful for implementing your own dropdown functionality using the OnDropdown event.
property DropdownArrow: Boolean default True;
When True, the button displays an arrow if DropdownMenu is assigned.
property DropdownArrowWidth: Integer default 9;
Specifies the amount of pixels to reserve for the dropdown arrow. (This doesn't affect the size of the actual arrow.) Typically, this property will only be changed if DropdownCombo is set to True.
property DropdownCombo: Boolean default False;
When True, the button is split into two parts, like the Undo and Redo buttons in Office 97. Clicking the left side of the button generates an OnClick event, and clicking the right side displays the popup menu specified by DropdownMenu. Note: The extra two pixels on the right side are there intentionally because Office 97 has them, and it provides better separation when you have several DropdownCombo-style buttons in a row.
property DropdownMenu: TPopupMenu;
When this is assigned, the button will display this popup menu instead of calling the OnClick handler when clicked. The menu appears below the button, or to the right if the parent toolbar is docked to the left or right side.
property Flat: Boolean default True;
When True, the button has the Office 97 appearance.
property Glyph: TBitmap;
Same as TSpeedButton's Glyph property, except it supports a fifth glyph which is used when the mouse is over the button.
property GlyphMask: TBitmap;
This is a special-purpose property. Assigning a bitmap to this property is optional. When one is assigned, TToolbarButton97 uses it as the transparent mask, instead of generating its own based on the value of Glyph's bottom-left pixel. The mask bitmap should contain only black and white colors. This property can be useful when you have a need to display an icon (meaning a TIcon or HICON) as a glyph on a button. Assign its XOR bitmap to Glyph, and its AND bitmap to GlyphMask.
property HighlightWhenDown: Boolean default True;
When False, the dithered pattern will not be shown when the button's Down property is True.
property ModalResult: TModalResult default mrNone;
Just like TButton's ModalResult property.
property ImageIndex: Integer default -1;
The particular image to use from the image list specified by the Images property.
property Images: TCustomImageList;
This property specifies an optional image list to use for glyphs. When an image list is assigned to this property, it overrides the Glyph/GlyphMask properties.
property NoBorder: Boolean default False;
When True, the button never displays a border (regardless of the Flat property's setting).
property NumGlyphs: TNumGlyphs97;
TNumGlyphs97 = 1..5;
Same as TSpeedButton's NumGlyphs property, except it supports a fifth glyph which is used when the mouse is over the button.
property OldDisabledStyle: Boolean default False;
When True, the generated disabled glyphs look like TSpeedButton and earlier versions of TToolbarButton97. When False, they use the Office 97 look.
property Opaque: Boolean default True;
When True, the button is not transparent, which prevents the "blinking" effect that you see when you move the mouse over it or click it (as seen in Delphi 3's TSpeedButton, and most other button components). You should only need to set this to False if you have a Background for the dock.
property Repeating: Boolean default False;
When True, OnClick events are periodically fired while the mouse button is down on the button, using the interval specified by RepeatInterval, and an initial delay specified by RepeatDelay.
property RepeatDelay: Integer default 400;
The initial delay, in milliseconds, before OnClick events are fired at the rate specified by RepeatInterval. Only applicable when Repeating is True.
property RepeatInterval: Integer default 100;
The interval, in milliseconds, at which OnClick events are fired after the initial delay specified by RepeatDelay has expired. Only applicable when Repeating is True.
property ShowBorderWhenInactive: Boolean default False;
When True, button borders are shown even when the application is inactive (applies to flat buttons only).
property WordWrap: Boolean default False;
When True, text that is too wide for the button is wrapped into several lines.
Key Events:

property OnDropdown: TButtonDropdownEvent;
TButtonDropdownEvent = procedure(Sender: TObject; var ShowMenu, RemoveClicks: Boolean) of object;
Occurs before the button's dropdown menu is about to be displayed, or if DropdownAlways is True, when the left button is pressed over the button.
If a menu is assigned to the DropdownMenu property, the ShowMenu parameter will be True when the event handler is called. Setting ShowMenu to False will cause it to not show the dropdown menu after the event handler returns.
The RemoveClicks parameter is always initially True. Normally TToolbarButton97 removes from the message queue additional clicks on the button while the dropdown menu is displayed. This is so clicking on the button and clicking it again will close the dropdown menu and not display it again. Setting RemoveClicks to False will prevent this.
property OnMouseEnter: TNotifyEvent;
Occurs when the mouse cursor moves inside the button.
property OnMouseExit: TNotifyEvent;
Occurs when the mouse cursor leaves the button.
TToolbarSep97 Reference
Description:

The TToolbarSep97 component is used to create separators between toolbar buttons. It automatically adjusts its size and orientation at design and run time. At run time, it also adjusts its visibility when necessary.

Key Properties:

property Blank: Boolean default False;
When True, the separator does not have a beveled appearance.
property SizeHorz: Integer default 6;
The width of the separator when the parent toolbar is docked to the top or bottom.
property SizeVert: Integer default 6;
The height of the separator when the parent toolbar is docked to the left or right.
TEdit97 Reference
Description:

The TEdit97 component is identical to Delphi's TEdit component, except it has the Office 97 appearance, and is missing a few properties that are not applicable.

Known Problems and Conflicts
Problem:
I have a combo box on my toolbar, and whenever I dock or undock the toolbar the combo box's ItemIndex property is reset.
Solution:
This is a bug in the VCL (affecting Delphi/C++Builder versions prior to 4.0), not Toolbar97. Please read the Remarks section of the TToolbar97 reference for information on how to work around this.

Problem:
I have my docks at the edges of the form, but for some reason toolbars are docking in unexpected places, such as the middle of the form.
Solution:
This is due to the design of the VCL, and usually happens if you have multiple controls with the same Align setting. Try setting the FixAlign property for the docks exhibiting this problem.

Problem:
My RichEdit component is now limiting itself to 64KB after adding a toolbar to my form.
Solution:
This problem exists in Windows 95 and 98 (not NT/2000). The bug is not in TCustomToolWindow97, but TCustomToolWindow97 does cause this to start happening by its use of a WH_CALLWNDPROC hook. On 95/98, rich-edit controls start misbehaving if a WH_CALLWNDPROC hook of any kind is installed. Unfortunately there is not much that can be done about this problem, since TCustomToolWindow97 needs to use a hook in certain places for it to work properly, and even the latest RICHED32.DLL still has this problem. One option is to use a rich-edit component based on RICHED20.DLL, which does not exhibit this problem, and is Microsoft's current recommendation. (For one such component, try searching for riched98 or richedit98 on the Delphi Super Page. I'm also told that RxLib contains a Rich Edit 2.0 component.)

Problem:
UpDown controls on TToolbar97's conflict with TToolbar97's automatic control arrangement when their Associate property is assigned.
Solution:
Manually synchronize the UpDown control and its buddy window instead of using the Associate property.

Tips
When using the same background on several docks, it is more efficient to assign the background bitmap to one at design time, and copy it to the rest using code at run time. For example:
BottomDock.Background.Assign (TopDock.Background);
LeftDock.Background.Assign (TopDock.Background);
RightDock.Background.Assign (TopDock.Background);
This way you don't wind up with a larger EXE caused by duplicates of the same bitmap.

To quickly change the DisplayMode of all buttons on a TToolbar97, use code like this:
var
  I: Integer;
begin
  Toolbar971.BeginUpdate;
  try
    for I := 0 to Toolbar971.ControlCount-1 do
      if Toolbar971.Controls[I] is TToolbarButton97 then
        with TToolbarButton97(Toolbar971.Controls[I]) do begin
          DisplayMode := ...;
          Width := ...;
          Height := ...;
        end;
  finally
    Toolbar971.EndUpdate;
  end;
end;
Be warned: The TTreeView components in Delphi versions 2.0-3.02 and C++Builder 1.0 have a memory leak bug. Whenever a toolbar containing a TTreeView is docked or undocked, memory is leaked. More specificially, a TTreeView leaks memory whenever its handle gets "recreated." The amount of memory leaked can range from a few bytes to several thousand bytes, depending on the number of nodes. Delphi 4.0 and C++Builder 3.0 fix this bug.