Color controls v 1.0
----------------------

The library contains set of components working with lists that give capabilities to control their appearance


Contents
------------
  Overview
  Installation
  Demonstration program
  Copyright stuff
  Feedback



Overview
------------
Color controls - is a set of components that give capabilities to control their appearance.
Here they are:

  TColorPresets - A component that contains all color, font and frames setups.
  TColorListBox - child of TListBox 
  TColorCheckListBox - child of TCheckListBox
  TColor_ComboBox - child of TComboBox
  TColorValueListEditor - child of TValueListEditor (Only for Delphi 6, 7)
  TColorStringGrid - child of TStringGrid 
  TColorDrawGrid - child of TDrawGrid 
  TColorDBGrid - child of TDBGrid, grids columns can also contain color setups
  TColorDBListBox - child of TDBListBox 
  TColorDBComboBox - child of TDBComboBox 
  TColorDBLookupListBox - child of TDBLookupListBox 
  TColorDBLookupComboBox - child of TDBLookupComboBox 

All components have new property "ColorPresets". If it is not assigned, components act like usual. By placing on a form TColorPresets component and by assigning it to one of listed controls programmer obtains possibility to control such properties:

  ColorActive1 - Color of even-numbered lines, when control has focus 
  ColorActive2 - Color of odd-numbered lines, when control has focus
  ColorInactive1 - Color of even-numbered lines, when control has no focus
  ColorInactive2 - Color of odd-numbered lines, when control has no focus
  ColorSelectedActive - Color of selected line, when control has focus 
  ColorSelectedInactive - Color of selected line, when control has focus 
  ColorMultiSelectedActive1 - Color of even-numbered selected lines, when control has focus and more than 1 row is selected
  ColorMultiSelectedActive2 - Color of odd-numbered selected lines, when control has focus and more than 1 row is selected
  ColorMultiSelectedInactive1 - Color of even-numbered selected lines, when control has no focus and more than 1 row is selected
  ColorMultiSelectedInactive2 - Color of odd-numbered selected lines, when control has focus and more than 1 row is selected
  ColorInplaceEditor - Color of the inplace editor. For example, TDBGrid have one - it is shown when some record is in edit mode
  
  FontActive1 - Font of even-numbered lines, when control has focus 
  FontActive2 - Font of odd-numbered lines, when control has focus
  FontInactive1 - Font of even-numbered lines, when control has no focus
  FontInactive2 - Font of odd-numbered lines, when control has no focus
  FontSelectedActive - Font of selected line, when control has focus 
  FontSelectedInactive - Font of selected line, when control has focus 
  FontMultiSelectedActive1 - Font of even-numbered selected lines, when control has focus and more than 1 row is selected
  FontMultiSelectedActive2 - Font of odd-numbered selected lines, when control has focus and more than 1 row is selected
  FontMultiSelectedInactive1 - Font of even-numbered selected lines, when control has no focus and more than 1 row is selected
  FontMultiSelectedInactive2 - Font of odd-numbered selected lines, when control has focus and more than 1 row is selected
  FramePenActive - TPen property that define style of frame around a record in focused control
  FramePenInactive - TPen property that define style of frame around a record in not focused control
  FrameOverride - Boolean property that says to or not to draw custom frames around records.

  _ColorPreset - Property that store color schemes
  _FontPreset - Property that store font schemes

All these properties are stored in TColorPresets component and are working in similar way with all controls.
Exception is TPen.Style property for frames - it is not working with all controls.

TColorDBGrid contain columns that allow assigning its own TColorPresets property, this gives a more flexible way to setup the appearance of grid.

On the whole, usage of Color controls improves the visibility of information shown by controls



Installation
---------------
Unpack archive into folder listed in Delphi PATH, and open a package:
  ColorControlsD5.dpk for Delphi 5
  ColorControlsD6.dpk for Delphi 6
  ColorControlsD7.dpk for Delphi 7

In components install window press Compile then Install.
Close window and save changes.
Now a new tab had appeared in component palette named "Color controls"



Demonstration program
-------------------------------
Package has folder Demo, it contains demo project with compiled exe.



Copyright stuff
----------------------------
Color controls are free for use, so I am not responsible for any errors in your projects caused by usage of Color controls.
However, because white bears are always beat me and take away all my money, I can not buy enough vodka for my children. So, it would be nice of you to send me some money on Webmoney:

Z102207833214
R105236886302
E343027611446
U352388665804


Feedback
-----------------
Errors, raptures and propositions please send on ColorControls@mail.ru 


---------------------------
Kuzovkov Peter Evgenievich
Кузовков Петр Евгениевич 
20.05.2007

