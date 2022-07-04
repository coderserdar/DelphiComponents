==============================================================================
  TFontSizeCombobox, TFontCharsetCombobox 
  (C) Sergey Tkachenko (www.trichview.com)
  These components are freeware.
  ver 1.1, Apr 16, 2001
------------------------------------------------------------------------------
  For Delphi 6 
  (also compatible with Delphi 3,4,5, C++Builder 3,4,5)
------------------------------------------------------------------------------
                          Installation
Run Delphi.
Choose Component|Install Component, select SVFontCombos.pas as unit file name, 
and select a package to install these components 
(for example, dclusr60.dpk - Borland User Components), click OK.
When the Package editor is open, click Compile button on its toolbar.
After compiling, you'll see two new components in the "RichView Misc"
page of the IDE component Palette.

Demo project is in Demo subdirectory.
------------------------------------------------------------------------------
 TFontSizeComboBox - combobox for selecting sizes of font                     
 TFontCharsetComboBox - combobox for selecting character sets of font         
 Assign FontName property of these combos and they will be filled with values 
------------------------------------------------------------------------------
 TFontCharsetComboBox uses Items.Objects property itself, do not use it.      
 TFontCharsetComboBox has additional properties:                              
 AddDefaultCharset: Boolean - if True, DEFAULT_CHARSET item will be added     
 DefaultCharsetCaption: String - caption of list item above                   
 Charsets[Index: Integer]: TFontCharset - returns Charset of the Index-th item
 function IndexOfCharset(Charset: TFontCharset):Integer - returns index of    
 item with specified Charset in combo, or -1 if not found                     
==============================================================================
                   * * * * * * * * * * * * * * * * * *
               THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT
               WARRANTY OF ANY KIND, EITHER EXPRESSED OR
               IMPLIED. THE USER ASSUMES THE ENTIRE RISK
                 OF ANY DAMAGE CAUSED BY THIS SOFTWARE.

                 IN NO EVENT SHALL THE AUTHOR BE LIABLE
               FOR DAMAGE OF ANY KIND, LOSS OF DATA, LOSS
               OF PROFITS, BUSINESS INTERRUPTION OR OTHER
             PECUNIARY LOSS ARISING DIRECTLY OR INDIRECTLY.
                  * * * * * * * * * * * * * * * * * *