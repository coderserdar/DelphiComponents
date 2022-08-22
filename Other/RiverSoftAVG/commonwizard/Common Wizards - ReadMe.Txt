Hello and welcome to the Common Wizards package!

Contents
	About the Common Wizards package
	Installation Instructions
	Uninstall Instructions
	How to use the New Collection Wizard
	Contacting RiverSoftAVG

-------------------------------------------------------------------------
About the Common Wizards package
-------------------------------------------------------------------------

The Common Wizards package is a free package of design-time wizards for Delphi from RiverSoftAVG.com  Currently, the CommonWizards.dpk file contains only one wizard:

	New Collection Wizard	- Write code to create a new collection and 
				  collection item descendant

	Version History
		1.01	September 2002
			- Fixed bug where if form was displayed when wizard
			  was executed, nothing would happen (Thanks Marc S)
		1.0     August 2002
			- Initial Version

-------------------------------------------------------------------------
Installation Instructions
-------------------------------------------------------------------------

How to install the New Collection Wizard

1. Unzip the file into any directory.
2. Start Delphi.  Open the CommonWizards.dpk package
3. This package requires the PasLexD60 package (a freeware Pascal Parser/Lexer, also available from our download package).  If you are using Delphi 5, you need to remove PasLexD60 from the package and add the correct version of the PasLex package
3. Compile and Install the package
4. That's it!  You're done.  The New Collection Wizard is now available from the File->New->Other... window in the "New" tab sheet.

Enjoy!

If there are any problems, don't hesitate to contact me at support@RiverSoftAVG.com


-------------------------------------------------------------------------
Uninstall Instructions
-------------------------------------------------------------------------

1.	Start Delphi.  
2.	Select Component->Install Package...  
3.	Find the package labeled "RiverSoftAVG Common Wizards", select it and click Remove.


-------------------------------------------------------------------------
How to use the New Collection Wizard
-------------------------------------------------------------------------

The New Collection Wizard is a handy little wizard for Delphi to take the drudgery out of coding TCollection descendants.  Use this New Collection Wizard to quickly and safely create TCollection and TCollectionItem descendants.  It adds the source code it generates to the current file you are editing.  

Start the wizard by opening the New Collection Wizard in the File->New...->Other... New tab sheet.  You should see a window appear titled "New Collection Wizard".  There are 4 edit boxes/combo boxes at the top, a 3 tab page control in the middle, and Ok/Cancel buttons at the bottom.

The 4 edit boxes are where you specify the name of the collection and the collection item, what class the collection descends from, and the owner of the collection.  So, for example, if you wanted to define a TMyCollection = class(TOwnedCollection), which has TMyCollectionItem items and whose owner is TMyComponent, you would fill in TMyCollectionItem, TMyCollection, TOwnedCollection, and TMyComponent from top to bottom in each of the edit boxes.

The 3 tab sheets in the middle specify Options, Collection Item properties, and a Preview.  The Options tab sheet controls what source code is generated.  The Collection Item Properties tab sheet allows you to enter properties for your collection item.  The wizard allows you to define the property name and type, private field, and optional read and write accessor methods.  The preview tab is especially useful.  This tab shows you the code that the New Collection Wizard will generate based on your inputs and options.  The easiest way to understand the different edit boxes and options of the wizard is to experiment and then check the code in the Preview pane.

When you are finished setting options and entering information, click Finish and the source is generated!


-------------------------------------------------------------------------
Contacting RiverSoftAVG
-------------------------------------------------------------------------

For technical support send email to:  

  support@RiverSoftAVG.com

-------------------------------------------------------------------------

 Unless otherwise noted, all materials provided in this release
 are Copyright © 2002, Thomas G. Grubb.

-------------------------------------------------------------------------
