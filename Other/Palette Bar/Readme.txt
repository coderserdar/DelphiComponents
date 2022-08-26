Legal status
PaletteBar is FREEWARE. Freeware means, that you can use this soft-
ware without paying anything for it.
This software is protected under the law of the Federal Republic of
Germany, the European Union and other countries.
Please note, that I can’t take any warranty for the function of this soft-
ware on any machine or software environment.
You have to use it for your own risk!
Copyright (C) 2003 by Peter Hellinger Software.
All rights are reserved.
Address:
Peter Hellinger Software,
Zerzabelshofstr. 41,
D-90480 Nürnberg, Germany
Email:
mail@helli.de
Web:
http://delphi.helli.de
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 4 of 16
2. Some Basics
The component palette in the Borland IDE is a little bit badly arranged,
and if you installed a lot of components then you spend more time in
clicking and searching than in developing your application.
So, it is time for a new way of adding components and work with big
component packages. It's time for PaletteBar!
PaletteBar works with the following Borland IDEs:
-
C++Builder 6
-
Delphi 6 Personal Edition
-
Delphi 6 Standard / Professional / Enterprise
-
Delphi 7 Personal Edition
-
Delphi 7 Professional / Enterprise / Architect
A version for Kylix Open 3 is under development and will be available
soon.
3. Installation
Please make sure, that your IDE is not running while you are installing
PaletteBar!
Run the PB330.EXE setup program and follow the installation advices.
Please select the Version you like to install. Use "Architect, Enterprise,
Professional" for PaletteBar with CLX-Support, use "Standard, Personal
Edition" for the IDE's without CLX, or choose the IDE's you prefer
manually from the list.
The setup program copies all necessary files to the folder you have
chosen.
After setup, PaletteBar is ready to use...
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 5 of 16
4. New in this Version
- Fixed initialisation problem
Some windows DLLs are loaded faster since a few weeks, maybe any
updates from Microsoft… Ok, Delphi’s loads faster and when
PaletteBar comes to initialise the list of components, the Delphi IDE
has not initialized the built in component bar, which is read out by
PaletteBar. So only one category is read out (mostly Standard),
others are not filled.
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 6 of 16
5. Using PaletteBar
After installing PaletteBar, the menu “PaletteBar” appears next to the
Tools-menu of the IDE. Select the entry “PaletteBar to show up the
main window. Place it anywhere on your desktop, the window remem-
bers size, position and whether it was opened the last time you ran the
IDE. As you see below, you can dock it anywhere in the IDE. Please
remember to save your IDE-Desktop to restore docking state at next
loading.
Initial window of PaletteBar, docked to Objectinspector
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 7 of 16
5.1. The tool bar
On top of the window you see a tool bar with 11 buttons. You see
also a list of the components, which are on the actual tab of the
built in component palette. On the right side you see the "A to Z
bar". On the bottom of the window you see the so called Searchbar.
5.1.1.
The main menu
The first button is the main menu of
PaletteBar; all other Buttons are represented in
this menu, and not further explained.
Here you find all the pages of the common component palette.
On the first run, all known pages are listed under the menu en-
try “Others”
If you have already configured your individual categories (see
chapter 4.5 Configure Category), then you see in addition to
"Others" menu entries for the Meta categories you have en-
tered.
5.1.2.
Actual Category
Show all components of the category you have
chosen. The name of the category is shown
below in the header of the component list.
5.1.3.
All components
Shows all components you have installed in one
big list.
5.1.4.
Actual Form
Show all components on the currently edited
form. Please note, that a form editor must be
activated in the IDE (the form has to be the top
window) to use this feature.
5.1.5.
History
List all components you have already used in
this session. If you want to store the history
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 8 of 16
from session to session, you can choose the "Save History" op-
tion from the "Configure PaletteBar" menu.
5.1.6.
Favourites
Lists all your favourite components. To add a
component to the favourite list, simply right
click on the component in the list and choose
"Add to favourites". In the favourites list you can delete compo-
nents in the same way (right click context menu).
5.1.7.
Edit Categories
Shows a dialog box where you can edit the
"meta” categories. (See chapter 4.5, “Edit
Categories”)
5.1.8.
Configure Palette
Run the standard IDE dialog for configuring the
pages of the common component palette.
5.1.9.
Configure PaletteBar
Brings up the configuration dialog PaletteBar
wizard. (See chapter 4.6 “Configure PaletteBar”)
5.1.10. Show Help for Component
If a component is selected in the list, you can
call the online help with the corresponding help
page for the component.
5.1.11. About PaletteBar
Shows Information about the program version.
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 9 of 16
5.2. The Component List
In the component list you see the components according to the se-
lection you have made: Actual category, All components, History,
Actual Form or Favourites.
Clicking the Header of the list sorts the list in alphabetical or re-
verse order. For “Actual category”, the list is also shown in the
original order from the built in component palette.
With the left and right arrows in the header, you can browse trough
the component categories installed in your IDE.
5.3. The A-Z Bar
Selecting an item from the A-Z Bar scrolls the component list to the
first item that begins with the corresponding letter.
If you have selected the “Filter mode” feature from the context
menu or from the global configuration, the list shows only the com-
ponents with the corresponding letter.
5.4. The Search bar
With the Search bar you can search for a component or a group of
components. Just enter the text you want to look for in the edit
field and press the “Go” button. PaletteBar searches all compo-
nents and lists those with matching the text you entered. For ex-
ample enter the text “button”, and PaletteBar shows you all compo-
nents with “button” in their names, (TButton, TSpeedButton, TRa-
dioButton and TSpinButton for example).
PaletteBar saves your searched keywords and you can choose them
again from the combo box.
5.5. Inserting Components
To insert a component to your form is quite simple: Just click on
the component you wish to insert, hold down the left mouse button
and drag the component to your form. The component is inserted at
the position, you release the mouse button.
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 10 of 16
5.6. Edit Categories
If you select “Edit Categories” from the tool bar or the main menu,
you get the following dialog to add “meta categories”.
The Edit Categories dialog
“Meta” means that this category did not contain components di-
rectly, but contains groups of categories. This helps you to navi-
gate quicker through the components than with the built in palette,
because you can jump directly to any component page you like.
On initial run, PaletteBar adds automatic all pages from the built in
component palette to the Meta category “Others”.
You can add your individual Meta categories by clicking the button
“New”. The new category is inserted at the end of the list with the
name “new category”. You can always edit the name of the Meta
categories (except “Others”) by double clicking the entry or press-
ing the F2 key when the entry is selected.
To add a category (with components) to a meta category simple
drag and drop the category item to the meta category you wish to
put it. If you want to copy the category – i.e. to have the category
under more than one Meta category – hold down the shift key on
your keyboard when dropping.
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 11 of 16
To delete a Meta category, select the entry and click the “Delete”
button. The categories in the Meta category you wish to delete are
added automatic to “Others”.
Please note, that you can not delete categories or the Meta cate-
gory “Others”; also, you can not have a category more than once
in a Meta category.
5.7. Configure PaletteBar
With the new designed configure dialog (select “Configure Palette-
Bar” from the tool bar or the main menu), you can configure Palet-
teBar for your individual needs.
The Configure dialog
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 12 of 16
5.7.1.
List of components
Configure the look and feel of the component list. You can also
configure the list by the context menu of the list (clicking with
the right button into the list).
• View
Select the display options for the list. Choose “Symbols” to show
only the symbol for the component, choose “Text” to show only
the text, or choose “Symbol and Text” to show both of them.
• Text size
Select “Small”, “Middle” or “Big” size for the text used in the
component list.
• Hot tracking
Select “Hot tracking” (selecting components by moving the
mouse over the components instead of clicking to select) for the
list, or deselect, if you don’t like this feature.
5.7.2.
Toolbar
Configure the look of the tool bar. You can also configure the
tool bar by its context menu.
• Show Buttons
Select the buttons you want to have displayed on the tool bar.
• Position
Select the position, the tool bar is displayed. Choose “Top”,
“Right” or “Left”
5.7.3.
A – Z bar
Place the A-Z Bar on the left or the right side of the component
list, or switch it off, if you don't want to use it. Select “Filter
mode” for activating Filter mode. Refer to chapter 5.3 “The A-Z
bar” for more information.
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 13 of 16
5.7.4.
History
• Save history
If selected, PaletteBar saves the history list when the IDE is
shutting down.
You can clear the history list by selecting “clear History” in the
context menu of the history list.
5.7.5.
Search Bar
Select the display options for the Searchbar.
• Top, Bottom
Display the Search Bar at the top or the bottom of the window.
• Visible
Select, if the Search bar is visible or not
5.7.6.
Language
Actual you can choose between English, French, German, Potu-
guese and Turkish.
PaletteBar has the ability to add individual languages “on the
fly” by the user. Please refer chapter 5.8 for defining additional
languages.
5.7.7.
Clear/Delete
In the “Clear” field you can choose which list to delete:
- History
- Favourites
- Search text
Press the Delete button to delete the list you choose.
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 14 of 16
5.8. Additional languages
PaletteBar has the ability to add individual languages “on the fly” by
the user. The language strings are stored in usual text files in the
PaletteBar directory. They are named “palettebar.<lang>”.
<lang> is the grammalogue of the language, for example “EN” for
English or “DE” for German.
To localize PaletteBar for your language simply translate the con-
tent of “palettebar.EN” to your language, and rename it according
to your language (i.e. palettebar.FR for French).
To let PaletteBar know your language, you have to register it in an-
other text file named “palettebar.lng”. The file contains the
strings that are displayed in the language menu in a special format:
<language>_<name>_<grammalogue>
<language> is the grammalogue of the language the <name> part
belongs to. <name> holds the string for the menu, and finally
<grammalogue> is the file extension of the file where the corre-
sponding language strings are stored. Example:
EN_English_EN
EN_German_DE
DE_Englisch_EN
DE_Deutsch_DE
To add a individual language, you have to add the menu stings as
described above. For example French:
EN_English_EN
EN_German_DE
EN_French_FR
DE_Englisch_EN
DE_Deutsch_DE
DE_Französisch_FR
FR_Anglais_EN
FR_Allemand_DE
FR_Français_FR
If you have added a language please sends it to me (see Email ad-
dress above) and I put it in the next public release of PaletteBar.
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 15 of 16
6. Known Issues
6.1. In some cases it is possible that the fresh docked PaletteBar main
window is not zoomed correct to the extends of its parent. This is
not a bug in PaletteBar, because it did not know when it is docked.
The whole work is done by the IDE, so this is a bug in the IDE dock-
ing routines. Just change the size of the parent window a little bit
and PaletteBar zooms to fit in the window.
7. Credits
I wish to thank the following people:
Andrea Jacobs for help and assistance with documentation and transla-
tion.
Jacob Thurman for help and assistance with docking functions.
Bruno Sonnino for Portuguese translation.
Phillipe Lucarz for French translation.
Cumhur Semin for Turkish translation.
Jürgen Kehrel, Brian Brunoni, Steve Gouldstone, Ralf Grenzing and An-
thony Egerton for suggestions and help.
dageek for CompBar, the inspiration for PaletteBar
PaletteBar – an alternative component palette for Delphi and C++Builder
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
Page 16 of 16
8. History
Date
Version
Annotation
12.12.2003
3.32 -
Fixed initialisation problem
(only one category filled when loaded)
04.12.2003
3.31
- Fixed problem with getting component Icons
12.10.2003
3.30
- Added Turkish language file
- Added browse buttons to the header line
- Categories Dialog: a category can now also
dragged to the “Others” section
- Changed way to get the component images
and relations.
16.09.2003
3.21
- Fixed access violation with menu and closed
PaletteBar dialog
- Added French language file
14.09.2003
3.20
- In the edit field, you can use now the “?” as a place
holder for any char
- The toolbar can now totally removed, if the menu is
displayed in the IDEs main menu.
- smaller size, loads now faster
- Bug at loading new components fixed
01.09.2003
3.15
New Features:
- New A2Z-Toolbar
- Added Save-Feature to the search field
- Supports IDE version without CLX-Support
(D6 Personal and D7 Personal)
- Fixed bugs:
- IDE crash fixed again (grmblfx!)
01.08.2003
3.00
New Features:
- Supports docking
- complete internal redesign
- new configuration dialog
- Fixed bugs:
- IDE crash fixed again
29.06.2003
2.02
Fixed bugs:
- IDE crashed at shutdown
- Components not inserted on datamodule
28.06.2003
2.01
Fixed bug with changing languages
27.06.2003
2.00
Quantum leap…
- Runs now with Delphi 6 and C++Builder 6
- Code optimized,
- Actual Form feature added,
- added multiple meta categories,
- add support for individual languages
- extend configuration capabilities,
- improved Edit Categories dialog,
- and much more…
15.06.2003
1.02
CLX-Problems solved
15.06.2003
1.01
Adding support for Toolbar positioning
14.06.2003
1.00
First Release