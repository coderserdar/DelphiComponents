==========================================
        Component TVHTMLExport
 (C)opyright 2001-2002 by Oliver Killguss
==========================================


TTVHTMLExport exports tree's from the Delphi's TTreeView into a HTML file. Now it's possible to
auto generate links within nodes and produce title, header and footer to output file. TTVHTMLExport
develop it's own images for the nodes appearance but you can also select your own images for
development. All used images will copied to destination path of generated file. Full compiled
updated demo with sources also included.

This Component is FREEWARE with full source code. I still hold the copyright.
If you have any ideas for improvement or bug reports, don't hesitate to e-mail me.
If you use this component in a commercial product so don't forget to give me some credits

WHATS NEW?
==========
1.20 - OnParseURL Event added. Demo improved (only Delphi)

How to use
==========
Since last time there are many changes beginning at component model. There are new features added
and some properties are now implemented in own classes e.g. THeader, TFooter and TTitle. Look at
the list below to read more about changes since last version. Now you can create own style sheets or
import them by using the loadfromfile method but remember that both tags <style> and </style> are
automatically created when file will be generated. Be sure that your imported css file has no <style>
and </style> tag or there causes some displaying problems.


PROPERTIES
==========
+ Header
  - Show            - if true the lines will be shown in output file
  - Lines           - contains text for output
  - Bold            - add's <b> bold tag to output generation to print lines bold
+ Footer
  - Show            - if true the lines will be shown in output file
  - Lines           - contains text for output
  - Bold            - add's <b> bold tag to output generation to print lines bold
+ Title
  - Show            - if true the lines will be shown in output file
  - Text            - contains text for output
  - Size            - means the header tag <h1> .. <h6> for header size
- CreateURL         - If true all links will be clickable for example you have a node like
                      "Please visit our Website at www.mySite.com for more information" will
                      automatically create a clickable link to the site
- FolderImage       - means the image if node is not expandable
- FolderOpenImage   - means image of expanded node
- Indent            - is the indent between nodes
- ShowBorder        - to draw border on tree
- ShowFolderImage   - set to true if you want to use the image at nodes root
- StyleSheet        - is a stringlist which contains a sytle sheet if manually written or
                      loaded by method LoadFromFile. Please note that <style> and </style> tag
                      is set by the component so you don't need to include it on your code
- UseInternalImages - if true imported images by the user will disabled and the delivered images
                      will be used


EVENTS
======
OnParseURL(URL: String; UseURL: Boolean) - Occurs when a URL is parsed from generated tree. You can use this event to
                                           excluse URL's from dispatching generating link on outputfile by compare
                                           URL with any string and set UseURL to false if wanted.
                                           Fired only when CreateURL is set to true.


METHODS
=======
SaveToHTML(Filename: String, Tree: TTreeView) - Call this method if you want to save a tree
                                                to a file. Parameters filename and tree are
                                                recommended
Show(Tree: TTreeView)                         - Opens Browser with generated tree specified


INSTALL COMPONENT
=================
Copy sources from directory lib to your preferred components path and install it by choose install components,
selecting the *.pas file and confirm.


LICENCE
=======
This Component is freeware so you can use or modify it. See Licence.txt for more details.
Please send me your suggestions and/or modifications to ensure that all users profit from expanding functionality.


Have Fun !

O.Killguss

e-Mail: killguss@web.de