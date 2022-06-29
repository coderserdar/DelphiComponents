AutoCorrect Components v1.0.3 (23/9/09)
Author
Chris Rolliston (http://delphihaven.wordpress.com/).
Licence
MPL 1.1 (text at http://www.mozilla.org/MPL/MPL-1.1.html).
Supported Delphi versions
Separate versions for Delphi 7 on the one hand and Delphi 2005 or above on the other.
What does the code do exactly?
Implements for VCL controls that descend from TCustomEdit or TCustomCombo a large sub-set of the AutoCorrect functionality had by ‘proper’ word processors. Specifically, the following is implemented:
User-defined ‘replace text as you type’ entries, including formatted ones if the active control is a rich edit.
TWo INitial CApitals correction, with an exception list that is automatically added to when the user immediately undoes a correction.
Conversion of 'straight' quotes into ‘curly’ (‘smart’) quotes.
Conversion of hyphens to dashes: two hyphens in a row (--) turn into an em-dash, a hyphen with a space either side ( - ) becomes an en-dash with a space either side.
Conversion of 1/2 to ½, 1/4 to ¼, and 3/4 to ¾.
Auto-indentation for rich edit controls — in other words, allows the user to set the active paragraph’s left- and first-indent properties by tabbing and backspacing.
How does it work?
In essence, the code works by trapping the KeyPress method (OnKeyPress event) of the active control, testing for a trigger key that will lead to correction testing proper kicking in. The initial trapping, however, is ‘manual’ in the sense that the TAutoCorrectEngine component does not actively ‘seek out’ key press events.
What’s the quickest way to see it in action?
Open up the ‘AutoCorrect demo and design-time package’ project group and run the last project listed (AutoCorrectDemo.dpr). Since the TAutoCorrectEngine component is created at runtime here, no installation is required first.
How do I use the code myself?
Firstly, compile and install the design-time package, dclCCRAutoCorrect.dpk. This should lead to five new components being added to the component palette: TAutoCorrectEngine, TComboBoxWithAutoCorrect, TEditWithAutoCorrect, TMemoWithAutoCorrect and TRichEditWithAutoCorrect.
Ensure CCR.AutoCorrect.dcu and CCR.AutoCorrect.Consts.dcu end up somewhere in your library paths — the easiest way to do this is to add the component’s directory as a library path itself. In Delphi 2007, you do this by going to Tools|Options, Environment Options  Delphi Options  Library - Win32, and clicking on the ellipsis button for ‘Library path’.
Create a new VCL forms application and add a TAutoCorrectEngine component to the main form; double click the component to add some custom ‘replace-as-you-type’ entries.
Add one or more TxxxWithAutoCorrect controls to the form, and assign their AutoCorrectEngine property to the TAutoCorrectEngine component.
Compile and run.
But I don’t want to use your custom descendants of TEdit, TRichEdit, etc.!
No problem — you’ll just have to do a bit of manual work, calling as appropriate the KeyDownOccurred, KeyPressOccurred and UndoOccurred methods of the TAutoCorrectEngine component. The last of these is best done by defining an undo action (i.e., a TAction with Ctrl+Z as its shortcut), UndoOccurred being called immediately after the undo operation is performed. As for the other two, you can either handle the OnKeyXXX events of the controls themselves or — if you set KeyPreview to True — the parent form. In the second case, pass ActiveControl as the first parameter to KeyXXXOccurred. See the main demo for an example of all this — the process is simpler than it may sound.
The small print
Turbo Delphi Explorer compatibility: in short, you don’t need to install TAutoCorrectEngine to actually use it, and adding custom entries at runtime is pretty easy. Check out the main demo, which doesn’t use a design-time instance of the component and so can be open and run from inside the Turbo Delphi IDE.
Rich edit bugs: note that the TRichEditWithAutoCorrect class works around bugs in the underlying DLL the VCL’s TCustomRichEdit wraps. Specifically, v1 of the DLL (used until Delphi 2009) uncurls smart quotes when they are streamed or pasted in, and v1 to v3 of the DLL don’t load en- and em-dashes correctly, loading them as hyphens.