This is a first attempt at a TStretchHandle component (I hope I am
not reinventing the wheel, but I couldn't find anything).  It lets
you drag & resize other controls at runtime, and it works for me -
although some things could probably be done better, and I would like
to hear about them.

To use it, pass the name of the control you want to drag in the Attach
method.  You can set the color of the grabhandles via the Color property.
The sample project uses the Click events of several components to attach
the same TStretchHandle component to them (it detaches automatically
before adopting a new kid).

I couldn't find a better way to draw a runtime rubberband over everything
else than by means of yet another transparent control, called TOverlay,
used only internally.  It causes the form repaint when you attach the
handles to controls on the form, since it sizes itself to the Parent
of the control being managed.

Any sugestions, thoughts to:

Anthony Scott
2335 N. Spaulding
Chicago IL 60647

CIS: 75567,3547

PS: to install, run HANDLES.PAS from Options|Install Components, as per
    usual... to test without installing, load & run PROJECT1.DPR.

