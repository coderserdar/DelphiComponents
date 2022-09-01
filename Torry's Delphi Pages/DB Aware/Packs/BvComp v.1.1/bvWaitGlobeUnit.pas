unit bvWaitGlobeUnit;

interface

{$ifdef LINUX}
 ERROR: resource format is not compatible with Linux
{$endif}

const findfileres='findfile';
      waitfileres='worker';

implementation

{$R bvwaitunit.res}

end.
