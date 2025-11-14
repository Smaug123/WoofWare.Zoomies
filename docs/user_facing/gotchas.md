# Ways you can hold WoofWare.Zoomies wrong

## Do not call `Console.SetIn`

While the render loop is running, you must not call `Console.SetIn`.
If you do, unspecified things may go wrong, including (for example) the framework throwing an exception with "Cannot see if a key has been pressed when...".
(One specific way this can happen is if the console input starts out interactive, but then is changed to redirect from a file.)
