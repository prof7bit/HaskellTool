HaskellTool
===========

HaskellTool is intended as a simple text editor for editing 
Haskell Code. It has an integrated GHCi console and a few 
convenient keyboard shortcuts to load/reload the current editor 
file into the Haskell interpreter, it will highlight compilation
errors directly in the source code and also has the ability to
show type hints for identifiers on mouse-over and navigate to
the source location where they are defined.

Since this project is only in an early experimental pre-alpha
state most of the above mentioned features are only planned
and not yet implemented ;-)

This software is mainly written in Pascal (using Lazarus) 
because this is still by far the most advanced way to quickly 
get a feature-rich native and cross platform graphical user 
interface with no dependency hell up and running but it is 
planned to implement parts of it in Haskell, especially the 
parts that need to interact directly with the GHC API.
