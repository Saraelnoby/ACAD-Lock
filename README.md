# ACAD-Lock
AutoCAD script for locking down a drawing in native format so it can be supplied and reviewed, but not stolen from. (Not including line tracing the whole thing (which can always be done anyway)).

## Installation
Edit your existing acad.lsp file to include the .lsp in this package, or create yourself a new one with the below:
```
(defun s::startup ()
(load "lockup.lsp")
)
```
Then place the acad.lsp in your chosen AutoCAD file search path.

Place the lockup.lsp in your chosen AutoCAD file search path.

Not sure where you should put it? Example:
```
C:\Program Files\Autodesk\AutoCAD 2015
```
```
C:\Program Files\Autodesk\AutoCAD 2015\Acade
```
If you have trouble, read the AutoCAD documentation for where to put these files. ```DSETTINGS``` will allow you to check your current paths.

Optionally, you can place the lockup.lsp anywhere and load it via drop and drag, or menu for each drawing you wish to use it on.

An acad.lsp is included in the package for convenience for new users.

## Usage
The AutoCAD command to run is ```lockup```.
This will begin the prompts.

Follow the prompts and answer as required and it will begin locking up the drawing.

The AutoCAD command to undo the lock is ```undolockup```.