Glop
====

The goal is to provide simple OpenGL window and context manipulation code as well as system
input handling (i.e. mouse & keyboard).

Direct FFI bindings to system functions are used so no third party C lib is required
except system libraries.

Dependencies
------------
 - CFFI

Tested CL implementations/platforms
--------------------------------------
 - CLISP/X11
 - CLISP/Win32 (Windows XP)
 - SBCL/X11
 - CCL/X11
 - ECL/X11
 
Running the tests
-----------------
Make sure `glop.asd` and `glop-test.asd`  are in a location known to asdf and run:

    (asdf:operate 'asdf:load-op :glop-test)
    
Then you can run an hello world test with:

    (glop-test:test-gl-hello)
    
Available tests are:

 - `test-manual-create`: manual window create/destroy
 - `test-multiple-contexts`: multiple OpenGL contexts for a single window
 - `test-with-window`: glop:with-window macro usage
 - `test-manual-events`: manual event dispatching
 - `test-gl-hello`: cl-opengl hello world example
 - `test-gl-hello-fullscreen`: same in fullscreen
 - `test-gl-hello-gl3`: same with OpenGL 3.x context
 - `test-multiple-windows`: two hello world windows each one with its own GL context
 - `test-on-event`: hello world using the on-event dispatch code
 - `test-subclassing`: how to make your own window class and use it
 
In all tests except `test-manual-events` you can press the following keys:

 - ESC: close the window
 - 'f': toggle fullscreen mode (change display mode)
 - 'g': set window to fullscreen state (no display mode change)
 - 'h': hide mouse cursor
 - 'j': show mouse cursor

Quick start
-----------
To use glop, make sure `glop.asd`  is in a location known to asdf and run:

    (asdf:operate 'asdf:load-op :glop)
    
Now you can just do:

    (glop:with-window (win "My title" 800 600)
        ;; gl init code here
        (loop while (glop:dispatch-events win :blocking nil) do
           ;; gl code here
           (glop:swap-buffers win)))

The `glop:dispatch-events` macro will take care of processing glop events and call corresponding
methods. Generic functions for these methods are:

 - `(on-key window pressed keycode keysym string)`
 - `(on-button window pressed button)`
 - `(on-mouse-motion window x y dx dy)`
 - `(on-resize window new-width new-height)`
 - `(on-draw window)`
 - `(on-close window)`
 
None of them have a default definition, so you should implement all these methods in you application.

There's another method based dispatch mechanism with the `on-event` generic function.
To use it just pass `:on-foo nil` to `glop:dispatch-events`.
In that case the `(on-event window event)` method will be called instead of `on-foo` methods.

The `glop:dispatch-events` macro isn't mandatory and you can use your own event dispatch code,
see `glop-test:test-manual-events` for an example of how to do this.

You may also completely bypass glop's event handling mechanism and use your own,
see `glop-test:test-custom-event-loop` (X11 only) for a simple example of how it may be done.
Basically just don't call any of glop's event related functions and do the work yourself.

See `test.lisp` for more details.

Notes
-----
 
See also [issues](http://github.com/patzy/glop/issues) on github.


