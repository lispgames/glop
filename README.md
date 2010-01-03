Glop README
===========

The goal is to provide simple OpenGL window and context manipulation code as well as system
input handling (i.e. mouse & keyboard).

Direct FFI bindings to system functions are used so no third party C lib is required
except system libraries.

Dependencies
------------
 - CFFI

Supported CL implementations/platforms
--------------------------------------
 - CLISP/X11
 - CLISP/Win32
 - SBCL/X11
 
Running the tests
-----------------
Make sure `glop.asd` and `glop-test.asf`  are in a location known to asdf and run:

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
 - `test-multiple-windows`: two hello world windows
 
In all tests except `test-manual-events` you can press the ESC key to close the window
and the 'f' key to toggle fullscreen.

Usage
-----
To use glop, make sure `glop.asd`  is in a location known to asdf and run:

    (asdf:operate 'asdf:load-op :glop)
    
Now you can just do:

    (glop:with-window (win "My title" 800 600)
        ;; gl init code here
        (loop while (glop:dispatch-events win :blocking nil) do
           ;; gl code here
           (glop:swap-buffers win)))

See `test.lisp` for a few examples.

