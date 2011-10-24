#include <AppKit/AppKit.h>


/******************************************************************************/
/***                               NSColor                                  ***/
/******************************************************************************/


NSColor *NSColorBlackColor () { return [NSColor blackColor]; }
NSColor *NSColorBlueColor () { return [NSColor blueColor]; }
NSColor *NSColorBrownColor () { return [NSColor brownColor]; }
NSColor *NSColorCyanColor () { return [NSColor cyanColor]; }
NSColor *NSColorDarkGrayColor () { return [NSColor darkGrayColor]; }
NSColor *NSColorGrayColor () { return [NSColor grayColor]; }
NSColor *NSColorGreenColor () { return [NSColor greenColor]; }
NSColor *NSColorLightGrayColor () { return [NSColor lightGrayColor]; }
NSColor *NSColorMagentaColor () { return [NSColor magentaColor]; }
NSColor *NSColorOrangeColor () { return [NSColor orangeColor]; }
NSColor *NSColorPurpleColor () { return [NSColor purpleColor]; }
NSColor *NSColorRedColor () { return [NSColor redColor]; }
NSColor *NSColorWhiteColor () { return [NSColor whiteColor]; }
NSColor *NSColorYellowColor () { return [NSColor yellowColor]; }


/******************************************************************************/
/***                               NSEvent                                  ***/
/******************************************************************************/


NSUInteger NSEventGetType (NSEvent *event)
{
  return [event type];
}

unsigned short NSEventKeyCode (NSEvent *event)
{
  return [event keyCode];
}

NSUInteger NSEventModifierFlags (NSEvent *event)
{
  return [event modifierFlags];
}

NSWindow *NSEventWindow (NSEvent *event)
{
  return [event window];
}

void NSEventLocationInWindow (NSEvent *event, NSPoint *point)
{
  *point = [event locationInWindow];
}

NSInteger NSEventButtonNumber (NSEvent *event)
{
  return [event buttonNumber];
}

CGFloat NSEventDeltaX (NSEvent *event)
{
  return [event deltaX];
}

CGFloat NSEventDeltaY (NSEvent *event)
{
  return [event deltaY];
}


/******************************************************************************/
/***                            NSApplication                               ***/
/******************************************************************************/


void NSApplicationSharedApplication ()
{
  [NSApplication sharedApplication];
}


/******************************************************************************/
/***                              NSWindow                                  ***/
/******************************************************************************/


NSWindow *NSWindowAllocInit (int x, int y, int width, int height)
{
  NSWindow *window =
    [[NSWindow alloc]
       initWithContentRect:NSMakeRect(x, y, width, height)
                 styleMask:NSClosableWindowMask | NSTitledWindowMask
                   backing:NSBackingStoreBuffered
                     defer:NO];
  return window;
}

void NSWindowSetBackgroundColor (NSWindow *window, NSColor *color)
{
  [window setBackgroundColor:color];
}

void NSWindowSetTitle (NSWindow *window, NSString *title)
{
  [window setTitle:title];
}

void NSWindowMakeKeyAndOrderFront (NSWindow *window, id sender)
{
  [window makeKeyAndOrderFront:sender];
}

NSEvent *NSWindowNextEvent (NSWindow *window)
{
  return [window nextEventMatchingMask:NSAnyEventMask];
}

void NSWindowSetReleasedWhenClosed (NSWindow *window, BOOL state)
{
  [window setReleasedWhenClosed:state];
}

void NSWindowClose (NSWindow *window)
{
  [window close];
}

void NSWindowOrderOut (NSWindow *window, id sender)
{
  [window orderOut:sender];
}

void NSWindowSetContentView (NSWindow *window, NSView *view)
{
  [window setContentView:view];
}

void NSWindowSetDelegate (NSWindow *window, NSObject *delegate)
{
  [window setDelegate:(id <NSWindowDelegate>)delegate];
}

void NSWindowSetNextResponder (NSWindow *window, NSResponder *responder)
{
  [window setNextResponder:responder];
}

void NSWindowSetAcceptsMouseMovedEvents (NSWindow *window, BOOL acceptEvents)
{
  [window setAcceptsMouseMovedEvents:acceptEvents];
}

void NSWindowDiscardRemainingEvents (NSWindow *window)
{
  [window discardEventsMatchingMask:NSAnyEventMask
          beforeEvent:nil];
}

/******************************************************************************/
/***                                NSMenu                                  ***/
/******************************************************************************/


NSMenu *NSMenuAllocInit (NSString *title)
{
  return [[NSMenu alloc] initWithTitle:title];
}

void NSMenuAddItem (NSMenu *menu, NSMenuItem *item)
{
  [menu addItem:item];
}

void NSMenuAddItemWithTitle (NSMenu *menu, NSString *title, SEL selector,
                             NSString *keyEquiv)
{
  [menu addItemWithTitle:title action:selector keyEquivalent:keyEquiv];
}


/******************************************************************************/
/***                             NSMenuItem                                 ***/
/******************************************************************************/


NSMenuItem *NSMenuItemAllocInit (NSString *title, SEL selector,
                                 NSString *keyEquiv)
{
  return [[NSMenuItem alloc]
           initWithTitle:title
           action:selector
           keyEquivalent:keyEquiv];
}


/******************************************************************************/
/***                         NSOpenGLPixelFormat                            ***/
/******************************************************************************/


NSOpenGLPixelFormat *NSOpenGLPixelFormatInit
  (NSOpenGLPixelFormatAttribute *attribs)
{
  return [[NSOpenGLPixelFormat alloc] initWithAttributes:attribs];
}


/******************************************************************************/
/***                           NSOpenGLContext                              ***/
/******************************************************************************/


void NSOpenGLContextClearDrawable (NSOpenGLContext *context)
{
  [context clearDrawable];
}

void NSOpenGLContextFlushBuffer (NSOpenGLContext *context)
{
  [context flushBuffer];
}
