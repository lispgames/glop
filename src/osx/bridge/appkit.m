#include <AppKit/AppKit.h>
#include <malloc/malloc.h>
#include <CoreServices/CoreServices.h>
#include <Availability.h>
#include <unistd.h>
#include <string.h>


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

#if __MAC_OS_X_VERSION_MIN_REQUIRED < __MAC_10_11
NSUInteger NSEventModifierFlags (NSEvent *event)
{
  return [event modifierFlags];
}
#endif

NSWindow *NSEventWindow (NSEvent *event)
{
  return [event window];
}

NSPoint *NSEventLocationInWindow (NSEvent *event)
{
  NSPoint point = [event locationInWindow];
  NSPoint *ptr = malloc(sizeof(NSPoint));
  memcpy(ptr, &point, sizeof(NSPoint));
  return ptr;
}

NSPoint *NSEventMouseLocation ()
{
  NSPoint point = [NSEvent mouseLocation];
  NSPoint *ptr = malloc(sizeof(NSPoint));
  memcpy(ptr, &point, sizeof(NSPoint));
  return ptr;
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

NSString *NSEventCharacters (NSEvent *event)
{
  return [event characters];
}

void GlopSendNoticeEvent (NSWindow *window)
{
  NSTimeInterval time = AbsoluteToDuration(UpTime())/(NSTimeInterval)1000.0;
  NSEvent *event =
    [NSEvent otherEventWithType:NSApplicationDefined
                       location:NSMakePoint(0.0, 0.0)
                  modifierFlags:0
                      timestamp:time
                   windowNumber:window == NULL ? 0 : [window windowNumber]
                        context:NULL
                        subtype:0
                          data1:0
                          data2:0];
  [NSApp postEvent:event atStart:NO];
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
  [window setFrameTopLeftPoint:NSMakePoint(x, y)];
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

NSView *NSWindowContentView (NSWindow *window)
{
  return [window contentView];
}

// Implied by NSWindow and NSView without an actual protocol.
@protocol FrameProtocol
- (NSRect)frame;
@end

NSRect *NSFrameMethod (NSObject *object)
{
  NSRect *rect = malloc(sizeof(NSRect));
  NSRect tmp = [(id <FrameProtocol>)object frame];
  memcpy(rect, &tmp, sizeof(NSRect));
  return rect;
}

void NSWindowSetFrameTopLeftPoint (NSWindow *window, int x, int y)
{
  [window setFrameTopLeftPoint:NSMakePoint(x, y)];
}

void NSWindowSetFrame (NSWindow *window, int x, int y, int width, int height)
{
  [window setFrame:NSMakeRect(x, y, width, height) display:YES];
  [window setFrameTopLeftPoint:NSMakePoint(x, y)];
}

void NSWindowSetStyleMask (NSWindow *window, NSUInteger syleMask)
{
  [window setStyleMask:syleMask];
}

void NSWindowSetLevel (NSWindow *window, NSInteger level)
{
  [window setLevel:level];
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


NSOpenGLContext *NSOpenGLContextInit (NSOpenGLPixelFormat *format)
{
  return [[NSOpenGLContext alloc] initWithFormat:format
                                    shareContext:nil];
}

void NSOpenGLContextMakeCurrentContext (NSOpenGLContext *context)
{
  [context makeCurrentContext];
}

void NSOpenGLContextSetView (NSOpenGLContext *context, NSView *view)
{
  [context setView:view];
}

void NSOpenGLContextSetFullScreen (NSOpenGLContext *context)
{
  [context setFullScreen];
}

void NSOpenGLContextClearDrawable (NSOpenGLContext *context)
{
  [context clearDrawable];
}

void NSOpenGLContextFlushBuffer (NSOpenGLContext *context)
{
  [context flushBuffer];
}

void NSOpenGLContextUpdate (NSOpenGLContext *context)
{
  [context update];
}
