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

void NSWindowMakeKeyAndOrderFront (NSWindow *window, id sender)
{
  [window makeKeyAndOrderFront:sender];
}

NSEvent *NSWindowNextEvent (NSWindow *window)
{
  return [window nextEventMatchingMask:NSAnyEventMask];
}


/******************************************************************************/
/***                               NSMenu                                   ***/
/******************************************************************************/


NSMenu *NSMenuAllocInit (NSString *title)
{
  return [[[NSMenu alloc] initWithTitle:title] autorelease];
}

void NSMenuAddItem (NSMenu *menu, NSMenuItem *item)
{
  [menu addItem:item];
}


/******************************************************************************/
/***                             NSMenuItem                                 ***/
/******************************************************************************/


//NSMenuItem *NSMenuItemInitWithTitle (NSString *title)
