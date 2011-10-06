#import "GlopApp.h"


NSApplication *GlopAppSharedApplication ()
{
  return [GlopApp sharedApplication];
}

void GlopAppSetMainMenu (GlopApp *app, NSMenu *menu)
{
  [app setMainMenu:menu];
}

NSEvent *GlopAppNextEvent (GlopApp *app, bool blocking)
{
  return [app nextEventMatchingMask:NSAnyEventMask
           untilDate:(blocking ? [NSDate distantFuture] : [NSDate date])
           inMode:NSDefaultRunLoopMode
           dequeue:YES];
}

void GlopAppSendEvent (GlopApp *app, NSEvent *event)
{
  [app sendEvent:event];
}

void GlopAppUpdateWindows (GlopApp *app)
{
  [app updateWindows];
}

void GlopAppRun (GlopApp *app)
{
  [app run];
}

@implementation GlopApp

- (void)run
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  
  [self finishLaunching];
  
  shouldKeepRunning = YES;
  do
  {
    GlopAppSendEvent(self, GlopAppNextEvent(self, YES));
    GlopAppUpdateWindows(self);
  } while (shouldKeepRunning);
  
  [pool release];
}

- (void)terminate:(id)sender
{
  shouldKeepRunning = NO;
}

@end
