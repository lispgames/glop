#import "CustomApplication.h"

@implementation CustomApplication

- (void)runIteration
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSEvent *event =
    [self nextEventMatchingMask:NSAnyEventMask
                      untilDate:[NSDate distantFuture]
                         inMode:NSDefaultRunLoopMode
                        dequeue:YES];
  [self sendEvent:event];
  [self updateWindows];

  [pool release];
}

- (void)run
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  
  [self finishLaunching];
  
  shouldKeepRunning = YES;
  do
  {
    [pool release];
    pool = [[NSAutoreleasePool alloc] init];
    
    NSEvent *event =
      [self nextEventMatchingMask:NSAnyEventMask
            untilDate:[NSDate distantFuture]
            inMode:NSDefaultRunLoopMode
            dequeue:YES];
    [self sendEvent:event];
    [self updateWindows];
  } while (shouldKeepRunning);
  
  [pool release];
}

- (void)terminate:(id)sender
{
  shouldKeepRunning = NO;
}

@end

NSApplication *CustomApplicationSharedApplication ()
{
  return [CustomApplication sharedApplication];
}

void CustomApplicationRunIteration (CustomApplication *app)
{
  [app runIteration];
}

void CustomApplicationRun (CustomApplication *app)
{
  [app run];
}
