#import <Cocoa/Cocoa.h>

@interface CustomApplication : NSApplication
{
  bool shouldKeepRunning;
}

- (void)runIteration;
- (void)run;
- (void)terminate:(id)sender;

@end
