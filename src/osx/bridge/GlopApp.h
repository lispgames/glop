#import <Cocoa/Cocoa.h>

@interface GlopApp : NSApplication
{
  bool shouldKeepRunning;
}

- (void)run;
- (void)terminate:(id)sender;

@end
