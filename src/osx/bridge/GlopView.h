#import <Cocoa/Cocoa.h>

typedef void(*GlopEventCallback)(NSEvent*);


@interface GlopView : NSView <NSWindowDelegate>
{
  GlopEventCallback eventCallback;
}

- (id)initWithEventCallback:(GlopEventCallback)callback;
@end
