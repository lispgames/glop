#import <Appkit/Appkit.h>

typedef void(*GlopEventCallback)(NSEvent*);


@interface GlopWindowResponder : NSResponder <NSWindowDelegate>
{
  GlopEventCallback eventCallback;
}

- (id)initWithEventCallback:(GlopEventCallback)callback;
@end
