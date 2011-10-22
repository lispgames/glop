#import <Cocoa/Cocoa.h>
#import <malloc/malloc.h>

typedef enum {
  GlopNoticeWindowClose,
  GlopNoticeWindowResize
} GlopNoticeType;

typedef struct glopNotice {
  GlopNoticeType type;
  NSWindow *source;
} GlopNotice;

typedef void(*GlopNoticeCallback)(GlopNotice*);
typedef void(*GlopEventCallback)(NSEvent*);


@interface GlopView : NSView <NSWindowDelegate>
{
  GlopEventCallback eventCallback;
  GlopNoticeCallback noticeCallback;
}

- (id)initWithEventCallback:(GlopEventCallback)eventCallbackFunc
             noticeCallback:(GlopNoticeCallback)noticeCallbackFunc;

@end
