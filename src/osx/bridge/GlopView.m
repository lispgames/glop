#import "GlopView.h"

@implementation GlopView

- (id)initWithEventCallback:(GlopEventCallback)eventCallbackFunc
             noticeCallback:(GlopNoticeCallback)noticeCallbackFunc;
{
  noticeCallback = noticeCallbackFunc;
  eventCallback = eventCallbackFunc;
  return [self init];
}

- (BOOL)acceptsFirstResponder
{
  return YES;
}

- (BOOL)canBecomeKeyView
{
  return YES;
}

- (BOOL)isOpaque
{
  return YES;
}

- (void)keyUp:(NSEvent *)event
{
  eventCallback(event);
}

- (void)keyDown:(NSEvent *)event
{
  eventCallback(event);
}

- (void)mouseDown:(NSEvent *)event
{
  eventCallback(event);
}

- (void)mouseUp:(NSEvent *)event
{
  eventCallback(event);
}

- (void)mouseMoved:(NSEvent *)event
{
  eventCallback(event);
}

- (void)mouseDragged:(NSEvent *)event
{
  eventCallback(event);
}

- (void)rightMouseDown:(NSEvent *)event
{
  eventCallback(event);
}

- (void)rightMouseUp:(NSEvent *)event
{
  eventCallback(event);
}

- (void)rightMouseDragged:(NSEvent *)event
{
  eventCallback(event);
}

- (void)otherMouseDown:(NSEvent *)event
{
  eventCallback(event);
}

- (void)otherMouseUp:(NSEvent *)event
{
  eventCallback(event);
}

- (void)otherMouseDragged:(NSEvent *)event
{
  eventCallback(event);
}

- (void)scrollWheel:(NSEvent *)event
{
  eventCallback(event);
}

- (void)flagsChanged:(NSEvent *)event
{
  eventCallback(event);
}

- (void)windowWillClose:(NSNotification *)notification
{
  GlopNotice *notice = malloc(sizeof(GlopNotice));
  notice->type = GlopNoticeWindowClose;
  notice->source = [notification object];
  noticeCallback(notice);
}

- (void)windowDidResize:(NSNotification *)notification
{
  GlopNotice *notice = malloc(sizeof(GlopNotice));
  notice->type = GlopNoticeWindowResize;
  notice->source = [notification object];
  noticeCallback(notice);
}

@end


GlopView *GlopViewInit (GlopEventCallback eventCallbackFunc,
                        GlopNoticeCallback noticeCallbackFunc)
{
  return [[GlopView alloc] initWithEventCallback:eventCallbackFunc
                                  noticeCallback:noticeCallbackFunc];
}

