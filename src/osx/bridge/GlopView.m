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

- (void)sendGlopNotice:(GlopNoticeType)type
{
  GlopNotice *notice = malloc(sizeof(GlopNotice));
  notice->type = type;
  notice->source = [self window];
  noticeCallback(notice);
}

- (void)windowWillClose:(NSNotification *)notification
{
  [self sendGlopNotice:GlopNoticeWindowClose];
}

- (void)windowDidResize:(NSNotification *)notification
{
  [self sendGlopNotice:GlopNoticeWindowResize];
}

- (void)windowDidExpose:(NSNotification *)notification
{
  [self sendGlopNotice:GlopNoticeWindowExpose];
}

- (void)windowDidBecomeKey:(NSNotification *)notification
{
  [self sendGlopNotice:GlopNoticeWindowFocus];
}

- (void)windowDidResignKey:(NSNotification *)notification
{
  [self sendGlopNotice:GlopNoticeWindowUnfocus];
}

@end


GlopView *GlopViewInit (GlopEventCallback eventCallbackFunc,
                        GlopNoticeCallback noticeCallbackFunc)
{
  return [[GlopView alloc] initWithEventCallback:eventCallbackFunc
                                  noticeCallback:noticeCallbackFunc];
}
