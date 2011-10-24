#import "GlopWindowResponder.h"

@implementation GlopWindowResponder

- (id)initWithEventCallback:(GlopEventCallback)callback
{
  eventCallback = callback;
  return [self init];
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

- (void)rightMouseDown:(NSEvent *)event
{
  eventCallback(event);
}

- (void)rightMouseUp:(NSEvent *)event
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

- (void)scrollWheel:(NSEvent *)event
{
  eventCallback(event);
}

- (void)flagsChanged:(NSEvent *)event
{
  eventCallback(event);
}

/*
- (void)mouseDragged:(NSEvent *)event
{
  eventCallback(event);
}

- (void)rightMouseDragged:(NSEvent *)event
{
  eventCallback(event);
}

- (void)otherMouseDragged:(NSEvent *)event
{
  eventCallback(event);
}

- (void)mouseEntered:(NSEvent *)event
{
  eventCallback(event);
}

- (void)mouseExited:(NSEvent *)event
{
  eventCallback(event);
}
*/

- (void)windowWillClose:(NSNotification *)notification
{
}

@end

GlopWindowResponder *GlopWindowResponderInit (GlopEventCallback callback)
{
  return [[GlopWindowResponder alloc] initWithEventCallback:callback];
}
