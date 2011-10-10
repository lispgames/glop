#import "GlopView.h"

@implementation GlopView

- (id)initWithEventCallback:(GlopEventCallback)callback;
{
  eventCallback = callback;
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

- (void)windowWillClose:(NSNotification *)notification
{
}

@end


GlopView *GlopViewInit (GlopEventCallback callback)
{
  return [[GlopView alloc] initWithEventCallback:callback];
}

