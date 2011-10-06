#import "GlopGLView.h"

@implementation GlopGLView

- (void)drawRect:(NSRect)dirtyRect
{
}

@end


GlopGLView *GlopGLViewInit (int x, int y, int width, int height,
                                NSOpenGLPixelFormat *pixelFormat)
{
  NSRect frame = NSMakeRect(x, y, width, height);
  return [[GlopGLView alloc] initWithFrame:frame pixelFormat:pixelFormat];
}

NSOpenGLContext *GlopGLViewOpenGLContext (GlopGLView *view)
{
  return [view openGLContext];
}

void GlopGLViewSetOpenGLContext (GlopGLView *view, NSOpenGLContext *context)
{
  [view setOpenGLContext:context];
}

void GlopGLViewClearGLContext (GlopGLView *view)
{
  [view clearGLContext];
}

void GlopGLViewSetNextResponder (GlopGLView *view, NSResponder *responder)
{
  [view setNextResponder:responder];
}
