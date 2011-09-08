#include <ApplicationServices/ApplicationServices.h>
#include <AppKit/AppKit.h>
#include <malloc/malloc.h>
#include <stdio.h>


typedef struct displayModeInfo {
  int width;
  int height;
  double refresh;
  int depth;
  CGDisplayModeRef mode;
} DisplayModeInfo;

typedef struct iterator {
  void *pointer;
} Iterator;

int displayModeGetDepth (CGDisplayModeRef ref)
{
  CFStringRef pixelEncoding = CGDisplayModeCopyPixelEncoding(ref);
  if (CFStringCompare(pixelEncoding, CFSTR(IO32BitDirectPixels),
                      kCFCompareCaseInsensitive) == kCFCompareEqualTo)
    return 32;
  else if (CFStringCompare(pixelEncoding, CFSTR(IO16BitDirectPixels),
                           kCFCompareCaseInsensitive) == kCFCompareEqualTo)
    return 16;
  else if (CFStringCompare(pixelEncoding, CFSTR(IO8BitIndexedPixels),
                           kCFCompareCaseInsensitive) == kCFCompareEqualTo)
    return 8;
  return -1;
}

void setDisplayInfo (const void *value, void *context)
{
  DisplayModeInfo *info
    = (DisplayModeInfo *)((Iterator *)context)->pointer;
  CGDisplayModeRef mode = (CGDisplayModeRef)value;
  info->width = CGDisplayModeGetWidth(mode);
  info->height = CGDisplayModeGetHeight(mode);
  info->refresh = CGDisplayModeGetRefreshRate(mode);
  info->depth = displayModeGetDepth(mode);
  info->mode = mode;
  ((Iterator *)context)->pointer += sizeof(DisplayModeInfo);
}

DisplayModeInfo *getDisplayModeInfoArray (long *size)
{
  CFArrayRef modeArray = CGDisplayCopyAllDisplayModes(CGMainDisplayID(), NULL);
  Iterator context;
  *size = (long)CFArrayGetCount(modeArray);
  DisplayModeInfo *array = malloc(sizeof(DisplayModeInfo) * *size);
  context.pointer = array;
  CFArrayApplyFunction(modeArray, (CFRange){0, *size},
                       setDisplayInfo, &context);
  return array;
}

NSAutoreleasePool *makeAutoreleasePool ()
{
  return [[NSAutoreleasePool alloc] init];
}

void releasePool (NSAutoreleasePool *pool)
{
  [pool release];
}

void initNSApp ()
{
  [NSApplication sharedApplication];
}

NSWindow *openWindow (int x, int y, int width, int height)
{
  NSWindow *window =
    [[[NSWindow alloc]
       initWithContentRect:NSMakeRect(x, y, width, height)
                 styleMask:NSClosableWindowMask | NSTitledWindowMask
                   backing:NSBackingStoreBuffered
                     defer:NO] autorelease];
  [window setBackgroundColor:[NSColor blueColor]];
  [window makeKeyAndOrderFront:NSApp];
}
