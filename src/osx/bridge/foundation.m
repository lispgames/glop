#include <Foundation/Foundation.h>


/******************************************************************************/
/***                           NSAutoreleasePool                            ***/
/******************************************************************************/


NSAutoreleasePool *NSAutoreleasePoolAllocInit ()
{
  return [[NSAutoreleasePool alloc] init];
}

void NSAutoreleasePoolRelease (NSAutoreleasePool *pool)
{
  [pool release];
}

/******************************************************************************/
/***                               NSArray                                  ***/
/******************************************************************************/


NSUInteger NSArrayCount (NSArray *array)
{
  return [array count];
}

void *NSArrayObjectAtIndex (NSArray *array, NSUInteger index)
{
  return [array objectAtIndex:index];
}


/******************************************************************************/
/***                               NSString                                 ***/
/******************************************************************************/


const char *NSStringCStringUsingEncoding (NSString *string,
                                          NSStringEncoding encoding)
{
  return [string cStringUsingEncoding:encoding];
}
