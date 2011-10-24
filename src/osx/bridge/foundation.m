#include <Foundation/Foundation.h>


/******************************************************************************/
/***                           NSAutoreleasePool                            ***/
/******************************************************************************/


NSAutoreleasePool *NSAutoreleasePoolAllocInit ()
{
  return [[NSAutoreleasePool alloc] init];
}

NSObject *NSAutorelease (NSObject *object)
{
  return [object autorelease];
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

NSString *NSStringAllocInitWithCString (char *string, NSStringEncoding encoding)
{
  return [[NSString alloc] initWithCString:string encoding:encoding];
}
