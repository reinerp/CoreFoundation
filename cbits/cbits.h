#include <CoreFoundation/CFString.h>
#include <CoreFoundation/CFArray.h>

void hsCFStringGetCharacters(CFStringRef theString, CFIndex len, UniChar *buffer);
CFArrayRef hsCFArrayCreate(const void **values, CFIndex numValues);
void hsCFArrayGetValues(CFArrayRef theArray, CFIndex len, const void **values);
CFDictionaryRef hsCFDictionaryCreate(const void **keys, const void **values, CFIndex numValues);
