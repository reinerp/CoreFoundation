#include <CoreFoundation/CFString.h>
#include <CoreFoundation/CFArray.h>

void hsCFStringGetCharacters(CFStringRef theString, CFIndex len, UniChar *buffer);
CFArrayRef hsCFArrayCreate(const CFTypeRef *values, CFIndex numValues);
void hsCFArrayGetValues(CFArrayRef theArray, CFIndex len, const CFTypeRef *values);
