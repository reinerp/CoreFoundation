#include "cbits.h"

void hsCFStringGetCharacters(CFStringRef theString, CFIndex len, UniChar *buffer) {
  CFStringGetCharacters(theString, CFRangeMake(0, len), buffer);
}

CFArrayRef hsCFArrayCreate(const CFTypeRef *values, CFIndex numValues) {
    CFArrayCreate(NULL, values, numValues, &kCFTypeArrayCallBacks);
}

void hsCFArrayGetValues(CFArrayRef theArray, CFIndex len, const CFTypeRef *values) {
  CFArrayGetValues(theArray, CFRangeMake(0, len), values);
}
