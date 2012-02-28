#include "cbits.h"

void hsCFStringGetCharacters(CFStringRef theString, CFIndex len, UniChar *buffer) {
  CFStringGetCharacters(theString, CFRangeMake(0, len), buffer);
}

CFArrayRef hsCFArrayCreate(const void **values, CFIndex numValues) {
    CFArrayCreate(NULL, values, numValues, &kCFTypeArrayCallBacks);
}

void hsCFArrayGetValues(CFArrayRef theArray, CFIndex len, const void **values) {
  CFArrayGetValues(theArray, CFRangeMake(0, len), values);
}

CFDictionaryRef hsCFDictionaryCreate(const void **keys, const void **values, CFIndex numValues) {
  CFDictionaryCreate(NULL, keys, values, numValues, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
}
