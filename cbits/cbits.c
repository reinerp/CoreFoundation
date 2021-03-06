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

CFBooleanRef hsTrue() {
  return kCFBooleanTrue;
}

CFBooleanRef hsFalse() {
  return kCFBooleanFalse;
}

CFNumberType hsFloat64Type() {
  return kCFNumberFloat64Type;
}

CFNumberType hsInt64Type() {
  return kCFNumberSInt64Type;
}

CFStringRef hsAnyApp() {
  return kCFPreferencesAnyApplication;
}

CFStringRef hsAnyHost() {
  return kCFPreferencesAnyHost;
}

CFStringRef hsAnyUser() {
  return kCFPreferencesAnyUser;
}

CFStringRef hsCurrentApp() {
  return kCFPreferencesCurrentApplication;
}

CFStringRef hsCurrentHost() {
  return kCFPreferencesCurrentHost;
}

CFStringRef hsCurrentUser() {
  return kCFPreferencesCurrentUser;
}
