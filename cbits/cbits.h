#include <CoreFoundation/CFString.h>
#include <CoreFoundation/CFArray.h>
#include <CoreFoundation/CFNumber.h>
#include <CoreFoundation/CFPreferences.h>

void hsCFStringGetCharacters(CFStringRef theString, CFIndex len, UniChar *buffer);
CFArrayRef hsCFArrayCreate(const void **values, CFIndex numValues);
void hsCFArrayGetValues(CFArrayRef theArray, CFIndex len, const void **values);
CFDictionaryRef hsCFDictionaryCreate(const void **keys, const void **values, CFIndex numValues);
CFBooleanRef hsFalse();
CFBooleanRef hsTrue();
CFNumberType hsFloat64Type();
CFNumberType hsInt64Type();
CFStringRef hsAnyApp();
CFStringRef hsAnyHost();
CFStringRef hsAnyUser();
CFStringRef hsCurrentApp();
CFStringRef hsCurrentHost();
CFStringRef hsCurrentUser();
