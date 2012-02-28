#include "cbits.h"

void hsCFStringGetCharacters(CFStringRef theString, CFIndex len, UniChar *buffer) {
  CFStringGetCharacters(theString, CFRangeMake(0, len), buffer);
}
