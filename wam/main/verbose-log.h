#pragma once

//#define VERBOSE_LOG
#define HEADLESS

#ifdef VERBOSE_LOG
#undef HEADLESS
#define VERBOSE(X) X
#else
#define VERBOSE(X)
#endif

#ifdef HEADLESS
#define LOG(X)
#else
#define LOG(X) X
#endif
