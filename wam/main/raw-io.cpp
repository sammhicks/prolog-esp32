#include "raw-io.h"

TimeoutException::TimeoutException() {}

const char *TimeoutException::what() const noexcept { return "Read Timeout"; }
