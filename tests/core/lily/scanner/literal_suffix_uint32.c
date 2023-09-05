#include "util.c"

#include <base/test.h>

CASE(literal_suffix_uint32, {
    RUN_SCANNER(FILE_LITERAL_SUFFIX_UINT32);
    SCANNER_ITERATOR();

    TEST_ASSERT_EQ(CURRENT()->kind, LILY_TOKEN_KIND_LITERAL_SUFFIX_UINT32);
    TEST_ASSERT_EQ(NEXT()->literal_suffix_uint16, 100);
    TEST_ASSERT_EQ(CURRENT()->kind, LILY_TOKEN_KIND_LITERAL_SUFFIX_UINT32);
    TEST_ASSERT_EQ(NEXT()->literal_suffix_uint16, 2);

    FREE_SCANNER();
});
