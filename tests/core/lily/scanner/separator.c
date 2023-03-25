#include "util.c"

#include <base/test.h>
#include <stdio.h>

SIMPLE(separator, {
        RUN_SCANNER("./tests/core/lily/scanner/input/separator.lily");
        SCANNER_ITERATOR();

        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_ARROW);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_DOLLAR);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_DOT_DOT_DOT);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_DOT);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_FAT_ARROW);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_HASHTAG);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_INVERSE_ARROW);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_L_BRACE);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_R_BRACE);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_L_PAREN);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_R_PAREN);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_L_HOOK);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_R_HOOK);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_SEMICOLON);
        TEST_ASSERT_EQ(NEXT()->kind, LILY_TOKEN_KIND_EOF);

        FREE_SCANNER();
});