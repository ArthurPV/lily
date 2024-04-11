/*
 * MIT License
 *
 * Copyright (c) 2022-2024 ArthurPV
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <base/assert.h>
#include <base/dir.h>
#include <base/yaml.h>

#include <core/cc/ci/config.h>
#include <core/cc/ci/generator.h>
#include <core/cc/ci/include.h>
#include <core/cc/ci/parser.h>
#include <core/cc/ci/result.h>
#include <core/cc/ci/scanner.h>

#include <stdio.h>

#define CI_CONFIG "CI.yaml"

int
main(int argc, char **argv)
{
    // TODO: implement a real CLI, like in bin/lily/main.c, bin/lilyc/main.c

    CIConfig config = parse__CIConfig();

    // TODO: Get the files from YAML config
    if (argc < 2) {
        printf("ci [ci_files...]\n");
        exit(1);
    }

    CIResult result = NEW(CIResult);

    load_builtin__CIResult(&result, &config);

    for (Usize i = 1; i < argc; ++i) {
        add_and_run__CIResult(&result, strdup(argv[i]), config.standard);
    }

    run__CIGenerator(&result);

    FREE(CIResult, &result);
    FREE(CIConfig, &config);

    destroy__CIInclude();
}
