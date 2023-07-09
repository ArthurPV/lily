/*
 * MIT License
 *
 * Copyright (c) 2022-2023 ArthurPV
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

#include <base/alloc.h>

#include <core/lily/compiler/package/library.h>
#include <core/lily/compiler/package/package.h>

CONSTRUCTOR(LilyLibrary *,
            LilyLibrary,
            String *name,
            String *version,
            String *url,
            String *path,
            LilyPackage *package)
{
    LilyLibrary *self = lily_malloc(sizeof(LilyLibrary));

    self->name = name;
    self->version = version;
    self->url = url;
    self->path = path;
    self->package = package;

    return self;
}

DESTRUCTOR(LilyLibrary, LilyLibrary *self)
{
    FREE(String, self->name);
    FREE(String, self->version);

    if (self->url) {
        FREE(String, self->url);
    }

    FREE(String, self->path);
    FREE(LilyPackage, self->package);

    lily_free(self);
}