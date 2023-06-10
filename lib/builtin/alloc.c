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

#include <base/assert.h>
#include <base/platform.h>

#include <builtin/alloc.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#if defined(LILY_LINUX_OS) || defined(LILY_APPLE_OS)
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#elif defined(LILY_WINDOWS_OS)
#include <windows.h>
#else
#error "This OS is not yet supported"
#endif

void *
__align__$Alloc(void *mem, Usize align)
{
    ASSERT(align % 2 == 0);

    Uptr addr = (Uptr)mem;
    Uptr aligned_addr = (addr + align - 1) & ~(align - 1);

    return (void *)aligned_addr;
}

void *
__alloc__$Alloc(Usize size, Usize align)
{
#if defined(LILY_LINUX_OS) || defined(LILY_APPLE_OS)
    Usize max_capacity = sysconf(_SC_PHYS_PAGES) * sysconf(_SC_PAGESIZE);
#elif defined(LILY_WINDOWS_OS)
    MEMORYSTATUSEX status;
    status.dwLength = sizeof(status);
    GlobalMemoryStatusEx(&status);
    Usize max_capacity = status.ullTotalPhys;
#endif

    if (size > max_capacity) {
        perror("Lily(Fail): too much memory allocation allocated");
        exit(1);
    }

#if defined(LILY_LINUX_OS) || defined(LILY_APPLE_OS)
    int fd = open("/dev/zero", O_RDWR);

    if (fd == -1) {
        perror("Lily(Fail): fail to open `/dev/zero`");
        exit(1);
    }

    void *mem = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

    if (mem == MAP_FAILED) {
        perror("Lily(Fail): fail to allocate memory");
        exit(1);
    }

    close(fd);
#elif defined(LILY_WINDOWS_OS)
    LPVOID mem =
      VirtualAlloc(NULL, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);

    if (!mem) {
        perror("Lily(Fail): fail to allocate memory");
        exit(1);
    }
#else
#error "This OS is not yet supported"
#endif

    if (align > 0) {
        mem = __align__$Alloc(mem, align);
    }

    return mem;
}

void *
__resize__$Alloc(void *mem, Usize new_size, Usize align)
{
    if (!mem) {
        perror(
          "Lily(Fail): fail to resize a pointer, because the value is NULL");
        exit(1);
    }

#if defined(LILY_LINUX_OS) || defined(LILY_APPLE_OS)
    Usize max_capacity = sysconf(_SC_PHYS_PAGES) * sysconf(_SC_PAGESIZE);
#elif defined(LILY_WINDOWS_OS)
    MEMORYSTATUSEX status;
    status.dwLength = sizeof(status);
    GlobalMemoryStatusEx(&status);
    Usize max_capacity = status.ullTotalPhys;
#else
#error "This OS is not yet supported"
#endif

    if (new_size > max_capacity) {
        perror("Lily(Fail): too much memory allocation allocated");
        exit(1);
    }

#if defined(LILY_LINUX_OS) || defined(LILY_APPLE_OS)
    int fd = open("/dev/zero", O_RDWR);

    if (fd == -1) {
        perror("Lily(Fail): fail to open `/dev/zero`");
        exit(1);
    }

    mem = mmap(mem, new_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

    if (mem == MAP_FAILED) {
        perror("Lily(Fail): fail to allocate memory");
        exit(1);
    }

    close(fd);
#elif defined(LILY_WINDOWS_OS)
    mem = VirtualAlloc(mem, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);

    if (!mem) {
        perror("Lily(Fail): fail to allocate memory");
        exit(1);
    }
#else
#error "This OS is not yet supported"
#endif

    if (align > 0) {
        mem = __align__$Alloc(mem, align);
    }

    return mem;
}

void
__free__$Alloc(void *mem, Usize size, Usize align)
{
    if (!mem) {
        perror("Lily(Fail): fail to free a pointer, because the value is NULL");
        exit(1);
    }

    if (align > 0) {
        mem = __align__$Alloc(mem, align);
    }

#if defined(LILY_LINUX_OS) || defined(LILY_APPLE_OS)
    if (munmap(mem, size) == -1) {
        perror("Lily(Fail): fail to free memory");
        exit(1);
    }
#elif defined(LILY_WINDOWS_OS)
    if (!VirtualFree(mem, 0, MEM_RELEASE)) {
        perror("Lily(Fail): fail to free memory");
        exit(1);
    }
#else
#error "This OS is not yet supported"
#endif

    mem = 0;
}