/*
 * Copyright (C) 2026 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. AND ITS CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL APPLE INC. OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

#if ENABLE(GPU_PROCESS) && PLATFORM(COCOA)

#include "RemoteImageBufferSetProxy.h"
#include <WebCore/PlatformCALayerDelegatedContents.h>
#include <wtf/TypeCasts.h>

namespace WebKit {

// A fence for delegated contents that are finished by the GPU process, which can wait for it
// before forwarding the commit to the UI process.
class GPUProcessDelegatedContentsFence : public WebCore::PlatformCALayerDelegatedContentsFence {
public:
    // Returns false if this process has to wait for the fence instead.
    virtual bool addToGPUProcessFlushes(ThreadSafeImageBufferSetFlusher::GPUProcessFlushes&) = 0;

private:
    bool isGPUProcessDelegatedContentsFence() const final { return true; }
};

} // namespace WebKit

SPECIALIZE_TYPE_TRAITS_BEGIN(WebKit::GPUProcessDelegatedContentsFence)
    static bool isType(const WebCore::PlatformCALayerDelegatedContentsFence& fence) { return fence.isGPUProcessDelegatedContentsFence(); }
SPECIALIZE_TYPE_TRAITS_END()

#endif // ENABLE(GPU_PROCESS) && PLATFORM(COCOA)
