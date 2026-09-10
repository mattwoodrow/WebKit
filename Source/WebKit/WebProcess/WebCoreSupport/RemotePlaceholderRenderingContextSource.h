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

#if ENABLE(OFFSCREEN_CANVAS)

#include <WebCore/PlaceholderRenderingContextSource.h>

namespace WebKit {

// The offscreen half of an OffscreenCanvas whose placeholder canvas element lives in a different
// web content process, as happens when a canvas is transferred to a cross-site iframe under site
// isolation. Each committed frame is shared with the UI process as an ImageBufferBackendHandle
// (an IOSurface send right where available), which applies it to the placeholder's compositing
// layer and relays it to the process owning the placeholder for the canvas element itself.
class RemotePlaceholderRenderingContextSource final : public WebCore::PlaceholderRenderingContextSource {
public:
    static Ref<RemotePlaceholderRenderingContextSource> create(WebCore::PlaceholderRenderingContextIdentifier);

private:
    explicit RemotePlaceholderRenderingContextSource(WebCore::PlaceholderRenderingContextIdentifier);

    void setPlaceholderBuffer(WebCore::ImageBuffer&, bool originClean, bool opaque, LayerUpdate) final;
};

} // namespace WebKit

#endif // ENABLE(OFFSCREEN_CANVAS)
