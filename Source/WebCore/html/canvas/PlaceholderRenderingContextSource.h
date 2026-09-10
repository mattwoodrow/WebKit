/*
 * Copyright (C) 2017-2026 Apple Inc. All rights reserved.
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

#include <WebCore/PlaceholderRenderingContextIdentifier.h>
#include <WebCore/PlatformLayerIdentifier.h>
#include <wtf/Function.h>
#include <wtf/Lock.h>
#include <wtf/Markable.h>
#include <wtf/MainThread.h>
#include <wtf/TZoneMalloc.h>
#include <wtf/ThreadSafeWeakPtr.h>
#include <wtf/WeakPtr.h>

namespace WebCore {

class GraphicsLayer;
class GraphicsLayerAsyncContentsDisplayDelegate;
class Document;
class ImageBuffer;
class PlaceholderRenderingContext;

// Thread-safe interface to submit frames from worker to the placeholder rendering context.
//
// When the OffscreenCanvas is transferred to a document hosted in another process, the offscreen
// side instead holds a remote subclass of this source, which forwards each frame back to the
// process owning the placeholder. That process looks the source up by identifier and calls
// setPlaceholderBuffer() on this class, so both the compositor and the canvas element are updated
// exactly as they are for a same-process transfer.
class PlaceholderRenderingContextSource : public ThreadSafeRefCountedAndCanMakeThreadSafeWeakPtr<PlaceholderRenderingContextSource> {
    WTF_MAKE_TZONE_ALLOCATED_EXPORT(PlaceholderRenderingContextSource, WEBCORE_EXPORT);
    WTF_MAKE_NONCOPYABLE(PlaceholderRenderingContextSource);
public:
    static Ref<PlaceholderRenderingContextSource> create(PlaceholderRenderingContext&);
    WEBCORE_EXPORT virtual ~PlaceholderRenderingContextSource();

    PlaceholderRenderingContextIdentifier identifier() const { return m_identifier; }

    // Returns the source with this identifier, if its placeholder lives in this process.
    WEBCORE_EXPORT static RefPtr<PlaceholderRenderingContextSource> sourceWithIdentifier(PlaceholderRenderingContextIdentifier);

    // Set by the WebKit layer so that a DetachedOffscreenCanvas decoded from another process can be
    // reconnected to the placeholder it left behind.
    using RemoteSourceFactory = Function<RefPtr<PlaceholderRenderingContextSource>(PlaceholderRenderingContextIdentifier)>;
    WEBCORE_EXPORT static void setRemoteSourceFactory(RemoteSourceFactory&&);
    static RefPtr<PlaceholderRenderingContextSource> createRemoteSource(PlaceholderRenderingContextIdentifier);

    // Set by the WebKit layer, so it can retire any cross-process access granted to a placeholder
    // that no longer exists. Only called for placeholders owned by this process.
    using DestructionHandler = Function<void(PlaceholderRenderingContextIdentifier)>;
    WEBCORE_EXPORT static void setDestructionHandler(DestructionHandler&&);

    // Set by the WebKit layer. Reports which compositing layer, if any, currently displays this
    // placeholder, so that a frame committed from another process can be applied to it directly
    // rather than being relayed through this one. std::nullopt means "no longer composited".
    using LayerChangeHandler = Function<void(PlaceholderRenderingContextIdentifier, std::optional<PlatformLayerIdentifier>)>;
    WEBCORE_EXPORT static void setLayerChangeHandler(LayerChangeHandler&&);

    // Whether this frame still has to be pushed to the compositor. A frame arriving from another
    // process may already have been applied to the placeholder's layer by the UI process.
    enum class LayerUpdate : bool { Needed, AlreadyApplied };

    // Called by the offscreen context to submit the frame.
    WEBCORE_EXPORT virtual void setPlaceholderBuffer(ImageBuffer&, bool originClean, bool opaque, LayerUpdate = LayerUpdate::Needed);

    // Submits a frame that arrived from the process holding the offscreen half. That process
    // already took its own copy, and this one is on the main thread holding the only reference, so
    // this skips the clone and cross-thread hop that setPlaceholderBuffer() needs.
    WEBCORE_EXPORT void setPlaceholderBufferFromRemoteProcess(Ref<ImageBuffer>&&, bool originClean, bool opaque, LayerUpdate);

    // Called by the placeholder context to attach to compositor layer.
    void setContentsToLayer(GraphicsLayer&, ImageBuffer*, bool opaque);

    // The document holding the placeholder canvas element, when it lives in this process. Lets the
    // WebKit layer find the page whose rendering backend should own an incoming remote frame.
    WEBCORE_EXPORT Document* placeholderDocument() const;

protected:
    WEBCORE_EXPORT explicit PlaceholderRenderingContextSource(PlaceholderRenderingContextIdentifier);

private:
    explicit PlaceholderRenderingContextSource(PlaceholderRenderingContext&);
    void reportLayerChange(std::optional<PlatformLayerIdentifier>);

    const PlaceholderRenderingContextIdentifier m_identifier;
    WeakPtr<PlaceholderRenderingContext> m_placeholder; // For main thread use.
    Lock m_lock;
    RefPtr<GraphicsLayerAsyncContentsDisplayDelegate> m_delegate WTF_GUARDED_BY_LOCK(m_lock);
    unsigned m_bufferVersion { 0 }; // For OffscreenCanvas holder thread use (main or worker).
    unsigned m_delegateBufferVersion WTF_GUARDED_BY_LOCK(m_lock) { 0 };
    unsigned m_placeholderBufferVersion WTF_GUARDED_BY_CAPABILITY(mainThread) { 0 };
    Markable<PlatformLayerIdentifier> m_reportedLayerID WTF_GUARDED_BY_CAPABILITY(mainThread);
};

} // namespace WebCore

#endif // ENABLE(OFFSCREEN_CANVAS)
