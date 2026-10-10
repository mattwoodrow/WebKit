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

#include "Attachment.h"
#include "MessageNames.h"
#include <wtf/Forward.h>
#include <wtf/Vector.h>

namespace IPC {

class Decoder;
class Encoder;

template<typename> struct ArgumentCoder;

// A fully encoded message, carried as an argument of another message so that it can be
// relayed through a process and dispatched by the final recipient.
class WrappedMessage {
public:
    explicit WrappedMessage(UniqueRef<Encoder>&&);

    WrappedMessage(WrappedMessage&&) = default;
    WrappedMessage& operator=(WrappedMessage&&) = default;

    WrappedMessage copy() const;

    std::optional<MessageName> messageName() const;
    uint64_t destinationID() const;

    std::unique_ptr<Decoder> createDecoder() &&;
    // Returns an encoder that holds the message so far, to encode further arguments into it.
    std::unique_ptr<Encoder> createEncoder() &&;

private:
    friend struct ArgumentCoder<WrappedMessage>;
    WrappedMessage(Vector<uint8_t>&&, Vector<Attachment>&&);

    Vector<uint8_t> m_data;
    Vector<Attachment> m_attachments;
};

template<> struct ArgumentCoder<WrappedMessage> {
    static void encode(Encoder&, WrappedMessage&&);
    static std::optional<WrappedMessage> decode(Decoder&);
};

} // namespace IPC
