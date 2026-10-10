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

#include "config.h"
#include "WrappedMessage.h"

#include "ArgumentCoders.h"
#include "Decoder.h"
#include "Encoder.h"
#include <wtf/UniqueRef.h>

namespace IPC {

WrappedMessage::WrappedMessage(UniqueRef<Encoder>&& encoder)
    : m_data(encoder->span())
    , m_attachments(encoder->releaseAttachments())
{
}

WrappedMessage::WrappedMessage(Vector<uint8_t>&& data, Vector<Attachment>&& attachments)
    : m_data(WTF::move(data))
    , m_attachments(WTF::move(attachments))
{
}

WrappedMessage WrappedMessage::copy() const
{
    return { Vector<uint8_t> { m_data }, Vector<Attachment> { m_attachments } };
}

std::optional<MessageName> WrappedMessage::messageName() const
{
    auto decoder = Decoder::create(m_data.span(), [](auto) { }, { });
    if (!decoder)
        return std::nullopt;
    return decoder->messageName();
}

uint64_t WrappedMessage::destinationID() const
{
    auto decoder = Decoder::create(m_data.span(), [](auto) { }, { });
    return decoder ? decoder->destinationID() : 0;
}

std::unique_ptr<Decoder> WrappedMessage::createDecoder() &&
{
    // Decoders take attachments from the end, so they expect them in reverse order of encoding.
    m_attachments.reverse();
    return Decoder::create(m_data.span(), WTF::move(m_attachments));
}

std::unique_ptr<Encoder> WrappedMessage::createEncoder() &&
{
    auto messageName = this->messageName();
    if (!messageName)
        return nullptr;

    auto encoder = makeUnique<Encoder>(*messageName, destinationID());
    // The new encoder has written the same header, so the arguments keep their alignment.
    auto headerSize = encoder->span().size();
    if (headerSize > m_data.size())
        return nullptr;
    // Keep the message flags, which are the first byte.
    encoder->mutableSpan()[0] = m_data[0];
    encoder->encodeSpan(m_data.subspan(headerSize));
    for (auto& attachment : m_attachments)
        encoder->addAttachment(WTF::move(attachment));
    return encoder;
}

void ArgumentCoder<WrappedMessage>::encode(Encoder& encoder, WrappedMessage&& message)
{
    encoder << message.m_data.span();
    encoder << message.m_attachments.size();
    for (auto& attachment : message.m_attachments)
        encoder << WTF::move(attachment);
}

std::optional<WrappedMessage> ArgumentCoder<WrappedMessage>::decode(Decoder& decoder)
{
    auto data = decoder.decode<std::span<const uint8_t>>();
    auto attachmentCount = decoder.decode<size_t>();
    if (!data || !attachmentCount)
        return std::nullopt;

    Vector<Attachment> attachments;
    for (size_t i = 0; i < *attachmentCount; ++i) {
        auto attachment = decoder.decode<Attachment>();
        if (!attachment)
            return std::nullopt;
        attachments.append(WTF::move(*attachment));
    }
    return WrappedMessage { Vector<uint8_t> { *data }, WTF::move(attachments) };
}

} // namespace IPC
