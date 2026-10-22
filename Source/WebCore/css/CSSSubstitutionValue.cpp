// Copyright 2015 The Chromium Authors. All rights reserved.
// Copyright (C) 2016-2021 Apple Inc. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//    * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//    * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "config.h"
#include "CSSSubstitutionValue.h"

#include "CSSPropertyParser.h"
#include "CSSSubstitutionParser.h"
#include "CSSVariableData.h"

namespace WebCore {

// The tokens are flat, including nested ones, and functionId() is CSSValueID::Invalid for anything that
// is not a function token, so a linear scan finds inherit() at any depth.
static bool containsInheritFunctionToken(const CSSVariableData& data)
{
    if (!data.context().cssInheritFunctionEnabled)
        return false;
    for (auto& token : data.tokens()) {
        if (token.functionId() == CSSValueID::Inherit)
            return true;
    }
    return false;
}

CSSSubstitutionValue::CSSSubstitutionValue(Ref<CSSVariableData>&& data, const CSSNamespacePrefixMap& namespacePrefixMap)
    : CSSValue(ClassType::Substitution)
    , m_data(WTF::move(data))
    , m_namespacePrefixMap(namespacePrefixMap)
    , m_containsInheritFunction(containsInheritFunctionToken(m_data))
{
    cacheSimpleReference();
}

Ref<CSSSubstitutionValue> CSSSubstitutionValue::create(const CSSParserTokenRange& range, const CSSNamespacePrefixMap& namespacePrefixMap, const CSSParserContext& context)
{
    return adoptRef(*new CSSSubstitutionValue(CSSVariableData::create(range, context), namespacePrefixMap));
}

Ref<CSSSubstitutionValue> CSSSubstitutionValue::create(Ref<CSSVariableData>&& data)
{
    return adoptRef(*new CSSSubstitutionValue(WTF::move(data), CSSNamespacePrefixMap { }));
}

bool CSSSubstitutionValue::equals(const CSSSubstitutionValue& other) const
{
    return arePointingToEqualData(m_data, other.m_data);
}

String CSSSubstitutionValue::customCSSText(const CSS::SerializationContext&) const
{
    if (m_stringValue.isNull())
        m_stringValue = m_data->serialize();
    return m_stringValue;
}

const CSSParserContext& CSSSubstitutionValue::context() const
{
    return m_data->context();
}

auto CSSSubstitutionValue::variableReferences() const -> const VariableReferences&
{
    if (m_variableReferences)
        return *m_variableReferences;

    VariableReferences references;
    auto& tokens = m_data->tokens();

    for (size_t i = 0; i < tokens.size(); ++i) {
        auto& token = tokens[i];
        if (token.type() != FunctionToken)
            continue;

        switch (token.functionId()) {
        case CSSValueID::Var: {
            size_t nameIndex = i + 1;
            while (nameIndex < tokens.size() && (tokens[nameIndex].type() == NonNewlineWhitespaceToken || tokens[nameIndex].type() == NewlineToken))
                ++nameIndex;
            if (nameIndex < tokens.size() && tokens[nameIndex].type() == IdentToken && CSSSubstitutionParser::isValidCustomPropertyName(tokens[nameIndex]))
                references.names.append(tokens[nameIndex].value().toAtomString());
            else
                references.dependsOnMoreThanNames = true;
            break;
        }
        case CSSValueID::Env:
        case CSSValueID::Inherit:
        case CSSValueID::Attr:
        case CSSValueID::RandomItem:
        case CSSValueID::If:
        case CSSValueID::Ident:
        case CSSValueID::InternalAutoBase:
            references.dependsOnMoreThanNames = true;
            break;
        default:
            // A dashed function -- a custom function -- may read any custom property.
            if (isCustomPropertyName(token.value()))
                references.dependsOnMoreThanNames = true;
            break;
        }
    }

    m_variableReferences = WTF::move(references);
    return *m_variableReferences;
}

void CSSSubstitutionValue::cacheSimpleReference()
{
    ASSERT(!m_simpleReference);

    auto range = m_data->tokenRange();

    auto functionId = range.peek().functionId();

    if (functionId == CSSValueID::InternalAutoBase) {
        range.consumeBlock();
        if (range.atEnd())
            m_simpleReference = SimpleReference { { }, CSSValueID::InternalAutoBase };
        return;
    }

    if (functionId != CSSValueID::Var && functionId != CSSValueID::Env)
        return;

    auto variableRange = range.consumeBlock();
    if (!range.atEnd())
        return;

    variableRange.consumeWhitespace();

    // var() names a <custom-property-name> and env() an <ident>. Anything else takes the full
    // substitution path, since var()'s name argument may be any <declaration-value>.
    auto& nameToken = variableRange.consumeIncludingWhitespace();
    if (nameToken.type() != IdentToken)
        return;
    if (functionId == CSSValueID::Var && !CSSSubstitutionParser::isValidCustomPropertyName(nameToken))
        return;

    // No fallback support on this path.
    if (!variableRange.atEnd())
        return;

    m_simpleReference = SimpleReference { nameToken.value().toAtomString(), functionId };
}

} // namespace WebCore
