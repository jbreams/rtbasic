#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

#include "llvm/ADT/StringRef.h"

#include "lexer.h"

Token::Token(Lexer* lexer, Tag value, std::string strValue, Token::NumValue numValue)
    : tag(value),
      value(std::move(numValue)),
      strValue(std::move(strValue)),
      line(lexer->_line),
      startPos(lexer->_lastPos),
      endPos(lexer->_pos),
      lineNumber(lexer->_lineCount) {}

using StringToTagMap = std::unordered_map<std::string, Token::Tag>;
const StringToTagMap& stringToTagMap() {
    static const StringToTagMap _stringToTag = {
        {"AND", Token::And},           {"AS", Token::As},
        {"DIM", Token::Dim},           {"DOUBLE", Token::DoubleType},
        {"ELSE", Token::Else},         {"END", Token::End},
        {"EXTERN", Token::Extern},     {"FOR", Token::For},
        {"FUNCTION", Token::Function}, {"GOSUB", Token::Gosub},
        {"GOTO", Token::Goto},         {"IF", Token::If},
        {"INPUT", Token::Input},       {"INTEGER", Token::IntegerType},
        {"LET", Token::Let},           {"OR", Token::Or},
        {"NEXT", Token::Next},         {"PRINT", Token::Print},
        {"RETURN", Token::Return},     {"STEP", Token::Step},
        {"STRING", Token::StringType}, {"SUB", Token::Sub},
        {"THEN", Token::Then},         {"TO", Token::To},
        {"WHILE", Token::While},       {"WEND", Token::Wend},
        {"NOT", Token::Not},           {"DO", Token::Do},
        {"LOOP", Token::Loop},         {"UNTIL", Token::Until},
    };
    return _stringToTag;
}

using CharToTagMap = std::unordered_map<char, Token::Tag>;
const CharToTagMap& charToTagMap() {
    static const CharToTagMap _charToTag = {
        {'<', Token::Lt},
        {'>', Token::Gt},
        {'=', Token::Eq},
        {'+', Token::Plus},
        {'-', Token::Minus},
        {'/', Token::Divide},
        {'*', Token::Multiply},
        {'^', Token::Exp},
        {',', Token::Comma},
        {'(', Token::LParens},
        {')', Token::RParens},
    };
    return _charToTag;
}

std::ostream& operator<<(std::ostream& stream, const Token& tok) {
    if (!tok.strValue.empty()) {
        stream << "Tag: " << tok.tag << " value: \"" << tok.strValue << "\"";
    } else if (!tok.value.has_value()) {
        stream << "Tag: " << tok.tag;
        if (tok.tag == Token::Eof) {
            stream << " value: Eof";
        } else if (tok.tag == Token::Newline) {
            stream << " value: Newline";
        }
    } else {
        stream << "Tag: " << tok.tag << " value: ";
        mpark::visit([&](const auto val) { stream << val; }, *tok.value);
    }

    auto prefix = tok.line->substr(0, tok.startPos);
    auto token = tok.line->substr(tok.startPos, tok.endPos - tok.startPos);
    auto suffix = tok.line->substr(tok.endPos);

    stream << " Occurred on line " << tok.lineNumber << ": \"" << prefix << "\" --> \"" << token
           << "\" <-- \"" << suffix << "\"";
    return stream;
}

LexerError::LexerError(Lexer* lexer, const char* message) {
    std::stringstream ss;
    ss << message << " (error at " << lexer->_lineCount << ":" << lexer->_pos << ": \""
       << lexer->_line->substr(0, lexer->_pos) << "\" -> \"" << lexer->_line->substr(lexer->_pos)
       << "\")";
    _message = ss.str();
}

Token Lexer::_extractNumber() {
    auto ptr = _line->c_str() + _pos;
    char* endPtr = nullptr;
    int64_t intVal = 0;
    double intPart;
    double dblVal = std::strtod(ptr, &endPtr);
    llvm::StringRef ref(ptr, endPtr - ptr);

    bool isDouble = true;

    if (dblVal == HUGE_VAL) {
        intVal = std::strtoll(ptr, &endPtr, 10);
        isDouble = false;
    } else if (!ref.contains(".") && std::modf(dblVal, &intPart) == 0) {
        intVal = intPart;
        isDouble = false;
    }
    _pos = (endPtr - ptr) + _pos;

    if (isDouble) {
        return Token(this, Token::Double, dblVal);
    } else {
        return Token(this, Token::Integer, intVal);
    }
}

long Lexer::_extractInt(int base) {
    auto ptr = _line->c_str() + _pos;
    char* endPtr = nullptr;
    double val = std::strtol(ptr, &endPtr, base);
    _pos = (endPtr - ptr) + _pos;

    return val;
}

std::string Lexer::_extractEscapedString(char endAt) {
    std::stringstream ss;
    _pos++;
    char prev;

    do {
        prev = _line->at(_pos++);
        if (prev == '\\') {
            auto escape = _line->at(_pos++);
            switch (escape) {
                case 't':
                    ss << '\t';
                    break;
                case 'n':
                    ss << '\n';
                    break;
                case 'r':
                    ss << '\r';
                    break;
                case 'b':
                    ss << '\b';
                    break;
                case 'a':
                    ss << '\a';
                    break;
                case 'f':
                    ss << '\f';
                    break;
                case '\'':
                    ss << '\'';
                    break;
                case '\"':
                    ss << '\"';
                    break;
                case '\\':
                    ss << '\\';
                    break;
                case 'v':
                    ss << 'v';
                    break;
                case '?':
                    ss << '?';
                    break;
                case 'x': {
                    _pos++;
                    char val = _extractInt(16);
                    ss << val;
                } break;
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9': {
                    char val = _extractInt();
                    ss << val;
                } break;
            }
        } else if (prev != endAt) {
            ss << prev;
        }
    } while (prev != endAt);

    return ss.str();
}

Token Lexer::lex() {
    auto ret = _lex();
    return ret;
}

Token Lexer::_lex() {
    if (!_putBackTokens.empty()) {
        auto ret = std::move(_putBackTokens.front());
        _putBackTokens.pop_front();
        return ret;
    }

    auto& stringToTag = stringToTagMap();
    auto& charToTag = charToTagMap();

    while (!_stream->eof() && _line->empty()) {
        _line = std::make_shared<std::string>();
        std::getline(*_stream, *_line);
        _lineCount++;
        _pos = 0;
        _seenToken = false;
    }

    if (_stream->eof()) {
        return Token(this, Token::Eof);
    }

    while (_pos < _line->size() && std::isspace(_line->at(_pos))) {
        _pos++;
    };

    _lastPos = _pos;
    if (_pos == _line->size()) {
        Token ret(this, Token::Newline);
        _line = std::make_shared<std::string>();
        return ret;
    }

    bool isFirstToken = [this] {
        auto old = _seenToken;
        _seenToken = true;
        return old == false;
    }();

    auto charIt = charToTag.find(_line->at(_pos));
    if (charIt != charToTag.end()) {
        if (charIt->second == Token::Minus && std::isdigit(_line->at(_pos + 1))) {
            return _extractNumber();
        } else if (charIt->second == Token::Gt) {
            auto nextChar = _line->at(++_pos);
            if (nextChar == '=') {
                _pos++;
                return Token(this, Token::Gte, ">=");
            } else if (nextChar == '<') {
                _pos++;
                return Token(this, Token::Neq, "><");
            } else {
                return Token(this, charIt->second, charIt->first);
            }
        } else if (charIt->second == Token::Lt) {
            auto nextChar = _line->at(++_pos);
            if (nextChar == '=') {
                _pos++;
                return Token(this, Token::Lte, "<=");
            } else if (nextChar == '>') {
                _pos++;
                return Token(this, Token::Neq, "<>");
            } else {
                return Token(this, charIt->second, charIt->first);
            }
        } else {
            return Token(this, charIt->second, _line->at(_pos++));
        }
    } else if (std::isdigit(_line->at(_pos))) {
        if (isFirstToken) {  // The first token seen on a line (that's a number) is a line label
            std::stringstream ss;
            ss << _extractInt();
            return Token(this, Token::Label, ss.str());
        }
        return _extractNumber();
    } else if (_line->at(_pos) == '\"') {
        return Token(this, Token::EscapedString, _extractEscapedString('\"'));
    } else if (_line->at(_pos) == '.') {
        auto ellipsis = _line->substr(_pos, 3);
        if (ellipsis == "...") {
            _pos += 3;
            return Token(this, Token::Ellipsis, ellipsis);
        } else {
            throw LexerError(this, "Expected ellipsis");
        }
    } else if (std::isalpha(_line->at(_pos))) {
        auto start = _pos;
        while (_pos < _line->size() && std::isalpha(_line->at(_pos))) {
            _pos++;
        }

        auto token = _line->substr(start, _pos - start);
        if (token == "REM") {
            auto end = _line->size();
            token = _line->substr(start, end);
            _pos = end;
            return Token(this, Token::Rem, token);
        }
        if (_pos < _line->size()) {
            if (isFirstToken && _line->at(_pos) == ':') {
                _pos++;
                return Token(this, Token::Label, token);
            } else if (_line->at(_pos) == '#' || _line->at(_pos) == '$' || _line->at(_pos) == '%') {
                token += _line->at(_pos++);
                return Token(this, Token::Variable, token);
            }
        }

        auto it = stringToTag.find(token);
        if (it == stringToTag.end()) {
            return Token(this, Token::String, std::move(token));
        } else {
            return Token(this, it->second, std::string(it->first));
        }
    } else {
        throw LexerError(this, "Unknown input");
    }
}
