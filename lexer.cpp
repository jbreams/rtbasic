#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

#include "lexer.h"

using StringToTagMap = std::unordered_map<std::string, Token::Tag>;
const StringToTagMap& stringToTagMap() {
    static const StringToTagMap _stringToTag = {
        {"AND", Token::And},
        {"AS", Token::As},
        {"DOUBLE", Token::DoubleType},
        {"ELSE", Token::Else},
        {"END", Token::End},
        {"EXTERN", Token::Extern},
        {"FOR", Token::For},
        {"FUNCTION", Token::Function},
        {"GOSUB", Token::Gosub},
        {"GOTO", Token::Goto},
        {"IF", Token::If},
        {"INPUT", Token::Input},
        {"LET", Token::Let},
        {"OR", Token::Or},
        {"NEXT", Token::Next},
        {"PRINT", Token::Print},
        {"RETURN", Token::Return},
        {"STEP", Token::Step},
        {"STRING", Token::StringType},
        {"SUB", Token::Sub},
        {"THEN", Token::Then},
        {"TO", Token::To},
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
        {'$', Token::Dollar},
        {'#', Token::Pound},
    };
    return _charToTag;
}

std::ostream& operator<<(std::ostream& stream, const Token& tok) {
    if (tok.value.empty()) {
        stream << "Token tag: " << tok.tag;
        if (tok.tag == Token::Eof) {
            stream << " Eof";
        } else if (tok.tag == Token::Newline) {
            stream << " Newline";
        }
    } else {
        stream << "Token tag: " << tok.tag << " value: " << tok.value;
    }
    return stream;
}

LexerError::LexerError(Lexer* lexer, const char* message) {
    std::stringstream ss;
    ss << message << " (error at " << lexer->_lineCount << ":" << lexer->_pos << ": \""
       << lexer->_line.substr(0, lexer->_pos) << "\" -> \"" << lexer->_line.substr(lexer->_pos)
       << "\")";
    _message = ss.str();
}

Token Lexer::_extractNumber() {
    auto ptr = _line.c_str() + _pos;
    char* endPtr = nullptr;
    int64_t intVal = 0;
    double intPart;
    double dblVal = std::strtod(ptr, &endPtr);
    bool isDouble = true;

    if (dblVal == HUGE_VAL) {
        intVal = std::strtoll(ptr, &endPtr, 10);
        isDouble = false;
    } else if (std::modf(dblVal, &intPart) == 0) {
        intVal = intPart;
        isDouble = false;
    }
    _pos = (endPtr - ptr) + _pos;

    if (isDouble) {
        return Token(Token::Double, dblVal);
    } else {
        return Token(Token::Integer, intVal);
    }
}

long Lexer::_extractInt(int base) {
    auto ptr = _line.c_str() + _pos;
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
        prev = _line[_pos++];
        if (prev == '\\') {
            auto escape = _line[_pos++];
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

    while (!_stream->eof() && _line.empty()) {
        std::getline(*_stream, _line);
        _lineCount++;
        _pos = 0;
        _seenToken = false;
    }

    if (_stream->eof()) {
        return Token(Token::Eof);
    }

    while (std::isspace(_line[_pos])) {
        _pos++;
    };

    if (_pos == _line.size()) {
        _line.clear();
        return Token(Token::Newline);
    }

    bool isFirstToken = [this] {
        auto old = _seenToken;
        _seenToken = true;
        return old == false;
    }();

    auto charIt = charToTag.find(_line[_pos]);
    if (charIt != charToTag.end()) {
        if (charIt->second == Token::Minus && std::isdigit(_line[_pos + 1])) {
            return _extractNumber();
        } else if (charIt->second == Token::Gt) {
            auto nextChar = _line[++_pos];
            if (nextChar == '=') {
                return Token(Token::Gte, ">=");
            } else if (nextChar == '<') {
                return Token(Token::Neq, "><");
            } else {
                return Token(charIt->second, charIt->first);
            }
        } else if (charIt->second == Token::Lt) {
            auto nextChar = _line[++_pos];
            if (nextChar == '=') {
                return Token(Token::Lte, "<=");
            } else if (nextChar == '>') {
                return Token(Token::Neq, "<>");
            } else {
                return Token(charIt->second, charIt->first);
            }
        } else {
            return Token(charIt->second, _line[_pos++]);
        }
    } else if (std::isdigit(_line[_pos])) {
        if (isFirstToken) {  // The first token seen on a line (that's a number) is a line label
            std::stringstream ss;
            ss << _extractInt();
            return Token(Token::Label, ss.str());
        }
        return _extractNumber();
    } else if (_line[_pos] == '\"') {
        return Token(Token::EscapedString, _extractEscapedString('\"'));
    } else if (_line[_pos] == '.') {
        auto ellipsis = _line.substr(_pos, 3);
        if (ellipsis == "...") {
            _pos += 3;
            return Token(Token::Ellipsis, ellipsis);
        } else {
            throw LexerError(this, "Expected ellipsis");
        }
    } else if (std::isalpha(_line[_pos])) {
        auto start = _pos;
        while (_pos < _line.size() && std::isalpha(_line[_pos])) {
            _pos++;
        }

        auto token = _line.substr(start, _pos - start);
        if (token == "REM") {
            auto end = _line.size();
            token = _line.substr(start, end);
            _pos = end;
            return Token(Token::Rem, token);
        }
        if (isFirstToken && _line[_pos] == ':') {
            _pos++;
            return Token(Token::Label, token);
        }

        auto it = stringToTag.find(token);
        if (it == stringToTag.end()) {
            return Token(Token::String, std::move(token));
        } else {
            return Token(it->second, std::string(it->first));
        }
    } else {
        throw LexerError(this, "Unknown input");
    }
}
