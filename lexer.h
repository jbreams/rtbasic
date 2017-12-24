#pragma once

#include <boost/variant.hpp>
#include <deque>
#include <istream>
#include <ostream>
#include <string>

struct Token {
    virtual ~Token() = default;

    enum Tag {
        Number,
        String,
        EscapedString,
        Newline,
        Eof,
        Label,
        Extern,
        Ellipsis,
        As,
        Sub,
        Function,

        StringType,
        DoubleType,

        Print,
        If,
        Then,
        For,
        To,
        Step,
        Next,
        Else,
        Goto,
        Input,
        Let,
        Gosub,
        Return,
        End,
        Rem,

        Lte,
        Gte,
        Neq,
        Lt,
        Gt,
        Eq,

        Plus,
        Minus,
        Divide,
        Multiply,
        Exp,

        Comma,
        LParens,
        RParens,

        Dollar,
        Percent,
        Pound,

        MaxTag
    };

    Token() = default;
    explicit Token(Tag value) : tag(value) {}
    template <typename T>
    explicit Token(Tag value, T extra) : tag(value), value(std::move(extra)) {}

    bool isEnding() const {
        return tag == Newline || tag == Eof;
    }

    bool isFunctionDef() const {
        return tag == Sub || tag == Extern || tag == Function;
    }

    Tag tag = MaxTag;
    boost::variant<std::string, double, char> value;
};

std::ostream& operator<<(std::ostream& stream, const Token& token);

class Lexer;
class LexerError : public std::exception {
public:
    LexerError(Lexer* lexer, const char* message);
    const char* what() const noexcept override {
        return _message.c_str();
    }

private:
    std::string _message;
};

class Lexer {
public:
    explicit Lexer(std::istream* stream) : _stream(stream) {
        stream->exceptions(std::ios_base::badbit);
    }

    Token lex();
    int lineCount() const {
        return _lineCount;
    }

    void putBack(Token tok) {
        _putBackTokens.push_back(std::move(tok));
    }

    Token peek() {
        auto ret = _lex();
        putBack(ret);
        return ret;
    }

private:
    Token _lex();

    friend class LexerError;
    double _extractNumber();
    long _extractInt(int base = 10);
    std::string _extractEscapedString(char endAt);

    std::string _line;
    std::istream* _stream;
    int _lineCount = 0;
    bool _seenToken = false;
    size_t _pos;
    std::deque<Token> _putBackTokens;
};
