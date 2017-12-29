#pragma once

#include <boost/variant.hpp>
#include <deque>
#include <istream>
#include <ostream>
#include <string>

class Lexer;
struct Token {
    virtual ~Token() = default;

    enum Tag {
        Double,
        Integer,
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
        Variable,
        Dim,

        IntegerType,
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
        And,
        Or,

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

        MaxTag
    };

    Token() = default;
    explicit Token(Lexer* lexer, Tag tag) : Token(lexer, tag, std::string(), NumValue()) {}
    explicit Token(Lexer* lexer, Tag tag, std::string extra)
        : Token(lexer, tag, std::move(extra), NumValue()) {}
    template <typename T,
              typename = std::enable_if_t<!std::is_same<T, std::string>::value &&
                                          !std::is_same<T, const char*>::value>>
    explicit Token(Lexer* lexer, Tag tag, T extra)
        : Token(lexer, tag, std::string(), std::move(extra)) {}

    bool isEnding() const {
        return tag == Newline || tag == Eof;
    }

    bool isFunctionDef() const {
        return tag == Sub || tag == Extern || tag == Function;
    }

    Tag tag = MaxTag;
    using NumValue = boost::variant<int64_t, double, char>;
    NumValue value;
    std::string strValue;

    std::shared_ptr<std::string> line;
    int startPos;
    int endPos;
    int lineNumber;

private:
    explicit Token(Lexer* lexer, Tag value, std::string strValue, NumValue numValue);
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
    explicit Lexer(std::istream* stream) : _line(std::make_shared<std::string>()), _stream(stream) {
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
    friend struct Token;
    Token _extractNumber();
    long _extractInt(int base = 10);
    std::string _extractEscapedString(char endAt);

    std::shared_ptr<std::string> _line;
    std::istream* _stream;
    int _lineCount = 0;
    bool _seenToken = false;
    size_t _lastPos;
    size_t _pos;
    std::deque<Token> _putBackTokens;
};
