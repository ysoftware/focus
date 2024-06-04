// TODO: we're leaking a lot of memory
// TODO: $0 is looking weird

highlight_swift_syntax :: (using buffer: *Buffer) {
    lexer: Lexer;
    lexer.contents = cast(string) bytes;
    tokenize(*lexer);

    last_token: Token = ---;

    for token: lexer.tokens {
        if !(token.start == 0 && token.end == 0) {
            color := color_for_token_type(token.type, token.is_capitalized);
            memset(colors.data + token.start, xx color, token.end - token.start);
        } else {
            print("Warning: This token does not have correct cursor information: %\n", token);
        }

        if last_token.type == .IDENTIFIER && !last_token.is_capitalized
            && (token.type == #char "(" || token.type == #char "{") 
        {
            memset(colors.data + last_token.start, xx Code_Color.FUNCTION, last_token.end - last_token.start);
        }

        last_token = token;
    }
}

#scope_file

color_for_token_type :: (type: Token_Type, is_capitalized: bool) -> Code_Color {
    if type == {
    case .IDENTIFIER;
        if !is_capitalized {
            return .DEFAULT;
        } else {
            return .TYPE;
        }
    case .ATTRIBUTE;
        return .TYPE;
    case .DIRECTIVE;
        return .KEYWORD;
    case .BINDING;  
        return .TYPE;
    case .INOUT;
        return .VALUE_KEYWORD;
    case .NUMBER;
        return .VALUE;
    case .STRING;
        return .STRING;
    case .COMMENT;
        return .COMMENT;
    }

    if type < 200 {
        return .PUNCTUATION;
    } else if type > 200 && type < 250 {
        return .OPERATION;
    } else { // keywords
        return .KEYWORD;
    }

    return .DEFAULT;
}


tokenize :: (using lexer: *Lexer) {
    while is_not_eof(lexer) {

        if contents[cursor] == #char "\n" {
            token := Token.{ type = .LINE_BREAK, start = cursor, end = cursor + 1 };

            array_add(*tokens, token);
            cursor += 1;
            continue;
        }

        trim_left(lexer);
        if !is_not_eof(lexer) {
            break;
        }

        // comment // line
        if contents[cursor] == #char "/" && peek_next(lexer) == #char "/" {
            comment_start := cursor;
            while is_not_eof(lexer) {
                cursor += 1;
                if contents[cursor] == #char "\n" {
                    cursor += 1;
                    break;
                }
            }
            token := Token.{ type = .COMMENT, start = comment_start, end = cursor };
            array_add(*tokens, token);
            continue;
        }

        // comment /* block */
        if contents[cursor] == #char "/" && peek_next(lexer) == #char "*" {
            comment_start := cursor;
            comment_block_depth := 1;
            cursor += 2;
            while is_not_eof(lexer) {
                if contents[cursor] == #char "/" && peek_next(lexer) == #char "*" {
                    cursor += 1;
                    comment_block_depth += 1;
                } else if contents[cursor] == #char "*" && peek_next(lexer) == #char "/" {
                    cursor += 1;
                    comment_block_depth -= 1;
                }
                cursor += 1;
                if comment_block_depth == 0 {
                    break;
                }
            }
            token := Token.{ type = .COMMENT, start = comment_start, end = cursor };
            array_add(*tokens, token);
            continue;
        }

        // string literal
        if contents[cursor] == #char "\"" {
            builder: String_Builder;
            init_string_builder(*builder);
            defer reset(*builder);

            string_start := cursor;
            cursor += 1;

            while true {
                if !is_not_eof(lexer) {
                    cursor += 1;
                    break;
                }

                if contents[cursor] == #char "\"" {
                    cursor += 1;
                    break;
                }

                if contents[cursor] == #char "\\" {
                    cursor += 1;

                    if !is_not_eof(lexer) {
                        cursor += 1;
                        break;
                    }

                    if contents[cursor] == {
                    case #char "\"";
                        append(*builder, #char "\"");
                    case #char "'";
                        append(*builder, #char "'");
                    case #char "n";
                        append(*builder, #char "\n");
                    case #char "t";
                        append(*builder, #char "\t");
                    case #char "r";
                        append(*builder, #char "\r");
                    case #char "\\";
                        append(*builder, #char "\\");
                    case; // invalid case, so we handle it as \ (since we don't produce errors)
                        append(*builder, #char "\\");
                        continue;
                    }
                    cursor += 1;
                    continue;
                }

                append(*builder, contents[cursor]);
                cursor += 1;
            }

            token := Token.{ type = .STRING, start = string_start, end = cursor };
            token.value = builder_to_string(*builder);

            array_add(*tokens, token);
            continue;
        }

        // identifier or keyword, or other special identifiers
        if is_alpha_fast(contents[cursor]) 
            || is_any(contents[cursor], "_")
            || (is_any(contents[cursor], "@$&#") && is_alpha_fast(peek_next(lexer))
        ) {
            identifier_start := cursor;
            cursor += 1;

            while is_not_eof(lexer)
                && (is_alpha_fast(contents[cursor])
                    || is_digit(contents[cursor])
                    || contents[cursor] == #char "_")
            { 
                cursor += 1;
            }

            identifier := slice(contents, identifier_start, cursor - identifier_start);
            add_token_from_identifier(lexer, identifier, identifier_start, cursor);
            continue;
        }

        // number
        if is_digit(contents[cursor]) 
            || (contents[cursor] == #char "-" 
                && is_digit(peek_next(lexer)) 
                && peek_next(lexer, next=2) != #char "b")
        {
            builder: String_Builder;
            init_string_builder(*builder);
            defer reset(*builder);
            append(*builder, contents[cursor]);
            number_start := cursor;
            cursor += 1;

            has_decimal_point := false;
            has_exp := false;
            is_boolean := false;
            is_hex := false;

            while is_not_eof(lexer) {
                if contents[cursor] == #char "\n" || contents[cursor] == #char " " {
                    break;
                } else if is_boolean && !is_digit(contents[cursor]) {
                    break;
                } else if contents[cursor] == #char "p" {
                    if !is_hex || has_exp {
                        break;
                    }
                    has_exp = true;
                    if peek_next(lexer) == #char "-" {
                        append(*builder, contents[cursor]);
                        cursor += 1;
                    }
                } else if contents[cursor] == #char "." {
                    if (number_start - cursor == 1 && contents[number_start] == #char "-")
                        || has_decimal_point
                        || !is_digit(peek_next(lexer)) // also hex a-f
                        || has_exp
                        || is_boolean
                    {
                        break;
                    }
                    has_decimal_point = true;
                } else if contents[cursor] == #char "e" {
                    if has_exp {
                        break;
                    }
                    has_exp = true;
                    if peek_next(lexer) == #char "-" {
                        append(*builder, contents[cursor]);
                        cursor += 1;
                    }
                } else if number_start == cursor - 1 && contents[number_start] == #char "0" {
                    if contents[cursor] == #char "b" { 
                        if !is_digit(peek_next(lexer)) {
                            break;
                        }
                        is_boolean = true;
                    } else if contents[cursor] == #char "x" {
                        if !is_digit(peek_next(lexer)) 
                            && !(peek_next(lexer) & 0b1011111 >= #char "A" 
                                && peek_next(lexer) & 0b1011111 <= #char "F")
                        {
                            break;
                        }
                        is_hex = true;
                    }
                } else if is_hex {
                    if !is_digit(contents[cursor])
                        && !(contents[cursor] & 0b1011111 >= #char "A"
                            && contents[cursor] & 0b1011111 <= #char "F")
                    {
                        break;
                    }
                } else if !(is_digit(contents[cursor]) || contents[cursor] == #char "_") {
                    break;
                }
                
                append(*builder, contents[cursor]);
                cursor += 1;
            }

            token := Token.{ type = .NUMBER, start = number_start, end = cursor };
            token.value = builder_to_string(*builder);

            array_add(*tokens, token);
            continue;
        }

        // operators
        if contents[cursor] < 127 && contents[cursor] > 32 {
            token := Token.{ type = xx contents[cursor], start = cursor };

            if contents[cursor] == {
            case #char "<";
                if peek_next(lexer) == {
                case #char "<";
                    token.type = .SHIFT_LEFT;
                case #char "=";
                    token.type = .LESS_EQUALS;
                }
            case #char ">";
                if peek_next(lexer) == {
                case #char ">";
                    token.type = .SHIFT_RIGHT;
                case #char "=";
                    token.type = .GREATER_EQUALS;
                }
            case #char "=";
                if peek_next(lexer) == #char "=" {
                    if peek_next(lexer, next=2) == #char "=" {
                        token.type = .TRIPLE_EQUALS;
                    } else {
                        token.type = .IS_EQUAL;
                    }
                }
            case #char "!";
                if peek_next(lexer) == #char "=" {
                    token.type = .IS_NOT_EQUAL;
                }
            case #char "+";
                if peek_next(lexer) == #char "=" {
                    token.type = .PLUS_EQUALS;
                }
            case #char "-";
                if peek_next(lexer) == { 
                case #char "=";
                    token.type = .MINUS_EQUALS;
                case #char ">";
                    token.type = .RIGHT_ARROW;
                }
            case #char "*";
                if peek_next(lexer) == #char "=" {
                    token.type = .TIMES_EQUALS;
                }
            case #char "/";
                if peek_next(lexer) == #char "=" {
                    token.type = .DIV_EQUALS;
                }
            case #char "%";
                if peek_next(lexer) == #char "=" {
                    token.type = .MODE_EQUALS;
                }
            case #char "&";
                if peek_next(lexer) == {
                case #char "&";
                    token.type = .LOGICAL_AND;
                case #char "=";
                    token.type = .LOGICAL_AND_EQUALS;
                }
            case #char "|";
                if peek_next(lexer) == {
                case #char "|";
                    token.type = .LOGICAL_OR;
                case #char "=";
                    token.type = .LOGICAL_OR_EQUALS;
                }
            case;
                token.type = xx contents[cursor];
            }
            
            cursor += 1;
            if token.type >= 200 {
                cursor += 1;
            }
            if token.type >= 240 {
                cursor += 1;
            }

            token.end = cursor;
            array_add(*tokens, token);
            continue;
        }

        cursor += 1;
        trim_left(lexer);
    }

    token: Token;
    token.type = .EOF;
    token.start = cursor;
    token.end = cursor + 1;
    array_add(*tokens, token);
}

peek_next :: (using lexer: Lexer, next: s64 = 1) -> u8 {
    if is_not_eof(lexer, with_next=next) {
        return contents[cursor + next];
    }
    return 0;
}

is_not_eof :: (using lexer: Lexer, with_next: s64 = 0) -> bool {
    return cursor + with_next <= contents.count - 1;
}

trim_left :: (lexer: *Lexer) {
    while is_not_eof(lexer) && is_any(lexer.contents[lexer.cursor], DEFAULT_SPACES) {
        lexer.cursor += 1;
    }
}

add_token_from_identifier :: (using lexer: Lexer, identifier: string, start: s64, end: s64) {
    token: Token;
    token.start = start;
    token.end = end;

    if compare(identifier, "final") == 0 {
        token.type = .KEYWORD_FINAL;
    } else if compare(identifier, "class") == 0 {
        token.type = .KEYWORD_CLASS;
    } else if compare(identifier, "struct") == 0 {
        token.type = .KEYWORD_STRUCT;
    } else if compare(identifier, "func") == 0 {
        token.type = .KEYWORD_FUNC;
    } else if compare(identifier, "return") == 0 {
        token.type = .KEYWORD_RETURN;
    } else if compare(identifier, "init") == 0 {
        token.type = .KEYWORD_INIT;
    } else if compare(identifier, "internal") == 0 {
        token.type = .KEYWORD_INTERNAL;
    } else if compare(identifier, "public") == 0 {
        token.type = .KEYWORD_PUBLIC;
    } else if compare(identifier, "private") == 0 {
        token.type = .KEYWORD_PRIVATE;
    } else if compare(identifier, "let") == 0 {
        token.type = .KEYWORD_LET;
    } else if compare(identifier, "var") == 0 {
        token.type = .KEYWORD_VAR;
    } else if compare(identifier, "lazy") == 0 {
        token.type = .KEYWORD_LAZY;
    } else if compare(identifier, "nil") == 0 {
        token.type = .KEYWORD_NIL;
    } else if compare(identifier, "true") == 0 {
        token.type = .KEYWORD_TRUE;
    } else if compare(identifier, "false") == 0 {
        token.type = .KEYWORD_FALSE;
    } else if compare(identifier, "if") == 0 {
        token.type = .KEYWORD_IF;
    } else if compare(identifier, "else") == 0 {
        token.type = .KEYWORD_ELSE;
    } else if compare(identifier, "while") == 0 {
        token.type = .KEYWORD_WHILE;
    } else if compare(identifier, "do") == 0 {
        token.type = .KEYWORD_DO;
    } else if compare(identifier, "guard") == 0 {
        token.type = .KEYWORD_GUARD;
    } else if compare(identifier, "switch") == 0 {
        token.type = .KEYWORD_SWITCH;
    } else if compare(identifier, "case") == 0 {
        token.type = .KEYWORD_CASE;
    } else if compare(identifier, "default") == 0 {
        token.type = .KEYWORD_DEFAULT;
    } else if compare(identifier, "for") == 0 {
        token.type = .KEYWORD_FOR;
    } else if compare(identifier, "continue") == 0 {
        token.type = .KEYWORD_CONTINUE;
    } else if compare(identifier, "break") == 0 {
        token.type = .KEYWORD_BREAK;
    } else if compare(identifier, "fallthrough") == 0 {
        token.type = .KEYWORD_FALLTHROUGH;
    } else if compare(identifier, "defer") == 0 {
        token.type = .KEYWORD_DEFER;
    } else if compare(identifier, "where") == 0 {
        token.type = .KEYWORD_WHERE;
    } else if compare(identifier, "async") == 0 {
        token.type = .KEYWORD_ASYNC;
    } else if compare(identifier, "await") == 0 {
        token.type = .KEYWORD_AWAIT;
    } else if compare(identifier, "throws") == 0 {
        token.type = .KEYWORD_THROWS;
    } else if compare(identifier, "throw") == 0 {
        token.type = .KEYWORD_THROW;
    } else if compare(identifier, "as") == 0 {
        token.type = .KEYWORD_AS;
    } else if compare(identifier, "is") == 0 {
        token.type = .KEYWORD_IS;
    } else if compare(identifier, "override") == 0 {
        token.type = .KEYWORD_OVERRIDE;
    } else if compare(identifier, "extension") == 0 {
        token.type = .KEYWORD_EXTENSION;
    } else if compare(identifier, "set") == 0 {
        token.type = .KEYWORD_SET;
    } else if compare(identifier, "import") == 0 {
        token.type = .KEYWORD_IMPORT;
    } else if compare(identifier, "get") == 0 {
        token.type = .KEYWORD_GET;
    } else if compare(identifier, "weak") == 0 {
        token.type = .KEYWORD_WEAK;
    } else if compare(identifier, "unowned") == 0 {
        token.type = .KEYWORD_UNOWNED;
    } else if compare(identifier, "strong") == 0 {
        token.type = .KEYWORD_STRONG;
    } else if compare(identifier, "self") == 0 {
        token.type = .KEYWORD_SELF;
    } else if compare(identifier, "inout") == 0 {
        token.type = .KEYWORD_INOUT;

    } else if begins_with(identifier, "@") {
        token.type = .ATTRIBUTE;
    } else if begins_with(identifier, "#") {
        token.type = .DIRECTIVE;
    } else if begins_with(identifier, "&") {
        token.type = .INOUT;
    } else if begins_with(identifier, "$") {
        token.type = .BINDING;
    } else {
        token.type = .IDENTIFIER;
    }

    if token.type == .ATTRIBUTE 
        || token.type == .DIRECTIVE 
        || token.type == .INOUT 
        || token.type == .IDENTIFIER
    {
        token.value = copy_string(identifier);
    }

    array_add(*tokens, token);
}

is_capitalized :: (c: u8) -> bool {
    return c >= #char "A" && c <= #char "Z";
}

is_alpha_fast :: (char: u8) -> bool {
    c := char & 0b1011111;
    return c >= #char "A" && c <= #char "Z";
}

Lexer :: struct {
    file_name: string;
    contents: string;
    cursor: s64;
    tokens: [..] Token;
}

Token :: struct {
    type: Token_Type;
    value: string;
    start: s64;
    end: s64;
}

Token_Type :: enum {
    // Here all Token_Types from ASCII codes
    LINE_BREAK :: 10;

    EOF :: 128;
    COMMENT;
    STRING;
    NUMBER;

    IDENTIFIER;
    ATTRIBUTE; // For example: @escaping
    BINDING; // $publisher
    INOUT; // &subscribers
    DIRECTIVE; // #if


    PLUS_EQUALS :: 200; // two symbol operators
    MINUS_EQUALS;
    TIMES_EQUALS;
    DIV_EQUALS;
    MODE_EQUALS;
    IS_EQUAL;
    IS_NOT_EQUAL;
    LOGICAL_AND;
    LOGICAL_OR;
    LESS_EQUALS;
    GREATER_EQUALS;
    LOGICAL_AND_EQUALS;
    LOGICAL_OR_EQUALS;

    RIGHT_ARROW; // ->
    DOUBLE_DOT;
    DOUBLE_DOLLAR;

    SHIFT_LEFT;
    SHIFT_RIGHT;

    DOUBLE_MINUS;
    DOUBLE_PLUS;

    TRIPLE_EQUALS :: 240; // three symbol operators
    SHIFT_LEFT_EQUALS;
    SHIFT_RIGHT_EQUALS;
    BITWISE_AND_EQUALS;
    BITWISE_OR_EQUALS;
    BITWISE_XOR_EQUALS;

    KEYWORD_FINAL :: 250;
    KEYWORD_CLASS;
    KEYWORD_STRUCT;
    KEYWORD_FUNC;
    KEYWORD_RETURN;
    KEYWORD_INIT;
    KEYWORD_INTERNAL;
    KEYWORD_PUBLIC;
    KEYWORD_PRIVATE;
    KEYWORD_LET;
    KEYWORD_VAR;
    KEYWORD_LAZY;
    KEYWORD_NIL;
    KEYWORD_TRUE;
    KEYWORD_FALSE;
    KEYWORD_IF;
    KEYWORD_ELSE;
    KEYWORD_WHILE;
    KEYWORD_DO;
    KEYWORD_GUARD;
    KEYWORD_SWITCH;
    KEYWORD_CASE;
    KEYWORD_DEFAULT;
    KEYWORD_FOR;
    KEYWORD_CONTINUE;
    KEYWORD_BREAK;
    KEYWORD_FALLTHROUGH;
    KEYWORD_DEFER;
    KEYWORD_WHERE;
    KEYWORD_ASYNC;
    KEYWORD_AWAIT;
    KEYWORD_THROWS;
    KEYWORD_THROW;
    KEYWORD_AS;
    KEYWORD_IS;
    KEYWORD_OVERRIDE;
    KEYWORD_SET;
    KEYWORD_GET;
    KEYWORD_EXTENSION;
    KEYWORD_IMPORT;
    KEYWORD_WEAK;
    KEYWORD_STRONG;
    KEYWORD_UNOWNED;
    KEYWORD_SELF;
    KEYWORD_INOUT;
}

#import "Basic";
#import "File";
#import "String";

