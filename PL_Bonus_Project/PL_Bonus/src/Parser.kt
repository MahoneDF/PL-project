// Parser.kt
// Tokenizer + recursive descent parser for the language

// ----------------------
// Tokenizer
// ----------------------
sealed class Token {
    data class Ident(val value: String) : Token()
    data class Keyword(val value: String) : Token()
    data class Symbol(val value: String) : Token()
    data class IntLit(val value: Int) : Token()
    data class StringLit(val value: String) : Token()
    object EOF : Token()
}

fun tokenize(src: String): List<Token> {
    val tokens = mutableListOf<Token>()
    val re = Regex("\\s+|//.*|[a-zA-Z_][a-zA-Z0-9_]*|[0-9]+|\"[^\"]*\"|[{}(),:+\\[\\]=<>]")
    val matches = re.findAll(src)
    for (m in matches) {
        val s = m.value
        if (s.isBlank() || s.startsWith("//")) continue
        when {
            s.matches(Regex("[0-9]+")) -> tokens += Token.IntLit(s.toInt())
            s.startsWith("\"") -> tokens += Token.StringLit(s.drop(1).dropLast(1))
            s in listOf("if", "then", "else", "fun", "record", "not", "true", "false", "list") ->
                tokens += Token.Keyword(s)
            s in listOf("{", "}", "(", ")", ",", ":", "+", "=", "[", "]", "<", ">") ->
                tokens += Token.Symbol(s)
            else -> tokens += Token.Ident(s)
        }
    }
    tokens += Token.EOF
    return tokens
}

// ----------------------
// Parser
// ----------------------
class Parser(private val tokens: List<Token>) {
    private var pos = 0

    private fun peek(): Token = tokens.getOrElse(pos) { Token.EOF }
    private fun next(): Token = tokens.getOrElse(pos++) { Token.EOF }
    private fun expect(t: Token) {
        val n = next()
        if (n != t) error("Expected $t, got $n")
    }

    // Helper: parse comma-separated lists until a terminator
    private inline fun <T> parseCommaList(
        terminator: Token.Symbol,
        parseOne: () -> T
    ): List<T> {
        val items = mutableListOf<T>()
        var first = true
        while (first || peek() == Token.Symbol(",")) {
            if (!first) next() // consume comma
            first = false
            if (peek() == terminator) break
            items += parseOne()
        }
        return items
    }

    // ---------- Top level ----------
    fun parseProgram(): Program {
        val stmts = mutableListOf<Stmt>()
        while (peek() != Token.EOF) stmts += parseStmt()
        return Program(stmts)
    }

    private fun parseStmt(): Stmt {
        return when (val tk = peek()) {
            is Token.Keyword -> when (tk.value) {
                "record" -> {
                    next()
                    val name = (next() as Token.Ident).value
                    expect(Token.Symbol("{"))
                    val fields =
                        if (peek() == Token.Symbol("}")) emptyList()
                        else parseCommaList(Token.Symbol("}")) {
                            val fname = (next() as Token.Ident).value
                            expect(Token.Symbol(":"))
                            val t = parseType()
                            FieldDecl(fname, t)
                        }
                    expect(Token.Symbol("}"))
                    Stmt.RecordDecl(name, fields)
                }
                "fun" -> {
                    next()
                    val name = (next() as Token.Ident).value
                    expect(Token.Symbol("("))
                    val params =
                        if (peek() == Token.Symbol(")")) emptyList()
                        else parseCommaList(Token.Symbol(")")) {
                            val kind = parseKind()
                            val pname = (next() as Token.Ident).value
                            expect(Token.Symbol(":"))
                            val t = parseType()
                            Param(kind, pname, t)
                        }
                    expect(Token.Symbol(")"))
                    expect(Token.Symbol("="))
                    val body = parseExpr()
                    Stmt.Fun(name, params, body)
                }
                else -> Stmt.ExprStmt(parseExpr())
            }
            is Token.Ident -> {
                when (tk.value) {
                    "affine", "relevant" -> {
                        // variable declaration without 'let'
                        val kind = parseKind()
                        val name = (next() as Token.Ident).value
                        expect(Token.Symbol("="))
                        val e = parseExpr()
                        Stmt.Let(kind, name, e)
                    }
                    else -> Stmt.ExprStmt(parseExpr())
                }
            }
            else -> Stmt.ExprStmt(parseExpr())
        }
    }

    private fun parseKind(): VarKind {
        val id = (next() as Token.Ident).value
        return when (id) {
            "affine" -> VarKind.affine
            "relevant" -> VarKind.relevant
            else -> error("Unknown var kind: $id")
        }
    }

    // ---------- Expressions ----------
    private fun parseExpr(): Expr = parseIf()

    private fun parseIf(): Expr {
        return if (peek() is Token.Keyword && (peek() as Token.Keyword).value == "if") {
            next()
            val cond = parseExpr()
            expect(Token.Keyword("then"))
            val t = parseExpr()
            expect(Token.Keyword("else"))
            val e = parseExpr()
            Expr.If(cond, t, e)
        } else parseSum()
    }

    private fun parseSum(): Expr {
        var e = parseTerm()
        while (peek() == Token.Symbol("+")) {
            next()
            val r = parseTerm()
            e = Expr.Plus(e, r)
        }
        return e
    }

    private fun parseTerm(): Expr {
        return when (val tk = next()) {
            is Token.IntLit -> Expr.IntLit(tk.value)
            is Token.StringLit -> Expr.StringLit(tk.value)
            is Token.Keyword -> when (tk.value) {
                "true" -> Expr.BoolLit(true)
                "false" -> Expr.BoolLit(false)
                "not" -> Expr.Not(parseTerm())
                else -> error("Unexpected keyword ${tk.value}")
            }
            is Token.Ident -> {
                if (peek() == Token.Symbol("(")) {
                    // function call
                    next()
                    val args =
                        if (peek() == Token.Symbol(")")) emptyList()
                        else parseCommaList(Token.Symbol(")")) { parseExpr() }
                    expect(Token.Symbol(")"))
                    Expr.Call(tk.value, args)
                } else if (peek() == Token.Symbol("{")) {
                    // record value
                    next()
                    val fields =
                        if (peek() == Token.Symbol("}")) emptyList()
                        else parseCommaList(Token.Symbol("}")) {
                            val fname = (next() as Token.Ident).value
                            expect(Token.Symbol("="))
                            val fe = parseExpr()
                            FieldAssign(fname, fe)
                        }
                    expect(Token.Symbol("}"))
                    Expr.RecordVal(tk.value, fields)
                } else {
                    Expr.Var(tk.value)
                }
            }
            is Token.Symbol -> when (tk.value) {
                "(" -> {
                    val e = parseExpr()
                    expect(Token.Symbol(")"))
                    e
                }
                "[" -> {
                    val elems =
                        if (peek() == Token.Symbol("]")) emptyList()
                        else parseCommaList(Token.Symbol("]")) { parseExpr() }
                    expect(Token.Symbol("]"))
                    Expr.ListLit(elems)
                }
                else -> error("Unexpected symbol ${tk.value}")
            }
            else -> error("Unexpected token $tk")
        }
    }

    // ---------- Types ----------
    private fun parseType(): Type {
        return when (val tk = next()) {
            is Token.Ident -> when (tk.value) {
                "int" -> Type.TInt
                "string" -> Type.TString
                "bool" -> Type.TBoolean
                else -> Type.TRecord(tk.value)
            }
            is Token.Keyword -> when (tk.value) {
                "list" -> {
                    expect(Token.Symbol("<"))
                    val elem = parseType()
                    expect(Token.Symbol(">"))
                    Type.TList(elem)
                }
                else -> error("Unexpected keyword in type: ${tk.value}")
            }
            else -> error("Unexpected token in type: $tk")
        }
    }
}