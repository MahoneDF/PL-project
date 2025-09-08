// Checker.kt
// Type checker for the language with forward-declared functions & records.

enum class VarKind { affine, relevant }

sealed class Type {
    object TInt : Type() { override fun toString() = "int" }
    object TString : Type() { override fun toString() = "string" }
    object TBoolean : Type() { override fun toString() = "bool" }
    data class TList(val elem: Type) : Type() { override fun toString() = "list<$elem>" }
    data class TRecord(val name: String) : Type() { override fun toString() = "record<$name>" }
    data class TFun(val params: List<Type>, var ret: Type) : Type() {
        override fun toString() = "(" + params.joinToString(", ") + ") -> $ret"
    }
    object TUnknown : Type() { override fun toString() = "unknown" }
}

// ----------------------
// AST
// ----------------------
data class Program(val stmts: List<Stmt>)

sealed class Stmt {
    data class Let(val kind: VarKind, val name: String, val expr: Expr) : Stmt()
    data class Fun(val name: String, val params: List<Param>, val body: Expr) : Stmt()
    data class RecordDecl(val name: String, val fields: List<FieldDecl>) : Stmt()
    data class ExprStmt(val expr: Expr) : Stmt()
}

data class Param(val kind: VarKind, val name: String, val type: Type)
data class FieldDecl(val name: String, val type: Type)

sealed class Expr {
    data class Var(val name: String) : Expr()
    data class IntLit(val n: Int) : Expr()
    data class StringLit(val s: String) : Expr()
    data class BoolLit(val b: Boolean) : Expr()
    data class Call(val fname: String, val args: List<Expr>) : Expr()
    data class Plus(val l: Expr, val r: Expr) : Expr()
    data class Not(val e: Expr) : Expr()
    data class If(val c: Expr, val t: Expr, val e: Expr) : Expr()
    data class RecordVal(val name: String, val fields: List<FieldAssign>) : Expr()
    data class FieldAccess(val target: Expr, val field: String) : Expr()
    data class ListLit(val elems: List<Expr>) : Expr()
}

data class FieldAssign(val name: String, val expr: Expr)

// ----------------------
// Environment & Usage Tracking
// ----------------------
data class Binding(val type: Type, val kind: VarKind, var outsideUses: Int = 0)

class Env {
    private val stack = ArrayDeque<MutableMap<String, Binding>>()
    fun push() = stack.addLast(mutableMapOf())
    fun pop(): MutableMap<String, Binding> = stack.removeLast()
    fun define(name: String, type: Type, kind: VarKind) {
        stack.last()[name] = Binding(type, kind)
    }
    fun lookup(name: String): Binding? {
        for (s in stack.reversed()) {
            s[name]?.let { return it }
        }
        return null
    }
    fun currentBindings(): Map<String, Binding> = stack.last()
    fun allBindings(): List<Pair<String, Binding>> = stack.flatMap { it.entries }.map { it.key to it.value }
}

// Record declarations (nominal)
class RecordEnv {
    private val map = mutableMapOf<String, Map<String, Type>>()
    fun define(name: String, fields: Map<String, Type>) { map[name] = fields }
    fun get(name: String): Map<String, Type>? = map[name]
    fun all(): Map<String, Map<String, Type>> = map
}

// ----------------------
// Type Checker
// ----------------------
class TypeChecker {
    val errors = mutableListOf<String>()
    private val env = Env()
    private val records = RecordEnv()
    private val funBodies = mutableMapOf<String, Pair<List<Param>, Expr>>() // store bodies for pass 2

    fun checkProgram(p: Program) {
        env.push()

        // -------- Pass 1: collect records & function signatures --------
        for (s in p.stmts) {
            when (s) {
                is Stmt.RecordDecl -> {
                    if (records.get(s.name) != null) errors.add("Record '${s.name}' already declared.")
                    else {
                        s.fields.forEach { validateType(it.type, env) }
                        records.define(s.name, s.fields.associate { it.name to it.type })
                    }
                }
                is Stmt.Fun -> {
                    if (env.lookup(s.name) != null) errors.add("Function '${s.name}' already declared.")
                    else {
                        s.params.forEach { validateType(it.type, env) }
                        val paramTypes = s.params.map { it.type }
                        env.define(s.name, Type.TFun(paramTypes, Type.TUnknown), VarKind.relevant)
                        funBodies[s.name] = s.params to s.body
                    }
                }
                else -> {} // skip lets/exprs
            }
        }

        // -------- Pass 2: check lets, exprs, and function bodies --------
        for (s in p.stmts) {
            when (s) {
                is Stmt.RecordDecl -> {} // already handled
                is Stmt.Fun -> checkFun(s)
                else -> checkStmt(s)
            }
        }

        // Final relevant checks at global scope
        val globalBindings = env.pop()
        for ((name, b) in globalBindings) {
            if (b.kind == VarKind.relevant && b.outsideUses == 0 && b.type !is Type.TFun) {
                errors.add("Relevant variable '$name' not used outside branches.")
            }
        }
    }

    private fun validateType(t: Type, env: Env) {
        when (t) {
            is Type.TList -> validateType(t.elem, env)
            is Type.TRecord -> {
                if (records.get(t.name) == null) {
                    errors += "Unknown specified type ${t.name}"
                }
            }
            is Type.TFun -> {
                t.params.forEach { validateType(it, env) }
                validateType(t.ret, env)
            }
            else -> {} // int, string, bool are always fine
        }
    }

    private fun checkFun(f: Stmt.Fun) {
        val b = env.lookup(f.name) ?: return
        val funT = b.type as? Type.TFun ?: return

        env.push()
        for (p in f.params) env.define(p.name, p.type, p.kind)
        val bodyT = typeOf(f.body, inBranch = false, branchMap = null)
        funT.ret = bodyT
        val localBindings = env.pop()

        for ((name, bind) in localBindings) {
            if (f.params.any { it.name == name }) {
                if (bind.kind == VarKind.relevant && bind.outsideUses == 0) {
                    errors.add("Relevant parameter '$name' of function '${f.name}' not used outside branches.")
                }
                if (bind.kind == VarKind.affine && bind.outsideUses > 1) {
                    errors.add("Affine parameter '$name' of function '${f.name}' used more than once outside branches.")
                }
            }
        }
    }

    private fun checkStmt(s: Stmt) {
        when (s) {
            is Stmt.Let -> {
                val t = typeOf(s.expr, inBranch = false, branchMap = null)
                env.define(s.name, t, s.kind)
            }
            is Stmt.ExprStmt -> typeOf(s.expr, inBranch = false, branchMap = null)
            else -> {}
        }
    }

    // -------- Expression type checking --------
    private fun typeOf(e: Expr, inBranch: Boolean, branchMap: MutableMap<String, Int>?): Type = when (e) {
        is Expr.IntLit -> Type.TInt
        is Expr.StringLit -> Type.TString
        is Expr.BoolLit -> Type.TBoolean
        is Expr.Var -> {
            val b = env.lookup(e.name)
            if (b == null) {
                errors.add("Use of undefined variable '${e.name}'.")
                Type.TUnknown
            } else {
                if (inBranch) branchMap?.merge(e.name, 1, Int::plus)
                else {
                    b.outsideUses++
                    if (b.kind == VarKind.affine && b.outsideUses > 1) {
                        errors.add("Affine variable '${e.name}' used more than once in definite context.")
                    }
                }
                b.type
            }
        }
//        is Expr.Plus -> {
//            val lt = typeOf(e.l, inBranch, branchMap)
//            val rt = typeOf(e.r, inBranch, branchMap)
//            if (lt != rt) errors.add("Type mismatch for '+': $lt vs $rt")
//            when (lt) {
//                Type.TInt, Type.TString, Type.TBoolean -> lt
//                is Type.TList -> lt
//                is Type.TRecord -> {
//                    val spec = records.get(lt.name)
//                    if (spec == null) errors.add("Unknown record type '${lt.name}' for '+'.")
//                    lt
//                }
//                else -> { errors.add("Invalid type for '+': $lt"); Type.TUnknown }
//            }
//        }
        is Expr.Plus -> {
            val lt = typeOf(e.l, inBranch, branchMap)
            val rt = typeOf(e.r, inBranch, branchMap)
            if (lt != rt) errors.add("Type mismatch for '+': $lt vs $rt")
            when {
                lt is Type.TInt && rt is Type.TInt -> Type.TInt
                lt is Type.TString && rt is Type.TString -> Type.TString
                lt is Type.TBoolean && rt is Type.TBoolean -> Type.TBoolean

                lt is Type.TList && rt is Type.TList -> {
                    if (lt.elem != rt.elem) {
//                        errors += "List element type mismatch in plus: $lt vs $rt"
                        Type.TUnknown
                    } else {
                        Type.TList(lt.elem)
                    }
                }

                lt is Type.TRecord && rt is Type.TRecord -> {
                    if (lt.name != rt.name) {
//                        errors += "Record type mismatch in plus: ${lt.name} vs ${rt.name}"
                        Type.TUnknown
                    } else {
                        // retrieve record fields from environment
                        val rec = records.get(lt.name)
                        if (rec == null) {
//                            errors += "Unknown record type ${lt.name} for '+'."
                            Type.TUnknown
                        }
                        lt
                    }
                }

                else -> {
                    errors += "Invalid operands for plus: $lt and $rt"
                    Type.TUnknown
                }
            }
        }
        is Expr.Not -> {
            val t = typeOf(e.e, inBranch, branchMap)
            if (t != Type.TBoolean) errors.add("Operator 'not' requires bool, got $t")
            Type.TBoolean
        }
        is Expr.If -> {
            val ct = typeOf(e.c, inBranch, branchMap)
            if (ct != Type.TBoolean) errors.add("Condition of if must be bool, got $ct")

            val snapshot = env.allBindings().associate { it.first to it.second.outsideUses }
            val thenMap = mutableMapOf<String, Int>()
            val tt = typeOf(e.t, inBranch = true, branchMap = thenMap)
            val elseMap = mutableMapOf<String, Int>()
            val et = typeOf(e.e, inBranch = true, branchMap = elseMap)

            if (tt != et) errors.add("If branches must return same type: $tt vs $et")

            val allNames = (thenMap.keys + elseMap.keys + snapshot.keys).toSet()
            for (name in allNames) {
                val b = env.lookup(name) ?: continue
                val outBefore = snapshot[name] ?: 0
                val thenUses = thenMap[name] ?: 0
                val elseUses = elseMap[name] ?: 0
                if (b.kind == VarKind.affine) {
                    if ((thenUses > 0) xor (elseUses > 0)) {
                        errors.add("Affine variable '$name' used in only one branch of if.")
                    }
                    if (outBefore + thenUses > 1) errors.add("Affine variable '$name' overused in then branch.")
                    if (outBefore + elseUses > 1) errors.add("Affine variable '$name' overused in else branch.")
                }
            }
            tt
        }
        is Expr.Call -> {
            val b = env.lookup(e.fname)
            if (b == null || b.type !is Type.TFun) {
                errors.add("Call to undefined or non-function '${e.fname}'.")
                Type.TUnknown
            } else {
                val ft = b.type as Type.TFun
                if (ft.params.size != e.args.size) errors.add("Function '${e.fname}' arity mismatch.")
                for ((i, arg) in e.args.withIndex()) {
                    val at = typeOf(arg, inBranch, branchMap)
                    if (i < ft.params.size && at != ft.params[i]) {
                        errors.add("Argument $i of '${e.fname}' expected ${ft.params[i]}, got $at")
                    }
                }
                ft.ret
            }
        }
        is Expr.RecordVal -> {
            val spec = records.get(e.name)
            if (spec == null) {
                errors.add("Unknown record '${e.name}'.")
                Type.TUnknown
            } else {
                val got = e.fields.associate { it.name to typeOf(it.expr, inBranch, branchMap) }
                val missing = spec.keys - got.keys
                val extra = got.keys - spec.keys
                if (missing.isNotEmpty()) errors.add("Record ${e.name} missing fields $missing")
                if (extra.isNotEmpty()) errors.add("Record ${e.name} has extra fields $extra")
                for ((fname, ftype) in spec) {
                    val gt = got[fname]
                    if (gt != null && gt != ftype) errors.add("Field $fname of ${e.name} expected $ftype, got $gt")
                }
                Type.TRecord(e.name)
            }
        }
        is Expr.FieldAccess -> {
            val tt = typeOf(e.target, inBranch, branchMap)
            if (tt is Type.TRecord) {
                val fields = records.get(tt.name)
                val ftype = fields?.get(e.field)
                if (ftype == null) {
                    errors.add("Record ${tt.name} has no field '${e.field}'.")
                    Type.TUnknown
                } else ftype
            } else {
                errors.add("Field access on non-record type $tt")
                Type.TUnknown
            }
        }
        is Expr.ListLit -> {
            if (e.elems.isEmpty()) {
                errors.add("Cannot infer type of empty list literal [].")
                Type.TUnknown
            } else {
                val t0 = typeOf(e.elems[0], inBranch, branchMap)
                for (i in 1 until e.elems.size) {
                    val ti = typeOf(e.elems[i], inBranch, branchMap)
                    if (ti != t0) errors.add("List elements must all have same type: got $t0 and $ti")
                }
                Type.TList(t0)
            }
        }
    }
}